# ------------------------------------------------------------------------
# Funciones de apoyo de los Capítulos 7 y 8 (análisis descriptivo y regresión)
# Uso en un capítulo:   source("R/funciones_extra_cap78.R")
# Requiere tidyverse (dplyr, tibble, purrr) y broom cargados.
# ------------------------------------------------------------------------

# ---- Cap. 7.5: estandarización directa ---------------------------------

#' Estandarización directa de una tasa o de un riesgo
#'
#' @param datos tibble con UNA FILA por grupo x estrato (p. ej., unidad x grupo de edad)
#' @param grupo nombre (texto) de la columna que define lo que se compara (unidad, tratamiento...)
#' @param estrato nombre (texto) de la columna que identifica el estrato (edad, o edad x sexo)
#' @param eventos nombre de la columna con el número de eventos
#' @param denominador nombre de la columna con personas-año (tasa) o con el número de personas (riesgo)
#' @param estandar opcional: tibble con columnas `estrato` y `pop_estandar`. Si es NULL, el estándar
#'   es la suma del denominador de todos los grupos (la "población total del estudio")
#' @param tipo "tasa" (varianza de Poisson) o "riesgo" (varianza binomial)
#' @param por multiplicador (100 = por 100 personas-año o %; 1000, etc.)
#' @return tibble por grupo: crudo, estandarizado, error estándar e IC (aprox. normal)
estandarizar_directa <- function(datos, grupo, estrato, eventos, denominador,
                                 estandar = NULL, tipo = c("tasa", "riesgo"), por = 100, nivel = 0.95) {
  tipo <- match.arg(tipo)
  z <- stats::qnorm(1 - (1 - nivel) / 2)
  d <- tibble::tibble(grupo = datos[[grupo]], estrato = datos[[estrato]],
                      d = datos[[eventos]], n = datos[[denominador]])
  if (is.null(estandar)) {
    estandar <- d |> dplyr::group_by(estrato) |> dplyr::summarise(pop_estandar = sum(n), .groups = "drop")
  }
  estandar <- estandar |> dplyr::mutate(peso = pop_estandar / sum(pop_estandar))
  d |>
    dplyr::left_join(estandar |> dplyr::select(estrato, peso), by = "estrato") |>
    dplyr::group_by(grupo) |>
    dplyr::summarise(
      crudo = sum(d) / sum(n),
      estandarizado = sum(peso * d / n),
      varianza = if (tipo == "tasa") sum(peso^2 * d / n^2) else sum(peso^2 * (d / n) * (1 - d / n) / n),
      .groups = "drop"
    ) |>
    dplyr::mutate(ee = sqrt(varianza), inf = estandarizado - z * ee, sup = estandarizado + z * ee,
                  dplyr::across(c(crudo, estandarizado, ee, inf, sup), \(x) por * x)) |>
    dplyr::select(grupo, crudo, estandarizado, ee, inf, sup)
}

#' Razón de dos tasas (o riesgos) estandarizados, con IC por el método delta (en escala log)
#' @param res salida de estandarizar_directa()   @param numerador,referencia nombres de los grupos
razon_estandarizada <- function(res, numerador, referencia, nivel = 0.95) {
  z <- stats::qnorm(1 - (1 - nivel) / 2)
  a <- res[res$grupo == numerador, ]; b <- res[res$grupo == referencia, ]
  rr <- a$estandarizado / b$estandarizado
  se_log <- sqrt((a$ee / a$estandarizado)^2 + (b$ee / b$estandarizado)^2)
  tibble::tibble(razon = rr, inf = exp(log(rr) - z * se_log), sup = exp(log(rr) + z * se_log))
}

# ---- Cap. 8.3: conteos con sobredispersión (datos SIMULADOS) --------------

#' Episodios de peritonitis en pacientes en diálisis peritoneal (SIMULADO)
#'
#' Verdad conocida: razón de tasas (IRR) del programa de entrenamiento = 0.60 y de la diabetes = 1.30.
#' Cada paciente tiene una "fragilidad" aleatoria no medida (gamma, media 1) que multiplica su tasa:
#' eso crea SOBREDISPERSIÓN (la varianza de los conteos supera su media).
#' @return tibble: id, programa (0/1), diabetes (0/1), seguimiento_anios, episodios
simular_peritonitis <- function(n = 500, semilla = 1) {
  set.seed(semilla)
  programa <- rbinom(n, 1, 0.5)
  diabetes <- rbinom(n, 1, 0.4)
  seguimiento <- round(runif(n, 0.5, 3), 2)
  fragilidad <- rgamma(n, shape = 1.5, rate = 1.5)
  tasa <- 0.55 * 0.60^programa * 1.30^diabetes * fragilidad          # episodios por paciente-año
  tibble::tibble(id = 1:n, programa, diabetes, seguimiento_anios = seguimiento,
                 episodios = rpois(n, tasa * seguimiento))
}

# ---- Cap. 8.4: interacción aditiva entre dos exposiciones binarias -----------

#' RR conjuntos y medidas de interacción aditiva (RERI, AP, SI) y multiplicativa
#'
#' Ajusta un modelo de Poisson (enlace log: los coeficientes son log-RR) con el producto a:b y
#' devuelve los RR de cada combinación frente a UNA sola referencia (a = 0, b = 0), como recomiendan
#' Knol y VanderWeele (2012). Para que la referencia sea la categoría de MENOR riesgo, codifica
#' ambas exposiciones como "de riesgo" (recodifica el factor protector).
#' Está pensada para usarse con boot::boot(): el argumento `i` son los índices de la muestra.
#'
#' @param datos base   @param i índices (para bootstrap)   @param desenlace nombre de la columna 0/1
#' @param a,b nombres de las dos exposiciones binarias (0/1)
#' @param covariables vector de texto con covariables de ajuste (puede incluir I(edad/10), etc.)
#' @return vector con RR10, RR01, RR11, RERI, AP, SI, razon_multiplicativa y RR_a_dentro_de_b1
#'   (RR10 = RR de a cuando b = 0; RR_a_dentro_de_b1 = RR11 / RR01 = RR de a cuando b = 1)
interaccion_aditiva <- function(datos, i = seq_len(nrow(datos)), desenlace, a, b, covariables = character()) {
  f <- stats::reformulate(c(paste0(a, "*", b), covariables), response = desenlace)
  m <- suppressWarnings(stats::glm(f, data = datos[i, ], family = stats::poisson))
  cf <- stats::coef(m)
  rr10 <- exp(cf[[a]]); rr01 <- exp(cf[[b]]); rr11 <- exp(cf[[a]] + cf[[b]] + cf[[paste0(a, ":", b)]])
  c(RR10 = rr10, RR01 = rr01, RR11 = rr11,
    RERI = rr11 - rr10 - rr01 + 1,
    AP = (rr11 - rr10 - rr01 + 1) / rr11,
    SI = (rr11 - 1) / ((rr10 - 1) + (rr01 - 1)),
    razon_multiplicativa = rr11 / (rr10 * rr01),
    RR_a_dentro_de_b1 = rr11 / rr01)
}

# ---- Cap. 8.5: ejemplo "feo" para aprender a leer diagnósticos (datos SIMULADOS) ----------

#' Calcificación vascular según el tiempo en hemodiálisis (SIMULADO)
#' La relación verdadera es CURVA (cuadrática) y se añaden dos valores extremos (atípicos influyentes).
simular_calcificacion <- function(n = 120, semilla = 66) {
  set.seed(semilla)
  anios <- runif(n, 0, 15)
  score <- 20 + 0.35 * anios^2 + rnorm(n, 0, 6)
  d <- tibble::tibble(anios_hd = anios, score_calcificacion = score)
  dplyr::bind_rows(tibble::tibble(anios_hd = c(12, 14.5), score_calcificacion = c(190, 230)), d)
}

# ---- Cap. 8.6: tablas de regresión y estabilidad de la selección de variables -------------

#' Tabla "crudo y ajustado" de un modelo: una fila por covariable
#'
#' Ajusta un modelo univariable por cada variable (estimación cruda) y un modelo multivariable con todas
#' (estimación ajustada), y los junta en una tabla "estimación (IC 95 %) y p". Es el equivalente en R base
#' de gtsummary::tbl_uvregression() + tbl_regression() + tbl_merge().
#' OJO: sirve para PRESENTAR, no para elegir variables (ver Cap. 8.6).
#' @param variables vector de texto con los términos (p. ej. "isglt2", "I(edad/10)", "sexo")
#' @param familia familia de glm (poisson, binomial, gaussian)   @param offset nombre de la variable de tiempo (opcional)
#' @param exponenciar TRUE para RR/OR/IRR; FALSE para diferencias de medias   @param etiquetas vector nombrado term -> texto
tabla_univ_multi <- function(datos, desenlace, variables, familia = stats::poisson, offset = NULL,
                             exponenciar = TRUE, etiquetas = NULL, digitos = 2) {
  agregar_offset <- function(terminos) c(terminos, if (!is.null(offset)) paste0("offset(log(", offset, "))"))
  ajustar <- function(terminos) stats::glm(stats::reformulate(agregar_offset(terminos), desenlace), data = datos, family = familia)
  formatear <- function(m) {
    broom::tidy(m, conf.int = TRUE, exponentiate = exponenciar) |>
      dplyr::filter(term != "(Intercept)") |>
      dplyr::mutate(est = sprintf(paste0("%.", digitos, "f (%.", digitos, "f, %.", digitos, "f)"), estimate, conf.low, conf.high),
                    p = dplyr::if_else(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))) |>
      dplyr::select(term, est, p)
  }
  crudo <- purrr::map_dfr(variables, \(v) formatear(ajustar(v)))
  multi <- formatear(ajustar(variables))
  tabla <- dplyr::left_join(crudo, multi, by = "term", suffix = c("_crudo", "_ajustado"))
  if (!is.null(etiquetas)) tabla$term <- dplyr::recode(tabla$term, !!!etiquetas)
  names(tabla) <- c("Variable", "Crudo (IC 95 %)", "p (crudo)", "Ajustado (IC 95 %)", "p (ajustado)")
  tabla
}

#' Estabilidad de la selección de variables por bootstrap (idea de Heinze, Wallisch y Dunkler, 2018)
#'
#' Repite en cada muestra bootstrap una eliminación hacia atrás por AIC (con la exposición SIEMPRE dentro del
#' modelo) y cuenta con qué frecuencia entra cada variable candidata ("frecuencia de inclusión"). Guarda
#' también el coeficiente de la exposición en el modelo global y en el modelo seleccionado de cada muestra.
#' @return lista: seleccionadas (modelo en la muestra original), inclusion (tibble), n_modelos_distintos,
#'   pct_modelo_original y coeficientes (tibble: replica, global, seleccionado)
estabilidad_seleccion <- function(datos, desenlace, exposicion, candidatas, B = 100,
                                  familia = stats::binomial, semilla = 1) {
  f_global <- stats::reformulate(c(exposicion, candidatas), desenlace)
  f_minimo <- stats::reformulate(exposicion, desenlace)
  elegir <- function(d) {
    m <- suppressWarnings(stats::glm(f_global, data = d, family = familia))
    mm <- suppressWarnings(stats::step(m, scope = list(lower = f_minimo, upper = f_global),
                                       direction = "backward", trace = 0))
    list(terminos = attr(stats::terms(mm), "term.labels"),
         global = unname(stats::coef(m)[exposicion]), seleccionado = unname(stats::coef(mm)[exposicion]))
  }
  set.seed(semilla)
  original <- elegir(datos)
  boots <- purrr::map(seq_len(B), \(b) elegir(datos[sample(nrow(datos), replace = TRUE), ]))
  terminos <- purrr::map(boots, "terminos")
  inclusion <- tibble::tibble(
    variable = candidatas,
    inclusion_pct = 100 * purrr::map_dbl(candidatas, \(v) mean(purrr::map_lgl(terminos, \(s) v %in% s))),
    en_modelo_original = candidatas %in% original$terminos)
  modelos <- purrr::map_chr(terminos, \(s) paste(sort(setdiff(s, exposicion)), collapse = " + "))
  list(seleccionadas = original$terminos, inclusion = inclusion, n_modelos_distintos = dplyr::n_distinct(modelos),
       pct_modelo_original = 100 * mean(modelos == paste(sort(setdiff(original$terminos, exposicion)), collapse = " + ")),
       coeficientes = tibble::tibble(replica = seq_len(B), global = purrr::map_dbl(boots, "global"),
                                     seleccionado = purrr::map_dbl(boots, "seleccionado")))
}
