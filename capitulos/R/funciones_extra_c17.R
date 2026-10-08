# ------------------------------------------------------------------------
# Funciones de apoyo del Capítulo 17 (modelos de pronóstico)
# Uso en un capítulo:   source("R/funciones_extra_c17.R")
# Requiere tidyverse y survival. Algunas funciones usan cmprsk (riesgos competitivos).
# Todos los datos que usan estas funciones son SIMULADOS (cohorte_pronostico, cohorte_validacion).
# ------------------------------------------------------------------------

# ---- Datos -------------------------------------------------------------

#' Prepara una cohorte de pronóstico: sexo como factor, ln(UACR) y los indicadores de cada causa
#' (estado: 0 censura, 1 falla renal, 2 muerte antes de la falla renal)
preparar_pronostico <- function(datos) {
  datos |>
    dplyr::mutate(sexo = factor(sexo, levels = c("Mujer", "Hombre")),
                  lu = log(uacr),
                  krt = as.integer(estado == 1),
                  muerte = as.integer(estado == 2)) |>
    as.data.frame()
}

#' "Oráculo" de la cohorte de pronóstico: una cohorte grande SIN censura, simulada con el mismo
#' mecanismo. Como todos los pacientes se siguen hasta el evento o los 60 meses, la proporción con
#' falla renal a 24 o 60 meses es el riesgo VERDADERO de esa población (solo posible en simulación).
#' @param cohorte "desarrollo" (como cohorte_pronostico) o "validacion" (como cohorte_validacion)
oraculo_pronostico <- function(n = 50000, cohorte = c("desarrollo", "validacion"), semilla = 99) {
  cohorte <- match.arg(cohorte)
  entorno <- new.env()
  source("R/simular_cohorte_pronostico.R", local = entorno)
  preparar_pronostico(entorno$simular_cohorte_pronostico(n, semilla, cohorte, censura = FALSE))
}

# ---- 17.1 Tamaño de muestra ------------------------------------------------

#' Eventos por parámetro (EPP; en la literatura, "EPV" cuando se cuentan variables)
#' @param eventos número de eventos (el menor de los dos grupos si el desenlace es binario)
#' @param parametros número de coeficientes que se estiman (no de variables: un spline cuenta varios)
epv <- function(eventos, parametros) eventos / parametros

#' Tamaño de muestra mínimo para desarrollar un modelo de pronóstico: versión SIMPLIFICADA
#' de los criterios de Riley y colaboradores (Riley et al., Stat Med 2019 y BMJ 2020).
#' Es una implementación didáctica propia; para un protocolo usa la herramienta oficial
#' (paquete pmsampsize). Devuelve el n que exige cada criterio; se necesita el MAYOR.
#'
#'   Criterio 1: encogimiento esperado >= `shrinkage` (el modelo no se sobreajusta mucho).
#'   Criterio 2: optimismo en el R2 de Nagelkerke <= `delta` (diferencia entre aparente y real).
#'   Criterio 3: precisión del riesgo global (margen de error <= `margen`).
#'
#' @param parametros número de coeficientes candidatos (incluye términos de spline y dummies)
#' @param r2_cs R2 de Cox-Snell que se espera del modelo (de un estudio previo o piloto)
#' @param tipo "binario" (usa `prevalencia`) o "supervivencia" (usa `tasa`, `seguimiento_medio`, `riesgo_t`)
#' @param prevalencia proporción con el desenlace (binario)
#' @param tasa eventos por persona-año (supervivencia)
#' @param seguimiento_medio seguimiento medio en años (supervivencia)
#' @param riesgo_t riesgo acumulado esperado a tiempo `t` (p. ej. a 5 años); para el criterio 3 en supervivencia
tamano_muestra_riley <- function(parametros, r2_cs, tipo = c("binario", "supervivencia"),
                                 prevalencia = NULL, tasa = NULL, seguimiento_medio = NULL,
                                 riesgo_t = NULL, shrinkage = 0.9, delta = 0.05, margen = 0.05) {
  tipo <- match.arg(tipo)
  z <- stats::qnorm(0.975)

  # Fracción de eventos por persona y R2 de Cox-Snell máximo posible (depende del desenlace)
  if (tipo == "binario") {
    e <- prevalencia
    lnl0_por_persona <- e * log(e) + (1 - e) * log(1 - e)
  } else {
    e <- tasa * seguimiento_medio                       # eventos por persona en el seguimiento
    lnl0_por_persona <- e * log(e) - e
  }
  r2_max <- 1 - exp(2 * lnl0_por_persona)

  n_formula <- function(s) parametros / ((s - 1) * log(1 - r2_cs / s))

  n1 <- n_formula(shrinkage)
  s2 <- r2_cs / (r2_cs + delta * r2_max)                # encogimiento que exige un optimismo <= delta
  n2 <- n_formula(s2)

  if (tipo == "binario") {
    n3 <- (z / margen)^2 * prevalencia * (1 - prevalencia)
  } else {                                              # aproximación exponencial para el riesgo a t
    S <- 1 - riesgo_t
    eventos3 <- (z * S * log(1 / S) / margen)^2
    n3 <- eventos3 / riesgo_t
  }

  tibble::tibble(
    criterio = c("1. Encogimiento (>= 0.90)", "2. Optimismo del R2 (<= 0.05)", "3. Precisión del riesgo global"),
    n = ceiling(c(n1, n2, n3)),
    eventos = ceiling(c(n1, n2, n3) * e)
  )
}

# ---- 17.2-17.3 Riesgo absoluto con y sin riesgo competitivo ---------------------

#' Riesgo a `t` meses de un modelo de Cox de UNA causa, ignorando la competencia (1 - S(t)).
#' Es lo que hacen KFRE y casi todas las calculadoras clásicas: trata la muerte como censura.
riesgo_cox_ingenuo <- function(modelo, datos, t) {
  bh <- survival::basehaz(modelo, centered = TRUE)
  h0 <- stats::approx(c(0, bh$time), c(0, bh$hazard), xout = t, method = "constant", f = 0, rule = 2)$y
  lp <- stats::predict(modelo, newdata = datos, type = "lp")
  as.numeric(1 - exp(-h0 * exp(lp)))
}

#' Riesgo a `t` meses de la causa 1 (incidencia acumulada) combinando dos modelos de Cox por causa:
#' el del evento de interés y el del evento competidor. Usa riesgos mensuales y la fórmula de
#' Aalen-Johansen: CIF(t) = suma sobre los meses de S(inicio del mes) x probabilidad de la causa 1 en ese mes.
#' @param m_evento coxph() de la causa de interés (falla renal)
#' @param m_competidor coxph() de la causa competidora (muerte)
#' @param t horizonte en meses (entero)
cif_cox <- function(m_evento, m_competidor, datos, t) {
  meses <- seq_len(t)
  incremento <- function(m) {
    bh <- survival::basehaz(m, centered = TRUE)
    H <- stats::approx(c(0, bh$time), c(0, bh$hazard), xout = c(0, meses), method = "constant", f = 0, rule = 2)$y
    diff(H)
  }
  e1 <- exp(stats::predict(m_evento, newdata = datos, type = "lp"))
  e2 <- exp(stats::predict(m_competidor, newdata = datos, type = "lp"))
  h1 <- outer(as.numeric(e1), incremento(m_evento))              # riesgo mensual de la causa 1
  h2 <- outer(as.numeric(e2), incremento(m_competidor))          # riesgo mensual de la causa 2
  h_tot <- h1 + h2
  acum <- t(apply(h_tot, 1, cumsum))                             # riesgo acumulado total
  s_previa <- exp(-cbind(0, acum[, seq_len(t - 1), drop = FALSE]))   # supervivencia al inicio de cada mes
  # probabilidad de tener la causa 1 en el mes = (parte de la causa 1) x (prob. de algún evento en el mes)
  p_mes <- ifelse(h_tot > 0, h1 / h_tot * (1 - exp(-h_tot)), 0)
  as.numeric(rowSums(s_previa * p_mes))
}

#' Incidencia acumulada OBSERVADA (Aalen-Johansen) de la causa `causa` a `t` meses, con su error estándar
cif_observada <- function(tiempo, estado, t, causa = 1) {
  ci <- cmprsk::cuminc(tiempo, estado, cencode = 0)
  tp <- cmprsk::timepoints(ci, t)
  fila <- paste("1", causa)
  list(est = unname(tp$est[fila, 1]), ee = unname(sqrt(tp$var[fila, 1])))
}

#' Tabla de calibración: riesgo medio predicho vs. incidencia acumulada observada por grupos de riesgo
#' @param g número de grupos de igual tamaño (quintiles = 5, deciles = 10)
tabla_calibracion <- function(pred, tiempo, estado, t, g = 5, causa = 1) {
  grupo <- dplyr::ntile(pred, g)
  purrr::map_dfr(seq_len(g), \(k) {
    i <- grupo == k
    o <- cif_observada(tiempo[i], estado[i], t, causa)
    tibble::tibble(grupo = k, n = sum(i), predicho = mean(pred[i]), observado = o$est, ee = o$ee,
                   inf = pmax(0, o$est - 1.96 * o$ee), sup = pmin(1, o$est + 1.96 * o$ee))
  })
}

#' Gráfico de calibración: observado (eje y) contra predicho (eje x); diagonal = calibración perfecta
grafico_calibracion <- function(tab, titulo = NULL, color = "#0072B2") {
  lim <- max(c(tab$predicho, tab$sup), na.rm = TRUE) * 1.05
  ggplot2::ggplot(tab, ggplot2::aes(predicho, observado)) +
    ggplot2::geom_abline(linetype = "dashed", color = "grey40") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = inf, ymax = sup), width = lim / 60, color = color, alpha = 0.7) +
    ggplot2::geom_line(color = color, alpha = 0.5) +
    ggplot2::geom_point(color = color, size = 2.4) +
    ggplot2::coord_equal(xlim = c(0, lim), ylim = c(0, lim)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = "Riesgo predicho", y = "Riesgo observado", title = titulo)
}

#' Pseudo-observaciones de la incidencia acumulada de la causa `causa` a `t` meses (una por paciente).
#' Su promedio es la incidencia acumulada observada (Aalen-Johansen), pero cada paciente aporta un
#' valor individual que se puede usar como "desenlace observado" aunque haya censura y riesgo competitivo.
pseudo_cif <- function(tiempo, estado, t, causa = 1) {
  d <- data.frame(tiempo = tiempo, estado = estado)
  pr <- prodlim::prodlim(prodlim::Hist(tiempo, estado) ~ 1, data = d)
  as.numeric(prodlim::jackknife(pr, times = t, cause = causa))
}

#' Resumen de calibración de riesgos a `t` meses con riesgo competitivo (intercepto y pendiente).
#' Se regresan las pseudo-observaciones sobre logit(riesgo predicho) con un modelo tipo GEE:
#'   - pendiente: coeficiente de logit(predicho) (ideal = 1; < 1 = predicciones demasiado extremas)
#'   - intercepto ("calibración en grande"): estimado con la pendiente fijada en 1 (ideal = 0;
#'     < 0 = el modelo sobreestima el riesgo)
#'   - oe: incidencia observada / riesgo medio predicho (ideal = 1)
resumen_calibracion <- function(pred, tiempo, estado, t, causa = 1) {
  pseudo <- pseudo_cif(tiempo, estado, t, causa)
  d <- data.frame(ps = pseudo, x = stats::qlogis(pmin(pmax(pred, 1e-4), 1 - 1e-4)), id = seq_along(pred))
  g_pend <- geepack::geese(ps ~ x, id = id, data = d, mean.link = "logit", variance = "binomial")
  g_int <- geepack::geese(ps ~ 1 + offset(x), id = id, data = d, mean.link = "logit", variance = "binomial")
  tibble::tibble(predicho_medio = mean(pred), observado = mean(pseudo), oe = mean(pseudo) / mean(pred),
                 intercepto = unname(g_int$beta[1]), ee_intercepto = unname(sqrt(g_int$vbeta[1, 1])),
                 pendiente = unname(g_pend$beta[2]), ee_pendiente = unname(sqrt(g_pend$vbeta[2, 2])))
}

#' Índice C de Harrell de una puntuación de riesgo (lp mayor = más riesgo) para un evento con censura
c_harrell <- function(lp, tiempo, evento) {
  cc <- survival::concordance(survival::Surv(tiempo, evento) ~ lp, reverse = TRUE)
  c(C = unname(cc$concordance), ee = sqrt(unname(cc$var)))
}

# ---- 17.7 PROBAST --------------------------------------------------------

#' Las 20 preguntas guía de PROBAST (Moons et al., Ann Intern Med 2019), en 4 dominios.
#' Texto traducido y resumido; el instrumento oficial es la referencia.
probast_preguntas <- function() {
  tibble::tribble(
    ~dominio,        ~id,   ~pregunta,
    "Participantes", "1.1", "¿Las fuentes de datos fueron adecuadas (cohorte, ensayo, registro)?",
    "Participantes", "1.2", "¿Las inclusiones y exclusiones fueron adecuadas?",
    "Predictores",   "2.1", "¿Los predictores se definieron y midieron igual en todos los participantes?",
    "Predictores",   "2.2", "¿Se evaluaron sin conocer el desenlace?",
    "Predictores",   "2.3", "¿Están disponibles en el momento en que se usará el modelo?",
    "Desenlace",     "3.1", "¿El desenlace se determinó de forma apropiada?",
    "Desenlace",     "3.2", "¿Se usó una definición estándar o preespecificada?",
    "Desenlace",     "3.3", "¿Se excluyeron los predictores de la definición del desenlace?",
    "Desenlace",     "3.4", "¿Se definió y determinó igual en todos los participantes?",
    "Desenlace",     "3.5", "¿Se determinó sin conocer los predictores?",
    "Desenlace",     "3.6", "¿El intervalo entre predictores y desenlace fue apropiado?",
    "Análisis",      "4.1", "¿Había un número razonable de eventos?",
    "Análisis",      "4.2", "¿Los predictores continuos y categóricos se trataron bien?",
    "Análisis",      "4.3", "¿Se incluyó en el análisis a todos los participantes inscritos?",
    "Análisis",      "4.4", "¿Los datos faltantes se manejaron bien?",
    "Análisis",      "4.5", "¿Se evitó seleccionar predictores por análisis univariable?",
    "Análisis",      "4.6", "¿Se tuvieron en cuenta las complejidades de los datos (censura, riesgos competitivos, agrupamiento)?",
    "Análisis",      "4.7", "¿Se evaluaron bien las medidas de rendimiento (discriminación y calibración)?",
    "Análisis",      "4.8", "¿Se controló el sobreajuste y el optimismo?",
    "Análisis",      "4.9", "¿Los predictores y pesos del modelo final corresponden al análisis multivariable?"
  )
}

#' Juicio de riesgo de sesgo por dominio según la regla de PROBAST.
#' @param respuestas tibble con columnas `id` y `respuesta` (S, PS, PN, N, SI = sin información)
#' @return tibble por dominio: riesgo de sesgo "Bajo", "Alto" o "Poco claro" y el global
#' Regla: cualquier N/PN -> riesgo ALTO; todas S/PS -> BAJO; el resto (hay SI) -> POCO CLARO.
#' El juicio global es ALTO si algún dominio es alto, BAJO si todos son bajos, POCO CLARO si no.
probast_juicio <- function(respuestas) {
  d <- probast_preguntas() |> dplyr::left_join(respuestas, by = "id")
  por_dominio <- d |>
    dplyr::group_by(dominio = factor(dominio, levels = unique(probast_preguntas()$dominio))) |>
    dplyr::summarise(riesgo_sesgo = dplyr::case_when(
      any(respuesta %in% c("N", "PN")) ~ "Alto",
      all(respuesta %in% c("S", "PS")) ~ "Bajo",
      TRUE ~ "Poco claro"), .groups = "drop")
  global <- dplyr::case_when("Alto" %in% por_dominio$riesgo_sesgo ~ "Alto",
                             all(por_dominio$riesgo_sesgo == "Bajo") ~ "Bajo",
                             TRUE ~ "Poco claro")
  dplyr::bind_rows(por_dominio |> dplyr::mutate(dominio = as.character(dominio)),
                   tibble::tibble(dominio = "GLOBAL", riesgo_sesgo = global))
}

# ---- 17.1 y 17.4 Sobreajuste: submuestras de la cohorte contra el oráculo -----------

#' Experimento de sobreajuste. Repite R veces: toma una submuestra de `n` pacientes de `datos`, ajusta un
#' Cox con los 7 predictores del modelo + dm + ecv + `ruidosas` variables de puro ruido (candidatos "hambrientos"),
#' y mide el rendimiento (a) en la propia submuestra (aparente), (b) con bootstrap (opcional) y (c) en un
#' "oráculo" grande de la misma población (la verdad).
#' @return tibble con una fila por repetición: eventos, parametros, c_aparente, c_bootstrap, c_oraculo, pendiente_oraculo
experimento_sobreajuste <- function(datos, oraculo, n, ruidosas = 6, R = 50, semilla = 1, bootstrap = FALSE, B = 40) {
  base <- c("edad", "sexo", "tfg", "lu", "albumina", "fosforo", "bicarbonato", "dm", "ecv")
  ruido <- paste0("ruido", seq_len(ruidosas))
  fml <- stats::reformulate(c(base, ruido), response = "survival::Surv(tiempo_meses, krt)")
  set.seed(semilla)
  oraculo <- oraculo[seq_len(min(nrow(oraculo), 20000)), ]
  for (v in ruido) oraculo[[v]] <- stats::rnorm(nrow(oraculo))
  dplyr::bind_rows(lapply(seq_len(R), function(r) {
    d <- datos[sample(nrow(datos), n), ]
    for (v in ruido) d[[v]] <- stats::rnorm(n)
    if (bootstrap) {
      ajuste <- rms::cph(fml, data = d, x = TRUE, y = TRUE)
      vb <- rms::validate(ajuste, B = B)
      c_boot <- (vb["Dxy", "index.corrected"] + 1) / 2
      m <- survival::coxph(fml, data = d)
    } else {
      m <- survival::coxph(fml, data = d); c_boot <- NA_real_
    }
    lp_o <- stats::predict(m, newdata = oraculo, type = "lp")
    tibble::tibble(eventos = sum(d$krt), parametros = length(stats::coef(m)),
                   c_aparente = unname(summary(m)$concordance[1]), c_bootstrap = c_boot,
                   c_oraculo = unname(c_harrell(lp_o, oraculo$tiempo_meses, oraculo$krt)["C"]),
                   pendiente_oraculo = unname(stats::coef(survival::coxph(survival::Surv(tiempo_meses, krt) ~ lp_o, data = oraculo))))
  }))
}

# ---- 17.6 Un "mundo" donde el aprendizaje automático sí puede aportar ------------

#' Pacientes en hemodiálisis SIMULADOS (hipotéticos) con un riesgo de muerte a 1 año de forma NO lineal.
#' Verdad: el riesgo sube en U con el IMC, se dispara solo cuando coinciden edad > 75 años y albúmina < 3.5 g/dL
#' (interacción) y crece de forma lineal con la PCR. Es un escenario didáctico, no un hallazgo clínico.
simular_mundo_no_lineal <- function(n, semilla = 1) {
  set.seed(semilla)
  d <- tibble::tibble(
    imc = stats::rnorm(n, 27, 5), edad = stats::rnorm(n, 65, 12), albumina = stats::rnorm(n, 3.7, 0.5),
    pcr = stats::rnorm(n, 0, 1), hb = stats::rnorm(n, 10.5, 1.3), sexo = stats::rbinom(n, 1, 0.5))
  lp <- -2.2 + 0.035 * (d$imc - 27)^2 + 1.6 * (d$edad > 75 & d$albumina < 3.5) + 0.45 * d$pcr
  d$muerte_1a <- stats::rbinom(n, 1, stats::plogis(lp))
  d$riesgo_verdadero <- stats::plogis(lp)
  d
}

# ---- El modelo del Capítulo 17 (empaquetado para reutilizarlo en 17.3-17.7) ---------

#' Ajusta los dos modelos de Cox por causa que se desarrollan en 17.2:
#'   - krt: falla renal (diálisis o trasplante), con 8 predictores preespecificados (los 4 de KFRE,
#'     tres laboratorios y diabetes);
#'   - muerte: el evento competidor, con los predictores clínicos de mortalidad.
#' Juntos permiten calcular el riesgo absoluto real (incidencia acumulada) con cif_cox().
#' `model = TRUE` guarda los datos dentro del objeto para poder calcular riesgos basales después.
ajustar_modelos_pronostico <- function(datos) {
  list(
    krt = survival::coxph(survival::Surv(tiempo_meses, krt) ~ edad + sexo + tfg + lu + albumina + fosforo + bicarbonato + dm,
                          data = datos, model = TRUE, x = TRUE, y = TRUE),
    muerte = survival::coxph(survival::Surv(tiempo_meses, muerte) ~ edad + sexo + tfg + dm + ecv + albumina,
                             data = datos, model = TRUE, x = TRUE, y = TRUE)
  )
}

#' Cuando la muestra YA está dada: máximo de parámetros que se pueden estimar sin pasarse de sobreajuste.
#' Invierte los criterios 1 y 2 de tamano_muestra_riley() (el criterio 3 no depende del número de parámetros).
#' @param n pacientes disponibles; @param r2_cs R2 de Cox-Snell esperado; @param r2_max R2 de Cox-Snell máximo posible
parametros_maximos <- function(n, r2_cs, r2_max, shrinkage = 0.9, delta = 0.05) {
  p_para <- function(s) n * (s - 1) * log(1 - r2_cs / s)
  s2 <- r2_cs / (r2_cs + delta * r2_max)
  floor(min(p_para(shrinkage), p_para(s2)))
}

#' R2 de Cox-Snell máximo posible según el desenlace (depende del riesgo basal; ver Riley et al.)
r2_cs_maximo <- function(tipo = c("binario", "supervivencia"), prevalencia = NULL, eventos_por_persona = NULL) {
  tipo <- match.arg(tipo)
  e <- if (tipo == "binario") prevalencia else eventos_por_persona
  lnl0 <- if (tipo == "binario") e * log(e) + (1 - e) * log(1 - e) else e * log(e) - e
  1 - exp(2 * lnl0)
}
