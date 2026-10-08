# ------------------------------------------------------------------------
# Simulaciones de DAG con verdad conocida (Capítulo 3.5)
#   source("R/funciones_dag_sim.R")      # requiere tidyverse cargado
#
# simular_dag()        : genera datos simulados de un DAG pequeño; la verdad
#                        (efecto total de X sobre Y) viaja en attr(, "verdad").
# comparar_ajustes()   : estima el efecto de X sobre Y sin ajustar y ajustando
#                        por cada variable candidata (y restringiendo, si se pide).
# replicar_ajustes()   : repite lo anterior en muchas simulaciones y resume sesgo
#                        medio y variabilidad.
# cambio_en_estimacion(): criterio del "cambio en la estimación" (> 10 %).
#
# Escenarios (X = iSGLT2, Y = pendiente de TFGe en mL/min/1.73 m2/año; todo simulado):
#   "confusor"    uacr_basal -> X, uacr_basal -> Y
#   "mediador"    X -> uacr_6m -> Y  (más un efecto directo)
#   "colisionador" X -> hosp <- Y
#   "sesgo_z"     z_centro -> X, u_fragilidad -> X, u_fragilidad -> Y (U no medida)
#   "solo_y"      pas_basal -> Y (X aleatorizado)
# ------------------------------------------------------------------------

#' Simula un DAG con verdad conocida
#' @param tipo uno de "confusor", "mediador", "colisionador", "sesgo_z", "solo_y"
#' @param n tamaño de muestra
#' @param efecto efecto total verdadero de X sobre Y. Continuo: mL/min/año
#'   (positivo = beneficio). Binario: log-OR condicional (negativo = protector).
#' @param binario si TRUE, Y es un evento 0/1 (solo "confusor" y "solo_y")
#' @param prop_directo en "mediador": fracción del efecto total que NO pasa por el mediador
#' @return tibble con x, y y las demás variables; atributos "verdad", "verdad_directo",
#'   "no_medidos" y "binario"
simular_dag <- function(tipo = c("confusor", "mediador", "colisionador", "sesgo_z", "solo_y"),
                        n = 2000, efecto = if (binario) log(0.7) else 1,
                        binario = FALSE, prop_directo = 0.4) {
  tipo <- match.arg(tipo)
  if (binario && !tipo %in% c("confusor", "solo_y"))
    stop("La versión binaria solo está disponible para 'confusor' y 'solo_y'.")
  dano <- if (binario) 1 else -1          # signo de "más de esto = peor desenlace"
  base <- if (binario) -1 else -2         # nivel basal del desenlace
  # Desenlace a partir de un predictor lineal: evento 0/1 o continuo con ruido N(0, 1)
  generar_y <- function(lp) if (binario) rbinom(n, 1, plogis(lp)) else lp + rnorm(n)
  no_medidos <- character(); directo <- efecto

  if (tipo == "confusor") {
    uacr_basal <- rnorm(n)                                          # log-UACR basal (estandarizada)
    x <- rbinom(n, 1, plogis(-0.3 + 0.9 * uacr_basal))              # más albuminuria => más probabilidad de recibir iSGLT2
    y <- generar_y(base + efecto * x + dano * 0.8 * uacr_basal)     # y a más albuminuria, peor pendiente
    d <- tibble(x, y, uacr_basal)
  }
  if (tipo == "mediador") {
    x <- rbinom(n, 1, 0.5)                                          # sin confusión: así aislamos el mediador
    uacr_6m <- -1 * x + rnorm(n)                                    # el fármaco reduce la albuminuria
    directo <- prop_directo * efecto
    b <- -(1 - prop_directo) * efecto                               # efecto de la albuminuria sobre la pendiente
    y <- generar_y(base + directo * x + b * uacr_6m)                # total = directo + (-1)(b)
    d <- tibble(x, y, uacr_6m)
  }
  if (tipo == "colisionador") {
    x <- rbinom(n, 1, 0.5)
    y <- generar_y(base + efecto * x)
    hosp <- rbinom(n, 1, plogis(-0.5 - 1.5 * x - 1.5 * (y - base)))  # la reducen el fármaco y una buena pendiente
    d <- tibble(x, y, hosp)
  }
  if (tipo == "sesgo_z") {
    u_fragilidad <- rnorm(n); z_centro <- rnorm(n)                  # U no se mide; Z = tendencia del centro a prescribir
    x <- rbinom(n, 1, plogis(-0.2 + 1.5 * z_centro + 0.8 * u_fragilidad))
    y <- generar_y(base + efecto * x + dano * 1.0 * u_fragilidad)
    d <- tibble(x, y, z_centro, u_fragilidad); no_medidos <- "u_fragilidad"
  }
  if (tipo == "solo_y") {
    x <- rbinom(n, 1, 0.5)                                          # aleatorizado
    pas_basal <- rnorm(n)
    y <- generar_y(base + efecto * x + dano * (if (binario) 1.5 else 1) * pas_basal)
    d <- tibble(x, y, pas_basal)
  }
  structure(d, verdad = efecto, verdad_directo = directo, no_medidos = no_medidos,
            binario = binario, tipo = tipo)
}

#' Efecto de X sobre Y sin ajustar y ajustando por cada candidata
#' @param d datos de simular_dag() (o cualquier tibble con x e y)
#' @param candidatas variables por las que ajustar, de una en una (por defecto, todas las medidas)
#' @param restringir nombre de una variable 0/1: añade el análisis restringido a ese valor = 1
#' @param verdad efecto total verdadero (por defecto, el del atributo)
#' @return tibble: ajuste, conjunto, estimado, ee, ic_inf, ic_sup, verdad, sesgo
comparar_ajustes <- function(d, candidatas = NULL, restringir = NULL, verdad = attr(d, "verdad")) {
  binario <- isTRUE(attr(d, "binario"))
  if (is.null(candidatas)) candidatas <- setdiff(names(d), c("x", "y", attr(d, "no_medidos")))
  ajustar <- function(vars, datos = d, etiqueta = NULL) {
    m  <- if (binario) glm(reformulate(c("x", vars), "y"), binomial(), datos)
          else lm(reformulate(c("x", vars), "y"), datos)
    co <- summary(m)$coefficients["x", ]
    if (is.null(etiqueta))
      etiqueta <- if (length(vars)) paste("Ajustando por", paste(vars, collapse = " + ")) else "Sin ajustar"
    tibble(ajuste = etiqueta,
           conjunto = if (length(vars)) paste(vars, collapse = "+") else "ninguno",
           estimado = co[[1]], ee = co[[2]])
  }
  res <- bind_rows(
    ajustar(character()),
    map(candidatas, \(v) ajustar(v)) |> list_rbind(),
    if (length(candidatas) > 1) ajustar(candidatas, etiqueta = "Ajustando por todas"),
    if (!is.null(restringir))
      ajustar(character(), datos = filter(d, .data[[restringir]] == 1),
              etiqueta = paste0("Restringido a ", restringir, " = 1")) |> mutate(conjunto = paste0("restringido_", restringir))
  )
  res |> mutate(ic_inf = estimado - 1.96 * ee, ic_sup = estimado + 1.96 * ee,
                verdad = verdad, sesgo = estimado - verdad)
}

#' Repite simular_dag() + comparar_ajustes() y resume
#' @param reps número de simulaciones   @param ... argumentos de simular_dag()
#' @return tibble: ajuste, conjunto, estimado (media), sesgo (medio), ee_empirico (DE de las
#'   estimaciones entre simulaciones), ee_modelo (media del error estándar), verdad
replicar_ajustes <- function(tipo, reps = 200, candidatas = NULL, restringir = NULL, ...) {
  map(seq_len(reps), \(i) comparar_ajustes(simular_dag(tipo, ...), candidatas, restringir)) |>
    list_rbind() |>
    group_by(ajuste, conjunto, verdad) |>
    summarise(sesgo = mean(sesgo), ee_empirico = sd(estimado), ee_modelo = mean(ee),
              estimado = mean(estimado), .groups = "drop") |>
    arrange(conjunto != "ninguno")
}

#' Criterio del cambio en la estimación: ¿cambia el coeficiente de X más de `umbral` al añadir la variable?
#' @return tibble: variable, sin_v, con_v, cambio_rel, regla ("incluir" si |cambio| > umbral)
cambio_en_estimacion <- function(d, variables, umbral = 0.10) {
  binario <- isTRUE(attr(d, "binario"))
  b <- function(vars) {
    f <- reformulate(c("x", vars), "y")
    coef(if (binario) glm(f, binomial(), d) else lm(f, d))[["x"]]
  }
  tibble(variable = variables, sin_v = b(character()),
         con_v = map_dbl(variables, \(v) b(v))) |>
    mutate(cambio_rel = (con_v - sin_v) / abs(sin_v),
           regla = if_else(abs(cambio_rel) > umbral, "incluir", "descartar"))
}
