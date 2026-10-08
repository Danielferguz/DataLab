# ------------------------------------------------------------------------
# Funciones de apoyo del Capítulo 15 (series de tiempo y datos geoespaciales)
# Uso en un capítulo:   source("R/funciones_extra_cap15.R")
# Requiere tidyverse cargado; sf y spdep solo para las funciones espaciales.
# ------------------------------------------------------------------------

# ---- Series de tiempo --------------------------------------------------

#' Añade a la serie las variables habituales de una serie interrumpida
#'
#' Espera las columnas `mes_num` (1, 2, 3, ...), `mes` (1-12), `casos` y `poblacion`.
#'   tiempo   : meses desde el inicio (0, 1, 2, ...)
#'   post     : 1 desde el primer mes de la política (calendario, para ambas regiones)
#'   t_post   : meses desde el inicio de la política (0 en el primer mes de la política y antes)
#'   s1, c1   : primer par de armónicos (seno y coseno) de la estacionalidad anual
#'   tasa     : casos por 100 000 habitantes y por mes;  ltasa = log(tasa)
preparar_serie <- function(datos, mes_politica = 49) {
  datos |>
    dplyr::mutate(tiempo = mes_num - 1,
                  post = as.integer(mes_num >= mes_politica),
                  t_post = pmax(0, mes_num - mes_politica),
                  s1 = sin(2 * pi * mes / 12), c1 = cos(2 * pi * mes / 12),
                  tasa = casos / poblacion * 1e5, ltasa = log(tasa))
}

#' Amplitud y mes del pico de una estacionalidad armónica
#'
#' Si el modelo tiene `b_sen * sin(2 pi mes / 12) + b_cos * cos(2 pi mes / 12)`, la curva
#' equivale a `A * cos(2 pi (mes - mes_pico) / 12)`.
amplitud_pico <- function(b_sen, b_cos) {
  c(amplitud = unname(sqrt(b_sen^2 + b_cos^2)),
    mes_pico = unname((atan2(b_sen, b_cos) * 12 / (2 * pi)) %% 12))
}

#' Combinación lineal de coeficientes (p. ej., el efecto k meses después del inicio)
#'
#' @param modelo glm/lm ajustado
#' @param pesos vector con nombre: coeficientes y su multiplicador (p. ej., c(post = 1, t_post = 12))
#' @param vcov. matriz de covarianzas a usar (por defecto, la del modelo; puede ser robusta)
#' @param exponenciar TRUE devuelve razones (RR); FALSE, la combinación en escala log
comb_lineal <- function(modelo, pesos, vcov. = stats::vcov(modelo), exponenciar = TRUE, nivel = 0.95) {
  nombres <- names(pesos)
  b <- stats::coef(modelo)[nombres]
  est <- sum(b * pesos)
  ee <- sqrt(as.numeric(t(pesos) %*% vcov.[nombres, nombres] %*% pesos))
  z <- stats::qnorm(1 - (1 - nivel) / 2)
  r <- c(estimado = est, inf = est - z * ee, sup = est + z * ee)
  if (exponenciar) exp(r) else r
}

#' Gráfico de autocorrelación (ACF) o autocorrelación parcial (PACF) con ggplot
ggacf <- function(x, lag_max = 24, parcial = FALSE, titulo = NULL, nivel = 0.95) {
  a <- if (parcial) stats::pacf(x, lag.max = lag_max, plot = FALSE, na.action = stats::na.pass)
       else stats::acf(x, lag.max = lag_max, plot = FALSE, na.action = stats::na.pass)
  d <- tibble::tibble(rezago = as.numeric(a$lag), r = as.numeric(a$acf))
  d <- dplyr::filter(d, rezago > 0)
  lim <- stats::qnorm(1 - (1 - nivel) / 2) / sqrt(sum(!is.na(x)))
  ggplot2::ggplot(d, ggplot2::aes(rezago, r)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::geom_hline(yintercept = c(-lim, lim), linetype = "dashed", colour = "#0072B2") +
    ggplot2::geom_segment(ggplot2::aes(xend = rezago, yend = 0), linewidth = 0.8) +
    ggplot2::labs(x = "Rezago (meses)", y = if (parcial) "PACF" else "ACF", title = titulo) +
    ggplot2::theme_minimal(base_size = 11)
}

# ---- Datos espaciales --------------------------------------------------

#' Mapa coropletico de una variable numérica (clases), con ggplot2 + sf
#'
#' @param cortes límites de las clases; si es NULL se usan cuantiles de la propia variable.
#'   Para comparar varios mapas, pasa los mismos `cortes` a todos.
mapa_coropletico <- function(mapa, var, titulo = NULL, leyenda = NULL, n_clases = 5, paleta = "YlOrRd", cortes = NULL) {
  v <- mapa[[var]]
  if (is.null(cortes)) cortes <- unique(stats::quantile(v, seq(0, 1, length.out = n_clases + 1), na.rm = TRUE))
  cortes[1] <- min(cortes[1], min(v, na.rm = TRUE)); cortes[length(cortes)] <- max(cortes[length(cortes)], max(v, na.rm = TRUE))
  mapa$.clase <- cut(v, cortes, include.lowest = TRUE, dig.lab = 3)
  ggplot2::ggplot(mapa) +
    ggplot2::geom_sf(ggplot2::aes(fill = .clase), colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_brewer(palette = paleta, name = leyenda %||% var, drop = FALSE) +
    ggplot2::labs(title = titulo) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "right")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Diagrama de dispersión de Moran: valor estandarizado (eje x) vs. promedio de sus vecinos (eje y)
moran_scatter <- function(x, lw, etiquetas = NULL) {
  z <- as.numeric(scale(x))
  d <- tibble::tibble(z = z, retardo = spdep::lag.listw(lw, z), etiqueta = etiquetas)
  ggplot2::ggplot(d, ggplot2::aes(z, retardo)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") + ggplot2::geom_vline(xintercept = 0, colour = "grey60") +
    ggplot2::geom_point(colour = "#0072B2", alpha = 0.8) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "#D55E00", linewidth = 0.8, formula = y ~ x) +
    ggplot2::labs(x = "Valor estandarizado del distrito", y = "Promedio estandarizado de sus vecinos") +
    ggplot2::theme_minimal(base_size = 11)
}

#' Clasificación LISA (Moran local) en Alto-Alto, Bajo-Bajo, Alto-Bajo, Bajo-Alto o no significativo
#'
#' @param ajuste método de `p.adjust()` para corregir por las 60 pruebas simultáneas
#'   ("none" = sin corregir; "fdr" = tasa de falsos descubrimientos)
cuadrante_lisa <- function(x, lw, alfa = 0.05, ajuste = "none") {
  loc <- spdep::localmoran(x, lw)
  p <- stats::p.adjust(loc[, ncol(loc)], method = ajuste)   # última columna = valor p (bilateral)
  z <- as.numeric(scale(x)); retardo <- spdep::lag.listw(lw, z)
  out <- dplyr::case_when(p >= alfa ~ "No significativo",
                          z > 0 & retardo > 0 ~ "Alto-Alto",
                          z < 0 & retardo < 0 ~ "Bajo-Bajo",
                          z > 0 & retardo < 0 ~ "Alto-Bajo",
                          TRUE ~ "Bajo-Alto")
  factor(out, levels = c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No significativo"))
}

# ---- Evaluación de pronósticos -----------------------------------------

#' Medidas de exactitud de un pronóstico (RMSE, MAE, MAPE y MASE)
#'
#' @param pred pronóstico para el periodo de prueba
#' @param real valores observados en el periodo de prueba
#' @param entrenamiento serie usada para ajustar (sirve para el MASE)
#' @param m periodicidad (12 = mensual): el MASE compara con el error medio de
#'   repetir el mismo mes del año anterior dentro de la muestra de entrenamiento
#'   (igual que `forecast::accuracy()`). MASE < 1: mejor que ese pronóstico ingenuo.
exactitud <- function(pred, real, entrenamiento, m = 12) {
  e <- as.numeric(real) - as.numeric(pred)
  c(RMSE = sqrt(mean(e^2)), MAE = mean(abs(e)), MAPE = 100 * mean(abs(e / as.numeric(real))),
    MASE = mean(abs(e)) / mean(abs(diff(as.numeric(entrenamiento), lag = m))))
}
