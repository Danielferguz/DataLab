# ------------------------------------------------------------------------
# Funciones de apoyo del libro (sin dependencias fuera de tidyverse/base)
# Uso en un capítulo:   source("R/funciones_epi.R")
# ------------------------------------------------------------------------

#' Medidas de asociación desde una tabla 2x2
#'
#' @param a expuestos con evento       @param b expuestos sin evento
#' @param c no expuestos con evento    @param d no expuestos sin evento
#' @param nivel nivel de confianza (por defecto 0.95)
#' @return tibble con riesgos, RD, RR y OR con IC (método de Wald en log)
medidas_2x2 <- function(a, b, c, d, nivel = 0.95) {
  z  <- stats::qnorm(1 - (1 - nivel) / 2)
  n1 <- a + b; n0 <- c + d
  r1 <- a / n1; r0 <- c / n0
  rr <- r1 / r0;       se_rr <- sqrt(1/a - 1/n1 + 1/c - 1/n0)
  or <- (a * d) / (b * c); se_or <- sqrt(1/a + 1/b + 1/c + 1/d)
  rd <- r1 - r0;       se_rd <- sqrt(r1 * (1 - r1) / n1 + r0 * (1 - r0) / n0)
  tibble::tibble(
    medida   = c("Proporción en expuestos", "Proporción en no expuestos",
                 "Diferencia (RD)", "Razón de proporciones (RR o RP)", "Odds ratio (OR)"),
    estimado = c(r1, r0, rd, rr, or),
    ic_inf   = c(NA, NA, rd - z * se_rd, exp(log(rr) - z * se_rr), exp(log(or) - z * se_or)),
    ic_sup   = c(NA, NA, rd + z * se_rd, exp(log(rr) + z * se_rr), exp(log(or) + z * se_or))
  )
}

#' Número necesario a tratar (o a dañar) con su IC, a partir de la diferencia de riesgos
#' @param rd diferencia de riesgos (riesgo tratados - riesgo controles)
#' @param rd_inf,rd_sup límites del IC de la diferencia de riesgos
nnt_desde_rd <- function(rd, rd_inf = NA, rd_sup = NA) {
  tibble::tibble(
    rd = rd, nnt = 1 / abs(rd),
    tipo = dplyr::if_else(rd < 0, "NNT (el tratamiento reduce el evento)",
                          "NND (el tratamiento aumenta el evento)"),
    # Si el IC de la RD incluye 0, el IC del NNT NO es un intervalo continuo
    ic_incluye_cero = !is.na(rd_inf) & !is.na(rd_sup) & rd_inf < 0 & rd_sup > 0,
    nnt_ic_inf = if (is.na(rd_inf)) NA_real_ else 1 / max(abs(rd_inf), abs(rd_sup)),
    nnt_ic_sup = if (is.na(rd_inf)) NA_real_ else 1 / min(abs(rd_inf), abs(rd_sup))
  )
}

#' Diferencia de medias estandarizada (SMD) entre dos grupos, con pesos opcionales
#' @param x variable numérica o binaria (0/1)   @param g tratamiento 0/1
#' @param w pesos (por defecto 1)
smd <- function(x, g, w = rep(1, length(x))) {
  m <- function(v, wt) stats::weighted.mean(v, wt)
  v <- function(v, wt) { mu <- m(v, wt); sum(wt * (v - mu)^2) / sum(wt) }
  m1 <- m(x[g == 1], w[g == 1]); m0 <- m(x[g == 0], w[g == 0])
  s  <- sqrt((v(x[g == 1], w[g == 1]) + v(x[g == 0], w[g == 0])) / 2)
  (m1 - m0) / s
}

#' E-value (VanderWeele & Ding) para una razón de riesgos / hazard / odds (evento raro)
#' @param rr estimación puntual (si < 1 se invierte)   @param lim límite del IC más cercano a 1
e_value <- function(rr, lim = NA) {
  f <- function(x) { x <- ifelse(x < 1, 1 / x, x); x + sqrt(x * (x - 1)) }
  tibble::tibble(
    e_value_punto = f(rr),
    e_value_ic    = if (is.na(lim)) NA_real_ else if ((rr < 1 && lim >= 1) || (rr > 1 && lim <= 1)) 1 else f(lim)
  )
}

#' Dibuja un DAG sencillo solo con ggplot2 (sin ggdag)
#' @param nodos tibble con columnas: nombre, x, y, etiqueta, rol
#'   rol in {"exposicion","desenlace","confusor","mediador","colisionador","otro","no medido"}
#' @param aristas tibble con columnas: de, a  (nombres de nodos)
#' @param encoger fracción del segmento que se recorta en cada extremo
#' @param expandir margen relativo en x e y (aumentar si las etiquetas se cortan)
dibujar_dag <- function(nodos, aristas, encoger = 0.14, tam_texto = 3.6, expandir = c(0.12, 0.18)) {
  colores <- c(exposicion = "#0072B2", desenlace = "#D55E00", confusor = "#E69F00",
               mediador = "#009E73", colisionador = "#CC79A7", otro = "grey70",
               `no medido` = "grey90")
  etiquetas_rol <- c(exposicion = "Exposición", desenlace = "Desenlace", confusor = "Confusor",
                     mediador = "Mediador", colisionador = "Colisionador", otro = "Otra variable",
                     `no medido` = "No medido")
  seg <- aristas |>
    dplyr::left_join(nodos |> dplyr::select(de = nombre, x0 = x, y0 = y), by = "de") |>
    dplyr::left_join(nodos |> dplyr::select(a = nombre, x1 = x, y1 = y), by = "a") |>
    dplyr::mutate(dx = x1 - x0, dy = y1 - y0,
                  xi = x0 + encoger * dx, yi = y0 + encoger * dy,
                  xf = x1 - encoger * dx, yf = y1 - encoger * dy)
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = seg, ggplot2::aes(x = xi, y = yi, xend = xf, yend = yf),
                          arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed"),
                          color = "grey30", linewidth = 0.6) +
    ggplot2::geom_label(data = nodos, ggplot2::aes(x = x, y = y, label = etiqueta, fill = rol),
                        size = tam_texto, label.padding = grid::unit(0.3, "lines")) +
    ggplot2::scale_fill_manual(values = colores, labels = etiquetas_rol, name = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = expandir[1])) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = expandir[2])) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(label = ""))) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------
# Funciones nefrológicas (Cap. 5)
# ------------------------------------------------------------------------

#' Limpia nombres de columnas: minúsculas, sin tildes, sin símbolos, con guion bajo
limpiar_nombres <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    gsub(pattern = "[^a-z0-9]+", replacement = "_") |>
    gsub(pattern = "^_|_$", replacement = "")
}

#' Conversión de creatinina sérica (1 mg/dL = 88.4 µmol/L)
umol_a_mgdl <- function(x) x / 88.4
mgdl_a_umol <- function(x) x * 88.4

#' TFGe por CKD-EPI 2021 con creatinina (sin raza), mL/min/1.73 m2
#' @param creat_mgdl creatinina sérica en mg/dL
#' @param edad años   @param sexo "F" o "M"
ckd_epi_2021 <- function(creat_mgdl, edad, sexo) {
  mujer  <- sexo == "F"
  kappa  <- ifelse(mujer, 0.7, 0.9)
  alfa   <- ifelse(mujer, -0.241, -0.302)
  cociente <- creat_mgdl / kappa
  142 * pmin(cociente, 1)^alfa * pmax(cociente, 1)^-1.200 * 0.9938^edad * ifelse(mujer, 1.012, 1)
}

#' Categoría G de KDIGO según TFGe
categoria_g <- function(tfg) {
  cut(tfg, breaks = c(-Inf, 15, 30, 45, 60, 90, Inf), right = FALSE,
      labels = c("G5", "G4", "G3b", "G3a", "G2", "G1"), ordered_result = TRUE) |>
    factor(levels = c("G1", "G2", "G3a", "G3b", "G4", "G5"), ordered = TRUE)
}

#' Categoría A de KDIGO según UACR (mg/g)
categoria_a <- function(uacr) {
  cut(uacr, breaks = c(-Inf, 30, 300, Inf), right = FALSE,
      labels = c("A1", "A2", "A3"), ordered_result = TRUE)
}

#' Riesgo KDIGO (mapa de calor): bajo, moderado, alto, muy alto
riesgo_kdigo <- function(g, a) {
  tabla <- rbind(
    G1  = c("Bajo", "Moderado", "Alto"),
    G2  = c("Bajo", "Moderado", "Alto"),
    G3a = c("Moderado", "Alto", "Muy alto"),
    G3b = c("Alto", "Muy alto", "Muy alto"),
    G4  = c("Muy alto", "Muy alto", "Muy alto"),
    G5  = c("Muy alto", "Muy alto", "Muy alto"))
  colnames(tabla) <- c("A1", "A2", "A3")
  out <- rep(NA_character_, length(g))
  ok <- !is.na(g) & !is.na(a)
  out[ok] <- tabla[cbind(as.character(g[ok]), as.character(a[ok]))]
  factor(out, levels = c("Bajo", "Moderado", "Alto", "Muy alto"), ordered = TRUE)
}
