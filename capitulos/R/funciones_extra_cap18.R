# ------------------------------------------------------------------------
# Funciones extra del Capítulo 18 (Análisis de mediación)
#   source("R/funciones_extra_cap18.R")
#
# Alternativas propias, ejecutables aquí, a los paquetes `mediation` y `CMAverse`:
#  1) verdad_mediacion(), verdad_mediacion_riesgo(), verdad_mediacion_tiempo():
#     efectos verdaderos a partir del oráculo (resultados potenciales de cada paciente).
#  2) ajustar_mediacion(): método de productos y de diferencias (para boot()).
#  3) efectos_naturales() + descomponer(): fórmula g / simulación; sirve con
#     interacción y con desenlaces binarios.
#  4) sensibilidad_rho(): análisis de sensibilidad al confusor mediador-desenlace
#     no medido (correlación de errores rho, modelos lineales).
#  5) rho_implicito(): traduce "fuerza" clínica de un confusor a rho.
# Requiere las bases generadas con R/simular_mediacion.R.
# ------------------------------------------------------------------------

#' Carga una base de mediación con su oráculo
#' @param version "" (base), "_int" (con interacción) o "_u" (confusor mediador-desenlace no medido)
cargar_mediacion <- function(version = "", carpeta = "Bases") {
  readr::read_csv(file.path(carpeta, paste0("ckd_mediacion", version, "_oraculo.csv")), show_col_types = FALSE) |>
    dplyr::mutate(sexo = factor(sexo, levels = c("Mujer", "Hombre")), luacr = log(pmax(uacr_basal, 1)))
}

#' Lo que ve el investigador (sin oráculo), con ln(UACR) basal
observar_mediacion <- function(o) {
  dplyr::select(o, id:evento, luacr)
}

#' Prepara una base recién simulada con simular_mediacion() para analizarla (sexo como factor, ln UACR)
preparar_mediacion <- function(d) {
  dplyr::mutate(d, sexo = factor(sexo, levels = c("Mujer", "Hombre")), luacr = log(pmax(uacr_basal, 1)))
}

#' Covariables basales (los confusores medidos "C") como términos de fórmula
covariables_med <- c("I(edad / 10)", "sexo", "hba1c", "pas", "tfg_basal", "luacr", "ecv")

# ---- 1. La verdad, a partir del oráculo -------------------------------------

#' Efectos verdaderos en la escala de la pendiente (diferencias de medias)
#' TE = total; NDE = directo natural [Y(1,M0)-Y(0,M0)]; NIE = indirecto natural [Y(1,M1)-Y(1,M0)]
#' NDE_alt/NIE_alt = la otra descomposición (con A = 0 como referencia del indirecto)
#' CDE = directo controlado con m = 0; INTref, INTmed, PIE = resto de la descomposición en 4 vías
verdad_mediacion <- function(o) {
  te <- mean(o$y11 - o$y00); nde <- mean(o$y10 - o$y00); nie <- mean(o$y11 - o$y10)
  cde <- mean(o$y1_m0 - o$y0_m0)
  c(TE = te, NDE = nde, NIE = nie, NDE_alt = mean(o$y11 - o$y01), NIE_alt = mean(o$y01 - o$y00),
    CDE = cde, INTref = nde - cde, INTmed = mean(o$y11 - o$y10 - o$y01 + o$y00), PIE = mean(o$y01 - o$y00),
    PM = nie / te)
}

#' Efectos verdaderos para el evento binario a 36 meses (riesgos, DR y RR)
verdad_mediacion_riesgo <- function(o) {
  r <- c(r00 = mean(o$ev00), r11 = mean(o$ev11), r10 = mean(o$ev10), r01 = mean(o$ev01))
  c(r, RR_total = r[["r11"]] / r[["r00"]], RR_directo = r[["r10"]] / r[["r00"]], RR_indirecto = r[["r11"]] / r[["r10"]],
    DR_total = r[["r11"]] - r[["r00"]], DR_directo = r[["r10"]] - r[["r00"]], DR_indirecto = r[["r11"]] - r[["r10"]])
}

#' Efectos verdaderos en escala log-tiempo (razón de tiempos; modelos AFT)
verdad_mediacion_tiempo <- function(o) {
  c(total = mean(log(o$t11 / o$t00)), directo = mean(log(o$t10 / o$t00)), indirecto = mean(log(o$t11 / o$t10)))
}

# ---- 2. Método de productos y de diferencias ------------------------------

#' Dos modelos lineales (mediador y desenlace) y los efectos por productos y por diferencias.
#' Pensada para boot(): ajustar_mediacion(datos, i, ...)
#' @param cov vector con los términos de las covariables (confusores basales)
ajustar_mediacion <- function(datos, i = seq_len(nrow(datos)), cov = covariables_med,
                              trat = "isglt2", med = "delta_lnuacr", y = "pendiente_tfg") {
  d <- datos[i, ]
  m_tot <- stats::lm(stats::reformulate(c(trat, cov), y), data = d)          # efecto total (sin el mediador)
  m_med <- stats::lm(stats::reformulate(c(trat, cov), med), data = d)        # modelo del mediador
  m_out <- stats::lm(stats::reformulate(c(trat, med, cov), y), data = d)     # modelo del desenlace
  te <- unname(stats::coef(m_tot)[trat]); a <- unname(stats::coef(m_med)[trat])
  b <- unname(stats::coef(m_out)[med]);   nde <- unname(stats::coef(m_out)[trat])
  c(TE = te, a = a, b = b, NDE = nde, NIE_producto = a * b, NIE_diferencia = te - nde, PM = a * b / te)
}

# ---- 3. Fórmula g / simulación ---------------------------------------------

#' Promedios de los resultados potenciales Y(a, M(a')) a partir de dos modelos ajustados.
#' Para cada paciente: (1) predice su mediador bajo a' (más ruido con la desviación residual),
#' (2) predice el desenlace fijando el tratamiento en a y el mediador en ese valor, (3) promedia.
#' @param mod_m modelo lineal del mediador; mod_y modelo del desenlace (lm o glm)
#' @param mval valor del mediador para el efecto directo controlado (CDE)
efectos_naturales <- function(datos, mod_m, mod_y, trat = "isglt2", med = "delta_lnuacr",
                              sims = 30, semilla = 1, mval = 0) {
  n <- nrow(datos)
  lineal <- inherits(mod_y, "lm") && !inherits(mod_y, "glm")      # lineal en m: basta el valor esperado
  if (lineal) sims <- 1
  sig <- if (lineal) 0 else stats::sigma(mod_m)
  poner <- function(a) { d <- datos; d[[trat]] <- a; d }
  mu0 <- stats::predict(mod_m, newdata = poner(0)); mu1 <- stats::predict(mod_m, newdata = poner(1))
  set.seed(semilla)
  ruido <- matrix(stats::rnorm(n * sims, 0, sig), n, sims)         # mismo ruido en los dos mundos
  ey <- function(a, mu) {
    mean(vapply(seq_len(sims), function(s) {
      d <- poner(a); d[[med]] <- mu + ruido[, s]
      mean(stats::predict(mod_y, newdata = d, type = "response"))
    }, numeric(1)))
  }
  ey_fijo <- function(a) { d <- poner(a); d[[med]] <- mval; mean(stats::predict(mod_y, newdata = d, type = "response")) }
  c(y00 = ey(0, mu0), y11 = ey(1, mu1), y10 = ey(1, mu0), y01 = ey(0, mu1),
    y1_m = ey_fijo(1), y0_m = ey_fijo(0))
}

#' Convierte los promedios de efectos_naturales() en efectos (diferencia o razón)
descomponer <- function(medias, escala = c("diferencia", "razon")) {
  escala <- match.arg(escala)
  m <- as.list(medias)
  if (escala == "diferencia") {
    te <- m$y11 - m$y00; nde <- m$y10 - m$y00; nie <- m$y11 - m$y10; cde <- m$y1_m - m$y0_m
    c(TE = te, NDE = nde, NIE = nie, NDE_alt = m$y11 - m$y01, NIE_alt = m$y01 - m$y00,
      CDE = cde, INTref = nde - cde, INTmed = m$y11 - m$y10 - m$y01 + m$y00, PIE = m$y01 - m$y00, PM = nie / te)
  } else {
    c(RR_total = m$y11 / m$y00, RR_directo = m$y10 / m$y00, RR_indirecto = m$y11 / m$y10,
      PM_log = log(m$y11 / m$y10) / log(m$y11 / m$y00))
  }
}

# ---- 4. Sensibilidad al confusor mediador-desenlace no medido --------------

#' Efecto indirecto (por productos) si los errores del modelo del mediador y del desenlace
#' estuvieran correlacionados con coeficiente rho (modelos lineales).
#' Idea: el coeficiente b de OLS estima b + rho * sd_y / sd_m, donde sd_y es la desviación del
#' error "estructural" del desenlace: sd_y = sd_residual / sqrt(1 - rho^2).
#' @return tibble con rho, NIE(rho), NDE(rho) y PM(rho)
sensibilidad_rho <- function(datos, rho = round(seq(-0.6, 0.6, by = 0.05), 2), cov = covariables_med,
                             trat = "isglt2", med = "delta_lnuacr", y = "pendiente_tfg") {
  m_tot <- stats::lm(stats::reformulate(c(trat, cov), y), data = datos)
  m_med <- stats::lm(stats::reformulate(c(trat, cov), med), data = datos)
  m_out <- stats::lm(stats::reformulate(c(trat, med, cov), y), data = datos)
  te <- unname(stats::coef(m_tot)[trat]); a <- unname(stats::coef(m_med)[trat]); b <- unname(stats::coef(m_out)[med])
  sd_m <- stats::sigma(m_med); sd_y <- stats::sigma(m_out)
  b_rho <- b - rho * sd_y / (sd_m * sqrt(1 - rho^2))
  tibble::tibble(rho = rho, NIE = a * b_rho, NDE = te - a * b_rho, PM = a * b_rho / te)
}

#' Valor de rho que anularía el efecto indirecto (b = 0)
rho_anula_nie <- function(datos, cov = covariables_med, trat = "isglt2", med = "delta_lnuacr", y = "pendiente_tfg") {
  m_med <- stats::lm(stats::reformulate(c(trat, cov), med), data = datos)
  m_out <- stats::lm(stats::reformulate(c(trat, med, cov), y), data = datos)
  r <- unname(stats::coef(m_out)[med]) * stats::sigma(m_med) / stats::sigma(m_out)
  r / sqrt(1 + r^2)
}

#' rho implícito de un confusor U (estandarizado, SD = 1) que mueve el mediador en `efecto_m`
#' unidades y el desenlace en `efecto_y` unidades por cada SD de U, con ruido propio sd_m y sd_y
rho_implicito <- function(efecto_m, efecto_y, sd_m, sd_y) {
  (efecto_m * efecto_y) / sqrt((efecto_m^2 + sd_m^2) * (efecto_y^2 + sd_y^2))
}
