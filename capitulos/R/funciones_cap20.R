# ------------------------------------------------------------------------
# Funciones del Capítulo 20 (cálculo del tamaño de muestra)
#   source("R/funciones_cap20.R")
# Solo requieren tidyverse y el paquete base `stats` (survival y lme4 se
# llaman con `::` únicamente en las funciones que los usan).
# Todos los escenarios son HIPOTÉTICOS o SIMULADOS.
#
#   20.1  grafico_potencia()        dos distribuciones, regiones de rechazo
#   20.2  potencia_medias(), n_medias(), potencia_props(), n_props()
#   20.3  n_eventos(), prob_evento(), n_pacientes_surv(), simular_surv()
#   20.4  deff(), potencia_cluster(), k_cluster(), simular_cluster()
#   20.5  potencia_sim()
#   20.6  n_prec_prop(), n_prec_media(), n_sens_esp()
#   20.7  limites_secuenciales(), n_inflar()
# ------------------------------------------------------------------------

# ---- 20.1 Figura de las dos distribuciones --------------------------------

#' Dibuja la distribución de la diferencia de medias bajo H0 (sin efecto) y bajo H1 (efecto = delta),
#' con las regiones de rechazo, el error tipo I (alfa) y el error tipo II (beta).
#' Usa la aproximación normal (suficiente para ilustrar).
grafico_potencia <- function(delta, sd, n_por_grupo, alfa = 0.05) {
  ee <- sd * sqrt(2 / n_por_grupo)                  # error estándar de la diferencia de medias
  zc <- qnorm(1 - alfa / 2)
  crit <- zc * ee                                   # valor crítico de la diferencia observada
  x <- seq(-4 * ee, delta + 4 * ee, length.out = 800)
  d0 <- tibble::tibble(x = x, y = dnorm(x, 0, ee))
  d1 <- tibble::tibble(x = x, y = dnorm(x, delta, ee))
  ggplot2::ggplot() +
    # regiones: cada una es una cinta que vale 0 fuera de su zona
    ggplot2::geom_ribbon(data = d1, ggplot2::aes(x, ymin = 0, ymax = ifelse(x <= crit, y, 0), fill = "Error tipo II (beta)"), alpha = 0.5) +
    ggplot2::geom_ribbon(data = d1, ggplot2::aes(x, ymin = 0, ymax = ifelse(x > crit, y, 0), fill = "Potencia"), alpha = 0.35) +
    ggplot2::geom_ribbon(data = d0, ggplot2::aes(x, ymin = 0, ymax = ifelse(abs(x) >= crit, y, 0), fill = "Error tipo I (alfa)"), alpha = 0.85) +
    ggplot2::geom_line(data = d0, ggplot2::aes(x, y, color = "H0: no hay efecto"), linewidth = 1) +
    ggplot2::geom_line(data = d1, ggplot2::aes(x, y, color = paste0("H1: efecto verdadero = ", delta)), linewidth = 1) +
    ggplot2::geom_vline(xintercept = c(-crit, crit), linetype = "dashed") +
    ggplot2::scale_color_manual(values = c("#555555", "#0072B2"), name = NULL) +
    ggplot2::scale_fill_manual(values = c("Error tipo I (alfa)" = "#D55E00", "Error tipo II (beta)" = "#E69F00",
                                          "Potencia" = "#56B4E9"), name = NULL) +
    ggplot2::labs(x = "Diferencia de medias observada en el ensayo (bicarbonato - placebo)", y = "Densidad") +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical", legend.spacing.y = grid::unit(0, "pt"),
                   axis.text.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
}

# ---- 20.2 Diferencia de medias y de proporciones ---------------------------

#' Potencia EXACTA (t de Student, dos colas) para comparar dos medias.
#' @param n1,n2 tamaños por grupo   @param delta diferencia a detectar   @param sd desviación estándar común
potencia_medias <- function(n1, n2 = n1, delta, sd, alfa = 0.05) {
  gl <- n1 + n2 - 2
  ncp <- delta / (sd * sqrt(1 / n1 + 1 / n2))
  tc <- qt(1 - alfa / 2, gl)
  pt(tc, gl, ncp, lower.tail = FALSE) + pt(-tc, gl, ncp)
}

#' Tamaño de muestra para comparar dos medias.
#' @param razon n2 / n1 (1 = igual tamaño; 2 = el doble en el segundo grupo)
#' @param exacto FALSE = fórmula normal (a mano); TRUE = con la t de Student (como `pwr`)
#' @return tibble con n1, n2 y total (sin redondear hacia abajo: siempre `ceiling`)
n_medias <- function(delta, sd, alfa = 0.05, potencia = 0.80, razon = 1, exacto = TRUE) {
  za <- qnorm(1 - alfa / 2); zb <- qnorm(potencia)
  n1 <- (1 + 1 / razon) * sd^2 * (za + zb)^2 / delta^2          # fórmula normal
  if (exacto) {
    n1 <- ceiling(n1)
    while (potencia_medias(n1, ceiling(razon * n1), delta, sd, alfa) < potencia) n1 <- n1 + 1
    while (n1 > 2 && potencia_medias(n1 - 1, ceiling(razon * (n1 - 1)), delta, sd, alfa) >= potencia) n1 <- n1 - 1
  }
  n1 <- ceiling(n1)
  tibble::tibble(n1 = n1, n2 = ceiling(razon * n1), total = n1 + ceiling(razon * n1))
}

#' Potencia (aproximación normal, variancia agrupada bajo H0) para comparar dos proporciones.
potencia_props <- function(n1, n2 = n1, p1, p2, alfa = 0.05) {
  za <- qnorm(1 - alfa / 2)
  pbar <- (n1 * p1 + n2 * p2) / (n1 + n2)
  ee0 <- sqrt(pbar * (1 - pbar) * (1 / n1 + 1 / n2))              # error estándar bajo H0
  ee1 <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)            # error estándar bajo H1
  d <- abs(p1 - p2)
  pnorm((d - za * ee0) / ee1) + pnorm((-d - za * ee0) / ee1)
}

#' Tamaño de muestra para comparar dos proporciones (fórmula clásica, p. ej. Fleiss).
#' @param razon n2 / n1
n_props <- function(p1, p2, alfa = 0.05, potencia = 0.80, razon = 1) {
  za <- qnorm(1 - alfa / 2); zb <- qnorm(potencia)
  pbar <- (p1 + razon * p2) / (1 + razon)
  n1 <- (za * sqrt((1 + 1 / razon) * pbar * (1 - pbar)) +
           zb * sqrt(p1 * (1 - p1) + p2 * (1 - p2) / razon))^2 / (p1 - p2)^2
  n1 <- ceiling(n1)
  tibble::tibble(n1 = n1, n2 = ceiling(razon * n1), total = n1 + ceiling(razon * n1))
}

#' Inflar el tamaño de muestra por pérdidas: n / (1 - perdidas)   (NO n * (1 + perdidas))
n_inflar <- function(n, perdidas) ceiling(n / (1 - perdidas))

# ---- 20.3 Tiempo hasta evento ---------------------------------------------

#' Número de EVENTOS necesarios (fórmula de Schoenfeld, 1983).
#' @param hr razón de riesgos a detectar   @param p proporción asignada al grupo experimental (0.5 = 1:1)
n_eventos <- function(hr, alfa = 0.05, potencia = 0.80, p = 0.5) {
  (qnorm(1 - alfa / 2) + qnorm(potencia))^2 / (p * (1 - p) * log(hr)^2)
}

#' Potencia de un ensayo con `d` eventos (inversa de Schoenfeld)
potencia_eventos <- function(d, hr, alfa = 0.05, p = 0.5) {
  pnorm(sqrt(d * p * (1 - p)) * abs(log(hr)) - qnorm(1 - alfa / 2))
}

#' Probabilidad de que un paciente tenga el evento antes del cierre del estudio, con riesgo
#' instantáneo constante `lambda` (por mes), reclutamiento uniforme durante `acum` meses y
#' cierre `dur` meses después del inicio (seguimiento mínimo = dur - acum).
prob_evento <- function(lambda, acum, dur) {
  1 - (exp(-lambda * (dur - acum)) - exp(-lambda * dur)) / (lambda * acum)
}

#' De eventos a pacientes: N = eventos / P(evento), con P(evento) promedio de los dos brazos.
n_pacientes_surv <- function(eventos, lambda0, hr, acum, dur, p = 0.5) {
  pe <- (1 - p) * prob_evento(lambda0, acum, dur) + p * prob_evento(lambda0 * hr, acum, dur)
  ceiling(eventos / pe)
}

#' Simula UN ensayo con tiempos exponenciales, reclutamiento uniforme y cierre administrativo.
#' @param perdida_mes riesgo instantáneo (por mes) de pérdida del seguimiento, igual en ambos brazos
#' @return tibble con `brazo` (0/1), `tiempo` (meses desde la entrada) y `evento` (0/1)
simular_surv <- function(n, hr, lambda0, acum, dur, perdida_mes = 0) {
  brazo <- rep(0:1, length.out = n)
  entrada <- runif(n, 0, acum)
  t_ev <- rexp(n, lambda0 * ifelse(brazo == 1, hr, 1))
  t_pe <- if (perdida_mes > 0) rexp(n, perdida_mes) else rep(Inf, n)
  t_cierre <- dur - entrada
  tiempo <- pmin(t_ev, t_pe, t_cierre)
  tibble::tibble(brazo = brazo, tiempo = tiempo, evento = as.integer(t_ev <= pmin(t_pe, t_cierre)))
}

# ---- 20.4 Ensayos por conglomerados ------------------------------------------

#' Efecto de diseño con conglomerados de tamaño medio m y correlación intraclase icc
deff <- function(m, icc) 1 + (m - 1) * icc

#' Potencia (t sobre las medias de conglomerado) con k conglomerados por brazo y m pacientes en cada uno
#' @param sd desviación estándar TOTAL del desenlace entre pacientes (dentro + entre conglomerados)
potencia_cluster <- function(k, m, icc, delta, sd, alfa = 0.05) {
  potencia_medias(n1 = k, n2 = k, delta = delta, sd = sd * sqrt(deff(m, icc) / m), alfa = alfa)
}

#' Conglomerados por brazo para una potencia dada (búsqueda directa; Inf si ni con m infinito alcanza)
k_cluster <- function(m, icc, delta, sd, alfa = 0.05, potencia = 0.80) {
  k <- 2
  while (potencia_cluster(k, m, icc, delta, sd, alfa) < potencia) {
    k <- k + 1
    if (k > 5000) return(Inf)
  }
  k
}

#' Simula UN ensayo por conglomerados (efecto del tratamiento `delta` sobre la media del desenlace)
#' @param k conglomerados POR BRAZO   @param m pacientes por conglomerado   @param sd_total DE total
#' @return tibble con `unidad`, `brazo` (0/1) y `y`
simular_cluster <- function(k, m, icc, delta, sd_total) {
  sd_u <- sd_total * sqrt(icc)                      # DE entre conglomerados
  sd_e <- sd_total * sqrt(1 - icc)                  # DE dentro de conglomerado
  unidad <- rep(seq_len(2 * k), each = m)
  brazo <- as.integer(unidad > k)
  u <- rnorm(2 * k, 0, sd_u)[unidad]
  tibble::tibble(unidad = factor(unidad), brazo = brazo, y = u + delta * brazo + rnorm(2 * k * m, 0, sd_e))
}

# ---- 20.5 Potencia por simulación ----------------------------------------------

#' Potencia por simulación de Monte Carlo.
#' @param generar function(n, ...) que devuelve UN conjunto de datos simulado
#' @param analizar function(datos) que devuelve un valor p (numérico) o un vector con nombre
#'   con `p` y, opcionalmente, `est` (la estimación del efecto)
#' @param n tamaño de muestra que se pasa a `generar`   @param nsim número de ensayos simulados
#' @param semilla semilla (reproducible)   @param ... argumentos extra para `generar`
#' @return tibble de una fila: potencia, IC 95 % de Monte Carlo (Clopper-Pearson), error estándar
#'   de Monte Carlo, estimación media del efecto, análisis fallidos
potencia_sim <- function(generar, analizar, n, nsim = 1000, alfa = 0.05, semilla = 1, ...) {
  set.seed(semilla)
  res <- vapply(seq_len(nsim), function(i) {
    r <- tryCatch(analizar(generar(n, ...)), error = function(e) c(p = NA_real_, est = NA_real_))
    if (is.null(names(r))) r <- c(p = unname(r)[1], est = NA_real_)
    c(p = unname(r["p"]), est = if ("est" %in% names(r)) unname(r["est"]) else NA_real_)
  }, numeric(2))
  p <- res["p", ]
  ok <- !is.na(p)
  rechazos <- sum(p[ok] < alfa)
  ic <- stats::binom.test(rechazos, sum(ok))$conf.int
  tibble::tibble(n = n, potencia = rechazos / sum(ok), ic_inf = ic[1], ic_sup = ic[2],
                 error_mc = sqrt(rechazos / sum(ok) * (1 - rechazos / sum(ok)) / sum(ok)),
                 est_media = if (all(is.na(res["est", ]))) NA_real_ else mean(res["est", ], na.rm = TRUE),
                 nsim = sum(ok), fallidos = sum(!ok))
}

#' Generador de un ensayo SIMULADO de bicarbonato con desenlace continuo (TFGe a 12 meses).
#' Replica del piloto: TFGe basal ~ N(30, 6), pérdida de 3 mL/min/1.73 m2 al año sin tratamiento
#' y DE del cambio a 12 meses = `sd_cambio` (2.6).
#' @param n pacientes POR GRUPO (el ensayo tiene 2 n)
#' @param efecto mejora media del cambio de TFGe en quien RECIBE el fármaco
#' @param r2_cov fracción de la varianza del cambio explicada por una covariable pronóstica `z`
#'   (p. ej. log de la UACR); se pierde si el análisis no ajusta por ella
#' @param perdida riesgo de perder el dato a 12 meses (placebo, activo)
#' @param informativa 0 = pérdidas al azar; > 0 = pierden más los que evolucionan peor
#' @param adherencia probabilidad de tomar el fármaco en el brazo activo (1 = todos)
#' @param contaminacion probabilidad de tomar el fármaco asignados a placebo (cruce)
#' @return tibble: brazo (asignado, 0/1), recibe (lo que tomó realmente), tfg0, z, tfg12 (NA si se perdió)
generar_tfg <- function(n, efecto = 1.0, sd_cambio = 2.6, sd_basal = 6, r2_cov = 0.25,
                        perdida = c(0, 0), informativa = 0, adherencia = 1, contaminacion = 0) {
  N <- 2 * n
  brazo <- rep(0:1, each = n)
  recibe <- ifelse(brazo == 1, rbinom(N, 1, adherencia), rbinom(N, 1, contaminacion))
  tfg0 <- rnorm(N, 30, sd_basal)
  z <- rnorm(N)
  desv <- sd_cambio * (sqrt(r2_cov) * z + sqrt(1 - r2_cov) * rnorm(N))      # variación individual del cambio
  tfg12 <- tfg0 - 3 + efecto * recibe + desv
  pl <- perdida[brazo + 1]
  p_perd <- plogis(qlogis(pmax(pl, 1e-9)) - informativa * desv / sd_cambio)  # más pérdidas si evoluciona peor
  tfg12[pl > 0 & runif(N) < p_perd] <- NA
  tibble::tibble(brazo = brazo, recibe = recibe, tfg0 = tfg0, z = z, tfg12 = tfg12)
}

# ---- 20.6 Precisión en lugar de potencia -------------------------------------------

#' n para estimar una proporción con IC de semiancho `d` (Wald). `N` finito opcional (corrección).
n_prec_prop <- function(p, d, conf = 0.95, N = Inf) {
  z <- qnorm(1 - (1 - conf) / 2)
  n0 <- z^2 * p * (1 - p) / d^2
  ceiling(n0 / (1 + (n0 - 1) / N))
}

#' n para estimar una media con IC de semiancho `d` (aproximación normal)
n_prec_media <- function(sd, d, conf = 0.95) {
  ceiling((qnorm(1 - (1 - conf) / 2) * sd / d)^2)
}

#' n para estimar sensibilidad y especificidad con IC de semiancho `d`.
#' La sensibilidad solo se estima en los enfermos y la especificidad en los sanos:
#' hay que reclutar hasta tener suficientes de ambos, según la prevalencia.
n_sens_esp <- function(sens, esp, prevalencia, d, conf = 0.95) {
  enf <- n_prec_prop(sens, d, conf)
  san <- n_prec_prop(esp, d, conf)
  tibble::tibble(enfermos_necesarios = enf, sanos_necesarios = san,
                 total_por_sens = ceiling(enf / prevalencia),
                 total_por_esp = ceiling(san / (1 - prevalencia)),
                 total = max(ceiling(enf / prevalencia), ceiling(san / (1 - prevalencia))))
}

# ---- 20.7 Análisis interinos -----------------------------------------------------------

#' Límites de parada por simulación de Monte Carlo (alternativa propia a `rpact`).
#' Se simula el estadístico z acumulado de un ensayo con K análisis equiespaciados en información
#' (movimiento browniano) y se calibra la constante `C` de modo que el error tipo I global sea `alfa`:
#'   Pocock:           c_k = C                 (igual en todos los análisis)
#'   O'Brien-Fleming:  c_k = C / sqrt(t_k)     (muy exigente al inicio)
#' @param K número de análisis   @param tipo "pocock" o "obf"   @param R trayectorias simuladas
#' @return vector de K límites en la escala z (bilateral)
limites_secuenciales <- function(K, tipo = c("pocock", "obf"), alfa = 0.05, R = 2e5, semilla = 1) {
  tipo <- match.arg(tipo)
  set.seed(semilla)
  t_k <- (1:K) / K
  incr <- matrix(rnorm(R * K, 0, sqrt(1 / K)), R, K)            # incrementos de información
  for (k in seq_len(K)[-1]) incr[, k] <- incr[, k] + incr[, k - 1]   # suma acumulada por columnas
  z <- incr / rep(sqrt(t_k), each = R)                          # z_k = S_k / sqrt(t_k)
  forma <- if (tipo == "pocock") rep(1, K) else 1 / sqrt(t_k)
  maxz <- do.call(pmax, as.data.frame(abs(z) / rep(forma, each = R)))   # el menor C que NO rechaza en ninguna mirada
  C <- unname(quantile(maxz, 1 - alfa))
  C * forma
}
