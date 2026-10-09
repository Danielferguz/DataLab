# ------------------------------------------------------------------------
# SIMULACIONES del Capítulo 19 (métodos cuasi-experimentales), subcapítulos 19.4 y 19.5
#   source("R/simular_cap19_cuasi.R")
# Todos los datos son SIMULADOS. En cada caso conocemos la verdad (columnas "oráculo").
# Requiere: tidyverse (dplyr, tibble, tidyr).
# Semillas: se eligieron entre ~25 candidatas por ser MUESTRAS TÍPICAS (el estimador cae a
# menos de ~1 error estándar de la verdad). Con otras semillas el azar mueve las cifras;
# en promedio los estimadores aciertan (comprobado por Monte Carlo al escribir el capítulo).
#
# 1) simular_iv()   Variable instrumental: preferencia del nefrólogo (19.4)
# 2) simular_did()  Diferencias en diferencias: derivación temprana por regiones (19.5)
# 3) simular_rdd()  Regresión discontinua: protocolo "iSGLT2 si UACR >= 300" (19.5)
# ------------------------------------------------------------------------

# ========================================================================
# 1) VARIABLE INSTRUMENTAL
# ------------------------------------------------------------------------
# Pacientes con ERC y DM2 atendidos por n_med nefrólogos (asignación "como al azar").
# Cada nefrólogo j tiene una PREFERENCIA latente phi_j ~ N(0, sd_pref) por recetar iSGLT2.
# Tratamiento (lógica de umbral latente, con ruido logístico):
#   lat_i = -0.35 + 0.045 (TFGe-46) + 0.55 (lnUACR-4.8) + 0.15 (HbA1c-7.8) + 0.30 ECV
#           - 0.045 (edad-64) - 0.70 fragilidad + s_i * phi_j + e_i,    A_i = 1 si lat_i > 0
#   s_i = peso de la preferencia: grande si la albuminuria es baja (zona gris, el médico
#   decide) y pequeño si es alta (indicación clara). Siempre > 0 => MONOTONÍA.
# Resultado (pendiente anual de TFGe; más alto = mejor), como en simular_ckd.R:
#   Y0 = -2.3 - 0.45 (lnUACR-4.8) - 0.02 (PAS-135) - 0.25 (HbA1c-7.8) - 0.5 ECV
#        - 0.03 (edad-64) + 0.02 (TFGe-46) - 0.60 fragilidad + N(0,1)
#   efecto individual = max(0.9 + 0.6 (lnUACR-4.8) + N(0,0.3), 0);  Y1 = Y0 + efecto.
# La fragilidad NO se mide: confunde (menos tratamiento y peor pronóstico).
# El instrumento observado es la PREFERENCIA OBSERVADA del médico: proporción de los OTROS
# pacientes de ese médico que recibió iSGLT2 (excluyendo al paciente mismo).
#
# VERDAD (oráculo):
#   ite                efecto individual;  ATE = mean(ite)
#   tipo               siempre / nunca / cumplidor, según el tratamiento que habría recibido
#                      con un médico de preferencia BAJA (media de los médicos bajo la mediana)
#                      o ALTA (media de los médicos sobre la mediana)
#   LATE = mean(ite) de los cumplidores (aprox. lo que estima el Wald con el instrumento
#   dicotomizado en la mediana; el grupo exacto de cumplidores depende del contraste).
# ========================================================================
simular_iv <- function(n = 6000, n_med = 40, sd_pref = 0.6, semilla = 1904) {
  set.seed(semilla)
  edad <- round(pmin(pmax(rnorm(n, 64, 10), 30), 90))
  hombre <- rbinom(n, 1, 0.55)
  hba1c <- round(pmin(pmax(rnorm(n, 7.8, 1.2), 5.5), 12), 1)
  pas <- round(pmin(pmax(rnorm(n, 135 + 0.25 * (edad - 64), 15), 95), 200))
  tfg <- round(pmin(pmax(rnorm(n, 46 - 0.30 * (edad - 64), 9), 20), 75), 1)
  log_uacr <- 4.8 + 0.25 * (hba1c - 7.8) + 0.012 * (pas - 135) - 0.015 * (tfg - 46) + rnorm(n, 0, 1.0)
  uacr <- pmax(round(exp(log_uacr)), 1)
  ecv <- rbinom(n, 1, plogis(-1.2 + 0.04 * (edad - 64) + 0.3 * hombre))
  fragilidad <- 0.03 * (edad - 64) + rnorm(n, 0, 1)

  # Médicos y preferencias (el paciente "cae" con un médico sin elegirlo: supuesto de independencia)
  medico <- sample.int(n_med, n, replace = TRUE)
  phi <- rnorm(n_med, 0, sd_pref)
  phi_bajo <- mean(phi[phi < median(phi)]); phi_alto <- mean(phi[phi >= median(phi)])

  lat_base <- -0.35 + 0.045 * (tfg - 46) + 0.55 * (log_uacr - 4.8) + 0.15 * (hba1c - 7.8) +
    0.30 * ecv - 0.045 * (edad - 64) - 0.70 * fragilidad
  s <- pmin(pmax(1.5 - 0.5 * (log_uacr - 4.8), 0.3), 2.5)
  e <- rlogis(n)
  a <- as.integer(lat_base + s * phi[medico] + e > 0)
  a_bajo <- as.integer(lat_base + s * phi_bajo + e > 0)
  a_alto <- as.integer(lat_base + s * phi_alto + e > 0)

  y0 <- -2.3 - 0.45 * (log_uacr - 4.8) - 0.02 * (pas - 135) - 0.25 * (hba1c - 7.8) - 0.5 * ecv -
    0.03 * (edad - 64) + 0.02 * (tfg - 46) - 0.60 * fragilidad + rnorm(n, 0, 1)
  ite <- pmax(0.9 + 0.60 * (log_uacr - 4.8) + rnorm(n, 0, 0.3), 0)
  y <- ifelse(a == 1, y0 + ite, y0)

  # Preferencia observada: proporción de OTROS pacientes del mismo médico tratados
  tot <- tapply(a, medico, sum)[as.character(medico)]
  cnt <- as.numeric(table(medico)[as.character(medico)])
  pref <- (as.numeric(tot) - a) / (cnt - 1)

  tibble::tibble(
    id = seq_len(n), medico, edad, sexo = ifelse(hombre == 1, "Hombre", "Mujer"), hba1c, pas,
    tfg_basal = tfg, uacr, ecv, isglt2 = a, pendiente_tfg = round(y, 2), preferencia = pref,
    # --- oráculo ---
    fragilidad = round(fragilidad, 2), ite = round(ite, 2), pendiente_0 = round(y0, 2),
    pref_verdadera = phi[medico],
    tipo = dplyr::case_when(a_bajo == 1 & a_alto == 1 ~ "siempre", a_bajo == 0 & a_alto == 0 ~ "nunca",
                            a_bajo == 0 & a_alto == 1 ~ "cumplidor", TRUE ~ "desafiante")
  )
}

# ========================================================================
# 2) DIFERENCIAS EN DIFERENCIAS (panel región x año)
# ------------------------------------------------------------------------
# 24 regiones, 2016-2025. Las regiones con política (n_trat) la aplican desde 2021:
# "derivación temprana": el médico de primaria deriva a nefrología con TFGe >= 30.
# Desenlace: % de primeras consultas de nefrología con TFGe >= 30 (derivación temprana).
#   y_rt = a_r + g * (t - 2016) + efecto_k [si tratada y t >= 2021] + dif_tend * (t - 2016) [si tratada]
#          + u_rt,   u_rt = AR(1) dentro de la región (rho = 0.6, DE marginal 2.0)
#   a_r: nivel basal (las regiones que adoptan la política parten MÁS BAJO: autoselección en nivel)
#   g = +1.0 punto/año (tendencia común). efecto_k, k = años desde 2021: 3, 6, 8, 8, 8
#   => ATT promedio de 2021-2025 = 6.6 puntos. dif_tend > 0 rompe las tendencias paralelas.
# ========================================================================
simular_did <- function(n_reg = 24, n_trat = 10, dif_tend = 0, semilla = 1925) {
  set.seed(semilla)
  anios <- 2016:2025; politica <- 2021
  efectos <- c(3, 6, 8, 8, 8)
  reg <- tibble::tibble(region = sprintf("R%02d", seq_len(n_reg)),
                        tratada = as.integer(seq_len(n_reg) %in% sample.int(n_reg, n_trat))) |>
    dplyr::mutate(base = ifelse(tratada == 1, 30, 38) + rnorm(n_reg, 0, 3))
  d <- tidyr::expand_grid(region = reg$region, anio = anios) |>
    dplyr::left_join(reg, by = "region") |>
    dplyr::mutate(k = anio - politica, post = as.integer(anio >= politica),
                  efecto = ifelse(tratada == 1 & post == 1, efectos[pmin(pmax(k, 0), 4) + 1], 0))
  # ruido AR(1) por región
  u <- unlist(lapply(seq_len(n_reg), function(i) as.numeric(stats::arima.sim(list(ar = 0.6), n = length(anios), sd = 2 * sqrt(1 - 0.36)))))
  d$y0 <- d$base + 1.0 * (d$anio - 2016) + d$tratada * dif_tend * (d$anio - 2016) + u
  d$pct_temprana <- round(d$y0 + d$efecto, 1)
  d |> dplyr::select(region, anio, tratada, post, k_rel = k, pct_temprana, y0, efecto)
}

# ========================================================================
# 3) REGRESIÓN DISCONTINUA NÍTIDA: "se prescribe iSGLT2 si UACR >= 300 mg/g"
# ------------------------------------------------------------------------
# Protocolo hospitalario hipotético: solo se autoriza el iSGLT2 con UACR >= 300 (todos los de
# arriba lo reciben, ninguno de abajo). Covariables y pronóstico como en simular_ckd.R
# (sin fragilidad: el diseño no la necesita). Efecto individual = max(0.9 + 0.6 (lnUACR-4.8) + N(0,.3), 0).
# VERDAD: el efecto en el umbral es 0.9 + 0.6 (ln 300 - 4.8) = 1.44 (ATE de toda la población ~0.9).
# manipulacion = TRUE: al medir, parte de los pacientes con UACR verdadera en [265, 300) con mejor
# pronóstico se "empuja" a 300-315 (repetir la muestra hasta que "salga" >= 300).
# ========================================================================
simular_rdd <- function(n = 4000, manipulacion = FALSE, semilla = 1922) {
  set.seed(semilla)
  edad <- round(pmin(pmax(rnorm(n, 64, 10), 30), 90))
  hba1c <- round(pmin(pmax(rnorm(n, 7.8, 1.2), 5.5), 12), 1)
  pas <- round(pmin(pmax(rnorm(n, 135 + 0.25 * (edad - 64), 15), 95), 200))
  tfg <- round(pmin(pmax(rnorm(n, 46 - 0.30 * (edad - 64), 9), 20), 75), 1)
  ecv <- rbinom(n, 1, plogis(-1.2 + 0.04 * (edad - 64)))
  log_uacr <- 4.8 + 0.25 * (hba1c - 7.8) + 0.012 * (pas - 135) - 0.015 * (tfg - 46) + rnorm(n, 0, 1.0)
  uacr_ver <- exp(log_uacr)
  y0 <- -2.3 - 0.45 * (log_uacr - 4.8) - 0.02 * (pas - 135) - 0.25 * (hba1c - 7.8) - 0.5 * ecv -
    0.03 * (edad - 64) + 0.02 * (tfg - 46) + rnorm(n, 0, 1)
  ite <- pmax(0.9 + 0.60 * (log_uacr - 4.8) + rnorm(n, 0, 0.3), 0)
  uacr_obs <- uacr_ver
  if (manipulacion) {
    candidatos <- uacr_ver >= 265 & uacr_ver < 300 & y0 > median(y0)   # los de mejor pronóstico
    mueve <- candidatos & runif(n) < 0.9
    uacr_obs[mueve] <- runif(sum(mueve), 300, 315)
  }
  uacr <- pmax(round(uacr_obs, 1), 1)
  trat <- as.integer(uacr >= 300)
  tibble::tibble(id = seq_len(n), edad, hba1c, pas, tfg_basal = tfg, ecv, uacr, isglt2 = trat,
                 pendiente_tfg = round(ifelse(trat == 1, y0 + ite, y0), 2),
                 # --- oráculo ---
                 ite = round(ite, 2), uacr_verdadera = uacr_ver)
}
