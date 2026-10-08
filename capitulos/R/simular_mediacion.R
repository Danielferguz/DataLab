# ------------------------------------------------------------------------
# Simulación didáctica para el Capítulo 18 (Análisis de mediación)
#   iSGLT2 -> cambio de albuminuria a los 6 meses -> pendiente de TFGe
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS. No provienen de pacientes reales.
#
# Línea de tiempo (por paciente)
#   mes 0   : variables basales (C) e inicio (o no) del iSGLT2 (A)
#   mes 6   : mediador M = cambio de ln(UACR) entre el mes 0 y el mes 6
#             (negativo = baja la albuminuria; -0.35 equivale a -30 %)
#   mes 6-24: desenlace Y = pendiente de TFGe (mL/min/1.73 m2/año) y,
#             en paralelo, evento renal compuesto en 36 meses (tiempo T).
#
# Ecuaciones estructurales (la "verdad")
#   M(a)   = m0(C) + alfa * a + u2_m * U2 + u3_m * U3 + l_m * dL(a) + e_m
#   Y(a,m) = y0(C) + theta1 * a + beta * m + theta3 * a * m
#            + u1_y * U1 + u2_y * U2 + l_y * dL(a) + e_y
#   log h(a,m) = h0(C) - k * [theta1*a + beta*m + theta3*a*m + u1_y*U1 + u2_y*U2 + l_y*dL(a)]
#                (el riesgo de evento "sigue" a la pendiente; k = 0.5)
#   T(a,m) = E / h(a,m), con E ~ Exp(1) propio de cada paciente (hazards proporcionales).
#   Un paciente tiene, entonces, M(0), M(1) y Y(a, m) para cualquier a y m:
#   por eso se pueden calcular los efectos NATURALES (que usan Y(1, M(0))).
#
# Efectos verdaderos con los valores por defecto (sin interacción, sin confusores
# no medidos):
#   efecto de A sobre M  alfa   = -0.35   (baja la UACR un ~30 %)
#   efecto de M sobre Y  beta   = -1.5    (por cada +1 en ln UACR, -1.5 mL/min/año)
#   efecto directo       theta1 = +0.60   (NDE = CDE = 0.60)
#   efecto indirecto     alfa*beta = +0.525  (NIE)
#   efecto total                  = +1.125 ; proporción mediada = 46.7 %
#
# Cuatro "supuestos" que se pueden romper a voluntad (Cap. 18.3):
#   (1) confusión A-Y no medida  : fragilidad U1  -> u1_a (sobre A) y u1_y (sobre Y)
#   (2) confusión M-Y no medida  : actividad inflamatoria U2 -> u2_m y u2_y
#   (3) confusión A-M no medida  : intensidad del bloqueo RAAS U3 -> u3_a (sobre A) y u3_m (sobre M)
#   (4) confusor M-Y afectado por A: PAS a los 6 meses L -> l_a (A sobre L), l_m y l_y
#   Con todos en 0 (por defecto) los cuatro supuestos se cumplen (dado C).
# Interacción A x M: theta3 != 0.
# Mediación moderada (Cap. 18.6): alfa_z != 0 hace que el efecto del fármaco sobre la albuminuria
#   dependa de la UACR basal: alfa(z) = alfa + alfa_z * (ln UACR basal - 4.8).
# ------------------------------------------------------------------------

simular_mediacion <- function(n = 5000, semilla = 2026,
                              alfa = -0.35, beta = -1.5, theta1 = 0.60, theta3 = 0,
                              alfa_z = 0,                # mediación moderada: el efecto de A sobre M cambia con ln(UACR) basal (por unidad)
                              u1_a = 0, u1_y = 0,        # (1) fragilidad: u1_a > 0 reduce la prob. de tratar
                              u2_m = 0, u2_y = 0,        # (2) inflamación: sube M (u2_m > 0), empeora Y (u2_y < 0)
                              u3_a = 0, u3_m = 0,        # (3) RAAS: u3_a > 0 favorece tratar; u3_m < 0 baja M
                              l_a = -6, l_m = 0, l_y = 0, # (4) PAS 6 meses: l_a = efecto de A sobre L (mmHg)
                              log_h0 = -6.6, k_haz = 0.5, # riesgo basal y vínculo pendiente -> riesgo
                              aleatorio = FALSE) {       # TRUE: el iSGLT2 se asigna al azar (ensayo), sin depender de C
  set.seed(semilla)

  # ---- Covariables basales (mismas distribuciones que ckd_isglt2) --------
  edad    <- round(pmin(pmax(rnorm(n, 64, 10), 30), 90))
  hombre  <- rbinom(n, 1, 0.55)
  hba1c   <- round(pmin(pmax(rnorm(n, 7.8, 1.2), 5.5), 12), 1)
  pas     <- round(pmin(pmax(rnorm(n, 135 + 0.25 * (edad - 64), 15), 95), 200))
  tfg     <- round(pmin(pmax(rnorm(n, 46 - 0.30 * (edad - 64), 9), 20), 75), 1)
  log_uacr <- 4.8 + 0.25 * (hba1c - 7.8) + 0.012 * (pas - 135) - 0.015 * (tfg - 46) + rnorm(n, 0, 1.0)
  uacr    <- round(exp(log_uacr))
  luacr0  <- log(pmax(uacr, 1))               # lo que el analista ve (UACR redondeada)
  ecv     <- rbinom(n, 1, plogis(-1.2 + 0.04 * (edad - 64) + 0.3 * hombre))

  # ---- Variables latentes / no medidas (siempre se generan: mismo flujo aleatorio) ----
  U1 <- 0.03 * (edad - 64) + rnorm(n)          # fragilidad
  U2 <- rnorm(n)                               # actividad inflamatoria / glomerular
  U3 <- rnorm(n)                               # intensidad del bloqueo RAAS
  eps_l <- rnorm(n, 0, 8)                      # variación de la PAS a los 6 meses
  eps_m <- rnorm(n, 0, 0.45)
  eps_y <- rnorm(n, 0, 1.0)
  E     <- rexp(n)                             # ruido del tiempo hasta el evento

  # ---- Asignación del iSGLT2 (por indicación clínica, sobre variables medidas) ----
  lp_trat <- -0.35 + 0.045 * (tfg - 46) + 0.55 * (log_uacr - 4.8) + 0.15 * (hba1c - 7.8) +
             0.30 * ecv - 0.045 * (edad - 64) - u1_a * U1 + u3_a * U3
  A <- rbinom(n, 1, if (aleatorio) 0.5 else plogis(lp_trat))

  # ---- Potenciales: confusor posterior L, mediador M ----------------------
  dL <- function(a) l_a * a + eps_l                       # cambio de PAS a los 6 meses
  M_fun <- function(a) {
    0.10 - 0.10 * (luacr0 - 4.8) + 0.005 * (pas - 135) + 0.03 * (hba1c - 7.8) +
      alfa * a + alfa_z * a * (luacr0 - 4.8) + u2_m * U2 + u3_m * U3 + l_m * dL(a) + eps_m
  }
  M0 <- M_fun(0); M1 <- M_fun(1)

  # ---- Potenciales del desenlace: pendiente de TFGe -----------------------
  y_base <- -2.3 - 0.45 * (luacr0 - 4.8) - 0.020 * (pas - 135) - 0.25 * (hba1c - 7.8) -
            0.50 * ecv - 0.030 * (edad - 64) + 0.020 * (tfg - 46)
  # parte "estructural" (la que también mueve el riesgo de evento)
  est_fun <- function(a, m) theta1 * a + beta * m + theta3 * a * m + u1_y * U1 + u2_y * U2 + l_y * dL(a)
  Y_fun <- function(a, m) y_base + est_fun(a, m) + eps_y

  # ---- Potenciales del tiempo hasta el evento (hazards proporcionales) ----
  h_base <- log_h0 + 0.30 * (luacr0 - 4.8) + 0.012 * (pas - 135) + 0.12 * (hba1c - 7.8) +
            0.30 * ecv - 0.030 * (tfg - 46) + 0.010 * (edad - 64)
  T_fun <- function(a, m) E / exp(h_base - k_haz * est_fun(a, m))

  t00 <- T_fun(0, M0); t11 <- T_fun(1, M1); t10 <- T_fun(1, M0); t01 <- T_fun(0, M1)
  e00 <- as.integer(t00 <= 36); e11 <- as.integer(t11 <= 36); e10 <- as.integer(t10 <= 36); e01 <- as.integer(t01 <= 36)
  M_obs <- ifelse(A == 1, M1, M0)
  Y_obs <- Y_fun(A, M_obs)
  T_obs <- T_fun(A, M_obs)
  dL_obs <- ifelse(A == 1, dL(1), dL(0))

  tibble::tibble(
    id = 1:n, edad, sexo = ifelse(hombre == 1, "Hombre", "Mujer"),
    hba1c, pas, tfg_basal = tfg, uacr_basal = uacr, ecv,
    isglt2 = A,
    delta_lnuacr = round(M_obs, 3),                       # mediador: cambio de ln(UACR) a 6 meses
    uacr_6m = round(uacr * exp(M_obs), 0),
    pas_6m = round(pas + dL_obs),                         # PAS a los 6 meses (posterior al tratamiento)
    pendiente_tfg = round(Y_obs, 2),
    tiempo_meses = pmax(round(pmin(T_obs, 36), 2), 0.01),
    evento = as.integer(T_obs <= 36),
    # --- oráculo (no existe en la vida real) ---
    fragilidad = round(U1, 2), inflamacion = round(U2, 2), raas = round(U3, 2),
    m0 = round(M0, 3), m1 = round(M1, 3),
    y00 = round(Y_fun(0, M0), 2),     # Y(0, M(0)): sin tratamiento
    y11 = round(Y_fun(1, M1), 2),     # Y(1, M(1)): con tratamiento
    y10 = round(Y_fun(1, M0), 2),     # Y(1, M(0)): tratado, pero con la albuminuria "de no tratado"
    y01 = round(Y_fun(0, M1), 2),     # Y(0, M(1)): no tratado, pero con la albuminuria "de tratado"
    y1_m0 = round(Y_fun(1, 0), 2),    # Y(1, m = 0): para el efecto directo controlado (CDE)
    y0_m0 = round(Y_fun(0, 0), 2),    # Y(0, m = 0)
    t00 = round(t00, 3), t11 = round(t11, 3), t10 = round(t10, 3), t01 = round(t01, 3),
    ev00 = e00, ev11 = e11, ev10 = e10, ev01 = e01
  )
}

#' Separa lo observable de lo que solo existe porque simulamos
observado_mediacion <- function(d) {
  dplyr::select(d, id:evento)
}

#' Genera los archivos CSV de la carpeta Bases/ (ejecutar a mano):
#'   source("capitulos/R/simular_mediacion.R"); guardar_mediacion("capitulos/Bases")
#' Tres versiones:
#'   ckd_mediacion        : se cumplen los cuatro supuestos (sin interacción)
#'   ckd_mediacion_int    : hay interacción exposición x mediador (theta3 = -1)
#'   ckd_mediacion_u      : confusor M-Y no medido (inflamación), el caso "realista" de 18.5
guardar_mediacion <- function(carpeta = "Bases") {
  versiones <- list(
    ckd_mediacion     = simular_mediacion(),
    ckd_mediacion_int = simular_mediacion(theta3 = -1.0, semilla = 2027),
    ckd_mediacion_u   = simular_mediacion(u2_m = 0.25, u2_y = -0.8, semilla = 2028)
  )
  for (nm in names(versiones)) {
    d <- versiones[[nm]]
    readr::write_csv(observado_mediacion(d), file.path(carpeta, paste0(nm, ".csv")))
    readr::write_csv(d, file.path(carpeta, paste0(nm, "_oraculo.csv")))
  }
  invisible(versiones)
}
