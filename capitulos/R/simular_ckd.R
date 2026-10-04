# ------------------------------------------------------------------------
# Simulación didáctica: iSGLT2 en enfermedad renal crónica (ERC) con DM2
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS. No provienen de pacientes reales.
# Sirven para que el lector conozca la "verdad" (efecto causal) y compruebe
# si cada método de análisis la recupera.
#
# Pregunta causal del libro:
#   En adultos con ERC (G3-G4) y diabetes tipo 2 atendidos en nefrología,
#   ¿iniciar un iSGLT2 enlentece la pérdida de función renal, comparado con
#   no iniciarlo?
#
# Variables observadas
#   id            identificador
#   edad          años
#   sexo          "Mujer" / "Hombre"
#   hba1c         %
#   pas           presión arterial sistólica (mmHg)
#   tfg_basal     TFGe basal (mL/min/1.73 m2)
#   uacr          albuminuria (mg/g)
#   ecv           enfermedad cardiovascular previa (0/1)
#   isglt2        tratamiento: inició iSGLT2 (1) / no (0)
#   pendiente_tfg cambio anual de TFGe (mL/min/1.73 m2/año). Más alto = mejor
#   tiempo_meses  seguimiento hasta el evento renal o fin (máx. 36 meses)
#   evento        evento renal compuesto (TFGe -40 %, diálisis o trasplante)
#
# Variable NO medida (solo en la versión "oráculo")
#   fragilidad    latente: menos probabilidad de recibir iSGLT2 y peor pronóstico
#
# Columnas "oráculo" (solo existen porque simulamos; en la vida real NO se
# observan): pendiente_0, pendiente_1, ite, t_evento_0, t_evento_1, ...
# ------------------------------------------------------------------------

simular_ckd <- function(n = 5000, semilla = 2025) {
  set.seed(semilla)

  edad    <- round(pmin(pmax(rnorm(n, 64, 10), 30), 90))
  hombre  <- rbinom(n, 1, 0.55)
  hba1c   <- round(pmin(pmax(rnorm(n, 7.8, 1.2), 5.5), 12), 1)
  pas     <- round(pmin(pmax(rnorm(n, 135 + 0.25 * (edad - 64), 15), 95), 200))
  tfg     <- round(pmin(pmax(rnorm(n, 46 - 0.30 * (edad - 64), 9), 20), 75), 1)
  log_uacr <- 4.8 + 0.25 * (hba1c - 7.8) + 0.012 * (pas - 135) -
              0.015 * (tfg - 46) + rnorm(n, 0, 1.0)
  uacr    <- round(exp(log_uacr))
  ecv     <- rbinom(n, 1, plogis(-1.2 + 0.04 * (edad - 64) + 0.3 * hombre))

  # Fragilidad: NO medida
  fragilidad <- 0.03 * (edad - 64) + rnorm(n, 0, 1)

  # ---- Asignación del tratamiento (por indicación clínica) --------------
  # Más probable si: TFGe más alta, más albuminuria, HbA1c alta, ECV;
  # menos probable si: edad avanzada, fragilidad.
  lp_trat <- -0.35 + 0.045 * (tfg - 46) + 0.55 * (log_uacr - 4.8) +
             0.15 * (hba1c - 7.8) + 0.30 * ecv - 0.045 * (edad - 64) -
             0.70 * fragilidad
  isglt2  <- rbinom(n, 1, plogis(lp_trat))

  # ---- Resultado 1: pendiente anual de TFGe ------------------------------
  pend_0 <- -2.3 - 0.45 * (log_uacr - 4.8) - 0.020 * (pas - 135) -
            0.25 * (hba1c - 7.8) - 0.50 * ecv - 0.030 * (edad - 64) +
            0.020 * (tfg - 46) - 0.60 * fragilidad + rnorm(n, 0, 1.0)
  # El beneficio del iSGLT2 es mayor con más albuminuria (heterogeneidad)
  efecto <- pmax(0.9 + 0.60 * (log_uacr - 4.8) + rnorm(n, 0, 0.3), 0)
  pend_1 <- pend_0 + efecto
  pendiente <- ifelse(isglt2 == 1, pend_1, pend_0)

  # ---- Resultado 2: evento renal compuesto en 36 meses -------------------
  # Riesgo instantáneo (por mes) con efecto multiplicativo constante HR = 0.70
  lp_haz <- -5.55 + 0.30 * (log_uacr - 4.8) + 0.012 * (pas - 135) +
            0.12 * (hba1c - 7.8) + 0.30 * ecv - 0.030 * (tfg - 46) +
            0.010 * (edad - 64) + 0.35 * fragilidad
  tasa_0 <- exp(lp_haz)
  t0 <- rexp(n, tasa_0)
  t1 <- t0 / 0.70   # con riesgo exponencial, HR = 0.70 equivale a T1 = T0 / 0.70
  t_obs <- ifelse(isglt2 == 1, t1, t0)
  evento <- as.integer(t_obs <= 36)
  tiempo <- pmin(t_obs, 36)

  tibble::tibble(
    id = 1:n, edad, sexo = ifelse(hombre == 1, "Hombre", "Mujer"),
    hba1c, pas, tfg_basal = tfg, uacr, ecv, isglt2,
    pendiente_tfg = round(pendiente, 2),
    tiempo_meses = round(tiempo, 1), evento,
    # --- oráculo ---
    fragilidad = round(fragilidad, 2),
    pendiente_0 = round(pend_0, 2), pendiente_1 = round(pend_1, 2),
    ite = round(pend_1 - pend_0, 2),
    evento_36m_0 = as.integer(t0 <= 36), evento_36m_1 = as.integer(t1 <= 36)
  )
}

# Para regenerar los archivos CSV de la carpeta Bases/ (ejecutar a mano):
#   source("capitulos/R/simular_ckd.R"); guardar_ckd("capitulos/Bases")
guardar_ckd <- function(carpeta = "Bases") {
  d <- simular_ckd()
  oraculo <- c("fragilidad", "pendiente_0", "pendiente_1", "ite",
               "evento_36m_0", "evento_36m_1")
  readr::write_csv(dplyr::select(d, -dplyr::all_of(oraculo)),
                   file.path(carpeta, "ckd_isglt2.csv"))
  readr::write_csv(d, file.path(carpeta, "ckd_isglt2_oraculo.csv"))
  invisible(d)
}
