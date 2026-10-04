# ------------------------------------------------------------------------
# ECA SIMULADO: bicarbonato de sodio vs. placebo en ERC G3b-G4 con acidosis leve
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS. Sirven para los análisis de ensayos
# aleatorizados (Cap. 11): ITT, ANCOVA, mediciones repetidas, subgrupos,
# no adherencia (CACE) y desenlaces compuestos.
#
# Diseño: 640 pacientes, asignación 1:1, estratificada por centro (16 centros).
# Seguimiento: TFGe a los 0, 6, 12 y 24 meses; eventos hasta 36 meses.
#
# Verdad conocida:
#   - Solo se beneficia quien TOMA el fármaco (cumplidor: ~75 % de los asignados a bicarbonato).
#   - Efecto del tratamiento en cumplidores sobre la pendiente de TFGe (mL/min/1.73 m2 por año):
#         0.90 + 0.30 * (20 - bicarbonato basal)      -> mayor beneficio con acidosis más marcada
#   - Efecto sobre el riesgo de falla renal: HR = 0.70 en cumplidores.
#   - Los NO cumplidores tienen peor pronóstico (efecto del "paciente cumplidor"):
#     el análisis por protocolo está sesgado.
# ------------------------------------------------------------------------

simular_eca_bicarbonato <- function(n = 640, semilla = 2027) {
  set.seed(semilla)
  centro <- sample(1:16, n, TRUE)
  efecto_centro <- rnorm(16, 0, 1.2)[centro]                        # algunos centros tienen pacientes con mejor/peor evolución

  edad  <- round(pmin(pmax(rnorm(n, 64, 11), 30), 88))
  sexo  <- sample(c("Mujer", "Hombre"), n, TRUE, prob = c(0.42, 0.58))
  dm    <- rbinom(n, 1, 0.40)
  tfg0  <- round(pmin(pmax(rnorm(n, 30, 6), 15), 44), 1)
  bic0  <- round(pmin(pmax(rnorm(n, 20.0 + 0.04 * (tfg0 - 30), 1.8), 14), 24), 1)
  luacr <- rnorm(n, 5.0 + 0.5 * dm, 1.2)
  uacr  <- round(exp(luacr))
  pas   <- round(pmin(pmax(rnorm(n, 136, 15), 100), 190))

  # Aleatorización 1:1 estratificada por centro (bloques)
  brazo <- integer(n)
  for (c in 1:16) {
    idx <- which(centro == c)
    brazo[idx] <- sample(rep(0:1, length.out = length(idx)))
  }

  # Cumplimiento latente: quienes tomarían el fármaco si se les asigna (más jóvenes, con mejor estado)
  cumplidor <- rbinom(n, 1, plogis(1.1 - 0.025 * (edad - 64) - 0.3 * dm + 0.1 * (bic0 - 20)))
  adhiere <- as.integer(brazo == 1 & cumplidor == 1)                 # tomó el fármaco (solo observable en el brazo activo)

  # Pendiente anual de TFGe sin tratamiento
  pend0 <- -2.6 - 0.025 * (edad - 64) - 0.35 * (luacr - 5) - 0.012 * (pas - 136) - 0.55 * dm +
           0.8 * (cumplidor - 0.75) + efecto_centro * 0.25 + rnorm(n, 0, 0.8)
  efecto_ind <- pmax(0.90 + 0.30 * (20 - bic0), 0)                   # efecto individual en cumplidores
  pend <- pend0 + adhiere * efecto_ind

  visita <- c(0, 6, 12, 24)
  tfg_sin <- sapply(visita, \(m) tfg0 + pend * m / 12)
  tfg_obs <- tfg_sin + matrix(rnorm(n * 4, 0, 2.2), n, 4)            # error de medición
  tfg_obs[, 1] <- tfg0                                               # basal medido sin error adicional
  colnames(tfg_obs) <- paste0("tfg_", c("0", "6", "12", "24"))

  # Datos faltantes: abandono monótono, más frecuente si la TFGe previa es baja
  dejo <- matrix(FALSE, n, 4)
  for (k in 2:4) {
    prev_ok <- if (k == 2) rep(TRUE, n) else !dejo[, k - 1]
    p <- plogis(-3.0 - 0.10 * (tfg_obs[, k - 1] - 30) + 0.3 * (brazo == 1))
    dejo[, k] <- !prev_ok | rbinom(n, 1, p) == 1
  }
  tfg_obs[dejo] <- NA

  # Eventos: falla renal y muerte (tiempo continuo hasta 36 meses)
  h_krt <- 0.0085 * exp(-0.09 * (pend0 + 2.6) * 3 + 0.03 * (30 - tfg0) + 0.25 * (luacr - 5)) * ifelse(adhiere == 1, 0.70, 1)
  h_mue <- 0.0035 * exp(0.045 * (edad - 64) + 0.4 * dm - 0.3 * (cumplidor - 0.75))
  t_krt <- rexp(n, h_krt); t_mue <- rexp(n, h_mue)
  cens  <- runif(n, 24, 36)
  t_ev  <- pmin(t_krt, t_mue, cens, 36)
  estado <- ifelse(t_ev == t_krt & t_krt <= pmin(cens, 36), 1L, ifelse(t_ev == t_mue & t_mue <= pmin(cens, 36), 2L, 0L))

  datos <- tibble::tibble(
    id = 1:n, centro, brazo = ifelse(brazo == 1, "Bicarbonato", "Placebo"),
    edad, sexo, dm, tfg0, bic0, uacr, pas, adhiere,
    tfg_6 = round(tfg_obs[, 2], 1), tfg_12 = round(tfg_obs[, 3], 1), tfg_24 = round(tfg_obs[, 4], 1),
    tiempo_meses = round(t_ev, 2), estado
  )
  oraculo <- tibble::tibble(id = 1:n, cumplidor, pend0, efecto_ind, pend)
  list(datos = datos, oraculo = oraculo)
}

guardar_eca_bicarbonato <- function(carpeta = "Bases") {
  x <- simular_eca_bicarbonato()
  readr::write_csv(x$datos, file.path(carpeta, "eca_bicarbonato.csv"))
  readr::write_csv(x$oraculo, file.path(carpeta, "eca_bicarbonato_oraculo.csv"))
  invisible(x)
}
