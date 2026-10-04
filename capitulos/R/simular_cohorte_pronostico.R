# ------------------------------------------------------------------------
# Cohorte SIMULADA de pacientes con ERC G3b-G5 (pronóstico y supervivencia)
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS. Sirven para los capítulos de supervivencia
# (9), pronóstico (17) y para ilustrar riesgos competitivos.
#
# Desenlaces (tiempo discreto mensual, hasta 60 meses):
#   estado = 1 -> falla renal (inicio de diálisis o trasplante)
#   estado = 2 -> muerte antes de la falla renal (riesgo competitivo)
#   estado = 0 -> censura (fin de seguimiento o pérdida)
#
# Verdad conocida (log-razones de riesgos instantáneos, por causa):
#   Falla renal:  TFGe (por 10 unidades menos)  +0.95
#                 ln(UACR) (por unidad)          +0.45
#                 edad (por 10 años)             -0.20   (a igual TFGe, menos falla y más muerte)
#                 diabetes:                      +0.95 los primeros 18 meses, 0 después (NO proporcional)
#                 fosforo (por mg/dL)            +0.25;  bicarbonato (por mEq/L) -0.06;  albumina (por g/dL) -0.30
#   Muerte:       edad (por 10 años)             +0.55
#                 diabetes +0.35;  ECV +0.50;  albumina (por g/dL) -0.55
#                 TFGe (por 10 unidades menos)   +0.15
# ------------------------------------------------------------------------

simular_cohorte_pronostico <- function(n = 3000, semilla = 2026, cohorte = c("desarrollo", "validacion"),
                                       meses_max = 60, censura = TRUE) {
  cohorte <- match.arg(cohorte)
  set.seed(semilla)
  val <- cohorte == "validacion"

  edad   <- round(pmin(pmax(rnorm(n, if (val) 70 else 66, 12), 25), 95))
  sexo   <- sample(c("Mujer", "Hombre"), n, TRUE, prob = c(0.45, 0.55))
  dm     <- rbinom(n, 1, plogis(-0.3 + 0.01 * (edad - 66)))
  ecv    <- rbinom(n, 1, plogis(-1.3 + 0.04 * (edad - 66) + 0.4 * dm))
  tfg    <- round(pmin(pmax(rnorm(n, if (val) 30 else 27, 8), 5), 44), 1)
  luacr  <- rnorm(n, 5.6 + 0.6 * dm - 0.025 * (tfg - 27), 1.3)
  luacr  <- pmin(luacr, log(6000))                       # tope fisiológico razonable
  uacr   <- round(exp(luacr))
  albumina    <- round(pmin(pmax(rnorm(n, 4.0 - 0.01 * (edad - 66) - 0.15 * dm - 0.012 * (30 - tfg) , 0.4), 2), 5), 1)
  fosforo     <- round(pmin(pmax(rnorm(n, 3.7 - 0.03 * (tfg - 27), 0.6), 2), 8), 1)
  bicarbonato <- round(pmin(pmax(rnorm(n, 23 + 0.07 * (tfg - 27), 3), 12), 32), 0)
  hombre <- as.integer(sexo == "Hombre")

  # Riesgos instantáneos mensuales (por causa)
  lp_krt <- 0.095 * (27 - tfg) + 0.45 * (luacr - 5.6) - 0.020 * (edad - 66) + 0.25 * (fosforo - 3.7) -
            0.06 * (bicarbonato - 23) - 0.30 * (albumina - 4) + 0.10 * hombre
  lp_dth <- 0.055 * (edad - 66) + 0.35 * dm + 0.50 * ecv - 0.55 * (albumina - 4) + 0.015 * (27 - tfg) + 0.20 * hombre

  base_krt <- 0.0040; base_dth <- 0.0027

  estado <- integer(n); tiempo <- rep(meses_max, n)
  vivo <- rep(TRUE, n)
  for (m in seq_len(meses_max)) {
    b_dm <- ifelse(m <= 18, 0.95, 0.00)                   # efecto de la diabetes cambia con el tiempo
    h_krt <- base_krt * exp(lp_krt + b_dm * dm)
    h_dth <- base_dth * exp(lp_dth)
    u <- runif(n)
    ev_k <- vivo & (u < h_krt)
    ev_d <- vivo & !ev_k & (u < h_krt + h_dth)
    estado[ev_k] <- 1L; estado[ev_d] <- 2L
    tiempo[ev_k | ev_d] <- m
    vivo <- vivo & !(ev_k | ev_d)
  }

  # Censura: entrada escalonada (seguimiento máximo entre 24 y 60 meses) + pérdidas ~1.5 %/año
  # (con censura = FALSE se obtiene la "verdad": todos se siguen hasta el evento o los 60 meses)
  if (censura) {
    seg_max  <- sample(24:meses_max, n, TRUE)
    perdida  <- ceiling(rexp(n, rate = 0.015 / 12))
    cens     <- pmin(seg_max, perdida)
    ya_cens  <- tiempo > cens
    estado[ya_cens] <- 0L
    tiempo[ya_cens] <- cens[ya_cens]
  }

  tibble::tibble(
    id = seq_len(n), edad, sexo, dm, ecv, tfg, uacr, albumina, fosforo, bicarbonato,
    tiempo_meses = tiempo, estado
  )
}

guardar_cohorte_pronostico <- function(carpeta = "Bases") {
  readr::write_csv(simular_cohorte_pronostico(3000, 2026, "desarrollo"), file.path(carpeta, "cohorte_pronostico.csv"))
  readr::write_csv(simular_cohorte_pronostico(1500, 7070, "validacion"), file.path(carpeta, "cohorte_validacion.csv"))
}
