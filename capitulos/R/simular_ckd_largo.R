# ------------------------------------------------------------------------
# COHORTE LONGITUDINAL SIMULADA: TFGe con visitas irregulares, abandono y KRT (Cap. 14)
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS. Sirven para el Cap. 14.3 (pendiente de TFGe con
# modelos mixtos) y 14.6 (trayectorias y modelos conjuntos).
#
# Diseño: 600 pacientes con ERC G3b-G4 seguidos hasta 36 meses. Visitas planeadas
# cada 3 meses, pero irregulares: la fecha real se desvía de la planeada y el 20 % de
# las visitas no ocurre. TFGe en mL/min/1.73 m2.
#
# VERDAD CONOCIDA (t = años desde la basal)
#   TFGe verdadera(t) = b0_i + b1_i * t + dip_i * (1 - exp(-t / 0.2))
#     b0_i = 38 - 0.20*(edad-64) - 3.0*dm - 2.0*(ln UACR - 5.2) + u0_i
#     b1_i = -3.0 + 1.0*tratamiento - 0.45*(ln UACR - 5.2) - 0.6*dm + u1_i
#     dip_i = -3.0 * tratamiento   (descenso inicial "hemodinámico", constante de tiempo ~2.4 meses)
#     (u0_i, u1_i) ~ N(0, DE 7.0 y 2.0), correlación -0.3
#   => Pendiente CRÓNICA (la que importa a largo plazo): el tratamiento la mejora en +1.0 mL/min/año.
#   => Ojo: el descenso inicial hace que una recta simple NO sea el modelo correcto.
#   Medición observada = verdadera + error N(0, 3.0^2).
#
# ABANDONO (MAR): tras cada visita atendida, el paciente se pierde de seguimiento con
#   probabilidad  plogis(-3.7 - 0.07*(TFGe observada - 35) + 0.02*(edad - 64)):
#   depende de lo ya observado (MAR), no del valor futuro.
#
# KRT (inicio de diálisis o trasplante), riesgo instantáneo (por año) que depende de la TFGe
# VERDADERA actual (parámetro de asociación alfa):
#   h(t) = 0.10 * exp(alfa * (TFGe verdadera(t) - 25)),   alfa = -0.10 por mL/min
#   => HR = exp(1.0) = 2.72 por cada 10 mL/min MENOS. Tras la KRT ya no hay mediciones.
# ------------------------------------------------------------------------

parametros_ckd_largo <- function() {
  list(alfa = -0.10, h0 = 0.10, tfg_ref = 25, dip = -3.0, tau_dip = 0.2,
       pend_base = -3.0, efecto_pend_trat = 1.0, sd_error = 3.0)
}

simular_ckd_largo <- function(n = 600, semilla = 2029) {
  set.seed(semilla)
  P <- parametros_ckd_largo()
  edad <- round(pmin(pmax(rnorm(n, 64, 11), 30), 88))
  sexo <- sample(c("Mujer", "Hombre"), n, TRUE, prob = c(0.42, 0.58))
  dm <- rbinom(n, 1, 0.45)
  luacr <- rnorm(n, 5.2 + 0.4 * dm, 1.1)
  uacr <- round(exp(luacr))
  trat <- rbinom(n, 1, plogis(-0.6 + 0.5 * dm + 0.30 * (luacr - 5.2)))

  z1 <- rnorm(n); z2 <- rnorm(n)
  u0 <- 7.0 * z1
  u1 <- 2.0 * (-0.3 * z1 + sqrt(1 - 0.3^2) * z2)
  b0 <- 38 - 0.20 * (edad - 64) - 3.0 * dm - 2.0 * (luacr - 5.2) + u0
  b1 <- P$pend_base + P$efecto_pend_trat * trat - 0.45 * (luacr - 5.2) - 0.6 * dm + u1
  dip <- P$dip * trat
  tfg_ver <- function(i, t) b0[i] + b1[i] * t + dip[i] * (1 - exp(-t / P$tau_dip))   # t en años

  # ---- Evento KRT: se simula en una rejilla fina (cada 0.5 mes) ----
  paso <- 0.5 / 12
  rejilla <- seq(paso, 3, by = paso)
  t_krt <- rep(Inf, n)
  for (i in seq_len(n)) {
    h <- P$h0 * exp(pmin(P$alfa * (tfg_ver(i, rejilla) - P$tfg_ref), 6))
    ev <- runif(length(rejilla)) < 1 - exp(-h * paso)
    if (any(ev)) t_krt[i] <- rejilla[which(ev)[1]] * 12                       # en meses
  }

  # ---- Visitas: irregulares, con abandono MAR ----
  filas <- vector("list", n); salida <- character(n); t_fin <- numeric(n)
  for (i in seq_len(n)) {
    planeadas <- seq(0, 36, by = 3)
    reales <- c(0, pmax(planeadas[-1] + rnorm(length(planeadas) - 1, 0, 0.6), 0.2))
    atiende <- c(TRUE, runif(length(planeadas) - 1) > 0.20)
    mes_i <- numeric(0); tfg_i <- numeric(0)
    salida[i] <- "Fin del estudio"; t_fin[i] <- 36
    for (k in seq_along(planeadas)) {
      if (reales[k] >= t_krt[i]) { salida[i] <- "KRT"; t_fin[i] <- t_krt[i]; break }
      if (!atiende[k]) next
      y <- tfg_ver(i, reales[k] / 12) + rnorm(1, 0, P$sd_error)
      y <- max(round(y, 1), 3)
      mes_i <- c(mes_i, round(reales[k], 2)); tfg_i <- c(tfg_i, y)
      if (k > 1 && runif(1) < plogis(-3.7 - 0.07 * (y - 35) + 0.02 * (edad[i] - 64))) {
        if (reales[k] < t_krt[i]) { salida[i] <- "Abandono"; t_fin[i] <- reales[k] }
        break
      }
    }
    if (salida[i] == "Fin del estudio" && t_krt[i] <= 36) { salida[i] <- "KRT"; t_fin[i] <- t_krt[i] }
    filas[[i]] <- tibble::tibble(id = i, mes = mes_i, tfge = tfg_i)
  }
  largo <- dplyr::bind_rows(filas) |>
    dplyr::group_by(id) |> dplyr::mutate(visita = dplyr::row_number() - 1L) |> dplyr::ungroup() |>
    dplyr::select(id, visita, mes, tfge)

  pacientes <- tibble::tibble(
    id = 1:n, edad, sexo, dm, uacr, tratamiento = trat,
    tiempo_seg_meses = round(t_fin, 2), krt = as.integer(salida == "KRT"), salida,
    n_visitas = as.integer(table(factor(largo$id, levels = 1:n)))
  )
  oraculo <- tibble::tibble(id = 1:n, b0 = round(b0, 3), b1 = round(b1, 3), dip = dip,
                            tfg_ver_36 = round(sapply(1:n, tfg_ver, t = 3), 3),
                            tfg_ver_0 = round(b0, 3))
  list(pacientes = pacientes, largo = largo, oraculo = oraculo)
}

guardar_ckd_largo <- function(carpeta = "Bases") {
  x <- simular_ckd_largo()
  readr::write_csv(x$pacientes, file.path(carpeta, "ckd_largo_pacientes.csv"))
  readr::write_csv(x$largo, file.path(carpeta, "ckd_largo.csv"))
  readr::write_csv(x$oraculo, file.path(carpeta, "ckd_largo_oraculo.csv"))
  invisible(x)
}
