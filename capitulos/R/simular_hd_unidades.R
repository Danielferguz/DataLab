# ------------------------------------------------------------------------
# UNIDADES DE HEMODIÁLISIS SIMULADAS: pacientes dentro de unidades (Cap. 14)
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS. Sirven para el Cap. 14 (datos agrupados y
# longitudinales): ICC, efectos aleatorios y benchmarking de centros, GEE vs.
# modelos mixtos y ensayos por conglomerados.
#
# Diseño: 40 unidades de hemodiálisis, 25-60 pacientes por unidad (≈1 700 en total),
# hemoglobina (Hb, g/dL) mensual durante 12 meses y hospitalización (sí/no) a 12 meses.
# Una intervención A NIVEL DE UNIDAD (protocolo de ajuste de EPO/hierro guiado por
# algoritmo) se asigna al azar a 20 de las 40 unidades, estratificada por tipo de unidad.
#
# VERDAD CONOCIDA
#  Hemoglobina (g/dL), mes m = 1..12:
#    hb = 10.7 + 0.40*intervencion                       <- efecto de la intervención: +0.40
#         - 0.30*mujer - 0.20*dm - 0.35*cateter + 0.50*(albumina - 3.7)
#         - 0.10*(tipo = Hospitalaria)                   <- efecto de unidad (covariable de unidad)
#         + bW*(edad - edad_media_unidad)                <- efecto DENTRO de la unidad: bW = -0.030 por año
#         + bB*(edad_media_unidad - 64)                  <- efecto ENTRE unidades:      bB = +0.040 por año
#         + u_j + b_i + e_ij(m)
#    u_j ~ N(0, 0.35^2)  (intercepto aleatorio de unidad)
#    b_i ~ N(0, 0.80^2)  (intercepto aleatorio de paciente)
#    e_ij(m): AR(1) con rho = 0.5 y DE = 0.65 (variabilidad mensual dentro del paciente)
#    => ICC de unidad en la Hb ~ 0.35^2 / (0.35^2 + 0.80^2 + 0.65^2) ~ 0.10
#  Hospitalización a 12 meses (binaria):
#    logit P = -1.30 + 0.020*(edad-64) + 0.35*dm + 0.45*cateter - 0.60*(albumina-3.7)
#              + 0.10*(razon_pac_enf - 5) - 0.50*intervencion + v_j
#    v_j ~ N(0, 0.70^2) => ICC latente de unidad ~ 0.70^2/(0.70^2 + pi^2/3) ~ 0.13
#    Efecto condicional de la intervención: OR = exp(-0.50) = 0.61
#  efecto_unidad_hb (en *_oraculo_unidades.csv) = u_j + 0.40*intervencion - 0.10*(Hospitalaria)
#  + 0.040*(edad media de la unidad - 64): el efecto total de la unidad sobre la Hb.
#  El archivo *_oraculo_pacientes.csv guarda los predictores lineales con y sin
#  intervención (para calcular la verdad MARGINAL) y el *_oraculo_unidades.csv guarda
#  u_j y v_j verdaderos (para evaluar los BLUP).
#
# Faltantes: ~5 % de las mediciones mensuales faltan al azar y ~7 % de los pacientes
# salen de la unidad (trasplante o traslado) en un mes aleatorio; la salida es más
# probable a mayor edad (MAR).
# ------------------------------------------------------------------------

simular_hd_unidades <- function(n_unidades = 40, semilla = 2023) {
  set.seed(semilla)
  J <- n_unidades

  # ---- Nivel unidad ----
  unidades <- tibble::tibble(
    unidad = sprintf("U%02d", 1:J),
    n_pac = sample(25:60, J, replace = TRUE),
    tipo_unidad = sample(rep(c("Hospitalaria", "Extrahospitalaria"), length.out = J)),
    razon_pac_enf = round(pmin(pmax(rnorm(J, 5, 0.9), 3.2), 7.5), 1),   # pacientes por enfermera en el turno
    edad_base = rnorm(J, 0, 5),                                           # diferencia de edad promedio entre unidades
    u_hb = rnorm(J, 0, 0.35),
    v_hosp = rnorm(J, 0, 0.70)
  )
  # Aleatorización estratificada por tipo de unidad (mitad y mitad dentro de cada tipo)
  unidades$intervencion <- 0L
  for (tp in unique(unidades$tipo_unidad)) {
    idx <- which(unidades$tipo_unidad == tp)
    unidades$intervencion[idx] <- sample(rep(0:1, length.out = length(idx)))
  }

  # ---- Nivel paciente ----
  pac <- unidades[rep(seq_len(J), unidades$n_pac), ] |>
    dplyr::mutate(id = dplyr::row_number(), .before = 1)
  n <- nrow(pac)
  pac$edad <- round(pmin(pmax(rnorm(n, 64 + pac$edad_base, 11), 25), 92))
  pac$sexo <- sample(c("Mujer", "Hombre"), n, TRUE, prob = c(0.40, 0.60))
  pac$dm <- rbinom(n, 1, plogis(-0.4 + 0.02 * (pac$edad - 64)))
  pac$cateter <- rbinom(n, 1, plogis(-0.7 + 0.015 * (pac$edad - 64) + 0.2 * (pac$tipo_unidad == "Hospitalaria")))
  pac$albumina <- round(pmin(pmax(rnorm(n, 3.7 - 0.15 * pac$dm - 0.25 * pac$cateter, 0.40), 2.2), 4.8), 1)
  pac$b_i <- rnorm(n, 0, 0.80)

  # Edad promedio REAL de cada unidad (efecto entre unidades vs. dentro)
  pac <- pac |>
    dplyr::group_by(unidad) |>
    dplyr::mutate(edad_media_unidad = mean(edad)) |>
    dplyr::ungroup()

  mu_hb <- 10.7 - 0.30 * (pac$sexo == "Mujer") - 0.20 * pac$dm - 0.35 * pac$cateter +
    0.50 * (pac$albumina - 3.7) - 0.10 * (pac$tipo_unidad == "Hospitalaria") +
    (-0.030) * (pac$edad - pac$edad_media_unidad) + 0.040 * (pac$edad_media_unidad - 64) +
    pac$u_hb + pac$b_i

  # ---- Hemoglobina mensual (12 meses) con residuo AR(1) ----
  rho <- 0.5; sd_e <- 0.65
  E <- matrix(0, n, 12)
  E[, 1] <- rnorm(n, 0, sd_e)
  for (m in 2:12) E[, m] <- rho * E[, m - 1] + sqrt(1 - rho^2) * rnorm(n, 0, sd_e)
  HB <- mu_hb + 0.40 * pac$intervencion + E

  # ---- Faltantes: salida de la unidad (MAR por edad) y mediciones perdidas al azar ----
  sale <- rbinom(n, 1, plogis(-3.1 + 0.03 * (pac$edad - 64)))
  mes_salida <- ifelse(sale == 1, sample(4:12, n, TRUE), 13L)
  perdida <- matrix(rbinom(n * 12, 1, 0.05), n, 12) == 1
  for (m in 1:12) HB[mes_salida <= m, m] <- NA
  HB[perdida] <- NA
  HB <- round(HB, 1)

  # ---- Hospitalización a 12 meses ----
  lp_base <- -1.30 + 0.020 * (pac$edad - 64) + 0.35 * pac$dm + 0.45 * pac$cateter -
    0.60 * (pac$albumina - 3.7) + 0.10 * (pac$razon_pac_enf - 5) + pac$v_hosp
  lp0 <- lp_base; lp1 <- lp_base - 0.50
  lp_obs <- ifelse(pac$intervencion == 1, lp1, lp0)
  hosp <- rbinom(n, 1, plogis(lp_obs))

  # ---- Salidas ----
  pacientes <- tibble::tibble(
    id = pac$id, unidad = pac$unidad, edad = pac$edad, sexo = pac$sexo, dm = pac$dm,
    cateter = pac$cateter, albumina = pac$albumina,
    tipo_unidad = pac$tipo_unidad, razon_pac_enf = pac$razon_pac_enf,
    intervencion = pac$intervencion, hospitalizado = hosp, meses_observados = pmin(mes_salida, 12L)
  )
  hb_largo <- tibble::tibble(
    id = rep(pac$id, each = 12), unidad = rep(pac$unidad, each = 12),
    mes = rep(1:12, times = n), hb = as.vector(t(HB))
  ) |> dplyr::filter(!is.na(hb))
  oraculo_pac <- tibble::tibble(id = pac$id, unidad = pac$unidad, b_i = pac$b_i,
                                lp_hosp_sin = lp0, lp_hosp_con = lp1,
                                hb_media_sin_interv = mu_hb) |>
    dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 4)))
  oraculo_uni <- unidades |>
    dplyr::mutate(edad_media_unidad = as.numeric(tapply(pac$edad, pac$unidad, mean)[unidad])) |>
    dplyr::mutate(
      # efecto TOTAL de la unidad sobre la Hb: parte aleatoria + intervención + tipo + contexto de edad
      efecto_unidad_hb = u_hb + 0.40 * intervencion - 0.10 * (tipo_unidad == "Hospitalaria") +
        0.040 * (edad_media_unidad - 64)) |>
    dplyr::select(unidad, u_hb, v_hosp, edad_media_unidad, efecto_unidad_hb) |>
    dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 4)))
  unidades_out <- unidades |> dplyr::select(unidad, tipo_unidad, razon_pac_enf, intervencion)

  list(pacientes = pacientes, hb = hb_largo, unidades = unidades_out,
       oraculo_pacientes = oraculo_pac, oraculo_unidades = oraculo_uni)
}

guardar_hd_unidades <- function(carpeta = "Bases") {
  x <- simular_hd_unidades()
  readr::write_csv(x$pacientes, file.path(carpeta, "hd_unidades.csv"))
  readr::write_csv(x$hb, file.path(carpeta, "hd_unidades_hb.csv"))
  readr::write_csv(x$oraculo_pacientes, file.path(carpeta, "hd_unidades_oraculo_pacientes.csv"))
  readr::write_csv(x$oraculo_unidades, file.path(carpeta, "hd_unidades_oraculo_unidades.csv"))
  invisible(x)
}
