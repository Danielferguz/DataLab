# ------------------------------------------------------------------------
# SERIE SIMULADA de inicios mensuales de diálisis en dos regiones (Cap. 15)
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS. Sirven para el capítulo de series de tiempo:
# componentes (15.1), modelos de conteo (15.2), serie temporal interrumpida
# (15.3), autocorrelación y ARIMA (15.4) y serie con grupo control (15.5).
#
# Diseño: 96 meses (enero 2017 a diciembre 2024) en dos regiones:
#   - "Intervenida": en el mes 49 (enero 2021) empieza un programa regional de
#                    detección temprana de ERC y tratamiento nefroprotector.
#   - "Control":     sin programa.
#
# Modelo generador (escala log), para la región r y el mes t = 1..96:
#   log mu_rt = log(poblacion_rt) + a_r                       # tasa basal (offset = población)
#             + 0.004 * (t - 1)                               # tendencia: +0.4 % mensual (~ +4.9 % anual)
#             + 0.10 * cos(2 pi (mes - 7) / 12)               # estacionalidad: pico en julio, amplitud 10 %
#             - 0.15 * ola_covid                              # ola COVID-19: meses 39-46 (mar-oct 2020), -14 %
#             + politica_t * (-0.10 + (-0.005) * (t - 49))    # SOLO región intervenida
#   donde politica_t = 1 si t >= 49, y t - 49 = 0 en el primer mes del programa.
#   casos_rt ~ Poisson( mu_rt * exp(u_rt) ),  u_rt = AR(1) con phi = 0.8 y DE marginal 0.07
#   (el ruido AR(1) produce a la vez autocorrelación y sobredispersión).
#   La semilla 1398 se eligió entre 1500 candidatas porque, con ella, los estimadores
#   (serie interrumpida y serie con control) quedan cerca de la verdad, las
#   tendencias previas de ambas regiones son paralelas en la muestra y la
#   autocorrelación residual es visible (para que el fenómeno sea claro al
#   enseñarlo). Con otras semillas el ruido puede producir tendencias previas
#   aparentemente distintas aunque la verdad sea paralela; en promedio sobre
#   semillas los estimadores son insesgados.
#
# VERDAD CONOCIDA (efecto del programa, solo región intervenida):
#   - Cambio de nivel inmediato (mes 49):   beta_nivel     = -0.10  -> RR = 0.905 (-9.5 %)
#   - Cambio de pendiente (por mes):        beta_pendiente = -0.005 -> RR mensual = 0.995
#     (la tendencia pasa de +0.4 %/mes a -0.1 %/mes: el aumento se detiene).
#   - Efecto acumulado k meses después del inicio: exp(-0.10 - 0.005 * k)
#         12 meses: 0.85 | 24 meses: 0.80 | 47 meses (dic 2024): 0.72
# Tendencias PARALELAS antes de la política (misma tendencia, misma estacionalidad,
# mismo choque COVID en ambas regiones); lo que difiere es el nivel basal y la población.
#
# Archivos:
#   serie_inicio_dialisis.csv          lo que ve el investigador
#   serie_inicio_dialisis_oraculo.csv  + esperado con y sin programa (verdad)
# ------------------------------------------------------------------------

simular_serie_inicio_dialisis <- function(semilla = 1398, n_meses = 96, mes_politica = 49) {
  set.seed(semilla)
  t       <- seq_len(n_meses)
  fecha   <- seq(as.Date("2017-01-01"), by = "month", length.out = n_meses)
  mes     <- as.integer(format(fecha, "%m"))
  anio    <- as.integer(format(fecha, "%Y"))
  ola     <- as.integer(t >= 39 & t <= 46)           # ola COVID-19 (mar-oct 2020)
  post    <- as.integer(t >= mes_politica)
  t_post  <- pmax(0, t - mes_politica)                 # 0 en el primer mes del programa

  una_region <- function(region, tasa_base_100mil, pob0, crec_pob_anual, intervenida) {
    poblacion <- round(pob0 * (1 + crec_pob_anual)^((t - 1) / 12))
    efecto    <- if (intervenida) post * (-0.10 - 0.005 * t_post) else 0
    lp_cf     <- log(poblacion) + log(tasa_base_100mil / 1e5) + 0.004 * (t - 1) +
                 0.10 * cos(2 * pi * (mes - 7) / 12) - 0.15 * ola
    mu_cf     <- exp(lp_cf)                              # esperado SIN programa
    mu        <- exp(lp_cf + efecto)                     # esperado CON programa (si corresponde)
    # Ruido AR(1) multiplicativo (DE marginal 0.09, phi = 0.5)
    phi <- 0.8; de_marg <- 0.07
    u <- numeric(n_meses); u[1] <- rnorm(1, 0, de_marg)
    for (i in 2:n_meses) u[i] <- phi * u[i - 1] + rnorm(1, 0, de_marg * sqrt(1 - phi^2))
    casos <- rpois(n_meses, mu * exp(u))
    tibble::tibble(region = region, mes_num = t, fecha = fecha, anio = anio, mes = mes,
                   casos = casos, poblacion = poblacion, politica = as.integer(intervenida) * post,
                   ola_covid = ola, mu_esperado = mu, mu_contrafactual = mu_cf)
  }

  # Tasas basales (por 100 000 habitantes por MES): 3.0 -> ~36 por 100 000 al año
  dplyr::bind_rows(
    una_region("Intervenida", 3.0, 5.0e6, 0.005, TRUE),
    una_region("Control",     2.8, 4.0e6, 0.003, FALSE)
  )
}

# Para regenerar los archivos CSV de la carpeta Bases/ (ejecutar a mano):
#   source("capitulos/R/simular_serie_inicio_dialisis.R"); guardar_serie_inicio_dialisis("capitulos/Bases")
guardar_serie_inicio_dialisis <- function(carpeta = "Bases") {
  d <- simular_serie_inicio_dialisis()
  oraculo <- c("mu_esperado", "mu_contrafactual")
  readr::write_csv(dplyr::select(d, -dplyr::all_of(oraculo)),
                   file.path(carpeta, "serie_inicio_dialisis.csv"))
  readr::write_csv(d, file.path(carpeta, "serie_inicio_dialisis_oraculo.csv"))
  invisible(d)
}
