# ------------------------------------------------------------------------
# Base SIMULADA para el Cap. 16 (diagnóstico): biomarcador urinario y lesión
# renal aguda (LRA) en pacientes hospitalizados
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS. No representan valores de ningún kit real.
#
# Escenario: 2 000 adultos hospitalizados (30 % en UCI, 70 % en sala). A todos se
# les mide, al ingreso, un biomarcador urinario de daño tubular (tipo NGAL, ng/mL,
# distribución asimétrica a la derecha) y se desea saber si desarrollarán una LRA
# KDIGO >= estadio 2 en las siguientes 72 h (estándar de referencia: creatinina seriada).
#
# VERDAD CONOCIDA (documentada para poder comprobar los métodos)
#  - Prevalencia de LRA estadio >= 2: ~ 17 % global (UCI ~ 30 %, sala ~ 11 %).
#  - Riesgo clínico (logit): edad (+0.025 por año), diabetes (+0.5), sepsis (+1.0),
#    nefrotóxicos (+0.8), ln(creatinina de ingreso) (+1.2), UCI (+0.9).
#  - Estadio de la LRA: entre los casos, 60 % estadio 2 y 40 % estadio 3 en sala;
#    40 % y 60 % en UCI (la UCI tiene casos más graves: espectro). Entre los no
#    casos, ~12 % tiene LRA estadio 1 (leve; cuenta como "no caso").
#  - ln(biomarcador) = ln(55) + desplazamiento por estadio (0: 0; 1: +0.30; 2: +0.95; 3: +1.45)
#                      + 0.40 * sepsis + 0.15 * diabetes + 0.10 * UCI + ruido N(0, 0.60)
#    La sepsis eleva el biomarcador SIN lesión renal (fuente de falsos positivos).
#  - Verificación parcial (para ilustrar el sesgo de verificación): no todos los
#    pacientes completan la creatinina seriada que define el estándar de referencia.
#    P(verificado) = plogis(-0.6 + 1.3 * z_ln(biomarcador) + 0.5 * sepsis + 0.4 * nefrotoxicos);
#    depende de lo que ya se sabe (biomarcador, clínica), NO del estado verdadero de LRA
#    una vez conocidos esos datos (verificación "ignorable" o MAR dado el resultado de la prueba).
#
# Columnas: id, entorno ("UCI"/"Sala"), edad, sexo, diabetes, sepsis, nefrotoxicos,
#   creat_ingreso (mg/dL), ngal (ng/mL, biomarcador simulado), estadio_kdigo (0-3),
#   lra (1 = KDIGO >= 2, la verdad completa que solo se conoce porque es una simulación),
#   verificado (0/1) y lra_obs (lra si verificado; NA si no).
# ------------------------------------------------------------------------

simular_dx_lra <- function(n = 2000, semilla = 2028) {
  set.seed(semilla)
  uci  <- rbinom(n, 1, 0.30)
  edad <- round(pmin(pmax(rnorm(n, 62 + 3 * uci, 15), 18), 95))
  sexo <- sample(c("Mujer", "Hombre"), n, TRUE, prob = c(0.45, 0.55))
  diabetes     <- rbinom(n, 1, plogis(-1.1 + 0.015 * (edad - 62)))
  sepsis       <- rbinom(n, 1, ifelse(uci == 1, 0.42, 0.10))
  nefrotoxicos <- rbinom(n, 1, ifelse(uci == 1, 0.30, 0.20))
  creat <- exp(rnorm(n, log(0.95) + 0.004 * (edad - 62) + 0.10 * diabetes + 0.15 * sepsis + 0.05 * uci, 0.30))
  creat <- round(pmin(pmax(creat, 0.4), 6), 2)

  lp <- -2.75 + 0.020 * (edad - 62) + 0.4 * diabetes + 0.9 * sepsis + 0.7 * nefrotoxicos +
        1.0 * log(creat / 0.95) + 0.8 * uci
  lra <- rbinom(n, 1, plogis(lp))

  # Estadio KDIGO: casos = 2 o 3; no casos = 0 o 1 (leve)
  p3 <- ifelse(uci == 1, 0.60, 0.40)
  estadio <- ifelse(lra == 1, ifelse(runif(n) < p3, 3L, 2L), ifelse(runif(n) < 0.12, 1L, 0L))

  desplaza <- c(`0` = 0, `1` = 0.30, `2` = 0.90, `3` = 1.40)[as.character(estadio)]
  ln_bio <- log(55) + unname(desplaza) + 0.45 * sepsis + 0.15 * diabetes + 0.10 * uci + rnorm(n, 0, 0.72)
  ngal <- round(exp(ln_bio), 1)

  # Verificación parcial: depende del biomarcador y de la clínica, no del estado verdadero
  z <- as.numeric(scale(log(ngal)))
  p_ver <- plogis(-0.6 + 1.3 * z + 0.5 * sepsis + 0.4 * nefrotoxicos)
  verificado <- rbinom(n, 1, p_ver)

  tibble::tibble(
    id = seq_len(n), entorno = ifelse(uci == 1, "UCI", "Sala"), edad, sexo, diabetes, sepsis,
    nefrotoxicos, creat_ingreso = creat, ngal, estadio_kdigo = estadio, lra,
    verificado, lra_obs = ifelse(verificado == 1, lra, NA_integer_)
  )
}

guardar_dx_lra <- function(carpeta = "Bases") {
  readr::write_csv(simular_dx_lra(), file.path(carpeta, "dx_lra.csv"))
}
