# ------------------------------------------------------------------------
# Simuladores del Cap. 8.7 (ordinal, multinomial y conteos con sobredispersion)
# TODOS los datos son SIMULADOS (no provienen de pacientes reales) y la verdad
# (los parametros) es conocida, para comprobar si cada modelo la recupera.
# ------------------------------------------------------------------------

# ---- 1) Albuminuria a 12 meses (A1/A2/A3), desenlace ORDINAL --------------
# P(Y >= A2) = plogis(a2 + lp + b1 * isglt2);  P(Y >= A3) = plogis(a3 + lp + b2 * isglt2)
# Si b1 == b2 se cumple el supuesto de odds proporcionales (OR comun = exp(b1)).
# Si b1 != b2 el supuesto es FALSO (el farmaco actua distinto en cada corte).
# El tratamiento depende solo de covariables medidas: no hay confusion no medida.
# Probabilidades verdaderas (A1, A2, A3) de cada fila de `d` si TODOS reciben `trat` (0 o 1)
prob_alb_verdad <- function(d, trat, beta_trat = c(-0.7, -0.7)) {
  lp <- 0.35 * (d$hba1c - 7.8) + 0.02 * (d$pas - 135) + 0.40 * d$ecv + 0.01 * (d$edad - 64)
  p_ge2 <- plogis(0.4  + lp + beta_trat[1] * trat)
  p_ge3 <- pmin(plogis(-1.2 + lp + beta_trat[2] * trat), p_ge2)
  cbind(A1 = 1 - p_ge2, A2 = p_ge2 - p_ge3, A3 = p_ge3)
}

simular_albuminuria <- function(n = 2000, semilla = 87, beta_trat = c(-0.7, -0.7)) {
  set.seed(semilla)
  edad  <- round(pmin(pmax(rnorm(n, 64, 10), 30), 90))
  hba1c <- round(pmin(pmax(rnorm(n, 7.8, 1.2), 5.5), 12), 1)
  pas   <- round(pmin(pmax(rnorm(n, 135, 15), 95), 200))
  ecv   <- rbinom(n, 1, 0.30)
  isglt2 <- rbinom(n, 1, plogis(-0.2 + 0.25 * (hba1c - 7.8) + 0.02 * (pas - 135) + 0.3 * ecv))
  d <- tibble::tibble(id = 1:n, edad, hba1c, pas, ecv, isglt2)
  p <- prob_alb_verdad(d, trat = isglt2, beta_trat = beta_trat)
  u <- runif(n)
  cat_num <- 1 + (u > p[, "A1"]) + (u > p[, "A1"] + p[, "A2"])
  d$alb12 <- factor(c("A1", "A2", "A3")[cat_num], levels = c("A1", "A2", "A3"), ordered = TRUE)
  d
}

# ---- 2) Modalidad de terapia renal sustitutiva (TRS), desenlace NOMINAL ----
# Pacientes con ERC G5 que eligen/reciben: hemodialisis (referencia), dialisis
# peritoneal o manejo conservador. Logit multinomial con HD como referencia.
modalidades_trs <- c("Hemodiálisis", "Diálisis peritoneal", "Manejo conservador")

# Coeficientes verdaderos (filas: DP y MC frente a HD)
coef_trs_verdad <- function() {
  rbind(`Diálisis peritoneal` = c(int = -1.0, edad10 = -0.20, comorb = -0.15, lejos = 1.10, autonomo = 0.60),
        `Manejo conservador`  = c(int = -1.5, edad10 =  0.80, comorb =  0.35, lejos = 0.50, autonomo = -0.70))
}

# Probabilidades verdaderas de cada modalidad para cada fila de `d` (columnas: modalidades_trs)
prob_trs_verdad <- function(d) {
  B <- coef_trs_verdad()
  X <- cbind(1, (d$edad - 70) / 10, d$comorb, d$lejos, d$autonomo)
  eta <- cbind(0, X %*% t(B))
  p <- exp(eta) / rowSums(exp(eta))
  colnames(p) <- modalidades_trs
  p
}

simular_trs <- function(n = 1500, semilla = 87) {
  set.seed(semilla)
  edad     <- round(pmin(pmax(rnorm(n, 70, 12), 30), 95))
  comorb   <- rpois(n, pmax(2.5 + 0.04 * (edad - 70), 0.3))        # indice de comorbilidad (conteo)
  lejos    <- rbinom(n, 1, 0.30)                                    # vive a >1 h de un centro de HD
  autonomo <- rbinom(n, 1, plogis(1 - 0.04 * (edad - 70)))          # autonomia + apoyo para tecnica domiciliaria
  d <- tibble::tibble(id = 1:n, edad, comorb, lejos, autonomo)
  p <- prob_trs_verdad(d)
  k <- apply(p, 1, \(pp) sample.int(3, 1, prob = pp))
  d$modalidad <- factor(modalidades_trs[k], levels = modalidades_trs)
  d
}

# ---- 3) Ingresos hospitalarios en el primer ano de hemodialisis (CONTEO) ---
# Tasa = 1.40 * 0.70^programa * 1.30^diabetes * fragilidad (gamma, media 1) por paciente-ano.
# Verdad: IRR del programa = 0.70, IRR de diabetes = 1.30.
# Con `prop_ceros > 0` una fraccion de pacientes nunca ingresa (ceros "estructurales").
simular_ingresos <- function(n = 600, semilla = 87, theta = 1.2, prop_ceros = 0) {
  set.seed(semilla)
  programa <- rbinom(n, 1, 0.5)
  diabetes <- rbinom(n, 1, 0.45)
  seguimiento_anios <- round(runif(n, 0.3, 1), 2)
  fragilidad <- rgamma(n, shape = theta, rate = theta)             # media 1, varianza 1/theta
  tasa <- 1.40 * 0.70^programa * 1.30^diabetes * fragilidad
  ingresos <- rpois(n, tasa * seguimiento_anios)
  ingresos[rbinom(n, 1, prop_ceros) == 1] <- 0L
  tibble::tibble(id = 1:n, programa, diabetes, seguimiento_anios, ingresos)
}
