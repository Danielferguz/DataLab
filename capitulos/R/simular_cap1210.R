# ------------------------------------------------------------------------
# Datos SIMULADOS para el subcapítulo 12.10 (PS con encuestas, faltantes y 3 tratamientos)
# ------------------------------------------------------------------------
# Tres simuladores, cada uno con VERDAD CONOCIDA y SIN confusión no medida:
# así, si un método no acierta, el fallo es del método y no de lo que no vemos.
#   1. simular_poblacion_ps()  : región con 400 comunidades; iSGLT2 (sí/no) y pendiente de TFGe.
#                                El efecto es MAYOR en la zona rural -> el efecto de la muestra
#                                (sobremuestreo rural) no es el efecto de la población.
#   2. simular_ps_faltantes()  : cohorte clínica con UACR y HbA1c faltantes (MAR, dependen
#                                también del tratamiento y del desenlace).
#   3. simular_tres_trat()     : exposición de tres niveles (Ninguno / aGLP-1 / iSGLT2).
# Requiere tidyverse y R/simular_encuesta_erc.R (para muestrear_bietapico()).
# ------------------------------------------------------------------------

# ---- 1. Población con diseño muestral --------------------------------------
simular_poblacion_ps <- function(semilla = 1210) {
  set.seed(semilla)
  n_cl <- 400
  cl <- tibble::tibble(
    cluster = 1:n_cl,
    area = c(rep("Urbano", 240), rep("Rural", 160)),
    tam = rpois(n_cl, ifelse(area == "Urbano", 150, 90)) + 30,
    u_trat = rnorm(n_cl, 0, 0.7),     # costumbre de prescripción de la comunidad
    u_y = rnorm(n_cl, 0, 0.6),        # variación de la pendiente entre comunidades
    u_ite = rnorm(n_cl, 0, 0.4)       # el beneficio también varía entre comunidades
  )
  pob <- cl[rep(seq_len(n_cl), cl$tam), ] |>
    dplyr::mutate(
      id = dplyr::row_number(),
      rural = as.integer(area == "Rural"),
      edad = round(pmin(pmax(rnorm(dplyr::n(), 66 - 2 * rural, 9), 40), 90)),
      sexo = sample(c("F", "M"), dplyr::n(), TRUE),
      tfg = round(pmin(pmax(rnorm(dplyr::n(), 48 - 0.3 * (edad - 66), 9), 20), 75), 1),
      luacr = round(rnorm(dplyr::n(), 5 + 0.3 * rural, 1), 2),
      p_trat = plogis(-0.4 + 0.04 * (tfg - 48) + 0.5 * (luacr - 5) - 0.04 * (edad - 66) - 0.7 * rural + u_trat),
      isglt2 = rbinom(dplyr::n(), 1, p_trat),
      y0 = -2.4 - 0.4 * (luacr - 5) - 0.03 * (edad - 66) + 0.02 * (tfg - 48) - 0.3 * rural + u_y + rnorm(dplyr::n()),
      ite = 0.7 + 0.8 * rural + 0.3 * (luacr - 5) + u_ite + rnorm(dplyr::n(), 0, 0.3),
      y1 = y0 + ite,
      pendiente_tfg = round(ifelse(isglt2 == 1, y1, y0), 2)
    )
  pob
}

# Columnas que un investigador NO ve (oráculo) y que se quitan de la muestra
cols_oraculo_ps <- c("u_trat", "u_y", "u_ite", "rural", "p_trat", "y0", "y1", "ite")

guardar_ps_encuesta <- function(carpeta = "Bases") {
  pob <- simular_poblacion_ps()
  m <- muestrear_bietapico(pob, conglom = c(Urbano = 20, Rural = 40), por_cluster = 40, semilla = 1211)
  m <- m |> dplyr::select(-dplyr::all_of(cols_oraculo_ps), -tam.x) |> dplyr::rename(tam = tam.y)
  readr::write_csv(dplyr::select(m, id, cluster, area, peso, tam, N_conglom, edad, sexo, tfg, luacr, isglt2, pendiente_tfg),
                   file.path(carpeta, "ps_encuesta_erc.csv"))
  invisible(m)
}

# ---- 2. Cohorte con datos faltantes ----------------------------------------
# Devuelve la base completa (con oráculo); `con_faltantes()` borra valores.
simular_ps_completa <- function(n = 3000, semilla = 1212) {
  set.seed(semilla)
  edad <- round(pmin(pmax(rnorm(n, 64, 10), 30), 90))
  sexo <- ifelse(rbinom(n, 1, 0.55) == 1, "Hombre", "Mujer")
  hba1c <- round(pmin(pmax(rnorm(n, 7.8, 1.2), 5.5), 12), 1)
  tfg_basal <- round(pmin(pmax(rnorm(n, 46 - 0.30 * (edad - 64), 9), 20), 75), 1)
  luacr <- 4.8 + 0.25 * (hba1c - 7.8) - 0.015 * (tfg_basal - 46) + rnorm(n)
  ecv <- rbinom(n, 1, plogis(-1.2 + 0.04 * (edad - 64) + 0.3 * (sexo == "Hombre")))
  ps <- plogis(-0.35 + 0.045 * (tfg_basal - 46) + 0.55 * (luacr - 4.8) + 0.15 * (hba1c - 7.8) +
               0.30 * ecv - 0.045 * (edad - 64))
  isglt2 <- rbinom(n, 1, ps)
  y0 <- -2.3 - 0.60 * (luacr - 4.8) - 0.030 * (edad - 64) + 0.020 * (tfg_basal - 46) -
        0.25 * (hba1c - 7.8) - 0.50 * ecv + rnorm(n)
  ite <- 0.9 + 0.3 * (luacr - 4.8)
  y1 <- y0 + ite
  tibble::tibble(id = 1:n, edad, sexo, hba1c, tfg_basal, luacr = round(luacr, 2), ecv, isglt2,
                 pendiente_tfg = round(ifelse(isglt2 == 1, y1, y0), 2), ite = round(ite, 3))
}

# Faltantes MAR: la UACR falta (~35 %) según edad, TFGe, tratamiento y DESENLACE; la HbA1c (~10 %) según edad y desenlace
con_faltantes <- function(d, semilla = 1213) {
  set.seed(semilla)
  lp <- -0.7 + 0.03 * (d$edad - 64) - 0.04 * (d$tfg_basal - 46) + 0.8 * d$isglt2 - 0.9 * (d$pendiente_tfg + 2)
  a <- uniroot(\(a) mean(plogis(lp + a)) - 0.35, c(-4, 4))$root
  d$luacr[rbinom(nrow(d), 1, plogis(lp + a)) == 1] <- NA
  lp2 <- -2.3 + 0.02 * (d$edad - 64) - 0.5 * (d$pendiente_tfg + 2)
  d$hba1c[rbinom(nrow(d), 1, plogis(lp2)) == 1] <- NA
  d
}

# ---- 3. Exposición de tres niveles -----------------------------------------
simular_tres_trat <- function(n = 4000, semilla = 1214) {
  set.seed(semilla)
  edad <- round(pmin(pmax(rnorm(n, 64, 10), 30), 90))
  sexo <- ifelse(rbinom(n, 1, 0.55) == 1, "Hombre", "Mujer")
  imc <- round(pmin(pmax(rnorm(n, 31, 5), 19), 50), 1)
  hba1c <- round(pmin(pmax(rnorm(n, 7.8, 1.2), 5.5), 12), 1)
  tfg_basal <- round(pmin(pmax(rnorm(n, 46 - 0.30 * (edad - 64), 9), 20), 75), 1)
  luacr <- round(4.8 + 0.25 * (hba1c - 7.8) - 0.015 * (tfg_basal - 46) + rnorm(n), 2)
  ecv <- rbinom(n, 1, plogis(-1.2 + 0.04 * (edad - 64) + 0.3 * (sexo == "Hombre")))
  # utilidades respecto de "Ninguno" -> probabilidades multinomiales (softmax)
  u_glp <- -0.9 + 0.10 * (imc - 31) + 0.30 * (hba1c - 7.8) - 0.03 * (edad - 64) + 0.2 * ecv
  u_sglt <- -0.5 + 0.04 * (tfg_basal - 46) + 0.55 * (luacr - 4.8) + 0.3 * ecv - 0.03 * (edad - 64)
  den <- 1 + exp(u_glp) + exp(u_sglt)
  P <- cbind(Ninguno = 1 / den, `aGLP-1` = exp(u_glp) / den, iSGLT2 = exp(u_sglt) / den)
  a <- apply(P, 1, \(p) sample(1:3, 1, prob = p))
  y0 <- -2.3 - 0.45 * (luacr - 4.8) - 0.03 * (edad - 64) + 0.02 * (tfg_basal - 46) -
        0.25 * (hba1c - 7.8) - 0.5 * ecv - 0.03 * (imc - 31) + rnorm(n)
  y_glp <- y0 + 0.4 + 0.1 * (luacr - 4.8)
  y_sglt <- y0 + 0.9 + 0.5 * (luacr - 4.8)
  y <- cbind(y0, y_glp, y_sglt)
  tibble::tibble(id = 1:n, edad, sexo, imc, hba1c, tfg_basal, luacr, ecv,
                 trat = factor(a, 1:3, c("Ninguno", "aGLP-1", "iSGLT2")),
                 pendiente_tfg = round(y[cbind(1:n, a)], 2),
                 y_ninguno = y0, y_glp1 = y_glp, y_isglt2 = y_sglt)
}
