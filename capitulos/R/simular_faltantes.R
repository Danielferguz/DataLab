# ------------------------------------------------------------------------
# Datos faltantes SIMULADOS sobre la base ckd_isglt2
# ------------------------------------------------------------------------
# Parte de la base completa ckd_isglt2 (oráculo) y borra valores con tres
# mecanismos. Como se conoce el valor verdadero, se puede comprobar cuánto sesga
# cada análisis.
#   MCAR: la falta no depende de nada (azar puro)
#   MAR : la falta depende de variables OBSERVADAS (edad, TFGe, tratamiento, desenlace)
#   MNAR: la falta depende del PROPIO valor que falta (UACR alta -> más probable que falte)
# ------------------------------------------------------------------------

generar_faltantes <- function(d, mecanismo = c("MAR", "MCAR", "MNAR"), semilla = 77) {
  mecanismo <- match.arg(mecanismo)
  set.seed(semilla)
  n <- nrow(d)
  lp <- switch(mecanismo,
    MCAR = rep(-0.65, n),
    MAR  = -0.65 + 0.045 * (d$edad - 64) - 0.06 * (d$tfg_basal - 46) + 0.6 * d$isglt2 - 0.30 * d$pendiente_tfg,
    MNAR = -0.65 + 0.9 * (log(d$uacr) - mean(log(d$uacr))) + 0.0 * d$isglt2
  )
  p <- plogis(lp)
  # calibrar el intercepto para ~33 % de faltantes en UACR
  f <- function(a) mean(plogis(lp + a)) - 0.33
  a <- uniroot(f, c(-4, 4))$root
  falta_uacr <- rbinom(n, 1, plogis(lp + a)) == 1
  d$uacr[falta_uacr] <- NA
  # HbA1c: 8 % MCAR
  d$hba1c[rbinom(n, 1, 0.08) == 1] <- NA
  # PAS: faltan más en los mayores (MAR) ~6 %
  d$pas[rbinom(n, 1, plogis(-3.2 + 0.04 * (d$edad - 64))) == 1] <- NA
  d
}

guardar_ckd_faltantes <- function(carpeta = "Bases") {
  o <- readr::read_csv(file.path(carpeta, "ckd_isglt2_oraculo.csv"), show_col_types = FALSE)
  base <- generar_faltantes(o, "MAR")
  oraculo <- c("fragilidad", "pendiente_0", "pendiente_1", "ite", "evento_36m_0", "evento_36m_1")
  readr::write_csv(dplyr::select(base, -dplyr::all_of(oraculo)), file.path(carpeta, "ckd_faltantes.csv"))
  invisible(base)
}
