# ------------------------------------------------------------------------
# Funciones de apoyo para los Capítulos 12 (estudios observacionales) y 13 (datos faltantes)
# Uso:   source("R/funciones_extra_cap1213.R")   (requiere tidyverse cargado)
# Todos los datos que usan estas funciones son SIMULADOS.
# ------------------------------------------------------------------------

#' Lee el "oráculo" de ckd_isglt2 (incluye fragilidad y resultados potenciales)
#' y crea las variables que se usan en los análisis: sexo como factor, ln(UACR) y etiqueta del tratamiento.
cargar_oraculo <- function(ruta = "Bases/ckd_isglt2_oraculo.csv") {
  readr::read_csv(ruta, show_col_types = FALSE) |>
    dplyr::mutate(sexo = factor(sexo, levels = c("Mujer", "Hombre")),
                  luacr = log(uacr),
                  trat = factor(isglt2, levels = 0:1, labels = c("No inicia", "Inicia iSGLT2")))
}

#' Quita del oráculo lo que NO se observa en la vida real: queda la base que vería un investigador
observado <- function(oraculo) {
  dplyr::select(oraculo, -fragilidad, -pendiente_0, -pendiente_1, -ite, -dplyr::starts_with("evento_36m"))
}

#' Los tres efectos causales verdaderos de la pendiente de TFGe (solo posible con el oráculo)
verdad_pendiente <- function(oraculo) {
  c(ATE = mean(oraculo$ite),
    ATT = mean(oraculo$ite[oraculo$isglt2 == 1]),
    ATU = mean(oraculo$ite[oraculo$isglt2 == 0]))
}

#' Tamaño de muestra efectivo de un conjunto de pesos (cuánta información conserva la ponderación)
ess <- function(w) sum(w)^2 / sum(w^2)

#' Fórmula del puntaje de propensión que se usa en todo el Capítulo 12 (los confusores medidos)
confusores_medidos <- "I(edad / 10) + sexo + hba1c + pas + tfg_basal + luacr + ecv"

#' Tabla de balance: DEM de cada covariable sin ajustar y con cada juego de pesos
#' @param pesos lista nombrada de vectores de pesos
tabla_dem <- function(datos, vars, grupo = "isglt2", pesos = list()) {
  out <- tibble::tibble(variable = vars,
                        sin_ajustar = purrr::map_dbl(vars, \(v) smd(datos[[v]], datos[[grupo]])))
  for (nm in names(pesos)) {
    out[[nm]] <- purrr::map_dbl(vars, \(v) smd(datos[[v]], datos[[grupo]], pesos[[nm]]))
  }
  out
}

#' Diagrama de flujo sencillo con ggplot2 (cajas y flechas), en una o dos filas (serpiente)
#' @param etiquetas vector de textos (usa \n para saltos de línea)
#' @param filas 1 o 2
#' @param resaltar posiciones de cajas que se pintan de otro color (p. ej. el paso crítico)
diagrama_flujo <- function(etiquetas, filas = 1, resaltar = integer(), tam_texto = 3.3,
                           color_caja = "#E8F1FA", color_resalte = "#FCE9C8") {
  n <- length(etiquetas)
  por_fila <- ceiling(n / filas)
  d <- tibble::tibble(i = seq_len(n), texto = etiquetas) |>
    dplyr::mutate(fila = (i - 1) %/% por_fila + 1,
                  col0 = (i - 1) %% por_fila + 1,
                  col = dplyr::if_else(fila %% 2 == 1, col0, por_fila + 1 - col0),   # fila par va de derecha a izquierda
                  x = col, y = -fila * 1.25,
                  relleno = dplyr::if_else(i %in% resaltar, color_resalte, color_caja))
  w <- 0.40; h <- 0.46
  flechas <- purrr::map_dfr(seq_len(n - 1), \(k) {
    a <- d[k, ]; b <- d[k + 1, ]
    if (a$fila == b$fila) {
      s <- sign(b$x - a$x)
      tibble::tibble(x = a$x + s * w, xend = b$x - s * w, y = a$y, yend = b$y)
    } else {
      tibble::tibble(x = a$x, xend = b$x, y = a$y - h, yend = b$y + h)
    }
  })
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = flechas, ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(length = grid::unit(0.16, "cm"), type = "closed"),
                          color = "grey35", linewidth = 0.6) +
    ggplot2::geom_rect(data = d, ggplot2::aes(xmin = x - w, xmax = x + w, ymin = y - h, ymax = y + h, fill = relleno),
                       color = "grey35", linewidth = 0.4) +
    ggplot2::geom_text(data = d, ggplot2::aes(x = x, y = y, label = texto), size = tam_texto, lineheight = 0.95) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_cartesian(xlim = c(0.45, por_fila + 0.55), ylim = c(-filas * 1.25 - 0.55, -1.25 + 0.55)) +
    ggplot2::theme_void()
}

#' Gráfico de puntos: estimación de cada método frente al valor verdadero (línea naranja)
#' @param tabla tibble con columnas `metodo` y `estimado` (opcionales: `ic_inf`, `ic_sup`)
#' @param verdad valor verdadero del efecto
#' @param etiqueta_x texto del eje x
grafico_estimaciones <- function(tabla, verdad, etiqueta_x = "Efecto estimado sobre la pendiente de TFGe (mL/min/1.73 m²/año)") {
  tabla <- dplyr::mutate(tabla, metodo = forcats::fct_inorder(metodo))
  g <- ggplot2::ggplot(tabla, ggplot2::aes(estimado, forcats::fct_rev(metodo))) +
    ggplot2::geom_vline(xintercept = verdad, color = "#D55E00", linewidth = 1)
  if (all(c("ic_inf", "ic_sup") %in% names(tabla))) {
    g <- g + ggplot2::geom_errorbarh(ggplot2::aes(xmin = ic_inf, xmax = ic_sup), height = 0.2, color = "#0072B2", na.rm = TRUE)
  }
  g + ggplot2::geom_point(size = 3, color = "#0072B2") +
    ggplot2::annotate("text", x = verdad, y = nlevels(tabla$metodo) + 0.45, label = "verdad", color = "#D55E00", hjust = -0.15, size = 3.3) +
    ggplot2::labs(x = etiqueta_x, y = NULL, caption = "Línea naranja: efecto verdadero (solo conocido porque los datos son simulados).") +
    ggplot2::theme_minimal(base_size = 11)
}

#' Formatea enteros grandes con espacio de miles ("2 350")
miles <- function(x) format(x, big.mark = " ", trim = TRUE, scientific = FALSE)

#' Vector de pesos de un objeto `matchit` alineado con la base original (0 = no emparejado)
#' Sirve para pasarlo a love_plot() / tabla_dem() y comparar el balance antes y después.
pesos_matchit <- function(m, datos, id = "id") {
  mm <- MatchIt::match.data(m)
  w <- rep(0, nrow(datos))
  w[match(mm[[id]], datos[[id]])] <- mm$weights
  w
}

#' AIPW (doblemente robusto) con bosques aleatorios (ranger) y validación cruzada ("cross-fitting")
#' Los modelos del desenlace (uno por brazo) y del tratamiento se ajustan con ranger en una parte de
#' los datos y se predicen en la otra. Devuelve el ATE para `desenlace` (continuo o 0/1).
#' @param d base con columnas `isglt2` y el desenlace   @param covs nombres de los confusores
aipw_ml <- function(d, covs, desenlace = "pendiente_tfg", K = 2, arboles = 300, semilla = 1) {
  set.seed(semilla)
  n <- nrow(d); a <- d$isglt2; y <- d[[desenlace]]
  carpeta <- sample(rep(seq_len(K), length.out = n))
  mu1 <- mu0 <- ps <- numeric(n)
  for (k in seq_len(K)) {
    ent <- d[carpeta != k, ]; val <- d[carpeta == k, ]
    f_y <- stats::as.formula(paste(desenlace, "~", paste(covs, collapse = " + ")))
    rf1 <- ranger::ranger(f_y, data = ent[ent$isglt2 == 1, ], num.trees = arboles)
    rf0 <- ranger::ranger(f_y, data = ent[ent$isglt2 == 0, ], num.trees = arboles)
    rfp <- ranger::ranger(stats::as.formula(paste("factor(isglt2) ~", paste(covs, collapse = " + "))),
                          data = ent, probability = TRUE, num.trees = arboles)
    mu1[carpeta == k] <- predict(rf1, val)$predictions
    mu0[carpeta == k] <- predict(rf0, val)$predictions
    ps[carpeta == k]  <- predict(rfp, val)$predictions[, "1"]
  }
  ps <- pmin(pmax(ps, 0.025), 0.975)             # evita pesos gigantes
  psi <- mu1 - mu0 + a * (y - mu1) / ps - (1 - a) * (y - mu0) / (1 - ps)
  c(ATE = mean(psi), ee = stats::sd(psi) / sqrt(n))
}
