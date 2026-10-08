# ------------------------------------------------------------------------
# Funciones extra del Cap. 14 (datos longitudinales y multinivel)
# Uso en un capítulo:   source("R/funciones_extra_cap14.R")
# Dependen de base R + tidyverse (dplyr, ggplot2, tibble).
# ------------------------------------------------------------------------

#' Correlación intraclase (ICC) y efecto de diseño con el estimador de ANOVA
#' (válido con conglomerados de tamaño desigual).
#'
#' @param y variable numérica (una fila por paciente)
#' @param g identificador del conglomerado (unidad, centro...)
#' @return tibble con ICC, tamaño medio, CV del tamaño y efecto de diseño
icc_anova <- function(y, g) {
  g <- as.factor(g)
  n_j <- as.numeric(table(g)); N <- sum(n_j); J <- length(n_j)
  media_g <- tapply(y, g, mean)
  ss_entre <- sum(n_j * (media_g - mean(y))^2)
  ss_dentro <- sum((y - media_g[as.character(g)])^2)
  cm_entre <- ss_entre / (J - 1); cm_dentro <- ss_dentro / (N - J)
  n0 <- (N - sum(n_j^2) / N) / (J - 1)                       # tamaño "efectivo" de conglomerado
  icc <- (cm_entre - cm_dentro) / (cm_entre + (n0 - 1) * cm_dentro)
  tibble::tibble(icc = icc, conglomerados = J, tamano_medio = mean(n_j),
                 cv_tamano = stats::sd(n_j) / mean(n_j),
                 deff = 1 + (mean(n_j) - 1) * icc)
}

#' Efecto de diseño de un ensayo por conglomerados (Eldridge et al., 2006, con CV del tamaño)
#' @param m tamaño medio de conglomerado   @param icc correlación intraclase   @param cv coeficiente de variación de los tamaños
efecto_diseno <- function(m, icc, cv = 0) 1 + ((cv^2 + 1) * m - 1) * icc

#' Gráfico de "oruga" (caterpillar) de efectos aleatorios con intervalos
#' @param tabla tibble con `etiqueta`, `estimado`, `ic_inf`, `ic_sup` (y opcionalmente `grupo` para colorear)
#' @param etiqueta_x texto del eje x
oruga <- function(tabla, etiqueta_x = "Efecto de la unidad (desviación respecto del promedio)") {
  tabla <- dplyr::arrange(tabla, estimado) |>
    dplyr::mutate(etiqueta = factor(etiqueta, levels = etiqueta),
                  fuera = ic_inf > 0 | ic_sup < 0)
  g <- ggplot2::ggplot(tabla, ggplot2::aes(estimado, etiqueta)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey50", linetype = 2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ic_inf, xmax = ic_sup, color = fuera), height = 0, linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = fuera), size = 1.6) +
    ggplot2::scale_color_manual(values = c(`FALSE` = "grey55", `TRUE` = "#D55E00"), guide = "none") +
    ggplot2::labs(x = etiqueta_x, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6.5))
  g
}

#' Último valor observado hasta un momento (LOCF) para cada paciente
#' @param largo tibble con id, tiempo y valor
#' @param ids pacientes   @param momentos momentos (misma longitud que ids)
ultimo_valor <- function(largo, ids, momentos, id = "id", tiempo = "mes", valor = "tfge") {
  por_id <- split(largo[c(tiempo, valor)], largo[[id]])
  mapply(function(i, s) {
    z <- por_id[[as.character(i)]]
    z <- z[z[[tiempo]] <= s, ]
    if (nrow(z) == 0) NA_real_ else utils::tail(z[[valor]], 1)
  }, ids, momentos)
}

#' Predicción de la TFGe "actual" de cada paciente en cada momento, usando SOLO los datos
#' observados hasta ese momento (BLUP acumulado) de un modelo lineal mixto con
#' intercepto y pendiente aleatorios `(1 + anios | id)`.
#'
#' Se usa en el enfoque "en dos etapas" para el Cox con covariable dependiente del tiempo
#' (14.6): evita mirar al futuro y suaviza el error de medición.
#'
#' @param modelo lmer con efectos aleatorios (1 + anios | id) y la parte fija escrita
#'   con funciones de `anios` (p. ej. pmin(anios, 0.5)) para poder predecir en cualquier momento
#' @param datos datos usados para ajustar (con id, anios, la respuesta y las covariables)
#' @param ids,momentos pacientes y momentos (en AÑOS) donde se quiere la predicción
#' @param respuesta nombre de la respuesta   @param id nombre del identificador
blup_acumulado <- function(modelo, datos, ids, momentos, respuesta = "tfge", id = "id") {
  D <- lme4::VarCorr(modelo)[[id]][1:2, 1:2]
  s2 <- stats::sigma(modelo)^2
  datos$xb <- stats::predict(modelo, newdata = datos, re.form = NA)    # parte fija en cada medición
  nuevo <- datos[match(ids, datos[[id]]), , drop = FALSE]              # una fila por (paciente, momento)
  nuevo$anios <- momentos
  xb_momento <- stats::predict(modelo, newdata = nuevo, re.form = NA)  # parte fija en el momento s
  por_id <- split(datos[c(id, "anios", respuesta, "xb")], datos[[id]])
  unlist(Map(function(i, s, xb_s) {
    obs <- por_id[[as.character(i)]]
    obs <- obs[obs$anios <= s + 1e-9, ]
    Z <- cbind(1, obs$anios)
    V <- Z %*% D %*% t(Z) + s2 * diag(nrow(obs))
    b <- D %*% t(Z) %*% solve(V, obs[[respuesta]] - obs$xb)            # BLUP con lo observado hasta s
    as.numeric(xb_s + b[1] + b[2] * s)
  }, ids, momentos, xb_momento), use.names = FALSE)
}
