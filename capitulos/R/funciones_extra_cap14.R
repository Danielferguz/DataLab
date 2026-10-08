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

# ------------------------------------------------------------------------
# 14.5  Ensayos por conglomerados: potencia y número de conglomerados
# ------------------------------------------------------------------------

#' Potencia de un ensayo por conglomerados con desenlace continuo (comparación de dos medias de
#' conglomerado; aproximación con la distribución t y 2(k - 1) grados de libertad).
#'
#' @param k conglomerados por brazo   @param m tamaño medio de conglomerado
#' @param icc correlación intraclase   @param delta diferencia a detectar
#' @param sd desviación estándar TOTAL del desenlace (entre y dentro de conglomerados)
#' @param cv coeficiente de variación del tamaño de los conglomerados (0 = todos iguales)
potencia_conglomerados <- function(k, m, icc, delta, sd, cv = 0, alfa = 0.05) {
  deff <- efecto_diseno(m, icc, cv)
  ee <- sqrt(2 * sd^2 * deff / (k * m))          # error estándar de la diferencia de medias
  gl <- 2 * (k - 1)
  tc <- stats::qt(1 - alfa / 2, gl)
  ncp <- delta / ee
  stats::pt(tc, gl, ncp, lower.tail = FALSE) + stats::pt(-tc, gl, ncp)
}

#' Número de conglomerados POR BRAZO para alcanzar una potencia dada (búsqueda directa).
unidades_necesarias <- function(delta, sd, icc, m, cv = 0, alfa = 0.05, potencia = 0.80) {
  k <- 2
  while (potencia_conglomerados(k, m, icc, delta, sd, cv, alfa) < potencia && k < 5000) k <- k + 1
  k
}

# ------------------------------------------------------------------------
# 14.6  Modelo de supervivencia con una covariable longitudinal (TFGe)
# ------------------------------------------------------------------------

#' Datos en formato "proceso de conteo": una fila por paciente y por cada momento en que ocurre
#' un evento en la cohorte (conjunto en riesgo), con el ÚLTIMO valor observado de la TFGe.
#' @param pac una fila por paciente: id, tiempo_seg_meses, krt (0/1)   @param largo id, mes, tfge
armar_riesgos <- function(pac, largo) {
  tev <- sort(unique(pac$tiempo_seg_meses[pac$krt == 1]))
  # (requiere library(survival) cargada: survSplit() busca `Surv` en la fórmula)
  cp <- survival::survSplit(stats::as.formula("Surv(tiempo_seg_meses, krt) ~ ."), data = as.data.frame(pac),
                            cut = tev, start = "t0", end = "t1", event = "evento")
  cp <- cp[cp$t1 %in% tev, ]                                    # solo intervalos que terminan en un evento
  # TFGe más reciente (y tiempo de esa medición) a cada momento: equivale a ultimo_valor(), pero vectorizado
  cp$ultimo <- NA_real_; cp$meses_desde_med <- NA_real_
  filas <- split(seq_len(nrow(cp)), cp$id)
  for (i in names(filas)) {
    z <- largo[largo$id == as.integer(i), ]
    z <- z[order(z$mes), ]
    pos <- findInterval(cp$t1[filas[[i]]], z$mes)               # índice de la última medición con mes <= t
    cp$ultimo[filas[[i]]] <- z$tfge[pmax(pos, 1)]
    cp$meses_desde_med[filas[[i]]] <- cp$t1[filas[[i]]] - z$mes[pmax(pos, 1)]
  }
  tibble::as_tibble(cp)
}

#' TFGe "actual" predicha para cada (paciente, momento) con el BLUP calculado con TODAS las
#' mediciones del paciente, también las posteriores al momento (mira al futuro; solo para comparar).
blup_total <- function(modelo, datos, ids, momentos, id = "id") {
  re <- lme4::ranef(modelo)[[id]]
  base <- datos[match(ids, datos[[id]]), , drop = FALSE]
  base$anios <- momentos
  as.numeric(stats::predict(modelo, newdata = base, re.form = NA) +
               re[as.character(ids), 1] + re[as.character(ids), 2] * momentos)
}

#' TFGe verdadera (oráculo) de cada (paciente, momento en meses); misma fórmula del simulador.
tfge_verdadera <- function(oraculo, ids, meses, tau = 0.2) {
  o <- oraculo[match(ids, oraculo$id), ]
  t <- meses / 12
  o$b0 + o$b1 * t + o$dip * (1 - exp(-t / tau))
}

#' Una réplica del experimento de 14.6: simula una cohorte nueva y estima la asociación
#' (coeficiente por cada 10 mL/min de TFGe) con cinco enfoques. Verdad: -1.0.
#' Requiere simular_ckd_largo() cargada (R/simular_ckd_largo.R).
replica_cox_td <- function(semilla, n = 600) {
  x <- simular_ckd_largo(n, semilla)
  pac <- x$pacientes |> dplyr::mutate(edad_c = edad - 64, luacr_c = log(uacr) - 5.2)
  d <- x$largo |> dplyr::left_join(pac, by = "id") |>
    dplyr::mutate(anios = mes / 12, trat = factor(tratamiento, 0:1, c("Sin iSGLT2", "iSGLT2")))
  cp <- armar_riesgos(pac, x$largo)
  m <- suppressWarnings(lme4::lmer(tfge ~ trat * (pmin(anios, 0.5) + pmax(anios - 0.5, 0)) + edad_c + dm + luacr_c + (1 + anios | id), data = d))
  cp$blup_ac <- blup_acumulado(m, d, cp$id, cp$t1 / 12)
  cp$blup_tot <- blup_total(m, d, cp$id, cp$t1 / 12)
  cp$verdadera <- tfge_verdadera(x$oraculo, cp$id, cp$t1)
  basal <- x$largo |> dplyr::filter(visita == 0) |> dplyr::select(id, tfge0 = tfge)
  pb <- dplyr::left_join(pac, basal, by = "id")
  ajuste <- function(formula, datos) { s <- summary(survival::coxph(formula, data = datos))$coefficients[1, ]; c(coef = s[["coef"]], ee = s[["se(coef)"]]) }
  rbind(basal = ajuste(survival::Surv(tiempo_seg_meses, krt) ~ I(tfge0 / 10), pb),
        ultimo = ajuste(survival::Surv(t0, t1, evento) ~ I(ultimo / 10), cp),
        blup_acumulado = ajuste(survival::Surv(t0, t1, evento) ~ I(blup_ac / 10), cp),
        blup_total = ajuste(survival::Surv(t0, t1, evento) ~ I(blup_tot / 10), cp),
        tfge_verdadera = ajuste(survival::Surv(t0, t1, evento) ~ I(verdadera / 10), cp)) |>
    tibble::as_tibble(rownames = "enfoque") |> dplyr::mutate(semilla = semilla)
}
