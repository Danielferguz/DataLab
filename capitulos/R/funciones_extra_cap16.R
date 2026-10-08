# ------------------------------------------------------------------------
# Funciones extra para el Cap. 16 (diagnóstico)
# Uso en un capítulo:   source("R/funciones_extra_cap16.R")
# Solo dependen de base R, dplyr, tibble y ggplot2 (tidyverse).
# ------------------------------------------------------------------------

# ---- 0. Formato para el texto en línea (se definen solo si no existen ya) ----
if (!exists("pct")) pct <- function(x, digitos = 1) paste0(formatC(100 * x, format = "f", digits = digitos), " %")
if (!exists("num")) num <- function(x, digitos = 2) formatC(x, format = "f", digits = digitos)

# ---- 1. Tabla 2x2 y medidas de exactitud diagnóstica ---------------------

#' Tabla 2x2 de una prueba dicotómica frente al estándar de referencia
#' @param prueba_pos lógico/0-1: prueba positiva   @param enfermo lógico/0-1: estándar de referencia positivo
#' @return lista con a (VP), b (FP), c (FN), d (VN)
tabla_2x2 <- function(prueba_pos, enfermo) {
  p <- as.logical(prueba_pos); e <- as.logical(enfermo)
  list(a = sum(p & e, na.rm = TRUE), b = sum(p & !e, na.rm = TRUE),
       c = sum(!p & e, na.rm = TRUE), d = sum(!p & !e, na.rm = TRUE))
}

#' Medidas diagnósticas con IC (Wilson para proporciones; log para razones de verosimilitud)
#' @param a verdaderos positivos @param b falsos positivos @param c falsos negativos @param d verdaderos negativos
medidas_dx <- function(a, b, c, d, nivel = 0.95) {
  z <- stats::qnorm(1 - (1 - nivel) / 2)
  wilson <- function(x, n) { ci <- stats::prop.test(x, n, conf.level = nivel, correct = FALSE)$conf.int; c(x / n, ci) }
  se <- wilson(a, a + c); es <- wilson(d, b + d); vp <- wilson(a, a + b); vn <- wilson(d, c + d)
  lrp <- (a / (a + c)) / (b / (b + d)); se_lrp <- sqrt(1 / a - 1 / (a + c) + 1 / b - 1 / (b + d))
  lrn <- (c / (a + c)) / (d / (b + d)); se_lrn <- sqrt(1 / c - 1 / (a + c) + 1 / d - 1 / (b + d))
  tibble::tibble(
    medida   = c("Sensibilidad", "Especificidad", "VPP", "VPN", "RV+ (LR+)", "RV- (LR-)", "Prevalencia en la muestra"),
    estimado = c(se[1], es[1], vp[1], vn[1], lrp, lrn, (a + c) / (a + b + c + d)),
    ic_inf   = c(se[2], es[2], vp[2], vn[2], exp(log(lrp) - z * se_lrp), exp(log(lrn) - z * se_lrn), NA),
    ic_sup   = c(se[3], es[3], vp[3], vn[3], exp(log(lrp) + z * se_lrp), exp(log(lrn) + z * se_lrn), NA)
  )
}

#' VPP y VPN por Bayes a partir de sensibilidad, especificidad y prevalencia
vpp_vpn <- function(sens, esp, prev) {
  tibble::tibble(prev = prev,
                 vpp = sens * prev / (sens * prev + (1 - esp) * (1 - prev)),
                 vpn = esp * (1 - prev) / (esp * (1 - prev) + (1 - sens) * prev))
}

#' Probabilidad posprueba a partir de la preprueba y una razón de verosimilitud (Bayes en odds)
prob_post <- function(pre, lr) { o <- pre / (1 - pre) * lr; o / (1 + o) }

#' Nomograma de Fagan simple: une la probabilidad preprueba con la posprueba a través de la RV
#' @param pre probabilidad preprueba (un número)  @param lr vector con nombre de razones de verosimilitud
grafico_fagan <- function(pre, lr) {
  pr_ticks <- c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99, 0.995, 0.999)
  lr_ticks <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100)
  izq <- tibble::tibble(x = 0, y = -stats::qlogis(pr_ticks), lab = paste0(100 * pr_ticks, " %"))
  der <- tibble::tibble(x = 2, y = stats::qlogis(pr_ticks), lab = paste0(100 * pr_ticks, " %"))
  med <- tibble::tibble(x = 1, y = log(lr_ticks) / 2, lab = as.character(lr_ticks))
  rectas <- tibble::tibble(nombre = names(lr), lr = as.numeric(lr)) |>
    dplyr::mutate(y0 = -stats::qlogis(pre), y1 = stats::qlogis(prob_post(pre, lr)),
                  post = prob_post(pre, lr),
                  etiqueta = sprintf("%s = %s: %s %%", nombre, format(round(lr, 2), nsmall = 2), format(round(100 * post, 1), nsmall = 1)))
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = tibble::tibble(x = 0:2), ggplot2::aes(x = x, xend = x, y = -7, yend = 7), colour = "grey50") +
    ggplot2::geom_point(data = izq, ggplot2::aes(x, y), shape = 3, size = 1.5) +
    ggplot2::geom_point(data = der, ggplot2::aes(x, y), shape = 3, size = 1.5) +
    ggplot2::geom_point(data = med, ggplot2::aes(x, y), shape = 3, size = 1.5) +
    ggplot2::geom_text(data = izq, ggplot2::aes(x - 0.05, y, label = lab), hjust = 1, size = 2.8) +
    ggplot2::geom_text(data = der, ggplot2::aes(x + 0.05, y, label = lab), hjust = 0, size = 2.8) +
    ggplot2::geom_text(data = med, ggplot2::aes(x + 0.05, y, label = lab), hjust = 0, size = 2.8) +
    ggplot2::geom_segment(data = rectas, ggplot2::aes(x = 0, xend = 2, y = y0, yend = y1, colour = etiqueta), linewidth = 0.9) +
    ggplot2::annotate("text", x = c(0, 1, 2), y = 7.6, label = c("Preprueba", "Razón de\nverosimilitud", "Posprueba"), fontface = "bold", size = 3.2) +
    ggplot2::scale_colour_manual(values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7")[seq_len(nrow(rectas))], name = "Resultado") +
    ggplot2::coord_cartesian(xlim = c(-0.45, 2.45), ylim = c(-7, 8.3)) +
    ggplot2::theme_void(base_size = 11) + ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
}

# ---- 2. Rendimiento a distintos puntos de corte --------------------------

#' Sensibilidad, especificidad, VPP, VPN, J de Youden y % de positivos para varios cortes
#' (la prueba es "positiva" cuando x >= corte)
metricas_corte <- function(x, y, cortes) {
  purrr::map_dfr(cortes, function(k) {
    t <- tabla_2x2(x >= k, y)
    tibble::tibble(corte = k, sens = t$a / (t$a + t$c), esp = t$d / (t$b + t$d),
                   vpp = t$a / (t$a + t$b), vpn = t$d / (t$c + t$d),
                   youden = sens + esp - 1, positivos = (t$a + t$b) / length(x))
  })
}

#' AUC rápida (equivale a la U de Mann-Whitney / estadístico c); sirve para bootstrap
auc_rapida <- function(p, y) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' Corte de Youden (máximo de sens + esp - 1) para una prueba con "positivo = x >= corte" (versión rápida)
corte_youden <- function(x, y) {
  u <- sort(unique(x)); n1 <- sum(y == 1); n0 <- sum(y == 0)
  tc <- tabulate(match(x[y == 1], u), length(u)); tn <- tabulate(match(x[y == 0], u), length(u))
  sens <- 1 - (cumsum(tc) - tc) / n1          # casos con x >= u_k
  esp  <- (cumsum(tn) - tn) / n0              # controles con x <  u_k
  u[which.max(sens + esp - 1)]
}

#' Sensibilidad y especificidad de la regla "positivo si x >= corte" en unos datos
sens_esp <- function(x, y, corte) c(sens = mean(x[y == 1] >= corte), esp = mean(x[y == 0] < corte))

#' Coste esperado por paciente de la regla "x >= corte": cada falso positivo cuesta 1 y cada falso negativo `razon`
coste_corte <- function(x, y, cortes, razon = 5) {
  purrr::map_dfr(cortes, function(k) {
    t <- tabla_2x2(x >= k, y)
    tibble::tibble(corte = k, coste = (t$b * 1 + t$c * razon) / length(x), fp = t$b, fn = t$c)
  })
}

#' Corte elegido por validación cruzada K-fold: en cada vuelta el corte de Youden se busca SIN el fold
#' de prueba y se aplica a él; devuelve la sensibilidad y especificidad "honestas" (agrupadas)
corte_youden_cv <- function(x, y, K = 10, semilla = 1) {
  set.seed(semilla)
  fold <- sample(rep(seq_len(K), length.out = length(x)))
  pred <- logical(length(x)); cortes <- numeric(K)
  for (k in seq_len(K)) {
    cortes[k] <- corte_youden(x[fold != k], y[fold != k])
    pred[fold == k] <- x[fold == k] >= cortes[k]
  }
  t <- tabla_2x2(pred, y)
  list(sens = t$a / (t$a + t$c), esp = t$d / (t$b + t$d), cortes = cortes)
}

# ---- 3. Predicciones fuera de muestra y optimismo -------------------------

#' Probabilidades predichas por validación cruzada K-fold (cada paciente se predice con un modelo
#' que NO lo vio). Para evitar el optimismo de evaluar un modelo en los mismos datos con que se ajustó.
prob_cv <- function(formula, datos, K = 10, semilla = 1) {
  set.seed(semilla)
  fold <- sample(rep(seq_len(K), length.out = nrow(datos)))
  p <- numeric(nrow(datos))
  for (k in seq_len(K)) {
    m <- stats::glm(formula, data = datos[fold != k, ], family = stats::binomial)
    p[fold == k] <- stats::predict(m, newdata = datos[fold == k, ], type = "response")
  }
  p
}

#' AUC con corrección de optimismo por bootstrap (Harrell): ajusta en cada remuestra, evalúa en ella
#' (aparente) y en la muestra original (prueba); el optimismo es la diferencia media.
auc_optimismo <- function(formula, datos, B = 200, semilla = 1) {
  set.seed(semilla)
  y <- datos[[all.vars(formula)[1]]]
  m0 <- stats::glm(formula, data = datos, family = stats::binomial)
  aparente <- auc_rapida(stats::fitted(m0), y)
  opt <- replicate(B, {
    i <- sample(nrow(datos), replace = TRUE)
    mb <- stats::glm(formula, data = datos[i, ], family = stats::binomial)
    auc_rapida(stats::fitted(mb), datos[[all.vars(formula)[1]]][i]) -
      auc_rapida(stats::predict(mb, newdata = datos, type = "response"), y)
  })
  tibble::tibble(auc_aparente = aparente, optimismo = mean(opt), auc_corregida = aparente - mean(opt))
}

# ---- 4. Reclasificación (NRI) e IDI ---------------------------------------

#' Índice de reclasificación neta (NRI) e IDI entre un modelo viejo y uno nuevo.
#' Con `cortes` (probabilidades) es categórico; con cortes = NULL es continuo (cualquier subida/bajada).
nri <- function(p_viejo, p_nuevo, y, cortes = NULL) {
  if (is.null(cortes)) { sube <- p_nuevo > p_viejo; baja <- p_nuevo < p_viejo
  } else {
    cv <- cut(p_viejo, c(-Inf, cortes, Inf), labels = FALSE); cn <- cut(p_nuevo, c(-Inf, cortes, Inf), labels = FALSE)
    sube <- cn > cv; baja <- cn < cv
  }
  ev <- y == 1; ne <- y == 0
  nri_ev <- mean(sube[ev]) - mean(baja[ev]); nri_ne <- mean(baja[ne]) - mean(sube[ne])
  idi <- (mean(p_nuevo[ev]) - mean(p_nuevo[ne])) - (mean(p_viejo[ev]) - mean(p_viejo[ne]))
  tibble::tibble(nri_eventos = nri_ev, nri_no_eventos = nri_ne, nri = nri_ev + nri_ne, idi = idi)
}

# ---- 5. Calibración -------------------------------------------------------

#' Resumen de calibración: intercepto (calibración en lo grande), pendiente, O:E, Brier, Brier escalado e ICI
resumen_calibracion <- function(p, y) {
  eps <- 1e-8
  lp <- stats::qlogis(pmin(pmax(p, eps), 1 - eps))
  ajuste <- stats::coef(stats::glm(y ~ lp, family = stats::binomial))
  pend <- unname(ajuste[2]); interc_recta <- unname(ajuste[1])        # recta de calibración: intercepto y pendiente libres
  cil  <- unname(stats::coef(stats::glm(y ~ offset(lp), family = stats::binomial))[1])  # intercepto con pendiente fijada en 1
  brier <- mean((p - y)^2); prev <- mean(y)
  suav <- stats::loess(y ~ p, span = 0.75, degree = 2)
  ici <- mean(abs(stats::predict(suav) - p))
  tibble::tibble(intercepto_CITL = cil, intercepto_recta = interc_recta, pendiente = pend, O_E = sum(y) / sum(p), brier = brier,
                 brier_escalado = 1 - brier / (prev * (1 - prev)), ICI = ici)
}

#' Tabla de calibración por grupos de riesgo predicho (con IC de Wilson de la proporción observada)
tabla_calibracion <- function(p, y, grupos = 10) {
  g <- dplyr::ntile(p, grupos)
  tibble::tibble(p = p, y = y, g = g) |>
    dplyr::group_by(g) |>
    dplyr::summarise(n = dplyr::n(), predicho = mean(p), observado = mean(y), eventos = sum(y), .groups = "drop") |>
    dplyr::mutate(ic_inf = purrr::map2_dbl(eventos, n, \(x, m) stats::prop.test(x, m, correct = FALSE)$conf.int[1]),
                  ic_sup = purrr::map2_dbl(eventos, n, \(x, m) stats::prop.test(x, m, correct = FALSE)$conf.int[2]))
}

#' Gráfico de calibración: puntos por grupo de riesgo + curva suavizada (loess) + diagonal ideal
grafico_calibracion <- function(p, y, grupos = 10, titulo = NULL, suavizar = TRUE, limite = NULL) {
  tb <- tabla_calibracion(p, y, grupos)
  lim <- if (is.null(limite)) max(c(tb$ic_sup, tb$predicho), na.rm = TRUE) else limite
  g <- ggplot2::ggplot() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey40") +
    ggplot2::geom_errorbar(data = tb, ggplot2::aes(x = predicho, ymin = ic_inf, ymax = ic_sup), width = 0.01, colour = "#0072B2", alpha = 0.6) +
    ggplot2::geom_point(data = tb, ggplot2::aes(predicho, observado), colour = "#0072B2", size = 2)
  if (suavizar) {
    s <- stats::loess(y ~ p, span = 0.75); xs <- seq(min(p), max(p), length.out = 100)
    g <- g + ggplot2::geom_line(data = tibble::tibble(x = xs, y = pmin(pmax(stats::predict(s, xs), 0), 1)), ggplot2::aes(x, y), colour = "#D55E00", linewidth = 0.9)
  }
  g + ggplot2::coord_equal(xlim = c(0, lim), ylim = c(0, lim)) +
    ggplot2::labs(x = "Riesgo predicho", y = "Proporción observada", title = titulo) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Prueba de Hosmer-Lemeshow (se muestra para ver por qué se desaconseja)
hosmer_lemeshow <- function(p, y, g = 10) {
  gr <- dplyr::ntile(p, g)
  o <- tapply(y, gr, sum); e <- tapply(p, gr, sum); n <- tapply(y, gr, length)
  est <- sum((o - e)^2 / (e * (1 - e / n)))
  tibble::tibble(grupos = g, chi2 = est, gl = g - 2, valor_p = stats::pchisq(est, g - 2, lower.tail = FALSE))
}

# ---- 6. Curvas de decisión (beneficio neto) -------------------------------

#' Beneficio neto de usar un modelo (o una prueba) para decidir a quién intervenir
#' BN(pt) = VP/n - FP/n * pt / (1 - pt)
#' @param p probabilidad predicha (o 0/1 de una prueba)  @param y desenlace 0/1  @param umbrales umbrales de decisión pt (0 < pt < 1)
beneficio_neto <- function(p, y, umbrales) {
  n <- length(y)
  purrr::map_dfr(umbrales, function(pt) {
    pos <- p >= pt
    tibble::tibble(umbral = pt, vp = sum(pos & y == 1), fp = sum(pos & y == 0),
                   beneficio_neto = sum(pos & y == 1) / n - sum(pos & y == 0) / n * pt / (1 - pt))
  })
}

#' Beneficio neto de "intervenir a todos" (la prevalencia sustituye a VP/n) y de "a ninguno" (siempre 0)
beneficio_neto_todos <- function(y, umbrales) {
  prev <- mean(y)
  tibble::tibble(umbral = umbrales, beneficio_neto = prev - (1 - prev) * umbrales / (1 - umbrales))
}

#' Curva de decisión lista para graficar: "Intervenir a todos", "A ninguno" y uno o más modelos
#' @param modelos lista con nombre de vectores de probabilidad predicha
curva_decision <- function(y, modelos, umbrales = seq(0.01, 0.60, by = 0.01)) {
  dplyr::bind_rows(
    dplyr::mutate(beneficio_neto_todos(y, umbrales), estrategia = "Intervenir a todos"),
    tibble::tibble(umbral = umbrales, beneficio_neto = 0, estrategia = "No intervenir a nadie"),
    purrr::imap_dfr(modelos, \(p, nombre) dplyr::mutate(beneficio_neto(p, y, umbrales)[c("umbral", "beneficio_neto")], estrategia = nombre))
  )
}

grafico_dca <- function(df, ymin = -0.05, xmax = 0.6) {
  est <- unique(df$estrategia)
  modelos <- setdiff(est, c("Intervenir a todos", "No intervenir a nadie"))
  cols <- c("Intervenir a todos" = "grey55", "No intervenir a nadie" = "black",
            stats::setNames(c("#D55E00", "#0072B2", "#009E73", "#CC79A7")[seq_along(modelos)], modelos))
  ggplot2::ggplot(df, ggplot2::aes(umbral, beneficio_neto, colour = estrategia, linetype = estrategia)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_linetype_manual(values = c("Intervenir a todos" = "dashed", "No intervenir a nadie" = "solid",
                                              stats::setNames(rep("solid", length(modelos)), modelos))) +
    ggplot2::coord_cartesian(ylim = c(ymin, max(df$beneficio_neto) * 1.05), xlim = c(0, xmax)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::labs(x = "Umbral de decisión (riesgo a partir del cual intervendrías)", y = "Beneficio neto", colour = NULL, linetype = NULL) +
    ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = "bottom")
}

# ---- 7. Error estándar de un AUC (Hanley-McNeil, 1982) y meta-análisis -----

#' EE aproximado de un AUC conocida solo por su valor y los tamaños de muestra (Hanley y McNeil, 1982)
#' @param auc AUC   @param n1 número de casos   @param n0 número de no casos
se_auc_hanley <- function(auc, n1, n0) {
  q1 <- auc / (2 - auc); q2 <- 2 * auc^2 / (1 + auc)
  sqrt((auc * (1 - auc) + (n1 - 1) * (q1 - auc^2) + (n0 - 1) * (q2 - auc^2)) / (n1 * n0))
}
