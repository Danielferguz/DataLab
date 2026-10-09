# ------------------------------------------------------------------------
# Funciones del Cap. 19 (19.1-19.3): tratamientos que cambian en el tiempo
# Requieren tidyverse y los datos de simular_hd_epo() (R/simular_cap19_epo.R).
# Convención: `largo` = una fila por paciente y mes (persona-tiempo);
#             `ancho` = una fila por paciente (hb_1..hb_K, dosis_alta_1..dosis_alta_K).
# ------------------------------------------------------------------------

#' Añade el pasado de cada paciente a la tabla larga: el valor del mes anterior (y dos meses atrás)
#' Al inicio (mes 1) no hay dosis previa: todos parten con dosis estándar (0).
agregar_rezagos <- function(largo) {
  largo <- dplyr::arrange(largo, id, mes)                    # panel equilibrado: K filas por paciente, en orden
  K <- max(largo$mes)
  primero <- largo$mes == 1
  atras <- function(x, k = 1) c(rep(NA, k), utils::head(x, -k))      # valor de k filas atrás (dentro del paciente, ver abajo)
  largo$hb_basal <- rep(largo$hb[primero], each = K)                  # Hb de la primera visita
  largo$hb_previa <- ifelse(primero, largo$hb, atras(largo$hb))       # Hb del mes anterior (en el mes 1, la misma)
  largo$dosis_previa <- ifelse(primero, 0, atras(largo$dosis_alta))   # dosis del mes anterior (en el mes 1, estándar = 0)
  largo$dosis_previa2 <- ifelse(largo$mes <= 2, 0, atras(largo$dosis_alta, 2))
  largo$dosis_acum_previa <- as.vector(apply(matrix(largo$dosis_alta, nrow = K), 2, cumsum)) - largo$dosis_alta
  largo
}

#' De largo a ancho: una fila por paciente, con hb_1..hb_K, dosis_alta_1..dosis_alta_K,
#' dosis_acum (meses con dosis alta) y el desenlace final.
a_ancho_hd <- function(largo) {
  basal <- largo |> dplyr::filter(mes == 1) |> dplyr::select(id, edad, dm, tiempo_hd)
  seguim <- largo |>
    dplyr::select(id, mes, hb, dosis_alta) |>
    tidyr::pivot_wider(names_from = mes, values_from = c(hb, dosis_alta), names_sep = "_")
  final <- largo |> dplyr::filter(!is.na(calidad_vida)) |> dplyr::select(id, calidad_vida)
  basal |>
    dplyr::left_join(seguim, by = "id") |>
    dplyr::left_join(final, by = "id") |>
    dplyr::mutate(dosis_acum = rowSums(dplyr::across(dplyr::starts_with("dosis_alta_"))))
}

#' Remuestrea PACIENTES completos con reemplazo (todas sus filas) y les da un id nuevo
remuestrear_pacientes <- function(largo) {
  ids <- sample(unique(largo$id), replace = TRUE)
  tibble::tibble(id = ids, id_nuevo = seq_along(ids)) |>
    dplyr::inner_join(largo, by = "id", relationship = "many-to-many") |>
    dplyr::mutate(id = id_nuevo) |>
    dplyr::select(-id_nuevo)
}

# ---------------------------- 19.2: pesos y MSM ----------------------------

#' Pesos de un modelo estructural marginal para una dosis binaria que cambia mes a mes.
#' f_num: fórmula del NUMERADOR (probabilidad de la dosis dado el pasado de tratamiento y lo basal)
#' f_den: fórmula del DENOMINADOR (además, la Hb actual: lo que realmente miró el nefrólogo)
#' Añade: p_num, p_den (probabilidad de la dosis que el paciente SÍ recibió ese mes),
#'        sw (peso estabilizado acumulado) y w_ns (peso sin estabilizar acumulado).
pesos_msm <- function(largo, f_num, f_den) {
  largo <- dplyr::arrange(largo, id, mes)
  m_num <- stats::glm(f_num, data = largo, family = stats::binomial())
  m_den <- stats::glm(f_den, data = largo, family = stats::binomial())
  prob_recibida <- function(m) ifelse(largo$dosis_alta == 1, stats::fitted(m), 1 - stats::fitted(m))
  largo$p_num <- prob_recibida(m_num)
  largo$p_den <- prob_recibida(m_den)
  K <- max(largo$mes)
  producto_por_paciente <- function(x) as.vector(apply(matrix(x, nrow = K), 2, cumprod))   # producto acumulado mes a mes
  largo$sw <- producto_por_paciente(largo$p_num / largo$p_den)
  largo$w_ns <- producto_por_paciente(1 / largo$p_den)
  largo
}

#' Tabla de un peso por paciente (el del último mes) junto con dosis acumulada, covariables y desenlace
pesos_finales <- function(largo_con_pesos) {
  d <- dplyr::arrange(largo_con_pesos, id, mes)
  ult <- d[d$mes == max(d$mes), ]                                  # fila del último mes de cada paciente
  tibble::tibble(id = ult$id, dosis_acum = as.vector(rowsum(d$dosis_alta, d$id)),
                 sw = ult$sw, w_ns = ult$w_ns, edad = ult$edad, dm = ult$dm, tiempo_hd = ult$tiempo_hd,
                 calidad_vida = ult$calidad_vida)
}

#' Ajusta el MSM: calidad de vida ~ dosis acumulada, ponderando por `peso`.
#' Devuelve el objeto lm. Si el peso es sin estabilizar, no se incluyen covariables basales
#' (estimación del MSM sin ellas); con peso estabilizado se ajusta por edad, dm y tiempo_hd,
#' que son las que entran en el numerador.
ajustar_msm <- function(fin, peso = "sw", covariables = c("edad", "dm", "tiempo_hd"), truncar = NULL) {
  p <- fin[[peso]]
  if (!is.null(truncar)) {                                   # truncar = c(0.01, 0.99): recorta a esos percentiles
    lim <- stats::quantile(p, truncar)
    p <- pmin(pmax(p, lim[1]), lim[2])
  }
  f <- stats::reformulate(c("dosis_acum", covariables), response = "calidad_vida")
  stats::lm(f, data = fin, weights = p)
}

#' Efecto de "6 meses de dosis alta frente a 6 meses de dosis estándar" según el MSM (K * coeficiente)
efecto_msm <- function(largo, f_num, f_den, peso = "sw", covariables = c("edad", "dm", "tiempo_hd"), truncar = NULL) {
  K <- max(largo$mes)
  fin <- pesos_finales(pesos_msm(largo, f_num, f_den))
  unname(K * stats::coef(ajustar_msm(fin, peso, covariables, truncar))["dosis_acum"])
}

#' Intervalo de 95 % por bootstrap de pacientes: repite TODO (modelos + estimación) en cada remuestra.
#' `estimador` es una función(largo) que devuelve un número (o un vector con nombres).
#' Devuelve ee, inf, sup (un vector si el estimador es un número; una matriz con una columna por estimación si es un vector).
bootstrap_pacientes <- function(largo, estimador, B = 200, semilla = 1) {
  set.seed(semilla)
  semillas <- sample.int(1e6, B)                      # una semilla por remuestra (así cada una es distinta y reproducible)
  est <- sapply(semillas, function(s) { set.seed(s); estimador(remuestrear_pacientes(largo)) })
  est <- if (is.null(dim(est))) rbind(est) else est   # filas = estimaciones, columnas = remuestras
  res <- rbind(ee = apply(est, 1, stats::sd), inf = apply(est, 1, stats::quantile, 0.025), sup = apply(est, 1, stats::quantile, 0.975))
  if (ncol(res) == 1) { r <- res[, 1]; names(r) <- c("ee", "inf", "sup"); r } else res
}

# ----------------------- 19.3: fórmula g paramétrica -----------------------

#' Ajusta los tres modelos de la fórmula g (largo con rezagos).
#' Hb del mes t dado el pasado; dosis del mes t dado el pasado (solo para el curso natural);
#' calidad de vida final dado todo el historial.
ajustar_modelos_g <- function(largo) {
  m_hb <- stats::lm(hb ~ hb_previa + hb_basal + dosis_previa + dosis_previa2 + edad + dm + tiempo_hd,
                    data = dplyr::filter(largo, mes >= 2))
  m_a <- stats::glm(dosis_alta ~ dosis_previa + hb + edad + dm + tiempo_hd, family = stats::binomial(), data = largo)
  ancho <- a_ancho_hd(largo)
  K <- max(largo$mes)
  f_y <- stats::reformulate(c(paste0("hb_", 1:K), paste0("dosis_alta_", 1:K), "edad", "dm", "tiempo_hd"),
                            response = "calidad_vida")
  m_y <- stats::lm(f_y, data = ancho)
  list(hb = m_hb, sd_hb = stats::sigma(m_hb), a = m_a, y = m_y, K = K,
       base = dplyr::select(ancho, edad, dm, tiempo_hd, hb_1))
}

#' Simula por Monte Carlo el mundo bajo una estrategia y devuelve la calidad de vida media
#' (y la Hb media de cada mes). estrategia: "natural", "siempre", "nunca", "dinamica" o vector 0/1 de largo K.
#' El ruido (e) es el mismo para todas las estrategias si se pasa la misma `semilla`.
simular_formula_g <- function(mod, estrategia = "natural", umbral = 10, copias = 5, semilla = 1) {
  K <- mod$K
  base <- mod$base[rep(seq_len(nrow(mod$base)), copias), ]           # varias copias de cada paciente suavizan el azar
  n <- nrow(base)
  set.seed(semilla)
  e <- matrix(stats::rnorm(n * K, 0, mod$sd_hb), n, K)               # ruido de la Hb, común a todas las estrategias
  u_a <- matrix(stats::runif(n * K), n, K)
  hb <- a <- matrix(NA_real_, n, K)
  for (t in 1:K) {
    hb[, t] <- if (t == 1) base$hb_1 else {
      nd <- data.frame(hb_previa = hb[, t - 1], hb_basal = hb[, 1], dosis_previa = a[, t - 1],
                       dosis_previa2 = if (t > 2) a[, t - 2] else 0,
                       edad = base$edad, dm = base$dm, tiempo_hd = base$tiempo_hd)
      stats::predict(mod$hb, nd) + e[, t]
    }
    a[, t] <- if (is.numeric(estrategia)) estrategia[t]
              else if (estrategia == "siempre") 1
              else if (estrategia == "nunca") 0
              else if (estrategia == "dinamica") as.numeric(hb[, t] < umbral)
              else {                                                    # curso natural: la dosis se simula con su modelo
                nd <- data.frame(dosis_previa = if (t > 1) a[, t - 1] else 0, hb = hb[, t],
                                 edad = base$edad, dm = base$dm, tiempo_hd = base$tiempo_hd)
                as.numeric(u_a[, t] < stats::predict(mod$a, nd, type = "response"))
              }
  }
  colnames(hb) <- paste0("hb_", 1:K); colnames(a) <- paste0("dosis_alta_", 1:K)
  nd_y <- data.frame(hb, a, edad = base$edad, dm = base$dm, tiempo_hd = base$tiempo_hd)
  list(y = mean(stats::predict(mod$y, nd_y)), hb_medias = colMeans(hb), dosis_medias = colMeans(a))
}

#' Efectos de la fórmula g frente a "nunca" para varias estrategias (devuelve un vector con nombre)
efectos_formula_g <- function(largo, estrategias = c("siempre", "dinamica"), copias = 5, semilla = 1) {
  mod <- ajustar_modelos_g(agregar_rezagos(largo))
  y0 <- simular_formula_g(mod, "nunca", copias = copias, semilla = semilla)$y
  vapply(estrategias, \(e) simular_formula_g(mod, e, copias = copias, semilla = semilla)$y - y0, numeric(1))
}

# ----------------------- 19.3: estimación g (SNM lineal) -----------------------

#' Estimación g de un modelo estructural anidado lineal con un efecto psi_t por mes y sin modificadores.
#' Para t = K, K-1, ..., 1: se "quita" el efecto de la dosis de los meses ya estimados y se busca el psi_t
#' que deja a la dosis del mes t sin asociación con lo que queda (dado el pasado medido).
#' f_ps: fórmula del modelo de la dosis (el que miraba la Hb). Devuelve un vector psi_1..psi_K.
estimacion_g_snm <- function(largo, f_ps) {
  d <- agregar_rezagos(largo)
  K <- max(d$mes)
  d$p1 <- stats::fitted(stats::glm(f_ps, data = d, family = stats::binomial()))
  A <- matrix(d$dosis_alta, ncol = K, byrow = TRUE)
  P <- matrix(d$p1, ncol = K, byrow = TRUE)
  h <- d$calidad_vida[d$mes == K]                      # lo que queda: parte del desenlace observado
  psi <- numeric(K)
  for (t in K:1) {
    psi[t] <- sum(h * (A[, t] - P[, t])) / sum(A[, t] * (A[, t] - P[, t]))
    h <- h - psi[t] * A[, t]                           # quita el efecto estimado de la dosis del mes t
  }
  psi
}
