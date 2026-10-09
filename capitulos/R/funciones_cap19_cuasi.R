# ------------------------------------------------------------------------
# Funciones de apoyo de los subcapítulos 19.4 (instrumentales), 19.5 (DiD y RDD) y 19.6 (heterogeneidad)
#   source("R/funciones_cap19_cuasi.R")      (requiere tidyverse cargado)
# Paquetes: stats, ranger. Todos los datos de los capítulos son SIMULADOS.
# ------------------------------------------------------------------------

# ======================= 19.4  Variable instrumental =======================

#' Mínimos cuadrados en dos etapas "a mano" (sin errores estándar; solo para ver la mecánica y simular)
#' @param z instrumento: vector o matriz (varios instrumentos)
#' @param covs matriz numérica de covariables (sin intercepto)
#' @return c(efecto = coef. del tratamiento en la 2.ª etapa, F = F conjunta de los instrumentos en la 1.ª etapa)
dos_etapas <- function(y, a, z, covs) {
  z <- as.matrix(z); c1 <- cbind(1, covs)
  e1 <- stats::lm.fit(cbind(c1, z), a)                 # 1.ª etapa: A ~ instrumentos + covariables
  r0 <- sum(stats::lm.fit(c1, a)$residuals^2)          # sin instrumentos
  r1 <- sum(e1$residuals^2)
  F  <- ((r0 - r1) / ncol(z)) / (r1 / (length(a) - ncol(c1) - ncol(z)))
  e2 <- stats::lm.fit(cbind(c1, a_hat = a - e1$residuals), y)   # 2.ª etapa con A predicho
  c(efecto = unname(e2$coefficients["a_hat"]), F = F)
}

#' Simulación de Monte Carlo: OLS vs IV con instrumento fuerte o débil
#' Genera B muestras con simular_iv() y guarda el estimador OLS, el IV y el F de la 1.ª etapa.
#' @param instrumento "preferencia" (un instrumento continuo) o "medico" (una indicadora por médico,
#'   es decir, muchos instrumentos)
mc_iv <- function(B = 300, n = 1500, sd_pref = 0.6, instrumento = c("preferencia", "medico"), semilla = 1) {
  instrumento <- match.arg(instrumento)
  purrr::map_dfr(seq_len(B), \(b) {
    d <- simular_iv(n = n, sd_pref = sd_pref, semilla = semilla * 100000 + b)
    cv <- cbind(edad10 = d$edad / 10, hombre = as.numeric(d$sexo == "Hombre"), d$hba1c, d$pas,
                d$tfg_basal, log(d$uacr), d$ecv)
    z <- if (instrumento == "preferencia") d$preferencia else stats::model.matrix(~ factor(medico), d)[, -1]
    ols <- unname(stats::lm.fit(cbind(1, d$isglt2, cv), d$pendiente_tfg)$coefficients[2])
    est <- dos_etapas(d$pendiente_tfg, d$isglt2, z, cv)
    tibble::tibble(rep = b, ols = ols, iv = est[["efecto"]], F = est[["F"]])
  })
}

# ======================= 19.5  Diferencias en diferencias =======================

#' Estudio de eventos (event study) con lm(): coeficientes de "tratada x tiempo relativo" con
#' efectos fijos de región y de año, referencia = el periodo `ref` y errores estándar agrupados por región.
#' @param d panel con columnas region, anio, tratada (0/1), k_rel (años desde la política) y el desenlace `y`
#' @return lista: tabla (k, est, ee, inf, sup), modelo, vcov agrupada y nombres de los coeficientes previos ("leads")
evento_estudio <- function(d, y = "pct_temprana", ref = -1) {
  d$ev <- ifelse(d$tratada == 1, as.character(d$k_rel), as.character(ref))   # controles: siempre en la referencia
  d$ev <- stats::relevel(factor(d$ev), ref = as.character(ref))
  m <- stats::lm(stats::reformulate(c("ev", "factor(region)", "factor(anio)"), y), data = d)
  V <- sandwich::vcovCL(m, cluster = d$region)
  b <- stats::coef(m); i <- grep("^ev", names(b))
  tabla <- tibble::tibble(k = as.integer(sub("^ev", "", names(b)[i])), est = unname(b[i]),
                          ee = sqrt(diag(V))[i]) |>
    dplyr::bind_rows(tibble::tibble(k = ref, est = 0, ee = 0)) |>
    dplyr::arrange(k) |> dplyr::mutate(inf = est - 1.96 * ee, sup = est + 1.96 * ee)
  list(tabla = tabla, modelo = m, vcov = V, leads = names(b)[i][as.integer(sub("^ev", "", names(b)[i])) < ref])
}

# ======================= 19.5  Regresión discontinua =======================

#' Ajuste local en el umbral (RDD nítida). x debe estar CENTRADA en el umbral (0).
#' Regresión lineal ponderada (kernel triangular) a cada lado, con una pendiente distinta por lado.
#' @return tibble con h, estimación del salto, error estándar robusto (HC1), IC 95 % y n dentro del ancho
rdd_local <- function(d, y, x, h, grado = 1) {
  dd <- tibble::tibble(yy = d[[y]], xx = d[[x]]) |>
    dplyr::filter(abs(xx) < h) |>
    dplyr::mutate(sobre = as.integer(xx >= 0), peso = 1 - abs(xx) / h)      # kernel triangular
  f <- if (grado == 1) yy ~ sobre * xx else yy ~ sobre * (xx + I(xx^2))
  m <- stats::lm(f, data = dd, weights = peso)
  ee <- sqrt(diag(sandwich::vcovHC(m, type = "HC1")))[["sobre"]]
  est <- unname(stats::coef(m)[["sobre"]])
  tibble::tibble(h = h, estimado = est, ee = ee, inf = est - 1.96 * ee, sup = est + 1.96 * ee, n = nrow(dd))
}

#' Ancho de banda por validación cruzada de borde (Imbens y Lemieux, 2008): para cada punto
#' cercano al umbral se predice su resultado con una recta local ajustada SOLO con puntos más
#' alejados del umbral (en su mismo lado); se elige el h con menor error cuadrático medio.
cv_ancho <- function(d, y, x, rejilla, prop = 0.5) {
  yy <- d[[y]]; xx <- d[[x]]
  pred_lado <- function(i, h, lado) {
    u <- xx - xx[i]
    j <- if (lado == "izq") (u < 0 & u > -h & xx < 0) else (u > 0 & u < h & xx >= 0)   # más lejos del umbral
    if (sum(j) < 5) return(NA_real_)
    w <- 1 - abs(u[j]) / h; uj <- u[j]; yj <- yy[j]
    s0 <- sum(w); s1 <- sum(w * uj); s2 <- sum(w * uj^2); t0 <- sum(w * yj); t1 <- sum(w * uj * yj)
    (s2 * t0 - s1 * t1) / (s0 * s2 - s1^2)                                       # intercepto en x_i
  }
  cerca_izq <- which(xx < 0 & xx > stats::quantile(xx[xx < 0], 1 - prop))
  cerca_der <- which(xx >= 0 & xx < stats::quantile(xx[xx >= 0], prop))
  purrr::map_dfr(rejilla, \(h) {
    e <- c(vapply(cerca_izq, \(i) yy[i] - pred_lado(i, h, "izq"), numeric(1)),
           vapply(cerca_der, \(i) yy[i] - pred_lado(i, h, "der"), numeric(1)))
    tibble::tibble(h = h, ecm = mean(e^2, na.rm = TRUE))
  })
}

#' Medias por "cajas" de la variable de asignación (para el gráfico de RDD)
cajas_rdd <- function(d, y, x, ancho) {
  tibble::tibble(yy = d[[y]], xx = d[[x]]) |>
    dplyr::mutate(caja = floor(xx / ancho) * ancho + ancho / 2) |>
    dplyr::group_by(caja) |>
    dplyr::summarise(media = mean(yy), n = dplyr::n(), .groups = "drop")
}

#' Prueba de densidad sencilla en el umbral: cuenta pacientes por cajas de ancho `ancho` en
#' [corte - rango, corte + rango) y ajusta un modelo de Poisson con tendencia lineal (la densidad
#' ya puede ir bajando) y un salto en el corte. Un salto grande de la densidad sugiere manipulación.
#' @return lista con la tabla de conteos y el cociente de conteos (RC) arriba/abajo del corte, IC 95 % y p
prueba_densidad <- function(x, corte, ancho = 10, rango = 100) {
  cajas <- tibble::tibble(x = x) |>
    dplyr::filter(x >= corte - rango, x < corte + rango) |>
    dplyr::count(caja = floor((x - corte) / ancho) * ancho + ancho / 2 + corte, name = "n") |>
    tidyr::complete(caja = seq(corte - rango + ancho / 2, corte + rango - ancho / 2, by = ancho), fill = list(n = 0)) |>
    dplyr::mutate(centro = caja - corte, sobre = as.integer(centro > 0))
  m <- stats::glm(n ~ centro + sobre, data = cajas, family = stats::quasipoisson)
  b <- summary(m)$coefficients["sobre", ]
  list(cajas = cajas, rc = exp(b[["Estimate"]]), inf = exp(b[["Estimate"]] - 1.96 * b[["Std. Error"]]),
       sup = exp(b[["Estimate"]] + 1.96 * b[["Std. Error"]]), p = b[["Pr(>|t|)"]])
}

# ======================= 19.6  Heterogeneidad del efecto =======================

#' CATE con "dos aprendices" (T-learner) y ajuste cruzado en K pliegues.
#' Para cada pliegue: ajusta un modelo del desenlace SOLO en los tratados y otro SOLO en los no
#' tratados (con el resto de pacientes) y predice mu1(x) - mu0(x) en el pliegue de prueba.
#' @param modelo "lm" (regresión lineal) o "ranger" (bosque aleatorio)
t_learner <- function(d, y, a, covs, modelo = c("lm", "ranger"), K = 5, semilla = 1) {
  modelo <- match.arg(modelo); set.seed(semilla)
  pliegue <- sample(rep_len(seq_len(K), nrow(d)))
  cate <- numeric(nrow(d))
  ajustar <- function(tr) {
    if (modelo == "lm") {
      m <- stats::lm(stats::reformulate(covs, y), data = tr)
      function(nuevo) stats::predict(m, nuevo)
    } else {
      m <- ranger::ranger(stats::reformulate(covs, y), data = tr, num.trees = 300, min.node.size = 20)
      function(nuevo) stats::predict(m, nuevo)$predictions
    }
  }
  for (k in seq_len(K)) {
    tr <- d[pliegue != k, ]; te <- d[pliegue == k, ]
    f1 <- ajustar(tr[tr[[a]] == 1, ]); f0 <- ajustar(tr[tr[[a]] == 0, ])
    cate[pliegue == k] <- f1(te) - f0(te)
  }
  cate
}

#' Efecto medio por subgrupo con fórmula g (modelo del desenlace con interacciones)
efecto_g_grupo <- function(d, grupo, f_out) {
  m <- stats::lm(f_out, data = d)
  dif <- stats::predict(m, dplyr::mutate(d, isglt2 = 1)) - stats::predict(m, dplyr::mutate(d, isglt2 = 0))
  tapply(dif, d[[grupo]], mean)
}

#' Efecto medio por subgrupo con IPW (PS estimado con todos; medias ponderadas dentro del subgrupo)
efecto_ipw_grupo <- function(d, grupo, f_ps) {
  ps <- stats::fitted(stats::glm(f_ps, data = d, family = stats::binomial))
  w <- ifelse(d$isglt2 == 1, 1 / ps, 1 / (1 - ps))
  vapply(split(seq_len(nrow(d)), d[[grupo]]), \(i) {
    stats::weighted.mean(d$pendiente_tfg[i][d$isglt2[i] == 1], w[i][d$isglt2[i] == 1]) -
      stats::weighted.mean(d$pendiente_tfg[i][d$isglt2[i] == 0], w[i][d$isglt2[i] == 0])
  }, numeric(1))
}

#' Bootstrap (percentil) de una función que devuelve un vector con nombres
boot_vec <- function(d, fn, B = 200, semilla = 1) {
  set.seed(semilla)
  est <- fn(d)
  r <- replicate(B, fn(d[sample.int(nrow(d), replace = TRUE), ]))
  tibble::tibble(grupo = names(est), estimado = as.numeric(est),
                 inf = apply(r, 1, stats::quantile, 0.025), sup = apply(r, 1, stats::quantile, 0.975))
}

#' Calibración por deciles del CATE estimado: media estimada vs media del efecto verdadero
calibracion_deciles <- function(cate, ite, k = 10) {
  tibble::tibble(cate, ite) |>
    dplyr::mutate(decil = dplyr::ntile(cate, k)) |>
    dplyr::group_by(decil) |>
    dplyr::summarise(estimado = mean(cate), verdadero = mean(ite), .groups = "drop")
}
