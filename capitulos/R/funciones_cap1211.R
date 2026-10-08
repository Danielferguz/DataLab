# ------------------------------------------------------------------------
# Funciones de apoyo del subcapítulo 12.11 (aprendizaje automático y TMLE)
# Uso:   source("R/funciones_cap1211.R")   (requiere tidyverse cargado)
# Paquetes: glmnet, ranger, gbm, rpart. Todos los datos del capítulo son SIMULADOS.
#
# Convención: un "aprendiz" es una función  f(x, y)  que ajusta un modelo con la
# matriz de covariables `x` y el desenlace `y` (siempre en [0, 1]: binario o
# continuo reescalado) y DEVUELVE otra función  predecir(nuevo_x)  con las
# predicciones. Así todos los aprendices (y el super learner) son intercambiables.
# ------------------------------------------------------------------------

#' Matriz numérica de covariables (los factores pasan a indicadoras 0/1)
matriz_cov <- function(d, covs) stats::model.matrix(~ ., data = d[covs])[, -1, drop = FALSE]

es_binario <- function(y) all(y %in% c(0, 1))
acotar <- function(p, eps = 0.001) pmin(pmax(p, eps), 1 - eps)

# --- Aprendices ----------------------------------------------------------

#' Regresión (logística si y es 0/1; "cuasibinomial" si y es continuo en [0,1]), sin interacciones
aprendiz_glm <- function(x, y) {
  datos <- data.frame(y = y, x)
  fam <- if (es_binario(y)) stats::binomial() else stats::quasibinomial()
  m <- suppressWarnings(stats::glm(y ~ ., data = datos, family = fam))
  function(nuevo) suppressWarnings(as.numeric(stats::predict(m, data.frame(nuevo), type = "response")))
}

#' Amplía la matriz con cuadrados e interacciones de a pares (para que LASSO pueda "ver" curvas)
ampliar_x <- function(x) {
  cont <- colnames(x)[apply(x, 2, function(v) length(unique(v)) > 2)]
  cuad <- x[, cont, drop = FALSE]^2; colnames(cuad) <- paste0(cont, "^2")
  pares <- utils::combn(colnames(x), 2, simplify = FALSE)
  inter <- sapply(pares, function(p) x[, p[1]] * x[, p[2]])
  colnames(inter) <- sapply(pares, paste, collapse = ":")
  cbind(x, cuad, inter)
}

#' LASSO (glmnet) con penalización elegida por validación cruzada; opcionalmente con términos ampliados
aprendiz_lasso <- function(x, y, ampliar = TRUE) {
  xe <- if (ampliar) ampliar_x(x) else x
  fam <- if (es_binario(y)) "binomial" else "gaussian"
  m <- glmnet::cv.glmnet(xe, y, family = fam, alpha = 1, nfolds = 5)
  function(nuevo) as.numeric(predict(m, if (ampliar) ampliar_x(nuevo) else nuevo, s = "lambda.1se", type = "response"))
}

#' Bosque aleatorio (ranger)
aprendiz_bosque <- function(x, y, arboles = 300, nodo_min = 20) {
  if (es_binario(y)) {
    m <- ranger::ranger(x = x, y = factor(y, levels = 0:1), probability = TRUE, num.trees = arboles, min.node.size = nodo_min)
    function(nuevo) predict(m, nuevo)$predictions[, "1"]
  } else {
    m <- ranger::ranger(x = x, y = y, num.trees = arboles, min.node.size = nodo_min)
    function(nuevo) predict(m, nuevo)$predictions
  }
}

#' Boosting de árboles (gbm): 300 árboles pequeños, tasa de aprendizaje 0.05
aprendiz_boosting <- function(x, y, arboles = 300, profundidad = 3) {
  m <- gbm::gbm.fit(as.data.frame(x), y, distribution = if (es_binario(y)) "bernoulli" else "gaussian",
                    n.trees = arboles, interaction.depth = profundidad, shrinkage = 0.05,
                    bag.fraction = 0.5, n.minobsinnode = 20, verbose = FALSE)
  function(nuevo) predict(m, as.data.frame(nuevo), n.trees = arboles, type = "response")
}

#' Árbol de decisión (rpart) podado con el parámetro de complejidad de menor error de validación cruzada
aprendiz_arbol <- function(x, y) {
  datos <- data.frame(y = y, x)
  m <- rpart::rpart(y ~ ., data = datos, method = "anova", control = rpart::rpart.control(cp = 0.001, minbucket = 20))
  cp_opt <- m$cptable[which.min(m$cptable[, "xerror"]), "CP"]
  m <- rpart::prune(m, cp = cp_opt)
  function(nuevo) as.numeric(predict(m, data.frame(nuevo)))
}

# --- Validación cruzada y cross-fitting -----------------------------------

#' Asigna cada paciente a uno de K pliegues al azar
pliegues <- function(n, K = 5, semilla = 1) {
  set.seed(semilla)
  sample(rep(seq_len(K), length.out = n))
}

#' Predicciones "fuera de pliegue": cada paciente se predice con un modelo que NO lo vio
predecir_cruzado <- function(aprendiz, x, y, K = 5, semilla = 1) {
  f <- pliegues(nrow(x), K, semilla)
  p <- numeric(nrow(x))
  for (k in seq_len(K)) p[f == k] <- aprendiz(x[f != k, , drop = FALSE], y[f != k])(x[f == k, , drop = FALSE])
  p
}

#' Ajusta los dos "modelos auxiliares" (desenlace por brazo y puntaje de propensión) con cross-fitting.
#' K = 1 significa SIN cross-fitting: se ajusta y se predice en los mismos pacientes (riesgo de sobreajuste).
#' @param y desenlace en [0,1]   @param a tratamiento 0/1   @param x matriz de covariables
#' @return lista con Q1, Q0 (desenlace esperado si todos/ninguno tratados) y g (PS)
auxiliares <- function(y, a, x, aprendiz_q, aprendiz_g = aprendiz_q, K = 5, semilla = 1) {
  n <- length(y)
  f <- if (K == 1) rep(1, n) else pliegues(n, K, semilla)
  Q1 <- Q0 <- g <- numeric(n)
  for (k in seq_len(max(f))) {
    ent <- if (K == 1) rep(TRUE, n) else f != k          # pacientes de ajuste
    val <- f == k                                         # pacientes donde se predice
    Q1[val] <- aprendiz_q(x[ent & a == 1, , drop = FALSE], y[ent & a == 1])(x[val, , drop = FALSE])
    Q0[val] <- aprendiz_q(x[ent & a == 0, , drop = FALSE], y[ent & a == 0])(x[val, , drop = FALSE])
    g[val]  <- aprendiz_g(x[ent, , drop = FALSE], a[ent])(x[val, , drop = FALSE])
  }
  list(Q1 = acotar(Q1), Q0 = acotar(Q0), g = g)
}

# --- Estimadores (todos con el desenlace y en [0,1]) -----------------------

#' AIPW con su error estándar (curva de influencia)
estimar_aipw <- function(y, a, Q1, Q0, g, cota = 0.025) {
  g <- pmin(pmax(g, cota), 1 - cota)
  psi_i <- Q1 - Q0 + a * (y - Q1) / g - (1 - a) * (y - Q0) / (1 - g)
  c(est = mean(psi_i), ee = stats::sd(psi_i) / sqrt(length(y)))
}

#' TMLE para el ATE, con el desenlace en [0,1] (binario o reescalado). Devuelve estimación,
#' error estándar, epsilon y el valor inicial (fórmula g) para comparar.
tmle_manual <- function(y, a, Q1, Q0, g, cota = 0.025) {
  g <- pmin(pmax(g, cota), 1 - cota)
  QA <- ifelse(a == 1, Q1, Q0)
  H  <- a / g - (1 - a) / (1 - g)                                  # covariable "clever"
  eps <- unname(stats::coef(suppressWarnings(
    stats::glm(y ~ -1 + H + offset(stats::qlogis(QA)), family = stats::quasibinomial())))["H"])
  Q1s <- stats::plogis(stats::qlogis(Q1) + eps / g)                # Q1 actualizada
  Q0s <- stats::plogis(stats::qlogis(Q0) - eps / (1 - g))          # Q0 actualizada
  psi <- mean(Q1s - Q0s)
  D <- H * (y - ifelse(a == 1, Q1s, Q0s)) + Q1s - Q0s - psi       # curva de influencia
  c(est = psi, ee = stats::sd(D) / sqrt(length(y)), eps = eps, inicial = mean(Q1 - Q0))
}

#' Análisis completo: reescala el desenlace, ajusta los auxiliares y calcula fórmula g, IPW, AIPW y TMLE
#' en la escala ORIGINAL del desenlace.
#' @param y desenlace (continuo o 0/1)  @param a tratamiento 0/1  @param x matriz de covariables
analizar <- function(y, a, x, aprendiz_q, aprendiz_g = aprendiz_q, K = 5, semilla = 1, cota = 0.025) {
  lo <- if (es_binario(y)) 0 else min(y); amp <- if (es_binario(y)) 1 else max(y) - min(y)
  ys <- (y - lo) / amp
  aux <- auxiliares(ys, a, x, aprendiz_q, aprendiz_g, K, semilla)
  g <- pmin(pmax(aux$g, cota), 1 - cota)
  ipw <- stats::weighted.mean(ys[a == 1], 1 / g[a == 1]) - stats::weighted.mean(ys[a == 0], 1 / (1 - g[a == 0]))
  ai <- estimar_aipw(ys, a, aux$Q1, aux$Q0, aux$g, cota)
  tm <- tmle_manual(ys, a, aux$Q1, aux$Q0, aux$g, cota)
  tibble::tibble(metodo = c("Fórmula g", "IPW", "AIPW", "TMLE"),
                 estimado = amp * c(mean(aux$Q1 - aux$Q0), ipw, ai[["est"]], tm[["est"]]),
                 ee = amp * c(NA, NA, ai[["ee"]], tm[["ee"]])) |>
    dplyr::mutate(ic_inf = estimado - 1.96 * ee, ic_sup = estimado + 1.96 * ee)
}

# --- Mini super learner ------------------------------------------------------

#' Pesos no negativos que minimizan el error cuadrático de las predicciones de validación cruzada
#' (mínimos cuadrados no negativos con optim); se normalizan para que sumen 1
pesos_nnls <- function(Z, y) {
  J <- ncol(Z)
  ajuste <- stats::optim(rep(1 / J, J), function(w) sum((y - Z %*% w)^2), method = "L-BFGS-B", lower = rep(0, J))
  w <- pmax(ajuste$par, 0)
  w / sum(w)
}

#' Construye un aprendiz que combina `candidatos` (lista nombrada de aprendices) con pesos de validación cruzada.
#' La función devuelta lleva los atributos `pesos` y `error_cv` (error cuadrático medio de cada candidato).
super_aprendiz <- function(candidatos, K = 5, semilla = 1) {
  function(x, y) {
    Z <- sapply(candidatos, function(ap) predecir_cruzado(ap, x, y, K, semilla))
    w <- pesos_nnls(Z, y)
    ajustes <- lapply(candidatos, function(ap) ap(x, y))
    predecir <- function(nuevo) as.numeric(sapply(ajustes, function(h) h(nuevo)) %*% w)
    attr(predecir, "pesos") <- stats::setNames(w, names(candidatos))
    attr(predecir, "error_cv") <- stats::setNames(colMeans((y - Z)^2), names(candidatos))
    predecir
  }
}

# --- Simulación con no linealidades (verdad conocida) ------------------------

#' Cohorte SIMULADA donde el tratamiento y el desenlace dependen de las covariables de forma NO lineal
#' (curvas e interacciones). Devuelve la base y la verdad (ATE de la muestra, con ambos resultados potenciales).
simular_no_lineal <- function(n = 3000, semilla = 11) {
  set.seed(semilla)
  edad <- rnorm(n); tfg <- rnorm(n); luacr <- rnorm(n); hba1c <- rnorm(n); ecv <- rbinom(n, 1, 0.3)  # estandarizadas
  lp <- 0.2 - 0.9 * edad^2 + 0.6 * luacr + 0.7 * tfg * luacr + 0.3 * hba1c + 0.3 * ecv
  a  <- rbinom(n, 1, plogis(lp))
  mu0 <- -2 - 0.8 * edad^2 - 0.6 * luacr - 0.8 * tfg * luacr + 0.5 * tfg + 0.3 * ecv + 0.2 * hba1c
  ite <- 0.9 + 0.5 * luacr + 0.5 * (tfg > 0)
  e <- rnorm(n)
  y <- ifelse(a == 1, mu0 + ite, mu0) + e
  list(datos = tibble::tibble(edad, tfg, luacr, hba1c, ecv, a, y), verdad = mean(ite))
}
