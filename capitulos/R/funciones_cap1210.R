# ------------------------------------------------------------------------
# Funciones de apoyo para el subcapítulo 12.10 (requiere tidyverse, survey, sandwich y R/funciones_epi.R)
# ------------------------------------------------------------------------

#' Peso IPW (ATE) de un tratamiento binario: 1/PS en tratados y 1/(1-PS) en no tratados
peso_ipw <- function(ps, a) ifelse(a == 1, 1 / ps, 1 / (1 - ps))

#' Efecto de A sobre el desenlace con un diseño muestral cuyos pesos son `w` (peso muestral x peso del PS)
#' Devuelve estimado, EE con el diseño (conglomerados + estratos) e IC 95 %.
efecto_diseno <- function(datos, w, formula = pendiente_tfg ~ isglt2, termino = "isglt2") {
  dis <- survey::svydesign(ids = ~cluster, strata = ~area, weights = w, data = datos, nest = TRUE)
  fit <- survey::svyglm(formula, dis)
  ic <- stats::confint(fit)[termino, ]
  tibble::tibble(estimado = unname(stats::coef(fit)[termino]), ee = unname(survey::SE(fit)[termino]),
                 ic_inf = unname(ic[1]), ic_sup = unname(ic[2]))
}

#' IPW "dentro" de una base (para repetirlo en cada imputación): PS logístico, pesos ATE y efecto ponderado
#' @param ps PS ya calculado (para el enfoque "promediar PS"); si es NULL, se estima en la base.
#' Devuelve el estimado, su varianza robusta (HC1) y el PS usado.
ipw_una_base <- function(d, formula_ps, ps = NULL, y = "pendiente_tfg", a = "isglt2") {
  if (is.null(ps)) ps <- stats::fitted(stats::glm(formula_ps, data = d, family = binomial))
  w <- peso_ipw(ps, d[[a]])
  fit <- stats::lm(stats::reformulate(a, y), data = d, weights = w)
  list(estimado = unname(stats::coef(fit)[a]), varianza = sandwich::vcovHC(fit, "HC1")[a, a], ps = ps)
}

#' DEM máxima entre todos los pares de niveles de un tratamiento con 3 o más niveles
dem_max_pares <- function(x, g, w = rep(1, length(x))) {
  pares <- utils::combn(levels(g), 2, simplify = FALSE)
  max(vapply(pares, \(p) { k <- g %in% p; abs(smd(x[k], as.integer(g[k] == p[2]), w[k])) }, numeric(1)))
}

#' Contrastes por pares de una regresión lm(y ~ trat) con 3 niveles (el primero es la referencia)
#' @param V matriz de varianzas de los coeficientes (p. ej., sandwich::vcovHC)
contrastes_pares <- function(fit, V) {
  K <- cbind(1, diag(3)[, -1])                      # medias de cada nivel = intercepto + coeficiente
  pares <- list(`aGLP-1 - Ninguno` = c(2, 1), `iSGLT2 - Ninguno` = c(3, 1), `iSGLT2 - aGLP-1` = c(3, 2))
  purrr::imap_dfr(pares, \(p, nm) {
    v <- K[p[1], ] - K[p[2], ]
    est <- sum(v * stats::coef(fit)); ee <- sqrt(drop(t(v) %*% V %*% v))
    tibble::tibble(contraste = nm, estimado = est, ee = ee, ic_inf = est - 1.96 * ee, ic_sup = est + 1.96 * ee)
  })
}
