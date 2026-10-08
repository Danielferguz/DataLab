# Funciones auxiliares del Cap. 8.7 (datos simulados; ver R/simular_cap87.R)

#' IRR del programa con cuatro estrategias para conteos: Poisson ingenuo, Poisson con
#' errores robustos (sandwich), cuasi-Poisson y binomial negativa. Devuelve una fila por
#' estrategia con IRR, EE (escala log), IC 95 % de Wald.
ajustar_conteos <- function(d, coef_interes = "programa") {
  f <- ingresos ~ programa + diabetes + offset(log(seguimiento_anios))
  m_p <- glm(f, data = d, family = poisson)
  m_q <- glm(f, data = d, family = quasipoisson)
  m_n <- suppressWarnings(MASS::glm.nb(f, data = d))
  ee <- c(summary(m_p)$coef[coef_interes, 2],
          sqrt(diag(sandwich::vcovHC(m_p, type = "HC0")))[coef_interes],
          summary(m_q)$coef[coef_interes, 2],
          summary(m_n)$coef[coef_interes, 2])
  b <- c(coef(m_p)[coef_interes], coef(m_p)[coef_interes], coef(m_q)[coef_interes], coef(m_n)[coef_interes])
  tibble::tibble(modelo = c("Poisson", "Poisson + sándwich", "Cuasi-Poisson", "Binomial negativa"),
                 irr = exp(b), ee = unname(ee), inf = exp(b - 1.96 * ee), sup = exp(b + 1.96 * ee))
}

#' OR del tratamiento en cada corte de una variable ordinal `alb12` (A1 < A2 < A3):
#' una logística para "A2 o A3 vs A1" y otra para "A3 vs A1 o A2". Si los dos OR son
#' parecidos, el supuesto de odds proporcionales es razonable.
or_por_corte <- function(d, rhs = "isglt2 + I(edad / 10) + hba1c + pas + ecv") {
  purrr::map_dfr(c("A2 o A3 vs A1" = 2, "A3 vs A1 o A2" = 3), \(k) {
    d$y <- as.integer(as.integer(d$alb12) >= k)
    m <- glm(stats::as.formula(paste("y ~", rhs)), data = d, family = binomial)
    ci <- confint.default(m)["isglt2", ]
    tibble::tibble(or = exp(coef(m)["isglt2"]), inf = exp(ci[1]), sup = exp(ci[2]))
  }, .id = "corte")
}
