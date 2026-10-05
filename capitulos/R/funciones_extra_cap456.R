# ------------------------------------------------------------------------
# Funciones extra de los Capítulos 4, 5 y 6 (R, limpieza de datos y muestreo)
#   source("R/funciones_extra_cap456.R")
#
# Cap. 5:
#   registrar_paso()   diario de limpieza: filas y pacientes tras cada paso
#   antes_despues()    cuántos valores distintos tenía una columna antes y después
# Cap. 6 (encuestas con el paquete {survey}):
#   prev_svy()         prevalencia ponderada (con IC) de una variable 0/1, total o por grupos
#   tabla_svy_categ()  "Tabla 1" de variables categóricas: n sin ponderar, % sin ponderar,
#                      % ponderado con IC (equivale a gtsummary::tbl_svysummary)
#   tabla_or_svy()     OR crudos y ajustados de un modelo con el diseño (svyglm)
# ------------------------------------------------------------------------

# ---- Cap. 5 ---------------------------------------------------------------

#' Añade una fila al diario de limpieza
#' @param diario tibble con el diario (o NULL para empezar uno)
#' @param paso   texto que describe el paso
#' @param datos  base tal como queda después del paso (con la columna `id_paciente`)
registrar_paso <- function(diario = NULL, paso, datos) {
  nueva <- tibble::tibble(paso = paso, filas = nrow(datos),
                          pacientes = dplyr::n_distinct(datos$id_paciente))
  dplyr::bind_rows(diario, nueva)
}

#' Número de valores distintos de una columna antes y después de limpiarla
#' @param antes,despues listas con nombre: cada elemento es el vector (columna) antes y después
antes_despues <- function(antes, despues) {
  tibble::tibble(variable = names(antes),
                 categorias_antes   = purrr::map_int(antes,   dplyr::n_distinct),
                 categorias_despues = purrr::map_int(despues, dplyr::n_distinct))
}

# ---- Cap. 6 ---------------------------------------------------------------

#' Prevalencia ponderada con IC (logit) de una variable 0/1 con un diseño de {survey}
#' @param dis    objeto svydesign
#' @param y      nombre (texto) de la variable 0/1
#' @param por    nombre (texto) de la variable de agrupación; NULL = total
#' @return tibble con nivel, prevalencia, inf y sup (proporciones)
prev_svy <- function(dis, y = "erc", por = NULL) {
  f_y <- stats::as.formula(paste("~", y))
  if (is.null(por)) {
    ci <- survey::svyciprop(f_y, dis, method = "logit")
    return(tibble::tibble(variable = "Total", nivel = "Región",
                          prevalencia = as.numeric(ci),
                          inf = attr(ci, "ci")[1], sup = attr(ci, "ci")[2]))
  }
  niveles <- levels(factor(dis$variables[[por]]))
  purrr::map_dfr(niveles, \(n) {
    sub <- eval(bquote(subset(dis, .(as.name(por)) == .(n))))
    ci <- survey::svyciprop(f_y, sub, method = "logit")
    tibble::tibble(variable = por, nivel = n, prevalencia = as.numeric(ci),
                   inf = attr(ci, "ci")[1], sup = attr(ci, "ci")[2])
  })
}

#' "Tabla 1" de variables categóricas: sin ponderar vs. ponderada con IC
#' @param datos base (data.frame) y `dis` su objeto de diseño
#' @param vars  vector de nombres de variables categóricas (o 0/1)
tabla_svy_categ <- function(datos, dis, vars) {
  purrr::map_dfr(vars, \(v) {
    x <- factor(datos[[v]])
    purrr::map_dfr(levels(x), \(n) {
      ind <- as.numeric(x == n)
      dis2 <- stats::update(dis, .ind = ind)
      ci <- survey::svyciprop(~.ind, dis2, method = "logit")
      tibble::tibble(variable = v, nivel = n,
                     n_sin_pond = sum(ind), pct_sin_pond = 100 * mean(ind),
                     pct_pond = 100 * as.numeric(ci),
                     ic_inf = 100 * attr(ci, "ci")[1], ic_sup = 100 * attr(ci, "ci")[2])
    })
  })
}

#' OR crudos y ajustados de un modelo logístico con el diseño de la encuesta
#' @param dis       objeto svydesign
#' @param desenlace nombre (texto) del desenlace 0/1
#' @param predictores vector de nombres (o expresiones tipo "I(edad/10)")
tabla_or_svy <- function(dis, desenlace, predictores) {
  ajustar <- function(f) {
    m <- survey::svyglm(stats::as.formula(f), design = dis, family = stats::quasibinomial)
    b <- stats::coef(m); ic <- stats::confint(m)
    tibble::tibble(termino = names(b), or = exp(b), inf = exp(ic[, 1]), sup = exp(ic[, 2])) |>
      dplyr::filter(termino != "(Intercept)")
  }
  crudos <- purrr::map_dfr(predictores, \(p) ajustar(paste(desenlace, "~", p)))
  multi  <- ajustar(paste(desenlace, "~", paste(predictores, collapse = " + ")))
  dplyr::left_join(crudos, multi, by = "termino", suffix = c("_crudo", "_ajustado"))
}
