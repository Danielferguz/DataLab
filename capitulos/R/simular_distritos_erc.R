# ------------------------------------------------------------------------
# BASE ESPACIAL SIMULADA: 60 distritos con pacientes en diálisis (Cap. 15.6)
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS y los distritos son INVENTADOS (una malla de
# 6 filas x 10 columnas de unos 10 x 10 km, con vértices desplazados al azar
# para que parezcan distritos irregulares). No corresponden a ningún lugar real.
#
# Variables observadas (distritos_erc.csv):
#   id_distrito, fila, col, x_km, y_km (centroide), poblacion,
#   pacientes_dialisis (prevalentes), pobreza_pct, dist_centro_km (al centro
#   de diálisis más cercano)
#
# Modelo generador (escala log) para el distrito i:
#   log lambda_i = log(60 / 100 000)                       # prevalencia basal: 60 por 100 000
#                  + 0.15 * (pobreza_pct - 30) / 10        # +16 % por cada +10 puntos de pobreza
#                  - 0.012 * (dist_centro_km - 15)         # -1.2 % por cada km más lejos del centro de diálisis
#                  + xi_i                                  # campo ESPACIAL (autocorrelación residual)
#   xi_i = campo gaussiano suave (covarianza exponencial, rango ~ 20 km, DE 0.15)
#          + "foco": +0.40 en los distritos a <= 14 km del punto (72, 18) (valle con alta carga)
#   pacientes_dialisis_i ~ Poisson(poblacion_i * lambda_i)
#   pobreza_pct (espacialmente suave): 30 + 10 * campo gaussiano (rango ~ 35 km)
#
# La semilla 410 se eligió entre 500 candidatas para que el fenómeno sea claro al
# enseñarlo (autocorrelación espacial evidente, un foco y un punto frío detectables,
# coeficientes estimados cerca de los verdaderos); no cambia la verdad del modelo.
#
# VERDAD CONOCIDA: ver distritos_erc_oraculo.csv (tasa verdadera lambda_i por 100 000 y
# el término espacial xi_i). El coeficiente verdadero de pobreza es +0.15 por cada
# 10 puntos (RR = 1.16) y el de la distancia es -0.012 por km (RR = 0.89 por cada +10 km).
# La tasa de diálisis NO es la carga de enfermedad: donde el acceso es peor
# (lejos del centro) se dializa a menos gente aunque haya más ERC.
# ------------------------------------------------------------------------

# --- Geometría: vértices de la malla con desplazamiento aleatorio ---------
.vertices_malla <- function(n_fila = 6, n_col = 10, lado = 10, semilla = 410) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(semilla)
  g <- expand.grid(i = 0:n_fila, j = 0:n_col)
  g$x <- g$j * lado + runif(nrow(g), -2.5, 2.5)
  g$y <- g$i * lado + runif(nrow(g), -2.5, 2.5)
  g
}

#' Polígonos (sf) de los 60 distritos inventados. Coordenadas en km (sin CRS).
crear_poligonos_distritos <- function(n_fila = 6, n_col = 10, lado = 10, semilla = 410) {
  v <- .vertices_malla(n_fila, n_col, lado, semilla)
  pt <- function(i, j) as.numeric(v[v$i == i & v$j == j, c("x", "y")])
  celdas <- expand.grid(fila = 1:n_fila, col = 1:n_col)
  geom <- lapply(seq_len(nrow(celdas)), function(k) {
    f <- celdas$fila[k]; cl <- celdas$col[k]
    sf::st_polygon(list(rbind(pt(f - 1, cl - 1), pt(f - 1, cl), pt(f, cl), pt(f, cl - 1), pt(f - 1, cl - 1))))
  })
  celdas$id_distrito <- sprintf("D%02d", seq_len(nrow(celdas)))
  sf::st_sf(celdas[, c("id_distrito", "fila", "col")], geometry = sf::st_sfc(geom))
}

simular_distritos_erc <- function(semilla = 410, n_fila = 6, n_col = 10, lado = 10) {
  v <- .vertices_malla(n_fila, n_col, lado, semilla)
  pt <- function(i, j) as.numeric(v[v$i == i & v$j == j, c("x", "y")])
  celdas <- expand.grid(fila = 1:n_fila, col = 1:n_col)
  cen <- t(sapply(seq_len(nrow(celdas)), function(k) {   # centroide = promedio de los 4 vértices
    f <- celdas$fila[k]; cl <- celdas$col[k]
    colMeans(rbind(pt(f - 1, cl - 1), pt(f - 1, cl), pt(f, cl), pt(f, cl - 1)))
  }))
  n <- nrow(celdas)

  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(semilla + 1)

  d <- as.matrix(dist(cen))
  campo <- function(rango, de) as.numeric(t(chol(de^2 * exp(-d / rango) + diag(1e-8, n))) %*% rnorm(n))
  pobreza <- pmin(pmax(30 + 10 * campo(35, 1), 5), 70)
  # Dos centros de diálisis (en dos distritos concretos)
  centros <- cen[c(which(celdas$fila == 2 & celdas$col == 3), which(celdas$fila == 5 & celdas$col == 8)), , drop = FALSE]
  dist_centro <- apply(cen, 1, \(p) min(sqrt((centros[, 1] - p[1])^2 + (centros[, 2] - p[2])^2)))
  foco <- as.numeric(sqrt((cen[, 1] - 72)^2 + (cen[, 2] - 18)^2) <= 14)
  xi <- campo(20, 0.15) + 0.40 * foco
  poblacion <- round(exp(rnorm(n, log(45000), 0.65)) / 100) * 100
  poblacion <- pmin(pmax(poblacion, 8000), 250000)
  lambda <- (60 / 1e5) * exp(0.15 * (pobreza - 30) / 10 - 0.012 * (dist_centro - 15) + xi)
  casos <- rpois(n, poblacion * lambda)

  d_obs <- tibble::tibble(id_distrito = sprintf("D%02d", seq_len(n)), fila = celdas$fila, col = celdas$col,
                          x_km = round(cen[, 1], 2), y_km = round(cen[, 2], 2), poblacion = poblacion,
                          pacientes_dialisis = casos, pobreza_pct = round(pobreza, 1),
                          dist_centro_km = round(dist_centro, 1))
  d_orac <- tibble::tibble(id_distrito = d_obs$id_distrito, tasa_verdadera_100mil = 1e5 * lambda,
                           xi_espacial = xi, foco = foco)
  list(datos = d_obs, oraculo = d_orac)
}

# Para regenerar los archivos CSV de la carpeta Bases/ (ejecutar a mano):
#   source("capitulos/R/simular_distritos_erc.R"); guardar_distritos_erc("capitulos/Bases")
guardar_distritos_erc <- function(carpeta = "Bases") {
  x <- simular_distritos_erc()
  readr::write_csv(x$datos, file.path(carpeta, "distritos_erc.csv"))
  readr::write_csv(x$oraculo, file.path(carpeta, "distritos_erc_oraculo.csv"))
  invisible(x)
}
