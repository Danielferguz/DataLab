# ------------------------------------------------------------------------
# Región simulada para el capítulo de muestreo (datos SIMULADOS)
# ------------------------------------------------------------------------
# Población: adultos de una región con 600 conglomerados (comunidades o
# sectores): 360 urbanos y 240 rurales. Variable de interés: ERC (sí/no).
# La prevalencia verdadera de la población se calcula directamente.
# ------------------------------------------------------------------------

simular_poblacion_erc <- function(semilla = 123) {
  set.seed(semilla)
  n_cl <- 600
  cl <- tibble::tibble(
    cluster = 1:n_cl,
    area = c(rep("Urbano", 360), rep("Rural", 240)),
    tam = rpois(n_cl, ifelse(area == "Urbano", 130, 70)) + 20,
    u_cl = rnorm(n_cl, 0, 0.55)                        # variación entre conglomerados
  )
  pob <- cl[rep(seq_len(n_cl), cl$tam), ] |>
    dplyr::mutate(
      id = dplyr::row_number(),
      edad = round(pmin(pmax(rnorm(dplyr::n(), ifelse(area == "Rural", 50, 46), 15), 18), 90)),
      sexo = sample(c("F", "M"), dplyr::n(), TRUE),
      diabetes = rbinom(dplyr::n(), 1, plogis(-3.2 + 0.035 * (edad - 45) + 0.3 * (area == "Urbano") + u_cl * 0.4)),
      hta = rbinom(dplyr::n(), 1, plogis(-2.2 + 0.045 * (edad - 45) + 0.2 * (area == "Rural") + u_cl * 0.4)),
      erc = rbinom(dplyr::n(), 1, plogis(-3.3 + 0.045 * (edad - 45) + 0.9 * diabetes + 0.6 * hta +
                                          0.35 * (area == "Rural") + u_cl))
    ) |>
    dplyr::select(id, cluster, area, edad, sexo, diabetes, hta, erc)
  pob
}

# Muestreo bietápico estratificado: se eligen conglomerados al azar dentro de cada
# estrato y luego 25 adultos al azar dentro de cada conglomerado elegido.
# Las zonas rurales se sobremuestrean (más conglomerados) -> pesos distintos.
muestrear_bietapico <- function(pob, conglom = c(Urbano = 30, Rural = 30), por_cluster = 25, semilla = 321) {
  set.seed(semilla)
  marco <- pob |> dplyr::count(cluster, area, name = "tam")
  n_h <- marco |> dplyr::count(area, name = "N_conglom")
  elegidos <- marco |>
    dplyr::group_by(area) |>
    dplyr::group_modify(~ dplyr::slice_sample(.x, n = conglom[[.y$area]])) |>
    dplyr::ungroup() |>
    dplyr::left_join(n_h, by = "area") |>
    dplyr::mutate(n_conglom = conglom[area],
                  p1 = n_conglom / N_conglom,           # prob. de elegir el conglomerado
                  p2 = por_cluster / tam,               # prob. de elegir al adulto dentro del conglomerado
                  peso = 1 / (p1 * p2))
  pob |>
    dplyr::inner_join(dplyr::select(elegidos, cluster, peso, tam, N_conglom), by = "cluster") |>
    dplyr::group_by(cluster) |>
    dplyr::slice_sample(n = por_cluster) |>
    dplyr::ungroup()
}

# Pacientes que consultan a una clínica de nefrología: la probabilidad de consultar
# depende de tener ERC, diabetes, HTA y edad -> muestra de conveniencia SESGADA.
simular_consulta_clinica <- function(pob, semilla = 7) {
  set.seed(semilla)
  p <- plogis(-6.5 + 2.2 * pob$erc + 0.8 * pob$diabetes + 0.5 * pob$hta + 0.02 * (pob$edad - 45))
  pob[rbinom(nrow(pob), 1, p) == 1, ]
}

# No respuesta en la encuesta: responder es menos probable con ERC, mayor edad y zona rural.
simular_no_respuesta <- function(m, semilla = 11) {
  set.seed(semilla)
  m$respondio <- rbinom(nrow(m), 1, plogis(1.4 - 1.0 * m$erc - 0.02 * (m$edad - 45) - 0.4 * (m$area == "Rural")))
  m
}

guardar_encuesta_erc <- function(carpeta = "Bases") {
  pob <- simular_poblacion_erc()
  m <- muestrear_bietapico(pob)
  readr::write_csv(m, file.path(carpeta, "encuesta_erc.csv"))
  invisible(m)
}
