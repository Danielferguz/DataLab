# ------------------------------------------------------------------------
# Funciones extra de los Capítulos 1-3 (sin dependencias fuera de tidyverse/base)
#   source("R/funciones_extra_a.R")
#
# 1) interpretar_asociacion(): redacta la frase de interpretación de una medida
#    de asociación (RP, RR, RTI, OR, HR) siguiendo el esquema de "plantillas de
#    interpretación" que usa el autor en sus cursos (EviSalud): población,
#    medida, desenlace, grupos, magnitud y significación.
# 2) caminos_dag(), conjuntos_ajuste(): alternativa propia, ejecutable aquí, a
#    {dagitty}: enumeran los caminos entre exposición y desenlace, dicen cuáles
#    están abiertos y buscan los conjuntos mínimos de ajuste (criterio de la
#    puerta trasera).
# ------------------------------------------------------------------------

# ---- 1. Plantilla de interpretación --------------------------------------

#' Frase de interpretación de una razón (RP, RR, RTI, OR o HR)
#' @param medida "RP", "RR", "RTI", "OR" o "HR"
#' @param estimado,ic_inf,ic_sup estimación puntual e IC 95 %
#' @param poblacion texto: "En adultos con ERC G3-G4 y DM2"
#' @param desenlace texto: "de evento renal a 36 meses"
#' @param expuesto,control texto de cada grupo (exposición categórica)
#' @param unidad si la exposición es numérica: "10 mL/min/1.73 m2 adicionales de TFGe basal"
interpretar_asociacion <- function(medida, estimado, ic_inf, ic_sup, poblacion, desenlace,
                                   expuesto = NULL, control = NULL, unidad = NULL) {
  sustantivo <- c(RP = "la prevalencia", RR = "el riesgo", RTI = "la tasa de incidencia",
                  OR = "los odds", HR = "el hazard (riesgo instantáneo)")
  stopifnot(medida %in% names(sustantivo))
  verbo <- if (medida == "OR") "fueron" else "fue"
  magnitud <- if (estimado >= 2) sprintf("%.1f veces mayor", estimado)
              else sprintf("%.0f %% %s", 100 * abs(estimado - 1), if (estimado >= 1) "mayor" else "menor")
  signif <- if (ic_inf > 1 || ic_sup < 1) "fue estadísticamente significativo"
            else "no fue estadísticamente significativo (el IC incluye 1)"
  ic <- sprintf("(%s = %.2f; IC 95 %% %.2f a %.2f)", medida, estimado, ic_inf, ic_sup)
  if (is.null(unidad)) {
    sprintf("%s, %s %s en el grupo %s %s %s que en el grupo %s %s. Este resultado %s.",
            poblacion, sustantivo[[medida]], desenlace, expuesto, verbo, magnitud, control, ic, signif)
  } else {
    sprintf("%s, por cada %s, %s %s %s %s %s. Este resultado %s.",
            poblacion, unidad, sustantivo[[medida]], desenlace, verbo, magnitud, ic, signif)
  }
}

# ---- 2. Utilidades de DAG -------------------------------------------------

# Descendientes de un nodo (sin incluirlo)
.descendientes <- function(aristas, nodo) {
  out <- character(); frontera <- nodo
  while (length(frontera) > 0) {
    hijos <- aristas$a[aristas$de %in% frontera]
    hijos <- setdiff(hijos, out)
    out <- c(out, hijos); frontera <- hijos
  }
  out
}

# Todos los caminos simples entre dos nodos, ignorando la dirección de las flechas
.todos_los_caminos <- function(aristas, desde, hasta) {
  vecinos <- function(n) unique(c(aristas$a[aristas$de == n], aristas$de[aristas$a == n]))
  caminos <- list()
  explorar <- function(camino) {
    ult <- camino[length(camino)]
    if (ult == hasta) { caminos[[length(caminos) + 1]] <<- camino; return(invisible()) }
    for (v in setdiff(vecinos(ult), camino)) explorar(c(camino, v))
  }
  explorar(desde)
  caminos
}

# ¿Está abierto un camino dado un conjunto de ajuste Z? (regla de d-separación)
.camino_abierto <- function(aristas, camino, ajuste) {
  if (length(camino) < 3) return(TRUE)
  for (i in 2:(length(camino) - 1)) {
    m <- camino[i]; a <- camino[i - 1]; b <- camino[i + 1]
    colision <- any(aristas$de == a & aristas$a == m) && any(aristas$de == b & aristas$a == m)
    if (colision) {
      # colisionador: cerrado salvo que se ajuste por él o por un descendiente suyo
      if (!(m %in% ajuste || any(.descendientes(aristas, m) %in% ajuste))) return(FALSE)
    } else {
      # mediador o confusor (no colisionador): se cierra al ajustar por él
      if (m %in% ajuste) return(FALSE)
    }
  }
  TRUE
}

#' Lista los caminos entre exposición y desenlace, su tipo y si están abiertos
#' @param aristas tibble con columnas de, a
#' @param ajuste vector de nombres de nodos por los que se ajusta
#' @return tibble: camino, tipo ("causal", "puerta trasera" o "otro"), abierto
caminos_dag <- function(aristas, exposicion = "X", desenlace = "Y", ajuste = character()) {
  cam <- .todos_los_caminos(aristas, exposicion, desenlace)
  tibble::tibble(camino = cam) |>
    dplyr::mutate(
      texto = purrr::map_chr(camino, \(p) {
        flechas <- purrr::map_chr(seq_len(length(p) - 1), \(i)
          if (any(aristas$de == p[i] & aristas$a == p[i + 1])) " -> " else " <- ")
        paste0(p[1], paste0(flechas, p[-1], collapse = ""))
      }),
      tipo = purrr::map_chr(camino, \(p) {
        entra_en_x <- any(aristas$de == p[2] & aristas$a == p[1])
        todas_hacia_adelante <- all(purrr::map_lgl(seq_len(length(p) - 1), \(i)
          any(aristas$de == p[i] & aristas$a == p[i + 1])))
        if (todas_hacia_adelante) "causal" else if (entra_en_x) "puerta trasera" else "otro"
      }),
      abierto = purrr::map_lgl(camino, \(p) .camino_abierto(aristas, p, ajuste))
    ) |>
    dplyr::select(camino = texto, tipo, abierto)
}

#' Conjuntos mínimos de ajuste (criterio de la puerta trasera)
#' @param no_medidos nodos que no se pueden usar (latentes)
#' @return lista de vectores de nombres; lista vacía si no hay ningún conjunto válido
conjuntos_ajuste <- function(aristas, exposicion = "X", desenlace = "Y", no_medidos = character()) {
  nodos <- unique(c(aristas$de, aristas$a))
  candidatos <- setdiff(nodos, c(exposicion, desenlace, no_medidos,
                                 .descendientes(aristas, exposicion)))
  cam <- .todos_los_caminos(aristas, exposicion, desenlace)
  traseros <- Filter(\(p) any(aristas$de == p[2] & aristas$a == p[1]), cam)  # flecha que entra a X
  cumple <- function(z) all(!purrr::map_lgl(traseros, \(p) .camino_abierto(aristas, p, z)))
  encontrados <- list()
  for (k in 0:length(candidatos)) {
    combos <- if (k == 0) list(character()) else utils::combn(candidatos, k, simplify = FALSE)
    for (z in combos) {
      if (any(purrr::map_lgl(encontrados, \(m) all(m %in% z)))) next   # ya contiene un conjunto mínimo
      if (cumple(z)) encontrados[[length(encontrados) + 1]] <- z
    }
  }
  encontrados
}

# ---- 3. Formato de porcentajes con espacio ("12.3 %") ---------------------
#' @param x proporción (0-1)   @param digitos decimales
pct <- function(x, digitos = 1) paste0(formatC(100 * x, format = "f", digits = digitos), " %")

#' Número con decimales fijos ("0.50", "6.60") para el texto en línea
num <- function(x, digitos = 2) formatC(x, format = "f", digits = digitos)
