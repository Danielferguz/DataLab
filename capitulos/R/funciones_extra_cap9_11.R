# ------------------------------------------------------------------------
# Funciones extra para los capítulos 9-11 (supervivencia, sesgos, ECA)
# Uso en un capítulo:   source("R/funciones_extra_cap9_11.R")
# Solo dependen de base R + dplyr.
# ------------------------------------------------------------------------

#' Win ratio jerárquico (Pocock 2012) para un ensayo de dos brazos
#'
#' Compara TODOS los pares (un tratado, un control) siguiendo una jerarquía:
#'   nivel 1: muerte        -> pierde quien muere antes (si el otro sigue observado)
#'   nivel 2: falla renal   -> pierde quien la tiene antes (si el otro sigue observado)
#'   nivel 3: cambio de TFGe -> gana el mayor cambio (mejor función), si ambos tienen dato
#' Los pares que ningún nivel decide son empates.
#'
#' @param datos data.frame con: brazo ("Placebo"/"Bicarbonato"), tiempo_meses, estado
#'   (0 censura, 1 falla renal, 2 muerte) y dif24 (cambio de TFGe; puede ser NA)
#' @return lista: por_nivel (victorias de cada brazo en cada nivel), pares, win_ratio
win_ratio <- function(datos) {
  tr <- datos[datos$brazo == "Bicarbonato", ]      # tratados
  ct <- datos[datos$brazo == "Placebo", ]          # controles
  nt <- nrow(tr); nc <- nrow(ct)

  # Matrices de pares: filas = pacientes tratados, columnas = pacientes control
  M <- function(x_t, x_c) list(t = matrix(x_t, nt, nc), c = matrix(rep(x_c, each = nt), nt, nc))
  tiempo <- M(tr$tiempo_meses, ct$tiempo_meses)

  # Nivel con eventos de tiempo: pierde quien tiene el evento y el otro sigue en seguimiento al menos hasta entonces
  ganador_evento <- function(tipo) {
    ev <- M(tr$estado == tipo, ct$estado == tipo)
    list(gana_t = ev$c & (tiempo$c <= tiempo$t) & !(ev$t & (tiempo$t < tiempo$c)),
         gana_c = ev$t & (tiempo$t <= tiempo$c) & !(ev$c & (tiempo$c < tiempo$t)))
  }
  n1 <- ganador_evento(2)                                    # nivel 1: muerte
  n2 <- ganador_evento(1)                                    # nivel 2: falla renal
  g1_t <- n1$gana_t & !n1$gana_c;  g1_c <- n1$gana_c & !n1$gana_t
  libre <- !(g1_t | g1_c)                                    # pares aún sin decidir
  g2_t <- n2$gana_t & !n2$gana_c & libre;  g2_c <- n2$gana_c & !n2$gana_t & libre
  libre <- libre & !(g2_t | g2_c)

  # Nivel 3: cambio de TFGe a 24 meses (el mayor cambio gana), en pares aún empatados y con dato en ambos
  d <- M(tr$dif24, ct$dif24)
  valido <- libre & !is.na(d$t) & !is.na(d$c)
  g3_t <- valido & (d$t > d$c);  g3_c <- valido & (d$c > d$t)

  gt <- c(muerte = sum(g1_t), falla_renal = sum(g2_t), tfge = sum(g3_t))
  gc <- c(muerte = sum(g1_c), falla_renal = sum(g2_c), tfge = sum(g3_c))
  list(por_nivel = rbind(gana_tratado = gt, gana_control = gc), pares = nt * nc,
       win_ratio = sum(gt) / sum(gc))
}
