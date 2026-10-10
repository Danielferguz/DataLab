# ------------------------------------------------------------------------
# Funciones del Capítulo 22 (certeza de la evidencia, GRADE)
#   source("R/funciones_cap22.R")
# Requiere: tidyverse (dplyr, tibble) y metafor. Pensadas para un objeto `rma`
# ajustado con yi = log(RR) (o diferencia de medias) y vi/sei.
#
# IMPORTANTE: ninguna de estas funciones CALIFICA la certeza. GRADE es un juicio
# que toma una persona (o un panel) con razones explícitas. Aquí solo hay:
#   - marcas_grade():   reúne las SEÑALES DE ALERTA de cada dominio;
#   - ois_rr():         tamaño óptimo de información para un RR agrupado;
#   - sof_binario(), sof_continuo(): efectos relativo y absoluto para la tabla SoF;
#   - certeza_final():  CONTABILIDAD de los niveles que tú decidiste subir o bajar;
#   - marca_efecto_grande(): ¿el RR cae en la zona que GRADE llama «efecto grande»?
# Todos los datos del libro son FICTICIOS.
# ------------------------------------------------------------------------

niveles_certeza <- c("Muy baja", "Baja", "Moderada", "Alta")

# ---- Contabilidad de niveles ----------------------------------------------

#' Aplica los niveles que TÚ decidiste subir o bajar (no decide nada)
#' @param inicio "Alta" (ensayos; ROBINS-I) o "Baja" (observacionales, enfoque clásico)
#' @param bajar  número total de niveles que se bajan (suma de los dominios que bajan)
#' @param subir  número total de niveles que se suben
#' @return el nivel final, limitado a la escala Muy baja - Alta
certeza_final <- function(inicio = "Alta", bajar = 0, subir = 0) {
  stopifnot(inicio %in% niveles_certeza, bajar >= 0, subir >= 0)
  pos <- match(inicio, niveles_certeza) - bajar + subir
  niveles_certeza[min(4, max(1, pos))]
}

#' Símbolos habituales de las tablas SoF: ⊕⊕◯◯ = Baja
simbolo_certeza <- function(nivel) {
  n <- match(nivel, niveles_certeza)
  vapply(n, \(i) paste0(strrep("⊕", i), strrep("◯", 4 - i)), character(1))
}

# ---- Riesgo de sesgo ------------------------------------------------------

#' Proporción del peso del metaanálisis que aporta cada categoría de riesgo de sesgo
#' @param res objeto `rma`   @param categoria vector (factor) con una categoría por estudio
peso_por_categoria <- function(res, categoria) {
  stopifnot(length(categoria) == res$k)
  tibble::tibble(categoria = categoria, peso = as.numeric(stats::weights(res)) / 100) |>
    dplyr::group_by(categoria, .drop = FALSE) |>
    dplyr::summarise(k = dplyr::n(), peso = sum(peso), .groups = "drop")
}

# ---- Imprecisión: tamaño óptimo de información (OIS) ------------------------

#' Tamaño óptimo de información para un RR agrupado
#' Fórmula de dos proporciones (aproximación normal, la misma de power.prop.test),
#' multiplicada por 1 / (1 - I2) para tener en cuenta la heterogeneidad.
#' @param p_ctrl riesgo esperado en el control   @param rr efecto relativo que se quiere detectar
#' @param i2 heterogeneidad (0 a 1) usada como aproximación de la «diversidad» D2
ois_rr <- function(p_ctrl, rr, alfa = 0.05, potencia = 0.80, i2 = 0) {
  stopifnot(p_ctrl > 0, p_ctrl < 1, rr > 0, i2 >= 0, i2 < 1)
  p1 <- p_ctrl; p2 <- p_ctrl * rr; pm <- (p1 + p2) / 2
  n_grupo <- ((stats::qnorm(1 - alfa / 2) * sqrt(2 * pm * (1 - pm)) +
               stats::qnorm(potencia) * sqrt(p1 * (1 - p1) + p2 * (1 - p2)))^2) / (p1 - p2)^2
  n_grupo <- ceiling(n_grupo)
  tibble::tibble(rr = rr, n_por_grupo = n_grupo, n_total = 2 * n_grupo,
                 factor_heterogeneidad = 1 / (1 - i2),
                 n_total_ajustado = ceiling(2 * n_grupo / (1 - i2)))
}

# ---- Las marcas de los cinco dominios -----------------------------------------

#' Reúne las SEÑALES DE ALERTA de los cinco dominios que bajan la certeza
#' No califica: devuelve cada marca con su valor, una referencia orientativa (heurística,
#' NO una regla de GRADE) y si la marca «salta» (TRUE), no salta (FALSE) o no se puede
#' calcular con datos (NA). La calificación la decide una persona.
#' @param res objeto `rma` (efectos aleatorios)   @param datos tabla con una fila por estudio
#' @param rob nombre de la columna con el riesgo de sesgo global (Bajo / Algunas dudas / Alto)
#' @param tipo "razon" (yi = log RR) o "diferencia" (yi = diferencia de medias)
#' @param umbral valor del efecto a partir del cual la decisión clínica no cambia
#'   (RR 0.90, o la mínima diferencia importante)
#' @param ois resultado de ois_rr()   @param n_total participantes totales del metaanálisis
marcas_grade <- function(res, datos, rob = "rob_global", tipo = c("razon", "diferencia"),
                         umbral = 0.90, ois = NULL, n_total = NULL,
                         umbral_peso = 0.25, umbral_i2 = 50, alfa_egger = 0.10) {
  tipo <- match.arg(tipo)
  stopifnot(nrow(datos) == res$k)
  nulo <- if (tipo == "razon") 1 else 0
  tr <- if (tipo == "razon") exp else identity
  fmt <- \(x, d = 2) formatC(x, format = "f", digits = d)
  dentro <- \(v, lo, hi) (v >= min(lo, hi)) & (v <= max(lo, hi))
  ic <- \(m) c(tr(as.numeric(m$b)), tr(m$ci.lb), tr(m$ci.ub))

  est <- ic(res)
  fila <- \(dominio, marca, valor, referencia, senal)
    tibble::tibble(dominio = dominio, marca = marca, valor = valor, referencia = referencia, senal = senal)

  # 1. Riesgo de sesgo
  r <- as.character(datos[[rob]]); w <- as.numeric(stats::weights(res)) / 100
  p_alto <- sum(w[r == "Alto"]); p_nobajo <- sum(w[r != "Bajo"])
  sin <- r != "Alto"
  if (sum(sin) >= 3 && sum(sin) < res$k) {
    r_sin <- metafor::rma(yi = res$yi[sin], vi = res$vi[sin], method = res$method)
    e_sin <- ic(r_sin)
    cambia <- dentro(umbral, est[2], est[3]) != dentro(umbral, e_sin[2], e_sin[3])
    txt_sin <- sprintf("%s (%s a %s)", fmt(e_sin[1]), fmt(e_sin[2]), fmt(e_sin[3]))
  } else { cambia <- NA; txt_sin <- "no calculable" }
  m_rob <- fila("Riesgo de sesgo", "Peso en estudios de riesgo alto", sprintf("%.0f %%", 100 * p_alto),
                sprintf("señal si ≥ %.0f %%", 100 * umbral_peso), p_alto >= umbral_peso) |>
    dplyr::bind_rows(
      fila("Riesgo de sesgo", "Peso en estudios sin riesgo bajo", sprintf("%.0f %%", 100 * p_nobajo),
           "informativa", NA),
      fila("Riesgo de sesgo", "Estimación sin los de riesgo alto", txt_sin,
           "señal si cambia la posición del IC respecto al umbral", cambia))

  # 2. Inconsistencia
  pr <- stats::predict(res)
  pi_ <- c(tr(pr$pi.lb), tr(pr$pi.ub))
  opuestos <- sum(sign(res$yi) != sign(as.numeric(res$b)) & res$yi != 0)
  m_inc <- dplyr::bind_rows(
    fila("Inconsistencia", "I²", sprintf("%.0f %%", res$I2), sprintf("señal si ≥ %d %%", umbral_i2), res$I2 >= umbral_i2),
    fila("Inconsistencia", "Prueba Q (p)", fmt_p(res$QEp), "señal si p < 0.10", res$QEp < 0.10),
    fila("Inconsistencia", "Intervalo de predicción", sprintf("%s a %s", fmt(pi_[1]), fmt(pi_[2])),
         "señal si incluye el valor nulo", dentro(nulo, pi_[1], pi_[2])),
    fila("Inconsistencia", "Estudios en sentido opuesto al agrupado", as.character(opuestos), "informativa", NA))

  # 3. Evidencia indirecta: no sale de los números
  m_ind <- fila("Evidencia indirecta", "Población, intervención, comparador y desenlace vs. tu pregunta",
                "revisar a mano", "ninguna: compara el PICO de los estudios con el tuyo", NA)

  # 4. Imprecisión
  m_imp <- dplyr::bind_rows(
    fila("Imprecisión", "IC 95 % del efecto agrupado", sprintf("%s a %s", fmt(est[2]), fmt(est[3])), "informativa", NA),
    fila("Imprecisión", "El IC incluye el valor nulo", ifelse(dentro(nulo, est[2], est[3]), "sí", "no"),
         "señal si incluye el nulo", dentro(nulo, est[2], est[3])),
    fila("Imprecisión", sprintf("El IC incluye el umbral de decisión (%s)", fmt(umbral)),
         ifelse(dentro(umbral, est[2], est[3]), "sí", "no"), "señal si el umbral cae dentro del IC",
         dentro(umbral, est[2], est[3])))
  if (!is.null(ois) && !is.null(n_total)) {
    m_imp <- dplyr::bind_rows(m_imp, fila("Imprecisión", "Participantes vs. OIS ajustado",
      sprintf("%s de %s", format(n_total, big.mark = " "), format(ois$n_total_ajustado, big.mark = " ")),
      "señal si no se alcanza el OIS", n_total < ois$n_total_ajustado))
  }

  # 5. Sesgo de publicación
  if (res$k >= 10) {
    eg <- metafor::regtest(res, model = "lm")
    tf <- metafor::trimfill(res); e_tf <- ic(tf)
    m_pub <- dplyr::bind_rows(
      fila("Sesgo de publicación", "Prueba de Egger (p)", fmt_p(eg$pval),
           sprintf("señal si p < %.2f", alfa_egger), eg$pval < alfa_egger),
      fila("Sesgo de publicación", "Estudios «faltantes» (trim-and-fill)", as.character(tf$k0), "informativa", NA),
      fila("Sesgo de publicación", "Estimación tras trim-and-fill", sprintf("%s (%s a %s)", fmt(e_tf[1]), fmt(e_tf[2]), fmt(e_tf[3])),
           "señal si el IC corregido incluye el umbral", dentro(umbral, e_tf[2], e_tf[3])))
  } else {
    m_pub <- fila("Sesgo de publicación", "Egger y embudo", "k < 10: no valen", "con pocos estudios no se evalúan", NA)
  }
  dplyr::bind_rows(m_rob, m_inc, m_ind, m_imp, m_pub)
}

#' Formatea un valor p (p < 0.0001 se escribe «< 0.0001»)
fmt_p <- function(p) ifelse(p < 1e-4, "< 0.0001", formatC(p, format = "f", digits = ifelse(p < 0.01, 4, 3)))

#' Convierte TRUE/FALSE/NA de la columna `senal` en texto para una tabla
senal_txt <- function(x) ifelse(is.na(x), "—", ifelse(x, "SÍ", "no"))

# ---- Tabla de resumen de hallazgos (SoF) --------------------------------------

#' Efectos relativo y absoluto de un desenlace binario, por 1000 pacientes
#' El riesgo basal se toma como fijo (su incertidumbre no entra en el IC).
#' @param res objeto `rma` con yi = log RR   @param p_ctrl riesgo basal (proporción)
sof_binario <- function(res, p_ctrl, por = 1000) {
  v <- exp(c(as.numeric(res$b), res$ci.lb, res$ci.ub))     # estimación, límite inferior, límite superior
  tibble::tibble(
    rr = v[1], rr_inf = v[2], rr_sup = v[3], riesgo_ctrl = por * p_ctrl,
    riesgo_trat = por * p_ctrl * v[1], trat_inf = por * p_ctrl * v[2], trat_sup = por * p_ctrl * v[3],
    evitados = por * p_ctrl * (1 - v[1]),
    evitados_inf = por * p_ctrl * (1 - v[3]),      # el RR más alto da el menor beneficio
    evitados_sup = por * p_ctrl * (1 - v[2]))
}

#' Efecto de un desenlace continuo (diferencia de medias) con su IC
sof_continuo <- function(res) {
  tibble::tibble(md = as.numeric(res$b), md_inf = res$ci.lb, md_sup = res$ci.ub)
}

#' Formatea «estimación (inferior a superior)»
f_ic <- function(est, inf, sup, digitos = 2) {
  f <- \(x) formatC(x, format = "f", digits = digitos)
  paste0(f(est), " (", f(inf), " a ", f(sup), ")")
}

# ---- Subir la certeza: ¿efecto grande? ---------------------------------------

#' Marca si un RR cae en la zona de «efecto grande» (RR < 0.5 o > 2) o «muy grande» (< 0.2 o > 5)
#' Es solo una marca: GRADE pide además que no haya sesgos plausibles que lo expliquen.
#' @param ic_inf,ic_sup límites del IC 95 %
marca_efecto_grande <- function(rr, ic_inf, ic_sup) {
  f <- \(x) ifelse(x < 1, 1 / x, x)               # simetriza: 0.5 y 2 pesan lo mismo
  cercano <- ifelse(ic_inf > 1 | ic_sup < 1, ifelse(rr < 1, ic_sup, ic_inf), 1)
  tibble::tibble(rr = rr, ic_inf = ic_inf, ic_sup = ic_sup,
    magnitud = dplyr::case_when(f(rr) > 5 ~ "muy grande", f(rr) > 2 ~ "grande", TRUE ~ "no grande"),
    ic_sigue_grande = f(cercano) > 2)
}
