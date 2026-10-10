# ------------------------------------------------------------------------
# simular_metaanalisis(): 20 ensayos FICTICIOS de un iSGLT2 frente a placebo en ERC
# Datos 100 % simulados (los nombres de los ensayos son inventados). Verdad conocida:
#   - RR/HR verdadero del evento renal compuesto (efecto medio de los ensayos): 0.75
#   - heterogeneidad real entre ensayos: tau = 0.25 en log(RR)
#   - el efecto es MAYOR (más protector) cuanto mayor es la albuminuria media del ensayo
#   - efecto de "estudios pequeños": los ensayos pequeños con resultado poco favorable
#     tienen más probabilidad de NO publicarse (sesgo de publicación simulado)
#   - desenlace continuo: diferencia de medias de la pendiente de TFGe (mL/min/1.73 m2/año),
#     efecto medio verdadero 0.90 (a favor del fármaco)
# Uso:  source("R/simular_metaanalisis.R");  ensayos <- simular_metaanalisis()
#       attr(ensayos, "verdad")   # parámetros verdaderos
# ------------------------------------------------------------------------
simular_metaanalisis <- function(k = 20, semilla = 2121) {
  set.seed(semilla)
  nombres <- c("ALBA", "BRIO", "CEDRO", "DELTA", "EOLO", "FARO", "GAIA", "HELIO", "ITACA", "JUNO",
               "KAIROS", "LIRA", "MIRA", "NOVA", "ONDA", "PAMPA", "QUASAR", "RIO", "SOL", "TERRA")
  rob_niv <- c("Bajo", "Algunas dudas", "Alto")
  repeat {
    d <- tibble::tibble(
      id = seq_len(k * 2),
      estudio = NA_character_,
      anio = sample(2008:2023, k * 2, replace = TRUE),
      n_trat = round(exp(rnorm(k * 2, log(450), 0.9))),
      uacr_media = round(exp(rnorm(k * 2, log(250), 0.6))),
      tfg_media = round(rnorm(k * 2, 45, 7), 1),
      pct_dm = round(runif(k * 2, 30, 100)),
      seguimiento_meses = round(runif(k * 2, 12, 48))
    ) |>
      dplyr::filter(n_trat >= 40) |>
      dplyr::mutate(n_ctrl = round(n_trat * runif(dplyr::n(), 0.9, 1.1)))
    # efecto verdadero de cada ensayo: media 0.75 en RR, más protector con más albuminuria
    d <- d |>
      dplyr::mutate(
        mu_i = log(0.75) - 0.25 * (log(uacr_media) - log(250)),
        theta_i = rnorm(dplyr::n(), mu_i, 0.25),
        riesgo_ctrl = plogis(qlogis(0.12) + 0.0135 * (seguimiento_meses - 30) + rnorm(dplyr::n(), 0, 0.3)),
        ev_ctrl = rbinom(dplyr::n(), n_ctrl, riesgo_ctrl),
        ev_trat = rbinom(dplyr::n(), n_trat, pmin(0.95, riesgo_ctrl * exp(theta_i))),
        # estimación del ensayo: log RR con su error estándar
        log_rr = log(((ev_trat + 0.5) / (n_trat + 0.5)) / ((ev_ctrl + 0.5) / (n_ctrl + 0.5))),
        se_log_rr = sqrt(1 / (ev_trat + 0.5) - 1 / (n_trat + 0.5) + 1 / (ev_ctrl + 0.5) - 1 / (n_ctrl + 0.5)),
        # desenlace continuo
        md_i = rnorm(dplyr::n(), 0.90 + 0.15 * (log(uacr_media) - log(250)), 0.25),
        sd_pendiente = 3.2,
        md_tfg = rnorm(dplyr::n(), md_i, sd_pendiente * sqrt(1 / n_trat + 1 / n_ctrl)),
        se_md_tfg = sd_pendiente * sqrt(1 / n_trat + 1 / n_ctrl),
        # publicación: los ensayos pequeños y poco favorables se publican menos
        z = log_rr / se_log_rr,
        p_pub = plogis(0.2 + 3 * (se_log_rr < 0.12) - 1.5 * z * (se_log_rr >= 0.12)),
        publicado = runif(dplyr::n()) < p_pub
      )
    pub <- d |> dplyr::filter(publicado)
    if (nrow(pub) >= k) break
    k <- k + 4
  }
  pub <- pub[seq_len(k), ] |> dplyr::arrange(anio)
  pub$estudio <- paste0(nombres[seq_len(nrow(pub))], " (", pub$anio, ")")
  # riesgo de sesgo (RoB 2) por dominio: más "alto" en ensayos pequeños
  prob_alto <- plogis(-2.6 - 0.0025 * (pub$n_trat - 450))
  dom <- function() factor(ifelse(runif(nrow(pub)) < prob_alto, "Alto",
                                   ifelse(runif(nrow(pub)) < 0.15, "Algunas dudas", "Bajo")), levels = rob_niv)
  pub$rob_aleatorizacion <- dom(); pub$rob_desviaciones <- dom(); pub$rob_datos_faltantes <- dom()
  pub$rob_medicion <- dom(); pub$rob_seleccion <- dom()
  pub$rob_global <- factor(apply(pub[, grep("^rob_", names(pub))], 1, function(r) {
    r <- as.character(r); if ("Alto" %in% r) "Alto" else if ("Algunas dudas" %in% r) "Algunas dudas" else "Bajo"
  }), levels = rob_niv)
  out <- pub |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::select(id, estudio, anio, n_trat, n_ctrl, ev_trat, ev_ctrl, log_rr, se_log_rr, md_tfg, se_md_tfg,
                  seguimiento_meses, uacr_media, tfg_media, pct_dm, dplyr::starts_with("rob_"), theta_verdadero = theta_i)
  attr(out, "verdad") <- list(rr_medio = 0.75, tau = 0.25, md_medio = 0.90, pendiente_log_uacr = -0.25,
                              nota = "theta_verdadero es el log(RR) verdadero de cada ensayo (oráculo)")
  out
}
