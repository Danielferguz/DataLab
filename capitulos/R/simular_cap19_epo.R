# ------------------------------------------------------------------------
# HEMODIÁLISIS Y EPO SIMULADAS: tratamiento y confusor que cambian en el tiempo (Cap. 19)
# ------------------------------------------------------------------------
# Todos los datos son SIMULADOS. Sirven para los subcapítulos 19.1-19.3 (confusión
# dependiente del tiempo, modelos estructurales marginales y fórmula g).
#
# Diseño: pacientes en hemodiálisis seguidos K = 6 visitas mensuales (mes 1..K). Todos
# empiezan con dosis estándar de EPO (agente estimulante de la eritropoyesis, AEE).
# En cada visita t:
#   1. se mide la hemoglobina hb_t (g/dL);
#   2. el nefrólogo decide la dosis del mes: dosis_alta_t = 1 (dosis alta) o 0 (estándar);
#   3. esa dosis modifica la hemoglobina del mes siguiente.
# Al final del mes K se mide la calidad de vida (0-100, más alto = mejor).
#
# VERDAD CONOCIDA (todo lineal, para que las verdades se puedan derivar a mano)
#  Covariables basales: edad, dm (diabetes), tiempo_hd (años en hemodiálisis)
#  u (estado inflamatorio latente, N(0,1)): NO MEDIDO, baja la hemoglobina y la calidad de vida.
#  Hemoglobina:   mu = 10.8 - 0.45*u - 0.010*(edad-62) - 0.30*dm - 0.05*(tiempo_hd-3.3)
#                 hb_1 = mu + e_1,                 e_1 ~ N(0, 0.8^2)
#                 hb_t = mu + 0.5*(hb_{t-1} - mu) + 0.8*dosis_alta_{t-1} + e_t,  e_t ~ N(0, 0.5^2)
#                 => una dosis alta sube la Hb 0.8 g/dL al mes siguiente y su efecto decae a la mitad cada mes
#  Decisión (mundo observado):
#                 logit P(dosis_alta_t = 1) = -0.3 + 1.2*(10.5 - hb_t) + 1.5*dosis_alta_{t-1}
#                                             - 0.02*(edad-62) + 0.3*dm + efecto_u_en_a*u
#                 (con efecto_u_en_a = 0 el nefrólogo decide SOLO con lo medido: intercambiabilidad
#                 secuencial; con efecto_u_en_a > 0 mira además la inflamación, que no queda registrada)
#  Calidad de vida a los K meses:
#                 y = 55 + 5.0*(mean(hb_1..hb_K) - 10.5) - 0.4*sum(dosis_alta_t)
#                       - 3.0*u - 0.8*(edad-62)/10 - 2.0*dm + N(0, 7^2)
#                 => cada mes de dosis alta tiene un EFECTO DIRECTO de -0.4 (efectos adversos)
#                    y un efecto INDIRECTO positivo a través de la Hb.
#
# Estrategias (argumento `estrategia`): NULL = lo que ocurrió; "siempre" = dosis alta todos los meses;
# "nunca" = dosis estándar todos los meses; "dinamica" = dosis alta solo si hb_t < `umbral`;
# o un vector 0/1 de largo K (estrategia estática a medida, p. ej. c(1,1,1,0,0,0)).
# Con la misma semilla, el ruido y las covariables basales son los mismos bajo cualquier estrategia
# (números aleatorios comunes): la diferencia entre estrategias es el efecto causal.
#
# Devuelve un tibble en formato PERSONA-TIEMPO (largo): una fila por paciente y mes.
# `calidad_vida` solo se registra en la fila del mes K (NA en las demás).
# Con incluir_u = TRUE agrega la columna u_infl (el oráculo del estado inflamatorio).
# ------------------------------------------------------------------------

simular_hd_epo <- function(n = 5000, K = 6, semilla = 2024, estrategia = NULL, umbral = 10,
                           efecto_u_en_a = 0, incluir_u = FALSE) {
  set.seed(semilla)
  # ---- Todo el azar se genera primero: así las estrategias comparten el mismo ruido ----
  edad      <- pmin(pmax(round(rnorm(n, 62, 13)), 25), 90)
  dm        <- rbinom(n, 1, 0.40)
  tiempo_hd <- round(rgamma(n, shape = 2, rate = 0.6), 1)
  u         <- rnorm(n)
  e_hb      <- matrix(rnorm(n * K, 0, 0.5), n, K)
  e_hb[, 1] <- rnorm(n, 0, 0.8)
  u_a       <- matrix(runif(n * K), n, K)
  e_y       <- rnorm(n, 0, 6)

  mu <- 10.8 - 0.45 * u - 0.010 * (edad - 62) - 0.30 * dm - 0.05 * (tiempo_hd - 3.3)

  # ---- Evolución mes a mes ----
  hb <- a <- matrix(NA_real_, n, K)
  hb[, 1] <- mu + e_hb[, 1]
  for (t in 1:K) {
    if (t > 1) hb[, t] <- mu + 0.4 * (hb[, t - 1] - mu) + 1.0 * a[, t - 1] + e_hb[, t]
    a_prev <- if (t == 1) rep(0, n) else a[, t - 1]
    if (is.null(estrategia)) {
      lp <- -0.5 + 0.6 * (10.5 - hb[, t]) + 0.8 * a_prev - 0.02 * (edad - 62) + 0.3 * dm + efecto_u_en_a * u
      a[, t] <- as.numeric(u_a[, t] < plogis(lp))
    } else if (is.numeric(estrategia)) {
      a[, t] <- estrategia[t]
    } else if (estrategia == "siempre") {
      a[, t] <- 1
    } else if (estrategia == "nunca") {
      a[, t] <- 0
    } else if (estrategia == "dinamica") {
      a[, t] <- as.numeric(hb[, t] < umbral)
    } else stop("estrategia no reconocida")
  }

  y <- 55 + 6.0 * (rowMeans(hb) - 10.5) - 0.4 * rowSums(a) - 3.0 * u -
    0.8 * (edad - 62) / 10 - 2.0 * dm + e_y

  # ---- Formato largo (persona-tiempo) ----
  largo <- tibble::tibble(
    id = rep(seq_len(n), each = K), mes = rep(seq_len(K), times = n),
    hb = as.vector(t(hb)), dosis_alta = as.vector(t(a)),
    edad = rep(edad, each = K), dm = rep(dm, each = K), tiempo_hd = rep(tiempo_hd, each = K),
    calidad_vida = as.vector(t(cbind(matrix(NA_real_, n, K - 1), y)))
  )
  if (incluir_u) largo$u_infl <- rep(u, each = K)
  largo
}

# Verdad: calidad de vida media si TODOS siguieran cada estrategia (población grande, mismas semillas)
# Devuelve un tibble con una fila por estrategia: media de calidad de vida y diferencia frente a "nunca".
verdad_hd_epo <- function(n_mc = 200000, K = 6, semilla = 99, umbral = 10) {
  est <- list(nunca = "nunca", siempre = "siempre", dinamica = "dinamica")
  medias <- vapply(est, function(e) {
    d <- simular_hd_epo(n_mc, K, semilla, estrategia = e, umbral = umbral)
    mean(d$calidad_vida, na.rm = TRUE)
  }, numeric(1))
  tibble::tibble(estrategia = names(medias), media_y = unname(medias), efecto_vs_nunca = unname(medias - medias["nunca"]))
}
