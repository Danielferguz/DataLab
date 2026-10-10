# ------------------------------------------------------------------------
# Funciones del Capítulo 21 (Revisiones sistemáticas y metaanálisis)
#   source("R/funciones_cap21.R")
# Todo es SIMULADO / FICTICIO: ninguna búsqueda real, ningún artículo real.
#
# 1) simular_busqueda(): biblioteca ficticia de registros devueltos por tres bases
#    (con duplicados, variantes de título y verdad conocida)          -> 21.1, 21.2
# 2) normalizar_titulo(), deduplicar_registros(): deduplicación por DOI,
#    título normalizado y título aproximado (adist)                  -> 21.2
# 3) kappa_cohen(), cribar_dos_revisores(): cribado simulado          -> 21.2
# 4) flujo_prisma_simulado(), dibujar_prisma(): diagrama PRISMA 2020  -> 21.1
# 5) grafico_semaforo(), grafico_barras_rob(): riesgo de sesgo        -> 21.3
# 6) dibujar_red(): red de comparaciones                              -> 21.7
# ------------------------------------------------------------------------

# ---- 1. Biblioteca ficticia de registros ----------------------------------

# Devuelve un tibble con UNA FILA POR REGISTRO devuelto por una base (PubMed, Embase,
# CENTRAL). Un mismo trabajo puede aparecer en varias bases con el título ligeramente
# distinto. Columnas "de verdad" (id_unico, tipo, relevante_ta, incluido_ft, motivo_ft,
# estudio) solo existen porque es una simulación: en la vida real no las tienes.
simular_busqueda <- function(semilla = 2121) {
  set.seed(semilla)
  acron <- c("ALBA", "BRIO", "CEDRO", "DELTA", "EOLO", "FARO", "GAIA", "HELIO", "ITACA", "JUNO",
             "KAIROS", "LIRA", "MIRA", "NOVA", "ONDA", "PAMPA", "QUASAR", "RIO", "SOL", "TERRA")
  anios_inc <- c(2008, 2008, 2009, 2010, 2011, 2011, 2012, 2014, 2015, 2015,
                 2016, 2016, 2016, 2016, 2016, 2017, 2020, 2020, 2022, 2023)
  farm  <- c("dapagliflozin", "empagliflozin", "canagliflozin", "ertugliflozin", "sotagliflozin", "SGLT2 inhibitors")
  res   <- c("kidney function decline", "kidney failure", "albuminuria progression", "eGFR slope",
             "a composite kidney outcome", "doubling of serum creatinine")
  pob   <- c("chronic kidney disease", "diabetic kidney disease", "CKD stages 3-4",
             "albuminuric chronic kidney disease", "non-diabetic chronic kidney disease", "advanced chronic kidney disease")
  pais  <- c("Spain", "Mexico", "Argentina", "Japan", "Denmark", "Korea", "Chile", "Canada", "Italy", "Brazil")
  otros <- c("finerenone", "semaglutide", "sodium bicarbonate", "allopurinol", "spironolactone", "febuxostat",
             "patiromer", "roxadustat", "colecalciferol", "atorvastatin")
  pick <- \(v, n) sample(v, n, replace = TRUE)

  # --- principales (20 ensayos elegibles) y sus componentes de título
  inc <- tibble::tibble(farm = pick(farm, 20), res = pick(res, 20), pob = pick(pob, 20), estudio = acron)
  titulo_inc <- sprintf("%s versus placebo and %s in adults with %s: the %s randomized, double-blind trial",
                        inc$farm, inc$res, inc$pob, inc$estudio)
  # --- 4 informes secundarios de ensayos incluidos (título casi igual al del principal)
  sec_i <- sample(20, 4)
  titulo_sec <- paste0(titulo_inc[sec_i], ". Post hoc analysis by baseline albuminuria")
  # --- 5 "casi elegibles": parecen elegibles por el título pero fallan en el texto completo
  nm <- tibble::tibble(
    titulo = c(
      sprintf("%s versus glimepiride and %s in adults with %s: a randomized open-label trial", pick(farm, 2), pick(res, 2), pick(pob, 2)),
      sprintf("%s versus placebo and glycaemic control in adults with %s: a randomized trial", pick(farm, 1), pick(pob, 1)),
      sprintf("Short-term effects of %s versus placebo on %s in adults with %s: a 24-week randomized trial", pick(farm, 1), pick(res, 1), pick(pob, 1)),
      sprintf("%s versus placebo and %s in kidney transplant recipients: a randomized trial", pick(farm, 1), pick(res, 1))),
    motivo = c("Comparador activo (sin placebo)", "Comparador activo (sin placebo)", "Sin desenlace renal",
               "Seguimiento < 12 meses", "Población no elegible"))
  # --- no elegibles (con "aspecto" distinto)
  gen <- function(tipo, n, f) tibble::tibble(titulo = f(n), tipo = tipo)
  no_el <- dplyr::bind_rows(
    gen("Cohorte observacional", 110, \(n) sprintf("%s and %s in adults with %s: a retrospective cohort study in %s", pick(farm, n), pick(res, n), pick(pob, n), pick(pais, n))),
    gen("ECA de otro fármaco", 80, \(n) sprintf("%s versus placebo and %s in adults with %s: a randomized controlled trial in %s", pick(otros, n), pick(res, n), pick(pob, n), pick(pais, n))),
    gen("ECA de iSGLT2 sin ERC", 45, \(n) sprintf("%s versus placebo and %s in patients with %s: a randomized trial in %s", pick(farm, n),
                                                  pick(c("hospitalisation for heart failure", "cardiovascular death", "glycaemic control", "body weight"), n),
                                                  pick(c("heart failure", "type 2 diabetes", "acute myocardial infarction", "obesity", "atrial fibrillation"), n), pick(pais, n))),
    gen("Estudio en animales", 40, \(n) sprintf("%s attenuates %s in %s with %s", pick(farm, n),
                                                pick(c("renal fibrosis", "glomerular hyperfiltration", "tubular injury", "podocyte loss"), n),
                                                pick(c("mice", "rats", "zebrafish"), n), pick(c("subtotal nephrectomy", "streptozotocin diabetes", "unilateral obstruction", "high-fat diet"), n))),
    gen("Revisión o editorial", 45, \(n) sprintf("%s and the kidney: %s %s", pick(farm, n),
                                                  pick(c("mechanisms", "a narrative review", "an editorial", "clinical perspectives", "pharmacology"), n),
                                                  pick(c("in diabetes", "in older adults", "for nephrologists", "in 2024", "and open questions"), n))),
    gen("Pediátrico u otro", 12, \(n) sprintf("%s in children with %s: a pilot study in %s", pick(farm, n), pick(pob, n), pick(pais, n))),
    gen("Otro tema de nefrología", 60, \(n) sprintf("%s in patients receiving %s: %s in %s", pick(c("Vascular access patency", "Intradialytic hypotension", "Hyperphosphataemia", "Anaemia management", "Peritonitis rates"), n),
                                                    pick(c("haemodialysis", "peritoneal dialysis", "home dialysis"), n),
                                                    pick(c("a cohort study", "a randomized trial", "a quality improvement project", "a cross-sectional survey"), n), pick(pais, n)))
  )
  motivo_no <- c("Cohorte observacional" = "No es un ECA", "ECA de otro fármaco" = "Intervención no elegible",
                 "ECA de iSGLT2 sin ERC" = "Población no elegible", "Estudio en animales" = "No es un estudio en humanos",
                 "Revisión o editorial" = "No es un ECA", "Pediátrico u otro" = "Población no elegible",
                 "Otro tema de nefrología" = "Intervención no elegible")
  u <- dplyr::bind_rows(
    tibble::tibble(titulo = titulo_inc, tipo = "ECA elegible", anio = anios_inc, estudio = acron, motivo_ft = NA_character_),
    tibble::tibble(titulo = titulo_sec, tipo = "Informe secundario", anio = anios_inc[sec_i] + sample(0:2, 4, replace = TRUE),
                   estudio = acron[sec_i], motivo_ft = NA_character_),
    tibble::tibble(titulo = nm$titulo, tipo = "Casi elegible", anio = sample(2010:2023, 5), estudio = NA_character_, motivo_ft = nm$motivo),
    no_el |> dplyr::mutate(anio = sample(2008:2024, dplyr::n(), replace = TRUE), estudio = NA_character_,
                           motivo_ft = unname(motivo_no[tipo]))
  )
  u$titulo <- paste0(toupper(substr(u$titulo, 1, 1)), substr(u$titulo, 2, nchar(u$titulo)))   # primera letra en mayúscula
  # títulos únicos (por si el azar repite alguno)
  while (anyDuplicated(u$titulo)) { i <- which(duplicated(u$titulo)); u$titulo[i] <- paste0(u$titulo[i], " (", sample(letters, length(i), TRUE), ")") }
  u <- u |>
    dplyr::mutate(id_unico = dplyr::row_number(),
                  relevante_ta = tipo %in% c("ECA elegible", "Informe secundario", "Casi elegible"),
                  incluido_ft = tipo %in% c("ECA elegible", "Informe secundario"),
                  autor = paste(sample(c("Garcia", "Lopez", "Martin", "Rossi", "Silva", "Nakamura", "Jensen", "Kim", "Costa", "Moreno",
                                         "Ferrari", "Hansen", "Ortega", "Santos", "Weber", "Alvarez"), dplyr::n(), TRUE),
                                sample(LETTERS[1:20], dplyr::n(), TRUE)),
                  doi = sprintf("10.9999/ficticio.%04d", id_unico))
  # un informe secundario comparte primer autor con el informe principal (como en la vida real)
  es_sec <- u$tipo == "Informe secundario"
  u$autor[es_sec] <- u$autor[match(u$estudio[es_sec], u$estudio)]

  # --- en qué bases aparece cada trabajo
  es_eca <- u$tipo %in% c("ECA elegible", "Informe secundario", "Casi elegible", "ECA de otro fármaco", "ECA de iSGLT2 sin ERC")
  p_pm <- ifelse(u$relevante_ta, 0.95, 0.85); p_em <- ifelse(u$relevante_ta, 0.85, 0.75)
  p_ce <- ifelse(es_eca, 0.6, 0.08)
  en <- cbind(PubMed = runif(nrow(u)) < p_pm, Embase = runif(nrow(u)) < p_em, CENTRAL = runif(nrow(u)) < p_ce)
  sin <- rowSums(en) == 0; en[sin, "PubMed"] <- TRUE

  perturbar <- function(t, base) {
    if (base == "Embase" && runif(1) < 0.30) t <- sub("randomized", "randomised", t)
    if (base == "Embase" && runif(1) < 0.20) t <- toupper(substr(t, 1, 1)) |> paste0(tolower(substr(t, 2, nchar(t))))
    if (base == "CENTRAL" && runif(1) < 0.30) t <- paste0(t, ".")
    if (base == "CENTRAL" && runif(1) < 0.15) t <- sub(": .*$", "", t)         # subtítulo recortado
    if (base != "PubMed" && runif(1) < 0.10) {                                # una errata de una letra
      k <- sample(10:(nchar(t) - 5), 1); substr(t, k, k) <- sample(letters, 1)
    }
    t
  }
  filas <- purrr::map_dfr(seq_len(nrow(u)), function(i) {
    bs <- colnames(en)[en[i, ]]
    purrr::map_dfr(bs, function(b) {
      tibble::tibble(id_unico = u$id_unico[i], base = b, titulo = perturbar(u$titulo[i], b),
                     autor = if (b == "Embase") paste0(u$autor[i], ".") else u$autor[i],
                     anio = u$anio[i] + ifelse(b == "Embase" && runif(1) < 0.10, 1L, 0L),
                     doi = if (runif(1) < c(PubMed = 0.90, Embase = 0.75, CENTRAL = 0.35)[[b]]) u$doi[i] else NA_character_)
    })
  })
  filas |>
    dplyr::left_join(dplyr::select(u, id_unico, tipo, relevante_ta, incluido_ft, motivo_ft, estudio), by = "id_unico") |>
    dplyr::slice_sample(prop = 1) |>
    dplyr::mutate(id_registro = dplyr::row_number(), .before = 1)
}

# ---- 2. Deduplicación -----------------------------------------------------

#' Título "limpio" para comparar: minúsculas, sin tildes ni signos, ortografía británica -> americana
normalizar_titulo <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("randomised", "randomized", x, fixed = TRUE)
  x <- gsub("[^a-z0-9 ]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Agrupa nodos conectados por pares (matriz de 2 columnas); devuelve el menor índice de cada grupo
.agrupar_pares <- function(n, pares) {
  g <- seq_len(n)
  if (is.null(pares) || nrow(pares) == 0) return(g)
  repeat {
    cambio <- FALSE
    for (k in seq_len(nrow(pares))) {
      a <- g[pares[k, 1]]; b <- g[pares[k, 2]]
      if (a != b) { g[g == max(a, b)] <- min(a, b); cambio <- TRUE }
    }
    if (!cambio) break
  }
  g
}

#' Deduplicación en tres pasos: (1) mismo DOI, (2) mismo título normalizado,
#' (3) título parecido (distancia de edición relativa <= umbral) del mismo primer autor y año igual o contiguo.
#' Añade: clave, grupo_dup (índice del registro que se conserva), duplicado, paso_dup.
deduplicar_registros <- function(reg, umbral = 0.08) {
  n <- nrow(reg)
  reg$clave <- normalizar_titulo(reg$titulo)
  grupo <- seq_len(n); paso <- rep(NA_character_, n)
  # paso 1: DOI
  d1 <- !is.na(reg$doi) & duplicated(reg$doi)
  grupo[d1] <- match(reg$doi[d1], reg$doi); paso[d1] <- "1. Mismo DOI"
  # paso 2: título normalizado (entre los que quedan)
  rest <- which(!d1)
  d2 <- rest[duplicated(reg$clave[rest])]
  grupo[d2] <- match(reg$clave[d2], reg$clave); paso[d2] <- "2. Mismo título normalizado"
  # paso 3: título aproximado (entre los que quedan), dentro de cada primer autor y con año igual o contiguo
  rest <- which(is.na(paso))
  reg$autor_norm <- normalizar_titulo(reg$autor)
  pares <- NULL
  for (a in unique(reg$autor_norm[rest])) {
    idx <- rest[reg$autor_norm[rest] == a]
    if (length(idx) < 2) next
    D <- utils::adist(reg$clave[idx]) / outer(nchar(reg$clave[idx]), nchar(reg$clave[idx]), pmax)
    cerca <- abs(outer(reg$anio[idx], reg$anio[idx], "-")) <= 1
    w <- which(D <= umbral & cerca & upper.tri(D), arr.ind = TRUE)
    if (nrow(w)) pares <- rbind(pares, cbind(idx[w[, 1]], idx[w[, 2]]))
  }
  if (!is.null(pares)) {
    pares <- unique(pares)
    g3 <- .agrupar_pares(n, pares)
    d3 <- rest[g3[rest] != rest]
    grupo[d3] <- g3[d3]; paso[d3] <- "3. Título aproximado (adist)"
  }
  repeat { g2 <- grupo[grupo]; if (identical(g2, grupo)) break; grupo <- g2 }
  reg$grupo_dup <- grupo
  reg$duplicado <- grupo != seq_len(n)
  reg$paso_dup <- paso
  reg
}

# ---- 3. Cribado por dos revisores -----------------------------------------

#' Kappa de Cohen a mano para dos clasificaciones (vectores del mismo largo)
#' Devuelve: acuerdo observado, acuerdo esperado por azar, kappa y error estándar aproximado
kappa_cohen <- function(a, b) {
  niv <- union(unique(a), unique(b))
  tab <- table(factor(a, niv), factor(b, niv))
  n <- sum(tab)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / n^2
  k <- (po - pe) / (1 - pe)
  ee <- sqrt(po * (1 - po) / (n * (1 - pe)^2))      # aproximación habitual
  c(acuerdo = po, azar = pe, kappa = k, ee = ee)
}

#' Dos revisores independientes cribando títulos y resúmenes (simulado).
#' sens_a, sens_b: probabilidad de "Incluir" si el registro es relevante.
#' Los no relevantes que "se parecen" (ECA de otro fármaco o de iSGLT2 sin ERC) se cuelan más.
cribar_dos_revisores <- function(unicos, sens_a = 0.96, sens_b = 0.94, semilla = 8) {
  set.seed(semilla)
  n <- nrow(unicos)
  fp <- dplyr::case_when(unicos$tipo == "ECA de iSGLT2 sin ERC" ~ 0.15, unicos$tipo == "ECA de otro fármaco" ~ 0.06,
                         unicos$tipo == "Cohorte observacional" ~ 0.04, TRUE ~ 0.01)
  p_a <- ifelse(unicos$relevante_ta, sens_a, fp)
  p_b <- ifelse(unicos$relevante_ta, sens_b, fp * 1.2)
  dec <- \(p) ifelse(runif(n) < p, "Incluir", "Excluir")
  out <- unicos
  out$rev_a <- dec(p_a); out$rev_b <- dec(p_b)
  out$conflicto <- out$rev_a != out$rev_b
  # el tercer revisor acierta el 97 % de las veces (acuerdo por consenso)
  acierta <- runif(n) < 0.97
  tercero <- ifelse(xor(unicos$relevante_ta, !acierta), "Incluir", "Excluir")
  out$tercero <- ifelse(out$conflicto, tercero, NA_character_)
  out$decision <- ifelse(out$conflicto, out$tercero, out$rev_a)
  out
}

# ---- 4. Flujo PRISMA 2020 -------------------------------------------------

#' Ejecuta todo el cribado simulado y devuelve los números del diagrama PRISMA.
#' Se asume que los pocos duplicados que la deduplicación automática no detecta se eliminan
#' a mano antes de cribar (por eso n_duplicados = registros - trabajos distintos).
flujo_prisma_simulado <- function(semilla = 2121, semilla_cribado = 8, umbral = 0.08) {
  reg <- simular_busqueda(semilla) |> deduplicar_registros(umbral)
  unicos <- reg |> dplyr::filter(!duplicado) |> dplyr::distinct(id_unico, .keep_all = TRUE)
  crib <- cribar_dos_revisores(unicos, semilla = semilla_cribado)
  ft <- crib |> dplyr::filter(decision == "Incluir")
  excl_ft <- ft |> dplyr::filter(!incluido_ft) |> dplyr::count(motivo_ft, sort = TRUE)
  inc <- ft |> dplyr::filter(incluido_ft)
  list(
    n_bases = table(factor(reg$base, c("PubMed", "Embase", "CENTRAL"))) |> c(),
    n_registros = nrow(reg), n_duplicados = nrow(reg) - nrow(unicos), n_cribados = nrow(unicos),
    n_excl_ta = sum(crib$decision == "Excluir"), n_buscados = nrow(ft), n_no_recuperados = 0L,
    n_evaluados = nrow(ft), motivos = excl_ft, n_excl_ft = sum(excl_ft$n),
    n_informes = nrow(inc), n_estudios = dplyr::n_distinct(inc$estudio),
    relevantes_perdidos = sum(crib$relevante_ta & crib$decision == "Excluir")
  )
}

#' Dibuja un diagrama de flujo PRISMA 2020 con ggplot2 a partir de una lista `n`
#' (la que devuelve flujo_prisma_simulado(), o una hecha a mano con los mismos nombres).
dibujar_prisma <- function(n, tam = 2.9) {
  fmt <- \(x) format(x, big.mark = " ")
  base_txt <- paste(sprintf("%s %s", names(n$n_bases), fmt(n$n_bases)), collapse = " · ")
  motivos <- if (nrow(n$motivos)) paste(sprintf("%s (n = %s)", n$motivos$motivo_ft, n$motivos$n), collapse = "\n") else "-"
  cajas <- tibble::tribble(
    ~id, ~x, ~y, ~w, ~h, ~texto, ~tipo,
    "a", 3.5, 8.6, 5.6, 1.45, paste0("Registros identificados en bases de datos\n(n = ", fmt(n$n_registros), ")\n", base_txt), "main",
    "b", 9.3, 8.6, 4.6, 1.45, paste0("Registros eliminados antes del cribado:\nduplicados (n = ", fmt(n$n_duplicados), ")"), "side",
    "c", 3.5, 6.6, 5.6, 1.0, paste0("Registros cribados (título y resumen)\n(n = ", fmt(n$n_cribados), ")"), "main",
    "d", 9.3, 6.6, 4.6, 1.0, paste0("Registros excluidos\n(n = ", fmt(n$n_excl_ta), ")"), "side",
    "e", 3.5, 4.9, 5.6, 1.0, paste0("Informes buscados para recuperación\n(n = ", fmt(n$n_buscados), ")"), "main",
    "f", 9.3, 4.9, 4.6, 1.0, paste0("Informes no recuperados\n(n = ", fmt(n$n_no_recuperados), ")"), "side",
    "g", 3.5, 3.2, 5.6, 1.0, paste0("Informes evaluados para elegibilidad\n(n = ", fmt(n$n_evaluados), ")"), "main",
    "h", 9.3, 2.9, 4.6, 1.9, paste0("Informes excluidos (n = ", fmt(n$n_excl_ft), "):\n", motivos), "side",
    "i", 3.5, 1.2, 5.6, 1.3, paste0("Estudios incluidos en la revisión\n(n = ", fmt(n$n_estudios), ")\nInformes de los estudios incluidos\n(n = ", fmt(n$n_informes), ")"), "final"
  )
  flechas <- tibble::tribble(
    ~x, ~y, ~xend, ~yend,
    3.5, 7.87, 3.5, 7.1,  3.5, 6.1, 3.5, 5.4,  3.5, 4.4, 3.5, 3.7,  3.5, 2.7, 3.5, 1.85,
    6.3, 8.6, 7.0, 8.6,  6.3, 6.6, 7.0, 6.6,  6.3, 4.9, 7.0, 4.9,  6.3, 3.2, 7.0, 3.0
  )
  fases <- tibble::tribble(~y0, ~y1, ~etq, 7.9, 9.5, "Identificación", 2.55, 7.4, "Cribado", 0.4, 2.0, "Incluidos")
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = fases, ggplot2::aes(xmin = 0, xmax = 0.55, ymin = y0, ymax = y1), fill = "#0072B2") +
    ggplot2::geom_text(data = fases, ggplot2::aes(x = 0.275, y = (y0 + y1) / 2, label = etq), angle = 90, color = "white", size = tam, fontface = "bold") +
    ggplot2::geom_segment(data = flechas, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          arrow = grid::arrow(length = grid::unit(0.16, "cm"), type = "closed"), color = "grey30") +
    ggplot2::geom_rect(data = cajas, ggplot2::aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, fill = tipo),
                       color = "grey30", linewidth = 0.4, show.legend = FALSE) +
    ggplot2::geom_text(data = cajas, ggplot2::aes(x = x, y = y, label = texto), size = tam, lineheight = 0.95) +
    ggplot2::scale_fill_manual(values = c(main = "#E8F1FA", side = "#F4F4F4", final = "#DDEFE7")) +
    ggplot2::coord_cartesian(xlim = c(0, 11.8), ylim = c(0.2, 9.5)) +
    ggplot2::theme_void()
}

# ---- 5. Riesgo de sesgo (RoB 2) -------------------------------------------

.niveles_rob <- c("Bajo", "Algunas dudas", "Alto")
.col_rob <- c("Bajo" = "#009E73", "Algunas dudas" = "#F0E442", "Alto" = "#D55E00")
.sim_rob <- c("Bajo" = "+", "Algunas dudas" = "?", "Alto" = "-")
.dom_rob <- c(rob_aleatorizacion = "D1 Aleatorización", rob_desviaciones = "D2 Desviaciones",
              rob_datos_faltantes = "D3 Datos faltantes", rob_medicion = "D4 Medición", rob_seleccion = "D5 Selección del resultado",
              rob_global = "Global")

rob_largo <- function(d, incluir_global = TRUE) {
  cols <- names(.dom_rob)[names(.dom_rob) %in% names(d)]
  if (!incluir_global) cols <- setdiff(cols, "rob_global")
  d |>
    tidyr::pivot_longer(dplyr::all_of(cols), names_to = "dominio", values_to = "juicio") |>
    dplyr::mutate(dominio = factor(.dom_rob[dominio], levels = .dom_rob[cols]),
                  juicio = factor(as.character(juicio), levels = .niveles_rob))
}

#' Gráfico "semáforo": un círculo por ensayo y dominio (símbolo + color, legible sin color)
grafico_semaforo <- function(d, tam = 6) {
  l <- rob_largo(d)
  l$estudio <- factor(l$estudio, levels = rev(unique(d$estudio)))
  ggplot2::ggplot(l, ggplot2::aes(dominio, estudio)) +
    ggplot2::geom_point(ggplot2::aes(fill = juicio), shape = 21, size = tam, color = "grey25") +
    ggplot2::geom_text(ggplot2::aes(label = .sim_rob[as.character(juicio)]), size = tam * 0.6, fontface = "bold") +
    ggplot2::scale_fill_manual(values = .col_rob, drop = FALSE, name = "Riesgo de sesgo") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 25, hjust = 0),
                   legend.position = "bottom")
}

#' Barras apiladas 100 %: proporción de ensayos (o de pacientes, si ponderar = TRUE) por juicio y dominio
grafico_barras_rob <- function(d, ponderar = FALSE) {
  d$peso <- if (ponderar) d$n_trat + d$n_ctrl else 1
  l <- rob_largo(d) |>
    dplyr::group_by(dominio, juicio) |> dplyr::summarise(p = sum(peso), .groups = "drop_last") |>
    dplyr::mutate(p = p / sum(p)) |> dplyr::ungroup() |>
    tidyr::complete(dominio, juicio, fill = list(p = 0))
  l$dominio <- factor(l$dominio, levels = rev(levels(l$dominio)))
  ggplot2::ggplot(l, ggplot2::aes(p, dominio, fill = juicio)) +
    ggplot2::geom_col(width = 0.7, color = "white", position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = .col_rob, drop = FALSE, name = "Riesgo de sesgo") +
    ggplot2::scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
    ggplot2::labs(x = if (ponderar) "Porcentaje de pacientes (ponderado por tamaño)" else "Porcentaje de ensayos", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", panel.grid.major.y = ggplot2::element_blank())
}

# ---- 6. Red de comparaciones (21.7) ---------------------------------------

#' Dibuja la red: nodos en círculo, grosor de la arista = número de ensayos
#' @param ensayos tibble con columnas trat1, trat2 (una fila por ensayo)
dibujar_red <- function(ensayos, tam_texto = 3.4) {
  trats <- sort(unique(c(ensayos$trat1, ensayos$trat2)))
  k <- length(trats)
  nodos <- tibble::tibble(trat = trats, ang = 2 * pi * (seq_len(k) - 1) / k + pi / 4) |>
    dplyr::mutate(x = cos(ang), y = sin(ang))
  aristas <- ensayos |>
    dplyr::mutate(a = pmin(trat1, trat2), b = pmax(trat1, trat2)) |>
    dplyr::count(a, b, name = "n_ensayos") |>
    dplyr::left_join(dplyr::select(nodos, a = trat, x0 = x, y0 = y), by = "a") |>
    dplyr::left_join(dplyr::select(nodos, b = trat, x1 = x, y1 = y), by = "b")
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = aristas, ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = n_ensayos), color = "grey55", lineend = "round") +
    ggplot2::geom_text(data = aristas, ggplot2::aes(x = (x0 + x1) / 2, y = (y0 + y1) / 2, label = n_ensayos),
                       size = tam_texto, fontface = "bold", color = "white") +
    ggplot2::geom_label(data = nodos, ggplot2::aes(x, y, label = trat), size = tam_texto + 0.4, fill = "#E8F1FA", label.padding = grid::unit(0.4, "lines")) +
    ggplot2::scale_linewidth(range = c(2.5, 8), guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.25)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
    ggplot2::theme_void()
}

#' Metaanálisis en red por contrastes (ensayos de DOS brazos), mínimos cuadrados ponderados
#' @param red tibble con trat1, trat2, yi (efecto de trat1 frente a trat2, p. ej. log HR) y sei
#' @param referencia tratamiento de referencia (los parámetros son efectos frente a él)
#' @param orden orden deseado de los tratamientos (opcional, solo afecta a la presentación)
#' @param tau2 varianza entre ensayos común (0 = efecto común/fijo)
#' @return lista: d (efectos frente a la referencia), V (covarianzas), X (matriz de diseño), Q, gl
#' No maneja ensayos de tres o más brazos (sus contrastes están correlacionados).
nma_contrastes <- function(red, referencia = "Placebo", orden = NULL, tau2 = 0) {
  trats <- setdiff(sort(unique(c(red$trat1, red$trat2))), referencia)
  if (!is.null(orden)) trats <- intersect(orden, trats)
  X <- sapply(trats, \(t) as.numeric(red$trat1 == t) - as.numeric(red$trat2 == t))
  if (!is.matrix(X)) X <- matrix(X, ncol = 1, dimnames = list(NULL, trats))
  w <- 1 / (red$sei^2 + tau2)
  V <- solve(t(X) %*% (w * X))                       # (X' W X)^-1
  b <- drop(V %*% t(X) %*% (w * red$yi))
  names(b) <- trats; dimnames(V) <- list(trats, trats)
  Q <- sum(w * (red$yi - drop(X %*% b))^2)
  list(d = b, V = V, X = X, Q = Q, gl = nrow(red) - length(trats), referencia = referencia)
}

#' Todas las comparaciones por pares (A frente a B) de un ajuste de nma_contrastes()
pares_nma <- function(fit) {
  nom <- c(names(fit$d), fit$referencia)
  d <- c(fit$d, 0); names(d) <- nom
  V <- matrix(0, length(nom), length(nom), dimnames = list(nom, nom)); V[names(fit$d), names(fit$d)] <- fit$V
  utils::combn(nom, 2, simplify = FALSE) |>
    purrr::map_dfr(\(p) tibble::tibble(trat1 = p[1], trat2 = p[2], est = d[[p[1]]] - d[[p[2]]],
                                       se = sqrt(V[p[1], p[1]] + V[p[2], p[2]] - 2 * V[p[1], p[2]])))
}

#' Evidencia directa (ensayos que comparan exactamente ese par) e indirecta (red sin esos ensayos)
#' para cada par. Devuelve est/se de cada una (NA si no existe).
evidencia_directa_indirecta <- function(red, pares, referencia = "Placebo", orden = NULL) {
  purrr::pmap_dfr(list(pares$trat1, pares$trat2), function(a, b) {
    es_par <- (red$trat1 == a & red$trat2 == b) | (red$trat1 == b & red$trat2 == a)
    dir <- red[es_par, ]
    y <- ifelse(dir$trat1 == a, dir$yi, -dir$yi)
    w <- 1 / dir$sei^2
    dir_est <- if (nrow(dir)) sum(w * y) / sum(w) else NA_real_
    dir_se <- if (nrow(dir)) 1 / sqrt(sum(w)) else NA_real_
    resto <- red[!es_par, ]
    ind <- tryCatch({
      f <- nma_contrastes(resto, referencia, orden)
      nom <- c(names(f$d), referencia)
      if (!all(c(a, b) %in% nom)) stop("sin conexión")
      d <- c(f$d, 0); names(d) <- nom
      V <- matrix(0, length(nom), length(nom), dimnames = list(nom, nom)); V[names(f$d), names(f$d)] <- f$V
      c(est = d[[a]] - d[[b]], se = sqrt(V[a, a] + V[b, b] - 2 * V[a, b]))
    }, error = function(e) c(est = NA_real_, se = NA_real_))
    tibble::tibble(trat1 = a, trat2 = b, k_directo = nrow(dir), dir_est = dir_est, dir_se = dir_se,
                   ind_est = unname(ind["est"]), ind_se = unname(ind["se"]))
  })
}
