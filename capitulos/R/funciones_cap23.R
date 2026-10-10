# ------------------------------------------------------------------------
# Funciones del Capítulo 23 (Reporte, reproducibilidad y comunicación)
#   source("R/funciones_cap23.R")
#
# Requiere tidyverse. flextable solo en ft_libro(); jsonlite en renv_lock_ejemplo().
#
#  1. lista_verificacion(), cruzar_manuscrito(), resumen_lista()   (23.1)
#  2. diagrama_flujo(), flujo_consort(), flujo_observacional()       (23.1)
#  3. paleta_libro, theme_libro(), ft_libro(), guardar_fig()         (23.2)
#  4. registrar_sesion(), renv_lock_ejemplo(), analisis_minimo()     (23.3)
#  5. p_desenlaces(), p_subgrupos(), simular_multiplicidad()         (23.5)
#  6. k_anonimato(), celdas_pequenas()                               (23.6)
#
# IMPORTANTE: las listas de verificación de abajo son un RESUMEN NO OFICIAL,
# escrito con palabras propias y con numeración PROPIA (no es la numeración de
# las guías). Para redactar un manuscrito usa siempre la guía original
# (https://www.equator-network.org/).
# ------------------------------------------------------------------------

# ======================================================================
# 1. Lista de verificación en blanco y cruce con un manuscrito
# ======================================================================

#' Lista de verificación en blanco (resumen no oficial)
#' @param guia "CONSORT" (ECA) o "STROBE" (observacionales)
#' @return tibble con id (numeración propia), sección, ítem y columnas vacías para llenar
lista_verificacion <- function(guia = c("CONSORT", "STROBE")) {
  guia <- match.arg(guia)
  items <- switch(guia,
    CONSORT = tibble::tribble(
      ~id,   ~seccion,        ~item,
      "C01", "Título/resumen", "El título dice que es un ensayo aleatorizado y el resumen cuenta diseño, métodos, resultados y conclusiones.",
      "C02", "Introducción",   "Se explica la justificación y se plantean objetivos o hipótesis concretos.",
      "C03", "Diseño",         "Se describe el tipo de ensayo (paralelo, cruzado...) y la razón de asignación.",
      "C04", "Participantes",  "Se dan los criterios de elegibilidad y los lugares donde se reclutó.",
      "C05", "Intervenciones", "Las intervenciones se describen con detalle suficiente para repetirlas.",
      "C06", "Desenlaces",     "El desenlace primario y los secundarios están definidos, con su momento de medición; se cuentan los cambios posteriores al inicio.",
      "C07", "Tamaño",         "Se explica cómo se calculó el tamaño de muestra y si hubo análisis interinos o reglas de detención.",
      "C08", "Aleatorización", "Se dice cómo se generó la secuencia y qué restricciones tuvo (bloques, estratificación).",
      "C09", "Aleatorización", "Se dice quién y cómo ocultó la asignación hasta el momento de asignar.",
      "C10", "Enmascaramiento","Se dice quién desconocía la asignación (pacientes, clínicos, evaluadores) y cómo se logró.",
      "C11", "Estadística",    "Se describen los métodos para el desenlace primario y los secundarios, los subgrupos y los ajustes.",
      "C12", "Resultados",     "Hay un diagrama de flujo con los números por grupo en cada etapa y las razones de pérdidas y exclusiones.",
      "C13", "Resultados",     "Se dan las fechas de reclutamiento y seguimiento y por qué terminó el ensayo.",
      "C14", "Resultados",     "Hay una tabla de características basales por grupo (sin pruebas de significación).",
      "C15", "Resultados",     "Se indica cuántos entraron en cada análisis y si fue por intención de tratar.",
      "C16", "Resultados",     "Cada desenlace se informa con el tamaño del efecto y su precisión (IC).",
      "C17", "Resultados",     "Se informan los daños o efectos no deseados de cada grupo.",
      "C18", "Discusión",      "Se discuten las limitaciones: sesgos, imprecisión y multiplicidad.",
      "C19", "Otra información","Se da el número de registro del ensayo y dónde consultar el protocolo.",
      "C20", "Otra información","Se declaran las fuentes de financiamiento y su papel."),
    STROBE = tibble::tribble(
      ~id,   ~seccion,        ~item,
      "S01", "Título/resumen", "El título o el resumen indican el diseño del estudio y el resumen es informativo.",
      "S02", "Introducción",   "Se explica la justificación y se plantean objetivos o hipótesis.",
      "S03", "Métodos",        "Se presentan los elementos clave del diseño al inicio del texto, con lugares y fechas.",
      "S04", "Métodos",        "Se dan los criterios de elegibilidad, las fuentes y cómo se seleccionó a los participantes.",
      "S05", "Métodos",        "Exposición, desenlace, confusores y modificadores de efecto están definidos.",
      "S06", "Métodos",        "Se dice de dónde sale cada dato y cómo se midió, y si fue comparable entre grupos.",
      "S07", "Métodos",        "Se describe qué se hizo para reducir las fuentes de sesgo.",
      "S08", "Métodos",        "Se explica cómo se decidió el tamaño del estudio.",
      "S09", "Estadística",    "Se describen los métodos: control de la confusión, subgrupos, datos faltantes, pérdidas y análisis de sensibilidad.",
      "S10", "Resultados",     "Se cuentan los participantes en cada etapa (elegibles, incluidos, analizados) y las razones de exclusión; un diagrama ayuda.",
      "S11", "Resultados",     "Se describen las características de los participantes y cuántos datos faltan en cada variable.",
      "S12", "Resultados",     "Se informa el número de eventos o las medidas resumen del desenlace.",
      "S13", "Resultados",     "Se dan las estimaciones crudas y ajustadas con IC y se dice por qué variables se ajustó.",
      "S14", "Resultados",     "Se informan otros análisis (subgrupos, sensibilidad).",
      "S15", "Discusión",      "Se discuten las limitaciones, con la dirección probable de los sesgos.",
      "S16", "Discusión",      "Se interpreta con cautela y se discute si los resultados se pueden generalizar.",
      "S17", "Otra información","Se declara el financiamiento y su papel."))
  items |>
    dplyr::mutate(guia = guia, .before = 1) |>
    dplyr::mutate(estado = NA_character_, donde = NA_character_, nota = NA_character_)
}

#' Cruza una lista en blanco con la revisión de un manuscrito
#' @param lista salida de lista_verificacion()
#' @param manuscrito tibble con id, estado ("reportado", "no reportado", "no aplica"),
#'        donde (sección o página donde está) y nota (obligatoria si "no aplica")
#' @return la lista completada, con la columna `alerta` (qué falta por justificar)
cruzar_manuscrito <- function(lista, manuscrito) {
  validos <- c("reportado", "no reportado", "no aplica")
  desconocidos <- setdiff(manuscrito$id, lista$id)
  if (length(desconocidos) > 0) stop("Ítems que no están en la lista: ", paste(desconocidos, collapse = ", "))
  malos <- setdiff(unique(manuscrito$estado), validos)
  if (length(malos) > 0) stop("Estado no válido: ", paste(malos, collapse = ", "), ". Usa: ", paste(validos, collapse = " / "))
  for (col in c("donde", "nota")) if (!col %in% names(manuscrito)) manuscrito[[col]] <- NA_character_

  lista |>
    dplyr::select(-estado, -donde, -nota) |>
    dplyr::left_join(manuscrito |> dplyr::select(id, estado, donde, nota), by = "id") |>
    dplyr::mutate(
      estado = dplyr::coalesce(estado, "sin evaluar"),
      alerta = dplyr::case_when(
        estado == "sin evaluar"                       ~ "falta evaluar",
        estado == "reportado" & is.na(donde)          ~ "indica dónde está",
        estado == "no aplica" & is.na(nota)           ~ "justifica por qué no aplica",
        estado == "no reportado"                      ~ "decide: ¿se añade o se justifica?",
        TRUE                                          ~ ""))
}

#' Resumen de una lista cruzada. El denominador excluye los "no aplica".
resumen_lista <- function(x) {
  n_aplica <- sum(x$estado != "no aplica")
  tibble::tibble(
    items = nrow(x),
    reportado = sum(x$estado == "reportado"),
    no_reportado = sum(x$estado == "no reportado"),
    no_aplica = sum(x$estado == "no aplica"),
    sin_evaluar = sum(x$estado == "sin evaluar"),
    pct_de_los_aplicables = reportado / n_aplica,
    alertas = sum(x$alerta != ""))
}

# ======================================================================
# 2. Diagramas de flujo con ggplot2
# ======================================================================

#' Dibuja un diagrama de cajas y flechas
#' @param cajas tibble: id, x, y, w, texto, tipo ("etapa", "excluido" o "fase"); la altura se calcula
#' @param flechas tibble: de, a, tipo ("v" recta vertical, "lado" sale de la columna central hacia la caja,
#'        "codo" baja, gira y baja)
diagrama_flujo <- function(cajas, flechas = NULL, tam_texto = 3.1) {
  lineas <- stringr::str_count(cajas$texto, "\n") + 1
  cajas <- cajas |>
    dplyr::mutate(h = 0.42 * lineas + 0.45,
                  xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2)

  segs <- NULL
  if (!is.null(flechas) && nrow(flechas) > 0) {
    segs <- purrr::pmap_dfr(flechas, function(de, a, tipo, ...) {
      d <- cajas[match(de, cajas$id), ]; b <- cajas[match(a, cajas$id), ]
      if (tipo == "v") {
        tibble::tibble(x = d$x, y = d$ymin, xend = b$x, yend = b$ymax, punta = TRUE)
      } else if (tipo == "lado") {
        lado <- if (b$x > d$x) b$xmin else b$xmax
        tibble::tibble(x = d$x, y = b$y, xend = lado, yend = b$y, punta = TRUE)
      } else {                                              # codo
        ym <- (d$ymin + b$ymax) / 2
        tibble::tibble(x = c(d$x, d$x, b$x), y = c(d$ymin, ym, ym),
                       xend = c(d$x, b$x, b$x), yend = c(ym, ym, b$ymax), punta = c(FALSE, FALSE, TRUE))
      }
    })
  }

  g <- ggplot2::ggplot()
  if (!is.null(segs)) {
    g <- g +
      ggplot2::geom_segment(data = dplyr::filter(segs, !punta), ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            color = "grey35", linewidth = 0.5) +
      ggplot2::geom_segment(data = dplyr::filter(segs, punta), ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            color = "grey35", linewidth = 0.5,
                            arrow = ggplot2::arrow(length = grid::unit(2.2, "mm"), type = "closed"))
  }
  g +
    ggplot2::geom_rect(data = cajas, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = tipo),
                       color = "grey35", linewidth = 0.4) +
    ggplot2::geom_text(data = cajas, ggplot2::aes(x = x, y = y, label = texto, fontface = ifelse(tipo == "fase", "bold", "plain")),
                       size = tam_texto, lineheight = 0.95) +
    ggplot2::scale_fill_manual(values = c(etapa = "white", excluido = "#F1F1F1", fase = "#D6E6F4"), guide = "none") +
    ggplot2::coord_cartesian(xlim = c(0, 10), ylim = c(min(cajas$ymin) - 0.3, max(cajas$ymax) + 0.3), expand = FALSE) +
    ggplot2::theme_void()
}

.n <- function(x) format(x, big.mark = " ", trim = TRUE)

#' Diagrama de flujo CONSORT de un ensayo de dos brazos
#' @param evaluados n evaluados para elegibilidad
#' @param excluidos vector con nombre: razón -> n (la suma es el total de excluidos)
#' @param brazos vector con nombre: brazo -> n aleatorizados
#' @param recibieron vector con nombre (mismo orden) con quienes recibieron la intervención asignada; NA si no se registra
#' @param sin_seguimiento vector con nombre: n sin la medición principal
#' @param analizados vector con nombre: n analizados para el desenlace principal
flujo_consort <- function(evaluados, excluidos, brazos, recibieron, sin_seguimiento, analizados,
                          medicion = "TFGe a 12 meses", tam_texto = 3.1) {
  xs <- c(3.9, 7.3); ids <- c("izq", "der")
  ys <- c(eval = 13.4, exc = 11.5, alea = 9.9, asig = 7.9, seg = 5.7, ana = 3.6)
  txt_exc <- paste0("Excluidos (n = ", .n(sum(excluidos)), ")\n",
                    paste0(names(excluidos), ": ", .n(excluidos), collapse = "\n"))
  linea_rec <- ifelse(is.na(recibieron), "Intervención recibida: no registrada",
                      paste0("Recibieron la intervención: ", .n(recibieron)))
  cajas <- dplyr::bind_rows(
    tibble::tibble(id = c("eval", "exc", "alea"), x = c(5.6, 8.35, 5.6), y = ys[c("eval", "exc", "alea")], w = c(3.3, 2.9, 3.3), tipo = "etapa",
                   texto = c(paste0("Evaluados para elegibilidad\n(n = ", .n(evaluados), ")"), txt_exc,
                             paste0("Aleatorizados\n(n = ", .n(sum(brazos)), ")"))),
    tibble::tibble(id = paste0("asig_", ids), x = xs, y = ys[["asig"]], w = 3.0, tipo = "etapa",
                   texto = paste0("Asignados a ", names(brazos), "\n(n = ", .n(brazos), ")\n", linea_rec)),
    tibble::tibble(id = paste0("seg_", ids), x = xs, y = ys[["seg"]], w = 3.0, tipo = "etapa",
                   texto = paste0("Sin ", medicion, "\n(n = ", .n(sin_seguimiento), ")")),
    tibble::tibble(id = paste0("ana_", ids), x = xs, y = ys[["ana"]], w = 3.0, tipo = "etapa",
                   texto = paste0("Analizados: ", medicion, "\n(n = ", .n(analizados), ")")),
    tibble::tibble(id = paste0("fase_", 1:4), x = 0.95, y = ys[c("eval", "asig", "seg", "ana")], w = 1.6, tipo = "fase",
                   texto = c("Reclutamiento", "Asignación", "Seguimiento", "Análisis")))
  cajas$tipo[cajas$id == "exc"] <- "excluido"
  flechas <- tibble::tribble(
    ~de, ~a, ~tipo,
    "eval", "alea", "v",
    "eval", "exc", "lado",
    "alea", "asig_izq", "codo", "alea", "asig_der", "codo",
    "asig_izq", "seg_izq", "v", "asig_der", "seg_der", "v",
    "seg_izq", "ana_izq", "v", "seg_der", "ana_der", "v")
  diagrama_flujo(cajas, flechas, tam_texto)
}

#' Diagrama de participantes de un estudio observacional (STROBE/RECORD)
#' @param revisados n de registros revisados; @param excluidos vector con nombre razón -> n
#' @param grupos vector con nombre: grupo -> n; @param eventos vector con eventos por grupo (opcional)
flujo_observacional <- function(revisados, excluidos, grupos, eventos = NULL, tam_texto = 3.1) {
  ys <- c(rev = 10.6, exc = 8.9, inc = 7.3, gru = 4.9)
  txt_exc <- paste0("Excluidos (n = ", .n(sum(excluidos)), ")\n",
                    paste0(names(excluidos), ": ", .n(excluidos), collapse = "\n"))
  txt_gru <- paste0(names(grupos), "\n(n = ", .n(grupos), ")",
                    if (!is.null(eventos)) paste0("\nEventos: ", .n(eventos)) else "")
  cajas <- dplyr::bind_rows(
    tibble::tibble(id = c("rev", "exc", "inc"), x = c(4.4, 7.9, 4.4), y = ys[c("rev", "exc", "inc")], w = c(3.6, 3.4, 3.6),
                   tipo = c("etapa", "excluido", "etapa"),
                   texto = c(paste0("Registros revisados\n(n = ", .n(revisados), ")"), txt_exc,
                             paste0("Incluidos en el análisis\n(n = ", .n(sum(grupos)), ")"))),
    tibble::tibble(id = paste0("g", seq_along(grupos)), x = c(2.7, 6.1)[seq_along(grupos)], y = ys[["gru"]], w = 3.1,
                   tipo = "etapa", texto = txt_gru))
  flechas <- tibble::tibble(
    de = c("rev", "rev", "inc", "inc"), a = c("inc", "exc", "g1", "g2"), tipo = c("v", "lado", "codo", "codo"))
  diagrama_flujo(cajas, flechas, tam_texto)
}

# ======================================================================
# 3. Tablas y figuras publicables
# ======================================================================

#' Paleta de Okabe-Ito (distinguible con las formas habituales de daltonismo)
paleta_libro <- c(azul = "#0072B2", naranja = "#D55E00", verde = "#009E73", amarillo = "#E69F00",
                  celeste = "#56B4E9", rosa = "#CC79A7", gris = "#999999")

#' Tema de figuras del libro: fondo blanco, rejilla suave, leyenda abajo, texto legible al reducir
theme_libro <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      axis.title = ggplot2::element_text(color = "grey20"),
      axis.text = ggplot2::element_text(color = "grey25"),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05)),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(color = "grey40", hjust = 0, size = ggplot2::rel(0.8)),
      plot.caption.position = "plot",
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank())
}

#' Tabla flextable lista para Word: bordes de revista, fuente fija, pie opcional
#' @param df data.frame ya formateado (texto), @param titulo título de la tabla, @param pie vector de líneas de pie
ft_libro <- function(df, titulo = NULL, pie = NULL, fuente = "Arial", tam = 9) {
  ft <- flextable::flextable(df)
  if (!is.null(titulo)) ft <- flextable::set_caption(ft, caption = titulo)
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = fuente, part = "all")
  ft <- flextable::fontsize(ft, size = tam, part = "all")
  ft <- flextable::align(ft, j = seq_along(df)[-1], align = "center", part = "all")
  if (!is.null(pie)) ft <- flextable::add_footer_lines(ft, pie)
  flextable::autofit(ft)
}

#' Guarda una figura en varios formatos (por defecto en una carpeta temporal)
#' @return tibble con el archivo, el tamaño en píxeles (png) y el tamaño en KB
guardar_fig <- function(p, nombre, ancho_cm = 17, alto_cm = 10, dpi = 300,
                        carpeta = tempdir(), formatos = c("png", "pdf")) {
  rutas <- file.path(carpeta, paste0(nombre, ".", formatos))
  for (r in rutas) ggplot2::ggsave(r, p, width = ancho_cm, height = alto_cm, units = "cm", dpi = dpi, bg = "white")
  tibble::tibble(archivo = basename(rutas), kb = round(file.size(rutas) / 1024),
                 pixeles = vapply(rutas, function(r) if (grepl("png$", r)) {
                   d <- dim(png::readPNG(r, native = TRUE)); paste(d[2], "x", d[1])
                 } else "vectorial", ""))
}

# ======================================================================
# 4. Proyecto reproducible
# ======================================================================

#' Resumen de la sesión: R, sistema y versión de cada paquete usado
registrar_sesion <- function(paquetes) {
  tibble::tibble(
    componente = c("R", "Sistema", paquetes),
    version = c(paste(R.version$major, R.version$minor, sep = "."), R.version$platform,
                vapply(paquetes, function(p) as.character(utils::packageVersion(p)), "")))
}

#' Texto de un renv.lock ILUSTRATIVO con las versiones realmente instaladas.
#' renv lo escribe solo (renv::snapshot()); el Hash lo calcula renv, aquí va un marcador.
renv_lock_ejemplo <- function(paquetes) {
  pk <- lapply(stats::setNames(paquetes, paquetes), function(p)
    list(Package = p, Version = as.character(utils::packageVersion(p)), Source = "Repository",
         Repository = "CRAN", Hash = "<calculado por renv>"))
  lock <- list(R = list(Version = paste(R.version$major, R.version$minor, sep = "."),
                        Repositories = list(list(Name = "CRAN", URL = "https://cloud.r-project.org"))),
               Packages = pk)
  jsonlite::toJSON(lock, pretty = TRUE, auto_unbox = TRUE)
}

#' Mini-análisis reproducible: razón de tasas (IRR) de un tratamiento, cruda y ajustada,
#' con IC por bootstrap. Todo lo que cambia el resultado entra como argumento.
#' @return lista con `resultado` (tibble) y `meta` (qué archivo, qué semilla, qué ajuste)
analisis_minimo <- function(ruta, exposicion = "isglt2", desenlace = "evento", tiempo = "tiempo_meses",
                            ajuste = c("edad", "sexo", "hba1c", "pas", "tfg_basal", "luacr", "ecv"),
                            n_boot = 200, semilla = 2026) {
  datos <- readr::read_csv(ruta, show_col_types = FALSE)
  datos$luacr <- log(datos$uacr)
  ajustar <- function(d, vars) {
    f <- stats::reformulate(c(exposicion, vars, paste0("offset(log(", tiempo, "))")), desenlace)
    exp(stats::coef(stats::glm(f, data = d, family = stats::poisson))[[exposicion]])
  }
  set.seed(semilla)                                              # la semilla se fija UNA vez, aquí
  boot <- replicate(n_boot, ajustar(datos[sample.int(nrow(datos), replace = TRUE), ], ajuste))
  list(resultado = tibble::tibble(
         modelo = c("Crudo", "Ajustado"),
         irr = c(ajustar(datos, character()), ajustar(datos, ajuste)),
         ic_inf = c(NA, stats::quantile(boot, 0.025)), ic_sup = c(NA, stats::quantile(boot, 0.975))),
       meta = list(archivo = basename(ruta), md5 = unname(tools::md5sum(ruta)), filas = nrow(datos),
                   semilla = semilla, n_boot = n_boot, ajuste = ajuste))
}

# ======================================================================
# 5. Multiplicidad: el jardín de senderos bifurcados
# ======================================================================

# Prueba t de dos muestras (varianzas iguales), vectorizada por filas (cada fila = un estudio)
.p_t_filas <- function(x, y) {
  n1 <- ncol(x); n2 <- ncol(y)
  m1 <- rowMeans(x); m2 <- rowMeans(y)
  v1 <- (rowSums(x^2) - n1 * m1^2) / (n1 - 1); v2 <- (rowSums(y^2) - n2 * m2^2) / (n2 - 1)
  sp <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
  2 * stats::pt(-abs((m1 - m2) / (sp * sqrt(1 / n1 + 1 / n2))), n1 + n2 - 2)
}

#' Valores p de k desenlaces independientes en n_est estudios (matriz n_est x k)
#' @param d vector de longitud k con el efecto verdadero (en DE) de cada desenlace; 0 = sin efecto
p_desenlaces <- function(n_est, n_por_grupo, k, d = rep(0, k)) {
  sapply(seq_len(k), function(j)
    .p_t_filas(matrix(stats::rnorm(n_est * n_por_grupo), n_est), matrix(stats::rnorm(n_est * n_por_grupo, d[j]), n_est)))
}

#' Valores p del efecto del tratamiento dentro de k subgrupos al azar (sin efecto real en ninguno)
#' Cada estudio tiene un solo desenlace y k variables binarias sin relación con él (matriz n_est x k)
p_subgrupos <- function(n_est, n_total, k) {
  t(replicate(n_est, {
    trat <- sample(rep(0:1, length.out = n_total)); y <- stats::rnorm(n_total)
    vapply(seq_len(k), function(j) {
      s <- stats::rbinom(n_total, 1, 0.5) == 1
      stats::t.test(y[s & trat == 1], y[s & trat == 0], var.equal = TRUE)$p.value
    }, 0)
  }))
}

#' Error global y potencia con distintas correcciones, con 1 desenlace verdadero y k - 1 nulos
#' @return tibble: método, probabilidad de al menos un falso positivo, proporción de falsos entre los hallazgos, potencia
simular_multiplicidad <- function(n_est = 2000, n_por_grupo = 64, k = 20, d_verdadero = 0.5, semilla = 1) {
  set.seed(semilla)
  p <- p_desenlaces(n_est, n_por_grupo, k, d = c(d_verdadero, rep(0, k - 1)))   # columna 1 = efecto verdadero
  metodos <- c(`Sin corregir` = "none", Bonferroni = "bonferroni", Holm = "holm", `Benjamini-Hochberg` = "BH")
  purrr::map_dfr(names(metodos), function(m) {
    pa <- if (metodos[[m]] == "none") p else t(apply(p, 1, stats::p.adjust, method = metodos[[m]]))
    falsos <- rowSums(pa[, -1, drop = FALSE] < 0.05); total <- falsos + (pa[, 1] < 0.05)
    tibble::tibble(metodo = m,
                   error_global = mean(falsos > 0),                         # P(al menos un falso positivo)
                   falsos_entre_hallazgos = mean(ifelse(total > 0, falsos / total, 0)),   # proporción de falsos entre los «positivos» (FDR)
                   potencia = mean(pa[, 1] < 0.05))
  })
}

# ======================================================================
# 6. Privacidad: k-anonimato sencillo y celdas pequeñas
# ======================================================================

#' k-anonimato: tamaño de cada "clase de equivalencia" (personas que comparten los mismos cuasi-identificadores)
#' @return tibble de una fila con k mínimo, número de personas únicas y % de personas en clases con k < umbral
k_anonimato <- function(datos, cuasi, umbral = 5) {
  clases <- datos |> dplyr::count(dplyr::across(dplyr::all_of(cuasi)), name = "k")
  tibble::tibble(
    cuasi_identificadores = paste(cuasi, collapse = " + "),
    n_clases = nrow(clases),
    k_minimo = min(clases$k),
    personas_unicas = sum(clases$k == 1),
    pct_en_clases_menores = sum(clases$k[clases$k < umbral]) / nrow(datos))
}

#' Tabla de contingencia con las celdas menores que el umbral marcadas
#' @return tibble largo: fila, columna, n y `publicable` (FALSE si n < umbral y n > 0 se suprime)
celdas_pequenas <- function(datos, fila, columna, umbral = 5) {
  datos |>
    dplyr::count(.data[[fila]], .data[[columna]], .drop = FALSE) |>
    dplyr::rename(fila = 1, columna = 2) |>
    dplyr::mutate(publicable = n >= umbral | n == 0,
                  mostrar = dplyr::if_else(publicable, as.character(n), paste0("<", umbral)))
}

#' Crea (por defecto en una carpeta temporal) el esqueleto de carpetas de un proyecto reproducible
#' @return vector con las carpetas y archivos creados (rutas relativas a la raíz)
crear_esqueleto <- function(raiz = file.path(tempdir(), "mi_estudio")) {
  carpetas <- c("datos/crudos", "datos/procesados", "R", "analisis", "informes", "resultados/tablas", "resultados/figuras")
  for (d in carpetas) dir.create(file.path(raiz, d), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("# mi_estudio", "", "Los datos crudos no se modifican ni se suben al repositorio."), file.path(raiz, "README.md"))
  writeLines(c("datos/", "resultados/", ".Renviron", ".Rhistory", ".RData", ".Rproj.user/", "renv/library/"), file.path(raiz, ".gitignore"))
  sort(list.files(raiz, recursive = TRUE, include.dirs = TRUE, all.files = TRUE))
}
