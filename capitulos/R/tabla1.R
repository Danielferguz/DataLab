# ------------------------------------------------------------------------
# tabla1(): "Tabla 1" de características basales (sin paquetes externos)
# ------------------------------------------------------------------------
# Uso:
#   source("R/tabla1.R")
#   tabla1(datos, vars = c("edad", "sexo", "tfg_basal"), grupo = "isglt2",
#          no_normales = "uacr", etiquetas = c(edad = "Edad, años"))
#
# - Numéricas: media (DE); mediana [Q1, Q3] si están en `no_normales`.
# - Categóricas (texto, factor o lógicas): n (%). Una variable 0/1 se declara en `binarias`.
# - Con `grupo` agrega columnas por grupo, valor p y diferencia estandarizada (DEM / SMD).
# - Siempre indica los datos faltantes (n y %) de cada variable.
# ------------------------------------------------------------------------

tabla1 <- function(datos, vars, grupo = NULL, no_normales = character(), binarias = character(),
                   etiquetas = NULL, digitos = 1, valor_p = TRUE, dem = TRUE, faltantes = TRUE) {

  fmt  <- function(x, d = digitos) formatC(x, format = "f", digits = d, big.mark = "")
  pct  <- function(n, N) paste0(n, " (", fmt(100 * n / N), "%)")
  etq  <- function(v) if (!is.null(etiquetas) && v %in% names(etiquetas)) etiquetas[[v]] else v

  g <- if (is.null(grupo)) factor(rep("Total", nrow(datos))) else factor(datos[[grupo]])
  niveles_g <- levels(g)
  p_corto <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))

  resumen_num <- function(x, mediana) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return("—")
    if (mediana) { q <- stats::quantile(x, c(.25, .5, .75)); paste0(fmt(q[2]), " [", fmt(q[1]), ", ", fmt(q[3]), "]") }
    else paste0(fmt(mean(x)), " (", fmt(stats::sd(x)), ")")
  }

  dem_cat <- function(ind, g2) {                    # DEM para un indicador 0/1 entre dos grupos
    p1 <- mean(ind[g2 == niveles_g[2]], na.rm = TRUE); p0 <- mean(ind[g2 == niveles_g[1]], na.rm = TRUE)
    s <- sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
    if (s == 0) NA_real_ else (p1 - p0) / s
  }
  dem_num <- function(x, g2) {
    m1 <- mean(x[g2 == niveles_g[2]], na.rm = TRUE); m0 <- mean(x[g2 == niveles_g[1]], na.rm = TRUE)
    s <- sqrt((stats::var(x[g2 == niveles_g[2]], na.rm = TRUE) + stats::var(x[g2 == niveles_g[1]], na.rm = TRUE)) / 2)
    (m1 - m0) / s
  }

  fila <- function(etiqueta, valores, p = NA_real_, d = NA_real_, falt = "") {
    out <- tibble::tibble(Variable = etiqueta)
    for (k in seq_along(niveles_g)) out[[niveles_g[k]]] <- valores[k]
    if (!is.null(grupo) && length(niveles_g) > 1) out[["Total"]] <- valores[length(niveles_g) + 1]
    if (faltantes) out[["Faltantes"]] <- falt
    if (!is.null(grupo) && valor_p) out[["p"]] <- p_corto(p)
    if (!is.null(grupo) && dem && length(niveles_g) == 2) out[["DEM"]] <- ifelse(is.na(d), "", fmt(d, 2))
    out
  }

  filas <- list(
    fila("n", c(as.character(table(g)), if (!is.null(grupo)) as.character(nrow(datos))))
  )
  # En la fila de n no hay faltantes ni p
  filas[[1]]$Faltantes <- if (faltantes) "" else NULL

  for (v in vars) {
    x <- datos[[v]]
    es_cat <- is.character(x) || is.factor(x) || is.logical(x) || v %in% binarias
    n_falt <- sum(is.na(x)); txt_falt <- if (n_falt == 0) "0" else pct(n_falt, length(x))
    grupos <- c(split(x, g), if (!is.null(grupo)) list(x))

    if (!es_cat) {
      med <- v %in% no_normales
      etiqueta_v <- paste0(etq(v), if (med) ", mediana [Q1, Q3]" else ", media (DE)")
      p <- NA_real_
      if (!is.null(grupo) && valor_p && length(niveles_g) >= 2) {
        p <- tryCatch(
          if (length(niveles_g) == 2) {
            if (med) stats::wilcox.test(x ~ g)$p.value else stats::t.test(x ~ g)$p.value
          } else if (med) stats::kruskal.test(x ~ g)$p.value else summary(stats::aov(x ~ g))[[1]][["Pr(>F)"]][1],
          error = function(e) NA_real_)
      }
      d <- if (!is.null(grupo) && length(niveles_g) == 2) dem_num(x, g) else NA_real_
      filas[[length(filas) + 1]] <- fila(etiqueta_v, vapply(grupos, resumen_num, "", mediana = med), p, d, txt_falt)
    } else {
      xf <- if (v %in% binarias) factor(x, levels = c(0, 1), labels = c("No", "Sí")) else factor(x)
      p <- NA_real_
      if (!is.null(grupo) && valor_p && length(niveles_g) >= 2 && nlevels(xf) >= 2) {
        tb <- table(xf, g)
        p <- tryCatch(
          if (any(suppressWarnings(stats::chisq.test(tb)$expected) < 5)) stats::fisher.test(tb, simulate.p.value = nrow(tb) > 2)$p.value
          else stats::chisq.test(tb, correct = FALSE)$p.value,
          error = function(e) NA_real_)
      }
      if (v %in% binarias) {   # una sola fila: n (%) con el valor 1
        val <- vapply(grupos, \(z) pct(sum(z == 1, na.rm = TRUE), sum(!is.na(z))), "")
        d <- if (!is.null(grupo) && length(niveles_g) == 2) dem_cat(as.numeric(x), g) else NA_real_
        filas[[length(filas) + 1]] <- fila(paste0(etq(v), ", n (%)"), val, p, d, txt_falt)
      } else {
        filas[[length(filas) + 1]] <- fila(paste0(etq(v), ", n (%)"), rep("", length(grupos)), p, NA_real_, txt_falt)
        for (nv in levels(xf)) {
          val <- vapply(grupos, \(z) pct(sum(z == nv, na.rm = TRUE), sum(!is.na(z))), "")
          d <- if (!is.null(grupo) && length(niveles_g) == 2) dem_cat(as.numeric(x == nv), g) else NA_real_
          f <- fila(paste0("\u00a0\u00a0\u00a0", nv), val, NA_real_, d, "")
          filas[[length(filas) + 1]] <- f
        }
      }
    }
  }
  dplyr::bind_rows(filas)
}
