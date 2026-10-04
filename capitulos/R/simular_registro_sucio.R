# ------------------------------------------------------------------------
# Registro de nefrología "sucio" (datos SIMULADOS) para practicar limpieza
# ------------------------------------------------------------------------
# Se genera primero una base LIMPIA (la verdad) y luego se la estropea con los
# errores típicos de un registro real. Así, al final del Cap. 5 se puede
# comprobar si la limpieza recuperó la verdad.
#
# Archivos que crea (en Bases/):
#   registro_nefro_sucio.csv   base con errores
#   registro_nefro_verdad.csv  base limpia (solo posible por simulación)
#   lab_visitas_ancho.csv      creatinina en 3 visitas (formato ancho)
#   medicamentos.csv           un fármaco por fila (formato largo)
# ------------------------------------------------------------------------

simular_registro <- function(n = 420, semilla = 77) {
  set.seed(semilla)
  id <- sprintf("P%03d", 1:n)
  sexo <- sample(c("F", "M"), n, replace = TRUE)
  fecha_nac <- as.Date("1940-01-01") + sample(0:(365 * 50), n, replace = TRUE)
  fecha_ingreso <- as.Date("2022-01-01") + sample(0:900, n, replace = TRUE)
  edad <- as.integer(floor(as.numeric(fecha_ingreso - fecha_nac) / 365.25))
  peso <- round(rnorm(n, 72, 13), 1)
  talla <- round(rnorm(n, 163, 9))
  creat <- round(pmin(pmax(exp(rnorm(n, log(1.6), 0.45)), 0.7), 7), 2)
  uacr <- round(exp(rnorm(n, log(90), 1.1)))
  pas <- round(rnorm(n, 138, 18)); pad <- round(rnorm(n, 82, 10))
  diagnostico <- sample(c("ERC diabética", "ERC hipertensiva", "Glomerulopatía", "Otro"),
                        n, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))
  diabetes <- ifelse(diagnostico == "ERC diabética", "Sí", sample(c("Sí", "No"), n, TRUE, c(0.2, 0.8)))
  tibble::tibble(id, sexo, fecha_nac, fecha_ingreso, edad, peso, talla, creat_mgdl = creat,
                 uacr, pas, pad, diagnostico, diabetes)
}

ensuciar_registro <- function(v, semilla = 78) {
  set.seed(semilla)
  n <- nrow(v)
  fmt_fecha <- function(x) {
    k <- runif(length(x))
    ifelse(k < 0.8, format(x, "%d/%m/%Y"), format(x, "%Y-%m-%d"))
  }
  # Creatinina: ~15 % en µmol/L; ~30 % escrita con coma decimal; algunas "ND"
  en_umol <- runif(n) < 0.15
  creat <- ifelse(en_umol, round(v$creat_mgdl * 88.4), v$creat_mgdl)
  creat_txt <- ifelse(runif(n) < 0.30, sub("\\.", ",", as.character(creat)), as.character(creat))
  creat_txt[sample(n, 8)] <- "ND"
  sucio <- tibble::tibble(
    `ID Paciente` = v$id,
    `Sexo ` = sample_var(v$sexo, n),
    `Fecha Nac.` = fmt_fecha(v$fecha_nac),
    `Fecha de ingreso` = fmt_fecha(v$fecha_ingreso),
    Edad = v$edad,
    `Peso (kg)` = v$peso,
    `Talla (cm)` = v$talla,
    Creatinina = creat_txt,
    Unidad_creat = ifelse(en_umol, "umol/L", "mg/dL"),
    `UACR mg/g` = v$uacr,
    PAS = v$pas, PAD = v$pad,
    Diagnostico = variar_dx(v$diagnostico),
    DM = ifelse(runif(n) < 0.1, tolower(v$diabetes), v$diabetes)
  )
  # códigos de faltante y valores imposibles
  sucio$PAS[sample(n, 12)] <- 999
  sucio$`UACR mg/g`[sample(n, 10)] <- -1
  sucio$PAS[sample(n, 3)] <- 1400            # error de tipeo (140)
  sucio$`Peso (kg)`[sample(n, 3)] <- 0
  i_edad <- sample(n, 3); sucio$Edad[i_edad] <- sucio$Edad[i_edad] + 100
  sucio$PAD[sample(n, 4)] <- NA
  # espacios sobrantes en IDs
  i_id <- sample(n, 15); sucio$`ID Paciente`[i_id] <- paste0(" ", sucio$`ID Paciente`[i_id])
  # fecha de registro y duplicados: exactos y con datos discordantes (se queda el más reciente)
  sucio$fecha_registro <- as.character(as.Date("2024-07-01") + sample(0:30, n, TRUE))
  dup_exacto <- sucio[sample(n, 10), ]
  dup_conflicto <- sucio[sample(n, 4), ]
  dup_conflicto$PAS <- 130; dup_conflicto$fecha_registro <- "2024-08-15"   # actualización posterior
  rbind(sucio, dup_exacto, dup_conflicto)[sample(n + 14), ]
}

sample_var <- function(x, n) {
  dplyr::case_when(
    x == "F" ~ sample(c("F", "f", "Femenino", "FEMENINO", "F "), n, TRUE, c(.5, .1, .2, .1, .1)),
    TRUE     ~ sample(c("M", "m", "Masculino", "masculino", "Hombre"), n, TRUE, c(.5, .1, .2, .1, .1)))
}
variar_dx <- function(x) {
  n <- length(x)
  dplyr::case_when(
    x == "ERC diabética"   & runif(n) < .3 ~ sample(c("erc diabetica", "ERC diabetica ", "Nefropatía diabética"), n, TRUE),
    x == "ERC hipertensiva" & runif(n) < .3 ~ sample(c("erc hipertensiva", "ERC Hipertensiva "), n, TRUE),
    x == "Glomerulopatía"  & runif(n) < .3 ~ sample(c("glomerulopatia", "Glomerulopatia "), n, TRUE),
    TRUE ~ x)
}

guardar_registro <- function(carpeta = "Bases") {
  v <- simular_registro()
  s <- ensuciar_registro(v)
  readr::write_csv(s, file.path(carpeta, "registro_nefro_sucio.csv"))
  readr::write_csv(v, file.path(carpeta, "registro_nefro_verdad.csv"))
  # Visitas de laboratorio en formato ancho (creatinina basal, 6 y 12 meses)
  set.seed(79)
  lab <- tibble::tibble(id = v$id, creat_basal = v$creat_mgdl) |>
    dplyr::mutate(creat_6m = round(creat_basal * exp(rnorm(dplyr::n(), 0.04, 0.12)), 2),
                  creat_12m = round(creat_6m * exp(rnorm(dplyr::n(), 0.05, 0.12)), 2)) |>
    dplyr::slice_sample(prop = 0.9)
  readr::write_csv(lab, file.path(carpeta, "lab_visitas_ancho.csv"))
  # Medicamentos: un fármaco por fila (algunos pacientes con varios, otros ninguno)
  set.seed(80)
  med <- tibble::tibble(id = sample(v$id, 600, replace = TRUE),
                        farmaco = sample(c("IECA/ARA-II", "iSGLT2", "Estatina", "Diurético", "Bicarbonato"),
                                         600, TRUE, c(.35, .15, .25, .15, .10))) |>
    dplyr::distinct()
  readr::write_csv(med, file.path(carpeta, "medicamentos.csv"))
  invisible(list(sucio = s, verdad = v))
}
