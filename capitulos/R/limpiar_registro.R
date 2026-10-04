# Pasos de limpieza básica del registro (resumen de los Caps. 5.1 a 5.3).
# Se usa en los subcapítulos 5.4 a 5.6 para partir de una base ya estandarizada.
limpiar_basico <- function(raw) {
  raw |>
    dplyr::rename_with(limpiar_nombres) |>
    dplyr::mutate(
      id_paciente = stringr::str_trim(id_paciente),
      sexo_txt = stringr::str_to_lower(stringr::str_trim(sexo)),
      sexo = dplyr::case_when(sexo_txt %in% c("f", "femenino") ~ "F",
                              sexo_txt %in% c("m", "masculino", "hombre") ~ "M"),
      dm = dplyr::if_else(stringr::str_to_lower(dm) %in% c("sí", "si"), "Sí", "No"),
      dx_norm = diagnostico |> stringr::str_squish() |> stringr::str_to_lower() |>
        stringi::stri_trans_general("Latin-ASCII"),
      diagnostico = dplyr::case_when(
        stringr::str_detect(dx_norm, "diabet")    ~ "ERC diabética",
        stringr::str_detect(dx_norm, "hipertens") ~ "ERC hipertensiva",
        stringr::str_detect(dx_norm, "glomerul")  ~ "Glomerulopatía",
        dx_norm == "otro"                         ~ "Otro"),
      fecha_nac        = lubridate::as_date(lubridate::parse_date_time(fecha_nac, orders = c("dmy", "ymd"))),
      fecha_de_ingreso = lubridate::as_date(lubridate::parse_date_time(fecha_de_ingreso, orders = c("dmy", "ymd"))),
      fecha_registro   = lubridate::as_date(fecha_registro)
    ) |>
    dplyr::select(-sexo_txt, -dx_norm)
}
