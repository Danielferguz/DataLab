# Genera archivos de ejemplo en distintos formatos (CSV con ;, Excel, Stata, SPSS)
# a partir de ckd_isglt2.csv, para practicar la importación (Cap. 4.3).
# Ejecutar a mano:  source("capitulos/R/crear_archivos_ejemplo.R")
library(tidyverse); library(haven)
ckd <- read_csv("capitulos/Bases/ckd_isglt2.csv", show_col_types = FALSE) |> slice(1:150) |>
  select(id, edad, sexo, hba1c, pas, tfg_basal, uacr, ecv, isglt2)

# CSV europeo/latinoamericano: separador ";" y coma decimal
write_csv2(ckd, "capitulos/Bases/ejemplo_pacientes_puntoycoma.csv")

# Stata y SPSS (con etiquetas de variable y de valores)
ckd_lab <- ckd |>
  mutate(sexo = labelled(if_else(sexo == "Hombre", 1L, 2L), c(Hombre = 1L, Mujer = 2L), label = "Sexo"),
         isglt2 = labelled(isglt2, c(`No inició` = 0, `Inició iSGLT2` = 1), label = "Tratamiento"),
         tfg_basal = labelled(tfg_basal, label = "TFGe basal (mL/min/1.73 m2)"))
write_dta(ckd_lab, "capitulos/Bases/ejemplo_pacientes.dta")
write_sav(ckd_lab, "capitulos/Bases/ejemplo_pacientes.sav")
