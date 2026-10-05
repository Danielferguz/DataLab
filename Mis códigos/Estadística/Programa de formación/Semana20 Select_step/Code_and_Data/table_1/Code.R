setwd("C:/Users/danfe/Dropbox/Proyectos Daniel/Cursos - diplomados - Maestrias/Programa de autoformación en investigación y análisis de datos/Rmarkdowns/Estadística/Programa de formación/Semana13/Code_and_Data/table_1")

# Load data ------------------------------------------------------
case1.bodyfat <-
  read.table("data/case1_bodyfat.txt", header = T, sep = ";")

# Estimation of models -------------------------------------------
mod1 <- lm(siri ~ weight_kg, data = case1.bodyfat, x = T, y = T)
mod2 <- lm(siri ~ weight_kg + height_cm, data = case1.bodyfat, x = T, y = T)
mod3 <- lm(siri ~ weight_kg + abdomen, data = case1.bodyfat, x = T, y = T)
mod4 <- lm(siri ~ weight_kg + height_cm + abdomen, data = case1.bodyfat, 
           x = T, y = T)

# Coefficients and R?  -------------------------------------------
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
