* Mortalidad

import excel "C:\Users\danfe\Dropbox\Proyectos Daniel\Cursos - diplomados - Maestrias\Programa de autoformación en investigación y análisis de datos\Rmarkdowns\Estadística\Programa de formación\Semana12 svy_ENDES\study_data_cleaned.xlsx", sheet("Sheet1") firstrow

svyset [pw = surv_weight_cluster_strata], psu(cluster_number_uid) strata (health_district) single(certainty)

svy: tab sex

tab died
svy: tab died

tab died sex, row
svy: tab  died sex, row ci

svy: tab  age_category sex, row ci


tab  age_category sex, row 

svy: tab age_category
tab age_category

encode age_category, gen (age_category2)
proportion age_category2

recode age_category2 (6 = 7 "45+ years")  (5=6 "30-44 years")   (3=5 "15-29 years") (4=4  "3-14 years") (1=3 "0-2 years")  (8=2 "9-12 months")  (7=1 "6-8 months") (2=0 "0-5 months"), gen (age_category4)

encode sex, gen (sex2)
proportion sex2 

svy: tab sex2 died, row ci


gen sex3 = sex2-1

svy: poisson sex2 i.died i.age_category4, irr

svy: logistic sex3 i.age_category4
svy: logistic sex3 i.died
svy: logistic sex3 i.died i.age_category4







* ENDES

use ENDE.dta, clear

svyset [pw = PONDERACION], psu(CONGLOMERADO) strata (ESTRATO) single(certainty)

drop if DIETA==.

svy: proportion HIPERTENSION
tab HIPERTENSION
svy: tab HIPERTENSION

svy: mean PESO
estat sd

svy, subpop (if HIPERTENSION==1): mean PESO
estat sd

tab DIETA HIPERTENSION, chi2
svy: tab DIETA HIPERTENSION, row

codebook DIETA
svy, subpop (if HIPERTENSION==1): proportion DIETA
svy, subpop (if HIPERTENSION==2): proportion DIETA

*******Tabla 3: Regresion********************
*Modelo crudo
recode HIPERTENSION (1=0) (2=1)

svy: logistic HIPERTENSION i.DIETA
svy: logistic HIPERTENSION PESO
*Modelo ajustado: variables p<0.2 en bivariado

svy: logistic HIPERTENSION  i. SEXO i.AREA_RESIDENCIA  i.QUINTIL_BIENESTAR i.REGION_NATURAL i.OBESIDAD i.DIETA






















