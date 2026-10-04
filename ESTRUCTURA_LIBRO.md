# Estructura propuesta del libro (borrador para tu aprobación)

Objetivo: completar el libro **de lo simple a lo complejo**, siempre con ejemplos de nefrología, código reutilizable y cuadros de resumen. Este documento propone la estructura **antes** de escribir; cuando la apruebes (o la ajustes) sigo capítulo por capítulo.

> **Estado de tus códigos de Dropbox:** no pude leerlos. El entorno bloquea `www.dropbox.com` (política de red). Opciones: (a) agregar `dropbox.com` en *Network access → Custom → Allowed domains* del entorno, (b) subir los archivos `.R`/`.qmd` a una carpeta del repo (p. ej. `mis_codigos/`), o (c) pegarme los más importantes. Cuando los tenga, cada capítulo marcará qué código tuyo se integró.

---

## 1. Principios de diseño

1. **Escalera de complejidad.** Cada capítulo empieza con la pregunta clínica y el método más simple, y termina con la versión que usaría un investigador exigente. Cada subcapítulo lleva una marca: 🟢 básico · 🟡 intermedio · 🔴 avanzado.
2. **Un universo clínico coherente.** Todos los ejemplos ocurren en la misma "clínica de nefrología" simulada, con datos generados por scripts (`R/simular_*.R`) donde conocemos la verdad: así cada método se comprueba.
3. **Plantilla fija por subcapítulo:** ① pregunta clínica (🩺) → ② idea en palabras → ③ código paso a paso → ④ cómo interpretar y reportar → ⑤ errores frecuentes (⚠️) → ⑥ 🎯 *En resumen* → ⑦ ejercicio propuesto.
4. **Parsimonia:** una sola herramienta por problema en el texto principal; las alternativas van en cuadros 💡 "Alternativa".
5. **Código honesto:** sin ocultar complejidad. Funciones auxiliares en `R/funciones_epi.R` para no ensuciar el texto, pero siempre se muestra qué hacen.
6. **Todo se renderiza y se publica** (`docs/`) en cada entrega.

---

## 2. Estructura propuesta

Los **Capítulos 1-3 ya están escritos**. Propongo que funcionen como "visión de conjunto" (el Cap. 3.4 es el "juego completo": una vista panorámica de todo lo que sigue) y que de ahí en adelante se vaya de lo simple a lo complejo.

### Bloque A · Fundamentos (ya escrito)

| Cap. | Título | Subcapítulos | Estado |
|---|---|---|---|
| 1 | Pregunta de investigación | 1.1 Tipos de estudio · 1.2 Diseños · 1.3 Tareas científicas · 1.4 Descriptivas · 1.5 Predictivas · 1.6 Causales | ✅ revisado |
| 2 | Medidas en epidemiología | 2.1 Frecuencia · 2.2 Asociación · 2.3 Impacto · 2.4 ¿Qué es la asociación? | ✅ reescrito |
| 3 | Casualidad vs. causalidad | 3.1 ECA · 3.2 Resultados potenciales · 3.3 DAG y ajuste · 3.4 Análisis completo | ✅ reescrito |

### Bloque B · Herramientas y datos (🟢 → 🟡)

| Cap. | Título | Subcapítulos | Caso / datos | Paquetes |
|---|---|---|---|---|
| **4** | R y Quarto para el clínico | 4.1 🟢 Instalar y orientarse (RStudio, proyectos, `here`) · 4.2 🟢 Objetos, funciones y paquetes · 4.3 🟢 Importar datos (CSV, Excel, SPSS/Stata con `haven`) · 4.4 🟢 Quarto: informes reproducibles · 4.5 🟡 Cómo pedir ayuda y leer errores | `ckd_isglt2.csv` | `tidyverse`, `readxl`, `haven`, `here` |
| **5** | Limpieza y manejo de datos | 5.1 🟢 Datos ordenados (*tidy*) · 5.2 🟢 Seleccionar, filtrar, crear variables (`dplyr`) · 5.3 🟢 Fechas y tiempos (`lubridate`) · 5.4 🟡 Variables nefrológicas: **TFGe CKD-EPI 2021**, categorías G/A de KDIGO, unidades (mg/dL ↔ µmol/L) · 5.5 🟡 Control de calidad: duplicados, valores imposibles, diccionario de datos · 5.6 🟡 Unir tablas y datos largos/anchos (`pivot_*`, `joins`) | **Registro "sucio"** (`registro_nefro_sucio.csv`) con errores típicos | `tidyverse`, `lubridate` |
| **6** | Muestreo y población de estudio | 6.1 🟢 Población objetivo, marco y muestra · 6.2 🟢 Muestreo aleatorio simple, estratificado y por conglomerados · 6.3 🟡 Muestras complejas: **pesos y diseño** · 6.4 🟡 Sesgo de selección en la muestra · 6.5 🔴 Prevalencia de ERC en una encuesta por conglomerados | **Encuesta regional de ERC** (estratos × conglomerados) | `survey` |

### Bloque C · Describir y asociar (🟢 → 🟡)

| Cap. | Título | Subcapítulos | Caso / datos | Paquetes |
|---|---|---|---|---|
| **7** | Análisis descriptivo | 7.1 🟢 Variables y su distribución · 7.2 🟢 **Tabla 1** (función propia `tabla1()`) · 7.3 🟢 Gráficos que comunican (ggplot2) · 7.4 🟡 Intervalos de confianza y bootstrap · 7.5 🟡 Estandarización por edad (directa/indirecta) | `ckd_isglt2` | `tidyverse` |
| **8** | Análisis de asociación y regresión | 8.1 🟢 Dos variables: pruebas *t*, χ², Wilcoxon, correlación · 8.2 🟢 Regresión lineal · 8.3 🟡 Regresión logística y de Poisson (RR directo) · 8.4 🟡 Interacción y efecto modificador · 8.5 🟡 Supuestos, diagnóstico y no linealidad (splines) · 8.6 🔴 Cómo reportar una regresión (y la *Table 2 fallacy*) | `ckd_isglt2` | `broom`, `sandwich`, `splines`, `marginaleffects` |
| **9** *(nuevo)* | Análisis de supervivencia | 9.1 🟢 Tiempo hasta evento y censura · 9.2 🟢 Kaplan-Meier y log-rank · 9.3 🟡 Regresión de Cox · 9.4 🟡 Riesgos proporcionales: cómo verificar y qué hacer · 9.5 🔴 **Riesgos competitivos** (diálisis vs. muerte) · 9.6 🔴 Supervivencia media restringida (RMST) | `cohorte_pronostico` | `survival`, `survminer`, `cmprsk` |

### Bloque D · Causalidad aplicada (🟡 → 🔴)

| Cap. | Título | Subcapítulos | Caso / datos | Paquetes |
|---|---|---|---|---|
| **10** | ¡¡¡No todo es confusión!!! (sesgos) | 10.1 🟡 Sesgo de selección y colisionadores (restringir a hospitalizados) · 10.2 🟡 Sesgo de tiempo inmortal · 10.3 🟡 Sesgo de información y error de medición (TFGe, albuminuria) · 10.4 🟡 Causalidad inversa y sesgo de indicación · 10.5 🔴 Sesgo de supervivencia y de pérdidas · 10.6 🔴 Cómo cuantificar un sesgo (análisis de sesgo) | simulaciones breves | base R, `tidyverse` |
| **11** | Análisis inferencial en ensayos aleatorizados | 11.1 🟢 Diseño del análisis: preespecificar · 11.2 🟢 Comparación de dos grupos (ITT) · 11.3 🟡 **ANCOVA**: ajustar por el valor basal · 11.4 🟡 Mediciones repetidas (TFGe a 6/12/24 meses) · 11.5 🟡 Subgrupos y heterogeneidad (cuándo creerlos) · 11.6 🔴 No adherencia: *per-protocol*, CACE/IV · 11.7 🔴 Desenlaces compuestos y *win ratio* | **ECA simulado**: bicarbonato vs. placebo en ERC G3b-4 | `lme4`/`nlme`, `emmeans`, `survival` |
| **12** | Análisis inferencial en estudios observacionales | 12.1 🟡 **Ensayo objetivo** (emulación) paso a paso · 12.2 🟡 Regresión ajustada vs. estratificación · 12.3 🟡 Puntaje de propensión: modelo, solapamiento, recorte · 12.4 🟡 Emparejamiento (`MatchIt`) · 12.5 🟡 Ponderación (IPW, ATE/ATT/ATU; pesos estabilizados) · 12.6 🔴 Estandarización (fórmula g) y **doblemente robustos (AIPW)** · 12.7 🔴 Efectos causales con **desenlaces de supervivencia** (KM/Cox ponderados, riesgo a *t*) · 12.8 🔴 Análisis de sensibilidad: E-value, confusor simulado, controles negativos · 12.9 🔴 Comparador activo y *new-user design* | `ckd_isglt2` | `MatchIt`, `marginaleffects`, `survival`, `sandwich` |
| **13** *(nuevo)* | Datos faltantes | 13.1 🟢 Mecanismos MCAR/MAR/MNAR (con DAGs) · 13.2 🟢 Explorar el patrón de faltantes · 13.3 🟡 Análisis de casos completos: cuándo sirve · 13.4 🟡 **Imputación múltiple** (`mice`) · 13.5 🔴 Imputación en análisis causal y de supervivencia · 13.6 🔴 Análisis de sensibilidad (MNAR, *tipping point*) | `ckd_faltantes` (albuminuria perdida) | `mice`, `VIM` |

> *Nota:* "Datos faltantes" se adelanta respecto a tu lista original porque los Caps. 14-18 lo necesitan.

### Bloque E · Datos con estructura (🟡 → 🔴)

| Cap. | Título | Subcapítulos | Caso / datos | Paquetes |
|---|---|---|---|---|
| **14** | Datos longitudinales y multinivel | 14.1 🟡 Por qué la independencia no se cumple · 14.2 🟡 Efectos aleatorios: pacientes dentro de unidades de diálisis · 14.3 🟡 **Pendiente de TFGe** con modelos mixtos · 14.4 🔴 GEE vs. modelos mixtos (cuál elegir) · 14.5 🔴 Ensayos por conglomerados e ICC · 14.6 🔴 Trayectorias y *joint models* (visión general) | `hd_unidades` | `lme4`, `lmerTest`, `nlme`, `geepack`, `broom.mixed` |
| **15** | Series de tiempo | 15.1 🟢 Componentes: tendencia, estacionalidad · 15.2 🟡 Modelos para conteos (inicios de diálisis por mes) · 15.3 🟡 **Serie temporal interrumpida (ITS)**: evaluar una política · 15.4 🔴 Autocorrelación y estacionalidad en el modelo · 15.5 🔴 Serie con grupo control | `serie_inicio_dialisis` | `forecast`, `segmented`, `mgcv`, `sandwich` |

### Bloque F · Predicción (🟡 → 🔴)

| Cap. | Título | Subcapítulos | Caso / datos | Paquetes |
|---|---|---|---|---|
| **16** | Análisis predictivos: diagnóstico | 16.1 🟢 Sensibilidad, especificidad, VPP/VPN y prevalencia · 16.2 🟢 Curva ROC y AUC · 16.3 🟡 Punto de corte y *trade-offs* · 16.4 🟡 Modelo diagnóstico multivariable · 16.5 🟡 Calibración · 16.6 🔴 Curvas de decisión (beneficio neto) · 16.7 🔴 Sesgo de verificación, espectro y *spin* | **Biomarcador urinario para lesión renal aguda** | `pROC`, `rms` |
| **17** | Análisis predictivos: pronóstico | 17.1 🟡 Tamaño de muestra para un modelo (eventos por parámetro; criterios de Riley) · 17.2 🟡 Desarrollo (Cox / logística) tipo **KFRE** · 17.3 🟡 Rendimiento: discriminación (C), **calibración**, Brier · 17.4 🔴 Validación interna con *bootstrap* (optimismo) · 17.5 🔴 Validación externa y recalibración · 17.6 🔴 Aprendizaje automático (bosques, LASSO) vs. regresión · 17.7 🔴 Reporte: TRIPOD+AI, PROBAST | `cohorte_pronostico` + cohorte de validación | `rms`, `glmnet`, `ranger`, `riskRegression` |

### Bloque G · Temas avanzados de causalidad (🔴)

| Cap. | Título | Subcapítulos | Caso / datos | Paquetes |
|---|---|---|---|---|
| **18** | Análisis de mediación | 18.1 🟡 Efecto total, directo e indirecto · 18.2 🟡 Método de productos · 18.3 🔴 Supuestos (cuatro confusiones) · 18.4 🔴 Mediación con interacción y desenlaces binarios · 18.5 🔴 Interpretación clínica: ¿albuminuria como mediador del iSGLT2? | `ckd_isglt2` + mediador | base R, `boot`, `marginaleffects` |
| **19** *(nuevo)* | Tratamientos que cambian en el tiempo y diseños cuasi-experimentales | 19.1 🔴 Confusión dependiente del tiempo (EPO/hemoglobina en hemodiálisis) · 19.2 🔴 Modelos estructurales marginales · 19.3 🔴 Estimación g (visión general) · 19.4 🔴 Variables instrumentales (preferencia del médico) · 19.5 🔴 Diferencias en diferencias y regresión discontinua (umbral de TFGe) · 19.6 🔴 Heterogeneidad del efecto (quién se beneficia) | datos simulados | `survival`, `sandwich`, `AER`/`ivreg` si disponible |

### Bloque H · Planificar, sintetizar y comunicar (🟡 → 🔴)

| Cap. | Título | Subcapítulos | Caso / datos | Paquetes |
|---|---|---|---|---|
| **20** | Cálculo de tamaño muestral | 20.1 🟢 La lógica: efecto, variabilidad, α, potencia · 20.2 🟢 Diferencia de medias y de proporciones (`pwr`) · 20.3 🟡 Tiempo hasta evento (número de eventos, Schoenfeld) · 20.4 🟡 ECA por conglomerados (efecto de diseño) · 20.5 🟡 **Simulación** como herramienta de potencia · 20.6 🔴 Modelos de predicción y precisión (IC en vez de potencia) · 20.7 🔴 Ajustes por pérdidas y análisis interinos | ECA bicarbonato | `pwr`, `rpact`, base R |
| **21** | Revisiones sistemáticas y metaanálisis | 21.1 🟢 Protocolo y pregunta PICOS (PROSPERO, PRISMA) · 21.2 🟢 Búsqueda y selección · 21.3 🟡 Extracción y riesgo de sesgo (RoB 2 / ROBINS-I) · 21.4 🟡 **Metaanálisis** de efectos fijos y aleatorios · 21.5 🟡 Heterogeneidad (I², τ², intervalos de predicción) · 21.6 🔴 Metarregresión y sesgo de publicación · 21.7 🔴 Metaanálisis en red (visión general) | **Metaanálisis hipotético** de ensayos de iSGLT2 (datos ficticios, rotulados) | `metafor` |
| **22** | Certeza de la evidencia (GRADE) | 22.1 🟢 Qué es y por qué · 22.2 🟡 Los cinco dominios que bajan y los tres que suben · 22.3 🟡 Tabla *Summary of Findings* · 22.4 🔴 GRADE con estudios no aleatorizados (ROBINS-I) · 22.5 🔴 De la evidencia a la recomendación (EtD) | continúa el metaanálisis | — |
| **23** *(nuevo)* | Reporte, reproducibilidad y comunicación | 23.1 🟢 Qué guía de reporte usar (CONSORT, STROBE, TRIPOD+AI, PRISMA, RECORD, TARGET) · 23.2 🟢 Tablas y figuras publicables · 23.3 🟡 Proyecto reproducible (Quarto + `renv` + git) · 23.4 🟡 Plan de análisis estadístico (SAP) · 23.5 🔴 Preregistro y análisis exploratorio vs. confirmatorio · 23.6 🔴 Ética y privacidad de datos clínicos | todos | `renv`, `here` |

### Apéndices
A. Cuadros de resumen (ya existe, se amplía capítulo a capítulo) · B. Diccionario de datos de todas las bases simuladas · C. Plantillas de código por tipo de análisis · D. Glosario ES-EN · E. Soluciones a los ejercicios.

---

## 3. Temas que propongo agregar a tu lista original

| Tema nuevo | Por qué hace falta |
|---|---|
| **Análisis de supervivencia (Cap. 9)** | Casi todos los desenlaces nefrológicos son tiempo hasta evento (diálisis, muerte); los Caps. 11, 12 y 17 lo suponen. Incluye **riesgos competitivos**, muy relevantes en ERC |
| **Regresión como herramienta (Cap. 8)** | Es la base de casi todo el libro; se enseña antes de ECA/EO |
| **Sesgos (Cap. 10)** | Tu capítulo "No todo es confusión" ya existía; se amplía a selección, tiempo inmortal y error de medición |
| **Datos longitudinales (Cap. 14)** | La TFGe se mide repetidamente; pendientes y modelos mixtos son el análisis estándar |
| **Tratamientos variables en el tiempo y cuasi-experimentales (Cap. 19)** | Son los métodos "G" que mencionas en el prefacio; completan *What If* (parte III) |
| **Datos faltantes antes de lo complejo (Cap. 13)** | Los análisis posteriores (mixtos, predicción, causal) son inválidos si se ignoran |
| **Reporte y reproducibilidad (Cap. 23)** | Cierra el ciclo: de la pregunta a la publicación |
| **Curvas de decisión y aprendizaje automático (Caps. 16-17)** | Estándar actual en predicción clínica |

---

## 4. Orden de lectura y dependencias

```
1 Pregunta → 2 Medidas → 3 Causalidad (visión general)
        ↓
4 R → 5 Datos → 6 Muestreo → 7 Descriptivo → 8 Asociación/Regresión → 9 Supervivencia
        ↓
10 Sesgos → 11 ECA → 12 Observacional → 13 Faltantes
        ↓
14 Longitudinal/multinivel → 15 Series de tiempo
        ↓
16 Diagnóstico → 17 Pronóstico
        ↓
18 Mediación → 19 G-métodos y cuasi-experimentales
        ↓
20 Tamaño muestral → 21 RS/MA → 22 GRADE → 23 Reporte
```

Cada capítulo declara sus **prerrequisitos** y a qué capítulo **vuelve** (p. ej., el Cap. 12 retoma el 3.4 con más detalle).

---

## 5. Bases de datos simuladas (universo clínico único)

| Base | Contenido | Se usa en |
|---|---|---|
| `ckd_isglt2` ✅ | ERC + DM2, iSGLT2, pendiente de TFGe, evento renal | 2, 3, 7, 8, 12, 13, 18 |
| `registro_nefro_sucio` | Registro con errores (nombres, unidades, fechas, duplicados) | 4, 5 |
| `encuesta_erc` | Muestreo por estratos y conglomerados con pesos | 6 |
| `cohorte_pronostico` | ERC G3-5, tiempo hasta diálisis con muerte como riesgo competitivo; segunda cohorte de validación | 9, 17 |
| `eca_bicarbonato` | ECA de bicarbonato en ERC con acidosis: TFGe a 0/6/12/24 meses, adherencia y abandonos | 11, 14, 20 |
| `hd_unidades` | Pacientes anidados en unidades de diálisis | 14 |
| `serie_inicio_dialisis` | Inicios mensuales de diálisis 2015-2024, cambio de política en 2020 | 15 |
| `dx_lra` | Biomarcador urinario y lesión renal aguda (estándar de referencia) | 16 |
| `ckd_faltantes` | `ckd_isglt2` con albuminuria y HbA1c faltantes (MAR) | 13 |
| `meta_isglt2` | Efectos por ensayo (**ficticios**, rotulados como tales) | 21, 22 |

Cada base tiene un script `R/simular_<nombre>.R` con **la verdad conocida** documentada.

---

## 6. Lo que **no** puedo ejecutar aquí (y cómo lo trataré)

CRAN y GitHub están bloqueados en este entorno, así que solo puedo verificar el código con paquetes disponibles por apt. Plan:

| Paquete | Disponible aquí | Tratamiento |
|---|---|---|
| `survival`, `survminer`, `cmprsk`, `mice`, `survey`, `lme4`, `nlme`, `geepack`, `pROC`, `rms`, `metafor`, `forecast`, `segmented`, `glmnet`, `ranger`, `pwr`, `rpact`, `haven`, `readxl`, `lubridate`, `MatchIt`, `marginaleffects`, `emmeans`, `riskRegression` | ✅ | Ejecutados y verificados antes de publicar |
| `gtsummary`, `tableone`, `janitor`, `meta`, `mediation`, `cobalt`, `WeightIt`, `dagitty`, `ggdag`, `tsibble`, `ggsurvfit`, `dcurves`, `naniar` | ❌ | Se usan **alternativas propias** verificadas (p. ej., `tabla1()`, `smd()`, mediación por productos) y se muestra el paquete "estándar" en un cuadro 💡 como alternativa, claramente marcado *no ejecutado* |

---

## 7. Tus materiales de Drive: qué contienen y dónde encajan

Inventario de las dos carpetas compartidas (`Epidemiología` y `Estadística/Programa de formación`). Los `.Rmd` solo se pueden descargar como base64 desde el conector de Drive, así que **no puedo leer su código**; sí puedo leer PDF/PPTX/DOCX. Para integrar tu código, **sube los `.R`, `.Rmd`, `.qmd` y las bases `.csv` pequeñas al repo** (carpeta `mis_codigos/`).

| Tu carpeta | Capítulo del libro |
|---|---|
| Epi: Semana 1 tipo_estudios · 2 medidas_frecuencia · 3 medidas_asociación · 4 confusión/sesgo/missclassification (E-value, episensr) · 5 epidemiología básica con R (estandarización, linelist) · 6 descripción/predicción/causalidad · 7-10 causalidad, DAG, colisionador (`DAG ejemplo.qmd`, `collier.Rmd`) · 24 "No solo es confusión" | Caps. 1, 2, 3, 10 |
| Estadística: Semana 1 introR · 2 limpieza_manejo · 3 análisis desc./bivariado | Caps. 4, 5, 7, 8.1 |
| Semana 6 reg_lineal · 7 reg_logistic · 8 reg_poisson · 9 reg_resumen · 10 resumen · 20 Select_step | Cap. 8 (y selección de variables en 8.5 / 17) |
| Semana 11 sample_size · 12 svy_ENDES | Caps. 20 y 6 |
| Semana 13 Multilevel · 14 logistic_cond_GEE · 15 Series_temp_Geoespa | Caps. 14 y 15 (+ regresión logística condicional, 8.3) |
| Semana 17 ECA · 18 ECNA · 19 métodos de ajuste de confusión · 22 Matching_PS_IPW · 23 Métodos_G · 24 Confusión no observada | Caps. 11, 12, 19 |
| Semana 27 interacción · 28 mediación | Caps. 8.4 y 18 |
| `prediction models` · `Supervivencia` · `Target Trial Emulation` (+ zip `steroids-trial-emulation`) | Caps. 16-17, 9, 12.1 |

**Temas adicionales que tus materiales sugieren y que incorporaré:** regresión logística condicional (casos y controles pareados), selección de variables paso a paso (y por qué desaconsejarla), análisis geoespacial (como subcapítulo opcional del 15), encuestas complejas con diseño (ENDES: se simulará una equivalente) y ensayos clínicos **no** aleatorizados (ECNA).

---

## 8. Decisiones (respondidas por ti)

Orden propuesto: ✅ aprobado · Temas nuevos: ✅ incluidos · Publicación: solo GitHub Pages (`docs/`, reemplazando la carpeta anterior en cada render con `./publicar.sh`) · Extensión: capítulos tan largos como haga falta.

---

## 9. Decisiones previas (histórico)

1. **Orden.** ¿Apruebas el orden propuesto (R y datos *antes* de los análisis de ECA/EO), o prefieres el de tu índice original (R en el puesto 6)? Esto implica renumerar los archivos `Capítulo4…20.qmd` (hoy son placeholders vacíos).
2. **Temas nuevos.** ¿Incluyo todos los de §3 (9, 10 ampliado, 13 adelantado, 14 longitudinal, 19, 23)?
3. **Acceso a tus códigos de Dropbox** (ver arriba).
4. **Publicación.** El sitio de GitHub Pages se actualiza desde la rama que tengas configurada (normalmente `main`, carpeta `docs/`). Mi trabajo está en la rama `claude/causal-inference-nephrology-book-3u7l8h`: ¿abro un PR a `main` en cada entrega, o prefieres que la rama de Pages sea esta?
5. **Profundidad.** ¿Cada capítulo con ~6-9 subcapítulos como arriba (≈ 30-45 min de lectura), o prefieres versiones más cortas?
