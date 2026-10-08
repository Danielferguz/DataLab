# Mapa de código y materiales externos → capítulos de DataLab

Qué hay en cada repositorio de referencia, en qué capítulo del libro conviene aprovecharlo y qué cambiaría. **Nada se copia tal cual**: se adapta a los datos simulados de nefrología y se cita la fuente.

## 0. Licencias (importante antes de reutilizar)

| Fuente | Licencia | Qué implica |
|---|---|---|
| EpiMethods, TMLE workshop, Intro2ML (E. Karim) | **CC0 1.0** (dominio público) | Se puede adaptar libremente. Se cita igualmente. |
| *Causal Inference in R* (r-causal) | **CC BY-NC-SA 4.0** | Adaptar texto o código obliga a atribuir, no comercial y **compartir igual**. Mientras el libro no tenga esa licencia, usar **ideas y estructura** y escribir código propio. |
| `kathoffman/steroids-trial-emulation` | **sin archivo de licencia** | No copiar código. Reimplementar el flujo con datos propios y citar el artículo (Hoffman et al., *JAMA Netw Open*, 2022). |
| Andrew Heiss («Códigos imprescindibles quarto») | no consultada | **Pendiente**: la carpeta no está en tu repositorio ni en tu Drive (ver §5). |

## 1. Qué hay en cada repositorio

-   **EpiMethods** (`ehsanx/EpiMethods`): ~16 bloques temáticos en Quarto. Casi todos con datos reales (RHC, NHANES, CCHS), simulaciones con `simcausal` y ejercicios. Los datos no se reutilizan; el **esquema** y los **pasos** sí.
-   **TMLE workshop** (`ehsanx/TMLEworkshop`): RHC → regresión → G-computación → IPTW → TMLE **paso a paso a mano** → paquetes (`tmle`, `ltmle`, `AIPW`, `sl3`). El archivo `4tmle.Rmd` construye el TMLE en pasos con `glm()` (el super learner entra después), así que el esquema se puede reimplementar con R base.
-   **Intro2ML** (`ehsanx/into2ML`): predicción con R (continuo y binario), partición, validación cruzada, LASSO/ridge/elástica, árbol, k-means, y reflexiones (desarrollo, validación, implicación clínica, lectura crítica).
-   **Causal Inference in R** (`r-causal`): 24 capítulos, de resultados potenciales a evidencia. Usa `ggdag`, `dagitty`, `propensity`, `halfmoon`, `tipr`, `touringplans` y `causalworkshop` (datos), entre otros.
-   **steroids-trial-emulation** (K. Hoffman): emulación de ensayo con **tratamiento variable en el tiempo** y desenlace de supervivencia (paquete `lmtp`, super learner `sl3`), con datos de juguete y gráficos (línea de tiempo del tratamiento, *forest plot*).

## 2. Mapa por capítulo del libro

Leyenda de esfuerzo: 🟢 pequeño ajuste · 🟡 subcapítulo nuevo o ampliación grande · 🔴 trabajo mayor / depende de paquetes no instalables aquí.

| Cap. del libro | Material externo | Qué se puede mejorar o añadir | Esfuerzo |
|---|---|---|---|
| **1.3-1.6** Pregunta de investigación | EpiMethods `researchquestion1-4` (pregunta predictiva y causal, PICOT con artículo ejemplo) | Plantilla «pregunta → PICOT → variables → base analítica» con un artículo de nefrología. | 🟢 |
| **3.2** Resultados potenciales | r-causal cap. 3 y 10; TMLE workshop `2gcomp` (G-computación como **problema de datos faltantes**) | ✅ Hecho: «El juego entero» (Pasos 9-15). Falta (opcional) la vista «tabla de 8 pacientes con datos faltantes de los contrafactuales» de `2gcomp`. | 🟢 |
| **3.3** DAG | r-causal cap. 4-5 (*quartets*); EpiMethods `confounding2-6` (mediador, colisionador, sesgo Z, colapsabilidad, cambio en la estimación) | Reemplazar `simcausal` por simulaciones propias con la misma lógica: demuestran **qué pasa al ajustar por cada tipo de nodo**. | 🟡 |
| **3.4** Confusión no medida | r-causal cap. 16 (`tipr`) | Añadir la tabla «cuán fuerte tendría que ser U». Parte ya cubierta en 3.2 (Paso 14). | 🟢 |
| **4-5** R, Quarto y limpieza | EpiMethods `wrangling1-6`, `accessing1-8` (importar SAS/Stata, `nhanesA`) | Ejemplos de importación SAS/SPSS/Stata con `haven` (ya en 4.3); sin datos NHANES. | 🟢 |
| **6** Muestreo | EpiMethods `surveydata0-9` (pesos, subpoblaciones, `svyTable1`) | **Subpoblaciones**: `subset()` sobre el diseño, no sobre los datos (error clásico). Verificar que 6.3 lo diga. | 🟡 |
| **7** Descriptivo | EpiMethods `exploring0-2`; `predictivefactors7` (bootstrap) | Tabla 1 con `tableone`/`gtsummary` condicional (ya en 7.2); bootstrap explicado a mano (7.4). | 🟢 |
| **8** Regresión | EpiMethods `nonbinary1` (multinomial/ordinal), `nonbinary3` (Poisson, binomial negativa), `predictivefactors1` (colinealidad), `confounding8-9` (*Table 2 fallacy*, variable conjunta vs. interacción) | Añadir a 8.3: **regresión ordinal/multinomial** (categoría G o A de KDIGO como desenlace) y **binomial negativa** con sobredispersión. | 🟡 |
| **9** Supervivencia | EpiMethods `nonbinary2/2a` (tipos de censura, KM, Cox en NHANES) | Gráfico de tipos de censura (izquierda, derecha, intervalo) en 9.1. | 🟢 |
| **10** Sesgos | EpiMethods `confounding3-4` (colisionador, sesgo Z, `confounding3b`) | Versión simulada propia del **sesgo Z** (ajustar por un instrumento aumenta el sesgo): no está en el plan. | 🟡 |
| **12.1** Ensayo objetivo | `steroids-trial-emulation` (tabla de emulación; elegibilidad, tiempo cero) | La tabla de emulación del 3.2 ya existe; ampliar con el ejemplo del esteroide (tiempo cero = hipoxia). | 🟢 |
| **12.3-12.5** PS, emparejamiento, ponderación | r-causal cap. 8-9, 11 (`halfmoon`, `cobalt`); EpiMethods `propensityscore1-9`; TMLE `3ipw` | Estandarizar el flujo en **cuatro pasos** de TMLE workshop (exposición → PS → balance → desenlace); histograma espejo y *love plot* ya están. | 🟢 |
| **12.x** PS con encuestas, imputación múltiple y exposiciones con más de 2 niveles | EpiMethods `propensityscore3-9` (Zanutto 2006, DuGoff 2014, `survey`, `mice`, `VGAM`) | **Nuevo 12.10**: PS con pesos muestrales; PS + imputación múltiple (dentro vs. promediando); exposición de 3 niveles (p. ej., iSGLT2, aGLP1, ninguno). | 🟡 |
| **12.6** Fórmula g y doblemente robustos | TMLE `2gcomp`, `2gcomp2`, `4tmle`, `5software` | Paso a paso de **G-comp con árbol/LASSO**; **TMLE manual** (9 pasos con `glm`); comparar con `tmle`/`AIPW` condicional. | 🟡 |
| **12.7** Supervivencia causal | r-causal cap. 19; Hoffman (`lmtp`) | Riesgo ponderado a *t* con Kaplan-Meier ponderado y fórmula g para desenlaces de tiempo. | 🟡 |
| **12.8** Sensibilidad | r-causal cap. 16 (`tipr`) | Complementa el Paso 14 de 3.2: tabla de confusor simulado y controles negativos. | 🟡 |
| **12.10 / nuevo** Aprendizaje automático causal | EpiMethods `machinelearningCausal1-5,11,12`; TMLE `2gcomp2`, `3ipw2`, `4tmle`; r-causal cap. 21 | **Nuevo subcapítulo**: PS con *random forest*/LASSO (`ranger`, `glmnet`, `gbm` sí disponibles), validación cruzada, *cross-fitting* y TMLE manual. `SuperLearner`/`tmle`/`sl3` solo con el patrón condicional. | 🔴 |
| **13** Datos faltantes | EpiMethods `missingdata0-7, 11-12` (faltante en desenlace, subpoblaciones, MCAR, efecto modificador, imputación y diseño muestral); r-causal cap. 15 | Añadir: **faltantes en exposición y desenlace**, prueba de MCAR de Little (visión), imputar dentro de una subpoblación. | 🟡 |
| **14** Longitudinal y multinivel | EpiMethods `longitudinal1-2` (modelos mixtos y GEE) | Ya cubierto con `hd_unidades`; comparar estructuras de correlación (independiente, intercambiable, AR1) en 14.4. | 🟢 |
| **16-17** Predicción | EpiMethods `predictivefactors1-7`, `machinelearning1-10`; Intro2ML `02-11` | Ya hay AUC, Brier, calibración, bootstrap. Añadir: **partición vs. validación cruzada vs. bootstrap** en una sola figura; **LASSO/ridge/elástica y CART** comparados; lectura crítica de modelos de ML (guías). | 🟡 |
| **17 (extra)** No supervisado | Intro2ML `07`; EpiMethods `machinelearning6` | K-means para **fenotipos de ERC** (opcional, no está en el plan). | 🔴 |
| **18** Mediación | EpiMethods `mediation1-4` (Baron-Kenny, enfoque ponderado, `regmedint`); r-causal cap. 17 | Comparar productos, ponderación y `regmedint`; mantener las advertencias sobre Baron-Kenny. | 🟡 |
| **19** Tratamientos en el tiempo, IV, DiD | r-causal cap. 14, 18, 22, 23; `steroids-trial-emulation` (regímenes dinámicos, `lmtp`) | Estructura del cap. 19: formato ancho con un indicador por tiempo; **regímenes dinámicos** («si hipoxia, esteroide 6 días»); *forest plot* y línea de tiempo del tratamiento. | 🔴 |
| **20** Tamaño muestral | EpiMethods `Simulation*.qmd` | Usar el esquema de simulación para la potencia (20.5). | 🟡 |
| **23** Reporte y reproducibilidad | EpiMethods `reporting1-2` (Git, GitHub, plantilla de libro Quarto) | Sección Git/GitHub paso a paso (ya la hiciste con tu cuenta). | 🟢 |

## 3. Propuestas de subcapítulos nuevos (a confirmar)

1.  **12.10 PS con encuestas, imputación múltiple y exposiciones de más de dos niveles** (EpiMethods `propensityscore3-9`).
2.  **12.11 Aprendizaje automático y TMLE** (EpiMethods `machinelearningCausal`, TMLE workshop). Es el tema que más aporta a un lector que ya sabe IPW y fórmula g.
3.  **8.7 Regresión ordinal, multinomial y binomial negativa** (EpiMethods `nonbinary1,3`).
4.  **3.3 bis. Simulaciones de DAG** (mediador, colisionador, sesgo Z): demostraciones en un solo gráfico.

## 4. Paquetes de esas fuentes que **no se pueden instalar aquí**

`ggdag`, `dagitty`, `halfmoon`, `propensity`, `tipr`, `WeightIt`, `cobalt`, `tableone`, `gtsummary`, `SuperLearner`, `tmle`, `ltmle`, `lmtp`, `sl3`, `AIPW`, `regmedint`, `simcausal`. Se usan con el patrón de evaluación condicional de `GUIA_ESTILO.md` §3 bis, **siempre** junto a una versión propia ejecutable.

## 5. Pendiente de tu parte

-   **Andrew Heiss / «Códigos imprescindibles quarto»**: no está en `danielferguz/datalab` (ni en `main` ni en la rama), ni en tu Drive. Dime la URL del repositorio o súbelo a `Mis códigos/` y lo integro en el Capítulo 3.2. Sus datos no se reutilizarían: se adaptaría el **flujo de trabajo** al caso de nefrología.
-   Confirmar si quieres los **subcapítulos nuevos** de §3.
