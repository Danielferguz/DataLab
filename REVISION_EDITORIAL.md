# Revisión editorial: DataLab for causal inference

Revisión de texto y código para convertir el repositorio en un libro de inferencia causal **fácil de entender para un residente de nefrología**, con ejemplos de la especialidad, código reutilizable y cuadros de resumen. Referencias de contenido: *Causal Inference: What If* (Hernán y Robins) y <https://www.r-causal.org/>.

Fecha de la revisión: 2026-10-04 · Rama: `claude/causal-inference-nephrology-book-3u7l8h`

---

## 1. Resumen ejecutivo

| Área | Estado inicial | Qué se hizo |
|---|---|---|
| **Compilación** | El libro **no compilaba** (Cap. 2.1 falla con `ggplot2` actual; `_quarto.yml` apuntaba a `capitulo1.qmd` en minúscula; los Caps. 3.2-3.4 requerían ≥ 15 paquetes no declarados y uno solo de GitHub) | Libro completo compila (`quarto render`, ~2 min). Dependencias reducidas y declaradas en `capitulos/intro.qmd` |
| **Ejemplos** | Astronautas en Marte, mosquiteros y malaria, campañas políticas | Un único caso de estudio nefrológico y coherente en toda la Parte 2 y 3: **iSGLT2 en ERC con diabetes** (datos simulados) |
| **Código** | Casi todo `echo = FALSE` (el lector no veía el código), números escritos a mano en el texto | Código visible y copiable; **todas las cifras del texto se calculan con código en línea** |
| **Errores conceptuales** | Varios (ver §3) | Corregidos en los capítulos reescritos; lista de pendientes en §6 |
| **Cuadros de resumen / tips** | Pocos | Convención de 4 cuadros en todos los capítulos (§5) + Apéndice con cuadros de consulta rápida |

**Rehecho por completo:** 2.1, 2.2, 2.3, 2.4, 3.2, 3.3, 3.4, introducciones de las Partes 2 y 3, `intro.qmd`, prefacio y apéndice.
**Editado:** Capítulos 1, 1.1-1.6 y 3.1 (correcciones, ejemplos de nefrología, cuadros, código en 3.1).
**No tocado:** capítulos en preparación (4-20, 3.5-3.10): ver §7.

---

## 2. Cómo reproducir

```bash
# 1. Paquetes (una vez). dagitty es opcional para renderizar: sin él se omiten solo sus bloques
Rscript -e 'install.packages(c("tidyverse","broom","survival","survminer","MatchIt","marginaleffects","patchwork","DT","dagitty"))'

# 2. Renderizar el libro (genera docs/)
quarto render
```

* Datos simulados: `capitulos/Bases/ckd_isglt2.csv` (observados) y `ckd_isglt2_oraculo.csv` (con resultados potenciales y la variable no medida).
* Generador: `capitulos/R/simular_ckd.R` (semilla fija; ver los comentarios del encabezado para el diccionario de variables).
* Funciones de apoyo: `capitulos/R/funciones_epi.R` (`medidas_2x2`, `nnt_desde_rd`, `smd`, `e_value`, `dibujar_dag`).
* **`docs/` no se regeneró.** Contiene el render antiguo; vuelve a ejecutar `quarto render` en tu equipo antes de publicar (en el entorno de revisión no estaban instalados `downlit`/`xml2`, así que los enlaces automáticos de código no se habrían generado).

### Qué se verificó y qué no

| Verificado ejecutando el código | **No pude ejecutar** |
|---|---|
| Libro completo renderizado con Quarto 1.10 / R 4.3.3. Todos los bloques de R de los Caps. 2.x, 3.1-3.4 producen salida; valores del texto comprobados contra la salida | Bloques con **`{dagitty}`** (Cap. 3.3: `paths`, `adjustmentSets`): CRAN está bloqueado por la política de red del entorno. Están protegidos con `eval: !expr tiene_dagitty`: se saltan si el paquete falta y se ejecutan si está. **Ejecútalos una vez en tu equipo** y confirma que el conjunto de ajuste incluye edad, ECV, fragilidad, HbA1c, TFGe basal y albuminuria; y que sin fragilidad no devuelve ninguno |
| Las "verdades" simuladas (ATE, ATT, ATU, RR) coinciden con lo que recuperan IPW y fórmula g | Contraste de las **referencias bibliográficas y datos de artículos** con las fuentes originales (ver §6) |

---

## 3. Hallazgos de contenido (errores del texto original)

### Parte 1 (Caps. 1.x)

| Capítulo | Hallazgo | Estado |
|---|---|---|
| 1.2 | "Estudios de incidencia… pueden estimarse en estudios transversales": **incorrecto**; una encuesta transversal única no estima incidencia | Corregido |
| 1.2 | Casos y controles "no mide incidencia ni prevalencia: solo permite calcular el OR como estimador del RR": incompleto (el OR estima el RR/razón de tasas según el muestreo) | Corregido |
| 1.2 | Typos: *trasversal, siguimiento, se puede consideran, Muchos fuentes* | Corregido |
| 1.1 | *Persepectiva, calcular alguna medidas*; AMSTAR-II (nombre correcto: AMSTAR-2); ejemplo de metaanálisis con cifras inventadas ("25 estudios") | Corregido (ejemplo hipotético sin cifras) |
| 1.3 | Callouts en inglés ("Warning!"), "calculas" | Corregido |
| 1.6 | Ejemplos de Boston (mascarillas, ≈12 000 casos) y Netflix (controles sintéticos) **sin referencia** | **Pendiente** de citar (ver §6) |

### Capítulo 3.1 (ECA)

| Hallazgo | Estado |
|---|---|
| "ITT protege contra sesgos de selección **y adherencia**": falso. ITT no corrige la mala adherencia (diluye el efecto). Añadido recuadro | Corregido |
| "mITT = excluir pérdidas ajenas a la intervención": definición errónea y peligrosa; pérdidas de seguimiento no se resuelven excluyendo. Reescrito (no adherencia vs. pérdida vs. mITT) | Corregido |
| "ITT … evita error tipo I" sin matiz; en **no inferioridad** el ITT favorece al tratamiento nuevo | Matizado |
| Ejemplo DEFENDER: se retiró la afirmación de "sobres sellados y numerados" (no verificable) | Ver §6 |
| El capítulo no tenía código | Añadida sección práctica (aleatorización simple/bloques/estratificada; simulación ITT vs. PP) |

### Parte 2 (medidas)

| Capítulo | Hallazgo | Estado |
|---|---|---|
| **2.1** | **El código falla** con `ggplot2` actual: `geom_segment(aes(x = 6, yend = "P1"))` sin `xend` (pretendía una línea vertical) | Rehecho |
| 2.1 | El mismo gráfico se copiaba 4 veces con datos distintos; el texto hablaba de "meses" con ejes en años; los números del texto (n = 9, 4 eventos/43 persona-años) no coincidían con la figura; el IAM por "astronautas" | Rehecho con cohorte clara; cifras calculadas |
| **2.2** | "La OR es la razón entre las odds **entre los casos (grupo expuesto) y los controles (grupo no expuesto)**": confunde la definición | Corregido |
| 2.2 | RTI = 3/21 vs 1/22 no concordaba con la figura; interpretaciones "RR = 0.84", "RP = 0.64" sobrantes sin dato asociado | Rehecho con datos y código |
| 2.2 | "Hazard = probabilidad de evento en un instante": es una **tasa**, no una probabilidad | Corregido |
| 2.2 | Título "Inferencia clásica vs. bayesiana" pero **no hay contenido bayesiano** | Retitulado; marcado como pendiente |
| **2.3** | Capítulo de *impacto* decía "las principales medidas de **asociación** son…" | Corregido |
| 2.3 | Ejemplo de FER: "RR = 2.5 de **hipertensión** en tratados con **IECA**", incoherente; remitía a "uno de los RA fue −5 %" (no existía) | Rehecho |
| 2.3 | "Solo tiene sentido interpretar RA, NNT y FER cuando el RR es **significativo**": incorrecto (se interpreta el IC) | Corregido |
| 2.3 | Faltaba la dependencia del NNT con el riesgo basal y el horizonte; faltaba la fracción atribuible poblacional | Añadido |

### Parte 3 (causalidad)

| Capítulo | Hallazgo | Estado |
|---|---|---|
| 3.2 | "ATE = ATT + sesgo de selección": descomposición mal planteada (la correcta es *diferencia observada = ATT + sesgo de selección*) | Corregido |
| 3.2 | Intercambiabilidad descrita como "el ATT debería ser igual al ATU e igual al ATE": falso (eso es homogeneidad del efecto) | Corregido |
| 3.2 | *Diferencias en diferencias* "sin depender de un grupo de control" (**falso**: usa uno); *regresión discontinua* "identifica el ATE" (es un efecto **local**); IV "ATE para quienes cumplen" (es el **LATE**) | Corregido |
| 3.2 | Cifras escritas a mano (−14.7, −17.6, −11.7, "ATT −16.29") que no se calculaban y no coincidían entre sí ni con el capítulo 3.3 (que usaba otro dataset con efecto verdadero −10) | Una sola base; todas las cifras calculadas |
| 3.2 | Chunks con `echo = FALSE` + opciones `#|`: el lector no veía el código; dependencias no declaradas (`gt`, `ggtext`, `showtext`, `MoMAColors`, `ggokabeito`, `WeightIt`, fuente Jost); escribe un CSV dentro del capítulo | Rehecho con `tidyverse` + `marginaleffects` |
| 3.2 | Tono informal ("Twitter", "láser antimosquitos de Bill Gates", "multiverso de la locura") ajeno a un libro para residentes | Reescrito |
| 3.3 | Referencias cruzadas de **bookdown** (`\@ref(fig:...)`) en un proyecto **Quarto**: se muestran como texto literal | Eliminadas |
| 3.3 | Inglés residual (títulos de figuras, "More simply, we can collapse…"); LaTeX roto (`\$\text{Salud} \La salud…`) | Corregido |
| 3.3 | "Ahora podemos usar lenguaje causal" tras ajustar: **sobreafirmación** (solo si el DAG es correcto y no hay confusión no medida) | Corregido |
| 3.3 | IC de `lm(..., weights = ipw)` presentados como válidos: **no lo son** (ignoran la estimación del PS) | Explicado; bootstrap en 3.4 |
| 3.3 | Dos datasets distintos con efecto verdadero −15 (3.2) y −10 (3.3-3.4) | Unificado |
| 3.4 | **Error de traducción**: *net* (mosquitero) traducido como "Internet" y "neto" ("el uso de Internet se ve afectado por los ingresos…", "programa neto gratuito"); "EAT" por ATE; párrafo duplicado; "Figura 2.x" y "Sección 3.3" heredados del libro original | Reescrito |
| 3.4 | Requiere `causalworkshop` (solo GitHub), `propensity`, `halfmoon`, `tipr`, `rsample` | Reescrito con funciones propias (`smd`, `e_value`, bootstrap en R base) |

---

## 4. Hallazgos de estructura y código del repositorio

| Archivo | Problema | Estado |
|---|---|---|
| `_quarto.yml` | `part: capitulos/capitulo1.qmd` (el archivo es `Capítulo1.qmd`; falla en Linux/macOS) | Corregido |
| `_quarto.yml` | 17 capítulos "En redacción"/"Texto" en el índice | Comentados (se reactivan al completarlos) |
| `_quarto.yml` | Sin `lang: es`; numeración global de secciones ("Capítulo 3" contenía secciones 11-14) | `lang: es`, `number-sections: false` |
| `capitulos/Capítulo - ejemplo de código.qmd` | `setwd("C:/Users/danfe/Dropbox/…")` con ruta personal absoluta; no está en el índice | **Pendiente**: usar rutas relativas / proyecto RStudio |
| `capitulos/Capítulo COPIA -  COPIA.qmd`, `3.5` a `3.10` | Archivos idénticos de 271 bytes (plantilla) | Pendiente de decidir (¿borrar?) |
| `_common.R` | Código de *R for Data Science* sin uso; `status()` dice "second edition of R for Data Science" | Pendiente: eliminar o adaptar |
| `DESCRIPTION`, `.travis.yml`, `package.json`, `index.tex`, `index.log`, `exception.log` | Restos de plantilla *bookdown* / Travis / Node; `DESCRIPTION` importa `bookdown` | Pendiente: eliminar |
| `docs/` (32 MB) | Render commiteado: diffs enormes y desactualizado | Pendiente: publicar con GitHub Actions o regenerar |
| `references.bib` | Solo contiene la cita de ejemplo (Knuth); referencias escritas a mano en cada capítulo | Pendiente: migrar a BibTeX/`@cita` |
| `fig/` | ≈ 45 imágenes **huérfanas** (ya no se usan): ejemplos de ciencia política, mosquiteros y capturas de salida de R; incluye 2 figuras de tus artículos (`Articulo_cap2_2.png`, `Articulo_cap2_3.png`) que ya no se citan | Pendiente de decidir; ver §6 (licencias) |
| `Bases/mosquito_nets*.csv`, `Bases/edu_age.csv`, `capitulos/mosquito_nets_v2.csv` | Ya no se usan (este último lo generaba un chunk antiguo dentro de `capitulos/`) | Pendiente de decidir |

---

## 5. Convención de cuadros (para todos los capítulos)

| Cuadro | Quarto | Uso |
|---|---|---|
| 🎯 **En resumen / Para llevarte** | `callout-important` | Tabla o lista con lo esencial de la sección |
| 💡 **Tip práctico** | `callout-tip` | Atajos, código listo para copiar, buenas prácticas |
| ⚠️ **Cuidado** | `callout-warning` | Errores frecuentes, límites de un método |
| 🩺 **En la consulta de nefrología / Ejemplo** | `callout-note` | Viñetas clínicas |

Todas las cifras del texto deben salir de código en línea (`` `r ...` ``); los ejemplos hipotéticos se rotulan *(hipotético)*; el estilo de decimales es **punto** (consistente con las salidas de R).

---

## 6. Pendientes que requieren tu decisión o verificación

1. **Derechos de autor y licencias.** Los Caps. 3.2-3.4 originales eran traducciones muy cercanas a materiales de terceros (curso de A. Heiss; capítulos de *Causal Inference in R*) y las imágenes `fig/` incluían figuras de esas fuentes y capturas de diapositivas (`rothman.jpg`, `DAG.png`, `dr-cheese.png`). El aviso del prefacio ("no está destinado a la venta… *fair use*") **no equivale a una licencia**. Los capítulos reescritos usan un ejemplo propio y citan la estructura, pero **te recomiendo revisar la licencia de cada fuente** y decidir qué imágenes huérfanas conservar. Las imágenes de tus propios artículos (`Articulo_cap*.png`) no tienen este problema.
2. **Datos a contrastar con la fuente original:**
   * Ejemplo **DEFENDER** (Cap. 3.1): n = 507, 22 UCI, resultado "no significativo", resultado jerárquico: revisar con el artículo (JAMA 2024).
   * **Boston/mascarillas** y **Netflix** (Cap. 1.6): añadir la referencia.
   * Figuras `Articulo_cap1_*`, `Articulo_cap3_*` (imágenes): no pude validar su contenido interno (p. ej., las cifras del ejemplo "coreimumab" del 3.1 están dentro de la imagen).
3. **Declaración de uso de IA.** Los capítulos tenían "Esta sección fue editada usando ChatGPT". Lo sustituí por una **nota editorial** que añade esta revisión (ChatGPT; revisión con Claude) y añadí una declaración general al prefacio. Ajusta el texto según prefieras.
4. **Sección bayesiana** (Cap. 2.2): anunciada y no escrita; aparece como pendiente.
5. **Confirmar con un nefrólogo/a clínico/a** el realismo de los parámetros simulados (`R/simular_ckd.R`): efecto medio ≈ 0.9 mL/min/1.73 m²/año, evento renal ≈ 15 % a 36 meses, etc. Son **didácticos**, no estimaciones epidemiológicas.

---

## 7. Hoja de ruta propuesta para los capítulos en preparación

Mapa sugerido (en el orden de *What If* y *Causal Inference in R*) para completar el libro, siempre con el mismo caso de estudio y los mismos cuadros:

| Nuevo capítulo | Contenido | Ejemplo en nefrología | Referencia |
|---|---|---|---|
| Muestreo y tamaño de muestra | Cálculo de tamaño muestral, poder; muestreo en registros | Tamaño de muestra para un ECA de proteinuria | *What If* (cap. 10) |
| Datos faltantes | Mecanismos MCAR/MAR/MNAR; imputación múltiple | Albuminuria no medida en 30 % | `mice` |
| Regresión y confusión | Modelos de desenlace; Table 2 fallacy | Efecto de PA sistólica sobre TFGe | *What If* (cap. 15) |
| Puntaje de propensión en profundidad | Estratificación, emparejamiento, recorte, pesos estabilizados | iSGLT2 vs. iDPP4 (comparador activo) | *What If* (cap. 15); r-causal |
| Fórmula g y modelos doblemente robustos | Estandarización, AIPW, TMLE | Pendiente de TFGe a 1 año | *What If* (cap. 13, 21) |
| Análisis de supervivencia causal | Kaplan-Meier ponderado, Cox ponderado, riesgos competitivos (muerte y diálisis) | Tiempo a diálisis | *What If* (cap. 17) |
| Variables que cambian en el tiempo | Confusión dependiente del tiempo, MSM, estimación g | Anemia/ESA y mortalidad en hemodiálisis | *What If* (parte III) |
| Mediación | Efectos directo e indirecto | iSGLT2 → albuminuria → TFGe | r-causal |
| Variables instrumentales y diseños cuasi-experimentales | IV, diferencias en diferencias, discontinuidad | Preferencia del médico; umbrales de TFGe | *What If* (cap. 16) |
| Heterogeneidad del efecto | Efectos por subgrupo (albuminuria) | Quién se beneficia más | r-causal |
| Evaluación de la certeza de la evidencia | GRADE con estudios no aleatorizados | — | cap. 20 actual |
