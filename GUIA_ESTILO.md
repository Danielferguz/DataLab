# Guía de estilo del libro (versión "amigable")

Lectora/lector objetivo: **residente de nefrología** que sabe medicina, sabe poco de estadística y casi nada de R. Quiere entender, no memorizar.
Referencias de enfoque: *Causal Inference in R* (r-causal.org), *Epidemiological Methods* (EpiMethods, E. Karim) y el estilo explicativo de Andrew Heiss (andrewheiss.com/blog): cercano, paso a paso, con intuición antes que fórmulas.

## 1. Tono y voz

-   **Tuteo** ("verás", "calcula"), voz cálida y directa. Primera persona del plural para el recorrido ("veamos", "ahora simulamos").
-   **Una idea por párrafo**, párrafos de 2-4 líneas. Frases cortas. Nada de muros de texto ni de tablas largas sin explicar.
-   **Primero la intuición, después el código, después la interpretación.** La fórmula, si hace falta, va *después* de la idea y en un solo bloque.
-   **Define cada término al usarlo por primera vez** (en una frase, con un ejemplo de nefrología). Si un término tiene sinónimos (DEM/SMD), úsalos juntos solo la primera vez.
-   **Cuenta una historia clínica corta** al abrir cada subcapítulo (2-4 líneas): el paciente, la duda, por qué importa.
-   Humor muy ocasional y siempre al servicio de la idea (una analogía, una "trampa clásica"). Nada de relleno.
-   Evita anglicismos innecesarios; deja el término en inglés entre paréntesis la primera vez (*propensity score*) cuando sea el que el lector verá en la literatura.

## 2. Estructura fija de cada subcapítulo

1.  **Título** claro (pregunta o acción: "¿Qué es un puntaje de propensión?", "Cómo comparar dos grupos").
2.  **🩺 Escenario** (callout-note, 2-4 líneas): el caso clínico que motiva el tema.
3.  **Lo que vamos a hacer** (2-3 viñetas cortas).
4.  **Desarrollo en pasos numerados** (Paso 1, Paso 2...). Cada paso: idea en 1-3 líneas → código corto → "Qué vemos" (interpretación con las cifras reales de la salida).
5.  **Trampas frecuentes** (⚠️, máximo 1-2 callouts por subcapítulo; no repetir lo ya dicho).
6.  **🎯 En resumen** (3-5 viñetas) y **Ejercicio** (2-3 preguntas).
7.  **Para leer más** (solo en la página del capítulo, ver §6).

Máximo **3-4 callouts por subcapítulo** en total. Una tabla solo si compara de verdad opciones; si es una lista, usa viñetas.

## 3. Código

-   **Corto, legible y comentado línea por línea cuando es nuevo.** Si un bloque pasa de ~25 líneas, divídelo en pasos con texto entre medias.
-   Estilo tidyverse con `|>`. Nombres en español sin tildes, minúsculas con `_`.
-   Un bloque = una idea. Etiqueta (`#| label:`) única en todo el libro.
-   **Muestra primero el resultado "feo" y luego el bueno** cuando enseñes un error (sesgo, mala imputación, PH violado).
-   Código **reutilizable**: funciones propias van en `capitulos/R/` y se cargan con `source()`.
-   Adaptar y simplificar el código del autor (`Mis códigos/`) a **datos de nefrología simulados del libro**. Para paquetes que el autor usa pero que no se pueden instalar aquí (`tableone`, `gtsummary`, `cobalt`, `WeightIt`, `dagitty`, `ggdag`, `janitor`, `naniar`, `meta`, `mediation`…), incluir el código del paquete en un bloque **`#| eval: false`** claramente rotulado "Con el paquete X (no se ejecuta aquí)", **además** de la versión ejecutable propia.
-   Nunca usar datos reales de pacientes ni copiar bases de `Mis códigos/`. Todo dato es simulado o hipotético y se **rotula**.

## 4. Cifras y verdad

-   **Toda cifra del texto sale de la salida real del código.** Usa código en línea (`` `r ...` ``) para las cifras importantes; si escribes un número a mano, verifícalo.
-   Con datos simulados con verdad conocida (`ckd_isglt2` y oráculo, `cohorte_pronostico`, `eca_bicarbonato`, `ckd_faltantes`, `encuesta_erc`), **compara siempre contra la verdad**.
-   Decimales con punto. Porcentajes con espacio fino o sin él, de forma consistente ("12 %").

## 5. Callouts (siempre los mismos)

| Icono | Clase | Uso |
|---|---|---|
| 🩺 | `callout-note` | Escenario clínico |
| 💡 | `callout-tip` | Truco práctico / alternativa de paquete |
| ⚠️ | `callout-warning` | Trampa frecuente |
| 🎯 | `callout-important` | En resumen |

## 6. Lecturas del autor por capítulo

Al final de la **página del capítulo** (`CapítuloN.qmd`) hay una sección `## Para leer más` con 3-6 lecturas **realmente pertinentes**. Fuentes que el autor pidió considerar:

-   *Causal Inference in R* (Barrett, D'Agostino McGowan, Gerke): https://www.r-causal.org/
-   *Epidemiological Methods* (E. Karim): https://ehsanx.github.io/EpiMethods/
-   *Practical Propensity Score Methods Using R*: https://www.practicalpropensityscore.com/ y https://rydaro.github.io/index.html
-   Diez Roux AV. Multilevel analysis in public health research. *Annu Rev Public Health*. 2000;21:171-192.
-   Iniciativa STRATOS (guías por grupo temático): https://www.stratos-initiative.org/publications/
-   *An Introduction to Spatial Data Analysis and Visualisation in R* (CDRC): https://data.geods.ac.uk/dataset/an-introduction-to-spatial-data-analysis-and-visualisation-in-r
-   Hyndman RJ, Athanasopoulos G. *Forecasting: Principles and Practice* (3.ª ed.): https://otexts.com/fpp3/
-   Blog de Andrew Heiss (explicaciones modelo): https://www.andrewheiss.com/blog/

**Esas páginas están bloqueadas en este entorno y no se han podido leer.** Por eso: solo se describe de ellas lo que es de conocimiento general (autores, tema, para qué sirven) y nunca se les atribuyen contenidos, cifras ni citas concretas. Se rotula "Lectura recomendada por el autor".

## 7. Qué NO tocar

-   No cambiar `_quarto.yml`, `publicar.sh`, `docs/`, ni los simuladores de `capitulos/R/` (salvo añadir funciones nuevas a `funciones_epi.R` o archivos nuevos). Si se cambia un simulador o una base, se rompe la verdad verificada.
-   No cambiar los nombres de archivo de los capítulos.
-   No hacer `git commit` / `git push`: lo hace la persona coordinadora.
