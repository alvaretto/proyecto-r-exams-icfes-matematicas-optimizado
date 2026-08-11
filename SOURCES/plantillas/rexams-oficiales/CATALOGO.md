# Catálogo de plantillas oficiales de R/exams

> **Qué es esto:** los ejercicios-plantilla que distribuye el propio paquete `exams`.
> Son la referencia de **sintaxis de R/exams**, no del estándar pedagógico ICFES.
> Antes de copiar nada de aquí, lee la jerarquía de autoridad en [README.md](README.md).

**Corpus:** 46 ejercicios · 45 `.Rmd` + 46 `.Rnw` = 91 archivos · `exams` 2.4.2 (+ `penguins` de 2.4-4)  
**Procedencia y verificación:** ver [VERSION.txt](VERSION.txt)

---

## Índice por área

- [General](#general) — 12 ejercicios
- [Matemáticas](#matematicas) — 10 ejercicios
- [Estadística](#estadística) — 22 ejercicios
- [Ciencias de la Computación](#ciencias-de-la-computación) — 2 ejercicios
- [Índice por técnica](#indice-por-tecnica)
- [Discrepancias declaradas](#discrepancias-declaradas)

---

## General

| Ejercicio | extype | Formatos | Qué demuestra | Uso ICFES sugerido |
|---|---|---|---|---|
| `capitals` | mchoice | Rmd+Rnw | `mchoice` con muestreo de una lista de pares país-capital para generar distractores | Distractores por muestreo de un pool. En ICFES esto NO basta: la regla #1 exige errores conceptuales, no ítems ajenos al azar |
| `countrycodes` | string | Rmd+Rnw | `string` con la respuesta sorteada de una matriz de códigos ISO | Segundo ejemplo de `string`, con el pool de respuestas en una estructura de datos |
| `currency8` | num | Rmd+Rnw | `num` con símbolos monetarios UTF-8 (€, £) en el enunciado | **Referencia directa para el repo**: prueba que caracteres no ASCII sobreviven a los 5 formatos. Relevante para tildes (regla #7) y para el incidente U+2212 |
| `flags` | schoice | Rmd+Rnw | `schoice` cuyas opciones son banderas Unicode construidas desde una matriz de países | Opciones no textuales sin generar PNG. Alternativa ligera cuando la regla #4 (un PNG por opción) resulta excesiva |
| `function` | string | Rmd+Rnw | `extype: string` con respuesta de texto libre evaluada por coincidencia exacta | Único tipo del repertorio oficial que el repo no usa. Referencia si se evalúa introducir respuesta abierta corta |
| `penguins` | cloze (dinámico) | Rmd+Rnw | Los gaps se declaran **inline en la prosa** con `add_cloze(valor, ...)`, y `format_metainfo()` deriva de ellos `exclozetype`, `exsolution` y `extol`. No se mantiene a mano ninguna lista de tipos ni de soluciones | **La técnica de mayor valor potencial del corpus para este repo**: elimina por construcción el desajuste entre número de gaps, orden de `##ANSWERi##` y `exclozetype` (regla #14 de `codigo-rmd.md`, Incidente A del orquestador CLOZE). **No renderiza con exams 2.4.2** (ver [discrepancia 3](#discrepancias-declaradas)) |
| `Rlogo` | schoice | Rmd+Rnw | `include_supplement()` resolviendo el archivo desde `find.package("exams")` con `recursive = TRUE` | El patrón oficial de auto-contención documentado en `.claude/docs/AUTOCONTENCION_REXAMS.md`. Referencia obligada antes de adjuntar cualquier recurso externo a un `.Rmd` |
| `sumdiff` | num | Rmd+Rnw | `num` mínimo: dos parámetros aleatorios y una `exsolution` calculada inline | El esqueleto más pequeño que renderiza. Punto de partida para leer la anatomía de un `.Rmd` de R/exams |
| `swisscapital` | schoice | Rmd+Rnw | `schoice` de conocimiento factual, la variante de una sola respuesta de `switzerland` | Molde mínimo de `schoice` con `exshuffle` |
| `switzerland` | mchoice | Rmd+Rnw | `mchoice` de conocimiento factual con afirmaciones verdaderas/falsas mezcladas | Molde de `mchoice`. Sin valor pedagógico ICFES (recuerdo puro, DOK 1) |
| `vowels` | cloze (schoice\|schoice\|schoice\|schoice\|schoice\|schoice) | Rmd+Rnw | CLOZE de 6 gaps `schoice` homogéneos sobre una misma tabla de referencia | El molde más cercano al estándar CLOZE del repo (mínimo 6 partes). Ojo: sus 6 gaps son del mismo tipo, sin Progressive Disclosure |
| `vowels2` | schoice | Rmd+Rnw | La misma materia que `vowels` colapsada a un único `schoice` | Contraste directo CLOZE vs SCHOICE sobre contenido idéntico: útil para decidir el tipo de un ítem |

## Matemáticas

| Ejercicio | extype | Formatos | Qué demuestra | Uso ICFES sugerido |
|---|---|---|---|---|
| `cholesky` | mchoice | Rmd+Rnw | `mchoice` sobre descomposición de Cholesky con matrices renderizadas en LaTeX | Emisión de matrices LaTeX que sobreviven a HTML y PDF. Referencia de formato, no de contenido (fuera del alcance Saber 11) |
| `deriv` | num | Rmd+Rnw | `num` con derivada simbólica y `extol` para la tolerancia numérica | Molde de respuesta numérica con tolerancia. Componente Numérico-variacional |
| `deriv2` | schoice | Rmd+Rnw | La misma derivada como `schoice`, con distractores derivados de errores de aplicación de la regla del producto | **El más cercano al diseño ICFES del repo**: sus distractores son errores de procedimiento identificables, no ruido. Comparar con el pool de errores conceptuales de la regla #1 |
| `dist` | num | Rmd+Rnw | `num` con figura generada por `plot()` de R base y teorema de Pitágoras | Gráfico dinámico en R base (no ggplot2). Componente Espacial/Métrico |
| `dist2` | cloze (num\|num\|num) | Rmd+Rnw | CLOZE `num\|num\|num`: tres distancias sobre una misma figura | Progressive Disclosure incipiente: varias preguntas sobre un único estímulo visual. Molde útil para CLOZE geométrico |
| `dist3` | schoice | Rmd+Rnw | La variante `schoice` de `dist`, con la figura como estímulo compartido | Contraste num vs schoice sobre el mismo gráfico |
| `fruit` | num | Rmd+Rnw | `num` con imágenes PNG incrustadas en base64 y decodificadas en tiempo de render (`base64enc`) | **Técnica de alto valor**: imágenes auto-contenidas sin archivo externo ni supplement. Emite `![](x.png){width=...}` con atributo, coherente con la regla #18 |
| `fruit2` | schoice | Rmd+Rnw | La variante `schoice` del sistema de ecuaciones pictórico | Sistema de ecuaciones presentado con iconos: molde de estímulo visual no cartesiano |
| `hessian` | schoice | Rmd+Rnw | `schoice` cuyas cuatro opciones son matrices 2x2 en LaTeX | Opciones que son objetos matemáticos, no texto. Referencia para ítems con opciones en notación |
| `lagrange` | num | Rmd+Rnw | `num` de optimización con restricción y figura de curvas de nivel | Gráfico de contorno dinámico. Fuera del alcance Saber 11 por contenido |

## Estadística

| Ejercicio | extype | Formatos | Qué demuestra | Uso ICFES sugerido |
|---|---|---|---|---|
| `anova` | mchoice | Rmd+Rnw | `mchoice` sobre tabla ANOVA con salida de R formateada y figura | Interpretación de salida estadística. Componente Aleatorio |
| `boxhist` | cloze (dinámico) | Rmd+Rnw | CLOZE con `exclozetype` **dinámico** (`r paste(type, collapse="\|")`) más un CSV escrito por el propio ejercicio | Dos técnicas en una: número de gaps variable por versión, y datos entregados al estudiante como archivo adjunto generado al vuelo |
| `boxhist2` | cloze (dinámico) | Rmd+Rnw | La variante de `boxhist` con formato de tabla flexible | Muestra cómo parametrizar el formato de salida sin tocar la lógica |
| `boxplots` | mchoice | Rmd+Rnw | `mchoice` de interpretación de dos diagramas de caja generados con `boxplot()` | Emparejable con los ejercicios de diagrama de caja del repo. Referencia de sintaxis del gráfico, no del diseño de distractores |
| `confint` | num | Rmd+Rnw | `num` de intervalo de confianza de dos colas (no aparece listado en la web) | Variante base de la familia `confint` |
| `confint2` | cloze (num\|num) | Rmd+Rnw | CLOZE `num\|num`: límite inferior y superior como gaps separados | Patrón para pedir dos componentes de una misma respuesta sin que uno delate al otro |
| `confint3` | cloze (verbatim\|verbatim) | Rmd+Rnw | CLOZE con gaps `verbatim`, que inyectan HTML/texto crudo en el hueco (versión extendida para Moodle) | Tipo de gap poco conocido. Relevante para el Incidente G del repo: qué se puede y qué no dentro de un gap CLOZE |
| `essayreg` | string | Rmd+Rnw | `extype: string` usado como pregunta de ensayo, con metadatos `exextra` para la rúbrica | Uso de `exextra[...]` para transportar información fuera de los campos estándar — el mismo mecanismo con el que el repo lleva las 6 dimensiones ICFES |
| `essayreg2` | cloze (num\|num\|essay\|file) | Rmd+Rnw | CLOZE `num\|num\|essay\|file` que combina respuesta cerrada y abierta en un ítem | Híbrido cerrado+abierto. No aplicable a NOPS |
| `fourfold` | cloze (num\|num\|num\|num) | Rmd+Rnw | CLOZE `num` x4 sobre una tabla de contingencia 2x2 | Cuatro celdas, cuatro gaps: molde de tabla completada por el estudiante |
| `fourfold2` | cloze (num\|num\|num\|num\|num\|num\|num\|num\|num) | Rmd+Rnw | La versión de 9 gaps con formato flexible y metadatos `exextra` | Escala el patrón anterior a tabla con marginales |
| `gaussmarkov` | mchoice | Rmd+Rnw | `mchoice` de conocimiento teórico sobre supuestos de Gauss-Markov | Molde de ítem teórico. DOK 1-2 |
| `lm` | cloze (schoice\|num) | Rmd+Rnw | CLOZE `schoice\|num` con los datos entregados en un `regression.csv` generado por el ejercicio | Primer escalón de la familia `lm`: dato externo + dos preguntas encadenadas |
| `lm2` | cloze (string\|mchoice\|num\|num\|schoice) | Rmd+Rnw | CLOZE de 5 gaps con **tipos mixtos** `string\|mchoice\|num\|num\|schoice` | **El mejor ejemplo oficial de Progressive Disclosure**: teoría → aplicación, escalando el tipo de respuesta. Molde de referencia para CLOZE de 6 partes del repo |
| `lm3` | cloze (string\|mchoice\|num\|num\|schoice\|essay\|file) | Rmd+Rnw | La misma progresión ampliada a 7 gaps añadiendo `essay` y `file` (subida de archivo) | Techo del formato CLOZE. Los gaps `essay`/`file` solo existen en Moodle: inútiles para PDF/NOPS |
| `regression` | num | Rmd+Rnw | `num` de regresión lineal calculada a mano, sin datos externos | Molde de cálculo estadístico paso a paso |
| `relfreq` | mchoice | Rmd+Rnw | `mchoice` sobre tabla de frecuencias relativas construida desde una matriz | Tabla estadística como estímulo. Verificar contra la regla #20 (guard `none`) si se adopta el patrón de tabla |
| `scatterplot` | mchoice | Rmd+Rnw | `mchoice` de lectura de un diagrama de dispersión | Componente Aleatorio, competencia Interpretación |
| `tstat` | num | Rmd+Rnw | `num` del estadístico t de una muestra | Molde de cálculo inferencial |
| `tstat2` | schoice | Rmd+Rnw | La variante `schoice` de `tstat` | Par num/schoice para comparar cómo cambia el diseño de distractores al cerrar la respuesta |
| `tstat_verbatim` | cloze (num\|verbatim\|num\|num\|num\|schoice) | Rnw | CLOZE de **6 gaps** con tipos mixtos, uno de ellos `verbatim` (salida cruda de R como respuesta). Solo existe en `.Rnw` | El único template oficial que llega a 6 partes con tipos mixtos, que es el mínimo del estándar CLOZE de este repo. No está listado en la web ni tiene versión `.Rmd` |
| `ttest` | mchoice | Rmd+Rnw | `mchoice` de interpretación de un test t de dos muestras | Interpretación de resultado inferencial |

## Ciencias de la Computación

| Ejercicio | extype | Formatos | Qué demuestra | Uso ICFES sugerido |
|---|---|---|---|---|
| `automaton` | mchoice | Rmd+Rnw | `mchoice` con diagrama de autómata en TikZ y transiciones sorteadas por versión | Segundo ejemplo de TikZ dinámico, con topología variable — no solo etiquetas |
| `logic` | schoice | Rmd+Rnw | `schoice` con circuito lógico dibujado en **TikZ** interpolado desde R | Referencia de TikZ dinámico, el lenguaje 1 del Flujo B (regla #3). El circuito se construye con `paste0()` desde variables R |

---

## Índice por técnica {#indice-por-tecnica}

Para cuando necesitas *un ejemplo oficial de X*.

| Técnica | Ejercicios |
|---|---|
| Gráfico dinámico en TikZ (Flujo B, lenguaje 1) | `logic`, `automaton` |
| Gráfico dinámico en R base | `dist`, `dist2`, `dist3`, `lagrange`, `boxplots`, `scatterplot`, `anova`, `boxhist`, `boxhist2`, `lm`, `lm2`, `lm3`, `penguins` |
| Imagen incrustada en base64 (sin archivo externo) | `fruit`, `fruit2` |
| `include_supplement()` desde el paquete instalado | `Rlogo` |
| Datos entregados al estudiante como CSV generado al vuelo | `boxhist`, `boxhist2`, `lm`, `lm2`, `lm3`, `essayreg2` |
| CLOZE con tipos mixtos (Progressive Disclosure) | `lm2`, `lm3`, `essayreg2`, `tstat_verbatim`, `penguins` |
| CLOZE con `exclozetype` dinámico | `boxhist`, `boxhist2`, `penguins` |
| CLOZE homogéneo (todos los gaps del mismo tipo) | `vowels`, `fourfold`, `fourfold2`, `confint2`, `dist2`, `confint3` |
| Gap `verbatim` (texto crudo dentro del hueco) | `confint3`, `tstat_verbatim` |
| Gaps `essay` / `file` (solo Moodle) | `lm3`, `essayreg2` |
| `extype: string` (respuesta abierta corta) | `function`, `countrycodes`, `essayreg` |
| Metadatos `exextra[...]` personalizados | `essayreg`, `fourfold2` |
| Matrices y notación LaTeX en las opciones | `hessian`, `cholesky` |
| Tablas como estímulo | `relfreq`, `fourfold`, `fourfold2`, `cholesky`, `fruit`, `fruit2`, `logic` |
| Caracteres no ASCII / UTF-8 en el enunciado | `currency8`, `flags`, `vowels` |
| Dataset externo de R | `penguins` |
| Par num ↔ schoice sobre el mismo contenido | `deriv/deriv2`, `dist/dist3`, `tstat/tstat2`, `fruit/fruit2`, `vowels/vowels2` |

---

## Discrepancias declaradas {#discrepancias-declaradas}

Registradas durante la extracción. No se corrigió ningún archivo: se documentan.

### 1. El `extype` publicado en la web no coincide con el archivo

`https://www.r-exams.org/templates/` anuncia un tipo que el `Meta-information` real contradice
en **11 de 43** entradas listadas. El catálogo de arriba usa el valor del archivo.

| Ejercicio | Web anuncia | Archivo declara |
|---|---|---|
| `boxhist` | mchoice | **cloze (dinámico)** |
| `boxhist2` | mchoice | **cloze (dinámico)** |
| `cholesky` | num | **mchoice** |
| `confint2` | num | **cloze (num\|num)** |
| `confint3` | num | **cloze (verbatim\|verbatim)** |
| `dist2` | num | **cloze (num\|num\|num)** |
| `essayreg` | essay | **string** |
| `essayreg2` | essay | **cloze (num\|num\|essay\|file)** |
| `fourfold` | num | **cloze (num x4)** |
| `fourfold2` | num | **cloze (num x9)** |
| `lm` | num | **cloze (schoice\|num)** |

### 2. Inventario: la web y el paquete no coinciden

- **Solo en el paquete, ausentes de la web:** `confint`, `vowels2`, `tstat_verbatim`.
- **Solo en la web, ausente de `exams` 2.4.2:** `penguins` — añadido en 2.4-4 (2026-07-31).
  Descargado de upstream y verificado por SHA. Marcado `origen: web` en `VERSION.txt`.
- La página declara «47 templates» pero solo enumera 43. Por eso el censo de este
  catálogo **no** se tomó de la web, sino del paquete instalado más la API de GitHub
  del repositorio upstream.

### 3. `penguins` no renderiza con la versión instalada

Usa **dos** funciones que no existen en `exams` 2.4.2 —verificado con
`getNamespaceExports("exams")`, ambas `FALSE`—:

| Función | Usos en `penguins.Rmd` | Para qué |
|---|---|---|
| `add_cloze()` | 6 (líneas 34, 46, 48, 49, 50, 73) | Declarar cada gap inline en la prosa |
| `format_metainfo()` | 4 (líneas 76, 110, 111, 112) | Derivar Answerlist, `exclozetype`, `exsolution` y `extol` |

Render real intentado el 2026-08-10 con `exams2html()`:

```
Error: no se pudo encontrar la función "add_cloze"
```

Falla primero en `add_cloze` porque se evalúa antes, en el cuerpo de la pregunta.
Se conserva por su valor como referencia de sintaxis; para ejecutarlo hace falta
actualizar a `exams` ≥ 2.4-3.

### 4. Columnas sin dato web

Ninguna. La categoría temática de `confint`, `vowels2` y `tstat_verbatim` se asignó
por analogía con su familia (`confint2`/`vowels`/`tstat`), no por dato de la web.

---

*Generado el 2026-08-10 a partir de los archivos reales, no de la página web.*
