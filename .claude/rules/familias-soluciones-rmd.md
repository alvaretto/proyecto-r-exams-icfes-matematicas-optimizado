# Regla #21 — Familias de Soluciones Reutilizables para `.Rmd`

## Principio Fundamental

**Los patrones de corrección recurrentes se institucionalizan como "familias de soluciones" reutilizables: snippets canónicos + helpers sourceables + verificación. Antes de resolver un problema conocido de forma ad-hoc, usar la familia correspondiente.**

Esta regla NO crea comportamiento nuevo obligatorio por sí sola; es el **índice operativo** de patrones probados. La generación de ejercicios (`generar-cloze`, `generar-schoice`, orquestadores) DEBE aplicar las familias relevantes por defecto. Cada familia tiene un snippet copy-paste y, cuando aplica, una función en la librería `.claude/scripts/snippets_familias_rmd.R`.

Origen: auditoría profunda de `rango_colesterol_..._cloze_v1` (2026-06-05), que destiló estos patrones a partir de Errores 22 + responsividad Moodle/HTML.

---

## Familia 1 — Generación robusta SIN cuelgue (Error 22)

**Síntoma:** el render se congela (sin error) en una fracción de semillas. Causa: un bucle `repeat`/`while` que **resamplea hasta cumplir una condición que puede ser imposible** para ciertos datos.

**PROHIBIDO:**
```r
repeat {                                 # sin contador, condición posiblemente imposible
  v <- sample(rango[1]:rango[2], n, replace = TRUE)
  if (max(v) - min(v) > objetivo) break  # imposible si objetivo >= span
}
```

**Familia (construcción determinista del objetivo):**
```r
# pick_int(a,b): entero uniforme en [a,b]; seguro cuando a==b (evita trampa sample(escalar,1))
pick_int <- function(a, b) if (a >= b) a else sample(a:b, 1L)

span <- rango[2] - rango[1]
# Si el objetivo "mayor que X" no es alcanzable, ajustar la decisión en vez de buscar
if (rango_mas_grande >= span) decision <- FALSE else decision <- sample(c(TRUE, FALSE), 1)
objetivo <- if (decision) pick_int(rango_mas_grande + 1, span) else pick_int(0, rango_mas_grande)

base <- pick_int(rango[1], rango[2] - objetivo)
relleno <- if (objetivo == 0) rep(base, n - 2) else sample(base:(base + objetivo), n - 2, replace = TRUE)
valores <- sample(c(base, base + objetivo, relleno))
stopifnot(max(valores) - min(valores) == objetivo)
```

**Si el bucle es inevitable:** SIEMPRE contador + `max_intentos` + `stopifnot`/fallback (como el `while` de validez de datos).

**Verificación:** stress test con `setTimeLimit` por semilla (≥200). Test: `tests/testthat/test_data_generation_no_hang.R`.
**Ver:** Error 22 (`patrones-errores-conocidos.md`), `feedback_repeat_sin_cota_cuelgue.md`.

---

## Familia 2 — Tablas RESPONSIVAS cross-formato (móvil)

**Síntoma:** en Moodle/HTML la tabla desborda en pantallas pequeñas (no hay scroll).

**Clave técnica:** bajo R-exams `knitr::is_latex_output()` es el ÚNICO discriminador (TRUE = PDF/NOPS; FALSE = HTML/Moodle/**DOCX** indistinguibles). Una tabla HTML cruda **se aplana** en DOCX. La solución que funciona en TODOS los formatos es un **fenced div de pandoc** (`::: {style=...}`, OJO el espacio) envolviendo una tabla **Markdown nativa**: en HTML/Moodle → `<div overflow-x><table>`; en DOCX → el div se ignora y la tabla Markdown se convierte a tabla nativa `<w:tbl>`; en PDF → longtable (requiere guard `\newcounter{none}`, regla #20).

**Familia (helper):**
```r
tabla_responsiva <- function(df, align = NULL, bold_rows = integer(0)) {
  df2 <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  if (length(bold_rows)) for (r in bold_rows)
    df2[r, ] <- lapply(df2[r, , drop = FALSE], function(x) paste0("**", x, "**"))  # negrita nativa
  md <- paste(knitr::kable(df2, format = "markdown", align = align), collapse = "\n")
  if (knitr::is_latex_output()) md
  else paste0("::: {style=\"overflow-x:auto; -webkit-overflow-scrolling:touch; max-width:100%; margin:0.6em 0;\"}\n\n",
              md, "\n\n:::\n")
}
# Uso: cat(tabla_responsiva(df, align = c("l", rep("c", k)), bold_rows = idx_correcto), "\n")
```

**Requisitos:** mantener el guard `\@ifundefined{c@none}{\newcounter{none}}{}` al inicio de `Question` (la rama LaTeX sigue emitiendo `format="markdown"`).
**Verificación:** render 4 formatos; en HTML/Moodle buscar `<div ... overflow-x`; en DOCX confirmar `<w:tbl>` (no texto aplanado); en PDF compilar con pandoc de RStudio (3.8.3).

---

## Familia 3 — Ecuaciones display RESPONSIVAS

**Síntoma:** ecuaciones `$$...$$` largas desbordan horizontalmente en móvil (MathJax no parte líneas).

**Familia (helper):**
```r
eq_display <- function(tex) {
  if (knitr::is_latex_output()) paste0("$$", tex, "$$")
  else paste0("::: {style=\"overflow-x:auto; -webkit-overflow-scrolling:touch; max-width:100%;\"}\n\n",
              "$$", tex, "$$\n\n:::\n")   # OJO: espacio en "::: {"; MathJax renderiza dentro del div
}
# Uso en chunk results='asis':
# cat(eq_display(paste0("\\text{Rango} = ", maximo, " - ", minimo, " = ", rango)))
```

**Nota:** convertir cada `$$...$$` del cuerpo Markdown a un chunk `results='asis'` que llame `eq_display()` (necesario para la rama condicional). MathJax sigue renderizando el math dentro del fenced div (verificado en Moodle/HTML). En DOCX el div se ignora y el math se convierte a OMML.

---

## Familia 4 — Coherencia de MARCAS en CLOZE (schoice/mchoice)

**Síntoma:** una opción falsa aparece marcada como correcta (o viceversa) en Moodle.

**Reglas de la familia:**
1. Construir `opciones` y `sol` en el **mismo orden** (derivar `sol` de la misma estructura que genera los textos). Nunca calcular `sol` por una ruta y los textos por otra.
2. `exshuffle: TRUE` es obligatorio (ICFES / `ERR_C4`). R-exams re-mezcla opciones y `exsolution` de forma consistente (verificado: 0 desalineaciones en 50 semillas para mchoice cloze). NO requiere `exshuffle:FALSE`.
3. La `Solution` identifica la opción correcta por **contenido/código**, nunca por letra/posición (regla #19, letter-independence).
4. Si hay mezcla interna adicional con `sample()`, permutar **a la vez** `opciones`, `sol` y cualquier vector paralelo (códigos, etc.) con la MISMA permutación.

**Verificación canónica (marca-vs-verdad):** renderizar a Moodle (N semillas), parsear cada `{1:MULTICHOICE/MULTIRESPONSE:...}` y comparar la marca (`=`/`%pos%` = verdadera; `%-...%` = falsa) contra la verdad RECOMPUTADA del texto. 0 mismatches = OK. (Si una exportación Moodle vieja muestra desalineación pero el `.Rmd` actual verifica OK → re-exportar.)
**Ver:** Error 17, regla #19, regla #6 (`codigo-rmd.md`).

---

## Familia 5 — Trampa `sample()` con vector de longitud 1

**Síntoma:** `sample(x, k)` devuelve valores en `1:x` cuando `length(x)==1` (R reinterpreta el escalar como `n`). **Variante length-0:** si `x` es `NULL`/vacío (típicamente un campo inexistente o mal escrito en una `list`, p.ej. `ctx$entidades` cuando el campo se llamó `quienes`), `sample(x, k)` cae en `sample.int(0, ...)` → `"primer argumento inválido"`, error **críptico e intermitente** (solo cuando el RNG toca el elemento defectuoso).

**Familia:**
```r
pick_int   <- function(a, b)  if (a >= b) a else sample(a:b, 1L)        # 1 entero en [a,b]
safe_sample <- function(x, size, replace = TRUE) {                      # muestra segura
  if (length(x) == 0L)                                                  # guarda length-0
    stop("safe_sample(): vector de entrada vacío o NULL (¿campo inexistente o mal escrito en una lista?).")
  if (length(x) == 1L) rep(x, size) else sample(x, size, replace = replace)
}
```
Usar en cualquier muestreo cuyo soporte pueda colapsar a un solo valor (rangos degenerados, `objetivo == 0`, etc.). La guarda `length-0` convierte el error críptico de `sample.int` en un mensaje que apunta a la causa real (campo NULL/typo). Origen: `grafica-circular-consumo-agua` v2 (2026-06-22), pool de contextos con un campo `quienes` donde los demás usaban `entidades`.

---

## Familia 6 — Opciones gráficas de diagramas vectoriales cardinales

**Cuándo aplica:** SCHOICE cuyas 4 opciones son diagramas del tipo "magnitud + dirección" sobre ejes cardinales (desplazamientos, vectores, rumbos). Requiere `library(grid)`. Consolida cuatro defensas que costaron incidentes documentados: Error 23 (etiqueta del ángulo solapada), Error 24 (predictibilidad posicional), Error 25 (el nombre del PNG delata el rol de la opción en el XML de Moodle) y Error 26 + H7 (diagramas degenerados por escala).

**Helpers canónicos** (en `.claude/scripts/snippets_familias_rmd.R`):

| Helper | Qué garantiza |
|---|---|
| `orientaciones_cardinales()` | Pool de 4 cuadrantes (NE/NO/SE/SO). Sortear **uno por versión** y aplicarlo a las 4 opciones: rompe la predictibilidad posicional (regla #22 §P4) preservando la estructura relativa |
| `seleccionar_combinacion_con_cascada()` | Enumeración de combinaciones + **cascada de umbrales** en vez de umbral único. Nunca un bucle de reintento sin cota (Familia 1) |
| `renombrar_opciones_neutral()` | Renombrado a `diagrama_a/b/c/d.png` **POST-mezcla** (regla #22 §P6, Error 25) |
| `dibujar_diagrama_cardinal()` | Un PNG por opción (nunca una grilla, regla #4), con el piso de radio que evita el solape de la etiqueta del ángulo |

**Patrón de la cascada** (lo que distingue esta familia): un umbral de legibilidad único falla por los dos extremos — si es bajo deja diagramas degenerados, y si es alto hay versiones donde **ninguna** combinación cumple y el `stopifnot` revienta el render. La cascada prueba el escalón más exigente y baja de a uno, de modo que cada versión se queda en el más alto que le sea factible:

```r
RATIOS_LEGIBILIDAD <- c(0.40, 0.35, 0.30, 0.25)
pares_validos <- list(); ratio_aplicado <- NA_real_
for (r_min in RATIOS_LEGIBILIDAD) {
  pares_validos <- filtrar_pares(r_min)
  if (length(pares_validos) > 0) { ratio_aplicado <- r_min; break }
}
stopifnot(length(pares_validos) > 0, !is.na(ratio_aplicado))
```

**Regla de diseño de distractores que acompaña a esta familia:** si cada opción muestra su valor numérico ("40 km"), el pool debe incluir **varios** errores que produzcan la MISMA magnitud que la correcta y difieran solo en la dimensión evaluada (lado del eje, eje de referencia, eje perpendicular). Si solo la correcta y un distractor comparten el rótulo, el estudiante calcula el valor y descarta la mitad de las opciones sin mirar el dibujo. Antes de añadir uno de estos distractores hay que **demostrar** que su dirección no colisiona con ninguna otra opción en ningún cuadrante ni ángulo del rango.

**Escala de dibujo:** derivarla del **máximo efectivamente dibujado entre las opciones elegidas**, nunca de una constante ni del valor de un distractor concreto (Error 26 y su gemelo: un distractor cuya longitud en píxeles queda fija por identidad algebraica).

**Origen y verificación:** `desplazamiento-avion-aeropuerto` (2026-07-28). Sobre 80 semillas: 0.40 aplicado en 79/80, vector más corto 48 px (antes 30), 0 fallos; opciones que comparten el rótulo de la correcta 2→24 %, 3→60 %, 4→16 %.

---

## Librería de helpers

Las funciones de las familias 1–3, 5 y 6 están en:

```
.claude/scripts/snippets_familias_rmd.R
```

Por la auto-contención de los `.Rmd` (R-exams copia el ejercicio a un edir temporal), **se recomienda copiar** el helper necesario dentro del chunk `data_generation` del `.Rmd` (no `source()` por ruta frágil). La librería es la fuente de verdad de la versión canónica de cada helper.

---

## Integración con generación

- `generar-cloze` y `generar-schoice`: incluir por defecto `tabla_responsiva()`/`eq_display()` cuando el ejercicio tenga tablas/ecuaciones, y aplicar Familia 1 (sin `repeat` sin cota).
- Orquestadores: agregar a su pre-flight la verificación de las familias relevantes.
- Ejercicios con **opciones gráficas**: aplicar Familia 6 completa (orientación sorteada, cascada de legibilidad, renombrado neutral post-mezcla y distractores que conserven la magnitud). Los orquestadores la cablean en sus pre-flight checks y en los incidentes I-L (schoice) / K-N (cloze).

---

## Tests asociados

| Familia | Verificación |
|---|---|
| 1 (sin cuelgue) | `tests/testthat/test_data_generation_no_hang.R` (estático + runtime timeout) |
| 2 (tablas responsivas) | render 4 formatos + grep `overflow-x` (HTML/Moodle) + `<w:tbl>` (DOCX) |
| 3 (ecuaciones responsivas) | igual que F2 (divs por ecuación) |
| 4 (marcas cloze) | verificador marca-vs-verdad sobre XML Moodle |
| 5 (sample escalar) | cubierto por F1 + análisis estático de `sample(` en `calcula()` |
| 6 (diagramas cardinales) | `Rscript .claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40` + `exams2moodle()` y grep del XML (solo `diagrama_a/b/c/d.png`) + medición del rank de longitud de la correcta sobre ≥40 versiones + inspección visual ampliada ≥×2 en los ángulos extremos del rango |

---

**Versión:** 1.1
**Fecha:** 2026-07-28 (v1.1 — indexada la **Familia 6** (opciones gráficas de diagramas vectoriales
cardinales), que ya existía en la librería de helpers sin estar documentada aquí; v1.0 2026-06-05)
**Estado:** ACTIVO (índice operativo de patrones; aplicar las familias relevantes en toda generación/corrección)
**Origen:** auditoría `rango_colesterol_..._cloze_v1` (Errores 22 + responsividad); Familia 6 desde
`desplazamiento-avion-aeropuerto` (Errores 23-26 + hallazgos H4/H7)
