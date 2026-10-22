# Regla #21 — Familias de Soluciones Reutilizables (versión compacta)

> Texto íntegro —snippets completos de cada familia, medición de `is_latex_output()` en los 5 pipelines, verificación por familia—: `.claude/docs/reglas/familias-soluciones-rmd.md`. Helpers canónicos: `.claude/scripts/snippets_familias_rmd.R` (copiarlos al chunk `data_generation`, no `source()` por ruta).

**Principio.** Antes de resolver ad-hoc un problema conocido, aplicar su familia. Generación y orquestadores las aplican por defecto.

- **F1 Sin cuelgue (Error 22):** nada de `repeat`/`while` que resamplee hasta una condición posiblemente imposible; construir el objetivo de forma determinista (`pick_int`). Si el bucle es inevitable: contador + `max_intentos` + `stopifnot`. Test: `test_data_generation_no_hang.R`.
- **F2 Tablas responsivas:** `tabla_responsiva()` — fenced div `::: {style="overflow-x:auto"}` alrededor de una tabla Markdown nativa (sobrevive en DOCX y PDF); mantener el guard `\newcounter{none}` (regla #20). **En el PDF no parte el texto de las celdas** (R/exams fuerza `--columns=99999` y sus plantillas no cargan `array`/`calc`): la función aborta si el ancho de contenido supera 85 caracteres (medido; `ancho_max_pdf = NULL` solo tras revisar el PDF). Con frases en las celdas, `lista_responsiva()`. Test: `test_tabla_responsiva_pdf.R`.
- **F3 Ecuaciones responsivas:** `eq_display()` en chunk `results='asis'`.
- **F4 Marcas CLOZE:** `opciones` y `sol` construidos en el mismo orden; misma permutación para vectores paralelos; verificar marca-vs-verdad en el XML de Moodle.
- **F5 `sample()` escalar/vacío:** `pick_int` / `safe_sample` (guarda length-0).
- **F6 Diagramas cardinales:** orientación sorteada por versión, cascada de umbrales de legibilidad, renombrado neutral POST-mezcla, distractores que conservan la magnitud de la correcta, escala del máximo dibujado.

⚠️ `knitr::is_latex_output()` es **SIEMPRE FALSE** bajo R/exams (los 5 pipelines tejen a Markdown y pandoc enruta por tipo de bloque). La rama LaTeX de `tabla_responsiva()`/`eq_display()` es **RAMA MUERTA** inocua; no copiar ese idioma donde las ramas difieran.

**Versión:** 1.3 (2026-10-22: F2 en el PDF) · compacta desde 2026-09-29.
