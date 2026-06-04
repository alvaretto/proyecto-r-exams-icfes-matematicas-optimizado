# Regla #20 — Tablas Markdown + pandoc ≥3.7 (guard del contador `none`)

## Principio Fundamental

**Todo `.Rmd` que emita una tabla Markdown (vía `knitr::kable(format = "markdown")` o construcción manual con `cat("| ...")`) y se renderice a PDF/NOPS DEBE incluir, al inicio de la sección `Question`, un bloque raw LaTeX que defina el contador `none` si falta.**

Esta regla NO tiene excepciones para archivos en `01-En-PreDesarrollo/`, `02-En-Desarrollo/` ni `03-En-Produccion/`. Aplica a cualquier `.Rmd` con tablas Markdown, independientemente de cuántas tablas tenga.

---

## Origen: incidente 2026-06-03 (rango_colesterol, RStudio)

El ejercicio `rango_colesterol_metacognitivo_interpretacion_n3_schoice_v1` pasó TODA la validación del pipeline (FASES 2A-2J, exams2pdf 5/5, exams2nops OK) ejecutada desde terminal. El usuario lo renderizó en **RStudio** con `exams2nops()` y obtuvo:

```
! LaTeX Error: No counter 'none' defined.

Error: LaTeX failed to compile rango_..._1.tex.
```

**Causa raíz:** RStudio usa su **pandoc bundleado** (`/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64/pandoc`, versión **3.8.3**), distinto del pandoc de terminal (**3.6**) que usa `Rscript` y, por tanto, la validación del pipeline. Pandoc **≥3.7** envuelve toda tabla `longtable` (lo que produce un pipe table Markdown al convertirse a LaTeX) con:

```latex
{\def\LTcaptype{none} % do not increment counter
\begin{longtable}[]{@{}...@{}}
...
\end{longtable}
}
```

El `\def\LTcaptype{none}` asume que existe un contador LaTeX llamado `none`. La plantilla LaTeX minimalista de R-exams **no lo define** (ni carga el paquete `caption`), así que `longtable` falla. Pandoc 3.6 NO emite ese wrapper → por eso el bug es invisible en terminal y solo aparece en RStudio.

Es un caso **gemelo del Error 16 (`\pandocbounded`)**: "pasa en el entorno del agente, falla en el entorno destino del usuario" por diferencia de versión de pandoc.

---

## Patrón Aceptado (ÚNICO)

Insertar, inmediatamente después del encabezado `Question`, un bloque raw LaTeX con guardia:

```rmd
Question
========

` ``{=latex}
\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother
` ``

` ``{r enunciado, echo=FALSE, results='asis'}
...
```

### Por qué este patrón

- **`{=latex}`**: pandoc lo emite SOLO en salida LaTeX (PDF/NOPS) y lo descarta en HTML/DOCX → no ensucia los otros formatos.
- **`\@ifundefined{c@none}{...}{}`**: define el contador SOLO si no existe. Imprescindible en `exams2nops()` con múltiples ejercicios: sin la guardia, el segundo ejercicio haría `\newcounter{none}` dos veces → "counter already defined".
- **Posición al inicio de `Question`**: garantiza que el contador exista antes de la primera tabla, y cubre también las tablas de la sección `Solution` (mismo documento LaTeX en `exams2pdf`).

Verificado con pandoc **3.8.3 y 3.6**: `exams2pdf`, `exams2nops` (×3 ejercicios), `exams2html`, `exams2pandoc(docx)` → todos OK; sin fuga de LaTeX crudo en HTML. Commit del fix original: `d22caf93`.

---

## Patrones PROHIBIDOS

### ❌ Tabla Markdown sin el guard

```rmd
Question
========

` ``{r tabla, echo=FALSE, results='asis'}
cat(knitr::kable(df, format = "markdown"))
` ``
```
→ Falla en RStudio (pandoc 3.8.3) con `No counter 'none' defined`.

### ❌ Definir el contador sin guardia en ejercicios NOPS multi-ítem

```rmd
` ``{=latex}
\newcounter{none}
` ``
```
→ En `exams2nops(rep(f, N))` el segundo ítem redefine `none` → `LaTeX Error: Command \c@none already defined`.

---

## Defensa Automática (4 capas)

### Capa 1 — Generación (skills + orquestadores)

`generar-schoice`, `generar-cloze`, `orquestador-schoice` (y orquestador CLOZE) incluyen el guard por defecto en TODO `.Rmd` que use tablas Markdown.

### Capa 2 — Hook PostToolUse FASE 2K

`post-exams2-validation.sh` agrega FASE 2K tras 2J:

```bash
TIENE_TABLA=$(grep -nE 'kable\(|format[[:space:]]*=[[:space:]]*"markdown"|cat\("\|' "$RMD_FILE")
TIENE_GUARD=$(grep -nE 'newcounter\{none\}|@ifundefined\{c@none\}' "$RMD_FILE")
# tabla sin guard -> ERR_TABLA_NONE (bloqueante)
```

Código de error: `ERR_TABLA_NONE` (bloqueante).

### Capa 3 — Test de regresión

`tests/testthat/test_markdown_tablas_none_guard.R` recorre los `.Rmd` de `01-En-PreDesarrollo/`, `02-En-Desarrollo/` y `03-En-Produccion/` (excepto `Ejemplos-Funcionales-Rmd/`): si un `.Rmd` tiene tabla Markdown y NO tiene el guard, falla.

### Capa 4 — Validación con el pandoc de RStudio (recomendada)

Al validar PDF/NOPS de un ejercicio con tablas, probar TAMBIÉN con el pandoc de RStudio:

```r
Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64")
stopifnot(as.character(rmarkdown::pandoc_version()) >= "3.7")
exams2pdf("archivo.Rmd", n = 1)
exams2nops(rep("archivo.Rmd", 3), n = 1)
```

---

## Excepciones (NINGUNA)

No hay excepciones. Si un `.Rmd` no usa tablas Markdown, el guard no es requerido (la FASE 2K lo reporta como `⊘ no requerido`). Si las usa, el guard es obligatorio.

---

## Tests Asociados

| Test | Verifica |
|---|---|
| `tests/testthat/test_markdown_tablas_none_guard.R` | Detección estática de tablas Markdown sin guard en .Rmd existentes |

---

## Referencias

- Error 21 en `.claude/docs/patrones-errores-conocidos.md`
- Regla #18 `markdown-imagenes-pdf.md` (mismo patrón: diferencia de pandoc en el entorno destino)
- Memoria: `feedback_pandoc_ltcaptype_none.md`
- Pandoc 3.7 changelog (cambio en el wrapper de tablas captionless)
- Commit del fix original: `d22caf93`

---

**Versión:** 1.0
**Fecha:** 2026-06-03
**Estado:** ACTIVO Y OBLIGATORIO
**Excepciones:** NINGUNA
**Aplica a:** todo `.Rmd` con tablas Markdown que renderice en PDF/NOPS.
