# Regla #20 — Tablas Markdown + pandoc ≥3.8.1 (guard del contador `none`)

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

**Causa raíz:** RStudio usa su **pandoc bundleado** (`/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64/pandoc`, versión **3.8.3**), distinto del pandoc de terminal (**3.6**) que usa `Rscript` y, por tanto, la validación del pipeline. A partir de **pandoc 3.8.1** (2025-09-29), toda tabla `longtable` sin caption (lo que produce un pipe table Markdown al convertirse a LaTeX) se envuelve en un bloque `\def\LTcaptype{...}` para evitar que incremente el contador de tablas numeradas. El nombre exacto de ese contador **cambió entre versiones**:

| Versión pandoc | Wrapper emitido | Error LaTeX resultante |
|---|---|---|
| ≤ 3.8.0 | (ninguno; sin envoltorio) | Sin error — comportamiento previo al bug |
| 3.8.1 | `\def\LTcaptype{}` (cadena **vacía**) | `! LaTeX Error: No counter '' defined.` |
| ≥ 3.8.2.1 (incl. 3.8.3) | `\def\LTcaptype{none}` | `! LaTeX Error: No counter 'none' defined.` |

Verificado contra el código fuente de pandoc (`src/Text/Pandoc/Writers/LaTeX/Table.hs`), caso ≥3.8.2.1:

```latex
{\def\LTcaptype{none} % do not increment counter
\begin{longtable}[]{@{}...@{}}
...
\end{longtable}
}
```

En ambos casos (`{}` o `{none}`) el `\def\LTcaptype{...}` asume que existe un contador LaTeX con ese nombre. La plantilla LaTeX minimalista de R-exams **no lo define** (ni carga el paquete `caption`), así que `longtable` falla. Pandoc 3.6 y ≤3.8.0 NO emiten ese wrapper → por eso el bug es invisible en terminal (pandoc 3.6) y solo aparece en RStudio (pandoc ≥3.8.1).

**Fuentes verificadas (consultadas 2026-07-28):**
- Changelog pandoc 3.8.1 (2025-09-29): "LaTeX writer: Ensure that unlabelled tables don't increment counter (#11141)." — https://github.com/jgm/pandoc/blob/main/changelog.md
- Changelog pandoc 3.8.2.1 (2025-10-20): "LaTeX writer/template: small fix for unnumbered tables for compatibility with older LaTeX installations (#11201)." — cambia el nombre del contador de `` (vacío) a `none`.
- Issues de origen: https://github.com/jgm/pandoc/issues/11189 y https://github.com/jgm/pandoc/issues/11201
- **Hallazgo importante**: se buscó `\newcounter{none}` en el código fuente de pandoc (`LaTeX.hs`, `Table.hs`) y en las plantillas oficiales (`jgm/pandoc-templates`, tags 3.8–3.8.2.1) y **NO EXISTE en ninguna parte**. pandoc cambia el NOMBRE del contador entre versiones, pero nunca lo DEFINE — el guard de esta regla es necesario en cualquier pandoc ≥3.8.1, no solo en la rama "≥3.7" mencionada en versiones previas de esta regla.
- **Limitación honesta del guard actual**: `\newcounter{none}` cubre el caso ≥3.8.2.1 (contador `none`). El caso intermedio pandoc 3.8.1 (contador con nombre **vacío**) no queda cubierto por este guard — pero es una franja de versiones muy estrecha (2025-09-29 a 2025-10-20) y no se ha observado en el pipeline real (RStudio bundlea 3.8.3, terminal usa 3.6).

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
stopifnot(as.character(rmarkdown::pandoc_version()) >= "3.8.1")
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
- Pandoc changelog 3.8.1 (2025-09-29) — https://github.com/jgm/pandoc/blob/main/changelog.md — "LaTeX writer: Ensure that unlabelled tables don't increment counter (#11141)"
- Pandoc changelog 3.8.2.1 (2025-10-20) — mismo changelog — "LaTeX writer/template: small fix for unnumbered tables for compatibility with older LaTeX installations (#11201)"
- Issues: https://github.com/jgm/pandoc/issues/11189 , https://github.com/jgm/pandoc/issues/11201
- Código fuente verificado: `src/Text/Pandoc/Writers/LaTeX/Table.hs` (tags 3.8.1 y 3.8.2.1)
- Fecha de consulta de estas fuentes: 2026-07-28
- Commit del fix original: `d22caf93`

---

**Versión:** 1.1
**Fecha:** 2026-07-28 (v1.1 — corrección factual de versión pandoc: ≥3.8.1 en vez de ≥3.7, tabla de comportamiento por versión, fuentes verificadas con changelog e issues oficiales; v1.0 2026-06-03)
**Estado:** ACTIVO Y OBLIGATORIO
**Excepciones:** NINGUNA
**Aplica a:** todo `.Rmd` con tablas Markdown que renderice en PDF/NOPS.
