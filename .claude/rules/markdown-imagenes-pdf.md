# Regla #18 — Markdown-Imágenes para PDF (anti `\pandocbounded`)

## Principio Fundamental

**Toda imagen incluida en un `.Rmd` que vaya a renderizarse en PDF (vía `exams2pdf()` o `exams2nops()`) DEBE emitirse con atributo `width` explícito o vía `\includegraphics` directo. NUNCA usar `![](file.png)` Markdown crudo sin width.**

Esta regla NO tiene excepciones para archivos en `01-En-PreDesarrollo/`, `02-En-Desarrollo/` ni `03-En-Produccion/`. Aplica a chunks R que emiten Markdown vía `cat()` y a Markdown directo en el cuerpo del `.Rmd`.

---

## Origen: incidente 2026-05-03 (orquestador-schoice)

Durante la corrida del orquestador-schoice para `interseccion_ingresos_gastos_metacognitivo_interpretacion_n2_schoice_v1`, las FASES 2A-2G del agente reportaron "4/4 formatos OK" y "20/20 semillas OK". El usuario ejecutó `exams2pdf()` en su entorno real (TinyTeX en Manjaro/zsh) y obtuvo:

```
! Undefined control sequence.
l.5 \pandocbounded
                  {\includegraphics[keepaspectratio]{grafico_equilibrio.png}}
```

Causa: el `.Rmd` contenía un chunk R que emitía `cat("![](grafico_equilibrio.png)\n")` sin atributo width. Desde **pandoc 3.2.1** (2024-06-24), al convertir Markdown→LaTeX, pandoc envuelve `\includegraphics`/`\includesvg` en un comando `\pandocbounded{...}` cuando la imagen NO especifica width/height explícito — ese macro se define en la plantilla LaTeX estándar de pandoc, pero **NO está definido** en los templates LaTeX minimalistas que R-exams usa (ni en TinyTeX stock).

**Fuente verificada (changelog pandoc 3.2.1, consultado 2026-07-28)**: "New method for ensuring images don't overflow (#9660). [...] The new approach uses a new macro `\pandocbounded` that is now defined in the LaTeX template. [...] The LaTeX writer has been changed to enclose `\includegraphics` and `\includesvg` commands in this macro when they don't explicitly specify a width or height." El propio changelog advierte explícitamente: "If custom templates are used with the new LaTeX writer, they will have to be updated to include the new `\pandocbounded` macro, or an error will be raised because of the undefined macro." — esto describe EXACTAMENTE el fallo con la plantilla minimalista de R-exams.

Dato adicional: pandoc **3.8.1** agregó "LaTeX reader: Ignore `\pandocbounded` (#11140)" — pero eso afecta al LECTOR de LaTeX, no resuelve el problema de ESCRITURA que produce este error.

El bug es de pipeline: HTML/DOCX no pasan por LaTeX, así que no fallan; PDF/NOPS sí.

---

## Actualización 2026-08-15 — R/exams ≥ 2.4-1 ya DEFINE el macro

**El modo de fallo original ya no se reproduce con la versión instalada.** Desde **exams 2.4-1**
todas las plantillas LaTeX del paquete traen la definición no-op:

```latex
\providecommand{\pandocbounded}[1]{#1}
```

Presente en `exams/tex/{plain,plain8,exam,form,solution,plain-highlight}.tex` (verificado en la
2.4-2 instalada). El propio `NEWS.md` del paquete lo declara: *«Added support for
`\pandocbounded{}` (as produced by pandoc starting from version 3.2.1) in all current LaTeX
templates as well as `exams2nops()`. Currently, `\pandocbounded{}` does not anything in the exams
templates so that the size of the graphics in PDF output still has to be controlled in one of the
previously available ways.»*

Medido el 2026-08-15 sobre un fixture con `cat("![](fig.png)\n")` **sin** width y `exams2pdf()`:
el `.tex` contiene `\pandocbounded{\includegraphics[keepaspectratio]{fig.png}}` y **compila** (PDF
de 15 KB con la imagen dentro).

**Qué cambia y qué NO cambia:**

- **Deja de ser un fallo de compilación** con las plantillas del paquete. El `Undefined control
  sequence` sigue vivo únicamente con **plantilla propia** (`exams2pdf(template=...)`) o con
  exams < 2.4-1.
- **La regla sigue vigente** por la razón que el propio `NEWS` señala: el macro es un **no-op**, así
  que **no controla el tamaño**. Sin `{width=...}`, la imagen sale al tamaño que dicte
  `\setkeys{Gin}` de la plantilla, no el que el ejercicio pretende. El atributo sigue siendo
  obligatorio.
- **Consecuencia operativa en el hook:** la FASE 2I usaba `grep -l 'pandocbounded'`, que casa esa
  **definición** y por tanto marcaba `ERROR 16` en **cualquier** `.tex`, tuviera o no imágenes.
  Medido sobre `pendiente-rectas-paralelas-n4` (ejercicio sin una sola imagen): **6 de sus 8 `.tex`
  disparaban; usos reales = 0**. Corregido el 2026-08-15: el detector exige ahora
  `\pandocbounded{` (uso) **y ausencia** de `\providecommand`/`\newcommand`/`\def` del macro en el
  conjunto del render. Test: `tests/testthat/test_fase2i_pandocbounded_detector.R`.

---

## Patrones Aceptados

### ✅ Patrón A — `cat()` con atributo width (RECOMENDADO)

```r
` ``{r mostrar_grafico, echo=FALSE, results='asis'}
cat("![](grafico_equilibrio.png){width=80%}\n")
` ``
```

**Razón**: cuando pandoc detecta `{width=...}`, NO usa `\pandocbounded`; emite directo `\includegraphics[width=0.8\textwidth]{...}`.

Validado en producción: `diagrama_venn_encuesta_metacognitivo_*.Rmd` línea ~1070.

### ⛔ Patrón B — RETIRADO el 2026-08-15: PIERDE LA IMAGEN EN EL PDF

```r
# ⛔ NO USAR — medido: la imagen NO llega al PDF
` ``{r mostrar_grafico, echo=FALSE, results='asis'}
if (knitr::is_latex_output()) {
  cat("\\includegraphics[width=0.8\\textwidth]{grafico_equilibrio.png}")
} else {
  cat('<img src="grafico_equilibrio.png" width="80%" alt="" />')
}
` ``
```

**Por qué falla, con dos causas encadenadas:**

1. `knitr::is_latex_output()` es **FALSE también en `exams2pdf()` y `exams2nops()`** — R/exams
   teje siempre a Markdown y delega la conversión en pandoc, así que durante el `knit` no hay
   destino LaTeX que detectar. La rama LaTeX **nunca se ejecuta**. Ver regla #21 §Familia 2 para
   la medición completa sobre los cinco pipelines.
2. Se ejecuta por tanto la rama `else`, que emite **HTML crudo**, y el escritor LaTeX de pandoc
   **descarta el HTML crudo**.

Medido el 2026-08-15 sobre un fixture renderizado con `exams2pdf()`: el `.tex` resultante
contiene **0 `\includegraphics` y 0 `<img>`** — la figura desaparece sin error, sin warning y sin
que ninguna fase del arsenal lo note. El PDF compila (9 KB frente a los 15 KB del mismo ejercicio
con la imagen). **Es un fallo silencioso**, peor que uno ruidoso.

### ✅ Patrón B' — sustituto medido: emitir AMBOS, sin condicional

Si de verdad hacen falta markups distintos por formato, no hay que ramificar: se emiten los dos y
**pandoc enruta por tipo de bloque**, descartando el que no corresponde a su destino.

```r
` ``{r mostrar_grafico, echo=FALSE, results='asis'}
cat("\\includegraphics[width=0.8\\textwidth]{grafico_equilibrio.png}\n")  # sobrevive solo en LaTeX
cat('<img src="grafico_equilibrio.png" width="80%" alt="" />\n')          # sobrevive solo en HTML
` ``
```

Equivalente con bloques raw explícitos (mismo resultado medido, preferible si el contenido es
estático): ` ```{=latex} ` … ` ``` ` y ` ```{=html} ` … ` ``` `.

Verificado sobre fixture: PDF → 1 `\includegraphics`, 0 `<img>`; HTML → 1 `<img>`,
0 `includegraphics`. Cero fuga cruzada en ambos sentidos.

**Cuándo usarlo**: solo si necesitas markups realmente distintos por formato. Para el caso normal
—una imagen con un tamaño— el **Patrón A** basta y es más simple.

### ✅ Patrón C — `knitr::include_graphics()` con `out.width`

```r
` ``{r mostrar_grafico, echo=FALSE, out.width="80%"}
knitr::include_graphics("grafico_equilibrio.png")
` ``
```

**Cuándo usarlo**: chunk completo dedicado a una sola imagen, sin necesidad de `cat()`.

### ✅ Patrón D — Markdown directo CON atributo (solo si la imagen es estática)

```markdown
![](logo_institucional.png){width=30%}
```

**Cuándo usarlo**: imagen NO depende de variables R (no es generada dinámicamente). Si `r ...` aparece cerca, prefiere Patrón A.

---

## Patrones PROHIBIDOS

### ❌ Markdown sin width

```markdown
❌ ![](grafico.png)
❌ ![texto alternativo](g.png)
❌ ![](`r ruta_grafico`)
```

### ❌ `cat()` sin width

```r
❌ cat("![](g.png)\n")
❌ cat(paste0("![](", ruta, ")"))
```

### ❌ `cat()` con width pero ANTES del nombre

```r
❌ cat("{width=80%}![](g.png)\n")   # sintaxis pandoc inválida
```

---

## Defensa Automática

### Capa 1 — Hook PreToolUse (escáner pre-write)

`pre-write-rmd-gate.sh` (futuro) detecta el patrón prohibido en archivos `.Rmd` antes de Write/Edit:

```bash
# Detectar Markdown sin width en .Rmd nuevos/modificados
if grep -nE '!\[[^]]*\]\([^)]+\.png\)(?!\{)' archivo.Rmd; then
  echo "❌ BLOQUEADO: imagen Markdown sin atributo {width=...}"
  echo "   Ver .claude/rules/markdown-imagenes-pdf.md"
  exit 2
fi
```

### Capa 2 — Hook PostToolUse FASE 2I (post-render)

`post-exams2-validation.sh` agrega FASE 2I tras 2H:

1. Localiza el `.tex` generado por `exams2pdf()` en `output_pdf/` o `dir=`.
2. Ejecuta `grep -c '\pandocbounded' archivo.tex`.
3. Si retorna ≥1 → ERROR bloqueante con instrucciones de fix.

### Capa 3 — Test de regresión

`tests/testthat/test_pandocbounded_y_solution_coherence.R`:

- Itera sobre todos los `.Rmd` en `02-En-Desarrollo/` y `03-En-Produccion/`.
- Aplica regex de detección de Markdown sin width.
- Reporta como falla cualquier archivo con el patrón prohibido.

### Capa 4 — Detractor (FASE 2C)

El dominio `codigo_rexams` del detractor busca el patrón y lo reporta como objeción ALTA.

---

## Excepciones (NINGUNA)

No hay excepciones a esta regla. Si un caso especial requiere `![](file.png)` sin width, primero documentarlo en un ADR y obtener aprobación humana antes de modificar esta regla.

---

## Tests Asociados

| Test | Verifica |
|---|---|
| `tests/testthat/test_pandocbounded_y_solution_coherence.R` | Detección estática de `![](*.png)` sin width en .Rmd existentes |
| `tests/testthat/test_renderizado_4_formatos.R` | Renderizado real de los 4 formatos para muestra representativa |

---

## Referencias

- Error #16 en `.claude/docs/patrones-errores-conocidos.md`
- Pandoc changelog 3.2.1 (2024-06-24) — https://github.com/jgm/pandoc/blob/main/changelog.md — introducción de `\pandocbounded` (#9660)
- Pandoc changelog 3.8.1 (2025-09-29) — mismo changelog — "LaTeX reader: Ignore `\pandocbounded` (#11140)" (solo afecta al lector, no al escritor)
- Fecha de consulta de estas fuentes: 2026-07-28
- TeX StackExchange: https://tex.stackexchange.com/questions/665980/pandocbounded-undefined
- Ejemplo funcional validado: `diagrama_venn_encuesta_metacognitivo_*.Rmd` línea 1070

---

**Versión:** 1.2
**Fecha:** 2026-08-15 (v1.2 — **Patrón B RETIRADO**: medido que pierde la imagen en el PDF, por dos
causas encadenadas (`is_latex_output()` es FALSE también en `exams2pdf`/`exams2nops`, y pandoc
descarta el HTML crudo al escribir LaTeX); sustituto **Patrón B'** verificado. Nueva sección
«Actualización 2026-08-15»: R/exams ≥ 2.4-1 ya define `\pandocbounded` como no-op, así que el fallo
de compilación no se reproduce con las plantillas del paquete — la regla sigue vigente por el
**tamaño**, no por la compilación — y el detector de la FASE 2I quedó corregido para distinguir
definición de uso; v1.1 2026-07-28 — precisión factual: `\pandocbounded` se introdujo en pandoc 3.2.1 [2024-06-24], no en un "3.x" genérico; fuentes verificadas con changelog oficial; v1.0 2026-05-03)
**Estado:** ACTIVO Y OBLIGATORIO
**Excepciones:** NINGUNA
**Aplica a:** todo archivo `.Rmd` que renderice imágenes en PDF/NOPS.
