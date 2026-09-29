# Reglas para Código R/Markdown (versión compacta)

> Texto íntegro —ejemplos antes/después de cada regla, excepciones históricas de `exshuffle`, búsqueda de ejemplos funcionales—: `.claude/docs/reglas/codigo-rmd.md`.

**Antes de editar un `.Rmd`:** entender el error, verificar la solución en ejemplos funcionales (`A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/`, inmutables), no hacer cambios experimentales, validar los 4 formatos (HTML, PDF, DOCX, NOPS).

## Nunca
1. `include_tikz()` con `markup="tex"` ni ramificar con `is_latex_output()`: esa función es FALSE también en `exams2pdf()`/`exams2nops()` (RAMA MUERTA; pierde la figura en PDF). Usar `include_tikz(..., format = "png", markup = "markdown")`.
2. Mezclar Python/R sin validar ambos.
3. Menos de 200 versiones únicas (`exams2html(n = 200)`).
4. Omitir alguno de los 4 formatos. 5. Modificar ejemplos funcionales.
6. `exshuffle: FALSE` salvo SCHOICE con opciones gráficas PNG (regla #4); la Solution es letter-independent (regla #19).
7. `_neg_` sin su test (regla #10).
8. Errores conceptuales sin `precondicion`; `calcula()` debe `stop()` fuera de contexto.
9. Comparar `calcula()` sin guardia `is.na()`.
10. `set.seed()` en tests sin guardar/restaurar `.Random.seed`.
13. Imagen Markdown sin `{width=...}` (regla #18).
14. `##ANSWERi##` fuera de orden o faltantes en CLOZE: uno por tipo de `exclozetype`, justo tras su parte.

## 5 coherencias (antes de aprobar)
Semántica · Visual-Texto · Matemática · Código · General (incluye `DOK ≥ 3 ⇒ Nivel ≥ 3`).

**Metadatos obligatorios:** `exname`, `extype`, `exsolution`, `exshuffle`, `extol` + `exextra[Type|Competencia|Componente|Afirmacion|Evidencia|Nivel]`.

**Helpers compartidos:** `include_supplement("helper.R")` + `source("helper.R")`, nunca `source()` por ruta relativa (`.claude/docs/AUTOCONTENCION_REXAMS.md`).

Compacta desde 2026-09-29.
