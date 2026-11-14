# Reglas para Código R/Markdown (versión compacta)

> Texto íntegro —ejemplos antes/después de cada regla, excepciones históricas de `exshuffle`, búsqueda de ejemplos funcionales—: `.claude/docs/reglas/codigo-rmd.md`.

**Antes de editar un `.Rmd`:** entender el error, verificar la solución en ejemplos funcionales (`A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/`, inmutables), no hacer cambios experimentales, validar los 4 formatos (HTML, PDF, DOCX, NOPS).

## Nunca
1. `include_tikz()` con `markup="tex"` ni ramificar con `is_latex_output()`: esa función es FALSE también en `exams2pdf()`/`exams2nops()` (RAMA MUERTA; pierde la figura en PDF). Usar `include_tikz(..., format = match_exams_device(), markup = "markdown")` (PDF vectorial en `exams2pdf()`/`exams2nops()`; PNG en Word y HTML; SVG o PNG en Moodle; medido el 2026-11-13) o `format = "png"`.
2. Mezclar Python/R sin validar ambos.
3. Menos de **100 versiones únicas sobre 150 renders** (`exams2html(n = 150)`). Umbral único desde 2026-11-13: sustituye a «250 de 300» y a «200 de 200».
4. Omitir alguno de los 4 formatos. 5. Modificar ejemplos funcionales.
6. `exshuffle: FALSE` salvo SCHOICE con opciones gráficas PNG (regla #4); la Solution es letter-independent (regla #19). En un CLOZE impreso con gráficas rotuladas, el `FALSE` va en la llamada `exams2pdf()` del semillero, no en el `.Rmd` (regla #26).
7. `_neg_` sin su test (regla #10).
8. Errores conceptuales sin `precondicion`; `calcula()` debe `stop()` fuera de contexto.
9. Comparar `calcula()` sin guardia `is.na()`.
10. `set.seed()` en tests sin guardar/restaurar `.Random.seed`.
13. Imagen Markdown sin `{width=...}` (regla #18).
14. `##ANSWERi##` fuera de orden o faltantes en CLOZE: uno por tipo de `exclozetype`, justo tras su parte.
15. Tejer el `.Rmd` con el botón Knit de RStudio: le antepone YAML y anula una firma atada al md5 (regla #26).

## 5 coherencias (antes de aprobar)
Semántica · Visual-Texto · Matemática · Código · General (incluye `DOK ≥ 3 ⇒ Nivel ≥ 3`).

**Metadatos obligatorios:** `exname`, `extype`, `exsolution`, `exshuffle`, `extol` + `exextra[Type|Competencia|Componente|Afirmacion|Evidencia|Nivel]`.

**Helpers compartidos:** `include_supplement("helper.R")` + `source("helper.R")`, nunca `source()` por ruta relativa (`.claude/docs/AUTOCONTENCION_REXAMS.md`).

Compacta desde 2026-09-29.
