# Regla #18 — Imágenes Markdown para PDF: `{width=...}` obligatorio (versión compacta)

> Texto íntegro —changelog de pandoc 3.2.1, medición con exams 2.4-2, detector FASE 2I—: `.claude/docs/reglas/markdown-imagenes-pdf.md`.

**Principio.** Toda imagen emitida vía Markdown (directo o `cat()`) lleva `{width=...}`. Sin excepciones.

- Con exams ≥ 2.4-1 `\pandocbounded` está definido como **no-op**, así que ya no rompe la compilación con las plantillas del paquete (sí con plantilla propia); la regla sigue vigente porque sin width **no se controla el tamaño**.
- ✅ **A:** `cat("![](g.png){width=80%}\n")` · ✅ **B':** emitir `\includegraphics[...]` Y `<img ...>` sin condicional (pandoc descarta el que no corresponde) · ✅ **C:** `knitr::include_graphics()` con `out.width` · ✅ **D:** `![](logo.png){width=30%}` estático.
- ⛔ **Patrón B retirado:** `if (knitr::is_latex_output())` — es FALSE también en `exams2pdf()`/`exams2nops()` y la rama HTML se descarta en LaTeX: la imagen desaparece del PDF sin error.
- ❌ `![](g.png)`, `cat("![](g.png)\n")`, `{width}` antes del nombre.

**Defensa:** hook FASE 2I (distingue uso de definición del macro, `test_fase2i_pandocbounded_detector.R`); `test_pandocbounded_y_solution_coherence.R`; detractor (objeción ALTA). Error 16.

**Versión:** 1.2 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
