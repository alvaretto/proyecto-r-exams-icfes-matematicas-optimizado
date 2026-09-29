# Regla #20 — Tablas Markdown + pandoc ≥ 3.8.1: guard del contador `none` (versión compacta)

> Texto íntegro —comportamiento por versión de pandoc, fuentes, incidente RStudio—: `.claude/docs/reglas/markdown-tablas-pandoc.md`.

**Principio.** Todo `.Rmd` con tabla Markdown (`kable(format = "markdown")` o `cat("| ...")`) incluye, justo después del encabezado `Question`:

````
```{=latex}
\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother
```
````

Por qué: pandoc ≥ 3.8.1 (RStudio bundlea 3.8.3; la terminal usa 3.6) envuelve `longtable` en `\def\LTcaptype{none}` y la plantilla de R/exams no define ese contador → `No counter 'none' defined`. La guardia `\@ifundefined` evita "already defined" en `exams2nops()` multi-ítem; `{=latex}` no ensucia HTML/DOCX.

**Defensa:** skills/orquestadores lo insertan; hook FASE 2K (`ERR_TABLA_NONE`, bloqueante); `test_markdown_tablas_none_guard.R`; validar también con el pandoc de RStudio (`RSTUDIO_PANDOC=/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64`). Error 21.

**Versión:** 1.1 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
