# Regla #20 — Tablas Markdown + pandoc ≥ 3.8.1: guard del contador `none` (versión compacta)

> Texto íntegro —comportamiento por versión de pandoc, fuentes, incidente RStudio—: `.claude/docs/reglas/markdown-tablas-pandoc.md`.

**Principio.** Todo `.Rmd` con tabla Markdown (`kable(format = "markdown")` o `cat("| ...")`) incluye, justo después del encabezado `Question`:

````
```{=latex}
\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother
```
````

Por qué: pandoc ≥ 3.8.1 (en este equipo, 3.10 en RStudio y 3.11 en la terminal) envuelve `longtable` en `\def\LTcaptype{none}` y la plantilla de R/exams no define ese contador → `No counter 'none' defined`. La guardia `\@ifundefined` evita "already defined" en `exams2nops()` multi-ítem; `{=latex}` no ensucia HTML/DOCX.

**Desde `exams` 2.4-4** las plantillas del paquete definen `{none}` y `\LTcaptype`. El guard sigue siendo obligatorio: las copias de `pcielo*.tex` y `solpcielo*.tex` en cada ejercicio no lo definen, la versión instalada puede ser anterior y `\@ifundefined` no choca con la definición nueva.

**Defensa:** skills/orquestadores lo insertan; hook FASE 2K (`ERR_TABLA_NONE`, bloqueante); `test_markdown_tablas_none_guard.R`; validar también con el pandoc de RStudio (`RSTUDIO_PANDOC=/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64`). Error 21.

**Versión:** 1.2 (2026-11-04: `exams` 2.4-4 y plantillas propias) · compacta desde 2026-09-29 · Excepciones: NINGUNA.
