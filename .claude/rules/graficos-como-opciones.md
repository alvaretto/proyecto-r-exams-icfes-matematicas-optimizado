# Regla #4 — Gráficos Como Opciones Individuales (versión compacta)

> Texto íntegro —código completo del patrón, catálogo de distractores por formato, cambios v3→v6—: `.claude/docs/reglas/graficos-como-opciones.md`.

**Principio.** Si las opciones SCHOICE son gráficos, cada una es un **PNG separado** referenciado en el Answerlist. Prohibido `grid.arrange()` o una grilla única.

## Obligatorio
- **Sin títulos con letras** en los gráficos: `labs(title = NULL)`.
- Mezcla interna con `sample()` + `exshuffle: FALSE` (R-exams no reescribe la prosa de Solution); la Solution identifica la correcta por contenido (regla #19).
- Nombres neutrales `diagrama_a.png`…`diagrama_d.png` asignados **POST-mezcla** (Error 25: un nombre semántico es invisible en HTML/PDF pero visible en el XML de Moodle).
- Answerlist: `cat("* ![](diagrama_a.png){width=60%}\n")` (regla #18).
- Escala compartida calculada sobre TODAS las opciones; excluir errores fuera de rango (EST-BOX-01).
- **Formato equilibrado**: al menos 2 opciones comparten el formato de la correcta (ideal 2+2); `stopifnot` en data_generation.

## Verificación obligatoria
```bash
Rscript -e 'library(exams); exams2moodle("archivo.Rmd", n = 1, dir = "moodle_output")'
grep -oE 'diagrama_[a-z]+\.png' moodle_output/*.xml | sort -u   # sólo letras
```
Ver `codigo-rmd.md` regla #6 y NOMENCLATURA en `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`. Ejemplo funcional: `Ejemplos-Funcionales-Rmd/estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`.

**Versión**: 6.0 · compacta desde 2026-09-29.
