# Regla #4 — Gráficos Como Opciones Individuales (versión compacta)

> Texto íntegro —código completo del patrón, catálogo de distractores por formato, cambios v3→v6—: `.claude/docs/reglas/graficos-como-opciones.md`.

**Principio.** Si las opciones SCHOICE son gráficos, cada una es un **PNG separado** referenciado en el Answerlist. Prohibido `grid.arrange()` o una grilla única.

## Obligatorio
- **Sin títulos con letras** en los gráficos: `labs(title = NULL)`.
- Mezcla interna con `sample()` + `exshuffle: FALSE` (R-exams no reescribe la prosa de Solution); la Solution identifica la correcta por contenido (regla #19).
- Nombres neutrales `diagrama_a.png`…`diagrama_d.png` asignados **POST-mezcla** (Error 25: un nombre semántico es invisible en HTML/PDF pero visible en el XML de Moodle).
- **Sufijo por versión (v6.1):** `diagrama_a_<id>.png`, con `<id>` hexadecimal sorteado al final de `data_generation` y **el mismo** en todas las figuras de la versión (enunciado y Solution incluidos). Sin él, con `exams` < 2.4-3, `exams2pdf/exams2nops(rep(archivo, n))` muestran en todas las preguntas las figuras de una sola versión: R/exams renombra los duplicados, pero no reescribe la referencia si la imagen va seguida de `&#8203;`. Desde 2.4-3 solo persiste con dos imágenes del mismo nombre en una línea; el sufijo se conserva porque la versión instalada puede ser anterior. Nunca un sufijo semántico. Helper: `renombrar_opciones_neutral(..., sufijo = fig_id)`.
- Answerlist: `cat("* ![](diagrama_a_", fig_id, ".png){width=60%}\n", sep = "")` o `![](diagrama_a_`r fig_id`.png){width=60%}` (regla #18).
- Escala compartida calculada sobre TODAS las opciones; excluir errores fuera de rango (EST-BOX-01).
- **Formato equilibrado**: al menos 2 opciones comparten el formato de la correcta (ideal 2+2); `stopifnot` en data_generation.

## Verificación obligatoria
```bash
Rscript -e 'library(exams); exams2moodle("archivo.Rmd", n = 1, dir = "moodle_output")'
grep -ohE 'diagrama_[^."/]+\.png' moodle_output/*.xml | sort -u | grep -vE '^diagrama_[a-z](_[0-9a-f]+)?\.png$'   # debe salir vacío
```
El grep previo `diagrama_[a-z]+\.png` no ve los nombres con sufijo: su salida vacía no probaba nada.
Con varias copias: `exams2pdf(rep("archivo.Rmd", 3), ...)` y `pdfimages -list` deben dar imágenes distintas por pregunta.
Ver `codigo-rmd.md` regla #6 y NOMENCLATURA en `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`. Ejemplo funcional: `Ejemplos-Funcionales-Rmd/estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`.

**Impresión:** varias preguntas en un PDF, NOPS en oficio y gráficas rotuladas de un CLOZE en papel siguen la regla #26 (`material-aula.md`).

**Versión**: 6.3 (2026-11-09: la impresión remite a la regla #26) · compacta desde 2026-09-29.
