# Regla #19 — Solution Letter-Independence (versión compacta)

> Texto íntegro —incidente 2026-05-12, evidencia de `read_exercise.R`, regex de cada capa, migración de legado—: `.claude/docs/reglas/solution-letter-independence.md`.

**Principio.** La sección `Solution` NUNCA identifica la opción correcta por letra/posición (A-D). La identifica por contenido (`descripcion_corta`), código del error o etiqueta semántica. Razón: `shuffle_choice()` de R/exams permuta Answerlist, feedback por opción y `exsolution`, pero **no la prosa** de Solution; y Moodle re-mezcla por su cuenta.

**Prohibido en Solution:** P1 `` `r letra_correcta` `` / `` `r letras[...]` `` en encabezados; P2 la letra interpolada en prosa; P3 `cat("**Opción ", l, ...)`; P4 literal "Opción A/B/C/D".

**Aceptado:** encabezado sin letra; "La respuesta correcta es la que afirma: …"; iterar distractores por `err$codigo — err$nombre`. `letra_correcta` puede existir sólo para logs/asserts internos.

**Defensa:** gate pre-write; hook FASE 2J (`ERR_SOL_LETRA_R`, `ERR_SOL_LETRA_CAT`, `ERR_SOL_LETRA_LITERAL`, bloqueantes); `tests/testthat/test_letter_independence.R`; detractor (objeción CRÍTICA). Vecino no cubierto por la 2J: la prosa tampoco debe enumerar las opciones "en orden".

**Versión:** 1.1 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
