# Regla #22 — Diversidad Sustantiva (versión compacta)

> Texto íntegro —origen, tablas de medición, §P7-A..F completos, historial v1.0→v1.8—: `.claude/docs/reglas/diversidad-sustantiva.md`. Leerlo antes de diseñar una batería §P7, rediseñar ramas o declarar un override.

**Principio.** La diversidad debe ser SUSTANTIVA: los datos y el contenido de la respuesta correcta cambian entre versiones. "N versiones únicas del render" mide el envoltorio (contexto, orden, reflexiones), no la sustancia. Sin excepciones.

## Patrones prohibidos
- **P1** Parámetros que determinan la clave como literales fijos → aleatorizar (`sample`/`runif`/…).
- **P2** PNGs de opciones copiados con `file.copy()` → generarlos por versión.
- **P3** Reportar diversidad sólo por conteo de renders.
- **P4** Clave siempre en la misma posición/orientación/cuadrante (el validador por valor da PASS igual). Sortear la orientación de la escena y reflejarla en el texto.
- **P4-bis** Veredicto de la clave invariante ("No, porque…" en el 100 %) aunque el valor varíe → sortear si la afirmación evaluada es verdadera. Tras añadir una clave alternativa: guardas anti-colisión contra TODAS las claves, revisar distractores escritos para la clave única, y medir H1/H2 **por rama** (el agregado no acredita).
- **P5** Distractor posicional outlier (giro 180°, longitud única) → cuasi-acierto que difiera sólo en la dimensión evaluada.
- **P6** Fuga por metadato no visual (nombre de archivo, id) → nombres neutrales POST-mezcla; verificar con `exams2moodle()` + grep del XML.
- **P7** Batería de eliminación sin cierre por familias (magnitud, divisibilidad, signo, posición, formato, léxico): *una batería incompleta no mide «sin señal», mide «sin sonda»*. Helper `.claude/scripts/bateria_eliminacion.R`; veredicto por **exceso sobre techo nulo**, no por tasa: ≤ +2 pp sin canal · +2 a +8 pp zona gris · ≥ +8 pp canal. Incluir al menos una regla **relacional** entre opciones (§P7-E); una regla con aplicabilidad 0 % no cubre nada.
  - **§P7-A** Aceptable ≤ +5,3 pp (vara oficial, `bateria_referencia_icfes.R`, usar `nlast()` no `n1()`); obliga sólo > +8 pp.
  - **§P7-B** Margen < 15 % = inexplotable, no es defecto.
  - **§P7-C** La batería se congela al inicio; si se amplía, re-medir todo el histórico.
  - **§P7-D** Máximo 3 pasadas; luego cierre con residuo declarado. Tras cada mejora de diagnosticidad, verificar que la clave sigue siendo verdadera.
  - **§P7-E** Medir todo canal también sobre la instancia canónica (enumeración exacta). Aceptar un canal > +8 pp es un OVERRIDE humano firmado (H-5); la batería queda en rojo.
  - **§P7-F** Antes de abrir otra rama, enumerar el espacio de diseño: puede haber una sola combinación viable (canal estructural → override, no rediseño).

## Defensa automática
- `Rscript .claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 100` (orquestador paso 9): `ERR_DIV_COSMETICA` (exit 1, **bloqueante**), `WARN_DIV_BAJA`, `WARN_DIV_INDET`.
- Hook FASE 2N: `WARN_DIV_ESTATICA` (file.copy de PNGs o sin funciones aleatorias en data_generation).
- `validar_diagnosticidad.R` sondas H1/H2/H3/H3b (H3b releva a H2/H3 cuando el prefijo es uniforme; la ceguera se declara).
- §P7: veredictos `PASS` · `BLOQUEA` · `SIN_COBERTURA` · `NO_CONCLUYENTE` (no es PASS) · `UMBRAL_DEGENERADO`.
- Tests: `test_diversidad_sustantiva.R`, `test_bateria_eliminacion.R`.

**Versión:** 1.8 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
