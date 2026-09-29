# Regla #3 — Workflow Secuencial del Graficador Experto (versión compacta)

> Texto íntegro —estados JSON, plantillas de presentación, manejo de errores, ejemplo de flujo—: `.claude/docs/reglas/graficador-secuencial.md`.

**Principio.** SIEMPRE se generan las TRES versiones, en orden **TikZ → Python (reticulate) → R (ggplot2)**, iterando AUTOMÁTICAMENTE cada una hasta **≥ 98 %** de similitud (máximo 10 iteraciones, luego escalar). El **usuario decide** cuál usar.

- Prohibido omitir un lenguaje, decidir por el usuario, detenerse antes del 98 % o pedir aprobación intermedia.
- Cada versión es dinámica (datos interpolados desde R) y verifica las 5 coherencias.
- Al final: tabla comparativa (similitud, iteraciones, ventajas) + previews de las tres + pregunta explícita; esperar la selección antes de generar el `.Rmd`.
- Si alguna no alcanza 98 %: informar la mejor similitud y ofrecer opciones.

Comandos: `/auto-refinar-grafico`, `/estado-graficador`.

**Versión**: 2.0 · compacta desde 2026-09-29.
