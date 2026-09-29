# Regla #2 — Flujo B (Graficador Experto) Obligatorio (versión compacta)

> Texto íntegro —criterios de detección, archivos de estado requeridos, mensaje de bloqueo—: `.claude/docs/reglas/flujo-b-obligatorio.md`.

**Principio.** Si el ejercicio tiene gráficos —en el enunciado, en las opciones o en la solución (barras, líneas, dispersión, circular, geometría, plano cartesiano, tablas que requieren visualización)— el Flujo B es OBLIGATORIO. Sin excepciones, aunque el gráfico parezca simple.

- La decisión se toma MIRANDO el JPG del ítem (regla #24 H-1) y se registra en `ejercicio_state.json` (`flujo_b.requerido`); si hay duda, preguntar al usuario.
- `/analizar-icfes` declara la decisión; `/generar-schoice` y `/generar-cloze` bloquean si `requerido = true` y el Flujo B no está completo (gate de la regla #16).
- El Flujo B se ejecuta con `/auto-refinar-grafico` siguiendo la regla #3 (TikZ → Python → R, ≥ 98 %, el usuario elige) y verifica las 5 coherencias.

**Version**: 1.0 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
