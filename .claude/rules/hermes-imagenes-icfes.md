# Regla #24 — Hermes: triaje y fidelidad de figuras de cuadernillo (versión compacta)

> Texto íntegro —incidentes Q053/Q067/Q143/q090/q038, motor `$MOTOR_HERMES`, integración por paso—: `.claude/docs/reglas/hermes-imagenes-icfes.md`. Estrategia completa: `.claude/skills/hermes-imagenes/SKILL.md`.

**Principio.** Antes de reproducir cualquier figura de un ítem escaneado, MIRAR el recorte del JPG (Read). La descripción textual sobre-clasifica. Aquí las figuras son vectoriales (regla #3); de Hermes se importan sus gates.

- **H-1 Gate visual:** `flujo_b.requerido` se justifica con lo VISTO en el JPG, no con el `.md`.
- **H-2 ⛔ La trampa ES la pregunta:** reproducir la figura con sus errores deliberados; jamás normalizar. Screening: "cuál es el error", "misma información", "no coincide", "presenta mal", "inconsistente", "¿es correcta la gráfica/tabla?", "X afirma que…".
- **H-3 Fidelidad por tipo:** barras = 3 fuentes; tablas = celda a celda; geometría = **inventario bidireccional de rótulos** (atrapa el rótulo agregado); curvas = checklist dirigido (incl. estilo de línea).
- **H-4 Ancla en el número IMPRESO** y crop al borde del contenido; celda ilegible = falla; crop sin figura = PARAR. Dos fuentes derivadas que coinciden no son confirmación.
- **H-5 Asimetría:** endurecer es autónomo; **relajar nunca** (requiere humano).

Excepciones: ninguna para H-2 y H-5.

**Versión:** 1.1 · compacta desde 2026-09-29.
