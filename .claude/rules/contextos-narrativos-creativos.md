# Regla #11 — Contextos Narrativos Creativos (versión compacta)

> Texto íntegro —pool de ejemplo con 8 plantillas completas—: `.claude/docs/reglas/contextos-narrativos-creativos.md`.

**Principio.** Contextos variados, naturales y no mecánicos. PROHIBIDO el patrón repetitivo "Un(a) [oficio], [nombre], registró…".

- Mínimo **6 plantillas** con al menos **5 tipos** de estructura: acción en curso, descubrimiento, situación problema, narración periodística, diálogo implícito, perspectiva del estudiante, contexto institucional, pregunta retórica.
- Cada plantilla es una **función** `plantilla = function(prot, n)`, no un string fijo; legible de forma independiente.
- "registró"/"recopiló" en ≤ 25 % de las plantillas.
- Protagonistas de al menos 2 categorías (personas, instituciones, eventos, documentos, encuestas).
- Deben sonar naturales leídas en voz alta. Uso: `ctx <- contextos[[sample(length(contextos), 1)]]; ctx$plantilla(sample(ctx$protagonistas, 1), n)`.

El detractor (dominio pedagógico) verifica estos puntos. Aplica a `.Rmd` generados desde 2026-02-10.

**Versión**: 1.0 · compacta desde 2026-09-29.
