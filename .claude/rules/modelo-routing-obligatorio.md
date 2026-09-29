# Regla #14 — Routing Obligatorio de Modelos (versión compacta)

> Texto íntegro —plantilla de delegación, antipatrones—: `.claude/docs/reglas/modelo-routing-obligatorio.md`. La regla global de `~/.claude/CLAUDE.md` prevalece si difiere.

**Principio.** Antes de ejecutar un skill, leer su `model_recommendation`: `opus` o ausente → inline; `sonnet`/`haiku` → delegar con `Task(subagent_type="general-purpose", model=...)` e incluir en el prompt ruta, estado del workflow y resultados previos.

- **Opus (inline):** generar-schoice, generar-cloze, skill-retroalimentacion, analizar-icfes, validar-pedagogico, skill-detractor.
- **Sonnet:** generar-codigo-tikz/python/r, comparar-similitud-visual, refinar-codigo-grafico, diagnosticar-errores, corregir-graficos, corregir-error-imagen, analizar-imagen-grafica.
- **Haiku:** validar-renderizado, validar-diversidad, validar-icfes, validar-coherencia, gestionar-estado-graficador, transferir-conocimiento-grafico, promover-ejercicio.
- Los agentes con `model:` en su frontmatter ya tienen routing nativo.
- Excepciones: el skill necesita imágenes ya en el contexto padre, es trivial, o el usuario pide ejecutarlo inline.

**Versión**: 1.0 · compacta desde 2026-09-29.
