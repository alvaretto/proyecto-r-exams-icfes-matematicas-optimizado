# Regla #16 — Workflow State Enforcement (versión compacta)

> Texto íntegro —mensajes del gate, campos extra por paso, flujo ejemplo—: `.claude/docs/reglas/workflow-state-enforcement.md`.

**Principio.** Todo `.Rmd` en `01-En-PreDesarrollo/` o `02-En-Desarrollo/` pasa por el workflow completo. El gate `.claude/hooks/pre-write-rmd-gate.sh` (PreToolUse) BLOQUEA (exit 2) la escritura si en `ejercicio_state.json` falta `analisis_icfes`, si `flujo_b.requerido` es `null`, o si es `true` y el Flujo B no está completo. Se permite si `generacion_rmd.completado = true` (corrección), fuera de esos directorios, o en `Ejemplos-Funcionales/`. Falla abierto ante JSON inválido.

**CLI:** `.claude/scripts/workflow-state.sh init <dir> --tipo schoice|cloze` · `complete <dir> <paso> [--key value]` · `check` · `status` · `next`. Schema: `.claude/schemas/ejercicio_state.schema.json`.

**11 pasos:** 1 `analisis_icfes` · 2 `flujo_b` · 3 `generacion_rmd` · 4 `retroalimentacion` · 5 `renderizado_4_formatos` · 6 `arsenal_post_render` · 7 `detractor_fase2c` · 8 `coherencias_5` · 9 `validar_diversidad` · 10 `validar_icfes` · 11 `aprobacion_usuario`. Los 4-11 no tienen gate mecánico pero son obligatorios; se sellan sólo al ejecutarse (auditar por artefacto, no por flag).

**`flujo_b.requerido = null`:** PREGUNTAR al usuario si requiere gráficos; nunca asumirlo.

**Versión**: 1.0 · compacta desde 2026-09-29.
