# Ciclo de Validación y Corrección Automática (versión compacta)

> Texto íntegro —diagramas del ciclo, plantillas de reporte, patrón correcto completo, v5.0—: `.claude/docs/reglas/ciclo-validacion.md`.

**Regla crítica:** tras CUALQUIER cambio a un `.Rmd`: re-renderizar, mostrar preview, verificar las 5 coherencias. Nunca asumir que un cambio produjo el efecto sin verificación visual.

```
FASE 1  Renderizado HTML/PDF/DOCX/NOPS
FASE 2A Validación matemática [hook post-exams2-validation.sh]  → si ERRORES, corregir
FASE 2B Preview PDF→PNG [hook]  → Claude hace Read() de cada PNG
FASE 2C Detractor [obligatoria, regla #9]  → CRÍTICAS/ALTAS: corregir y volver a FASE 1
FASE 3  5 coherencias documentadas + aprobación del usuario
```

- Si hay problemas: consultar ejemplos funcionales (3A), volver a FASE 1 (3B), documentar sólo tras éxito completo en `patrones-errores-conocidos.md` (3C).
- Si hay imagen ICFES original: comparar original vs generada y listar TODAS las diferencias.

## Prohibido
- Validación ciega ("el PDF se generó") o marcar completado sin inspección visual real.
- Saltarse la comparación con el original.
- **Lenguaje minimizador**: "ninguno significativo", "sin objeciones relevantes". Reportar "Ninguno" sólo si hay cero; si no, listar cada hallazgo.

Compacta desde 2026-09-29.
