# Roadmap — Desplazamiento avión→aeropuerto

> Estado detallado y contexto de decisiones en [`../HANDOFF.md`](../HANDOFF.md). Este documento
> ordena los hitos en el tiempo con fechas absolutas y marca explícitamente qué bloquea qué.

## 1. Línea de tiempo (hitos completados)

| Fecha | Hito | Commit / evidencia |
|---|---|---|
| 2026-06-27 07:31 | Inicio del workflow (`ejercicio_state.json` creado) | — |
| 2026-06-27 16:06 | `.Rmd` generado, renderiza en 4 formatos | `renderizado_4_formatos` completado |
| 2026-06-27 16:18 | Detractor FASE 2C — veredicto **APROBAR** | `detractor_fase2c.veredicto = "APROBAR"` |
| 2026-06-27 16:20 | Validación de diversidad — 294/300 versiones únicas | `validar_diversidad.versiones_unicas = 294` |
| 2026-06-27 21:03 | Aprobación de usuario (workflow de 11 pasos declarado completo) | `aprobacion_usuario.completado = true` |
| 2026-06-28 | **Segunda ronda de fixes** (fuera del workflow de 11 pasos): Error 23 (etiquetas solapadas) y Error 24 (predictibilidad posicional) corregidos y verificados | commits `169ab8c6`, `287afc01`, `dd5f10d1`, `779d7383` |
| 2026-07-28 | `HANDOFF.md` redactado; inventario de ruido vs. fuente activa (`Semillero*.R`) corregido; `SP/docs/` creado (este documento) | Esta sesión |

**Importante**: la aprobación del 2026-06-27 21:03 quedó **desactualizada** por los fixes del
2026-06-28. Los pasos `validar_diversidad` y `coherencias_5` de `ejercicio_state.json` deben
tratarse como *pendientes de re-confirmación*, no como definitivos (ver
[BACKLOG.md](BACKLOG.md)).

## 2. Estado actual de los objetivos específicos (OE1-OE11)

Fuente: [`../HANDOFF.md` §2](../HANDOFF.md#2-objetivos-específicos).

```mermaid
flowchart LR
    OE1[OE1 Render 4 formatos ✅] --> OE2[OE2 Diversidad sustantiva ✅]
    OE2 --> OE3[OE3 Diversidad posicional ✅]
    OE3 --> OE4[OE4 Etiquetas sin solape ✅]
    OE4 --> OE5[OE5 Distractor plausible ✅]
    OE5 --> OE6[OE6 Modularizar ⏳]
    OE6 --> OE7[OE7 Documentar ⏳ ← este pase]
    OE7 --> OE8[OE8 Cablear orquestador ⏳]
    OE8 --> OE9[OE9 .claude local ⏳]
    OE9 --> OE10[OE10 Promover a 02-En-Desarrollo ⏳]
    OE10 --> OE11[OE11 Validación Nivel 3 en aula ⏳ FUTURO]
    OE11 --> PROD[03-En-Produccion]
```

| OE | Estado | Bloquea a |
|---|---|---|
| OE1-OE5 | ✅ Hecho (verificado 2026-07-28) | — |
| OE6 (modularizar helpers a `SP/R/`) | ⏳ Pendiente | OE7 parcialmente (el Blueprint ya documenta la arquitectura actual sin modularizar; no es un bloqueo duro) |
| OE7 (documentación) | ⏳ Este pase de trabajo | — |
| OE8 (cablear orquestador) | ⏳ Pendiente | Nada aguas abajo de este subproyecto, pero es deuda que afecta a **futuros** ejercicios con opciones gráficas si se invoca el comando en vez del agente |
| OE9 (`SP/.claude/` local) | ⏳ Pendiente (nota: existe un `.claude/CLAUDE.md` local creado el 2026-07-28, revisar su alcance antes de dar OE9 por cerrado) | — |
| OE10 (promover a `02-En-Desarrollo/`) | ⏳ Pendiente — **decisión explícita de no promover en este pase** (HANDOFF D4) | OE11 |
| OE11 (validación Nivel 3 en aula) | ⏳ Futuro | Promoción a `03-En-Produccion/` |

## 3. Vía a `02-En-Desarrollo/`

El movimiento a `02-En-Desarrollo/` (OE10) requiere, en orden:

1. **Re-confirmar `validar_diversidad` y `coherencias_5`** con el `.Rmd` tal como quedó tras los
   fixes del 2026-06-28 (Errores 23 y 24). Comando:
   ```bash
   Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R \
     desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd --n 40
   ```
   Exit 0 (`PASS` o `WARN_DIV_BAJA`) es condición necesaria pero **no** suficiente — ver el
   hallazgo P0 de [BACKLOG.md](BACKLOG.md): el validador de diversidad por *valor* no detecta el
   sesgo de `GEO-DES-03` como "siempre el vector más largo" porque el valor sí varía entre
   versiones (solo el *rango relativo* es invariante).
2. **Resolver el P0 del backlog** (distractor `GEO-DES-03` extremo por construcción) — es un
   defecto pedagógico real (atajo de eliminación sin razonamiento), no cosmético.
3. Opcionalmente completar OE6 (modularización) y OE8 (cablear orquestador) — no son bloqueos
   duros para `02-En-Desarrollo/`, pero si se difieren deben quedar registrados como deuda en el
   HANDOFF del ejercicio que se promueva.
4. Ejecutar `/promover-ejercicio` (o el paso equivalente del orquestador) una vez lo anterior
   esté resuelto.

## 4. Gate de validación Nivel 3 (aula) para `03-En-Produccion/`

Según `../../../.claude/rules/detractor-obligatorio.md` y el flujo general documentado en
`../../../.claude/CLAUDE.md`, la promoción final a `03-En-Produccion/` (OE11) requiere
**evidencia de aplicación real con estudiantes** (Nivel 3), no solo validación automática
(Nivel 1-2). Esto significa:

- El ejercicio debe aplicarse en aula (formato NOPS o Moodle) con un grupo real de estudiantes.
- Se debe recoger evidencia de que los distractores discriminan como se espera (los estudiantes
  que cometen el error conceptual `GEO-DES-0X` seleccionan la opción correspondiente, no al
  azar).
- Esta evidencia es un **requisito de `/promover-ejercicio`**, no un paso automatizable — no
  tiene fecha objetivo fijada porque depende de la disponibilidad de un grupo de aplicación.

**No se fija fecha para OE11** porque depende de la programación académica de la institución, no
del ritmo de desarrollo del ejercicio. Los pasos 1-4 de la sección anterior (vía a
`02-En-Desarrollo/`) sí son ejecutables en la próxima sesión de trabajo.

## 5. Referencias cruzadas

- [`../HANDOFF.md`](../HANDOFF.md) — objetivos, decisiones, hallazgos, riesgos
- [`BACKLOG.md`](BACKLOG.md) — ítems priorizados con criterios de aceptación verificables
- `../../../.claude/rules/workflow-state-enforcement.md` — los 11 pasos del workflow y su gate
- `../../../.claude/rules/detractor-obligatorio.md` — requisito de evidencia Nivel 3 para promover
