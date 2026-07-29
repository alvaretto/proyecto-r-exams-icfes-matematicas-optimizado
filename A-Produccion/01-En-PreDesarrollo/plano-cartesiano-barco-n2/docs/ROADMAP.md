# Roadmap — Coordenadas de vértices en el plano cartesiano (barco)

> Ruta del subproyecto desde `01-En-PreDesarrollo/` hasta `03-En-Produccion/`, con los gates que
> hay que superar en cada tramo. Para el detalle de lo pendiente ver
> [`BACKLOG.md`](BACKLOG.md); para el estado de trabajo, [`../HANDOFF.md`](../HANDOFF.md).

---

## 1. Línea de tiempo

| Fecha | Hito | Evidencia |
|---|---|---|
| 2026-07-01 | Generación del ejercicio vía `/orquestador-schoice` (11 pasos) desde el ítem ICFES `MAT-2026-1-022` | commit `fc5a8c1a` |
| 2026-07-01 | Flujo B completado: gráfico del barco en R/ggplot2 | `ejercicio_state.json` → `flujo_b.completado = true` |
| 2026-07-01 | Los 11 pasos del workflow marcados completos, incluida `aprobacion_usuario` | `ejercicio_state.json` |
| *(sin commitear)* | Consolidación de 8 chunks → 5 y normalización de tildes | `git diff` al inicio de 2026-07-28 |
| 2026-07-28 | Re-validación completa, enumeración exhaustiva del espacio de versiones, documentación del subproyecto, `.claude/` local, auditoría adversarial | Esta sesión |
| 2026-07-28 | P0.1 resuelto: `GEO-COORD-03` (diagonal, eliminable por su forma) sustituido por `GEO-COORD-04` (desplazamiento de una unidad) | `docs/BACKLOG.md` P0.1 |
| 2026-07-28 | P2.5 resuelto: Solution ampliada con las 6 subsecciones canónicas (+ *Propiedades del concepto* y *Caso específico*) | `docs/BACKLOG.md` P2.5 |
| 2026-07-28 | P2.7 resuelto: retiradas las 4 exclusiones de `y_pool` (una de ellas justificada de forma falsa); espacio de versiones 222 → 374 | `docs/BACKLOG.md` P2.7 |
| 2026-07-28 | P1.1 cerrado: re-medición de ratios sobre el espacio de 374 + opción **A′** (`ratio ≥ 2` por construcción, decisión del usuario); espacio de versiones 374 → 318 | `docs/BACKLOG.md` P1.1 §«Resolución del residual» |

---

## 2. Objetivos específicos y su estado

Estos OE **no existían declarados** en ningún documento antes del 2026-07-28: `/goal` no encontró
`README`, `ROADMAP`, `CLAUDE.md` local ni memoria de proyecto. Se declaran aquí por primera vez y
se persisten en la memoria del proyecto.

| OE | Enunciado | Estado | Evidencia |
|---|---|---|---|
| **OE1** | Ejercicio SCHOICE N2 que renderiza en los 4 formatos canónicos + Moodle | ✅ | `Rscript verificar_render.R` → 5/5 OK |
| **OE2** | Corrección matemática de la clave en **todo** el espacio de versiones, no solo en semillas muestreadas | ✅ | Enumeración exhaustiva: **0/318** desajustes del bounding box (0/374 antes de P1.1/A′; 0/222 antes de P2.7) |
| **OE3** | Diversidad sustantiva (regla #22): la respuesta correcta varía entre versiones | ✅ | `validar_diversidad_sustantiva.R --n 40` → PASS (**37** valores únicos); **318** preguntas distintas por enumeración (222 → 374 en P2.7, 374 → 318 en P1.1/A′) |
| **OE4** | Cumplimiento de las reglas #18, #19, #20, #21 | ✅ | grep verificado: `{width=80%}`, 0 referencias a letra en Solution, guard `\newcounter{none}` presente, helpers Familia 1/5 |
| **OE5** | Coherencia matemática y semántica (Niveles 1-5, Capas A-D) | ✅ | `validar_coherencia_matematica.R` → APROBADO, 0 errores |
| **OE6** | Documentación completa del subproyecto (README, SYLLABUS, ROADMAP, BACKLOG, BLUEPRINT, HANDOFF) | ✅ | Esta sesión |
| **OE7** | `.claude/` local con particularidades operativas y regla del casco paramétrico | ✅ | `.claude/CLAUDE.md` + `.claude/rules/barco-parametrico.md` |
| **OE8** | Modularización dentro de la restricción de auto-contención | ✅ parcial | Externo reestructurado (`_archivo/`, `docs/`, `verificar_render.R`); el `.Rmd` permanece auto-contenido **por bloqueo de herramienta** — ver [`BACKLOG.md`](BACKLOG.md) P1.1 |
| **OE9** | Auditoría adversarial sin objeciones bloqueantes | ✅ | Auditoría ejecutada (adversarial + 8 dominios + visual). El único hallazgo bloqueante (`GEO-COORD-03` eliminable por su forma) se **resolvió**: sustituido por `GEO-COORD-04`, estructura 2×2 en 318/318. Ver [`BACKLOG.md`](BACKLOG.md) P0.1 |
| **OE10** | Promoción a `02-En-Desarrollo/` | ⬜ | Criterios técnicos cumplidos. Pendiente **re-confirmación humana** de la aprobación (§3): el registro del 2026-07-01 es anterior a P0.1/P2.5/P2.7/P1.1 |
| **OE11** | Validación Nivel 3 en aula → `03-En-Produccion/` | ⬜ | Requiere aplicación con estudiantes reales |

---

## 3. Vía a `02-En-Desarrollo/`

Criterios que deben cumplirse **todos** antes de mover el subproyecto:

- [x] Los 11 pasos de `ejercicio_state.json` completados.
- [x] 4 formatos + Moodle renderizan sin error.
- [x] `validar_coherencia_matematica.R` → APROBADO.
- [x] `validar_diversidad_sustantiva.R --n 40` → PASS.
- [x] Reglas #18/#19/#20/#21/#22 verificadas.
- [x] Documentación del subproyecto completa y con referencias cruzadas sin enlaces rotos.
- [x] `Rscript tests/run_all_tests.R` del repo raíz en verde (20 suites, 0 fallidas, 2026-07-28).
- [x] Auditoría adversarial (adversario + detractor + auditor visual) sin objeciones
      CRÍTICAS/ALTAS abiertas — **OE9**. El bloqueante P0.1 quedó resuelto.
- [x] Las 4 opciones comparten estructura 2×2 en **las 318 versiones** (criterio de cierre de P0.1):
      318/318 en las cuatro, verificado por enumeración exhaustiva (222/222 al resolver P0.1,
      374/374 tras P2.7, re-confirmado tras acotar el espacio en P1.1/A′).
- [x] Residual visual de P1.1 resuelto: `ratio ≥ 2` por construcción, **0** versiones degeneradas
      (invariante I-9 de `BLUEPRINT.md`).

**Todos los criterios técnicos están cumplidos al 2026-07-28** (P0.1, P1.1, P2.5 y P2.7 cerrados).

**Bloqueo administrativo pendiente, no técnico.** `ejercicio_state.json` marca
`aprobacion_usuario` y `detractor_fase2c` como completados el **2026-07-01**, es decir, sobre una
versión anterior del ítem: desde entonces cambió el tercer distractor (`GEO-COORD-03` →
`GEO-COORD-04`), la Solution ganó dos subsecciones (P2.5), `y_pool` perdió sus exclusiones (P2.7) y
el espacio quedó acotado a `ratio ≥ 2` (P1.1/A′). Ese registro **no se modificó**: alterar una
aprobación humana no es decisión de un agente. Antes de promover hay que re-confirmar la aprobación
sobre la versión actual. El usuario indicó el 2026-07-28 que **revisará el ítem personalmente**
antes de decidir.

**Comando de promoción**: el movimiento entre `01-` y `02-` es un `git mv` del directorio completo;
tras moverlo hay que re-verificar las rutas relativas `../../../../.claude/...` de los documentos
(la profundidad no cambia entre `01-En-PreDesarrollo/` y `02-En-Desarrollo/`, así que **no**
deberían romperse, pero hay que comprobarlo).

---

## 4. Gate de validación Nivel 3 (aula) para `03-En-Produccion/`

`03-En-Produccion/` es inmutable y solo admite ejercicios con evidencia de aplicación real. El
skill `/promover-ejercicio` exige:

1. Aplicación del ítem con un grupo de estudiantes reales de grado 10 u 11.
2. Registro de la distribución de respuestas por opción.
3. Análisis de la **diagnosticidad de los distractores**: cada uno de `GEO-COORD-01/02/04` debería
   captar una fracción no trivial de las respuestas incorrectas. Un distractor con 0 % de elección
   es un distractor muerto y debe rediseñarse.
4. Evidencia de que el ítem discrimina: los estudiantes de mayor desempeño global aciertan más.

Hasta que exista esa evidencia, el ejercicio permanece en `01-` o `02-`, por muy verde que esté
toda la validación automática. **La validación automática mide corrección, no calidad
psicométrica.**

---

## 5. Referencias cruzadas

- [`../README.md`](../README.md) · [`../HANDOFF.md`](../HANDOFF.md)
- [`SYLLABUS.md`](SYLLABUS.md) · [`BACKLOG.md`](BACKLOG.md) · [`BLUEPRINT.md`](BLUEPRINT.md)
- [`../.claude/CLAUDE.md`](../.claude/CLAUDE.md)
- `RR/.claude/rules/workflow-state-enforcement.md` — regla #16, los 11 pasos y su gate

---

**Versión:** 1.2 · **Fecha:** 2026-07-28 (v1.2 — P1.1 cerrado con la opción A′: cifras actualizadas
a **318**, gate de promoción con el bloqueo administrativo explícito, OE10 con su condición real;
v1.1 — P2.5 y P2.7 resueltos: línea de tiempo, OE3, gate de promoción y cifras actualizados a 374)
