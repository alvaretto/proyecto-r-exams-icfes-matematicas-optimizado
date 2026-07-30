# Roadmap — Permutaciones de los pescadores en la venia final

> Ruta del subproyecto desde `01-En-PreDesarrollo/` hasta `03-En-Produccion/`, con los gates que
> hay que superar en cada tramo. Para el detalle de lo pendiente ver [`BACKLOG.md`](BACKLOG.md);
> para el estado de trabajo, [`../HANDOFF.md`](../HANDOFF.md) (pendiente).

---

## 1. Línea de tiempo

| Fecha | Hito | Evidencia |
|---|---|---|
| 2026-07-29 | Inicio del subproyecto; `ejercicio_state.json` creado | `timestamp_inicio: 2026-07-29T16:28:02` |
| 2026-07-29 | `analisis_icfes` completado: clasificación oficial del ítem `MAT-2026-1-004` **adoptada**, no re-derivada | `ejercicio_state.json` → `analisis_icfes.completado = true`; [`SYLLABUS.md`](SYLLABUS.md) §1 |
| 2026-07-29 | Flujo B = `false` (el ejercicio no requiere gráficos) | `ejercicio_state.json` → `flujo_b.requerido = false, completado = true` |
| 2026-07-29 | Decisión de diseño D1: DOK 3 / Bloom "Evaluar" para preservar la coherencia con el Nivel 4 oficial, aprobada por el usuario | [`BLUEPRINT.md`](BLUEPRINT.md) §4.2 |
| 2026-07-29 | Decisión de diseño D2: rango `n ∈ {4,5,6}` fijado por enumeración exhaustiva | [`BLUEPRINT.md`](BLUEPRINT.md) §2 |
| 2026-07-29 | Generación del `.Rmd` (4 chunks R + 1 guard LaTeX) | archivo en el subproyecto |
| 2026-07-29 16:44 | Verificación real ejecutada: HTML, PDF, DOCX, NOPS y Moodle renderizan sin error | `verif_render/` (mtime de `plain1.pdf`, `nops1.pdf`, `pandoc1.docx`, `moodle/perm_check.xml`) |
| 2026-07-29 | Coherencia matemática y diversidad sustantiva verificadas | `validar_coherencia_matematica.R` → APROBADO; `validar_diversidad_sustantiva.R --n 40` → exit 0 |
| 2026-07-29 | Ortografía verificada | `corregir_ortografia_espanol.R` → sin errores |
| 2026-07-29 | Documentación inicial del subproyecto (este README + `docs/*`) | Sesión previa |
| 2026-07-29 | Auditoría adversarial ejecutada sobre el `.Rmd` (dos adversarios independientes) | `ejercicio_state.json` → `detractor_fase2c.veredicto = "APROBAR CON CAMBIOS (aplicados)"` |
| 2026-07-29 | Pool de errores ampliado de 3 a 5 (regla #1: mínimo 4-6 por ejercicio); `EST-PER-01` renombrado y su descripción corregida (doble supuesto `n-1`/`n`, no `n^n`); nueva decisión D3 (excepción canónica que fuerza los 3 errores oficiales) | `.Rmd`: pool `errores_conceptuales` y bloque de selección; [`BLUEPRINT.md`](BLUEPRINT.md) §4.8 |
| 2026-07-29 | Re-validación completa tras los cambios: `V1`-`V8` verdes (`V6` ahora exhaustivo, 30/30 ternas), `validar_coherencia_matematica.R` → APROBADO 0 errores, `validar_diversidad_sustantiva.R --n 40` → exit 0, ortografía sin errores, 0 coincidencias de letter-independence | `verif_render/`, salidas de los 4 scripts (esta sesión) |
| 2026-07-29 | `ejercicio_state.json` sincronizado: 10/11 pasos `completado: true` (`validar_diversidad` registra `versiones_unicas: 297`) | `ejercicio_state.json` |
| 2026-07-29 | Documentación del subproyecto actualizada tras la auditoría (este README + `docs/*`, v1.1) | Esta sesión |
| 2026-07-29 | Code-review de alta intensidad sobre la rama: 10 defectos distintos, 9 aplicados (3 de ellos eran verificadores que se citaban como evidencia verde estando vacíos) | Commit `002ebe22`; [`../HANDOFF.md`](../HANDOFF.md) §5.3 |
| 2026-07-30 | **Decisión D4 autorizada por el usuario**: pool 5 → 7 (`EST-PER-06` `(n+1)!`, `EST-PER-07` `2·n!`) + invariante **I-7** (toda terna con ≥1 distractor > `n!`) para cerrar el hallazgo **H1** | [`BLUEPRINT.md`](BLUEPRINT.md) §4.9 y §3.1; [`BACKLOG.md`](BACKLOG.md) H1 |
| 2026-07-30 | Barrido de 6 configuraciones del pool medido antes de fijar la elegida; se descartó «pool 5 + I-7» porque deja el rango de la clave fijo en 3.º (patrón posicional puro) | [`BLUEPRINT.md`](BLUEPRINT.md) §3.1 |
| 2026-07-30 | `verificar_render.R` gana **V9** (la selección real del chunk se queda en el espacio legal, 240 semillas) y V6 pasa a medir el espacio legal con guardas que FALLAN, no avisan | `verificar_render.R` |
| 2026-07-30 | Re-verificación completa: `V1`-`V9` verdes (V6 **105/105** ternas, 93 legales; clave en 1.º/2.º/3.º, nunca 4.º; «elegir el mayor» 0,0 %), suite `I-1..I-7` 0 fail/0 error/0 skip, coherencia matemática APROBADO, diversidad exit 0, ortografía sin errores | `verif_render/`, salidas de los 4 scripts |
| 2026-07-30 | Diversidad re-medida con el pool de 7: **298/300** versiones únicas, 89 de 93 ternas legales alcanzadas, 16 instancias canónicas | 300 evaluaciones del `data_generation` |
| 2026-07-30 | Scripts de exportación institucional añadidos (`Semillero*.R`, `pcielo*.tex`) | [`BACKLOG.md`](BACKLOG.md) P2.1 |
| 2026-07-30 | Banco de Moodle fijado en **100 preguntas** por decisión del usuario, con justificación en el script. Se corrigió de paso una lectura errónea propia: la regla #3 («< 200 versiones únicas») gobierna la capacidad del ejercicio (`exams2html(n=200)`, aquí 298/300), no el tamaño del banco exportado | `SemilleroMoodle_v2.R`; [`BACKLOG.md`](BACKLOG.md) P2.1 |
| 2026-07-30 | Banco de 100 regenerado y verificado: 100/100 clave = `n!`, 4 opciones distintas, I-7 respetada, 99/100 preguntas completas distintas, 18 enunciados distintos | `salida/*.xml` (derivado, no versionado) |
| 2026-07-30 | Documentación sincronizada con el código (este ROADMAP + README + `docs/*` + `.claude/*`, v2.0): la anterior seguía describiendo pool de 5, I-1..I-6, 30 ternas y H1 abierto | Esta sesión |

**Nota de sincronización (actualizada 2026-07-29).** Tras la auditoría adversarial,
`ejercicio_state.json` quedó sincronizado con el estado real: **10 de 11** pasos marcan
`completado: true` — incluido `validar_diversidad`, que registra `versiones_unicas: 297` — y el
veredicto de `detractor_fase2c` quedó como `"APROBAR CON CAMBIOS (aplicados)"`. El único paso
pendiente es `aprobacion_usuario`, que por diseño no puede completar un agente — ver §3.

---

## 2. Objetivos específicos y su estado

| OE | Enunciado | Estado | Evidencia |
|---|---|---|---|
| **OE1** | Fidelidad al ítem oficial `MAT-2026-1-004` | ✅ | Contexto canónico verbatim (`V7` de `verificar_render.R`); pool de errores tomado literalmente de las Justificaciones MetaCognitivas oficiales ([`SYLLABUS.md`](SYLLABUS.md) §3) |
| **OE2** | Paridad literal de campos oficiales vs. catálogos canónicos | ✅ | Clasificación **adoptada**, no re-derivada — Competencia, Componente, Afirmación, Evidencia, Nivel copiados carácter por carácter de la ficha oficial ([`SYLLABUS.md`](SYLLABUS.md) §1) |
| **OE3** | Pool de errores derivado de las justificaciones oficiales, con `calcula()` determinista | ✅ | **7** errores `EST-PER-01` a `07` (3 → 5 en la auditoría adversarial del 2026-07-29 por la regla #1; 5 → 7 en la decisión D4 del 2026-07-30 para cerrar H1), 3 elegidos por versión del espacio legal salvo la excepción canónica D3; `calcula()` puras sin `sample`/`runif` (invariante I-10, [`BLUEPRINT.md`](BLUEPRINT.md) §5, §4.8 y §4.9) |
| **OE4** | Familia `n ∈ {4,5,6}` con unicidad y plausibilidad por construcción | ✅ | Enumeración exhaustiva de las 105 ternas ([`BLUEPRINT.md`](BLUEPRINT.md) §2 y §3); invariantes I-1 a I-4 **e I-7** verificadas en tiempo de generación y por `V6`/`V9` |
| **OE5** | `.Rmd` auto-contenido | ✅ | Helpers y pool de errores dentro de `data_generation` (invariante I-8; renumerada desde I-6 al introducir el código sus propias I-6 e I-7 — ver la nota de numeración de [`BLUEPRINT.md`](BLUEPRINT.md) §5) |
| **OE6** | Solution canónica y letter-independent | ✅ | 7 encabezados (6 subsecciones canónicas), identificación por contenido/código (chunk `solucion`) |
| **OE7** | Render verde 4 formatos + Moodle | ✅ | `verificar_render.R` → `V1`-`V9` OK (`verif_render/`, re-verificado el 2026-07-30 sobre el pool de 7) |
| **OE8** | Diversidad sustantiva + ≥250/300 | ✅ | `validar_diversidad_sustantiva.R --n 40` → exit 0, `WARN_DIV_BAJA` (esperado y aceptado, [`BACKLOG.md`](BACKLOG.md) P1.2); 300 evaluaciones del `data_generation`: **298/300** versiones únicas de render, **89 de 93** ternas legales alcanzadas, 16 instancias canónicas (re-medido el 2026-07-30 con el pool de 7) |
| **OE9** | Documentación con referencias cruzadas y auditoría adversarial limpia | ✅ | Auditoría adversarial del 2026-07-29 con **dos adversarios independientes**; veredicto `"APROBAR CON CAMBIOS"`, cambios aplicados (pool 3→5, decisión D3, renombrado de `EST-PER-01`) y todo re-validado. Después: code-review de alta intensidad (9 de 10 defectos aplicados) y cierre de H1 con la decisión D4. Documentación sincronizada con el código el 2026-07-30 (v2.0) |
| **OE10** | Promoción a `02-En-Desarrollo/` | ⬜ | Criterios técnicos mayormente cumplidos; pendiente sincronizar `ejercicio_state.json` con el estado real y obtener aprobación humana explícita — ver §3 |
| **OE11** | Validación Nivel 3 en aula → `03-En-Produccion/` | ⬜ | Requiere aplicación con estudiantes reales de grado 10-11 |

---

## 3. Vía a `02-En-Desarrollo/`

Criterios que deben cumplirse **todos** antes de mover el subproyecto:

- [x] Clasificación oficial adoptada y verificada carácter por carácter ([`SYLLABUS.md`](SYLLABUS.md) §1).
- [x] 4 formatos + Moodle renderizan sin error (`verif_render/`, re-verificado tras la auditoría).
- [x] `validar_coherencia_matematica.R` → APROBADO, 0 errores.
- [x] `validar_diversidad_sustantiva.R --n 40` → exit 0 (`WARN_DIV_BAJA` documentado y aceptado,
      no bloqueante — [`BACKLOG.md`](BACKLOG.md) P1.2).
- [x] `corregir_ortografia_espanol.R` → sin errores.
- [x] Reglas #1 / #8 / #10 / #11 / #19 / #20 / #21 / #22 verificadas ([`BLUEPRINT.md`](BLUEPRINT.md) §4-5).
- [x] Documentación del subproyecto completa y con referencias cruzadas sin enlaces rotos.
- [x] `ejercicio_state.json` sincronizado con el estado real: **10/11** pasos `completado: true`
      (ver nota de §1).
- [x] Auditoría adversarial formal registrada sobre esta versión del `.Rmd`: dos adversarios
      independientes, veredicto `"APROBAR CON CAMBIOS (aplicados)"`.
- [ ] `HANDOFF.md` y `.claude/CLAUDE.md` locales escritos (pendientes de otro agente en esta misma
      sesión).
- [ ] Re-confirmación de aprobación humana (`aprobacion_usuario` en `ejercicio_state.json`).

**Los criterios técnicos de contenido, render y auditoría adversarial están cumplidos al
2026-07-29.** Lo único que falta para `02-En-Desarrollo/` es cerrar la documentación local
pendiente de otro agente (`HANDOFF.md`, `.claude/CLAUDE.md`) y obtener la aprobación humana
explícita — ninguna de las dos es una decisión que le corresponda tomar a un agente por su cuenta.

**Comando de promoción**: el movimiento entre `01-` y `02-` es un `git mv` del directorio completo;
tras moverlo hay que re-verificar las rutas relativas `../../../.claude/...` de los documentos (la
profundidad no cambia entre `01-En-PreDesarrollo/` y `02-En-Desarrollo/`, así que no deberían
romperse, pero hay que comprobarlo).

**Destino planificado en producción** (una vez alcance `03-En-Produccion/`):
`03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/10-Combinatoria_Permutaciones-Variaciones-Combinaciones/`.

---

## 4. Gate de validación Nivel 3 (aula) para `03-En-Produccion/`

`03-En-Produccion/` es inmutable y solo admite ejercicios con evidencia de aplicación real. El
skill `/promover-ejercicio` exige:

1. Aplicación del ítem con un grupo de estudiantes reales de grado 10 u 11.
2. Registro de la distribución de respuestas por opción.
3. Análisis de la **diagnosticidad de los distractores**: cada uno de los siete (`EST-PER-01` a
   `05`) debería captar una fracción no trivial de las respuestas incorrectas en las versiones
   donde aparece. Un distractor con 0 % de elección es un distractor muerto y debe rediseñarse.
4. Evidencia de que el ítem discrimina: los estudiantes de mayor desempeño global aciertan más.

Hasta que exista esa evidencia, el ejercicio permanece en `01-` o `02-`, por muy verde que esté
toda la validación automática. **La validación automática mide corrección, no calidad
psicométrica.**

---

## 5. Referencias cruzadas

- [`../README.md`](../README.md) · [`../HANDOFF.md`](../HANDOFF.md) (pendiente)
- [`SYLLABUS.md`](SYLLABUS.md) · [`BACKLOG.md`](BACKLOG.md) · [`BLUEPRINT.md`](BLUEPRINT.md)
- `../.claude/CLAUDE.md` (pendiente)
- `../../../../.claude/rules/workflow-state-enforcement.md` — regla #16, los 11 pasos y su gate
- `../.claude/rules/permutaciones-parametricas.md` — contrato local del pool `n!` (pendiente)

---

**Versión**: 2.0 (bitácora del code-review y de la decisión D4; OE3/OE4/OE5/OE7/OE8/OE9 actualizados)
**Fecha**: 2026-07-30
