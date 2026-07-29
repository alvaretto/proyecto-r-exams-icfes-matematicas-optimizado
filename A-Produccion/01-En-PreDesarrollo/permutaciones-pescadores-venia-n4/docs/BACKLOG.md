# Backlog — Permutaciones de los pescadores en la venia final

> Pendientes priorizados. `P1` es deuda que hay que resolver o documentar antes de escalar el
> patrón; `P2` es diferible; `P3` está bloqueado por evidencia externa (no es accionable por un
> agente). No hay ítems `P0` (bloqueantes de promoción) detectados a la fecha de este documento.
> Ver [`ROADMAP.md`](ROADMAP.md) para los gates de promoción.

---

## P1 — Deuda de desarrollo

### P1.1 — Falso positivo del corrector ortográfico sobre nombres de archivo en comentarios R — 🟡 MITIGADO, no resuelto en la herramienta

**Origen:** ejecución de `corregir_ortografia_espanol.R` sobre el `.Rmd` (2026-07-29).

`../../../.claude/scripts/corregir_ortografia_espanol.R` convierte el nombre de archivo
`codigo-rmd.md` en `código-rmd.md` dentro de comentarios de R, pese a que la regla #7
(`../../../.claude/rules/ortografia-espanol.md`, sección "Excepciones") declara explícitamente
que las rutas y nombres de archivo en comentarios quedan excluidos de la corrección automática.

**Mitigación aplicada:** las referencias a esa regla dentro del `.Rmd` de este subproyecto se
escriben como ruta completa (`.claude/rules/codigo-rmd.md`, con el prefijo de directorio), que sí
queda excluida por el patrón de detección del script — a diferencia del nombre de archivo suelto
`codigo-rmd.md`, que el script sí altera cuando aparece sin la ruta.

**Falta:** reportar el falso positivo al mantenedor de `corregir_ortografia_espanol.R`. Está fuera
del alcance de este subproyecto — es una herramienta compartida por todo el repositorio, análoga
en naturaleza al bloqueo de auto-contención que documenta el hermano `plano-cartesiano-barco-n2`
en su propio backlog (P1.2 de ese subproyecto): un defecto de una herramienta común, no del
ejercicio.

**Verificación de la mitigación:** `Rscript ../../../.claude/scripts/corregir_ortografia_espanol.R
permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd` → sin errores (2026-07-29,
con la mitigación ya aplicada).

---

### P1.2 — `WARN_DIV_BAJA` es estructural, no es deuda accionable — ✅ ACEPTADO Y DOCUMENTADO

**Origen:** `validar_diversidad_sustantiva.R --n 40` (2026-07-29): exit 0, 40/40 evaluadas, **3
claves posibles** (24, 120, 720 — una por cada valor de `n` en `N_POOL`), `WARN_DIV_BAJA`.

El aviso es estructural: solo hay 3 respuestas correctas distintas posibles porque las claves
dependen únicamente de `n! con n ∈ {4,5,6}` — el rango de `n` está **fijado por las
justificaciones oficiales del ítem** `MAT-2026-1-004` (ver [`SYLLABUS.md`](SYLLABUS.md) §3) — no
es un parámetro libre que se pueda ampliar para aumentar la cardinalidad de claves distintas. Esto
sigue siendo cierto **después** de la auditoría adversarial del 2026-07-29 que amplió el pool de
errores de 3 a 5: el número de claves posibles depende del rango de `n`, no del número de
distractores en el pool, así que el warning no cambia con esa ampliación.

**Por qué no es deuda accionable:** ampliar el rango de `n` más allá de `{4,5,6}` (para tener más
de 3 claves posibles) exigiría apartarse de los valores de `n` de la ficha oficial, lo que rompería
**OE1** (fidelidad al ítem oficial) — ver la medición completa del rango en
[`BLUEPRINT.md`](BLUEPRINT.md) §2: `n ≤ 3` colisiona opciones (equivalente a `ERR_ANS_C`), `n ≥ 7`
produce un distractor descartable por magnitud sin razonar (regla #22, patrón P5).

**Decisión:** se acepta el warning y se documenta aquí, en vez de intentar "resolverlo" bajando la
fidelidad al ítem oficial. Solo sería accionable si en el futuro se autoriza explícitamente
apartarse de la ficha oficial del ítem — una decisión que no le corresponde tomar a un agente.

**Evidencia complementaria (fuera del validador, muestreo directo de 300 evaluaciones del
`data_generation`, no enumeración exhaustiva para el número de claves — trivial porque solo hay 3
valores de `n`, ver [`BLUEPRINT.md`](BLUEPRINT.md) §2):** tras la ampliación del pool a 5 errores,
**297/300** versiones únicas de render (antes de la ampliación: 280/300), con las **10 de 10**
ternas de error posibles alcanzadas y **16** instancias canónicas. La distribución de claves por
valor de `n` y la distribución por contexto narrativo no se re-midieron en esta pasada — no son
necesarias para este ítem, que depende solo de la cardinalidad de claves distintas (3), invariante
ante el cambio del pool de errores.

---

### P1.3 — Pool de errores por debajo del mínimo de la regla #1 — ✅ RESUELTO (2026-07-29)

**Origen:** la regla #1 (`../../../.claude/rules/ejercicios-metacognitivos.md`, «Mínimo 4-6
errores por ejercicio») exige un pool de entre 4 y 6 errores conceptuales. La primera versión del
`.Rmd` tenía exactamente **3** errores (`EST-PER-01/02/03`) para 3 espacios en la Solution, así que
el **tipo** de error mostrado nunca variaba entre versiones — el pool y los slots coincidían
exactamente.

**Resolución:** la auditoría adversarial del 2026-07-29 (dos adversarios independientes) señaló el
incumplimiento de la regla #1 y propuso ampliar el pool. Se agregaron dos errores nuevos
(`EST-PER-04`, fórmula de permutación circular aplicada a una fila; `EST-PER-05`, principio
aditivo en lugar de multiplicativo), llevando el pool a **5** entradas, de las que cada versión
selecciona **3** al azar (salvo la instancia canónica, que fuerza los 3 oficiales — decisión D3,
ver [`BLUEPRINT.md`](BLUEPRINT.md) §4.8). De paso se corrigió `EST-PER-01`, cuyo nombre y
descripción anteriores eran matemáticamente inconsistentes con su propia fórmula (ver
[`SYLLABUS.md`](SYLLABUS.md) §3).

**Verificación de la resolución:** `verificar_render.R` `V6` enumera exhaustivamente las 30 ternas
posibles (3 valores de `n` × C(5,3) = 10) y confirma que las 4 opciones son siempre distintas; en
300 evaluaciones del `data_generation` se alcanzaron las **10 de 10** ternas posibles y **297/300**
versiones únicas de render (antes: 280/300, con solo 1 terna posible).

---

## P2 — Diferible

### P2.1 — Scripts de exportación institucional (`Semillero*.R`, plantillas `pcielo*.tex`) no existen todavía

El subproyecto hermano `plano-cartesiano-barco-n2` tiene `SemilleroUnico_v2.R`,
`SemilleroMoodle_v2.R`, `SemilleroCloze.R` y las plantillas `pcielo.tex` / `solpcielo.tex` /
`pcielo_nosol.tex`, que exportan el ejercicio con el membrete institucional (I. E. Pedacito de
Cielo) a PDF/DOCX/Moodle/NOPS/webquiz interactivo. Este subproyecto **no los tiene**.

**Falta evaluar** si este ejercicio los necesita antes de exportarlo para uso real en el aula, o
si `verificar_render.R` — que ya cubre HTML/PDF/DOCX/NOPS/Moodle sin plantillas institucionales,
pensado para verificación (CI), no para exportación — es suficiente para el caso de uso actual.
Ver [`../README.md`](../README.md) §"Exportación institucional".

---

### H1 (antes P1.4) — La clave nunca está entre las 2 opciones menores: adivinable al 50 % — 🔴 ABIERTO, REQUIERE DECISIÓN HUMANA

**Origen:** observado parcialmente por el adversario matemático el 2026-07-29 y catalogado entonces
como 🔵 BAJA. El code-review de alta intensidad del 2026-07-29 lo **midió** y lo reclasificó: el
razonamiento con que se cerró («desde que el pool se amplió a 5 errores el rango ya no es
invariante, 3.º o 4.º, lo que diluye el patrón») era **incorrecto** — que el rango varíe entre 3.º y
4.º no diluye nada, porque ambos están en la mitad alta.

**Medición exhaustiva** (3 valores de `n` × C(5,3) = 30 ternas, el espacio completo):

| rango de la clave por magnitud | ternas | % |
|---|---:|---:|
| 1.º o 2.º (mitad baja) | **0** | 0 % |
| 3.º | 18 | 60 % |
| 4.º (la clave es el máximo) | 12 | 40 % |

**Consecuencia para el estudiante:** descartar las dos opciones menores sin saber combinatoria deja
una adivinanza al **50 %** en vez del 25 %; y la heurística «elegir el número mayor» acierta en el
**40 %** de las versiones. Es la regla #22 patrón P5 (distractor descartable por magnitud) invertida
sobre la CLAVE, y contradice el pre-flight 14 del `orquestador-schoice`, que exige «verificar el
ORDEN/RANK de la respuesta correcta».

**Por qué I-3 no lo detecta:** `stopifnot(max(all_vals) / correcta_val <= 15)` es **unilateral**.
Cuando la clave ES el máximo, la expresión vale `correcta_val/correcta_val = 1.0` y la guarda pasa
trivialmente. Peor caso medido: `n=6` con terna {cuadrado, cardinal, suma} → opciones
`{720, 36, 21, 6}`, donde la clave es **20× el mayor distractor** y el ratio de I-3 da 1,0×.

**Por qué no se corrige en esta pasada:** el pool sólo contiene UNA fórmula mayor que `n!`
(`n^(n-1)`), así que llevar la clave a la mitad baja exige **añadir fórmulas > `n!`** al pool. Eso:

1. cambia el contenido evaluado del ítem (decisión pedagógica, no de mantenimiento);
2. obliga a re-medir el espacio completo (C(6,3)=20 ternas × 3 `n` = 60) y a re-validar render,
   diversidad y las 6 invariantes — la regla local lo exige explícitamente;
3. colisiona con **OE1**: la instancia canónica debe reproducir las 4 opciones oficiales
   `{4, 16, 24, 64}` de `MAT-2026-1-004`, donde la clave 24 también es la 3.ª. Es decir, **el propio
   ítem oficial del ICFES tiene esta propiedad**; corregirla en las versiones no canónicas crea una
   asimetría deliberada que hay que decidir, no deducir;
4. roza la decisión cerrada **D2** (`n ∈ {4,5,6}` medido), porque las fórmulas candidatas
   (p. ej. `(n+1)!` = 5040 en `n=6`, 7,0×) mueven la razón de magnitud.

**Mitigación aplicada mientras se decide:** `verificar_render.R` V6 ya no sólo imprime el rango, sino
que (a) FALLA si el rango de la clave llegara a ser un valor ÚNICO en las 30 ternas (deriva a patrón
posicional puro), (b) reporta `clave / mayor distractor` para hacer visible la dominancia que I-3 no
acota, y (c) emite un AVISO explícito mientras la clave no alcance nunca los puestos 1.º o 2.º.

**Decisión pendiente del usuario:** aceptar la propiedad por fidelidad al ítem oficial (y dejar H1
como observación permanente), o autorizar la ampliación del pool con fórmulas > `n!` y la
re-medición completa que conlleva.

---

## P3 — Bloqueado por evidencia externa (no técnico, no accionable por un agente)

### P3.1 — OE10 (promoción a `02-En-Desarrollo/`) y OE11 (evidencia Nivel 3 en aula) pendientes

Ver [`ROADMAP.md`](ROADMAP.md) §2-4.

- **OE10** ya tiene resuelto lo técnico: `ejercicio_state.json` está sincronizado (**10/11** pasos
  `completado: true`) y la auditoría adversarial formal se ejecutó el 2026-07-29 (dos adversarios
  independientes, veredicto `"APROBAR CON CAMBIOS (aplicados)"`). Falta completar `HANDOFF.md` y
  `.claude/CLAUDE.md` (pendientes de otro agente en esta misma sesión) y obtener una aprobación
  humana explícita (`aprobacion_usuario`).
- **OE11** requiere aplicación del ítem con estudiantes reales de grado 10-11 y análisis de
  diagnosticidad por distractor (ahora sobre un pool de **cinco** códigos, `EST-PER-01` a `05`) —
  el gate que la validación automática **no** puede sustituir (ver [`ROADMAP.md`](ROADMAP.md) §4).

Ninguno de los dos ítems es resoluble por un agente trabajando solo sobre el código del ejercicio:
ambos dependen de una decisión o una acción humana externa al `.Rmd`.

---

## Referencias cruzadas

- [`../README.md`](../README.md) · [`../HANDOFF.md`](../HANDOFF.md) (pendiente)
- [`SYLLABUS.md`](SYLLABUS.md) · [`ROADMAP.md`](ROADMAP.md) · [`BLUEPRINT.md`](BLUEPRINT.md)
- `../.claude/CLAUDE.md` (pendiente)
- `../../../../.claude/rules/diversidad-sustantiva.md` — regla #22, contexto de P1.2
- `../../../../.claude/rules/ortografia-espanol.md` — regla #7, contexto de P1.1
- `../../plano-cartesiano-barco-n2/docs/BACKLOG.md` — precedente del bloqueo de auto-contención
  (su P1.2) y del formato de este documento
- `../.claude/rules/permutaciones-parametricas.md` — contrato local del pool `n!` y del pool de
  cinco errores conceptuales (pendiente, lo escribe otro agente)

---

**Versión**: 1.1
**Fecha**: 2026-07-29
