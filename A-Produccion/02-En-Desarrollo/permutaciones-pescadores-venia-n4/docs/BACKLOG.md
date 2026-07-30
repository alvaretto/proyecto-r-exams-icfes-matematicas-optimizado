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
valores de `n`, ver [`BLUEPRINT.md`](BLUEPRINT.md) §2):** con el pool de 7 (decisión D4),
**298/300** versiones únicas de render, **89 de las 93** ternas legales alcanzadas y **16**
instancias canónicas. Progresión histórica: 280/300 con el pool de 3, 297/300 con el de 5. La distribución de claves por
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

**Superado el 2026-07-30 (decisión D4).** El pool subió de 5 a **7** al cerrar el hallazgo H1 (ver
abajo). Las cifras de este párrafo son las del estado intermedio y se conservan por trazabilidad; las
vigentes son 105 ternas enumeradas, 93 legales, 89 alcanzadas en 300 evaluaciones y **298/300**
versiones únicas.

---

## P2 — Diferible

### P2.1 — Scripts de exportación institucional (`Semillero*.R`, plantillas `pcielo*.tex`) — ✅ RESUELTO (2026-07-30)

El subproyecto ya tiene los cuatro artefactos que antes solo existían en el hermano
`plano-cartesiano-barco-n2`: `SemilleroUnico_v2.R`, `SemilleroMoodle_v2.R`, `SemilleroCloze.R` y las
plantillas `pcielo.tex` / `pcielo_nosol.tex` / `solpcielo.tex`, que exportan el ejercicio con el
membrete institucional (I. E. Pedacito de Cielo) a PDF/DOCX/Moodle/NOPS y al webquiz interactivo.

Se copiaron del hermano y se adaptaron: la única diferencia funcional respecto al original es
`archivo_examen`, apuntado al `.Rmd` de este subproyecto. Verificado por ejecución: `salida/` contiene
el PDF, el DOCX, el NOPS, el HTML interactivo y el XML de Moodle generados.

**Tamaño del banco de Moodle — decidido por el usuario (2026-07-30):** `copias <- 100`.

Este punto pasó por una lectura errónea que conviene dejar registrada, porque es fácil de repetir.
El script llegó con `copias <- 100` al copiarse del hermano, y en la primera pasada se "corrigió" a
300 invocando la regla #3 de `../../../.claude/rules/codigo-rmd.md` («NO crear ejercicios con < 200
versiones únicas»). **Esa regla no gobierna `copias`**: habla de la CAPACIDAD del ejercicio para
generar versiones distintas y se valida con `exams2html(archivo, n = 200)` — requisito cumplido y
medido aquí (**298/300**). Cuántas preguntas se exportan al banco es una decisión de uso, no una
restricción de la regla.

El code-review del 2026-07-29 marcó el mismo valor como defecto #8 en el hermano
`desplazamiento-avion-aeropuerto` con esa misma lectura. Lo que allí era un defecto real es que el
valor se cambió **sin comentario**: nadie podía distinguir una decisión de un descuido. Aquí queda
con su justificación explícita en el propio script.

Banco verificado (100 preguntas): **100/100** con la clave = `n!`, 4 opciones distintas, **I-7**
respetada (la clave nunca es la mayor), **99/100** preguntas completas distintas y 18 enunciados
distintos (6 contextos × 3 valores de `n` — el techo del diseño narrativo).

**Distinción fuente/derivado:** los `Semillero*.R` y los `pcielo*.tex` son **fuente** (se versionan);
`salida/`, `verif_render/` y los `.html`/`.pdf`/`.docx`/`.xml`/`.rds` de la raíz del subproyecto son
**derivados** (ignorados, ver [`../.gitignore`](../.gitignore) y `../README.md` §"Estructura de
archivos").

---

### H1 (antes P1.4) — La clave nunca está entre las 2 opciones menores: adivinable al 50 % — ✅ RESUELTO (2026-07-30, decisión D4)

> Se conserva en su posición histórica dentro de `P2` por trazabilidad: nació como P1.4, se
> reclasificó a hallazgo abierto y se cerró con autorización explícita del usuario el 2026-07-30.

**Origen:** observado parcialmente por el adversario matemático el 2026-07-29 y catalogado entonces
como 🔵 BAJA. El code-review de alta intensidad del 2026-07-29 lo **midió** y lo reclasificó: el
razonamiento con que se cerró («desde que el pool se amplió a 5 errores el rango ya no es
invariante, 3.º o 4.º, lo que diluye el patrón») era **incorrecto** — que el rango varíe entre 3.º y
4.º no diluye nada, porque ambos están en la mitad alta.

**El defecto medido** (pool de 5, espacio completo de 30 ternas):

| rango de la clave por magnitud | ternas | % |
|---|---:|---:|
| 1.º o 2.º (mitad baja) | **0** | 0 % |
| 3.º | 18 | 60 % |
| 4.º (la clave es el máximo) | 12 | 40 % |

Consecuencia: descartar las dos opciones menores sin saber combinatoria dejaba una adivinanza al
**50 %** en vez del 25 %, y «elegir el número mayor» acertaba el **40 %** de las versiones. Es la
regla #22 patrón P5 invertido sobre la CLAVE. `I-3` no lo detectaba por ser **unilateral**: cuando la
clave es el máximo, `max(all_vals)/correcta_val` vale 1,0× y la guarda pasa trivialmente. Peor caso:
`n=6` con {cuadrado, cardinal, suma} → `{720, 36, 21, 6}`, clave **20×** el mayor distractor.

---

**Resolución — decisión D4, autorizada por el usuario el 2026-07-30.**

De las dos salidas planteadas (aceptar la propiedad por fidelidad al ítem oficial, o ampliar el pool
con fórmulas mayores que `n!`) se autorizó la segunda. Se aplicaron **dos** cambios, porque ninguno
funciona solo — barrido completo en [`BLUEPRINT.md`](BLUEPRINT.md) §3.1:

1. **Pool 5 → 7**: `EST-PER-06` (`(n+1)!`, contar una posición más de las que hay) y `EST-PER-07`
   (`2·n!`, duplicar el conteo por el orden inverso). Ambos son errores conceptuales diagnósticos por
   derecho propio, no relleno numérico.
2. **Invariante I-7**: toda terna debe contener al menos un distractor mayor que `n!`. La terna se
   elige enumerando el espacio legal y sorteando un índice — nunca con un bucle de reintento
   (regla #21 Familia 1, Error 22).

**Estado medido después** (espacio completo de 105 ternas, 93 legales):

| métrica | antes (pool 5) | después (pool 7 + I-7) |
|---|---:|---:|
| rango de la clave | 3.º / 4.º | **1.º / 2.º / 3.º** (nunca 4.º) |
| mitad baja (1.º o 2.º) | 0,0 % | **41,9 %** |
| «elegir el mayor» acierta | 40,0 % | **0,0 %** |
| clave / mayor distractor | hasta 20,00× | ≤ **0,50×** |
| versiones únicas (300 evals) | 297 | **298** |

**Las cuatro objeciones que bloqueaban el cierre, resueltas:**

1. *«Cambia el contenido evaluado»* — sí, y por eso requería autorización humana. Concedida.
2. *«Obliga a re-medir el espacio completo»* — hecho: 105/105 ternas verdes en `V6`, más `V9` sobre
   240 semillas para la selección real, la suite `I-1..I-7`, coherencia matemática APROBADO,
   diversidad exit 0 y ortografía sin errores.
3. *«Colisiona con OE1»* — **no colisiona.** Los tres errores oficiales incluyen `EST-PER-01`
   (`64 > 24`), así que la terna canónica cumple I-7 por sí sola: `MAT-2026-1-004` se sigue
   reproduciendo verbatim con `{24, 64, 16, 4}` y su clave en 3.º, igual que el original. La asimetría
   es deliberada: **fidelidad en la instancia canónica, mitigación en las variantes**.
4. *«Roza la decisión D2»* — **no la roza.** `(n+1)!` vale 7,0× y `2·n!` vale 2,0× en el peor `n`,
   ambas por debajo del 10,8× que ya aportaba `EST-PER-01`. El umbral de 15× y `n ∈ {4,5,6}` quedan
   intactos.

**Guardas de no-regresión** (fallan, no avisan): `V6` re-mide las tres cifras en cada corrida y
devuelve FAIL si la mitad baja cae a 0 %, si «elegir el mayor» sube de 0 % o si el rango se vuelve
constante; `V9` comprueba que la selección real se queda en el espacio legal; el test fija
`expect_gte(length(pool), 7L)`. Documentado como particularidad **13** en
[`../.claude/CLAUDE.md`](../.claude/CLAUDE.md) e invariante **I-7** en
[`../.claude/rules/permutaciones-parametricas.md`](../.claude/rules/permutaciones-parametricas.md).

---

## P3 — Bloqueado por evidencia externa (no técnico, no accionable por un agente)

### P3.1 — OE11 (evidencia Nivel 3 en aula) pendiente — 🟡 EN CURSO

Ver [`ROADMAP.md`](ROADMAP.md) §2-4.

- **OE10 — ✅ CUMPLIDO el 2026-07-30.** Aprobación humana explícita («Aprobado para llevar al aula y
  testear con estudiantes») → `ejercicio_state.json` en **11/11** pasos y el subproyecto promovido a
  `02-En-Desarrollo/` con todas sus rutas resincronizadas.
- **OE11 — pendiente.** Requiere aplicación del ítem con estudiantes reales de grado 10-11 y análisis
  de diagnosticidad por distractor (sobre el pool de **siete** códigos, `EST-PER-01` a `07`) — el gate
  que la validación automática **no** puede sustituir (ver [`ROADMAP.md`](ROADMAP.md) §4). Es el único
  objetivo abierto del subproyecto.

Qué mirar cuando vuelvan los datos del aula: si algún distractor no lo elige **nadie**, no discrimina
y conviene revisarlo; y si `EST-PER-01` (`n^(n-1)`) resulta el más elegido, coincidiría con lo que
reporta la ficha oficial del ítem, lo que sería evidencia de que la generalización de `n = 4` a
`n ∈ {4,5,6}` conserva el comportamiento del original.

Destino en producción ya reservado:
`03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/10-Combinatoria_Permutaciones-Variaciones-Combinaciones/permutaciones_pescadores_venia_n4/`.

---

## Referencias cruzadas

- [`../README.md`](../README.md) · [`../HANDOFF.md`](../HANDOFF.md)
- [`SYLLABUS.md`](SYLLABUS.md) · [`ROADMAP.md`](ROADMAP.md) · [`BLUEPRINT.md`](BLUEPRINT.md)
- [`../.claude/CLAUDE.md`](../.claude/CLAUDE.md) — particularidad 13 (decisión D4)
- `../../../../.claude/rules/diversidad-sustantiva.md` — regla #22, contexto de P1.2
- `../../../../.claude/rules/ortografia-espanol.md` — regla #7, contexto de P1.1
- `../../plano-cartesiano-barco-n2/docs/BACKLOG.md` — precedente del bloqueo de auto-contención
  (su P1.2) y del formato de este documento
- [`../.claude/rules/permutaciones-parametricas.md`](../.claude/rules/permutaciones-parametricas.md)
  — contrato local: la clave `n!`, el pool de siete errores conceptuales y las invariantes I-1..I-7

---

**Versión**: 2.0 (H1 y P2.1 cerrados; `copias` de Moodle corregido de 100 a 300)
**Fecha**: 2026-07-30
