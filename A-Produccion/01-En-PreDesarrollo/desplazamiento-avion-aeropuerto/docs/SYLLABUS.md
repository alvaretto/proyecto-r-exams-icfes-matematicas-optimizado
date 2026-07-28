# Syllabus — Desplazamiento avión→aeropuerto

> Ver [`../HANDOFF.md`](../HANDOFF.md) para el estado de trabajo completo. Este documento
> describe **qué enseña y evalúa** el ejercicio, no su estado de desarrollo.

## 1. Ficha de metadatos ICFES (copia literal del `.Rmd`)

**Regla dura de este documento**: los campos siguientes son texto oficial ICFES y se copian
**carácter por carácter** de la sección `Meta-information` del `.Rmd`
(`desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd`, líneas
740-760 — el `.Rmd` creció de 667 a 760 líneas el 2026-07-28 al ampliar el pool de errores y
retirar el reseed; el archivo sigue en desarrollo activo, cita re-verificada con `grep -n` el
2026-07-28, ver nota sobre estabilidad de citas en [BLUEPRINT.md](BLUEPRINT.md#6-verificación-de-citas-de-línea-2026-07-28-última-pasada)).
Ninguno se parafrasea. Si algún campo generara
duda de fidelidad se marcaría `[VERIFICAR]` en vez de reformularse — no fue necesario en esta
revisión, los 13 campos `exextra[...]` coinciden con el texto fuente.

| Campo (`.Rmd`) | Valor literal |
|---|---|
| `extype` | `schoice` |
| `exshuffle` | `FALSE` (mezcla interna con `sample()`, ver [BLUEPRINT §4](BLUEPRINT.md#4-decisiones-de-diseño-con-su-porqué)) |
| `exextra[Type]` | `SCHOICE` |
| `exextra[Competencia]` | `Interpretacion` |
| `exextra[Componente]` | `Geometrico-Metrico` |
| `exextra[Afirmacion]` | `Comprende y transforma la informacion cuantitativa y esquematica presentada en distintos formatos.` |
| `exextra[Evidencia]` | `Transforma la representacion de una o mas piezas de informacion.` |
| `exextra[Nivel]` | `3` |
| `exextra[DOK]` | `3` |
| `exextra[Bloom]` | `Analizar` |
| `exextra[SOLO]` | `Relacional` |
| `exextra[TipoMetacognicion]` | `evaluacion_representacion_grafica` |
| `exextra[Descriptor]` | `D3.8 - Selecciona informacion necesaria para resolver problemas que involucran caracteristicas medibles de figuras geometricas elementales.` |
| `exextra[Estandar]` | `Identifico caracteristicas de localizacion de objetos en sistemas de representacion cartesiana y geografica. (6-7, Pensamiento espacial)` |
| `exextra[Origen]` | `MAT-2026-1-020 (cuadernillo 2026-1, pregunta 114)` |

**Nota de coherencia DOK↔Nivel** (regla obligatoria en
`../../../../.claude/rules/ejercicios-metacognitivos.md`, sección "Coherencia Nivel ICFES ↔ DOK"):
DOK 3 con Bloom "Analizar/Evaluar" exige Nivel ICFES ≥ 3. Aquí `Nivel = 3`, coherente.

**Nota de discrepancia con `ejercicio_state.json` — RESUELTA 2026-07-28**: el paso
`analisis_icfes` de `ejercicio_state.json` había quedado (registro original 2026-06-27T07:32)
con valores tempranos del análisis (`"nivel": 2, "dok": 2, "bloom": "Comprender"`) que no
coincidían con los metadatos finales del `.Rmd` (Nivel 3 / DOK 3 / Bloom Analizar). El campo se
corrigió y hoy `ejercicio_state.json` reporta `"nivel": 3, "dok": 3, "bloom": "Analizar",
"solo": "Relacional"` — coherente con el `.Rmd`. Ver [BACKLOG.md](BACKLOG.md) P1.2 (RESUELTO).

## 2. Qué evalúa el ítem

El enunciado (ver cualquiera de las 8 plantillas narrativas del pool en `data_generation`, §10,
líneas 363-518) presenta un avión a `distancia_total` km del aeropuerto en una dirección dada
(`dir_desc`), que avanza `distancia_avanzada` km hacia el aeropuerto sin cambiar de rumbo. El
estudiante debe identificar, entre **cuatro diagramas vectoriales**, cuál representa
correctamente la nueva posición.

Este ítem **no evalúa cálculo aritmético**. La resta `distancia_total - distancia_avanzada` es
trivial y se explicita paso a paso en la sección "Procedimiento correcto" de la Solution (`.Rmd`
líneas 685-696). Lo que evalúa es si el
estudiante **integra correctamente dos magnitudes en una representación gráfica simultánea**:

1. **Magnitud (distancia)**: ¿la longitud del segmento en el diagrama corresponde a la distancia
   restante correcta?
2. **Dirección (ángulo + lado del eje)**: ¿el ángulo se mide desde el eje cardinal correcto y
   hacia el lado correcto (este u oeste del eje norte/sur)?

Un estudiante que solo verifica la distancia (ignorando la dirección) o solo verifica el ángulo
(ignorando la magnitud) puede seleccionar un distractor. Esto corresponde a Bloom "Analizar"
(descomponer la representación en sus dos componentes y verificar cada una contra el enunciado)
y a DOK 3 (pensamiento estratégico no rutinario: no hay un procedimiento de un solo paso que
resuelva el ítem por sí mismo, se requiere coordinar dos criterios de verificación).

## 3. Pool de errores conceptuales (distractores diagnósticos)

Los distractores **no son ruido numérico**: cada uno es un error conceptual documentado, con
código, nombre y causa raíz, generado por la misma función `dibujar_diagrama()` que dibuja la
opción correcta — la diferencia entre opciones es exclusivamente los parámetros que reciben
(distancia y/o ángulo/eje), nunca el método de dibujo. El pool tiene **7 errores candidatos**
(`.Rmd` §5, líneas 180-246); cada versión presenta **3 de los 7**: `GEO-DES-01` (espejo)
**siempre** está presente — es el discriminador central del ítem —, y los otros 2 se sortean
entre `{GEO-DES-02, GEO-DES-03, GEO-DES-04, GEO-DES-05, GEO-DES-06, GEO-DES-07}`, filtrados
primero por su `precondicion` y luego por una **cascada de ratios de legibilidad**
(`c(0.40, 0.35, 0.30, 0.25)`, degradando de a un escalón solo si la versión no admite ninguna
pareja válida en el escalón anterior; bloque de selección en `.Rmd` §7, líneas 255-348).

**Por qué el pool mezcla las tres clases de magnitud** (mayor, igual y menor que la distancia
correcta): si todos los distractores fueran siempre más largos o más cortos que la respuesta
correcta, la longitud del vector delataría la opción por sí sola, sin que el estudiante tuviera
que leer distancia y dirección (ver `../../../../.claude/rules/diversidad-sustantiva.md` §P5). Con
representantes de las tres clases, la longitud deja de ser una pista confiable.

| Código | Nombre | Qué diagnostica | Longitud vs. correcta | Precondición | Causa raíz (copiada del `.Rmd`) |
|---|---|---|---|---|---|
| `GEO-DES-01` | Dirección reflejada (lado opuesto del eje) | El estudiante calcula bien la **distancia** pero invierte el **lado** del eje hacia el que mide el ángulo (confunde este/oeste del mismo eje norte o sur) | igual | siempre | "Confusión sobre el lado del eje [...] hacia el cual se mide el ángulo (este por oeste). El estudiante obtiene la distancia correcta pero invierte la orientación lateral, sin integrar bien la dirección dada en el enunciado." |
| `GEO-DES-02` | Distancia recorrida en vez de restante | El estudiante confunde el trayecto **ya recorrido** con la **nueva posición** — no resta, reporta el desplazamiento mismo como si fuera la distancia al destino | menor o mayor* | siempre | "Confusión entre distancia recorrida y distancia restante. El estudiante lee correctamente los [...] km del enunciado pero no los resta de los [...] km iniciales." |
| `GEO-DES-03` | Suma en vez de resta | El estudiante interpreta "avanzar hacia" como alejamiento adicional en la misma dirección, en vez de acercamiento | mayor | siempre | "Interpretación errónea del sentido del movimiento: 'hacia' se interpreta como 'en dirección a' (alejándose) en lugar de 'aproximándose a'." |
| `GEO-DES-04` | Posición inicial sin actualizar | El estudiante identifica bien la dirección, pero no aplica el avance: deja la distancia inicial como si desplazarse no hubiera cambiado la posición | mayor | siempre | "No se interpreta que desplazarse hacia un punto de referencia cambia la distancia a ese punto. Se copia el dato del enunciado sin operar con él." |
| `GEO-DES-05` | Ángulo medido desde el eje perpendicular | El estudiante calcula bien la distancia, pero intercambia los dos ejes de una dirección cardinal compuesta: mide el ángulo desde el eje que se nombra en segundo lugar, no desde el primero | igual | `ángulo ≠ 45°` | "Confusión en el orden de lectura de una dirección cardinal compuesta. El ángulo se mide desde el eje que se nombra en segundo lugar, no desde el primero." |
| `GEO-DES-06` | Resta aplicada dos veces | El estudiante identifica correctamente que hay que restar, pero repite la operación — es un error de control del procedimiento, no de comprensión del concepto | menor | `distancia_avanzada < distancia_total / 2` y `distancia_total ≠ 3 × distancia_avanzada` | "Duplicación de la operación al reprocesar el dato. Es un error de control del procedimiento, no de comprensión del concepto: restar es lo correcto, pero se repite." |
| `GEO-DES-07` | Ángulo medido desde el eje cardinal opuesto | El estudiante calcula bien la distancia **y** el lado hacia el que se abre el ángulo, pero mide los grados desde el eje cardinal opuesto (norte↔sur) al que indica el enunciado — conserva distancia y lado, falla solo en el eje de referencia | igual | siempre | "Lectura incompleta de la notación de rumbo: se retiene el lado hacia el que se abre el ángulo [...] pero se pierde el eje desde el cual se mide [...]. El estudiante opera bien con la distancia y con el lado, y falla solo en el eje de referencia." |

*`GEO-DES-02` puede ser mayor o menor que la correcta según cuál de `distancia_avanzada` /
`distancia_restante` sea mayor en esa versión — no tiene un signo fijo, a diferencia de los demás.

Cada error tiene una función `calcula()` (pura, sin `sample`/`runif` internos — cumple la regla
de determinismo de `../../../../.claude/rules/ejercicios-metacognitivos.md`) y una `precondicion`
declarada: cinco (`GEO-DES-01/02/03/04/07`) son `function(params) TRUE` (aplican siempre); dos
(`GEO-DES-05/06`) son condicionales, para evitar los casos degenerados en los que el error
coincidiría exactamente con otra opción o produciría un valor no representable.

Como cada versión sortea 2 de los 6 errores candidatos restantes (`GEO-DES-02` a `GEO-DES-07`,
además de `GEO-DES-01`, fijo), dos estudiantes con semillas distintas no ven necesariamente el
mismo conjunto de distractores — la combinación de trampas presentadas también varía entre
versiones, no solo los valores numéricos. Con el pool de 7, hay **tres** distractores que
comparten la magnitud exacta de la correcta (`GEO-DES-01`, `GEO-DES-05`, `GEO-DES-07`), así que
el rótulo de distancia visible en el diagrama coincide con el de la correcta con mucha más
frecuencia que antes (medición exacta abajo), obligando al estudiante a verificar dirección, no
solo magnitud.

**Nota de diseño (regla #22 §P5) — evolución 2026-07-28**: el pool y el mecanismo de selección
pasaron por tres correcciones el mismo día, tras una revisión adversarial doble:

- **H3 (RESUELTO)**: con solo 3 distractores fijos, `GEO-DES-03` (suma) producía por
  construcción algebraica el diagrama de mayor longitud entre las cuatro opciones en el 100% de
  las versiones (`escala_px_km` se derivaba de `distancia_total + distancia_avanzada`, el mismo
  valor de `GEO-DES-03`). Se amplió el pool a 6 errores y se desacopló la escala (ver
  [BLUEPRINT.md §4](BLUEPRINT.md#4-decisiones-de-diseño-con-su-porqué)).
- **H4 (RESUELTO)**: incluso con el pool de 6, el rótulo de distancia visible en cada diagrama
  permitía descartar opciones sin razonar sobre la dirección (solo 2 candidatas quedaban en el
  40% de las versiones). Se agregó el séptimo error, `GEO-DES-07`, que conserva distancia **y**
  lado — el pool sube a 7 y a tres los distractores que comparten magnitud con la correcta.
- **H7 (RESUELTO)**: el umbral único de legibilidad (ratio 0.25) dejaba el vector más corto en
  30 px en ~10% de las versiones, con el arco y la etiqueta del ángulo sobresaliendo del propio
  vector. Se reemplazó por la cascada de ratios `c(0.40, 0.35, 0.30, 0.25)` descrita arriba.

Medición final tras los tres fixes (ver HANDOFF.md §5.2): sobre 40 semillas, la correcta alcanza
el rank 1 de longitud en 18/40 (45%, antes 0/200), rank 2 en 18/40 y rank 3 en 4/40, con el
vector más corto en 48 px de mínimo (antes 30 px, mediana 75,7 px). Sobre 80 semillas, las
opciones que comparten el rótulo de distancia con la correcta se reparten en 2 (24%), 3 (60%) y
4 (16%) de las versiones — en ese 16% el filtro numérico ya no descarta ninguna opción. Ver
[BACKLOG.md](BACKLOG.md) P0.1 (H3), P1.4 (H7) y P1.5 (H4) para el detalle completo de cada
hallazgo.

## 4. Prerrequisitos del estudiante

- Lectura de un plano con ejes cardinales (N/S/E/O) y ángulos medidos desde un eje de referencia.
- Noción de que "acercarse" a un punto de referencia implica **restar** distancia, y
  "alejarse" implica **sumar**.
- Comparación proporcional de longitudes en una representación a escala (los cuatro diagramas
  comparten la misma `escala_px_km`, ver [BLUEPRINT.md](BLUEPRINT.md)).
- Vocabulario geométrico básico: dirección, ángulo, magnitud, eje.

No se requiere trigonometría explícita ni cálculo con fórmulas — el ítem es de **lectura e
interpretación de una representación esquemática**, coherente con el componente
Geométrico-Métrico y la competencia Interpretación y Representación.

## 5. Referencias cruzadas

- [`../HANDOFF.md`](../HANDOFF.md) — estado de trabajo, anatomía del `.Rmd`, decisiones
- [`BLUEPRINT.md`](BLUEPRINT.md) — arquitectura técnica y contrato de `dibujar_diagrama()`
- [`BACKLOG.md`](BACKLOG.md) — hallazgos P0.1 (`GEO-DES-03` extremo), P1.4 (piso de
  legibilidad) y P1.5 (rótulo numérico descartaba opciones) — los tres RESUELTOS 2026-07-28,
  ver §3
- `../../../../.claude/rules/ejercicios-metacognitivos.md` — Progressive Disclosure, pool de
  errores, coherencia DOK↔Nivel
- `../../../../.claude/rules/graficos-como-opciones.md` — opciones gráficas como PNG individuales
- `../../../../.claude/docs/patrones-errores-conocidos.md` — Errores 22, 23, 24
