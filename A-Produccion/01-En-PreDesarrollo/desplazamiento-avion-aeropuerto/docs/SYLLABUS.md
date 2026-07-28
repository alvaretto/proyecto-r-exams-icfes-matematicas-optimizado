# Syllabus — Desplazamiento avión→aeropuerto

> Ver [`../HANDOFF.md`](../HANDOFF.md) para el estado de trabajo completo. Este documento
> describe **qué enseña y evalúa** el ejercicio, no su estado de desarrollo.

## 1. Ficha de metadatos ICFES (copia literal del `.Rmd`)

**Regla dura de este documento**: los campos siguientes son texto oficial ICFES y se copian
**carácter por carácter** de la sección `Meta-information` del `.Rmd`
(`desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd`, líneas
544-561). Ninguno se parafrasea. Si algún campo generara duda de fidelidad se marcaría
`[VERIFICAR]` en vez de reformularse — no fue necesario en esta revisión, los 13 campos
`exextra[...]` coinciden con el texto fuente.

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
`../../../.claude/rules/ejercicios-metacognitivos.md`, sección "Coherencia Nivel ICFES ↔ DOK"):
DOK 3 con Bloom "Analizar/Evaluar" exige Nivel ICFES ≥ 3. Aquí `Nivel = 3`, coherente.

**Nota de discrepancia con `ejercicio_state.json`**: el paso `analisis_icfes` de
`ejercicio_state.json` (registrado 2026-06-27T07:32) quedó con valores tempranos del análisis
(`"nivel": 2, "dok": 2, "bloom": "Comprender", "solo": "Relacional"`) que **no** coinciden con
los metadatos finales del `.Rmd` (Nivel 3 / DOK 3 / Bloom Analizar). El `.Rmd` es la fuente de
verdad — es el artefacto que se renderiza y se evalúa — pero el campo JSON quedó desactualizado.
Ítem de backlog: ver [BACKLOG.md](BACKLOG.md) "Re-confirmar pasos de `ejercicio_state.json`".

## 2. Qué evalúa el ítem

El enunciado (ver cualquiera de las 8 plantillas narrativas del pool en `data_generation`,
líneas 166-319) presenta un avión a `distancia_total` km del aeropuerto en una dirección dada
(`dir_desc`), que avanza `distancia_avanzada` km hacia el aeropuerto sin cambiar de rumbo. El
estudiante debe identificar, entre **cuatro diagramas vectoriales**, cuál representa
correctamente la nueva posición.

Este ítem **no evalúa cálculo aritmético**. La resta `distancia_total - distancia_avanzada` es
trivial y se explicita paso a paso en la Solution (`.Rmd` líneas 486-499). Lo que evalúa es si el
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

Los tres distractores **no son ruido numérico**: cada uno es un error conceptual documentado,
con código, nombre y causa raíz, generado por la misma función `dibujar_diagrama()` que dibuja
la opción correcta — la diferencia entre opciones es exclusivamente los parámetros que reciben
(distancia y/o ángulo/eje), nunca el método de dibujo. Pool completo en `.Rmd` líneas 127-155.

| Código | Nombre | Qué diagnostica | Causa raíz (copiada del `.Rmd`) |
|---|---|---|---|
| `GEO-DES-01` | Dirección reflejada (lado opuesto del eje) | El estudiante calcula bien la **distancia** pero invierte el **lado** del eje hacia el que mide el ángulo (confunde este/oeste del mismo eje norte o sur) | "Confusión sobre el lado del eje [...] hacia el cual se mide el ángulo (este por oeste). El estudiante obtiene la distancia correcta pero invierte la orientación lateral, sin integrar bien la dirección dada en el enunciado." |
| `GEO-DES-02` | Distancia recorrida en vez de restante | El estudiante confunde el trayecto **ya recorrido** con la **nueva posición** — no resta, reporta el desplazamiento mismo como si fuera la distancia al destino | "Confusión entre distancia recorrida y distancia restante. El estudiante lee correctamente los [...] km del enunciado pero no los resta de los [...] km iniciales." |
| `GEO-DES-03` | Suma en vez de resta | El estudiante interpreta "avanzar hacia" como alejamiento adicional en la misma dirección, en vez de acercamiento | "Interpretación errónea del sentido del movimiento: 'hacia' se interpreta como 'en dirección a' (alejándose) en lugar de 'aproximándose a'." |

Cada error tiene una función `calcula()` (pura, sin `sample`/`runif` internos — cumple la regla
de determinismo de `../../../.claude/rules/ejercicios-metacognitivos.md`) y una `precondicion`
que en los tres casos es `function(params) TRUE` (aplican siempre, no dependen de paridad ni de
otras propiedades condicionales de los datos).

**Nota de diseño (regla #22 §P5, ver [BACKLOG.md](BACKLOG.md) P0)**: `GEO-DES-03` (suma) produce
por construcción algebraica el diagrama de mayor longitud entre las cuatro opciones en el
100% de las versiones — ver el hallazgo P0 del backlog para el detalle y el fix recomendado.

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
- [`BACKLOG.md`](BACKLOG.md) — hallazgo P0 sobre `GEO-DES-03`
- `../../../.claude/rules/ejercicios-metacognitivos.md` — Progressive Disclosure, pool de
  errores, coherencia DOK↔Nivel
- `../../../.claude/rules/graficos-como-opciones.md` — opciones gráficas como PNG individuales
- `../../../.claude/docs/patrones-errores-conocidos.md` — Errores 22, 23, 24
