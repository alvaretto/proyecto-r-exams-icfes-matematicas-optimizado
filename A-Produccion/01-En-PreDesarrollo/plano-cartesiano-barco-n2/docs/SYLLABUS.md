# Syllabus — Coordenadas de los vértices de un barco en el plano cartesiano

> Ver [`../HANDOFF.md`](../HANDOFF.md) para el estado de trabajo completo. Este documento
> describe **qué enseña y evalúa** el ejercicio, no su estado de desarrollo.

## 1. Ficha de metadatos ICFES (copia literal del `.Rmd`)

**Regla dura de este documento**: los campos siguientes son texto oficial del `.Rmd` y se copian
**carácter por carácter** de la sección `Meta-information`
(`../coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd`,
líneas 467-483, re-verificadas con `grep -n` el 2026-07-28 tras P2.5/P2.7). Ninguno se parafrasea.

| Campo (`.Rmd`) | Valor literal |
|---|---|
| `exname` | `coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1` |
| `extype` | `schoice` |
| `exsolution` | `` `r paste(sol, collapse = "")` `` (vector binario dinámico, ver §3) |
| `exshuffle` | `TRUE` (las opciones son de texto; la Solution identifica cada una por su contenido y código de error, nunca por letra — regla #19; ver [BLUEPRINT.md](BLUEPRINT.md)) |
| `extol` | `0` |
| `exextra[Type]` | `SCHOICE` |
| `exextra[Competencia]` | `Interpretacion` |
| `exextra[Componente]` | `Geometrico-metrico` |
| `exextra[Afirmacion]` | `Comprende y transforma la informacion cuantitativa y esquematica presentada en distintos formatos` |
| `exextra[Evidencia]` | `Da cuenta de las caracteristicas basicas de la informacion presentada en diferentes formatos` |
| `exextra[Nivel]` | `2` |
| `exextra[DOK]` | `2` |
| `exextra[Bloom]` | `Comprender` |
| `exextra[SOLO]` | `Multi-estructural` |
| `exextra[TipoMetacognicion]` | `analisis_error` |

**Nota de coherencia DOK↔Nivel** (regla obligatoria en
`../../../../.claude/rules/ejercicios-metacognitivos.md`, sección "Coherencia Nivel ICFES ↔ DOK"):
DOK 2 con Bloom "Comprender" es compatible con Nivel ICFES 1 o 2. Aquí `Nivel = 2`, coherente —
no hay una discrepancia como la que tuvo que corregirse en el subproyecto hermano
`desplazamiento-avion-aeropuerto` (DOK 3 exigiendo Nivel ≥ 3).

## 2. Qué evalúa el ítem

El enunciado (chunk `question_body`, líneas 310-322) ubica a un protagonista jugando "batalla
naval": tiene un barco en un plano cartesiano de 10×10 (`grid_max <- 10L`, línea 21) y debe leer,
a partir de la figura `plano_barco.png`, las coordenadas `(x, y)` de los cuatro vértices que
encierran el barco.

Este ítem **no evalúa cálculo aritmético** — no hay ninguna operación que resolver, solo lectura
de un plano. Lo que evalúa es si el estudiante:

1. **Distingue el eje x del eje y**: lee la extensión horizontal del barco como coordenada x y la
   extensión vertical como coordenada y, sin invertirlas.
2. **Identifica el rango completo del objeto**, no solo su punto medio o un tramo parcial.
3. **Lee las marcas del eje, no los cuadros de la cuadrícula**, y combina correctamente los valores
   extremos en los cuatro pares ordenados que forman el rectángulo.

Esto corresponde a Bloom "Comprender" (interpretar una representación gráfica y traducirla a
notación de coordenadas) y a DOK 2 (aplicación de una habilidad/concepto en un contexto
concreto, sin que se requiera una estrategia de varios pasos no rutinaria) — coherente con
Nivel ICFES 2.

## 3. Pool de errores conceptuales (distractores diagnósticos)

Los tres distractores **no son ruido numérico**: cada uno es un error conceptual documentado, con
código, nombre y diagnóstico explícito en la Solution (`errores_info`, líneas 112-151). Los tres
se construyen a partir de las mismas variables `x_min`, `x_max`, `y_min`, `y_max` que generan la
respuesta correcta (líneas 46-52) — la diferencia entre opciones es exclusivamente cómo se leen o
combinan esas coordenadas, nunca datos distintos.

| Código | Nombre | Cómo se construye (`.Rmd`) | Qué diagnostica |
|---|---|---|---|
| `GEO-COORD-01` | Inversión de ejes | Líneas 64-70: intercambia cada par `(x, y)` por `(y, x)` | El estudiante calcula bien la extensión del barco pero invierte los ejes: lee la extensión horizontal como si fuera vertical y viceversa. Es el error más frecuente al leer coordenadas: asumir que "el primer número siempre es x" sin verificarlo contra el eje horizontal real. |
| `GEO-COORD-02` | Rango reducido al centro | Líneas 72-78: usa `x_mid = floor((x_min+x_max)/2)` y `x_mid+1` en vez de `x_min` y `x_max`, conservando `y_min`/`y_max` correctos | El estudiante identifica bien el eje vertical, pero en el horizontal no localiza los extremos izquierdo y derecho del barco — reporta solo una franja de 1 unidad en el centro, como si el "ancho" del objeto no importara. |
| `GEO-COORD-04` | Desplazamiento de una unidad al contar la cuadrícula | Líneas 80-100: desplaza el rango horizontal una unidad (`x_min + d`, `x_max + d`, con `d = +1` o `−1` según el margen que quede en la grilla) y conserva el rango vertical correcto | El estudiante identifica bien la forma del rectángulo y el rango vertical, pero cuenta los **cuadros** de la cuadrícula en lugar de las **marcas** del eje — o empieza a contar desde el primer cuadro que ocupa el barco en vez de desde la línea donde comienza. Es el error de lectura de escala más frecuente en planos cuadriculados. |

> **Nota histórica.** Hasta el 2026-07-28 el tercer distractor era `GEO-COORD-03` («puntos sobre la
> diagonal `y = x`»). Se retiró porque era el único cuyos cuatro puntos tenían la forma `(v,v)` —
> colineales — mientras los otros tres comparten la estructura «2 valores de x × 2 de y». Eso
> permitía descartarlo por la **forma del texto**, sin mirar la figura ni leer una coordenada, y
> subía el acierto por azar del 25 % al 33 %. Verificado por enumeración exhaustiva sobre el espacio
> de 222 vigente entonces: rompía la estructura en 222/222 versiones (re-confirmado en 374/374 tras
> ampliar el espacio en P2.7). Ver [`BACKLOG.md`](BACKLOG.md) P0.1.

**Plausibilidad para un estudiante de grado 10-11**: los tres errores son transcripciones
razonables de una lectura apresurada del plano — no requieren un malentendido exótico. Invertir
`(x, y)` por `(y, x)` (`GEO-COORD-01`) es el error de coordenadas más documentado en la literatura
de errores geométricos; tomar solo el centro (`GEO-COORD-02`) ocurre cuando el estudiante ubica el
barco pero no traza mentalmente sus bordes; y el desplazamiento de una unidad (`GEO-COORD-04`)
surge de contar cuadros en vez de marcas del eje, un error de lectura de escala que aparece en
cualquier representación cuadriculada.

**Los cuatro comparten la misma estructura.** Las cuatro opciones —correcta y distractores— tienen
la forma «2 valores de x × 2 valores de y en las 4 combinaciones», que es la estructura real de los
vértices de un rectángulo alineado a los ejes. Verificado en 374/374 versiones (222/222 en el
momento de P0.1, re-confirmado sobre el espacio ampliado tras P2.7). Ninguna se puede descartar por
su forma: hay que leer las coordenadas del barco.

**Nota de diseño — unicidad garantizada por el `stopifnot`, no por exclusiones del pool**: hasta el
2026-07-28, `y_min` se sorteaba de un `y_pool` filtrado con 4 exclusiones que evitaban que el
distractor de inversión coincidiera con la respuesta correcta o entre sí (`y_min != x_min`,
`y_max != x_max`, `y_min != x_max`, `y_max != x_min`). Se midió (`docs/BACKLOG.md` P2.7) que
**ninguna era necesaria**: la justificación de la primera («evita que `GEO-COORD-01` colapse sobre
la correcta») era falsa, porque esa colisión exigiría `alto_barco == 0`, imposible con
`alto_barco >= 1`. Las cuatro se retiraron; `y_pool` recorre hoy **todo** el rango disponible
(línea 49). El chunk conserva dos redes de seguridad en tiempo de ejecución:
`stopifnot(length(y_pool) > 0L)` (línea 50), por si el pool quedara vacío, y
`stopifnot(length(unique(all_opts)) == 4L)` (línea 103), por si dos opciones coincidieran — esta
última es la que realmente garantiza la unicidad.

**Ninguna de las dos puede dispararse dentro del espacio de parámetros declarado.** Se comprobó por
enumeración exhaustiva (2026-07-28, re-medida tras retirar las exclusiones) de las combinaciones
`(ancho, alto, x_min, y_min)`: **374 combinaciones válidas → 374 respuestas correctas distintas**
(222 antes de retirar las exclusiones), con **0** casos de `y_pool` vacío y **0** colisiones entre
opciones. Los `stopifnot` no son código muerto: documentan la invariante y protegerían ante un
cambio futuro de los rangos de `ancho_barco`/`alto_barco`.

Cada error tiene su texto (`opciones_pre`, líneas 108-113) mezclado con `sample(4L)` (línea 114) y
un vector `sol` derivado del mismo orden mezclado (línea 117) — la mezcla determina tanto el
Answerlist como `exsolution`, sin una ruta de cálculo separada para cada uno (cumple la Familia 4
de `../../../../.claude/rules/familias-soluciones-rmd.md`, coherencia de marcas).

### 3.1 Solution con las 6 subsecciones canónicas

Además del análisis por opción (correcta + 3 distractores con su código y diagnóstico), la
Solution (chunk `solucion`, líneas 343-451) cubre las seis subsecciones que exige la regla #1
(`ejercicios-metacognitivos.md`, «Sección Solution Obligatoria»): *Análisis del error*,
*Procedimiento correcto*, **Propiedades del concepto** (líneas 397-415: cuatro propiedades sobre
pares ordenados y rectángulos alineados a los ejes), **Caso específico** (líneas 416-437:
transferencia — desplazar el barco 1 unidad en vertical y recalcular los vértices), *Reflexión
metacognitiva* y *Estrategia para evitar el error*. Las dos subsecciones en negrita se añadieron el
2026-07-28 (ver `docs/BACKLOG.md` P2.5).

## 4. Prerrequisitos del estudiante

- Lectura de un plano cartesiano de un solo cuadrante (valores positivos en ambos ejes).
- Distinción entre eje horizontal (x) y eje vertical (y), y de la convención de escritura
  `(x, y)`.
- Noción de que un rectángulo queda determinado por sus valores extremos en cada eje (mínimo y
  máximo de x, mínimo y máximo de y), no por un único punto central.
- No se requiere fórmula de área, perímetro ni distancia entre puntos — el ítem es de **lectura
  directa de coordenadas**, coherente con el componente Geométrico-métrico y la competencia
  Interpretación y representación.

## 5. Referencias cruzadas

- [`../HANDOFF.md`](../HANDOFF.md) — estado de trabajo, decisiones, hallazgos abiertos
- [`../README.md`](../README.md) — cómo verificar, renderizar y exportar el ejercicio
- [`BLUEPRINT.md`](BLUEPRINT.md) — arquitectura técnica de `dibujar_barco()` y del pipeline de
  mezcla de opciones
- [`BACKLOG.md`](BACKLOG.md) — pendientes priorizados
- [`ROADMAP.md`](ROADMAP.md) — hitos con fechas
- `../../../../.claude/rules/ejercicios-metacognitivos.md` — Progressive Disclosure, pool de
  errores, coherencia DOK↔Nivel
- `../../../../.claude/rules/solution-letter-independence.md` — regla #19, por qué la Solution
  identifica opciones por código de error y no por letra
- `../../../../.claude/rules/familias-soluciones-rmd.md` — Familia 1 (`pick_int`), Familia 4
  (coherencia de marcas), Familia 5 (`safe_sample`)
- `../../../../.claude/rules/diversidad-sustantiva.md` — regla #22, por qué los parámetros del
  barco están aleatorizados y no son literales fijos

---

**Versión:** 1.1 · **Fecha:** 2026-07-28 (v1.1 — P2.5: nueva §3.1 sobre las 6 subsecciones de la
Solution; P2.7: `y_pool` sin exclusiones, espacio de versiones 222 → 374; citas de línea
re-verificadas)
