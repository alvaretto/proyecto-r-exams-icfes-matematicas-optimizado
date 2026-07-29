# Blueprint — Coordenadas de vértices en el plano cartesiano (barco)

> Arquitectura técnica del ejercicio. Para el encuadre pedagógico ver
> [`SYLLABUS.md`](SYLLABUS.md); para el estado de trabajo ver
> [`../HANDOFF.md`](../HANDOFF.md).

| Campo | Valor |
|---|---|
| **Archivo fuente** | `coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd` |
| **Líneas** | 483 (verificado con `wc -l`, 2026-07-28, tras P2.5/P2.7) |
| **Chunks R** | 5 |
| **Lenguaje gráfico** | R nativo (ggplot2) — decidido en Flujo B |
| **Tipo** | SCHOICE, opciones de **texto**, una figura compartida |
| **Auto-contenido** | **Sí, obligatorio** (ver §5, invariante I-1) |

---

## 1. Pipeline de generación

```
semilla (R RNG)
    │
    ├─► ancho_barco ∈ [3,6]        línea 24
    ├─► alto_barco  ∈ [1,2]        línea 25
    ├─► x_min ∈ [1, 10-ancho]      línea 27  ──►  x_max = x_min + ancho   línea 28
    │
    ├─► y_pool: 1:(10-alto), SIN exclusiones (P2.7, retiradas 2026-07-28)  líneas 30-49
    │      └─► y_min ← safe_sample(y_pool)        línea 51
    │             └─► y_max = y_min + alto        línea 52
    │
    ├─► correcta           = bounding box [x_min,x_max] × [y_min,y_max]   líneas 55-62
    ├─► GEO-COORD-01 inv   = (x,y) escrito como (y,x)                     líneas 64-70
    ├─► GEO-COORD-02 cen   = franja central de 1 unidad                   líneas 72-78
    ├─► GEO-COORD-04 desp  = rango horizontal desplazado 1 unidad         líneas 80-100
    │      └─► stopifnot(4 opciones distintas + dentro de la grilla)      líneas 102-105
    │
    ├─► perm ← sample(4); sol ← posición de "correcta"                   líneas 108-117
    ├─► protagonista ← 1 de 8 nombres                                    líneas 162-167
    ├─► reflexion    ← 1 de 4 textos                                     líneas 169-195
    │
    └─► dibujar_barco(x_min, x_max, y_min, y_max, 10, "plano_barco.png") línea 305
              └─► PNG 7×7 in @150 dpi, fondo blanco                      línea 302
```

**Espacio de versiones** (enumeración exhaustiva, no muestreo — 2026-07-28, re-medido tras P2.7):

| Dimensión | Cardinalidad |
|---|---|
| Combinaciones geométricas `(ancho, alto, x_min, y_min)` válidas | **374** (222 antes de P2.7; +68 % al retirar las 4 exclusiones de `y_pool`) |
| Respuestas correctas **distintas** | **374** (biyección: no hay dos combinaciones con la misma clave) |
| × protagonistas | 8 |
| × reflexiones metacognitivas | 4 |
| × órdenes de opción (`exshuffle: TRUE`) | 24 |
| **Renders distintos posibles** | 374 × 8 × 4 × 24 = **287 232** |

La cifra que importa para la regla #22 es la primera: **374 preguntas sustantivamente
distintas**. Las otras tres dimensiones son envoltorio (protagonista, reflexión, orden), y por sí
solas no constituirían diversidad.

---

## 2. Anatomía del `.Rmd` (5 chunks)

| # | Chunk | Líneas | Qué hace |
|---|---|---|---|
| 1 | `data_generation` | 1-309 | Todo el cálculo: helpers, parámetros, opciones, pool de errores, protagonista, reflexión, y la función `dibujar_barco()` con su llamada |
| — | guard LaTeX | 315 | `\@ifundefined{c@none}{\newcounter{none}}{}` — regla #20 |
| 2 | `question_body` | 318-330 | Enunciado + imagen (`{width=80%}`, línea 325) + pregunta |
| 3 | `answerlist_q` | 334-338 | Emite las 4 opciones del enunciado |
| 4 | `solucion` | 343-451 | Respuesta correcta, análisis por opción, procedimiento, **propiedades del concepto** (P2.5), **caso específico** (P2.5), reflexión, estrategia |
| 5 | `answerlist_s` | 455-465 | Feedback por opción (Correcto/Incorrecto + código de error) |

La consolidación de 8 chunks a 5 se hizo en una sesión previa (commit pendiente al inicio de
2026-07-28) y se conserva: menos chunks significa menos puntos donde el estado del entorno puede
divergir entre formatos.

---

## 3. Contrato de `dibujar_barco()`

```r
dibujar_barco(xmn, xmx, ymn, ymx, gmax, filename)   # líneas 198-303
```

| Aspecto | Contrato |
|---|---|
| **Entradas** | Extremos enteros del barco y tamaño de grilla |
| **Salida** | Archivo PNG en `filename` (7×7 in, 150 dpi, fondo blanco) — línea 302 |
| **Efecto lateral** | Escribe en el `cwd` **del render**, que es el directorio temporal de `xexams()`, no el subproyecto |
| **Determinismo** | Función pura respecto de sus argumentos: no llama a `sample()`, `runif()` ni `set.seed()` |

### Geometría del casco (líneas 208-214) — el invariante que sostiene la clave

```r
prof <- function(t) {
  ifelse(t < 0.15, (h/2) * (t/0.15)^0.7,      # proa: se abre
  ifelse(t < 0.85, h/2,                        # centro: ancho completo
                   (h/2) * ((1-t)/0.15)^0.5))  # popa: se cierra
}
```

En el tramo central (`t ∈ [0.15, 0.85]`) el perfil vale exactamente `h/2`, de modo que el borde
superior alcanza `cy + h/2 = y_max` y el inferior `cy - h/2 = y_min`. En los extremos (`t = 0` y
`t = 1`) el perfil vale 0 y el casco se reduce a un punto a media altura, en `x = x_min` y
`x = x_max` respectivamente.

**Consecuencia**: el bounding box del casco es exactamente `[x_min, x_max] × [y_min, y_max]` — que
es, literalmente, la respuesta correcta. Verificado por enumeración exhaustiva sobre las 374
combinaciones (222 antes de P2.7, re-confirmado sobre el espacio ampliado): **0 casos de
desajuste**.

Esta es la razón por la que el ítem es matemáticamente correcto y no solo "correcto en las semillas
que se probaron". Si alguien modifica `prof()` de forma que el casco deje de alcanzar `h/2` en el
tramo central, **la clave del ejercicio pasa a ser falsa** sin que ningún validador sintáctico lo
detecte.

### Radio de las bandas oscuras, acotado por el ancho (línea 249)

```r
rb <- min(h, w * 0.25)
```

Las dos bandas decorativas (líneas 251-262) tienen radio proporcional a `h`, pero sus centros están
separados por una fracción de `w` (0.16w − 0.03w = 0.13w). Sin este acotado, con `h` máximo y `w`
mínimo los radios crecían mientras la separación se encogía, y las dos bandas se fundían en una sola
mancha: 72.3% de solape en `(w=3, h=2)` y 65.2% en `(w=4, h=2)`. Acotar el radio por `0.25*w` deja el
solape máximo en 37% — el mismo nivel que `(w=4, h=1)`, que ya se veía bien — y no altera los casos
alargados (`w=5,6` con `h=1`, que quedan exactamente igual que antes). Las bandas son **decorativas**:
no participan del *bounding box* del casco, así que este cambio no toca la invariante I-2.

### Capas del dibujo

| Capa | Líneas | Elemento |
|---|---|---|
| Grilla | 271-272 | Líneas horizontales/verticales gris claro en 0..gmax |
| Ejes | 273-274 | Segmentos gruesos desde el origen, con flecha implícita |
| Casco | 218, 275-276 | Polígono relleno `gray96`, borde negro `linewidth = 2.0` |
| Línea interior | 220-232, 277-278 | Polígono inset, sin relleno |
| Ojos de buey | 234-237, 283-284 | 6 puntos en 2 columnas de 3 |
| Bandas oscuras | 239-262, 279-280 | Dos crecientes `gray15`, radio `rb` acotado por el ancho |
| Puente | 264-268, 281-282 | Rectángulo `gray15` |
| Rótulos de eje | 289-292 | «x» e «y» en negrita, tamaño 6 |

---

## 4. Decisiones de diseño con su porqué

### 4.1 `exshuffle: TRUE` (línea 472) — y por qué aquí sí

La regla #6 permite `exshuffle: FALSE` + `sample()` interno **solo** cuando hay opciones gráficas
individuales cuya Solution referencia la opción correcta por letra. Aquí no se da ese caso: las
opciones son de texto y la Solution las identifica por **contenido** (línea 347: `"La respuesta
correcta es la que indica las coordenadas **", correcta, "**"`) y por **código de error**
(línea 372: `"**", err$codigo, " — ", err$nombre, ": "`). Cumple la regla #19 sin necesidad de
desactivar el barajado.

El `sample(4)` de la línea 114 no contradice a `exshuffle: TRUE`: R/exams reordena a la vez
`questionlist`, `solutionlist` y `exsolution` con la misma permutación (`read_exercise.R`), de modo
que la doble mezcla sigue siendo coherente.

### 4.2 `y_pool` sin exclusiones — resuelto P2.7 (2026-07-28)

```r
y_pool <- 1L:(grid_max - alto_barco)   # línea 49, SIN exclusiones
```

Hasta el 2026-07-28 este bloque aplicaba cuatro exclusiones (`y_min ≠ x_min`, `y_max ≠ x_max`,
`y_min ≠ x_max`, `y_max ≠ x_min`), heredadas del distractor retirado `GEO-COORD-03` (4 puntos sobre
la diagonal `y = x`): si cualquiera de esos valores coincidía, ese distractor mostraba un punto
repetido y era descartable de un vistazo.

**Las cuatro se retiraron** tras medir por enumeración exhaustiva (`docs/BACKLOG.md` P2.7):

| Configuración | Versiones | Colisiones |
|---|---|---|
| Las 4 exclusiones (antes) | 222 | 0 |
| Solo las 2 primeras | 286 | 0 |
| **Ninguna (aplicado)** | **374** | **0** |

**Hallazgo:** la exclusión `y_min ≠ x_min` se justificaba como «evita que `GEO-COORD-01`
(inversión) colapse sobre la correcta». Esa justificación era **falsa**: la inversión sólo podría
igualar a la correcta si a la vez `y_min == x_min` **y** `y_max == x_min`, es decir
`alto_barco == 0`, imposible con `alto_barco >= 1`. Ninguna de las cuatro exclusiones era
necesaria; el `stopifnot(length(unique(all_opts)) == 4L)` (línea 103) es la red de seguridad real,
no el filtrado del pool.

**Espacio de versiones: 222 → 374 (+68 %)**, sin ninguna colisión. Re-verificado exhaustivamente:
sobre las 374 combinaciones, **0 colisiones** y **0 casos de `y_pool` vacío**. El `stopifnot` de la
línea 50 y el de la línea 103 siguen sin poder dispararse dentro del espacio de parámetros
declarado — ya no por el filtrado del pool, sino porque la geometría del rectángulo lo garantiza
por construcción. No reintroducir exclusiones sin volver a medir (ver §5, invariante I-3).

### 4.3 Construcción determinista, sin bucles de reintento (regla #21, Familia 1)

El pool `y_pool` se construye **por filtrado directo**, no por `repeat { ... if (ok) break }`. No
hay ningún bucle de reintento en el chunk, de modo que el Error 22 (cuelgue por condición
inalcanzable) no puede producirse aquí. Los helpers `pick_int()` (línea 13) y `safe_sample()`
(líneas 14-18) son las versiones canónicas de la Familia 1/5 de la regla #21, copiadas dentro del
chunk por la restricción de auto-contención (§5, I-1). Con `y_pool` sin exclusiones (§4.2), el
filtrado directo sigue vigente — sólo cambió el pool de partida, no el mecanismo de construcción.

### 4.4 Distractores construidos por `paste0`, sin campo `calcula()`

La regla #1 (`ejercicios-metacognitivos.md`) describe un pool de errores con `precondicion` y
`calcula()`. Aquí los distractores no son valores numéricos derivados de una función, sino
**cadenas de coordenadas** construidas directamente (líneas 55-100), y `errores_info`
(líneas 120-159) guarda el diagnóstico pedagógico de cada uno. La validación semántica de Capas A-D
pasa igualmente (`validar_coherencia_matematica.R` → APROBADO), porque no hay funciones `calcula()`
sobre las que verificar determinismo. Ver la discusión en [`BACKLOG.md`](BACKLOG.md).

### 4.5 Solution con las 6 subsecciones canónicas — resuelto P2.5 (2026-07-28)

La regla #1 (`ejercicios-metacognitivos.md`, «Sección Solution Obligatoria») exige seis
subsecciones. Hasta el 2026-07-28 el chunk `solucion` cubría cuatro: *Respuesta correcta* +
*Análisis de cada opción* (cubre «Análisis del error»), *Procedimiento correcto*, *Reflexión
metacognitiva* y *Estrategia para evitar el error*. Se añadieron las dos que faltaban, entre
*Procedimiento correcto* y *Reflexión metacognitiva*:

| Subsección | Líneas | Contenido |
|---|---|---|
| **Propiedades del concepto** | 397-415 | 4 propiedades: el orden del par ordenado no es intercambiable; un rectángulo alineado a los ejes queda determinado por 2 valores de x y 2 de y; el rectángulo que encierra un objeto usa el mínimo y el máximo de cada eje; los 4 vértices nunca están alineados |
| **Caso específico** | 416-437 | Transferencia: si el barco se desplaza 1 unidad en vertical (dirección adaptativa, `sube <- if (y_max < grid_max) 1L else -1L`, línea 417), el rango en x no cambia y solo se desplazan las segundas coordenadas de los 4 vértices |

Verificado en el XML de Moodle renderizado: las 7 cabeceras `### ` de la Solution están presentes
(*Respuesta correcta*, *Análisis de cada opción*, *Procedimiento correcto*, *Propiedades del
concepto*, *Caso específico*, *Reflexión metacognitiva*, *Estrategia para evitar el error*). Ver
[`BACKLOG.md`](BACKLOG.md) P2.5.

### 4.6 El tercer distractor conserva la estructura 2×2 (líneas 80-100)

Hasta el 2026-07-28 el tercer distractor era `GEO-COORD-03`: 4 puntos sobre la diagonal `y = x`,
con la forma `(v, v)` — cada coordenada tenía el mismo valor en x que en y. Los otros dos
distractores (`GEO-COORD-01`, `GEO-COORD-02`) y la respuesta correcta comparten en cambio la
estructura «2 valores de x combinados con 2 valores de y» (un rectángulo). Un estudiante que
reconociera ese patrón podía descartar `GEO-COORD-03` por la FORMA del texto (4 pares idénticos),
sin necesidad de mirar la figura, elevando su probabilidad de acierto por azar de 25% a 33% entre
las 3 opciones restantes. Enumerado exhaustivamente en el espacio vigente entonces: el patrón se
cumplía en 222/222 versiones.

`GEO-COORD-04` («desplazamiento de una unidad al contar la cuadrícula») lo sustituye conservando la
estructura 2×2: es el mismo rectángulo desplazado `desplaz` unidades en el eje x
(`desplaz <- if (x_max < grid_max) 1L else -1L`, línea 93 — la dirección es adaptativa para que el
rectángulo desplazado no se salga de la grilla). Verificado, y re-confirmado tras P2.7 sobre el
espacio ampliado a 374: las 4 opciones cumplen la estructura 2×2 en 374/374 versiones, y el
`stopifnot` de la línea 105 confirma que el desplazamiento nunca saca al distractor de los límites
`[1, grid_max]`.

Ver la discusión completa en [`BACKLOG.md`](BACKLOG.md) P0.1 y P2.7.

---

## 5. Invariantes que no se deben romper

| # | Invariante | Por qué | Cómo verificarlo |
|---|---|---|---|
| **I-1** | El `.Rmd` permanece **auto-contenido**: `dibujar_barco()`, `pick_int()` y `safe_sample()` viven dentro de `data_generation` | `validar_diversidad_sustantiva.R` (regla #22, obligatorio) hace `setwd(tempdir())` y evalúa el chunk en un `new.env()` fuera del pipeline de `xexams()`; ahí `include_supplement()` no tiene estado interno y falla. El hermano `desplazamiento-avion-aeropuerto` lo intentó y falló 40/40 semillas | `Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40` |
| **I-2** | `prof()` vale exactamente `h/2` en `t ∈ [0.15, 0.85]` | Es lo que hace que el bounding box del casco sea la clave. Romperlo invalida la respuesta correcta sin error de sintaxis | Enumeración exhaustiva (§1) o inspección visual del PNG |
| **I-3** | `y_pool` NO lleva exclusiones; el `stopifnot` de unicidad es la red de seguridad | Se midió (P2.7, 2026-07-28) que ninguna de las 4 exclusiones anteriores era necesaria — incluida la que se creía que evitaba que `GEO-COORD-01` colapsara sobre la correcta, cuya justificación era falsa. `stopifnot(length(unique(all_opts)) == 4L)` (línea 103) es lo que realmente garantiza 4 opciones distintas. **No reintroducir exclusiones de `y_pool` sin volver a medir** | `stopifnot` líneas 50 y 103 + enumeración exhaustiva (374 combinaciones, 0 colisiones) |
| **I-4** | La imagen se emite con `{width=80%}` (línea 325) | Regla #18: sin atributo, pandoc genera `\pandocbounded` y `exams2pdf()` falla | `grep -nE '!\[[^]]*\]\([^)]+\.png\)' <rmd>` |
| **I-5** | El guard `\newcounter{none}` (línea 315) se conserva | Regla #20: pandoc ≥3.8.1 puede envolver tablas en `\def\LTcaptype{none}`. Es barato y protege ante la futura adición de una tabla | `grep -n 'c@none' <rmd>` |
| **I-6** | La Solution nunca nombra la letra de una opción | Regla #19: Moodle puede rebarajar y la prosa de Solution no se reordena (`read_exercise.R`) | `sed -n '/^Solution/,/^Meta/p' <rmd> \| grep -E 'letra_correcta\|Opción [A-D]'` |
| **I-7** | No hay `set.seed()` dentro de ningún chunk | Corrompería el RNG del render y colapsaría la diversidad | `grep -n 'set.seed' <rmd>` |
| **I-8** | `dibujar_barco()` no llama a funciones aleatorias | Debe ser pura respecto de sus argumentos para que el dibujo coincida con la clave | `grep -nE 'sample\(\|runif\(\|rnorm\(' ` dentro de las líneas 190-295 |

---

## 6. Verificación de las citas de línea (2026-07-28)

Todas las referencias de línea de este documento se comprobaron contra el archivo real con
`grep -n`. Anclas de control:

| Línea | Contenido real |
|---|---|
| 13 | `pick_int <- function(a, b) if (a >= b) a else sample(a:b, 1L)` |
| 49 | `y_pool <- 1L:(grid_max - alto_barco)` |
| 50 | `stopifnot(length(y_pool) > 0L)` |
| 93 | `desplaz <- if (x_max < grid_max) 1L else -1L` |
| 103 | `stopifnot(length(unique(all_opts)) == 4L)` |
| 198 | `dibujar_barco <- function(xmn, xmx, ymn, ymx, gmax, filename) {` |
| 249 | `rb <- min(h, w * 0.25)` |
| 305 | `dibujar_barco(x_min, x_max, y_min, y_max, grid_max, "plano_barco.png")` |
| 399 | `"### Propiedades del concepto\n\n",` |
| 420 | `"### Caso específico\n\n",` |
| 472 | `exshuffle: TRUE` |

Si alguien edita el `.Rmd`, estas citas se desplazan. Re-verificarlas es parte del mantenimiento de
este documento.

---

## 7. Referencias cruzadas

- [`../README.md`](../README.md) — entrada del subproyecto
- [`../HANDOFF.md`](../HANDOFF.md) — estado de trabajo y cómo retomar
- [`SYLLABUS.md`](SYLLABUS.md) — encuadre pedagógico y pool de errores
- [`BACKLOG.md`](BACKLOG.md) — pendientes priorizados
- [`ROADMAP.md`](ROADMAP.md) — ruta a producción
- [`../.claude/CLAUDE.md`](../.claude/CLAUDE.md) — particularidades operativas para agentes
- Reglas del repo raíz: `#18` `markdown-imagenes-pdf.md` · `#19` `solution-letter-independence.md` ·
  `#20` `markdown-tablas-pandoc.md` · `#21` `familias-soluciones-rmd.md` ·
  `#22` `diversidad-sustantiva.md`
- `RR/.claude/docs/AUTOCONTENCION_REXAMS.md` — mecanismo de copia a tempdir de R/exams

---

**Versión:** 1.2 · **Fecha:** 2026-07-28 (v1.2 — P2.5: Solution con 6 subsecciones canónicas
[§2, §4.5]; P2.7: retiradas las 4 exclusiones de `y_pool`, espacio de versiones 222 → 374 [§1,
§4.2, invariante I-3]; todas las citas de línea re-verificadas contra el `.Rmd` de 483 líneas)
