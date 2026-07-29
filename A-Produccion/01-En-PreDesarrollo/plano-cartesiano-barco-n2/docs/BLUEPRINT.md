# Blueprint — Coordenadas de vértices en el plano cartesiano (barco)

> Arquitectura técnica del ejercicio. Para el encuadre pedagógico ver
> [`SYLLABUS.md`](SYLLABUS.md); para el estado de trabajo ver
> [`../HANDOFF.md`](../HANDOFF.md).

| Campo | Valor |
|---|---|
| **Archivo fuente** | `coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd` |
| **Líneas** | 436 (verificado con `wc -l`, 2026-07-28) |
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
    ├─► y_pool: 1:(10-alto) menos 4 exclusiones   líneas 37-41
    │      └─► y_min ← safe_sample(y_pool)        línea 43
    │             └─► y_max = y_min + alto        línea 44
    │
    ├─► correcta           = bounding box [x_min,x_max] × [y_min,y_max]   líneas 47-52
    ├─► GEO-COORD-01 inv   = (x,y) escrito como (y,x)                     líneas 55-61
    ├─► GEO-COORD-02 cen   = franja central de 1 unidad                   líneas 63-70
    ├─► GEO-COORD-04 desp  = rango horizontal desplazado 1 unidad         líneas 72-91
    │      └─► stopifnot(4 opciones distintas + dentro de la grilla)      líneas 94-97
    │
    ├─► perm ← sample(4); sol ← posición de "correcta"                   líneas 100-109
    ├─► protagonista ← 1 de 8 nombres                                    líneas 154-158
    ├─► reflexion    ← 1 de 4 textos                                     líneas 161-187
    │
    └─► dibujar_barco(x_min, x_max, y_min, y_max, 10, "plano_barco.png") línea 297
              └─► PNG 7×7 in @150 dpi, fondo blanco                      línea 294
```

**Espacio de versiones** (enumeración exhaustiva, no muestreo — 2026-07-28):

| Dimensión | Cardinalidad |
|---|---|
| Combinaciones geométricas `(ancho, alto, x_min, y_min)` válidas | **222** |
| Respuestas correctas **distintas** | **222** (biyección: no hay dos combinaciones con la misma clave) |
| × protagonistas | 8 |
| × reflexiones metacognitivas | 4 |
| × órdenes de opción (`exshuffle: TRUE`) | 24 |
| **Renders distintos posibles** | 222 × 8 × 4 × 24 = **170 496** |

La cifra que importa para la regla #22 es la primera: **222 preguntas sustantivamente
distintas**. Las otras tres dimensiones son envoltorio (protagonista, reflexión, orden), y por sí
solas no constituirían diversidad.

---

## 2. Anatomía del `.Rmd` (5 chunks)

| # | Chunk | Líneas | Qué hace |
|---|---|---|---|
| 1 | `data_generation` | 1-301 | Todo el cálculo: helpers, parámetros, opciones, pool de errores, protagonista, reflexión, y la función `dibujar_barco()` con su llamada |
| — | guard LaTeX | 306-308 | `\@ifundefined{c@none}{\newcounter{none}}{}` — regla #20 |
| 2 | `question_body` | 310-322 | Enunciado + imagen (`{width=80%}`) + pregunta |
| 3 | `answerlist_q` | 326-330 | Emite las 4 opciones del enunciado |
| 4 | `solucion` | 335-404 | Respuesta correcta, análisis por opción, procedimiento, reflexión, estrategia |
| 5 | `answerlist_s` | 408-418 | Feedback por opción (Correcto/Incorrecto + código de error) |

La consolidación de 8 chunks a 5 se hizo en una sesión previa (commit pendiente al inicio de
2026-07-28) y se conserva: menos chunks significa menos puntos donde el estado del entorno puede
divergir entre formatos.

---

## 3. Contrato de `dibujar_barco()`

```r
dibujar_barco(xmn, xmx, ymn, ymx, gmax, filename)   # líneas 190-295
```

| Aspecto | Contrato |
|---|---|
| **Entradas** | Extremos enteros del barco y tamaño de grilla |
| **Salida** | Archivo PNG en `filename` (7×7 in, 150 dpi, fondo blanco) — línea 294 |
| **Efecto lateral** | Escribe en el `cwd` **del render**, que es el directorio temporal de `xexams()`, no el subproyecto |
| **Determinismo** | Función pura respecto de sus argumentos: no llama a `sample()`, `runif()` ni `set.seed()` |

### Geometría del casco (líneas 200-206) — el invariante que sostiene la clave

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
es, literalmente, la respuesta correcta. Verificado por enumeración exhaustiva sobre las 222
combinaciones: **0 casos de desajuste**.

Esta es la razón por la que el ítem es matemáticamente correcto y no solo "correcto en las semillas
que se probaron". Si alguien modifica `prof()` de forma que el casco deje de alcanzar `h/2` en el
tramo central, **la clave del ejercicio pasa a ser falsa** sin que ningún validador sintáctico lo
detecte.

### Radio de las bandas oscuras, acotado por el ancho (línea 241)

```r
rb <- min(h, w * 0.25)
```

Las dos bandas decorativas (líneas 243-254) tienen radio proporcional a `h`, pero sus centros están
separados por una fracción de `w` (0.16w − 0.03w = 0.13w). Sin este acotado, con `h` máximo y `w`
mínimo los radios crecían mientras la separación se encogía, y las dos bandas se fundían en una sola
mancha: 72.3% de solape en `(w=3, h=2)` y 65.2% en `(w=4, h=2)`. Acotar el radio por `0.25*w` deja el
solape máximo en 37% — el mismo nivel que `(w=4, h=1)`, que ya se veía bien — y no altera los casos
alargados (`w=5,6` con `h=1`, que quedan exactamente igual que antes). Las bandas son **decorativas**:
no participan del *bounding box* del casco, así que este cambio no toca la invariante I-2.

### Capas del dibujo

| Capa | Líneas | Elemento |
|---|---|---|
| Grilla | 263-264 | Líneas horizontales/verticales gris claro en 0..gmax |
| Ejes | 265-266 | Segmentos gruesos desde el origen, con flecha implícita |
| Casco | 210, 267-268 | Polígono relleno `gray96`, borde negro `linewidth = 2.0` |
| Línea interior | 212-224, 269-270 | Polígono inset, sin relleno |
| Ojos de buey | 226-229, 275-276 | 6 puntos en 2 columnas de 3 |
| Bandas oscuras | 231-254, 271-272 | Dos crecientes `gray15`, radio `rb` acotado por el ancho |
| Puente | 256-260, 273-274 | Rectángulo `gray15` |
| Rótulos de eje | 281-284 | «x» e «y» en negrita, tamaño 6 |

---

## 4. Decisiones de diseño con su porqué

### 4.1 `exshuffle: TRUE` (línea 425) — y por qué aquí sí

La regla #6 permite `exshuffle: FALSE` + `sample()` interno **solo** cuando hay opciones gráficas
individuales cuya Solution referencia la opción correcta por letra. Aquí no se da ese caso: las
opciones son de texto y la Solution las identifica por **contenido** (línea 339: `"La respuesta
correcta es la que indica las coordenadas **", correcta, "**"`) y por **código de error**
(líneas 364-368). Cumple la regla #19 sin necesidad de desactivar el barajado.

El `sample(4)` de la línea 106 no contradice a `exshuffle: TRUE`: R/exams reordena a la vez
`questionlist`, `solutionlist` y `exsolution` con la misma permutación (`read_exercise.R`), de modo
que la doble mezcla sigue siendo coherente.

### 4.2 Las 4 exclusiones de `y_pool` (líneas 30-41)

```r
y_pool <- setdiff(y_pool, x_min)                  # y_min ≠ x_min
y_pool <- y_pool[y_pool + alto_barco != x_max]    # y_max ≠ x_max
y_pool <- setdiff(y_pool, x_max)                  # y_min ≠ x_max
y_pool <- y_pool[y_pool + alto_barco != x_min]    # y_max ≠ x_min
```

Las dos primeras (`y_min ≠ x_min`, `y_max ≠ x_max`) siguen haciendo falta para que
**`GEO-COORD-01` (inversión) no colapse sobre la correcta**: sin ellas, un barco con coordenadas
simétricas produciría un distractor idéntico a la respuesta.

Las dos últimas (`y_min ≠ x_max`, `y_max ≠ x_min`) quedaron **heredadas del distractor retirado**
`GEO-COORD-03` (4 puntos sobre la diagonal `y = x`, construido como
`(x_min,x_min), (y_min,y_min), (y_max,y_max), (x_max,x_max)`): si cualquiera de esos cuatro valores
coincidía, ese distractor mostraba un punto repetido y era descartable de un vistazo. Tras la
sustitución por `GEO-COORD-04` (§4.5) ya no protegen a ningún distractor específico, pero **se
conservan** porque siguen garantizando 4 opciones distintas en las 222 combinaciones (`stopifnot`
línea 95). Podrían relajarse, pero eso ampliaría el espacio de versiones y exige re-validar la
enumeración completa — ver [`BACKLOG.md`](BACKLOG.md) P2.7.

Verificado exhaustivamente: sobre las 222 combinaciones, **0 colisiones** y **0 casos de `y_pool`
vacío**. Es decir, ni el `stopifnot` de la línea 42 ni el de la línea 95 pueden dispararse dentro
del espacio de parámetros declarado. No son código muerto: son la red que documenta la invariante.

### 4.3 Construcción determinista, sin bucles de reintento (regla #21, Familia 1)

El pool `y_pool` se construye **por filtrado directo**, no por `repeat { ... if (ok) break }`. No
hay ningún bucle de reintento en el chunk, de modo que el Error 22 (cuelgue por condición
inalcanzable) no puede producirse aquí. Los helpers `pick_int()` (línea 13) y `safe_sample()`
(líneas 14-18) son las versiones canónicas de la Familia 1/5 de la regla #21, copiadas dentro del
chunk por la restricción de auto-contención (§5, I-1).

### 4.4 Distractores construidos por `paste0`, sin campo `calcula()`

La regla #1 (`ejercicios-metacognitivos.md`) describe un pool de errores con `precondicion` y
`calcula()`. Aquí los distractores no son valores numéricos derivados de una función, sino
**cadenas de coordenadas** construidas directamente (líneas 55-91), y `errores_info`
(líneas 112-151) guarda el diagnóstico pedagógico de cada uno. La validación semántica de Capas A-D
pasa igualmente (`validar_coherencia_matematica.R` → APROBADO), porque no hay funciones `calcula()`
sobre las que verificar determinismo. Ver la discusión en [`BACKLOG.md`](BACKLOG.md).

### 4.5 El tercer distractor conserva la estructura 2×2 (líneas 72-91)

Hasta el 2026-07-28 el tercer distractor era `GEO-COORD-03`: 4 puntos sobre la diagonal `y = x`,
con la forma `(v, v)` — cada coordenada tenía el mismo valor en x que en y. Los otros dos
distractores (`GEO-COORD-01`, `GEO-COORD-02`) y la respuesta correcta comparten en cambio la
estructura «2 valores de x combinados con 2 valores de y» (un rectángulo). Un estudiante que
reconociera ese patrón podía descartar `GEO-COORD-03` por la FORMA del texto (4 pares idénticos),
sin necesidad de mirar la figura, elevando su probabilidad de acierto por azar de 25% a 33% entre
las 3 opciones restantes. Enumerado exhaustivamente: el patrón se cumplía en 222/222 versiones.

`GEO-COORD-04` («desplazamiento de una unidad al contar la cuadrícula») lo sustituye conservando la
estructura 2×2: es el mismo rectángulo desplazado `desplaz` unidades en el eje x
(`desplaz <- if (x_max < grid_max) 1L else -1L`, línea 85 — la dirección es adaptativa para que el
rectángulo desplazado no se salga de la grilla). Verificado: las 4 opciones cumplen la estructura
2×2 en 222/222 versiones, y el `stopifnot` de la línea 97 confirma que el desplazamiento nunca saca
al distractor de los límites `[1, grid_max]`.

Ver la discusión completa en [`BACKLOG.md`](BACKLOG.md) P0.1.

---

## 5. Invariantes que no se deben romper

| # | Invariante | Por qué | Cómo verificarlo |
|---|---|---|---|
| **I-1** | El `.Rmd` permanece **auto-contenido**: `dibujar_barco()`, `pick_int()` y `safe_sample()` viven dentro de `data_generation` | `validar_diversidad_sustantiva.R` (regla #22, obligatorio) hace `setwd(tempdir())` y evalúa el chunk en un `new.env()` fuera del pipeline de `xexams()`; ahí `include_supplement()` no tiene estado interno y falla. El hermano `desplazamiento-avion-aeropuerto` lo intentó y falló 40/40 semillas | `Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40` |
| **I-2** | `prof()` vale exactamente `h/2` en `t ∈ [0.15, 0.85]` | Es lo que hace que el bounding box del casco sea la clave. Romperlo invalida la respuesta correcta sin error de sintaxis | Enumeración exhaustiva (§1) o inspección visual del PNG |
| **I-3** | Las 4 exclusiones de `y_pool` se mantienen | Las 2 primeras evitan que `GEO-COORD-01` colapse sobre la correcta; las 2 últimas, heredadas del retirado `GEO-COORD-03`, siguen garantizando 4 opciones distintas | `stopifnot` líneas 42, 95 y 97 + enumeración exhaustiva |
| **I-4** | La imagen se emite con `{width=80%}` (línea 317) | Regla #18: sin atributo, pandoc genera `\pandocbounded` y `exams2pdf()` falla | `grep -nE '!\[[^]]*\]\([^)]+\.png\)' <rmd>` |
| **I-5** | El guard `\newcounter{none}` (líneas 306-308) se conserva | Regla #20: pandoc ≥3.8.1 puede envolver tablas en `\def\LTcaptype{none}`. Es barato y protege ante la futura adición de una tabla | `grep -n 'c@none' <rmd>` |
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
| 42 | `stopifnot(length(y_pool) > 0L)` |
| 85 | `desplaz <- if (x_max < grid_max) 1L else -1L` |
| 95 | `stopifnot(length(unique(all_opts)) == 4L)` |
| 190 | `dibujar_barco <- function(xmn, xmx, ymn, ymx, gmax, filename) {` |
| 241 | `rb <- min(h, w * 0.25)` |
| 297 | `dibujar_barco(x_min, x_max, y_min, y_max, grid_max, "plano_barco.png")` |
| 425 | `exshuffle: TRUE` |

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

**Versión:** 1.1 · **Fecha:** 2026-07-28
