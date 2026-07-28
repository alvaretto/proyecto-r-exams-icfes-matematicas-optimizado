# Blueprint — Coordenadas de vértices en el plano cartesiano (barco)

> Arquitectura técnica del ejercicio. Para el encuadre pedagógico ver
> [`SYLLABUS.md`](SYLLABUS.md); para el estado de trabajo ver
> [`../HANDOFF.md`](../HANDOFF.md).

| Campo | Valor |
|---|---|
| **Archivo fuente** | `coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd` |
| **Líneas** | 401 (verificado con `wc -l`, 2026-07-28) |
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
    ├─► y_pool: 1:(10-alto) menos 4 exclusiones   líneas 35-40
    │      └─► y_min ← safe_sample(y_pool)        línea 41
    │             └─► y_max = y_min + alto        línea 42
    │
    ├─► correcta          = bounding box [x_min,x_max] × [y_min,y_max]   líneas 45-50
    ├─► GEO-COORD-01 inv  = (x,y) escrito como (y,x)                     líneas 54-59
    ├─► GEO-COORD-02 cen  = franja central de 1 unidad                   líneas 62-68
    ├─► GEO-COORD-03 dia  = 4 puntos sobre la recta y = x                líneas 71-76
    │      └─► stopifnot(4 opciones distintas)                           líneas 79-80
    │
    ├─► perm ← sample(4); sol ← posición de "correcta"                   líneas 89-92
    ├─► protagonista ← 1 de 8 nombres                                    líneas 135-139
    ├─► reflexion    ← 1 de 4 textos                                     líneas 142-168
    │
    └─► dibujar_barco(x_min, x_max, y_min, y_max, 10, "plano_barco.png") línea 262
              └─► PNG 7×7 in @150 dpi, fondo blanco                      línea 259
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
| 1 | `data_generation` | 1-266 | Todo el cálculo: helpers, parámetros, opciones, pool de errores, protagonista, reflexión, y la función `dibujar_barco()` con su llamada |
| — | guard LaTeX | 271-273 | `\@ifundefined{c@none}{\newcounter{none}}{}` — regla #20 |
| 2 | `question_body` | 275-287 | Enunciado + imagen (`{width=80%}`) + pregunta |
| 3 | `answerlist_q` | 291-295 | Emite las 4 opciones del enunciado |
| 4 | `solucion` | 300-369 | Respuesta correcta, análisis por opción, procedimiento, reflexión, estrategia |
| 5 | `answerlist_s` | 373-383 | Feedback por opción (Correcto/Incorrecto + código de error) |

La consolidación de 8 chunks a 5 se hizo en una sesión previa (commit pendiente al inicio de
2026-07-28) y se conserva: menos chunks significa menos puntos donde el estado del entorno puede
divergir entre formatos.

---

## 3. Contrato de `dibujar_barco()`

```r
dibujar_barco(xmn, xmx, ymn, ymx, gmax, filename)   # líneas 171-260
```

| Aspecto | Contrato |
|---|---|
| **Entradas** | Extremos enteros del barco y tamaño de grilla |
| **Salida** | Archivo PNG en `filename` (7×7 in, 150 dpi, fondo blanco) — línea 259 |
| **Efecto lateral** | Escribe en el `cwd` **del render**, que es el directorio temporal de `xexams()`, no el subproyecto |
| **Determinismo** | Función pura respecto de sus argumentos: no llama a `sample()`, `runif()` ni `set.seed()` |

### Geometría del casco (líneas 177-187) — el invariante que sostiene la clave

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

### Capas del dibujo

| Capa | Líneas | Elemento |
|---|---|---|
| Grilla | 228-229 | Líneas horizontales/verticales gris claro en 0..gmax |
| Ejes | 230-231 | Segmentos gruesos desde el origen, con flecha implícita |
| Casco | 187, 232-233 | Polígono relleno `gray96`, borde negro `linewidth = 2.0` |
| Línea interior | 189-201, 234-235 | Polígono inset, sin relleno |
| Ojos de buey | 203-206, 240-241 | 6 puntos en 2 columnas de 3 |
| Bandas oscuras | 208-219, 236-237 | Dos crecientes `gray15` |
| Puente | 221-225, 238-239 | Rectángulo `gray15` |
| Rótulos de eje | 246-249 | «x» e «y» en negrita, tamaño 6 |

---

## 4. Decisiones de diseño con su porqué

### 4.1 `exshuffle: TRUE` (línea 390) — y por qué aquí sí

La regla #6 permite `exshuffle: FALSE` + `sample()` interno **solo** cuando hay opciones gráficas
individuales cuya Solution referencia la opción correcta por letra. Aquí no se da ese caso: las
opciones son de texto y la Solution las identifica por **contenido** (línea 305: `"La respuesta
correcta es la que indica las coordenadas **", correcta, "**"`) y por **código de error**
(líneas 330-332). Cumple la regla #19 sin necesidad de desactivar el barajado.

El `sample(4)` de la línea 89 no contradice a `exshuffle: TRUE`: R/exams reordena a la vez
`questionlist`, `solutionlist` y `exsolution` con la misma permutación (`read_exercise.R`), de modo
que la doble mezcla sigue siendo coherente.

### 4.2 Las 4 exclusiones de `y_pool` (líneas 35-40)

```r
y_pool <- setdiff(y_pool, x_min)                  # y_min ≠ x_min
y_pool <- y_pool[y_pool + alto_barco != x_max]    # y_max ≠ x_max
y_pool <- setdiff(y_pool, x_max)                  # y_min ≠ x_max
y_pool <- y_pool[y_pool + alto_barco != x_min]    # y_max ≠ x_min
```

Existen para que **`GEO-COORD-03` (diagonal) tenga siempre 4 puntos distintos**. Ese distractor se
construye como `(x_min,x_min), (y_min,y_min), (y_max,y_max), (x_max,x_max)`: si cualquiera de los
cuatro valores coincidiera, el distractor mostraría un punto repetido y sería descartable de un
vistazo. De paso, la primera exclusión impide que `GEO-COORD-01` (inversión) colapse sobre la
respuesta correcta.

Verificado exhaustivamente: sobre las 222 combinaciones, **0 colisiones** y **0 casos de `y_pool`
vacío**. Es decir, ni el `stopifnot` de la línea 40 ni el de la línea 80 pueden dispararse dentro
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
**cadenas de coordenadas** construidas directamente (líneas 54-76), y `errores_info`
(líneas 95-132) guarda el diagnóstico pedagógico de cada uno. La validación semántica de Capas A-D
pasa igualmente (`validar_coherencia_matematica.R` → APROBADO), porque no hay funciones `calcula()`
sobre las que verificar determinismo. Ver la discusión en [`BACKLOG.md`](BACKLOG.md).

---

## 5. Invariantes que no se deben romper

| # | Invariante | Por qué | Cómo verificarlo |
|---|---|---|---|
| **I-1** | El `.Rmd` permanece **auto-contenido**: `dibujar_barco()`, `pick_int()` y `safe_sample()` viven dentro de `data_generation` | `validar_diversidad_sustantiva.R` (regla #22, obligatorio) hace `setwd(tempdir())` y evalúa el chunk en un `new.env()` fuera del pipeline de `xexams()`; ahí `include_supplement()` no tiene estado interno y falla. El hermano `desplazamiento-avion-aeropuerto` lo intentó y falló 40/40 semillas | `Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40` |
| **I-2** | `prof()` vale exactamente `h/2` en `t ∈ [0.15, 0.85]` | Es lo que hace que el bounding box del casco sea la clave. Romperlo invalida la respuesta correcta sin error de sintaxis | Enumeración exhaustiva (§1) o inspección visual del PNG |
| **I-3** | Las 4 exclusiones de `y_pool` se mantienen | Garantizan 4 opciones distintas y que `GEO-COORD-03` no tenga puntos repetidos | `stopifnot` líneas 40 y 80 + enumeración exhaustiva |
| **I-4** | La imagen se emite con `{width=80%}` (línea 282) | Regla #18: sin atributo, pandoc genera `\pandocbounded` y `exams2pdf()` falla | `grep -nE '!\[[^]]*\]\([^)]+\.png\)' <rmd>` |
| **I-5** | El guard `\newcounter{none}` (líneas 271-273) se conserva | Regla #20: pandoc ≥3.8.1 puede envolver tablas en `\def\LTcaptype{none}`. Es barato y protege ante la futura adición de una tabla | `grep -n 'c@none' <rmd>` |
| **I-6** | La Solution nunca nombra la letra de una opción | Regla #19: Moodle puede rebarajar y la prosa de Solution no se reordena (`read_exercise.R`) | `sed -n '/^Solution/,/^Meta/p' <rmd> \| grep -E 'letra_correcta\|Opción [A-D]'` |
| **I-7** | No hay `set.seed()` dentro de ningún chunk | Corrompería el RNG del render y colapsaría la diversidad | `grep -n 'set.seed' <rmd>` |
| **I-8** | `dibujar_barco()` no llama a funciones aleatorias | Debe ser pura respecto de sus argumentos para que el dibujo coincida con la clave | `grep -nE 'sample\(\|runif\(\|rnorm\(' ` dentro de las líneas 171-260 |

---

## 6. Verificación de las citas de línea (2026-07-28)

Todas las referencias de línea de este documento se comprobaron contra el archivo real con
`grep -n`. Anclas de control:

| Línea | Contenido real |
|---|---|
| 13 | `pick_int <- function(a, b) if (a >= b) a else sample(a:b, 1L)` |
| 40 | `stopifnot(length(y_pool) > 0L)` |
| 80 | `stopifnot(length(unique(all_opts)) == 4L)` |
| 171 | `dibujar_barco <- function(xmn, xmx, ymn, ymx, gmax, filename) {` |
| 262 | `dibujar_barco(x_min, x_max, y_min, y_max, grid_max, "plano_barco.png")` |
| 282 | `cat("![](plano_barco.png){width=80%}\n\n")` |
| 390 | `exshuffle: TRUE` |

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

**Versión:** 1.0 · **Fecha:** 2026-07-28
