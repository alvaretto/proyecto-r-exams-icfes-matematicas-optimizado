# Blueprint — Permutaciones de los pescadores en la venia final

> Arquitectura técnica del ejercicio. Para el encuadre pedagógico ver
> [`SYLLABUS.md`](SYLLABUS.md); para el estado de trabajo ver [`../HANDOFF.md`](../HANDOFF.md)
> (pendiente).

| Campo | Valor |
|---|---|
| **Archivo fuente** | `permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd` |
| **Líneas** | 481 (verificado con `wc -l`, 2026-07-29 — creció de 405 tras la auditoría adversarial que amplió el pool de errores) |
| **Chunks R** | 4 (`data_generation`, `question_body`, `answerlist_q`, `solucion`) + 1 guard LaTeX |
| **Lenguaje gráfico** | Ninguno — Flujo B = `false`, el ejercicio no tiene figura |
| **Tipo** | SCHOICE, opciones de **texto** (valores numéricos), sin imagen |
| **Auto-contenido** | **Sí, obligatorio** (ver §5, invariante I-6) |

---

## 1. Pipeline de generación

```
semilla (R RNG)
    │
    ├─► n ← safe_sample(N_POOL, 1L)   N_POOL = {4, 5, 6}
    │      (rango medido por enumeración exhaustiva, §2)
    │
    ├─► correcta_val = n!
    ├─► desarrollo (texto "n × (n-1) × ... × 1" para la Solution)
    │
    ├─► errores_conceptuales (pool de 5, con precondicion/calcula)
    │      ├─► EST-PER-01  calcula(n) = n^(n-1)
    │      ├─► EST-PER-02  calcula(n) = n*n
    │      ├─► EST-PER-03  calcula(n) = n
    │      ├─► EST-PER-04  calcula(n) = (n-1)!
    │      └─► EST-PER-05  calcula(n) = n*(n+1)/2
    │
    ├─► ctx ← 1 de 6 contextos narrativos (canónico + 5 variantes)
    ├─► texto_enunciado, texto_pregunta ← ctx$enunciado/pregunta(n)
    │
    ├─► aplicables ← filtrado por precondicion (los 5 aplican siempre)
    ├─► es_canonica ← (ctx_idx==1 && n==4)
    ├─► sel ← 3 oficiales SI es_canonica, si no sample(aplicables,3)  [Decisión D3]
    ├─► vals ← calcula(n) de los 3 errores seleccionados
    │      ├─► stopifnot I-1..I-4 (unicidad, ≠correcta, ratio≤15x, enteros>0)
    │      └─► stopifnot I-6 (canónica ⇒ opciones = {24,64,16,4})
    │
    ├─► perm ← sample(4L); opciones ← mezcla; sol ← posición correcta
    │      └─► stopifnot I-5 (sol coincide con correcta, 4 únicas)
    │
    ├─► reflexion ← 1 de 6 reflexiones metacognitivas
    └─► errores_info (codigo/nombre/texto/diagnostico/causa_raiz)
```

No hay ninguna función de dibujo: al no tener figura, este ejercicio no necesita el equivalente de
`dibujar_barco()` de `plano-cartesiano-barco-n2`. El "contrato" que sostiene la corrección de la
clave aquí es **algebraico**, no geométrico — ver §3.

---

## 2. Rango de `n` — medido por enumeración exhaustiva (Decisión de diseño D2)

A diferencia de `plano-cartesiano-barco-n2` (donde el espacio de versiones se mide sobre 4
variables geométricas continuas), aquí el "espacio de parámetros" es un único entero `n`, así que
la enumeración exhaustiva es directa: se evaluó cada valor entero de `n` de 2 a 8 contra las
mismas cuatro fórmulas (`n!`, `n^(n-1)`, `n²`, `n`) que produce el `.Rmd`.

| `n` | `n!` | `n^(n-1)` | `n²` | `n` | Opciones únicas | Máx/clave | Veredicto |
|---|---|---|---|---|---|---|---|
| 2 | 2 | 4 | 2 | 2 | 2,0x | Colisión doble (`n! == n^(n-1)`) |
| 6 | 9 | 9 | 3 | 3 | 1,5x | Colisión `n^(n-1) == n²` |
| **4** | **24** | 64 | 16 | 4 | **4** | 2,7x | **OK** |
| **5** | **120** | 625 | 25 | 5 | **4** | 5,2x | **OK** |
| **6** | **720** | 7776 | 36 | 6 | **4** | 10,8x | **OK** |
| 5040 | 117649 | 49 | 7 | 4 | 23,3x | Outlier de magnitud |
| 40320 | 2097152 | 64 | 8 | 4 | 52,0x | Outlier de magnitud |

**Rango adoptado: `n ∈ {4, 5, 6}`**, codificado en `N_POOL <- c(4L, 5L, 6L)`. El umbral
de plausibilidad de magnitud se fijó en **15×** (`stopifnot(max(all_vals) / correcta_val <= 15)`,
regla #22 patrón P5: un distractor cuya magnitud es órdenes superior a la clave se
descarta sin razonar). La ventana medida entre el último `n` aceptado (10,8×, `n=6`) y el primero
rechazado (23,3×, `n=7`) es un factor **2,2** — holgura declarada, el umbral no está al filo.

`n ≤ 3` se descarta por colisión de opciones (rompería `ERR_ANS_C`, invariante I-1); `n ≥ 7` se
descarta porque `n^(n-1)` se vuelve un outlier descartable por su sola magnitud, sin que el
estudiante necesite razonar sobre el conteo (el mismo patrón P5 de la regla #22 documentado en el
subproyecto hermano `desplazamiento-avion-aeropuerto`, aunque allí aplicado a una dimensión
geométrica y aquí a una algebraica).

**Nota (2026-07-29):** la tabla anterior usa solo las cuatro fórmulas originales (`n!`, `n^(n-1)`,
`n²`, `n`), porque así se fijó `N_POOL` cuando el pool de errores tenía 3 entradas. Tras la
auditoría adversarial que amplió el pool a 5 (`EST-PER-04` circular, `EST-PER-05` aditivo), el
contrato de unicidad y plausibilidad debe verificarse para **todas** las combinaciones de 3
distractores posibles, no solo para esta terna fija — ver §3, que reemplaza esta tabla como fuente
de verdad del contrato vigente.

---

## 3. Contrato algebraico del pool de errores (equivalente al contrato geométrico de los hermanos)

`plano-cartesiano-barco-n2` sostiene su clave con un contrato **geométrico** (el *bounding box*
del casco dibujado debe coincidir con la clave). Aquí, al no haber dibujo, el contrato es
**algebraico**: para cada `n ∈ N_POOL` y para **cada una de las C(5,3) = 10 combinaciones** de tres
distractores elegidos del pool de cinco fórmulas (`n^(n-1)`, `n²`, `n`, `(n-1)!`, `n(n+1)/2`), el
conjunto {correcta, distractor, distractor, distractor} debe tener **4 elementos distintos**,
ninguno debe superar **15×** la clave, y los cuatro deben ser enteros positivos. Son **30
combinaciones en total** (3 valores de `n` × 10 ternas) — el espacio completo, no una muestra. Este
contrato se verifica en dos capas independientes:

1. **En tiempo de generación** (`.Rmd`): los `stopifnot` I-1 a I-4 (sobre la terna
   efectivamente seleccionada en esa versión) e I-6 (sobre la instancia canónica) abortan la
   generación si el contrato se rompe.
2. **Fuera del render**, por enumeración exhaustiva de las 30 combinaciones en
   `verificar_render.R` (`V6`): para cada `n ∈ {4,5,6}` recorre las 10 ternas
   posibles de `utils::combn(5, 3)`, calcula las 4 opciones y verifica unicidad, `distractor ≠
   correcta` y `max/clave ≤ 15`, de forma independiente del RNG del render.

**Resultado de la enumeración exhaustiva (2026-07-29, 30/30 ternas verdes):** la razón máx/clave
observada se **amplió** de `[2,7×, 10,8×]` (cuando el pool tenía 3 distractores fijos, siempre
incluidos) a **`[1,0×, 10,8×]`** — el límite superior no cambió (sigue siendo `n=6` con
`EST-PER-01`), pero ahora existen ternas que **excluyen** el distractor más grande (`EST-PER-01`,
`n^(n-1)`), y en esas ternas el valor máximo de las 4 opciones es la propia clave (ratio 1,0×). Una
consecuencia directa: **el rango de la correcta por magnitud ya no es invariante**. Antes, con los
3 distractores oficiales siempre presentes, la clave ocupaba siempre la 3.ª posición al ordenar las
4 opciones (`cardinal < cuadrado < clave < repeticion`). Ahora, en las ternas que excluyen
`EST-PER-01`, la clave puede ser la **mayor** de las 4 (4.ª posición) — por ejemplo, para `n=4` con
la terna {`EST-PER-02`=16, `EST-PER-04`=6, `EST-PER-05`=10}: ordenadas, `6 < 10 < 16 < 24`, la
clave (24) queda en 4.ª posición, no en 3.ª. `verificar_render.R` reporta el conjunto de rangos
observados (`rank_corr`) explícitamente para dejar esta variación auditable en cada corrida.

A diferencia del contrato geométrico del hermano —que puede romperse en silencio si alguien
"suaviza" `prof()` sin que ningún validador sintáctico lo note—, el contrato algebraico aquí es una
propiedad **decidible por cálculo directo** de cinco fórmulas cerradas: no depende de ninguna
función auxiliar que alguien pueda editar sin querer, y la enumeración exhaustiva de las 30
combinaciones (en vez de una muestra) elimina cualquier duda sobre combinaciones no cubiertas.

---

## 4. Decisiones de diseño con su porqué

### 4.1 Rango `n ∈ {4,5,6}` — ver §2 arriba (Decisión D2)

### 4.2 DOK 3 / Bloom "Evaluar" para preservar la coherencia con el Nivel 4 oficial (Decisión D1)

El Nivel 4 proviene de la ficha oficial adoptada del ítem `MAT-2026-1-004` (ver
[`SYLLABUS.md`](SYLLABUS.md) §1) y **no es negociable**: cambiarlo rompería **OE2** (paridad
literal de campos oficiales vs. catálogos canónicos). Pero el cálculo aritmético crudo —multiplicar
`n` factores decrecientes hasta llegar a `n!`— es, tomado de forma aislada, **DOK 2** (aplicación
de una técnica conocida en un contexto concreto, Bloom "Aplicar"), no DOK 3 ni 4.

La tabla "Coherencia Nivel ICFES ↔ DOK" de
`../../../../.claude/rules/ejercicios-metacognitivos.md` no ofrece una combinación perfecta para
este caso: marca DOK 2 como compatible solo con Nivel 1-2 (incompatible con Nivel 4, que es el
oficial), y el propio DOK 3 que se termina asignando aparece en esa tabla como compatible
*estrictamente* con Nivel 3 — Nivel 4 solo figura como compatible con DOK 4 ("Extended Thinking",
Bloom "Crear/Sintetizar").

**Las dos alternativas descartadas y por qué:**

| Alternativa | Por qué se descartó |
|---|---|
| Bajar a Nivel 2 (coherente con DOK 2) | Rompe OE2: el Nivel es un campo oficial adoptado de la ficha, no un valor libre para re-clasificar |
| Subir a DOK 4 / Bloom "Crear" (coherente con Nivel 4) | El ítem no exige crear un método nuevo: exige evaluar por qué tres estrategias dadas fallan y aplicar la correcta. Etiquetarlo "Crear" sería sobre-clasificar la demanda cognitiva real |

**Resolución adoptada (aprobada por el usuario, 2026-07-29): DOK 3 / Bloom "Evaluar"**,
justificada no por la operación aritmética en sí (que sigue siendo DOK 2 si se mira aislada), sino
por la **carga metacognitiva completa del ítem**: la sección Solution exige evaluar tres
estrategias erróneas (`EST-PER-01/02/03`) identificando en cada una si hay o no reemplazo, y
transferir ese criterio a un segundo ítem oficial (`MAT-2026-1-029`, ver
[`SYLLABUS.md`](SYLLABUS.md) §3.1) con la relación inversa (conteo **con** repetición). Es el
compromiso más defendible entre los dos extremos descartados — no una coherencia perfecta con la
tabla de la regla #1, sino una decisión documentada, explícita y con trade-off declarado, en vez
de una discrepancia silenciosa como la que tuvo que corregirse (en sentido opuesto: DOK 3 exigiendo
Nivel ≥ 3) en el subproyecto hermano `desplazamiento-avion-aeropuerto`.

### 4.3 Ausencia total de figura — Flujo B = `false`

`plano_barco.png` (hermano `plano-cartesiano-barco-n2`) tiene una figura compartida por las 4
opciones; `desplazamiento-avion-aeropuerto` tiene 4 figuras, una por opción. Este ejercicio no
tiene ninguna: el enunciado (`.Rmd`) es texto puro y las opciones
son valores numéricos. Es el tercer patrón arquitectónico distinto dentro de la misma familia de
subproyectos (opciones gráficas / figura compartida con opciones de texto / sin figura). No aplica
ninguna de las verificaciones de la regla #18 (`markdown-imagenes-pdf.md`, atributo `{width=...}`)
porque no hay ninguna imagen que renderizar.

### 4.4 `exshuffle: TRUE` — y por qué aquí sí

La regla #6 (`codigo-rmd.md`) permite `exshuffle: FALSE` + `sample()` interno **solo** cuando hay
opciones gráficas individuales cuya Solution referencia la opción correcta por letra. Aquí las
opciones son de texto y la Solution las identifica por **contenido** (`"La respuesta
correcta es la opción que indica **", correcta, "**"`) y por **código de error** (
`"**", e$codigo, " — ", e$nombre, ...`) — cumple la regla #19 (letter-independence) sin necesidad
de desactivar el barajado.

El `sample(4L)` de la mezcla interna no contradice `exshuffle: TRUE`: R/exams reordena a la vez
`questionlist`, `solutionlist` y `exsolution` con la misma permutación (`read_exercise.R`), así
que la doble mezcla sigue siendo coherente — la misma razón documentada en
`plano-cartesiano-barco-n2/docs/BLUEPRINT.md` §4.1.

### 4.5 Sin chunk `answerlist_s` (feedback por opción bajo Solution)

`plano-cartesiano-barco-n2` tiene un quinto chunk (`answerlist_s`) que emite feedback
Correcto/Incorrecto por opción bajo la sección Solution. Este ejercicio no lo tiene: el diagnóstico
por opción ya está íntegro dentro del chunk `solucion`, en la subsección *Análisis de cada opción*
, que recorre `errores_info` y emite código, nombre, valor y diagnóstico de cada
distractor. Por eso el `.Rmd` queda con **4 chunks R**, no 5 — una diferencia estructural, no una
omisión.

### 4.6 `safe_sample()` usado de forma defensiva, no porque el caso límite se dispare hoy

En `plano-cartesiano-barco-n2`, `safe_sample()` (Familia 5, regla #21) es indispensable porque
`alto_pool` genuinamente colapsa a longitud 1 cuando `ancho_barco == 3`. Aquí, los tres usos de
`safe_sample()` —`n <- safe_sample(N_POOL, 1L)` (`N_POOL` longitud 3), `ctx_idx <-
safe_sample(seq_along(contextos), 1L)` (longitud 6) y `reflexion <-
safe_sample(reflexiones, 1L)` (longitud 6)— actúan siempre sobre vectores de longitud
mayor a 1: el caso límite que protege `safe_sample()` (la trampa `sample(escalar)` de la Familia 5)
nunca se dispara con el diseño actual. Se usa de todas formas por consistencia con el patrón
declarado en el comentario de cabecera del chunk ("Familias 1 y 5 de la regla #21") y
porque no tiene costo: si en el futuro `N_POOL` se redujera a un solo valor, el código seguiría
siendo seguro sin cambios.

`ex_uid` usa `safe_sample(c(letters, 0:9), 8L, replace = TRUE)` con un propósito
distinto: generar un identificador aleatorio de 8 caracteres, no seleccionar 1 elemento de un pool
de opciones — el parámetro `size = 8L` con `replace = TRUE` es la forma correcta de generar una
cadena, y tampoco colapsa (el alfabeto de origen tiene 36 símbolos).

### 4.7 Instancia canónica y el ítem espejo `MAT-2026-1-029`

El contexto 1 (`.Rmd`) no es una plantilla narrativa más: es la reproducción
**verbatim** del ítem oficial cuando `n = 4` — enunciado y pregunta idénticos carácter por
carácter a `MAT-2026-1-004`, verificado automáticamente por `verificar_render.R` (`V7`, líneas
167-188). El ítem espejo `MAT-2026-1-029` (mismo descriptor `D4.8`, conteo **con** repetición) se
usa como *Caso específico* de la Solution (§4.2 arriba) — no es un ejemplo decorativo: ancla la
Solution a un segundo ítem oficial real y refuerza que la distinción con/sin reemplazo es el eje
diagnóstico completo del pool de errores (ver [`SYLLABUS.md`](SYLLABUS.md) §3.1). Desde que el pool
creció a 5 errores (§4.8), el puntero de la Solution hacia `EST-PER-01` en el *Caso específico* es
**condicional**: solo se emite si ese error está entre los tres seleccionados en la versión actual
(`.Rmd`).

### 4.8 Decisión D3 — excepción canónica que fuerza los 3 errores oficiales

La regla #1 (`ejercicios-metacognitivos.md`, «Mínimo 4-6 errores por ejercicio») exigía ampliar el
pool más allá de las 3 entradas originales. Pero **OE1** (fidelidad al ítem oficial
`MAT-2026-1-004`) exige que exista al menos una versión que reproduzca el ítem completo, incluidas
sus cuatro opciones oficiales (64, 24, 16, 4) — y esas cuatro opciones dependen exactamente de los
tres errores oficiales (`EST-PER-01/02/03`), no de una terna cualquiera del pool ampliado.

**La resolución (Decisión D3):** la selección normal de errores es `sample(aplicables, 3)` sobre el
pool de 5 (`.Rmd`). Pero cuando la versión es la **instancia canónica** — contexto 1 (el
verbatim de `MAT-2026-1-004`) **y** `n == 4` simultáneamente — se fuerzan los tres errores
oficiales (`CODIGOS_OFICIALES <- c("EST-PER-01", "EST-PER-02", "EST-PER-03")`;
condicional `es_canonica`). Así conviven la regla #1 (pool ampliado, variedad de
tipos de error) y OE1 (al menos una versión reproduce el ítem original íntegro).

**Guardada por una invariante nueva (I-6):** `stopifnot(setequal(all_vals, c(24L, 64L, 16L, 4L)))`
dentro de `if (es_canonica)` (`.Rmd`) — si alguna edición futura rompiera la
excepción canónica (por ejemplo, si `CODIGOS_OFICIALES` cambiara o si el filtrado por
`precondicion` excluyera alguno de los tres errores oficiales para `n=4`), la generación de esa
versión abortaría en vez de producir una instancia canónica con opciones incorrectas.

**Frecuencia esperada:** la instancia canónica ocurre cuando coinciden `ctx_idx == 1` (1 de 6
contextos) y `n == 4` (1 de 3 valores) — 1/18 de las versiones, aproximadamente. En 300
evaluaciones del `data_generation` se observaron **16** instancias canónicas (ver
[`ROADMAP.md`](ROADMAP.md) §1), consistente con esa fracción esperada.

---

## 5. Invariantes que no se deben romper

| # | Invariante | Por qué | Cómo verificarlo |
|---|---|---|---|
| **I-1** | Las 4 opciones son distintas: `length(unique(all_vals)) == 4L` | Previene `ERR_ANS_C` (opciones duplicadas) | `stopifnot`; 0 colisiones en las 30 ternas exhaustivas (§3) |
| **I-2** | Ningún distractor coincide con la respuesta correcta | Un distractor igual a la clave no discrimina nada | `stopifnot` |
| **I-3** | Plausibilidad de magnitud: `max(all_vals) / correcta_val <= 15` | Regla #22, patrón P5: un distractor *outlier* de magnitud se descarta sin razonar | `stopifnot`; rango observado ahora `[1,0×, 10,8×]` (§3), umbral con holgura |
| **I-4** | Las 4 opciones son enteros positivos | Coherencia de tipo con la pregunta ("¿de cuántas formas...?") | `stopifnot` |
| **I-5** | Exactamente una opción marcada y coincide con `n!` | Correctitud de `exsolution` (Nivel 5B de `validar_coherencia_matematica.R`) | `stopifnot` |
| **I-6** (nueva, 2026-07-29) | En la instancia canónica (`ctx_idx==1` y `n==4`), el conjunto de opciones es exactamente `{24, 64, 16, 4}` | Fidelidad al ítem oficial (OE1) cuando la Decisión D3 fuerza los 3 errores oficiales | `stopifnot` (dentro de `if (es_canonica)`), ver §4.8 |
| **I-7** (antes I-6) | El `.Rmd` permanece **auto-contenido**: `pick_int()`, `safe_sample()`, `fmt()` y `errores_conceptuales` viven dentro de `data_generation` | `validar_diversidad_sustantiva.R` (regla #22, obligatorio) hace `setwd(tempdir())` y evalúa el chunk en un `new.env()` fuera del pipeline de `xexams()`; ahí `include_supplement()` falla — el hermano `desplazamiento-avion-aeropuerto` lo midió: 40/40 semillas | `Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40` |
| **I-8** (antes I-7) | No hay `set.seed()` dentro de ningún chunk | Corrompería el RNG del render y colapsaría la diversidad (regla #10) | `grep -n 'set.seed' <rmd>` → 0 coincidencias dentro de los chunks del ejercicio (el único `set.seed()` del subproyecto vive en `verificar_render.R`, fuera del `.Rmd`, uso estándar para reproducibilidad de la verificación) |
| **I-9** (antes I-8) | Las **cinco** `calcula()` del pool son funciones puras (sin `sample`/`runif`/`rnorm`) | Capa D de `validar_coherencia_matematica.R`: `calcula()` debe ser determinista | `calcula()` en, 102, 120, 139, 160 — puramente aritméticas sobre `n` |

**Nota de numeración (2026-07-29):** el `.Rmd` etiqueta en sus propios comentarios una invariante
`I-6` (la de la instancia canónica) que colisionaba con la numeración anterior de
este documento, donde I-6 era la del auto-contenido. Se resolvió renumerando las invariantes
"meta" (auto-contenido, sin `set.seed`, `calcula()` puras) de I-6/I-7/I-8 a I-7/I-8/I-9, dejando
I-6 para la invariante que el código mismo etiqueta así.

---

## 6. Inventario de construcciones del `data_generation`

Este inventario se indexa por **construcción**, no por número de línea. Las versiones previas de
este documento fijaban ~93 números de línea del `.Rmd`; el code-review del 2026-07-29 encontró
varios ya erróneos (entre ellos el `stopifnot` de I-3, citado como «línea 142» cuando estaba en la
291) y, como cualquier edición del `.Rmd` desplaza el resto en silencio, enviaban al lector al
bloque equivocado. Localiza cada elemento con `grep -n` sobre su identificador.

| Construcción real |
|---|
| `pick_int <- function(a, b) if (a >= b) a else sample(a:b, 1L)` |
| `safe_sample <- function(x, size = 1L, replace = FALSE) { ... }` |
| `N_POOL <- c(4L, 5L, 6L)` |
| `n <- safe_sample(N_POOL, 1L)` |
| `correcta_val <- as.integer(factorial(n))` |
| pool de **5** errores conceptuales (`EST-PER-01` a `05`) |
| `calcula = function(n) as.integer(n^(n - 1L))` (`EST-PER-01`) |
| `calcula = function(n) as.integer(n * n)` (`EST-PER-02`) |
| `calcula = function(n) as.integer(n)` (`EST-PER-03`) |
| `calcula = function(n) as.integer(factorial(n - 1L))` (`EST-PER-04`, nuevo) |
| `calcula = function(n) as.integer(n * (n + 1L) / 2L)` (`EST-PER-05`, nuevo) |
| pool de 6 contextos narrativos (regla #11) |
| `ctx_idx <- safe_sample(seq_along(contextos), 1L)` |
| `CODIGOS_OFICIALES <- c("EST-PER-01", "EST-PER-02", "EST-PER-03")` (Decisión D3) |
| filtrado por `precondicion` (patrón genérico, regla #8) |
| `es_canonica`; selección de 3 errores (o de los 3 oficiales si es canónica — Decisión D3) |
| `stopifnot(length(unique(all_vals)) == 4L)` (I-1) |
| `stopifnot(!any(unname(vals) == correcta_val))` (I-2) |
| `stopifnot(max(all_vals) / correcta_val <= 15)` (I-3) |
| `stopifnot(all(all_vals > 0L), all(all_vals == as.integer(all_vals)))` (I-4) |
| `if (es_canonica) stopifnot(setequal(all_vals, c(24L, 64L, 16L, 4L)))` (I-6, nueva) |
| `perm     <- sample(4L)` |
| `stopifnot(sum(sol) == 1L, ...)`; `stopifnot(identical(opciones[which(sol == 1L)], correcta))`; `stopifnot(length(unique(opciones)) == 4L)` (I-5) |
| `reflexion <- safe_sample(reflexiones, 1L)` |
| `\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother` |
| comentario: regla #19, opciones identificadas por contenido y código, nunca por letra |
| bucle sobre `errores_info` (Análisis de cada opción) |
| 429-446 | *Caso específico* — transferencia al ítem espejo `MAT-2026-1-029`; puntero condicional a `EST-PER-01` |
| `exshuffle: TRUE` |

Ninguna entrada de este inventario depende de números de línea, así que sobrevive a las ediciones
del `.Rmd`. Si añades una construcción, añádela aquí por su identificador, nunca por su línea.

---

## 7. Referencias cruzadas

- [`../README.md`](../README.md) — entrada del subproyecto
- [`../HANDOFF.md`](../HANDOFF.md) — estado de trabajo y cómo retomar (pendiente)
- [`SYLLABUS.md`](SYLLABUS.md) — encuadre pedagógico y pool de errores
- [`BACKLOG.md`](BACKLOG.md) — pendientes priorizados
- [`ROADMAP.md`](ROADMAP.md) — hitos y objetivos específicos
- `../.claude/CLAUDE.md` — particularidades operativas para agentes (pendiente)
- `../.claude/rules/permutaciones-parametricas.md` — contrato local del pool `n!` y del pool de
  cinco errores conceptuales (pendiente, lo escribe otro agente)
- Reglas del repo raíz: `#1` `ejercicios-metacognitivos.md` · `#8`/`#10` `codigo-rmd.md` ·
  `#11` `contextos-narrativos-creativos.md` · `#19` `solution-letter-independence.md` ·
  `#20` `markdown-tablas-pandoc.md` · `#21` `familias-soluciones-rmd.md` ·
  `#22` `diversidad-sustantiva.md`
- `../../../../.claude/docs/AUTOCONTENCION_REXAMS.md` — mecanismo de copia a tempdir de R/exams

---

**Versión**: 1.1
**Fecha**: 2026-07-29
