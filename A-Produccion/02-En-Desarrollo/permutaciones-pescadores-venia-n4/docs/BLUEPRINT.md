# Blueprint — Permutaciones de los pescadores en la venia final

> Arquitectura técnica de la familia. Para el encuadre pedagógico ver
> [`SYLLABUS.md`](SYLLABUS.md); para el estado de trabajo ver [`../HANDOFF.md`](../HANDOFF.md).

El subproyecto contiene **dos variantes** del mismo ítem, que comparten el contrato paramétrico
(`N_POOL`, clave `n!`, pool de 7 errores, invariantes I-1..I-7 y la instancia canónica) y difieren
solo en la forma de interrogar. La arquitectura común está en §1-§5; lo propio de la variante
CLOZE, en **§7**.

| Campo | SCHOICE (raíz) | CLOZE (`cloze/`) |
|---|---|---|
| **Archivo fuente** | `permutaciones_pescadores_..._n4_schoice_v1.Rmd` | `cloze/permutaciones_pescadores_..._n4_cloze_v1.Rmd` |
| **Líneas** | 601 (`wc -l`, 2026-07-30) | 971 (`wc -l`, 2026-07-30) |
| **Chunks R** | 4 (`data_generation`, `question_body`, `answerlist_q`, `solucion`) + 1 guard LaTeX | 9 (`data_generation`, `enunciado`, `parte2`, `parte3`, `parte4`, `parte6`, `answerlist_q`, `solucion`, `answerlist_s`) + 1 guard LaTeX |
| **Verificador** | `verificar_render.R` (V1–V9) | `cloze/verificar_render.R` (V1–V11) |
| **Formatos** | HTML, PDF, DOCX, **NOPS**, Moodle | HTML, PDF, DOCX, Moodle — **NOPS es N/A** (§7.5) |
| **Lenguaje gráfico** | Ninguno — Flujo B = `false`, no hay figura | Ninguno — ídem |
| **Tipo** | SCHOICE, 1 pregunta, opciones de texto | CLOZE, 6 partes Progressive Disclosure |
| **Auto-contenido** | **Sí, obligatorio** (ver §5, invariante I-8) | **Sí, obligatorio** (ídem) |

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
    ├─► errores_conceptuales (pool de 7, con precondicion/calcula)
    │      ├─► EST-PER-01  calcula(n) = n^(n-1)      > n!
    │      ├─► EST-PER-02  calcula(n) = n*n
    │      ├─► EST-PER-03  calcula(n) = n
    │      ├─► EST-PER-04  calcula(n) = (n-1)!
    │      ├─► EST-PER-05  calcula(n) = n*(n+1)/2
    │      ├─► EST-PER-06  calcula(n) = (n+1)!       > n!   [Decisión D4]
    │      └─► EST-PER-07  calcula(n) = 2*n!         > n!   [Decisión D4]
    │
    ├─► ctx ← 1 de 6 contextos narrativos (canónico + 5 variantes)
    ├─► texto_enunciado, texto_pregunta ← ctx$enunciado/pregunta(n)
    │
    ├─► aplicables ← filtrado por precondicion (los 7 aplican siempre)
    ├─► es_mayor ← calcula(n) > correcta_val   (derivado, no lista hardcoded)
    ├─► es_canonica ← (ctx_idx==1 && n==4)
    ├─► sel ← 3 oficiales SI es_canonica, si no:                   [Decisión D3]
    │      ├─► ternas  ← combn(aplicables, 3)          espacio completo
    │      ├─► legales ← ternas con >= 1 distractor > n!  [I-7, Decisión D4]
    │      └─► sample(legales, 1)   ← índice sorteado, NUNCA bucle de reintento
    ├─► vals ← calcula(n) de los 3 errores seleccionados
    │      ├─► stopifnot I-1..I-4 (unicidad, ≠correcta, ratio≤15x, enteros>0)
    │      ├─► stopifnot I-6 (canónica ⇒ opciones = {24,64,16,4})
    │      └─► stopifnot I-7 (algún distractor > clave ⇒ la clave nunca es la mayor)
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
| 2 | 2 | 2 | 4 | 2 | 2 | 2,0x | Colisión doble (`n! == n^(n-1)`) |
| 3 | 6 | 9 | 9 | 3 | 3 | 1,5x | Colisión `n^(n-1) == n²` |
| **4** | **24** | 64 | 16 | 4 | **4** | 2,7x | **OK** |
| **5** | **120** | 625 | 25 | 5 | **4** | 5,2x | **OK** |
| **6** | **720** | 7776 | 36 | 6 | **4** | 10,8x | **OK** |
| 7 | 5040 | 117649 | 49 | 7 | 4 | 23,3x | Outlier de magnitud |
| 8 | 40320 | 2097152 | 64 | 8 | 4 | 52,0x | Outlier de magnitud |

> **Corrección de formato (2026-07-30).** Hasta hoy, cuatro de las siete filas de esta tabla
> (`n` = 2, 3, 7, 8) **no tenían su primera celda**, así que se leían desplazadas una columna: la
> fila de `n=3` empezaba por `| 6 |` y parecía decir «n = 6». El defecto fue reportado por `/goal`
> el 2026-07-29 y la memoria del proyecto lo daba por corregido; no lo estaba. Las cifras se han
> recalculado con `factorial(n)`, `n^(n-1)`, `n^2` y `n` para `n` de 2 a 8 antes de reescribirla.

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
`n²`, `n`), porque así se fijó `N_POOL` cuando el pool de errores tenía 3 entradas. El pool creció
después a 5 (auditoría adversarial: `EST-PER-04` circular, `EST-PER-05` aditivo) y luego a **7**
(decisión **D4**: `EST-PER-06` `(n+1)!` y `EST-PER-07` `2·n!`, ambas mayores que `n!`, para cerrar el
hallazgo H1). El contrato de unicidad y plausibilidad debe verificarse para **todas** las
combinaciones de 3 distractores posibles, no solo para esta terna fija — ver §3, que reemplaza esta
tabla como fuente de verdad del contrato vigente.

Las dos fórmulas añadidas por D4 **no mueven** el techo de magnitud: en `n=6`, `(n+1)! = 5040` es
7,0× la clave y `2·n! = 1440` es 2,0×, ambas por debajo del 10,8× que ya aportaba `EST-PER-01`. El
umbral de 15× y el rango `n ∈ {4,5,6}` siguen intactos, así que D4 **no roza** la decisión D2.

---

## 3. Contrato algebraico del pool de errores (equivalente al contrato geométrico de los hermanos)

`plano-cartesiano-barco-n2` sostiene su clave con un contrato **geométrico** (el *bounding box*
del casco dibujado debe coincidir con la clave). Aquí, al no haber dibujo, el contrato es
**algebraico**: para cada `n ∈ N_POOL` y para **cada una de las C(7,3) = 35 combinaciones** de tres
distractores elegidos del pool de siete fórmulas (`n^(n-1)`, `n²`, `n`, `(n-1)!`, `n(n+1)/2`,
`(n+1)!`, `2·n!`), el conjunto {correcta, distractor, distractor, distractor} debe tener **4
elementos distintos**, ninguno debe superar **15×** la clave, y los cuatro deben ser enteros
positivos. Son **105 combinaciones en total** (3 valores de `n` × 35 ternas) — el espacio completo,
no una muestra.

Sobre ese espacio, la selección del `.Rmd` se restringe además al **espacio legal**: las ternas con
al menos un distractor mayor que `n!` (invariante **I-7**, decisión **D4**). Son **93 de las 105**;
las 12 descartadas son exactamente las que no contienen ninguna de las tres fórmulas mayores
(`C(4,3) = 4` por cada valor de `n`). Este contrato se verifica en **tres** capas independientes:

1. **En tiempo de generación** (`.Rmd`): los `stopifnot` I-1 a I-4 (sobre la terna efectivamente
   seleccionada en esa versión), I-6 (sobre la instancia canónica) e I-7 abortan la generación si el
   contrato se rompe.
2. **Fuera del render, sobre el ESPACIO**, por enumeración exhaustiva de las 105 combinaciones en
   `verificar_render.R` (`V6`): para cada `n ∈ {4,5,6}` recorre las 35 ternas de
   `utils::combn(7, 3)` y verifica unicidad, `distractor ≠ correcta` y `max/clave ≤ 15` sobre el
   espacio completo; las métricas que ve el estudiante (rango de la clave, dominancia) las mide
   **solo sobre las 93 legales**, porque incluir las ilegales daría un falso verde: aportan
   precisamente los rangos que la restricción existe para eliminar.
3. **Fuera del render, sobre la SELECCIÓN** (`V9`, 240 semillas): comprueba que el chunk *realmente*
   se restringe al espacio legal. V6 mide el espacio, no la selección: si alguien borrara el filtro
   `legales`, V6 seguiría verde informando «mitad baja 41,9 %» mientras el ejercicio emite otra vez
   ternas donde la clave es la mayor.

**Resultado de la enumeración exhaustiva (2026-07-29, 105/105 ternas verdes):** la razón máx/clave
observada es **`[1,0×, 10,8×]`**; el límite superior sigue siendo `n=6` con `EST-PER-01`. Sobre las
93 ternas legales, el **rango de la clave por magnitud es 1.º, 2.º o 3.º — nunca 4.º**: la clave
queda en la mitad baja en el **41,9 %** de las ternas, «elegir el número mayor» acierta en el
**0,0 %**, y la clave vale como máximo **0,50×** el mayor distractor. `verificar_render.R` imprime
las tres cifras en cada corrida y **falla** —no avisa— si cualquiera de ellas regresa.

A diferencia del contrato geométrico del hermano —que puede romperse en silencio si alguien
"suaviza" `prof()` sin que ningún validador sintáctico lo note—, el contrato algebraico aquí es una
propiedad **decidible por cálculo directo** de siete fórmulas cerradas: no depende de ninguna
función auxiliar que alguien pueda editar sin querer, y la enumeración exhaustiva de las 105
combinaciones (en vez de una muestra) elimina cualquier duda sobre combinaciones no cubiertas.

### 3.1 Barrido de configuraciones del pool (evidencia de la decisión D4)

Antes de fijar el pool en 7 fórmulas + I-7 se **midieron** las alternativas por enumeración
exhaustiva del espacio de cada una. La tabla es reproducible: extrae las fórmulas del `.Rmd` real
(no una copia) y recorre `combn` sobre cada configuración.

| Configuración | ternas | rangos de la clave | % mitad baja | % la clave es la mayor | dominancia máx |
|---|---:|:---:|---:|---:|---:|
| A. pool 5, sin I-7 | 30 | 3/4 | 0,0 % | **40,0 %** | 20,00× |
| B. pool 5 + I-7 | 30 | **3 (fijo)** | 0,0 % | 0,0 % | 0,38× |
| C. pool 6 (+`(n+1)!`) + I-7 | 60 | 2/3 | 25,0 % | 0,0 % | 0,38× |
| D. pool 6 (+`2·n!`) + I-7 | 60 | 2/3 | 25,0 % | 0,0 % | 0,50× |
| E. pool 7, sin I-7 | 105 | 1/2/3/4 | 37,1 % | 11,4 % | 20,00× |
| **F. pool 7 + I-7 — ELEGIDA** | **105** | **1/2/3** | **41,9 %** | **0,0 %** | **0,50×** |

Ninguna configuración produjo opciones duplicadas ni superó el umbral de 15×, así que la elección se
decidió solo por el patrón de magnitud. Lecturas que fijan la decisión:

- **A es el defecto H1** en su estado original: la clave nunca cae en la mitad baja y «elegir el
  mayor» acierta 2 de cada 5 versiones.
- **B (añadir solo la restricción, sin ampliar el pool) NO es viable.** Elimina el atajo del máximo,
  pero deja el rango de la clave **fijo en 3.º** en las 30 ternas — un patrón posicional puro, que es
  justo lo que la guarda de V6 rechaza (regla #22, patrón P4). Corrige un atajo creando otro.
- **C y D funcionan** pero cubren la mitad baja solo el 25 % y no alcanzan nunca el 1.º puesto,
  porque una terna de 3 no puede contener 3 fórmulas mayores si solo hay 2 en el pool.
- **E muestra que las fórmulas nuevas no bastan por sí solas**: sin I-7 sigue habiendo un 11,4 % de
  ternas donde la clave es la mayor. La restricción es la mitad esencial de D4, no un adorno.
- **F** es la configuración mínima que cierra H1 sin borrar ningún distractor diagnóstico: 7 es el
  tamaño más pequeño que permite rango 1.º y maximiza la cobertura de mitad baja. Por eso el test
  `test_permutaciones_invariantes.R` fija `expect_gte(length(pool), 7L)`: encogerlo reintroduce el
  patrón en silencio.

El pool de 7 excede el «4-6» que menciona el comentario de la regla #1
(`.claude/rules/ejercicios-metacognitivos.md`), cuyo texto es *«Mínimo 4-6 errores por ejercicio»* —
un piso, no un techo. 7 es el precio medido de cerrar H1.

**Reproducir la tabla:** el script del barrido no se versiona (es un instrumento de medición de una
sola decisión, no infraestructura). Su lógica está congelada en las capas permanentes: V6 mide la
fila F en cada corrida de `verificar_render.R`, y el bloque `I-7` de
`tests/testthat/test_permutaciones_invariantes.R` reproduce sus dos guardas de no-regresión
(`any(rangos <= 2)` y `!any(rangos == 4)`) contra el pool extraído del `.Rmd`.

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
carácter a `MAT-2026-1-004`, verificado automáticamente por el chequeo `V7` de
`verificar_render.R` (localízalo con `grep -n 'V7' verificar_render.R`; la versión previa de este
documento lo citaba como «líneas 167-188», que ya no era cierto — es justo lo que prohíbe la
particularidad 12). El ítem espejo `MAT-2026-1-029` (mismo descriptor `D4.8`, conteo **con**
repetición) se
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

**La resolución (Decisión D3):** la selección normal de errores sortea una terna del espacio legal
del pool de 7 (`.Rmd`; con la decisión D4 el sorteo pasó de `sample(aplicables, 3)` a enumerar las
ternas que cumplen I-7 y sortear un índice). Pero cuando la versión es la **instancia canónica** — contexto 1 (el
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

### 4.9 Decisión D4 — pool de 7 y restricción I-7 para cerrar el hallazgo H1

**Autorizada por el usuario el 2026-07-30.** El hallazgo H1 quedó registrado en
[`BACKLOG.md`](BACKLOG.md) como decisión humana pendiente con dos salidas: aceptar la propiedad por
fidelidad al ítem oficial, o ampliar el pool con fórmulas mayores que `n!`. Se autorizó la segunda.

**El problema.** Con el pool de 5, las cuatro fórmulas menores que `n!` (`n²`, `n`, `(n-1)!`,
`n(n+1)/2`) superaban en número a la única mayor (`n^(n-1)`). Consecuencia medida sobre las 30
ternas: la clave quedaba en 3.º lugar en el 60 % y era **la mayor** en el 40 % restante, nunca en la
mitad baja. Un estudiante que descartara las dos opciones menores sin saber combinatoria pasaba de
adivinar al 25 % a adivinar al **50 %**, y la heurística «elegir el número mayor» acertaba 2 de cada
5 versiones. Es la regla #22 patrón P5 aplicada a la **clave** en vez de a un distractor.

**La corrección tiene dos mitades, y ninguna funciona sola** (§3.1):

1. **Dos fórmulas nuevas mayores que `n!`** — `EST-PER-06` `(n+1)!` (contar una posición más de las
   que hay) y `EST-PER-07` `2·n!` (duplicar el conteo por el orden inverso). Ambas son errores
   conceptuales diagnósticos por derecho propio, no relleno numérico: la primera es un error de
   conteo del conjunto antes de aplicar la fórmula; la segunda, un doble conteo por una simetría que
   el factorial ya incluye — el error simétrico de la fórmula circular de `EST-PER-04`.
2. **La restricción I-7**: toda terna debe contener al menos un distractor mayor que `n!`. Sin ella,
   el pool de 7 aún deja un 11,4 % de ternas donde la clave es la mayor (configuración E).

**Coste y por qué se acepta.** El pool sube a 7, por encima del «4-6» que menciona la regla #1 (cuyo
texto es *«Mínimo 4-6»*, un piso). La alternativa de quedarse en 6 cubre la mitad baja solo el 25 % y
nunca alcanza el 1.º puesto. Quedarse en 5 y añadir solo la restricción **empeora** el ítem: deja el
rango de la clave fijo en 3.º, un patrón posicional puro que la propia guarda de V6 rechaza.

**Lo que D4 NO toca:**

- **La instancia canónica.** Los tres errores oficiales incluyen `EST-PER-01` (`64 > 24`), así que la
  terna canónica cumple I-7 por sí sola y `MAT-2026-1-004` se sigue reproduciendo verbatim con sus
  opciones `{24, 64, 16, 4}` — donde la clave es la 3.ª, igual que en el original. La asimetría es
  deliberada: **fidelidad en la instancia canónica, mitigación en las variantes**.
- **La decisión D2.** Las fórmulas nuevas valen 7,0× y 2,0× la clave en el peor `n`, por debajo del
  10,8× que ya aportaba `EST-PER-01`. El umbral de 15× y `n ∈ {4,5,6}` quedan intactos (§2).
- **La forma de sortear.** La terna se elige enumerando el espacio legal y sorteando un índice, nunca
  con un bucle de reintento (regla #21, Familia 1 — el patrón del Error 22 que cuelga el render).

**Coste de mantenimiento asumido:** cualquier cambio futuro del pool obliga a re-medir el espacio
completo. Está cableado para que falle en vez de degradarse: `V6` re-mide las 105 ternas y las tres
cifras de H1 en cada corrida, `V9` comprueba la selección real sobre 240 semillas, y el test fija
`expect_gte(length(pool), 7L)`.

---

## 5. Invariantes que no se deben romper

| # | Invariante | Por qué | Cómo verificarlo |
|---|---|---|---|
| **I-1** | Las 4 opciones son distintas: `length(unique(all_vals)) == 4L` | Previene `ERR_ANS_C` (opciones duplicadas) | `stopifnot`; 0 colisiones en las **105** ternas exhaustivas (§3) |
| **I-2** | Ningún distractor coincide con la respuesta correcta | Un distractor igual a la clave no discrimina nada | `stopifnot` |
| **I-3** | Plausibilidad de magnitud: `max(all_vals) / correcta_val <= 15` | Regla #22, patrón P5: un distractor *outlier* de magnitud se descarta sin razonar | `stopifnot`; rango observado `[1,0×, 10,8×]` (§3), umbral con holgura. **Es unilateral**: no acota que la clave domine — eso lo cubre I-7 |
| **I-4** | Las 4 opciones son enteros positivos | Coherencia de tipo con la pregunta ("¿de cuántas formas...?") | `stopifnot` |
| **I-5** | Exactamente una opción marcada y coincide con `n!` | Correctitud de `exsolution` (Nivel 5B de `validar_coherencia_matematica.R`) | `stopifnot` |
| **I-6** (2026-07-29) | En la instancia canónica (`ctx_idx==1` y `n==4`), el conjunto de opciones es exactamente `{24, 64, 16, 4}` | Fidelidad al ítem oficial (OE1) cuando la Decisión D3 fuerza los 3 errores oficiales | `stopifnot` (dentro de `if (es_canonica)`), ver §4.8 |
| **I-7** (nueva, 2026-07-29) | Al menos un distractor supera la clave: `any(unname(vals) > correcta_val)` ⇒ la clave **nunca** es la opción de mayor magnitud | Cierra el hallazgo H1: sin ella, «elegir el número mayor» acertaba el 40 % de las versiones y la clave llegaba a valer 20× el mayor distractor. I-3 no puede darlo porque cuando la clave *es* el máximo su ratio vale 1,0× y pasa trivialmente | `stopifnot`; `V6` (espacio: 93/105 legales) + `V9` (selección: 240/240), ver §3.1 y Decisión D4 |
| **I-8** (antes I-7) | El `.Rmd` permanece **auto-contenido**: `pick_int()`, `safe_sample()`, `fmt()` y `errores_conceptuales` viven dentro de `data_generation` | `validar_diversidad_sustantiva.R` (regla #22, obligatorio) hace `setwd(tempdir())` y evalúa el chunk en un `new.env()` fuera del pipeline de `xexams()`; ahí `include_supplement()` falla — el hermano `desplazamiento-avion-aeropuerto` lo midió: 40/40 semillas | `Rscript ../../../.claude/scripts/validar_diversidad_sustantiva.R <rmd> --n 40` |
| **I-9** (antes I-8) | No hay `set.seed()` dentro de ningún chunk | Corrompería el RNG del render y colapsaría la diversidad (regla #10) | `grep -n 'set.seed' <rmd>` → 0 coincidencias dentro de los chunks del ejercicio (los únicos `set.seed()` del subproyecto viven en `verificar_render.R` y en la suite de tests, fuera del `.Rmd`, uso estándar para reproducibilidad de la verificación) |
| **I-10** (antes I-9) | Las **siete** `calcula()` del pool son funciones puras (sin `sample`/`runif`/`rnorm`) | Capa D de `validar_coherencia_matematica.R`: `calcula()` debe ser determinista | Los siete `calcula = function(n)` del bloque `errores_conceptuales` son puramente aritméticos sobre `n`; localizarlos con `grep -n 'calcula *= *function' <rmd>` |

**Alcance (actualizado 2026-07-30).** Las diez invariantes de esta tabla rigen para **las dos
variantes**: los `stopifnot` de I-1..I-7 están duplicados literalmente en los dos `.Rmd` porque el
pool y el contrato son los mismos, y las meta I-8..I-10 (auto-contenido, sin `set.seed`, `calcula()`
puras) se aplican igual a ambos. La variante CLOZE añade **tres invariantes propias, C-1..C-3**,
descritas en §7.3; se numeran con prefijo `C` en vez de continuar la serie `I` justamente para no
disparar otra vez la renumeración que describe la nota siguiente.

**Nota de numeración (actualizada 2026-07-29).** El `.Rmd` etiqueta invariantes en sus propios
comentarios, y esas etiquetas son la fuente de verdad: cuando colisionan con la numeración de este
documento, se renumeran las invariantes **"meta"** de este documento (auto-contenido, sin
`set.seed`, `calcula()` puras), nunca las del código.

Ha ocurrido dos veces. Primero el `.Rmd` introdujo `I-6` (instancia canónica) y las meta pasaron de
I-6/I-7/I-8 a I-7/I-8/I-9. Ahora la decisión D4 introdujo `I-7` (la clave nunca es la mayor) y las
meta pasan de I-7/I-8/I-9 a **I-8/I-9/I-10**. Las siete primeras (I-1..I-7) son exactamente las que
el código verifica con `stopifnot` y las que enumera
[`../.claude/rules/permutaciones-parametricas.md`](../.claude/rules/permutaciones-parametricas.md).

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
| pool de **7** errores conceptuales (`EST-PER-01` a `07`) |
| `calcula = function(n) as.integer(n^(n - 1L))` (`EST-PER-01`) |
| `calcula = function(n) as.integer(n * n)` (`EST-PER-02`) |
| `calcula = function(n) as.integer(n)` (`EST-PER-03`) |
| `calcula = function(n) as.integer(factorial(n - 1L))` (`EST-PER-04`) |
| `calcula = function(n) as.integer(n * (n + 1L) / 2L)` (`EST-PER-05`) |
| `calcula = function(n) as.integer(factorial(n + 1L))` (`EST-PER-06`, D4 — mayor que `n!`) |
| `calcula = function(n) as.integer(2L * factorial(n))` (`EST-PER-07`, D4 — mayor que `n!`) |
| pool de 6 contextos narrativos (regla #11) |
| `ctx_idx <- safe_sample(seq_along(contextos), 1L)` |
| `CODIGOS_OFICIALES <- c("EST-PER-01", "EST-PER-02", "EST-PER-03")` (Decisión D3) |
| filtrado por `precondicion` (patrón genérico, regla #8) |
| `es_mayor <- vapply(errores_conceptuales, function(e) e$calcula(n) > correcta_val, logical(1L))` (clasificación DERIVADA, no lista de códigos) |
| `es_canonica`; selección de 3 errores (o de los 3 oficiales si es canónica — Decisión D3) |
| `ternas <- utils::combn(aplicables, 3L)`; `legales <- which(apply(ternas, 2L, function(idx) any(es_mayor[idx])))`; `sel <- sort(ternas[, safe_sample(legales, 1L)])` (I-7 / D4 — enumeración + índice sorteado, sin bucle de reintento) |
| `stopifnot(length(unique(all_vals)) == 4L)` (I-1) |
| `stopifnot(!any(unname(vals) == correcta_val))` (I-2) |
| `stopifnot(max(all_vals) / correcta_val <= 15)` (I-3) |
| `stopifnot(all(all_vals > 0L), all(all_vals == as.integer(all_vals)))` (I-4) |
| `if (es_canonica) stopifnot(setequal(all_vals, c(24L, 64L, 16L, 4L)))` (I-6) |
| `stopifnot(any(unname(vals) > correcta_val))` (I-7, nueva — D4) |
| `perm     <- sample(4L)` |
| `stopifnot(sum(sol) == 1L, ...)`; `stopifnot(identical(opciones[which(sol == 1L)], correcta))`; `stopifnot(length(unique(opciones)) == 4L)` (I-5) |
| `reflexion <- safe_sample(reflexiones, 1L)` |
| `\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother` |
| comentario: regla #19, opciones identificadas por contenido y código, nunca por letra |
| bucle sobre `errores_info` (Análisis de cada opción) |
| subsección *Caso específico* de la `Solution` — transferencia al ítem espejo `MAT-2026-1-029`; puntero condicional a `EST-PER-01` |
| `exshuffle: TRUE` |

Ninguna entrada de este inventario depende de números de línea, así que sobrevive a las ediciones
del `.Rmd`. Si añades una construcción, añádela aquí por su identificador, nunca por su línea.

---

## 7. La variante CLOZE (`cloze/`)

Añadida el 2026-07-30. Reutiliza **íntegro** el pipeline de §1 hasta `errores_info` y añade encima
la descomposición en partes. El patrón de subdirectorio se tomó del hermano
`01-En-PreDesarrollo/Rango-Colesterol-Pacientes/Cloze/`, que ya convive así con su SCHOICE.

### 7.1 Por qué una variante y no un reemplazo

La SCHOICE es la que sostiene **OE1**: reproduce el ítem oficial tal como se evalúa, en una sola
pregunta con cuatro opciones. Descomponerlo en seis partes cambia lo que se mide —deja de ser «¿sabe
resolverlo?» y pasa a ser «¿reconoce cada pieza del razonamiento?»—, que es útil en clase pero ya no
es el ítem oficial. Por eso conviven: **la CLOZE no sustituye a la SCHOICE**.

### 7.2 Las 6 partes (Progressive Disclosure)

`exclozetype: schoice|num|schoice|num|mchoice|schoice`

| Parte | Tipo | Qué pregunta | Clave | Demanda |
|---|---|---|---|---|
| 1 | `schoice` | La pregunta del ítem oficial | `n!` | Aplicar |
| 2 | `num` | Cuántos quedan para el segundo lugar | `n-1` | Comprender el decrecimiento |
| 3 | `schoice` | Qué error produce un valor dado | el error mostrado | Analizar |
| 4 | `num` | Conteo **con** repetición: `n` cifras de `{1..n}` | `n^n` | Transferir |
| 5 | `mchoice` | 6 afirmaciones, `k ∈ {2,3,4}` verdaderas | vector binario | Evaluar |
| 6 | `schoice` V/F | Factor de crecimiento de `n` a `n+1` | V si el factor es `n+1` | Evaluar |

Dos detalles de diseño que no son cosméticos:

- **La Parte 4 es el ítem espejo `MAT-2026-1-029` convertido en pregunta.** En la SCHOICE ese ítem
  aparecía solo como *Caso específico* de la Solution, es decir, después de responder. Aquí el
  estudiante tiene que ejecutarlo, y el contraste `n!` vs `n^n` deja de ser una lectura para
  convertirse en la única dimensión que separa las dos respuestas.
- **La cuarta opción de la Parte 3 viene de un error que NO se mostró en la Parte 1.** Si las cuatro
  descripciones fueran las de los tres distractores vistos más la correcta, el estudiante podría
  resolverla por eliminación sin evaluar ninguna fórmula.

### 7.3 Invariantes propias de la variante (C-1 a C-3)

Las I-1..I-7 se heredan sin cambios. Estas tres son nuevas porque describen propiedades que la
SCHOICE no necesitaba.

| # | Invariante | Por qué | Dónde se verifica |
|---|---|---|---|
| **C-1** | Para cada `n ∈ N_POOL`, las 7 fórmulas del pool y `n!` dan **8 valores distintos dos a dos** | La Parte 3 muestra un valor y pide identificar su error entre 4 descripciones, **una de las cuales pertenece a un error ajeno a la terna**. Si dos fórmulas coincidieran en valor para ese `n`, esa parte tendría dos respuestas correctas — e **I-1 no lo vería**, porque solo mira la terna seleccionada | `stopifnot` en el chunk + `V10` del verificador, sobre `N_POOL` completo |
| **C-2** | nº de `##ANSWERi##` == nº de `exclozetype` == nº de bloques de `exsolution` == nº de bloques de `extol` == 6, y los placeholders están **en orden** | Regla #14. Un `##ANSWERi##` fuera de orden es el Incidente A del orquestador CLOZE y **ningún render lo detecta**: compila igual y asocia la respuesta a la parte equivocada | `stopifnot` en el chunk (conteos) + `V8` (orden, análisis estático del `.Rmd`) |
| **C-3** | Answerlist del enunciado = **16** ítems (solo gaps de elección: 4+4+6+2); Answerlist de la Solution = **18** (+1 por cada gap `num`) | Es el contrato de R/exams. La asimetría 16 ≠ 18 es un **falso positivo recurrente** de los auditores, que la leen como descuadre | `stopifnot` en el chunk + `V8` |

Sobre C-3, con precisión sobre lo que respalda cada mitad: la omisión de las entradas `num` en el
Answerlist del **enunciado** está documentada oficialmente («the empty entries for all other `num`
or `string`/`essay`/`file` elements can optionally be omitted», `NEWS.md` de `exams` 2.4-1). Que el
Answerlist de la **Solution** lleve además un ítem por gap `num` **no está declarado en la
documentación oficial** (consultado 2026-07-30): es el comportamiento observado en el render y el
que usa el hermano `Rango-Colesterol-Pacientes/Cloze/`.

### 7.4 Decisiones D5 y D6

**D5 — dos divergencias deliberadas respecto del SCHOICE.**

1. `fmt()` **no agrupa miles**. La SCHOICE escribe `7.776` al estilo del cuadernillo ICFES porque
   todas sus opciones se eligen con el ratón. Aquí hay dos gaps `num` que el estudiante **escribe**,
   y un separador de miles convierte `46.656` en el decimal 46,656 al parsear: respuesta correcta
   marcada como incorrecta. Una sola convención numérica en todo el ítem. **No afecta a OE1**: en la
   instancia canónica (`n = 4`) las cuatro opciones son menores que 1000.
2. **No se define `pick_int()`**. En la SCHOICE quedó sin ninguna invocación (código muerto). La
   Familia 1 de la regla #21 se cumple por la vía que importa: ningún bucle de reintento.

**D6 — la Solution nunca enumera la Parte 5 en su orden interno.** Medido sobre el HTML, no
deducido: con `exshuffle: TRUE`, R/exams reordena los **dos** Answerlists con la misma permutación
—quedan alineados entre sí— pero **no toca la prosa** de la Solution, que el `.Rmd` emite con
`cat()`. La primera versión listaba ahí las 6 afirmaciones en el orden del chunk y, tras el
barajado, esa lista y las opciones quedaban en órdenes distintos.

Es un modo de fallo **vecino** al de la regla #19 pero distinto: la regla #19 prohíbe identificar
una opción por su **letra**; aquí nadie cita letras — el defecto es enumerar en un **orden** que
R/exams cambia después.

Se descartó la salida fácil (`exshuffle: FALSE`, que alinearía los tres bloques): la aleatorización
interna la dan `perm`/`perm3`/`perm5`, pero `validar_coherencia_matematica.R` marca **`ERR_C4`**
—bloqueante— porque ICFES exige mezcla, y pedir una excepción para conservar una lista **redundante**
es mal negocio: el Answerlist de la Solution ya da el veredicto de cada afirmación. La regla que
queda: la prosa puede **agrupar** por valor de verdad (no afirma nada sobre posiciones), nunca
reproducir la lista en su orden interno.

### 7.5 NOPS es N/A, y no por los gaps `num`

`exams2nops()` **rechaza cualquier `extype: cloze`**, con independencia de los tipos de gap.
Verificado en el código de `exams` 2.4.2, que antes de mirar `exclozetype` hace:

```r
utype      <- sapply(ufile, function(n) x[[n]]$type)
wrong_type <- ufile[utype == "cloze"]
if (length(wrong_type) > 0L) stop(paste("the following exercises are cloze exercises:", ...))
```

La documentación oficial **no lo declara**: `?exams2nops` enumera los tipos soportados
(`schoice`, `mchoice`, y soporte limitado de `string`) y omite `cloze` sin decir que no lo admite;
el tutorial `/tutorials/exams2nops/` no menciona la palabra. Es una omisión documental, no una
prohibición escrita (consultado 2026-07-30). `V4` comprueba que el motivo del rechazo **sigue siendo
ese** y no otro: si algún día `exams` admitiera cloze en NOPS, o si fallara por otra causa, lo dirá.

### 7.6 Evidencia medida (2026-07-30)

| Verificación | Resultado |
|---|---|
| `cloze/verificar_render.R` | **V1–V11 todo verde** |
| V5 — Moodle | **12/12** versiones: 6 gaps en orden y con su tipo; Parte 1 = `n!`, Parte 2 = `n-1`, Parte 4 = `n^n` |
| V6 — espacio | **105/105** ternas; 93 legales; rango de la clave 1/2/3; mitad baja 41,9 %; «elegir el mayor» 0,0 % |
| V9 — selección | **240/240** versiones; 84 ternas distintas |
| V10 — C-1 | 8 valores por `n` distintos dos a dos en `n = 4/5/6` |
| V11 — D6 | 6/6 afirmaciones con el mismo veredicto en prosa y Answerlist |
| `validar_coherencia_matematica.R` | **APROBADO, 0 errores** |
| `validar_diversidad_sustantiva.R --n 40` | exit 0 · `WARN_DIV_BAJA` (estructural, §5 de `BACKLOG.md`) |
| Ortografía | sin errores |
| Diversidad (300 evaluaciones) | **300/300 versiones únicas** · 90 de 93 ternas legales · 12 instancias canónicas · reparto de `n` 90/113/97 |

**Prueba de mutación del verificador** (se desactivaron las guardas internas para que el mutante
llegue a renderizar):

| Mutación | Resultado |
|---|---|
| `exshuffle: FALSE` → `TRUE` sin agrupar la prosa | **V11 falla**; V5 sigue verde (la clave no cambia) — el guard mide lo que dice medir |
| Clave falsa en la Parte 1 + I-5 y el `stopifnot` del alias desactivados | **V5 falla**: 8 incoherencias en 12/12 versiones |

La segunda mutación tuvo que repetirse: al primer intento el chunk abortaba por un `stopifnot`
**distinto** del que se pretendía neutralizar (el del bloque de alias), de modo que el mutante no
llegaba a probar V5. Es la utilidad concreta de la prueba de mutación: descubrió que la clave está
protegida por **dos** guardas independientes, no una.

---

## 8. Referencias cruzadas

- [`../README.md`](../README.md) — entrada del subproyecto
- [`../HANDOFF.md`](../HANDOFF.md) — estado de trabajo y cómo retomar
- [`SYLLABUS.md`](SYLLABUS.md) — encuadre pedagógico y pool de errores
- [`BACKLOG.md`](BACKLOG.md) — pendientes priorizados
- [`ROADMAP.md`](ROADMAP.md) — hitos y objetivos específicos
- [`../.claude/CLAUDE.md`](../.claude/CLAUDE.md) — particularidades operativas para agentes
- [`../.claude/rules/permutaciones-parametricas.md`](../.claude/rules/permutaciones-parametricas.md)
  — contrato local: la clave `n!`, el pool de **siete** errores conceptuales y las invariantes
  I-1..I-7 (más las C-1..C-3 propias de la variante CLOZE, §7.3)
- Reglas del repo raíz: `#1` `ejercicios-metacognitivos.md` · `#8`/`#10` `codigo-rmd.md` ·
  `#11` `contextos-narrativos-creativos.md` · `#19` `solution-letter-independence.md` ·
  `#20` `markdown-tablas-pandoc.md` · `#21` `familias-soluciones-rmd.md` ·
  `#22` `diversidad-sustantiva.md`
- `../../../../.claude/docs/AUTOCONTENCION_REXAMS.md` — mecanismo de copia a tempdir de R/exams

---

**Versión**: 3.0 (variante CLOZE en §7, con las invariantes C-1..C-3 y las decisiones D5 y D6;
tabla de §2 reconstruida —cuatro filas estaban descuadradas—; recuento de líneas y referencias
«(pendiente)» sincronizados con la realidad; la cita por número de línea de §4.7 sustituida por
ancla)
**Fecha**: 2026-07-30
