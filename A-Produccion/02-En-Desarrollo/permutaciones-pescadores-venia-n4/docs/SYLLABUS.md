# Syllabus — Permutaciones de los pescadores en la venia final

> Ver [`../HANDOFF.md`](../HANDOFF.md) para el estado de trabajo completo (pendiente de escribir
> en esta sesión). Este documento describe **qué enseña y evalúa** el ejercicio, no su estado de
> desarrollo.

## 1. Ficha de metadatos ICFES (copia literal del `.Rmd`)

**Regla dura de este documento**: los campos siguientes son texto oficial del `.Rmd` y se copian
**carácter por carácter** de la sección `Meta-information`
(`../permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd`, chunk `solucion`,
verificadas con `grep -n` el 2026-07-29). Ninguno se parafrasea.

| Campo (`.Rmd`) | Valor literal |
|---|---|
| `exname` | `permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1` |
| `extype` | `schoice` |
| `exsolution` | `` `r paste(sol, collapse = "")` `` (vector binario dinámico, ver §3) |
| `exshuffle` | `TRUE` (las opciones son de texto; la Solution identifica cada una por su contenido y su código de error, nunca por letra — regla #19; ver [BLUEPRINT.md](BLUEPRINT.md) §4.4) |
| `extol` | `0` |
| `exextra[Type]` | `SCHOICE` |
| `exextra[Competencia]` | `Formulacion` |
| `exextra[Componente]` | `Aleatorio` |
| `exextra[Afirmacion]` | `Frente a un problema que involucre informacion cuantitativa, plantea e implementa estrategias que lleven a soluciones adecuadas.` |
| `exextra[Evidencia]` | `Resuelve un problema que involucra informacion cuantitativa o esquematica.` |
| `exextra[Nivel]` | `4` |
| `exextra[DOK]` | `3` |
| `exextra[Bloom]` | `Evaluar` |
| `exextra[SOLO]` | `Relacional` |
| `exextra[TipoMetacognicion]` | `analisis_error` |

**Clasificación oficial ADOPTADA, no re-derivada** (comentario de cabecera del `.Rmd`): estos
campos provienen de la ficha oficial del ítem `MAT-2026-1-004`, en
`/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Alineacion-Curricular-de-items-ERA-2026/Matematicas/Alineacion-curricular-de-items-Matematicas-ERA-2026.md`,
líneas 965-996 — incluye Competencia, Componente, Contenido, Grado sugerido, Afirmación,
Evidencia (`FyE_E3`), Descriptor `D4.8` y el Estándar MEN asociado. No se re-derivaron a partir
del enunciado, a diferencia de cómo se clasifican ítems sin ficha oficial de origen.

**Nota de coherencia DOK↔Nivel (Decisión de diseño D1).** El Nivel 4 es **canónico e intocable**
porque proviene de la ficha oficial adoptada (arriba), pero el cálculo crudo de `n!` —multiplicar
factores decrecientes— es, en sí mismo, **DOK 2** (aplicación de una técnica conocida, Bloom
"Aplicar"). La tabla "Coherencia Nivel ICFES ↔ DOK" de
`../../../../.claude/rules/ejercicios-metacognitivos.md` marca DOK 2 como compatible solo con
Nivel 1-2, e incluso el DOK 3 asignado aquí aparece en esa tabla como compatible estrictamente con
Nivel 3 (Nivel 4 solo figura como compatible con DOK 4). No hay una combinación perfecta
disponible sin contradecir alguno de los dos anclajes (el Nivel oficial de la ficha, o la
aritmética real del cálculo).

**Resolución**, aprobada por el usuario el 2026-07-29: asignar **DOK 3 / Bloom "Evaluar"**, no
por la operación aritmética en sí, sino por la **carga metacognitiva de la Solution** — el
estudiante debe poder evaluar por qué cada una de las tres estrategias erróneas
(`EST-PER-01/02/03`, §3) falla, distinguiendo específicamente cuándo un conteo es *con* o *sin*
reemplazo, y transferir ese criterio al caso de conteo con repetición que se presenta como *Caso
específico* en la Solution (§3.1). Es la solución de compromiso más defendible entre las dos
opciones peores: bajar el Nivel a 2 (contradice la ficha oficial adoptada, rompe OE2) o subir el
DOK a 4/Bloom "Crear" (no hay creación de un método nuevo, solo evaluación de estrategias dadas).
Ver la discusión completa en [`BLUEPRINT.md`](BLUEPRINT.md) §4.2.

## 2. Qué evalúa el ítem

El enunciado original (ítem `MAT-2026-1-004`, reproducido verbatim en el contexto canónico del
`.Rmd`, `contextos[[1]]`): *«En una obra de teatro, hay 4 personas que interpretan pescadores. Al
finalizar la obra, los 4 pescadores deben ubicarse en fila en el escenario y hacer una venia ante
el público. ¿De cuántas formas pueden ubicarse los cuatro pescadores durante la venia final?»*
(opciones oficiales: 64, **24**, 16, 4; clave B).

Este ítem evalúa si el estudiante:

1. **Reconoce que el conteo es sin reemplazo**: cada pescador, una vez ubicado en una posición de
   la fila, deja de estar disponible para las posiciones restantes — el conjunto de opciones
   decrece en cada paso.
2. **Aplica el principio multiplicativo correctamente**, multiplicando el número de opciones
   disponibles en cada una de las `n` posiciones (`n × (n-1) × ... × 1`), no un producto truncado
   ni una potencia.
3. **Distingue "cuántos elementos hay" de "de cuántas formas se ordenan"** — el error más básico
   del pool (`EST-PER-03`) responde simplemente `n`.

Esto corresponde al descriptor oficial **D4.8**: *«Resuelve problemas de conteo que requieren el
uso de permutaciones»* (catálogo `niveles-mat.json`, nivel 4, índice 8), a la evidencia
**`FyE_E3`**: *«Resuelve un problema que involucra información cuantitativa o esquemática»*
(catálogo `evidencias-mat.json`), y al estándar MEN: *«Resuelvo y planteo problemas usando
conceptos básicos de conteo y probabilidad (combinaciones, permutaciones, espacio muestral,
muestreo aleatorio, muestreo con remplazo). (10°-11°, Pensamiento aleatorio)»*.

## 3. Pool de errores conceptuales (distractores diagnósticos)

Los distractores **no son ruido numérico**: cada uno es un error conceptual, y ninguno se calcula
con `sample`/`runif`/`rnorm` (verificado — Capa D de `validar_coherencia_matematica.R`). El pool
`errores_conceptuales` (`.Rmd`, chunk `data_generation`) tiene **cinco** entradas — ampliado de 3 a 5 en la
auditoría adversarial del 2026-07-29 para cumplir la regla #1
(`ejercicios-metacognitivos.md`, «Mínimo 4-6 errores por ejercicio»: el pool original tenía
exactamente 3 para 3 espacios, así que el *tipo* de error nunca variaba entre versiones). Los tres
primeros (`EST-PER-01/02/03`) se toman **literalmente** de las Justificaciones MetaCognitivas de la
ficha oficial `MAT-2026-1-004` (comentario del pool en el `.Rmd`) y generalizados de `n = 4` a
`n ∈ {4,5,6}`; `EST-PER-04/05` son ampliación propia del subproyecto para cumplir el mínimo de la
regla #1, y `EST-PER-06/07` son la ampliación de la **decisión D4** (2026-07-30) que cerró el
hallazgo H1 — ambas mayores que `n!`, para que la clave no sea nunca la opción de mayor magnitud.

| Código | Nombre | `calcula(n)` (`.Rmd`) | Valor en `n=4` | Qué diagnostica |
|---|---|---|---|---|
| (correcta) | Permutación de `n` elementos | `factorial(n)` | 24 | — |
| `EST-PER-01` | Repetición sin descontar los elementos ya ubicados | `n^(n - 1L)` | 64 | Doble supuesto: cuenta solo `n-1` posiciones —como si la última quedara determinada sola— y conserva las `n` opciones en cada una. Aplica el principio multiplicativo **con** reemplazo a una situación **sin** reemplazo. Renombrado el 2026-07-29 (ver nota abajo). |
| `EST-PER-02` | Producto de solo dos posiciones | `n * n` | 16 | Trunca el principio multiplicativo en dos factores y además lo aplica con reemplazo — como si solo hubiera dos posiciones y en cada una se pudiera elegir de nuevo entre los `n` elementos. |
| `EST-PER-03` | Confusión entre cardinal del conjunto y número de arreglos | `n` | 4 | Confunde «cuántas formas hay de ordenar `n` elementos» con «cuántos elementos hay» — un error de interpretación de la pregunta, previo a cualquier cálculo, sin aplicar ningún principio de conteo. |
| `EST-PER-04` | Fórmula de permutación circular aplicada a una fila | `factorial(n - 1L)` | 6 | Aplica la fórmula de permutaciones **circulares**, `(n-1)!`, que descuenta las rotaciones porque un círculo no tiene primer lugar; en una fila sí lo hay, y cada rotación produce un arreglo distinto. |
| `EST-PER-05` | Principio aditivo en lugar de multiplicativo | `n * (n + 1L) / 2L` | 10 | Suma la secuencia de posiciones disponibles en vez de multiplicarla. Identifica correctamente cuántas opciones quedan en cada posición pero combina esos conteos con el principio aditivo en vez del multiplicativo. |
| `EST-PER-06` | Cuenta una posición más de las que hay | `factorial(n + 1L)` | 120 | Aplica **bien** el principio multiplicativo, pero sobre un conjunto que no es el del problema: cuenta `n+1` lugares por llenar cuando el enunciado describe `n` elementos. El método es correcto; el conjunto sobre el que se aplica, no. Error de conteo de las posiciones **antes** de la fórmula. |
| `EST-PER-07` | Duplica el conteo por el orden inverso | `2L * factorial(n)` | 48 | Obtiene bien las `n!` permutaciones y luego las duplica, suponiendo que leer la fila de izquierda a derecha y de derecha a izquierda son dos colocaciones distintas que hay que sumar. Doble conteo por una simetría que el factorial ya incluye: es el error **simétrico** de `EST-PER-04` — aquél divide por rotaciones que no debería descontar, éste multiplica por reflexiones ya contadas. |

**Corrección del 2026-07-29 sobre `EST-PER-01`**: la ficha anterior de este documento describía el
error como «supone `n` opciones en cada posición», lo que implicaría $n^n = 256$ para $n=4$, no
$n^{n-1} = 64$ como calcula `calcula()`. La auditoría adversarial (dos adversarios independientes,
en consenso) señaló la incoherencia. El nombre y la descripción actuales explican el **doble
supuesto** real: contar solo `n-1` posiciones (como si la última quedara determinada sola) y, en
cada una de esas `n-1`, conservar las `n` opciones — fiel al texto oficial de la ficha, que habla
de «tres posiciones» para `n=4` (`n-1 = 3`).

**Selección por versión (regla #1 + Decisiones D3 y D4).** Cada versión muestra 3 de los 7 errores.
La terna se elige **enumerando el espacio legal** —las combinaciones que contienen al menos un
distractor mayor que `n!`, invariante I-7— y sorteando un índice con `safe_sample()`, entre los
aplicables por `precondicion` (todas aplican siempre para este ítem). Nunca con un bucle de
reintento: eso es el Error 22 y cuelga el render (regla #21, Familia 1). La única excepción es la **instancia canónica** (contexto 1 con `n = 4`): ahí se
fuerzan los tres errores oficiales (`EST-PER-01/02/03`, `CODIGOS_OFICIALES` en `.Rmd`)
para que esa versión reproduzca íntegro el ítem `MAT-2026-1-004`, incluidas sus cuatro opciones
oficiales (64, 24, 16, 4). Ver la decisión D3 en [`BLUEPRINT.md`](BLUEPRINT.md) §4.8.

**Plausibilidad para un estudiante de grado 10-11**: los siete errores son transcripciones
razonables de una lectura apresurada del principio multiplicativo o de una fórmula memorizada sin
verificar su condición — no requieren un malentendido exótico. Confundir permutación con variación
con repetición (`EST-PER-01`) es, según la propia ficha oficial del ítem, el distractor más elegido
en la aplicación real; truncar el producto a dos factores (`EST-PER-02`) ocurre cuando se memoriza
"multiplicar" sin contar cuántas posiciones hay que llenar; responder el cardinal (`EST-PER-03`) es
el error de interpretación más elemental de la pregunta; aplicar la fórmula circular (`EST-PER-04`)
ocurre cuando se memoriza `(n-1)!` sin distinguir arreglo circular de arreglo en fila; y sumar en
vez de multiplicar (`EST-PER-05`) es la confusión más básica entre los dos principios de conteo;
contar una posición de más (`EST-PER-06`) es un desliz de lectura del enunciado que deja el método
intacto; y duplicar por el orden inverso (`EST-PER-07`) es la sobrecorrección de quien recuerda que
«en algunos conteos hay que dividir o multiplicar por una simetría» sin verificar si aquí aplica.

**Los siete tienen la misma estructura algebraica** (un valor entero positivo derivado de `n`), y
la unicidad y plausibilidad de magnitud de cada terna {correcta, distractor, distractor,
distractor} se garantiza **por construcción**, no por exclusión de casos: el rango de `n` y la
enumeración exhaustiva de las **105** ternas posibles (3 valores de `n` × C(7,3) = 35 combinaciones)
se verificaron para que las cuatro opciones sean siempre distintas, ninguna sea descartable por
magnitud desproporcionada y **la clave nunca sea la mayor** (I-7). Ver la tabla de medición completa
en [`BLUEPRINT.md`](BLUEPRINT.md) §3 y el barrido que fijó el tamaño del pool en §3.1.

### 3.1 Solution con las 6 subsecciones canónicas + ítem espejo

El chunk `solucion` del `.Rmd` cubre las seis subsecciones que exige la regla #1
(`ejercicios-metacognitivos.md`, «Sección Solution Obligatoria»): *Respuesta correcta*
+ *Análisis de cada opción* (cubre «Análisis del error» —
identifica cada opción por su código `EST-PER-0x`, nunca por letra, regla #19), *Procedimiento
correcto*, *Propiedades del concepto* (cuatro propiedades sobre
permutaciones — orden importa, crecimiento del factorial, diferencia con conteo con repetición),
*Caso específico*, *Reflexión metacognitiva* y *Estrategia para
evitar el error*.

**El *Caso específico* transfiere a un segundo ítem oficial real**, no a un ejemplo inventado: el
ítem espejo `MAT-2026-1-029`, que comparte el mismo descriptor `D4.8` pero pide un conteo **con**
repetición (códigos de 4 cifras con dígitos de `{1,...,5}` que sí pueden repetirse:
`5×5×5×5 = 5⁴ = 625`). La Solution señala que aplicar la potencia al problema de la fila de
pescadores es precisamente el error `EST-PER-01`, pero **solo cuando ese error está entre los tres
seleccionados en la versión actual** (subsección *Caso específico*) — desde que el pool creció a cinco
entradas, `EST-PER-01` ya no está garantizado en cada versión, salvo en la instancia canónica,
donde sí está siempre presente por ser uno de los tres errores oficiales (Decisión D3,
[`BLUEPRINT.md`](BLUEPRINT.md) §4.8). El eje diagnóstico del pool sigue siendo la distinción
con/sin reemplazo que separa a los dos ítems oficiales.

## 4. Prerrequisitos del estudiante

- Principio multiplicativo de conteo (si un evento tiene `a` resultados y otro `b`, ambos juntos
  tienen `a × b`).
- Noción de que, al ordenar elementos **sin reemplazo**, un elemento ya ubicado deja de estar
  disponible para las posiciones restantes.
- Notación y cálculo del factorial (`n!`).
- Distinción entre «cuántos elementos hay» (el cardinal de un conjunto) y «de cuántas formas se
  pueden organizar» (el número de arreglos) — la pregunta que el error `EST-PER-03` responde mal.
- No se requiere la fórmula general de variaciones con repetición (`n^k`) como prerrequisito: el
  ítem la introduce como contraste en el *Caso específico* de la Solution, no en el enunciado.

## 5. Referencias cruzadas

- [`../HANDOFF.md`](../HANDOFF.md) — estado de trabajo, decisiones, hallazgos abiertos (pendiente)
- [`../README.md`](../README.md) — cómo verificar y renderizar el ejercicio
- [`BLUEPRINT.md`](BLUEPRINT.md) — arquitectura técnica del pool `errores_conceptuales` y del
  pipeline de mezcla de opciones
- [`BACKLOG.md`](BACKLOG.md) — pendientes priorizados
- [`ROADMAP.md`](ROADMAP.md) — hitos y objetivos específicos (OE1-OE11)
- `../../../../.claude/rules/ejercicios-metacognitivos.md` — Progressive Disclosure, pool de
  errores, coherencia DOK↔Nivel
- `../../../../.claude/rules/solution-letter-independence.md` — regla #19, por qué la Solution
  identifica opciones por código de error y no por letra
- `../../../../.claude/rules/familias-soluciones-rmd.md` — Familia 1 (`pick_int`), Familia 5
  (`safe_sample`)
- `../../../../.claude/rules/diversidad-sustantiva.md` — regla #22, por qué `n` está aleatorizado
  y no es un literal fijo, y por qué solo hay 3 respuestas correctas distintas (ver
  [`BACKLOG.md`](BACKLOG.md) P1.2)
- `../../../../.claude/rules/contextos-narrativos-creativos.md` — regla #11, pool de 6 plantillas
  narrativas
- [`../.claude/rules/permutaciones-parametricas.md`](../.claude/rules/permutaciones-parametricas.md)
  — contrato local: la clave `n!`, el pool de siete errores conceptuales y las invariantes I-1..I-7

---

**Versión**: 2.0 (pool 5 → 7: `EST-PER-06` y `EST-PER-07` de la decisión D4)
**Fecha**: 2026-07-30
