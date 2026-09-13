# Invariantes locales — `teorema-coseno-datos-suficientes-n3`

> Regla #17 / Incidente `INC-CLAUDE-LOCAL`. Estas invariantes **prevalecen sobre el criterio
> genérico** dentro de este subproyecto. Si alguna contradice una regla del repo raíz, gana la
> del raíz y el conflicto **se reporta**, no se resuelve en silencio.

Origen: pregunta impresa **No. 50** del cuadernillo ERA-2026 Matemáticas (`pagina_015.jpg`,
pie impreso 20). Anclado por el **número impreso**, no por el mapeo: `mapa-paginas.md` y el OCR
inducían al ítem equivocado (la tabla de límites de velocidad, que es la Q49).

## I-1 · La fórmula usa las LETRAS DE LA FIGURA, para un lado DISTINTO del pedido

> **REESCRITA el 2026-09-13. Decisión del profesor**, comunicada a través del invocador del
> orquestador. Cambia de sentido respecto de la versión anterior, que ordenaba **no** armonizar
> la notación. Regla #24 **H-5** (relajar nunca es autónomo): queda por escrito quién lo decidió,
> cuándo, y qué variante se descartó y por qué.

### Lo que rige ahora

La fórmula del enunciado se escribe con las **letras de la figura de esa versión**, pero
**planteada para un lado distinto del que se pregunta**, sorteado entre los otros dos. Si se pide
`q`, el enunciado muestra por ejemplo `s² = q² + r² − 2 · q · r · cos S`.

Consecuencias operativas, todas con guardia mecánica en `data_generation`:

| Exigencia | Guardia |
|---|---|
| La fórmula NUNCA se plantea para el lado pedido | `stopifnot(!identical(lado_formula, x))` y `stopifnot(!identical(W_formula, X))` |
| El lado de la fórmula se **sortea**, no se fija | `lado_formula <- safe_sample(otros, 1L)` |
| El **orden de los dos sumandos** también se sortea | `pf <- safe_sample(setdiff(LADOS_T, lado_formula), 2L)` |
| La canónica conserva la fórmula genérica impresa | `stopifnot(identical(formula_enunciado, "a^2 = ..."))` |

**Por qué el lado se sortea y no se fija.** Un lado fijo (siempre el «siguiente» de la familia)
sería un patrón aprendible y, con él, un canal nuevo. Lo mismo vale para el orden de los sumandos:
sin sortearlo, el primero sería **siempre** el lado pedido — rasgo constante y por tanto explotable.

**Por qué hay que DECIRLO en el enunciado.** La rama general añade «Con los nombres del triángulo
de la figura, aplicado al lado **w**, la relación se escribe así:». Sin ese puente el ítem sería
engañoso: el estudiante podría creer que la fórmula dada es la que debe usar tal cual. La tarea es
trasladar, no adivinar que hay que trasladar.

### La variante que se DESCARTÓ, y por qué

Se le advirtió al profesor que hacer coincidir las letras **para el lado que se pregunta**
—`q² = r² + s² − 2 · r · s · cos Q` cuando se pide `q`— **disolvería el ítem**: la fórmula
nombraría exactamente los tres datos de la clave (`r`, `s`, `Q`) y bastaría emparejar símbolos
entre enunciado y opciones, sin geometría alguna. Sería DOK 1. **El profesor eligió la vía
intermedia** que aquí se describe. La guardia `stopifnot` de arriba impide que alguien reintroduzca
la variante literal «arreglando» el ítem dentro de seis meses.

### Qué se conserva de la versión anterior

La **exclusión de A, B y C** de las familias de letras (I-11) sigue vigente, y ahora importa más,
no menos: si la figura usara `a, b, c` y la fórmula también, volveríamos al caso literal por la
puerta de atrás.

La **instancia canónica NO cambia**: conserva `a² = b² + c² − 2 · b · c · cos α`, porque reproduce
el cuadernillo **verbatim** y esa fidelidad sostiene el gate H-3 (regla #24, H-2/H-5). Decisión
tomada por el invocador y declarada aquí.

### Coste medido del cambio: ver I-16

El cambio convierte la fórmula en un **estímulo con letras**, y por tanto en una fuente de canal
que antes no existía. Las cifras están en **I-16**, incluido un canal nuevo que **supera el corte
de +8 pp** y que se declara sin rebaja.

## I-2 · Inventario de rótulos cerrado: exactamente 6, `{Q, R, S, q, r, s}`

Ni uno más, en **ninguna** versión. **PROHIBIDO** añadir cotas, medidas, valores numéricos,
grados, flechas, marcas de ángulo recto, cuadrícula, ejes, título o leyenda. La figura original
**no tiene ni una sola cifra**; añadir una cambiaría el ítem.
Gate H-3, rama geometría rotulada: el inventario se verifica **bidireccionalmente** (lo que falta
y lo que sobra). El incidente `q090` fue una etiqueta **agregada** que un checklist de forma no vio.

## I-3 · Forma del original, medida (no estimada a ojo)

Vértices sobre el crop de 950×680: **Q (434, 100) · R (98, 510) · S (919, 412)**.
Ángulos **Q 96,6° (obtuso) · R 43,9° · S 39,6°**. Lados **q : r : s = 1 : 0,697 : 0,641**.
Base inclinada **+6,8°** (sube hacia la derecha).
**El vértice superior está a la IZQUIERDA** del punto medio de la base, por el 9,1 % del ancho.
Un briefing previo decía «a la derecha»: era falso y habría producido una figura espejada.

## I-4 · `lado_pedido` NO puede alterar el dibujo

`dibujar_triangulo_rotulado()` recibe `lado_pedido` y lo **ignora deliberadamente**
(`invisible(lado_pedido)`). Resaltar, engrosar o recolorear el lado buscado **delataría la
respuesta**. Si alguien «mejora» la función haciéndolo visible, el ítem queda roto.

## I-5 · RAMA ÚNICA: coseno pide lado. La alternancia de clave queda CERRADA.

**Estado desde 2026-09-13.** Este ítem tiene **una sola rama**: el enunciado presenta el
teorema del Coseno y se pide **un lado**. La clave es siempre «Lados *y* y *z* y ángulo *X*»
(los dos lados que concurren en el vértice buscado más el ángulo comprendido).

Las versiones anteriores de esta invariante prescribían **alternar la magnitud pedida**
(rama lado / rama ángulo) para que la primera palabra de la clave variase y `H3` no disparase.
Esa prescripción **se retira**: la enumeración del espacio demostró que la rama ángulo y
cualquier rama alternativa introducen un canal de formato del 100 %. Ver **I-10**.

**PROHIBIDO** reintroducir una segunda rama sin repetir antes la enumeración de
`salida/enumeracion/RESULTADO-enumeracion-4-ramas.txt` y demostrar que la rama candidata
tiene **clave única** *y* **al menos un distractor de su mismo molde**. Las cuatro
combinaciones posibles ya están enumeradas y sólo una las cumple.

**PROHIBIDA** también la «solución» por reordenamiento de la frase de la clave: haría pasar la
sonda sin cambiar la propiedad que la sonda mide. Es diversidad cosmética (regla #22).

## I-6 · La guardia anti-segunda-clave es una PRECONDICIÓN, no un texto corregido

`data_generation` lleva `resuelve_un_coseno()` — el criterio de I-7 hecho código — y estas
guardias, que abortan el render antes de producir un ítem defectuoso:

| Guardia | Qué impide |
|---|---|
| `resuelve_un_coseno(clave...)` | que la clave deje de resolver |
| `!any(vapply(distractores, resuelve_un_coseno, ...))` | **segunda clave** |
| `!any(x %in% d$lados)` | que una opción contenga el propio lado pedido |
| `all(n_datos == 3L)` | **canal de recuento**: las 4 opciones con 3 datos, como el cuadernillo |
| `n_molde_clave >= 3L` (2 en la canónica) | que la clave sea la única de su molde |

Esto sustituye a la antigua lista de exclusiones por enumeración manual, que era la que dejó
pasar `GEO-COS-08` (caso LAL en la rama ángulo): una precondición verificada vale más que una
corrección de texto medida después. Al eliminarse la rama ángulo, `pool_angulo` desaparece y
con él ese defecto **por construcción**, no por reescritura.

Cobertura: `verificar_espacio.R` enumera el espacio completo (10 subconjuntos → 1 clave) y
repite las cinco comprobaciones sobre N = 100 versiones, con 4 pruebas de mutación que
**declaran su sonda esperada** (Incidente P).

## I-7 · La Solution NO puede decir que la opción del ángulo adyacente sea «imposible»

Con dos lados y un ángulo **adyacente** (no comprendido) el teorema **sí** se plantea una vez:
queda una **ecuación de segundo grado** que admite **hasta dos triángulos** (caso lado-lado-ángulo).
Enumerado: `r=5, s=7, R=40°` → q ∈ {3,182 · 7,543}. Decir «imposible» es **matemáticamente falso**
y es un defecto de CORRECCIÓN (binario, bloqueante).
Igualmente: las opciones de un lado con dos ángulos **sí** son resolubles — por **ley de senos**.
El discriminador del ítem es «una única aplicación del **teorema del coseno**», no «resoluble».

### ⚠️ La cláusula que faltaba (añadida 2026-09-13 tras la objeción 1 del detractor)

El criterio completo es: **una sustitución entrega lo pedido con valor único A PARTIR DE LOS
DATOS ENUNCIADOS, sin medir ni comparar magnitudes sobre la figura.**

Sin esa cláusula el criterio es ambiguo y el veredicto del ítem se invierte. Medido: sobre el
triángulo que realmente se dibuja, `GEO-COS-01` **entrega valor único en el 90 % de las
versiones y en el 100 % de las canónicas** (en la canónica `r > s`, así que la cuadrática deja
una sola raíz positiva). Leído al pie de la letra sin la cláusula, ese distractor sería una
**segunda clave en 400/400** versiones.

La lectura correcta —y la que sanciona el ICFES— es que las opciones dan **símbolos, no
magnitudes**, y que una figura sin acotar no se mide (I-2). Con `r`, `s` y `R` como datos, la
sustitución deja `x = z·cos Y ± √(y² − z²sin²Y)`: dos ramas, y **nada en los datos permite
descartar una**. La ecuación de segundo grado no garantiza solución única — según los valores
puede haber dos triángulos, uno o ninguno (medido: 1 167 / 13 301 / 5 532 sobre 20 000 casos).

**Consecuencia para la Solution:** la prosa debe dar la razón **incondicional** («la ecuación no
garantiza solución única») y NUNCA la condicional («no entrega un valor único»), que es falsa en
la instancia canónica. `resuelve_un_coseno()` es la implementación fiel de este criterio.

## I-8 · La regla #11 (contextos narrativos creativos) NO aplica aquí

El ítem oficial es **formal y abstracto** («En un triángulo cualquiera…»), sin protagonista ni
situación. H-2 obliga a reproducirlo. Inventarle un contexto narrativo cambiaría el ítem.
Se declara no aplicable **explícitamente**, en vez de forzarla o de callarlo.

## I-9 · La figura va en el ENUNCIADO, no en las opciones

Por tanto `exshuffle: TRUE` y **`graficos-como-opciones.md` no aplica** (ni un PNG por opción,
ni formato equilibrado, ni fuga por nombre de archivo en Moodle). El PNG se emite **siempre con
`{width=...}`** (regla #18): sin él, el tamaño lo dicta la plantilla, no el ejercicio.

## I-10 · ⚖️ OVERRIDE FIRMADO — `H3 = 100 %` se acepta EN ROJO

> **Autorizado por el profesor el 2026-09-13**, tras ver la enumeración de las cuatro ramas.
> Regla #24 **H-5**: endurecer es autónomo, **relajar nunca** — por eso queda por escrito, con
> la cifra, y no como criterio implícito de un agente.

**La cifra.** `Rscript .claude/scripts/validar_diagnosticidad.R <rmd>` (N = 100, regla #23)
reporta `ERR_DIAG_SUPERFICIAL`, **exit 1**, por la sonda **H3**: *«la correcta empieza por
"lados" en el 100 % de las versiones»*.

**La razón, medida y no argumentada.** De las cuatro combinaciones *teorema × magnitud pedida*,
**sólo una** admite clave única **y** al menos un distractor de su mismo molde
(`salida/enumeracion/RESULTADO-enumeracion-4-ramas.txt`):

| Rama | Claves | Distractores | Del mismo molde | Viable |
|---|---:|---:|---:|:--:|
| **Coseno pide LADO** (el ítem oficial) | **1** | 9 | **2** | ✅ |
| Coseno pide ÁNGULO | 1 | 9 | 0 | ❌ |
| Seno pide LADO | **6** | 4 | 0 | ❌ |
| Seno pide ÁNGULO | 2 | 8 | 4 | ❌ |

La rama Seno tiene **seis claves**: con dos ángulos el tercero sale por suma 180°, así que los
seis conjuntos «un lado + dos ángulos» entregan el lado pedido con una sola aplicación
(contra-chequeado sobre 5.000 triángulos aleatorios, error relativo máximo 1,1·10⁻¹⁵, con
control negativo). Y ninguna rama distinta de la primera tiene un distractor de su molde, luego
su clave sería la única de su tipo en el 100 % de las versiones.

**Conclusión:** no existe segunda rama. Luego la primera palabra de la clave **no puede variar**
sin reordenar la frase, que I-5 prohíbe con razón. `H3 = 100 %` es **estructural: la clave ES el
teorema**, no un defecto de implementación.

**Contexto comparativo (§P7-A, la vara es el examen real, no el cero).**

| | Exceso §P7 | Veredicto |
|---|---:|---|
| Corpus oficial ICFES (426 ítems) | +4,6 pp | zona gris |
| Control oficial (399 ítems) | +5,3 pp | zona gris |
| **El propio ítem oficial de esta plana** (canal de molde) | **+18,6 pp** | BLOQUEA |
| **Este ejercicio, batería de 19 reglas CON la sonda D1** | **+18,8 pp** (máx 50,0 % · techo nulo 31,2 % · sd 2,7 pp) | **BLOQUEA** |
| *(histórico, batería de 17 reglas CIEGA a D1)* | *+4,4 pp* | *zona gris — cifra retirada* |

El ítem impreso del cuadernillo **tiene un canal de molde más fuerte que el nuestro**: lleva sólo
un distractor del molde de la clave, y nosotros llevamos dos.

> **⚠️ Corrección de la cifra (2026-09-13, D3).** Las lecturas de **+4,2 / +4,4 / +4,5 / +4,6 pp**
> que esta tabla arrastraba se obtuvieron con una batería que **no tenía sonda para el canal léxico
> intra-opción** — justamente el canal que el override **I-12** acepta en rojo al 50,0 %. Una
> batería sin esa sonda no medía «sin señal», medía «sin sonda» (regla #22 §P7). Con la sonda **D1**
> incorporada a `salida/enumeracion/bateria_auditada.R`, el máximo de la batería es el propio D1:
> **50,0 %**, techo nulo **31,2 %**, **exceso +18,8 pp**, veredicto **BLOQUEA**.
>
> **No es batería rellenada**: añadir D1 y su inversa movió el techo nulo sólo **0,1 pp**
> (31,1 → 31,2 %) y subió el máximo 14,5 pp. El `exceso_atómico` es **+25,0 pp**.
>
> **El residuo sigue amparado** por §P7-A: el ítem oficial impreso mide **el mismo 50,0 %** en esa
> regla (enumeración exacta sobre sus cuatro opciones), luego el exceso sobre la vara real es de
> **0 pp**. Lo que cambia es que la invariante ahora dice **la cifra medida**, no una obtenida con
> una batería ciega. **La batería se queda en rojo.**

### ⛔ Lo que este override NO autoriza

**PROHIBIDO** tocar `validar_diagnosticidad.R`, su umbral, la sonda H3 o la forma del texto de
las opciones para que el gate reporte `PASS`. **La batería se queda en rojo.** El día que
alguien la haga dar verde para poder cerrar, el gate queda apagado para siempre y el siguiente
ítem con una clave de verdad invariante pasará sin que nadie lo note.

### Segundo canal estructural: R1, y por qué la batería §P7 no puede verlo

**La batería §P7 sólo recibe las OPCIONES, nunca el enunciado.** Es ciega, por construcción, a
cualquier regla que relacione el estímulo con las opciones. El `+4,6 pp` que respalda este
override **no cubre** ese espacio. Medido por el detractor (2026-09-13):

| Regla estímulo→opción (azar 25,0 %) | Global | Canónicas | Resto |
|---|---:|---:|---:|
| **R1** «la que cita el ángulo cuya letra es la del lado pedido» | **100,0 %** | **100,0 %** | **100,0 %** |
| R2 «la que menciona la mayúscula del lado pedido» | 59,2 % | 33,3 % | 63,4 % |
| R3 (control) «la que cita dos lados» | 35,7 % | 50,0 % | 33,3 % |

**R1 acierta el 100 % sin geometría**: se pide `m`, se elige «ángulo `M`». Pero **no lo introduce
el ejercicio**: medido sobre el **ítem oficial impreso**, R1 también da **100 %**. Es consecuencia
directa de la convención `lado m ↔ ángulo M`, que es justamente sobre la que **I-1** monta la
tarea de transferencia. Exceso sobre la vara oficial: **0 pp**. Por §P7-A no obliga a nada, y no
puede cerrarse sin destruir el ítem.

> **⚠️ Las cifras de esta tabla NO son comparables con las del helper (2026-09-13).** Están medidas
> con puntuación **0/1** («acierta si la clave sobrevive al filtro»), no con la convención canónica
> de `bateria_eliminacion.R` (`score = 1/|S|`, cuyo nulo es exacto). Bajo 0/1, R1 da 100 % tanto
> aquí como en el impreso, porque la clave **siempre** cita el ángulo homónimo. Bajo la convención
> canónica, R1 mide **47,8 %** aquí y **33,3 %** en el impreso → **+14,5 pp**, no 0 pp: nuestras
> opciones dejan **2** candidatas donde el impreso deja **3**. Se declaran **las dos lecturas**
> porque divergen, y elegir la favorable sería exactamente lo que la regla de cierre prohíbe.

Se declara aquí, y no se deja implícito, por la lección de H3b: **declarar la ceguera vale más
que fingir cobertura**. Un auditor futuro lo redescubrirá; que lo encuentre ya escrito evita que
lo trate como hallazgo nuevo o que intente «cerrarlo» rompiendo la notación.

El `exit 1` de la diagnosticidad es, en este ejercicio, **resultado esperado y documentado**.
Cualquier otro código de error de ese mismo script (H1, H2, H3b) **sí es bloqueante** y no está
cubierto por este override.

## I-11 · Las familias de letras EXCLUYEN A, B y C (operativa de I-1)

La fórmula de la instancia **canónica** es `a² = b² + c² − 2bc·cos α`. Una familia de vértices
`{A, B, C}` daría lados `a, b, c` **idénticos** a los de esa fórmula y disolvería la tarea de
transferencia, que es la demanda cognitiva del ítem según **I-1**. Estaba en el pool y se
retiró el 2026-09-13.

**Tras la reescritura de I-1 esta exclusión importa MÁS, no menos.** Ahora la fórmula de la rama
general lleva las letras de la figura; si la figura usara `a, b, c`, coincidiría además con la
notación de la fórmula genérica del impreso y volveríamos al caso literal —el que el profesor
descartó— por la puerta de atrás.

Se excluyen además **I, L y O** por ambigüedad de glifo con `1` y `0` en la figura.

Hay **12 familias × 3 lados pedidos = 36 textos de clave distintos**, por encima del umbral 30
de `validar_diversidad_sustantiva.R`. Con las 5 familias anteriores eran 15 y el script
reportaba `WARN_DIV_BAJA`. Un `stopifnot` impide reintroducir cualquiera de las seis letras
prohibidas.


## I-12 · ⚖️ OVERRIDE FIRMADO — el canal léxico intra-opción se acepta EN ROJO al 50,0 %

> **Autorizado por el profesor el 2026-09-13**, al elegir `[r]` en WAIT_USER #3 con la cifra a la
> vista. La opción elegida decía literalmente que implicaba *«firmar la invariante I-12 con la
> batería EN ROJO»*. Regla #24 **H-5**: endurecer es autónomo, **relajar nunca** — por eso queda
> por escrito, con la cifra, y no como criterio implícito de un agente.

**El canal.** La regla *«elige la opción cuyos tres rótulos son letras distintas»* — es decir, la
que **no repite letra** entre los lados que cita y su ángulo. Es *option-internal*: no necesita el
enunciado, ni saber qué lado se pide, ni geometría alguna.

| Opción | Rótulos | ¿Repite? |
|---|---|---|
| `Lados r y s y ángulo Q` ← **clave** | r, s, Q | **no** |
| `Lados r y s y ángulo R` (`GEO-COS-01`, forzado) | r, s, R | sí (R↔r) |
| `Lados r y s y ángulo S` (`GEO-COS-02`, forzado) | r, s, S | sí (S↔s) |
| tercer distractor | — | según cuál salga |

**Cómo apareció: cerrando otro canal.** Forzar `GEO-COS-01` + `-02` bajó *«la que cita más lados»*
de 62,3 % a 35,5 %, pero **ambos repiten letra**, así que la clave quedó siendo la única
no-repetidora. Es el **ciclo de desplazamiento de canal** de la regla #22 §P7: se optimizó lo que
la batería medía y se empeoró lo que no miraba. Ninguna de las 17 reglas de la batería mira
coincidencias de letra **dentro** de una misma opción.

**Las cifras, medidas por dos vías independientes** (detractor y orquestador, N = 200 × 2 semillas):

| | Antes de I-12 | **Tras I-12** | Ítem oficial impreso |
|---|---:|---:|---:|
| Canal «no repite letra» | 73,0 % / 73,8 % (detractor: 74,0 / 75,5) | **50,0 % / 50,0 %** | **50,0 %** (enumeración exacta) |
| «la que cita más lados» | 35,9 % / 35,6 % | **35,9 % / 35,6 %** (sin cambio) | 50,0 % |
| Abortos de guarda | 0 | **0** | — |

**El fix.** El tercer hueco se restringe a los tres errores que NO repiten letra:

```r
sel <- c(1L, 2L, sample(c(4L, 6L, 9L), 1L))   # GEO-COS-04, -06, -09
```

**Por qué 50,0 % es el SUELO, no una elección.** La clave es no-repetidora **por definición**: su
ángulo es el opuesto al lado pedido, que no se cita. Y los otros tres no-repetidores del espacio
son todos de molde distinto, así que meterlos reabriría *«la que cita más lados»*. Con este
universo de 10 opciones no se baja de 50 %. Enumerado, no argumentado.

**Lo que el override acepta.** 50,0 % sobre un techo nulo de ≈31 % son **≈ +19 pp**, por encima del
corte de canal de +8 pp: la batería **sigue en rojo**. Lo que cambia es la **naturaleza** del
residuo — de *«canal nuestro, +23 pp sobre el examen real»* a *«canal estructural, exceso **0 pp**
sobre el ítem oficial impreso»*, que es exactamente la condición bajo la que I-10 ya ampara a H3 y
a R1.

**Descartado por medición, no por criterio.** Imitar el reparto del cuadernillo (forzar un solo
distractor del molde) **aborta el render en 169 de 200 versiones**: la guardia `n_molde_clave >= 3L`
lo rechaza. No es una alternativa disponible.

### Coste declarado

`GEO-COS-05`, `-07` y `-08` **no aparecerán nunca**. El pool efectivo alcanzable baja de **9 a 6**
entradas (1, 2, 4, 6, 9 en la rama general, más 3 en la canónica): sigue dentro del mínimo 4-6 de la
regla #1, pero **en su borde**, y roza el Incidente `INC-POOL-TAMANO`. Medido sobre 300 versiones:
`-01` 100,0 % · `-02` 83,7 % · `-06` 46,7 % · `-04` 30,3 % · `-09` 23,0 % · `-03` 16,3 % ·
`-05`/`-07`/`-08` **0,0 %**. Ver **I-15 §P1** y **§P2**.

### Verificación de que el fix no rompió la clave

Tras aplicarlo (regla #22 §P7-D: *la pasada que más mejoró la diagnosticidad fue la que volvió falsa
la clave*): **0** versiones donde la clave no resuelva, **0** segundas claves, y error relativo
máximo de la ley del coseno **6,4 × 10⁻¹⁶** sobre el triángulo efectivamente dibujado.

### ⛔ Lo que este override NO autoriza

**PROHIBIDO** tocar `bateria_eliminacion.R`, sus cortes, la sonda del canal léxico o el texto de las
opciones para que el gate dé verde. **El override es aceptar el rojo, no pintarlo.**

## I-13 · Fidelidad tipográfica al impreso (no confundir con I-1)

El cuadernillo imprime la fórmula con **puntos de multiplicación** (`a² = b² + c² − 2 · b · c · cos α`)
y los **rótulos de las opciones en negrita** (`Lado **r** y ángulos **R** y **Q**.`). Ambos se
reproducen desde 2026-09-13.

**Esto NO contradice I-1**: lo que I-1 gobierna es **qué letras** lleva la fórmula y **para qué
lado** se plantea; esta invariante gobierna su **composición tipográfica** (puntos de
multiplicación, rótulos en negrita). Son dimensiones distintas y ambas se copian del impreso.

> ⚠️ **Corregido el 2026-09-13.** Esta invariante decía que «I-1 prohíbe armonizar las letras de la
> fórmula genérica con las de la figura». **Eso dejó de ser cierto** cuando I-1 se reescribió: hoy
> I-1 **ordena** usar las letras de la figura, para un lado distinto del pedido. La frase antigua
> habría llevado a un lector futuro a revertir el cambio del profesor creyendo que respetaba una
> invariante.

El realce en negrita se aplica **sólo al emitir** el Answerlist (`realzar_rotulos()`): el vector
`opciones` se deja sin markup a propósito, para que guardias, `stopifnot` canónicos y validadores
sigan midiendo el mismo texto y sus cifras sigan siendo comparables entre ciclos. El realce es
uniforme en las cuatro opciones y en los tres moldes, así que **no introduce canal**. La conjunción
«y» queda fuera del realce, y un `stopifnot` garantiza que ninguna familia de I-11 use la letra Y.

## I-14 · Umbral de producto 250/300 — MEDIDO

La regla #3 exige **250+ versiones únicas sobre 300**. Medido el 2026-09-13 sobre el diseño vigente:

| Firma usada | Únicas / 300 (antes de la pasada 3) | **Únicas / 300 (vigente)** |
|---|---:|---:|
| Completa (pregunta + opciones + clave + reflexión + geometría) | 281 / 300 | **295 / 300** ✅ |
| Sólo texto (pregunta + opciones + clave) | 266 / 300 | **268 / 300** ✅ |

Re-medido el 2026-09-13 tras normalizar el orden de los ángulos (I-15 §D2): el fix **no degradó** la
diversidad, la subió. El campo `versiones_unicas` de `ejercicio_state.json` debe llevar **295**.

**No confundir con el `33`** que `validar_diversidad_sustantiva.R` reporta: ésa es la *diversidad
sustantiva* (número de textos de clave distintos sobre 100 versiones, umbral 30), otra magnitud.
El campo `versiones_unicas` de `ejercicio_state.json` corresponde al 250/300.

---

## I-15 · Residuos DECLARADOS al cerrar la pasada 3 de 3 (§P7-D)

> **Presupuesto agotado el 2026-09-13.** Tres pasadas de corrección de diagnosticidad:
> (1) forzar `GEO-COS-01/-02`, (2) restringir el tercer hueco a los no-repetidores (I-12),
> (3) normalizar el orden de los ángulos en `texto_conjunto()`. La regla #22 §P7-D **prohíbe una
> cuarta**. Lo que sigue se declara con su cifra y **NO se persigue**. La decisión de aceptarlo,
> pedir otra vuelta o abandonar el ítem es **del profesor**, no de un agente.

### §D2 — canal de ORDEN: cerrado, pero **desplazado a su inverso**

La pasada 3 puso el ángulo homónimo del lado pedido **al final**, como el cuadernillo. Medido
sobre N = 100 × 2 semillas, convención canónica `score = 1/|S|`:

| Regla (estímulo → opción) | ANTES | **DESPUÉS** | Ítem impreso | Exceso s/ impreso |
|---|---:|---:|---:|---:|
| **D2** «termina en el ángulo homónimo del lado pedido» | 91,3 % | **47,8 %** | 33,3 % | **+14,5 pp** |
| **E3** «**empieza** por el ángulo homónimo» (inverso de D2) | 56,5 % | **100,0 %** | **100,0 %** | **0 pp** |
| R1/E1 «cita el ángulo homónimo» | 47,8 % | 47,8 % | 33,3 % | +14,5 pp |
| D1 «no repite letra entre sus rótulos» | 50,0 % | 50,0 % | 50,0 % | **0 pp** |

**Lo que hay que leer, y no maquillar:** el fix **no eliminó** el canal de orden, lo **movió del
final al principio de la frase**. Es el ciclo de desplazamiento de canal que la regla #22 §P7
describe.

> **⚠️ CORRECCIÓN (objeción 6 del detractor, verificada por medición propia).** Esta invariante
> decía que «bajo el criterio del máximo explotable, el ítem pasa de 91,3 % a 100,0 %». **Es falso
> y se atribuía a la pasada 3 un empeoramiento que no ocurrió.** La regla
> **E4 «cita UN SOLO ángulo y ese ángulo es el homónimo del lado pedido»** mide **100,0 % antes y
> 100,0 % después** —depende sólo del *conjunto* de ángulos, que el reordenamiento no toca—, así
> que el máximo explotable **ya era 100 % antes**. En balance, la pasada 3 bajó D2 de 91,3 % a
> 47,8 % **sin subir el máximo**.
>
> **E4 no es una fuga**: es el criterio matemático del ítem enunciado como regla de selección
> («hacen falta los otros dos lados y el ángulo opuesto al buscado»). Quien la aplica **ha
> razonado**. Por §P7 —*las reglas de una batería son estrategias que un estudiante podría
> descubrir, no relleno*— contar como canal una regla que codifica el conocimiento objetivo haría
> fracasar a todo ítem bien formado. El que sí merece escrutinio es **E3**, que es puramente
> tipográfico y no exige geometría alguna.

**Por qué aun así se acepta, y bajo qué criterio exacto:**

1. El **ítem oficial impreso mide 100,0 % en esa misma regla** (enumeración exacta sobre sus cuatro
   opciones: sólo la clave tiene `Q` como primer rótulo mayúsculo; las otras tres empiezan por `R`).
   Exceso sobre la vara real: **0 pp**. §P7-A: *un ítem que no filtra más que el examen real no se
   declara defectuoso.*
2. Antes del fix el canal fuerte era **nuestro** (+58,0 pp sobre el impreso). Después, el canal
   fuerte es **del ítem oficial** (+0 pp). Cambia la **naturaleza** del residuo, igual que en I-12.
3. E3 es una manifestación de la convención `lado m ↔ ángulo M` sobre la que **I-1** monta la tarea:
   cerrarla exige romper la notación, y eso destruye el ítem (misma conclusión que R1 en I-10).
4. **Imposibilidad medida, no conveniencia:** con este universo de 10 opciones el 100 % **se puede
   mover de sitio pero no eliminar**. Diluir E3 exigiría un distractor cuyo primer rótulo mayúsculo
   fuese `X`, y los únicos candidatos son los que citan `X` entre dos ángulos: ponerlo primero es
   exactamente el orden pre-pasada-3, que **reabre D2 al 100 %** y además se aparta de la tipografía
   del impreso (H-2 / I-13).

> **⚠️ El «0 pp» NO es el argumento que sostiene esto — y conviene no apoyarse en él.** El ítem
> impreso es **una sola instancia de 4 opciones**: *cualquier* regla que aísle su clave mide 100 %
> ahí. **Control que lo demuestra:** la regla **P4 «elige la última»** mide **100,0 % en el
> impreso** (su clave es la opción D) y sólo **23,0 %** aquí. Nadie sostendría que el cuadernillo
> tiene un canal posicional del 100 %. Es el sesgo de selección que §P7 combate con el techo nulo,
> trasladado a una población de tamaño **uno**. Lo que sostiene la absolución de E3 son los puntos
> **2 y 4** (argumento estructural), no la cifra. Una vara de n = 1 **no absuelve por sí sola**.

**Residuo no amparado, declarado sin rebaja:** **D2 (+14,5 pp)** y **R1/E1 (+14,5 pp)** quedan por
encima del corte de +8 pp frente al impreso. Su causa es que nuestras opciones dejan **2** candidatas
donde el impreso deja **3** (el impreso usa dos distractores de molde `1L+2A` que citan el ángulo
homónimo; I-12 fuerza en su lugar `GEO-COS-01/-02`, que no lo citan). **Igualarlo sería la cuarta
pasada, y está prohibida.** Decisión del profesor.

### §G7 — canal RELACIONAL vivo: la familia §P7-E estaba cubierta sólo nominalmente

La batería declaraba una regla relacional, `del par con el mismo molde`, con **aplicabilidad 0,0 %**:
no disparaba nunca. §P7-E quedaba cubierta **de nombre**, que es peor que no cubrirla — produce
cobertura aparente, el mismo modo de fallo que la ceguera de H2/H3. Objeción 2 del detractor.

**Corregido el 2026-09-13**: sustituida por tres reglas relacionales que **sí** aplican. Batería de
**21 reglas**: máximo 50,0 % (D1) · techo nulo **31,4 %** · **exceso +18,6 pp** · **BLOQUEA**.
Añadirlas movió el techo **0,2 pp**: no es batería rellenada.

| Regla relacional | Aquí | Aplicable | Ítem impreso | Exceso |
|---|---:|---:|---:|---:|
| **G7 «la más parecida a las demás» (Jaccard de rótulos)** | **42,2 %** | 87,0 % | **sin señal** (las 4 opciones empatan en 0,5) | **≈ +17,2 pp** |
| G1 «del grupo mayoritario por lados citados» | 35,5 % | 100,0 % | 50,0 % | −14,5 pp |
| G3 «del grupo mayoritario por molde» | 29,0 % | 100,0 % | 25,0 % | +4,0 pp |

**G7 es un residuo NO amparado por la vara** (> +8 pp sobre el impreso) y **no se persigue**:
§P7-D prohíbe la cuarta pasada. Atenuantes declarados, sin inflar ni rebajar: (a) el Jaccard medio
no es mentalmente ejecutable tal cual; (b) su margen es un descarte a **dos** candidatas, muy por
debajo de E3; (c) **la centralidad de la clave es intrínseca** a construir distractores como
perturbaciones de ella — penalizarla penalizaría el buen diseño de distractores, y el nulo del 25 %
no contempla esa estructura. **La decisión de aceptarlo o pedir otra vuelta es del profesor.**

### §P1 — tres entradas del pool son código muerto

`GEO-COS-05`, `-07` y `-08`: **0 apariciones en 300 versiones**. Alcanzables **6 de 9**.
Causa: I-12 restringe el tercer hueco a `{4, 6, 9}`. Cifras completas en el bloque de I-12.

### §P2 — la variedad de TIPO de error es 2 sobre 3 posibles, en el 100 % de las versiones

Medido sobre 300 versiones, agrupando el pool en tres tipos conceptuales
(**A** dos lados + ángulo adyacente · **B** un lado + dos ángulos · **C** sólo los tres ángulos):
**todas** las versiones presentan exactamente **2 tipos distintos** entre sus tres distractores
(`{A,A,B}` o `{A,A,C}` en la rama general; `{B,B,A}` en la canónica). **0 %** llegan a los 3.

Es el precio directo de forzar `GEO-COS-01` + `-02` (pasada 1), que son **el mismo tipo A**.
Es un roce con el Incidente `INC-POOL-TAMANO`: el **tipo** de error varía menos de lo que el
recuento de 9 entradas sugiere. Ningún validador del arsenal lo mide.

### §C1 — comentario del pool corregido

Decía «aquí 9» sin matizar. Ahora declara «9 escritas, 6 alcanzables» con la cifra.

### Lo que §P7-D NO limita

Un defecto de **CORRECCIÓN** (clave falsa, segunda clave, Solution que afirme algo falso) se corrige
**aunque agote el presupuesto**: es binario. Verificado tras la pasada 3, N = 100 × 2 semillas:
**0** versiones con clave que no resuelva, **0** segundas claves, **0** abortos de guarda, error
relativo máximo de la identidad del coseno **5,55 × 10⁻¹⁶**, y `verificar_espacio.R` **APROBADO**
con sus 5 mutantes cazados por su propia sonda.

### §FASE-2C — objeciones del detractor aplicadas (2026-09-13)

Veredicto: **APROBAR_CON_CAMBIOS** (0 críticas · 2 altas · 3 medias · 2 bajas). Aplicadas:

| Obj. | Qué era | Fix | Verificación |
|---|---|---|---|
| **1** (alta) | `GEO-COS-06` se rotulaba «con su ángulo opuesto» y ese ángulo **no figura** en sus datos `{z,X,Y}`; contradecía la convención que la Solution enuncia dos párrafos antes. **48,5 %** de las versiones | `nombre` y `descripcion_corta` reescritos | canal-neutral: esos campos viven **sólo** en la Solution, nunca en el texto de las opciones |
| **2** (alta) | Única regla relacional con aplicabilidad **0,0 %** | G1 + G3 + G7 | ver §G7 |
| **3** (media) | **I-12 e I-4 sin guardia mecánica**: revertirlas no abortaba nada (mutantes M7 y M9: **0/60**) | dos `stopifnot`: no-repetición de letra en el tercer hueco, e igualdad **byte a byte** de dos dibujos que sólo difieren en `lado_pedido` | **M7 52/60** (los 8 restantes son canónicas, donde no aplica) · **M9 60/60** · control sin mutar **0/60** |
| **4** (media) | `salida/` era 48 min anterior al `.Rmd` | regenerado con el `.Rmd` vigente | — |
| **5** (baja) | `GEO-COS-05`/`-08` describían «los ángulos que no lo delimitan» — geométricamente **falso** (uno sí lo delimita) | redactados de nuevo | hoy son código muerto (§P1), pero M7 probó que nada impedía reactivarlos |
| **6** (baja) | comentario decía `GEO-COS-04 (z,X,Z…)`; con esos rótulos **repetiría letra** y contradiría el criterio del propio comentario | `(y,X,Z)` | — |
| **7** (baja) | (a) único metadato no-ASCII del bloque · (b) el **13,0 %** de las figuras bajaba a 14,5° de ángulo menor, muy lejos de los 39,6° del impreso | (a) `triangulos` · (b) cota **≥ 30°** por enumeración de la rejilla, sin `repeat` (Familia 1) | fidelidad de forma H-3 |

**Lo que el detractor confirmó por su cuenta**, con criterio escrito desde cero: 0/200 claves que no
resuelvan · 0/200 segundas claves · error relativo máx. **1,4 × 10⁻¹⁵** · los cuatro textos impresos
verbatim en 24/24 canónicas · la guardia de emparejamiento **no es tautológica** (mata 60/60).

Y una evidencia de Nivel 1 sobre I-7 que conviene no perder: **el propio cuadernillo sanciona la
cláusula**. Su opción C (`Lados r y s y ángulo R`) es exactamente el patrón `GEO-COS-01` —dos lados
y un ángulo adyacente— y el ICFES **no** la da por clave. Eso zanja la ambigüedad con la fuente
oficial, no con un argumento.

---

## I-16 · Coste MEDIDO del cambio de I-1: un canal NUEVO de fórmula, declarado sin rebaja

> **Medido el 2026-09-13**, inmediatamente después de aplicar el cambio de I-1. N = 100 (regla #23),
> dos semillas independientes, convención canónica `score = 1/|S|` (nulo exacto por regla = 25,0 %).
> Artefacto medido: md5 `d1f7b91675eac2af8129dcd3ebaa8e5c`.

### Lo que NO se movió

La **batería §P7 de 21 reglas se dejó CONGELADA** (§P7-C: ampliarla a mitad de ciclo cambiaría la
vara) y se corrió idéntica sobre el artefacto anterior y el nuevo:

| | Máximo | Techo nulo | Exceso | Veredicto |
|---|---:|---:|---:|---|
| Antes del cambio | 50,0 % (D1) | 31,4 % | **+18,6 pp** | BLOQUEA |
| **Después** | 50,0 % (D1) | 31,3 % | **+18,7 pp** | BLOQUEA |

Diferencia dentro del ruido (sd 2,8 pp). **Y era previsible**: la batería §P7 sólo recibe las
OPCIONES, y el cambio no tocó ni una. Es la ceguera que I-10 ya declaraba. *Un `sin cambio` de esa
batería no acredita que el cambio fuese inocuo* — por eso existe el bloque siguiente.

Tampoco se movió nada de `validar_diagnosticidad.R`, por la misma razón (sus cuatro sondas miran
las opciones): **H1 = 0 % · H2 = 0 % · H3 = 100 % · H3b = 16 %**, idénticos antes y después. El
`ERR_DIAG_SUPERFICIAL` sigue siendo el de H3, amparado por **I-10**.

### Lo que SÍ se movió: la fórmula pasó a ser un estímulo con letras

| Regla estímulo → opción | Antes | Después (2 semillas) | Ítem impreso | Exceso s/ impreso |
|---|---:|---:|---:|---:|
| **F1** «no cita el ángulo que aparece en la fórmula» | **inerte** (25,0 %) | **42,4 % / 40,7 %** | 25,0 % | **+16 a +17 pp** |
| **F6** «F1 **y además** no repite letra» (F1 ∧ D1) | **inerte** = D1, 50,0 % | **77,2 % / 72,0 %**; con filtro vacuo **80,5 % / 75,0 %** | 50,0 % | **+25 a +30 pp** |
| F5 «de las de un ángulo, la que no cita el de la fórmula» | 50,0 % | 46,8 % / 47,0 % | 50,0 % | −3 pp |
| F3 «la que más comparte con la fórmula» | 25,0 % | 3,2 % / 3,0 % | — | canal **inverso**, inofensivo |
| E3 «empieza por el ángulo homónimo» (residuo de I-15 §D2) | 100,0 % | 100,0 % | 100,0 % | 0 pp |

**«Inerte» tiene un significado exacto:** en el artefacto anterior y en el cuadernillo impreso la
fórmula es genérica (`a, b, c, α`), así que no cita **ningún** rótulo de la figura y el filtro por
su ángulo no descarta a nadie. La regla existía pero no podía disparar. Ese es el valor de partida
honesto, no un cero inventado.

### El hallazgo: F6 supera el corte de +8 pp y es un canal NUESTRO

`F6` deja **una sola candidata en el 85 % de las versiones de la rama general**. Su mecanismo es
puramente tipográfico y no exige geometría alguna:

1. El ángulo de la fórmula `W` es `Y` o `Z` — nunca `X`, por la regla del profesor.
2. `GEO-COS-01` y `-02` van **forzados** (I-12) y citan justamente `Y` y `Z`. Luego `F1` descarta
   siempre a uno de los dos **gratis**.
3. `D1` («no repite letra») descarta a **los dos**, porque ambos repiten (`y,z,Y` y `y,z,Z`).
4. El tercer hueco está restringido por **I-12** a los no-repetidores `{4, 6, 9}`, dos de los cuales
   citan `Y` o `Z`. Cuando ese tercero cita `W`, la conjunción aísla la clave.

### El MARGEN, que es lo que decide si el canal es explotable (§P7-B)

Una tasa alta con margen pequeño **no** es un defecto: la regla #22 §P7-B exime lo imperceptible.
**Aquí no aplica la exención.** Medido sobre 173 versiones no canónicas:

| Tamaño del conjunto que sobrevive a `F6` | Frecuencia |
|---|---:|
| **1 candidata → acierto SEGURO sin razonar** | **63,0 %** |
| 2 candidatas → 50 % | 37,0 % |
| 3 ó 4 candidatas | **0 %** |

Y **la clave sobrevive al filtro en el 100 % de las versiones**: la heurística nunca la descarta por
error, así que es *fiable* para quien la descubra. Score medio en la rama general: **81,5 %**. El
mismo filtro sobre el ítem impreso deja **2** candidatas → 50,0 %.

No es una señal marginal: es un descarte a **candidata única en casi 2 de cada 3 versiones**.

**Es el ciclo de desplazamiento de canal de la regla #22 §P7, en su forma más clara:** I-12 cerró el
canal léxico restringiendo el tercer hueco a los no-repetidores, y ese mismo cierre es lo que hoy
permite que `F1` lo remate. Se optimizó lo que la batería medía y se abrió lo que no miraba.

### Por qué NO se persiguió

Instrucción explícita del invocador al pedir el cambio: *«Si el cambio abre un canal > +8 pp sobre
el ítem impreso, **no lo persigas**: mídelo, decláralo con su cifra y repórtalo — la decisión de
aceptarlo es del profesor.»* Además:

- **Es estructural dado el diseño pedido.** La fórmula del coseno para el lado `w` cita
  necesariamente el ángulo `W`, y `W ∈ {Y, Z}` son exactamente los ángulos de los distractores que
  **I-12 obliga a forzar**. Cerrarlo exigiría reabrir I-12 o I-10, que están **firmados**.
- **H-5**: relajar nunca es autónomo; y aceptar un canal medido tampoco lo es.

### Cautela sobre la vara (heredada de I-15, y aquí importa)

La vara del ítem impreso es **una sola instancia de 4 opciones**, así que *cualquier* regla que
aísle su clave mide 100 % allí. **Control positivo medido:** «elige la última» da **100,0 % en el
impreso** y **29,0 %** aquí. La vara de n = 1 **infla** hacia arriba — y aun así `F6` la supera por
+30,5 pp. Es decir: la cautela juega **en contra** del ejercicio, no a su favor, y el hallazgo
sobrevive.

### Lo que NO se degradó (verificado, no supuesto)

| Comprobación | Resultado |
|---|---|
| Clave verdadera / segunda clave | **0 / 200** y **0 / 200** |
| La fórmula nunca se plantea para el lado pedido | **0 violaciones / 200** |
| `validar_multisemilla.R` | **APROBADO**, 0 fallos en 100 |
| `validar_diversidad_sustantiva.R` | **PASS**, 33 valores únicos |
| Umbral de producto 250/300 (regla #3) | **295 / 300** completa · **266 / 300** sólo texto |
| Fórmulas de enunciado distintas | **69 / 300** |
| Ortografía | limpio (el `--fix` sólo tocó comentarios) |
| `\pandocbounded` en uso / `LTcaptype` | **0 / 0**, con `includegraphics[width=0.62\linewidth…]` y **control positivo** de la sonda |

### Guardias nuevas: probadas por MUTACIÓN, no supuestas

Contrato del incidente `INC-MUTANTE-SONDA` (cada mutante declara su sonda y debe morir **por
ella**), sobre **copias en `/tmp`** — nunca el archivo real:

| Mutante | Qué revierte | Sonda esperada | Abortos / 60 | Veredicto |
|---|---|---|---:|---|
| M1 | `lado_formula <- x` (la fórmula se plantea para el lado pedido) | `lado_formula` | **49** (los 11 restantes son canónicas, donde no aplica) | cazado por SU sonda |
| M2 | la canónica pierde la fórmula genérica impresa | `formula_enunciado` | **11** (= exactamente las canónicas) | cazado por SU sonda |
| M3 | `W_formula <- X` (el ángulo pasa a ser el del lado pedido) | `W_formula` | **49** | cazado por SU sonda |
| — | **control sin mutar** | — | **0** | correcto |

Ninguno murió por una sonda ajena, así que las guardias **no son tautológicas**.

### Veracidad matemática de la fórmula nueva (defecto de CORRECCIÓN, binario)

La fórmula del enunciado pasó de ser un enunciado genérico a una **afirmación matemática con letras
concretas**. Si fuese falsa sería bloqueante. Verificada numéricamente sobre el triángulo
**efectivamente dibujado** en 172 versiones no canónicas: **0 malformadas**, **0 planteadas para el
lado pedido**, error relativo máximo de la identidad del coseno **1,02 × 10⁻¹⁵** (precisión de
máquina). Se comprueba además que el ángulo citado es el **opuesto** al lado despejado y que los dos
lados multiplicados son exactamente los otros dos.

### FASE 2C del cambio — objeciones del detractor (2026-09-13, veredicto `APROBAR_CON_CAMBIOS`)

**0 críticas · 1 alta · 3 medias · 3 bajas · 0 defectos de CORRECCIÓN.** Detractor independiente
(no escribió ni corrigió el artefacto), con criterio construido desde cero: batería propia de **351
reglas por enumeración exacta**, calibrada porque reproduce **diez** cifras ya publicadas aquí.

| Obj. | Qué era | Estado |
|---|---|---|
| **1 (ALTA)** | La reflexión metacognitiva «traducir una fórmula **general** a la notación de la figura» describía un paso que la rama general **ya no exige**: **19,0 %** de las versiones. El cambio actualizó `intro_teorema`, el puente y el punto 2 de la estrategia, y **se saltó el pool de reflexiones** | **APLICADA**: `reflexion_de_rama` condicionada a `es_canonica`. Verificado **0/254** en rama general |
| 2 (media) | En el **100 %** de la rama general, uno de los distractores forzados por I-12 **es exactamente el conjunto de datos que consume la fórmula mostrada** | **DECLARADA** abajo |
| 3 (media) | La canónica (1/8) pasa a plantear una **tarea cognitiva distinta**: traducir notación, frente a intercambiar papeles en las otras 7/8 | **DECLARADA** abajo |
| 4 (media) | El residuo que el orquestador declaró (F6) **apunta a la regla equivocada** | **CORREGIDO** abajo |
| 5 (baja) | `pandoc3.docx` era anterior al `.Rmd` | **APLICADA**: los 3 DOCX regenerados y verificados |
| 6 (baja) | La guardia canónica de la fórmula compara contra **el mismo literal** asignado 15 líneas antes: mata una edición de un sitio, no una coordinada | **MATIZADA**: es más débil que la de `lado_formula`, que sí es estructural |
| 7 (baja) | El puente negaba y afirmaba lo mismo («no es sustituir letra por letra: hay que cambiar los papeles») | **APLICADA**: «No basta con copiar la fórmula tal como viene dada: hay que **intercambiar los papeles**» |

#### §2 — un distractor es siempre el input de la fórmula mostrada (declarado, no revertido)

La fórmula muestra `w² = x² + v² − 2·x·v·cos W`; despejar `x` **con ella** exigiría `{w, v, W}`, que
es exactamente `GEO-COS-01` o `-02`, forzados por **I-12**. Doble filo, y por eso se declara en vez
de revertirse: **a favor**, es un distractor fuerte que diagnostica la concepción «uso la fórmula tal
como me la dan»; **en contra**, el aviso de que hay que **reescribirla** vive en la Solution, que el
estudiante ve después.

#### §3 — coste de equivalencia de la canónica: 1 de cada 8

Antes del cambio las 8/8 versiones pedían lo mismo. Ahora la canónica pide **traducir notación** y la
rama general **intercambiar papeles**. Un estudiante de cada ocho resuelve una tarea que no es la de
sus compañeros. Es una tensión con **H-2** (la fidelidad al impreso no se negocia) y se resuelve a
favor de H-2, declarando el coste.

#### §4 — CORRECCIÓN del residuo: la cifra limpia es **F1**, no F6

> El orquestador declaró `F6 = 77-80 %` con «+30,5 pp sobre el impreso». **Esa lectura estaba mal
> planteada en dos sentidos, y se corrige aquí.**

1. **F1/F6 NO TIENEN VARA.** El ítem impreso mide 25,0 % / 50,0 % en ellas **no porque resista el
   canal, sino porque el canal no se puede formular allí**: su fórmula es genérica, luego «el ángulo
   que aparece en la fórmula» no existe y el valor es el de **filtro vacuo**. Presentar la diferencia
   como «exceso sobre la vara» confunde *«el impreso resiste»* con *«ahí la regla no es evaluable»*.
   §P7-A no puede absolver ni condenar; el único criterio disponible es el **nulo atómico del 25 %**.
2. **El TECHO NO SE MOVIÓ, y F6 está dominada.** Barrido de 351 reglas buscando alguna que sature
   aquí sin saturar en el impreso: **ninguna**. El máximo era y sigue siendo **100 %**, vía `E3` y
   `E4`, ambas **preexistentes** y ya declaradas en I-15.

| Regla | Global (exacto) | Rama general | Exceso **atómico** | ¿Vara? |
|---|---:|---:|---:|---|
| **F1** «no cita el ángulo de la fórmula» | **42,0 %** | 44,4 % | **+17,0 pp** | **no** |
| F6 = D1∧F1 (lo que se había declarado) | 79,2 % | 83,3 % | +54,2 pp | no |
| D1∧F5 · E3∧F2i (no probadas por el orquestador) | 93,8 % · 90,6 % | **100 %** · **100 %** | — | no |

**La cifra honesta del coste del cambio es `F1 = 42,0 %, +17,0 pp atómicos`**, por encima del corte
de +8 pp. Las conjunciones son **argmax sobre 351 reglas**: sesgo de selección, justo lo que la
memoria del repositorio advierte («el residuo §P7 no es el máximo de la batería»).

3. **Hallazgo que debilita un argumento de I-15.** `A1∧D1` —«la que cita dos lados y cuyas tres
   letras son distintas»— mide **100 %**, no usa la fórmula, y es **extensionalmente idéntica a E4**.
   I-15 absuelve a E4 diciendo *«quien la aplica ha razonado»*; **ese argumento no sobrevive a la
   re-expresión**: la misma partición se ejecuta contando letras repetidas, sin concepto de «ángulo
   comprendido». La tasa no cambia; la **justificación** declarada es más débil de lo que I-15 afirma.
   Se registra sin perseguirlo: §P7-D lo prohíbe y el canal es **preexistente al cambio**.
