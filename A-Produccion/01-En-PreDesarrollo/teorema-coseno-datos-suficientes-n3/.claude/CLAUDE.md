# Invariantes locales — `teorema-coseno-datos-suficientes-n3`

> Regla #17 / Incidente `INC-CLAUDE-LOCAL`. Estas invariantes **prevalecen sobre el criterio
> genérico** dentro de este subproyecto. Si alguna contradice una regla del repo raíz, gana la
> del raíz y el conflicto **se reporta**, no se resuelve en silencio.

Origen: pregunta impresa **No. 50** del cuadernillo ERA-2026 Matemáticas (`pagina_015.jpg`,
pie impreso 20). Anclado por el **número impreso**, no por el mapeo: `mapa-paginas.md` y el OCR
inducían al ítem equivocado (la tabla de límites de velocidad, que es la Q49).

## I-1 · La discordancia de notación ES la tarea. NO armonizar.

El enunciado imprime la fórmula general con `a, b, c, α` mientras la figura usa `q, r, s` y
`Q, R, S`. **Esa discordancia es la demanda cognitiva del ítem**: el estudiante debe transferir
la fórmula genérica a la notación de la figura. Unificar las notaciones —el impulso «limpiador»
natural— convierte un ítem de transferencia en uno de sustitución mecánica.
Regla #24 H-2: se reproduce tal cual, incluidos sus rasgos incómodos.

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

La fórmula genérica del enunciado es `a² = b² + c² − 2bc·cos α`. Una familia de vértices
`{A, B, C}` daría lados `a, b, c` **idénticos** a los de la fórmula y disolvería la tarea de
transferencia, que es la demanda cognitiva del ítem según **I-1**. Estaba en el pool y se
retiró el 2026-09-13.

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

**Esto NO contradice I-1**: lo que I-1 prohíbe es armonizar las **letras** de la fórmula genérica
con las de la figura —eso disolvería la tarea de transferencia—, no imitar su **composición
tipográfica**.

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
