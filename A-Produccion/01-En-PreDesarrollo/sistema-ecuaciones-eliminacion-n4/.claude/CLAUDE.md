# Reglas locales — sistema-ecuaciones-eliminacion-n4

Invariantes de ESTE subproyecto. Prevalecen sobre el criterio genérico de cualquier agente
dentro de este directorio. Si una contradice una regla del repo raíz, gana la del raíz y el
conflicto se REPORTA, no se resuelve en silencio.

Origen: `MAT-2026-1-044` (ERA-2026 Sesión 2, pregunta impresa **44**, `pagina_013.jpg`),
verbatim de MAT-2026-1-130.

## L-1 — La opción D del cuadernillo NO tiene errata. NO "corregirla".

Un diagnóstico previo afirmó que `60L + 100R = 1.300.000` era errata del ICFES y que lo
correcto sería `40L`. **Es falso, y medido:** D dice «debió **sumar las ecuaciones**», y las
ecuaciones que el Paso 2 opera son E1 (`30L+60R=900.000`) y la del Paso 1 (`30L+40R=400.000`).
`30+30=60` · `60+40=100` · `900.000+400.000=1.300.000`. Exacto.

El `40L` sale de sumar E1 con E2 **original**, que no es lo que D describe.

Consecuencia: el señuelo de `ERR-ALG-05` se calcula SIEMPRE sobre la ecuación **mostrada en el
Paso 1** (`2a·L + (b+d_p)R = P1+P2_p`), nunca sobre E2 original. Un refactor que lo derive de
E2 original rompe la fidelidad al cuadernillo Y la aritmética. Blindado con `stopifnot()`.

## L-2 — Prohibido el distractor «multiplicar toda la ecuación por un factor de signo opuesto y SUMAR»

Es una ruta **matemáticamente válida**: ×(−k) toda E2 y sumar da el mismo R correcto
(verificado: L=20.000, R=5.000 por las dos rutas). Sería una segunda clave correcta.

La opción A canónica NO cae aquí: multiplica por −k **solo el primer término**, lo que sigue
violando la propiedad uniforme. Consérvala verbatim.

## L-3 — La instancia canónica viola `b > k·d`; es deliberado

Las versiones paramétricas exigen `b > k·d` para que el Paso 2 correcto no arroje coeficientes
negativos. El canónico tiene `b=60 < k·d=120` y se exceptúa por fidelidad (regla #24 H-2). No
"normalizar" el canónico para que cumpla la restricción general.

## L-4 — Ceguera declarada de sondas

Las 4 opciones comparten primera palabra («En»). Por construcción: **H2 = 0 % y H3 no se
imprime**. El relevo es **H3b**, que borra los dígitos de la firma — por eso lo que DEBE variar
entre versiones es el **vocabulario del procedimiento**, no sólo el número del paso.
Parametrizar sólo el número del paso produciría un PASS falso.

## L-5 — La Solution NO lleva nota sobre la opción D ni sobre el cuadernillo

Decisión del profesor: verbatim, sin nota editorial.

## L-6 — `calcula()` / `ejecutar_proc()` son funciones PURAS

Prohibido `sample`/`runif`/`rnorm` dentro (Capa D, `ERR_SEM_D`). El veredicto de cada opción se
obtiene EJECUTANDO su propuesta, no declarándolo.

## L-7 — Guarda contra verificación semántica vacua

Si el `proc` de un señuelo saliera idéntico al del estímulo (`proc == pr_show`), `ejecutar_prop()`
comprobaría «el estímulo no llega a R», que ya está aseverado antes: la sonda se verificaría a sí
misma. Medido vacuo en 27/100 versiones antes de la guarda. `proc_de_opcion()` aborta si ocurre.

## L-8 — La batería de §P7 está CONGELADA en `bateria_congelada.R`. No se le añaden reglas.

Pre-registro §P7-C del 2026-08-20. Cubre las seis familias y mide **por estrato**, porque las tres
ramas de este ítem son estructuralmente distintas y el agregado esconde canales al 100 %.

Si una auditoría descubre una familia sin sonda, se añade y **se re-mide el histórico completo**, o
se declara que las cifras anteriores no son comparables. Encadenar pasadas con baterías distintas y
tratar sus excesos como una serie es exactamente lo que §P7-C prohíbe, y ya pasó una vez aquí: la
batería anterior dio V4 por bueno en todo cuando había empeorado tres reglas.

Los cortes salen del helper `.claude/scripts/bateria_eliminacion.R`, **fuente única**. Este script
aborta si no lo encuentra en vez de inventarlos.

## L-9 — ⚠️ REFUTADA el 2026-08-20. `V1` NO era techo estructural.

**Lo que decía esta invariante:** que «divisor = coeficiente de su propia ecuación» (100 % en el
estrato del paso 3, exceso +75,0 pp) no se podía corregir eligiendo otros divisores, porque dividir
entre el coeficiente *es* la operación correcta, y que la única salida era no mostrar la ecuación.

**Era falso, y la premisa oculta era otra:** el canal no venía de *mostrar* la ecuación, sino de que
**todas las opciones de paso 3 mostraban LA MISMA** —la reducida—, con lo que la única cuyo divisor
aparecía en la ecuación que ella misma imprimía era la clave. Cada señuelo divide entre `d`, `b` o
`a`, que **son coeficientes**: de las ecuaciones *originales*. Haciendo que cada opción cite la
ecuación de la que su propio divisor es coeficiente, las dos quedan coherentes y `V1` cae de
**+21,8 pp a +0,3**. Ninguna ecuación citada es falsa: las tres existen en el enunciado, y el error
del señuelo pasa a ser **despejar de una ecuación que todavía tiene dos incógnitas** — que es un
error más nombrable que el que tenía antes.

**Y su complemento `E2`** («el divisor de la clave nunca es un coeficiente del sistema», también
100 %) lo causaba **un filtro de la grilla**: `coef = b − k·d` estaba prohibido de coincidir con
`a`, `d` o `b`. Ese filtro existía sólo para evitar que dos opciones con el mismo divisor tuvieran
texto idéntico — colisión que deja de existir cuando cada una cita su propia ecuación. Retirado:
`E2` **+21,8 → +0,0**.

**La lección, que vale más que el fix:** «techo estructural» se declaró tras comprobar que ninguna
*elección de divisores* lo movía. Pero el grado de libertad no estaba en el divisor: estaba en
**qué ecuación acompaña a cada divisor**, y en un filtro puesto por una razón que ya no aplicaba.
*Antes de declarar algo estructural, hay que enumerar los grados de libertad, no sólo el que se
tenía en la mano.*

## L-10 — Tras un borrado por patrón, comprobar PRESENCIA DE SÍMBOLOS, no que el código parsee.

El 2026-08-20 un regex demasiado amplio borró cinco funciones vivas (`proc_de_opcion`,
`ejecutar_prop`, `paso_de`, `txt_clave`, `txt_senuelo`) y **el chunk seguía parseando**. Una
comprobación de sintaxis no lo habría visto. Lo cazó `grep -c '^<fn> <- function'`.


## L-11 — El rediseño de la grilla (2026-08-20): qué exige cada estrato

La grilla ya no es uniforme: **cada estrato exige una propiedad numérica distinta**, y las tres
están en `estructuras`/`elegir_par`. Si se tocan, hay que volver a medir con `bateria_congelada.R`.

| Estrato | Filtro | Por qué |
|---|---|---|
| paso 2 | `dig_ok`: `\|b−k·d\|` y `b+k·d` con el mismo nº de dígitos | Iguala la longitud de la clave con la de su señuelo hermano `ERR-ALG-08`. Sin él, «la más corta» acertaba 30/30 |
| paso 3 | `coef_sis`: `\|b−k·d\| ∈ {a, c, d, b}` | Sin él, el divisor de la clave es SIEMPRE un número ausente del enunciado y `E2` aísla la clave 29/29 |
| todos | rejillas ampliadas `d ∈ [10,40]`, `b ∈ [50,200]` | Exigir `dig_ok` sobre la rejilla vieja dejaba 10 combinaciones `(k,d,b)`; sobre la ampliada, 50. El espacio pasa de 152 a 424 estructuras |

**`ERR-ALG-08` existe para empatar la forma, no para añadir un error más.** Es el hermano simétrico
de `ERR-ALG-04`: 04 aplica el cambio de signo a los términos con incógnita y no al independiente;
08 lo aplica sólo al término que se cancela. Su ecuación tiene la misma forma, los mismos números y
casi la misma longitud que la de la clave, y difiere sólo en el coeficiente. **Retirarlo reabre
`NTm` y `L1` al 100 % en el estrato 2.**

### ⚠️ Corrección V7 (2026-08-20): `dig_ok` sobrevivió a su justificación, y AUN ASÍ compra

`ERR-ALG-08` está **retirado desde V6** (lo sustituyó `ERR-ALG-09`, L-14), así que la fila de
arriba justifica `dig_ok` con un error que ya no se instancia. El detractor de la pasada de
confirmación dedujo de ahí que el filtro era vestigial y costaba el 57 % del espacio de
estructuras (204 de 476) sin comprar nada, y pidió medirlo antes de retirarlo.

**Medido (batería congelada, tres bases, N = 100 cada una), y su premisa es FALSA:**

| | con `dig_ok` | sin `dig_ok` |
|---|---:|---:|
| `L1` agregado | +12,5 / +13,0 / +11,5 | **+13,5 / +14,5 / +15,0** |
| `NTm` dentro del estrato 2 | +25,0 / +25,0 / +25,0 | **+32,1 / +28,1 / +25,0** |

El filtro **sigue igualando longitudes**, sólo que ahora con `ERR-ALG-09` en vez de con `08`:
los dos comparten el coeficiente `b − k·d` con la clave, y `dig_ok` acota su número de dígitos.
**NO se retira.** La corrección que sí procedía era documental, y es ésta: el motivo escrito
había caducado, el efecto no. *Un filtro cuya razón caducó no es lo mismo que un filtro inerte;
la diferencia se decide midiendo, no leyendo el comentario.*

## L-12 — El corrector de ortografía NO distingue un `#` dentro de una cadena

Al corregir comentarios por línea, `s.index("#")` trata `cat("### Reflexión\n\n", reflexion, …)`
como comentario y reescribe el **nombre de la variable** a `reflexión`. El chunk sigue parseando y
`data_generation` sigue evaluando: el fallo aparece **sólo al renderizar** (`objeto 'reflexión' no
encontrado`), porque la variable se usa en el chunk de la Solution.

Es L-10 en su versión ortográfica, y confirma la trampa 7 de `HANDOFF.md` §5: **la batería y el
verificador no miran el render**. Tras cualquier corrección masiva de texto, renderizar los cinco
formatos antes de dar nada por bueno.

## L-13 — El error real cae SOLO en los pasos 1 y 2. El estrato del paso 3 está retirado.

Desde V6. **No reintroducirlo sin leer §12.3 del HANDOFF.** Todos los canales que este ejercicio
peleó durante trece pasadas —`V1`, `E2`, `V5`, `DEN`, `U1`, `M5`, `G1`— vivían en ese estrato,
porque sus opciones nombran un divisor **y** citan una ecuación: dos grados de libertad que las de
paso 1 y 2 no tienen, y cada cierre en uno abría el otro.

**El criterio no es la métrica, es el ítem oficial:** el cuadernillo no tiene ninguna opción de
paso 3 — sus cuatro opciones son dos de paso 1 y dos de paso 2. El estrato 3 era extensión nuestra.

`ERR-ALG-06` y `ERR-ALG-07` siguen en el catálogo como errores documentados y **no se instancian**.
`eq_de` conserva sus ramas de paso 3, que hoy son código inalcanzable: se dejan porque describen
cómo debe citarse una ecuación si alguien reintroduce el estrato, y retirarlas perdería esa
información sin ganar nada.

## L-14 — `ERR-ALG-09` empata el COEFICIENTE con la clave, y ese es todo su propósito

Sustituyó a `ERR-ALG-08` (retirado en V6), que empataba la *forma* pero cuyo coeficiente era
`b + k·d`. Como **`|b − k·d| < b + k·d` es una identidad** —119/119 estructuras—, la clave del paso 2
llevaba SIEMPRE el coeficiente menor: una regla sin ningún contenido matemático acertaba el 100 % de
ese estrato.

`ERR-ALG-09` tiene el coeficiente **correcto** y yerra sólo en el término independiente, con dos
variantes sorteadas (`P1 − P2` y `P1 − k2·P2`) para que el cociente de la clave quede **entre** las
de los señuelos. Cambiarle el coeficiente reabre `G2` y `G4` (+10,9 y +33,2 pp medidos).

## L-15 — Una batería congelada no protege de que la vara sea CORTA

L-8 congela la batería para que nadie mueva la vara a mitad de ciclo, y eso sigue vigente. Pero V5
salió «7 de 8 canales cerrados» con ese instrumento y era **una regresión**: medido con reglas que
comparan dos números de la misma opción **por razón** —no por igualdad, que es lo único que la
batería mira— el canal pasó de −0,5 a **+40,9 pp**.

Antes de declarar un ciclo cerrado, medir también con **sondas fuera de la batería**, en script
aparte y con control aleatorio calibrado. Si disparan, se **declaran**; darlas de alta obliga a
re-medir el histórico completo (§P7-C).

## L-16 — El guard `var09()`: la variante de `ERR-ALG-09` obedece a DOS restricciones, y (a) manda

`ecu()` escribe `0` en el miembro que quede vacío. `ERR-ALG-09` cancela la incógnita (`cL = 0`),
así que si el signo del coeficiente superviviente y el del independiente son **opuestos**, todos
los términos caen en el mismo miembro y el otro sale `= 0`. **Ese `0` cuenta como número**: el
señuelo pasa a 3 números frente a los 2 de la clave y la clave queda SOLA como la de menos
números. Medido: 21 de 137 versiones del estrato 2, y son exactamente las 21 en que `NTm` aísla
la clave.

`var09()` elige la variante evitándolo, **pero primero comprueba el gate del bucle**: la ecuación
de una opción no puede coincidir con una del estímulo. Con el error real en el paso 1, la var 1 de
`ERR-ALG-09` **es** la ecuación del Paso 2 del estímulo (174/174 versiones), el bucle de 400
reintentos la rechaza siempre y el render **aborta**. Aplicar el fix del detractor tal cual, sin
esa comprobación: **27 abortos de 100**, medidos.

**PRIORIDAD (a) gate del bucle > (b) guard del `0`.** (a) es condición de existencia del ítem;
(b) es diagnosticidad. *Una mejora de diagnosticidad que impide construir el ítem no es una
mejora: es un ítem que no existe.*

## L-17 — `ERR-ALG-09` es apto como error real, y la Parte 4 muestra SIEMPRE las tres elegibles

**Por qué 09 sí y 02/03/05 no.** El criterio es que la cadena de tres pasos quede coherente con
**un solo** error. 09 cancela la primera incógnita, así que el Paso 3 divide legítimamente entre
el coeficiente superviviente y el fallo está sólo en el término independiente. 02 y 05 no cancelan
(el Paso 3 haría desaparecer la incógnita sin justificación → dos errores) y 03 cancela la otra y
calcula un valor **verdadero**. Es una distinción de estructura, no de conveniencia.

**Su propiedad en la Parte 4 es `PROP-INDEP`,** y no se solapa con ninguna: en 09 el Paso 1
multiplica todos los términos (PROP-UNIF se respeta) y el fallo está en la resta, que vuelve al
independiente original; en 04 el término sí viene del paso anterior, sólo con el signo equivocado
(PROP-SIGNO). Verificado además por ejecución en `viola_pr_show`.

**Las tres propiedades elegibles se muestran SIEMPRE.** Con `|S|` variable, la regla «elige la que
puede ser clave» paga `1/|S|` y por Jensen `E[1/|S|] > 1/E[|S|]`: medido **+42,7 / +37,5 / +36,0
pp**, con `|S| = 1` en 26-40 de cada 100 versiones. Fijando `|S| = 3` el pago es **exactamente
1/3** en toda versión → **+8,3 pp**, que es el **piso estructural** de un ítem con tres errores
reales posibles. El precio es que 3 de las 4 ranuras de P4 dejan de sortearse; la diversidad la
sostiene la Parte 1 (95/100 claves distintas).

**Residuo declarado:** esto cierra la regla **léxica**, no la **cruzada**. Quien lea el paso en la
Parte 1 acierta P4 con 100 % si el paso es 1 —PROP-UNIF es la única elegible de ese paso— y 1/2 si
es 2. Cerrarlo exigiría dos errores reales en el paso 1, y el pool sólo admite `ERR-ALG-01` allí.
El compuesto medido cae de **+30,8/+33,2/+30,8** a **+7,8/+7,3/+6,7 pp** (zona gris).

## L-18 — `Z1`, `Z3`, `W1`, `W3` NO se persiguen. El ítem oficial puntúa 1,0 en ellas.

Cuatro reglas fuera de la batería congelada —comparan dos números de la misma opción **por razón**,
o leen el enunciado— miden alto en nuestro ejercicio:

| regla | nuestro exceso | ítem oficial (pág. 18, pregunta 44) |
|---|---:|---|
| `Z1` mayor (último ÷ primer número) | +15,5 pp (obs. 35,5 %) | **selecciona la clave** (1,0) |
| `Z3` mayor (último ÷ penúltimo) | +10,4 pp | — |
| `W1` proporcional a `E2` | +27,1 pp (obs. 50,9 %) | **selecciona la clave** (1,0) |
| `W3` igual a `E2 × k` (el factor que nombra el Paso 1) | +35,7 pp (obs. 61,5 %) | **selecciona la clave** (1,0) |

**La clave del paso 1 ES `k·E2` por construcción del método**, así que es mayor que su compañera y
proporcional a la segunda ecuación. El ítem del ICFES tiene exactamente la misma propiedad y la
tiene **al 100 %**; nuestra generación puntúa **0,375 / 0,509 / 0,615**. Por §P7-A la vara es el
examen real, y en estas dimensiones el ejercicio es **estrictamente menos filtrable que el ítem que
reproduce**. Perseguirlas sería exigirle más que al examen — el error que §12.1 del HANDOFF
documenta como el más caro de este ejercicio.

Lo mismo vale para la objeción B del detractor CLOZE («entre las dos del mismo paso, la del número
mayor», +14/+22/+15 pp tras V7): es **el mismo canal** visto desde el par del paso 1.

**Limitación declarada:** el contraste oficial es **n = 1 ítem**, no el corpus de 426 de §4. Es
indicativo, no una vara calibrada. Darles vara de verdad exige correr estas reglas sobre el corpus
con `bateria_referencia_icfes.R` **antes** de tocar nada.

## L-19 — La batería congelada PRECEDE a §P7-E y no tiene regla relacional. Medida fuera: sin canal.

La batería se congeló el **2026-08-20**. La regla #22 **§P7-E** —que exige al menos una regla
**relacional entre pares** (conjunto de numerales idéntico, molde compartido, longitud gemela)— es
del **2026-08-22**, dos días posterior. Revisadas las 15 reglas congeladas, **ninguna lo es**:
`V1`/`E2` son intra-opción y el resto son predicados por opción o mínimos/máximos. Ninguna expresa
«hay dos opciones que se parecen entre sí, elige una de las dos».

Y este ítem **tiene** esa estructura a la vista: en el estrato de paso 2, la clave y `cod_mismo`
comparten el **miembro izquierdo completo** y difieren sólo en la constante.

**Medida FUERA de la batería el 2026-09-12** (como L-15 manda, y sin darla de alta): «elegir una de
las dos opciones que comparten el miembro izquierdo» da **0,203 → −4,7 pp**. **No hay canal**: el
par gemelo existe en los **dos** estratos —en paso 2 lo forman clave y `cod_mismo`; en paso 1, los
dos señuelos de paso 2— y los estratos se compensan.

En la misma pasada se midió y **refutó** una hipótesis sobre el mecanismo de `L1` —que «la más
corta» fuera en realidad «menos **dígitos** en el número final», mucho más perceptible que un 3,8 %
de caracteres—: `Dmin` = 19,9 % → **−5,1 pp**. *Un residuo medido y descartado vale tanto como uno
confirmado: impide que la próxima pasada lo gaste otra vez.*

**No se da de alta.** L-8 exigiría re-medir el histórico completo o declarar incomparables las
cifras previas. Si alguna vez se añade, esa es la condición.

## L-20 — El residuo §P7 de este ejercicio es `M6c`, NO `L1` ni `X1`

Hasta V7 se declaraba `L1 la más corta` como residuo porque era el máximo de la batería. **Era la
regla equivocada**: su margen mediano es **3,8 %** (máx 5,8 %), dos o tres caracteres sobre setenta,
y §P7-B la exime por inexplotable. Declarar como residuo lo único ya exento deja sin declarar lo que
no lo está.

El único que supera el corte de +8 pp **y** sobrevive al filtro de margen es
**`M6c «descartar la de mayor |valor|»`: +8,3 pp, la clave nunca es la opción del número final más
grande (299/300) y la separación con el siguiente es del 37,4 %.** Es explotable de un vistazo, sin
aritmética y sin leer el enunciado: sube el azar de 25 % a 33,3 %.

Causa estructural (§10.7.1): **los señuelos suman o multiplican, la clave resta.** No es corregible
cambiando de variante. El ítem oficial comparte la propiedad (n = 1, misma limitación que L-18).

⚠️ **Consecuencia para quien mida:** que `L1` suba —lo hizo en V8, de +11,3 a +24,3 pp de media— **no
es una regresión**, porque su margen no se movió ni un punto (3,9 % mediano / 6,1 % máximo, idéntico
dígito a dígito en V7 y V8). Comparar versiones por el máximo agregado de la batería induce a error
en este ejercicio: hay que mirar los márgenes, regla por regla.

### Corregida por la pasada 3 (2026-09-12): son TRES residuos, y `M6c` es sólo del SCHOICE

La primera redacción de L-20 declaraba `M6c` como «el» residuo «de este ejercicio». Dos errores:

**(a) Faltaban dos.** El re-pesado de V8 subió `NTm` (+0,8 → **+8,6 pp**, brecha del 50 %) y `U1`
(−2,8 → **+6,7 pp**, categórica: 1 incógnita frente a 2). §P7-B **no los exime** — exime lo
imperceptible, no lo contable. Mecanismo, que coincide con su predicción analítica: `U1` es
determinista dentro de cada estrato (clave entre las dos de una incógnita en 63/63, 65/65, 62/62 de
paso 2; 0/37, 0/35, 0/38 de paso 1), así que su exceso es **puro peso de mezcla**: `p₂·½ − ¼`, que
da +0,0 con `p₂ = 0,50` (V7) y +6,5 con `p₂ = 0,63` (V8).

**No son corregibles sin reabrir el canal de la Parte 4 del CLOZE**: uniformar el **código** y
uniformar el **paso** son incompatibles mientras el paso 1 tenga un solo código `apto_real`, y L-17
descartó la única salida. Se declaran.

**(b) `M6c` es del SCHOICE.** El CLOZE tiene el suyo: la regla léxica de P4 sobre las tres frases
**pre-especificadas** mide **+8,3 pp**, que es el **piso exacto** de `|S| = 3`. Ojo: el +10,7 pp que
llegó a escribirse era el **máximo sobre las tres frases**, inflado por selección — §P7 exigencia 2
prohíbe eso, y aplicarlo a las reglas pero no a las frases fue el descuido.

**Regla de conducta que se sigue de aquí:** al declarar un residuo, medir el margen de **todas** las
reglas que disparan, no sólo de la que sale máxima. Generalizar desde dos reglas —que es lo que pasó
aquí— produce una declaración incompleta, y una declaración incompleta es indistinguible de una
medición que no se hizo.

## L-21 — `K_CANON`: la frecuencia canónica y `pc_canon` son UNA sola constante

`is_canonical <- sample.int(K_CANON, 1L) == 1L` y `pc_canon <- 1/K_CANON` están acoplados **por
construcción**, con `stopifnot(abs(pc_canon - 1/K_CANON) < 1e-12)` que lo hace ejecutable.

**Por qué existe:** antes eran dos literales `12` independientes y el `stopifnot` de los pesos se
satisfacía para cualquier `pc_canon ∈ (0, 1/3)`, así que una deriva era **silenciosa**. No es
hipotético: §13.7 proponía cambiar 1/12 → 1/20 y §14.2 barrió cuatro valores. Medido: `sample.int(6L)`
dejando `pc_canon = 1/12` lleva `ERR-ALG-01` de 31,5 % a 39,5 % —reabriendo ~8 pp del canal que V8
cerró— **con 0 fallos de corrección y sin que nada dispare**.

Control positivo de la guarda: dispara en **40/40** versiones de un mutante desacoplado; y con
`K_CANON = 12L` el comportamiento es idéntico al verificado (marginal 36,7/30,7/32,7).

**Si alguna vez se cambia la frecuencia canónica, se cambia `K_CANON` y nada más.**
