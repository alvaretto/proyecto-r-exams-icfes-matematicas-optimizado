# rapidez-media-error-paso — invariantes locales

Ítem 09 de Evaluar para Avanzar 2023, Matemáticas 11-2. Fuente verbatim: `ITEM_OFICIAL.md`. Sustituye a
`2023-Matematicas-11-2-09-Op-M.Rmd`, que está en producción y no se toca. Verificador propio: `verificar_espacio.R`
(`Rscript --no-init-file verificar_espacio.R todo`; `--vanilla` no sirve en esta máquina).

Estas invariantes prevalecen sobre el criterio genérico dentro de este subproyecto. Lo que parece simplificable
aquí está medido.

## L-1 · En ninguna versión el error es de aritmética

Las tres cuentas del procedimiento dan bien; lo que falla es el significado (qué se resta, qué unidad lleva cada
cifra). Es la trampa del impreso, que por eso se conserva aunque la clave rote: revisar solo las cuentas nunca basta.
Exactamente un paso está mal. Guardas L-1 en `data_generation`, mutantes M2 y M9, oráculo.

## L-2 · Las condiciones de divisibilidad valen para todas las versiones

`D` múltiplo de `t2` y `d1` múltiplo de `dur` solo los necesitan algunas ramas, para que el paso 3 muestre una rapidez
entera. Se exigen en todas: si solo las cumpliera esa rama, la divisibilidad de lo que MUESTRA el procedimiento
delataría la clave. El alcance es ese: no cubre lo que resulta de sustituir una razón en un paso (ver `X1b` en Residuos).
Guarda L-2, mutante M13.

## L-3 · La canónica es el impreso, verbatim

Con `P_CANON = 0.05` (tope 0,10, el de los ejercicios hermanos). Conserva la trampa del Paso 3, los cuatro moldes de
sus opciones y el `−20 km` de su opción D, que es una errata de cuenta dentro del distractor: la `Solution` lo señala,
el texto no se corrige (regla #24 H-2). `stopifnot` literales contra `ITEM_OFICIAL.md`, mutantes M4 y M5.

## L-4 · Ninguna razón falsa repara el procedimiento

«La distancia recorrida es `d2`» se puede leer como el viaje completo desde el punto de partida (`INC-LECTURA-MULTIPLE`).
Dos defensas:

1. Las razones FINAL no se ofrecen contra el paso que ya está mal. Guarda L-4, mutante M3b.
2. **Criterio `repara()`:** una razón no se ofrece si, puesta en su paso, da la rapidez correcta. Cubre `d2 = v t2` o
   `d2 = v t1`, donde la razón es literalmente cierta en el viaje completo (segunda clave, 22,9 de 10 000 con el diseño
   anterior). Antes cubría también dos signos que se compensaban (4,09 %), que desaparecen con L-8. Oráculo
   `ORACULO_REPARA`, mutante M15.

Se verifica por **enumeración exacta**: `verificar_espacio.R enumerar` recorre los 9 564 estados, y el modo `exacto` todos
los conjuntos de razones. Una muestra no sirve: en 300 versiones se esperaba 0,7 casos.

**Residuo en la canónica (regla #13 v1.2, H5 de la FASE 2C r2).** La opción A impresa, «100 km y no 80 km», admite la lectura
de viaje completo, y el texto impreso no se reescribe (L-3). No crea una segunda clave: puestas en su paso, las tres razones
falsas dan 100/2 = 50, −20/2 = −10 y 80/(−2) = −40, y ninguna da 40. El oráculo se salta la canónica en la comprobación de L-4;
por eso va declarado aquí.

## L-5 · El formato no delata la clave

Medido en la batería §P7. No simplificar sin volver a medir. Las cifras de esta sección son las del diseño con ramas de
signo: documentan por qué existe cada defensa. Lo vigente está en «Residuos vigentes» y en `salida/tabla_firma_o2.md`.

- **Un solo molde** fuera de la canónica, «es A y no B», el de la opción A del impreso. Se probó también un molde corto
  («es A»): `REL-afirmado-citado-en-otra` subía a 39,2 % (+10,3 pp).
- **Paso repetido:** es el de la clave con P = 1/2. Con P = 0, como en el impreso, `REL-paso-unico` da 50 % (+21,1 pp).
- **Rango de la clave:** se sortea primero, sobre el espacio enumerado de conjuntos de razones (`RANGO_META`). Sin eso,
  «el mayor número afirmado» daba 44,7 % (+17,1 pp), y con pesos el canal solo se movía a «el segundo» (+12,4 pp).
- `EV-C-UNIDAD` también se ofrece como razón falsa contra un paso 3 correcto: es la concepción del impreso y aporta una
  cifra grande que compite con la clave.

## L-6 · Formato del fuente

- Ninguna comilla invertida dentro de los chunks, tampoco en comentarios: la FASE 2D corta el chunk en la primera y
  escanea el código R como si fuera texto (marcó `codigo` y `analisis`).
- La unidad y el signo que cierran una fórmula van dentro de ella: `M(km(x), ".")`.
- Sin `−` (U+2212) literal: rompe pdflatex. Las restas van en modo matemático.
- La franja del día sale de la primera hora, no de cada hora.

Sondas en `verificar_espacio.R` (H4 de la FASE 2C r2): `L-6: comilla invertida en un chunk` sobre el fuente, mutante M23, y
`L-6: signo fuera de la fórmula` sobre el texto, mutante M24. El `−` literal lo vigila la FASE 2O, y la franja, `ORACULO_DATOS`.

## L-7 · La primera hora ancla la primera distancia, nunca el verbo «inició»

«Donde inició a las 6:00» se lee como la hora de salida, y con esa lectura cambia lo recorrido (3,64 % de las versiones
con veredicto invertido, H1 de la FASE 2C r1). Toda plantilla pone la hora antes de la distancia. Guarda L-7, mutante M14.

## L-8 · El procedimiento nunca muestra un negativo

Decisión del profesor del 2026-11-05 (O-1). `EV-D-SIGNO` y `EV-T-SIGNO` no se sortean como error cometido. Siguen en el pool como
razones falsas, que es donde las puso el impreso (sus opciones B y D). Con un negativo a la vista, la clave era «225 km y no −225 km»:
la regla `Q`, elegir la opción que solo quita el menos, acertaba el 100 % en esas ramas y el 45,83 % en total (+17,03 pp), sin calcular.
Guarda L-8 en `data_generation`, antes de las de L-1: ningún error de signo cometido, y distancia, tiempo y rapidez mostrados positivos.
Desde la r3, también operandos. Oráculo `ORACULO_L8`. Mutantes M17a1 (reintroduce el sorteo), M17a2 (sorteo y entrada en el `switch`), M17b (un resultado negativo) y M17d (un operando negativo),
uno por componente de la guarda, y M17c y M17e (sin la guarda, los caza el oráculo).

## L-9 · La clave no es la única cifra nueva y creíble

Decisión del profesor del 2026-11-05 (O-2). Hasta la FASE 2C r3, toda razón falsa de distancia o tiempo era un negativo, un dato repetido o una suma que
superaba el dato mayor, así que la clave se hallaba sin obtener nunca `D` ni `dur`. Por eso: si la versión admite `EV-D-PROM` o `EV-T-CUENTA`, ofrece al menos una.
La obligación es parte de la invariante: sin ella `EST` sin rótulos queda en +16,39 pp, y con ella en +4,53 pp (medido en `.Rmd` y modelo, mutante O2-A). Guarda
L-9 en `data_generation`, mutante M25; oráculo `ORACULO_L9`, mutante M25b.

## Clasificación

**Componente confirmado por el profesor el 2026-11-05.** Ver `ITEM_OFICIAL.md`. El componente es Numérico-variacional por el Marco 2019, aunque el clasificador propuso
Geométrico-métrico; el Estándar es el métrico porque es el único del EBC que nombra la velocidad media. El cruce es
deliberado.

## Presupuesto §P7-D

Las 3 pasadas de diagnosticidad se gastaron antes de la primera FASE 2C (r0 → r3). Lo que quede se declara como residuo.
Un defecto de CORRECCIÓN se corrige siempre: los de la FASE 2C r1 no consumieron presupuesto. Las únicas excepciones a ese presupuesto son
O-1 y O-2.

## O-2 · Autorización de la variante C y de la marca «− 0»

> **Autorizado por el profesor el 2026-11-05**, tras la FASE 2C r3 (`salida/fase2c_r3_detractor.md`): «Autorizo C y mantengo el − 0». Regla #24 H-5 y
> §P7-D: queda por escrito quién decidió, cuándo y sobre qué medición.

**Por qué.** La FASE 2C r3 mostró que `EST-afirmado-nuevo` contaba los rótulos «Paso n»: sin ellos da +22,15 pp, no +8,41, y la comparación con la que se eligió la
variante D se invertía. La causa de fondo es la de L-9.

**Excepción a §P7-D, una sola intervención.** Autoriza esto y nada más, con las condiciones exactas de `salida/medicion_exacta/opcion_c/modelo.py`:

1. `EV-D-PROM`, razón falsa de distancia: promediar las dos lecturas, `(d1 + d2) / 2`. Solo si es entera y distinta de `D`, `Dm`, `d1` y `d2`.
2. `EV-T-CUENTA`, razón falsa de tiempo: contar las horas del reloj incluyendo las dos puntas, `dur + 1`. Solo si es distinta de `t1`, `t2`, `Tm` y `dur`.
3. La obligación de L-9, aplicada a los conjuntos de razones después del filtro de signo y antes de los pesos de `RANGO_META`.

Las dos son solo razones falsas, nunca error cometido, y `repara()` las filtra como a las demás. No autoriza reajustar `RANGO_META`, `P_CANON`, L-2 ni los pesos, tocar
otras razones ni abrir otra variante.

**La marca `− 0` se mantiene.** `h/km` y `+` son constructo; `− 0` es constructo parcial: basta ver que se resta algo que no hace nada. Se declara así (H8 de la r3),
con `M0` y `RE` en los residuos. `M0` no cambia con ninguna variante y en el impreso marca 100 %.

**Esta autorización no firma ninguna cifra.** Toda regla que pase de +8 pp en el `.Rmd` final se lista una por una, con su línea de código, su glosa contrastada con esa línea
y su cifra exacta, y se firma en el paso 11, después del detractor. Hasta entonces el verificador sale con exit 3, pendiente de firma.

**Firma del profesor, 2026-11-05, paso 11:** «Firmo las 64 filas, acepto las 6 623 combinaciones como residuo declarado y apruebo.» Regla #24 H-5 y §P7-E: queda por escrito
quién decidió, cuándo, qué se midió y qué se pierde.

- **Firmado:** las 64 filas de `salida/tabla_firma_o2.md` (md5 `22f3e72e7535c0d7f05e2aa0631210f7`), 33 reglas, 31 en no canónicas y 33 en mezcla, sobre el `.Rmd` md5
  `904af1ca1271d97c54123218c9db03c0`. Cableado en `FIRMA_O2` con `FIRMADA <- TRUE`: dentro del alcance, «ROJO, listado por O-2», exit 4, nunca PASS. Con otro md5 del `.Rmd`, exit 3:
  la firma no se traslada a otro archivo.
- **Residuo declarado, sin cubrir:** las 6 623 de 96 568 conjunciones de 1 a 3 cláusulas que llegan a +8 pp (`salida/medicion_exacta/det4/busca.out`). No se cablean, porque §P7-E no
  admite tope por familia. Una regla nueva medida por encima de +8 pp **no** queda cubierta y vuelve al profesor.
- **Lo que se pierde.** `NM`: en los pasos 1 y 2 la clave siempre afirma menos de lo que rechaza, porque todo error cometido infla la cifra. `EV-T-CUENTA` se lee «N + 1 y no N» en el
  46,85 % de las versiones, y quien la descarta lleva `EST` de +4,53 a +15,75 pp. `RE`, `NM` y la familia `EST-1` quedan por encima del impreso. Ninguna cifra mide dificultad real: el
  Nivel 3 sigue sin evidencia de aula.

**Cifras previstas.** Modelo Python de la r3, que reproduce D a la centésima, no el `.Rmd` (`opcion_c/c2.json`). Exceso sobre 28,8 %, no canónicas: `EST` sin rótulos +4,53 ·
`X0` +5,15 · `X0+` +8,27 · `EST+` +5,36 · `E1` +6,65 · `E2` +17,51 · `E3` +24,49 · `entre` +6,70 · `RE` +21,20 sin la fracción del paso 3 y +14,95 con ella · `U` +13,72 ·
`NM` +10,62 · `M0` +46,20 · `M2` +51,20 · `M3` +61,88 · `X1a` +5,07 · `X1b` +15,51 · `X1` +29,43 · `R10-150` +12,32 · `R20-120` +19,58 · `R20-120+ent` +36,67 ·
`R30-90` +30,59. **Si el `.Rmd` no las reproduce a la centésima, se para, se busca la causa y se lleva al profesor: no se reajusta nada.**

## O-1 · Override firmado: canales por encima de +8 pp

**Sin efecto desde el 2026-11-05 (FASE 2C r3).** Se conserva como registro: sus cifras valen solo para el md5 `25a924d4…`, y una de ellas, `EST-afirmado-nuevo`, no medía lo que
dice su glosa. La vigente es O-2.

> **Firmado por el profesor el 2026-11-05**, tras la FASE 2C r2 (`salida/fase2c_r2_detractor.md`): «Autorizo quitar las ramas de signo,
> firmo esas 9 cifras y confirmo el componente». Regla #24 H-5 y §P7-E: quedan por escrito quién decidió, cuándo, qué se midió y
> qué se pierde.

**Excepción a §P7-D.** Las 3 pasadas estaban agotadas. El profesor autoriza una sola intervención más: retirar `EV-D-SIGNO` y `EV-T-SIGNO`
del procedimiento (L-8; variante D de `salida/medicion_exacta/exacto.out`). No autoriza reajustar `RANGO_META`, ampliar L-2 ni abrir
otra variante.

**Alcance, regla por regla.** Enumeración exacta de la variante D con sus pesos reales, sobre no canónicas salvo donde se indica. Exceso
sobre el techo nulo de 28,8 %. «Sustituir» es `sust_val()` de `verificar_espacio.R`: lo que afirma la opción reemplaza el resultado de su
paso, el otro paso queda como lo muestra el procedimiento y se recalcula la rapidez.

| Regla | Qué mide | Exacta | Exceso firmado |
|---|---|---|---|
| `X0` | la cifra afirmada no repite un dato del enunciado, horas incluidas | 43,29 % | +14,49 pp |
| `EST-afirmado-nuevo` | la cifra afirmada no aparece en el enunciado ni en los pasos | 37,21 % | +8,41 pp |
| `MAG-segundo-afirmado`, en la mezcla con `P_CANON` = 0,05 | la segunda cifra afirmada más grande | 37,67 % | +8,87 pp |
| `X1b` | sustituida, da rapidez entera | 41,22 % | +12,42 pp |
| `X1` | sustituida, da positiva, en km/h y entera | 57,83 % | +29,03 pp |
| `R10-150` | sustituida, da de 10 a 150 km/h | 47,38 % | +18,58 pp |
| `R20-120` | sustituida, da de 20 a 120 km/h | 58,95 % | +30,15 pp |
| `R20-120+ent` | la anterior, y además entera | 70,05 % | +41,25 pp |
| `R30-90` | sustituida, da de 30 a 90 km/h | 72,12 % | +43,32 pp |

Cableado regla por regla en `FIRMA_O1` de `verificar_espacio.R`, con la cifra exacta y no con muestra (§P7-E): dentro del alcance, «ROJO, cubierto
por O-1», exit 4, nunca PASS; por encima, pendiente de firma, exit 3. Los mutantes O1 a O9 sacan cada regla de «cubierto» sobre una copia del
resultado, y O1-S, O1-I, O1-R y O1-E mutan el generador y re-enumeran. Una regla nueva por encima de +8 pp **no** queda cubierta. `X1a` (+7,35 pp) y `Q` (no aplica) se vigilan y no están firmadas. **Si el `.Rmd` corregido no reproduce estas
cifras, o alguna regla pasa de su alcance, se para y se lleva al profesor: no se reajusta nada.**

**Por qué se acepta.** `X1a` y las reglas R premian comprobar si el resultado tiene sentido, lo que la propia Solution recomienda, y
exigen sustituir y calcular. `X0` y `EST-afirmado-nuevo` son de una misma familia: corregir un valor trae una cifra nueva, y `X0` el
impreso la trae con más fuerza (50 %). `X1b` es del generador, porque la clave siempre da entera, y es la única que entrena algo falso;
ampliar L-2 solo la bajaba 2,3 pp, a costa de 57 de las 97 filas del espacio y de subir `DIV-mult10` de +3,4 a +7,4 pp.

**Lo que se pierde.** Con `R20-120` queda solo la clave en el 26,2 % de las versiones (47,6 % con `R30-90`); en el impreso, nunca. La marca
«− 0» aparece en el 50 % de las versiones, y el paso 2 conserva un solo error cometido, `EV-T-FINAL`. Ninguna cifra de esta tabla
mide dificultad real: el Nivel 3 sigue sin evidencia de aula.

## Residuos vigentes (md5 904af1ca…, enumeración exacta)

Cifras de `verificar_espacio.R exacto`, `salida/verificar_r12.txt`: 9 564 estados y 131 586 conjuntos de razones con sus pesos reales, 0 abortos, techo 28,8 %. Valen solo para ese md5:
`RMD_MD5` del verificador da exit 3 si el `.Rmd` cambia.

**Firmada el 2026-11-05 (O-2).** `FIRMADA <- TRUE`: el verificador sale con exit 4, «ROJO, listado por O-2», nunca PASS (`salida/verificar_r13.txt`). Toda regla medida con exceso >= +8 está en `salida/tabla_firma_o2.md` y en `FIRMA_O2`, 64 filas. Lo que esa tabla no agota: las
6 623 conjunciones de hasta 3 cláusulas de la FASE 2C r4 (`salida/medicion_exacta/det4/busca.out`).

**Batería congelada (51 reglas).** La muestra de N = 100 por rol sigue imprimiéndose, pero no clasifica nada.

| Regla | No canónicas | Exceso | Mezcla 0,05 | Exceso |
|---|---|---|---|---|
| `DIV-mult10` | 36,54 % | +7,74 pp | 36,38 % | +7,58 pp |
| `FMT-empieza-km` | 33,97 % | +5,17 pp | 33,94 % | +5,14 pp |
| `MAG-segundo-afirmado` | 33,86 % | +5,06 pp | 37,16 % | +8,36 pp, en la tabla |
| `DIV-mult5` | 31,12 % | +2,32 pp | 31,23 % | +2,43 pp |

`FMT-parentesis` y `FMT-sin-parentesis` no aplican: sin negativos en el procedimiento no hay paréntesis. Quedan porque la batería está congelada.

**Marcas (O-2).** `M0`, elegir en el paso con la marca, da 75,00 %, +46,20 pp, y 100 % en el impreso: `h/km` y `+` son constructo; `− 0` es constructo parcial. `RE`, elegir la opción que rechaza un dato del enunciado, da 50,00 %,
+21,20 pp, y 43,75 %, +14,95 pp, contando la fracción del paso 3: es esa misma marca leída desde las opciones, y el impreso da 25 %. Las dos están en la tabla.

**Vigiladas por debajo de +8:** `entre` +6,70; `E1` +6,65 (mezcla +9,88, en la tabla); `EST+` +5,36; `X0` +5,15; `X1a` +5,07; `EST-sin-rotulos` +4,53. `Q` da 25,00 %: no aplica. `EST-afirmado-nuevo` cuenta los rótulos «Paso n»: la batería está
congelada y la lectura correcta es `EST-sin-rotulos`.

**Canónica (P = 0,05):** aísla la clave en `MAG-segundo-afirmado`, `MAG-corta`, `MAG-corta-margen15`, `POS-paso3`, `FMT-fraccion` y `LEX-cociente`. No se reescribe (L-3). Las razones nuevas no entran en ella.

**Longitud (N = 100 por rol):** la clave es la más larga en el 17 % y la más corta en el 20 %, con margen mediano de 7,7 %: 0 % explotable (§P7-B). Por rol llega a 51 % y 60 %, también con margen por debajo de 15 %.

**Dos causas estructurales, de la FASE 2C r4.** En los pasos 1 y 2 la clave afirma menos de lo que rechaza en el 100 % de los casos, porque todo error cometido infla el número: por eso L-9 no compite con `NM`. Y `EV-T-CUENTA` se lee «N + 1 y no N»
en el 46,85 % de las versiones, así que contra quien no calcula la defensa real de L-9 es `EV-D-PROM`.

Cada regla por rama, en la salida del modo `exacto`.
