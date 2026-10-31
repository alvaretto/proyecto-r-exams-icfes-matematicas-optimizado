# Invariantes locales — `totales-promedios-reciclaje-n3`

> Regla #17 / Incidente `INC-CLAUDE-LOCAL` (pre-flight 19 de `orquestador-schoice`). Estas invariantes
> **prevalecen sobre el criterio genérico** dentro de este subproyecto. Si alguna contradice una regla
> del repo raíz, gana la del raíz y el conflicto **se reporta**, no se resuelve en silencio.
> Las aprobó el profesor al encargar el ejercicio (2026-10-29), en el prompt de lanzamiento del
> orquestador. Ese mismo día, tras el dry-run, reescribió L-3 y añadió L-5; tras la FASE 2C firmó el
> override O-1 y la normalización de la redacción en L-1. Relajar cualquiera exige su firma (regla #24 H-5).

**Origen:** pregunta impresa **50** del simulacro SAI «Prueba Nivelación 6» de grado 10, Sesión 2
(MAT-2026-07-050). Recorte: `originales/orig_pregunta_50.png`; plana: `pagina_014.jpg` de Todo-Pajaro.
Texto verbatim, tabla celda a celda, clave y clasificación: [`ITEM_OFICIAL.md`](../ITEM_OFICIAL.md).
Hallazgos del dry-run y decisiones: [`HANDOFF.md`](../HANDOFF.md).
**Contrasta contra esos archivos y el recorte, no contra tu recuerdo.**

**Notación.** G = la sede que gana (mayor total) y P = la que pierde; *n* = cantidad de cursos,
*a* = promedio por curso, *T* = *n* · *a*, *D* = *T*_G − *T*_P (la cifra de la clave). En el impreso,
G = Primaria (5 · 40 = 200 kg), P = Secundaria (4 · 45 = 180 kg) y *D* = 20 kg.

**Ramas (L-3).** En toda versión, cursos y promedio van en sentidos opuestos y *T*_G > *T*_P:

- **Rama CURSOS**, la del impreso: gana la sede de más cursos (*n*_G > *n*_P, *a*_G < *a*_P).
- **Rama PROMEDIO**: gana la sede de mayor promedio (*n*_G < *n*_P, *a*_G > *a*_P).

## L-1 · Fidelidad: las opciones son afirmaciones sobre quién ganó y por qué

La pregunta es la original y las cuatro opciones son **afirmaciones** sobre qué sede ganó y por qué,
como en el impreso. Reparto obligatorio en **todas** las versiones y en las dos ramas:

- dos afirmaciones dan el triunfo a G: la clave, con *D*, y un distractor con una cifra falsa;
- dos se lo dan a P: una con la única ventaja que P sí tiene en la tabla (el mayor promedio en la rama
  CURSOS; la mayor cantidad de cursos en la rama PROMEDIO) y otra con una razón que la tabla no sustenta.

Con ese reparto, ni el nombre de la sede ni «la opción distinta de las otras tres» delatan la clave.
**Prohibido** el patrón `analisis_error`, que cambia las opciones por descripciones de errores
conceptuales. La capa metacognitiva obligatoria (regla #1) va en la **Solution**: diagnóstico de cada
distractor (qué lectura de la tabla lleva a elegirlo), reflexión y estrategia.

**Instancia canónica verbatim** (rama CURSOS; probabilidad en L-5). Reproduce exactamente el impreso:
enunciado, tabla y las cuatro opciones de `ITEM_OFICIAL.md`, con la clave en el **contenido** de la opción
B del impreso («La sede primaria ganó el concurso porque participaron más cursos y aunque el promedio fue
menor, el total fue de 20 kilos más.»). La letra no se fija: con opciones de texto, `exshuffle: TRUE`
(`codigo-rmd` #6), y la Solution identifica la clave por contenido (regla #19). La canónica conserva las
rarezas del impreso («recogido» en el enunciado y «recolectado» en el encabezado; «la sede de secundaria»
en A; «ganó porque», sin «el concurso», en C). Las versiones no canónicas **no** heredan esas rarezas: cada
plantilla usa un solo verbo («recogido» o «recolectado») en el enunciado y en el encabezado (decisión del
profesor, 2026-10-29, tras la FASE 2C). Las demás versiones varían datos y contexto (regla #11:
mínimo 6 plantillas, sin el patrón «Un(a) [oficio], [nombre], registró…») y conservan la estructura de
`ITEM_OFICIAL.md` § Regla de uso.

Pool de errores (≥ 4, regla #1), anclado en las justificaciones de la ficha:

| Lectura errónea | Origen | En la canónica |
|---|---|---|
| Toma un dato aislado de la tabla como diferencia entre totales | ficha (A) | A: «40 kilos más» (40 es el promedio de G) |
| Compara solo los promedios, sin considerar que el número de cursos es distinto | ficha (C) | C: «mayor promedio con 5 kilos de más» |
| Inventa una explicación que no sale de la tabla (solo hay promedios por sede) | ficha (D) | D: «un curso que recogió más que los otros» |
| Compara solo la cantidad de cursos, sin considerar los promedios | espejo de la de C, para la rama PROMEDIO | — |
| Al menos una más, de diseño propio | — | — |

Ojo con A: la ficha empieza su justificación con «resta los promedios», pero esa lectura da 5 kg (la
cifra de C), no 40. El 40 del impreso sale de «toma un dato aislado».

Guardias en `data_generation`:

- un `stopifnot` que rechace opciones con vocabulario de error («estudiante», «error», «confundió»,
  «olvidó»…);
- un `stopifnot` del reparto 2 + 2: exactamente dos opciones nombran a G como ganadora y dos a P;
- un `stopifnot` de la canónica: frases y celdas comparadas contra literales de `ITEM_OFICIAL.md`.
  Esa guarda mira `tabla_df`; desde la ronda 4, `verificar_espacio.R` repite la suma de control sobre la
  tabla que se muestra (`tabla_md`, sin el relleno, la negrita ni el espacio duro), con el mutante M20.

## L-2 · Exactamente una afirmación verdadera por versión

Con datos aleatorios pueden quedar dos verdaderas. Ejemplo: G = 5 cursos × 36 kg = 180 kg y
P = 3 cursos × 48 kg = 144 kg; la diferencia es 36 kg, así que A («recogió 36 kilos más», con el
promedio de G) sería verdadera junto con B. Los validadores de Nivel 5 (regla #13) comparan valores; no
evalúan si una afirmación es verdadera. Por eso:

1. Cada afirmación lleva sus datos estructurados (sede que nombra como ganadora, cifra que cita, razón)
   y una **función de verdad pura** (sin `sample()` ni estado) que la evalúa con los datos de la versión.
   Guardia en `data_generation`: `stopifnot(sum(verdaderas) == 1, verdaderas[clave])`.
2. Desigualdades estrictas: *T*_G ≠ *T*_P, *a*_G ≠ *a*_P y *n*_G ≠ *n*_P. La cifra que cita el
   distractor del lado de G nunca es *D*.
3. **Trampa propia de la rama PROMEDIO:** ahí la frase «G ganó porque, aunque participaron menos cursos,
   tuvo mayor promedio con … kilos de más» es **verdadera**, así que no puede ser distractor del lado de G.
   La función de verdad la detecta; el filtro es por texto, no por código de error.
4. La unicidad se **construye** por diseño (familia F1: nada de remuestrear hasta que aparezca) y se
   verifica **por rama** con un script propio del subproyecto que recorre **todo** el espacio de
   parámetros si es finito (enumeración exacta, regla #23); si no lo es, con N = 100 semillas por rama.
5. El script incluye un mutante que revierte la construcción (no solo el `stopifnot`) y **debe fallar con
   él**: una invariante que ningún mutante mata es documentación, no guardia.

## L-3 · La trampa se conserva en la canónica y se alterna en las demás versiones

> **REESCRITA el 2026-10-29. Decisión del profesor**, tomada en la pausa conjunta previa al modo
> `ejecutar`, tras el dry-run del orquestador (§ 6.1 de su reporte, resumido en `HANDOFF.md`). Regla #24
> **H-5** (relajar nunca es autónomo): queda por escrito quién lo decidió, cuándo, qué se midió y qué
> variante se descartó.

**Lo que rige.**

- La **canónica** va siempre en la rama CURSOS, verbatim: la sede de mayor promedio pierde porque tiene
  menos cursos.
- Las **versiones no canónicas** sortean la rama: unas veces gana la sede de más cursos (rama CURSOS) y
  otras la de mayor promedio (rama PROMEDIO). En las dos hay que calcular los totales para responder.
- **Prohibida la dominancia**, es decir, una sede con más cursos **y** mayor promedio: se resolvería sin
  calcular.
- El reparto entre ramas se calibra para que la frecuencia global de la rama CURSOS, canónica incluida,
  quede cerca de 50 %, y se mide con N = 100.
- Guardias en `data_generation`, que se cumplen por construcción (F1):
  `stopifnot((n_G - n_P) * (a_G - a_P) < 0, T_G > T_P)` y, si `es_canonica`,
  `stopifnot(rama == "CURSOS")`.

**Por qué.** Con la versión anterior de L-3 (gana siempre la sede de más cursos y menor promedio), quien
ve varias versiones aprende que «la sede de mayor promedio nunca gana» y descarta sin calcular las dos
opciones que dan el triunfo a P. El acierto al azar pasa de 25 % a 50 %, una cota por construcción de
+25 pp, por encima de los +8 pp que obligan (§P7-A). Ninguna sonda automática lo ve: H2 y H3 quedan
ciegas por el prefijo uniforme «La…», y H3b da PASS al sortearse el nombre de la sede. Es el caso de
§P4-bis (veredicto de la clave invariante) y de la defensa que prescribe el pre-flight 12c para ítems
oficiales: la canónica conserva el veredicto y las demás alternan.

**Lo que se pierde.** En las versiones de la rama PROMEDIO la trampa del impreso se invierte: el error
que cuesta es comparar solo los cursos, no solo los promedios. La competencia (Argumentación, `Arg_E1`) y
el descriptor D3.10 no cambian: en las dos ramas la afirmación verdadera se justifica con los totales.

**La variante descartada.** Mantener la L-3 anterior y declarar el canal como residuo con override
firmado (§P7-E). Se descartó porque, en un banco de práctica con versiones al azar, el ítem entrenaría el
atajo que debe penalizar.

**Costo aceptado.** La clave alternativa invalida parte de lo que sirve para una sola clave: los
distractores se escriben y se miden **por rama** (H1 por rama; el agregado no acredita).

Permitido, porque no relaja nada: sortear qué sede (o qué conjunto, según el contexto) es G y en qué
fila de la tabla va.

## L-4 · Clasificación literal de la ficha y Nivel 3 sin inflar el DOK

- `exextra` con los valores literales de `ITEM_OFICIAL.md` § Clasificación oficial, cotejados contra el
  catálogo canónico: Competencia `Argumentación`, Componente `Aleatorio`, Nivel `3`, Descriptor D3.10,
  grado sugerido `6°-7°`, y la Afirmación, la Evidencia (`Arg_E1`) y el Estándar copiados carácter a
  carácter (regla #7: los valores de `exextra` llevan tildes; las claves, no).
- Nombre del `.Rmd`: `totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd`.
- Si el análisis del paso 1 clasifica distinto, se reporta como discrepancia de CONTENIDO y no se
  resuelve por cuenta propia.
- El DOK se mide con el verbo real de la tarea y nunca se mueve para cuadrar con el Nivel
  (`DOK ≥ 3 ⇒ Nivel ≥ 3`; la recíproca es falsa). Si hubiera que cambiar el Nivel o la ranura `_n3_`
  del nombre, se pregunta al profesor.

## L-5 · Probabilidad de la canónica: 0,10

> **Decisión del profesor, 2026-10-29.** La canónica reproduce el impreso que 10A y 10B respondieron en
> papel el 2026-07-22. Sale con probabilidad 0,10, igual que la pregunta 32 de esa misma sesión y esos
> mismos grupos (`../probabilidad-fichas-tabla-doble-entrada-n2/`, `P_CANON <= 0.10`).

Guardia en `data_generation`: `P_CANON <- 0.10`, `stopifnot(P_CANON <= 0.10)` y
`es_canonica <- sample(c(TRUE, FALSE), 1, prob = c(P_CANON, 1 - P_CANON))`. Los residuos de la canónica
(§P7-E) se declaran con su cifra y con su peso en la mezcla (`P_CANON` × exceso).

## O-1 · Override firmado: atajo de la razón sin sustento (H3 de la FASE 2C)

> **Firmado por el profesor el 2026-10-29**, en la pausa posterior a la FASE 2C (detractor `RECHAZAR`,
> hallazgo H3, ALTA, diagnosticidad; reporte íntegro en `salida/fase2c_r1_detractor.md`). Regla #24 **H-5**
> y §P7-E: queda por escrito quién lo decidió, cuándo, qué se midió y qué se pierde.

**El canal.** Por L-1, la razón sin sustento nombra siempre a la sede que pierde, y la frase «todos sus
cursos» siempre a la que gana. Quien vio varias versiones descarta así las dos opciones de P sin calcular:
el acierto pasa de 25 % a 50 %, y el detractor le atribuye un exceso de unos **+16,7 pp**, por encima de los
+8 pp que obligan (§P7-A).

**Por qué se acepta.** §P7-D está agotado: se usaron las 3 pasadas. Además, el defecto de corrección de esa
misma versión (H1) nació de una medida de diagnosticidad. La alternativa, una configuración espejo en las
versiones no canónicas, habría sido una 4.ª pasada y habría exigido enumerar antes el espacio (§P7-F).
También habría atribuido una razón sin sustento a la sede que sí gana, con riesgo de que se lea como
verdadera.

**Lo que se pierde.** El atajo queda abierto, y la batería §P7 queda en rojo para este canal (§P7-E).
~~Aun con el atajo, elegir entre las dos opciones de G exige calcular *D*.~~

**Premisa FALSA, medida en la ronda 2 (2026-10-29).** Sumando al atajo otra pista superficial se acierta sin
calcular *D*: hasta el 74,0 % en la rama CURSOS (S_U con S1) y el 69,5 % en la rama PROMEDIO (S_U con la
regla «suma»), con N = 100; entre el 71 % y el 79 % con 2 500 semillas. La frase se conserva tachada, no se
borra. O-1 sigue cubriendo solo S_U (+16,7 pp, dentro de su alcance). El atajo combinado no está firmado: se
mide en la variante C y vuelve al profesor en el WAIT_USER #3 de ese ciclo.

**Alcance.** El override cubre hasta **+16,7 pp**, la cifra medida en la versión que rechazó la FASE 2C. Se
vuelve a medir después del ciclo de corrección: si la cifra sube, o si aparece otro canal por encima de
+8 pp, vuelve al profesor. No cubre ningún defecto de CORRECCIÓN, que sigue siendo `RECHAZAR`.

### O-1 corregido: alcance nuevo (firmado el 2026-10-29, WAIT_USER #3 de la ronda 3)

> **Firmado por el profesor** al aprobar para el aula la versión md5 `cd94b59adfdea7b9c92dc074ce0a1435`
> (variante C; FASE 2C de la ronda 3 `APROBAR_CON_CAMBIOS`, reporte en `salida/fase2c_r3_detractor.md`).
> Sustituye el alcance anterior («hasta +16,7 pp»), cuya premisa quedó tachada más arriba.

**Alcance firmado.** Atajos que aciertan sin calcular ningún total, con N = 100 por rama, medidos el
2026-10-29 sobre ese md5:

| Atajo | CURSOS | PROMEDIO |
|---|---|---|
| S_U («la sede de la razón sin sustento pierde») | 50,0 % (+16,7 pp) | 50,0 % (+16,7 pp) |
| S_U + S1 (descartar la cifra que está en la tabla) | 71,0 % | 55,0 % |
| S_U + suma (descartar la cifra que es suma de otras dos) | 60,5 % | 62,0 % |
| S_U + las dos pistas | 79,0 % | 67,0 % |
| S_U + cifra atípica (ERR-TOTAL dobla a las otras cifras) | 51,5 % | 58,0 % |
| S_U + las tres pistas | **81,5 %** | **74,5 %** |

En la mezcla, con las tres pistas, el acierto es del 77,6 % (el detractor midió el 77,3 %). Son reglas
discretas, sin margen en el sentido de §P7-B.

**Residuos firmados en la misma decisión:**

- S1 en la rama PROMEDIO: +8,4 pp con N = 100 (de +7,0 a +7,4 pp en depuración), en zona gris.
- ERR-TOTAL leído como «total», es decir, «en *x* kilos» tomado por la cantidad: lo volvería la única
  verdadera en el 17-21 % de las versiones. El detractor y el paso 8 juzgan que no es una lectura razonable,
  porque es el complemento del comparativo, y ese desliz es justo el error que ERR-TOTAL diagnostica. No es
  CORRECCIÓN.
- Peso de la canónica en la mezcla: 0,10 × 75 pp = +7,5 pp (enumeración exacta).
- Clave canónica verbatim: «el total fue de 20 kilos más» admite la lectura «el total fue de 20 kilos»,
  que la volvería falsa sin volver verdadera otra opción. Se conserva por H-2 (regla #13 v1.2: en la
  canónica prevalece el impreso).

**Declarados sin firma**, porque están por debajo de +8 pp o son inexplotables:

- R_dif: +5,2 pp en CURSOS y +6,1 pp en PROMEDIO.
- El canal de largo: +10,4 pp en PROMEDIO, con un margen máximo del 2,0-2,7 % en caracteres (§P7-B).

**Alcance.** Si una medición posterior supera cualquiera de estas cifras, o aparece otro canal por encima
de +8 pp, vuelve al profesor (regla #22 §P7-E, alcance del override). No cubre ningún defecto de
CORRECCIÓN.

**Guarda ejecutable (ronda 4, H1 de la FASE 2C r4).** `verificar_espacio.R` lleva estas cifras como
`FIRMA_O1`, `FIRMA_S1_PROMEDIO` y `FIRMA_LARGO`, y las compara redondeadas a un decimal, como se firmaron.
Hasta lo firmado, el canal sigue EN ROJO y el verificador lo reporta «ROJO, cubierto por O-1 corregido»
(exit 4), nunca PASS: la regla #22 §P7-E deja la batería en rojo bajo un override. Por encima, PENDIENTE
DE FIRMA (exit 3). El mutante M19 fuerza ERR-TOTAL y debe sacar alguna cifra de lo firmado. Antes de la
ronda 4 el verificador comparaba con el 50 % de la premisa tachada y salía siempre con exit 3, así que no
distinguía «dentro del alcance» de «lo superó».

## CLOZE hermano (`cloze/`, desde el 2026-10-30)

> **Firmado por el profesor el 2026-10-30**, en la pausa posterior al dry-run del `orquestador-cloze`
> (pre-flight 26/26, `VEREDICTO_ORQUESTADOR: dry_run`). Respuesta literal: «1 n · 2 A, con tabla e hipótesis ·
> 3 3|1|1|1|1|1 · 4 firmo F-1 a F-4». Regla #24 **H-5**: queda por escrito quién decidió, cuándo, qué se midió
> y qué variantes se descartaron. Estado y evidencia del CLOZE: [`cloze/HANDOFF.md`](../cloze/HANDOFF.md).

`cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd` tiene su propio
`cloze/ejercicio_state.json`. Sus seis partes son `schoice|schoice|num|num|num|mchoice`, y la Parte 1 es este
SCHOICE.

- **L-1 a L-5 y «O-1 corregido» rigen la Parte 1 del CLOZE.** Por código, la Parte 1 es el SCHOICE: su bloque
  `data_generation` se trasplanta con un script, con tres renombres (`opciones_p1`, `sol_p1`, `feedback_p1`) y
  sin tocar su consumo del RNG. La guarda es E1 de `cloze/verificar_espacio_cloze.R`: objetos y `.Random.seed`
  iguales en 100 semillas más 20 canónicas. Un defecto que se vea en la Parte 1 se reporta; no se corrige ahí.
- **L-1 a L-3 se endurecen en las Partes 2 a 6** (endurecer es autónomo; solo se registra): sin vocabulario de
  error, una sola clave en la Parte 2 y sin dominancia en el caso paralelo.

### F-1 · Flujo B = n, heredado del SCHOICE

Tablas Markdown con `tabla_responsiva()` y la guarda `\newcounter{none}`, también la del otro colegio. Se
pierde solo el naranja de los encabezados, como en el SCHOICE. H-1: el JPG muestra una tabla 3 × 2 sin celdas
combinadas.

### F-2 · «O-1 corregido» se extiende por alcance a la Parte 1 del CLOZE

- Mismas cifras: `FIRMA_O1`, `FIRMA_S1_PROMEDIO` y `FIRMA_LARGO` de `verificar_espacio.R`.
- Condicionada a que E1 salga en verde y a que E4 (los canales de O-1 vueltos a medir sobre la Parte 1, por
  rama) quede dentro de lo firmado.
- Todo canal cruzado (de las Partes 2 a 6 hacia la Parte 1) por encima de +8 pp vuelve al profesor.
- No cubre ningún defecto de CORRECCIÓN.

### F-3 · Pesos `expoints: 3|1|1|1|1|1`

- La pregunta vale 8 en Moodle, y la Parte 1, 3/8.
- Se firmó con el costo medido a la vista (dry-run): con el atajo firmado (77,6 % de acierto sin calcular,
  frente al 25 % del azar), la Parte 1 da +19,7 pp de la nota de la pregunta sobre el azar. Las alternativas
  medidas eran 2|1|1|1|1|1 (+15,0 pp) y 1|1|1|1|1|1 (+8,8 pp).
- Guardas: E7 de `cloze/verificar_espacio_cloze.R` y la guarda de pesos de `cloze/exportar_moodle_cloze.R`.
  R/exams recicla `expoints` sin avisar si faltan entradas. Cambiar los pesos exige otra firma.

### F-4 · Estructura: Opción A del dry-run

- **Qué.** Justo después de `##ANSWER1##`: «Las Partes 2 a 6 se refieren a otro colegio, donde el concurso se
  hizo entre dos grados», y una segunda tabla hecha con la misma `tabla_responsiva()`: grados de sexto a
  noveno, el mismo material y un solo verbo, «recogido».
  - Parte 2 (schoice): qué indica el «promedio por curso» o qué hay que comparar para saber quién gana. Sin
    cifras ni nombres de sede; clave conceptual fija, declarada bajo V8, con paráfrasis.
  - Partes 3 y 4 (num): el total de cada grado, en un orden de mención sorteado.
  - Parte 5 (num): cuántos kilos más recogió el grado que ganó, sin nombrarlo.
  - Parte 6 (mchoice): cuatro afirmaciones sin cifras sobre el otro colegio, con «puede haber más de una»,
    entre ellas las hipótesis «el que perdió, con un curso más, habría ganado» (verdadera si *a*'_P > *D*') y
    «el que ganó, con un curso menos, habría perdido» (verdadera si *a*'_G > *D*'). El veredicto se sortea
    primero.
- **Subdecisiones:** (a) el otro colegio va en una tabla, no en prosa; (b) la Parte 6 incluye hipótesis.
- **Lectura firmada de `ITEM_OFICIAL.md` § Regla de uso:** «sin añadidos de contexto que el cuadernillo no
  menciona» se refiere a la Parte 1; en la canónica, las Partes 2 a 6 se añaden después.
- **Por qué.** La tabla oficial tiene cuatro números, y todo cálculo útil con ellos decide la Parte 1 (*D*,
  *T*_G, *T*_P) o ya está impreso en sus opciones (δ, *a*_G, *a*_P, las ventajas parciales): pedirlos sería la
  fuga del Incidente Y (V11).
- **Medido en el dry-run.** Sin sortear primero el veredicto, las hipótesis son verdaderas en el 68-85 % de las
  versiones. En el peor estrato (Parte 1 en PROMEDIO × otro colegio en PROMEDIO) quedan como mínimo 2
  combinaciones paralelas, y con el veredicto sorteado primero 1-2 de 111 versiones se quedan sin candidatos:
  cascada F1 (primero se amplía el espacio paralelo, *n*' hasta 10 y *k*' ≤ 4; después se cambia de plantilla),
  con cada caso contado y declarado. Ninguna exclusión de V11 se relaja.
- **Variantes descartadas:** B (todo sobre la tabla oficial, sin cifras: sin paso de calcular y con claves de
  lectura directa); C (descomponer *T*_G, *T*_P y *D*: es la fuga por valor); la pregunta oficial como Parte 6
  (Moodle muestra las seis partes a la vez).

**Nota que completa F-4** (añadida el 2026-10-30, tanda 2). La tabla del dry-run que el profesor firmó decía de
la Parte 6, y su registro de arriba lo omitía: «No nombran sedes de la Parte 1. Al menos una verdadera y una
falsa, sin pares complementarios, y el veredicto se sortea primero».

### Decisiones del profesor tras la tanda 1 (2026-10-30)

> **Firmadas por el profesor el 2026-10-30**, en la pausa posterior a la tanda 1 del `orquestador-cloze` (pasos
> 0 a 6b sobre el md5 `e58f3df91e31121ea66c2be0b0506c50`). Respuesta literal: «1 a · 2 a · 3.1 a · 3.2 a · 4 sí».
> Regla #24 **H-5**: queda por escrito quién decidió, cuándo y con qué cifras.

1. **(1 a) La Parte 6 queda aprobada tal como está.** Lleva HP, HG, una afirmación de «ventaja» (cuatro
   redacciones) y «Ganó el concurso el grado X». La de ventaja y la del ganador llevan siempre el mismo veredicto,
   para no formar un par complementario. Los veredictos se sortean primero, y siempre hay al menos una verdadera
   y una falsa. No entran las afirmaciones conceptuales que el dry-run daba de ejemplo. Cifras de la tanda 1
   (330 versiones): verdaderas hp 48,8 %, hg 47,9 %, ventaja 51,8 % y ganador 51,8 %; por rama del otro colegio,
   hp 60/38 % y hg 26/69 % (CURSOS/PROMEDIO; diseño: 1/3 o 2/3); 1, 2 o 3 verdaderas en 53, 223 y 54 versiones.
2. **(2 a) Los BAJA 1, 2 y 4 de la auditoría 6b, que están en la Parte 1, quedan como residuo declarado del
   CLOZE.** BAJA 1: «en 15 / kilos.» partido en opciones y Solution. BAJA 2: «El / Bosque» y «La / Pradera»
   partidos fuera de la tabla (semillas 6 y 13). BAJA 4: «ganará»/«ganó» en 8 de 24 contextos (6b con N = 24). No
   se toca la Parte 1 ni el texto que emite. Los espacios duros pasan al próximo ciclo del SCHOICE (ver abajo).
3. **(3.1 a) Aclaración de F-2 sobre los canales cruzados de más de +8 pp.** Si el canal nace de la redacción
   nueva (Partes 2 a 6) y se cierra endureciendo esa redacción, sin tocar la Parte 1 ni relajar ninguna
   exclusión, no hay que parar. Se reporta con su antes y su después, y cuenta como pasada de §P7-D. Solo vuelve
   al profesor si queda abierto.
4. **(3.2 a) Override firmado: la regla cruzada X9 en la canónica.** +25,0 pp con n = 30; peso en la mezcla
   0,10 × 25 = +2,5 pp. Queda firmada como residuo. Está cubierta por dominancia: en la canónica, el atajo ya
   firmado de «O-1 corregido» (0,10 × 75 pp) acierta el 100 %. La enumeración exacta de la tanda 2
   (`cloze/salida/x_canonica_exacta.R`) da la misma cifra: 75,0 %, +25,0 pp.
5. **(4 sí) `render_html_shots.R` se arregla en una tarea aparte.** Con Chromium 156, `--window-size` por debajo
   de 500 da una ventana de 500 px: ese script no sirve para capturas de 320 o 360 px. Para una captura estrecha,
   CDP `Emulation.setDeviceMetricsOverride` (`cloze/salida/6b/capturar_cdp_6b.py`), comprobando `innerWidth`.

### Leyes del CLOZE (tanda 2, 2026-10-30)

Salen de las decisiones de arriba. Cada una tiene su guarda ejecutable y un mutante que la mata por su propia
sonda (contrato de mutación del orquestador). Los mutantes viven en `cloze/verificar_espacio_cloze.R` (E6).

**L-C1 · Parte 6 (decisión 1 a).**

| Exige | Guarda | Mutante |
|---|---|---|
| Solo HP, HG, una de ventaja y la del ganador; ninguna conceptual | `stopifnot` «Parte 6: HP, HG, una de ventaja y la del ganador, nada más» | MC16: la del ganador se cambia por una afirmación conceptual |
| La de ventaja y la del ganador, con el mismo veredicto | `stopifnot` «Parte 6: sin pares complementarios» (`v_d == v_c`) | MC11: `v_d` se sortea aparte |
| Al menos una verdadera y una falsa | `stopifnot` «Parte 6: al menos una verdadera y una falsa» | MC5 |
| La marca sale del texto y de la tabla (V10) | `stopifnot` «V10: la marca de cada afirmación…» | MC6 |
| El veredicto se sortea primero | E3: hp y hg verdaderas en el 50 % ± 12 | MC13: veredicto calculado después de los datos |
| Sin cifras ni nombres de la Parte 1 | `stopifnot` de V11 por valor y por contenido | MC3 (contenido) |

**L-C2 · Canales cruzados de las Partes 2 a 6 (decisión 3.1 a).** Un canal cruzado de más de +8 pp que nazca de
la redacción nueva se cierra endureciendo esa redacción, con su antes y su después, y gasta una pasada de §P7-D
(Partes 2 a 6: 1 de 3 usada, en la tanda 1). Si queda abierto, vuelve al profesor.

| Exige | Guarda | Mutante |
|---|---|---|
| Un canal abierto de más de +8 pp no se cierra solo: queda pendiente de firma | E3 de `verificar_espacio_cloze.R`: PENDIENTE DE FIRMA, exit 3 | MC17: la rama del otro colegio copia la de la Parte 1 (abre X8) |
| No se toca la Parte 1 | E1 | MC14 y MC15 |
| No se relaja ninguna exclusión | `stopifnot` de V11 por valor | MC1 y MC2 |

**L-C3 · Override en la canónica: X9 (decisión 3.2 a), ampliado a X6, X10 y X11 (firma del 2026-10-30, tras la
tanda 2).** `verificar_espacio_cloze.R` lleva el alcance firmado **regla por regla** como `FIRMA_CANON`, con las
cifras de la **enumeración exacta** de la canónica, y lo compara redondeado a un decimal, como se firmó. La puerta
es `cloze/enumerar_canonica_cloze.R` (tanda 3): enumera los sorteos que ejecuta el propio código, no una
transcripción del modelo, y da las once reglas cruzadas; su resultado coincide con `cloze/salida/x_canonica_exacta.R`
en X6 a X11 y no depende de la semilla canónica. Dentro del alcance, cada regla sigue EN ROJO: «ROJO, cubierto por
el override de la canónica», exit 4, nunca PASS (regla #22 §P7-E). Por encima, o si cualquier otra regla supera
+8 pp en la canónica, PENDIENTE DE FIRMA (exit 3). La muestra de n = 30 canónicas se imprime solo como registro.

| Regla en la canónica (exacta) | Acierto | Exceso firmado | Peso en la mezcla | Mutante: sale de «cubierto» |
|---|---|---|---|---|
| X6 más palabras en común con la clave de la Parte 2 | 41,7 % | +16,7 pp sobre 25 % | +1,67 pp | MC19: una clave «comparar» de la Parte 2 gana «aunque»; X6 +25,0 pp |
| X9 S_U + la que nombra la ventaja del grado ganador | 75,0 % | +25,0 pp sobre 50 % | +2,50 pp | MC18: la canónica siempre con el otro colegio en CURSOS; X9 +50,0 pp (salen también X8 +25,0 y X11 +25,0) |
| X10 S_U + más palabras en común con la clave de la Parte 2 | 66,7 % | +16,7 pp sobre 50 % | +1,67 pp | MC20: una clave «comparar» gana «cantidad» y «curso»; X10 +25,0 pp |
| X11 S_U + más palabras en común con las verdaderas de la Parte 6 | 62,5 % | +12,5 pp sobre 50 % | +1,25 pp | MC21: las dos redacciones de ventaja «por promedio» ganan «total»; X11 +25,0 pp |

Las otras siete reglas, exactas en la canónica: X1 +0,0, X2 +0,0, X3 −1,0, X4 +0,3, X5 −25,0, X7 −16,7 y X8 +0,0 pp.

Integridad de la puerta (H4 de la FASE 2C r1, 2026-10-31): cada hoja de la enumeración cuenta las llamadas de cada
sorteo libre y falla si no son las esperadas, porque un sorteo nuevo con el texto de uno libre pasaría sin ramificarse.
Mutante: MC22 reescribe `v_c` con el texto de un sorteo libre; la cuenta lo detecta (1/3 de la masa con error).

Redacción de la tanda 2, sustituida en la tanda 3 (`FIRMA_X9_CANON`, con la muestra de n = 30, ya no existe):

~~**L-C3 · Override X9 en la canónica (decisión 3.2 a).** `verificar_espacio_cloze.R` lleva el alcance firmado
como `FIRMA_X9_CANON` (+25,0 pp con las 30 canónicas de la muestra con que se firmó) y lo compara redondeado a un
decimal. Dentro del alcance, X9 sigue EN ROJO: «ROJO, cubierto por el override X9», exit 4, nunca PASS (regla #22
§P7-E). Por encima, o si otra regla cruzada supera +8 pp en la canónica, PENDIENTE DE FIRMA (exit 3). Mutante:
MC18 deja la canónica siempre con el otro colegio en la rama CURSOS; X9 sube al 100 % (+50 pp) y sale de
«cubierto».~~

~~**Fuera del alcance de L-C3 (pendiente de firma desde la tanda 2, no aceptado).** En la canónica, con
enumeración exacta: X6 +16,7 pp, X10 +16,7 pp y X11 +12,5 pp (pesos en la mezcla de 1,67, 1,67 y 1,25 pp). Con
las 30 canónicas de la muestra: +8,3, +11,7 y +16,7 pp. La tanda 1 declaró solo el máximo (X9). Detalle en
`cloze/HANDOFF.md`.~~

**Firmado el 2026-10-30** por el profesor, tras la tanda 2. Respuesta literal: «a · aclaración sí». X6, X10 y X11
entran en L-C3 con sus cifras exactas (tabla de arriba y «Decisión del profesor tras la tanda 2», abajo). Las cifras
de la muestra de n = 30 se conservan solo como registro: no son la puerta.

**L-C4 · Batería CONGELADA por gap de las Partes 2 a 6 (tanda 4, 2026-10-31; decisión «1 a» del profesor).** La batería
es `cloze/bateria_partes_2_a_6.R`, versión «L-C4 v1», md5 `de20e0a986f86d44f0d3412517d2f07b`. Se congeló tras su línea base
(las sondas de H1 y H2 salen en rojo sobre el md5 `96cfcf35…`) y antes de medir el reparto (§P7-C). Cambiar una regla es
hacer otra batería y volver a medir todo el histórico. Puntuación y nulo, declarados en su cabecera. Parte 2: 1/|S| y nulo
exacto 25 %. Parte 6, por afirmación: nulo exacto p·q + (1 − p)(1 − q), como si el veredicto se permutara entre versiones.

| Exige | Guarda (E8 de `cloze/verificar_espacio_cloze.R`) | Mutante |
|---|---|---|
| La batería es la congelada | md5 comprobado; si no coincide, FALLA | — (integridad) |
| Parte 2. 20 reglas: léxico (incluidas las tres de H1), formato, posición, relacional y estímulo (la pregunta, las Partes 3 a 5, los encabezados de la tabla). Magnitud, divisibilidad y signo no aplican: sus opciones no llevan cifras (V11) | Enumeración exacta, 720 versiones por tipo, para todas las reglas salvo posición. Posición: sobre la muestra, porque depende del sorteo `o2`. Una regla > +8 pp queda PENDIENTE (exit 3) | MC23: la clave siempre primera. P2-P1 pasa a rojo |
| Parte 6. 34 reglas por afirmación: estímulo y magnitud (*k*′, δ′, los cursos y el promedio menores, entre ellas R_k, R_hg y R_δ de H2), divisibilidad, posición, formato y léxico. Signo no aplica. Relacionales: RL1 (par HP = HG, métrica del par) y RL2 (la contraria a «Ganó…», condicional al ganador). Control positivo al 100 % | Por estrato: mezcla, rama del otro colegio y *k*′. Una regla > +8 pp que supera el p95 del máximo con el veredicto permutado queda PENDIENTE. Por debajo de ese p95, NO CONCLUYENTE: residuo declarado | MC24: el ganador siempre en la fila 1. P6-P1 pasa a rojo |
| Partes 3 a 5 (num): ninguna regla de eliminación aplica (sin opciones y con tolerancia 0) | V11 por valor y «a un paso» (E3) | MC1, MC2 |

**L-C3 · ampliación (decisión 2 a del 2026-10-31): la canónica de la Parte 1, regla por regla.** E4 de
`cloze/verificar_espacio_cloze.R` guarda las 24 cifras exactas de la decisión, redondeadas a un decimal: `FIRMA_P1_CANON`
(12 reglas de la batería del SCHOICE) y `FIRMA_P1_SONDAS` (12 sondas de E4).
- Dentro de lo firmado: «ROJO, cubierto por la decisión 2 a» (exit 4), nunca PASS.
- Por encima, o con cualquier otra regla o sonda > +8 pp en la canónica: PENDIENTE (exit 3).
- Mutantes, en E8, sobre copias del registro canónico, porque la guarda de literales aborta cualquier mutante en el bloque
  P1. MP1 cambia «tuvo mayor promedio» por «tuvo mayor media», y «quedarse con 'promedio'» pasa a +75,0 pp. MP2 cambia «40
  kilos» por «4 kilos», y «más dígitos» pasa a +75,0 pp. Los dos salen de «cubierto».
- Las nueve reglas y sondas al 100 % (+75,0 pp) no pueden subir, así que no admiten mutante. Queda declarado.
- «Descarta la más larga» (+8,3 pp) está fuera de la batería y solo se declara: el verificador la imprime como residuo.

**L-C5 · H2 en la Parte 6, regla por regla y por estrato (tanda 5, 2026-10-31; decisión «H2 a» del profesor).**
`cloze/verificar_espacio_cloze.R` (E8) lleva el alcance firmado como `FIRMA_H2`: 21 filas (regla o su contraria, por
papel) por 5 estratos (mezcla, rama CURSOS, rama PROMEDIO, *k*′ = 1 y *k*′ ≥ 2), 69 celdas, con las cifras de V0 tal como
se firmaron (un decimal).
- **Puerta:** `cloze/enumerar_h2_cloze.R`, una enumeración exacta de las Partes 2 a 6 conducida por el código del `.Rmd`
  (aclaración iii de §P7-E). Sustituye `sample()` y `sample.int()` en su entorno, ramifica la rama del otro colegio, el
  orden de las combinaciones de veredictos (con la cascada F1 dentro), `v_c` y la redacción de «ventaja», e integra
  exactamente la fila del caso paralelo y la fila del ganador. Aplica las exclusiones de V11 de cada Parte 1, sobre las 100
  Partes 1 de la muestra estándar. Es exacta en las Partes 2 a 6, condicionada a esas Partes 1, y determinista.
- Dentro de lo firmado: «ROJO, cubierto por «H2 a»», exit 4, nunca PASS. Por encima, o > +8 pp en una celda no firmada:
  pendiente (exit 3).
- Las reglas del orden de las afirmaciones (P6-P2) quedan fuera de la puerta: dependen de `o6`, que no se ramifica. Se
  juzgan en la muestra de E8, con su p95.
- Contraste de la ponderación (`cloze/salida/contrastar_h2_ponderacion.R`): semilla por semilla, la puerta coincide con el
  modelo transcrito de `medir_reparto_h2.R` con las exclusiones y la cascada (diferencia máxima de peso 1,1·10⁻¹⁹ en 100
  Partes 1). Sin exclusiones, ese modelo es V0.

| Exige | Guarda (E8 de `cloze/verificar_espacio_cloze.R`) | Mutante |
|---|---|---|
| Cada celda de H2, dentro de lo firmado | `h2_juzgar()` contra `FIRMA_H2`, una sola función para las 69 celdas | MC25: la rama PROMEDIO del otro colegio, siempre con *k*′ = 1 (K1 en PROMEDIO). MC26: la rama CURSOS, siempre con δ′ < 10 (D2 en CURSOS). MC27: `v_c <- !v_hp` (RL2 · HP). Las demás celdas no tienen mutante propio |
| La puerta ejecuta el código | cada hoja cuenta las llamadas de cada sorteo libre; la masa con error tiene que ser 0 | sin mutante propio (el mismo mecanismo que MC22 en la puerta de L-C3) |

### Decisión del profesor tras la tanda 2 (2026-10-30)

> **Firmada por el profesor el 2026-10-30**, en la pausa posterior a la tanda 2 del `orquestador-cloze`, que se
> detuvo antes de la FASE 2C porque X6, X10 y X11 quedaban sin firmar en la canónica (md5
> `96cfcf356c01326d42f10ee4c36545b7`). Respuesta literal: «a · aclaración sí». Regla #24 **H-5**: queda por escrito
> quién decidió, cuándo y con qué cifras.

1. **(a) Override firmado: X6, X10 y X11 en la canónica**, con las cifras de enumeración exacta
   (`cloze/salida/x_canonica_exacta.R`, 96 casos ponderados):

   | Regla | Acierto | Exceso | Peso en la mezcla |
   |---|---|---|---|
   | X6 | 41,7 % | +16,7 pp sobre 25 % | 1,67 pp |
   | X10 | 66,7 % | +16,7 pp sobre 50 % | 1,67 pp |
   | X11 | 62,5 % | +12,5 pp sobre 50 % | 1,25 pp |

   - Cubiertas por la misma dominancia que X9: en la canónica, el atajo firmado de «O-1 corregido» acierta el 100 %.
   - X9 se queda en 75,0 %, +25,0 pp (decisión 3.2 a).
   - En la mezcla y por rama, ninguna pasa de +8 pp.
2. **(aclaración sí)** Firmar el máximo de una batería no cubre las demás reglas. El alcance se cablea regla por
   regla, en la canónica por enumeración exacta, y al pedir una firma se llevan todas las reglas que pasan el umbral
   en ese estrato, con su cifra exacta. El coordinador la añade al texto íntegro de la regla #22, §P7-E; en este
   subproyecto rige ya: L-C3, arriba.

### Decisión del profesor tras la tanda 3 (2026-10-31)

> **Firmada por el profesor el 2026-10-31**, en la pausa posterior a la tanda 3 del `orquestador-cloze`, que se
> detuvo tras la FASE 2C r1 (`RECHAZAR` por H1 y H2, de diagnosticidad, sin ninguna CORRECCIÓN) sobre el md5
> `96cfcf356c01326d42f10ee4c36545b7`. Respuesta literal: «1 a · 2 a · i ii iii · doc sí. Commit. Push». Regla #24
> **H-5**: queda por escrito quién decidió, cuándo y con qué cifras.

1. **(1 a) H2 se cierra rediseñando el sorteo de la tabla del otro colegio, en la pasada 2 de §P7-D** (Partes 2 a
   6; quedan 2 de 3). La firma autoriza ese cambio de F-4, porque toca cómo se sortean los veredictos de la Parte 6.
   - **Cifras de partida** (`cloze/salida/p7f_h2_espacio_96cfcf35.txt`, N = 100). «*k*′ ≥ 2 ⇒ ganó el de más
     cursos» acierta el 80,0 % (CURSOS 77,4 %, PROMEDIO 83,0 %; canónica 82,5 %, exacta). «HG es verdadera si y
     solo si *k*′ = 1» acierta el 74,0 %. δ′ ≤ 4 lleva a CURSOS en 14 de 14 versiones.
   - **Reparto autorizado** (el candidato de la tanda 3): *k*′ ∈ {1, 2}; *k*′ = 1 solo en CURSOS con HG y HP
     verdaderas y en PROMEDIO con HG y HP falsas, con la misma masa en las dos ramas; δ′ ≥ 5 en las dos ramas. El
     objetivo: que *k*′ y δ′ no informen la rama, HG ni HP (50 % cada una, dado *k*′).
   - **δ′ es la diferencia de los promedios por curso** (`abs(aG - aP)` en `espacio_paralelo()`). Al pedir la
     firma, el coordinador lo describió como «los totales difieren siempre en 5 kilos o más», que es incorrecto
     (*D*′ ≥ 6 ya rige). Se le comunicó la corrección al profesor con este registro. Acotar δ′ es endurecer.
   - **Costo presentado y aceptado:** con *k*′ = 1, HP y HG llevan siempre el mismo veredicto. El orquestador anotó
     otro: δ′ ≥ 10 queda más frecuente en PROMEDIO. Los dos se miden con la batería; si alguno abre un canal de más
     de +8 pp, vuelve al profesor.
   - **Orden:** primero, la batería por gap, congelada (L-C4); con ella se mide el reparto antes de tocar el
     `.Rmd`; después se aplica junto con la redacción de H1 y H3; y revisa un detractor NUEVO (FASE 2C r2).
   - **Sigue firmado, sin cambio:** el veredicto se sortea primero; al menos una verdadera y una falsa; sin pares
     complementarios; sin cifras ni sedes de la Parte 1 (V11); la composición de L-C1; los pesos.
2. **(2 a) Override firmado: la canónica de la Parte 1, regla por regla, por dominancia.** Cifras exactas de los 24
   órdenes de las opciones del impreso (`cloze/salida/p1_canonica_exacta_96cfcf35.txt`). Exceso sobre el 25 % del
   azar; peso en la mezcla = 0,10 × exceso.

   | Batería §P7 del SCHOICE (48 reglas) | Acierto | Exceso | Peso |
   |---|---|---|---|
   | la que más comparte | 100,0 % | +75,0 pp | +7,50 pp |
   | quedarse con «total» | 100,0 % | +75,0 pp | +7,50 pp |
   | cifra mediana | 100,0 % | +75,0 pp | +7,50 pp |
   | más dígitos | 50,0 % | +25,0 pp | +2,50 pp |
   | primer número par | 50,0 % | +25,0 pp | +2,50 pp |
   | último múltiplo de 10 | 50,0 % | +25,0 pp | +2,50 pp |
   | quedarse con «promedio» | 50,0 % | +25,0 pp | +2,50 pp |
   | par con más palabras en común (relacional) | 50,0 % | +25,0 pp | +2,50 pp |
   | descartar el último negativo | 33,3 % | +8,3 pp | +0,83 pp |
   | par por sede: menor cifra | 33,3 % | +8,3 pp | +0,83 pp |
   | descartar la que no tiene cifra | 33,3 % | +8,3 pp | +0,83 pp |
   | descartar «de más» | 33,3 % | +8,3 pp | +0,83 pp |

   | Sonda de E4 | Acierto | Exceso | Peso |
   |---|---|---|---|
   | S5 más cursos + menor cifra del par | 100,0 % | +75,0 pp | +7,50 pp |
   | S7 G real + menor cifra del par (cota del canal 2) | 100,0 % | +75,0 pp | +7,50 pp |
   | S_U + S1 | 100,0 % | +75,0 pp | +7,50 pp |
   | S_U + S1 + suma | 100,0 % | +75,0 pp | +7,50 pp |
   | S_U + atípica | 100,0 % | +75,0 pp | +7,50 pp |
   | S_U + S1 + suma + atípica | 100,0 % | +75,0 pp | +7,50 pp |
   | S1 descarta las cifras de la tabla | 50,0 % | +25,0 pp | +2,50 pp |
   | S2 par de la sede de la fila 1 | 50,0 % | +25,0 pp | +2,50 pp |
   | S3 par de la sede de más cursos | 50,0 % | +25,0 pp | +2,50 pp |
   | S_U la sede de la razón sin sustento pierde | 50,0 % | +25,0 pp | +2,50 pp |
   | S_U + suma | 50,0 % | +25,0 pp | +2,50 pp |
   | Atípica: descarta la cifra que dobla a las demás | 33,3 % | +8,3 pp | +0,83 pp |

   - Cubiertas por dominancia: en la canónica, el atajo firmado de «O-1 corregido» acierta el 100 %. Son herencia
     del texto verbatim del impreso (H-2) y del SCHOICE aprobado (E1), así que no se corrigen en el CLOZE.
   - Fuera de la batería congelada (§P7-C), solo declarada: «descarta la más larga», 33,3 % (+8,3 pp; margen de la
     más larga sobre la segunda, 23,8 %).
   - Se cablea en `cloze/verificar_espacio_cloze.R` regla por regla, con estas cifras redondeadas a un decimal.
     Dentro, «ROJO, cubierto» (exit 4). Por encima, o con cualquier otra regla o sonda por encima de +8 pp en la
     canónica, pendiente de firma (exit 3). Con mutantes que las saquen de «cubierto»; las que están al 100 % no
     pueden subir, y eso se declara.
   - En el SCHOICE, ver el pendiente de abajo.
3. **(i ii iii)** Las tres aclaraciones de §P7-E (dominancia, firmas antiguas por el máximo y cifra exacta
   ejecutando el código) están en el texto íntegro de la regla #22 desde la v3.33.2 del índice del repo. En este
   subproyecto rigen ya, y la decisión 2 a las aplica.

### Pendiente del SCHOICE para su próximo ciclo (decisión 2 a)

- Espacio duro (U+00A0) entre cifra y unidad («15 kilos») en las opciones y en la Solution, y en «El Bosque» y
  «La Pradera» fuera de la tabla. Es la causa de los BAJA 1 y 2 de la 6b del CLOZE.
- Después, re-empalmar el bloque P1 del CLOZE con `cloze/empalmar_bloque_p1.R` y repetir E1 de
  `cloze/verificar_espacio_cloze.R`. Hasta entonces, la Parte 1 del CLOZE sale igual que el SCHOICE.
- **(Decisión 2 a del 2026-10-31.)** Cablear la canónica regla por regla en `verificar_espacio.R`, con las 24 cifras
  exactas de esa decisión (12 reglas de la batería y 12 sondas de E4) y sus mutantes, en lugar del máximo
  (0,10 × 75 pp) con que la firmó «O-1 corregido». Por E1, las cifras del SCHOICE son las de la Parte 1 del CLOZE.
  Hasta entonces, la firma del máximo sigue vigente (aclaración ii de §P7-E).
- **(§P7-G, 2026-10-31.)** Declarar el estímulo en las baterías de `verificar_espacio.R`, o añadirles reglas del
  estímulo. Hasta entonces, al volver a correrlas imprimen `SIN_COBERTURA`, con las mismas cifras.

### Tanda 4 (2026-10-31): el reparto autorizado de H2, medido, no es viable (pendiente de decisión del profesor)

> Lo midió el `orquestador-cloze` con la batería L-C4 congelada, antes de tocar el `.Rmd` (§P7-F): enumeración exacta del
> espacio de `espacio_paralelo()` (`cloze/salida/medir_reparto_h2.R`, `cloze/salida/reparto_h2_96cfcf35.txt`). El `.Rmd`
> no cambió (md5 `96cfcf35…`). Detalle y opciones: `cloze/HANDOFF.md`.

- El reparto cierra lo que H2 nombraba: R_k, R_hg y R_δ dan +0,0 pp en la mezcla y en los dos estratos de *k*′.
- El canal se desplaza a otros rasgos de la tabla. En la mezcla, con cifras exactas: «δ′ ≥ 10 ⇒ ganó el de mayor
  promedio», +36,5 pp; «el grado de menos cursos tiene 5 o más ⇒ ganó el de mayor promedio», +28,6 pp; «el menor promedio
  es de 40 o más ⇒ ganó el de más cursos», +28,6 pp. En *k*′ = 1 llegan a +39,1, +35,7 y +32,5 pp. Las hipótesis caen
  por los mismos rasgos: hasta +25,8 pp en la mezcla y +39,1 pp en *k*′ = 1. Y δ′ ≥ 10 sale en el 20,9 % de las versiones
  de la rama CURSOS frente al 93,9 % de la rama PROMEDIO, que es el costo anotado.
- Es estructural. En *k*′ = 1, la celda CURSOS con HG y HP verdaderas exige *a*′ ≥ *n*′·δ′ + 6, es decir, *D*′ pequeño. La
  celda PROMEDIO con las dos falsas exige 2·*a*′ < (*n*′ − 1)·δ′, es decir, *D*′ grande. Para cada δ′, sus cursos no se
  solapan. Equilibrar δ′ deja «5 o más cursos» en +47,2 pp; equilibrar los cursos deja δ′ ≥ 10 en +44,8 pp. En *k*′ = 2,
  las seis celdas no comparten ningún valor de δ′.
- La decisión 1 a dice: «si alguno abre un canal de más de +8 pp, vuelve al profesor». Por eso la tanda se detuvo sin
  aplicar el reparto, ni H1 ni H3, que iban juntos. No se gastó la pasada 2 de §P7-D.

### Notas del coordinador tras la tanda 4 (2026-10-31)

- **Corrección de la premisa de la decisión 4 (tras la tanda 1).** Decía que, con Chromium 156, `--window-size` por
  debajo de 500 «da una ventana de 500 px», y que `render_html_shots.R` no servía para 320 o 360 px.
  - Eso solo vale para el JavaScript de carga y para `--dump-dom`. `--screenshot` vuelve a maquetar al ancho pedido
    antes de capturar.
  - Medido el 2026-10-31: una caja al 50 % mide 160 px en la captura de 320, y las capturas de un ejercicio real salen
    idénticas píxel a píxel con la CLI y con CDP. Las capturas anteriores de 320 y 360 eran válidas en lo que es CSS.
  - El arreglo (captura por CDP con la guarda `innerWidth == W`) quedó en `afac46a1`. El error de la premisa fue del
    coordinador.
- **Regla #22 §P7-G, el estímulo es una entrada de la batería**, aprobada por el profesor el 2026-10-31 («iv sí»). Desde
  que se aplicó, `evaluar_bateria()` da `SIN_COBERTURA` a toda batería sin regla del estímulo ni declaración.
  - En el CLOZE, la batería del SCHOICE sobre la Parte 1 (E4) ya imprime `SIN_COBERTURA` en sus tres estratos, con las
    mismas cifras. El verificador sigue en exit 3, porque decide por el exceso.
  - La próxima tanda declara el estímulo en esa batería (`estimulo_no_aplicable`, con su razón: los canales del
    enunciado se miden aparte, en las sondas de E4) o le añade reglas del estímulo.
  - La batería L-C4 ya lleva reglas del estímulo, en código propio.
  - En el SCHOICE, ver su pendiente, arriba.

### Decisión del profesor tras la tanda 4 (2026-10-31)

> **Firmada por el profesor el 2026-10-31**, en la pausa posterior a la tanda 4 del `orquestador-cloze`, que se
> detuvo porque el reparto autorizado de H2 no es viable (md5 `96cfcf356c01326d42f10ee4c36545b7`). Respuesta literal:
> «H2 a · 1 no, 2 sí · §P7-G commit y push». Regla #24 **H-5**: queda por escrito quién decidió, cuándo y con qué
> cifras.

1. **(H2 a) Override firmado: H2 sobre el diseño actual (V0), regla por regla y por estrato.**
   - Es un canal estructural (§P7-F). Las cuatro variantes medidas (V0 a V3) dejan reglas por encima de +8 pp,
     porque una hipótesis de cambio unitario solo es falsa con *D*′ grande, y *D*′ grande delata al ganador.
   - Cifras exactas de `cloze/salida/reparto_h2_96cfcf35.txt` (V0, 14 096 registros), como exceso sobre el nulo
     exacto de cada regla. δ′ es la diferencia de promedios por curso. Las reglas sobre el ganador valen igual para
     «ventaja» y para «Ganó…». Entre paréntesis va el código de la regla en ese archivo («c» indica la contraria).
     Un guion indica +8 pp o menos.

   | Regla | Mezcla | Rama CURSOS | Rama PROMEDIO | *k*′ = 1 | *k*′ ≥ 2 |
   |---|---|---|---|---|---|
   | 2 o más cursos de diferencia ⇒ ganó el de más cursos (K1, R_k) | +33,3 | +29,8 | +36,9 | +32,9 | +33,6 |
   | δ′ ≥ 10 ⇒ ganó el de mayor promedio (D2) | +24,0 | +16,3 | +31,7 | +32,1 | +12,3 |
   | el grado con menos cursos tiene 5 o más ⇒ ganó el de mayor promedio (N1) | +14,1 | — | +34,5 | +29,3 | — |
   | menor promedio de 40 o más ⇒ ganó el de más cursos (A1; en CURSOS, A1c) | +13,6 | +8,9 | +36,1 | +30,5 | — |
   | δ′ ≤ 4 ⇒ ganó el de más cursos (D1, R_δ; se aplica en el 25 %) | — | +12,7 | — | — | — |
   | HG verdadera si y solo si hay 1 curso de diferencia (HK1, R_hg) | +22,5 | +26,9 | — | — | — |
   | HP verdadera si y solo si hay 1 curso de diferencia (HK2) | — | +13,5 | — | — | — |
   | hipótesis según «5 o más cursos» (HN/HNc) · HP | +18,0 | +16,0 | +10,5 | +17,3 | +17,5 |
   | hipótesis según «5 o más cursos» (HN/HNc) · HG | — | — | +10,4 | +14,1 | — |
   | hipótesis según «δ′ ≥ 10» (HD/HDc) · HP | — | +10,3 | +8,5 | +23,6 | +13,7 |
   | hipótesis según «δ′ ≥ 10» (HD/HDc) · HG | — | — | — | +13,8 | +16,3 |
   | hipótesis según «promedio de 40 o más» (HA/HAc) · HP | — | — | — | +16,5 | — |
   | hipótesis según «promedio de 40 o más» (HA/HAc) · HG | — | — | — | +11,1 | +8,8 |
   | RL1: HP y HG con el mismo veredicto (métrica del par) | +8,3 | +11,1 | +11,1 | +13,0 | — |
   | RL2: hipótesis con el veredicto contrario a «Ganó…» (condicional), HP / HG | +33,3 / +33,3 | +33,3 / +33,3 | +33,3 / +33,3 | +37,8 / +33,6 | +30,2 / +24,6 |

   - El reparto autorizado en la decisión 1 a (tras la tanda 3) **no se aplica**: esta firma lo sustituye, y el
     sorteo de la tabla del otro colegio no cambia.
   - Se cablea en `cloze/verificar_espacio_cloze.R` como ley L-C5, con la enumeración exacta que ejecuta el código
     del `.Rmd` (aclaración iii de §P7-E), no con la muestra de E8. Con n = 100, D2 de la mezcla da +27,9 pp frente
     a +24,0 exacto.
     - Dentro de lo firmado: «ROJO, cubierto», exit 4.
     - Por encima, o con cualquier otra regla de la Parte 6 por encima de +8 pp en un estrato: pendiente de firma.
     - Lleva mutantes.
   - **Costo aceptado.** Las dos hipótesis se pueden adivinar por la forma de la tabla y, con el ganador ya
     calculado, por RL2. La Parte 6 vale 1 de 8 puntos, y el ganador sale de las Partes 3 y 4, que no se pueden
     adivinar.
   - **La pasada 2 de §P7-D queda para H1 y H3** (redacción de la Parte 2). Esa redacción debe bajar a +8 pp o menos
     **todas** las reglas en rojo de la Parte 2 en la batería L-C4, no solo las tres de H1.
2. **(1 no)** La batería por afirmación de la Parte 6 (mchoice) no se promueve al helper del repo. Hay un solo caso, y
   la vara de +8 pp no está calibrada para mchoice. Queda como ley local (L-C4).
3. **(2 sí)** Este caso entra como ejemplo de §P7-F en el texto íntegro de la regla #22 (aclaración, v3.34.1 del
   índice).
4. **(§P7-G commit y push)** Regla del repo: commit `56befdec`, en el remoto.

### Tanda 5 (2026-10-31): las cifras de «H2 a» con el código y las exclusiones de V11 (pendiente de decisión del profesor)

> Lo midió el `orquestador-cloze` al cablear L-C5, antes de reabrir (md5 `96cfcf356c01326d42f10ee4c36545b7`, sin
> cambios). Evidencia: `cloze/salida/h2_exacta_96cfcf35.txt`, `cloze/salida/h2_contraste_96cfcf35.txt` y
> `cloze/salida/verificar_espacio_cloze_r5.txt`. Detalle: `cloze/HANDOFF.md`.

- **Causa.** Las cifras firmadas (V0 de `cloze/salida/reparto_h2_96cfcf35.txt`) salen de `espacio_paralelo()` sin las
  exclusiones de V11. El código sí las aplica: quita del caso paralelo las filas cuyos cursos o promedios son cifras de las
  opciones de la Parte 1, o su *T*_G, *T*_P o *D*, y las filas cuyo total o diferencia es un número que la Parte 1 muestra
  o resuelve. Dependen de la Parte 1 de cada versión.
- **La ponderación es la del código**, comprobado semilla por semilla (ver L-C5). La diferencia con lo firmado sale solo
  de las exclusiones, más el ruido de la Parte 1: entre las semillas 1 a 50 y 51 a 100, la semidiferencia es de 0,3 pp de
  mediana y 1,4 pp como máximo.
- **Resultado sobre las 69 celdas firmadas:** 53 cambian a un decimal; 22 quedan por encima de lo firmado, de +0,1 a
  +0,8 pp; 2 bajan a +8 pp o menos (A1c en la rama CURSOS, de +8,9 a +7,0); y ninguna celda no firmada pasa de +8 pp.

| Estrato | Regla · papel | Firmada | Exacta con exclusiones |
|---|---|---|---|
| mezcla | A1 · ventaja y ganador | +13,6 | +14,2 |
| mezcla | D2 · ventaja y ganador | +24,0 | +24,3 |
| mezcla | N1 · ventaja y ganador | +14,1 | +14,8 |
| mezcla | HK1 · HG | +22,5 | +22,8 |
| rama CURSOS | HK1 · HG | +26,9 | +27,1 |
| rama CURSOS | HK2 · HP | +13,5 | +13,6 |
| rama PROMEDIO | D2 · ventaja y ganador | +31,7 | +32,5 |
| rama PROMEDIO | HN/HNc · HG | +10,4 | +10,6 |
| *k*′ = 1 | D2 · ventaja y ganador | +32,1 | +32,8 |
| *k*′ = 1 | HD/HDc · HP | +23,6 | +23,8 |
| *k*′ = 1 | HD/HDc · HG | +13,8 | +14,1 |
| *k*′ = 1 | HN/HNc · HG | +14,1 | +14,3 |
| *k*′ = 1 | HA/HAc · HP | +16,5 | +16,6 |
| *k*′ = 1 | HA/HAc · HG | +11,1 | +11,2 |
| *k*′ ≥ 2 | HA/HAc · HG | +8,8 | +9,3 |
| *k*′ ≥ 2 | HD/HDc · HP | +13,7 | +14,0 |
| *k*′ ≥ 2 | RL2 · HP | +30,2 | +30,3 |

- **La tanda se detuvo aquí**, como mandaba su orden («si la cifra exacta ejecutando el código difiere de la firmada en
  alguna regla, detente y trae las dos cifras»). No se reabrió el `.Rmd` ni se gastó la pasada 2 de §P7-D. L-C5 queda
  cableada con las cifras firmadas: el verificador sale con exit 3 y lista las 22 celdas como pendientes. §P7-G quedó
  aplicada a la batería de E4.
