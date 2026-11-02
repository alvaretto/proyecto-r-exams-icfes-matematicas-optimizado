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

**L-C4 v2 (tanda 7, 2026-10-31; decisión «a · 1 sí · 2 sí» del profesor).** `cloze/bateria_partes_2_a_6.R`, versión «L-C4 v2
(2026-10-31)», **md5 `820e51464b34deff35b830789584de8a`**, congelada antes de reescribir la Parte 2. v1 (`de20e0a9…`) está en el
commit `31b8fe9e`. Endurece v1 sin tocar sus reglas: sus cifras se reproducen dentro de v2 (diferencia máxima 1,1·10⁻¹⁴ pp) y
L-C5 sale idéntica en sus 335 celdas.
- Vocabulario por token, calculado sobre los textos: cada palabra y cada expresión de 2 o 3 palabras de las opciones, con su
  contraria («elige las que dicen t» y «descarta las que dicen t»); P2-VX, vocabulario exclusivo de la clave, y P2-VXE, el mismo
  buscado en el texto visible.
- Contrarias de todas las reglas de la Parte 2 de v1, y molde (la única con su primera palabra, sus tres primeras o sus tres últimas).
- Estímulo: el párrafo del otro colegio (P2-E5), su tabla (P2-E6) y todo el texto visible del ítem (P2-E7 a P2-E9).
- Parte 6: P6-VX y P6-VXF, vocabulario exclusivo de las verdaderas o de las falsas sobre todas las versiones (aplica 0 %; puede
  disparar si un texto quedara con veredicto fijo). La regla por token de la Parte 6 NO entra: en la mezcla da 0,0 pp; en los
  estratos de *k*′, hasta +16,7 pp, dominada por K1 firmada (+32,8 y +33,5 pp); en los de rama, +25,0 pp, que es el contenido
  lógico de la afirmación de ventaja (RL3). Declarada en `cloze/salida/bateria_v2_historico.txt`; decide el profesor si entra en
  una v3.
- La Parte 2 se mide exacta con `medir_p2_exacta()`: 3 claves × C(5, 3) tríos; las reglas del estímulo, sobre las 100 versiones
  de la muestra estándar como portadores. Guarda: E8 de `cloze/verificar_espacio_cloze.R` (md5). Mutante MC28: las tres claves
  «promedio» ganan «precisamente» y P2-VX pasa a +75,0 pp.
- Histórico (§P7-C), `cloze/salida/bateria_v2_historico.txt`: en `96cfcf35…`, 106 reglas en rojo en «promedio» y 121 en
  «comparar»; en `42f725bf…`, 54 y 69, entre ellas «dividido» +75,0 pp en «promedio» y «descarta las que dicen *por curso*»
  +25,0 pp en «comparar» (las de H1 y H2 de la r2), el párrafo P2-E5 +75,0 pp y P2-VX +75,0 pp en los dos tipos.

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

**Actualización de L-C5 en la tanda 6 (2026-10-31; decisión «a · iii ampliada sí» del profesor).**
- **Qué cambió.** `FIRMA_H2` (E8 de `cloze/verificar_espacio_cloze.R`) lleva ahora las 69 cifras exactas que da el código del
  `.Rmd` con las exclusiones de V11, sobre las 100 Partes 1 de la muestra estándar: la columna `ex` de la primera tabla de
  `cloze/salida/h2_exacta_96cfcf35.txt`, a un decimal, tal como se firmaron. Sustituyen a las de V0. Las dos celdas de A1 en
  la rama CURSOS quedan en 7,0. Nada más cambió en el verificador ni en el `.Rmd`.
- **Por qué.** Las cifras de V0 salían de un modelo transcrito sin las exclusiones de V11. La aclaración iii de §P7-E,
  ampliada, exige que toda cifra que se lleve a firma salga del código del `.Rmd`, con todos sus filtros.
- **Comprobado** (`cloze/salida/verificar_espacio_cloze_r6a.txt`, exit 3, 368 s): 67 celdas «ROJO, cubierto»; las dos de
  A1 en la rama CURSOS, en +8 pp o menos («ok»); ninguna por encima de lo firmado y ninguna celda no firmada por encima de
  +8 pp; MC25, MC26 y MC27, cazados por su sonda; 0 fallos; los 12 pendientes son de la Parte 2 (H1).

**L-C6 · Override del residuo de la Parte 2 «promedio» (tanda 8, 2026-10-31; decisión «b» del profesor).** La Parte 2 tiene solo
el tipo «promedio» (cambio de F-4 firmado). `cloze/verificar_espacio_cloze.R` (E8) lleva el alcance firmado regla por regla como
`FIRMA_P2`, con las cifras exactas de `cloze/salida/bateria_v2_898608dd.txt`, comparadas a un decimal. La medida es la exacta de L-C4
v2 (`medir_p2_exacta`: 30 combinaciones × 100 portadores; banco y pregunta de la semilla 1, como en `cloze/salida/medir_bateria_v2.R`).
Dentro de lo firmado, «ROJO, cubierto» (exit 4), nunca PASS; por encima, o con otra regla de la Parte 2 por encima de +8 pp,
pendiente (exit 3).

| Regla | Firmada | Exacta en `f98f9dda…` | Mutante: sale de «cubierto» (misma medida, 10 portadores) |
|---|---|---|---|
| P2-R2 · la que más palabras comparte con las otras tres | +21,9 | +21,9 | MC31: las tres claves repiten palabras de todos los distractores; +72,5 pp |
| P2-R1 · par con más palabras en común | +13,6 | +13,6 | MC32: las tres claves terminan en «y luego entre dos veces»; +21,4 pp |
| P2-E8c · descarta la única con una palabra visible que no tiene otra opción | +13,5 | +13,5 | MC33: cada distractor lleva tres palabras visibles exclusivas; +75,0 pp |
| P2-E5c · descarta la de más palabras en común con el párrafo del otro colegio | +11,1 | +11,1 | MC34: los cinco distractores repiten el párrafo; +75,0 pp |
| P2-M3c · descarta la única con sus tres últimas palabras | +10,0 | +10,0 | MC35: las claves 1 y 2 terminan como la 3 y la moda; +15,0 pp |

Ninguna otra de las 268 reglas de la Parte 2 pasa de +8 pp (P2-R3, +7,5, en zona gris). Las cinco cifras no se movieron al quitar
el otro tipo (`cloze/salida/bateria_v2_f98f9dda.txt`).

**L-C7 · Override de la regla por token de la Parte 6, por estrato (tanda 8; decisión «b» del profesor).** La familia «las que dicen
*t* son verdaderas» y su contraria (P6-V «t»), fuera de la batería congelada. `FIRMA_P6_TOKEN` (E8): mezcla 0,0; rama CURSOS +25,0;
rama PROMEDIO +25,0; *k*′ = 1 +16,4; *k*′ ≥ 2 +16,7 pp. Se mide sobre la misma enumeración exacta de L-C5 (100 Partes 1) con la misma
función que dio las cifras (`vocab_p6_tokens` de la batería). Cada regla por token (token, papel y sentido) por encima de +8 pp y
dentro de la cifra de su estrato: «ROJO, cubierto» (exit 4); por encima, pendiente (exit 3).

| Exige | Guarda (E8) | Mutante |
|---|---|---|
| Ninguna regla por token por encima de la cifra de su estrato | `p6_token_juzgar()` sobre la enumeración exacta: 1176 reglas por estrato | MC36: HP dice «sin duda» solo cuando es verdadera; «duda» +25,0 pp en la mezcla (firmado 0,0) |

- **Alcance, leído literal.** La firma es por estrato y para la familia. En `f98f9dda…` pasan de +8 pp 54 reglas por token en cada
  estrato de rama y de *k*′, y ninguna en la mezcla; todas quedan dentro de la cifra de su estrato (las mayores, de la afirmación de
  ventaja: «participado», «tantos cursos como», «tenido»; las menores, +8,4 pp). Si la firma cubría solo el máximo de cada estrato,
  las otras 53 de cada uno necesitan firma regla por regla (§P7-E): pregunta del WAIT_USER #3 de la tanda 8.

**MC19 y MC30 (tanda 8).** MC19 (L-C3, X6 en la canónica) muta ahora las tres claves «promedio»: X6 +75,0 pp. MC30 se retiró con el
tipo retirado de la Parte 2: su guarda ya no existe.

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

### Decisión del profesor tras la tanda 5 (2026-10-31)

> **Firmada por el profesor el 2026-10-31**, en la pausa posterior a la tanda 5 del `orquestador-cloze`. La tanda se
> detuvo al cablear L-C5, porque al ejecutar el código del `.Rmd` 22 de las 69 celdas de «H2 a» superaban lo firmado
> (md5 `96cfcf356c01326d42f10ee4c36545b7`). Respuesta literal: «a · iii ampliada sí». Regla #24 **H-5**: queda por
> escrito quién decidió, cuándo y con qué cifras.

1. **(a) El alcance de «H2 a» son las cifras exactas que da el código, en las 69 celdas.** Sustituyen a las de V0.
   - Fuente: la columna `ex` de la primera tabla de `cloze/salida/h2_exacta_96cfcf35.txt`. Incluye las exclusiones de
     V11 y usa las 100 Partes 1 de la muestra estándar, así que es determinista.
   - Celdas que suben (antes → ahora, en pp):

     | Estrato | Celdas |
     |---|---|
     | mezcla | A1 ventaja y ganador 13,6 → 14,2 · D2 ventaja y ganador 24,0 → 24,3 · N1 ventaja y ganador 14,1 → 14,8 · HK1·HG 22,5 → 22,8 |
     | rama CURSOS | HK1·HG 26,9 → 27,1 · HK2·HP 13,5 → 13,6 |
     | rama PROMEDIO | D2 ventaja y ganador 31,7 → 32,5 · HN·HG 10,4 → 10,6 |
     | *k*′ = 1 | D2 ventaja y ganador 32,1 → 32,8 · HD·HP 23,6 → 23,8 · HD·HG 13,8 → 14,1 · HN·HG 14,1 → 14,3 · HA·HP 16,5 → 16,6 · HA·HG 11,1 → 11,2 |
     | *k*′ ≥ 2 | HA·HG 8,8 → 9,3 · HD·HP 13,7 → 14,0 · RL2·HP 30,2 → 30,3 |

   - Las otras 47 celdas quedan iguales o más bajas. En la rama CURSOS, «menor promedio de 40 o más» (ventaja y
     ganador) baja a +7,0 pp y sale del umbral.
   - Ninguna celda no firmada pasa de +8 pp. Los dos residuos de orden (P6-P2, medidos con muestra: HG +9,9 y ganador
     +8,3 pp) siguen declarados como NO CONCLUYENTES.
   - Por qué hizo falta una segunda firma: las cifras de V0 salían de un modelo transcrito sin las exclusiones de V11.
     Fue un error del coordinador, que aplicó la aclaración iii solo a la canónica.
2. **(iii ampliada sí)** El coordinador amplía la aclaración iii de §P7-E en el texto íntegro de la regla #22: toda cifra
   que se lleve a firma sale del código del `.Rmd`, con todos sus filtros.

### Tanda 6 (2026-10-31): pasada 2 de §P7-D en la Parte 2 (H1 y H3 de la FASE 2C r1)

> Lo hizo el `orquestador-cloze` tras la decisión «a · iii ampliada sí». Reapertura #2 desde `generacion_rmd`. md5
> `96cfcf35…` → `42f725bf191b81784f76b858c26f7733`. Detalle y evidencia: `cloze/HANDOFF.md`.

- **Pasadas de §P7-D en las Partes 2 a 6: quedan 1** (usadas: tanda 1 y tanda 6).
- **H1 (Parte 2).** En cada tipo, las ocho opciones llevan las mismas palabras del estímulo (la pregunta y las Partes 3 a 5):
  «Lo que recogió … grado … sus cursos» en «promedio»; «grado», «del» y «otro» en «comparar». Ninguna lleva «total», coma ni
  condicional, ni empieza por «Únicamente», ni dice «sin tener en cuenta» o «exactamente». Por largo, una clave queda entre
  las cortas, otra en el medio y otra entre las largas. En «comparar», el distractor «mejor» se cambió por «cociente» (el
  promedio dividido entre la cantidad de cursos). Las claves no cambian de concepto.
  - Batería L-C4, exacta (720 versiones por tipo): todas las reglas de la Parte 2 en +2,8 pp o menos. Antes: en «promedio»,
    P2-L3 +50,0, P2-L1 +45,0, P2-L2 +45,0 y P2-E1 +20,8; en «comparar», P2-L1 +75,0, P2-E4 +55,0, P2-E2 +45,0, P2-E1 +41,7,
    P2-L4 +12,5, P2-L6 +12,5, P2-F3 +11,1 y P2-F4 +9,2 pp.
  - Fuera de la batería congelada (§P7-C), cinco sondas medidas para no abrir el canal inverso: la que menos comparte, la que
    queda fuera del par más parecido, el prefijo singleton, «descarta la más corta» y las tres primeras palabras. Antes, hasta
    +45,0 pp (las tres primeras palabras en «promedio») y +35,0 pp (la que menos comparte); ahora, +3,9 pp como máximo.
- **H3.** El encabezado «Parte 2 · …» de la Solution depende del tipo: «Lecturas del promedio que no sirven» o
  «Comparaciones que no deciden qué grado ganó».
- **Canónica (L-C3).** Al mover los textos de la Parte 2, X6 baja de +16,7 a −2,8 pp y X10 de +16,7 a +0,0 pp (exactas); X5
  sigue en −25,0. Siguen firmadas, pero ya no están en rojo. MC19 y MC20 se rehicieron para los textos nuevos (tres claves
  «comparar» y dos claves «promedio» ganan «aunque», «fue» y «menor»): X6 +25,0 y X10 +33,3 pp, cazados por su sonda.
- **Sin cambio en lo demás.** Por semilla (100), la Parte 1, la tabla del otro colegio, las Partes 3 a 6, el tipo y la
  posición de la clave de la Parte 2 y el `.Random.seed` final son idénticos a los del md5 anterior. L-C5 da las mismas 69
  cifras que antes de la pasada.
- **Verificador** (`cloze/salida/verificar_espacio_cloze_r6b.txt`): exit 4, 106 cifras dentro de lo firmado, 0 fallos y
  0 pendientes.
- **FASE 2C r2 (2026-10-31): `APROBAR_CON_CAMBIOS`, sin ninguna CORRECCIÓN** (`cloze/salida/fase2c_r2_detractor.md`; citas 13/0).
  Fuera de la batería congelada: en «promedio», la clave es la única que dice «dividido entre» (+75,0 pp exactos); en
  «comparar», «descarta las que dicen *por curso*» (+25,0 pp). La batería L-C4 no lleva el párrafo del otro colegio como
  estímulo (H3, para L-C4 v2). El `ERR_DIAG_SUPERFICIAL` de `validar_diagnosticidad.R` en p6 es un artefacto de su sonda H3
  sobre un mchoice (0,0 pp exactos; H4). **Pendiente de decisión del profesor:** gastar la última pasada de §P7-D en la Parte 2
  o firmar H1 y H2 como residuo.

### Decisión del profesor tras la tanda 6 (2026-10-31)

> **Firmada por el profesor el 2026-10-31**, en la pausa posterior a la tanda 6 del `orquestador-cloze` (md5
> `42f725bf191b81784f76b858c26f7733`; FASE 2C r2 `APROBAR_CON_CAMBIOS`, sin CORRECCIÓN). Respuesta literal: «a · 1 sí ·
> 2 sí». Regla #24 **H-5**: queda por escrito quién decidió, cuándo y con qué cifras.

1. **(a) La pasada 3 de §P7-D, que es la última, se gasta en la Parte 2.** Cierra H1 y H2 de la r2:
   - H1: en «promedio», «dividido» solo aparece en las claves (+75,0 pp). Se pone también en distractores, con
     operaciones equivocadas que sigan siendo falsas.
   - H2: en «comparar», «por curso» solo aparece en distractores (+25,0 pp). Se quita de ellos.

   Antes de reescribir, la batería pasa a L-C4 v2, que es endurecer:
   - una sonda de vocabulario exclusivo de la clave, calculada sobre los textos;
   - cada regla léxica con su contraria;
   - el párrafo del otro colegio como estímulo (H3 de la r2).

   Por §P7-C, la batería ampliada se vuelve a medir sobre el histórico. Revisa un detractor nuevo (FASE 2C r3). Agotada
   esta pasada, lo que quede de diagnosticidad se cierra con residuo declarado. Una CORRECCIÓN se corrige siempre.
2. **(1 sí) Aclaración en el texto íntegro de la regla #22:** una regla léxica de palabra fija cubre solo esa palabra. La
   familia léxica exige una sonda de vocabulario exclusivo de la clave sobre todos los textos visibles, y cada regla con su
   contraria. La escribe el coordinador.
3. **(2 sí) Arreglo de `validar_diagnosticidad.R`:** las sondas de selección única dejan de aplicarse a los huecos mchoice
   (H4 de la r2). Lo hace Codex, con su test, en un worktree aparte; se aplica al repo después de la tanda 7.

### Tanda 7 (2026-10-31): pasada 3 de §P7-D en la Parte 2, la última (detenida antes de la 6b)

> Lo hizo el `orquestador-cloze` tras la decisión «a · 1 sí · 2 sí». Reapertura #3 desde `generacion_rmd`. md5
> `42f725bf…` → `898608dd2a625c54b905684d4bbdb827`. Detalle y evidencia: `cloze/HANDOFF.md`.

- **H1 y H2 de la r2, cerrados.** En «promedio», las claves y cuatro distractores empiezan por «Lo que recogió ese grado
  dividido»; los distractores dividen con un operando equivocado (el promedio, otra vez entre dos, el número de cursos al
  cuadrado, los cursos de ambos grados), cada uno con su función de verdad (`valor_p2`) y una guarda que lo comprueba falso en
  todas las versiones, y queda la moda. «dividido» y «entre», +5,0 pp; P2-VX, +0,0. En «comparar», ningún distractor dice
  «curso» ni «por curso»; cada uno tiene su función de verdad (`compara_p2`).
- **Regla de parada de §P7-D.** Al cerrar el canal léxico se abrió el relacional: «la que más palabras comparte con las otras
  tres» (P2-R2) sube a +21,9 pp en «promedio» y a +33,9 pp en «comparar» (antes, +2,5 y −10,6). En «comparar» queda además el
  vocabulario exclusivo de cada redacción de la multiplicación (P2-VX +75,0 pp; doce tokens a +25,0 pp). Es la última pasada:
  todo se declara como residuo con su cifra exacta y va al profesor (`cloze/salida/bateria_v2_898608dd.txt`).
- **Sin cambio en L-C5** ni en la Parte 1. Pendientes al cerrar la tanda: la auditoría visual 6b, la FASE 2C r3 (detractor
  nuevo), los pasos 8 a 10 y el WAIT_USER #3.

### Decisión del profesor tras la tanda 7 (2026-10-31)

> **Firmada por el profesor el 2026-10-31**, en la pausa posterior a la tanda 7 del `orquestador-cloze` (md5
> `898608dd2a625c54b905684d4bbdb827`; §P7-D agotado). Respuesta literal: «b · 1 sí · 2 sí · 3 sí · commit y push de los
> dos». Regla #24 **H-5**: queda por escrito quién decidió, cuándo y con qué cifras.

1. **(b) La Parte 2 queda solo con el tipo «promedio». Cambio de F-4, firmado.**
   - Se quita «comparar». En la tanda 7 conservaba vocabulario exclusivo de la clave (+75,0 pp) y tenía 22 reglas por encima
     de +8 pp. Su concepto, comparar totales, ya lo evalúan la Parte 1 y las Partes 3 a 5.
   - **Override firmado: residuo de la Parte 2 «promedio»** (§P7-D agotado). Cifras exactas de
     `cloze/salida/bateria_v2_898608dd.txt`:

     | Regla | Qué hace | Exceso firmado |
     |---|---|---|
     | P2-R2 | la que más palabras comparte con las otras tres | +21,9 pp |
     | P2-R1 | par con más palabras en común (elige una del par) | +13,6 pp |
     | P2-E8c | contraria de P2-E8 (la única con una palabra del texto visible que no tiene otra opción) | +13,5 pp |
     | P2-E5c | contraria de P2-E5 (más palabras en común con el párrafo del otro colegio) | +11,1 pp |
     | P2-M3c | contraria de P2-M3 (la única con sus tres últimas palabras) | +10,0 pp |

   - Esas cifras se midieron con «comparar» todavía en el sorteo. Si al quitarlo alguna cifra exacta, medida con el código,
     supera la firmada, o aparece otra regla de la Parte 2 por encima de +8 pp, vuelve al profesor (aclaración iii de §P7-E).
   - **Override firmado: Parte 6, regla por token**, fuera de la batería:
     - en la mezcla, 0,0 pp;
     - en los estratos de *k*′, +16,4 y +16,7 pp: es una restricción de K1, ya firmada;
     - en los estratos de rama, +25,0 pp: reproduce el contenido lógico de la afirmación de ventaja (RL3).
2. **(1 sí)** Aclaración en el texto íntegro de §P7-F, tercer caso: igualar el vocabulario de las opciones sobre el molde de
   la clave desplaza el canal al relacional. La escribe el coordinador.
3. **(2 sí)** Aclaración en el texto íntegro de §P7-E, familia léxica: la exclusividad del vocabulario se calcula sobre todas
   las versiones juntas. La escribe el coordinador.
4. **(3 sí)** Se arregla `post-exams2-validation.sh` para que resuelva una ruta relativa después de un `cd` o un `setwd()`
   del propio comando. Lo hace Codex en un worktree aparte y se aplica después de la tanda 8. Hasta entonces, los
   `exams2*` del CLOZE van con la ruta absoluta del `.Rmd`.
5. **(commit y push de los dos)** `bc860ba9` (arreglo de `validar_diagnosticidad.R`) y `8e75b132` (el CLOZE tras la
   tanda 7), en el remoto.

### Tanda 8 (2026-10-31): Parte 2 solo «promedio», L-C6 y L-C7 cableadas (detenida antes de la FASE 2C r3)

> Lo hizo el `orquestador-cloze` tras la decisión «b · 1 sí · 2 sí · 3 sí». Reapertura #4 desde `generacion_rmd`. md5
> `898608dd…` → `f98f9dda47e4f3cafc4e8b4e82cc956b`. Su script abortó antes de escribir este registro: lo escribe el
> coordinador desde su reporte. Evidencia en `cloze/salida/`: `bateria_v2_f98f9dda.txt`, `verificar_espacio_cloze_r8b.txt`,
> `rng_tanda8.txt`, `x6_estratos_t8.txt`, `6b_r8/`, `retro_r8.txt`, `render_r8.log`, `exportar_r8.log`, `stress_n100_r8/`,
> `suites_r8.txt`.

- **Lo firmado, medido con el código.** Las cinco reglas de L-C6 dan exactamente lo firmado (+21,9; +13,6; +13,5; +11,1;
  +10,0 pp). La regla por token de la Parte 6 da por estrato lo firmado en L-C7: mezcla 0,0; CURSOS +25,0; PROMEDIO +25,0;
  *k*′ = 1 +16,4; *k*′ ≥ 2 +16,7 pp.
- **Sin efectos colaterales.** El RNG se conserva con `invisible(sample.int(2L, size = 1L))` en el lugar del sorteo retirado;
  E1 111/111; L-C5 idéntica; `validar_diagnosticidad.R` PASS con la Parte 6 excluida.
- **Inventario de L-C7 (coordinador, desde `verificar_espacio_cloze_r8b.txt`).** 216 celdas «ROJO, cubierto»: 54 reglas por
  token (directas y contrarias) en cada estrato de rama y de *k*′, ninguna en la mezcla y ninguna por encima de la cifra de su
  estrato. En cada estrato toman solo dos valores: 46 reglas en el máximo y 8 en su mitad (+12,5; +12,5; +8,2; +8,4 pp).
- **6b:** APTO_CON_OBSERVACIONES (N = 24, declarado), con dos BAJO tipográficos: «sirven» sola en su línea a 360 px (24/24) y
  «cuadrado.» sola a 320 px (15/96).
- **X6 en la rama PROMEDIO:** +8,5 pp en la puerta, con una muestra de n = 100; +6,0 pp en la medida exacta. Pendiente de
  decisión del profesor.
- **Decisión 3 (hook):** Codex llegó a su límite de uso (hasta el 2026-11-25). Hizo el arreglo un `implementador` y lo terminó
  el coordinador. Está aplicado, con `test_post_exams2_cwd.R` en 16 bloques y verde, y sin commit.
- **Pendiente:**
  - FASE 2C r3, con un detractor nuevo y sin `name:`.
  - Paso 8: hay que reconstruir el oráculo de coherencias para el banco actual.
  - Sellar los pasos 7 a 10. Los pasos 9 y 10 ya están medidos, pero sin sellar.
  - WAIT_USER #3.
  - El orquestador agotó sus 65 turnos, así que la próxima tanda se parte en dos.

### Decisión del profesor tras la tanda 8 (2026-10-31)

> **Firmada por el profesor el 2026-10-31**, en la pausa posterior a la tanda 8 del `orquestador-cloze` (md5
> `f98f9dda47e4f3cafc4e8b4e82cc956b`). Respuesta literal: «Va», dada sobre la lista de pendientes del coordinador, que traía
> las tres recomendaciones de abajo. Regla #24 **H-5**: queda por escrito quién decidió, cuándo y con qué cifras.

1. **X6 en la rama PROMEDIO de la Parte 1 (L-C2): se mide exacta sobre el sorteo de la Parte 2.**
   - La puerta lo medía con la clave sorteada en la muestra de 100: +8,5 pp, pendiente. Exacta sobre las tres claves y con las
     mismas Partes 1: +6,0 pp (`cloze/salida/x6_estratos_t8.txt`).
   - Es el método de L-C4 v2 y de L-C6 (`medir_p2_exacta` integra el sorteo de la Parte 2) y la excepción de enumeración
     exhaustiva de la regla #23. El orquestador no lo había cambiado porque, sin firma, eso relajaba una guarda (H-5).
   - Alcance: X5, X6 y X10, las reglas cruzadas que leen el sorteo de la Parte 2, se miden exactas sobre ese sorteo en la mezcla
     y en las dos ramas. X6 y X10 se integran sobre las 3 claves; X5, además, sobre los C(5,3) tríos de distractores. La
     canónica sigue con L-C3. Si alguna pasa de +8 pp, queda pendiente y vuelve al profesor.
   - **Dato que el coordinador no mostró al pedir la decisión.** Por clave, X6 da +9,0, +9,0 y +0,0 pp. Está en el mismo
     archivo y en el HANDOFF. La cifra exacta, +6,0, es su promedio, porque la clave se sortea uniforme entre las tres. Si el
     profesor quiere leerla clave por clave, la decisión vuelve a él.
2. **L-C7: la firma pasa a ser regla por regla.**
   - La lista es `cloze/firma_p6_palabras_f98f9dda.tsv` (md5 `2aa8e742…`). Salió por código de
     `cloze/salida/verificar_espacio_cloze_r8b.txt` y tiene 216 reglas: 54 por estrato de rama y de *k*′, ninguna en la mezcla.
     Cada una lleva su cifra exacta a un decimal.
   - Una regla dentro de su cifra y de la de su estrato queda «ROJO, cubierto» (exit 4). Una regla por palabra por encima de
     +8 pp que no esté en la lista, o que pase de su cifra, queda pendiente (exit 3), aunque no llegue a la cifra del estrato.
   - Con esto L-C7 se endurece: con la firma por estrato, una palabra nueva que llegara al máximo quedaba cubierta sin firma.
3. **6b: los dos BAJO tipográficos quedan como residuo declarado.** «sirven» queda sola en su línea a 360 px (24/24) y
   «cuadrado.» a 320 px (15/96), en `cloze/salida/6b_r8/`.

Las decisiones 1 y 2 se cablean en `cloze/verificar_espacio_cloze.R` (E8), cada una con su mutante. Lo registra la tanda que lo
aplique.

### Tanda «Va» (2026-10-31): las decisiones 1 y 2, cableadas en el verificador

> La hizo el coordinador tras la decisión «Va». El `.Rmd` no cambió (md5 `f98f9dda…`): solo cambió
> `cloze/verificar_espacio_cloze.R`. Evidencia: `cloze/salida/verificar_espacio_cloze_va.txt`, exit 4: «EN ROJO BAJO OVERRIDE
> FIRMADO», 327 cifras dentro de lo firmado, 0 fallos y 0 pendientes.

**Cómo se hizo.**
- El encargo (`cloze/salida/encargo_verificador_va.md`) fue a DeepSeek por `delegar.sh`. Codex no tiene cuota hasta el
  2026-11-25.
- DeepSeek escribió el cableado (+136/−27) y lo corrió una vez (`cloze/salida/verificador_va.txt`, interrumpida en E3).
  Después se quedó 9 minutos colgado en una llamada al proxy. Su proceso había sobrevivido al cierre de la sesión anterior.
- El coordinador lo detuvo a las 13:30 y completó el diff en lugar de relanzarlo.

**Defectos del diff de DeepSeek, corregidos por el coordinador:**
1. **La FALLA del sorteo de la Parte 2 estaba en la comprobación, no en el sorteo.** La comprobación tomaba los distractores
   con `opciones_p2[-sol_p2]`. `sol_p2` es lógico, así que el índice quedaba en 0/−1 y quitaba siempre la opción 1. Por eso
   fallaban las 74 de 100 versiones con la clave fuera de la posición 1. La medida exacta no usa esa línea. Con `!sol_p2`
   pasan 300 de 300.
2. **MC37 estaba en E3, antes de `mutar()` y de `filas_mut`.** La corrida se interrumpía ahí. Pasó a E6, después de MC17.
3. **MC38 salía siempre `mal_construido`.** Comprobaba su llegada con `all(grepl("justamente", …))` sobre un vector que lleva
   `""` en las dos redacciones «por cursos». Además, su comentario justificaba la semilla 2 con una premisa falsa: las
   redacciones BP salen en las dos ramas y `h2_exacta()` ramifica la rama del otro colegio. Ahora usa la semilla 1, como MC36.
4. **De diseño: MC37 medía solo la mezcla.** Daba +8,5 pp, «cazado» por medio punto. La mutación abre X6 en la rama CURSOS
   (+38,2 pp) y la vuelve antipredictiva en la PROMEDIO (−25,0 pp). Ahora juzga los tres estratos que juzga la puerta.

**Decisión 1 (L-C2).** Quedó en E3, donde vive la batería cruzada, y no en E8, como decía el registro de la decisión.
- `medir_exacta_p2()` integra el sorteo de la Parte 2 con el banco de la semilla 1: 3 claves × C(5,3) tríos, 30
  combinaciones equiprobables. Es una sola función, en la puerta y en MC37.
- El supuesto del sorteo se comprueba con `ok()` en las 300 versiones de la mezcla y de las dos ramas.

| Regla, exacta (exceso en pp) | Mezcla | Rama CURSOS | Rama PROMEDIO |
|---|---|---|---|
| X5 más palabras en común con todo el texto de las Partes 2 a 6 | 20,8 % (−4,2) | 22,1 % (−2,9) | 18,6 % (−6,4) |
| X6 más palabras en común con la clave de la Parte 2 | 24,9 % (−0,1) | 26,3 % (+1,3) | 31,0 % (+6,0) |
| X10 S_U + más palabras en común con la clave de la Parte 2 | 42,7 % (−7,3) | 46,0 % (−4,0) | 52,7 % (+2,7) |

- X6 por clave: en la mezcla, +1,0 / +1,0 / −2,2; en CURSOS, +2,0 / +2,0 / +0,0; en PROMEDIO, +9,0 / +9,0 / +0,0 pp. Son las
  cifras de `cloze/salida/x6_estratos_t8.txt`.
- Las nueve celdas salen «ok». Al lado se imprime la cifra de la muestra: X6 en la rama PROMEDIO da 33,5 % (+8,5 pp).

**Decisión 2 (L-C7, E8).** `p6_token_juzgar()` lee `cloze/firma_p6_palabras_f98f9dda.tsv` con su md5 comprobado (216 filas).
Una regla por palabra por encima de +8 pp queda «cubierta» solo si está en la lista y no pasa ni su cifra ni el tope de su
estrato (`FIRMA_P6_TOKEN`, que se conserva como cota). Fuera de la lista, o por encima, queda pendiente. En `f98f9dda…`, las
216 reglas de la lista existen en la enumeración exacta y salen cubiertas: 54 en cada estrato de rama y de *k*′, ninguna en la
mezcla. Ninguna regla queda cubierta fuera de la lista y ninguna queda pendiente. `p6_token_juzgar_estrato()` conserva la
regla anterior solo para el contraste de MC38.

| Ley | Exige | Guarda | Mutante |
|---|---|---|---|
| L-C2, decisión «Va» 1 | X5, X6 y X10, exactas sobre el sorteo de la Parte 2, en la mezcla y en cada rama; por encima de +8 pp, pendiente | E3: `medir_exacta_p2()` y `veredicto_cruzado()`; el supuesto del sorteo, con `ok()` | MC37: las tres claves ganan «aunque fue menor»; X6 exacta, en la mezcla +8,5 pp (n = 200), en CURSOS +38,2 (n = 76) y en PROMEDIO −25,0 (n = 105): pendiente |
| L-C7, decisión «Va» 2 | Regla por regla: «cubierta» solo dentro de su cifra de la lista y del tope de su estrato; fuera de la lista o por encima, pendiente | E8: `p6_token_juzgar()` con el md5 de la lista; las 216 de la lista salen cubiertas y ninguna fuera de ella | MC36: «duda», fuera de la lista, +25,0 pp en la mezcla. MC38: «justamente» en las dos redacciones BP; «justamente»c, +25,0 pp en CURSOS, queda pendiente con la firma regla por regla y cubierta con la de estrato; 12 reglas pasan de cubiertas a pendientes |

- **Reparto de las 327 cifras cubiertas:** 13 de «O-1 corregido», 2 de L-C3 (X9 y X11 en la canónica), 24 de la decisión 2 a,
  67 de L-C5, 5 de L-C6 y 216 de L-C7.
- **Lo demás no cambió.** Contra la r8b, con las etiquetas normalizadas: E1, E2, E4, E5, L-C3, L-C4, L-C5, L-C6, MC1 a MC35, MP1
  y MP2 dan una salida idéntica.
- **MC36 con una sola Parte 1:** 160 reglas por palabra pendientes, frente a 128 en la r8b. Se mide como MC25 a MC27, con una
  sola Parte 1, y con la firma regla por regla queda pendiente toda cifra que en esa Parte 1 pase la de la lista. Su veredicto
  lo decide «duda».
- **X6 por clave, +9,0 / +9,0 / +0,0 en la rama PROMEDIO:** sigue siendo el dato que el profesor no vio al decidir. Si quiere
  leer X6 clave por clave, la decisión vuelve a él.

### Tanda 9a (2026-10-31): FASE 2C r3 y paso 8, sin sellar

> La hizo el `orquestador-cloze` sobre el md5 `f98f9dda47e4f3cafc4e8b4e82cc956b`, con el verificador de la tanda «Va»
> (`e11dc058`). No cambiaron el `.Rmd`, el verificador ni `cloze/ejercicio_state.json`: no se selló ningún paso. Evidencia en
> `cloze/salida/`: `fase2c_r3_detractor.md` (reporte íntegro, sacado de la transcripción del detractor) y `coherencias_r9/`.

**FASE 2C r3 (`AgenteDetractor` nuevo, sin `name:`): `APROBAR_CON_CAMBIOS`, sin ninguna CORRECCIÓN.** Citas: 9 de 9
(`verificar_citas_detractor.py`, exit 0; el hook `SubagentStop` registró «ENTREGA VERIFICADA»).
- Comprobado con su código, sin hallazgo: en la Parte 2 solo la clave da el promedio, con todas las lecturas razonables
  («al cuadrado» como *T*/*n*² y como (*T*/*n*)²; «luego entre dos» como *T*/*n*/2 y como *T*/(*n*/2)), en las semillas 1:100 y
  analíticamente en todo el espacio paralelo. La explicación actual de la moda cierra el BAJA del paso 8 de la r3. El tipo
  retirado no deja rastro visible, y el sorteo conservado no cambia nada fuera del RNG (0 diferencias con `898608dd` en 100
  semillas).
- Reproducido con su código: E1 (120 versiones, 29 canónicas), L-C6 (P2-R1 +13,6, P2-R2 +21,9 y P2-R3 +7,5 pp), la lista de
  L-C7 (216 reglas, 0 diferencias), las 9 celdas de L-C2 y su desglose por clave, X6 y X10 en la canónica, RL1 y RL2.

| ID | Severidad · tipo | Qué | Estado |
|---|---|---|---|
| H1 | MEDIA · diagnosticidad | «Exacta» en L-C2 lo es sobre el sorteo de la Parte 2, pero muestral sobre la Parte 1 (una sola muestra de N = 100 por estrato). En 5 réplicas disjuntas de N = 100 por rama (la 1 reproduce el verificador), X6 da en CURSOS +1,3 / +6,7 / +3,0 / +7,3 / +3,0 y en PROMEDIO +6,0 / +5,7 / +7,0 / +2,0 / +5,0 pp: todas en «ok». Por clave, las claves 1 y 2 dan en CURSOS +2,0 / +10,0 / +4,5 / +11,0 / +4,5 y en PROMEDIO +9,0 / +8,5 / +10,5 / +3,0 / +7,5 pp; la clave 3, +0,0 en todas. Con las 500 juntas (solo depuración): CURSOS +4,3 (claves 1 y 2: +6,4) y PROMEDIO +5,1 (+7,7). Para CURSOS por clave, el registro de «Va» da +2,0, la menor de las cinco | Al profesor: es la lectura por clave que la decisión «Va» 1 le reservó, ahora en las dos ramas y con réplicas |
| H2 | MEDIA · cobertura del verificador | Ningún mutante prueba la segunda cláusula de L-C7 («o que pase de su cifra»): `p6_token_juzgar()` exige `round(ex, 1) <= f` y `<= fe`, y MC36 y MC38 solo prueban reglas fuera de la lista. Quedan sin prueba las 32 reglas de la lista firmadas a la mitad de su tope (8 por estrato). El detractor propone MC39 | Para el coordinador: endurecer el verificador (esta tanda no podía tocarlo) |
| H3 | BAJA · precisión de la Solution | Parte 6: «habría ganado con un curso más solo si su promedio por curso … fuera mayor que la diferencia» da solo la condición necesaria (la relación es «si y solo si») y, por el subjuntivo, en el caso verdadero se lee como contrafáctica. No afirma nada falso | Opcional: cambia el `.Rmd` (reapertura, pasos 3 a 6 y detractor nuevo) |

**Paso 8 (`AgenteValidadorVisual`, sin `name:`; oráculo reconstruido en `cloze/salida/coherencias_r9/`):
`VEREDICTO_COHERENCIAS: OK`.**
- El juicio de las 8 opciones de la Parte 2 (3 correctas y 5 incorrectas) lo escribió el validador sin abrir el `.Rmd`, y lo
  respaldó recalculando cada operación con la tabla mostrada: ninguna incorrecta da el promedio en 200 de 200 filas, y las
  explicaciones que dicen qué da una operación se cumplen en 240 de 240 apariciones.
- N = 100: de 126 a 129 comprobaciones por versión (12 720 en total), 0 fallos. Formatos: HTML 100, PDF 100, Moodle 100 (más 30
  por la ruta de `exportar_moodle_cloze.R`), PDF con el pandoc de RStudio 5 y DOCX 1; 5 529 comprobaciones entre formatos, 0
  fallos. 88 mutaciones de control, todas cazadas por su sonda.
- Límite de independencia, declarado por el validador: el volcado del pool le mostró la marca de R/exams junto a cada texto, y
  este archivo le llegó al contexto (Claude Code lo carga al leer cualquier archivo del subárbol). La evidencia independiente es
  el recálculo numérico, junto con la función de verdad propia del detractor.
- BAJA: el DOCX deja los `##ANSWERi##` a la vista (ya declarado); «al cuadrado» admite dos lecturas, las dos falsas, y la
  explicación describe una; las claves 2 y 3 están redactadas como operación, no como «lo que indica».

**Pendiente antes de la tanda 9b:** H1 y H3, al profesor; H2 (MC39), al coordinador. Si H3 se aplica, cambia el `.Rmd`:
reapertura y FASE 2C r4 con un detractor nuevo. Si no, la 9b sella el paso 7 con el reporte de la r3 y lo que se decida de H1
a H3. Los scripts del detractor quedaron en `cloze/salida/fase2c_r3/`.

### L-C8 · El paso 8 se cierra con un oráculo de coherencias que falla ante texto no juzgado (firmada el 2026-10-31)

> **Firmada por el profesor el 2026-10-31**, a propuesta del coordinador, tras preguntar si `salida/coherencias…` se volvía
> regla o ley permanente del `orquestador-cloze`. Respuesta literal: «Sí», a las dos propuestas: esta ley local y una fila en la
> tabla de puntos ciegos de `.claude/agents/orquestador-cloze.md`. **No es regla del repo:** según el criterio de promoción, le
> falta un caso grave o repetido, porque el oráculo dio 0 discrepancias y la Solution falsa del SCHOICE la cazó el detractor.
> Regla #24 **H-5**: queda por escrito quién decidió, cuándo y por qué.

**Exige.** El paso 8 de este CLOZE se cierra con el oráculo de coherencias de la ronda
(`cloze/salida/coherencias_r<n>/verificar_coherencias.R`), que cumple cuatro condiciones:
1. No usa objetos internos del `data_generation` ni abre el `.Rmd` para armar el pool. Toma lo que R/exams emite (`xexams()`)
   y lo recalcula desde las tablas que se muestran.
2. Falla ante todo texto de la Parte 2 que no juzgó.
3. Se rehace cada vez que cambia el banco.
4. Sus mutaciones de control mueren por su propia sonda, entre ellas una con un texto que no juzgó.

| Exige | Guarda (`coherencias_r9/verificar_coherencias.R`) | Mutante |
|---|---|---|
| Falla ante texto no juzgado | S2 por versión: «las cuatro opciones de la Parte 2 salen del banco de ocho textos que juzgué» | «opción de la Parte 2 del tipo retirado»: cazada por esa misma S2. «Cuarta paráfrasis correcta escrita por mí»: cazada, 5 fallos |
| Se rehace si cambia el banco | S2 global: «las opciones emitidas son EXACTAMENTE los ocho textos que juzgué». Un banco con un texto de más o de menos lo hace fallar | — (la condición es global; las mutaciones actúan sobre una versión) |
| Juicio propio, sin el generador | Pool `pool_parte2_emitido.*`, juzgado por el validador, más el recálculo de cada operación con la tabla: 200 de 200 filas y 240 de 240 explicaciones | 88 mutaciones de control (40 de coherencias y 48 de formatos), las 88 cazadas por su sonda |

- **Origen, medido.** El oráculo de la tanda 6 (`coherencias_r3/`) solo había juzgado 1 de las 8 redacciones que la Parte 2
  tiene desde la tanda 8: 1 de las 3 claves y ninguno de los 5 distractores (`cloze/salida/banco_vs_r3_lc8.R`/`.txt`). Con su S2
  habría fallado, así que hubo que rehacerlo: es `coherencias_r9/`, de la tanda 9a.
- **Límite, declarado por la tanda 9a.** El juicio del validador no fue ciego: el volcado del pool le mostró la marca de R/exams
  junto a cada texto, y este archivo le llegó al contexto, porque Claude Code lo carga al leer cualquier archivo del subárbol.
  La evidencia independiente es el recálculo numérico, junto con la función de verdad propia del detractor.
- **Endurecimiento** (autónomo, porque endurece): cuando el oráculo se rehaga, el volcado del pool para el juicio se hará sin las
  marcas de R/exams.
- **Fila del orquestador** (con el mismo «Sí»): «Paso 8 en verde con un oráculo de coherencias de banco fijo», en la tabla de
  puntos ciegos de `.claude/agents/orquestador-cloze.md`. El respaldo está en `.claude/backups/2026-10-31-oraculo-coherencias/`.
  Pasan las invariantes I-1 a I-10 y las cuatro suites que vigilan el orquestador.


### Cierre de la tanda 9a (2026-11-02): H2 cerrado con MC39, rótulo de L-C2 y fe de erratas de «Va»

> Lo hizo el coordinador, sin sellar nada. No cambiaron el `.Rmd` (md5 `f98f9dda47e4f3cafc4e8b4e82cc956b`) ni
> `cloze/ejercicio_state.json` (6/11). Solo cambió `cloze/verificar_espacio_cloze.R`: +37/−5 frente a `e11dc058`. Evidencia en
> `cloze/salida/`: `verificar_espacio_cloze_r9.txt` (445 s) y `comparar_r9_va.py`/`.txt`.

**H2 de la FASE 2C r3, cerrado: MC39 en E8 (L-C7, la cláusula «o que pase de su cifra»).** Endurece, así que es autónomo (H-5).
- **Sonda directa** (la que propuso el detractor). No depende del texto, así que sigue probando la cláusula aunque otro ciclo
  cambie las redacciones. Toma una de las 32 reglas de la lista que están firmadas por debajo del tope de su estrato (8 por
  estrato) y la sube 1 pp sobre su cifra. Comprueba que `p6_token_juzgar()` la deja pendiente y que `p6_token_juzgar_estrato()`
  la cubría: CURSOS · P6-V «curso habría ganado», ventaja a +13,5 pp (firmada +12,5, tope +25,0).
- **Mutante de texto.** BP_gano gana «perdió hubiera tenido», el trigrama que solo llevaba BP_perdio, y con eso pasa a delatar el
  veredicto. En CURSOS y PROMEDIO sube de +12,5 a +25,0 pp (tope +25,0). Queda pendiente con la firma regla por regla y cubierto
  con la de estrato: `cazado_por_su_sonda`.
- La fila de L-C7 en la tabla de leyes de «Va» suma MC39 a MC36 y MC38.

**H1, la parte del coordinador: el rótulo.** En E3, las 13 líneas de X5, X6 y X10 dicen ahora «exacta sobre el sorteo de la
Parte 2 y muestral sobre la Parte 1 (N = 100)». Las cifras no cambian. La lectura por clave sigue con el profesor.

**Verificador r9: exit 4, «EN ROJO BAJO OVERRIDE FIRMADO», 327 cifras dentro de lo firmado, 0 fallos y 0 pendientes.** Se
comparó con `verificar_espacio_cloze_va.txt`, devolviendo los 13 rótulos a su forma anterior y apartando las 5 líneas de MC39.
No queda ninguna línea de diferencia, sin contar los espacios finales de la tabla resumen, que se ensancha con la fila de MC39.
La primera corrida (2026-10-31, 15:01) se cortó al cerrar la sesión, en la línea 718 de 1 134 y sin resultado. Se repitió
entera el 2026-11-02.

**L-C8, comprobada en el oráculo de la 9a** (`coherencias_r9/verificar_coherencias.R`): la S2 por versión (línea 235) y la S2
global (líneas 511-515) están presentes, y la mutación con una redacción nueva (la cuarta paráfrasis correcta) está cazada por
su sonda (`control_moodle.txt`).

**Fe de erratas de § «Tanda «Va»».** Este archivo solo se amplía, así que la corrección se hace aquí y no en la tabla.
- La cabecera «Regla, exacta (exceso en pp)», el «exactas» de la fila de L-C2 en la tabla de leyes y el «+6,0 pp en la medida
  exacta» de la tanda 8 son exactos solo sobre el sorteo de la Parte 2. Sobre la Parte 1 son muestrales: una sola muestra de
  N = 100 por estrato, la del verificador.
- En las 5 réplicas disjuntas de la FASE 2C r3, X6 da por rama de +1,3 a +7,3 pp en CURSOS y de +2,0 a +7,0 pp en PROMEDIO.
  Para las claves 1 y 2, da de +2,0 a +11,0 pp en CURSOS y de +3,0 a +10,5 pp en PROMEDIO.
- Las cifras de la tabla no cambian; cambia lo que significan. La decisión «Va» 1 se tomó leyéndolas como exactas. Qué
  significa eso para ella lo decide el profesor (H1); no se relaja nada mientras tanto (H-5).

**Pendiente:** H1 y H3, al profesor; las candidatas del repo #3 a #5 de la 9a (tocan `.claude/`, regla #17); el sello del
paso 7, en la tanda 9b.

### Decisión del profesor tras la tanda 9a (2026-11-02)

> **Firmada por el profesor el 2026-11-02**, en el diálogo de cierre de la tanda 9a (md5 `f98f9dda47e4f3cafc4e8b4e82cc956b`,
> verificador r9 en exit 4). Respuestas literales: H1 «(a) Residuo declarado»; H3 «(a) Diferir»; candidatas «#3 Lanzamiento
> asíncrono, #4 §P7-E iii en el íntegro, #5 FASE 2C y hook de citas» y, al aclarar que también había marcado «Ninguna por
> ahora», «Las tres, ahora»; paso 7 «(a) Sellar con la r3». Regla #24 **H-5**: queda por escrito quién decidió, cuándo y con qué
> cifras.

1. **H1: X6 por clave y por rama, residuo declarado.** La puerta sigue juzgando X6 por rama, como en la decisión «Va» 1: exacta
   sobre el sorteo de la Parte 2 y muestral sobre la Parte 1, con N = 100 por estrato. No se relaja nada. El desglose «por
   clave» (según cuál de los tres textos de clave de la Parte 2 sale) queda como residuo, con las cifras de
   `cloze/salida/fase2c_r3/x6_replicas.txt` (5 réplicas disjuntas de N = 100 por rama; la réplica 1 es la del verificador):

   | X6, exceso en pp | Réplicas 1 a 5 | Las 500 juntas (solo depuración) |
   |---|---|---|
   | CURSOS, por rama | +1,3 / +6,7 / +3,0 / +7,3 / +3,0 | +4,3 |
   | CURSOS, claves 1 y 2 (cada una) | +2,0 / +10,0 / +4,5 / +11,0 / +4,5 | +6,4 |
   | PROMEDIO, por rama | +6,0 / +5,7 / +7,0 / +2,0 / +5,0 | +5,1 |
   | PROMEDIO, claves 1 y 2 (cada una) | +9,0 / +8,5 / +10,5 / +3,0 / +7,5 | +7,7 |
   | Clave 3, las dos ramas | +0,0 en las cinco | +0,0 |

   En la mezcla, que es lo que ve el estudiante (la clave de la Parte 2, no la rama), por clave da +1,0 / +1,0 / −2,2 pp
   (semillas 1:100).
2. **H3: se difiere.** La reescritura del «solo si … fuera mayor» de la Solution de la Parte 6 pasa al próximo ciclo del CLOZE,
   junto con el pendiente del SCHOICE. El `.Rmd` no cambia en este ciclo.
3. **Candidatas del repo #3, #4 y #5 de la 9a: se aplican ahora**, con respaldo y verificación de invariantes (regla #17), en un
   commit aparte del del CLOZE. #3: los orquestadores describen como bloqueante el lanzamiento sin `name:`, que es asíncrono
   (y el Paso 0 de la regla #9). #4: aclaración de §P7-E iii en el texto íntegro de la regla #22. #5: cómo transmite un
   orquestador la FASE 2C sin que lo bloquee el hook de citas.
4. **Paso 7: se sella con la FASE 2C r3.** MC39 se cableó después de la r3. Es compatible con la regla #9 por tres razones: el
   `.Rmd` es el mismo md5 que se auditó, MC39 es el cambio que pidió el propio `APROBAR_CON_CAMBIOS` (H2) y ninguna cifra firmada
   se movió (`cloze/salida/comparar_r9_va.txt`). La nota del sello lo declara, y el próximo detractor revisa MC39.

**¿Ley local o regla?**
- H1 y H3 no crean ley: un residuo declarado no es una invariante, y H3 es una instancia de la regla #13 v1.2.
- La lectura del paso 7 podría ser una aclaración de la regla #9. Diría que un cambio pedido por un `APROBAR_CON_CAMBIOS` que no
  toca el `.Rmd` ni las cifras firmadas no exige otro detractor antes del sello. No se promueve: es la primera vez, no es grave
  y suaviza la lectura literal de «un detractor que auditó una versión anterior no es FASE 2C». Por H-5, eso lo decide un humano
  caso por caso.

### Tanda 9b (2026-11-02): pasos 7 a 10 sellados, uno por uno

> La hizo el `orquestador-cloze` sobre el md5 `f98f9dda47e4f3cafc4e8b4e82cc956b`, con el verificador de `92dc8d69`
> (`6901624e…`). No cambiaron el `.Rmd`, el verificador, la batería L-C4 v2 (`820e5146…`) ni la lista de L-C7 (`2aa8e742…`). Se
> sellaron cuatro pasos de `cloze/ejercicio_state.json`, cada uno con su propio `complete`, después de comprobar su artefacto
> (18:26, 18:27, 18:28 y 18:30). El paso 11 queda para el profesor. Detalle: `cloze/HANDOFF.md` § «Tanda 9b».

- **Paso 7.** Se selló con la FASE 2C r3, como decidió el profesor el 2026-11-02 (decisión 4 de la tanda 9a).
  `verificar_citas_detractor.py` da exit 0 (9 citas, 0 problemas). La nota declara que MC39 llegó después y que lo revisa el
  próximo detractor.
- **Paso 8.** Se selló con `cloze/salida/coherencias_r9/`. Cumple L-C8: S2 por versión, S2 global y la mutación con texto no
  juzgado, cazada. Da 0 fallos sobre el md5 vigente.
- **Paso 9.**
  - `validar_diversidad_sustantiva.R` y `validar_diagnosticidad.R` se volvieron a correr con N = 100
    (`cloze/salida/*_r9.txt`, con el md5 en la cabecera). Los r8 no lo llevaban, y su procedencia solo se deducía por el mtime.
    Las salidas r9 son idénticas a las r8.
  - Por rama, `diagnosticidad_por_rama_r8.txt`. Unicidad: E5, 300 de 300.
- **Paso 10.** Dos scripts nuevos, de solo lectura, en `cloze/salida/`: `v5_v11_r9.py` y `literalidad_exextra_r9.py`, los dos con
  0 fallos.
  - Los seis campos oficiales (Competencia, Componente, Afirmación, Evidencia `Arg_E1`, Descriptor D3.10 = `niveles_oficiales/N3[9]`
    y Estándar `Aleatorio_6-7[7]`) son literales en el catálogo canónico e iguales, carácter a carácter, a `ITEM_OFICIAL.md`.
  - V6 se acredita por las comprobaciones M6 y M4 del oráculo, que emparejan por contenido. V7 es N/A. V10 se aplica a las
    hipótesis de la Parte 6.
- **¿Ley local o regla?** Ninguna nueva. Los validadores de la familia A no escriben en su salida el md5 del `.Rmd` que midieron,
  y por eso esta tanda tuvo que repetirlos. Se propone como mejora de herramienta al coordinador. No cumple el criterio de regla:
  no produjo ningún defecto y no se ha repetido.

### Aprobación del CLOZE y auditoría del sellado del SCHOICE (2026-11-02)

> **Firmada por el profesor el 2026-11-02**, en el WAIT_USER #3 de la tanda 9b. Respuestas literales: aprobación «[a] Aprobar»;
> candidatas 1 y 2 de la 9b, «Anotar»; candidata 3, «Auditar ahora». Regla #24 **H-5**.

**CLOZE aprobado para el aula: md5 `f98f9dda47e4f3cafc4e8b4e82cc956b`, 11/11** (`aprobacion_usuario` sellado con su nota).
Se aprueba con los residuos declarados en el HANDOFF. MC39 no lo ha revisado ningún detractor; lo revisa el próximo. Uso: Moodle,
con `exportar_moodle_cloze.R`. El DOCX no es apto para estudiantes; para papel, el SCHOICE. **No está en producción:** falta la
evidencia de aula, y la promoción viene después.

**Candidatas 1 y 2 de la 9b, anotadas** en `.claude/docs/HANDOFF_REPO.md`, pendiente 9: el md5 en la salida de los validadores y
el cotejo de `exextra` en el paso 10.

**Auditoría del sellado del SCHOICE (solo lectura; md5 vigente `e293d5c8…`, igual al aprobado).** Los pasos 3 a 6 se sellaron
en lote el 2026-10-29 a las 20:51:24, y los pasos 7 a 10 a las 21:05:03, con milisegundos entre unos y otros. Cada nota nombra
un artefacto que existe y es anterior a su sello:

| Paso | Artefacto | Resultado |
|---|---|---|
| 3 y 9 | `salida/verificar_espacio_ronda4b.txt` | Se sobrescribió a las 22:32, después del sello. Lo hizo el verificador más estricto de `e9c85009` (§P7-E v1.9: «rojo cubierto», nunca APROBADO), sobre el mismo md5. Hoy dice: EN ROJO BAJO OVERRIDE FIRMADO, 13 canales, 0 fallos y 0 pendientes. La nota cita «APROBADO (0 fallos)», que era la salida anterior |
| 5 | `salida/r4b/` | 20 HTML, 3 PDF, 2 NOPS, 3 DOCX y Moodle |
| 6 | informe del hook del 2026-10-29, 20:43 | Declara el md5 |
| 7 | `salida/fase2c_r4b_detractor.md` (21:03:40) | Lleva el marcador y el md5. Las citas las verificó a mano el coordinador a las 21:03:38: 8, sin problemas. Hoy fallan 5, porque los hallazgos H3 y H4 ya se corrigieron |
| 8 | `salida/paso8_r4_coherencias.md` (r4) más la invariancia r4 → r4b (texto visible idéntico en 100/100) | Arrastre declarado en la nota |
| 9 | `salida/r4b/paso9_validadores.txt` | Declara el md5 |
| 10 | `salida/r4b/diff_r3_r4b.patch` | Cero líneas `ex*`: la meta-información no cambió |

**Conclusión:** los sellos tienen artefacto. El lote es un defecto de procedimiento, no un trabajo sin hacer, y no hace falta
reabrir. **Hallazgo nuevo, del repo:** los dos detractores del SCHOICE (r4 y r4b) entregaron su reporte con la herramienta
`SubagentHandback`. El hook de citas solo lee el texto final, así que no los evaluó ni dejó registro, y los cubrió la verificación
manual. Queda en el pendiente 10 de `HANDOFF_REPO.md`, y se lleva al profesor.

**¿Ley local o regla?**
- La aprobación y las anotaciones no crean ley.
- El hueco del hook cumple el criterio de promoción: es general, porque vale para todo detractor; es grave, porque desactiva en
  silencio la defensa contra citas inventadas; se repitió dos veces; ningún gate lo ve, y `test_citas_detractor.R` no lo cubre.
  No crea una regla nueva: amplía la guarda de la regla #9 v1.6. Toca `.claude/` (regla #17), así que lo decide el profesor.
