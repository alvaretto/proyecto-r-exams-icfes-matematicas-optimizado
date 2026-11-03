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

### Ciclo 2026-11-02 (SCHOICE), tanda S1: detenida en el verificador, pendiente de decisión del profesor

> La hizo el `orquestador-schoice` por orden del profesor del 2026-11-02 («Hazlo ya»): el pendiente de arriba. Reapertura #5
> desde `generacion_rmd` (2/11). El `.Rmd` pasa del md5 `e293d5c8…` (aprobado para el aula; sigue en `HEAD`) a
> `5f6f3cf3e89241cd017b9febe0c71ddd`, **sin aprobar**. No se selló ningún paso. Evidencia: `salida/ciclo_20261102/`.

**Qué se aplicó.**
- **S1.** `ED <- "\u00a0"`, nunca el carácter literal. Va entre la cifra (o la fórmula que cierra en `$`) y «kilos» o «curso(s)»
  en las opciones, la Solution y el feedback, y en «El Bosque» y «La Pradera» (los nombres salen en el enunciado, las opciones y
  la Solution). En la canónica, las palabras del impreso no cambian (H-2): el espacio duro va en «40 kilos», «20 kilos» y
  «5 kilos», y la guarda verbatim compara sin él. Las celdas de cifras de la tabla conservan el espacio normal.
- **S5 y S6.** Comentarios de `tabla_responsiva()` y del espacio duro, con el umbral del Moodle del colegio.
- **S7.** Fuera `contexto_tipo`, el campo `tipo` de las plantillas (la estructura queda como comentario, regla #11), la rama
  `dif_cursos` de `verdad()` (un tipo desconocido ahora aborta), `b_debajo` en la canónica y el parámetro `m` de `MOLDES`.
  **`lado_gdis` se conserva en la canónica:** lo leen `rasgos()` de `verificar_espacio.R` y `rec()` de
  `cloze/verificar_espacio_cloze.R` (línea 117). Se quita en la tanda del re-empalme, junto con esa lectura.
- **S3.** `verificar_espacio.R` lleva la canónica regla por regla: `FIRMA_P1_CANON` y `FIRMA_P1_SONDAS`, las 24 cifras de
  la decisión 2 a, con MP1 y MP2. Las nueve al 100 % no pueden subir: no admiten mutante.
- **S4.** La batería declara el estímulo (`ESTIMULO_SC`). Deja de imprimir `SIN_COBERTURA` y las cifras no se mueven.
- **Guardas nuevas** (endurecer): en `data_generation`, «S1: espacio duro entre cifra y unidad y en los nombres de dos palabras»
  y «S1: en la canónica, el espacio duro solo entre cifra y unidad»; en el verificador, la sonda S1-Sol (opciones, Solution y
  feedback). Mutantes M23 a M27, cada uno cazado por su sonda.

**Invariancia, semillas 1 a 1000** (`salida/ciclo_20261102/invariancia_s1_s7.txt`):
- `.Random.seed` sale idéntico al final de `data_generation` en las 1000.
- Enunciado, opciones, Solution y feedback son idénticos salvo el espacio en las 1000.
- La tabla es idéntica byte a byte, y también la clave, la rama y la canónica.
- Quedan 0 cifras separadas de su unidad por un espacio normal. Control positivo, antes: 5111 en las opciones, 25 228 en la
  Solution y 6918 en el feedback.

**Por qué se detuvo.** `verificar_espacio.R` sale con exit 3: PENDIENTE DE FIRMA (1; 0 fallos).
- Las 13 cifras de «O-1 corregido» salen iguales, las 24 de la decisión 2 a salen exactas y no aparece ninguna regla nueva por
  encima de +8 pp.
- Cambia una sola cifra del alcance declarado (`FIRMA_LARGO`): el margen en palabras del canal de largo de la rama PROMEDIO,
  que pasa de 6,5 % a 6,7 %.
- **Causa:** `ntok()` de `.claude/scripts/bateria_referencia_icfes.R` cuenta palabras con `strsplit(…, "\\s+")`, y en R `\s`
  no casa U+00A0, así que «15 kilos» cuenta como una palabra. Todas las opciones pierden las mismas, la diferencia absoluta no
  cambia y baja el denominador (2/31 = 6,45 % pasa a 2/30 = 6,67 %). El estudiante lee las mismas palabras.
- **Comprobado.** Con el texto normalizado antes de medir, el verificador da exit 4, con 37 canales (13 de «O-1 corregido» y
  24 de la decisión 2 a) y 0 pendientes, y el margen vuelve a 6,5 %. Es una copia sin aplicar,
  `salida/ciclo_20261102/verificar_whatif.R`. Diff de cifras: `salida/ciclo_20261102/diff_cifras_antes_s1.txt`.

**Decisión pendiente (regla #24 H-5: relajar nunca es autónomo).**
- (a) Medir sobre el texto que lee el estudiante. El verificador pasaría U+00A0 a espacio antes de la batería y de los márgenes.
  Cambia la entrada del instrumento, no la batería (§P7-C) ni ninguna cifra firmada.
- (b) Firmar el margen nuevo: 6,7 % en palabras.
- (c) Quitar el espacio duro de las opciones. No resuelve el BAJA 1, que está en las opciones.

El CLOZE lee este `.Rmd` y este verificador (`RMD_S` y `VER_S`), así que su E1 deja de coincidir hasta el re-empalme (no
medido). Por F-2, la decisión rige también la Parte 1 del CLOZE.

**¿Ley local o regla?**
- **Candidata a ley local (L-6, espacio duro).** Las cifras y los nombres de dos palabras van con `ED`, nunca con el literal.
  Tiene guardas y mutantes (M23 a M27). Es endurecer; queda como ley si el profesor la confirma.
- **Candidata del repo.** Un contador de palabras con `\s` no separa en U+00A0. Es general (toda batería que cuente palabras
  sobre un ítem con espacio duro), ningún gate lo ve y movió una cifra del alcance firmado. Ampliaría la regla #22 §P7 o la
  herramienta; no crearía una regla nueva. Toca `.claude/` (regla #17), así que lo decide el profesor.

#### Decisión del profesor tras la parada de la tanda S1 (2026-11-02)

> **Firmada por el profesor el 2026-11-02** y transmitida por el coordinador. Respuestas literales: 1 «(a) Normalizar al medir
> (Recomendada)»; 2 «Firmar L-6 (Recomendada)»; 3 «Anotar (Recomendada)». Regla #24 **H-5**.

1. **(a) La batería y los márgenes miden el texto que lee el estudiante.**
   - En `verificar_espacio.R`, `norm_ed()` convierte U+00A0 en espacio en lo que entra a la batería §P7 (también en la
     canónica) y a los márgenes de largo, en caracteres y en palabras.
   - Las guardas S1, S1-Sol y L-6 y los mutantes M23 a M27 y M29 miran el texto crudo, con el espacio duro.
   - **M28** corre el mismo verificador sin la normalización (`VERIF_SIN_NORMALIZAR=1`). Da un solo pendiente, el canal de
     largo de la rama PROMEDIO con el 6,7 % en palabras, y queda cazado por su sonda.
   - **Resultado** (verificador md5 `b9ed1d78…`, `.Rmd` `5f6f3cf3…`, `salida/ciclo_20261102/verificar_espacio_s1_decision_a.txt`):
     exit 4 y 37 canales en rojo cubiertos (13 de «O-1 corregido» y 24 de la decisión 2 a), con 0 fallos y 0 pendientes. El
     margen en palabras vuelve al 6,5 %.
   - **Rige también para la Parte 1 del CLOZE tras el re-empalme (F-2).** Lo aplica la tanda del CLOZE, no la del SCHOICE.
2. **L-6, firmada** (abajo).
3. **`ntok()` del repo, anotado.** El coordinador lo dejó en `.claude/docs/HANDOFF_REPO.md`, pendiente 11. El helper
   compartido no se toca.

### L-6 · Espacio duro entre cifra y unidad y en los nombres de dos palabras (firmada el 2026-11-02)

Endurece L-1 a raíz de los BAJA 1 y 2 de la 6b del CLOZE: un renglón no parte una cifra de su unidad («15 / kilos») ni un
nombre de sede («El / Bosque»).

| Exige | Guarda | Mutante |
|---|---|---|
| `ED <- "\u00a0"` es una variable; el `.Rmd` no lleva el carácter literal | «L-6: el .Rmd no lleva el carácter U+00A0 literal…» (`verificar_espacio.R`) | M29: `ED` con el literal |
| Opciones y enunciado sin una cifra separada de su unidad («kilos», «curso(s)») por un espacio normal, ni «El Bosque» o «La Pradera» | `stopifnot` «S1: espacio duro entre cifra y unidad y en los nombres de dos palabras» | M24 (la canónica, en «40 kilos»), M25 («El Bosque») |
| Canónica: las palabras del impreso no cambian (H-2) y el espacio duro va solo entre cifra y unidad, tres veces | `stopifnot` «S1: en la canónica, el espacio duro solo entre cifra y unidad», más «L-1: canónica verbatim (opciones)», que compara sin el espacio duro | M23 (espacio duro fuera de su sitio), M4 (verbatim) |
| El cierre de las no canónicas lleva el espacio duro | `stopifnot` «regla #13 v1.1: la cifra va solo en el complemento» | M26 |
| Opciones, Solution y feedback emitidos sin una cifra ni un nombre partido | sonda S1-Sol (`verificar_espacio.R`) | M27 |
| La batería §P7 y los márgenes miden con U+00A0 convertido en espacio (decisión (a)) | `norm_ed()` | M28 |

La tabla queda fuera: sus celdas de cifras conservan el espacio normal. Es una decisión declarada, sin guarda; la tabla salió
idéntica byte a byte en las semillas 1 a 1000.

#### Cierre de la tanda S1 (2026-11-02): pasos 3 a 6 sellados, uno por uno

- **md5 final del `.Rmd`: `4cffbe8b1924b749c36d2b92369e4a83`.** La primera corrida del arsenal dio 1 error en la FASE 2D: el
  comentario de S5 nombraba el identificador de un argumento de la F2 terminado en «-cion». Se reescribió el comentario.
  Reapertura #6 desde `generacion_rmd`. La salida es idéntica byte a byte a la de `5f6f3cf3` en 1000 de 1000 semillas
  (`salida/ciclo_20261102/invariancia_s1_s7.txt`).
- **Verificador** (md5 `b9ed1d78…`, `salida/ciclo_20261102/verificar_espacio_s1_final.txt`): exit 4 y 37 canales cubiertos
  (13 de «O-1 corregido» y 24 de la decisión 2 a), con 0 fallos y 0 pendientes. M1 a M29, MP1 y MP2 quedan cazados por su sonda.
- **Sellos.** Cada paso se selló con su propio `complete`, en una llamada aparte, después de su artefacto:
  `generacion_rmd`, `retroalimentacion` (regla #19: 0 coincidencias), `renderizado_4_formatos`
  (`salida/ciclo_20261102/render_4cffbe8b/`: HTML, PDF, DOCX y NOPS; Moodle en `moodle_100/`, con N = 100) y
  `arsenal_post_render`. Quedan 6 de 11.
- **Arsenal:** 0 errores y 7 advertencias heurísticas. La FASE 2G da 100/100 y la 2H, 0 anomalías; la 2O no encuentra glifos
  que rompan pdflatex. En el PDF, `\pandocbounded` aparece 0 veces. La canónica se revisó a ojo en sus 4 páginas.
- **Caja del Moodle del colegio:** `salida/r4b/medir_qtext_moodle.py` queda **NO VERIFICABLE**, porque el servidor respondió
  HTTP 530. Como evidencia sustituta: en los 100 XML de Moodle (semillas 1 a 100), el HTML de la tabla es idéntico byte a byte
  al de `e293d5c8`, y el resto del enunciado también, salvo el espacio duro
  (`salida/ciclo_20261102/tablas_moodle_vs_e293d5c8.txt`). Por eso la medición del 2026-10-30 (0/100 tablas fuera de su caja a
  320 y a 360 px) vale para la tabla por identidad. La caja no se volvió a medir.
- **6b pareada** (`auditor-visual-html`, sin `name:`): seguía en marcha cuando el orquestador agotó sus turnos. Escribe en
  `salida/ciclo_20261102/6b/`, y su reporte se recupera de la transcripción (Paso 0 de la regla #9).

**¿Ley local o regla?** L-6 queda firmada (arriba). La herramienta de edición convirtió tres veces en literal un U+00A0
escrito como escape en texto del agente. En este `.Rmd` lo vigila ya la guarda de L-6 con M29. La candidata del repo es la
misma que la de `ntok()`: el espacio duro es invisible para los gates. Se anota en ese pendiente y no crea ley nueva.
**6b pareada, recibida** (`auditor-visual-html`, sin `name:`; resultado: APTO_CON_OBSERVACIONES). La muestra fueron 100
semillas pareadas contra `e293d5c8`, a 360 y 320 px: 400 capturas por CDP, con `innerWidth` comprobado. Evidencia en
`salida/ciclo_20261102/6b/` (`metricas.json`, `shots/`, `pares/`). El auditor leyó a ojo 4 de los 12 pares, y lo declara.

| Métrica (100 semillas) | 360 ref → nuevo | 320 ref → nuevo |
|---|---|---|
| Desborde horizontal, o tabla fuera de su caja | 0 → 0 | 0 → 0 |
| «cifra kilos/curso(s)» en texto, partidos | 341 → **0** (de 2003) | 347 → **0** (de 2003) |
| «El Bosque»/«La Pradera» fuera de la tabla, partidos | 74 → **0** (de 475) | 55 → **0** (de 475) |
| Fórmula `$…$` seguida de «kilos», partidos | 139 → **132** (de 1158) | 259 → **259** (de 1158) |

- **MEDIO, residuo de S1.** Cuando la cifra cierra una fórmula de la Solution («a × b = T kilos», «A − B = D kilos más»),
  Chromium admite un salto entre el `<math>` y el U+00A0, así que «kilos» sigue quedando sola en el renglón siguiente. Además
  ese renglón empieza ahora con un espacio: es un BAJO, que en la referencia no se veía porque el espacio colapsaba. No afecta a
  las opciones ni al enunciado.
  - Así, el BAJA 1 del CLOZE queda cerrado en las opciones y en el texto plano, pero no en las fórmulas de la Solution.
  - La guarda S1-Sol de L-6 mira el texto fuente, no el renglón, así que no lo ve.
  - El auditor sugiere, sin haberlo probado, envolver fórmula, espacio duro y unidad en `white-space:nowrap`, o usar U+2060.
    Cambiar eso toca la Solution, así que lo decide el profesor.
- **BAJO.** En 23 semillas a 360 px y en 1 a 320 px hay uno o dos renglones más («15 kilos» pasa entero al renglón
  siguiente). Es el efecto esperado, sin desbordes.

### Ciclo 2026-11-02 (SCHOICE), tanda S1b

> La hizo el `orquestador-schoice` en dos sesiones. La primera, el 2026-11-02, se interrumpió a las 20:34, sin reporte
> final y a mitad de una medición. La relanzada, el 2026-11-03, la terminó. Evidencia: `salida/ciclo_20261102/s1b/`; la
> de la relanzada, en `s1b/relanzada/`.

**La primera S1b (2026-11-02, decisión del profesor «Arreglarlo ahora»).**
- Aplicó V1: cuando la cifra cierra una fórmula, la unidad va dentro de ella, `$… = T\ \text{kilos}$`, en 20 sitios.
  Así quedó el md5 `5cf5b12f02e942091a3af899a81a2d6c`, con la guarda «L-6: la unidad que cierra una fórmula va dentro de la
  fórmula», M30, M31 y la sonda S1-Sol fórmula. Selló los pasos 3 a 6.
- **6b de V1: APTO_CON_OBSERVACIONES** (`s1b/6b/informe_6b_v1_5cf5b12f.md`).
  - MEDIO: cuando la fórmula queda al final del renglón, el punto baja solo al siguiente: 34 semillas a 360 px y 5 a 320 px,
    más 2 «:». Chromium admite un corte entre el `<math>`, que es atómico, y el signo que lo sigue.
  - BAJO: «kilos» sale con la letra de la fórmula.
  - BAJO: la fórmula entera queda en un renglón propio.
- La medición posterior (V2 y V1′) quedó sin cerrar, y su instrumento tenía dos defectos:
  1. «Fórmula + unidad partidas» no reconocía la unidad con el signo dentro y contaba sobre 428, no sobre 1158.
  2. El detector de huérfanos solo miraba `[.,:;]`.

**Decisión del profesor (2026-11-03).** Respuesta literal: «V1′ (Recomendada)». V1′ es V1 con el signo que sigue a la
unidad dentro de la fórmula: `$… = T\ \text{kilos.}$`. Se mide en HTML, PDF y Moodle, pareada con V1 y V2, y se aplica solo
si cierra en los tres. **Ampliación**, con respuesta literal «En esta S1b (Recomendada)»: en la misma reapertura se quita la
asignación muerta de `lado_gdis` en la canónica. Es el cierre de S7, el código muerto que quedaba.

**Medición pareada** (semillas 1 a 100; resumen en `relanzada/resumen_medicion_s1b2.txt`). El instrumento corregido es
`relanzada/cap_s1b2.py`. Sigue en HTML.

| HTML, 360 / 320 px | e293d5c8 | 4cffbe8b | V1 | V2 | V1′ |
|---|---|---|---|---|---|
| fórmula + unidad partidas (de 1158) | 139 / 259 | 132 / 259 | 0 / 0 | 0 / 0 | **0 / 0** |
| signo huérfano tras una fórmula, todos los signos | 0 / 0 | 0 / 0 | **34 / 5** | 0 / 0 | **0 / 0** |
| renglón que empieza con un espacio tras una fórmula | 0 / 0 | 132 / 259 | 0 / 0 | 0 / 0 | **0 / 0** |
| cifra-unidad (de 2403) · El/La (de 499) | 341 / 347 · 74 / 55 | 0 · 0 | 0 · 0 | 0 · 0 | **0 · 0** |

- En V1 el signo queda solo en las mismas semillas que vio su 6b.
- Desbordes, tablas fuera de su caja y fórmulas partidas por dentro: 0 en las cinco.
- **PDF** (100 ejercicios). Se mide con coordenadas, con `relanzada/pdf_renglones_bbox.py`:
  - Renglones que empiezan por la unidad tras una fórmula: 122 en e293d5c8 y 0 en las demás.
  - Renglones que empiezan por un signo: 0 en las cinco. El control positivo sintético, un salto de renglón forzado ante el
    punto, da 20 de 20.
  - `pdftotext` sin coordenadas daba 62 en las cinco, y la primera versión del detector 38. La causa es la negrita
    «**total**.»: es una fuente Type 3 con otra caja vertical, y la extracción la separaba de su renglón. El PDF está bien.
  - Compila con `\text{kilos.}` (730 veces).
- **Moodle** (n = 100, `set.seed(1)`):
  - V1′: 733 unidades con el signo dentro de `\(…\)`, 0 signos tras el cierre de una fórmula con unidad y 0 `<math>`.
  - V1 es el control positivo: 733 signos fuera.
- **V1′ cierra en los tres formatos.**

**Aplicación.**
- `.Rmd` md5 **`6c9d77058b32fdd8291a1ebaab581c8f`**. Son 14 sitios: la cadena `\\ \\text{kilos}$` aparece 21 veces en el
  fuente de V1.
  - 1 es el comentario de cabecera (línea 26). Por eso V1 tocó 20 sitios.
  - 6 van seguidos de texto sin signo: « y », « que compensan», « más», « a favor de».
  - 14 llevan el signo pegado.
  - Ningún signo viene en otra cadena del `paste0`/`cat` (`relanzada/generar_v1p2.py`).
- Prueba sobre el texto emitido, semillas 1 a 1000: los signos tras el cierre de una fórmula con unidad bajan de 7298 a 0.
- Además, la guarda «L-6: el signo que sigue a la unidad de una fórmula va dentro de la fórmula» (endurecer) y S7.

**Invariancia** (`relanzada/invariancia_*.txt`, semillas 1 a 1000):
- `.Random.seed` idéntico en las 1000.
- Enunciado, opciones y tabla idénticos byte a byte.
- Solution y feedback idénticos tras normalizar la posición del signo.
- La V1′ medida y la aplicada coinciden byte a byte.
- Quitar `lado_gdis` no cambia ni un byte de la salida.

**Verificador** (md5 `bd6eda09d54c8dae355371e291df95f6`, `relanzada/verificar_espacio_s1b2.txt`):
- Exit 4, con 37 canales (13 de «O-1 corregido» y 24 de la decisión 2 a), 0 fallos y 0 pendientes. Ninguna cifra firmada se
  mueve: el diff con la salida de V1 solo añade líneas.
- Cambios:
  - **S1-Sol signo**, con su control positivo: el signo dentro aparece en 100 de 100 versiones.
  - **M32**, cazado por esa sonda en 200 casos; S1-Sol y S1-Sol fórmula no lo ven.
  - **M33**, cazado por la guarda nueva.
  - **M30**, adaptado a `kilos.` y cazado.
  - **M31**, sin cambios y cazado.
  - El control positivo de S1-Sol fórmula había bajado de 1158 a 428 y seguía en OK. Ahora cuenta la unidad con o sin signo.
  - **S7:** `rasgos()` calcula el lado de la canónica a partir de sus cifras. Un `ok()` comprueba que da «encima» (x = 40,
    D = 20).
- La salida con y sin S7 es idéntica línea a línea.

**El CLOZE.** Hasta el re-empalme, `cloze/verificar_espacio_cloze.R` no puede correr la canónica contra este `.Rmd`, porque
`rec()` (línea 117) lee `lado_gdis`. Ya estaba desfasado desde S1: su E1 no coincide. Por F-2, V1′ rige también la Parte 1.

**Workflow.**
- Reapertura #8, 2026-11-03 06:59:39.
- Sellos, cada uno con su propio `complete` y después de su artefacto: `generacion_rmd` a las 07:04:12, `retroalimentacion`
  a las 07:04:21, `renderizado_4_formatos` a las 07:06:32 y `arsenal_post_render` a las 07:06:40. Quedan 6 de 11.
- El render está en `s1b/render_6c9d7705/`: HTML, PDF, DOCX, NOPS y Moodle n = 100.
- Arsenal: 0 errores y 7 advertencias heurísticas. La FASE 2G da 100/100 y la 2H, 0 anomalías.

**Candidata a cláusula de L-6, pendiente de la firma del profesor** (la tabla firmada de L-6 no se toca):

| Exige | Guarda | Mutante |
|---|---|---|
| Una fórmula que cierra en su unidad («kilos», «curso(s)») lleva dentro el signo que la sigue (`. , : ; ! ? ) ] » ”` y `…`): `$… = T\ \text{kilos.}$` | `stopifnot` «L-6: el signo que sigue a la unidad de una fórmula va dentro de la fórmula» (feedback y análisis de los distractores) y sonda S1-Sol signo de `verificar_espacio.R` (opciones, Solution y feedback emitidos; control positivo: el signo dentro en todas las versiones) | M33 (el feedback de la clave deja el punto fuera) y M32 (la lista de totales de la Solution lo saca) |

**¿Ley local o regla?**
- Local: la cláusula de arriba. Endurece, y la firma el profesor.
- Del repo, candidata:
  - Un control positivo con umbral bajo (`>= N`) no ve que su cuenta cae cuando cambia el texto que cuenta. Le pasó al
    instrumento HTML y al verificador.
  - `pdftotext` separa de su renglón la negrita en fuente Type 3. Una medición de renglones del PDF va por coordenadas y
    por solape vertical.
  - Las dos son generales e invisibles a los gates. Tocan `.claude/`, así que lo decide el profesor.

**6b pareada de la tanda S1b relanzada, recibida** (`auditor-visual-html`, sin `name:`): **APTO_CON_OBSERVACIONES**, sin
CRÍTICOS, ALTOS ni MEDIOS. Reporte íntegro: `salida/ciclo_20261102/s1b/6b/relanzada/informe_6b_6c9d7705_completo.md`.
- N = 100 semillas por versión y ancho, a 360 y 320 px. El HTML final es idéntico byte a byte al de la V1′ medida
  (`cmp`, 100 de 100).
- Frente a 4cffbe8b, e293d5c8 y 5cf5b12f, V1′ da 0 en las seis métricas. Los controles positivos reproducen sus cifras: 132
  y 259, 34 y 5, y 341/74 y 347/55.
- Leyó 9 imágenes a resolución completa: 1 par y 8 recortes, de las semillas 1, 5 y 7 a 360 px y 24, 25, 40, 53 y 59 a
  320 px. En V1 se ve el signo solo en las 8 semillas; en el final, en ninguna.
- BAJOS:
  - «kilos.» sale con la letra de la fórmula (ya declarado en V1).
  - La fórmula pasa entera al renglón siguiente y deja uno corto antes.
  - A 320 px, «= 210 kilos.» queda cerca del borde derecho (semilla 53), sin desborde medido.

**Verificación del coordinador (2026-11-03, sesión `42032564`), independiente de los scripts de la tanda**
(`salida/ciclo_20261102/s1b/relanzada/coordinador/`):
- `verificar_espacio.R`, repetido: exit 4 en 110 s, con las 283 líneas idénticas a `relanzada/verificar_espacio_s1b2.txt`
  (sin contar la línea de cierre).
- Invariancia con un script propio (`invariancia_coordinador.R`), del `.Rmd` de HEAD (`4cffbe8b`) frente a `6c9d7705`, en
  las semillas 1 a 1000:
  - `.Random.seed` idéntico en las 1000;
  - objetos, enunciado y opciones idénticos en las 1000;
  - Solution y feedback idénticos en las 1000, tras devolver la unidad y el signo a la forma vieja;
  - 0 fórmulas que cierran antes de su unidad y 0 signos fuera;
  - 11 575 unidades dentro de la fórmula (el control viejo da 11 575 fuera), 7 298 de ellas con el signo dentro.
- Diff del `.Rmd`, revisado. Solo cambian:
  - cadenas dentro de los chunks: 20 cierres, 14 de ellos con signo;
  - S7: 4 líneas sustituidas por 2 de comentario;
  - dos `stopifnot` nuevos.
  Ninguna prosa Markdown cambia.
- Corrección al reporte de la tanda: `salida/` sí está ignorado (`.gitignore`, `**/salida/`). La evidencia se añade con
  `git add -f`, sin los `render_*`, las `shots/` ni perfiles de Chromium.

### Ciclo 2026-11-02 (SCHOICE), tanda S2a

> La hizo el `orquestador-schoice` el 2026-11-03, tras la FASE 2C r5 (veredicto del detractor r5: APROBAR_CON_CAMBIOS, sin
> ninguna CORRECCIÓN; reporte íntegro en `salida/fase2c_r5_detractor.md`, con las 14 citas verificadas por el coordinador).
> Reapertura #9 desde `generacion_rmd` (2026-11-03 07:59:59). El `.Rmd` pasa de `6c9d7705…` a
> **`63e2713e6ac0bdbf250f4dc33901b2ec`**, y `verificar_espacio.R` de `bd6eda09…` a **`bdd0daf06ba933a1eff873770a755d59`**.
> Evidencia: `salida/ciclo_20261102/s2a/`. La tanda se detuvo antes de la FASE 2C r6, que lanza el coordinador con un
> detractor nuevo.

**Decisión del profesor (2026-11-03) sobre H2.** Respuesta literal: «Cerrarlo ahora (Recomendada)». Regla #24 **H-5**.
- El punto que sigue a la fórmula de la explicación de ERR-RESTA entra en la fórmula: `" = ", p$delta, "\\text{.}$ Esa resta…"`,
  en lugar de `"$. Esa resta…"` (línea 262 de `6c9d7705`; 265 de `63e2713e`).
- La sonda de signo y la guarda de L-6 pasan a cubrir TODA fórmula, con unidad o sin ella, cada una con su mutante.

**Decisiones del coordinador** (endurecer, H-5): en H1, controles exactos con un mutante cada uno, separadores ampliados y el
mutante de la r5 con U+2009, U+202F y U+2060; en H3, la tabla candidata de L-6 con la fila de V1 y la del signo, sin tocar la
firmada; en H4, `cap_s1b2.py` sin literales, con su assert y su descripción; en H5, una guarda real del canal 2 con su
mutante; y en H2, la fe de erratas del registro.

**Cómo se atendió cada hallazgo de la r5**

| Hallazgo | Qué se hizo | Cifras |
|---|---|---|
| H1 (MEDIA): los controles de S1-Sol no ven que su cuenta baje, y las sondas son una lista negra de separadores | Controles exactos: `n_dentro == 1158`, `sum(v_dentro_s) == 730` y uno nuevo, el signo dentro de una fórmula sin unidad (`\text{.}$`), que tiene que salir una vez en cada versión de E1 con ERR-RESTA como distractor de G y en ninguna otra. Entre el cierre de la fórmula y la unidad (S1-Sol fórmula), o entre el cierre y el signo (S1-Sol signo), las sondas admiten hasta tres caracteres cualesquiera que no sean letra, cifra, `$` ni salto de línea. Además, el texto emitido se calcula ahora con el `data_generation` del mismo texto que la Solution: antes, `s1_sol()` usaba siempre el del `.Rmd` sin mutar, así que un mutante del `data_generation` no llegaba a la Solution emitida | ERR-RESTA en E1: 42 de 100, lo que contó la r5. Falsos positivos de la clase ampliada: ninguno. En V1′ da 0 unidades fuera y 42 signos fuera, que son los de ERR-RESTA; en V1, 772 signos fuera, que son 730 + 42 (`s2a/antes_sondas_6c9d7705_y_v1.txt`). M34: 1158 → 1151; M35: 730 → 721; M36: 42 → 0; a cada uno lo caza solo su control. M39a, M39b y M39c (el mutante de la r5: la lista de totales saca la unidad de la fórmula con U+2009, U+202F o U+2060): 200 casos en S1-Sol fórmula y 0 con la clase anterior. M41a, M41b y M41c (el signo fuera tras esos mismos separadores): 200 en S1-Sol signo y 0 con la anterior |
| H2 (BAJA): una fórmula sin unidad seguida de un signo | Cerrado por la decisión del profesor. La guarda pasa a «L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula» (línea 601), con la clase ampliada. M37 (revierte H2) muere por ella en la semilla 1, y la guarda anterior no lo veía; M33, con el mensaje nuevo, también muere en la semilla 1. La sonda S1-Sol signo, generalizada: M38 (la lista de totales pierde la unidad y deja el punto fuera) da 200 casos, y 0 con la sonda anterior | En las semillas 1 a 1000, 0 fórmulas seguidas de un signo fuera de ellas, con unidad o sin ella; antes, 425. Hay 425 `\text{.}$`, uno por versión con ERR-RESTA |
| H3 (BAJA): la guarda de V1 lleva el prefijo «L-6:», pero no está en ninguna tabla | Tabla candidata nueva (abajo), con la fila de V1 y la del signo, ya general. La tabla firmada no se toca | — |
| H4 (BAJA): `cap_s1b2.py` decía no llevar literales y llevaba 10 U+00A0 | Los 10 U+00A0 del JS pasan a su escape de JavaScript, y también los `» ” …` de sus clases (3 de cada uno). Un assert comprueba que ni el JS ni el propio fuente llevan U+00A0, U+2009, U+202F o U+2060 literales. La descripción dice ahora lo que hace el código. Script: `s2a/editar_cap_s2a.py`; copia previa: `s2a/cap_s1b2_antes.py.txt` | md5 `e621bad3…` → `1d0117ed…`. Con los escapes deshechos, el JS es idéntico al de antes (`diff` vacío). Los controles de la 6b, abajo |
| H5 (BAJA): el `ok()` de S7 era tautológico y el campo `lado` no tenía consumidor | Guarda real del canal 2 en E2, sobre E1 y los estratos: `x$canon || ((x$X < x$D) == (x$lado == "debajo"))`. `rasgos()` da `lado = NA` en la canónica, que no sortea lado. Fuera el `ok()` tautológico. M40 quita el filtro `cand_g <- cand_g[lado_ok, ]` | 291 no canónicas (156 debajo de D y 135 encima), 0 fuera de su lado. M40: 32 de las 100 versiones de E1 quedan fuera de su lado, sin ningún error: cazado por su sonda |

**Fe de erratas (H2 de la r5).** El HANDOFF, en la sección «Tanda S1b», decía: «en HTML hubo 0 partidas en 100 semillas por
ancho (42 casos)». Esos 42 casos no eran 84 ensayos. La fórmula abre su propio párrafo tras un prefijo fijo y solo varía el
número de cifras de δ, así que en cada ancho comparten prácticamente la misma geometría: eran 2 observaciones (360 y 320 px
de `exams2html`). Aunque hubieran sido independientes, con la tasa de V1 (34 de 730, un 4,7 % a 360 px) se esperaban 2,0
casos partidos en 42, y ver 0 tenía una probabilidad del 13 %. Tampoco se midió en la caja del Moodle del colegio. Queda
cerrado por construcción con este cambio: 0 fórmulas seguidas de un signo fuera en 1000 semillas.

**Invariancia** (`s2a/invariancia_s2a_1000.txt`; de `6c9d7705` a `63e2713e`, semillas 1 a 1000, 19 s):
- `.Random.seed`, los objetos (`opciones`, `sol`, `rama`, `es_canonica`, `tabla_md`, `tabla_df`, `enunciado`), el enunciado,
  las opciones y la tabla: idénticos en las 1000.
- La Solution es idéntica byte a byte en 575. Las 425 que difieren son las de ERR-RESTA, y tras devolver el signo a su sitio
  quedan idénticas las 1000. El feedback es idéntico byte a byte en las 1000.
- 0 fórmulas que cierran antes de su unidad y 0 seguidas de un signo fuera; 11 575 unidades dentro de la fórmula, 7 298 de
  ellas con el signo dentro; 0 U+2009, U+202F o U+2060 en el texto emitido. Control: `6c9d7705` da 425 signos fuera.
- Ni el `.Rmd` ni el verificador llevan U+00A0, U+2009, U+202F o U+2060 literales (`grep -P` y una guarda nueva del
  verificador). Regla #25: sin glifos que rompan pdflatex. Regla #19: 0 coincidencias.

**Verificador** (`s2a/verificar_espacio_s2a.txt`, 109 s): exit 4, con 37 canales en rojo cubiertos (13 de «O-1 corregido» y
24 de la decisión 2 a), 0 fallos y 0 pendientes. M28 sale con exit 3 y su único pendiente. Ninguna cifra firmada se mueve.
Frente a la salida de la S1b relanzada, solo cambian la línea de S7 (sustituida por la de H5), la de M33 (por el mensaje
nuevo) y las de S1-Sol; se añaden las de M34 a M41 y la de los literales (`s2a/diff_verificador_s1b2_a_s2a.txt`).

**Render y arsenal.** Los pasos 3 a 6 se sellaron uno por uno, cada uno después de su artefacto: `generacion_rmd` a las
08:09:05, `retroalimentacion` a las 08:09:11, `renderizado_4_formatos` a las 08:12:08 y `arsenal_post_render` a las 08:12:18.
Quedan 6 de 11.
- Render oficial (`s2a/render_63e2713e/`): PDF 3 (semillas 1, 2 y 7), HTML 3, DOCX 3, NOPS 2 y Moodle n = 100.
- `exams2pdf` va el primero del comando, con `dir=` y `texdir=` literales. Así el hook vio el PDF (2B, 3 páginas, leídas) y
  los `.tex` (2I), y desaparece la séptima advertencia de la S1b, «PDF no encontrado».
- Arsenal: 0 errores y 6 advertencias heurísticas; la 2G da 100/100 y la 2H, 0 anomalías. `\pandocbounded`: 0 usos.
- Moodle (n = 100, `set.seed(1)`; `s2a/moodle_s2a.txt`): 0 signos tras el cierre de una fórmula, con unidad o sin ella, y 40
  `\text{.}` de ERR-RESTA. Controles: `6c9d7705` da 40 signos fuera, y V1, 773 (733 + 40).
- PDF (100 ejercicios, por coordenadas): 0 renglones que empiezan por un signo, como en V1′ y V1.
- El HTML de medición es idéntico byte a byte al de `6c9d7705` en las 58 semillas sin ERR-RESTA.

**Tabla candidata de L-6, pendiente de la firma del profesor** (H3 de la r5). Sustituye a la candidata de la tanda S1b, que
solo tenía la fila del signo que sigue a la unidad. La tabla firmada de L-6 no se toca.

| Exige | Guarda | Mutante |
|---|---|---|
| La unidad que cierra una fórmula («kilos», «curso(s)») va dentro de ella: `$… = T\ \text{kilos}$` (V1; tanda S1b, decisión «Arreglarlo ahora») | `stopifnot` «L-6: la unidad que cierra una fórmula va dentro de la fórmula» (línea 594 de `63e2713e`; feedback y análisis de los distractores) y sonda S1-Sol fórmula de `verificar_espacio.R` (opciones, Solution y feedback emitidos; cualquier separador que no sea letra ni cifra; control exacto: 1158 unidades dentro de la fórmula en E1) | M30 (el feedback de la clave cierra la fórmula antes de «kilos») y M31 (la Solution, en «kilos a favor de», con el espacio duro); M39a, M39b y M39c (la lista de totales, con U+2009, U+202F o U+2060); M34 (el control: quita un sitio, 1158 → 1151) |
| El signo que sigue a una fórmula, con unidad o sin ella (`. , : ; ! ? ) ] » ”` y `…`), va dentro de ella: `$… = T\ \text{kilos.}$` y `$… = δ\text{.}$` (V1′, tanda S1b relanzada, decisión «V1′ (Recomendada)»; generalizada en la tanda S2a, decisión «Cerrarlo ahora (Recomendada)») | `stopifnot` «L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula» (línea 601; feedback y análisis de los distractores; cualquier separador) y sonda S1-Sol signo (opciones, Solution y feedback emitidos; controles exactos: 730 unidades con el signo dentro y 42 signos dentro de una fórmula sin unidad, uno por versión de E1 con ERR-RESTA) | M33 y M37 (la guarda: el feedback de la clave y la explicación de ERR-RESTA dejan el punto fuera); M32, M38, M41a, M41b y M41c (la sonda: la lista de totales lo saca, sin unidad o tras U+2009, U+202F o U+2060); M35 y M36 (los controles: quitan un sitio, 730 → 721 y 42 → 0) |

Queda declarado, sin guarda: la tabla del enunciado conserva el espacio normal en sus celdas de cifras (L-6 firmada). Por
F-2, las dos filas rigen también la Parte 1 del CLOZE tras el re-empalme; esta tanda no tocó `cloze/`.

**¿Ley local o regla?**
- **Local:** la tabla candidata de arriba. Endurece, y la firma el profesor.
- **Del repo, candidatas.** Tocan `.claude/` (regla #17), así que lo decide el profesor:
  1. Un control positivo con umbral (`>= N`, `> 0`) no ve que su cuenta baja. Se cablea como conteo exacto con un mutante
     que quite un solo sitio. Ya era candidata en la S1b, y la r5 lo volvió a encontrar.
  2. Una clase de separadores escrita como lista negra no ve el separador siguiente: se escribe por complemento («todo lo
     que no sea letra ni cifra»).
  3. Un verificador que evalúa la Solution con el `data_generation` sin mutar no ve en el texto emitido los mutantes del
     `data_generation`. Es una forma de «la mutación no llegó al entorno» (`INC-MUTANTE-SONDA`); apareció al construir M36.
  4. En el paso 5, `exams2pdf` va el primero del comando, con `dir=` y `texdir=` literales: el hook toma el primer `dir=` que
     encuentra.

**Próximo paso:** la 6b pareada (abajo, cuando llegue) y la FASE 2C r6, con un detractor nuevo y sin `name:`. Qué tiene que
mirar la r6:
- el diff de `6c9d7705` a `63e2713e` (una cadena, el mensaje y el patrón de la guarda, y comentarios) y su invariancia;
- `verificar_espacio.R` `bdd0daf0…`: los controles exactos, `textos_sol()` con el `data_generation` del texto mutado, los
  mutantes M34 a M41 (que cada uno muera por su sonda y que los de control quiten un solo sitio), la guarda de H5 y M40, y si
  la clase ampliada puede dar un falso positivo con un `$` de apertura;
- `cap_s1b2.py` `1d0117ed…` y su equivalencia con el anterior;
- la tabla candidata de L-6, contra el código;
- y si queda otra fórmula de la Solution, la que sea, seguida de un signo fuera de ella en algún formato.

**6b pareada de la tanda S2a, recibida** (`auditor-visual-html`, sin `name:`): **APTO_VISUAL**, sin ningún hallazgo. Reporte
íntegro: `salida/ciclo_20261102/s2a/6b/informe_6b_63e2713e_completo.md`.
- N = 100 semillas por versión y ancho, a 360 y 320 px: 1000 mediciones por CDP, con 0 errores y `innerWidth` comprobado.
- Signo tras toda fórmula, con unidad o sin ella: en `63e2713e`, 0 (denominador 0) en los dos anchos; en `6c9d7705`, 42, con
  0 partidos. Las demás métricas de la S1b dan 0, y las alturas son idénticas a las de `6c9d7705` (±0 px).
- Controles positivos, iguales a los de antes en las 8 cifras: 4cffbe8b 132/259 (fórmula + unidad partidas), V1 34/5 (signo
  huérfano) y e293d5c8 341/347 (cifra-unidad) y 74/55 (El/La). Prueban que `cap_s1b2.py` con escapes (H4) mide lo mismo.
- PDF (100 ejercicios, por coordenadas): 0 renglones que empiezan por un signo, igual que V1′. Moodle: 0 signos tras el cierre
  de una fórmula; controles: 40 en `6c9d7705` y 773 en V1.
- Leyó a ojo 12 recortes, 6 pares de `63e2713e` y `6c9d7705`, de las semillas 1, 4 y 10 a 360 y a 320 px. El punto queda pegado
  a la cifra y «Esa resta…» sigue en el mismo renglón. El orquestador miró además el de la semilla 1 a 320 px.
- Límite declarado: en las 42 semillas con ERR-RESTA la fórmula acaba a mitad de renglón (x = 126 a 134 a 360 px; 209 a 217 a
  320 px), así que el peor caso, la fórmula al final del renglón, no se da en esos dos anchos. El cierre de H2 no depende de
  eso, porque es por construcción.
- Desviación declarada: `cap_s1b2.py` corrió sin `-I`, desde un directorio fuera del repo, porque `websockets` está en el site
  de usuario. Precisión del orquestador: el 40 de Moodle y el 42 de HTML son muestras distintas (el Moodle de 100 sale de un
  solo `set.seed(1)`, no de las semillas 1 a 100); ninguna es parte de la otra.

**Verificación del coordinador de la tanda S2a (2026-11-03, sesión `42032564`)**, independiente de sus scripts
(`salida/ciclo_20261102/s2a/coordinador/`):
- `verificar_espacio.R` (`bdd0daf0…`), repetido: exit 4 en 110 s, con las 297 líneas idénticas a `s2a/verificar_espacio_s2a.txt`
  (sin la línea de cierre). Los 13 mutantes del 33 al 41 quedan cazados. No hay fallos ni pendientes, salvo el de M28.
- Invariancia con un script propio, de `6c9d7705` (HEAD `e15e1e2e`) a `63e2713e`, en las semillas 1 a 1000:
  - `.Random.seed`, los objetos, el enunciado, las opciones y el feedback son idénticos en las 1000;
  - la Solution es idéntica byte a byte en 575 semillas y, al devolver el punto a su sitio, en las 1000.
- Signo tras el cierre de CUALQUIER fórmula, en el texto emitido (`contar_signo_toda_formula.R`, semillas 1 a 1000):
  - `6c9d7705`: 425, en 425 semillas, todas de ERR-RESTA;
  - `63e2713e`: 0, con 425 signos dentro de una fórmula sin unidad.
- La 6b dice «Veredicto: APTO_VISUAL», sin hallazgos. Diff del `.Rmd` revisado: 3 líneas de comentario, el sitio de ERR-RESTA y
  la guarda del signo generalizada.

### Ciclo 2026-11-02 (SCHOICE), tanda S2b

> La hizo el `orquestador-schoice` el 2026-11-03, tras la FASE 2C r6 (veredicto del detractor r6: APROBAR_CON_CAMBIOS, sin
> ninguna CORRECCIÓN; reporte íntegro en `salida/fase2c_r6_detractor.md`, con las 17 citas verificadas por el coordinador).
> Solo cambian el verificador, el instrumento y el registro. El `.Rmd` sigue en `63e2713e6ac0bdbf250f4dc33901b2ec`, sin
> reapertura y sin sellos (6/11). `verificar_espacio.R` pasa de `bdd0daf0…` a **`9c65474801b5b838183a0507233b8ea1`**, y
> `cap_s1b2.py`, de `1d0117ed…` a `aaa1b49e8b3134cb4ed24bed5a263e83`. Evidencia: `salida/ciclo_20261102/s2b/`.

**Cómo se atendió cada hallazgo de la r6**

| Hallazgo | Qué se hizo | Resultado |
|---|---|---|
| H1 (ALTA, DIAGNOSTICIDAD): el canal «resta», sin medir | E4 mide «resta» sola y sus seis combinaciones «… + resta» con las pistas firmadas de «O-1 corregido», con el criterio de R_dif (primera cifra de cada opción y `sc()`), en la mezcla de E1, en las ramas CURSOS y PROMEDIO (N = 100 cada una) y en la canónica por enumeración exacta (24 órdenes). Ninguna está firmada, así que la que pasa de +8 pp queda pendiente. Además: el barrido del estímulo, M44 y `ESTIMULO_SC` corregida, que ahora dice qué canales del estímulo mide E4 y cuáles no | 27 de las 28 cifras de H1, pendientes. Reproducen las del detractor y las del coordinador. M44 (ERR-RESTA siempre que se pueda): su cuota en el distractor de G pasa de 45 a 63 % en CURSOS y de 57 a 60 % en PROMEDIO, y «resta» sube de 58,8 a 72,2 % y de 67,8 a 70,0 %: cazado por su sonda |
| H2 (BAJA): los mutantes de sonda de la S2a no aíslan su sonda | `juzgar_s1()` simétrico: un mutante de sonda que además hace fallar un control sale «cazado_por_su_sonda_y_control». M38, M39a-c y M41a-c lo declaran en `ESPERA_CONTROL`. M42a, M42b y M42c (sonda S1-Sol signo) y M43 (sonda S1-Sol fórmula), aditivos: la frase «Comprobación: $D$» antes de «Propiedades del concepto», seguida de U+2009, U+202F o U+2060 y un punto, o de U+2060 y «kilos». Los separadores y los saltos de línea, con `intToUtf8()` | M42a-c: S1-Sol signo 100, las otras dos sondas 0, los controles en 1158 / 730 / 42 y la clase anterior en 0. M43: S1-Sol fórmula 100, y lo demás igual. Los siete mutantes de la S2a salen ahora «cazado_por_su_sonda_y_control», que es lo que declaran |
| H3 (BAJA): la guarda de V1 sigue siendo una lista negra, y la tabla candidata no lo dice | Opción (b): la tabla candidata, corregida, más abajo. La (a), ampliar la guarda de las líneas 594-595 del `.Rmd`, no se aplica ahora: si el profesor decide rediseñar por H1, va en ese mismo cambio | — |
| H4 (BAJA): `cap_s1b2.py` documenta `python3 -I`, que aquí no puede importar `websockets` | La línea de uso pasa a `python3 -E -P cap_s1b2.py …`, con su porqué en la cabecera, y un assert, antes de importar `websockets`, comprueba que el directorio de trabajo está fuera del repo | `python3 -E -P -c "import websockets"` importa websockets 15.0.1 del site de usuario (exit 0; con `-I`, exit 1). El assert corta dentro del repo y deja pasar fuera |

**Verificador** (`s2b/verificar_espacio_s2b.txt`, 129 s): **exit 3, PENDIENTE DE FIRMA (36; 0 fallos)**. Los 36 pendientes son
los 27 de H1 y los 9 del barrido. Las 37 cifras cubiertas (13 de «O-1 corregido» y 24 de la decisión 2 a) salen idénticas, y
M28 sigue cazado por su sonda con su único pendiente nuevo. Frente a la salida de la S2a, de sus 297 líneas solo cambian 9: los
siete veredictos de M38, M39 y M41, el resultado y la línea de cierre (`s2b/diff_verificador_s2a_a_s2b.txt`). Ninguna cifra
firmada se mueve. M19 mueve también el barrido: con ERR-TOTAL siempre que se pueda, el producto de una fila sube en las dos
ramas («n1 * a1 (i)»: de 28,0 a 40,0 % en CURSOS y de 32,5 a 46,8 % en PROMEDIO; «n2 * a2 (i)»: de 28,7 a 42,2 % y de 30,2 a
37,8 %). Las tablas de la firma las saca `s2b/tabla_firma_s2b.py` de esa salida, sin transcribir ninguna cifra.

**El barrido del estímulo** (regla #22 §P7, cierre por familias; §P7-G).
- **Qué mide.** Para cada operación de un paso entre dos celdas de la tabla (suma, resta, producto y cociente cuando es
  entero), dos lecturas. En la (i), la sede de la opción que cita el resultado gana, y se elige la otra opción de esa sede. En
  la (ii), la opción que cita el resultado es falsa y se descarta. Si ninguna opción lo cita, azar. Se añaden, declaradas, las
  cuatro operaciones de dos pasos que usa el ítem: las ventajas parciales de `calculos_tabla()`.
- **Por fila de la tabla** (fila 1 y fila 2), no por G y P: el estudiante no sabe qué fila gana, y rotular las celdas por G y
  por P presupone la respuesta. En los dos pares de la misma columna las dos rotulaciones dan lo mismo.
- **Ninguna regla de un paso equivale a resolver el ítem.** Los dos productos de fila juntos son T_G y T_P, y compararlos es
  resolverlo. Cada uno por separado es «un solo total», y se mide (BE2; su gemela, «n2 * a2 (i)», da +7,5 / +3,7 / +5,2 pp).
- **Resultado:** 28 operaciones × 2 lecturas × 4 estratos, y pasan de +8 pp 9 cifras, en 8 reglas. «|a1 - a2| (i)» es
  «resta» (RS1): el verificador comprueba que da las mismas cifras y la juzga una sola vez.
- **Ninguna la cubre una sonda firmada.** Todas señalan una opción que ya marca una pista firmada, pero con otra lectura:
  - BE1, en la canónica, descarta la C, que ya descarta S_U (firmada en la canónica a +25,0 pp).
  - BE2 señala la opción que cita un total, que es ERR-TOTAL. Esa opción ya la descarta «atípica» en las combinaciones
    firmadas con S_U; leerla como «esa sede gana» es nuevo.
  - BE3 a BE6, en la canónica: con las cifras del impreso, la resta de 45 y 5 y el producto de 1 por 40 dan 40, la cifra de
    la A (ERR-DATO: el promedio de G), que ya descarta S1 (firmada en la canónica a +25,0 pp).
  - BE5 y BE7, en CURSOS, y BE8, en PROMEDIO: cuando la fila es la de G, la operación da su ventaja parcial (ERR-PARCIAL), que
    ya descarta «suma» en las combinaciones firmadas.
- **Lectura de las de dos pasos (BE5, BE7 y BE8).** En la otra rama, la misma operación da la ventaja de P (la que cita
  padv), y la lectura (i) se vuelve antipredictiva: de −10,8 a −14,2 pp. El estudiante no ve la rama sin calcular, y en la
  mezcla de E1 quedan entre −5,2 y +4,0 pp. Es la situación de S3 a S6, que el verificador juzga en la mezcla ponderada. Aquí
  se juzgan, como toda regla nueva, en los cuatro estratos, y quedan pendientes: juzgarlas solo en la mezcla sería relajar la
  guarda, y eso lo decide el profesor (H-5).
- **No se midió:** las combinaciones de las reglas del barrido con S_U y las pistas firmadas, y otras operaciones de dos o más
  pasos.

**Tabla de cifras para la firma** (§P7-E, regla por regla y por estrato; aclaración iii: cifras del código). Acierto, y entre
paréntesis su exceso sobre el 25 % del azar, en pp. Peso de la canónica en la mezcla: 0,10 × su exceso (L-5). Pesos de diseño
de los estratos (W del verificador): CURSOS 0,40, PROMEDIO 0,50 y canónica 0,10. Son reglas discretas (igualdad exacta), sin
margen en el sentido de §P7-B, como las de O-1.

Reglas de H1, todas; en negrita, las cifras que pasan de +8 pp sobre el 25 % del azar:

| ID | Mezcla de E1 | Rama CURSOS | Rama PROMEDIO | Canónica (exacta, 24 órdenes) | Peso de la canónica en la mezcla |
|---|---|---|---|---|---|
| RS1 | **54,2 % (+29,2)** | **58,8 % (+33,8)** | **67,8 % (+42,8)** | 0,0 % (-25,0) | -2,50 pp |
| RS2 | **71,0 % (+46,0)** | **72,5 % (+47,5)** | **78,5 % (+53,5)** | **50,0 % (+25,0)** | +2,50 pp |
| RS3 | **79,0 % (+54,0)** | **86,0 % (+61,0)** | **79,5 % (+54,5)** | **100,0 % (+75,0)** | +7,50 pp |
| RS4 | **79,0 % (+54,0)** | **82,0 % (+57,0)** | **90,0 % (+65,0)** | **50,0 % (+25,0)** | +2,50 pp |
| RS5 | **86,0 % (+61,0)** | **93,5 % (+68,5)** | **91,0 % (+66,0)** | **100,0 % (+75,0)** | +7,50 pp |
| RS6 | **81,5 % (+56,5)** | **71,0 % (+46,0)** | **86,0 % (+61,0)** | **100,0 % (+75,0)** | +7,50 pp |
| RS7 | **93,0 % (+68,0)** | **94,5 % (+69,5)** | **98,0 % (+73,0)** | **100,0 % (+75,0)** | +7,50 pp |

- RS1: `resta: la sede de la opción que cita |aG - aP| gana; la otra de esa sede`
- RS2: `S_U + resta`
- RS3: `S_U + S1 + resta`
- RS4: `S_U + suma + resta`
- RS5: `S_U + S1 + suma + resta`
- RS6: `S_U + atípica + resta`
- RS7: `S_U + S1 + suma + atípica + resta`

Barrido del estímulo: las reglas con alguna cifra por encima de +8 pp (en negrita). «Aplica»: en qué fracción de las
versiones alguna opción cita el resultado (mezcla / CURSOS / PROMEDIO / canónica).

| ID | Mezcla de E1 | Rama CURSOS | Rama PROMEDIO | Canónica (exacta, 24 órdenes) | Peso de la canónica en la mezcla | Aplica |
|---|---|---|---|---|---|---|
| BE1 | 29,2 % (+4,2) | 28,7 % (+3,7) | 29,8 % (+4,8) | **33,3 % (+8,3)** | +0,83 pp | 51,0 / 45,0 / 57,0 / 100,0 % |
| BE2 | **34,0 % (+9,0)** | 28,0 % (+3,0) | 32,5 % (+7,5) | 25,0 % (+0,0) | +0,00 pp | 12,0 / 4,0 / 10,0 / 0,0 % |
| BE3 | 30,2 % (+5,2) | 23,2 % (-1,8) | 23,8 % (-1,2) | **100,0 % (+75,0)** | +7,50 pp | 15,0 / 7,0 / 9,0 / 100,0 % |
| BE4 | 25,6 % (+0,6) | 24,9 % (-0,1) | 25,1 % (+0,1) | **33,3 % (+8,3)** | +0,83 pp | 15,0 / 7,0 / 9,0 / 100,0 % |
| BE5 | 29,0 % (+4,0) | **34,0 % (+9,0)** | 14,2 % (-10,8) | **100,0 % (+75,0)** | +7,50 pp | 40,0 / 12,0 / 43,0 / 100,0 % |
| BE6 | 28,3 % (+3,3) | 26,0 % (+1,0) | 28,6 % (+3,6) | **33,3 % (+8,3)** | +0,83 pp | 40,0 / 12,0 / 43,0 / 100,0 % |
| BE7 | 19,8 % (-5,2) | **37,0 % (+12,0)** | 10,8 % (-14,2) | 25,0 % (+0,0) | +0,00 pp | 33,0 / 16,0 / 57,0 / 0,0 % |
| BE8 | 23,0 % (-2,0) | 11,0 % (-14,0) | **37,0 % (+12,0)** | 25,0 % (+0,0) | +0,00 pp | 24,0 / 56,0 / 16,0 / 0,0 % |

- BE1: `|a1 - a2| (ii)`
- BE2: `n1 * a1 (i)`
- BE3: `|n1 - a2| (i)`
- BE4: `|n1 - a2| (ii)`
- BE5: `dos pasos: |n1 - n2| * a1 (i)`
- BE6: `dos pasos: |n1 - n2| * a1 (ii)`
- BE7: `dos pasos: |n1 - n2| * a2 (i)`
- BE8: `dos pasos: n1 * |a1 - a2| (i)`

Cifras del barrido por encima de +8 pp: 9, en 8 reglas.

**Alcance.**
- Por invariancia, estas cifras son también las de la versión aprobada para el aula (`e293d5c8`) y las de la Parte 1 del CLOZE
  (F-2), en `cloze/`, que no se tocó ni se midió aparte. La invariancia está registrada en S1, S1b y S2a, en las semillas 1 a
  1000: las opciones (salvo el espacio duro), la clave, la rama y la tabla no han cambiado desde `e293d5c8`.
- **F-3.** Los pesos 3|1|1|1|1|1 del CLOZE se firmaron con el atajo del 77,6 %, que daba a la Parte 1 +19,7 pp de la nota de la
  pregunta sobre el azar. Por la misma cuenta, con RS7 en la mezcla de E1 (93,0 %): (93,0 − 25) × 3/8 = +25,5 pp. Supera la
  premisa de F-3, así que también vuelve al profesor.
- Es un canal nuevo por encima de +8 pp, y por «O-1 corregido» § Alcance vuelve al profesor. No lo introdujo la S2a. No es
  CORRECCIÓN: la clave sigue siendo la única afirmación verdadera.

**Opciones del profesor, sin elegir**
- **(a) Firmar regla por regla, como override estructural, igual que O-1:** las 36 cifras de las tablas de arriba (27 de H1 y 9
  del barrido), con su peso en la mezcla. La tanda que lo cablee añade la firma al verificador: dentro de lo firmado, «ROJO,
  cubierto» (exit 4); por encima, pendiente. M44 tendrá que sacar «resta» de «cubierto»: hoy la sube en las dos ramas. Por
  F-2, la firma alcanza la Parte 1 del CLOZE, y F-3 se revisa con la cifra nueva.
- **(b) Rediseñar.** Por ejemplo, que la resta de los promedios salga también del lado de P, como en la C del impreso, para que
  citarla deje de nombrar a la sede que gana. Toca el `.Rmd`: reapertura desde `generacion_rmd`, pasos 3 a 6, 6b y FASE 2C r7
  con un detractor nuevo. Antes exige la enumeración del espacio de diseño de §P7-F (en la rama PROMEDIO, P no tiene el mayor
  promedio, así que la frase de la C no le sirve tal cual) y una pasada más allá de §P7-D, que solo autoriza el profesor. La
  ampliación de la guarda de V1 (H3, opción a) iría en ese mismo cambio, y el barrido de esta tanda sirve de vara para el
  rediseño.
- **(c) Otra.**

Con cualquiera de las tres, decide también el profesor si el paso 7 se sella con la r6 o con un detractor nuevo. La S2b cambió
el verificador y el instrumento, no el `.Rmd`. El precedente, la decisión 4 de la tanda 9a del CLOZE, se tomó caso por caso.

**Tabla candidata de L-6, corregida (H3 de la r6, opción b), pendiente de la firma del profesor.** Sustituye a la candidata de la
tanda S2a, que sigue escrita más arriba. La tabla firmada de L-6 no se toca.

| Exige | Guarda | Mutante |
|---|---|---|
| La unidad que cierra una fórmula («kilos», «curso(s)») va dentro de ella (V1; tanda S1b, decisión «Arreglarlo ahora») | `stopifnot` «L-6: la unidad que cierra una fórmula va dentro de la fórmula» (líneas 594-595 de `63e2713e`; feedback y análisis de los distractores). **Solo ve el espacio normal y U+00A0** entre el cierre de la fórmula y la unidad. El resto de los separadores (U+2009, U+202F, U+2060…) lo cubre solo la sonda S1-Sol fórmula de `verificar_espacio.R`, sobre opciones, Solution y feedback emitidos: entre el cierre y la unidad admite hasta 3 caracteres que no sean letra ASCII, cifra, `$` ni salto de línea. Control exacto: 1158 unidades dentro de la fórmula en E1. La guarda tampoco ve la Solution, que se arma en su propio chunk | Guarda: M30 (el feedback de la clave cierra la fórmula antes de «kilos», con el espacio duro). Sonda: M31 (la Solution, en «kilos a favor de», con el espacio duro); M39a, M39b y M39c (la lista de totales, con U+2009, U+202F o U+2060; también hacen fallar los controles: «cazado_por_su_sonda_y_control»); y M43 (aditivo: los controles no se mueven y la clase anterior da 0). Control: M34 (quita un sitio: 1158 → 1151) |
| El signo que sigue a una fórmula, con unidad o sin ella (`. , : ; ! ? ) ] » ”` y `…`), va dentro de ella (V1′, tanda S1b relanzada, decisión «V1′ (Recomendada)»; generalizada en la tanda S2a, decisión «Cerrarlo ahora (Recomendada)») | `stopifnot` «L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula» (líneas 601-602; feedback y análisis de los distractores). Entre el cierre de la fórmula y el signo admite **hasta 3 caracteres que no sean letra ASCII, cifra, `$` ni salto de línea**: las letras con tilde y la ñ cuentan como separadores, y con 4 o más caracteres ya no se ve. La sonda S1-Sol signo usa la misma clase sobre opciones, Solution y feedback emitidos. Controles exactos: 730 unidades con el signo dentro y 42 signos dentro de una fórmula sin unidad, uno por versión de E1 con ERR-RESTA | Guarda: M33 y M37. Sonda: M32, M38, M41a, M41b y M41c (también hacen fallar un control: M38 y M41a-c salen «cazado_por_su_sonda_y_control»; M32 no pasa por `juzgar_s1()` y su comprobación no mira los controles); y M42a, M42b y M42c (aditivos: los controles no se mueven y la clase anterior da 0). Controles: M35 (730 → 721) y M36 (42 → 0) |

Queda declarado, sin guarda: la tabla del enunciado conserva el espacio normal en sus celdas de cifras (L-6 firmada). Por F-2,
las dos filas rigen también la Parte 1 del CLOZE tras el re-empalme.

**¿Ley local o regla?**
- **Local, candidatas** (endurecen; las confirma el profesor):
  - **L-7 · Barrido del estímulo.** E4 mide «resta», sus combinaciones con las pistas firmadas y el barrido de las operaciones de
    un paso entre dos celdas (más las cuatro de dos pasos), en las dos lecturas, por estrato y en la canónica exacta. Toda cifra
    por encima de +8 pp sin firma queda pendiente (exit 3). Guardas: `juzga_s2b()` y el cotejo de «|a1 - a2| (i)» con «resta».
    Mutantes: M44 y M19, también sobre el barrido.
  - La tabla candidata de L-6, corregida (arriba).
- **Del repo, candidatas.** Tocan `.claude/` (regla #17), así que las decide el profesor:
  1. §P7-G: una regla del estímulo que solo mira si una cifra de las opciones **está** en la tabla (S1) no cubre las
     **operaciones** con la tabla. La declaración del estímulo de este ítem pasó seis FASE 2C con un canal de +29 a +43 pp sin
     sonda. La familia del estímulo se cerraría con el barrido de las operaciones de un paso entre celdas, en las dos lecturas.
     Es general (todo ítem con una tabla numérica y cifras en las opciones), grave e invisible a los gates. Amplía §P7-G; no crea
     una regla nueva.
  2. Una declaración de cobertura (`estimulo_no_aplicable`, `familias_no_aplicables`) dice qué mide y qué no. La genérica («se
     mide aparte en E4») escondió el canal.
  3. `INC-MUTANTE-SONDA`: un mutante de sonda que además hace fallar un control prueba que la sonda es sensible, no que su clase
     ampliada sea necesaria. La necesidad la prueba un mutante aditivo, con los controles quietos. Es la primera vez, así que no
     cumple todavía el criterio de repetición.

**Restos fuera del repo.** La prueba del assert de `cap_s1b2.py` desde fuera del repo dejó `/tmp/x.json` y el perfil de
Chromium `/tmp/perfil_inexistente_s2b/`, sin ningún proceso vivo. El permiso no dejó borrarlos.

### Ciclo 2026-11-02 (SCHOICE), tanda S2c (§P7-F)

> La hizo el `orquestador-schoice` el 2026-11-03, por decisión del profesor tras la S2b, literal: «Enumerar §P7-F primero
> (Recomendada)». Solo análisis: el `.Rmd` sigue en `63e2713e6ac0bdbf250f4dc33901b2ec` y `verificar_espacio.R` en
> `9c65474801b5b838183a0507233b8ea1`, sin reapertura, sin sellos (6/11) y sin commit. Todo rediseño se midió con mutaciones en
> memoria del `data_generation`. Evidencia: `salida/ciclo_20261102/s2c/`.

**Instrumento.**
- `verificar_s2c.R` (md5 `669eae285fd1ea55f31177b490e1ec1e`) lo genera `construir_verificador_s2c.R` a partir de `verificar_espacio.R`:
  E1 a E4 y la decisión 2 a íntegros, sin los mutantes de E5. Aplica en memoria los 23 pares de `candidatos_s2c.R` (cada patrón,
  una sola vez) con los parámetros de cada candidato (`configs_s2c.sh`). Corridas: `salida_<ID>.txt` y `res_<ID>.rds`.
- **Controles.** B0, sin mutación, reproduce la S2b (RS1 54,2 / 58,8 / 67,8 / 0,0 %; 36 pendientes; exit 3). D0, con la mutación
  y los parámetros nulos, da B0 idéntico (`modelo_lecturas_s2c.txt`), y en las semillas 1 a 40 salen las mismas opciones, la misma
  clave y el mismo feedback que el original (`smoke_s2c.R`).
- **Parches de la copia, declarados.** E3 enumera la unión de los dos espacios (padv con parcialP y con δ). S7 y el canal 2
  toleran una opción sin cifra (solo la tiene D2). Y, fuera de la batería congelada (§P7-C), se miden la lectura (iii) de «resta»
  y del barrido («la sede de la opción que cita el resultado pierde; azar entre las dos de la otra») y el canal de formato con S_U.

**Enumeración del espacio** (exacta y sin RNG: `enumerar_espacio_s2c.R`, `enumeracion_espacio_s2c.txt`).

| Papel | ¿Puede citar δ? | Texto y restricción |
|---|---|---|
| Clave (G) | No | D ≠ δ por construcción (L-2) |
| Distractor de G | Sí: es ERR-RESTA, hoy | δ es la cifra de G que cae bajo D. Sin δ, G tiene cifra bajo D solo en el 30,4 % de las 2134 combinaciones de CURSOS y en el 3,7 % de las 982 de PROMEDIO: ERR-PARCIAL (= D + parcialP) y ERR-TOTAL (= T_G) quedan siempre encima, y los promedios solo si D > 20. En el 43,1 % (CURSOS) y el 56,5 % (PROMEDIO) de los pares (combinación, lugar de D) que usa el generador, algún reparto de lados obliga a ERR-RESTA |
| Ventaja de P (padv) | CURSOS: sí, ERR-PROM con δ, la C del impreso con el cierre común. PROMEDIO: no hay texto coherente | En PROMEDIO, P tiene el MENOR promedio en 982 de 982 combinaciones: la frase de la C no le sirve, y un ERR-CURSOS que cite δ presentaría la ventaja por curso de G como ventaja total de P. δ cae del mismo lado de D que parcialP en el 52,3 % (CURSOS) y el 41,0 % (PROMEDIO) de los pares |
| Razón sin sustento (psin) | Solo si se relaja la guarda H1 de la r1 (su cifra no sale de `calculos_tabla()`); relajar es decisión del profesor (H-5) | Factible, con psin del lado de δ y con G con cifra de su lado sin δ, en el 44,4 % (CURSOS) y el 36,5 % (PROMEDIO) de los pares |

- Toda afirmación de P que cita δ es falsa en todo el espacio (6 moldes por rama: 0 de 2134 y 0 de 982). L-2 se conserva en todos
  los candidatos: 6258 combinaciones de la unión, 0 violaciones, y 0 abortos en las 369 semillas de E1 y E2 (F1).
- El lugar de D es b + 1 con b uniforme: en promedio hay 1,5 cifras de distractor bajo D. Como δ < D en el 93,7 % (CURSOS) y el
  83,8 % (PROMEDIO) de las combinaciones, δ en P ocupa el hueco «debajo» que hoy llena G, y el canal 2 (S7) se abre.

**Modelo de las tres lecturas.** Explica las cifras, no las sustituye; coincide al 0,1 con lo medido en los 14 casos de
`modelo_lecturas_s2c.txt`. Si δ lo cita G en una fracción aG de las versiones y P en una aP:
- (i) RS1 = 25 + 75·aG − 25·aP;
- (ii) «descartar la que cita δ» = 25 + 25·(aG + aP)/3;
- (iii) «la sede que cita δ pierde» = 25 + 25·(aP − aG).

Con aP = 2·aG las tres valen 25 + 25·s/3 (s = aG + aP), su mínimo común, que no pasa de +8,3 pp. Con S_U, RS2 = 50 + 50·aG: solo
vuelve a lo firmado de S_U (50 %) con aG = 0.

**Tabla de decisión** (`tabla_decision_s2c.md`, generada desde `res_<ID>.rds` por `tabla_decision_s2c.R`; N = 100 por estrato).
Acierto en %, CURSOS / PROMEDIO.

| Candidato | Qué cambia | RS1 | (iii) | RS2 | RS7 | O-1 corregido: cifras por encima de lo firmado (de 13) | S7, canal 2 (pp sobre el 50 %) | Otros | Fallos / pendientes |
|---|---|---|---|---|---|---|---|---|---|
| B0 (hoy) | — | 58,8 / 67,8 | 13,8 / 10,8 | 72,5 / 78,5 | 94,5 / 98,0 | 0 | −7,0 / −4,0 | — | 0 / 36 |
| D1c40 | CURSOS: padv cita δ con p = 0,4; PROMEDIO, igual | 31,2 / 67,8 | 30,2 / 10,8 | 61,5 / 78,5 | 95,5 / 98,0 | 3, todas de CURSOS | +6,0 / −4,0 | Canal de largo en CURSOS +14,3 pp (margen 2,0 %), por encima de lo declarado (+10,4) | 0 / 41 |
| D1q50 | δ en P con p = 0,5 (padv en CURSOS, psin en PROMEDIO) | 23,8 / 60,8 | 34,8 / 14,8 | 58,5 / 75,5 | 94,5 / 97,0 | 6 | +9,0 / −1,0 | En PROMEDIO, δ en P solo en el 10 % | 2 / 45 |
| D1q100 | Igual, con p = 1 | 0,0 / 49,2 | 50,0 / 21,8 | 50,0 / 71,0 | 88,0 / 97,0 | 7 | +19,0 / +8,0 | — | 3 / 49 |
| D3 | CURSOS: ERR-RESTA pasa a P (padv, siempre) | 0,0 / 67,8 | 50,0 / 10,8 | 50,0 / 78,5 | 88,0 / 98,0 | 3 | +19,0 / −4,0 | — | 2 / 42 |
| D3b | D3, y en PROMEDIO sin δ | 0,0 / 25,0 | 50,0 / 25,0 | 50,0 / 50,0 | 88,0 / 99,0 | 8 | +19,0 / +19,0 | — | 5 / 48 |
| D4 | ERR-RESTA fuera de G | 25,0 / 25,0 | 25,0 / 25,0 | 50,0 / 50,0 | 98,5 / 99,0 | 10 | +9,0 / +19,0 | — | 5 / 47 |
| D2 | ERR-RESTA sin cifra («en lo mismo que se diferencian los dos promedios») | 25,0 / 25,0 | 25,0 / 25,0 | 50,0 / 50,0 | 73,0 / 70,0 | 1 | +14,5 / +21,5 | S_U + «descartar la que no cita cifra»: 72,5 / 78,5; con las tres pistas y «resta», 92,5 / 98,0. Lugar de D roto (4.º puesto: 8 % y 0 %). Batería §P7: +14,3 / +8,7 / +12,5 pp (mezcla / CURSOS / PROMEDIO) | 9 / 34 |

- En todos, la canónica no cambia: las 24 cifras de la decisión 2 a salen iguales, y RS1 sigue en 0,0 % exacto.
- La respuesta correcta sigue variando en todos: de 43 a 58 valores distintos de D en cada estrato de 100 versiones (hoy, de 46
  a 50).
- Barrido (i)/(ii), sin «resta»: hoy 9 cifras por encima de +8 pp; en los candidatos, de 9 a 15.

**Viables y no viables.** Ninguno es viable. El canal es estructural, y por §P7-F toca override, no rediseño.
- **D4, D3b y D3** sacan δ de G en una rama o en las dos y reabren el canal 2: ERR-RESTA era la cifra de G bajo D. Suben de 3 a 10
  cifras firmadas, porque el distractor de G queda repartido entre ERR-DATO, ERR-PARCIAL y ERR-TOTAL, que ya delatan S1, suma y
  atípica.
- **D1q50 y D1q100** suben de 6 a 7 cifras firmadas y fallan en el canal 2. En PROMEDIO dejan RS1 en el 60,8 % y el 49,2 %: no hay
  vehículo coherente.
- **D2** desplaza el canal tal cual: «descartar la que no cita cifra» con S_U da 72,5 / 78,5 %, las mismas cifras de RS2 hoy. Además
  rompe el lugar de D y la batería, y tiene 9 fallos.
- **D1c40** es el único sin fallos, y deja «resta» en CURSOS dentro de +8 pp en sus tres lecturas (31,2 / 30,6 / 30,2 %). Pero no toca
  PROMEDIO (RS1 67,8 %), sube 3 cifras firmadas de CURSOS y el canal de largo por encima de lo declarado, y RS7 no baja (95,5 %).
  No evita la firma: cambia qué se firma.
- Lo estructural no es la resta: con S_U firmado, quien sabe qué sede ganó solo tiene que separar la clave del distractor de G, y
  todos los tipos de error del pool lo delatan con una operación de un paso o con una pista superficial. RS7 queda entre el 88,0 y
  el 99,0 % en todo candidato que conserva la cifra en las cuatro opciones.

**Qué costaría D1c40, si el profesor lo quisiera de todos modos.**
- Reapertura desde `generacion_rmd`, pasos 3 a 6, 6b y FASE 2C r7 con un detractor nuevo, sin `name:`.
- En el mismo cambio, la guarda de L-6 de la línea 595, ampliada (H3 (a) de la r6).
- Unas 21 líneas de `data_generation` (`tamano_cambio_s2c.txt`) y un texto propio para la explicación de ERR-PROM con δ, que en
  la medición reutiliza la de la C.
- En `verificar_espacio.R`:
  - E3, con la unión de los espacios;
  - recalibrar los controles exactos de E1 ligados a ERR-RESTA (1158, 730 y 42) y los mutantes M36, M37 y M44;
  - dos mutantes nuevos: uno que devuelva δ a G y otro que lo ponga en G y en P a la vez (canal 8).
- La firma de las 3 cifras de CURSOS, del canal de largo y de «resta» en PROMEDIO.
- Re-empalme y medida de la Parte 1 del CLOZE (F-2). La premisa de F-3 sigue rota, porque RS7 no baja.

**Hallazgo nuevo: la lectura (iii).**
- En el diseño actual (B0) da 5 cifras por encima de +8 pp que la S2b no medía:
  - en la canónica, «|a1 − a2| (iii)», 50,0 % (+25,0 pp);
  - en CURSOS, «n1 · |a1 − a2|» +13,5 pp y «n2 · |a1 − a2|» +10,5 pp;
  - en PROMEDIO, «|n1 − n2| · a1» +10,7 pp y «|n1 − n2| · a2» +14,3 pp.
- Por construcción, cuando aplica, la cifra es la de padv (o, en la canónica, la de la C), así que señala como perdedora a la misma
  sede que S_U.
- Se declaran fuera de la batería congelada (§P7-C). Las decide el profesor: entran en la firma o en la batería del próximo ciclo.

**No se midió.**
- Un reparto de lados que priorice la cifra de G bajo D cuando P cita δ, para cerrar S7: exige D > 20 (30,4 % y 3,7 % de las
  combinaciones).
- psin con δ en CURSOS, otros valores de p y un ERR-CURSOS con δ en PROMEDIO (descartado por incoherente).
- Un tipo de error nuevo de G con cifra bajo D. La diferencia de los productos cruzados (nG·aP − nP·aG), por ejemplo, queda siempre
  encima de D en CURSOS y es negativa en PROMEDIO.
- «S_U + (iii)», las combinaciones del barrido con S_U y las operaciones de dos o más pasos fuera de las cuatro del ítem.
- Nada se renderizó (ningún `.Rmd` cambió), ni se midió la Parte 1 del CLOZE. Tampoco se corrió la suite de tests: no cambió
  código del repo.

**¿Ley local o regla?**
- **Local, candidatas** (endurecen; las confirma el profesor):
  - **L-8 · ERR-RESTA llena el hueco bajo D.** El canal 2 y «resta» están acoplados. Todo cambio del pool de G, o de quién cita δ,
    mide en la misma pasada S7, el lugar de D y las 13 cifras de «O-1 corregido». Guarda: el conteo exacto de
    `enumerar_espacio_s2c.R` en E3. Mutante: D4, con el que S7 tiene que pasar de +8 pp.
  - **L-7, ampliada:** el barrido también con la lectura (iii).
- **Del repo, candidatas.** Tocan `.claude/` (regla #17), así que las decide el profesor:
  1. §P7-G: el barrido del estímulo se mide en las tres lecturas, y la (iii) es el canal inverso de la (i) (Incidente F, punto 4).
     Sin ella, la S2b dejó sin ver 5 cifras por encima de +8 pp.
  2. Texto íntegro de §P7-F:
     - En un ítem 2 + 2, una pista que marca una sola opción no baja de 25 + 25·s/3 en sus tres lecturas.
     - Con un atajo firmado que da la sede ganadora, toda pista sobre el distractor de G suma 50·aG.
     - Antes de mover una cifra de lado, se enumera qué hueco de magnitud llena (lugar de D, canal 2).

     Es la forma enumerable del ciclo de desplazamiento de canal, y D2 lo muestra en estado puro.

### Ciclo 2026-11-02 (SCHOICE), tanda S2d

> La hizo el `orquestador-schoice` el 2026-11-03, tras la S2c. El `.Rmd` no cambia (md5 `63e2713e6ac0bdbf250f4dc33901b2ec`):
> sin reapertura y sin commit. Cambian el verificador y el registro, y se sellan los pasos 7 a 10, uno por uno. Se detiene en el
> WAIT_USER #3, sin sellar `aprobacion_usuario`. Evidencia: `salida/ciclo_20261102/s2d/`.

#### Decisiones del profesor (2026-11-03), literales

> Transmitidas por el coordinador en el encargo de la tanda S2d. Regla #24 **H-5**: queda por escrito quién decidió, cuándo y
> con qué cifras.

1. **H2 de la FASE 2C r5.** Respuesta literal: «Cerrarlo ahora (Recomendada)». Aplicada en la tanda S2a (md5 `63e2713e`).
2. **H1 de la FASE 2C r6.** Respuesta literal: «Enumerar §P7-F primero (Recomendada)». Hecha en la tanda S2c: ningún rediseño
   es viable, y el canal es estructural.
3. **Tras la S2c.** Respuesta literal: **«Firmar las 41 (Recomendada)»**. Override estructural, regla por regla y por estrato
   (§P7-E), como O-1:
   - las **36 cifras de la S2b** (RS1 a RS7 por estrato, con la canónica exacta, y BE1 a BE8): las de
     `salida/ciclo_20261102/s2b/tabla_firma_s2b.md`;
   - las **5 de la lectura (iii)** que midió la S2c sobre el diseño actual (B0, `salida/ciclo_20261102/s2c/salida_B0.txt`,
     líneas 311 a 322):
     - «resta» (iii) en la canónica: 50,0 % (+25,0 pp);
     - «dos pasos: n1 * |a1 - a2| (iii)» en CURSOS: 38,5 % (+13,5);
     - «dos pasos: n2 * |a1 - a2| (iii)» en CURSOS: 35,5 % (+10,5);
     - «dos pasos: |n1 - n2| * a1 (iii)» en PROMEDIO: 35,8 % (+10,7);
     - «dos pasos: |n1 - n2| * a2 (iii)» en PROMEDIO: 39,2 % (+14,3).
   - El paso 7 se sella con la r5 y la r6, porque el `.Rmd` no cambia. Los cambios del verificador posteriores a la r6 los
     revisará el próximo detractor.

**Identificadores del cableado.** RS1 a RS7 y BE1 a BE8 son los de `tabla_firma_s2b.md`. RS8 es «resta (iii)» en la canónica, y
BE9 a BE12 son las cuatro de dos pasos (iii), en el orden de la lista de arriba.

**Qué cubre.** Cada cifra cubre solo su regla en su estrato, hasta su cifra a un decimal. Quedan pendientes, y vuelven al
profesor: una cifra por encima de la firmada, la misma regla en otro estrato por encima de +8 pp y cualquier regla nueva por
encima de +8 pp. Un tope por estrato o por familia no cuenta como alcance firmado. No cubre ningún defecto de CORRECCIÓN. Qué
significa para la Parte 1 del CLOZE (F-2) y para los pesos de F-3 se decide en el ciclo del CLOZE.

#### Cableado de la firma y de la lectura (iii) (tanda S2d)

- **`verificar_espacio.R`: `9c654748…` → `031d4726f652df60a6d0dd49c8d8524b`.** Lo editó `s2d/editar_verificador_s2d.py`: 17 cambios,
  cada patrón exigido una sola vez, con la copia previa en `s2d/verificar_espacio_9c654748_antes.R.txt`. Ni el verificador ni el
  `.Rmd` llevan U+00A0, U+2009, U+202F ni U+2060 literales.
- **Lectura (iii) en E4.** `lee_iii()` es la de la S2c, sin cambios: la sede de la opción que cita el resultado pierde, y se elige
  al azar entre las dos opciones de la otra sede; su máximo es 50 %. Se mide en «resta» (`R_RESTA_III`) y en las 28 operaciones del
  barrido, en los cuatro estratos. «|a1 - a2| (iii)» se coteja con «resta (iii)» y se juzga una sola vez. Reproduce la S2c: «resta
  (iii)» da 16,8 / 13,8 / 10,8 / 50,0 % (mezcla / CURSOS / PROMEDIO / canónica), igual que `s2c/salida_B0.txt`, y por encima de
  +8 pp quedan exactamente las 5 cifras firmadas, con las mismas cifras a un decimal.
- **`FIRMA_S2D`**: 41 filas (identificador, regla, estrato y acierto a un decimal), impresas al correr el verificador.
  `veredicto_s2d()` es la única puerta, para el juicio y para los mutantes. Da «ok» con +8 pp o menos; «cubierto» si la fila de
  esa regla y ese estrato existe y la cifra no pasa de lo firmado; y «pendiente» en cualquier otro caso. Un cotejo final comprueba
  que las 41 filas existen en E4 y que las 41 valen lo firmado a un decimal: «41 iguales a lo firmado».
- **Mutantes**, los dos por la misma puerta:
  - M44 (ERR-RESTA siempre que se pueda): «resta» sube de 58,8 a 72,2 % en CURSOS y de 67,8 a 70,0 % en PROMEDIO, y queda
    «pendiente» en las dos ramas, fuera de RS1. Veredicto: `cazado_por_su_sonda`.
  - M45, nuevo (la fila de G siempre abajo, `gi <- 2L`): BE9 pasa de 38,5 a 50,0 % y BE11 de 35,8 a 50,0 %, las dos «pendientes»;
    BE10 y BE12 bajan a 25,0 %, y no hay renders con error. Veredicto: `cazado_por_su_sonda`.
  - M19 sigue moviendo el barrido, y M28 sigue cazado, con su único pendiente.
- **Resultado:** exit 4, «EN ROJO BAJO OVERRIDE FIRMADO (78 canales: 13 dentro del alcance de O-1 corregido, 24 de la decisión 2 a
  y 41 de la firma de la S2d; 0 fallos, 0 pendientes)», en 134 y 135 s. Las dos corridas son idénticas byte a byte (528 líneas,
  md5 `f3d6801a35011116f7c1b5126852d8a2`; `s2d/cmp_run1_run2.txt`). Frente a la S2b solo desaparecen los 36 pendientes, sus dos
  resúmenes y el resultado, y se añaden las 41 cubiertas, las dos tablas y los cotejos (`s2d/diff_verificador_s2b_a_s2d.txt`).
  Ninguna de las 37 cifras anteriores se mueve.
- **Lo que mostró M45.** La lectura (iii) de las de dos pasos depende del sorteo de la fila de G (P4). Con la fila fija, la cifra
  de padv sale siempre en la misma operación, y la regla llega al máximo de (iii), el 50 %. Con el sorteo, se reparte entre dos
  operaciones, y cada una queda entre el 35,5 y el 39,2 %.

#### Sellos de la tanda S2d, uno por uno

| Paso | Sello | Artefacto |
|---|---|---|
| 7 `detractor_fase2c`, `APROBAR_CON_CAMBIOS` | 10:50:28 | `salida/fase2c_r5_detractor.md` y `salida/fase2c_r6_detractor.md`. Citas al sellar: hoy fallan 9 de 14 y 6 de 17, todas en código corregido después, y ninguna de la r6 apunta al `.Rmd` (`s2d/citas_r5_al_sellar.txt`, `s2d/citas_r6_al_sellar.txt`) |
| 8 `coherencias_5` | 10:51:20 | `s2d/paso8_coherencias_63e2713e.md` (`AgenteValidadorVisual` nuevo, sin `name:`: COHERENCIAS_OK, 0 CRÍTICA, ALTA y MEDIA, 3 BAJA) y `s2d/paso8/` |
| 9 `validar_diversidad`, 271 únicas | 10:51:57 | `s2d/paso9/`: diversidad sustantiva PASS (95 valores de D), diagnosticidad PASS, multisemilla 100/100 y 271/300 únicas; más las dos corridas del verificador |
| 10 `validar_icfes` | 10:52:27 | `s2d/paso10/paso10_icfes.txt`: `validar_literalidad_exextra.py`, exit 0 y 0 fallos |

`aprobacion_usuario` queda sin sellar: WAIT_USER #3, con el paquete en el `HANDOFF.md`, § «tanda S2d».

#### ¿Ley local o regla?
- **Local, candidatas** (endurecen; las firma el profesor; texto exacto en el `HANDOFF.md`): la tabla de L-6 corregida en la S2b;
  L-7, ampliada con la lectura (iii) y ya cableada; y L-8, que todavía no tiene guarda en `verificar_espacio.R`.
- **Del repo:** ninguna nueva. Las candidatas de la S2b y de la S2c siguen pendientes de la decisión del profesor.

### Aprobación del SCHOICE y leyes firmadas (2026-11-03, WAIT_USER #3 de la tanda S2d)

> **Firmado por el profesor el 2026-11-03** y transmitido por el coordinador (sesión `42032564`). Respuestas literales:
> 1 «[a] Aprobar (Recomendada)»; 2 «L-6, dos filas (Recomendada)», «L-7 barrido (Recomendada)» y «L-8 hueco bajo D»;
> 3 «Mantener y cablear (Recomendada)». Regla #24 **H-5**.

**Aprobado para el aula:** el `.Rmd` con md5 `63e2713e6ac0bdbf250f4dc33901b2ec`, que sustituye a la aprobación de `e293d5c8`.
Paso 11 sellado a las 12:45:40, así que el SCHOICE queda en 11/11. Siguen la evidencia de aula (Nivel 3) y, después, la promoción.

**L-6 · cláusulas firmadas (amplían la tabla firmada de L-6; su texto es el de la candidata corregida en la S2b):**

| Exige | Guarda | Mutante |
|---|---|---|
| La unidad que cierra una fórmula («kilos», «curso(s)») va dentro de ella: `$… = T\ \text{kilos}$` | `stopifnot` «L-6: la unidad que cierra una fórmula va dentro de la fórmula» (líneas 594-595). Solo ve el espacio normal y U+00A0; los demás separadores los cubre solo la sonda S1-Sol fórmula del verificador | M30 (guarda); M31, M39a-c y M43 (sonda); M34 (control, 1158 → 1151) |
| El signo que sigue a una fórmula, con unidad o sin ella (`. , : ; ! ? ) ] » ”` y `…`), va dentro de ella: `$… = T\ \text{kilos.}$` y `$… = δ\text{.}$` | `stopifnot` «L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula» (líneas 601-602), que admite hasta 3 caracteres que no sean letra ASCII, cifra, `$` ni salto de línea; más la sonda S1-Sol signo | M33 y M37 (guarda); M32, M38, M41a-c y M42a-c (sonda); M35 y M36 (controles, 730 → 721 y 42 → 0) |

**L-7 · Barrido del estímulo (firmada; ya cableada en la S2d).**
- Exige: E4 mide «resta», sus combinaciones con las pistas firmadas y el barrido de las operaciones de un paso entre dos celdas,
  más las cuatro de dos pasos. Lo hace en las lecturas (i), (ii) y (iii), por estrato y en la canónica exacta. Toda cifra por
  encima de +8 pp fuera de `FIRMA_S2D` (su regla, su estrato y hasta su cifra) queda pendiente (exit 3).
- Guardas:
  - `juzga_s2b()` con `veredicto_s2d()`;
  - los cotejos «|a1 - a2| (i)» = «resta» y «|a1 - a2| (iii)» = «resta (iii)»;
  - el cotejo de las 41 filas, que tienen que existir y valer lo firmado.
- Mutantes: M44 (RS1 sale de «cubierto»), M45 (BE9 y BE11 salen de «cubierto») y M19.

**L-8 · ERR-RESTA llena el hueco por debajo de D (firmada; PENDIENTE DE CABLEAR).**
- Exige: el canal 2 y «resta» están acoplados. Todo cambio del pool de G, o de quién cita δ, mide en la misma pasada S7, el lugar
  de D y las 13 cifras de «O-1 corregido».
- Guarda: el conteo exacto de `salida/ciclo_20261102/s2c/enumerar_espacio_s2c.R`, llevado a E3.
- Mutante: D4, con el que S7 tiene que pasar de +8 pp.
- Hoy vive solo en el instrumento de la S2c. **Mientras no esté cableada en `verificar_espacio.R`, no es una invariante**
  ([[feedback_invariante_sin_guardia]]). La cablea el próximo ciclo del SCHOICE, o la tanda del CLOZE si toca este verificador.

**ERR-TOTAL (residuo de «O-1 corregido»): la firma sigue vigente.**
- Las cifras oficiales no cambiaron desde la ronda 3: 9 % en CURSOS y 17 % en PROMEDIO con N = 100 por rama, y una exposición
  exacta por configuraciones de 8536/39116 = 21,8 % y 3928/19640 = 20,0 %. Salen idénticas en las rondas 3, 4 y 4b, en la S1 y en
  la S2d.
- El 21,5 % del paso 8 (20 de 93) es otra muestra.
- Pendiente: cablear la cuota de ERR-TOTAL por rama en el verificador en el próximo ciclo, para que una subida real se vea.

**Para el CLOZE (ciclo siguiente):** la Parte 1 hereda por el re-empalme las opciones, la tabla y los canales.
- Decide el profesor:
  - si la firma de las 41 y las L-6, L-7 y L-8 se extienden a la Parte 1, como F-2 extendió «O-1 corregido»;
  - si se revisa F-3: con RS7 al 93,0 % en la mezcla, la Parte 1 pasa de +19,7 a +25,5 pp sobre el azar.
- `cloze/empalmar_bloque_p1.R` está atado al md5 `e293d5c8` y a las líneas 2-561. Hay que actualizarlo a `63e2713e` y volver a
  poner el marcador en el lugar del bloque viejo.

### Ciclo 2026-11-02 del CLOZE, tanda C1 (2026-11-03): re-empalme de la Parte 1 y H3; pasos 3 a 6 sellados

> La hizo el `orquestador-cloze` el 2026-11-03 (sesión `42032564`), por encargo del coordinador. Reapertura #5 desde
> `generacion_rmd` (13:18:39). `.Rmd`: `f98f9dda…` → **`27f789dc6327f7d6bda72cded61502cd`**, sin aprobar.
> `cloze/verificar_espacio_cloze.R`: `6901624e…` → **`6e76321a22e2026adaa3a4b5f576f070`**. Se detuvo antes de la FASE 2C
> r4, que lanza el coordinador con un detractor nuevo. Evidencia: `cloze/salida/ciclo_20261102/`. Estado: 6/11.

#### Decisiones del profesor que rigen la tanda (literales; regla #24 H-5)

1. **SCHOICE, 2026-11-02, decisión (a):** U+00A0 se normaliza solo al medir. Rige también la Parte 1 del CLOZE tras el
   re-empalme (F-2).
2. **2026-11-02, tras la 9a:** H3 de la FASE 2C r3 del CLOZE, diferido a este ciclo («la Solution de la Parte 6 dice
   "solo si … fuera mayor"; la relación es "si y solo si"»).
3. **2026-11-03, sobre la Parte 1:** «Extender por alcance (Recomendada)». Las 41 cifras de la firma de la S2d del
   SCHOICE (`FIRMA_S2D`) y las leyes L-6, L-7 y L-8 rigen la Parte 1 por identidad, como F-2 extendió «O-1 corregido».
   El verificador del CLOZE las comprueba sobre su Parte 1. Queda en rojo si alguna se mueve o si aparece otra regla por
   encima de +8 pp. L-8 todavía no está cableada en el verificador del SCHOICE.
4. **2026-11-03, sobre F-3:** «Mantener 3|1|1|1|1|1 (Recomendada)». Los pesos no cambian. F-3 se vuelve a firmar con la
   cifra nueva: con RS7 al 93,0 % en la mezcla, la Parte 1 da (93,0 − 25) × 3/8 = +25,5 pp sobre el azar (antes,
   +19,7). Queda registrado como declarado.

#### Qué se hizo

- **Re-empalme**, con `cloze/empalmar_bloque_p1.R` actualizado (md5 del SCHOICE `63e2713e`, `fin == 613`). Sin el
  marcador, el script lee de git el SCHOICE viejo (`8efdf504`, md5 `e293d5c8` comprobado), exige que las líneas 2-561 del
  CLOZE sean su bloque con los 4 renombres, línea a línea, y solo entonces las cambia por el marcador. Es idempotente.
  - Bloque nuevo: líneas 2-612 de `63e2713e` (611 líneas), cada renombre una vez.
  - El analizador no encontró otros usos de `opciones`, `sol` ni `feedback`: las guardas de S1 y L-6 no los usan, así que
    no hizo falta ningún renombre más.
  - Salida: `salida/ciclo_20261102/empalme_c1.txt`. Copia previa del script: `empalmar_bloque_p1_e293d5c8.R.txt`.
- **Fuera del bloque** (`salida/ciclo_20261102/editar_rmd_c1.py`: 13 patrones y 14 sitios, cada uno exigido un número exacto
  de veces):
  - los comentarios que nombraban `e293d5c8`;
  - los 2 U+00A0 literales del código propio, a su escape (mismo valor);
  - L-6 en la prosa propia de la Parte 1 de la Solution, con las 7 cadenas de la Solution del SCHOICE `63e2713e`
    («Procedimiento correcto» y «Caso específico»: unidad y signo dentro de la fórmula, y espacio duro);
  - y H3. Diff: `diff_rmd_f98f9dda_27f789dc.patch`; fuera del bloque, 34 líneas de diferencia.
- **H3** (Parte 6): «… habría ganado con un curso más si y solo si su promedio por curso, … kilos, es mayor que la
  diferencia, … kilos; y el grado … habría perdido con un curso menos si y solo si su promedio, … kilos, es mayor que esa
  diferencia». En indicativo: no presupone que la condición sea falsa.

#### Invariancia (`salida/ciclo_20261102/invariancia_c1.R` → `.txt`, semillas 1 a 1000)

- `.Random.seed` al final del `data_generation`, igual al de `f98f9dda` en 1000/1000, y los objetos propios del CLOZE,
  iguales. Del bloque viejo faltan solo los que quitó S7: `contexto_tipo` y, en la canónica, `b_debajo` y `lado_gdis`.
- Parte 1 = SCHOICE `63e2713e` (objetos y `.Random.seed` al final del bloque): 1000/1000, con 113 canónicas.
- Veredictos y opciones de la Parte 6, iguales a `f98f9dda`: 1000/1000. La Solution, igual en 1000/1000 tras deshacer en el
  texto el espacio duro, la unidad y el signo dentro de la fórmula, y H3.
- Prosa de la Parte 1 de la Solution: 0 unidades fuera de la fórmula, 0 signos fuera y 0 cifra-unidad o El/La con espacio
  normal (control `f98f9dda`: 8575 y 15 347). Quedan 8575 unidades dentro, 5298 con el signo.
- H3: la marca de HP y HG coincide con el bicondicional leído del texto en 1000/1000 (HP verdadera en 476, HG en 518; sin
  empates, por construcción de `espacio_paralelo()`).
- 0 U+00A0, U+2009, U+202F o U+2060 literales en el `.Rmd`. Regla #19: 0. Regla #25: OK. Ortografía: limpio.

#### Verificador (`6e76321a…`; `salida/ciclo_20261102/verificar_espacio_cloze_c1.txt`, 485 s)

**Exit 4: EN ROJO BAJO OVERRIDE FIRMADO, 368 cifras dentro de lo firmado (las 327 de la r9 y las 41 de la S2d), 0 fallos y
0 pendientes.**
- Frente a la r9 solo cambian 6 líneas: los dos md5, la línea de MP2 (el patrón lleva el espacio duro; la cifra es la misma)
  y el recuento del resultado (`comparar_r9_c1.txt`). No se movió ninguna cifra firmada del CLOZE: L-C2 a L-C7, X5, X6, X10,
  «O-1 corregido» y la decisión 2 a.
- La primera corrida dio FALLA (1), «E2 p1: 283/330» (`verificar_espacio_cloze_c1a_falla_e2p1.txt`). El oráculo de la Parte 1
  comparaba el nombre de la tabla, sin U+00A0, con el sujeto de la opción («El Bosque» con U+00A0). Era del oráculo, no del
  `.Rmd`.

| Cambio | Guarda | Mutante → sonda |
|---|---|---|
| `rec()` no lee `lado_gdis`: S7 lo quitó de la canónica y nadie consume el campo en el CLOZE | `ok()`: `rec()` corre en una canónica, sin `lado_gdis`, y en una no canónica | MC40: lo vuelve a leer; en la canónica, «objeto 'lado_gdis' no encontrado» |
| E4 mide la batería, los márgenes y la canónica de la Parte 1 con U+00A0 como espacio (decisión 1) | `norm_ed()` | MC43: sin normalizar, el margen en palabras de la rama PROMEDIO pasa de 6,5 % a 6,7 % y el canal de largo queda pendiente |
| MP2 con el espacio duro | `mut_p1()`: el patrón, en una sola opción | MP2: «más dígitos» +75,0 pp, cazado por su sonda |
| El oráculo de la Parte 1 (E2) lee el sujeto como se ve | `or_p1(x, nf = norm_ed)`: 330/330 | MC49: sin normalizar, 283/330; fallan exactamente las 47 versiones con nombres de dos palabras |
| Decisión 3: la región S2b-S2d de `SUB/verificar_espacio.R` (`031d4726…`, líneas 471-653, por marcadores únicos) se evalúa sobre las muestras del CLOZE | `veredicto_s2d()` y el cotejo: 41 de 41 iguales a lo firmado; una que se mueva queda pendiente | MC41 (M44 sobre el bloque P1): RS1 58,8 → 72,2 % y 67,8 → 70,0 %, pendiente. MC42 (M45): BE9 38,5 → 50,0 % y BE11 35,8 → 50,0 %, pendientes. Reproducen el SCHOICE |
| L-6 en la prosa de la Parte 1 de la Solution | sondas a 0 y controles exactos: 600 unidades dentro y 300 con el signo, en 100 versiones | MC45 (unidad), MC46 (signo) y MC47 (espacio normal), aditivos; MC48 (control: 600 → 500) |
| Sin U+00A0, U+2009, U+202F ni U+2060 literales en el `.Rmd` ni en el verificador | `lit_en()` | MC44 |

- L-7 rige la Parte 1 con la región S2b-S2d (el barrido en las tres lecturas). **L-8 no se cableó:** sigue pendiente en
  `SUB/verificar_espacio.R`, y sin guarda no es una invariante.
- F-3: +25,5 pp sobre el azar, declarado (decisión 4). E7 (pesos 3|1|1|1|1|1) sigue en verde.

#### Sellos, uno por uno

- `generacion_rmd`, 13:29:50; `retroalimentacion`, 13:30:15.
- `renderizado_4_formatos`, 13:32:17: `salida/ciclo_20261102/render_27f789dc/` (PDF 5, HTML 5, DOCX 1, Moodle n = 5; NOPS
  N/A).
- `arsenal_post_render`, 13:32:20: informe del hook `20261103-133049-847130`, 0 errores y 6 advertencias; 2G 100/100; 2H 0
  anomalías.
- El paso 3 se selló con la invariancia, antes del verificador. La FALLA de su primera corrida era del oráculo y no tocó el
  `.Rmd`.

#### 6b pareada (`auditor-visual-html`, sin `name:`): APTO_CON_OBSERVACIONES

N = 100 semillas por versión (`27f789dc` frente a `f98f9dda`), a 360 y 320 px: 400 páginas por CDP, con `innerWidth`
comprobado y la red bloqueada (con red, el MathJax de la CDN sustituía a veces el MathML y las métricas no se repetían).
Informe íntegro, recuperado de su transcripción: `salida/ciclo_20261102/6b_c1/informe_6b_c1_27f789dc_completo.md`.

| Parte 1, 360 / 320 px | `f98f9dda` | `27f789dc` |
|---|---|---|
| fórmula + unidad partidas (de 1158) | 160 / 274 | 0 / 0 |
| cifra-unidad partidas (de 2003) · El/La (de 475) | 317 / 335 · 79 / 98 | 0 / 0 · 0 / 0 |
| signo huérfano tras fórmula · renglón que empieza con espacio | 0 · 0 | 0 · 0 |

- Las Partes 2 a 6 salen idénticas en las dos versiones. El BAJA 1 de la r3 (fórmula y unidad partidas) da 259/1249 a
  360 px y 206/1249 a 320 px; «sirven» sola a 360 px, 100/100; «cuadrado.» sola a 320 px, 32 de 400 opciones; palabras
  sueltas en la Parte 2, 107/400 y 59/400.
- Desbordes, tablas fuera de su caja y fórmulas partidas por dentro: 0.
- **BAJO nuevo (H3):** en la Parte 6 de la Solution, «diferencia.» queda sola en el último renglón a 360 px en 99/100 (0/100
  en `f98f9dda`, y 0/100 a 320 px). Es cosmético.
- BAJO: la unidad dentro de la fórmula sale con la letra de la fórmula (el mismo BAJO de la S1b del SCHOICE). BAJO: +26 px
  de altura media a 360 px.

#### Qué debe auditar la FASE 2C r4 (detractor nuevo, sin `name:`)

- El re-empalme: `empalmar_bloque_p1.R`, el bloque (E1 en 1000 semillas) y las 34 líneas de diferencia fuera del bloque.
- `rec()` sin `lado_gdis`, MP2, la normalización de E4 y del oráculo E2 p1 (MC43 y MC49), y la extensión de la firma: que
  la región evaluada sea la del SCHOICE y que las 41 cifras se juzguen con su puerta (MC41 y MC42).
- Las sondas de L-6 en la prosa de la Parte 1, con sus controles exactos (MC45 a MC48), y la guarda de literales (MC44).
- H3: la redacción, la verdad del bicondicional en todo el espacio (con una función de verdad propia), que no cambie ningún
  veredicto, y el BAJO nuevo de la 6b.
- **MC39**, pendiente de un detractor nuevo desde la r3.
- F-3, declarado a +25,5 pp, y si L-8 sin cablear deja algo sin guarda en la Parte 1.

#### ¿Ley local o regla?

- **Local, candidata (L-C9; endurece y la firma el profesor):** el re-empalme de la Parte 1 se hace solo por script, desde el
  md5 aprobado del SCHOICE y comprobando antes el bloque viejo línea a línea. Toda medida u oráculo del CLOZE que lea texto de
  la Parte 1 lo normaliza como se ve (U+00A0 como espacio), y las guardas de L-6 lo leen crudo. Guardas: los `stopifnot` de
  `empalmar_bloque_p1.R`, `norm_ed()` y `or_p1()`. Mutantes: MC43 y MC49.
- **Del repo, nada nuevo.** La FALLA de E2 p1 es otro caso del pendiente 11 de `.claude/docs/HANDOFF_REPO.md` (las
  herramientas que comparan texto no ven el espacio duro), aunque esta vez falló a la vista. Además, la herramienta Write
  convirtió en literales los escapes Unicode de dos scripts de esta tanda (U+00A0, pero también U+00BB, U+201D, U+2026,
  U+2009, U+202F y U+2060). Se arreglaron con un paso de `chr()` y con marcadores. Es la candidata ya anotada, ampliada a
  Write.

**Verificación del coordinador de la tanda C1 del CLOZE (2026-11-03, sesión `42032564`)**, independiente de sus scripts
(`cloze/salida/ciclo_20261102/coordinador/`):
- `verificar_espacio_cloze.R` (`6e76321a…`), repetido: exit 4 en 476 s, con 1409 líneas idénticas a
  `cloze/salida/ciclo_20261102/verificar_espacio_cloze_c1.txt`. Hay 368 cifras dentro de lo firmado, 0 fallos y 0 pendientes.
- Invariancia con un script propio (`invariancia_cloze_coordinador.R`), semillas 1 a 1000:
  - (A) E1: la Parte 1 del CLOZE nuevo (`27f789dc`) es idéntica al SCHOICE aprobado (`63e2713e`) en las 1000. Se compararon
    opciones, clave, feedback, rama, canónica, tabla y respuesta correcta.
  - (B) El `.Random.seed` final coincide con el del CLOZE aprobado (`f98f9dda`) en las 1000, y los 18 objetos de las Partes 2 a 6
    también.
- Los sellos 3 a 6 llevan el md5 `27f789dc` y cada uno va después de su artefacto (la invariancia a las 13:23 y el render a las
  13:30). El SCHOICE no se tocó (`63e2713e`, verificador `031d4726`, 11/11).
- El diff fuera del bloque lo revisé y contiene los comentarios, dos U+00A0 literales pasados a escape, L-6 en la prosa de la
  Parte 1 y H3 en indicativo.

### FASE 2C r4 del CLOZE (2026-11-03): APROBAR_CON_CAMBIOS, sin CORRECCIÓN

> `AgenteDetractor` nuevo, sin `name:`, sobre el md5 `27f789dc…`. Fue el segundo intento. El primero, lanzado a las 14:00, murió
> sin entregar en el corte eléctrico de las 14:19: el Paso 0 de la regla #9 no encontró marcador en su transcripción, y lo que
> dejó a medias no se usó. El segundo, lanzado a las 14:32, entregó a las 14:53. El reporte íntegro, sacado de su transcripción,
> está en `cloze/salida/fase2c_r4_detractor.md`. Citas: 15 de 15 (`verificar_citas_detractor.py`, exit 0; el hook registró
> «ENTREGA VERIFICADA»). Su evidencia estaba en el scratchpad de la sesión (tmpfs) y se copió a `cloze/salida/fase2c_r4/`.

| ID | Severidad · tipo | Qué | Estado |
|---|---|---|---|
| H1 | BAJA · precisión de la Solution (regla #13) | Parte 6: «si y solo si su promedio por curso, *a*′_P kilos, es mayor que la diferencia, *D*′ kilos…». La comparación va en indicativo y con las cifras de la versión. El bicondicional es verdadero, pero la comparación, leída suelta, es falsa en 70 de 111 versiones según el detractor. Según el coordinador, en 62 de 100 (`salida/ciclo_20261102/coordinador/h1_r4_coordinador.*`: HP falsa en 43 y HG en 51, sin empates). Es el reflejo del H3 de la r3. Se suma el BAJO de la 6b de la C1: «diferencia.» queda sola a 360 px en 99 de 100 | Decisión del profesor, abajo: se reescribe ahora |
| H2 | BAJA · cobertura del verificador | El mutante de texto de MC39 añade 3 n-gramas exclusivos de BP_gano y deja otros 3 exclusivos de BC_gano. Ninguno está en la lista de L-C7, así que muere también por la cláusula «fuera de la lista». El detractor lo dedujo de los conjuntos de textos, sin correr `h2_exacta()` con el mutante. Además, el veredicto dice `cazado_por_su_sonda` en cuanto `j39` tiene filas, y no hay línea base | Endurecer, autónomo (H-5): veredicto `cazado_por_su_sonda_y_otra`, con el conteo de pendientes fuera de `j39`, y línea base `h2_exacta(COD_C, 1L)`, con las filas de `j39` en «cubierto» |
| H3 | BAJA · procedimiento (L-C8) | `coherencias_r9/verificar_coherencias.R` compara contra literales del texto anterior (líneas 360, 398, 403 y 414) y escribe un `.rds` en el directorio de trabajo (línea 610). No sirve para `27f789dc`: fallaría a la vista, así que no daría un falso verde | El paso 8 se cierra con un oráculo r10 nuevo, con los seis puntos del reporte |

Comprobado por el detractor, sin hallazgo:
- el re-empalme: idéntico byte a byte salvo los 4 renombres, y el script comprueba el md5 y el bloque viejo;
- el verificador: exit 4, con 1409 líneas idénticas a c1;
- la CORRECCIÓN en las 111 versiones de E1, con funciones de verdad propias sobre el texto emitido: 0 fallos; de 2 826
  igualdades aritméticas, ninguna falsa; la canónica, igual al impreso en las 20;
- MC39: prueba la cláusula, con el matiz de H2;
- F-3: la cuenta de +25,5 pp es correcta;
- L-8: está declarada sin cablear, y su guarda real es indirecta.

### Decisión del profesor tras la FASE 2C r4 del CLOZE (2026-11-03)

> **Firmada por el profesor el 2026-11-03** y transmitida por el coordinador. Respuesta literal: «Reescribir ahora
> (Recomendada)». Regla #24 **H-5**: queda por escrito quién decidió, cuándo y con qué cifras.

1. **H1 se reescribe en este ciclo.**
   - En el párrafo «Parte 6.» de la Solution, cada comparación con cifras de la versión va resuelta: dice «más» o «menos»
     según los datos y la conclusión que sigue. Por ejemplo: «sumaría 27 kilos, menos que la diferencia de 33 kilos: no le
     alcanzaría para ganar». El criterio general puede quedarse, pero sin cifras.
   - Toca el `.Rmd`, así que lleva reapertura desde `generacion_rmd` (tanda C1b), pasos 3 a 6, 6b y una FASE 2C r5 con un
     detractor nuevo. Después viene la tanda C2.
   - La recomendación del coordinador se apoyó en dos razones: la regla #13 v1.1 exige reescribir una frase cuando otra lectura
     razonable cambia su verdad, y es la segunda ronda que marca este párrafo (H3 de la r3 y H1 de la r4).
2. **H2 y H3 se aplican en cualquier caso, porque endurecen:** H2 en el verificador, en la C1b; H3 en el paso 8, en la C2.

### Ciclo 2026-11-02 del CLOZE, tanda C1b (2026-11-03): H1 y H2 de la FASE 2C r4; pasos 3 a 6 sellados

> La hizo el `orquestador-cloze` el 2026-11-03, por encargo del coordinador. Reapertura #6 desde `generacion_rmd` (16:27:09),
> con el reporte `cloze/salida/fase2c_r4_detractor.md`. `.Rmd`: `27f789dc…` → **`6f6f1b1311b0d681bd974c719ecd75d5`**, sin aprobar.
> `cloze/verificar_espacio_cloze.R`: `6e76321a…` → **`6d37faad687a5fc11e69b763429b6624`**. Se detuvo antes de la FASE 2C r5,
> que lanza el coordinador con un detractor nuevo. Evidencia: `cloze/salida/ciclo_20261102/c1b/`. Estado: 6/11.

#### Decisión que rige la tanda (literal; regla #24 H-5)

- **2026-11-03, tras la FASE 2C r4:** «Reescribir ahora (Recomendada)». H1 se reescribe en este ciclo. H2 se aplica en el
  verificador porque endurece (autónomo).

#### La redacción nueva del párrafo «Parte 6.» de la Solution (H1)

- Queda el criterio general, sin cifras: «Un curso más con el mismo promedio suma ese promedio al total, y un curso menos lo
  resta. Por eso basta comparar ese promedio con la diferencia entre los totales: si es mayor, el ganador cambia; si es menor,
  no.»
- Siguen dos frases con las cifras de la versión, cada una resuelta con los datos («más» o «menos») y con la conclusión que se
  sigue. Ejemplos (semillas 1, 5, 10 y 11):
  - HP verdadera: «Con un curso más, el grado sexto habría sumado a su total 53 kilos, más que la diferencia de 37 kilos:
    habría ganado.»
  - HP falsa: «Con un curso más, el grado séptimo habría sumado a su total 27 kilos, menos que la diferencia de 33 kilos: no le
    habría alcanzado para ganar.» (la semilla 10 que citó el coordinador)
  - HG verdadera: «Con un curso menos, el grado sexto habría restado a su total 42 kilos, más que la diferencia de 33 kilos:
    habría perdido.»
  - HG falsa: «Con un curso menos, el grado séptimo habría restado a su total 24 kilos, menos que la diferencia de 27 kilos:
    habría seguido ganando.»
- **Por qué es verdadera.** *T*′_P + *a*′_P > *T*′_G ⇔ *a*′_P > *D*′, y *T*′_G − *a*′_G < *T*′_P ⇔ *a*′_G > *D*′; los empates
  los excluyen `espacio_paralelo()` y la guarda «Parte 6: hipótesis sin empate». La palabra («más» o «menos») sale de
  `hp_supera <- dp$aP > dp$D` y `hg_supera <- dp$aG > dp$D`, y la conclusión, de la misma comparación. Un `stopifnot` nuevo del
  `data_generation` («H1 r4: la comparación de la Solution de la Parte 6 coincide con los veredictos sorteados de HP y HG») las
  ata a `v_hp` y `v_hg`, que dan la marca.
- **Cómo se comprobó la propuesta del detractor.** Su aritmética era correcta. Se cambió: «restaría» lleva ahora su
  complemento («a su total»), y la cifra va justo antes de la comparación («habría sumado a su total 27 kilos, menos que…»),
  para que «menos que la diferencia» no pueda leerse como referida al total. Los tiempos van todos en condicional compuesto,
  como la opción de la Parte 6 («… habría ganado el concurso»): por eso «no le habría alcanzado para ganar» y no «no le
  alcanzaría», que era el ejemplo del registro de la decisión. Lo audita la r5.
- L-6: espacio duro (`ED`) entre cada cifra y «kilos». También entre las dos últimas palabras («habría perdido», «seguido
  ganando»), para que el párrafo no termine con una palabra sola en su último renglón (el BAJO de la 6b de la C1). Lo mide la
  6b de esta tanda. Reglas #19, #25 y ortografía: limpias. Ningún U+00A0, U+2009, U+202F ni U+2060 literal.

#### Diff (`c1b/diff_rmd_27f789dc_c1b.patch`, script `c1b/editar_rmd_c1b.py`: 2 patrones, cada uno exigido una vez)

- `data_generation`: `hp_supera`, `hg_supera` y su `stopifnot` (6 líneas, con 3 de comentario), después de la guarda de la
  retroalimentación de la Parte 6. Sin sorteos.
- `solucion`: el `cat()` del párrafo «Parte 6.», con 3 líneas de comentario. Nada más cambia.

#### Invariancia (`c1b/invariancia_c1b.R` → `.txt`, semillas 1 a 1000, 113 canónicas, 60 s)

- `.Random.seed` al final del `data_generation`, igual al de `27f789dc` en 1000/1000.
- E1: la Parte 1 = SCHOICE `63e2713e` (objetos y `.Random.seed` al final del bloque P1): 0 diferencias.
- Todos los objetos de `27f789dc`, idénticos; los únicos nuevos son `hp_supera` y `hg_supera`, iguales a `v_hp` y `v_hg`.
- Fuera de los chunks `data_generation` y `solucion`, el `.Rmd` es idéntico byte a byte: con lo anterior, el enunciado, las
  opciones, las marcas y el feedback de las seis partes salen iguales.
- La Solution emitida es idéntica salvo el párrafo «Parte 6.» en 1000/1000.
- En el párrafo nuevo, leído del texto: 2000 comparaciones con cifras, 0 falsas, 0 conclusiones que no se siguen y 0 contra la
  marca de HP o HG (HP verdadera en 476, HG en 518). Control: en `27f789dc`, 656 de 1000 versiones tienen alguna comparación
  falsa leída suelta.

#### Verificador (`6d37faad…`; `c1b/verificar_espacio_cloze_c1b.txt`, 472 s)

**Exit 4: EN ROJO BAJO OVERRIDE FIRMADO, 368 cifras dentro de lo firmado, 0 fallos y 0 pendientes.** Frente a la C1, ninguna
línea cambia fuera de las 33 que añade la C1b (`c1b/comparar_c1_c1b.py` → `.txt`): no se movió ninguna cifra firmada.

| Cambio | Guarda o sonda | Mutante → resultado |
|---|---|---|
| H1: la comparación sale de los datos | `stopifnot` «H1 r4: …» del `data_generation` | MC52 (`hp_supera` con el promedio del grado que ganó): cazado por su sonda en la semilla 5 |
| H1: cada comparación con cifras del párrafo «Parte 6.» es verdadera, leída del TEXTO de la Solution emitida (las cifras y el «más» o «menos», con la tabla del otro colegio leída como se muestra) | sonda H1-Sol (100 versiones): falsa 0, cifra 0, concl 0, marca 0, esp 0; control exacto: 200 comparaciones, 100 «un curso más» y 100 «un curso menos» (ramas: 57/43 y 49/51) | MC50 (invierte «más» en «menos» en la rama verdadera de HP): falsa +57, concl +57, `cazado_por_su_sonda`. MC51 (vuelve a la redacción de `27f789dc`): falsa +43, marca +43 y el control cae a 100, `cazado_por_su_sonda_y_control`, declarado así. MC53 (espacio normal ante «kilos»): esp +100. MC54 (cita el promedio del que ganó): cifra +100, falsa +30. Los cuatro, aditivos salvo MC51 |
| H2: MC39 con línea base y conteo | `h2_exacta(COD_C, 1L)` sin mutar: las dos filas de `j39` salen +12,5 pp, «cubierto» | MC39 de texto: CURSOS y PROMEDIO, «perdió hubiera tenido», +12,5 → +25,0 pp, pendiente con la firma regla por regla y cubierta con la de estrato; además abre 33 pendientes nuevos fuera de `j39`, entre ellos los 6 n-gramas que dedujo el detractor (y «perdió hubiera» y «que perdió hubiera»): `cazado_por_su_sonda_y_otra`. Los 100 pendientes que la línea base ya tiene con una sola Parte 1 no cuentan |

- La sonda directa de MC39 (sin texto) no cambia.
- Con una sola Parte 1, la medida no es la firmada (100 pendientes en la línea base sin mutar). Por eso la línea base es la
  prueba: sin ella, «sube de +12,5» comparaba con la cifra firmada sobre 100 Partes 1.

#### Sellos, uno por uno

- `generacion_rmd` 16:41:59 (después del verificador, 16:41:03); `retroalimentacion` 16:42:07; `renderizado_4_formatos`
  16:42:13 (`c1b/render_6f6f1b13/`, 16:36: PDF 5, HTML 5, DOCX 1, Moodle n = 5; NOPS N/A por `cloze exercises`);
  `arsenal_post_render` 16:42:23 (informe del hook `20261103-163617-186493`: 0 errores y las 6 advertencias heurísticas de la
  C1; 2G 100/100; 2H 0 anomalías). Cada nota lleva el md5 `6f6f1b13…`.

#### Qué debe auditar la FASE 2C r5 (detractor nuevo, sin `name:`)

- La redacción nueva: su verdad en todo el espacio, con una función de verdad propia (las dos ramas del otro colegio y las tres
  combinaciones de veredictos de cada una); sus otras lecturas (regla #13 v1.1: «menos que la diferencia», «el ganador
  cambia», «habría restado a su total»); la concordancia y los tiempos (el cambio a «no le habría alcanzado»); y el espacio duro
  de las dos últimas palabras.
- La sonda H1-Sol: que lea del texto, que su patrón vea también la redacción vieja, su control exacto, y MC50 a MC54 (cada uno
  por su sonda; MC51 también por el control, como se declara), y la guarda de MC52.
- H2: la línea base de MC39, que los 33 pendientes nuevos los abra el mutante (los 100 de la línea base se excluyen) y que el
  veredicto lo diga.
- La invariancia (1000 semillas) y la 6b de esta tanda.
- Siguen de la C1: L-8 sin cablear en el verificador del SCHOICE y F-3 declarado a +25,5 pp. H3 de la r4 (el oráculo r10 del
  paso 8) va en la C2.

#### ¿Ley local o regla?

- **Local, candidata (L-C10; endurece y la firma el profesor):** la Solution no imprime una comparación con cifras de la versión
  que, leída suelta, pueda ser falsa. Cada una va resuelta con los datos, con su conclusión, y el criterio general va sin cifras.
  Guardas: el `stopifnot` «H1 r4» (MC52) y la sonda H1-Sol del verificador (MC50, MC51, MC53 y MC54). Es la segunda ronda que
  marca este párrafo (H3 de la r3 y H1 de la r4).
- **Del repo, nada nuevo por H1:** la regla #13 v1.1 ya lo cubre («toda lectura razonable cuenta»; las afirmaciones de la
  Solution sobre cifras sorteadas se garantizan con su `stopifnot`). No es grave (BAJA).
- **Del repo, candidata (toca `.claude/`, regla #17; lo decide el profesor):** un mutante que se mide sobre una muestra distinta
  de la firmada (aquí, una Parte 1 en lugar de 100) necesita su línea base en esa misma muestra. Sin ella, «sube de su cifra»
  compara con otra medida, y un mutante podía darse por cazado aunque la regla ya estuviera pendiente sin mutar. Sería una
  obligación más del contrato de mutación de los orquestadores. Es general, ningún gate lo ve y lo encontró un detractor.

#### 6b pareada de la tanda C1b (`auditor-visual-html`, sin `name:`): APTO_CON_OBSERVACIONES

N = 100 semillas por versión (`6f6f1b13` frente a `27f789dc`), a 360 y 320 px: 400 páginas por CDP, con `innerWidth`
comprobado y la red bloqueada. Instrumento `cap7.py` (el de la C1 más una sonda del párrafo, por `Range.getClientRects()`).
Informe: `cloze/salida/ciclo_20261102/c1b/6b_c1b/informe_6b_c1b_6f6f1b13.md`, con `metricas.json` y 8 recortes leídos a
resolución completa (6 del NUEVO, con las cuatro conclusiones, y 2 de la REFERENCIA).

| Párrafo «Parte 6.» | NUEVO 360 | REF 360 | NUEVO 320 | REF 320 |
|---|---|---|---|---|
| Último renglón con una sola palabra | **0/100** | 99/100 (control positivo: «diferencia.») | 0/100 | 0/100 |
| Renglones (media) | 13,5 | 11,0 | 15,6 | 12,0 |
| Cortes entre cifra y «kilos» · desborde · palabras partidas | 0/400 · 0 · 0 | 0/300 · 0 · 0 | 0/400 · 0 · 0 | 0/300 · 0 · 0 |

- **El BAJO de la 6b de la C1 queda cerrado:** «diferencia.» sola a 360 px pasa de 99/100 a 0/100.
- La Parte 1 sale idéntica en las dos versiones, con 0 cortes en todas las métricas de L-6 (fórmula con la unidad dentro 0/1158;
  cifra-unidad 0/2003; El/La 0/475). Las Partes 2 a 6 conservan el residuo heredado (fórmula + unidad partidas 263/1249 a 360 px
  y 209/1249 a 320 px, igual en las dos; la C1 medía 259 y 206 con otra copia de los HTML).
- Desbordes, tablas fuera de su caja, fórmulas partidas por dentro y fugas de markup: 0. Altura media de página: +45 px a 360 px
  (+0,56 %) y +65 px a 320 px (+0,70 %).
- **BAJO (residuo declarado):** a 320 px, en la frase de «un curso más», el renglón puede partir la conclusión entre «habría» y
  su participio («habría | ganado.»; en 3 de los 6 recortes del NUEVO). El espacio duro solo ata las dos últimas palabras del
  párrafo. Es un corte normal de un tiempo compuesto, no deja palabras solas y no se midió su frecuencia en las 100 semillas.

**Verificación del coordinador de la tanda C1b del CLOZE (2026-11-03, sesión `358ab328`)**, independiente de sus scripts
(`cloze/salida/ciclo_20261102/coordinador/`):
- `verificar_espacio_cloze.R` (`6d37faad…`), repetido: exit 4 en 470 s. Sus 1433 líneas son idénticas a las de
  `cloze/salida/ciclo_20261102/c1b/verificar_espacio_cloze_c1b.txt`, que solo tiene además su línea de cierre. Hay 368 cifras
  dentro de lo firmado, 0 fallos y 0 pendientes.
- Invariancia con un script propio (`invariancia_cloze_coordinador_c1b.R`), del CLOZE de la C1 (`27f789dc`, sacado de git en
  `45b1b34c`) al de la C1b (`6f6f1b13`):
  - (A) Semillas 1 a 1000. E1 (la Parte 1, igual al SCHOICE `63e2713e`), el `.Random.seed` final y los 180 objetos comunes,
    idénticos en las 1000. Los únicos objetos nuevos son `hp_supera` y `hg_supera`, y coinciden con `v_hp` y `v_hg` en las 1000.
    Son verdaderos en 476 y 518 semillas.
  - (B) Texto emitido (`xexams`), semillas 1 a 100. El enunciado, la `questionlist`, la `solutionlist`, la metainformación y la
    Solution fuera del párrafo «Parte 6.», idénticos en las 100. En el párrafo nuevo, las cifras y los grados son los del párrafo
    viejo de la misma semilla en 100 de 100. Las 200 comparaciones son verdaderas leídas sueltas, y las 200 conclusiones se siguen
    de ellas. Salen las cuatro combinaciones de ramas.
- En `27f789dc`, antes de la C1b, 62 de 100 versiones tenían alguna comparación falsa leída suelta (`h1_r4_coordinador.txt`).
- Los sellos 3 a 6 llevan el md5 `6f6f1b13`, y cada uno va después de su artefacto. El diff del `.Rmd` toca solo los dos sitios
  declarados (21 líneas, sin sorteos nuevos) y no lleva literales U+00A0, U+2009, U+202F ni U+2060.

### FASE 2C r5 del CLOZE (2026-11-03): APROBAR_CON_CAMBIOS, sin CORRECCIÓN

> `AgenteDetractor` nuevo, sin `name:`, sobre el md5 `6f6f1b13…` de la tanda C1b, todavía sin commit. Se lanzó a las 16:59 y
> entregó a las 17:18. El reporte íntegro, sacado de su transcripción, está en `cloze/salida/fase2c_r5_detractor.md`. Citas: 12
> de 12 (exit 0; el hook registró «ENTREGA VERIFICADA»). La evidencia está en `~/.cache/fase2c_r5_cloze/`, y sus scripts y
> salidas de texto, en `cloze/salida/fase2c_r5/`.

| ID | Severidad · tipo | Qué | Estado |
|---|---|---|---|
| H1 | BAJA · precisión de la Solution (regla #13 v1.1) | El criterio general nuevo («Por eso basta comparar ese promedio con la diferencia entre los totales: si es mayor, el ganador cambia; si es menor, no») admite dos lecturas que lo vuelven falso. (a) «si es mayor» puede tomar como sujeto «la diferencia», que es el antecedente más cercano, y así el criterio se invierte. (b) El alcance no dice que vale para un curso más o uno menos. Aplicado a la afirmación de ventaja por cursos (102 de 205 versiones), da el veredicto equivocado en 80. Con la lectura buscada es verdadero en 205 de 205 | Al profesor. Alternativa del detractor: «Por eso, con un curso más o uno menos, basta comparar ese promedio con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no.» Toca el `.Rmd` |
| H2 | BAJA · visual: el residuo está mal declarado | El corte de la conclusión en dos renglones no es ocasional. Medido por el detractor con N = 100: «habría ⏎ ganado.» sale en 57 de 57 a 320 px; «habría ⏎ seguido ganando.», en 43 de 51 a 320 px y en 29 de 51 a 360 px. Las páginas con alguno de los dos son 84 de 100 a 320 px y 29 de 100 a 360 px. No se viola ninguna ley | Hay que corregir la declaración del residuo. Lo del espacio duro tras «habría» lo decide el profesor |
| H3 | BAJA · cobertura del verificador | `mut_h1()` (línea 1541) da `cazado_por_su_sonda` aunque fallen también otras componentes, así que MC50, MC51 y MC54 no siguen la convención `_y_otra`. La componente «marca» (línea 748) lee `en$sol_p6` y no el `exsolution` emitido, aunque el comentario de la línea 712 diga lo contrario. El detractor comprobó aparte que las conclusiones coinciden con el `exsolution` emitido en 205 de 205 | Endurecer, autónomo (H-5). El coordinador lo comprobó en el código (líneas 1541-1542 y 748) |

Comprobado por el detractor, sin hallazgo:
- la verdad del párrafo nuevo en 205 versiones: 410 comparaciones y 410 conclusiones, con el contrafactual calculado desde la
  tabla que se muestra;
- los tiempos: «no le habría alcanzado» es correcto, y el «no le alcanzaría» de la decisión iba como ejemplo;
- L-6 y las reglas #19 y #25;
- la guarda «H1 r4» (MC52) y el control exacto de H1-Sol;
- MC39, con su línea base;
- la invariancia, en 100 semillas;
- la CORRECCIÓN de las seis partes en 205 versiones: de 5 251 igualdades, ninguna falsa; la canónica, 20 de 20.

Además dejó la lista de lo que tiene que comprobar el oráculo r10 del paso 8.

### Decisión del profesor tras la FASE 2C r5 del CLOZE (2026-11-03)

> **Firmada por el profesor el 2026-11-03** y transmitida por el coordinador. Respuestas literales: H1 «Corregir ahora
> (Recomendada)»; H2 «Solo declararlo (Recomendada)»; commit «Sí, ahora (Recomendada)». Regla #24 **H-5**: queda por escrito quién
> decidió, cuándo y con qué cifras.

1. **H1 se corrige ahora, con una sola frase.** El criterio general pasa a decir: «Por eso, con un curso más o uno menos, basta
   comparar ese promedio con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no.» Toca
   el `.Rmd`, así que lleva una tanda C1c (reapertura desde `generacion_rmd`, pasos 3 a 6 y 6b) y una FASE 2C r6 con un detractor
   nuevo.
   - **Regla de parada, aceptada con esta opción:** si la r6 solo trae BAJAs de redacción de este párrafo, quedan como residuo
     declarado. Es la tercera redacción que se marca en el párrafo (H3 de la r3, H1 de la r4 y H1 de la r5). Una CORRECCIÓN se
     corrige siempre.
2. **H2 queda como residuo, con su frecuencia real.** El `.Rmd` no cambia por esto, y no se pone espacio duro tras «habría».
   - Cifras del detractor (N = 100, sobre `6f6f1b13`): «habría ⏎ ganado.», 57 de 57 a 320 px y 0 de 57 a 360 px; «habría ⏎ seguido
     ganando.», 43 de 51 a 320 px y 29 de 51 a 360 px; páginas con alguno de los dos, 84 de 100 a 320 px y 29 de 100 a 360 px.
     Además, «no le ⏎ habría alcanzado», 43 de 43 a 320 px, y «habría ⏎ sumado», 100 de 100 a 320 px y 41 de 100 a 360 px.
   - Esto corrige la declaración de la tanda C1b, que decía «puede partir» y «no se midió su frecuencia». La 6b de la C1c mide las
     mismas cifras sobre el md5 final.
3. **H3 (verificador) lo aplica la tanda C1c,** porque endurece (H-5).
4. **Commit y push de la C1b,** con la evidencia de la r4 y la r5, antes de la C1c.
