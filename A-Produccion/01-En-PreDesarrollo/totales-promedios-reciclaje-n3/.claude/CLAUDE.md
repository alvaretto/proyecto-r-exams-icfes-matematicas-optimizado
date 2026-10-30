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
Hasta lo firmado da OK; por encima, PENDIENTE DE FIRMA (exit 3). El mutante M19 fuerza ERR-TOTAL y debe
sacar alguna cifra de lo firmado. Antes de la ronda 4 el verificador comparaba con el 50 % de la premisa
tachada y salía siempre con exit 3, así que no distinguía «dentro del alcance» de «lo superó».
