# HANDOFF — `totales-promedios-reciclaje-n3` (MAT-2026-07-050)

> Estado al 2026-10-30 (la caja de la pregunta en el Moodle del colegio, medida ese día). Antes de tocar nada, leer [`ITEM_OFICIAL.md`](ITEM_OFICIAL.md) y las invariantes
> [`.claude/CLAUDE.md`](.claude/CLAUDE.md), que prevalecen sobre este archivo. Versionado desde `17dfc787`
> (versión aprobada en la ronda 3, md5 `cd94b59a…`). **Vigente: md5 `e293d5c812b1f9790e2bb777b62de9a5`
> (ronda 4b), 11 de 11, aprobada para el aula el 2026-10-29:** ver § «Rondas 4 y 4b».
> **Auditoría del 2026-11-02 (solo lectura):** los pasos 3 a 6 y 7 a 10 se sellaron en lote, a milisegundos entre sí, pero
> cada sello tiene un artefacto anterior a él sobre este md5; no hace falta reabrir. Detalle y tabla en
> [`.claude/CLAUDE.md`](.claude/CLAUDE.md) § «Aprobación del CLOZE y auditoría del sellado del SCHOICE». El CLOZE hermano
> (`cloze/`) quedó aprobado para el aula ese mismo día.

## Qué se hizo

- El subproyecto llegó como `01-En-PreDesarrollo/50/` y se renombró el 2026-10-29 según
  `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md` § Estructura de Carpetas (kebab-case corto y descriptivo):
  `50/` → `totales-promedios-reciclaje-n3/`. La imagen pasó de `originales/50.png` a
  `originales/orig_pregunta_50.png` (mismo md5), con la convención `orig_pregunta_<N>.png` de los
  subproyectos recientes. Nada en el repo referenciaba la ruta anterior.
- `ITEM_OFICIAL.md`: transcripción verbatim, cotejada contra el recorte y la plana; clasificación cotejada
  contra el catálogo canónico (los cuatro campos coinciden).
- `.claude/CLAUDE.md`: invariantes L-1 a L-5.
- Dry-run de `orquestador-schoice` (2026-10-29): ningún pre-flight falla (25); 14 de 60 turnos; no escribió
  ni renderizó nada.

## Decisiones del profesor (2026-10-29), vinculantes

| Decisión | Valor | Dónde rige |
|---|---|---|
| Flujo B (WAIT_USER #1) | `n`: tabla Markdown con `tabla_responsiva()` y la guarda `\newcounter{none}` | paso 2 de `ejercicio_state.json` |
| Conflicto L-3 frente a §P4-bis | Alternar la rama ganadora en las versiones no canónicas | `.claude/CLAUDE.md` § L-3 |
| Probabilidad de la canónica | 0,10 | `.claude/CLAUDE.md` § L-5 |
| H3 de la FASE 2C: atajo de la razón sin sustento (+16,7 pp) | Override firmado, con alcance hasta +16,7 pp | `.claude/CLAUDE.md` § O-1 |
| «recogido»/«recolectado» en las versiones no canónicas | Normalizar: un solo verbo por plantilla; la canónica conserva la rareza | `.claude/CLAUDE.md` § L-1 |

## Hallazgos del dry-run para el paso 3 (recomendaciones de diseño, no invariantes)

Notación de `.claude/CLAUDE.md`. Rama CURSOS: *k* = *n*_G − *n*_P, *δ* = *a*_P − *a*_G y
*D* = *k* · *a*_G − *n*_P · *δ*. Rama PROMEDIO: *k* = *n*_P − *n*_G, *δ* = *a*_G − *a*_P y
*D* = *n*_G · *δ* − *k* · *a*_P.

### Canales que hay que medir y cerrar

| # | Canal | En la canónica | Defensa propuesta | Medición en el paso 9 |
|---|---|---|---|---|
| 1 | Perfil del ganador entre versiones | siempre la rama CURSOS | cerrado por la decisión de L-3 (alternar) | frecuencia global de cada rama, N = 100 |
| 2 | Magnitud entre las dos opciones de G | la clave dice 20 y A dice 40 | rama CURSOS: *T*_G y *k* · *a*_G superan siempre a *D*, y *a*_G también cuando *k* = 1 (como en el impreso), así que «elige la menor de las dos» acierta siempre; sortear *k* ≥ 1 y poner en el pool de G cifras falsas por debajo de *D* (*δ*; *a*_G cuando *k* ≥ 2). Rama PROMEDIO: rehacer el análisis con su identidad | rango de la cifra de la clave, N = 100 por rama |
| 3 | La cifra aparece en la tabla | 40 y 5 están en la tabla; 20 no | cifras falsas también calculadas (*δ*, *T*_G, promedios cruzados) | sonda propia: la batería §P7 solo ve las opciones |
| 4 | Opción sin cifra | D es la única | variantes con cifra, u otra opción sin cifra en parte de las versiones | batería §P7, familia formato: si no, +8,3 pp sobre el techo nulo |
| 5 | Léxico | «total», «menor», «fue» e «y» solo aparecen en la clave | distractores que digan «el total fue de X» con una X falsa (Error 32) | sonda léxica propia, por rama |
| 6 | Longitud | A 88, B 126 (clave), C 105 y D 156 caracteres | — | H1 y canales inversos con margen, por rama |
| 7 | Fila de G en la tabla | G está en la fila 1 | sortear la fila de G (P4) | frecuencia por rama |
| 8 | Cifra compartida | — | guarda *D* ≠ *δ* (5 × 40 = 200 contra 4 × 48 = 192 da *D* = *δ* = 8) | regla relacional (§P7-E) |

El dry-run recomienda congelar la batería al inicio (§P7-C) con las reglas 2, 4, 5, 7 y 8. Las sondas 1 y
3 van en el verificador propio del subproyecto, porque necesitan ver la tabla del enunciado.

### Requisitos de diseño

- **Pool por ranuras** (Incidente N); con una sola entrada por ranura, el tipo de error no variaría:
  - cifra falsa del lado de G, con 3 tipos o más (dato aislado *a*_G, resta de promedios *δ*, *T*_G
    tomado como diferencia y uno propio);
  - razón de P que sí está en la tabla, con 2 variantes o más por rama (mayor promedio en la rama CURSOS,
    más cursos en la rama PROMEDIO);
  - razón de P sin sustento, con 2 o 3 variantes, alguna con cifra.
- **Excepción canónica:** fuerza las opciones A, C y D oficiales en esa instancia.
- **Mutante de L-2:** revierte la construcción, no solo el `stopifnot`. Si la unicidad se construye por
  diseño, quitar la guarda no hace aparecer una segunda verdadera y el mutante sobrevive (guarda
  inalcanzable). Por ejemplo, permitir *D* = *a*_G con *k* = 2 (5 × 36 contra 3 × 48).
- **Compromiso que hay que medir:** con *k* = 1, *D* ≠ *a*_G se cumple solo pero queda abierto el canal 2;
  con *k* ≥ 2 el canal 2 se puede cerrar, y la guarda de L-2 pasa a ser necesaria.
- Exponer `opciones` y `sol` al final de `data_generation`; si no, sale `WARN_DIAG_INDET`, que no es PASS.
- Solution: identifica la clave por contenido (regla #19), no enumera las opciones en orden (Incidente Q) y
  escribe los productos como `$5 \times 40 = 200$`.

### Tabla y PDF

- `tabla_responsiva()`: la canónica mide 74 de 85 caracteres. Hay que medir **todas** las plantillas de
  contexto (son finitas), porque una con encabezados más largos aborta el render del PDF.
- El pandoc del PATH es 3.10.2 y el de RStudio, 3.10: la guarda `\newcounter{none}` es obligatoria en los
  dos entornos (regla #20).
- Con tabla Markdown se conserva todo el contenido; solo se pierde el color naranja de los encabezados
  (decisión de Flujo B = `n`).

### Notas para quien mantenga `.claude/agents/orquestador-schoice.md`

**Resueltas el 2026-10-29** (v3.31.1 del índice; detalle en `.claude/docs/CHANGELOG_CLAUDE_MD.md`):

- El punto 3 del dry-run de los dos orquestadores dice ahora que el filtro 3b bloquea (exit 2).
- El Incidente O (R en el CLOZE) y su «Validación realista» remiten al detector de la regla #25, en vez de
  una lista propia que marcaba `×`.

Las vigilan bloques nuevos de `test_glifos_latex.R` y `test_nomenclatura_rmd.R`. Rigen desde el commit:
los orquestadores de este día cargaron el texto viejo.

## Modo `ejecutar` del orquestador (2026-10-29)

Archivos nuevos: `totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd`, `verificar_espacio.R`
(verificador propio: `Rscript --no-init-file verificar_espacio.R`, exit 0 = aprobado) y `salida/`.

Diseño (decisiones de implementación, no invariantes; las invariantes siguen en `.claude/CLAUDE.md`):

- Las cuatro afirmaciones cierran igual, «… su total fue de *x* kilos más que el de *O*», así que todas citan
  una cifra y nombran las dos sedes (canales 4 y 5). Como en el impreso, de las dos de G una da el perfil de la
  sede (más cursos y menor promedio, o al revés) y la otra no; cuál de las dos es la clave se sortea.
- La ventaja de P cita su ventaja parcial (*n*_P · *δ* en la rama CURSOS, *k* · *a*_P en la rama PROMEDIO);
  la razón sin sustento cita una cifra inventada.
- El lugar de *D* entre las cuatro cifras se sortea (canal de magnitud, «cifra mediana»): el espacio de
  parámetros se filtra según ese lugar, sin remuestrear (F1).
- Si la cifra de P ya es un número de la tabla (*k* = 1 en la rama PROMEDIO), el distractor de G no cita otro
  dato aislado (canal 3).

Medido con `verificar_espacio.R` (salida en `salida/verificar_espacio_pasada3.txt`): enumeración de 3116
combinaciones sin segunda afirmación verdadera; 12 mutantes cazados por su sonda; batería §P7 congelada
(48 reglas) en +0,3 pp (mezcla) y +0,7 pp (rama CURSOS). Residuos declarados:

- Rama PROMEDIO: +10,4 pp por reglas de largo, con un margen máximo del 2,1 % (§P7-B: inexplotable); sin esas
  reglas, −0,2 pp. *Ronda 2, H3 de la FASE 2C: con otra muestra de N = 100 sale +2,4 pp. La cifra depende
  del bloque de semillas, y los márgenes (2,6 % o 2 palabras como máximo) la dejan inexplotable.*
- Canónica: 100 % en «la que MÁS comparte», «quedarse con 'total'» y «cifra mediana»; pesa 0,10 × 75 pp =
  +7,5 pp en la mezcla (L-5).
- Sonda S1 («descarta las cifras que están en la tabla»): +3,2 pp en la rama CURSOS y +8,4 pp en la rama
  PROMEDIO (sobre el 25 % del azar), y 50 % en la canónica.

§P7-D agotado: se usaron las 3 pasadas.

## FASE 2C: RECHAZAR (2026-10-29)

El `AgenteDetractor` rechazó la versión. El hook comprobó sus citas: 20 citas, 0 fallos. Los pasos 7 a 11
quedan sin sellar.

**La clave está limpia:** en 2000 de 2000 versiones hay una sola afirmación verdadera y es la marcada. El
paso 8 lo confirmó en 401 versiones con un oráculo que solo lee el HTML, y las 5 coherencias cumplen.

| ID | Severidad | Hallazgo | Arreglo propuesto |
|---|---|---|---|
| H1 | CRÍTICA (CORRECCIÓN) | En el 2,84 % de las versiones no canónicas la Solution dice que la cifra de la razón sin sustento «no sale de ningún cálculo con la tabla», y sí sale de uno (*δ*, la ventaja parcial de G, una suma o un producto). La semilla 109 se contradice | «…tampoco es la diferencia entre los totales, que es de *D* kilos», y esas cifras en `usadas` con su `stopifnot` y un mutante |
| H2 | ALTA | Con la cifra *T*_G, «su total fue de 318 kilos más que el de…» admite la lectura «su total fue de 318 kilos»: el distractor queda verdadero y la clave falsa. Pasa en el 17 % de las versiones | «su total superó en *x* kilos al de O», o quitar ERR-TOTAL |
| H3 | ALTA (diagnosticidad) | La razón sin sustento nombra siempre a la sede perdedora, y «todos sus cursos», siempre a la ganadora: atajo del 50 %, unos +16,7 pp | decide el profesor |
| H4 | MEDIA | «participó 1 curso más, que aportan», en el 25,7 % de las versiones | `if (p$k == 1) "aporta" else "aportan"` (líneas 224 y 255) |
| H5 | MEDIA | Las sondas S1 a S6 del verificador no tienen umbral; la regla «molde atípico» se contó como regla de largo sin medir su margen | ampliar `verificar_espacio.R` |
| H6 | BAJA | Dos `calcula()` sin `stop()` para cuando se llaman fuera de contexto | añadirlo |
| H7 | BAJA | Los encabezados `###` salen numerados en el PDF | revisar en el paso 3 |

H1 nació de una medida de diagnosticidad: para cerrar el canal 4, la razón sin sustento cita una cifra
inventada, y a veces esa cifra coincide con un cálculo. Es el riesgo que describe §P7-D. La suite completa no
terminó (timeout, exit 124); las 6 suites dirigidas pasaron.

## Ronda 2: corrección tras la FASE 2C (2026-10-29, `orquestador-schoice`)

`.Rmd` corregido: md5 `c2e2e4fb552d37bc06aae39202588b66`. Pasos 3 a 6 sellados con su nota. Evidencia en
`salida/verificar_espacio_ronda2.txt` y `salida/r2/`.

| Hallazgo | Arreglo | Guarda / prueba |
|---|---|---|
| H1 (CORRECCIÓN) | «La cifra de *y* kilos tampoco es la diferencia entre los totales, que es de *D* kilos.» *y* evita además `calculos_tabla()`: δ, *a*_G + *a*_P, *n*·δ, *k*·*a*, *T*_G, *T*_P y los productos cruzados | `stopifnot` «H1: …» y mutante M13; barrido de 1000 semillas: la frase es verdadera en 887/887 no canónicas |
| H2 | ERR-TOTAL sale del pool y de las cifras de G; el cierre «su total fue de *x* kilos más que el de *O*» se conserva | `stopifnot` «H2: ninguna cifra es el total de la sede que nombra» y mutante M16 |
| H4 | «participó 1 curso más, que aporta» | barrido: 0/1000 |
| H5 | umbral de +8 pp para S1-S6; guarda del canal 3; margen de «molde atipico» en palabras; sondas S_U y S_todos | `verificar_espacio.R`, mutante de medición M15 |
| H6 | `stop()` fuera de contexto en los 6 `calcula()` | prueba directa en E3 |
| H7 | encabezados de la Solution en negrita: 0 numerados en 7 PDF | `pdftotext` |
| L-1 | las no canónicas dicen «recogido» en el enunciado y en el encabezado; la canónica conserva «recolectado» | `stopifnot` «L-1: un solo verbo…» y mutante M14 |

**Variante descartada para H2, medida:** el cierre «su total superó en *x* kilos al de *O*» abría un canal
léxico de +18,1 pp en la rama CURSOS («la que MÁS comparte», 51 %) y de +14,3 pp en la rama PROMEDIO («la que
MENOS comparte», 47 %). Salida en `salida/verificar_espacio_ronda2_variante_supero.txt`.

**Medido sobre la versión corregida:** `verificar_espacio.R` APROBADO (0 fallos; 16 mutantes: 14 sobre
`data_generation`, cazados por su sonda, y 2 de medición, M12 y M15, detectados. La nota de sello de
`generacion_rmd` dice «15»: es un error de conteo, y la cifra correcta es esta); arsenal v6.3 con 0 errores y 7 advertencias heurísticas; diversidad sustantiva `PASS` (95 valores de
la clave en 100); diagnosticidad `PASS` (H1 0 % y H2/H3 ciegas por el prefijo uniforme); multisemilla 100/100;
287/300 versiones únicas; `exextra` literales en el catálogo canónico.

**Residuos para la firma del profesor (§P7-D agotado), medidos ANTES de la FASE 2C de la ronda 2.** Las
cifras corregidas y los canales nuevos están en § «FASE 2C, ronda 2», más abajo:

- O-1, que no cambió en este ciclo: S_U vale 50,0 % exacto en las dos ramas (+16,7 pp atribuibles, el alcance
  firmado). S_todos da 50 % por construcción y midió 44 % en CURSOS y 54 % en PROMEDIO con N = 100.
- S1 («descarta las cifras de la tabla»): 33,5 % en la rama PROMEDIO (+8,5 pp) y 33,2 % en la mezcla ponderada
  (+8,2 pp). La cota del diseño es +8,3 pp: con la guarda del canal 3, S1 equivale a descartar una opción de
  cuatro.
- Rama PROMEDIO, §P7-B: +10,4 pp por reglas de largo, con márgenes máximos de 2,1 % en caracteres y 6,7 % en
  palabras (inexplotable); sin esas reglas, +1,9 pp.
- Canónica: 0,10 × 75 pp = +7,5 pp en la mezcla.

## FASE 2C, ronda 2: APROBAR_CON_CAMBIOS (2026-10-29)

- **Quién y qué auditó:** un detractor nuevo (no el de la ronda 1), lanzado sin `name:`. Auditó el md5
  `c2e2e4fb…`, que es el vigente.
- **Reporte:** íntegro en `salida/fase2c_r2_detractor.md`, con 16 citas verificadas (0 problemas). El hook
  registró «ENTREGA VERIFICADA».
- **Corrección limpia:**
  - Enumeró 630 592 configuraciones con un oráculo que lee el texto mostrado: ninguna tiene una segunda
    verdadera ni la clave falsa.
  - La lectura «su total fue de *x* kilos» no vuelve verdadera ninguna opción.
  - En Solution y feedback hay 0 afirmaciones falsas (200 versiones y la canónica).
- **6b:** `APTO_CON_OBSERVACIONES`, con 0 hallazgos críticos y 6 bajos.
- **Paso 8:** las 5 coherencias cumplen, `COHERENCIAS_CON_OBSERVACIONES`, con 3 hallazgos bajos.
- **Pasos 7 a 10 sellados:** 10 de 11.

| ID | Severidad | Hallazgo | Estado |
|---|---|---|---|
| H1 | ALTA (diagnosticidad) | Quitar ERR-TOTAL subió la cuota de ERR-PARCIAL al 39,3 % en la rama PROMEDIO. Como *D* = parcial_G − parcial_P, eso abrió R_dif («la cifra que es diferencia de otras dos»): +9,6 pp en esperanza. Además, la premisa de O-1 («elegir entre las dos de G exige calcular *D*») es falsa: S_U más una pista superficial acierta entre el 71 % y el 79 % (2500 semillas) | **al profesor** (WAIT_USER #3) |
| H2 | MEDIA (testing) | El verificador convertía S1 por encima de +8 pp en exit 0 | **corregido:** sale con exit 3, «pendiente de firma»; S_todos pasa a aserción estructural; se añaden las sondas R_dif, S_U + S1 y S_U + suma |
| H3 | MEDIA | Las cifras +10,4 pp (§P7-B) y +8,5 pp (S1) dependen del bloque de semillas | **corregido aquí:** §P7-B en la rama PROMEDIO da entre +2,4 y +10,4 pp en dos muestras de N = 100, con márgenes de 2,6 % o 2 palabras como máximo, así que es inexplotable. S1 en la rama PROMEDIO da +7,3 pp en esperanza y +8,5 pp con N = 100 |
| H4 | BAJA | «La tabla da solo el promedio por curso de cada …», cuando la tabla también da la cantidad de cursos | **pendiente:** «Sobre lo recogido, la tabla da solo…». Tocar el `.Rmd` exige una FASE 2C nueva |

**Verificador sobre esta versión tras la FASE 2C** (`salida/verificar_espacio_ronda2_tras_fase2c.txt`): exit 3,
`PENDIENTE DE FIRMA (7; 0 fallos)`. Con N = 100 por rama:

| Sonda | CURSOS | PROMEDIO | Canónica |
|---|---|---|---|
| S_U (lo firmado en O-1) | 50,0 % | 50,0 % | 50 % |
| S_U + S1 | 74,0 % | 55,5 % | 100 % |
| S_U + suma | 64,0 % | 69,5 % | 50 % |
| R_dif | 32,0 % (+7,0) | 34,4 % (+9,4) | 25 % |
| S1 | 28,6 % | 33,5 % (+8,5) | 50 % |

**Variante C, medida sobre una copia** (`salida/verificar_espacio_ronda2_variante_C.txt`):

- **Qué cambia:** el cierre pasa a «su total fue más alto que el de *O* en *x* kilos», sin «fue de N kilos»,
  con lo que cumple la letra de la regla #13 v1.1. ERR-TOTAL se repone.
- **R_dif** baja a +5,2 pp en la rama CURSOS y a +6,1 pp en la rama PROMEDIO.
- **La batería** queda igual: +0,3 / +0,7 / +10,4 pp por reglas de largo, con márgenes de 2,0 % y 6,5 %.
- **L-2:** sin violaciones.
- **S_U más pistas:** entre el 55 % y el 71 %, así que la premisa de O-1 sigue siendo falsa.
- **Sus dos FALLA son de la copia:** M16 y la lista de H6 no cuentan con ERR-TOTAL.

**Otros cierres medidos:** «superó en» da +18,1 / +14,3 pp y «fue mayor que», +21,1 / +16,4 pp. En los dos hay un
canal léxico porque el cierre común pierde «más».

## Ronda 3: variante C (2026-10-29, `orquestador-schoice`)

Decisión del profesor en el WAIT_USER #3 de la ronda 2 (`r`, variante C); reapertura #2 desde `generacion_rmd`.
`.Rmd` corregido: md5 `cd94b59adfdea7b9c92dc074ce0a1435`. Evidencia en `salida/verificar_espacio_ronda3.txt`,
`salida/verificar_espacio_ronda3_tras_fase2c.txt` y `salida/r3/`. No es una pasada nueva de diagnosticidad (§P7-D
agotado): es cumplimiento de la regla #13 v1.1 y corrección.

| Cambio | Dónde | Guarda / prueba |
|---|---|---|
| Cierre de las no canónicas: «su total fue más alto que el de *O* en *x* kilos»; la canónica sigue verbatim | `cierre()` | M16 (vuelve al cierre «fue de *x* kilos más que»): cazado por su sonda en la semilla 1 |
| ERR-TOTAL repuesto (*x* = *T*_G) en el pool y en las cifras de G | `errores_conceptuales`, `cand_g` | H6: los 7 `calcula()` se detienen fuera de contexto |
| Lecturas del texto (regla #13 v1.1): `lecturas_de()`, `verdad(…, lectura)` y la guarda «ninguna lectura que admite el texto cambia el veredicto», que sustituye a la H2 de la ronda 2 | `data_generation` | M17 (ERR-TOTAL con el cierre «fue de *T*_G kilos más que»): cazado por su sonda en la semilla 3 |
| Guarda estructural: en las no canónicas la cifra aparece una vez, en «… en *x* kilos.» | `data_generation` | M18 (una razón sin sustento cita otra cifra): cazado por su sonda en la semilla 5 |
| H4 de la ronda 2: «Sobre lo recogido, la tabla da solo el promedio por curso de cada …» | `explica()` de ERR-INVENTA (U_a, U_b) | PDF de la semilla 3 revisado a ojo |

**Lecturas, medidas por enumeración** (`verificar_espacio.R`, E3):

- 89 916 textos no canónicos (todos los moldes con todas las cifras posibles de cada papel): ninguno admite la lectura
  «total» según el generador, y el oráculo léxico propio, más amplio («fue», «recogió», «con», «de»… + *N* kilos), no
  encuentra ninguna cifra presentada como cantidad.
- **Exposición declarada.** Si se leyera «en *x* kilos» como el total, la cifra falsa de G sería verdadera en 8 536 de
  39 116 configuraciones de G en CURSOS y en 3 928 de 19 640 en PROMEDIO, todas con *x* = *T*_G (ERR-TOTAL). Por
  versión, el detractor de la ronda 3 lo cifra en el 17,3 % (CURSOS) y el 20,9 % (PROMEDIO). La gramática del
  comparativo no admite esa lectura: es la misma estructura que la regla #13 v1.1 propone como reescritura («superó en
  N kilos a»), y el detractor lo juzgó igual (el desliz es el error que ERR-TOTAL diagnostica, no una segunda lectura).
- **Canónica (H-2).** Su clave verbatim («el total fue de 20 kilos más») admite «el total fue de 20 kilos», que la
  vuelve falsa sin volver verdadera ninguna otra opción. Es la única excepción de la guarda: la regla #13 v1.1 pediría
  reescribirla y la regla #24 H-2 lo prohíbe. Conflicto declarado, no resuelto en silencio.

**Verificador:** exit 3. Antes de la FASE 2C, «PENDIENTE DE FIRMA (5; 0 fallos)»; después, con las sondas que pidió el
detractor (cifra atípica, las dos pistas, las tres pistas con la atípica) y el margen §P7-B medido en todos los
estratos, «PENDIENTE DE FIRMA (11; 0 fallos)». 18 mutantes: 16 sobre `data_generation`, todos cazados por su sonda
(M1-M11, M13, M14, M16-M18), y 2 de medición (M12, M15), detectados. Enumeración de 3116 combinaciones sin segunda
verdadera. Modo de depuración nuevo: `VERIF_N=<n>` (regla #23: la cifra oficial es la de N = 100, el default).

**Arsenal y paso 9.** Arsenal v6.3: 0 errores, 7 advertencias heurísticas (2E/2F). Diversidad sustantiva `PASS` (95
valores de la clave en 100); diagnosticidad `PASS` (H1 0 %, margen mediano 2 %; H2/H3 ciegas por el prefijo uniforme;
H3b 6 %); multisemilla 100/100; 271/300 versiones únicas (contadas con las opciones ordenadas: las ~30 canónicas
cuentan como una).

**Vector de diagnosticidad** (N = 100 por rama, semillas 1001..; depuración con N = 1000 por rama, regla #23):

| Sonda | CURSOS | PROMEDIO | Canónica | Depuración CURSOS / PROMEDIO |
|---|---|---|---|---|
| S_U (lo firmado en O-1) | 50,0 % | 50,0 % | 50 % | 50,0 / 50,0 |
| S_U + S1 | 71,0 % | 55,0 % | 100 % | 67,7 / 52,8 |
| S_U + suma | 60,5 % | 62,0 % | 50 % | 58,5 / 59,4 |
| S_U + S1 + suma (las dos pistas) | 79,0 % | 67,0 % | 100 % | 72,2 / 62,1 |
| S_U + atípica | 51,5 % | 58,0 % | 100 % | 55,7 / 59,9 |
| S_U + S1 + suma + atípica | 81,5 % | 74,5 % | 100 % | 77,9 / 71,4 (mezcla 77,6) |
| Atípica sola | 25,1 % (+0,1) | 26,8 % (+1,8) | 33,3 % | 26,2 / 27,5 |
| R_dif | 30,2 % (+5,2) | 31,1 % (+6,1) | 25 % | 29,5 (+4,5) / 30,2 (+5,2) |
| S1 | 28,2 % (+3,2) | 33,4 % (+8,4) | 50 % | 27,6 (+2,6) / 32,4 (+7,4) |
| Batería §P7 congelada (48 reglas) | +0,7 pp | +10,4 pp por reglas de largo | 0,10 × 75 pp = +7,5 pp en la mezcla | mezcla: +0,3 pp |

- R_dif bajó de +9,4 pp (ronda 2, N = 100) a +6,1 pp en PROMEDIO: ya no supera +8 pp.
- ERR-TOTAL repuesto abre la pista de la cifra atípica (*T*_G dobla a las otras tres cifras en el 100 % de las
  versiones con ERR-TOTAL). Sola no llega a +2 pp; sumada a S_U y a las otras dos pistas es el atajo más alto medido.
- Canal de largo: con N = 100 da +10,4 pp en PROMEDIO; el detractor lo encontró en las **dos** ramas con su muestra de
  depuración (unos +11 pp). Margen máximo del 2,0-2,7 % en caracteres y del 6,5 % en palabras: §P7-B, inexplotable.
- S1 en PROMEDIO: +8,4 pp con N = 100 y +7,0 a +7,4 pp en las dos muestras de depuración (zona gris).

## FASE 2C, ronda 3: APROBAR_CON_CAMBIOS (2026-10-29)

- **Quién y qué auditó:** un detractor nuevo (ni el de la ronda 1 ni el de la ronda 2), lanzado sin `name:`, sobre el
  md5 `cd94b59a…`, el vigente en la ronda 3 (desde la ronda 4b el vigente es `e293d5c8…`).
- **Reporte:** íntegro en `salida/fase2c_r3_detractor.md`, con 11 citas verificadas (0 problemas); el hook registró
  «ENTREGA VERIFICADA».
- **Corrección limpia:** su oráculo lee el texto y la tabla mostrados: 1 677 264 textos de opción (todo el espacio) y
  7700 versiones reales, con una sola verdadera, la marcada; 0 afirmaciones falsas en la Solution y el feedback (795
  canónicas); canónica idéntica al impreso; Moodle con una sola fracción 100 por pregunta.
- **Testing:** M16, M17 y M18 llegan al entorno y mueren por su sonda con su propio arnés; ninguna guarda retirada
  entre la ronda 2 y la 3.

| ID | Severidad | Hallazgo | Estado |
|---|---|---|---|
| H1 | ALTA (diagnosticidad) | La cifra *T*_G de ERR-TOTAL es atípica: S_U + S1 + suma + atípica acierta el 77,3 % en la mezcla (depuración), sin calcular ningún total | **al profesor** (WAIT_USER #3); sondas añadidas al verificador como pendientes de firma |
| H2 | MEDIA (testing) | Los veredictos en el borde dependen del bloque de semillas: S1 en PROMEDIO (+8,4 con N = 100; +7,0 en depuración) y el canal de largo, que existe en las dos ramas | **corregido en el verificador:** margen §P7-B en todos los estratos; cifras declaradas con su intervalo |
| H3 | BAJA (Solution) | La causa raíz de ERR-TOTAL dice «calcula un total, pero no compara», y la afirmación sí compara: lo que falta es restar | **pendiente:** «…calcula un total, pero no le resta el del otro grupo». Tocar el `.Rmd` exige una FASE 2C nueva |

- **6b:** `APTO_CON_OBSERVACIONES` sobre 100 HTML (9 canónicas) y 52 capturas a 360 y 1024 px: 0 hallazgos críticos,
  altos o medios; 3 bajos (B1: cabeceras de la tabla sin relleno de celda, estilo de la familia F2; B2: «El / Bosque»
  partido a 360 px en 12/100; B3: un paréntesis huérfano en la Solution de ERR-TOTAL a 360 px).
- **Paso 8:** las 5 coherencias cumplen, `COHERENCIAS_CON_OBSERVACIONES`, con un oráculo propio sobre 31 200 versiones (1000 HTML,
  200 Moodle y 30 000 de texto) más 67 PDF, 40 DOCX y 30 NOPS; 4 hallazgos bajos: la lectura «total» residual de ERR-TOTAL
  (17,5 %, no es CORRECCIÓN), `cursos_txt` sin uso (línea 18), la causa raíz de ERR-TOTAL (la H3 del detractor) y un
  encabezado en negrita huérfano al pie en 11 de 60 PDF.
- **Pasos 7 a 10 sellados:** 10 de 11. La suite completa no se corrió en esta ronda (la corre el coordinador).

## Rondas 4 y 4b: los 6 bajos de la ronda 3, antes del aula (2026-10-29)

**Decisión del profesor (2026-10-29):** corregir ahora, antes de llevarlo al aula, los 6 hallazgos bajos que
la ronda 3 dejó para otro ciclo. En el aula se usan celulares, que es donde fallaban dos de ellos.
Reaperturas #3 (ronda 4) y #4 (ronda 4b), las dos desde `generacion_rmd`. La aprobación de la ronda 3 queda
des-sellada hasta la nueva decisión.

| Hallazgo (ronda 3) | Corrección | Evidencia |
|---|---|---|
| H3 del detractor: la causa raíz de ERR-TOTAL decía «no compara» | «…calcula un total, pero no le resta el del otro grupo» | verdadera en las 22/100 versiones con ERR-TOTAL (FASE 2C r4b) |
| Paso 8: `cursos_txt` sin uso | retirada | `.Random.seed` idéntico tras el chunk en las semillas 1-1000 (FASE 2C r4) |
| Paso 8: encabezado en negrita huérfano al pie del PDF | `encabezado()`: tras cada encabezado, bloque `{=latex}` con `\nobreak` y `\@afterheading` (el cierre de un título de sección de LaTeX) | semillas 700001-700060: 14 → 0 (el paso 8 contó 11: su detector no veía las ligaduras «fl»/«fi»); FASE 2C r4: 18/100 → 0/100; FASE 2C r4b: r3 42, r4b 0, mutante sin el bloque 51 |
| 6b B1: cabeceras de la tabla a 4 px | `<span>` por celda: cabeceras al 90 % y 0,5 em a la izquierda de las columnas 2 y 3 | separación mínima entre cabeceras 11,2-13,5 px (r3: 4-5,8) |
| 6b B2: «El / Bosque» partido a 360 px | espacio duro (U+00A0) solo en las celdas de «El Bosque» y «La Pradera»; en LaTeX sale `~` | 0/12 versiones partidas a 360, 320 y 1024 px |
| 6b B3: «(» huérfano en la Solution de ERR-TOTAL | `explica()` sin paréntesis alrededor de la fórmula | 0 casos en las 22 versiones con ERR-TOTAL a 360, 320 y 1024 px |

**Ronda 4b: regresión propia corregida.** El primer relleno de B1 (0,6 em por lado en todas las celdas) hacía
que la tabla no cupiera en su caja a 360 px en 49/100 versiones (0/100 en la versión aprobada) y a 320 px en
100/100 (9/100). Se midieron 9 variantes en 100 versiones (`salida/r4/medicion_relleno_tabla.md`): cualquier
relleno en las columnas 2 y 3 lleva la cifra a 320 px a 49/100, salvo que se compense bajando las cabeceras al
90 %. Con esa variante (F05), 0/100 a 360 px y 9/100 a 320 px (las canónicas, igual que en la versión aprobada).

**Invariancia.** Opciones, clave, `exsolution`, feedback, enunciado y celdas idénticos a la versión aprobada en
las semillas 1-1000 (FASE 2C r4); Question idéntica en 100/100 versiones pareadas, con 9 canónicas (FASE 2C r4b).
Por eso §P7, O-1 corregido y la canónica (H-2) no cambian: 271/300 únicas, diversidad y diagnosticidad PASS.

**Guardas nuevas en `verificar_espacio.R`** (no tocan el `.Rmd`; salida `salida/verificar_espacio_ronda4b.txt`:
«EN ROJO BAJO OVERRIDE FIRMADO (13 canales dentro del alcance de O-1 corregido; 0 fallos, 0 pendientes)»,
exit 4; antes, «PENDIENTE DE FIRMA (11; 0 fallos)» permanente. En la ronda 4b llegó a decir «APROBADO
(0 fallos)» con exit 0, y así quedó en el commit `8efdf504`; se corrigió el mismo 2026-10-29 porque la regla
#22 §P7-E deja la batería en rojo bajo un override, y un PASS apagaría el gate):
- H1 de la FASE 2C r4: el alcance firmado de «O-1 corregido» como tabla (`FIRMA_O1`, `FIRMA_S1_PROMEDIO`,
  `FIRMA_LARGO`), comparada a un decimal como se firmó; el mutante M19 (ERR-TOTAL siempre que se pueda) saca
  3 de las 10 cifras de lo firmado.
- H3 de la FASE 2C r4: la suma de control de la canónica también sobre la tabla mostrada (`tabla_md`); el
  mutante M20 cambia una celda después de `tabla_df`, y la guarda del `.Rmd` no lo ve.
- H1 de la FASE 2C r4b: guarda estructural de los 8 encabezados de la Solution; mutantes M21 y M22.

| Revisión | Veredicto | Reporte |
|---|---|---|
| Arsenal (r4 / r4b) | 0 errores, 6 / 7 advertencias heurísticas | informes en `~/.cache/post-exams2-validation/` |
| FASE 2C r4 (md5 `7d5855da…`) | `APROBAR_CON_CAMBIOS`, 0 CORRECCIÓN; H1 y H3 hechos en el verificador | `salida/fase2c_r4_detractor.md` |
| 6b r4 (100 HTML a 360, 320 y 1024 px) | `APTO_CON_OBSERVACIONES`: B1, B2 y B3 corregidos; la regresión del relleno, MEDIA | `salida/r4/6b_auditoria_visual.md` |
| Paso 8 r4 | `COHERENCIAS_CON_OBSERVACIONES`: 5/5, 0 fallos en 130 HTML, 46 PDF, 12 DOCX y 24 Moodle | `salida/paso8_r4_coherencias.md` |
| FASE 2C r4b (md5 `e293d5c8…`) | `APROBAR_CON_CAMBIOS`, 0 CORRECCIÓN, 0 críticas, 0 altas | `salida/fase2c_r4b_detractor.md` |
| Caja de la pregunta en el Moodle del colegio (2026-10-30, md5 `e293d5c8…`, N = 100) | 270 px a 320 y 310 px a 360; 0/100 tablas fuera de su caja, con Roboto y con Noto Sans | `salida/r4b/medicion_qtext_moodle.md` |

**Residuos de las rondas 4 y 4b, para otro ciclo (ninguno toca la clave, las opciones ni la Solution):**
- ~~**MEDIA, deuda de la familia F2 (H2 r4, H4 r4b)**~~. **Resuelta en el repo el 2026-10-29** (regla #21
  v1.4): la `tabla_responsiva()` canónica tiene `separacion` y `tam_cabecera`, pero **desactivados por
  defecto**. Medida en los 7 ejercicios que llaman a la F2, la misma variante saca la tabla de su caja a
  360 px en `grafica-circular-consumo-agua` v2 (de 0/20 a 12/20). Queda para el próximo ciclo de este
  `.Rmd` actualizar el comentario de su copia local («la función de snippets_familias_rmd.R no lo tiene»).
- ~~**BAJA, H2 r4b**~~. **No aparece en el Moodle del colegio: medido el 2026-10-30**
  (`salida/r4b/medicion_qtext_moodle.md`). El espacio duro sube el ancho mínimo de la tabla en las 12/100
  versiones de «El Bosque» / «La Pradera» (hasta +21,4 px), y eso solo pesaba con cajas de menos de 240 px. En
  el Moodle del colegio (Moodle 5.1.7, tema Boost) la caja de la tabla (`.qtext`) mide 270 px a 320 px y
  310 px a 360 px. Con las 100 versiones del md5 `e293d5c8…`, ninguna tabla sale de su caja (0/100 a 320 y a
  360 px, con Roboto, la letra de Android, y con Noto Sans). La más ancha es la canónica (225,6 px con Roboto);
  las de «El Bosque»/«La Pradera» llegan a 209,9 px. El medidor marca 49/100 con una caja de 200 px. Queda
  para el próximo ciclo de este `.Rmd` anotar ese umbral en el comentario del espacio duro, como pedía la FASE
  2C r4b.
- **BAJA, paso 8 r4, código muerto:** `contexto_tipo` y el campo `tipo` de cada plantilla, la rama `dif_cursos`
  de `verdad()`, `b_debajo` y `lado_gdis` en la rama canónica (`lado_gdis` sí lo lee el verificador) y el
  parámetro `m` de `MOLDES`.

## Próximo paso

1. ~~El profesor decide H3 y la redacción~~. **Hecho el 2026-10-29:** override O-1 con alcance hasta
   +16,7 pp, y «recogido»/«recolectado» normalizado en las versiones no canónicas (`.claude/CLAUDE.md`
   § O-1 y § L-1).
2. ~~Reabrir~~. **Hecho el 2026-10-29:** reapertura #1 desde `generacion_rmd` (4 pasos des-sellados; quedan
   2 de 11), con el reporte íntegro del detractor en `salida/fase2c_r1_detractor.md`. Sus 20 citas verifican
   contra los archivos actuales.
3. ~~Corregir H1, H2, H4, H6 y H7, y ampliar el verificador (H5)~~. **Hecho el 2026-10-29 (ronda 2):** ver
   § «Ronda 2».
4. ~~FASE 2C con un detractor nuevo y los pasos 8 a 10~~. **Hecho el 2026-10-29:** la FASE 2C de la ronda 2 dio
   `APROBAR_CON_CAMBIOS`, sin objeciones de CORRECCIÓN, y los pasos 7 a 10 están sellados: 10 de 11.
5. **WAIT_USER #3, con dos decisiones previas del profesor.** H1, ALTA en la FASE 2C de la ronda 2, bloquea la
   FASE 3 hasta que decida:
   - **(a) O-1:** corregir su texto, porque su premisa es falsa (S_U más una pista acierta entre el 55 % y el
     74 %), y fijar su alcance con las cifras de § «FASE 2C, ronda 2».
   - **(b) Qué versión va al aula:**
     - esta versión, con R_dif en +9,4 pp en la rama PROMEDIO, pendiente de su firma;
     - o la **variante C**. Exige reabrir desde `generacion_rmd`, volver a pasar la FASE 2C con un detractor
       nuevo y repetir el 6b y el paso 8. La H4 (BAJA) entra en ese ciclo.
6. **Decisión del profesor en el WAIT_USER #3 (2026-10-29): `r`, variante C.** Se reabre desde
   `generacion_rmd` (reapertura #2) para hacer cuatro cosas:
   - cambiar el cierre por «su total fue más alto que el de *O* en *x* kilos»;
   - reponer ERR-TOTAL;
   - corregir la H4 de la ronda 2;
   - medir el atajo combinado de O-1.

   La premisa falsa de O-1 queda tachada en `.claude/CLAUDE.md` § O-1, con su medición. Antes de relanzar, el
   coordinador corre la suite completa y hace el push de `7c36722d` y `e5adae39`, con el árbol quieto.

   **Hecho el 2026-10-29 (ronda 3):** ver § «Ronda 3» y § «FASE 2C, ronda 3», más arriba.
7. **WAIT_USER #3 de la ronda 3.** No hay objeciones de CORRECCIÓN. La H1 de la FASE 2C (ALTA, diagnosticidad) bloquea la
   FASE 3 hasta que decida el profesor:
   - **(a) O-1:** corregir su texto y fijar su alcance. Hoy cubre solo S_U (50 %, +16,7 pp). Quedan fuera, medidos por
     rama con N = 100 (y depuración N = 1000): S_U + S1, 71,0 / 55,0 % (67,7 / 52,8); S_U + suma, 60,5 / 62,0 %
     (58,5 / 59,4); las dos pistas, 79,0 / 67,0 % (72,2 / 62,1); S_U + atípica, 51,5 / 58,0 % (55,7 / 59,9); las tres
     pistas con la atípica, 81,5 / 74,5 % (77,9 / 71,4; 77,6 % en la mezcla, 77,3 % según el detractor).
   - **(b) Residuos para su firma:** S1 en PROMEDIO (+8,4 pp con N = 100; +7,0 a +7,4 pp en depuración); la exposición de
     ERR-TOTAL a la lectura «total» (17-21 % de las versiones; ni el detractor ni el paso 8 la tienen por CORRECCIÓN); la
     canónica (+7,5 pp en la mezcla) y su clave verbatim, que admite «el total fue de 20 kilos» (H-2 frente a la regla
     #13 v1.1). Declarados sin firma: el canal de largo (§P7-B, margen máximo 2,7 %, inexplotable) y R_dif
     (+5,2 / +6,1 pp, por debajo de +8).
   - **(c) «¿Lo apruebas para llevarlo al aula?»** `a` sella `aprobacion_usuario`; `r` reabre desde `generacion_rmd`
     (la H3 y los bajos del paso 8 entrarían en ese ciclo, con una FASE 2C nueva); `p` pausa.
8. Antes del push, el coordinador corre la suite completa (`R_TESTS_FULL=1`).
9. **Decisión del profesor en el WAIT_USER #3 de la ronda 3 (2026-10-29): `a`, aprobado para el aula.**
   `aprobacion_usuario` queda sellado: 11 de 11.
   - Firmó O-1 corregido, con alcance hasta 81,5 % (CURSOS) y 74,5 % (PROMEDIO) para los atajos combinados,
     y los residuos de (b) (`.claude/CLAUDE.md` § «O-1 corregido»).
   - El conflicto de la clave canónica se resolvió en la norma: regla #13 v1.2, en la canónica prevalece el
     impreso (H-2).
   - Los hallazgos bajos quedan para un ciclo futuro, porque cualquier edición del `.Rmd` exige una FASE 2C
     nueva: la H3 del detractor, `cursos_txt` sin uso, el encabezado huérfano al pie en 11 de 60 PDF y los 3
     del 6b.
10. **Siguiente hito: la aplicación en el aula** con estudiantes reales (Nivel 3). La promoción a
    `03-En-Produccion/` se pide solo con esa evidencia (`/promover-ejercicio`).

11. **Rondas 4 y 4b (2026-10-29):** los 6 bajos corregidos, ver § «Rondas 4 y 4b».
12. **Decisión del profesor en el WAIT_USER #3 de las rondas 4 y 4b (2026-10-29): `a`, aprobado para el aula**
    en la versión md5 `e293d5c8…`. `aprobacion_usuario` queda sellado: 11 de 11. Los residuos de § «Rondas 4 y
    4b» quedan para otro ciclo.
13. **Siguiente hito: la aplicación en el aula** con estudiantes reales (Nivel 3); la promoción a
    `03-En-Produccion/` se pide solo con esa evidencia (`/promover-ejercicio`).
14. ~~Medir el ancho real de `.qtext` en el Moodle del colegio a 320 px (H2 r4b)~~. **Hecho el 2026-10-30:**
    270 px, por encima del umbral de 240 px; 0/100 tablas fuera de su caja (`salida/r4b/medicion_qtext_moodle.md`).
    El hito pendiente sigue siendo el 13.

Aprobado para llevarlo al aula el 2026-10-29 (md5 `e293d5c8…`, ronda 4b); no está en producción.

## Ciclo 2026-11-02, tanda S1: detenida en el verificador

- Reapertura #5 desde `generacion_rmd`: quedan 2 de 11 pasos. El `.Rmd` en disco es el md5 `5f6f3cf3…`, sin aprobar. La versión
  aprobada para el aula (`e293d5c8…`) sigue en `HEAD`: `git show HEAD:<ruta del .Rmd>`.
- S1, S3, S4, S5, S6 y S7 están aplicados. `.Random.seed` sale idéntico en las semillas 1 a 1000, y el texto también, salvo el
  espacio.
- El verificador sale con exit 3 por una sola cifra: el margen en palabras del canal de largo de la rama PROMEDIO pasa de 6,5 %
  a 6,7 %. Lo causa el contador de palabras de la batería, que no separa en U+00A0. Detalle y opciones: `.claude/CLAUDE.md` local,
  § «Ciclo 2026-11-02 (SCHOICE), tanda S1». Evidencia: `salida/ciclo_20261102/`.
- **Próximo paso:** el profesor decide (a), (b) o (c). Con la decisión, la tanda S1 sigue así:
  1. Aplicarla y volver a correr el verificador (con la (a) se espera exit 4).
  2. Sellar los pasos 3 y 4.
  3. Renderizar los 4 formatos con el hook y sellar los pasos 5 y 6.
  4. Hacer la 6b pareada contra `e293d5c8…` a 360 y 320 px, más la caja de Moodle.

  Después viene la tanda S2: FASE 2C, pasos 8 a 10 y WAIT_USER #3.

**Actualización (2026-11-02, cierre de la tanda S1).** El profesor decidió (a), firmó L-6 y anotó lo de `ntok()`. El `.Rmd`
quedó en el md5 `4cffbe8b…`, con los pasos 3 a 6 sellados uno por uno (6 de 11). El verificador da exit 4, con 37 canales,
0 fallos y 0 pendientes. La caja de Moodle quedó NO VERIFICABLE (HTTP 530); las tablas son idénticas byte a byte en 100/100.
Detalle: `.claude/CLAUDE.md` local, § «Cierre de la tanda S1».
- **Próximo paso:**
  1. Recoger la 6b pareada de `salida/ciclo_20261102/6b/`, o de su transcripción si no dejó informe.
  2. Tanda S2: FASE 2C con un detractor nuevo, sin `name:`. Tiene que revisar S1, L-6, M23 a M29, MP1 y MP2.
  3. Pasos 8 a 10.
  4. WAIT_USER #3: la aprobación anterior era de `e293d5c8`.
- **Después, la tanda del re-empalme del CLOZE:** quitar `lado_gdis` de la canónica y de `rec()`, adaptar MP2 al espacio duro
  y aplicar la decisión (a) en E4.
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

## Ciclo 2026-11-02, tanda S1b (2026-11-02, relanzada el 2026-11-03)

- **Primera S1b (2026-11-02).**
  - Aplicó V1 (md5 `5cf5b12f…`): la unidad que cierra una fórmula va dentro de ella.
  - Su 6b dio APTO_CON_OBSERVACIONES: el punto quedaba solo tras la fórmula en 34 semillas a 360 px y en 5 a 320 px.
  - Se interrumpió a las 20:34, sin reporte, a mitad de medir V2 y V1′. Esa medición tenía dos defectos del instrumento.
- **Decisiones del profesor (2026-11-03).**
  - «V1′ (Recomendada)»: el signo que sigue a la unidad entra en la fórmula, `$… = T\ \text{kilos.}$`.
  - «En esta S1b (Recomendada)»: fuera la asignación muerta de `lado_gdis` en la canónica, que cierra S7.
- **Relanzada (2026-11-03).**
  - La medición pareada, con el instrumento corregido, da 0 para V1′ en HTML (360 y 320 px), PDF y Moodle. Los controles
    positivos ven cada defecto: 4cffbe8b y V1 en HTML, e293d5c8 y un control sintético en PDF, V1 en Moodle.
  - Aplicada: `.Rmd` md5 **`6c9d77058b32fdd8291a1ebaab581c8f`**, con 14 sitios.
  - Invariancia en 1000/1000 semillas.
  - Verificador (`bd6eda09…`): exit 4, 37 canales, 0 fallos y 0 pendientes, con M30 a M33 cazados por su sonda.
  - Reapertura #8 y pasos 3 a 6 sellados uno por uno: **6 de 11**.
  - Detalle y cifras: `.claude/CLAUDE.md` local, § «Ciclo 2026-11-02 (SCHOICE), tanda S1b». Evidencia:
    `salida/ciclo_20261102/s1b/relanzada/`.
- **Declarado.**
  - Fuera del alcance de V1′ quedan las fórmulas que NO cierran en unidad seguidas de un signo: 425 en 1000 semillas, 40 en
    el Moodle de 100. El mecanismo es el mismo, pero en HTML hubo 0 partidas en 100 semillas por ancho (42 casos).
  - El `pdf/ej1.pdf` del render oficial de la primera S1b (`s1b/render_5cf5b12f/pdf/`) quedó sobrescrito por el PDF de
    medición de 100 ejercicios. Los renders no se versionan, y las verificaciones de aquel render siguen en
    `s1b/render_s1b_verificaciones.txt`.
- **Próximo paso (tanda S2).**
  1. FASE 2C con un detector nuevo, sin `name:`. Sobre `6c9d7705`, frente a `4cffbe8b` y `e293d5c8`, tiene que revisar:
     - V1′, la guarda nueva de L-6 y su cláusula candidata;
     - S7;
     - los cambios del verificador: S1-Sol signo, M30 a M33, el `ok()` de S7 y el control de S1-Sol fórmula;
     - el instrumento de medición (`cap_s1b2.py`, `pdf_renglones_bbox.py`).
  2. Pasos 8 a 10.
  3. WAIT_USER #3. La aprobación anterior era de `e293d5c8`. El profesor firma además la cláusula candidata de L-6 y ve los
     residuos: la letra de «kilos.» dentro de la fórmula y las fórmulas sin unidad seguidas de un signo.
- **Después, la tanda del re-empalme del CLOZE:**
  - quitar la lectura de `lado_gdis` en `rec()` (`cloze/verificar_espacio_cloze.R`, línea 117);
  - adaptar MP2 al espacio duro;
  - aplicar la decisión (a) en E4;
  - llevar V1′ a la Parte 1 (F-2).

  Hasta entonces, el verificador del CLOZE no puede correr la canónica contra este `.Rmd`.

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

## Ciclo 2026-11-02, tanda S2a (2026-11-03)

- **Qué la abrió.** La FASE 2C r5 (veredicto del detractor r5: APROBAR_CON_CAMBIOS, sin ninguna CORRECCIÓN;
  `salida/fase2c_r5_detractor.md`) dejó H1 (MEDIA) y H2 a H5 (BAJA). Sobre H2, el profesor respondió el 2026-11-03, literal:
  «Cerrarlo ahora (Recomendada)». H1, H3, H4, H5 y la fe de erratas de H2 son endurecimientos del coordinador (H-5).
- **Hecho.**
  - Reapertura #9 desde `generacion_rmd`. El `.Rmd` pasa a **`63e2713e6ac0bdbf250f4dc33901b2ec`**: el punto que sigue a la
    fórmula de ERR-RESTA entra en ella (`= δ\text{.}$ Esa resta…`), y la guarda de L-6 del signo cubre toda fórmula, con
    unidad o sin ella, y todo separador que no sea letra ni cifra.
  - Invariancia en 1000/1000 semillas: `.Random.seed`, enunciado, opciones y tabla idénticos; la Solution solo cambia en las
    425 versiones con ERR-RESTA, y queda idéntica al devolver el signo; el feedback es idéntico byte a byte.
  - `verificar_espacio.R` pasa a **`bdd0daf06ba933a1eff873770a755d59`**: controles exactos (1158, 730 y 42), separadores
    ampliados, la sonda S1-Sol signo generalizada, la guarda real del canal 2 (H5) y los mutantes M34 a M41, todos cazados
    por su sonda. Exit 4, 37 canales, 0 fallos y 0 pendientes. Ninguna cifra firmada se mueve.
  - `cap_s1b2.py` (H4) sin U+00A0 literales y con su assert: md5 `1d0117ed…`.
  - Pasos 3 a 6 sellados uno por uno (08:09:05, 08:09:11, 08:12:08 y 08:12:18): **6 de 11**. Render en
    `salida/ciclo_20261102/s2a/render_63e2713e/`. Arsenal: 0 errores y 6 advertencias. Con `dir=` literal y `exams2pdf`
    primero, el hook vio el PDF y los `.tex`.
  - Detalle, cifras y la tabla candidata de L-6 (H3): `.claude/CLAUDE.md` local, § «Ciclo 2026-11-02 (SCHOICE), tanda S2a».
- **Fe de erratas de la sección anterior.** «En HTML hubo 0 partidas en 100 semillas por ancho (42 casos)» exageraba lo
  medido: los 42 casos eran 2 geometrías (una por ancho), no 84 ensayos, y no se midió la caja del Moodle del colegio. Queda
  cerrado por construcción: 0 fórmulas seguidas de un signo fuera en 1000 semillas.
- **Próximo paso.**
  1. La 6b pareada contra `6c9d7705` (N = 100 a 360 y 320 px; en marcha al escribir esto; su resultado va abajo).
  2. FASE 2C r6, con un detractor nuevo y sin `name:`. Lo que tiene que revisar está en el `.claude/CLAUDE.md` local.
  3. Pasos 7 a 10 y WAIT_USER #3. En el WAIT_USER #3 se pide además la firma de la tabla candidata de L-6.
- **6b pareada, recibida: APTO_VISUAL**, sin hallazgos (`salida/ciclo_20261102/s2a/6b/informe_6b_63e2713e_completo.md`).
  N = 100 a 360 y 320 px: 0 signos tras una fórmula, con o sin unidad (en `6c9d7705` eran 42, sin partir); controles positivos
  iguales a los de antes (132/259, 34/5, 341/347 y 74/55); PDF y Moodle en 0; 12 recortes leídos a ojo. Lo que sigue es la
  FASE 2C r6.

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

## Ciclo 2026-11-02, tanda S2b (2026-11-03)

- **Origen.** La FASE 2C r6 (veredicto del detractor r6: APROBAR_CON_CAMBIOS, sin ninguna CORRECCIÓN;
  `salida/fase2c_r6_detractor.md`, 17 citas verificadas por el coordinador) dejó H1 (ALTA, DIAGNOSTICIDAD: el canal «resta»,
  sin medir) y H2 a H4 (BAJA). Esta tanda solo toca el verificador, el instrumento y el registro: el `.Rmd` sigue en
  `63e2713e6ac0bdbf250f4dc33901b2ec`, sin reapertura ni sellos (**6 de 11**).
- **Hecho.**
  - `verificar_espacio.R` pasa de `bdd0daf0…` a **`9c65474801b5b838183a0507233b8ea1`**. Añade «resta» y sus seis combinaciones
    con las pistas de «O-1 corregido»; el barrido del estímulo (28 operaciones entre celdas de la tabla, en dos lecturas); M44
    (ERR-RESTA siempre que se pueda) y M19, también sobre el barrido; `ESTIMULO_SC` corregida; `juzgar_s1()` simétrico; M42a-c
    y M43, aditivos; y M28, que juzga solo los pendientes nuevos del proceso sin normalizar.
  - Resultado (`salida/ciclo_20261102/s2b/verificar_espacio_s2b.txt`, 129 s): **exit 3, 36 pendientes y 0 fallos**. Los
    pendientes son los 27 de H1 y los 9 del barrido. Las 37 cifras cubiertas salen idénticas, y ninguna cifra firmada se mueve.
    Frente a la S2a, de sus 297 líneas solo cambian 9: los siete veredictos de M38, M39 y M41, que ahora salen
    «cazado_por_su_sonda_y_control», el resultado y la línea de cierre (`s2b/diff_verificador_s2a_a_s2b.txt`).
  - `cap_s1b2.py` pasa de `1d0117ed…` a `aaa1b49e…` (H4): uso `python3 -E -P`, y un assert de que el directorio de trabajo
    está fuera del repo. `python3 -E -P -c "import websockets"` funciona.
  - H3, opción (b): la tabla candidata de L-6, corregida, en el `.claude/CLAUDE.md` local. La opción (a), ampliar la guarda
    del `.Rmd`, iría con un rediseño por H1.
- **Cifras clave.** Las tablas completas, regla por regla, con la canónica exacta y su peso, están en el `.claude/CLAUDE.md`
  local, § «Ciclo 2026-11-02 (SCHOICE), tanda S2b».
  - «resta» sola: 54,2 / 58,8 / 67,8 % (mezcla de E1 / CURSOS / PROMEDIO); en la canónica, 0,0 % exacto. Aplica en el 51,
    el 45 y el 57 % de las versiones, y en las 144 no canónicas en que aplica, la opción que cita la resta es ERR-RESTA, del
    lado de G, en las 144.
  - «S_U + S1 + suma + atípica + resta»: 93,0 / 94,5 / 98,0 %; en la canónica, 100,0 %. Lo firmado sin «resta» era 81,5 % en
    CURSOS y 74,5 % en PROMEDIO.
  - Barrido: pasan de +8 pp 9 cifras, en 8 reglas. Un solo total (el producto de la fila 1) da +9,0 pp en la mezcla; las de dos
    pasos (ventajas parciales), +9,0 y +12,0 pp en una rama y antipredictivas en la otra; y en la canónica las cifras del
    impreso dan coincidencias de hasta +75,0 pp.
- **Alcance.** Por la invariancia registrada en S1, S1b y S2a, el canal está también en la versión aprobada para el aula
  (`e293d5c8`) y en la Parte 1 del CLOZE (F-2), que no se tocó. Supera la premisa de F-3: con el 93,0 %, la Parte 1 daría
  +25,5 pp de la nota de la pregunta sobre el azar, frente a los +19,7 pp con que se firmó.
- **Opciones del profesor, sin elegir.** (a) Firmar regla por regla, como override estructural, igual que O-1. (b) Rediseñar;
  por ejemplo, que la resta de los promedios salga también del lado de P, como en la C del impreso. Exige antes la enumeración
  de §P7-F y una pasada más allá de §P7-D, que solo autoriza el profesor. (c) Otra. Detalle de cada una en el `.claude/CLAUDE.md`
  local.
- **Próximo paso.**
  1. La decisión del profesor sobre H1 y el barrido, y la firma de la tabla candidata de L-6 corregida.
  2. Con (a): cablear la firma en el verificador (exit 4) y decidir si el paso 7 se sella con la r6 o con un detractor nuevo.
     Con (b): reapertura, pasos 3 a 6, 6b y FASE 2C r7 con un detractor nuevo y sin `name:`.
  3. Después, los pasos 8 a 10 y el WAIT_USER #3.
- **Restos fuera del repo:** `/tmp/x.json` y `/tmp/perfil_inexistente_s2b/`, de la prueba del assert de `cap_s1b2.py`. El
  permiso no dejó borrarlos.
