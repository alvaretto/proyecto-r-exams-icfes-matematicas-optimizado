# HANDOFF — `totales-promedios-reciclaje-n3` (MAT-2026-07-050)

> Estado al 2026-10-29. Antes de tocar nada, leer [`ITEM_OFICIAL.md`](ITEM_OFICIAL.md) y las invariantes
> [`.claude/CLAUDE.md`](.claude/CLAUDE.md), que prevalecen sobre este archivo. El subproyecto no está
> versionado todavía (`??` en git).

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
  md5 `cd94b59a…`, el vigente.
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

Aprobado para llevarlo al aula el 2026-10-29; no está en producción.
