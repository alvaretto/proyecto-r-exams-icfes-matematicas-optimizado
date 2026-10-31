# HANDOFF — CLOZE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1`

> Frase disparadora: «Continúa con el CLOZE de totales-promedios-reciclaje-n3».
> Última actualización: 2026-10-31, tanda 6 del `orquestador-cloze`, **detenida tras la FASE 2C r2 (`APROBAR_CON_CAMBIOS`)**:
> pasos 3 a 6 resellados sobre el md5 `42f725bf…` (pasada 2 de §P7-D, H1 y H3); L-C5 con las cifras exactas del código. El
> detractor encontró, fuera de la batería congelada, un canal de +75,0 pp en la Parte 2: vuelve al profesor. Antes: tanda 5 (detenida en el paso A), tanda 4 (reparto de H2 no viable), tanda 3 (FASE 2C r1 `RECHAZAR`).
> Si este archivo y `ejercicio_state.json` discrepan, manda el JSON. Las invariantes del SCHOICE (L-1 a L-5,
> «O-1 corregido»), las firmas del CLOZE (F-1 a F-4), las decisiones del profesor y las leyes del CLOZE (L-C1 a L-C5)
> están en [`../.claude/CLAUDE.md`](../.claude/CLAUDE.md) § «CLOZE hermano». El ítem verbatim, en
> [`../ITEM_OFICIAL.md`](../ITEM_OFICIAL.md).

## Estado: 6/11 (reapertura #2; pasos 3 a 6 resellados en la tanda 6; `detractor_fase2c` NO sellado: FASE 2C r2 `APROBAR_CON_CAMBIOS`, pendiente de decisión del profesor)

`.Rmd`: md5 **`42f725bf191b81784f76b858c26f7733`** (antes `96cfcf35…`). No está aprobado: faltan la FASE 2C r2, las 5
coherencias, la diversidad, la validación ICFES y la aprobación del profesor. **No usar en clase.**

## Tanda 6 (2026-10-31): L-C5 con las cifras exactas del código y pasada 2 de §P7-D (H1 y H3)

**Resultado:** md5 `96cfcf35…` → **`42f725bf191b81784f76b858c26f7733`**. Reapertura #2 desde `generacion_rmd`
(`--reporte salida/fase2c_r1_detractor.md`); pasos 3 a 6 sellados de nuevo, cada uno con su artefacto en la nota.
Quedan **Partes 2 a 6: 1 pasada de §P7-D** (usadas: tanda 1 y tanda 6).

- **Paso A (L-C5).** `FIRMA_H2` = columna `ex` de `salida/h2_exacta_96cfcf35.txt` (decisión «a · iii ampliada sí»).
  `salida/verificar_espacio_cloze_r6a.txt` (exit 3, 368 s): 67 celdas «ROJO, cubierto», A1 en CURSOS (+7,0) en «ok»,
  0 por encima, 0 fuera de la firma; MC25 a MC27 cazados; solo los 12 pendientes de la Parte 2.
- **Paso B (H1).** Parte 2 reescrita en sus dos tipos. Las ocho opciones de cada tipo llevan las mismas palabras del
  estímulo («Lo que recogió … grado … sus cursos» en «promedio»; «grado», «del» y «otro» en «comparar»), sin «total»,
  coma ni condicional; por largo, una clave entre las cortas, otra en el medio y otra entre las largas. Distractor nuevo
  «cociente» (en lugar de «mejor») en «comparar». Exactas, 720 versiones por tipo (`salida/p2_redaccion_42f725bf.txt`):
  todas las reglas de L-C4 de la Parte 2 en +2,8 pp o menos (antes, hasta +75,0). Sondas extra fuera de la batería
  (la que menos comparte, fuera del par, prefijo, descarta la más corta, tres primeras palabras): antes hasta +45,0 pp;
  ahora +3,9 pp como máximo.
- **H3.** Encabezado de la Solution según el tipo de la Parte 2.
- **Invariancia.** Por semilla (100), la Parte 1, la tabla del otro colegio, las Partes 3 a 6, el tipo y la posición de la
  clave de la Parte 2 y el `.Random.seed` final son idénticos a HEAD (`salida/rng_partes_3a6_r3.txt`). L-C5, idéntica entre
  r6a y r6b (134 líneas).

- **FASE 2C r2 (`AgenteDetractor` nuevo, sin `name:`): `APROBAR_CON_CAMBIOS`.** Reporte íntegro en
  `salida/fase2c_r2_detractor.md` (sacado de su transcripción); `verificar_citas_detractor.py`: 13 citas, 0 problemas.
  Ninguna CORRECCIÓN; reproduce con código propio L-C4 (Parte 2 en +2,8 pp o menos, con los 3 materiales y los 6 pares de
  grados), las 69 celdas de «H2 a», RL1, RL2, las canónicas, los pesos y los espacios duros.

  | ID | Severidad · tipo | Qué | Estado |
  |---|---|---|---|
  | H1 | MEDIA · diagnosticidad | Tipo «promedio»: las tres claves dicen «dividido entre» y ningún distractor; «elige la que dice dividido», «… entre» o «más palabras con el párrafo del otro colegio»: **+75,0 pp exactos**. Es H1 de la r1 desplazado («total» → «dividido entre»). Fuera de L-C4 | **Al profesor** (§P7-C: no obliga en este ciclo) |
  | H2 | MEDIA · diagnosticidad | Tipo «comparar»: «descarta las que dicen *por curso*»: **+25,0 pp exactos** (tres distractores lo dicen, ninguna clave) | **Al profesor**, con H1 |
  | H3 | MEDIA · cobertura del verificador | L-C4 no lleva el párrafo del otro colegio (`intro_par`) como estímulo de la Parte 2; por ahí entró H1 | Para L-C4 v2 (próximo ciclo; obliga a volver a medir el histórico) |
  | H4 | BAJA · proceso | El `ERR_DIAG_SUPERFICIAL` de p6 es un artefacto de la sonda H3 sobre un mchoice: «las que empiezan por Si son verdaderas» da 0,0 pp exactos; el canal real condicionado es RL2 (+33,3, firmado) | Anotar esta justificación al sellar el paso 9 |

- **Abierto al cerrar la tanda (no ejecutado o sin resultado):**
- **6b (`auditor-visual-html`, N = 24, semillas de `render_6b.R`; 120 capturas por CDP a 320, 360 y 1024 px, con
  `innerWidth` comprobado): `APTO_CON_OBSERVACIONES`**, recibido después de cerrar el reporte de la tanda. Evidencia en
  `salida/6b_r3/`.
  - BAJA 1: en la Solution de las Partes 3 a 6 (texto propio), la cifra de una ecuación queda separada de «kilos.»: 129 cortes
    en 594 casos (feedback (c) a (f), «Procedimiento correcto», «Parte 6 · Afirmaciones falsas», «Caso específico»), aunque
    el espacio duro ya está. La métrica de la r2 no contaba esta clase. Propuesta del auditor, sin probar: envolver ecuación y
    unidad en un `span` con `white-space:nowrap`. Es de tipografía, no de diagnosticidad: no gasta pasada de §P7-D, pero
    cambiar el `.Rmd` exige reapertura y otro detractor.
  - BAJA 2: palabras sueltas al final de las opciones de la Parte 2 a 320 px (22 de 96) y de sus viñetas en la Solution.
    Estética; sin acción.
  - Sin hallazgos de markup, math, partes, tablas, tildes ni anomalías entre versiones. El encabezado de la Parte 2 sigue al
    tipo en 24 de 24. Posición de la clave en 200 versiones: Parte 2, p = 0,73; Parte 1, p = 0,46.
  - Residuos de la Parte 1 (decisión 2 a), iguales a la r2: «15 / kilos.» 154 cortes en 2 014 casos, todos en la Parte 1.
- **Paso 8 (`AgenteValidadorVisual`, salida en `salida/coherencias_r3/`): las 5 coherencias OK en HTML, PDF y Moodle**,
  con un verificador propio que no usa las funciones del `.Rmd` (N = 100: 106 comprobaciones por versión, 0 fallos; 2555
  ecuaciones; Moodle 35/35 versiones y 210 gaps; 20 mutaciones de control detectadas). **Sin sellar**: el orden exige el
  paso 7 antes. Dos BAJA:
  - el DOCX deja visibles los `##ANSWERi##` (comportamiento de `exams2pandoc` con cloze; ya declarado: el DOCX no es formato
    de entrega del CLOZE; para papel, el SCHOICE);
  - `expl2["moda"]` dice «Eso describe la moda, el valor más repetido» de «Lo que recogió la mayoría de sus cursos»: lo que
    recoge la mayoría es siempre la moda, pero la moda puede no ser mayoritaria. Precisión de la Solution, no CORRECCIÓN;
    se puede afinar en la misma reapertura que decida el profesor.
- **Corrección de una nota sellada:** la nota del paso 5 dice «PDF de la semilla 2»; `salida/revision_pdf_r3/c2-*.png` son
  páginas de `cloze2.pdf`, que es la **semilla 3** (CxC). La revisión a ojo vale para esa semilla.
- **Medido, sin sellar** (el orden exige la FASE 2C antes):
  - Paso 9: `salida/diversidad_sustantiva_r3.txt` (exit 0; p2 6/100 = fija-justificada), `salida/diagnosticidad_por_rama_r3.txt`
    y `salida/diagnosticidad_r3.txt`: **exit 1, `ERR_DIAG_SUPERFICIAL` en p6** (H3: en las 17 versiones con una sola
    verdadera, la verdadera empieza por «Si»). La Parte 6 es mchoice con «Puede haber más de una»; esa única verdadera es
    siempre una hipótesis porque «ventaja» y «Ganó…» llevan el mismo veredicto (L-C1, decisión 1 a), y H2 da 0 %. Pendiente
    de juicio del detractor y, si es canal, del profesor: no se tocó la Parte 6.
  - Paso 10: V1 a V4 OK (`salida/v1_v4_r3.txt`); literalidad de Afirmación, Evidencia, Descriptor y Estándar contra el
    catálogo canónico, OK; DOK 2, Bloom «Evaluar», Nivel 3.
  - Stress test visual N = 100 (`salida/stress_n100_r3/`): 0 anomalías. Suites del repo que barren los `.Rmd`: 6/6 en verde
    (`salida/suites_r3.txt`).

## Tanda 5 (2026-10-31): L-C5 cableada; las cifras exactas con el código superan lo firmado en 22 celdas

**Resultado: detenida en el paso A, como mandaba su orden** («si la cifra exacta ejecutando el código difiere de la
firmada en alguna regla, detente y trae las dos cifras»). No se reabrió el `.Rmd` ni se gastó la pasada 2 de §P7-D.
Registro y tabla de las 22 celdas: `../.claude/CLAUDE.md` § «Tanda 5».

- **Puerta de L-C5:** `enumerar_h2_cloze.R`. Enumeración exacta de las Partes 2 a 6 conducida por el código, para
  cualquier Parte 1: ramifica `rama_par`, `orden_h` (la cascada F1 corre dentro), `v_c` y `c6`; integra la fila del caso
  paralelo y la fila del ganador; cuenta las llamadas de cada sorteo libre. En E8, sobre las 100 Partes 1 de la muestra
  estándar (3200 hojas, masa con error 0, 15 892 filas únicas, 45 s). `FIRMA_H2` (69 celdas) y `h2_juzgar()` deciden
  «cubierto» o «pendiente» por celda.
- **Causa de la diferencia:** V0 (lo firmado) no aplica las exclusiones de V11, y el código sí. La ponderación es la del
  código: `salida/contrastar_h2_ponderacion.R` compara, semilla por semilla, la puerta con el modelo transcrito de
  `medir_reparto_h2.R` con exclusiones y cascada (diferencia máxima de peso 1,1·10⁻¹⁹). Ruido de la Parte 1 (semillas 1 a
  50 frente a 51 a 100): semidiferencia mediana 0,3 pp, máxima 1,4 pp (`salida/h2_contraste_96cfcf35.txt`).
- **Cifras** (`salida/h2_exacta_96cfcf35.txt`): 53 de las 69 celdas cambian a un decimal; 22 suben (las mayores: N1 en la
  mezcla, de +14,1 a +14,8; D2 en la rama PROMEDIO, de +31,7 a +32,5; D2 con *k*′ = 1, de +32,1 a +32,8; A1 en la mezcla, de
  +13,6 a +14,2); 2 bajan a +8 pp o menos (A1c en la rama CURSOS, de +8,9 a +7,0); ninguna celda no firmada pasa de +8 pp.
- Un primer contraste desactivaba las exclusiones con las sustituciones de MC1 y MC2: las guardas V11 del propio `.Rmd`
  abortan entonces 2/3 de las hojas, así que no valía y se retiró.
- **§P7-G:** las dos llamadas de `evaluar_bateria()` de E4 declaran `estimulo_no_aplicable` (los canales del enunciado y
  de la tabla de la Parte 1 se miden en las sondas de E4 y en la canónica regla por regla). Ya no imprimen `SIN_COBERTURA`:
  mezcla PASS (+0,3 pp), rama CURSOS PASS (+0,7 pp), rama PROMEDIO BLOQUEA (+10,4 pp, el canal de largo declarado en «O-1
  corregido»), las mismas cifras que en la tanda 4. La batería L-C4 ya lleva reglas del estímulo propias.
- **La Parte 6 en la muestra de E8** queda solo para las reglas del orden de las afirmaciones (P6-P2), que L-C5 no ramifica.
  Las relacionales RL1 y RL2 pasan a la puerta exacta.

## Tanda 4 (2026-10-31): batería L-C4 CONGELADA antes de medir el reparto (§P7-C)

**Resultado: el reparto autorizado no es viable (paso B). La tanda se detuvo antes de reabrir; no se gastó la pasada 2
de §P7-D.** Medido por enumeración exacta del espacio de `espacio_paralelo()` (`salida/medir_reparto_h2.R`,
`salida/reparto_h2_96cfcf35.txt`; las reglas de orden de las afirmaciones son nulas por construcción y no se evalúan ahí):

| Regla (Parte 6, ventaja y ganador salvo indicación) | Diseño actual (V0) | Reparto autorizado (V1), mezcla | V1, *k*′ = 1 | V1, *k*′ = 2 |
|---|---|---|---|---|
| R_k (H2) | +33,3 | +0,0 | +0,0 | +0,0 |
| R_hg (H2), HG | +22,5 | +0,0 | +0,0 | +0,0 |
| R_δ (H2, δ′ ≤ 4) | +6,3 (aplica 13 %) | +0,0 (aplica 0 %) | — | — |
| δ′ ≥ 10 ⇒ ganó el de mayor promedio | +24,0 | **+36,5** | **+39,1** | **+33,9** |
| el grado de menos cursos tiene 5 o más ⇒ mayor promedio | +14,1 | **+28,6** | **+35,7** | **+21,6** |
| el menor promedio es de 40 o más ⇒ más cursos | +13,6 | **+28,6** | **+32,5** | **+24,7** |
| hipótesis por esos rasgos (máximo, HP) | +18,0 | **+25,8** | **+39,1** | +21,2 (HG) |
| RL1, par HP = HG (métrica del par) | +8,3 | +16,7 | **+25,0** | +8,3 |
| RL2, hipótesis contrarias a «Ganó…» (condicional al ganador) | +33,3 | +41,7 | +50,0 | +33,3 |

- δ′ ≥ 10 en la rama CURSOS 20,9 % y en la rama PROMEDIO 93,9 % (el costo anotado en la tanda 3). HG y HP dado *k*′: 50,0 %
  en los dos estratos. Por rama, R_hg da +33,3 pp con signos opuestos (la rama no es observable; en la mezcla, 0).
- Cascada F1 de V1, con la Parte 1 real (semillas 1 a 100): 2,50 versiones de cada 100 sin candidatos en el espacio
  base y 0,50 tampoco en el ampliado (*n*′ ≤ 10).
- **Por qué no hay arreglo dentro del reparto autorizado.** En *k*′ = 1, CURSOS con HG y HP verdaderas exige
  *a*′ ≥ *n*′·δ′ + 6 (*D*′ pequeño) y PROMEDIO con las dos falsas, 2·*a*′ < (*n*′ − 1)·δ′ (*D*′ grande): para cada δ′, sus
  cursos no se solapan (tabla de soporte al principio del archivo de salida). V2, con δ′ equilibrado dentro de *k*′:
  «5 o más cursos» +47,2 pp y «promedio de 40 o más» +42,1 pp; además, en *k*′ = 2 las seis celdas no comparten ningún
  valor de δ′. V3, con los cursos equilibrados: δ′ ≥ 10 +44,8 pp y «promedio de 40 o más» +38,6 pp. En el fondo,
  «hipótesis falsas» exige *D*′ grande, y *D*′ grande hace evidente al ganador por las magnitudes de la tabla.

- Archivo: `bateria_partes_2_a_6.R`, versión «L-C4 v1 (2026-10-31)», **md5 `de20e0a986f86d44f0d3412517d2f07b`**.
  Congelada a las 2026-10-31, tras la línea base y antes de cualquier medida del reparto de H2. Cambiar una regla es
  hacer una batería nueva y volver a medir todo el histórico.
- Línea base sobre el md5 `96cfcf35…` (`salida/bateria_linea_base_96cfcf35.txt`, `salida/medir_bateria_cloze.R`):
  las sondas de H1 y H2 salen en rojo, así que la batería las ve. Parte 2 exacta: P2-L1 «contiene “total”» 100,0 %
  en «comparar» (+75,0 pp) y 70,0 % en «promedio» (+45,0 pp); P2-L2 70,0 % y P2-L3 75,0 % en «promedio». Parte 6,
  N = 100: R_k +31,3 pp (mezcla), R_hg +24,0 pp (mezcla), R_δ +12,8 pp (rama CURSOS).

## Firmado en la tanda 3: X6, X10 y X11 en la canónica (L-C3 ampliada)

La tanda 2 se detuvo porque, al cablear la decisión 3.2 a regla por regla, salieron tres reglas cruzadas más por
encima de +8 pp en la canónica. El profesor las firmó el 2026-10-30 («a · aclaración sí»; registro en
`../.claude/CLAUDE.md` § «Decisión del profesor tras la tanda 2»).

| Regla (canónica) | Exacta | Muestra n = 30 (tanda 2, solo registro) | Peso en la mezcla | Mutante |
|---|---|---|---|---|
| X6 más palabras en común con la clave de la Parte 2 | 41,7 % (+16,7 pp) | 33,3 % (+8,3 pp) | +1,67 pp | MC19 |
| X9 S_U + la que nombra la ventaja del ganador | 75,0 % (+25,0 pp) | 75,0 % (+25,0 pp) | +2,50 pp | MC18 |
| X10 S_U + más palabras en común con la clave de la Parte 2 | 66,7 % (+16,7 pp) | 61,7 % (+11,7 pp) | +1,67 pp | MC20 |
| X11 S_U + más palabras en común con las verdaderas de la Parte 6 | 62,5 % (+12,5 pp) | 66,7 % (+16,7 pp) | +1,25 pp | MC21 |

- **Puerta nueva (tanda 3):** `enumerar_canonica_cloze.R`, enumeración exacta **conducida por el código**: sustituye
  `sample()`/`sample.int()` en el entorno del `.Rmd`, ramifica los seis sorteos que deciden las reglas (192 hojas) e
  integra exactamente la fila del caso paralelo (X1 a X4) y los distractores de la Parte 2 (X5). Una transcripción
  del modelo no ve un mutante que cambie un sorteo: por eso X9 no tenía mutante posible con `x_canonica_exacta.R`.
- Las once reglas, exactas en la canónica: X1 +0,0, X2 +0,0, X3 −1,0, X4 +0,3, X5 −25,0, X6 +16,7, X7 −16,7,
  X8 +0,0, X9 +25,0, X10 +16,7 y X11 +12,5 pp. Fuera de lo firmado, ninguna pasa de +8 pp (la tanda 2 tenía X3 en
  +5,0 pp con la muestra).
- X6 a X11 coinciden con la transcripción de `salida/x_canonica_exacta.R`, y no cambian entre las semillas canónicas
  1001 y 1028 (otro orden de las opciones de la Parte 1).

## FASE 2C r1 (2026-10-31): `RECHAZAR`

`AgenteDetractor` nuevo, lanzado sin `name:`, sobre el md5 `96cfcf35…`. Reporte íntegro, recuperado de su
transcripción: `salida/fase2c_r1_detractor.md`. Citas: `verificar_citas_detractor.py` → 14 citas, 0 problemas.
Sin ningún defecto de CORRECCIÓN: 0 discrepancias de 1500 semillas contra su oráculo propio en las Partes 3 a 6.

| ID | Severidad · tipo | Qué | Estado |
|---|---|---|---|
| H1 | ALTA · diagnosticidad | Parte 2, canal léxico: «contiene “total”» acierta 100 % en «comparar» y 70 % en «promedio» (enumeración exacta; +60 pp con los dos tipos). Las Partes 3 y 4 dicen «en total». En «promedio», además, «descarta “Lo que recogió …”» 70 % y el condicional en -ría 50 %. Verificado aparte leyendo los textos | **Abierto.** Se cierra con redacción (permitido sin consulta), pero se agrupa con H2 y H3 en la pasada 2 de §P7-D |
| H2 | ALTA · diagnosticidad | Tabla del otro colegio: *k*′ predice quién ganó (R_k 80,0 %, N = 100; canónica 82,5 % exacta) y el veredicto de HG (74,0 %); δ′ ≤ 4 ⇒ CURSOS (14 de 14). Decide 2 o 3 de las 4 marcas de la Parte 6. Verificado aparte: `salida/p7f_h2_espacio_96cfcf35.txt` | **Abierto, al profesor** (no es redacción; ver abajo) |
| H3 | BAJA · otro | Encabezado fijo «Parte 2 · Lecturas del promedio que no sirven» también en el tipo «comparar» | Abierto (redacción de la Solution propia) |
| H4 | BAJA · robustez de la puerta L-C3 | `enumerar_canonica_cloze.R` reconocía los sorteos libres solo por su texto | **Corregido** (verificador, no el `.Rmd`): cada hoja cuenta las llamadas libres; mutante **MC22** (v_c con el texto de un sorteo libre) cazado por su sonda. `verificar_espacio_cloze_r3b.txt`: exit 4 |

## Pendiente de decisión del profesor (por esto se detuvo la tanda 3) — RESUELTO

> Lo decidió el profesor el 2026-10-31: «1 a · 2 a · i ii iii» tras la tanda 3 y «H2 a · 1 no, 2 sí» tras la tanda 4
> (`../.claude/CLAUDE.md`). Se conserva como registro.

1. **H2.** Cerrarlo exige cambiar cómo se sortea la fila del otro colegio (`data_generation` de las Partes 2 a 6),
   no solo la redacción. Enumeración previa (§P7-F, `salida/p7f_h2_espacio.R`):
   - **Estructural:** en CURSOS con *k*′ = 1 la HG es siempre verdadera (0 filas con HG falsa); en PROMEDIO casi
     no hay δ′ ≤ 4 (1 de 976 en el espacio base) ni *k*′ = 3 (6 de 976).
   - **Reparto candidato:** *k*′ ∈ {1, 2}; *k*′ = 1 solo en CURSOS (HG=V, HP=V) y en PROMEDIO (HG=F, HP=F), con la misma
     masa; δ′ ≥ 5 en las dos ramas. Equilibra rama, HG y HP dado *k*′ (cálculo de masas en el reporte del
     orquestador). **Costo:** con *k*′ = 1 las dos hipótesis llevan siempre el mismo veredicto (regla relacional
     nueva) y δ′ ≥ 10 queda más frecuente en PROMEDIO. Hay que medirlo con una batería congelada antes de elegir.
   - Opciones: (a) rediseñar el sorteo (pasada 2 de §P7-D) con esa batería; (b) firmar H2 como override con sus
     cifras (R_k 80,0 % N = 100 y 82,5 % canónica; R_hg 74,0 %; R_δ 14/14); (c) otra que proponga el profesor.
2. **Canónica de la Parte 1, regla por regla** (al aplicar la aclaración firmada el 2026-10-30). «O-1 corregido»
   firmó esa canónica como un solo número de estrato, el máximo de la batería (0,10 × 75 pp). Con la aclaración, ese
   máximo no cubre las demás reglas. Exacto, 24 órdenes (`salida/p1_canonica_exacta_96cfcf35.txt`): **12 de las 48
   reglas** de la batería §P7 del SCHOICE pasan de +8 pp (3 a +75,0; 5 a +25,0; 4 a +8,3) y **12 sondas** de E4
   (S5, S7, S_U + S1, S_U + S1 + suma, S_U + atípica y S_U + las tres a +75,0; S1, S2, S3, S_U y S_U + suma a +25,0;
   Atípica a +8,3). Fuera de la batería (§P7-C, no obliga en este ciclo): «descarta la más larga», +8,3 pp. Es
   herencia del SCHOICE aprobado (E1), con el texto verbatim (H-2): no se corrige en el CLOZE. Toca también al SCHOICE.
3. **H1 y H3** se corrigen con redacción en la misma pasada que H2, si el profesor elige (a). Mover los textos de la
   Parte 2 mueve X5, X6 y X10 en la canónica: si X6 o X10 pasan de +16,7 pp, vuelve al profesor (L-C3).

## Qué es

Versión CLOZE de la pregunta impresa 50 del simulacro SAI «Nivelación 6» de grado 10 (MAT-2026-07-050,
Argumentación, D3.10, Nivel 3, DOK 2). Seis partes, `schoice|schoice|num|num|num|mchoice`, pesos `3|1|1|1|1|1`:

| Parte | Tipo | Qué pide | Peso |
|---|---|---|---|
| 1 | schoice | La pregunta oficial. Por código es el SCHOICE de `..`: misma generación, mismo RNG (E1) | 3 |
| 2 | schoice | Qué indica el promedio por curso, o qué se compara para decidir quién gana (clave conceptual fija, 3 paráfrasis por tipo) | 1 |
| 3 | num | Total de un grado del otro colegio (orden de mención sorteado) | 1 |
| 4 | num | Total del otro grado | 1 |
| 5 | num | Cuántos kilos más recogió el grado que ganó, sin nombrarlo | 1 |
| 6 | mchoice | HP, HG, una de ventaja (4 redacciones) y «Ganó el concurso el grado X» (aprobada tal cual, decisión 1 a) | 1 |

Las Partes 2 a 6 trabajan sobre **otro colegio** (concurso entre dos grados, de sexto a noveno), con una tabla
propia. Motivo: la clave de la Parte 1 nombra a la sede ganadora y cita *D* = *T*_G − *T*_P; pedir *D*, *T*_G o
*T*_P, o nombrar a la ganadora, la delataría (Incidente Y, V11).

## Firmas y decisiones del profesor (resumen; texto completo en `../.claude/CLAUDE.md`)

- **F-1 a F-4 (2026-10-30, dry-run):** Flujo B = n; «O-1 corregido» extendido a la Parte 1; pesos `3|1|1|1|1|1`;
  estructura Opción A. Nota que completa F-4: «No nombran sedes de la Parte 1. Al menos una verdadera y una
  falsa, sin pares complementarios, y el veredicto se sortea primero».
- **Tras la tanda 1 (2026-10-30), «1 a · 2 a · 3.1 a · 3.2 a · 4 sí»:** la Parte 6 queda tal cual; los BAJA 1, 2 y
  4 de la 6b (Parte 1) son residuo y sus espacios duros van al próximo ciclo del SCHOICE; un canal cruzado nacido
  de la redacción nueva se cierra endureciéndola, sin parar, y solo vuelve al profesor si queda abierto; X9 en la
  canónica firmada (+25,0 pp); `render_html_shots.R` se arregla en otra tarea (no usarlo para 320 o 360 px). Quedó
  arreglado en `afac46a1`, y la premisa resultó falsa: ver «Cómo repetir cada medición».

## Leyes del CLOZE y sus guardas (tandas 2 a 5)

| Ley | Guarda | Mutante → sonda (E6 de `verificar_espacio_cloze.R`) |
|---|---|---|
| L-C1 · Parte 6 | `stopifnot` «Parte 6: HP, HG, una de ventaja y la del ganador, nada más» (nuevo); «sin pares complementarios»; «al menos una verdadera y una falsa»; V10; veredictos 50 % ± 12 | MC16 (nuevo: una conceptual en lugar de la del ganador), MC11 (`v_d` sorteado aparte), MC5, MC6, MC13: todos cazados por su sonda |
| L-C2 · canales cruzados | E3: `veredicto_cruzado()` deja PENDIENTE DE FIRMA (exit 3) todo canal > +8 pp; E1; V11 por valor | MC17 (nuevo: la rama del otro colegio copia la de la Parte 1; X8 +25,0 pp, n = 181 → pendiente), MC14, MC15, MC1, MC2 |
| L-C4 · batería congelada por gap (tanda 4) | E8: md5 de `bateria_partes_2_a_6.R`; Parte 2 exacta (posición en muestra), Parte 6 por estrato con el p95 del máximo permutado; > +8 pp = PENDIENTE | MC23 (clave de la Parte 2 siempre primera: P2-P1 en rojo), MC24 (ganador siempre en la fila 1: P6-P1 en rojo) |
| L-C5 · H2 en la Parte 6, regla por regla y por estrato («H2 a», tanda 5) | E8: `FIRMA_H2` (69 celdas) y `h2_juzgar()` sobre la enumeración exacta de `enumerar_h2_cloze.R` (100 Partes 1, con las exclusiones de V11); dentro, «cubierto» (exit 4); por encima o fuera de la firma, pendiente | MC25 (PROMEDIO con *k*′ = 1: K1 +50,0 pp), MC26 (CURSOS con δ′ < 10: D2), MC27 (`v_c <- !v_hp`: RL2 · HP); las demás celdas, sin mutante propio |
| L-C3 ampliada · canónica de la Parte 1 regla por regla (decisión 2 a, tanda 4) | E4: `FIRMA_P1_CANON` (12 reglas) y `FIRMA_P1_SONDAS` (12 sondas), exactas; dentro, «cubierto» (exit 4) | MP1 («quedarse con 'promedio'» a +75,0), MP2 («más dígitos» a +75,0), sobre copias del registro canónico; las nueve al 100 % no pueden subir |
| L-C3 · X6, X9, X10 y X11 en la canónica (ampliada en la tanda 3) | `FIRMA_CANON` (+16,7, +25,0, +16,7 y +12,5 pp, exactas); `veredicto_canon()` sobre la enumeración exacta de `enumerar_canonica_cloze.R`, las once reglas: dentro, «ROJO, cubierto» (exit 4, nunca PASS); por encima, o cualquier otra regla > +8 pp, pendiente | MC18 (X9 +50,0), MC19 (X6 +25,0), MC20 (X10 +25,0), MC21 (X11 +25,0): cada uno saca su regla de «cubierto» |

## Tanda 2 (2026-10-30): qué cambió en el `.Rmd` (solo tras el marcador del bloque P1)

1. **BAJA 3 de la 6b:** en «Respuestas · Parte 6» cada afirmación va en su renglón, agrupadas por veredicto
   (agrupar no afirma nada sobre posiciones, Incidente Q).
2. **Espacio duro** (`ED <- " "`) entre cifra y unidad en el texto propio de las Partes 3 a 6, en su Solution y
   en su retroalimentación. **No** en la Parte 1 ni en sus párrafos de la Solution, que siguen iguales al SCHOICE
   (decisión 2 a). **No** en las celdas de la tabla del otro colegio, que siguen como la tabla de la Parte 1 (las
   dos medidas en la caja `.qtext` del Moodle del colegio, 0/100 fuera de su caja; 0 cortes dentro de las tablas).
3. **Guarda de composición de la Parte 6** (L-C1), antes de las otras guardas de la Parte 6.

## Evidencia (`salida/`)

| Qué | Archivo | Resultado |
|---|---|---|
| Verificador, tanda 5 | `verificar_espacio_cloze_r5.txt` (386 s) | exit 3: 34 pendientes (22 de L-C5, las celdas de «H2 a» por encima de lo firmado; 12 de la Parte 2, H1); 86 cifras «cubiertas» (13 de «O-1 corregido», 4 de L-C3, 24 de la decisión 2 a y 45 de L-C5); 8 residuos; 0 fallos; MC25, MC26 y MC27 (L-C5), MC23, MC24, MP1 y MP2 cazados por su sonda; E4 sin `SIN_COBERTURA` (§P7-G) |
| L-C5, cifras exactas y contrastes | `h2_exacta_96cfcf35.txt` (`medir_h2_exacta.R`), `h2_contraste_96cfcf35.txt` (`contrastar_h2_ponderacion.R`) | 22 de 69 celdas por encima de lo firmado; ponderación del código = transcrita con exclusiones (1,1·10⁻¹⁹); ruido de la Parte 1 0,3 pp de mediana |
| Verificador, tanda 4 | `verificar_espacio_cloze_r4b.txt` | exit 3: 65 pendientes, todos de L-C4 (H1, H2 y los demás canales de la batería; 22 más quedan NO CONCLUYENTES); 41 cifras «cubiertas» (13 de «O-1 corregido», 4 de L-C3 y 24 de la decisión 2 a); 0 fallos; MC23, MC24, MP1 y MP2 cazados por su sonda |
| Batería L-C4, línea base | `bateria_linea_base_96cfcf35.txt`, `medir_bateria_cloze.R` | H1 y H2 en rojo; además, en la Parte 2 «comparar»: P2-E4 +55,0, P2-E2 +45,0, P2-E1 +41,7, P2-L4 y P2-L6 +12,5, P2-F3 +11,1, P2-F4 +9,2 pp (exacta) |
| Reparto de H2 (paso B) | `reparto_h2_96cfcf35.txt`, `medir_reparto_h2.R` | V1 no viable (tabla de arriba) |
| FASE 2C r1 | `fase2c_r1_detractor.md` | `RECHAZAR`; H1, H2 ALTA (diagnosticidad), H3, H4 BAJA; 0 CORRECCIÓN; citas 14/0 |
| Verificador tras H4 | `verificar_espacio_cloze_r3b.txt` (71 s) | exit 4, igual que la r3 más MC22 cazado por su sonda (22 mutantes en E6) |
| Canónica de la Parte 1 regla por regla | `p1_canonica_exacta.R`, `p1_canonica_exacta_96cfcf35.txt` | 12 de 48 reglas y 12 sondas por encima de +8 pp (exacto, 24 órdenes) |
| §P7-F de H2 | `p7f_h2_espacio.R`, `p7f_h2_espacio_96cfcf35.txt` | R_k 80,0 %, R_hg 74,0 %, R_δ 14/14 (N = 100); filas viables por rama, combinación, *k*′ y δ′ |
| Paso 9 (preparado, sin medición oficial) | `diagnosticidad_por_rama.R` | V8 y V9 por gap y por estrato, N = 100; se corre cuando se selle el paso 8 |
| Verificador propio, tanda 3 | `verificar_espacio_cloze_r3.txt` (68 s) | **exit 4: EN ROJO BAJO OVERRIDE FIRMADO (17 cifras: 13 de «O-1 corregido» y 4 de L-C3; 0 fallos, 0 pendientes)**. E3 de la canónica por enumeración exacta (192 hojas, masa 1, sin error, invariante entre semillas, igual a la transcripción). E6: 21 mutantes cazados por su sonda (MC18 a MC21 por la enumeración). El resto, igual que la tanda 2 |
| Verificador propio, tanda 2 | `verificar_espacio_cloze_r2.txt` (41 s) | **exit 3: PENDIENTE DE FIRMA (3; 0 fallos)**. E1 111/111 (20 canónicas). E2 330/330 en todos los gaps. E3: V11 por valor, contenido e independencia OK; batería cruzada ≤ +8 pp en la mezcla y por rama (tablas idénticas a la tanda 1); en la canónica, X9 cubierta y X6, X10, X11 pendientes. E4 = SCHOICE (13 cifras dentro de «O-1 corregido»). E5 300/300 únicas. E6: 18 mutantes cazados por su sonda. E7 4/4 |
| Enumeración exacta de la canónica | `x_canonica_exacta.R`, `x_canonica_exacta_e58f3df9.txt` | X6 +16,7, X7 −16,7, X8 0,0, X9 +25,0, X10 +16,7, X11 +12,5 pp (96 casos ponderados). Las Partes 2 a 6 no cambiaron su RNG en la tanda 2: la cifra vale para el md5 nuevo |
| Render, tanda 2 | `render_r2.log`, `html/`, `pdf/`, `tex/`, `docx/`, `moodle/` | HTML, PDF, DOCX y Moodle OK; NOPS N/A (`cloze exercises`); `\pandocbounded` solo como `\providecommand`; `\LTcaptype{none}` con guarda; 0 imágenes en gaps; 0 `##ANSWER` en el HTML |
| PDF revisado a ojo | `revision_pdf_r2/c1-*.png` | Canónica, 5 páginas: la lista de Respuestas, anidada y dentro del margen. Queda «20 / kilos.» en el párrafo de la Parte 1 (igual que el SCHOICE: residuo 2 a) |
| Arsenal (hook v6.3) | `~/.cache/post-exams2-validation/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1/20261030-231556-1522539/informe.txt` | 0 errores, 7 advertencias (2B sin PDF en el comando del hook; 2E «Tarea»; 2F tabla de metadatos, descripción de evaluación, análisis de distractores, patrón «Es posible que los estudiantes», `letra_correcta`); 2G 100/100; 2H 10 semillas, 0 anomalías |
| Moodle del aula | `exportar_r2.log`, `moodle_aula/` | 30/30 con pesos `3|1|1|1|1|1`; 60 MULTICHOICE_V, 30 MULTIRESPONSE, 90 NUMERICAL, 0 desplegables; 0 etiquetas en 180 gaps |
| Cortes cifra-unidad por CDP, pareado con la 6b | `r2_cdp/6b/medir_cortes_r2.txt`, `metricas_extra.json` (N = 24, las semillas de la 6b) | En la Solution, de 132/189/94/128 a 73/62/62/71 cortes (320/360 px y cajas de 270/310 px). Los que quedan llevan espacio normal: todos en texto de la Parte 1. Con espacio duro, 0. En opciones y enunciado, 21/8/14/24, sin cambio (Parte 1) |
| Captura CDP | `r2_cdp/6b/capturar_r2.txt`, `metricas.json`, `strips/` (N = 24 × 5 modos) | `innerWidth` = 320, 360, 1024, 320 y 360 en los cinco modos; 0 desbordes, 0 fugas de markup, ancho del documento ≤ viewport en 120/120 |
| Tanda 1 | `verificar_espacio_cloze_r1_*.txt`, `render_r1.log`, `6b/`, `stress_n100/`, `qtext/` | Ver el historial de este archivo en la tanda 1: 6b APTO_CON_OBSERVACIONES (4 BAJA), stress test N = 100 PASA, `.qtext` 0/100 fuera de su caja |

## Residuos declarados

- **Firmados (F-2), en la Parte 1 por rama:** los de «O-1 corregido», iguales a lo firmado (13 cifras; ver
  `verificar_espacio_cloze_r2.txt`). Canónica: 0,10 × 75 pp = +7,5 pp.
- **Firmados (L-C3):** en la canónica, X6 +16,7, X9 +25,0, X10 +16,7 y X11 +12,5 pp, exactas (pesos en la mezcla
  +1,67, +2,50, +1,67 y +1,25 pp). Cubiertos por dominancia: el atajo de «O-1 corregido» acierta el 100 % ahí.
- **Decisión 2 a (Parte 1):** «15 / kilos.» partido en opciones y Solution; «El / Bosque» y «La / Pradera» fuera
  de la tabla (semillas 6 y 13); «ganará»/«ganó» en 8 de 24 contextos. También los cortes cifra-unidad que quedan
  en los párrafos de la Parte 1 de la Solution, que son iguales a los del SCHOICE.
- **Cascada F1:** 1 de 330 versiones usó el paso 2 (espacio ampliado); ninguna, el 3 o el 4.
- **V11 «a un paso»:** *D* sale de una operación entre dos números de la tabla del otro colegio en el 11-17 % de las
  versiones (por rama); alguna respuesta de las Partes 3 a 5 sale de una operación entre dos números de la Parte 1
  en el 33-63 %. Coincidencias sin relación natural; las relaciones naturales están excluidas (0/330); X3 da
  23-24 %, por debajo del azar.
- **Parte 2:** clave conceptual fija por tipo (V8, fija-justificada): 6 textos de clave.
- **La vara §P7** está medida sobre opción múltiple simple; aplicarla gap a gap no está calibrado para CLOZE.

## Uso en el aula (cuando esté aprobado)

- **Moodle**, desde `cloze/salida`: `Rscript --no-init-file ../exportar_moodle_cloze.R [n]` (n de 1 a 100; 30 por
  defecto). La pregunta vale 8 puntos.
- **DOCX: no apto para estudiantes** (deja los `##ANSWERi##` a la vista). **Para papel, el SCHOICE.** NOPS es N/A.

## Cómo repetir cada medición (siempre desde `cloze/salida`)

```bash
cd A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida
Rscript --no-init-file ../verificar_espacio_cloze.R > verificar_espacio_cloze.txt; echo $?   # E1-E7; 4 = rojo cubierto, 3 = pendiente
Rscript --no-init-file x_canonica_exacta.R                                                   # contraste: modelo transcrito (X6-X11)
Rscript --no-init-file medir_h2_exacta.R > h2_exacta.txt                                     # L-C5: H2 exacta con el código, 100 Partes 1 (45 s)
Rscript --no-init-file contrastar_h2_ponderacion.R > h2_contraste.txt                        # L-C5: ponderación y ruido de la Parte 1 (80 s)
Rscript --no-init-file ../render_cloze.R                                                      # 4 formatos + NOPS N/A + .tex
Rscript --no-init-file ../exportar_moodle_cloze.R 30                                          # Moodle del aula + guardas
cd r2_cdp/6b && Rscript --no-init-file render_6b.R && python3 medir_cortes_6b.py ./html . ./fonts.conf <tmp> \
  && python3 capturar_cdp_6b.py ./html . ./fonts.conf 0 <tmp>                                 # CDP real: sin -I (usa websockets del usuario)
```

El hook v6.3 se dispara solo con un `exams2*("<ruta>.Rmd")` literal en el comando (`render_cloze.R` no lo dispara).
`render_html_shots.R` captura por CDP desde `afac46a1` y comprueba que la página mida el ancho pedido. Corrección del
2026-10-31: las capturas viejas de 320 y 360 px también eran válidas en lo que es CSS, porque `--screenshot` vuelve a
maquetar al ancho pedido. El 500 solo lo veían el JavaScript de carga y `--dump-dom`.

**Trampas:** nada de comillas invertidas dentro de los chunks; el bloque P1 no se edita a mano (se re-empalma con
`empalmar_bloque_p1.R` desde el SCHOICE). Un cambio en la Parte 1 exige al profesor. El Edit de las herramientas
convierte un ` ` escrito en el texto en el carácter literal: comprobar con `grep -nP '\xa0'`.

## Próximo paso

1. **Decisión del profesor sobre H1 y H2 de la FASE 2C r2** (regla de parada de §P7-D: el arreglo de H1 de la r1 cerró la batería
   y abrió el mismo canal con otra palabra):
   - (a) gastar la **última** pasada de §P7-D en la Parte 2: «dividido entre» en 4 o 5 distractores «promedio», con operandos
     equivocados y siempre falsos (propuesta del detractor, con su `expl2`), y «promedio» sin «por curso» en los distractores
     «comparar»; volver a medir L-C4, las sondas de H1 y H2, las cruzadas y la canónica; detractor nuevo;
   - (b) firmar H1 (+75,0 pp en «promedio») y H2 (+25,0 pp en «comparar») como residuo (§P7-E), con sus cifras exactas;
   - (c) otra.
2. Sellar 7 a 10 según la decisión (el paso 8 ya tiene artefacto: `salida/coherencias_r3/`); H4 en la nota del paso 9.
3. WAIT_USER #3 solo con la FASE 2C cerrada.
