# Auditoría del detractor, FASE 2C: `vueltas-llanta-perimetro-n4` (SCHOICE)

**Fecha**: 2026-11-10
**Artefacto**: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd`. Comprobé en disco que su md5 es `3d8dc37993f6bd298bf8b788636351fb`.
**Independencia**: soy un detractor nuevo. No escribí ni corregí este artefacto.
**Objeciones**: 5 en total.
- Ninguna de CORRECCIÓN.
- 1 ALTA de DIAGNOSTICIDAD.
- 3 MEDIAS: dos sobre la redacción de la Solution y una sobre la evidencia del paso 9.
- 1 BAJA.

## Resumen

La clave es verdadera en todas las versiones y en todas las formas de escritura. No hay una segunda expresión equivalente a la clave y ningún distractor es correcto. La versión canónica reproduce el impreso con la C oficial. Lo verifiqué ejecutando el chunk `data_generation` real en 1000 semillas, con un oráculo propio que **lee el enunciado tal como se muestra**, no las variables internas.

El problema serio es de diagnosticidad. Cada distractor «completo» (VL-B, VL-F, VL-G) es la clave con **un solo cambio**: la fracción invertida, ÷ en lugar de ×, o un factor K de más. Como el `.Rmd` obliga a que haya al menos dos de ellos, la clave queda en el centro del conjunto de opciones. Elegir «la opción de la que las demás parecen variaciones» acierta así:
- 81,0 % de las veces con N = 100, frente a un techo nulo de 27,3 %;
- 94,2 % de las versiones no canónicas.

El comentario de diseño de las líneas 15-17 afirma justo lo contrario. Además hay dos frases de la Solution que conviene reescribir y una evidencia del paso 9 que se midió sobre otro md5.

## 1. CORRECCIÓN: lo que verifiqué ejecutando

**Método**
- Extraje los chunks `data_generation` y `solucion` y los evalué con `set.seed(s)` para s = 1..1000, en entornos nuevos y con `Rscript --no-init-file`.
- El oráculo lee del texto renderizado tres cosas: la constante de la regla, la unidad de la distancia y del perímetro, y la distancia y la letra de la pregunta.
- Con eso calcula la verdad, (conversión real × d)/P. La conversión real es 100 para cm/m, 1000 para mm/m y 1000 para m/km.
- Después evalúa cada opción LaTeX en P = 37, 211 y 1234,5.
- Scripts: `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0e74dd34-847c-4a81-ab53-a25af8b1f7ff/scratchpad/audit2.R` (salida: `.../scratchpad/out2.txt`).

**Resultados (1000 semillas, 110 de ellas canónicas, es decir 11,0 %, coherente con P_CANON = 0,10)**

| Comprobación | Resultado |
|---|---|
| Errores al evaluar `data_generation` | 0 |
| Exactamente una opción equivalente a la verdad, y es la marcada | 1000/1000 |
| La constante del enunciado coincide con la conversión real de las unidades sorteadas | 1000/1000 |
| La unidad de la pregunta coincide con la de la regla (cm, mm, m; metros, kilómetros) | 1000/1000 |
| «Caso específico»: aritmética, unidad y «da lo mismo» | 0 discrepancias |
| «equivale a K·d/V», con la letra P o p de la versión | 0 discrepancias |
| Cifras del feedback de cada opción frente a mi evaluación, a la precisión mostrada | 0 discrepancias |
| Cada distractor mostrado aparece descrito en la Solution con su expresión exacta | 1000/1000 |
| Letras de opción en la Solution (regla #19) | 0 |
| Canónica: enunciado y opciones de la Guía con la C oficial; nunca «101/120» | Las guardas de las líneas 328-337 no fallaron en ninguna de las 110 |
| Glifos (regla #25): `validar_glifos_latex.R` | `OK: sin glifos que rompan pdflatex` |
| Literalidad de exextra: `validar_literalidad_exextra.py --item ITEM_OFICIAL.md` | `TOTAL fallos: 0` |

La prosa de la Solution no sigue el orden de las opciones (Incidente Q): enumera en el orden del banco. La concordancia de género (llanta/rueda frente a rodillo, «del rodillo», «un rodillo más grande») y las tildes son correctas en las versiones que leí.

## 2. Objeciones

### H1. Canal de convergencia: la clave es el centro de las cuatro opciones (DIAGNOSTICIDAD, ALTA)

**Qué se cuestiona**

La selección exige al menos dos distractores completos (línea 254). Además, la clave y los completos se escriben en la misma forma (líneas 115-137), y cada completo difiere de la clave en una sola dimensión:
- VL-B invierte la fracción;
- VL-F cambia × por ÷;
- VL-G añade × K.

Por eso la clave comparte la fracción con unas opciones y la operación con otras: es la que tiene el valor más frecuente en cada dimensión.

Ejemplo real, semilla 2, estilo `fraccion_izq`, terna VL-A+VL-B+VL-F:
- 1000/p ÷ 50
- **1000/p × 50**
- p/1000 × 50
- 1000/50

El comentario de las líneas 15-17 declara como propiedad del diseño que «ni "la que usa todos los datos" ni "la que más comparte con las demás"» aíslan la clave. Lo medido contradice la segunda mitad, y nada en el código vigila esa propiedad.

**Medición.** Reglas: centralidad por bigramas de tokens (la opción con mayor similitud media a las otras tres) y medoide por distancia de edición entre tokens. El techo nulo se calculó con 2000 permutaciones de la clave, tomando el máximo de las 3 reglas medidas.

| Muestra | Centralidad | Medoide | Techo nulo (media / p95) | Exceso |
|---|---|---|---|---|
| **N = 100 (semillas 1:100)** | **81,0 %** | **68,0 %** | 27,3 % / 33,0 % | **+53,7 pp / +40,7 pp** |
| Canónica (n = 110 de 1000) | 0,0 % | 0,0 % | | El ítem oficial no tiene el canal |
| No canónica (n = 890) | 94,2 % | 85,9 % | | |
| `fraccion_izq` (n = 320) | 100 % | 100 % | | |
| `numero_izq` (n = 298) | 100 % | 90,3 % | | |
| `fraccion_unica` (n = 272) | 80,9 % | 64,5 % | | |

- **Margen (§P7-B)**: la brecha relativa entre la suma de distancias de la clave y la de la segunda opción tiene una mediana de 30 % en los aciertos con N = 100, y de 20 % en el estrato no canónico. Está por encima del 15 %, así que el canal es explotable.
- Por terna (2000 semillas), la centralidad acierta el 100 % de las veces en 7 de las 10 ternas válidas.
- Los estratos usan más de 100 semillas porque es un análisis estratificado declarado (regla #23).

**Por qué es un defecto (fuentes de nivel 1)**
- El fenómeno está descrito en la literatura de medición: «Converging on Correct Answers: A Peculiarity of Multiple Choice Items», *Journal of Educational Measurement* 19(3), 211-220 (1982), DOI 10.1111/j.1745-3984.1982.tb00129.x. Suele atribuirse a J. K. Smith, pero el registro que consulté no muestra el autor.
- White y Zammarelli (1981), «Convergence principles: Information in the answer sets of some multiple-choice intelligence tests», según el registro: el conjunto de respuestas puede delatar la correcta sin leer la pregunta.
- No pude leer los resúmenes (HTTP 403). La evidencia cuantitativa es la medición de arriba.
- Normas del repositorio que aplican:
  - regla #22 §P7-A: obliga por encima de +8 pp;
  - §P7-B: un margen de 15 % o más es explotable;
  - §P7-E: la batería debe tener una regla relacional entre opciones. Las sondas de `validar_diagnosticidad.R` no miden centralidad: la batería no tenía sonda para esto.

**Riesgo concreto**
- En unas 9 de cada 10 versiones no canónicas, un estudiante que sabe elegir por convergencia acierta sin modelar la situación. Eso infla el Nivel 4 (D4.4).
- Va en contra de lo que el profesor aceptó al decidir: que «algunas versiones son más difíciles que el original» (`ITEM_OFICIAL.md:177`). Para un estudiante que sabe elegir por convergencia, las versiones nuevas son *más fáciles* que el impreso, que tiene 0 %.

**Alternativa propuesta.** Enumeré el espacio de diseño de forma exacta, con una instancia de K = 100 y d = 150 (`.../scratchpad/espacio.R`):
- Con la forma alineada actual, 28 de las 30 celdas (10 ternas × 3 estilos) dan 100 % de centralidad.
- Hay celdas sin canal:
  - `fraccion_unica` con {VL-A, VL-B, VL-F} o {VL-B, VL-D, VL-F}: 0 % de centralidad, y «usa los tres datos» en 33,3 %;
  - fuera de la restricción actual, {VL-A, VL-D, VL-F} en los tres estilos y {VL-D, VL-E, VL-F} en dos estilos: 0 % de centralidad, y «usa los tres datos» en 50 %.
- Por tanto el canal **no es estructural** (§P7-F).
- Propuesta: sortear la pareja (terna, estilo) desde una tabla de pesos que se calcule midiendo **todo el vector** a la vez:
  - centralidad;
  - medoide;
  - «usa los tres datos»;
  - las sondas `H3` y `H3b` del validador.
- Cablear esa tabla con una guarda y un mutante que muera si se revierten los pesos.
- Un arreglo a medias no basta: escribir cada opción con un estilo propio, sorteado por separado, deja el medoide en 40,0 % y la centralidad en 58,1 % (enumeración sobre las 10 ternas).
- **Coste:** menos variedad de errores por versión (VL-E y VL-G salen menos) y riesgo de que el canal se desplace a «usa los tres datos» o a la forma de la clave. Hay que aplicar la regla de parada del orquestador.
- Si ninguna mezcla deja todas las reglas por debajo de +8 pp, se trata como canal estructural y necesita la firma del profesor, regla por regla, con su cifra (§P7-E, H-5).
- En cualquier caso, hay que corregir el comentario de las líneas 15-17.

**Encuadre procedimental.** Según §P7-C (orquestador, líneas 232-237), un canal que la batería congelada no mide y que aparece en una auditoría a mitad de ciclo no obliga en este ciclo: lo decide el profesor. Recomiendo corregirlo ahora por tres razones:
- no es un residuo de una rama, sino el agregado (+41 a +54 pp);
- contradice una propiedad que el propio código declara;
- el espacio de diseño tiene celdas viables.

**Veredicto**: MODIFICAR. Bloquea la FASE 3 hasta que el profesor decida.

### H2. La evidencia del paso 9 no es de este md5 (OTRO, procedimiento, MEDIA)

**Qué se cuestiona**
- `salida/paso9_diversidad.txt` y `salida/paso9_diagnosticidad.txt` se midieron sobre el md5 `d611a97be8fe4b2e402eb3b69728a1cc`. Se escribieron a las 15:44, y el `.Rmd` cambió a las 15:45.
- Solo `paso9_stresstest.txt` corresponde a `3d8dc379…`.
- Por tanto el PASS, los «77 valores únicos» y las cifras de `H3`/`H3b` no certifican el artefacto auditado.

**Por qué**: los validadores escriben el md5 del archivo que miden precisamente para detectar esto (v3.37.0). La regla #16 pide auditar por artefacto.

**Riesgo concreto**: sellar `validar_diversidad` con evidencia de otra versión. No sé qué cambió entre `d611…` y `3d8d…`.

**Alternativa**: volver a ejecutar `validar_diversidad_sustantiva.R` y `validar_diagnosticidad.R` sin `--n` (N = 100) sobre el md5 final, después de los cambios.

**Veredicto**: MODIFICAR.

### H3. La Solution presenta como universal una prueba que solo es necesaria (prosa de la Solution, MEDIA)

**Qué se cuestiona**: «Toda expresión incorrecta falla al menos una de estas dos pruebas» (línea 408). Las dos pruebas son la monotonía (crece con la distancia y decrece con el perímetro) y las unidades.

**Por qué**
- En el contexto se lee como regla general, y como regla general es falsa: 2 × (100/P) × 150 crece con la distancia, decrece con el perímetro, da vueltas con unidades coherentes y es incorrecta.
- Para el banco de esta pregunta la frase sí es cierta:
  - VL-A, VL-B, VL-D y VL-F fallan la prueba de monotonía;
  - VL-E y VL-G la pasan y solo fallan la de unidades.
- La regla #13 v1.1 dice que toda lectura razonable cuenta y que, si cambia la verdad de la frase, se reescribe. No la clasifico como CORRECCIÓN porque ninguna versión afirma algo falso sobre sus propias opciones.

**Riesgo**: enseña una condición necesaria como si fuera suficiente, y el estudiante puede transferir ese criterio falso a otros problemas.

**Alternativa**: «Cada una de las tres expresiones incorrectas de esta pregunta falla al menos una de estas dos pruebas, que toda expresión correcta debe cumplir: … Pasar las dos no basta: la comprobación que decide es el caso específico.»

**Veredicto**: MODIFICAR.

### H4. La estrategia final, leída al pie de la letra, valida el distractor VL-E (pedagógico, MEDIA)

**Qué se cuestiona**: «compárala con el cálculo directo: distancia dividida entre perímetro.» (línea 424). «Distancia dividida entre perímetro» sin convertir unidades **es** VL-E (`expr = fr(dT, V)`, línea 200).

**Riesgo concreto**
- VL-E aparece en el 27,2 % de las versiones (ternas BEF, BEG y EFG: 544 de 2000 semillas).
- Ejemplo: con 150 m y P = 200 cm, VL-E da 150/200 = 0,75, y el «cálculo directo» literal también da 0,75. El estudiante concluiría que VL-E es correcta.
- Lo mitigan, sin anularlo, la viñeta anterior («misma unidad») y el caso específico, que sí convierte.

**Alternativa**: «… con el cálculo directo: la distancia, escrita en la misma unidad del perímetro, dividida entre el perímetro.»

**Veredicto**: MODIFICAR.

### H5. Se atribuye a la Guía una explicación con las cifras de la versión (BAJA)

**Qué se cuestiona**: en las versiones no canónicas, `nota_guia` termina con «así lo explica la Guía de orientación de Evaluar para Avanzar 2023 …» (línea 148), pero con las cifras y el objeto de la versión, por ejemplo «dividir el perímetro entre 1000 … la rueda … por 50». La Guía habla de «p entre 100», 150 metros y «la llanta» (`ITEM_OFICIAL.md:96`).

**Riesgo**: una paráfrasis presentada como si fuera de la Guía.

**Alternativa**: «… es la explicación que da la Guía … para el error análogo en la pregunta original», o reservar la atribución literal para la canónica.

**Veredicto**: MODIFICAR (opcional).

## 3. Mediciones que declaro sin objeción

- **«La opción que usa los tres datos»** (regla del estímulo).
  - Con N = 100 acierta el 34,6 %: +7,3 pp sobre el techo nulo de 27,3 %, zona gris.
  - Canónica: 50 % (n = 110), heredado de la estructura oficial (A no tiene P, D no tiene la distancia; §P7-E).
  - No canónica: 32,6 %, es decir +7,6 pp.
  - Descartar una expresión a la que le falta P o d además mide parte del constructo (son los mecanismos A y D de la Guía).
  - No obliga, pero la corrección de H1 no debe subirla.
- **Posición de la clave** (stress test sobre `3d8dc379…`): 23 / 25 / 31 / 21.
- **Diversidad propia**, solo orientativa y con otra métrica que la del validador: 71 combinaciones distintas de texto y estilo de la clave en las semillas 1:100.

## 4. Cobertura de los 8 dominios

| Dominio | Estado |
|---|---|
| Código R/exams | Sin bucles sin cota (F1), `pick` (F5), oráculo y guardas correctos. La propiedad declarada en las líneas 15-17 es falsa y no tiene guarda (H1) |
| Pedagógico | Siete contextos con funciones de plantilla y las seis secciones metacognitivas. Hallazgos H3, H4 y H5 |
| Visual | Sin figura (Flujo B no requerido por decisión del profesor). **No audité visualmente** los PNG, el PDF ni el DOCX; me limité a `render.log` (`RENDER_OK`). Lo cubre `coherencias_5` |
| Gramática y ortografía | Concordancia de género y número y tildes correctas en las versiones leídas |
| Coherencia matemática | Comprobada con el oráculo independiente: 0 fallos en 1000 semillas |
| Metacognición ICFES | exextra literales, 0 fallos. DOK 2 con N4 es admisible (la recíproca es falsa); viene de la ficha |
| Testing | Las guardas cubren corrección, errata y canónica. No hay guarda para la diagnosticidad relacional (H1) |
| Semántico (Nivel 4) | Cada descripción coincide con su expresión en las tres formas de escritura |
| Overrides e invariantes locales (v1.5) | El subproyecto no tiene `.claude/` propio. La única propiedad declarada es la de las líneas 15-17 (H1) |

## 5. Conclusión

**APROBAR CON CAMBIOS.** No hay hallazgos de CORRECCIÓN. Es obligatorio:
- reescribir las frases de H3 y H4;
- volver a medir el paso 9 sobre el md5 final (H2);
- corregir el comentario de las líneas 15-17;
- llevar H1 al profesor con sus cifras antes de la FASE 3. Por §P7-C, la decisión sobre ese canal es suya. Mi recomendación es corregirlo en este ciclo, midiendo el vector completo.

## 6. Próximos pasos

1. Hacer los cambios de texto H3 y H4 (y H5, opcional) y corregir el comentario de las líneas 15-17.
2. Explicar H1 al profesor en lenguaje llano: «en 9 de cada 10 versiones no canónicas se acierta eligiendo la expresión de la que las otras parecen variaciones». Darle las dos salidas con su coste: corregir ahora, o aceptarlo como residuo declarado de +54 pp.
3. Si decide corregir: tabla de pesos (terna, estilo) medida sobre el vector completo, con guarda y mutante. Como mucho 3 pasadas (§P7-D), y comprobar después de cada una que la clave sigue siendo verdadera.
4. Volver a renderizar y a pasar el arsenal. Repetir el paso 9 completo sobre el md5 final (H2).
5. Repetir `coherencias_5`, que se selló antes de la FASE 2C, sobre el md5 final.
6. Lanzar una FASE 2C nueva sobre ese md5: este reporte no cubre versiones posteriores.

## Fuentes
- [Converging on Correct Answers: A Peculiarity of Multiple Choice Items, J. Educ. Meas. 19(3) 211-220, 1982 (registro Scilit)](https://scilit.com/publications/83573fe43543b263827fff365b1c89c6)
- [Convergence principles: Information in the answer sets of some multiple-choice intelligence tests (UMN Conservancy)](https://conservancy.umn.edu/items/4aae3158-ea3d-4318-b011-39b100493aea)

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:16 | Así ni «la que usa todos los datos» ni «la que más comparte con las demás»
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:117 | distractores completos (VL-B, VL-F, VL-G), así que la forma tampoco la delata dentro de la versión.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:254 | validas <- Filter(function(t) sum(t %in% COMPLETOS) >= 2L &&
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/ITEM_OFICIAL.md:177 | algunas versiones son más difíciles que el original
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/paso9_diversidad.txt:1 | md5 del .Rmd: d611a97be8fe4b2e402eb3b69728a1cc
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/salida/paso9_diagnosticidad.txt:1 | md5 del .Rmd: d611a97be8fe4b2e402eb3b69728a1cc
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:408 | Toda expresión incorrecta falla al menos una de estas dos pruebas
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:424 | compárala con el cálculo directo: distancia dividida entre perímetro.
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:200 | expr = fr(dT, V),
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/vueltas_llanta_perimetro_numerico_variacional_formulacion_ejecucion_n4_schoice_v1.Rmd:148 | así lo explica la Guía de orientación de Evaluar para Avanzar 2023
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4/ITEM_OFICIAL.md:96 | al dividir p entre 100 obtienen la distancia que recorre la llanta en una vuelta
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
