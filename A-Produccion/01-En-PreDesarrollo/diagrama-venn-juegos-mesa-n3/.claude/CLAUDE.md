# Invariantes locales — `diagrama-venn-juegos-mesa-n3`

> Regla #17 / Incidente `INC-CLAUDE-LOCAL` (pre-flight 19 de `orquestador-schoice`). Estas invariantes
> **prevalecen sobre el criterio genérico** dentro de este subproyecto. Si alguna contradice una
> regla del repo raíz, gana la del raíz y el conflicto **se reporta**, no se resuelve en silencio.
> Las fijó el profesor al encargar el ejercicio (2026-11-06). Relajar cualquiera exige su firma
> (regla #24 H-5).

**Origen:** pregunta impresa **14** del simulacro SAI «Nivelación 6» de grado 10, Sesión 1, reciclada de Evaluar para
Avanzar 2023, Saber 10.°, Cuadernillo 2, pregunta 8 (`I_1957870`). Recortes en `Originales/`. Texto verbatim, regiones de las
cuatro opciones, clave, mecanismos y clasificación: [`ITEM_OFICIAL.md`](../ITEM_OFICIAL.md). **Contrasta contra ese archivo y el
recorte, no contra tu recuerdo.**

## I-1 · Fidelidad: las opciones son diagramas de Venn

La pregunta sigue siendo «¿cuál diagrama de Venn organiza los datos de manera adecuada?». Las cuatro opciones son diagramas de
Venn de tres conjuntos con la misma geometría: marco, tres círculos del mismo radio, un rótulo por conjunto y siete números. Solo
cambian los números. **Prohibido** el patrón que convierte las opciones en descripciones de errores conceptuales. La capa
metacognitiva obligatoria (regla #1) va en la **Solution**: qué lectura de los datos lleva a cada distractor, reflexión y estrategia.

Trazo, grosor, tamaño de letra y color son idénticos en las cuatro figuras de una versión: nada fuera de los números distingue a una
opción de otra.

## I-2 · Instancia canónica verbatim

Una instancia reproduce **exactamente** el impreso: el enunciado (jornada de juegos con empleados de una empresa; parqués, Monopoly
y bingo), los siete datos y los cuatro diagramas de `ITEM_OFICIAL.md`, con la clave en el **contenido** de C. La **letra** no se fija.
Probabilidad `P_CANON = 0.05`, como en `rapidez-media-error-paso` (L-3); tope 0,10. Se implementa con su `stopifnot` contra literales.

## I-3 · Los distractores salen de los mecanismos de la Guía oficial

Calculados con los datos de cada versión, con estos nombres de `ITEM_OFICIAL.md`: `a` (dos conjuntos, pero no el tercero), `b`, `c`,
`d` (intersecciones de dos), `P`, `M`, `B` (totales), y `z = b − a` (centro). Orden de regiones: solo primero, primero y segundo, solo
segundo, centro, primero y tercero, segundo y tercero, solo tercero.

| Opción impresa | Mecanismo | Regiones |
|---|---|---|
| A | Toma `a` como centro y `b` como zona exclusiva; completa el resto con las operaciones usuales. Conserva los tres totales y cumple 5 de los 7 datos | `P − b − d, b, M − b − c, a, d − a, c − a, B − d − c + a` |
| B | Centro y zonas dobles correctos, con cada total en la zona de un solo conjunto | `P, a, M, z, d − z, c − z, B` |
| D | La misma lectura de A, con cada dato puesto sin operar | `P, b, M, a, d, c, B` |

**En todas las opciones las siete regiones son enteros ≥ 0, y los cuatro vectores son distintos.** Una región negativa en el distractor
A lo delataría sin calcular. Se construye desde las regiones (familia F1), no remuestreando hasta que aparezca, con `stopifnot` en
`data_generation`.

Si una versión no admite alguno de los tres, es decir, si la fórmula da negativo o colisiona, esa combinación de datos no se sortea. Los
tres mecanismos se conservan en todas las versiones.

**Conflicto con la regla #1, resuelto por firma del profesor el 2026-11-06 (registrado en la FASE 2C r1; ver § Firma del profesor).** I-3 fija los tres distractores en
todas las versiones. La regla #1 y el pre-flight 20 del orquestador piden sortearlos de un pool de 4-6 errores, y aquí el tipo de error de las
opciones es invariante: solo cambian los datos. La regla raíz prevalece sobre la local, pero I-3 lleva la firma del profesor y relajarla
exige la suya (regla #24 H-5), así que no se resuelve en silencio: se mantiene I-3, que además es su preferencia estable (opciones fieles al
original y metacognición en la Solution), y él decidió mantenerla en el WAIT_USER #3. Lo que hay hoy: un pool de 5 errores, con los 3 ofrecibles fijos y
2 errores no ofrecibles, sorteados por versión, que solo aparecen en la Solution; eso varía la retroalimentación, no las opciones.

## I-4 · La clave está dentro del PNG

Ningún validador del repo lee los números de una figura: `ERR_ANS_B` y los demás comparan valores, no píxeles. Por eso
`verificar_espacio.R`, en este directorio, debe:

1. **Extraer la función de dibujo del `.Rmd` real**, no reimplementarla.
2. Comprobar que **solo el vector de la clave reproduce los siete datos** de la versión.
3. Comprobar que **el punto de cada rótulo numérico cae dentro de los círculos de su región y fuera de los demás**.
4. Enumerar el espacio si es finito, con un assert de su tamaño como guarda de deriva; si no lo es, usar N = 100.
5. Verificarse a sí mismo: **un mutante que intercambie dos regiones debe hacerlo fallar.**
6. **Leer lo impreso**, no solo los argumentos de `dibujar_venn`: `pdftotext -bbox` sobre el PDF del que sale el PNG, con cada número ubicado por
   los círculos que contienen su palabra y cada rótulo por su círculo más cercano. Una figura que reciba un `geom` propio es un fallo (I-1), y
   las cuatro figuras son iguales píxel a píxel fuera de los números. En los números se comprueban fuente, tamaño, ancho por cifra, color, tinta negra,
   posición y que no haya marcas bajo la línea base.
7. **Ejecutar el chunk `solucion`** en la canónica y en N = 100: cada párrafo casa con una plantilla cuyas cifras se contrastan con la figura y el
   enunciado, o está en la lista de frases sin datos, y además coincide letra a letra con el texto reconstruido desde las viñetas y en su sección. Un
   párrafo nuevo, reescrito o movido da rojo hasta que tenga su comprobación, y los bloques `{=latex}` solo admiten el literal de `encabezado`. Esto prueba que
   el texto es el revisado, no que sea verdadero: la verdad la dan los oráculos y la lectura de la FASE 2C. Las afirmaciones absolutas sobre datos sorteados
   llevan su `stopifnot` en `data_generation` (regla #13 v1.1).
8. **Comprobar lo que llega al LMS** en el XML real de Moodle: tipo de pregunta, pregunta, `fraction`, feedback de cada opción e imagen de la Solution, contra el
   dibujo calculado desde las viñetas del propio XML. `exsolution` decide la nota y el chunk `feedback` es lo que lee el estudiante; ninguna otra sección los mira.
9. **Vigilar la pregunta y las plantillas de contexto.** La pregunta se compara con el impreso, y las plantillas llevan un md5 congelado: si cambian, rojo
   hasta revisarlas, porque una frase puede añadir un dato que cambie la clave.

## I-5 · Legibilidad

Los números de dos cifras no se solapan ni tocan los trazos en las zonas estrechas, y se leen a 320 y 360 px. Se revisa el caso extremo:
dos cifras en el centro y en las tres zonas dobles a la vez.

**Papel (decisión del profesor del 2026-11-06, H9 de la FASE 2C).** El ítem se imprime en hojas de 216 × 330 mm: con `A-Produccion/perifericos/solpcielo.tex`,
una pregunta que empieza arriba de la página deja las cuatro opciones en una misma página. Por eso cada figura mide 5,5 cm en papel y 280 px en pantalla:
`{width=5.5cm style="width:280px!important;max-width:100%"}`. Sin `!important` gana el `width:5.5cm` que pandoc añade al final del `style`, y la figura queda en unos 208 px en todos los anchos, sin desborde. Eso es lo
que pasaría en un Moodle que limpie el HTML: el servidor icfes no lo hace (`forceclean` = 0, medido el 2026-11-06), y las preguntas usan `format_text` con `noclean`. En `verificar_espacio.R`, la
sección `papel` vigila las páginas; el literal del ancho lo vigilan `cableado`, `solucion` y `moodle`. Condiciones de uso, en `SemilleroUnico_v2.R` y vigiladas por la sección `uso`: NOPS, que fija A4, con
`exams2nops(..., twocolumn = TRUE, samepage = TRUE, header = "\\raggedbottom")`; `exams2pdf` con varias preguntas, con la plantilla local `solpcielo_una_por_pagina.tex`, porque con `solpcielo.tex` las que empiezan a
media página parten sus opciones. El DOCX no conserva las cuatro opciones juntas y ninguna sonda lo mira: no se usa para imprimir este ítem.

## Clasificación

Nivel 3, Descriptor D3.11, sin inflar el DOK. **Estándar de la ficha y Grado sugerido 6°-7°: decisión del profesor del 2026-11-06.** La
Guía oficial trae otro Estándar (Aleatorio 10-11); la discrepancia está declarada en `ITEM_OFICIAL.md` y no se reabre.

## Firma del profesor (2026-11-06)

Respuesta literal, transmitida por el coordinador: **«firmo tal cual, mantengo I-3, a»**. Vale para el `.Rmd` con md5 `f886dfaa45b80a5ee16f9820908049b5`: si cambia,
queda sin efecto y hay que medir de nuevo. Registro, con las glosas que leyó: [`salida/tabla_firma_p7.md`](../salida/tabla_firma_p7.md).

- **§P7-E:** override firmado, regla por regla, en dos listas separadas: 8 reglas en las variantes y 12 en la canónica. Está cableado en `verificar_espacio.R`
  (`FIRMADA`, `RMD_MD5`, `FIRMA_VARIANTES`, `FIRMA_CANONICA`, `decidir_exit()`), contra las cifras exactas. Exit 4 es «rojo cubierto por la firma», solo en modo `todo`:
  no es PASS.
- **I-3 frente a la regla #1:** se mantiene I-3. Conflicto resuelto por firma.
- **Condición:** si en el aula se ve que los estudiantes usan el atajo de `EST-menos-cifras-del-enunciado`, se aplica el filtro de la tabla. Eso cambia el `.Rmd`, así que
  exige otro detractor y otra firma.

## Fuentes de figura

La referencia del Flujo B son `Originales/orig_opcion_*.png`, con el cuadernillo oficial como segunda referencia de trazo y color.
**Prohibido** usar las figuras `q014-*.png` de Todo-Pajaro: están regeneradas con IA y su trazo varía entre opciones.

## CLOZE hermano (`Cloze/`, desde el 2026-11-09)

> **Firmado por el profesor el 2026-11-09**, en la pausa posterior al dry-run del `orquestador-cloze` (pre-flight 26/26,
> `VEREDICTO_ORQUESTADOR: dry_run`, sin escrituras en el repo). Respuesta literal, transmitida por el coordinador:
> **«D1 a · D2 A · D3 1|1|1|1|1|1 · D4 a, H24 a1 · D5 a»**. Regla #24 **H-5**: queda por escrito quién decidió, cuándo, qué se
> midió y qué variantes se descartaron. Estado y evidencia del CLOZE: `Cloze/HANDOFF.md`, que crea el orquestador.

`Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd` tiene su propio `Cloze/ejercicio_state.json`.
Sus seis partes son `schoice|num|num|num|num|mchoice`, y la Parte 1 es este SCHOICE.

- **I-1 a I-5 y la firma del 2026-11-06 rigen la Parte 1.** Por código, la Parte 1 es el SCHOICE: su chunk `data_generation` se
  trasplanta con un script desde el cuerpo firmado (md5 `f886dfaa…`; hasta el 2026-11-09 empezaba en la línea 7, después del YAML de un
  Knit de RStudio que ese día se quitó; el script y E1 aceptan las dos formas), con tres renombres (`opciones_p1`, `sol_p1`, `feedback_p1`) y sin tocar su consumo del RNG. La guarda es E1 de
  `Cloze/verificar_espacio_cloze.R`: objetos y `.Random.seed` iguales en 100 variantes y 20 canónicas. En la vista previa del dry-run
  dio 0 diferencias, y sus dos controles fallaron como debían. Un defecto que se vea en la Parte 1 se reporta; no se corrige ahí.
- **En las Partes 2 a 6 las invariantes se pueden endurecer** (es autónomo y solo se registra), nunca relajar.
- Fuera de `Cloze/`, el CLOZE solo escribe en esta sección. No se tocan el `.Rmd` SCHOICE, `ejercicio_state.json`, `HANDOFF.md`,
  `verificar_espacio.R` ni `salida/` de este directorio.

### C-1 · Flujo B heredado del SCHOICE (D1 a)

Requerido = sí, lenguaje = TikZ: la misma `dibujar_venn` y `GEOM_VENN`, que viajan en el chunk trasplantado, sobre la geometría de
`salida/flujo_b/geom_venn.json`. No se regenera el graficador. En el paso 2 del SCHOICE, TikZ, Python y R dieron 100,0 % en el
comparador.

### C-2 · Estructura: Alternativa A del dry-run (D2 A)

- **Qué.** Justo después de `##ANSWER1##`: «Las Partes 2 a 6 se refieren a otra encuesta», con otro contexto y sus siete datos en
  viñetas con la forma del enunciado.
  - Parte 2 (num): el centro, es decir, cuántas personas están en los tres conjuntos.
  - Parte 3 (num): una zona de dos conjuntos sin el tercero, sorteada entre las dos que el texto no da.
  - Parte 4 (num): una zona de un solo conjunto, sorteada entre las tres.
  - Parte 5 (num): el total de personas.
  - Parte 6 (mchoice): cuatro afirmaciones sin cifras. El veredicto se sortea primero, hay al menos una verdadera y una falsa, y
    ningún par es complementario.
- **Canónica:** la Parte 1 va literal (I-2), y la otra encuesta sale de los contextos de las variantes. Es la lectura del F-4 de
  `totales-promedios-reciclaje-n3`.
- **V11 en los dos sentidos:** ninguna de las cuatro respuestas numéricas coincide con un número impreso en la Parte 1 (sus 7 datos y
  los números de los 4 diagramas), y ningún dato de la otra encuesta coincide con un número de esos diagramas. El centro y el dato
  «pero no» (personas en dos conjuntos pero no en el tercero) se sortean entre 13 y 24, con cascada F1.
- **Sin vocabulario de error** y sin la frase «pero no» en las preguntas de las Partes 2 a 6.
- **Medido en el dry-run** con `medir_v11.R`, `medir_x.R` y `medir_escala.R`; el coordinador los repitió y dieron las mismas cifras:
  - con la escala de la Parte 1, 26 de 100 versiones no tienen ningún candidato en 2000 sorteos;
  - con 13 a 24, ninguna; la aceptación media es 5,6 % (mínima 0,70 %; canónica 1,75 %), y el total de la otra encuesta tiene mediana
    104 y máximo 137;
  - el atajo «elegir en la Parte 1 el diagrama con menos números en común con la otra encuesta» acierta el 34,6 % sin la exclusión y
    exactamente el 25 % con ella;
  - la medición tomó como Parte 4 la zona del tercer conjunto; con el sorteo entre las tres zonas se vuelve a medir en la ejecución.
- **Variantes descartadas:**
  - B, los mismos datos: el total y «X o Y» delatan la clave en el 100 % y «exactamente uno» en el 96 %. En la canónica, el total
    (32) es la suma del diagrama correcto entre 25, 32, 61 y 78.
  - C, un quinto diagrama: sus respuestas coinciden con números impresos entre el 11,5 % y el 43,6 %, y añade un PNG con sus propias
    guardas.

### C-3 · Pesos `expoints: 1|1|1|1|1|1` (D3)

- La Parte 1 vale 1/6 de la pregunta.
- Se firmó con el costo medido a la vista. Fórmula: (tasa de acierto − 25 %) × peso de la Parte 1, con 95 % de variantes y 5 % de
  canónica. Con estos pesos, un estudiante que conozca el atajo gana sobre el azar: `MAG-suma-segunda-menor` +12,5 pp,
  `EST-menos-cifras-del-enunciado` +6,8, `MAG-menor-maximo` +5,1 y `LEX-con-repetidos` +2,0.
- Opciones medidas y no elegidas: 3|1|1|1|1|1 daba +28,1, +15,4, +11,4 y +4,4; 2|1|1|1|1|1, +21,4, +11,7, +8,7 y +3,4; 1|2|2|2|2|2,
  +6,8, +3,7, +2,8 y +1,1. En `totales` se firmaron +19,7 pp.
- Guarda: R/exams recicla `expoints` sin avisar cuando faltan entradas, así que el verificador del CLOZE vigila los pesos, como E7 de
  `totales`. Cambiarlos exige otra firma.

### C-4 · Firma §P7 extendida por alcance a la Parte 1 (D4 a, H24 a1)

- Mismas cifras de `salida/tabla_firma_p7.md`: 8 reglas en las variantes y 12 en la canónica, más `LEX-con-repetidos` en las
  variantes por H24 (a1).
- Condicionada a que E1 salga en verde y a que esos canales, vueltos a medir sobre los objetos de la Parte 1 del CLOZE, queden dentro
  de lo firmado. Se cablea como en `verificar_espacio.R` (`FIRMADA`, md5 del `.Rmd` CLOZE, `FIRMA_VARIANTES`, `FIRMA_CANONICA`):
  exit 4 es «rojo cubierto por la firma», no PASS.
- Todo canal cruzado de las Partes 2 a 6 hacia la Parte 1 por encima de +8 pp vuelve al profesor; se mide con reglas del estímulo
  (§P7-G). La firma no cubre ningún defecto de CORRECCIÓN.
- **H24 (a1):** `LEX-con-repetidos` entra en la lista de variantes del CLOZE con 33,418 %, +8,39 pp sobre el techo exacto de 25,03 %.
  `verificar_espacio.R` y `salida/tabla_firma_p7.md` de este SCHOICE siguen con el techo de N = 100 hasta otra tarea.
- Glosas desde `verificar_espacio.R` con md5 `d7271f81…` (las líneas de `salida/tabla_firma_p7.md` son de la versión `b5192076…`, la
  que estaba en disco al firmar). `sm` es la suma de los siete números de un diagrama, `gr` su número más grande, `lit` cuántos de sus
  números aparecen en el enunciado y `rep_int` siete menos sus números distintos:
  - `MAG-suma-segunda-menor` (línea 1027): elige el diagrama con la segunda suma más baja.
  - `MAG-menor-maximo` (línea 1030): elige el diagrama cuyo número más grande es el menor.
  - `LEX-con-repetidos` (línea 1040): elige el diagrama que trae al menos un número repetido.
  - `EST-menos-cifras-del-enunciado` (línea 1044): elige el diagrama con menos números que aparecen en el enunciado.
  - Una regla que elige todas las opciones o ninguna no se aplica (`conj`, línea 1019).

### C-5 · Clasificación heredada sin reclasificar (D5 a)

Interpretación y representación · Aleatorio · Nivel 3 · D3.11 · DOK 2 · Evaluar · Relacional, con los `exextra` literales del SCHOICE
(`validar_literalidad_exextra.py`: exit 0). Solo `exextra[Type]` pasa a CLOZE. Condición: las Partes 2 a 6 se quedan en DOK 2.

### Conflictos declarados en el dry-run

Se mostraron al profesor junto con D1 a D5. Se reportan; no se resuelven en silencio.

1. **El Incidente G prevalece sobre la forma de I-1 e I-2.** Los cuatro diagramas van en el enunciado de la Parte 1, rotulados I a IV
   después de la mezcla, con nombres `diagrama_[a-d]_<fig_id>.png` y el ancho de I-5, y el gap ofrece texto («Diagrama I»…). En la
   canónica no se fijan ni la letra ni el rótulo.
2. **La Parte 1 hereda, ya firmados,** I-3 frente a la regla #1 y el orden A < clave < B < D de las sumas (estructural, §P7-F).
3. **`exshuffle: TRUE`:** la excepción de la regla #4 no aplica a un CLOZE. Se barajan los textos del gap; los rótulos del enunciado no
   se mueven. En el PDF impreso del semillero no se barajan: decisión «H1 c» del profesor tras la tanda 4 (abajo).
4. **La prohibición de I-1 de opciones que describan errores** se extiende a las Partes 2 a 6, como endurecimiento.

### Decisiones del profesor tras la tanda 3 (2026-11-09)

> **Firmadas por el profesor el 2026-11-09**, en la pausa posterior a la tanda 3 del `orquestador-cloze` (FASE 2C r2 con
> `APROBAR_CON_CAMBIOS` y sin CORRECCIÓN; `.Rmd` con md5 `889d828e8129e11b0ec5dceda0cf108b`). Respuesta literal, transmitida por el
> coordinador: **«1 b · semillero sí»**. Regla #24 **H-5**.

1. **(1 b) Plantilla para un PDF con varias preguntas.** En `Cloze/` se crea una copia de `../solpcielo_cloze.tex` con un solo cambio:
   `\clearpage` al empezar cada pregunta, el mismo que `../solpcielo_una_por_pagina.tex` hace sobre `../solpcielo.tex`. Conserva el
   recuadro «Cómo marcar» de la plantilla del CLOZE. Se mide en la sección `papel` del verificador del CLOZE, con su mutante, y queda como
   condición de uso de la I-5 para `exams2pdf` con varias preguntas. No cambia el `.Rmd`.
   - Medido antes (FASE 2C r2): con `../solpcielo_cloze.tex`, la segunda de tres preguntas parte sus cuatro figuras en 5 de 5 PDF; con
     `../solpcielo_una_por_pagina.tex`, 15 de 15 grupos de figuras quedan juntos, y 34 PDF en la sección `papel`.
   - Descartadas: (a) `../solpcielo_una_por_pagina.tex`, la plantilla del SCHOICE, que no trae «Cómo marcar»; (c) encerrar la cuadrícula
     2×2 en un `minipage`, que cambia el `.Rmd` y sería una cuarta pasada.
2. **Semillero: sí.** Se crea `Cloze/SemilleroCloze.R` a partir de `../SemilleroCloze.R` (copia de `A-Produccion/perifericos/`), con el PDF
   en oficio y la plantilla de (1 b), vigilado por el verificador del CLOZE como la sección `uso` vigila al semillero del SCHOICE.
   `../SemilleroCloze.R` no se toca: genera el PDF con `template = "plain"` de R/exams, que es A4 y no está medido para este ítem.

### Decisiones del profesor tras la tanda 4 (2026-11-09)

> **Firmadas por el profesor el 2026-11-09**, en el WAIT_USER #3 de la tanda 4 del `orquestador-cloze` (FASE 2C r3 con
> `APROBAR_CON_CAMBIOS` y sin CORRECCIÓN; `.Rmd` con md5 `889d828e8129e11b0ec5dceda0cf108b`). Respuesta literal, transmitida por el
> coordinador: **«H1 c · H2 100»**, sobre los hallazgos H1 y H2 de `Cloze/salida/fase2c_r3_detractor.md`. Todavía no es la aprobación
> del paso 11. Regla #24 **H-5**.

1. **H1 (c): `exshuffle = FALSE` solo en la llamada `exams2pdf()` de `Cloze/SemilleroCloze.R`.** El gap de la Parte 1 se imprime
   entonces «Diagrama I / Diagrama II / Diagrama III / Diagrama IV», y la letra que pide «Cómo marcar» («la opción (a) es la primera,
   (b) la segunda…») coincide con el número del diagrama. Con la mezcla de R/exams, el número del diagrama correcto difiere de su letra,
   en promedio, en 3 de cada 4 versiones impresas.
   - Medido: con `exshuffle = FALSE`, 9 de 9 preguntas en orden I-IV (orquestador, tanda 4) y 4 de 4 (coordinador, en el scratchpad);
     sin él, 1 de 4. `exshuffle` es argumento formal de `exams2pdf()` en exams 2.4.5.
   - La clave sigue sorteada: la Parte 1 mezcla los diagramas antes de rotularlos (`ops <- ops[sample.int(4L)]`) y la Parte 6 sortea
     su patrón de verdaderas y falsas (`v6`).
   - No cambian el `.Rmd`, la plantilla de (1 b) ni Moodle, que sigue barajando: las llamadas de HTML, Moodle y Canvas no llevan
     `exshuffle = FALSE`.
   - Queda como condición de uso del PDF impreso, vigilada en la sección `uso` del verificador con su literal y su mutante.
   - Descartadas: (a) una línea más en «Cómo marcar», que cambia la plantilla de (1 b) y solo avisa; (b) `exshuffle: FALSE` en el
     `.Rmd`, que cambia el md5 y contradice la regla 6 de `codigo-rmd.md`; (d) dejarlo como residuo declarado.
2. **H2 (100): `archivos = 100` en `Cloze/SemilleroCloze.R`**, en lugar de las 250 heredadas de `perifericos/`: el máximo de 100 del
   profesor rige también para las exportaciones. Vigilado en `uso` con su literal y su mutante.
3. **Cuarta pasada, solo sobre el semillero.** Las dos decisiones cambian un archivo que auditó la r3, así que son una cuarta pasada, por
   encima del máximo de tres de la restricción 6 (§P7-D). El profesor la autorizó al elegirlas, advertido por el coordinador, y queda
   limitada a `Cloze/SemilleroCloze.R` y a la sección `uso` del verificador. El `.Rmd` no cambia: `RMD_MD5` sigue en `889d828e…`. Antes
   de volver al WAIT_USER #3 hacen falta la r7 y una FASE 2C r4 con un detractor nuevo.

### Aprobación del profesor (2026-11-09)

> **Firmada por el profesor el 2026-11-09**, en el WAIT_USER #3 de la tanda 5 del `orquestador-cloze` (FASE 2C r4 con
> `APROBAR_CON_CAMBIOS` y sin CORRECCIÓN). Respuesta literal, transmitida por el coordinador: **«a»**, a la pregunta que pedía aprobar
> el `.Rmd` con md5 `889d828e8129e11b0ec5dceda0cf108b`. El coordinador selló el paso 11 (`aprobacion_usuario`).

- **Qué aprueba, para el aula:** `Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd` (md5
  `889d828e…`), con `Cloze/SemilleroCloze.R` (`d01bbf0f…`), `Cloze/verificar_espacio_cloze.R` (`bf11b0a4…`; exit 4 = rojo §P7 de la
  Parte 1 cubierto por C-4, no es PASS) y la plantilla de (1 b) (`290a3f79…`). Los cuatro md5 se comprobaron en disco al sellar, y la
  aprobación vale para ellos.
- **Condición de uso del PDF impreso:** generarlo con `generar_pdf()` de `Cloze/SemilleroCloze.R` (plantilla de 1 b y `exshuffle = FALSE`).
- **Residuos aceptados con la aprobación:** los de `Cloze/HANDOFF.md`, § «Residuos declarados». Ninguno es de CORRECCIÓN. Sigue sin
  auditar la nota del MULTIRESPONSE de la Parte 6 en Moodle (H5 de la r3), que se prueba en `lab`.
- **No es producción:** la promoción espera la evidencia de aula.

### Decisión del profesor del 2026-11-10: cuestionario interactivo en el semillero («1a»)

> **Firmada por el profesor el 2026-11-10**, en sesión directa con el coordinador. Respuesta literal: **«1a · 2a»**. La pregunta 1
> ofrecía (a) recuperar el semillero aprobado y agregarle el cuestionario interactivo, (b) solo recuperarlo o (c) dejarlo como estaba.
> La 2a quitó del `.Rmd` del SCHOICE las 6 líneas de YAML del Knit de RStudio (Error 43); sin ellas vuelve a `f886dfaa…`.

- **Contexto:** el 2026-11-10 a las 05:03, `Cloze/SemilleroCloze.R` estaba reemplazado por la copia genérica de `perifericos/` (md5
  `aa2be328…`): sin la plantilla de 1 b, sin `exshuffle = FALSE`, y con un PDF de prueba en A4 (`pdf_basico.Rmd`) en lugar del
  ejercicio. Ningún semillero del CLOZE generaba el cuestionario interactivo; solo el del SCHOICE.
- **Cambio:** se recuperó el semillero aprobado (`d01bbf0f…`, commit `0ab5fc64`) y se le agregó `generar_interactivo()`, que llama a
  `.claude/scripts/webquiz_autocontenido.R` con `config$preguntas_interactivo = 10` versiones y solo da por bueno el exit 0
  (`WEBQUIZ_OK`). La función entra en la opción 4 del menú, en la opción 6 (nueva) y en la corrida no interactiva. No lleva `exshuffle`.
  Semillero nuevo: `e33984c2fe3cf9fb57fdfdb9d79d7c57`.
- **Lo que no cambia:** `generar_html()`, `generar_moodle()`, `generar_canvas()`, `generar_pdf()`, `archivo_examen` y las claves aprobadas
  de `config` son idénticas a las de `d01bbf0f…` (comparadas por árbol sintáctico). El `.Rmd` sigue en `889d828e…`, el verificador en
  `bf11b0a4…` y la plantilla de 1 b en `290a3f79…`.
- **Verificado:**
  - `uso` aislado: 0 rojos y los 17 mutantes cazados por su sonda. Da exit 3, como toda corrida de una sola sección.
  - Semillero completo sin menú: exit 0 y 4 de 4 formatos.
  - PDF de 216 × 330 mm con las 10 preguntas en el orden I-IV.
  - Moodle con 100 preguntas y el gap de la Parte 1 en `MULTICHOICE_V`.
  - Cuestionario `WEBQUIZ_OK`: 234 fórmulas, 0 en TeX crudo y 0 recursos externos; mirado a 360 px.
  - Evidencia: `Cloze/salida/t6_1a/`.
- **Sin auditar (residuo):**
  - Ningún detractor revisó este cambio.
  - `uso` no vigila `generar_interactivo()`: si alguien la quita, ninguna sonda se pone en rojo.
  - Los botones del cuestionario no se probaron en un celular.
- La aprobación del 2026-11-09 citaba el semillero `d01bbf0f…`. Con «1a» el profesor autorizó este cambio, que solo agrega el
  cuestionario.
