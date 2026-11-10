# MAT-2026-07-014 — ítem oficial, transcripción verbatim

> **Por qué existe este archivo.** Los recortes de `Originales/` son imágenes: no se pueden citar con
> grep. Este archivo es su transcripción verbatim, cotejada el 2026-11-06 contra los recortes y contra el
> cuadernillo oficial.
>
> **Contrasta contra este archivo y, ante la duda, contra el recorte; nunca contra tu recuerdo.**

## Fuente

- Simulacro: SAI «Nivelación 6» de grado 10 (Los Tres Editores), Sesión 1, Matemáticas, **pregunta impresa
  14**, plana impresa 5, columna izquierda.
- Plana: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Simulacros-Decimo/julio-2026/Matematicas/Originales/pagina_004.jpg`,
  1464 × 2022 px, md5 `189a6169d30ef3983fff7e7ad3a0892d`.
- Recortes de esa plana, sin reescalar, en [`Originales/`](Originales/), revisados a ojo el 2026-11-06. Cajas en
  píxeles (izquierda, arriba, derecha, abajo):

| Archivo | Caja | Tamaño | md5 |
|---|---|---|---|
| `orig_pregunta_14.png` | (125, 315, 712, 1178) | 587 × 863 | `62282f5fd5ec431d4e15aca77dd84429` |
| `orig_opcion_A.png` | (173, 667, 415, 915) | 242 × 248 | `e236b5419c225610a55988f312c7b5ee` |
| `orig_opcion_B.png` | (448, 667, 688, 915) | 240 × 248 | `514646e1db8c7100f26074f7de781cb0` |
| `orig_opcion_C.png` | (173, 924, 415, 1174) | 242 × 250 | `168a04deb92cfc95637b2c3dfc7b08f2` |
| `orig_opcion_D.png` | (449, 925, 689, 1174) | 240 × 249 | `a0ca2472195ee405329bf8f3afee70a9` |

Los cuatro recortes por opción se rehicieron el 2026-11-06, tras el paso 2b: las primeras cajas de A y C empezaban en x = 180 y
cortaban la línea izquierda del marco (x = 176-179). Cada caja actual es la extensión medida de las cuatro líneas del marco más 3 px.
`geom_venn.json` se midió sobre las primeras cajas, con origen en la esquina del marco. Sigue vigente: las cajas actuales conservan
todo el contenido de las anteriores (solo D pierde una fila, y=924, sin tinta), y `instrumento.md5` no cambió.

- Ficha de alineación: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Simulacros-Decimo/julio-2026/Matematicas/Alineacion-curricular-de-items-Matematicas-Decimo-Jul-2026.md`,
  sección `### MAT-2026-07-014 — Diagrama de Venn de tres conjuntos`.
- Transcripción: `…/Matematicas/transcripcion-preguntas-matematicas-decimo-jul-2026.md`, `### Pregunta 14`.
- **Origen oficial:** Evaluar para Avanzar 2023, Saber 10.°, Cuadernillo 2, **pregunta 8** (`I_1957870`), con las mismas
  letras y las mismas cifras en las cuatro opciones.
  - Cuadernillo: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Evaluar-Para-Avanzar/Grado-10/2023/Matematicas/2023-Cuadernillo-Matematicas-10-2.pdf`,
    página 5 del PDF.
  - Guía: `…/Evaluar-Para-Avanzar/Grado-10/2023/Matematicas/2023-Guía-Matematicas-10-2.pdf`, páginas 30-31 del PDF.

**No usar como fuente ni como referencia** las figuras `q014-*.png` de `…/Matematicas/imagenes/` ni de
`…/Matematicas/auto_gen/multi/`: están regeneradas con IA (nano-banana-pro) y su trazo varía entre opciones (A y B más
gruesos, C y D más finos, según su propia `FIGURA-META`).

## Enunciado (verbatim)

> Para realizar una jornada de juegos con los empleados de una empresa, se recogen los siguientes datos con el fin de
> organizar a las personas de acuerdo con sus preferencias en cuanto a tres juegos de mesa.
>
> - 3 personas jugarían parqués y Monopoly, pero no bingo.
> - 10 personas jugarían parqués y Monopoly, 8 personas jugarían Monopoly y bingo, y 7 personas jugarían parqués y bingo.
> - 17 personas jugarían parqués, 13 personas jugarían bingo y 20 personas jugarían Monopoly.
>
> De acuerdo con lo anterior, ¿cuál diagrama de Venn organiza los datos de manera adecuada?

## Opciones (verbatim, con su letra oficial)

Las cuatro son diagramas de Venn. Sus regiones, cotejadas contra la plana y el cuadernillo:

| Letra | Solo parqués | Parqués y Monopoly | Solo Monopoly | Centro | Parqués y bingo | Monopoly y bingo | Solo bingo | Suma |
|---|---|---|---|---|---|---|---|---|
| A | 0 | 10 | 2 | 3 | 4 | 5 | 1 | 25 |
| B | 17 | 3 | 20 | 7 | 0 | 1 | 13 | 61 |
| **C** | **7** | **3** | **9** | **7** | **0** | **1** | **5** | **32** |
| D | 17 | 10 | 20 | 3 | 7 | 8 | 13 | 78 |

**Clave oficial: C.** Guía oficial: «Respuesta correcta C». Ficha: triangulación 3/3 concordante (proveedor SAI, Claude y
Gemini sobre la plana).

Estructura impresa, que la tabla no puede mostrar:

- Marco rectangular negro por opción, con tres círculos del mismo radio: Parqués arriba a la izquierda, Monopoly arriba a
  la derecha, Bingo abajo al centro.
- Rótulos en negrita, fuera de los círculos: «Parqués» en la esquina superior izquierda, «Monopoly» en la superior
  derecha, «Bingo» en la inferior derecha.
- Color del trazo: Parqués azul, Monopoly rojo, Bingo verde en el cuadernillo oficial; azul verdoso, granate y verde en el
  escaneo de la plana. Fondo blanco, sin relleno.
- Ninguna opción muestra un total ni un número fuera de los círculos.

## Resolución

Datos: `a = 3` (parqués y Monopoly, pero no bingo), `b = 10` (parqués y Monopoly), `c = 8` (Monopoly y bingo), `d = 7`
(parqués y bingo), `P = 17`, `B = 13`, `M = 20`.

| Región | Clave | Valor |
|---|---|---|
| Centro `z` | `b − a` | 10 − 3 = 7 |
| Parqués y Monopoly, sin bingo | `a` | 3 |
| Monopoly y bingo, sin parqués | `c − z` | 8 − 7 = 1 |
| Parqués y bingo, sin Monopoly | `d − z` | 7 − 7 = 0 |
| Solo parqués | `P − a − d` | 17 − 3 − 7 = 7 |
| Solo Monopoly | `M − a − c` | 20 − 3 − 8 = 9 |
| Solo bingo | `B − d − c + z` | 13 − 7 − 8 + 7 = 5 |

Los siete datos determinan las siete regiones: la solución es única.

## Mecanismo de cada distractor

Cita literal de la Guía oficial (páginas 30-31 del PDF) y su fórmula, comprobada contra el impreso (orden de regiones de
la tabla de opciones):

- **A.** «Es posible que los estudiantes que eligen la opción A crean que la frase "3 personas jugarían Parqués y Monopoly
  pero no Bingo" corresponda al dato del centro del diagrama y que la frase "10 personas jugarían Parqués y Monopoly"
  corresponda a la zona que falta para completar la intersección de los conjuntos de las personas que jugarían Parqués y de
  las personas que jugarían Monopoly, y en el resto del diagrama realicen las operaciones usuales.»
  Fórmula: `P − b − d, b, M − b − c, a, d − a, c − a, B − d − c + a`.
- **B.** «Es posible que los estudiantes que eligen la opción B encuentren de manera correcta la zona central y de las
  intersecciones de dos en dos, pero que confundan el conjunto de las personas que jugarían X juego con el conjunto de las
  personas que jugarían únicamente X juego.»
  Fórmula: `P, a, M, z, d − z, c − z, B`.
- **D.** «Es posible que los estudiantes que eligen la opción D crean que la frase "3 personas jugarían Parqués y Monopoly
  pero no Bingo" corresponda al dato del centro del diagrama y que la frase "10 personas jugarían Parqués y Monopoly"
  corresponda a la zona que falta para completar la intersección de los conjuntos de las personas que jugarían Parqués y de
  las personas que jugarían Monopoly, y en el resto del diagrama se coloque el dato de cada frase del enunciado en una zona
  del diagrama sin realizar ninguna operación.»
  Fórmula: `P, b, M, a, d, c, B`.

Datos que cumple cada opción, de los siete:

| Opción | Cumple | Falla |
|---|---|---|
| A | 5: `c`, `d`, `P`, `B`, `M` | `a` (muestra 10) y `b` (suma 13) |
| B | 4: `a`, `b`, `c`, `d` | `P` (suma 27), `M` (31) y `B` (21) |
| **C** | **7** | ninguno |
| D | 0 | `a` (10), `b` (13), `c` (11), `d` (10), `P` (37), `M` (41) y `B` (31) |

**A conserva los tres totales por construcción**, no por coincidencia: quien solo revise los totales de los círculos no la
descarta. Esa es la dificultad del ítem.

## Clasificación

Valores literales de la ficha. Competencia, Afirmación, Evidencia, Descriptor y Estándar se cotejaron el 2026-11-06 contra
el catálogo canónico (`/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Matematicas/catalogos-oficiales-mat/`):
coinciden los cinco.

| Campo | Valor |
|---|---|
| Competencia | Interpretación y representación |
| Componente | Aleatorio |
| Afirmación | Comprende y transforma la información cuantitativa y esquemática presentada en distintos formatos. |
| Evidencia | Transforma la representación de una o más piezas de información. |
| Descriptor | D3.11 — Identifica información relevante cuando el tipo de registro contiene información de más de tres categorías. |
| Nivel | 3 |
| Grado sugerido | 6°-7° |
| Estándar asociado | Reconozco la relación entre un conjunto de datos y su representación. (6°-7°, Pensamiento aleatorio) |
| Genérico | Sí |
| Contenido | Estadística |
| Tema Específico | Diagramas de Venn de tres conjuntos |
| Aprendizaje | Transforma datos verbales sobre tres conjuntos en un diagrama de Venn con sus regiones. |
| ¿Qué evalúa? | Capacidad de organizar datos de intersección y totales en un diagrama de Venn. |
| Tarea | A partir de un texto con cantidades de intersecciones y totales de tres grupos, seleccionar el diagrama de Venn que distribuye correctamente los datos. |
| Descripción Breve | Elige el diagrama de Venn que organiza las preferencias descritas en un texto. |

**Decisión del profesor del 2026-11-06, antes del paso 1: Estándar de la ficha.** La Guía oficial trae otro: «Interpreto y
comparo resultados de estudios con información estadística provenientes de medios de comunicación.» (Aleatorio 10-11). Los
dos son literales del EBC. Se usa el de la ficha, con Grado sugerido 6°-7°, porque describe la tarea de pasar datos a un
diagrama. La discrepancia queda declarada y no se vuelve a preguntar.

La Guía coincide con la ficha en la Afirmación y la Evidencia. Sus etiquetas «Interpretación.» y «Estadística.» son las
versiones cortas de Interpretación y representación y de Aleatorio. Su «¿Qué evalúa?» dice: «La capacidad del estudiante de
construir el diagrama de Venn de una encuesta a partir de información textual (dada en lenguaje natural).»

**El rótulo que imprime el editor no es oficial y no se usa:** «A.: 1.2 M / Compt.: Aleatorio / Comp.: Interpretación y
representación». Por eso queda fuera de los recortes.

## Justificaciones metacognitivas del ítem (de la ficha)

- **A**: Puse el 3 («parqués y Monopoly, pero no bingo») en el centro y el 10 completo en la zona de solo parqués y Monopoly, sin restarle el centro; los totales de cada juego cuadran (17, 20 y 13), pero parqués y Monopoly juntos suman 13, no 10.
- **B**: Dejé los totales (17, 20, 13) en las zonas de un solo juego sin restar las intersecciones.
- **C**: Respuesta correcta. Centro 10 − 3 = 7; solo MB 1; solo PB 0; solo parqués 7, solo Monopoly 9, solo bingo 5; y los totales dan 17, 20 y 13.
- **D**: Escribí 10, 8 y 7 en las intersecciones dobles y 3 en el centro, sin descontar el centro ni los totales.

## Regla de uso

La **instancia canónica** del `.Rmd` reproduce el enunciado y los cuatro diagramas verbatim, con la clave en el contenido de
C; la letra no se fija. Las **demás versiones** varían los datos y el contexto, y conservan la tarea, la geometría y los tres
mecanismos de error. Invariantes con sus guardias: [`.claude/CLAUDE.md`](.claude/CLAUDE.md).
