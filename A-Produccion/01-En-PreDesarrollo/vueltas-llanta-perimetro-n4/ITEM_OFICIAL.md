# MAT-2026-07-028 — ítem oficial, transcripción verbatim

> **Por qué existe este archivo.** El recorte de `Originales/` es una imagen: no se puede citar con grep. Este archivo es su
> transcripción verbatim, cotejada el 2026-11-10 contra el recorte, contra la ficha y contra el cuadernillo oficial de origen.
>
> **Contrasta contra este archivo y, ante la duda, contra el recorte o el cuadernillo oficial; nunca contra tu recuerdo.**

## Fuente

- Simulacro: SAI «Nivelación 6» de grado 10 (Los Tres Editores), Sesión 2, Matemáticas, **pregunta impresa 28**, plana impresa 9,
  columna izquierda.
- Plana: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Simulacros-Decimo/julio-2026/Matematicas/Originales/pagina_009.jpg`,
  1576 × 2177 px, md5 `01898c5b66377359f96142ebb97772cd`. La copia de [`Originales/pagina_009.jpg`](Originales/pagina_009.jpg) tiene
  el mismo md5. La plana trae las preguntas 27 a 31; aquí solo cuenta la 28.
- Recorte de esa plana, sin reescalar, en [`Originales/`](Originales/), revisado a ojo el 2026-11-10. Caja en píxeles (izquierda,
  arriba, derecha, abajo):

| Archivo | Caja | Tamaño | md5 |
|---|---|---|---|
| `orig_pregunta_28.png` | (140, 515, 790, 878) | 650 × 363 | `34c19727c0abee6a3ebc3c24cb9aafb9` |

  Va del número impreso «28.» a la última fila de opciones y deja fuera la línea del rótulo del editor, que no es oficial.

- Ficha de alineación: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Simulacros-Decimo/julio-2026/Matematicas/Alineacion-curricular-de-items-Matematicas-Decimo-Jul-2026.md`,
  sección `### MAT-2026-07-028 — Expresión para el número de vueltas de una llanta`.
- Transcripción: `…/Matematicas/transcripcion-preguntas-matematicas-decimo-jul-2026.md`, `### Pregunta 28 (archivo: MAT-2026-07-028)`.
- **Origen oficial:** Evaluar para Avanzar 2023, Matemáticas 9.°, Cuadernillo 1, **pregunta 12** (`I_1461453`), con el mismo
  enunciado y las mismas letras y el mismo orden de opciones.
  - Cuadernillo: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Evaluar-Para-Avanzar/Grado-09/2023/Matematicas/2023-Cuadernillo-Matematicas-9-1.pdf`,
    página 7 del PDF, md5 `498f459b9ed62ad4177d0e10dd357640`. Mirada el 2026-11-10 renderizada a 110 ppp.
  - Guía: `…/Evaluar-Para-Avanzar/Grado-09/2023/Matematicas/2023-Guia-PC-Matematicas-9-1.pdf`, página 38 del PDF, md5
    `82635d3112d38519609b9db1d0fc3a78`.

## Errata del editor en la opción C

La plana imprime en la opción C una expresión que no corresponde a la situación. El cuadernillo oficial y la ficha traen la correcta:

| Fuente | Opción C | Consecuencia |
|---|---|---|
| Plana SAI (`orig_pregunta_28.png`) | 101/120 × 150 | No contiene el perímetro: con ella ninguna opción es correcta |
| EPA 2023, Cuadernillo 1, pág. 7 | (100/p) × 150 | Es la clave: «Respuesta correcta C» en la Guía, pág. 38 |
| Ficha MAT-2026-07-028 | (100/P) × 150 | Corregida por la ficha el 2026-10-17; clave C, triangulación 3/3 |

**No es una trampa deliberada.** El enunciado no pregunta por un error, y con la errata el ítem no tendría clave. La regla #24 H-2
(reproducir los errores deliberados del impreso) no aplica: la instancia canónica usa la C oficial, y **ninguna versión del `.Rmd`
muestra «101/120»**. La ficha anota además que los estudiantes de grado 10 presentaron en julio de 2026 la versión con la errata, así
que el resultado de ese simulacro en este ítem no mide lo que el ítem evalúa.

Notación: la plana escribe **P** mayúscula (en negrita, en el enunciado y en las opciones) y EPA escribe *p* minúscula. La ficha y la
transcripción usan P; la instancia canónica conserva la P de la plana.

## Enunciado (verbatim de la plana)

> La cantidad de vueltas que da una llanta para recorrer un metro es igual a 100 dividido entre el perímetro de la llanta, donde el
> perímetro de la llanta se expresa en centímetros.
>
> ¿Cuál de las siguientes expresiones permite determinar la cantidad de vueltas que debe dar una llanta de perímetro **P** (en cm),
> para recorrer 150 metros?

El texto de EPA es el mismo, con *p* en lugar de **P**.

## Opciones (con su letra oficial)

| Letra | Plana SAI | EPA 2023 | Instancia canónica |
|---|---|---|---|
| A | 100/150 | 100/150 | `\frac{100}{150}` |
| B | 150 × P/100 | 150 × p/100 | `150 \times \frac{P}{100}` |
| **C** | 101/120 × 150 (errata) | (100/p) × 150 | `\frac{100}{P} \times 150` |
| D | 100/P | 100/p | `\frac{100}{P}` |

**Clave oficial: C.** Guía oficial: «Respuesta correcta C». Ficha: triangulación 3/3 concordante (proveedor SAI, Claude y Gemini
sobre la plana).

Estructura impresa: las cuatro opciones en una cuadrícula de 2 × 2 (A y B arriba, C y D abajo), con las fracciones en vertical y
el signo «×». No hay figura: el ítem es texto y expresiones algebraicas.

## Resolución

Una vuelta de la llanta recorre P cm. Un metro son 100 cm, así que en un metro la llanta da 100/P vueltas. Para 150 metros:

(100/P) × 150 = 15 000/P vueltas.

Unidades: (100 cm/m) ÷ (P cm/vuelta) = vueltas por metro; por 150 m da vueltas.

Comprobación con P = 200 cm: C da 100/200 × 150 = 75 vueltas, y en efecto 150 m = 15 000 cm = 75 × 200 cm. Con el mismo P, A da
0,67, B da 300 y D da 0,5.

## Mecanismo de cada distractor

Cita literal de la Guía oficial (página 38 del PDF; las fracciones van en vertical en el impreso) y su fórmula, con d = 150 la
distancia en metros:

| Opción | Cita de la Guía | Fórmula |
|---|---|---|
| A | «Es posible que los estudiantes que eligen la opción A consideren que el perímetro de la llanta es 150 metros y calculan la cantidad de vueltas como 100/150.» | `100/d`: toma la distancia como si fuera el perímetro y deja de lado P |
| B | «Posiblemente, los estudiantes que eligen la opción B consideran que al dividir p entre 100 obtienen la distancia que recorre la llanta en una vuelta, y al multiplicar este resultado por 150, obtienen la cantidad de vueltas en 150 metros.» | `d × P/100`: multiplica la distancia por los metros de cada vuelta, en lugar de dividirla entre ellos |
| D | «Es posible que los estudiantes que eligen la opción D consideren que, como p es el perímetro de la llanta, la cantidad de vueltas que debe dar la llanta, de acuerdo con el enunciado, es 100/p sin considerar que se deben recorrer 150 metros.» | `100/P`: da las vueltas por metro y no multiplica por la distancia |

Justificación oficial de la clave: «Como el perímetro de la llanta es p, la cantidad de vueltas que da una llanta para recorrer 1 metro
es 100/p, y como son 150 metros, se necesitan 100/p × 150 vueltas para recorrer esta distancia.»

## Clasificación

Valores literales de la ficha. Competencia, Componente, Afirmación, Evidencia, Descriptor y Estándar se cotejaron el 2026-11-10 contra
el catálogo canónico (`/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Matematicas/catalogos-oficiales-mat/`):
coinciden los seis.

| Campo | Valor |
|---|---|
| Competencia | Formulación y ejecución |
| Componente | Numérico-variacional |
| Afirmación | Frente a un problema que involucre información cuantitativa, plantea e implementa estrategias que lleven a soluciones adecuadas. |
| Evidencia | Diseña planes para la solución de problemas que involucran información cuantitativa o esquemática. |
| Descriptor | D4.4 — Modela usando lenguaje algebraico información dada en lenguaje natural, tablas o representaciones geométricas. |
| Nivel | 4 |
| Grado sugerido | 6°-7° |
| Estándar asociado | Describo y represento situaciones de variación relacionando diferentes representaciones (diagramas, expresiones verbales generalizadas y tablas). (6°-7°, Pensamiento variacional) |
| Genérico | No |
| Contenido | Álgebra y Cálculo |
| Tema Específico | Modelación algebraica de una relación inversa |
| Aprendizaje | Plantea una expresión algebraica a partir de una relación descrita en lenguaje natural. |
| ¿Qué evalúa? | Traducir un enunciado verbal a una expresión algebraica con una variable. |
| Tarea | Dada una relación entre cantidades descrita en lenguaje natural, selecciona la expresión algebraica que permite calcular la cantidad pedida. |
| Descripción Breve | Plantea la expresión algebraica que relaciona perímetro, distancia recorrida y número de vueltas. |

> **Advertencia para quien edite este archivo.** `validar_literalidad_exextra.py --item` lee como «Campo | Valor» **toda** fila de
> dos columnas del archivo, y si una clave se repite gana la última. Cualquier otra tabla de este archivo debe tener tres columnas o
> más.

### Tres lecturas de la clasificación (discrepancia declarada)

| Dimensión | Ficha (se usa) | Rótulo del editor (no oficial) | Guía EPA 2023 (marco de 9.°) |
|---|---|---|---|
| Competencia | Formulación y ejecución | Interpretación y representación | Comunicación |
| Componente | Numérico-variacional | Numérico-variacional | Numérico - variacional |
| Evidencia | Diseña planes para la solución de problemas que involucran información cuantitativa o esquemática. | «A.: 1.2»: según la nota de procedencia de la ficha, la evidencia 2 de Interpretación y representación (`IyR_E2`: «Transforma la representación de una o más piezas de información.») | Relaciona un fenómeno, o situación de variación, en diversas estructuras con el lenguaje algebraico que lo representa. |
| Afirmación | Frente a un problema que involucre información cuantitativa, plantea e implementa estrategias que lleven a soluciones adecuadas. | — | Expresa una misma información en diferentes lenguajes: natural, simbólico o textual, en contextos matemáticos o aplicados. |
| Estándar | Describo y represento situaciones de variación relacionando diferentes representaciones (diagramas, expresiones verbales generalizadas y tablas). (6°-7°) | — | Modelo situaciones de variación con funciones polinómicas. |
| ¿Qué evalúa? | Traducir un enunciado verbal a una expresión algebraica con una variable. | — | La capacidad para identificar la expresión algebraica que modela una situación de variación. |

Rótulo impreso por el editor, literal: «A.: 1.2 M / Compt.: Numérico-variacional / Comp.: Interpretación y representación». Por no
ser oficial queda fuera del recorte. La ficha lo cuenta entre las 11 preguntas en que la competencia del editor difiere de la suya.

**Se usa la ficha** porque es la fuente que indicó el profesor al encargar el ejercicio (2026-11-10), su clasificación se hizo sin ver
el rótulo y con los catálogos oficiales de Saber 11, y el paso 10 compara los `exextra` del `.Rmd` con la tabla «Campo | Valor» de
este archivo. Las otras dos lecturas no se usan ni para el nombre del archivo ni para los `exextra`. Si el análisis del paso 1
contradice la ficha, se muestra al profesor en la pausa 1; no se sustituye.

## Justificaciones metacognitivas del ítem (de la ficha)

- **A**: El estudiante divide 100 entre 150 y deja de lado el perímetro P, que es la variable del problema.
- **B**: El estudiante multiplica 150 por P/100, invirtiendo la relación: eso da metros por vuelta, no vueltas.
- **C**: Respuesta correcta. Vueltas por metro = 100/P; para 150 m se multiplica por 150.
- **D**: El estudiante da solo las vueltas para un metro y olvida multiplicar por los 150 metros.

## Regla de uso

La **instancia canónica** del `.Rmd` reproduce el enunciado de la plana y las cuatro expresiones, con la C oficial y la clave en el
**contenido** de C; la letra no se fija. Sus tres distractores son los tres mecanismos de la Guía. Las **demás versiones** varían los
datos y el contexto (por ejemplo, la distancia y el objeto que rueda) y conservan la tarea y las cuatro opciones como expresiones
algebraicas; sus tres distractores se sortean de un banco de seis errores, los tres de la Guía y tres nuevos (decisión del profesor del
2026-11-10, abajo). El 100 es la conversión 1 m = 100 cm: si una versión cambia las unidades, el enunciado y la constante cambian
juntos. La capa metacognitiva va en la Solution, no en las opciones.

## Decisiones del profesor

**2026-11-10, pausa 1 del `orquestador-schoice`** (paso 2, Flujo B, y pre-flight 20, tamaño del banco de errores). Respuesta literal,
transmitida por el coordinador: **«Sin figura» · «Mezclar con errores nuevos»**.

- **Flujo B: no requerido.** El ítem es texto y expresiones algebraicas; el ejercicio queda sin figura, como el original.
- **Errores de las versiones que no son la canónica:** se sortean 3 de un banco de 6, con d la distancia y la constante 100 de la
  conversión (o la que corresponda a las unidades sorteadas):
  - los tres de la Guía: `100/d` (olvida el perímetro), `d × P/100` (invierte la relación) y `100/P` (olvida la distancia);
  - tres nuevos, propuestos por el orquestador: `d/P` (no convierte los metros a centímetros), `100/(P × d)` (divide entre la
    distancia en lugar de multiplicar) y `(100/P) × (100 × d)` (convierte dos veces).
- **Instancia canónica:** con probabilidad cercana a 0,10, lleva los tres de la Guía, como el impreso.
- **Consecuencias que el profesor vio al decidir:** más variedad de errores; algunas versiones son más difíciles que el original,
  porque `d/P` también tiene la P abajo y la distancia arriba y obliga a pensar en las unidades; los tres errores nuevos no tienen
  respaldo en la Guía, así que su explicación en la Solution se apoya solo en el razonamiento.
- **Opción descartada:** los tres errores de la Guía en todas las versiones, con los nuevos solo en la explicación. Habría exigido
  firmar una excepción a la regla #1.
- Sustituye, para las versiones que no son la canónica, la instrucción de fidelidad del encargo («la misma estructura de error del
  original»). Las opciones siguen siendo expresiones algebraicas, nunca descripciones de errores.

**Decisiones posteriores** (tras las FASE 2C r1, r2 y r3, y la aprobación con firma en la pausa 3 del 2026-11-10): en
[`HANDOFF.md`](HANDOFF.md), § «Decisiones del profesor (literales)», y en [`salida/tabla_firma_p7.md`](salida/tabla_firma_p7.md),
§ «Firma del profesor». El tratamiento de la errata de la opción C quedó como aclaración de la regla #24 H-2, en
`.claude/docs/reglas/hermes-imagenes-icfes.md`.
