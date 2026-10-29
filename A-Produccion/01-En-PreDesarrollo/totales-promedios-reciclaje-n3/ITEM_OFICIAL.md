# MAT-2026-07-050 — ítem oficial, transcripción verbatim

> **Por qué existe este archivo.** El recorte `originales/orig_pregunta_50.png` es una imagen: no se
> puede citar con grep. Este archivo es su transcripción verbatim, cotejada contra el recorte y contra
> la plana el 2026-10-29. Sin ella, los agentes razonan de memoria y rompen la fidelidad al intentar
> «arreglar» el ítem, como pasó con MAT-2026-1-010 (`../informacion-insuficiente-lote-n4/ITEM_OFICIAL.md`).
>
> **Contrasta contra este archivo y, ante la duda, contra el recorte; nunca contra tu recuerdo.**

## Fuente

- Cuadernillo: SAI «Prueba Nivelación 6», grado décimo (Los Tres Editores S. A. S.), Sesión 2,
  Matemáticas, **pregunta impresa 50**, la última de Matemáticas de esa sesión. Aplicado en papel el
  2026-07-22 a 10A y 10B.
- Recorte de trabajo: [`originales/orig_pregunta_50.png`](originales/orig_pregunta_50.png), 2234 × 814 px,
  md5 `d68422f08964c57b7561a23db84abe6e`. Llegó al subproyecto el 2026-10-29 como `50/originales/50.png`;
  se renombró sin tocar el contenido (mismo md5). Va desde «50.» hasta el final de la opción D y deja
  fuera el rótulo del editor. Trae un artefacto de captura que no es contenido: un cursor de texto en forma
  de «I» junto a «¿cuál» (no está en la plana). Es una **ampliación**, no un recorte nativo: su ancho supera el de la plana
  completa (1546 px) y el de la mitad del escaneo nativo (página 7 del PDF: 3020 × 1980 px el escaneo doble).
- Plana: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Simulacros-Decimo/julio-2026/Matematicas/Originales/pagina_014.jpg`
  (1546 × 2135 px, 300 ppi), página impresa **14**. La pregunta 50 ocupa la parte superior; debajo empieza
  «CIENCIAS NATURALES II» con la pregunta 51. Revisada a ojo el 2026-10-29, anclada por el número impreso
  «50.»: el texto, la tabla y las opciones coinciden con el recorte.
- PDF: `…/julio-2026/Originales/Sesion-02.pdf`, md5 `8106c5741417667ebc4f4f14cf902fe5` (comprobado el
  2026-10-29), 15 escaneos de doble página. Según `…/julio-2026/Originales/INDICE.md`, el escaneo *k* trae
  las páginas impresas 2*k* (izquierda) y 2*k*+1 (derecha): la página impresa 14 es la **mitad izquierda de
  la página 7 del PDF**. No se copia aquí (`.gitignore:469` excluye los PDF de `01-En-PreDesarrollo/`).
- Ficha de alineación: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Simulacros-Decimo/julio-2026/Matematicas/Alineacion-curricular-de-items-Matematicas-Decimo-Jul-2026.md`,
  sección `### MAT-2026-07-050 — Comparación de totales a partir de promedios` (≈ línea 3197).
- Transcripción: `…/Matematicas/transcripcion-preguntas-matematicas-decimo-jul-2026.md`,
  `### Pregunta 50 (archivo: MAT-2026-07-050)` (≈ línea 1503).
- Origen del ítem (según la ficha): reciclado, contención 0.83, de Evaluar para Avanzar, grado 9, 2022
  (`Alineacion-curricular-de-items/Simulacros/Evaluar-Para-Avanzar/Grado-09/2022/Matematicas/2022-Matematicas-9-1.pdf`).

## Enunciado (verbatim)

> En un colegio se realiza un concurso de reciclaje que premiará a la sede que recoja la mayor cantidad
> de papel. La tabla muestra la cantidad de cursos que participaron y el promedio de papel recogido por
> curso en la sede primaria y en la sede secundaria.

*(tabla)*

> De acuerdo con la información anterior, ¿cuál de las siguientes afirmaciones es VERDADERA?

## Tabla (celda a celda, cotejada contra el recorte el 2026-10-29)

| Sede | Cantidad de cursos | Promedio de papel recolectado por curso |
|---|---|---|
| Primaria | 5 cursos | 40 kilos |
| Secundaria | 4 cursos | 45 kilos |

Estructura impresa:

- Tres columnas y dos filas de datos, **sin celdas combinadas**. El encabezado «Promedio de papel
  recolectado por curso» ocupa dos renglones dentro de su celda. Todo el texto va centrado.
- Encabezados en negrita y color naranja oscuro; «Primaria» y «Secundaria» en negrita negra; los valores
  en tipo normal. Bordes finos oscuros sobre fondo blanco.
- Las unidades van dentro de cada celda («5 cursos», «40 kilos»), no en el encabezado.

Rarezas del impreso que se conservan tal cual (regla #24 H-2: no se normaliza):

- El enunciado dice «papel **recogido** por curso» y el encabezado, «papel **recolectado** por curso».
- La opción A dice «la sede **de** secundaria»; las demás, «la sede secundaria».
- La opción C dice «ganó porque», sin «el concurso»; A, B y D dicen «ganó el concurso porque».

## Opciones (verbatim, con su letra oficial)

| Letra | Texto |
|---|---|
| A | La sede primaria ganó el concurso porque recogió 40 kilos más que la sede de secundaria. |
| **B** | **La sede primaria ganó el concurso porque participaron más cursos y aunque el promedio fue menor, el total fue de 20 kilos más.** |
| C | La sede secundaria ganó porque, aunque participaron menos cursos, tuvo mayor promedio con 5 kilos de más. |
| D | La sede secundaria ganó el concurso porque en ella hubo un curso que recogió más que los otros, aumentando la cantidad de papel recogida en todo el colegio. |

**Clave oficial: B.** Triangulación 3/3 concordante, según la ficha: proveedor SAI (Sábana), Claude
(visión, plana) y Gemini 3.8 Flash (plana). Recalculada el 2026-10-29:

| Sede | Cursos | Promedio | Total |
|---|---|---|---|
| Primaria | 5 | 40 kg | 5 × 40 = **200 kg** |
| Secundaria | 4 | 45 kg | 4 × 45 = **180 kg** |

Gana la primaria por 200 − 180 = **20 kg**.

| Opción | Qué afirma | ¿Verdadera? | Por qué |
|---|---|---|---|
| A | Ganó la primaria, por 40 kg | no | La primaria sí ganó, pero por 20 kg; 40 es el promedio de la primaria, no la diferencia de totales |
| **B** | Ganó la primaria, con más cursos y menor promedio, por 20 kg | **sí** | 5 > 4 cursos, 40 < 45 kg de promedio y 200 − 180 = 20 kg |
| C | Ganó la secundaria, por su mayor promedio (5 kg más) | no | Su promedio sí es 5 kg mayor (45 − 40), pero su total es menor: no ganó |
| D | Ganó la secundaria, por un curso destacado | no | No ganó, y la tabla solo da promedios por sede: no informa nada sobre cursos individuales |

Obsérvese la estructura: **A y B** atribuyen el triunfo a la sede que de verdad gana (A con una cifra
falsa, B con la correcta); **C y D**, a la que pierde (C con una razón que sí está en la tabla, el mayor
promedio; D con una razón que la tabla no sustenta). Por eso ni el nombre de la sede ni «la opción
distinta de las otras tres» delatan la clave. La trampa del ítem: la sede con mayor promedio **pierde**,
porque tiene menos cursos. Cualquier instancia canónica debe reproducir esa estructura tal cual.

## Clasificación oficial (para los `exextra[...]`)

Valores literales de la ficha. Afirmación, Evidencia (`Arg_E1`), Descriptor (D3.10, índice oficial 10 del
Nivel 3) y Estándar (`Aleatorio_6-7[7]`) se cotejaron carácter a carácter contra el catálogo canónico
(`/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Matematicas/catalogos-oficiales-mat/`:
`evidencias-mat.json`, `niveles-mat.json` y `estandares-mat-ebc.json`) el 2026-10-29: coinciden los cuatro.

| Campo | Valor literal |
|---|---|
| Competencia | Argumentación |
| Componente | Aleatorio |
| Afirmación | Valida procedimientos y estrategias matemáticas utilizadas para dar solución a problemas. |
| Evidencia | Plantea afirmaciones que sustentan o refutan una interpretación dada a la información disponible en el marco de la solución de un problema. |
| Descriptor | D3.10 — Justifica afirmaciones utilizando planteamientos y operaciones aritméticas o haciendo uso directo de un concepto, es decir, a partir de un único argumento. |
| Nivel | 3 |
| Grado sugerido | 6°-7° |
| Estándar asociado | Predigo y justifico razonamientos y conclusiones usando información estadística. (6°-7°, Pensamiento aleatorio) |
| Genérico | Sí |
| Contenido | Estadística |
| Tema Específico | Promedio y total de datos |
| Aprendizaje | Decide cuál afirmación es verdadera calculando totales a partir de promedios y cantidades. |
| ¿Qué evalúa? | Justificar una conclusión comparando totales obtenidos con operaciones aritméticas. |
| Tarea | A partir de una tabla con el número de grupos y el promedio por grupo de dos conjuntos, determinar cuál afirmación es verdadera calculando y comparando los totales. |
| Descripción Breve | Decide qué sede ganó comparando totales calculados con cantidad de cursos y promedios. |

**El rótulo que imprime el editor no es oficial y no se usa:** «A.: 3.3 M / Compt.: Aleatorio /
Comp.: Argumentación». Coincide con la ficha en componente y competencia; la ficha se clasificó sin verlo.

## Justificaciones metacognitivas del ítem oficial (de la ficha)

- **A**: El estudiante resta los promedios o toma un dato aislado (40) como diferencia entre totales, sin calcular los totales.
- **B**: Respuesta correcta. Los totales son 200 y 180 kilos: primaria recogió 20 kilos más.
- **C**: El estudiante compara solo los promedios (45 vs 40) y no considera que el número de cursos es distinto.
- **D**: El estudiante inventa una explicación sobre un curso destacado que no sale de la tabla, pues solo hay promedios por sede.

Nota sobre A: la primera lectura de la ficha, «resta los promedios», da 5 kg (la cifra que cita C), no
40. El 40 del impreso sale de la segunda, «toma un dato aislado»: es el promedio de la sede ganadora.

## Regla de uso

La **instancia canónica** del `.Rmd` (probabilidad 0,10, L-5) reproduce el enunciado, la tabla (celda a
celda) y las cuatro opciones **verbatim**, con la clave en el contenido de B (la letra la asigna
`exshuffle: TRUE`) y sin añadidos de contexto que el cuadernillo no menciona. Conserva la trampa del
impreso: el conjunto de mayor promedio pierde porque tiene menos grupos. Las **demás versiones** varían
los datos y el contexto, siempre que conserven la estructura: dos conjuntos (las sedes) con su cantidad de
grupos (los cursos) y el promedio por grupo, en sentidos opuestos; la pregunta por la afirmación
VERDADERA, y el reparto 2 + 2 de las afirmaciones. Por decisión del profesor del 2026-10-29 (L-3), en
ellas gana unas veces el conjunto de más grupos y otras el de mayor promedio. Invariantes con sus
guardias: [`.claude/CLAUDE.md`](.claude/CLAUDE.md).
