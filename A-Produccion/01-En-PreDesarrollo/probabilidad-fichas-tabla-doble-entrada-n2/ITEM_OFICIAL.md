# MAT-2026-07-032 — ítem oficial, transcripción verbatim

> **Por qué existe este archivo.** La fuente está en este directorio: `Sesion-02.pdf` es copia byte a
> byte del cuadernillo (md5 abajo), y `graficador/originales/` guarda la plana y los recortes extraídos
> de esa copia. Pero el PDF es un escaneo sin texto embebido, así que no se puede citar con grep: este
> archivo es su transcripción verbatim, cotejada contra la plana. Sin ella, los agentes razonan de
> memoria y rompen la fidelidad al intentar «arreglarla», como pasó con MAT-2026-1-010
> (`../informacion-insuficiente-lote-n4/ITEM_OFICIAL.md`).
>
> **Contrasta contra este archivo y, ante la duda, contra el recorte; nunca contra tu recuerdo.**

## Fuente

- Cuadernillo: SAI «Nivelación 6» de grado 10 (Los Tres Editores), Sesión 2, Matemáticas,
  **pregunta impresa 32**. Aplicado en papel el 2026-07-22 a 10A y 10B.
- PDF: [`Sesion-02.pdf`](Sesion-02.pdf), en este directorio. Copia byte a byte del original de
  Todo-Pajaro (`…/Simulacros-Decimo/julio-2026/Originales/Sesion-02.pdf`): md5
  `8106c5741417667ebc4f4f14cf902fe5` en los dos, comprobado el 2026-10-26. Escaneo de doble página sin
  texto embebido: el escaneo *k* trae las páginas impresas 2*k* (izquierda) y 2*k*+1 (derecha), así que
  la página impresa 10 es la **mitad izquierda de la página 5 del PDF**. No se versiona
  (`.gitignore:469` excluye los PDF de `01-En-PreDesarrollo/`); los recortes PNG de abajo sí, por LFS.
- Plana y recortes, extraídos de esa copia (`pdfimages -f 5 -l 5 -j`: imagen nativa de 2844 × 1864 px,
  247 ppi), anclados por el número impreso «32.» y revisados a ojo el 2026-10-26:
  - `graficador/originales/orig_plana_010.png`: mitad izquierda, la página impresa 10 completa.
  - `graficador/originales/orig_pregunta_32.png`: la pregunta, desde «32.» hasta el rótulo «A: 1.2 M…».
  - `graficador/originales/orig_enunciado_tabla.png`: solo la tabla.
- Plana equivalente en Todo-Pajaro, solo como referencia:
  `…/julio-2026/Matematicas/Originales/pagina_010.jpg`.
- Ficha de alineación: `/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Simulacros/Simulacros-Decimo/julio-2026/Matematicas/Alineacion-curricular-de-items-Matematicas-Decimo-Jul-2026.md`,
  sección `### MAT-2026-07-032 — Comparación de probabilidades con tabla de doble entrada` (≈ línea 2372).
- Transcripción: `…/Matematicas/transcripcion-preguntas-matematicas-decimo-jul-2026.md`, `### Pregunta 32`.
- Origen del ítem (según la ficha): reciclado, contención 0.80, de Evaluar para Avanzar, grado 9, 2023
  (`Alineacion-curricular-de-items/Simulacros/Evaluar-Para-Avanzar/Grado-09/2023/Matematicas/2023-Matematicas 9-1.pdf`).

## Enunciado (verbatim)

> Eduardo puso en una bolsa las fichas de un antiguo juego chino, en el que se usan fichas blancas y
> fichas negras de igual tamaño. Cada ficha tiene dibujada una figura geométrica y está clasificada en
> una categoría, par o impar. En la tabla se indica la cantidad de fichas de cada tipo que puso en la
> bolsa.

*(tabla)*

> Si se eligen al azar dos fichas de la bolsa, ¿cuáles de las siguientes fichas tienen la misma
> probabilidad de ser elegidas?

## Tabla (celda a celda, cotejada contra la plana el 2026-10-26)

| Color | Categoría | Círculo | Cuadrado |
|---|---|---|---|
| Blanca | Par | 12 | 11 |
| Blanca | Impar | 8 | 15 |
| Negra | Par | 16 | 20 |
| Negra | Impar | 10 | 8 |

Estructura impresa, que la tabla pipe de arriba no puede mostrar:

- Una fila de encabezado superior con **«Figura geométrica»** combinada sobre las columnas «Círculo» y
  «Cuadrado»; encima de «Color» y «Categoría» no hay celda.
- **«Blanca»** combina las dos filas Par e Impar de ese color; **«Negra»**, las dos suyas.
- Encabezados («Figura geométrica», «Color», «Categoría», «Círculo», «Cuadrado») en negrita, color
  magenta oscuro sobre fondo rosado; «Blanca» y «Negra» en negrita negra sobre blanco; «Par», «Impar» y
  los números en tipo normal. Bordes magenta.
- Total de fichas: 12 + 11 + 8 + 15 + 16 + 20 + 10 + 8 = **100**.

## Opciones (verbatim, con su letra oficial)

| Letra | Texto |
|---|---|
| A | Una ficha blanca con un círculo y una ficha negra con un cuadrado. |
| **B** | **Una ficha blanca con un cuadrado y una ficha negra con un círculo.** |
| C | Una ficha blanca de categoría par con un cuadrado y una ficha negra de categoría impar con un círculo. |
| D | Una ficha blanca de categoría impar con un círculo y una ficha negra de categoría par con un cuadrado. |

**Clave oficial: B.** Triangulación 3/3 concordante: proveedor SAI (Sábana), Claude (visión, plana) y
Gemini 3.8 Flash (plana).

| Opción | Primer tipo | Segundo tipo | ¿Iguales? |
|---|---|---|---|
| A | blanca con círculo: 12 + 8 = 20 | negra con cuadrado: 20 + 8 = 28 | no |
| **B** | blanca con cuadrado: 11 + 15 = 26 | negra con círculo: 16 + 10 = 26 | **sí** |
| C | blanca par con cuadrado: 11 | negra impar con círculo: 10 | no |
| D | blanca impar con círculo: 8 | negra par con cuadrado: 20 | no |

Obsérvese la estructura: **A y B** suman dos celdas por tipo (color × figura, sin categoría); **C y D**
toman una sola celda (color × categoría × figura) y dicen «de categoría…». La clave es una de las dos
cortas. Cualquier instancia canónica debe reproducir esa estructura tal cual.

## Clasificación oficial (para los `exextra[...]`)

Valores literales de la ficha. Afirmación, Evidencia (FyE_E3), Descriptor (D2.3) y Estándar se cotejaron
carácter a carácter contra el catálogo canónico
(`/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Matematicas/catalogos-oficiales-mat/`)
el 2026-10-26: coinciden los cuatro.

| Campo | Valor literal |
|---|---|
| Competencia | Formulación y ejecución |
| Componente | Aleatorio |
| Afirmación | Frente a un problema que involucre información cuantitativa, plantea e implementa estrategias que lleven a soluciones adecuadas. |
| Evidencia | Resuelve un problema que involucra información cuantitativa o esquemática. |
| Descriptor | D2.3 — Compara la probabilidad de eventos simples (casos favorables/casos posibles) cuando los casos posibles son los mismos en ambos eventos y en contextos similares a los presentados en el aula. |
| Nivel | 2 |
| Grado sugerido | 6°-7° |
| Estándar asociado | Resuelvo y formulo problemas a partir de un conjunto de datos presentados en tablas, diagramas de barras, diagramas circulares. (6°-7°, Pensamiento aleatorio) |
| Genérico | Sí |
| Contenido | Estadística |
| Tema Específico | Probabilidad en tablas de doble entrada |
| Aprendizaje | Compara la probabilidad de eventos simples a partir de una tabla de doble entrada con los mismos casos posibles. |
| ¿Qué evalúa? | Sumar frecuencias de una tabla y comparar la probabilidad de eventos con el mismo total de casos posibles. |
| Tarea | A partir de una tabla de frecuencias con varias clasificaciones, comparar la probabilidad de eventos simples y reconocer cuáles son equiprobables. |
| Descripción Breve | Identifica qué dos tipos de fichas tienen igual probabilidad usando una tabla con celdas combinadas. |

**El rótulo que imprime el editor no es oficial y no se usa:** «A.: 1.2 M / Compt.: Aleatorio /
Comp.: Interpretación y representación». La competencia es la de la ficha (Formulación y ejecución).

## Justificaciones metacognitivas del ítem oficial (de la ficha)

- **A**: El estudiante cree que las fichas "opuestas" en color y figura deben coincidir; las frecuencias son 20 y 28.
- **B**: Respuesta correcta. Las blancas con cuadrado son 11 + 15 = 26 y las negras con círculo son 16 + 10 = 26; como el total de fichas es el mismo, tienen igual probabilidad.
- **C**: El estudiante lee solo las celdas de una categoría y ve valores cercanos (11 y 10), pero no son iguales.
- **D**: El estudiante toma las celdas que se cruzan en la tabla sin sumar las categorías: 8 y 20 no son iguales.

## Regla de uso

La **instancia canónica** del `.Rmd` reproduce el enunciado, la tabla (celda a celda) y las cuatro
opciones **verbatim**, con la clave en el contenido de B (la letra la asigna `exshuffle: TRUE`) y sin
añadidos de contexto que el cuadernillo no menciona. Las
**demás versiones** varían la tabla y el contexto, siempre que conserven la estructura del descriptor
D2.3: eventos simples de **la misma bolsa** (mismo total de casos posibles) y **exactamente un** par de
tipos con la misma frecuencia. Invariantes con sus guardias: [`.claude/CLAUDE.md`](.claude/CLAUDE.md).
