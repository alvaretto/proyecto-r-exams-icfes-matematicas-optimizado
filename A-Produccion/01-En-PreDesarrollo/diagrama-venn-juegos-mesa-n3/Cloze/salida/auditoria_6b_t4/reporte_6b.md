## Auditoría visual 6b (tanda 4), CLOZE `diagrama_venn_juegos_mesa_..._n3_cloze_v1.Rmd`

**md5 del `.Rmd`:** `889d828e8129e11b0ec5dceda0cf108b` antes y después (sin cambios). Solo escribí en `C/salida/auditoria_6b_t4/`.

**N renderizado:** 26 versiones con `exams2html`, una por archivo y con `seed` explícita: 1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 27, 31, 37, 41, 53, 59, 61, 67, 71, 79, 83, 89, 92, 97, 101 y 150. Las 26 renderizaron OK, sin fallos. Hay 78 capturas, 3 por versión (320, 360 y 1024 px), en `C/salida/auditoria_6b_t4/sNNN/sNNN1_{320,360,1024}.png`. Por ejemplo: `…/auditoria_6b_t4/s007/s0071_320.png`.

**Cómo medí el ancho:** usé chromote con `mobile=FALSE`. Con `mobile=TRUE` la página cargaba a 980 px porque no tiene `<meta viewport>`, y esas capturas las descarté y repetí. En las 31 capturas finales de 320 y 360 que consulté, `innerWidth` fue 320 y 360, y `scrollWidth` fue 305 y 345. No hay desborde horizontal. Revisé a ojo 3 versiones completas y 24 cuadrículas de la Parte 1 a 320 px. A 360 y 1024 px miré solo 2 capturas (s053 a 360 y s027 a 1024). El resto de capturas a 360 y 1024 no las miré una a una; solo medí su ancho.

### Hallazgos
- **CRÍTICO:** ninguno.
- **ALTO:** ninguno.
- **MEDIO:** ninguno.
- **BAJO:** hay dos observaciones cosméticas. Ninguna impide leer.
  1. A 320 px, en algunas versiones los números de dos cifras de las zonas estrechas quedan cerca del trazo de un círculo. Se ven en la columna de s019 y en la de s041 de los montajes de la Parte 1: el «10» junto al «2» del centro, y el «12» de la zona doble. Los números no se solapan entre sí y se leen bien, pero en los casos extremos casi tocan la línea.
  2. En la Solution, las restas de las Partes 4 y 5 usan guion ASCII (`75 - 24 - 33 = 18`, `14 + 24 + …`). La sección «Procedimiento correcto» sí usa LaTeX con signo menos. La mezcla es solo tipográfica. Se ve en s007 a 320 px.

### Lo que sí comprobé, sin problemas
- **Markup:** no hay `##ANSWERi##`, `:::`, `$$`, `\frac`, `NA`, `NaN` ni mojibake. Un grep amplio dio falsos positivos, que eran bytes dentro del base64 de las imágenes incrustadas.
- **Matemáticas:** MathJax renderiza en la Solution (`10 − 3 = 7`, las sumas de los círculos), sin LaTeX crudo.
- **Partes:** están las seis y las Partes 2 a 6 llevan su enunciado. Los gaps aparecen como lista de textos («Diagrama II / Diagrama III / …») y la Parte 6 como cuatro afirmaciones.
- **Figuras de la Parte 1:** en todas las versiones hay cuatro figuras, rotuladas «Diagrama I…IV» encima de cada una, en una sola columna a 320 px. Miden unos 220 px de ancho a 320 px, porque `max-width:100%` recorta los 280 px. A 1024 px forman una cuadrícula de 2×2 a 280 px. Los rótulos «Parqués / Monopoly / Bingo» están dentro del marco y los colores y el trazo son iguales entre figuras.
- **Solution:**
  - La figura de la respuesta correcta se ve completa y es la que reproduce los siete datos.
  - Las sumas de la otra encuesta cuadran: 62, 75 y 66.
  - Parte 4: 75 − 24 − 33 = 18.
  - Parte 5: 122 es la suma de las siete zonas.
  - Las afirmaciones de la Parte 6 de s007 casan con los datos: las dos primeras son falsas y las dos últimas verdaderas.
  - La letra no se usa como identificador: la Solution dice «el Diagrama IV» según el rótulo del enunciado, que coincide con la figura mostrada.
- **Tildes y concordancia:** correctas en todas las versiones que leí. Los contextos de la otra encuesta (caja de compensación, campamento, idiomas, cine, platos) se leen bien.
- **Entre versiones:** no hay vacíos, truncados ni valores negativos. La posición de la clave varía: en s007 es el Diagrama IV y en s027 es el Diagrama II.

### Punto 4: «exams2html(seed = 92) da la canónica»
- **La cita es falsa.** `seed = 92` NO da la canónica.
  - El enunciado de s092 es «Te encargaron organizar la fiesta de la vereda…», con empanadas, arepas y tamales.
  - Sus datos son 9, 12, 14, 13, 34, 31 y 32.
- **`seed = 7` SÍ da la canónica.**
  - Enunciado: «jornada de juegos con los empleados de una empresa… parqués, Monopoly y bingo».
  - Datos: 3, 10, 8, 7, 17, 13 y 20. Coinciden con `ITEM_OFICIAL.md`.
  - Los cuatro diagramas son los del impreso: (17, 3, 20, 7, 0, 1, 13), (17, 10, 20, 3, 7, 8, 13), (0, 10, 2, 3, 4, 5, 1) y (7, 3, 9, 7, 0, 1, 5). El correcto es el de 7, 3, 9, 7, 0, 1 y 5.
  - En s007 el correcto es el Diagrama IV.
- **`seed = 27` también da la canónica,** igual que `seed = 7`, pero con otro rótulo para la clave: el Diagrama II. Eso concuerda con que la letra no se fije.
- Con esto confirmo que 7 y 27 son canónicas y que 92 no lo es. No comprobé las demás canónicas de la lista (47, 54, 66, 130, …).

**Fuera de alcance:** los widgets de Moodle, el PDF y el DOCX no se ven en `exams2html()`.

VEREDICTO_6B: APTO_VISUAL
