Las cuatro versiones pasan las cinco coherencias y no encontré hallazgos. El md5 del `.Rmd` es `889d828e8129e11b0ec5dceda0cf108b` antes y después, y no toqué nada fuera de `C/salida/paso8_t4/`.

**Renders** (en `C/salida/paso8_t4/`)
- HTML: `html/plain1..4.html`, con mathjax. Los renders no dieron error; solo avisos de pandoc por `--mathml` obsoleto.
- PDF: `pdf/solpcielo_cloze1..4.pdf`, con `solpcielo_cloze.tex`.
- PNG a 80 dpi: `pdf/p{1..4}-{1..4}.png`.

Como la Solution de las versiones 2 a 4 solo la leí como texto, no vi sus PNG. Contrasté cada cifra con los datos de la encuesta, recalculados a mano.

**Coherencias**

| Coherencia | Resultado | Evidencia |
|---|---|---|
| Semántica | OK | Las seis partes siguen el orden `schoice\|num\|num\|num\|num\|mchoice`. Las Partes 2 a 6 no usan vocabulario de error ni «pero no», y el texto de la Solution identifica el diagrama correcto por contenido, no por letra. Semillas 7, 1, 2 y 3. |
| Visual-Texto | OK | Semilla 7: enunciado, siete datos y los cuatro diagramas coinciden con `Originales/orig_pregunta_14.png`. Los diagramas I, II, III y IV equivalen a B, D, A y C del impreso, y la clave (C) queda en el IV. Los cuatro tienen el mismo trazo y tipografía, sin solapes y con rótulos legibles. |
| Matemática | OK | Ver el detalle por semilla debajo. |
| Código | OK | Los números de las figuras de la Parte 1 coinciden con la Solution y con el texto de cada versión. Los gaps de la hoja de respuestas del PDF coinciden con la Solution en las 4 versiones. No hay `##ANSWER`, `$`, `\`, `**` ni `{width` crudos en el PDF ni en el HTML, y no hay decimales espurios. |
| General | OK | Tildes correctas, «Parte N» en orden y cuatro figuras en una sola página del PDF en las 4 versiones. |

**Matemática, por semilla**
- **Semilla 7 (canónica).**
  - Parte 1: el IV tiene 7, 3, 9, 7, 0, 1, 5, que reproduce los siete datos; el gap marca la 4.ª opción. Los otros tres diagramas no cumplen los datos.
  - Otra encuesta (fútbol, voleibol, tenis):
    - Las respuestas son 14, 19, 18 y 122.
    - En la Parte 6, las verdaderas son la 3.ª (14 < 24) y la 4.ª (23 > 18).
    - Coincide con las marcas del PDF.
- **Semilla 1.**
  - Parte 1: el diagrama IV tiene 5, 9, 6, 5, 10, 5, 2, que cumple los datos. El gap marca la 3.ª opción.
  - Otra encuesta: las respuestas son 16, 11, 19 y 124.
  - Verdaderas: 2.ª y 4.ª.
- **Semilla 2.**
  - Parte 1: el III tiene 14, 8, 7, 7, 1, 9, 1, que cumple los datos. El gap marca la 2.ª opción.
  - Otra encuesta: las respuestas son 19, 10, 28 y 133.
  - Verdadera: solo la 2.ª.
- **Semilla 3.**
  - Parte 1: el II tiene 14, 8, 20, 11, 7, 7, 6, que cumple los datos. El gap marca la 2.ª opción.
  - Otra encuesta: las respuestas son 23, 1, 26 y 135.
  - Verdaderas: 1.ª y 4.ª.

En las cuatro versiones los otros tres diagramas incumplen algún dato.

**Observaciones, sin falla**
- En el HTML de `exams2html` los `##ANSWERi##` salen como `<p></p>` vacíos y no hay campos `<input>`, que es el comportamiento normal de esa salida. Las Partes 2 a 5 se ven como preguntas sin campo, y la Parte 1 y la 6 muestran sus opciones como texto separado por «/». Los campos reales aparecen en la hoja de respuestas del PDF (arriba, antes de las preguntas) y en Moodle, que aquí no revisé.
- La captura HTML con chromium cubrió solo la semilla 7; las otras tres las vi en el PDF.

**Hallazgos:** ninguno.

VEREDICTO_VISUAL: APTO
