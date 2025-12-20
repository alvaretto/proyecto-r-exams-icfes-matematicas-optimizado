Por favor, genera el código r-exams que recrea este escenario matemático, aleatorizando la mayor cantidad de variables posible, de tal manera que:

- se genere código r-exams para preguntas tipo Cloze
- se muestre en la sección 'Solution' un Diagrama de Venn con la opción correcta.
  - Ubicarás la gráfica del Diagrama de Venn para la opción correcta en el lugar adecuado (Solution).
- no se usen distractores (opciones incorrectas) con números negativos.
- no se muestre la respuesta directa en un área visible para el estudiante, como pueder ser el enunciado de la pregunta.
- se respeten la estructura,  lógica y leyes matemáticas propias del tema en cuestión.
- se puedan generar, como mínimo, 300 versiones diferentes de la pregunta gracias a la aleatorización.
- se genere un archivo 'NDV_01.Rmd' con el nuevo código.
- se visualice correctamente y de manera prolija el Diagrama de Venn para salidas exams2moodle.
- se visualice correctamente y de manera prolija el Diagrama de Venn para salidas exams2pdf.
- se visualice correctamente y de manera prolija el Diagrama de Venn para salidas exams2html.
- se visualice correctamente y de manera prolija el Diagrama de Venn para salidas exams2pandoc.
- se genere un archivo 'NDV_all_01.Rmd', con el nuevo código.

Adapta el nuevo código con base en este trozo de código Látex:

'
generate_venn <- function() {
  latex_code <- sprintf("
\\begin{center}
\\begin{tikzpicture}
\\begin{scope}[blend mode=screen]
  \\fill[red!30]   (0,0)   circle (2);
  \\fill[green!30] (0:2)  circle (2);
  \\fill[blue!30]  (300:2) circle (2);
\\end{scope}
\\node at (-2.5,2)  {Rock};
\\node at (4,2)   {Rap};
\\node at (1,-4)  {Pop};
\\node at (-1,0) {%d};
\\node at (1,-3)  {%d};
\\node at (3,0)   {%d};
\\node at (-0.3,-1.5)    {%d};
\\node at (1,1)  {%d};
\\node at (2.3,-1.45)   {%d};
\\node at (1,-0.7)   {%d};
\\end{tikzpicture}
\\end{center}", c, b, a, bc, ac, ab, abc)'

################################################################################

Revisa el archivo ‘NDV_01.Rmd’:

- Analiza y entiende su estructura, lógica, leyes matemáticas y objetivos.
- Adapta el código, de tal manera que:
  - la pregunta a resolver ya no sea '¿Cuántas de estas unidades vendidas tenían equipo de música?', sino '¿Cuántas de estas unidades vendidas tenían transmisión mecánica?'
  - se respeten la estructura, lógica, leyes matemáticas y objetivos generales del código.
  - se siga usando generate_venn_latex para la generación del Diagrama de Venn.
  - se respeten las coordenadas generales para posicionamiento de valores del código Latex
  - se pueda visualizar el Diagrama de venn para salidas exams2pdf, exams2moodle, exams2pandoc y exams2html.
- Genera un nuevo archivo 'NDV_02.Rmd', con el nuevo código.
- No hagas otros cambios estructurales que no tengan que ver con las presentes solicitudes.
- Responde siempre en español.


Errores: 

- Sigue sin visualizarse la salida exams2html ni exams2moodle ni exams2pandoc.




Analiza y entiende el archivo ‘genmus_v2.Rmd’. Cambia la lógica del código, de tal manera que:

  - Las preguntas a responder sean
    - ¿Cuántas personas escuchan RAP y POP?
    - ¿Cuántas personas escuchan solo dos de estos géneros musicales?
    - ¿Cuántas personas no escuchan ninguno de estos géneros musicales?
  - No cambies nada que no esté relacionado con las solicitudes actuales

######################

Analiza el código de 'Dvenn_all_GenMus.Rmd'. Genera una lista de 14 preguntas diferentes, similares a las que encuentras en ‘Answerlist’ (parte 1).

Adapta ‘Answerlist’, de tal manera que se muestren 4 preguntas seleccionadas aleatoriamente de la lista generada en el paso anterior.

Usa textos similares como distractores. Por ejemplo, puedes usar indistintamente ‘sólo escuchan a y b’, ‘solamente escuchan a y b’, ‘escuchan solamente b y a’, ‘escuchan a y b, pero no c’. Todas esas frases significan lo mismo, pero funcionan muy bien como distractores.

Haz todo eso sin romper la lógica global del código y respetando las leyes matemáticas que rigen los Diagramas de Venn.

Garantiza coherencia entre textos e imágenes.

Garantiza coherencia entre lo que se pregunta y lo que se explica detalladamente en ‘Solution’.

################################################################################