Fixtures del vigía de versiones de R/exams. Este preámbulo sin encabezado se ignora.

# exams 2.4-5

* Se corrige el manejo de \usepackage en los templates de Pandoc para
  evitar conflictos con LTcaptype en longtable bajo pdflatex.
* Las figuras ahora usan includegraphics para el graphics file principal.
  * El supplement ya no duplica la imagen del enunciado.
  * Se respeta el ancho declarado en el template de cada figura.
* Nueva opción exclozetype para los ejercicios CLOZE exportados a Moodle
  con exshuffle y exsolution controladas por el usuario.
* Corrección menor de redacción en la documentación.

# exams 2.4-4

* El stresstest ahora acepta un vector de semillas para reproducir las
  corridas de exams2nops.
* answerlist() ya no genera espacios rotos y num_to_schoice gana un
  parámetro nuevo.
* Ajuste interno sin efecto visible para el usuario.

# exams 2.4-3

* Primera versión estable de la rama 2.4 con soporte NOPS vía exams2nops.
* Se actualiza la portada del manual.
