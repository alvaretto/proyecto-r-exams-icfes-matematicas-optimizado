---
output:
  html_document:
    df_print: paged
    mathjax: true
  word_document: default
  pdf_document:
    latex_engine: xelatex
    keep_tex: true
header-includes:
- \usepackage[spanish]{babel}
- \usepackage{amsmath}
- \usepackage{fontspec}
- \usepackage{unicode-math}
- \usepackage{graphicx}
- \usepackage{adjustbox}
- \usepackage{tikz}
- \usepackage{pgfplots}
- \usepackage{booktabs}
- \usetikzlibrary{3d,babel}
icfes:
  competencia: interpretacion_representacion
  nivel_dificultad: 2
  contenido:
    categoria: estadistica
    tipo: generico
  contexto: matematico
  eje_axial: eje3
  componente: aleatorio
---















<!-- NOTA MANTENIMIENTO (R-exams): usar encabezados estilo setext (línea con ===) para Question, Solution y Meta-information. Mezclar estilos puede gatillar el error del parser: if (substr(x[b + 1L], 1L, 1L) == "=") { ... condition has length > 1 }. -->

Question
========

En la gráfica se muestra la probabilidad de que la variable aleatoria $x$ tome valores en cada uno de tres intervalos en que se ha dividido la curva.

![](prob_dist_grafico.png){width=12cm}

¿Cuál de las siguientes tablas representa la probabilidad de que la variable aleatoria $x$ tome los valores en el intervalo indicado?

<!-- NOTA MANTENIMIENTO (R-exams): las opciones contienen tablas. Para evitar que el parser cuente filas internas como ítems de respuesta, colapsamos cada tabla a una sola línea y generamos manualmente 4 bullets. -->

Answerlist
----------

- ![](tabla_opcion_a.png){width=40%}

- ![](tabla_opcion_b.png){width=40%}

- ![](tabla_opcion_c.png){width=40%}

- ![](tabla_opcion_d.png){width=40%}

Solution
========

Para encontrar la tabla correcta, debemos extraer la información directamente del gráfico y compararla con las opciones proporcionadas.

**Paso 1: Analizar la información del gráfico.**

El gráfico está dividido en tres intervalos con sus respectivas probabilidades:

1.  El primer intervalo va desde $x=0$ hasta $x=5$. La probabilidad en esta sección es **0,23**.
2.  El segundo intervalo (central) va desde $x=5$ hasta $x=10$. La probabilidad en esta sección es **0,53**.
3.  El tercer intervalo va desde $x=10$ hasta $x=14$. La probabilidad en esta sección es **0,23**.

**Paso 2: Construir la tabla esperada.**

Con la información anterior, la tabla correcta debe tener la siguiente estructura:

\includegraphics[width=3cm]{tabla_solucion.png}

**Paso 3: Comparar con las opciones.**

Al revisar las tablas de las opciones, la única que coincide perfectamente con los datos extraídos del gráfico es la tabla correcta. Las otras tablas presentan errores comunes:

  * **Error de probabilidad acumulada:** Una de las tablas muestra cómo la probabilidad se va sumando (0,23, 0,76, 1,00), lo cual es una interpretación incorrecta del gráfico.
  * **Error de definición de intervalo:** Otra tabla usa intervalos acumulativos (ej: $0 \le x \le 10$), lo cual no corresponde a las secciones individuales mostradas.
  * **Error de asignación:** Otra tabla intercambia los valores de probabilidad, asignando el valor central a los laterales y viceversa.

Por lo tanto, la respuesta correcta es la que representa fielmente los tres intervalos y sus probabilidades tal como se muestran en el gráfico.

Answerlist
----------
- Correcto. Esta tabla representa exactamente los tres intervalos y sus probabilidades correspondientes como se muestra en el gráfico.
- Incorrecto. Usa intervalos acumulativos (0 $\le$ x $\le$ a, 0 $\le$ x $\le$ b, 0 $\le$ x $\le$ c) y una columna acumulada; no corresponde a las tres secciones individuales.
- Incorrecto. Intercambia los valores asignando el central a los laterales.
- Incorrecto. No coincide con la asignación de probabilidades del gráfico.

Meta-information
================

exname: probabilidad\_distribucion\_grafico\_tabla
extype: schoice
exsolution: 1000
exshuffle: TRUE
exsection: Estadística/Probabilidad/Interpretación de gráficos
