

- Analiza y entiende cómo se configura y se maneja Python (Reticulate) en
los documentos 'Contrasennia_Cloze_All.Rmd' y 'I_1796473-Opc-B.Rmd'. Adapta esas configuraciones a las necesidades actuales de '.Rmd'

################################################################################

- La opción correcta actualmente para 'I_1796473-Opc-C.Rmd' es 'exsolution: 0010'; es decir, la opción representada por barras horizontales. 

- Modifica el código, de tal manera que, la opción correcta sea 'exsolution: 0001'; es decir, la opción representada por una torta. Usa distractores dentro de las opciones incorrectas. Asegúrate de que la opción correcta se ajuste a los argumentos matemáticos propuestos.

- Genera un archivo 'I_1796473-Opc-D.Rmd' con el nuevo código.

################################################################################

- La opción correcta actualmente para 'I_1796473-Opc-A.Rmd' es 'exsolution: 1000'; es decir, la opción representada por una Tabla. 

- La opción correcta actualmente para 'I_1796473-Opc-B.Rmd' es 'exsolution: 0100'; es decir, la opción representada por barras verticales. 

- La opción correcta actualmente para 'I_1796473-Opc-C.Rmd' es 'exsolution: 0010'; es decir, la opción representada por barras horizontales. 

- La opción correcta actualmente para 'I_1796473-Opc-D.Rmd' es 'exsolution: 0001'; es decir, la opción representada por una Torta.

Genera el código para un nuevo archivo 'I_1796473_All.Rmd', que fusione todos los códigos antes mencionados, optimizándolo, mediante funciones, clases y otras técnicas de nivel avanzado de programación, de tal manera que la opción correcta pueda ser una sola culaquiera entre las ya mencionadas (Tabla, Barras Verticales, Barras Horizontales o Torta). 

Usa siempre distractores dentro de las opciones incorrectas. 

Asegúrate de que la opción correcta se ajuste a los argumentos matemáticos propuestos.

################################################################################

Revisa 'AdopcionMascotas_all.Rmd'. 

Modificar código, de tal manera que se incluya este código:

```{r GeneracionGraficosTikZ, results='asis', warning=FALSE, message=FALSE}
image01 <- '
\\begin{tikzpicture}
  \\node{
    \\begin{tabular}{|l|c|}
    \\hline
    \\textbf{Animal} & \\textbf{Porcentaje de personas } \\\\ 
         & \\textbf{interesadas en adoptar} \\\\ \\hline
         %s & %s \\\\ \\hline
         %s & %s \\\\ \\hline
         %s & %s \\\\ \\hline
    \\end{tabular}
  };
\\end{tikzpicture}
'

# Introduce errores en los porcentajes (intercambiar porcentajes)
image01 <- sprintf(image01, selmascota[1], porxentaje2, selmascota[2], porxentaje3, selmascota[3], porxentaje1)
```

en 'GraficoGenerator' reemplazando el código de 'generar_tabla' 

- Identifica las partes que funcionan correctamente y no necesitan cambios.
- Mantén la estructura y funcionalidad original del archivo.
- Haz solo los ajustes mínimos necesarios para resolver el problema específico que se solicita
- Asegúrate de no introducir cambios innecesarios o potencialmente perjudiciales.






Aquí hay un ejemplo de cómo estás manejando la sección 'Retroalimentación':

'
Retroalimentación:
La representación correcta es la opción 3. Esta opción muestra exactamente los porcentajes mencionados en la pregunta:
10 % para iguana, 60 % para cerdo, y 30 % para serpiente. Las otras opciones contienen errores en los porcentajes o en
la forma de representación de los datos.'

Debería quedar así:

'
Retroalimentación:
La representación correcta es [Imagen de la gráfica que corresponde a la opción correcta]. Esta opción muestra exactamente los porcentajes mencionados en la pregunta:
10 % para iguana, 60 % para cerdo, y 30 % para serpiente. Las otras opciones contienen errores en los porcentajes o en la forma de representación de los datos.'

################################################################################

Analiza el documento 'AdopcionMascotas-Opc-A.Rmd'. Las gráficas de barras Verticles, Barras Horizontales y Torta tienen un tamaño exagerado. la gráfica de la tabla está perfecta. Por favor, resolver problemas para garantizar óptimas salidas en exams2moodle, exams2html, exams2pdf y exams2pandoc.

################################################################################

Analiza y comprende el código de 'I_1796473-Opc-A2.Rmd'. Corrige los errores:

'>>> quit
> library(exams)
> library(tidyverse)
> library(datasets)
> library(readxl)
> library(data.table)
> library(reticulate)
> 
> # Opciones de entornos Python
> python_paths <- c("/usr/bin/python3.11")
> 
> # Intenta establecer el entorno Python utilizando la primera ruta válida
> for (path in python_paths) {
+   use_python(path, required = FALSE)
+   if (py_config()$python_version != "") {
+     cat("Usando Python en:", path, "\n")
+     break
+   }
+ }
Usando Python en: /usr/bin/python3.11 
> 
> typ <- match_exams_device()
> options(scipen = 999)
> knitr::opts_chunk$set(warning = FALSE, message = FALSE)
> 
> # Vector de mascotas
> mascotas <- c('loro', 'perro', 'gato', 'gallina', 'hamster', 'cerdo', 'ternero', 'caballo', 'cabra')
> 
> # Seleccionar una muestra aleatoria de 3 elementos sin repetición
> selmascota <- sample(mascotas, 3)
> selmascota[3]
[1] "hamster"
> 
> nombremascota1 <- selmascota[1]
> nombremascota2 <- selmascota[2]
> nombremascota3 <- selmascota[3]
> 
> # Crear secuencia del 60 al 300 de 10 en 10
> numeros <- seq(60, 600, 10)
> 
> # Eliminamos el número 100 del vector
> numeros_sin_100 <- setdiff(numeros, 100)
> numeros_sin_100
 [1]  60  70  80  90 110 120 130 140 150 160 170 180 190 200 210 220 230 240
[19] 250 260 270 280 290 300 310 320 330 340 350 360 370 380 390 400 410 420
[37] 430 440 450 460 470 480 490 500 510 520 530 540 550 560 570 580 590 600
> 
> # Ahora hacemos el muestreo de este nuevo vector
> enkuestados <- sample(numeros_sin_100, 1)
> # Generar tres números para los porcentajes. Su suma siempre debe ser igual a 100
> generar_vector_unico <- function() {
+   repetir <- TRUE
+   while (repetir) {
+     # Generar el primer número como un múltiplo de 10 entre 10 y 60.
+     # Esto aumenta las posibilidades de tener tres números únicos.
+     primer_numero <- sample(seq(10, 60, by = 10), 1)
+     
+     # Calcular el máximo valor posible para el segundo número,
+     # asegurándose de que haya espacio para un tercer número único.
+     max_segundo_numero <- 90 - primer_numero
+     
+     # Generar el segundo número asegurando que sea diferente al primero
+     posibles_segundos <- seq(10, max_segundo_numero, by = 10)
+     posibles_segundos <- posibles_segundos[posibles_segundos != primer_numero]
+     if (length(posibles_segundos) > 0) {
+       segundo_numero <- sample(posibles_segundos, 1)
+     } else {
+       next
+     }
+     
+     # Calcular el tercer número necesario para que la suma sea 100,
+     # asegurándose de que sea diferente a los dos anteriores.
+     tercer_numero <- 100 - primer_numero - segundo_numero
+     
+     # Verificar si los tres números son únicos
+     if (length(unique(c(primer_numero, segundo_numero, tercer_numero))) == 3) {
+       repetir <- FALSE
+     }
+   }
+   
+   # Crear el vector
+   vector <- c(primer_numero, segundo_numero, tercer_numero)
+   
+   return(vector)
+ }
> 
> # Generar y mostrar el vector de porcentajes
> vector_resultado <- generar_vector_unico()
> vector_resultado
[1] 60 10 30
> 
> mashor <- max(vector_resultado)
> mashor
[1] 60
> 
> porxentaje1 <- vector_resultado[1]
> porxentaje2 <- vector_resultado[2]
> porxentaje3 <- vector_resultado[3]
> ####################################################
> 
> maskota1 <- (enkuestados*vector_resultado[1])/100 # Número de personas que adoptan maskota1
> maskota1
[1] 54
> maskota2 <- (enkuestados*vector_resultado[2])/100
> maskota2
[1] 9
> maskota3 <- (enkuestados*vector_resultado[3])/100
> maskota3
[1] 27
> 
> mashiormaskota <- max(maskota1, maskota2, maskota3)
> mashiormaskota
[1] 54
> 
> image01 <- '
+ \\begin{tikzpicture}
+   \\node{
+ \\begin{tabular}{|l|c|}
+ \\hline
+ \\textbf{Animal} & \\textbf{Porcentaje de personas } \\\\ 
+      & \\textbf{interesadas en adoptar} \\\\ \\hline
+      %s & %s \\\\ \\hline
+      %s & %s \\\\ \\hline
+      %s & %s \\\\ \\hline
+ \\end{tabular}
+ };
+ \\end{tikzpicture}
+ '
> 
> demas <- sample
> 
> image01 <-sprintf(image01, selmascota[1], porxentaje1, selmascota[2], porxentaje2, selmascota[3], porxentaje3)
> knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
> include_tikz(image01, name = "image01", markup = "markdown",format = typ, library = c("3d", "babel"), packages=c("tikz","xcolor"),width = "8.5cm")
![](image01.png){width=8.5cm}
> reticulate::repl_python()
>>> import matplotlib.pyplot as plt
>>> import numpy as np
>>> 
>>> mascota1py = r.nombremascota1 # De R a Python
>>> mascota2py = r.nombremascota2
>>> mascota3py = r.nombremascota3
>>> 
>>> porcentaje1py = r.porxentaje1 # De R a Python
>>> porcentaje2py = r.porxentaje2
>>> porcentaje3py = r.porxentaje3
>>> 
>>> mashorpy = r.mashor
>>> 
>>> # Datos
>>> animales = [mascota1py, mascota2py, mascota3py]
>>> cantidad = [porcentaje1py, porcentaje2py, porcentaje3py]
>>> 
>>> # Definir colores en tonos de azul
>>> colores_azules = ['#00B3E6', '#0066CC', '#000099']  
>>> 
>>> # Crear el gráfico de barras
>>> fig, ax = plt.subplots(figsize=(5, 4))
>>> 
>>> # Ajustar el ancho de las barras aquí para controlar indirectamente el espacio entre ellas
>>> ancho_barras = 0.5  # Un valor más pequeño de lo normal
>>> 
>>> # Dibujar las barras con el ancho especificado
>>> bars = ax.bar(animales, cantidad, color=colores_azules, width=ancho_barras)
>>> 
>>> # Resto del código de personalización
>>> ax.spines['top'].set_visible(False)
>>> ax.spines['right'].set_visible(False)
>>> ax.spines['left'].set_linewidth(2)
>>> ax.spines['bottom'].set_linewidth(2)
>>> ax.yaxis.grid(True, linestyle='--', linewidth=0.7, color='darkgray')
>>> plt.xticks(fontweight='bold')
([0, 1, 2], [Text(0, 0, 'gato'), Text(1, 0, 'gallina'), Text(2, 0, 'hamster')])
>>> plt.yticks(np.arange(0, mashorpy+1, 10), fontweight='bold')
([<matplotlib.axis.YTick object at 0x7f89887fe210>, <matplotlib.axis.YTick object at 0x7f8988a20750>, <matplotlib.axis.YTick object at 0x7f8988a4ac90>, <matplotlib.axis.YTick object at 0x7f898898c090>, <matplotlib.axis.YTick object at 0x7f898898d950>, <matplotlib.axis.YTick object at 0x7f898898e290>, <matplotlib.axis.YTick object at 0x7f8988bcb250>], [Text(0, 0.0, '0'), Text(0, 10.0, '10'), Text(0, 20.0, '20'), Text(0, 30.0, '30'), Text(0, 40.0, '40'), Text(0, 50.0, '50'), Text(0, 60.0, '60')])
>>> plt.xlabel(" Animales", fontweight='bold')
Text(0.5, 0, ' Animales')
>>> plt.ylabel("Cantidad de personas \ninteresadas en adoptar", fontweight='bold')
Text(0, 0.5, 'Cantidad de personas \ninteresadas en adoptar')
>>> 
>>> #plt.tight_layout()
>>> plt.show()
Traceback (most recent call last):
  File "<string>", line 1, in <module>
  File "/home/datalinux/R/x86_64-pc-linux-gnu-library/4.0/reticulate/python/rpytools/call.py", line 6, in python_function
    return call_r_function(f, *args, **kwargs)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
RuntimeError: argumento 'path' inválido
>>> 
>>> import matplotlib.pyplot as plt
>>> import numpy as np
>>> 
>>> mascota1py = r.nombremascota1 # De R a Python
>>> mascota2py = r.nombremascota2
>>> mascota3py = r.nombremascota3
>>> 
>>> porcentaje1py = r.porxentaje1 # De R a Python
>>> porcentaje2py = r.porxentaje2
>>> porcentaje3py = r.porxentaje3
>>> 
>>> padrino1py = r.maskota1
>>> padrino2py = r.maskota2
>>> padrino3py = r.maskota3
>>> 
>>> mashorpy = r.mashor
>>> mashiormaskotapy = r.mashiormaskota
>>> niveles = mashiormaskotapy/10
>>> 
>>> # Datos
>>> animales = [mascota3py, mascota2py, mascota1py]
>>> cantidad = [padrino3py, padrino2py, padrino1py]
>>> 
>>> # Definir colores en tonos de azul
>>> colores_azules = ['#00B3E6', '#0066CC', '#000099']  
>>> 
>>> # Crear el gráfico de barras horizontales
>>> fig, ax = plt.subplots(figsize=(6, 4))
>>> 
>>> # Ajustar el alto de las barras aquí para controlar indirectamente el espacio entre ellas
>>> alto_barras = 0.6  # Un valor más pequeño de lo normal
>>> 
>>> # Dibujar las barras horizontales con el alto especificado
>>> bars = ax.barh(animales, cantidad, color=colores_azules, height=alto_barras)
>>> 
>>> for bar in bars:
...     # Obtener la posición y el valor de la barra
...     valor = bar.get_width()
...     # Posicionar el texto ligeramente debajo de la barra. Ajustar el '-0.05' según sea necesario.
...     posicion = bar.get_y() - 0.05
...     
...     # Añadir texto debajo de la barra, a su extrema derecha
...     # 'va' cambia a 'top' para alinear el texto en la parte superior del punto especificado,
...     # efectivamente moviéndolo debajo de la barra.
...     ax.text(valor, posicion, f'{valor}', va='top', ha='right', color='black', fontweight='bold', fontsize=10)
...     
... # Ajustes finales y mostrar el gráfico
... ax.spines['top'].set_visible(False)
Text(27.0, -0.35, '27.0')
Text(9.0, 0.6499999999999999, '9.0')
Text(54.0, 1.65, '54.0')
>>> ax.spines['right'].set_visible(False)
>>> ax.spines['left'].set_linewidth(2)
>>> ax.spines['bottom'].set_linewidth(2)
>>> ax.xaxis.grid(True, linestyle='--', linewidth=0.7, color='darkgray')
>>> plt.yticks(fontweight='bold')
([0, 1, 2], [Text(0, 0, 'hamster'), Text(0, 1, 'gallina'), Text(0, 2, 'gato')])
>>> plt.xticks(np.arange(0, max(cantidad)+10, niveles), fontweight='bold')  # Ajuste en el rango para xticks basado en la máxima cantidad
([<matplotlib.axis.XTick object at 0x7f8988994b50>, <matplotlib.axis.XTick object at 0x7f8988bc9290>, <matplotlib.axis.XTick object at 0x7f898a95cf50>, <matplotlib.axis.XTick object at 0x7f898897bf10>, <matplotlib.axis.XTick object at 0x7f8988b4d050>, <matplotlib.axis.XTick object at 0x7f8988b4df50>, <matplotlib.axis.XTick object at 0x7f8988a58c10>, <matplotlib.axis.XTick object at 0x7f8988a5bc50>, <matplotlib.axis.XTick object at 0x7f8988b4d510>, <matplotlib.axis.XTick object at 0x7f8988b69150>, <matplotlib.axis.XTick object at 0x7f8988b69b10>, <matplotlib.axis.XTick object at 0x7f8989dbe250>], [Text(0.0, 0, '0.0'), Text(5.4, 0, '5.4'), Text(10.8, 0, '10.8'), Text(16.200000000000003, 0, '16.2'), Text(21.6, 0, '21.6'), Text(27.0, 0, '27.0'), Text(32.400000000000006, 0, '32.4'), Text(37.800000000000004, 0, '37.8'), Text(43.2, 0, '43.2'), Text(48.6, 0, '48.6'), Text(54.0, 0, '54.0'), Text(59.400000000000006, 0, '59.4')])
>>> plt.ylabel("Animales", fontweight='bold')
Text(0, 0.5, 'Animales')
>>> plt.xlabel("Porcentaje de personas \ninteresadas en adoptar", fontweight='bold')
Text(0.5, 0, 'Porcentaje de personas \ninteresadas en adoptar')
>>> 
>>> #plt.tight_layout()
>>> plt.show()
Traceback (most recent call last):
  File "<string>", line 1, in <module>
  File "/home/datalinux/R/x86_64-pc-linux-gnu-library/4.0/reticulate/python/rpytools/call.py", line 6, in python_function
    return call_r_function(f, *args, **kwargs)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
RuntimeError: argumento 'path' inválido
>>> 
>>> # Re-importing necessary libraries after execution state reset
>>> from matplotlib import pyplot as plt
>>> 
>>> adoptantes1py = r.maskota1 # De R a Python
>>> adoptantes2py = r.maskota2
>>> adoptantes3py = r.maskota3
>>> 
>>> # Re-defining data for the pie chart
>>> labels = ['Porcentaje de \nPersonas \ninteresadas en \nadoptar un \n{}'.format(mascota1py),
...           'Porcentaje de \nPersonas \ninteresadas en \nadoptar un \n{}'.format(mascota2py),
...           'Porcentaje de \nPersonas \ninteresadas en \nadoptar un \n{}'.format(mascota3py)]
>>> sizes = [adoptantes1py, adoptantes2py, adoptantes3py]  # Actual values for calculations
>>> sizes2 = [adoptantes1py, adoptantes2py, adoptantes3py]  # Values to display on the labels
>>> colors = ['#1f77b4', '#aec7e8', '#ff7f0e']
>>> explode = (0, 0, 0)  # No slice is exploded
>>> 
>>> # Labels with the sizes values
>>> pie_labels = ['{}: {}'.format(label, int(size)) for label, size in zip(labels, sizes2)]
>>> 
>>> # Plot
>>> fig, ax = plt.subplots(figsize=(6, 4), tight_layout=True)  # Ajustar el tamaño y el diseño de la figura.
>>> ax.pie(sizes, explode=explode, labels=pie_labels, colors=colors, shadow=True, startangle=0)
([<matplotlib.patches.Wedge object at 0x7f8989d56790>, <matplotlib.patches.Wedge object at 0x7f8988a8efd0>, <matplotlib.patches.Wedge object at 0x7f898a90e9d0>], [Text(-0.33991877217145816, 1.046162142464278, 'Porcentaje de \nPersonas \ninteresadas en \nadoptar un \ngato: 54'), Text(-0.6465636400433808, -0.8899187936962865, 'Porcentaje de \nPersonas \ninteresadas en \nadoptar un \ngallina: 9'), Text(0.6465639524941308, -0.8899185666875186, 'Porcentaje de \nPersonas \ninteresadas en \nadoptar un \nhamster: 27')])
>>> ax.axis('equal')  # La relación de aspecto igual garantiza que el gráfico circular se dibuje como un círculo.
(-1.0999990089472786, 1.0999999528070132, -1.0999989617539319, 1.0999989617560506)
>>> 
>>> # Reducing left and top margins
>>> plt.subplots_adjust(left=0.1, top=0.9)
>>> 
>>> # Display the plot
>>> plt.show()
Traceback (most recent call last):
  File "<string>", line 1, in <module>
  File "/home/datalinux/R/x86_64-pc-linux-gnu-library/4.0/reticulate/python/rpytools/call.py", line 6, in python_function
    return call_r_function(f, *args, **kwargs)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
RuntimeError: argumento 'path' inválido'


- Identifica las partes que funcionan correctamente y no necesitan cambios.
- Mantén la estructura y funcionalidad original del archivo.
- Haz solo los ajustes mínimos necesarios para resolver el problema específico que se solicita. 
- Asegúrate de no introducir cambios innecesarios o potencialmente perjudiciales.