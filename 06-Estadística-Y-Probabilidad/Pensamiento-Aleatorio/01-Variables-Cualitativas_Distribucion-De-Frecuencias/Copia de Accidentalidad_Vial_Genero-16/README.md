# Ejercicio: Accidentalidad Vial por Género

## Descripción General

Este ejercicio forma parte del proyecto de Matemáticas ICFES con R-Exams, específicamente en el área de Estadística y Probabilidad, Pensamiento Aleatorio. El ejercicio `accidentalidad-vial-genero-01.Rmd` presenta un problema de interpretación de gráficas estadísticas relacionadas con la mortalidad por accidentes de tránsito, diferenciada por género.

## Objetivo Pedagógico

El objetivo de este ejercicio es evaluar la capacidad del estudiante para:

- Interpretar gráficas estadísticas
- Analizar datos presentados en tablas y gráficos
- Identificar la representación gráfica correcta de un conjunto de datos
- Comprender la distribución de frecuencias en variables cualitativas (género)

## Estructura del Ejercicio

El ejercicio presenta:

1. Un escenario contextualizado sobre accidentalidad vial
2. Una gráfica que muestra la mortalidad total por años
3. Una tabla con datos específicos de víctimas de género masculino
4. Cuatro opciones de gráficas que representan los datos por género
5. El estudiante debe identificar la gráfica que representa correctamente los datos

## Aleatorización y Variabilidad

Este ejercicio implementa una amplia aleatorización para generar múltiples versiones:

- **Contexto**: Ciudades, términos utilizados (estudio, análisis, mortalidad, etc.)
- **Datos**: Años del estudio, valores de mortalidad, tendencias
- **Visualización**: Colores de gráficas, etiquetas, estilos de tabla
- **Distractores**: Diferentes tipos de errores en las opciones incorrectas

## Documentación Técnica del Código

### Configuración Inicial (líneas 7-44)

```r
# Configuración para todos los formatos de salida
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")

# Configurar el motor LaTeX globalmente
options(tikzLatex = "pdflatex")
options(tikzXelatex = FALSE)
options(tikzLatexPackages = c(
  "\\usepackage{tikz}",
  "\\usepackage{colortbl}"
))

# Carga de librerías necesarias
library(exams)
library(reticulate)
library(digest)
library(testthat)

# Configuración del dispositivo de salida y opciones de chunks
typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.showtext = FALSE,
  fig.cap = "",
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150
)

# Configuración para chunks de Python
knitr::knit_engines$set(python = function(options) {
  knitr::engine_output(options, options$code, '')
})

# Asegurar que Python esté correctamente configurado
use_python(Sys.which("python"), required = TRUE)
```

Esta sección establece:

- La configuración del formato numérico (punto decimal)
- Configuración de LaTeX para TikZ (usado en tablas)
- Carga de librerías necesarias
- Configuración de opciones para chunks de R
- Configuración del motor Python a través de reticulate

### Definición de Variables (líneas 46-142)

```r
# Establecer semilla aleatoria
set.seed(sample(1:10000, 1))

# Aleatorizar ciudades para el contexto del problema
ciudades <- c("Lima", "Bogotá", "Ciudad de México", "Santiago", "Buenos Aires", ...)
ciudades_seleccionadas <- sample(ciudades, sample(3:5, 1))
ciudades_texto <- paste(ciudades_seleccionadas, collapse = ", ")

# Aleatorizar términos para el contexto del problema
terminos_estudio <- c("estudio", "análisis", "investigación", "informe", "reporte")
termino_estudio <- sample(terminos_estudio, 1)
...

# Aleatorizar años para el estudio (mantener 4 años consecutivos)
año_inicial <- sample(2000:2018, 1)
años <- año_inicial:(año_inicial + 3)
años_texto <- paste(min(años), "y", max(años))

# Generar datos de mortalidad total con tendencia realista
base_mortalidad <- sample(4000:8000, 1)
variacion_maxima <- round(base_mortalidad * 0.2)  # Variación máxima de 20%

# Generar tendencia aleatoria para mortalidad total
tendencias <- c("creciente", "decreciente", "pico", "valle", "ondulante")
tendencia <- sample(tendencias, 1)

# Generación de datos según la tendencia seleccionada
mortalidad_total <- numeric(4)
if (tendencia == "creciente") {
  factor_incremento <- seq(1, 1.2, length.out = 4)
  mortalidad_total <- round(base_mortalidad * factor_incremento + rnorm(4, 0, variacion_maxima * 0.3))
} else if (tendencia == "decreciente") {
  ...
}

# Generar datos para hombres (aproximadamente 75-85% del total - proporción realista)
proporcion_hombres <- runif(4, 0.75, 0.85)
mortalidad_hombres <- round(mortalidad_total * proporcion_hombres)

# Calcular datos para mujeres (el resto)
mortalidad_mujeres <- mortalidad_total - mortalidad_hombres

# Aleatorizar colores y etiquetas
colores_disponibles <- c("blue", "red", "green", "purple", "orange", "brown", "black", "magenta", "cyan")
color_hombres_correcto <- sample(colores_disponibles, 1)
...

# Establecer la gráfica correcta
opciones <- c("lineas_separadas_A", "lineas_separadas_B", "lineas_separadas_C", "lineas_separadas_D")
opcion_correcta <- "lineas_separadas_A"
indice_correcto <- 1  # Índice de "lineas_separadas_A" en el vector opciones

# Vector de solución para r-exams
solucion <- integer(4)
solucion[indice_correcto] <- 1
```

Esta sección:

- Establece una semilla aleatoria para reproducibilidad
- Aleatoriza elementos del contexto (ciudades, términos)
- Define el rango de años para el estudio
- Genera datos de mortalidad total con diferentes tendencias
- Calcula la distribución por género (hombres/mujeres)
- Aleatoriza colores y etiquetas para las gráficas
- Define la opción correcta y el vector de solución

### Generación de Tabla TikZ (líneas 144-181)

```r
# Aleatorizar colores de la tabla
color_fondo_tabla <- sample(c("orange", "blue", "green", "red", "yellow", "cyan"), 1)
intensidad_color <- sample(c(10, 15, 20, 25, 30), 1)
color_tabla <- paste0(color_fondo_tabla, "!", intensidad_color)

# Función para generar el código TikZ de la tabla
generar_tabla_tikz <- function(años, datos_hombres, color_tabla, etiqueta_masculino) {
  # Crear tabla con TikZ
  tabla_code <- c("\\begin{tikzpicture}",
    "\\node[inner sep=0pt] {",
    "  \\begin{tabular}{|c|c|}",
    "    \\hline",
    paste0("    \\rowcolor{", color_tabla, "}"),
    paste0("    \\textbf{Año} & \\textbf{Personas de ", tolower(etiqueta_masculino), "} \\\\"),
    paste0("    \\textbf{} & \\textbf{víctimas de ", sample(c("accidentalidad vial", "siniestros de tránsito", "incidentes de tráfico"), 1), "} \\\\"),
    "    \\hline")
  
  # Añadir filas con datos
  for (i in 1:length(años)) {
    tabla_code <- c(tabla_code, paste0("    ", años[i], " & ", format(datos_hombres[i], big.mark = ","), " \\\\"))
    tabla_code <- c(tabla_code, "    \\hline")
  }
  
  # Cerrar la tabla
  tabla_code <- c(tabla_code,
    "  \\end{tabular}",
    "};",
    "\\end{tikzpicture}")
  
  return(tabla_code)
}

# Generar código TikZ para la tabla
tabla_tikz <- generar_tabla_tikz(años, mortalidad_hombres, color_tabla, etiqueta_masculino)
```

Esta sección:

- Aleatoriza los colores y estilos de la tabla
- Define una función para generar código TikZ que crea una tabla estilizada
- Genera el código TikZ para la tabla con los datos de mortalidad masculina

### Generación de Gráficas con Python (líneas 183-400)

```r
# Código Python para las gráficas
codigo_base_python <- "
import matplotlib.pyplot as plt
import numpy as np
import matplotlib
matplotlib.rcParams['font.size'] = 9

años = %s
mortalidad_hombres = %s
mortalidad_mujeres = %s
color_hombres = '%s'
color_mujeres = '%s'
etiqueta_masculino = '%s'
etiqueta_femenino = '%s'
"

# Reemplazar valores en el código Python
codigo_python_base <- sprintf(codigo_base_python, 
                            paste(años, collapse=", "), 
                            paste(mortalidad_hombres, collapse=", "), 
                            paste(mortalidad_mujeres, collapse=", "),
                            color_hombres_correcto,
                            color_mujeres_correcto,
                            etiqueta_masculino,
                            etiqueta_femenino)

# Código para graficar los totales
codigo_python_grafica_total <- paste0(codigo_python_base, "
# Configuración de la figura
plt.figure(figsize=(6, 3.5))

# Datos totales
mortalidad_total = [h + m for h, m in zip(mortalidad_hombres, mortalidad_mujeres)]

# Graficar los puntos y líneas
plt.plot(años, mortalidad_total, marker='o', color='", color_total, "', linestyle='-', linewidth=2, markersize=8)
...
")

# Opción A: CORRECTA - Representa los datos originales correctamente
codigo_python_opcion1 <- paste0(codigo_python_base, "
plt.figure(figsize=(5, 3.5))

# Graficar líneas separadas para hombres y mujeres (datos correctos)
plt.plot(años, mortalidad_hombres, marker='o', color=color_hombres, linestyle='-', label=etiqueta_masculino, linewidth=2)
plt.plot(años, mortalidad_mujeres, marker='o', color=color_mujeres, linestyle='-', label=etiqueta_femenino, linewidth=2)
...
")

# Opción B: INCORRECTA - Valores duplicados o modificados de forma significativa
distractor_b_factor <- runif(1, 1.8, 2.2)
codigo_python_opcion2 <- paste0(codigo_python_base, "
plt.figure(figsize=(5, 3.5))

# Crear datos incorrectos para esta opción
# Distractor: valores significativamente más altos
mortalidad_hombres_opcion2 = [int(h * ", distractor_b_factor, ") for h in mortalidad_hombres]
mortalidad_mujeres_opcion2 = mortalidad_mujeres  # Mantener estos valores correctos
...
")

# Opción C: INCORRECTA - Intercambia hombres y mujeres
codigo_python_opcion3 <- paste0(codigo_python_base, "
plt.figure(figsize=(5, 3.5))

# Intercambiar datos para esta opción (distractor)
mortalidad_hombres_opcion3 = mortalidad_mujeres  # Las líneas que deberían ser de mujeres
mortalidad_mujeres_opcion3 = mortalidad_hombres  # Las líneas que deberían ser de hombres
...
")

# Opción D: INCORRECTA - Años invertidos (tendencia temporal incorrecta)
codigo_python_opcion4 <- paste0(codigo_python_base, "
plt.figure(figsize=(5, 3.5))

# Invertir el orden de los años para crear un distractor convincente
años_invertidos = list(reversed(años))
# Usar list() en lugar de .copy() para evitar el error con tuplas
mortalidad_hombres_opcion4 = list(mortalidad_hombres)
mortalidad_mujeres_opcion4 = list(mortalidad_mujeres)
...
")

# Ejecutar los códigos de Python para generar las gráficas
py_run_string(codigo_python_grafica_total)
py_run_string(codigo_python_opcion1)
py_run_string(codigo_python_opcion2)
py_run_string(codigo_python_opcion3)
py_run_string(codigo_python_opcion4)
```

Esta sección:

- Define un código base de Python para generar gráficas con matplotlib
- Crea una gráfica de la mortalidad total
- Genera cuatro opciones de gráficas:
  1. **Opción A (correcta)**: Muestra los datos originales correctamente
  2. **Opción B (incorrecta)**: Muestra valores modificados (multiplicados por un factor)
  3. **Opción C (incorrecta)**: Intercambia los datos de hombres y mujeres
  4. **Opción D (incorrecta)**: Invierte la tendencia temporal (años en orden inverso)
- Ejecuta el código Python para generar las imágenes de las gráficas

### Presentación del Problema (líneas 403-439)

```r
Question
========

Una `r termino_empresa` dedicada a tratar los datos del tránsito en varias de las ciudades del continente americano (`r ciudades_texto`), ha realizado un `r termino_estudio` donde se muestran los `r termino_registro`s de `r termino_mortalidad` por `r termino_accidente` entre los años `r años[1]` y `r años[4]`.

```{r grafica_total, echo=FALSE, results='asis', fig.align='center'}
# Usando método alternativo para incluir imágenes
cat("![](grafica_total.png)")
```

Se necesita clasificar estos datos por género masculino y femenino. La tabla muestra el número de víctimas de género masculino por año.

```{r tabla_tikz, echo=FALSE, results='asis'}
include_tikz(tabla_tikz, 
             name = "tabla_datos", 
             markup = "markdown",
             format = typ,
             packages = c("tikz", "colortbl"),
             width = "8cm")
```

¿Cuál es la gráfica que muestra los resultados de `r termino_mortalidad` por `r termino_accidente` diferenciados por género?

Answerlist
----------

```{r options, echo=FALSE, results='asis'}
# Mostrar las opciones de gráficas usando método alternativo
cat("-\n")
cat("![](opcion1.png)\n\n")
cat("-\n")
cat("![](opcion2.png)\n\n")
cat("-\n")
cat("![](opcion3.png)\n\n")
cat("-\n")
cat("![](opcion4.png)\n\n")
```
```

Esta sección:

- Presenta el problema utilizando los términos y datos aleatorizados
- Muestra la gráfica de mortalidad total
- Incluye la tabla TikZ con los datos de víctimas masculinas
- Plantea la pregunta sobre cuál gráfica representa correctamente los datos por género
- Muestra las cuatro opciones de gráficas generadas

### Solución y Metadatos (líneas 441-471)

```r
Solution
========

La respuesta correcta es la gráfica que representa de manera precisa los datos de `r termino_mortalidad` por género:

```{r solucion_grafica, echo=FALSE, results='asis', fig.align='center'}
# Incluir la imagen de la opción correcta (opción 1) en la sección de solución
cat("![](opcion1.png)")
```

- Número total de víctimas por año: `r paste(format(mortalidad_total, big.mark=","), collapse=", ")`
- Víctimas de género `r tolower(etiqueta_masculino)` por año: `r paste(format(mortalidad_hombres, big.mark=","), collapse=", ")`
- Víctimas de género `r tolower(etiqueta_femenino)` por año: `r paste(format(mortalidad_mujeres, big.mark=","), collapse=", ")`

La gráfica correcta debe mostrar claramente la diferencia entre la mortalidad masculina y femenina para cada año del estudio. En este caso, los datos muestran que hay aproximadamente `r round(mean(mortalidad_hombres/mortalidad_mujeres))` veces más víctimas masculinas que femeninas en los `r termino_accidente`.

Answerlist
----------
- `r if(solucion[1] == 1) "Verdadero" else "Falso"`
- `r if(solucion[2] == 1) "Verdadero" else "Falso"`
- `r if(solucion[3] == 1) "Verdadero" else "Falso"`
- `r if(solucion[4] == 1) "Verdadero" else "Falso"`

Meta-information
================
exname: `r paste0(termino_mortalidad, "_", gsub(" ", "_", termino_accidente), "_genero")`
extype: schoice
exsolution: `r paste(as.integer(solucion), collapse="")`
exshuffle: TRUE
exsection: Interpretación de gráficas
```

Esta sección:

- Proporciona la solución correcta (Opción A)
- Muestra los datos completos para verificación
- Explica por qué la opción A es la correcta
- Incluye la lista de respuestas (Verdadero/Falso)
- Define los metadatos del ejercicio para R-exams:
  - Nombre del ejercicio (aleatorizado)
  - Tipo de ejercicio (schoice = selección única)
  - Vector de solución (1000 = primera opción correcta)
  - Configuración de barajado (TRUE = opciones en orden aleatorio)
  - Sección temática (Interpretación de gráficas)

## Características Técnicas Destacadas

1. **Integración R-Python**: Utiliza `reticulate` para generar gráficas con matplotlib desde R
2. **Visualización avanzada**: Combina TikZ para tablas y matplotlib para gráficas
3. **Alta aleatorización**: Genera múltiples versiones del mismo ejercicio
4. **Distractores pedagógicos**: Las opciones incorrectas representan errores comunes de interpretación
5. **Adaptabilidad**: Funciona con múltiples formatos de salida (HTML, PDF, DOCX)

## Requisitos Técnicos

- R con los paquetes: exams, reticulate, digest, testthat
- Python con matplotlib y numpy
- LaTeX con TikZ y colortbl para la generación de tablas
- Configuración adecuada de pdflatex para procesamiento de código TikZ

## Notas de Implementación

- El ejercicio está diseñado para funcionar con los formatos de salida de R-exams: exams2html, exams2pdf, exams2docx
- Las gráficas se generan dinámicamente en cada ejecución
- La tabla se genera con TikZ para una visualización de alta calidad en todos los formatos
- Se utilizan múltiples técnicas de aleatorización para maximizar la variabilidad de las versiones

## Posibles Mejoras

- Implementar más variaciones en los tipos de gráficas (barras, áreas, etc.)
- Añadir más distractores pedagógicos basados en errores comunes
- Incorporar análisis estadísticos más complejos
- Mejorar la accesibilidad visual de las gráficas
