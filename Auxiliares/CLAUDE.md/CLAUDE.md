# Instrucción para Análisis de Imagen Matemática y Generación de Código R-Exams

## TAREA PRINCIPAL
Analiza detalladamente la imagen matemática adjunta (.png) y genera un archivo .Rmd 
compatible con r-exams que recree exactamente el mismo escenario matemático con 
alta precisión y flexibilidad.

## ESPECIFICACIONES TÉCNICAS OBLIGATORIAS
1. **Tecnologías específicas a utilizar**:
   - **Expresiones matemáticas**: Exclusivamente código LateX con sintaxis r-exams
   - **Tablas**: Exclusivamente código TikZ con sintaxis r-exams
   - **Gráficas matemáticas**: Exclusivamente código Python (Reticulate) con sintaxis r-exams
   - **Otras visualizaciones**: Exclusivamente código Python (Reticulate) con sintaxis r-exams
   - **Resto del documento**: Código R con sintaxis r-exams

2. **Estructura del documento**:
   - Configuración inicial (metadatos, bibliotecas, opciones)
   - Definición y aleatorización de variables
   - Generación de elementos visuales (tablas/gráficas)
   - Formulación de pregunta y opciones de respuesta
   - Solución correcta con explicación detallada
   - Metainformación para r-exams

3. **Configuración avanzada**:
   - Optimiza para múltiples formatos: exams2moodle, exams2pdf, exams2pandoc y exams2nops
   - Implementa manejo de fuentes y caracteres especiales
   - Asegura compatibilidad multiplataforma
   - Incluye control de semillas aleatorias para reproducibilidad

## ALEATORIZACIÓN AVANZADA
1. **Requisitos de aleatorización**:
   - **Aleatoriza al menos 10 parámetros** diferentes (valores, nombres, contextos, colores)
   - **Garantiza mínimo 300 variantes distintas** del problema
   - **Conserva coherencia semántica y matemática** en todas las variantes
   - **Implementa al menos tres tipos de tendencias** en datos numéricos (crecientes, decrecientes, oscilantes)
   - **Aleatoriza elementos visuales** (colores, estilos, posiciones) manteniendo claridad didáctica

2. **Técnicas de aleatorización**:
   - Utiliza generadores de semillas pseudoaleatorias
   - Implementa listas de elementos contextuales (nombres, lugares, situaciones)
   - Crea variaciones paramétricas con restricciones de validez
   - Genera datos correlacionados con patrones realistas

## ESTILO Y FORMATO
1. **Codificación**:
   - Emplea nombres de variables, en español, descriptivos y consistentes
   - Comenta, en español, exhaustivamente cada sección del código
   - Utiliza indentación y espaciado para mejorar legibilidad
   - Modulariza el código para facilitar modificaciones

2. **Calidad visual**:
   - Configura tamaños apropiados para figuras y tablas, según salidas exams2*.
   - Implementa esquemas de color accesibles y distinguibles
   - Asegura etiquetado claro de ejes y elementos
   - Mantén consistencia estilística en todo el documento
   - Mantén la disposición espacial original de todas las gráficas

## VALIDACIÓN Y COMPROBACIÓN
1. Incluye verificaciones automáticas de coherencia matemática
2. Implementa pruebas de validez para valores generados aleatoriamente
3. Asegura que todas las respuestas incorrectas sean plausibles pero inequívocamente erróneas
4. Verifica el rango y distribución de valores para evitar casos extremos

## ESTRUCTURA ESPERADA DEL CÓDIGO
```r
---
[Metadatos y configuración YAML]
---
```{r inicio, include=FALSE}
[Configuración inicial y carga de bibliotecas]
```

```{r DefinicionDeVariables, message=FALSE, warning=FALSE, results='asis'}
[Aleatorización y definición de variables]
```

```{r generar_elementos_visuales}
[Código para tablas con TikZ y/o gráficas con Python]
```

Question
========
[Texto de la pregunta utilizando variables aleatorizadas]

[Elementos visuales como tablas y gráficos]

Answerlist
----------
[Opciones de respuesta]

Solution
========
[Explicación de la solución]

Answerlist
----------
[Indicadores de verdadero/falso para cada opción]

Meta-information
================
[Metadatos del ejercicio]
```

## NOMBRE DEL ARCHIVO
Nombra el archivo .Rmd siguiendo este formato: 
"[concepto-matemático]_[tipo-problema]_v[versión].Rmd"

## REFERENCIA
Utiliza como referencia técnica el código ejemplo proporcionado, adaptándolo al 
escenario de la imagen analizada. Mejora cualquier aspecto posible: estructura, 
aleatorización, visualización, pruebas de validez o documentación.


## CÓDIGO 1 DE EJEMPLO COMPLETO
Usa el siguiente código como referencia técnica, adaptándolo al escenario de la 
imagen analizada:

```r
---
output:
  html_document: default
  word_document: default
  pdf_document: default
---
```{r setup, include=FALSE}
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

library(exams)
library(reticulate)
library(digest)
library(testthat)

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

```{r DefinicionDeVariables, message=FALSE, warning=FALSE, results='asis'}
options(OutDec = ".")  # Asegurar punto decimal en este chunk

# Establecer semilla aleatoria
set.seed(sample(1:10000, 1))

# Aleatorizar ciudades para el contexto del problema
ciudades <- c("Lima", "Bogotá", "Ciudad de México", "Santiago", "Buenos Aires", 
              "Quito", "Caracas", "La Paz", "Asunción", "Montevideo", 
              "San José", "Panamá", "Managua", "Tegucigalpa", "San Salvador",
              "Santo Domingo", "Medellín", "Cali", "Barranquilla", "Cartagena")
ciudades_seleccionadas <- sample(ciudades, sample(3:5, 1))
ciudades_texto <- paste(ciudades_seleccionadas, collapse = ", ")

# Aleatorizar términos para el contexto del problema
terminos_estudio <- c("estudio", "análisis", "investigación", "informe", "reporte")
termino_estudio <- sample(terminos_estudio, 1)

terminos_accidentes <- c("accidentes de tránsito", "siniestros viales", "incidentes de tráfico", 
                         "colisiones vehiculares", "percances en carretera")
termino_accidente <- sample(terminos_accidentes, 1)

terminos_mortalidad <- c("mortalidad", "fallecimientos", "muertes", "víctimas fatales", "decesos")
termino_mortalidad <- sample(terminos_mortalidad, 1)

terminos_empresas <- c("empresa", "organización", "entidad", "institución", "compañía", "consultora")
termino_empresa <- sample(terminos_empresas, 1)

terminos_registro <- c("registro", "conteo", "recuento", "estadística", "cifra")
termino_registro <- sample(terminos_registro, 1)

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

mortalidad_total <- numeric(4)

if (tendencia == "creciente") {
  factor_incremento <- seq(1, 1.2, length.out = 4)
  mortalidad_total <- round(base_mortalidad * factor_incremento + rnorm(4, 0, variacion_maxima * 0.3))
} else if (tendencia == "decreciente") {
  factor_decremento <- seq(1.2, 1, length.out = 4)
  mortalidad_total <- round(base_mortalidad * factor_decremento + rnorm(4, 0, variacion_maxima * 0.3))
} else if (tendencia == "pico") {
  factores <- c(1, 1.1, 1.2, 1.05)
  mortalidad_total <- round(base_mortalidad * factores + rnorm(4, 0, variacion_maxima * 0.3))
} else if (tendencia == "valle") {
  factores <- c(1.15, 1.05, 1, 1.1)
  mortalidad_total <- round(base_mortalidad * factores + rnorm(4, 0, variacion_maxima * 0.3))
} else { # ondulante
  factores <- c(1, 1.15, 1.05, 1.2)
  mortalidad_total <- round(base_mortalidad * factores + rnorm(4, 0, variacion_maxima * 0.3))
}

# Asegurar que todos los valores sean positivos y tengan magnitud adecuada
mortalidad_total <- pmax(mortalidad_total, base_mortalidad * 0.9)
mortalidad_total <- pmin(mortalidad_total, base_mortalidad * 1.3)
mortalidad_total <- round(mortalidad_total)

# Generar datos para hombres (aproximadamente 75-85% del total - proporción realista)
proporcion_hombres <- runif(4, 0.75, 0.85)
mortalidad_hombres <- round(mortalidad_total * proporcion_hombres)

# Calcular datos para mujeres (el resto)
mortalidad_mujeres <- mortalidad_total - mortalidad_hombres

# Aleatorizar colores de las gráficas
colores_disponibles <- c("blue", "red", "green", "purple", "orange", "brown", "black", "magenta", "cyan")
color_hombres_correcto <- sample(colores_disponibles, 1)
colores_disponibles <- colores_disponibles[colores_disponibles != color_hombres_correcto]
color_mujeres_correcto <- sample(colores_disponibles, 1)

# Aleatorizar etiquetas de género
etiquetas_masculino <- c("Género Masculino")
etiqueta_masculino <- sample(etiquetas_masculino, 1)

etiquetas_femenino <- c("Género Femenino")
etiqueta_femenino <- sample(etiquetas_femenino, 1)

# Establecer la gráfica de líneas separadas A como la única correcta
opciones <- c("lineas_separadas_A", "lineas_separadas_B", "lineas_separadas_C", "lineas_separadas_D")
opcion_correcta <- "lineas_separadas_A"
indice_correcto <- 1  # Índice de "lineas_separadas_A" en el vector opciones

# Vector de solución para r-exams
solucion <- integer(4)
solucion[indice_correcto] <- 1
```

```{r generar_codigo_tikz}
options(OutDec = ".")  # Asegurar punto decimal en este chunk

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

```{r generar_graficas_python}
options(OutDec = ".")  # Asegurar punto decimal en este chunk

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

# Aleatorizar colores para el gráfico total
color_total <- sample(c("red", "blue", "green", "purple", "orange"), 1)

# Código para graficar los totales - CORREGIDO
codigo_python_grafica_total <- paste0(codigo_python_base, "
# Configuración de la figura
plt.figure(figsize=(6, 3.5))

# Datos totales
mortalidad_total = [h + m for h, m in zip(mortalidad_hombres, mortalidad_mujeres)]

# Graficar los puntos y líneas - SINTAXIS CORREGIDA
plt.plot(años, mortalidad_total, marker='o', color='", color_total, "', linestyle='-', linewidth=2, markersize=8)

# Añadir etiquetas a cada punto
for x, y in zip(años, mortalidad_total):
    plt.text(x, y + max(mortalidad_total)*0.02, f'{y:,}', ha='center', va='bottom', fontweight='bold', color='", color_total, "')

# Configuración del gráfico
plt.grid(True, linestyle='--', alpha=0.7)
plt.xticks(años)
plt.ylim(min(mortalidad_total) * 0.8, max(mortalidad_total) * 1.1)
plt.xlabel('Año')
plt.ylabel('Número de víctimas')
plt.tight_layout()

# Guardar gráfica
plt.savefig('grafica_total.png', dpi=150)
plt.close()
")

# Código para las cuatro opciones de gráficas de líneas separadas - CORREGIDO

# Opción A: CORRECTA - Representa los datos originales correctamente
codigo_python_opcion1 <- paste0(codigo_python_base, "
plt.figure(figsize=(5, 3.5))

# Graficar líneas separadas para hombres y mujeres (datos correctos) - SINTAXIS CORREGIDA
plt.plot(años, mortalidad_hombres, marker='o', color=color_hombres, linestyle='-', label=etiqueta_masculino, linewidth=2)
plt.plot(años, mortalidad_mujeres, marker='o', color=color_mujeres, linestyle='-', label=etiqueta_femenino, linewidth=2)

# Añadir etiquetas a cada punto
offset_hombres = max(mortalidad_hombres)*0.01
offset_mujeres = max(mortalidad_mujeres)*0.1
for x, y in zip(años, mortalidad_hombres):
    plt.text(x, y + offset_hombres, f'{y:,}', ha='center', va='bottom', color=color_hombres, fontsize=8)
for x, y in zip(años, mortalidad_mujeres):
    plt.text(x, y - offset_mujeres, f'{y:,}', ha='center', va='top', color=color_mujeres, fontsize=8)

# Configuración del gráfico
plt.grid(True, linestyle='--', alpha=0.7)
plt.xticks(años)
plt.ylim(0, max(mortalidad_hombres) * 1.15)
plt.xlabel('Año')
plt.ylabel('Número de víctimas')
plt.legend()
plt.tight_layout()

# Guardar gráfica
plt.savefig('opcion1.png', dpi=150)
plt.close()
")

# Aleatorizar distractores para las otras opciones
# Opción B: INCORRECTA - Valores duplicados o modificados de forma significativa
distractor_b_factor <- runif(1, 1.8, 2.2)
codigo_python_opcion2 <- paste0(codigo_python_base, "
plt.figure(figsize=(5, 3.5))

# Crear datos incorrectos para esta opción
# Distractor: valores significativamente más altos
mortalidad_hombres_opcion2 = [int(h * ", distractor_b_factor, ") for h in mortalidad_hombres]
mortalidad_mujeres_opcion2 = mortalidad_mujeres  # Mantener estos valores correctos

# Aleatorizar colores diferentes
color_hombres_b = '", sample(colores_disponibles[colores_disponibles != color_hombres_correcto], 1), "'
color_mujeres_b = '", sample(colores_disponibles[colores_disponibles != color_mujeres_correcto], 1), "'

# Graficar líneas separadas con datos incorrectos - SINTAXIS CORREGIDA
plt.plot(años, mortalidad_hombres_opcion2, marker='o', color=color_hombres_b, linestyle='-', label=etiqueta_masculino, linewidth=2)
plt.plot(años, mortalidad_mujeres_opcion2, marker='o', color=color_mujeres_b, linestyle='-', label=etiqueta_femenino, linewidth=2)

# Añadir etiquetas a cada punto
offset_hombres = max(mortalidad_hombres_opcion2)*0.01
offset_mujeres = max(mortalidad_mujeres_opcion2)*0.1
for x, y in zip(años, mortalidad_hombres_opcion2):
    plt.text(x, y + offset_hombres, f'{y:,}', ha='center', va='bottom', color=color_hombres_b, fontsize=8)
for x, y in zip(años, mortalidad_mujeres_opcion2):
    plt.text(x, y - offset_mujeres, f'{y:,}', ha='center', va='top', color=color_mujeres_b, fontsize=8)

# Configuración del gráfico
plt.grid(True, linestyle='--', alpha=0.7)
plt.xticks(años)
plt.ylim(0, max(mortalidad_hombres_opcion2) * 1.15)
plt.xlabel('Año')
plt.ylabel('Número de víctimas')
plt.legend()
plt.tight_layout()

# Guardar gráfica
plt.savefig('opcion2.png', dpi=150)
plt.close()
")

# Opción C: INCORRECTA - Intercambia hombres y mujeres
codigo_python_opcion3 <- paste0(codigo_python_base, "
plt.figure(figsize=(5, 3.5))

# Intercambiar datos para esta opción (distractor)
mortalidad_hombres_opcion3 = mortalidad_mujeres  # Las líneas que deberían ser de mujeres
mortalidad_mujeres_opcion3 = mortalidad_hombres  # Las líneas que deberían ser de hombres

# Aleatorizar colores diferentes
color_hombres_c = '", sample(colores_disponibles[colores_disponibles != color_hombres_correcto], 1), "'
color_mujeres_c = '", sample(colores_disponibles[colores_disponibles != color_mujeres_correcto], 1), "'

# Graficar líneas separadas con etiquetas incorrectas - SINTAXIS CORREGIDA
plt.plot(años, mortalidad_hombres_opcion3, marker='o', color=color_hombres_c, linestyle='-', label=etiqueta_femenino, linewidth=2)
plt.plot(años, mortalidad_mujeres_opcion3, marker='o', color=color_mujeres_c, linestyle='-', label=etiqueta_masculino, linewidth=2)

# Añadir etiquetas a cada punto
offset_hombres = max(mortalidad_hombres_opcion3)*0.1
offset_mujeres = max(mortalidad_mujeres_opcion3)*0.02
for x, y in zip(años, mortalidad_hombres_opcion3):
    plt.text(x, y + offset_hombres, f'{y:,}', ha='center', va='bottom', color=color_hombres_c, fontsize=8)
for x, y in zip(años, mortalidad_mujeres_opcion3):
    plt.text(x, y - offset_mujeres, f'{y:,}', ha='center', va='top', color=color_mujeres_c, fontsize=8)

# Configuración del gráfico
plt.grid(True, linestyle='--', alpha=0.7)
plt.xticks(años)
plt.ylim(0, max(mortalidad_mujeres_opcion3) * 1.2)
plt.xlabel('Año')
plt.ylabel('Número de víctimas')
plt.legend()
plt.tight_layout()

# Guardar gráfica
plt.savefig('opcion3.png', dpi=150)
plt.close()
")

# Opción D: INCORRECTA - Años invertidos (tendencia temporal incorrecta)
# CORREGIDO - Eliminado el uso de .copy() que causaba el error
codigo_python_opcion4 <- paste0(codigo_python_base, "
plt.figure(figsize=(5, 3.5))

# Invertir el orden de los años para crear un distractor convincente
años_invertidos = list(reversed(años))
# Usar list() en lugar de .copy() para evitar el error con tuplas
mortalidad_hombres_opcion4 = list(mortalidad_hombres)
mortalidad_mujeres_opcion4 = list(mortalidad_mujeres)

# Aleatorizar colores diferentes
color_hombres_d = '", sample(colores_disponibles[colores_disponibles != color_hombres_correcto], 1), "'
color_mujeres_d = '", sample(colores_disponibles[colores_disponibles != color_mujeres_correcto], 1), "'

# Graficar líneas con años invertidos - SINTAXIS CORREGIDA
plt.plot(años, list(reversed(mortalidad_hombres_opcion4)), marker='o', color=color_hombres_d, linestyle='-', label=etiqueta_masculino, linewidth=2)
plt.plot(años, list(reversed(mortalidad_mujeres_opcion4)), marker='o', color=color_mujeres_d, linestyle='-', label=etiqueta_femenino, linewidth=2)

# Añadir etiquetas a cada punto
offset_hombres = max(mortalidad_hombres_opcion4)*0.01
offset_mujeres = max(mortalidad_mujeres_opcion4)*0.1
for x, y in zip(años, list(reversed(mortalidad_hombres_opcion4))):
    plt.text(x, y + offset_hombres, f'{y:,}', ha='center', va='bottom', color=color_hombres_d, fontsize=8)
for x, y in zip(años, list(reversed(mortalidad_mujeres_opcion4))):
    plt.text(x, y - offset_mujeres, f'{y:,}', ha='center', va='top', color=color_mujeres_d, fontsize=8)

# Configuración del gráfico
plt.grid(True, linestyle='--', alpha=0.7)
plt.xticks(años)
plt.ylim(0, max(mortalidad_hombres_opcion4) * 1.15)
plt.xlabel('Año')
plt.ylabel('Número de víctimas')
plt.legend()
plt.tight_layout()

# Guardar gráfica
plt.savefig('opcion4.png', dpi=150)
plt.close()
")

# Ejecutar los códigos de Python para generar las gráficas
py_run_string(codigo_python_grafica_total)
py_run_string(codigo_python_opcion1)
py_run_string(codigo_python_opcion2)
py_run_string(codigo_python_opcion3)
py_run_string(codigo_python_opcion4)
```

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

## CÓDIGO 2 DE EJEMPLO COMPLETO
Usa también el siguiente código como referencia técnica, adaptándolo al escenario de la 
imagen analizada:

```r
---
output:
  word_document: default
  html_document: default
  pdf_document: default
---

# Metadatos ICFES
icfes:
  competencia:
    - formulacion_ejecucion
  nivel_dificultad: 3
  contenido:
    categoria: geometria
    tipo: no_generico
  contexto: matematico
  eje_axial: eje2
  componente: geometrico_metrico

```{r setup, include=FALSE}
# Configuración para todos los formatos de salida
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")

# Configurar el motor LaTeX globalmente
options(tikzLatex = "pdflatex")
options(tikzXelatex = FALSE)
options(tikzLatexPackages = c(
  "\\usepackage{tikz}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage{amsmath,amssymb}"
))

library(exams)
library(reticulate)
library(digest)
library(testthat)

typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.showtext = FALSE,
  fig.cap = "",
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150,
  fig.width = 4,      # Reducido de 5 a 4
  fig.height = 4,     # Reducido de 5 a 4
  out.width = "40%"   # Reducido de 60% a 40%
)

# Configuración para chunks de Python
knitr::knit_engines$set(python = function(options) {
  knitr::engine_output(options, options$code, '')
})

# Asegurar que Python esté correctamente configurado
use_python(Sys.which("python"), required = TRUE)
```

```{r DefinicionDeVariables, message=FALSE, warning=FALSE, results='asis'}
options(OutDec = ".")  # Asegurar punto decimal en este chunk

# Establecer semilla aleatoria
set.seed(sample(1:10000, 1))

# Aleatorizamos los nombres para contextualizar el problema
nombres <- c("Camilo", "Andrés", "Sofía", "Manuel", "Laura", "Carlos",
             "Daniela", "Miguel", "Valentina", "Eduardo", "Natalia",
             "José", "Isabella", "Gabriel", "Mariana", "Santiago", "Lucía",
             "Alejandro", "Catalina", "Mateo", "Valeria", "Sebastián", "Juliana")
nombre <- sample(nombres, 1)

# Aleatorizamos los tipos de recipientes
recipientes <- c("cilindro", "tubo", "conducto", "tanque cilíndrico",
                 "recipiente cilíndrico", "contenedor cilíndrico",
                 "ducto cilíndrico", "depósito cilíndrico", "cañería cilíndrica")
recipiente <- sample(recipientes, 1)

# Aleatorizar adjetivos para el cilindro interno
adjetivos_interno <- c("interno", "hueco", "vacío", "interior", "central", "medio")
adjetivo_interno <- sample(adjetivos_interno, 1)

# Aleatorizar líquidos
liquidos <- c("aceite", "combustible", "líquido", "fluido", "agua", "refrigerante",
              "solución", "mezcla", "sustancia")
liquido <- sample(liquidos, 1)

# Aleatorizar lo que se desea calcular
calculos <- c("la cantidad de", "el volumen de", "cuánto", "qué cantidad de",
              "cuántos litros de", "qué volumen de", "la capacidad de")
calculo <- sample(calculos, 1)

# Aleatorizar verbos para la acción
verbos <- c("llenar", "contener", "almacenar", "ocupar", "requerir")
verbo <- sample(verbos, 1)

# Aleatorizar medidas del cilindro (manteniendo consistencia matemática)
# Primero generamos el radio interno (entre 0.1 y 0.4 metros)
r_int <- round(runif(1, 0.1, 0.4), 2)

# Luego generamos el radio externo, asegurando que sea mayor que el interno
# Radio externo entre 1.5 y 2.5 veces el radio interno
factor_radio <- runif(1, 1.5, 2.5)
r_ext <- round(r_int * factor_radio, 2)

# Calculamos el diámetro externo (exactamente 2 veces el radio externo)
d_ext <- 2 * r_ext

# Altura del cilindro (entre 0.5 y 4 metros)
altura <- round(runif(1, 0.5, 4), 2)

# Calcular el grosor de la pared del cilindro
grosor <- round(r_ext - r_int, 1)  # Asegurar que grosor se redondea a 1 decimal

# Verificar coherencia física
if (grosor < 0.05) {
  # Asegurar un grosor mínimo de 0.05 unidades
  grosor <- round(runif(1, 0.05, 0.2), 2)
  r_ext <- r_int + grosor
  # Asegurar que el diámetro externo sea exactamente 2 veces el radio externo
  d_ext <- 2 * r_ext
}

# Calcular el volumen necesario (es el volumen del cilindro interior)
volumen <- round(pi * (r_int^2) * altura, 2)

# Aleatorizar unidades de medida
unidades_longitud <- c("m", "cm", "dm")
unidad <- sample(unidades_longitud, 1)

# Ajustar valores según la unidad
factor_conversion <- 1
if (unidad == "cm") {
  factor_conversion <- 100
  d_ext <- d_ext * factor_conversion
  r_int <- r_int * factor_conversion
  altura <- altura * factor_conversion
  volumen <- volumen * (factor_conversion^3)
  r_ext <- r_ext * factor_conversion
  grosor <- grosor * factor_conversion
} else if (unidad == "dm") {
  factor_conversion <- 10
  d_ext <- d_ext * factor_conversion
  r_int <- r_int * factor_conversion
  altura <- altura * factor_conversion
  volumen <- volumen * (factor_conversion^3)
  r_ext <- r_ext * factor_conversion
  grosor <- grosor * factor_conversion
}

# Redondear todos los valores para mayor claridad
d_ext <- round(d_ext, 1)
r_int <- round(r_int, 1)
altura <- round(altura, 1)
r_ext <- round(r_ext, 1)
grosor <- round(grosor, 1)
volumen <- round(volumen, 1)

# La respuesta correcta es "C) Altura del cilindro"
# Ya que para calcular el volumen necesitaríamos conocer la altura
opcion_correcta <- 3 # Índice de "Altura del cilindro" en el vector de opciones

# Vector de solución para r-exams (1 para la correcta, 0 para las incorrectas)
solucion <- c(0, 0, 1, 0)
```

```{r generar_cilindro_python, message=FALSE, warning=FALSE}
options(OutDec = ".")  # Asegurar punto decimal en este chunk

# Función para formatear valores numéricos (1 decimal para decimales, 0 para enteros)
formatear_numero <- function(numero) {
  if (numero %% 1 == 0) {
    return(as.integer(numero))
  } else {
    return(sprintf("%.1f", numero))
  }
}

# Formatear los valores
d_ext_fmt <- formatear_numero(d_ext)
r_ext_fmt <- formatear_numero(r_ext)
r_int_fmt <- formatear_numero(r_int)
altura_fmt <- formatear_numero(altura)
grosor_fmt <- formatear_numero(grosor)  # Asegura consistencia usando el grosor precalculado

# Código Python para generar el cilindro hueco similar a la imagen de referencia
codigo_python <- paste0("
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Ellipse, FancyArrowPatch, Polygon

# Parámetros del cilindro (en español)
radio_interno = ", r_int, "  # ", unidad, "
radio_externo = ", r_ext, "  # ", unidad, "
altura = ", altura, "  # ", unidad, "
diametro_externo = ", d_ext, "  # ", unidad, "
grosor = ", grosor, "  # ", unidad, " (valor calculado directamente en R)
unidad = '", unidad, "'

# Crear figura con tamaño reducido
fig, ax = plt.subplots(figsize=(4, 4))  # Reducido de 5x5 a 4x4

# Colores
azul_cilindro = '#4682B4'  # Azul para contornos
azul_relleno = '#EBF5FF'   # Azul muy claro para el relleno
gris_linea = '#777777'     # Gris para líneas punteadas

# Factores de perspectiva para las elipses
factor_elipse = 0.35
vradio_ext = radio_externo * factor_elipse
vradio_int = radio_interno * factor_elipse

# Centros del cilindro
centro_superior = (0, altura)
centro_inferior = (0, 0)

# Rellenar el cilindro con color azul claro
# Crear coordenadas para el cuerpo exterior visible
angulos = np.linspace(-np.pi, 0, 50)
x_ext_frontal = radio_externo * np.cos(angulos)
y_ext_frontal_inf = vradio_ext * np.sin(angulos)
y_ext_frontal_sup = altura + vradio_ext * np.sin(angulos)

# Polígono para el cuerpo exterior
puntos_cuerpo_ext = []
for i in range(len(angulos)):
    puntos_cuerpo_ext.append((x_ext_frontal[i], y_ext_frontal_inf[i]))
puntos_cuerpo_ext.append((radio_externo, 0))
puntos_cuerpo_ext.append((radio_externo, altura))
for i in range(len(angulos)-1, -1, -1):
    puntos_cuerpo_ext.append((x_ext_frontal[i], y_ext_frontal_sup[i]))
poligono_cuerpo_ext = Polygon(puntos_cuerpo_ext, closed=True, facecolor=azul_relleno, 
                         edgecolor='none', alpha=0.5, zorder=1)
ax.add_patch(poligono_cuerpo_ext)

# Polígono para el cuerpo interior (hueco)
x_int_frontal = radio_interno * np.cos(angulos)
y_int_frontal_inf = vradio_int * np.sin(angulos)
y_int_frontal_sup = altura + vradio_int * np.sin(angulos)

puntos_cuerpo_int = []
for i in range(len(angulos)):
    puntos_cuerpo_int.append((x_int_frontal[i], y_int_frontal_inf[i]))
puntos_cuerpo_int.append((radio_interno, 0))
puntos_cuerpo_int.append((radio_interno, altura))
for i in range(len(angulos)-1, -1, -1):
    puntos_cuerpo_int.append((x_int_frontal[i], y_int_frontal_sup[i]))
poligono_cuerpo_int = Polygon(puntos_cuerpo_int, closed=True, facecolor='white', 
                         edgecolor='none', zorder=2)
ax.add_patch(poligono_cuerpo_int)

# Dibujar líneas verticales
ax.plot([radio_externo, radio_externo], [0, altura], color=azul_cilindro, linewidth=1.2, zorder=5)
ax.plot([-radio_externo, -radio_externo], [0, altura], color=azul_cilindro, linewidth=1.2, zorder=5)
ax.plot([radio_interno, radio_interno], [0, altura], color=azul_cilindro, linewidth=1.2, zorder=5)
ax.plot([-radio_interno, -radio_interno], [0, altura], color=azul_cilindro, linewidth=1.2, zorder=5)

# LÍNEAS PUNTEADAS PARA LAS PARTES NO VISIBLES DE LAS TAPAS SUPERIOR E INFERIOR
# Semielipses posteriores (arco trasero)
angulos_traseros = np.linspace(0, np.pi, 100)  # Más puntos para curvas más suaves
x_trasero_ext = radio_externo * np.cos(angulos_traseros)
y_trasero_ext_inf = vradio_ext * np.sin(angulos_traseros)
y_trasero_ext_sup = altura + vradio_ext * np.sin(angulos_traseros)
x_trasero_int = radio_interno * np.cos(angulos_traseros)
y_trasero_int_inf = vradio_int * np.sin(angulos_traseros)
y_trasero_int_sup = altura + vradio_int * np.sin(angulos_traseros)

# Dibujar líneas punteadas para las semielipses posteriores
linea_punteada = {'linestyle': '--', 'color': gris_linea, 'linewidth': 1.0, 'zorder': 3,
                 'dashes': [3, 2]}  # Líneas punteadas correctas

# Semielipses posteriores inferiores
ax.plot(x_trasero_ext, y_trasero_ext_inf, **linea_punteada)
ax.plot(x_trasero_int, y_trasero_int_inf, **linea_punteada)

# Semielipses posteriores superiores
ax.plot(x_trasero_ext, y_trasero_ext_sup, **linea_punteada)
ax.plot(x_trasero_int, y_trasero_int_sup, **linea_punteada)

# Dibujar semielipses frontales visibles
# Semielipses frontales inferiores
angulos_frontales = np.linspace(-np.pi, 0, 100)
x_frontal_ext = radio_externo * np.cos(angulos_frontales)
y_frontal_ext_inf = vradio_ext * np.sin(angulos_frontales)
x_frontal_int = radio_interno * np.cos(angulos_frontales)
y_frontal_int_inf = vradio_int * np.sin(angulos_frontales)

ax.plot(x_frontal_ext, y_frontal_ext_inf, color=azul_cilindro, linewidth=1.2, zorder=5)
ax.plot(x_frontal_int, y_frontal_int_inf, color=azul_cilindro, linewidth=1.2, zorder=5)

# Semielipses frontales superiores
y_frontal_ext_sup = altura + vradio_ext * np.sin(angulos_frontales)
y_frontal_int_sup = altura + vradio_int * np.sin(angulos_frontales)
ax.plot(x_frontal_ext, y_frontal_ext_sup, color=azul_cilindro, linewidth=1.2, zorder=5)
ax.plot(x_frontal_int, y_frontal_int_sup, color=azul_cilindro, linewidth=1.2, zorder=5)

# Añadir puntos centrales
ax.plot(0, altura, 'o', color=azul_cilindro, markersize=3, zorder=6)
ax.plot(0, 0, 'o', color=azul_cilindro, markersize=3, zorder=6)

# Añadir flechas de radio
ax.add_patch(FancyArrowPatch((0, altura), (radio_interno, altura), arrowstyle='->', 
                           color=azul_cilindro, linewidth=0.8, mutation_scale=8, zorder=6))
ax.add_patch(FancyArrowPatch((0, 0), (radio_externo, 0), arrowstyle='->', 
                           color=azul_cilindro, linewidth=0.8, mutation_scale=8, zorder=6))

# FLECHAS Y ETIQUETAS DE DIMENSIONES
# Propiedades comunes para flechas
flecha_props = dict(arrowstyle='<->', color='black', linewidth=0.6, mutation_scale=6)

# 1. DIÁMETRO EXTERNO
diam_y = altura + vradio_ext*2 + altura*0.15
ax.add_patch(FancyArrowPatch((-radio_externo, diam_y), (radio_externo, diam_y), **flecha_props))
ax.text(0, diam_y + altura*0.06, f'Diámetro externo = {", d_ext_fmt, "} {unidad}', 
       fontweight='bold', ha='center', va='center', fontsize=7)

# 2. ALTURA
altura_x = radio_externo + radio_externo*0.3
ax.add_patch(FancyArrowPatch((altura_x, 0), (altura_x, altura), **flecha_props))
ax.text(altura_x + radio_externo*0.1, altura/2, 'Altura', 
       fontweight='bold', rotation=90, ha='center', va='center', fontsize=7)

# 3. RADIO INTERNO
radio_int_label_pos = (radio_interno*2.0, altura + altura*0.2)
ax.plot([radio_interno, radio_int_label_pos[0]], [altura, radio_int_label_pos[1]], 'k-', linewidth=0.6)
ax.plot([radio_int_label_pos[0]-0.1, radio_int_label_pos[0]], [radio_int_label_pos[1], radio_int_label_pos[1]], 'k-', linewidth=0.6)
ax.text(radio_int_label_pos[0], radio_int_label_pos[1], 
       f'Radio interno = {", r_int_fmt, "} {unidad}', 
       fontweight='bold', ha='left', va='center', fontsize=7)

# 4. GROSOR
grosor_start = -radio_externo + (radio_externo - radio_interno)/2
ax.add_patch(FancyArrowPatch((-radio_interno, altura), (-radio_externo, altura), **flecha_props, zorder=6))
grosor_label_pos = (-radio_externo*2.0, altura + altura*0.2)
ax.plot([-radio_externo, grosor_label_pos[0]], [altura, grosor_label_pos[1]], 'k-', linewidth=0.6)
ax.plot([grosor_label_pos[0], grosor_label_pos[0]+0.1], [grosor_label_pos[1], grosor_label_pos[1]], 'k-', linewidth=0.6)
ax.text(grosor_label_pos[0], grosor_label_pos[1], 
       f'Grosor = {", grosor_fmt, "} {unidad}', 
       fontweight='bold', ha='right', va='center', fontsize=7)

# 5. RADIO EXTERNO
radio_ext_label_pos = (radio_externo*2.0, -altura*0.2)
ax.plot([radio_externo, radio_ext_label_pos[0]], [0, radio_ext_label_pos[1]], 'k-', linewidth=0.6)
ax.plot([radio_ext_label_pos[0]-0.1, radio_ext_label_pos[0]], [radio_ext_label_pos[1], radio_ext_label_pos[1]], 'k-', linewidth=0.6)
ax.text(radio_ext_label_pos[0], radio_ext_label_pos[1], 
       f'Radio externo = {", r_ext_fmt, "} {unidad}', 
       fontweight='bold', ha='left', va='center', fontsize=7)

# Ajustar límites y aspecto de la figura
margen = max(radio_externo, altura) * 0.7  # Reducido de 0.8 a 0.7
ax.set_xlim(-radio_externo - margen, radio_externo + margen)
ax.set_ylim(-margen*0.6, altura + margen)  # Ajustado para reducir espacio vertical
ax.set_aspect('equal')
ax.axis('off')

# Guardar la figura con espacio ajustado para etiquetas
plt.tight_layout(pad=0.8)  # Reducido de 1.0 a 0.8
plt.savefig('cilindro_hueco.png', dpi=150, bbox_inches='tight', transparent=True)  # Reducido DPI de 200 a 150
plt.close()
")

# Ejecutar código Python para generar la figura
py_run_string(codigo_python)
```

Question
========

`r nombre` desea saber cuánto(a) `r liquido` se necesita para llenar un `r recipiente` interno(a), pero solamente cuenta con las medidas de las dimensiones que muestra la figura.

```{r mostrar_cilindro_python, echo=FALSE, results='asis', fig.align='center'}
# Detectar si se está generando para Moodle
es_moodle <- (match_exams_call() %in% c("exams2moodle", "exams2qti12", "exams2qti21", "exams2openolat"))

# Mostrar la imagen del cilindro generada con Python con ancho controlado
if(es_moodle) {
  cat("![](cilindro_hueco.png){width=30%}")  # Más pequeño para Moodle
} else {
  cat("![](cilindro_hueco.png){width=50%}")  # Tamaño normal para PDF/Word
}
```

```{r generar_tabla_tikz, echo=FALSE, results='asis'}
# Aleatorizar colores de la tabla
color_fondo_tabla <- sample(c("orange", "blue", "green", "cyan"), 1)
intensidad_color <- sample(c(10, 15, 20, 25), 1)
color_tabla <- paste0(color_fondo_tabla, "!", intensidad_color)

# Detectar si se está generando para Moodle para ajustar el ancho de la tabla
es_moodle <- (match_exams_call() %in% c("exams2moodle", "exams2qti12", "exams2qti21", "exams2openolat"))
ancho_tabla <- if(es_moodle) "4.5cm" else "6cm"

# Crear tabla con TikZ asegurando que grosor sea el mismo que en la imagen
tabla_tikz <- c(
  "\\begin{tikzpicture}",
  "\\node[inner sep=0pt] {",
  "  \\begin{tabular}{|c|c|c|}",
  "    \\hline",
  paste0("    \\rowcolor{", color_tabla, "}"),
  "    \\textbf{Dimensión} & \\textbf{Valor} & \\textbf{Unidad} \\\\",
  "    \\hline",
  paste0("    Radio interno & ", r_int, " & ", unidad, " \\\\"),
  "    \\hline",
  paste0("    Radio externo & ", r_ext, " & ", unidad, " \\\\"),
  "    \\hline",
  paste0("    Diámetro externo & ", d_ext, " & ", unidad, " \\\\"),
  "    \\hline",
  paste0("    Grosor & ", grosor, " & ", unidad, " \\\\"),
  "    \\hline",
  "  \\end{tabular}",
  "};",
  "\\end{tikzpicture}"
)

# Incluir la tabla TikZ
include_tikz(tabla_tikz,
             name = "tabla_datos",
             markup = "markdown",
             format = typ,
             packages = c("tikz", "colortbl"),
             width = ancho_tabla)
```

¿Cuál medida le falta a `r nombre` para hallar la cantidad deseada?

Answerlist
----------
- Radio externo.
- Diámetro externo.
- Altura del cilindro.
- Perímetro del cilindro.

Solution
========

La respuesta correcta es: **Altura del cilindro**.

Para calcular el volumen de `r liquido` necesario para llenar el `r recipiente` `r adjetivo_interno`, necesitamos calcular el volumen de un cilindro, cuya fórmula es:

$V = \pi \cdot r^2 \cdot h$

Donde:

- $r$ es el radio (en este caso, el radio interno = `r r_int` `r unidad`)
- $h$ es la altura del cilindro

En el problema se nos proporciona:

- Diámetro externo = `r d_ext` `r unidad`
- Radio interno = `r r_int` `r unidad`

Sin embargo, no se nos proporciona la altura del cilindro, que es esencial para calcular el volumen. Por lo tanto, a `r nombre` le falta conocer la altura del cilindro para poder calcular la cantidad de `r liquido` necesaria.

Si tuviéramos la altura, el volumen se calcularía así:

$V = \pi \cdot (`r r_int`)^2 \cdot h = `r round(pi * (r_int^2), 2)` \cdot h$ `r unidad`³

Answerlist
----------
- Falso
- Falso
- Verdadero
- Falso

Meta-information
================
exname: volumen_cilindro_hueco
extype: schoice
exsolution: 0010
exshuffle: TRUE
exsection: Geometría|Volumen|Cilindro
```

## RESUMEN e INSTRUCCIONES FINALES
1. Analiza la imagen proporcionada por el usuario
2. Genera un archivo .Rmd que reproduzca fielmente el escenario matemático
3. Asegúrate de seguir estrictamente las tecnologías especificadas
4. Implementa una aleatorización amplia y coherente
5. Nombra tu archivo siguiendo la convención establecida
6. Asegura que el código sea reproducible en diferentes entornos
7. Verifica la coherencia matemática y la validez de los datos generados.
8. Evita redundancia en las preguntas.
9. Mantén la claridad y legibilidad del código.
10. Asegúrate de que el código sea completamente funcional y libre de errores
11. Implementa Feedback adaptativo: Proporcionar retroalimentación específica según el tipo de error cometido.

Aplica siempre pensamiento profundo, paso a paso.
Define nombres/alias de variables en español.
Genera el código completo y adaptado al escenario de la imagen proporcionada.
Usa siempre el MCP Context7
Usa siempre el MCP Brave Search
Usa siempre el MCP Sequential Thinking
Agrega sistema de Metadatos ICFES, en el yaml.
Garantiza la coherencia matemática después de los cambios.
Evita aplicar cambios no solicitados
Responde siempre en español.
