# Sistema de Etiquetado ICFES para Ejercicios R-exams

Este documento describe el sistema de metadatos que se implementará en todos los ejercicios del repositorio para alinearlos con el marco de referencia ICFES.

## Estructura de Metadatos ICFES

Los metadatos se incluirán en cada archivo .Rmd como un bloque YAML al inicio del documento, después de las especificaciones de salida estándar de R Markdown.

```yaml
---
output:
  pdf_document: default
  html_document: default
---

# Metadatos ICFES
icfes:
  competencia: 
    - interpretacion_representacion  # Valores posibles: interpretacion_representacion, formulacion_ejecucion, argumentacion
  nivel_dificultad: 2                # Valores posibles: 1, 2, 3, 4
  contenido:
    categoria: estadistica           # Valores posibles: algebra_calculo, geometria, estadistica
    tipo: generico                   # Valores posibles: generico, no_generico
  contexto: familiar                 # Valores posibles: familiar, laboral, comunitario, matematico
  eje_axial: eje4                    # Valores posibles: eje1, eje2, eje3, eje4
  componente: aleatorio              # Valores posibles: geometrico_metrico, numerico_variacional, aleatorio
```

## Ejemplo de Implementación

A continuación se muestra un ejemplo de cómo se vería un ejercicio existente con los metadatos ICFES implementados:

```yaml
---
output:
  pdf_document: default
  html_document: default
---

# Metadatos ICFES
icfes:
  competencia: 
    - interpretacion_representacion
  nivel_dificultad: 2
  contenido:
    categoria: estadistica
    tipo: generico
  contexto: comunitario
  eje_axial: eje4
  componente: aleatorio

```{r inicio, include=FALSE}
library(exams)
library(tidyverse)
library(datasets)
library(readxl)
library(data.table)
library(reticulate)

# Configurar Python
use_python("/usr/bin/python3", required = TRUE)

typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```

```{r DefiniciónDeVariables, message=FALSE, warning=FALSE, results='asis'}
# Vector de mascotas
mascotas <- c('loro', 'perro', 'gato', 'gallina', 'hamster', 'cerdo', 'ternero', 'caballo', 'cabra')
...
```

## Guía para Asignación de Valores

### Competencia
- **interpretacion_representacion**: Ejercicios que requieren comprender y transformar información en diferentes formatos.
- **formulacion_ejecucion**: Ejercicios que requieren plantear y ejecutar estrategias para resolver problemas.
- **argumentacion**: Ejercicios que requieren validar o refutar conclusiones, estrategias o soluciones.

### Nivel de Dificultad
- **1**: Ejercicios que requieren lectura de información puntual en situaciones cotidianas.
- **2**: Ejercicios que requieren comparación de datos, identificación de valores representativos.
- **3**: Ejercicios que requieren selección de gráficas, comparación de información, reconocimiento de errores.
- **4**: Ejercicios que requieren resolución de problemas complejos, modelación, manipulación algebraica.

### Categoría de Contenido
- **algebra_calculo**: Ejercicios relacionados con números, operaciones, expresiones algebraicas, funciones.
- **geometria**: Ejercicios relacionados con figuras geométricas, medidas, transformaciones.
- **estadistica**: Ejercicios relacionados con representación de datos, probabilidad, inferencia.

### Tipo de Contenido
- **generico**: Contenidos fundamentales para todo ciudadano.
- **no_generico**: Contenidos específicos del quehacer matemático.

### Contexto
- **familiar**: Situaciones cotidianas del entorno familiar o personal.
- **laboral**: Tareas desarrolladas en el trabajo.
- **comunitario**: Situaciones relacionadas con la interacción social y la sociedad.
- **matematico**: Situaciones abstractas propias de las matemáticas.

### Eje Axial
- **eje1**: Interpretación de tablas, series temporales, diagramas.
- **eje2**: Geometría, visualización, movimientos en el plano.
- **eje3**: Funciones, teoría de números, álgebra.
- **eje4**: Estadística descriptiva, espacios muestrales, probabilidad.

### Componente
- **geometrico_metrico**: Relacionado con formas, tamaños, posiciones, transformaciones.
- **numerico_variacional**: Relacionado con números, operaciones, patrones, variaciones.
- **aleatorio**: Relacionado con datos, azar, probabilidad.

## Implementación en el Flujo de Trabajo

1. **Para ejercicios existentes**:
   - Revisar el contenido del ejercicio
   - Asignar los valores de metadatos ICFES apropiados
   - Añadir el bloque YAML al inicio del archivo

2. **Para nuevos ejercicios**:
   - Utilizar la plantilla con metadatos ICFES
   - Completar los valores según el diseño del ejercicio
   - Verificar la alineación con la matriz ICFES

3. **Para generación masiva**:
   - Modificar los scripts de generación para preservar los metadatos ICFES
   - Incluir los metadatos en los informes de generación
