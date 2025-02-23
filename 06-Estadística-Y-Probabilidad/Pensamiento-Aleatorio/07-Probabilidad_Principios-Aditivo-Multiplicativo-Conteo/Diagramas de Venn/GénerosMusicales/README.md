# Ejercicio de Diagrama de Venn - Géneros Musicales

Este proyecto contiene un ejercicio interactivo basado en R Markdown que genera preguntas aleatorias sobre diagramas de Venn relacionados con preferencias de géneros musicales entre adolescentes.

## Requisitos

Para ejecutar este proyecto, necesitará:

* R (versión 4.x o superior)
* Los siguientes paquetes de R:
  * `exams`
  * `knitr`

Para instalar las dependencias necesarias, ejecute el siguiente código en R:
```R
install.packages(c("exams", "knitr"))
```

## Estructura del Proyecto

- `DVenn_All_GenMus_02.Rmd`: Archivo principal que contiene el ejercicio
- `SemilleroUnico.R`: Script para generación masiva de ejercicios
- `functions_DVenn_All_GenMus_02.R`: Funciones auxiliares
- `test_DVenn_All_GenMus_02.R`: Archivo de prueba
- `salida/`: Directorio para archivos generados
- `docus/`: Documentación y recursos adicionales

## Ejecución

### Generación Individual
Para generar una sola versión del ejercicio:

1. Abra el archivo `DVenn_All_GenMus_02.Rmd` en RStudio o su editor preferido
2. Ejecute el documento usando:
```R
rmarkdown::render("DVenn_All_GenMus_02.Rmd")
```

### Generación Masiva
El archivo `SemilleroUnico.R` permite generar múltiples versiones del ejercicio en diferentes formatos:

1. Abra el archivo `SemilleroUnico.R` en RStudio
2. Configure los parámetros deseados:
   - `archivos`: Número de versiones a generar (default: 300)
   - Formato de salida (PDF, DOCX, HTML, Moodle)
3. Ejecute el script:
```R
source("SemilleroUnico.R")
```

Los archivos generados se almacenarán en el directorio `salida/`.

## Notas

- El ejercicio selecciona aleatoriamente 3 géneros musicales de una lista predefinida
- Genera 4 preguntas aleatorias con sus respectivas respuestas
- Incluye un diagrama de Venn que representa las intersecciones entre los conjuntos

Para más información, consulte la documentación en el directorio `docus/`.