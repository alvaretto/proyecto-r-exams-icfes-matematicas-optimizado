# Guía de Implementación del Marco ICFES en Ejercicios R-exams

Esta guía explica cómo implementar el sistema de etiquetado ICFES en el flujo de trabajo de creación y modificación de ejercicios para el repositorio de Matemáticas ICFES.

## Documentos de Referencia

1. **matriz_alineacion_icfes.md**: Mapeo entre la estructura del repositorio y el marco ICFES.
2. **plantilla_metadatos_icfes.md**: Descripción del sistema de etiquetado ICFES.
3. **plantilla_ejercicio_icfes.Rmd**: Plantilla para crear nuevos ejercicios con metadatos ICFES.
4. **actualizar_metadatos_icfes.R**: Script para añadir metadatos ICFES a ejercicios existentes.

## Flujo de Trabajo para Nuevos Ejercicios

### 1. Crear un nuevo ejercicio usando la plantilla

Para crear un nuevo ejercicio, copia la plantilla `plantilla_ejercicio_icfes.Rmd` y modifícala según tus necesidades:

```bash
cp plantilla_ejercicio_icfes.Rmd mi_nuevo_ejercicio.Rmd
```

### 2. Completar los metadatos ICFES

Edita la sección de metadatos ICFES al inicio del archivo:

```yaml
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

### 3. Desarrollar el ejercicio

Desarrolla el ejercicio siguiendo las mejores prácticas de R-exams y asegurándote de que se alinee con los metadatos ICFES especificados.

### 4. Probar el ejercicio

Genera una versión de prueba del ejercicio para verificar que funciona correctamente:

```r
library(exams)
exams2html("mi_nuevo_ejercicio.Rmd")
```

### 5. Ubicar el ejercicio en la estructura del repositorio

Coloca el ejercicio en la carpeta correspondiente según su categoría y tema:

```bash
mv mi_nuevo_ejercicio.Rmd 06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/01-Variables-Cualitativas_Distribucion-De-Frecuencias/
```

## Flujo de Trabajo para Ejercicios Existentes

### 1. Añadir metadatos ICFES manualmente

Para añadir metadatos ICFES a un ejercicio existente, edita el archivo y añade la sección de metadatos ICFES después del encabezado YAML:

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
  contexto: familiar
  eje_axial: eje4
  componente: aleatorio
```

### 2. Usar el script de actualización automática

Para añadir metadatos ICFES a múltiples ejercicios existentes, usa el script `actualizar_metadatos_icfes.R`:

```r
source("actualizar_metadatos_icfes.R")
update_all_metadata("06-Estadística-Y-Probabilidad")
```

## Generación Masiva de Ejercicios con Metadatos ICFES

Para la generación masiva de ejercicios, asegúrate de que los scripts de generación preserven los metadatos ICFES:

1. Modifica los scripts de generación para incluir los metadatos ICFES en los archivos generados.
2. Asegúrate de que los metadatos ICFES se incluyan en los informes de generación.

## Consulta de Ejercicios por Metadatos ICFES

Para consultar ejercicios por sus metadatos ICFES, puedes usar el siguiente script:

```r
# Función para buscar ejercicios por metadatos ICFES
find_exercises_by_metadata <- function(dir_path, metadata_filter) {
  # Buscar archivos .Rmd
  rmd_files <- list.files(dir_path, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)
  
  # Filtrar por metadatos ICFES
  filtered_files <- c()
  
  for (file_path in rmd_files) {
    # Leer el contenido del archivo
    content <- readLines(file_path, warn = FALSE)
    
    # Verificar si tiene metadatos ICFES
    if (any(grepl("# Metadatos ICFES", content))) {
      # Verificar si cumple con el filtro
      matches_filter <- TRUE
      
      for (filter_key in names(metadata_filter)) {
        filter_value <- metadata_filter[[filter_key]]
        
        # Buscar el valor en los metadatos ICFES
        if (!any(grepl(paste0(filter_key, ".*", filter_value), content, ignore.case = TRUE))) {
          matches_filter <- FALSE
          break
        }
      }
      
      if (matches_filter) {
        filtered_files <- c(filtered_files, file_path)
      }
    }
  }
  
  return(filtered_files)
}

# Ejemplo de uso:
# find_exercises_by_metadata("06-Estadística-Y-Probabilidad", list(competencia = "argumentacion", nivel_dificultad = "4"))
```

## Mejores Prácticas

1. **Consistencia**: Usa siempre los valores definidos en la guía de metadatos ICFES.
2. **Precisión**: Asegúrate de que los metadatos ICFES reflejen con precisión el contenido del ejercicio.
3. **Completitud**: Incluye todos los campos de metadatos ICFES en cada ejercicio.
4. **Actualización**: Mantén los metadatos ICFES actualizados cuando modifiques un ejercicio.
5. **Documentación**: Documenta cualquier decisión especial sobre la clasificación de un ejercicio.

## Recursos Adicionales

- [Documentación oficial del ICFES](https://www.icfes.gov.co)
- [Estándares Básicos de Competencias en Matemáticas](https://www.mineducacion.gov.co/1621/articles-116042_archivo_pdf2.pdf)
- [R-exams Documentation](http://www.r-exams.org/)
