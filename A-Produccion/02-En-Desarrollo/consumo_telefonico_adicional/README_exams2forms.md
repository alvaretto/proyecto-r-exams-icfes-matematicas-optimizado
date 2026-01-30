# Guía de Uso: exams2forms

## Descripción

`exams2forms` es un paquete de R que permite generar formularios HTML interactivos a partir de ejercicios R/exams. Los formularios generados incluyen:

- ✅ Verificación automática de respuestas
- 📊 Retroalimentación inmediata
- 🔄 Múltiples variaciones aleatorias
- 📱 Compatible con dispositivos móviles
- 🎯 Ideal para autoevaluación y aprendizaje autodirigido

## Instalación

```r
# Instalar desde CRAN
install.packages("exams2forms")

# Cargar el paquete
library(exams2forms)
```

## Uso Básico

### Opción 1: Generar Archivo HTML Standalone

**IMPORTANTE**: Para generar archivos HTML standalone, usa `exams2webquiz()` en lugar de `exams2forms()`:

```r
library(exams2forms)

# Generar formulario interactivo HTML standalone
exams2webquiz("consumo_telefonico_adicional_n2_v1.Rmd",
              n = 3,  # Número de variaciones
              dir = "Salida",
              name = "ejercicio_interactivo",
              edir = ".",
              encoding = "UTF-8",
              title = "Evaluación Interactiva de Matemáticas",
              solution = TRUE,   # Mostrar botón de solución
              shuffle = TRUE,    # Mezclar opciones
              mathjax = TRUE,    # Habilitar MathJax
              browse = FALSE)    # No abrir navegador automáticamente
```

**Nota**: `exams2webquiz()` genera automáticamente los archivos CSS y JS necesarios.

### Opción 2: Embeber en Documento Rmarkdown/Quarto

Crear un archivo `.Rmd` o `.qmd` con el siguiente contenido:

````markdown
---
title: "Ejercicios Interactivos de Matemáticas"
output:
  html_document:
    css: webex.css
    includes:
      after_body: webex.js
---

```{r setup, include=FALSE}
library(exams2forms)
```

```{r ejercicio, echo=FALSE, results="asis"}
# Crear directorio temporal para archivos intermedios
dir_temp <- tempfile("exams_temp_")
dir.create(dir_temp, recursive = TRUE)

# Generar ejercicios
exams2forms("consumo_telefonico_adicional_n2_v1.Rmd",
            n = 3,
            dir = dir_temp,  # Usar directorio temporal
            edir = ".",
            title = "Consumo Telefónico",
            browse = FALSE)

# Limpiar directorio temporal
unlink(dir_temp, recursive = TRUE)
```
````

**Nota importante**: No se puede usar `dir = NULL` directamente con `exams2forms` porque genera un error de directorio temporal. Siempre especifica un directorio de salida (puede ser temporal).

## Archivos Necesarios

Para que los formularios funcionen correctamente, necesitas los archivos CSS y JavaScript:

### webex.css y webex.js

Estos archivos vienen incluidos con el paquete `exams2forms`. Puedes copiarlos a tu directorio de trabajo:

```r
# Copiar archivos CSS y JS al directorio actual
file.copy(
  system.file("webex.css", package = "exams2forms"),
  "webex.css"
)

file.copy(
  system.file("webex.js", package = "exams2forms"),
  "webex.js"
)
```

## Características del Formulario Generado

Los formularios interactivos incluyen:

1. **Botón de Verificación (✓)**: Verifica si la respuesta es correcta
2. **Botón de Solución (?)**: Muestra la solución completa
3. **Botón Siguiente (↺)**: Cambia a la siguiente variación del ejercicio
4. **Contador de Respuestas**: Muestra cuántas respuestas correctas hay

## Compatibilidad

El archivo `.Rmd` existente (`consumo_telefonico_adicional_n2_v1.Rmd`) es completamente compatible con `exams2forms`. No requiere modificaciones.

### Tipos de Ejercicios Soportados

- ✅ Single-choice (schoice)
- ✅ Multiple-choice (mchoice)
- ✅ Numeric (num)
- ✅ Text (string)
- ✅ Cloze (combinación de los anteriores)

## Ejemplo de Uso en SemilleroUnico_v2.R

El archivo `SemilleroUnico_v2.R` ya incluye código comentado para usar `exams2forms`. Para activarlo:

1. Descomentar las líneas correspondientes
2. Instalar el paquete `exams2forms`
3. Ejecutar el script

```r
# Descomentar estas líneas en SemilleroUnico_v2.R
library(exams2forms)

exams2forms(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_sin_extension, "_forms_"),
            dir = dir_salida,
            edir = dir_ejercicios,
            encoding = "UTF-8",
            title = "Evaluación Interactiva de Matemáticas",
            verbose = TRUE,
            solution = TRUE,
            shuffle = TRUE,
            mathjax = TRUE)
```

## Recursos Adicionales

- **Documentación oficial**: https://www.r-exams.org/tutorials/exams2forms/
- **Tutorial completo**: https://www.r-exams.org/general/cran_release_242/
- **Paquete CRAN**: https://cran.r-project.org/package=exams2forms

## Notas Importantes

1. **Navegadores compatibles**: Chrome, Firefox, Safari, Edge (versiones recientes)
2. **JavaScript requerido**: Los formularios requieren JavaScript habilitado
3. **Offline**: Los formularios funcionan sin conexión a internet una vez descargados
4. **Privacidad**: No se envían datos a servidores externos, todo funciona localmente

## Solución de Problemas

### Error: "package 'exams2forms' is not available"

```r
# Verificar que el paquete esté instalado
install.packages("exams2forms")
```

### Error: "argumento tiene longitud cero" o "Temporary directory 'tdir' must not be the same"

Este error ocurre cuando se usa `dir = NULL` sin especificar un directorio temporal. **Solución**:

```r
# INCORRECTO - genera error
exams2forms("ejercicio.Rmd", n = 3, dir = NULL)

# CORRECTO - usar directorio temporal
dir_temp <- tempfile("exams_temp_")
dir.create(dir_temp, recursive = TRUE)
exams2forms("ejercicio.Rmd", n = 3, dir = dir_temp)
unlink(dir_temp, recursive = TRUE)
```

### Los formularios no se muestran correctamente

- Verificar que los archivos `webex.css` y `webex.js` estén en el mismo directorio
- Verificar que JavaScript esté habilitado en el navegador
- Revisar la consola del navegador para errores

### Las fórmulas matemáticas no se renderizan

- Asegurar que `mathjax = TRUE` esté configurado
- Verificar conexión a internet (MathJax se carga desde CDN por defecto)

