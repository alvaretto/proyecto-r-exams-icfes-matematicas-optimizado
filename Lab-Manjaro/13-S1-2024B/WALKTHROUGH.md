# WALKTHROUGH: Ejercicio 13 - Guía Paso a Paso

## Introducción

Esta guía te llevará paso a paso por el uso de ambas versiones del ejercicio: la versión PNG (`probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd`) y la versión TikZ vectorial (`probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd`).

## Preparación del Entorno

### 1. Verificar Directorio de Trabajo
```r
# Asegúrate de estar en el directorio correcto
setwd("Lab-Manjaro/13-S1-2024B")
getwd()  # Debe mostrar: .../Lab-Manjaro/13-S1-2024B
```

### 2. Cargar Librerías Necesarias
```r
# Para ambas versiones
library(exams)
library(knitr)

# Solo para 13.Rmd (versión PNG)
library(reticulate)  # Para integración Python/matplotlib
```

### 3. Verificar Archivos Disponibles
```r
# Listar archivos principales
list.files(pattern = "*.Rmd")
# Debe mostrar: "probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd",
#               "probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd",
#               "Copia de 13.Rmd"
```

## Parte I: Usando la Versión PNG (v1)

### Paso 1: Generación PDF
```r
# Generar un ejercicio en formato PDF
library(exams)
exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="salida")

# Verificar archivos generados
list.files("salida", pattern = "probabilidad_intervalos_curva.*\\.pdf")
```

**Resultado esperado:**

- `salida/probabilidad_intervalos_curva_interpretacion_representacion_n2_v1_1.pdf` - Archivo PDF con el ejercicio
- `tabla_opcion_*.png` - Imágenes de tablas generadas con matplotlib

### Paso 2: Generación HTML
```r
# Generar versión HTML para visualización web
rmarkdown::render("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", "html_document")

# Abrir en navegador (opcional)
browseURL("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.html")
```

**Resultado esperado:**

- `probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.html` - Archivo HTML con imágenes PNG embebidas
- Visualización correcta de tablas en navegador

### Paso 3: Generación DOCX
```r
# Generar versión Word/DOCX
exams2pandoc("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="salida")

# Verificar archivo generado
file.exists("salida/pandoc1.docx")
```

**Resultado esperado:**

- `salida/pandoc1.docx` - Documento Word con imágenes embebidas
- Tablas PNG visibles en Microsoft Word

### Paso 4: Generación Moodle
```r
# Generar XML para Moodle
exams2moodle("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="salida")

# Verificar XML generado
file.exists("salida/moodlequiz.xml")
```

**Resultado esperado:**

- `salida/moodlequiz.xml` - Archivo XML para importar en Moodle
- Referencias correctas a archivos de imagen

## Parte II: Usando la Versión TikZ (tikz_v1)

### Paso 1: Generación PDF (Calidad Vectorial)
```r
# Generar PDF con tablas vectoriales TikZ
exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="salida")

# Verificar archivos generados
list.files("salida", pattern = "probabilidad_intervalos_curva.*tikz.*\\.pdf")
list.files(pattern = "tabla_opcion_.*\\.pdf")
```

**Resultado esperado:**

- `salida/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1_1.pdf` - PDF con tablas vectoriales de alta calidad
- `tabla_opcion_*.pdf` - Tablas TikZ como archivos PDF independientes

### Paso 2: Generación HTML (Conversión Automática)
```r
# Generar HTML con conversión automática TikZ → PNG
rmarkdown::render("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", "html_document")

# Verificar archivos generados
file.exists("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.html")
list.files(pattern = "tabla_opcion_.*\\.png")
```

**Resultado esperado:**

- `probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.html` - HTML con imágenes PNG convertidas automáticamente
- `tabla_opcion_*.png` - Versiones PNG generadas por TikZ

### Paso 3: Generación DOCX (Compatibilidad Completa)
```r
# Generar DOCX con imágenes embebidas
exams2pandoc("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="salida")

# Verificar resultado
file.exists("salida/pandoc1.docx")
```

**Resultado esperado:**

- `salida/pandoc1.docx` - Word con imágenes de alta calidad
- Tablas visibles y bien formateadas

### Paso 4: Generación Moodle (XML Optimizado)
```r
# Generar XML para Moodle
exams2moodle("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="salida")

# Verificar XML
file.exists("salida/moodlequiz.xml")
```

**Resultado esperado:**

- `salida/moodlequiz.xml` - XML con referencias optimizadas
- Imágenes disponibles para subida a Moodle

## Parte III: Comparación Visual

### Paso 1: Comparar Calidad PDF
```r
# Generar ambas versiones para comparación
exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="salida", name="version_png_")
exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="salida", name="version_tikz_")

# Archivos para comparar:
# salida/version_png_1.pdf vs salida/version_tikz_1.pdf
```

**Observaciones esperadas:**

- **PNG**: Tablas con resolución fija, pueden verse pixeladas al hacer zoom
- **TikZ**: Tablas vectoriales, mantienen calidad a cualquier zoom

### Paso 2: Comparar Tamaños de Archivo
```r
# Verificar tamaños de archivos de imagen
png_sizes <- file.size(list.files(pattern = "tabla_opcion_.*\\.png"))
pdf_sizes <- file.size(list.files(pattern = "tabla_opcion_.*\\.pdf"))

cat("Tamaños PNG:", png_sizes, "bytes\n")
cat("Tamaños PDF:", pdf_sizes, "bytes\n")
```

**Resultado típico:**

- PNG: ~40-60 KB por tabla
- PDF: ~10-20 KB por tabla (más eficiente)

## Parte IV: Generación Masiva

### Usando Scripts Especializados

#### Paso 1: Generación Individual Masiva
```r
# Cargar script de generación
source("SemilleroUnico_v2.R")

# El script generará múltiples versiones automáticamente
# Verificar directorio salida/ para archivos generados
```

#### Paso 2: Generación para Moodle
```r
# Cargar script especializado para Moodle
source("SemilleroMoodle_v2.R")

# Genera XML optimizado para importación en Moodle
```

#### Paso 3: Verificar Diversidad
```r
# Probar diversidad de versiones (solo en desarrollo)
Sys.setenv("EXAMS_RUN_DIVERSITY" = "1")
rmarkdown::render("13-TikZ.Rmd", "html_document")

# Debe generar >300 versiones únicas sin errores
```

## Parte V: Solución de Problemas Comunes

### Problema 1: Error "variable de longitud cero"
```r
# Síntoma: Error en chunk generar_tablas_tikz
# Solución: Verificar que el chunk data_generation se ejecutó
exists("datos")
str(datos)  # Debe mostrar estructura con limite1, limite2, etc.
```

### Problema 2: Imágenes no aparecen en DOCX
```r
# Verificar que las imágenes se copiaron al directorio salida
list.files("salida", pattern = "tabla_opcion_.*\\.(png|pdf)")

# Si no están, ejecutar manualmente:
file.copy(list.files(pattern = "tabla_opcion_.*"), "salida/", overwrite = TRUE)
```

### Problema 3: Error de compilación TikZ
```r
# Verificar que TikZ está disponible
system("pdflatex --version")  # Debe mostrar versión de LaTeX

# Si falla, usar versión PNG como alternativa
rmarkdown::render("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", "html_document")
```

### Problema 4: Python no encontrado (solo 13.Rmd)
```r
# Verificar configuración de reticulate
library(reticulate)
py_config()  # Debe mostrar Python disponible

# Si falla, configurar Python:
use_python("/usr/bin/python3")  # Ajustar ruta según sistema
```

## Parte VI: Personalización Avanzada

### Modificar Parámetros de Generación
```r
# Editar rangos de probabilidades en data_generation
# Línea ~68: p_central <- sample(seq(0.40, 0.55, by = 0.01), 1)
# Cambiar a: p_central <- sample(seq(0.30, 0.60, by = 0.01), 1)

# Editar rangos de límites
# Línea ~71: limite1 <- sample(3:6, 1)
# Cambiar a: limite1 <- sample(2:8, 1)
```

### Personalizar Diseño de Tablas (Solo TikZ)
```r
# Modificar función generar_tabla_tikz para cambiar:
# - Colores: \textcolor{blue}{...}
# - Tamaños: scale=1.2
# - Bordes: |c|c| → ||c||c||
```

### Agregar Nuevos Formatos de Salida
```r
# Ejemplo: Generar para BlackBoard
exams2blackboard("13-TikZ.Rmd", n=5, dir="salida")

# Ejemplo: Generar para Canvas
exams2canvas("13-TikZ.Rmd", n=5, dir="salida")
```

## Conclusión

Este walkthrough te ha guiado por:

1. ✅ **Configuración inicial** del entorno de trabajo
2. ✅ **Uso de ambas versiones** (PNG y TikZ) del ejercicio
3. ✅ **Generación en 4 formatos** principales (PDF, HTML, DOCX, Moodle)
4. ✅ **Comparación de calidad** y características técnicas
5. ✅ **Solución de problemas** comunes
6. ✅ **Personalización avanzada** para necesidades específicas

### Próximos Pasos Recomendados

- **Experimentar** con diferentes parámetros de generación
- **Comparar** visualmente la calidad entre versiones PNG y TikZ
- **Integrar** en flujos de trabajo de producción de exámenes
- **Documentar** personalizaciones específicas para tu institución

## Parte VII: Casos de Uso Avanzados

### Caso 1: Producción Masiva para Examen Final
```r
# Generar 100 versiones diferentes para examen presencial
set.seed(12345)  # Para reproducibilidad
exams2pdf(c("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd",
            "probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd"), n=50,
          dir="examen_final",
          name="matematicas_icfes_",
          template="pcielo.tex")

# Resultado: 100 PDFs únicos (50 de cada versión)
```

### Caso 2: Banco de Preguntas para Moodle
```r
# Generar banco extenso para Moodle
exams2moodle(rep("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", 20), n=10,
             dir="banco_moodle",
             name="probabilidad_graficos")

# Resultado: 200 preguntas únicas en XML
```

### Caso 3: Material de Estudio HTML Interactivo
```r
# Generar múltiples versiones HTML para práctica
for(i in 1:10) {
  rmarkdown::render("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", "html_document",
                    output_file = paste0("practica_", i, ".html"))
}

# Resultado: 10 archivos HTML para práctica individual
```

### Caso 4: Comparación A/B de Versiones
```r
# Generar misma semilla para comparar versiones
set.seed(2024)
exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="comparacion", name="version_A_")
set.seed(2024)  # Misma semilla
exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="comparacion", name="version_B_")

# Resultado: PDFs idénticos en contenido, diferentes en calidad visual
```

## Parte VIII: Integración con Sistemas LMS

### Moodle
```r
# Configuración optimizada para Moodle
exams2moodle("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=20,
             dir="moodle_export",
             name="ejercicio_probabilidad",
             converter="pandoc-mathjax")

# Pasos en Moodle:
# 1. Ir a Banco de preguntas
# 2. Importar → Formato XML de Moodle
# 3. Subir moodlequiz.xml
# 4. Subir archivos de imagen desde salida/
```

### Canvas
```r
# Exportar para Canvas LMS
exams2canvas("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=15,
             dir="canvas_export",
             name="probabilidad_intervalos")
```

### BlackBoard
```r
# Exportar para BlackBoard
exams2blackboard("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=25,
                 dir="blackboard_export")
```

## Parte IX: Automatización con Scripts

### Script de Producción Completa
```r
# Crear script automatizado: generar_todo.R
generar_ejercicio_completo <- function(version = "tikz", cantidad = 10) {
  archivo <- if(version == "tikz") "probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd" else "probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd"

  # PDF para impresión
  exams2pdf(archivo, n=cantidad, dir="produccion/pdf")

  # HTML para web
  exams2html(archivo, n=cantidad, dir="produccion/html")

  # Moodle para LMS
  exams2moodle(archivo, n=cantidad, dir="produccion/moodle")

  # DOCX para edición
  exams2pandoc(archivo, n=cantidad, dir="produccion/docx")

  cat("Generación completa:", cantidad, "ejercicios en 4 formatos\n")
}

# Uso:
generar_ejercicio_completo("tikz", 50)
```

### Script de Validación
```r
# Crear script de validación: validar_ejercicios.R
validar_generacion <- function() {
  formatos <- c("pdf", "html", "docx", "moodle")
  versiones <- c("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd",
                 "probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd")

  resultados <- expand.grid(formato=formatos, version=versiones)
  resultados$exitoso <- FALSE

  for(i in 1:nrow(resultados)) {
    tryCatch({
      switch(resultados$formato[i],
        "pdf" = exams2pdf(resultados$version[i], n=1, dir="test"),
        "html" = rmarkdown::render(resultados$version[i], "html_document"),
        "docx" = exams2pandoc(resultados$version[i], n=1, dir="test"),
        "moodle" = exams2moodle(resultados$version[i], n=1, dir="test")
      )
      resultados$exitoso[i] <- TRUE
    }, error = function(e) {
      cat("Error en", resultados$version[i], "-", resultados$formato[i], ":", e$message, "\n")
    })
  }

  return(resultados)
}

# Uso:
resultados_validacion <- validar_generacion()
print(resultados_validacion)
```

## Parte X: Mejores Prácticas

### Gestión de Versiones
```r
# Usar control de versiones para ejercicios
# 1. Commit antes de cambios importantes
# 2. Tags para versiones estables
# 3. Branches para experimentos

# Ejemplo de flujo:
# git checkout -b experimento-nuevos-parametros
# # Modificar ejercicios
# git add .
# git commit -m "Experimento: nuevos rangos de probabilidad"
# git checkout main
# git merge experimento-nuevos-parametros
```

### Documentación de Cambios
```r
# Mantener log de cambios en cada ejercicio
# Agregar comentarios descriptivos:

# CAMBIO 2024-01-15: Ampliado rango de p_central de [0.40,0.55] a [0.35,0.60]
# MOTIVO: Mayor diversidad en ejercicios
# IMPACTO: +40% más combinaciones únicas

p_central <- sample(seq(0.35, 0.60, by = 0.01), 1)  # Modificado 2024-01-15
```

### Testing Sistemático
```r
# Crear suite de tests
test_ejercicio_13 <- function() {
  # Test 1: Diversidad de versiones
  versiones <- replicate(100, {
    datos_test <- generar_datos()
    paste(datos_test$limite1, datos_test$limite2, datos_test$p_central)
  })

  diversidad <- length(unique(versiones))
  cat("Diversidad:", diversidad, "de 100 versiones\n")

  # Test 2: Validez de probabilidades
  for(i in 1:10) {
    datos_test <- generar_datos()
    suma_prob <- datos_test$p_central + 2 * datos_test$p_lateral
    if(abs(suma_prob - 1.0) > 0.001) {
      stop("Error: probabilidades no suman 1.0")
    }
  }

  cat("Tests completados exitosamente\n")
}

# Ejecutar tests
test_ejercicio_13()
```

### Recursos Adicionales

- `README.md` - Documentación técnica completa
- `CONVERSION_TIKZ_TABLAS.md` - Detalles de la implementación TikZ
- `CORRECCION_ERROR_VARIABLE_LONGITUD_CERO.md` - Soluciones a errores específicos
- `OPTIMIZACIONES_APLICADAS.md` - Historial de mejoras implementadas

### Soporte y Comunidad

- **Issues**: Reportar problemas en el repositorio GitHub
- **Documentación**: Mantener actualizada la documentación técnica
- **Ejemplos**: Contribuir con nuevos casos de uso y ejemplos
- **Testing**: Validar cambios en múltiples entornos antes de producción
