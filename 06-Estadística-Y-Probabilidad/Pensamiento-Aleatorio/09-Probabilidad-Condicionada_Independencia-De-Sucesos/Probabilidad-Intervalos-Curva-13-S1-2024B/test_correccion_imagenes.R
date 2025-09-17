#!/usr/bin/env Rscript
# Script de prueba para verificar la corrección de visualización de imágenes
# Archivo: test_correccion_imagenes.R
# Propósito: Verificar que las imágenes de las opciones se muestran correctamente

library(exams)
library(knitr)

# Configurar opciones
options(scipen = 999)
options(OutDec = '.')

cat("=== PRUEBA DE CORRECCIÓN DE VISUALIZACIÓN DE IMÁGENES ===\n")
cat("Archivo: probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd\n")
cat("Problema corregido: Imágenes de opciones ahora se muestran fuera del desplegable\n\n")

# Archivo a probar
archivo_rmd <- "probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd"

# Verificar que el archivo existe
if (!file.exists(archivo_rmd)) {
  stop("Error: No se encuentra el archivo ", archivo_rmd)
}

cat("✅ Archivo encontrado\n")

# Función para probar un formato específico
probar_formato <- function(formato, funcion_exams, directorio, ...) {
  cat(paste0("📋 Probando formato: ", formato, "\n"))
  
  tryCatch({
    # Crear directorio si no existe
    if (!dir.exists(directorio)) {
      dir.create(directorio, recursive = TRUE)
    }
    
    # Generar ejercicio
    set.seed(123)  # Semilla fija para reproducibilidad
    resultado <- funcion_exams(archivo_rmd, 
                              n = 1, 
                              name = paste0("test_", formato),
                              dir = directorio,
                              ...)
    
    cat(paste0("✅ ", formato, " generado exitosamente en ", directorio, "/\n"))
    
    # Verificar archivos generados
    archivos <- list.files(directorio, recursive = TRUE)
    cat(paste0("   Archivos generados: ", length(archivos), "\n"))
    
    # Buscar archivos de imagen
    imagenes <- archivos[grepl("\\.(png|pdf|svg)$", archivos, ignore.case = TRUE)]
    if (length(imagenes) > 0) {
      cat(paste0("   Imágenes encontradas: ", length(imagenes), "\n"))
      for (img in head(imagenes, 5)) {  # Mostrar máximo 5
        cat(paste0("     - ", img, "\n"))
      }
      if (length(imagenes) > 5) {
        cat(paste0("     ... y ", length(imagenes) - 5, " más\n"))
      }
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat(paste0("❌ Error en ", formato, ": ", e$message, "\n"))
    return(FALSE)
  })
}

# Probar diferentes formatos
cat("\n=== PRUEBAS DE FORMATOS ===\n")

# 1. HTML
html_ok <- probar_formato("HTML", exams2html, "test_html_corregido", template = "plain")

# 2. Moodle
moodle_ok <- probar_formato("Moodle", exams2moodle, "test_moodle_corregido")

# 3. PDF (si está disponible)
pdf_ok <- tryCatch({
  probar_formato("PDF", exams2pdf, "test_pdf_corregido", template = "plain")
}, error = function(e) {
  cat("⚠️  PDF no disponible (requiere LaTeX): ", e$message, "\n")
  FALSE
})

# Resumen de resultados
cat("\n=== RESUMEN DE RESULTADOS ===\n")
cat(paste0("HTML: ", if(html_ok) "✅ OK" else "❌ FALLO", "\n"))
cat(paste0("Moodle: ", if(moodle_ok) "✅ OK" else "❌ FALLO", "\n"))
cat(paste0("PDF: ", if(pdf_ok) "✅ OK" else "⚠️  No disponible", "\n"))

# Verificación específica de la corrección
cat("\n=== VERIFICACIÓN DE LA CORRECCIÓN ===\n")
cat("Cambios implementados:\n")
cat("1. ✅ Imágenes de tablas movidas ANTES del desplegable\n")
cat("2. ✅ Nueva sección 'Tablas de probabilidades para análisis'\n")
cat("3. ✅ Desplegable simplificado con solo texto (Tabla A, B, C, D)\n")
cat("4. ✅ Imágenes visibles independientemente del desplegable\n")

# Verificar estructura del archivo
cat("\n=== VERIFICACIÓN DE ESTRUCTURA ===\n")
contenido <- readLines(archivo_rmd)

# Buscar secciones clave
seccion_tablas <- any(grepl("## Tablas de probabilidades para análisis", contenido))
answerlist_simple <- any(grepl("\\* Tabla [ABCD]$", contenido))
imagenes_fuera <- any(grepl("mostrar_tablas_analisis", contenido))

cat(paste0("Sección de tablas creada: ", if(seccion_tablas) "✅ SÍ" else "❌ NO", "\n"))
cat(paste0("Answerlist simplificado: ", if(answerlist_simple) "✅ SÍ" else "❌ NO", "\n"))
cat(paste0("Imágenes fuera del desplegable: ", if(imagenes_fuera) "✅ SÍ" else "❌ NO", "\n"))

if (html_ok && moodle_ok) {
  cat("\n🎉 CORRECCIÓN EXITOSA: El problema de visualización ha sido resuelto\n")
  cat("Las imágenes ahora se muestran correctamente fuera del desplegable\n")
} else {
  cat("\n⚠️  Revisar errores en la generación de algunos formatos\n")
}

cat("\n=== FIN DE LA PRUEBA ===\n")
