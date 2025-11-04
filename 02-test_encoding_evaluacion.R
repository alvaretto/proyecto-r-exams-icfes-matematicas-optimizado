#!/usr/bin/env Rscript

# Script para probar la corrección del problema de encoding con "Evaluación"

# Configuración global de encoding UTF-8
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
options(encoding = "UTF-8")

library(exams)

cat("=== PRUEBA DE ENCODING PARA 'Evaluación' ===\n")
cat("Locale actual:", Sys.getlocale("LC_CTYPE"), "\n")
cat("Encoding actual:", getOption("encoding"), "\n")
cat("\n")

# Probar con el archivo que contiene "Evaluación"
archivo_test <- "Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4/156-tabla_evaluaciones.Rmd"

if (file.exists(archivo_test)) {
  cat("Probando archivo:", archivo_test, "\n")
  
  tryCatch({
    # Generar una versión HTML para verificar el encoding
    exams2html(archivo_test, 
               n = 1,
               name = "test_encoding_evaluacion",
               dir = ".",
               edir = ".",
               encoding = "UTF-8",
               verbose = TRUE)
    
    cat("✅ Archivo HTML generado exitosamente\n")
    
    # Leer el archivo HTML generado y verificar si "Evaluación" aparece correctamente
    html_files <- list.files(pattern = "test_encoding_evaluacion.*\\.html$")
    if (length(html_files) > 0) {
      html_content <- readLines(html_files[1], warn = FALSE, encoding = "UTF-8")
      html_text <- paste(html_content, collapse = " ")
      
      if (grepl("Evaluación", html_text)) {
        cat("✅ La palabra 'Evaluación' aparece correctamente en el HTML\n")
      } else if (grepl("Evaluaci.*U\\+00F3", html_text)) {
        cat("❌ La palabra 'Evaluación' sigue apareciendo con problemas de encoding\n")
      } else {
        cat("⚠️  No se encontró la palabra 'Evaluación' en el HTML\n")
      }
      
      # Limpiar archivos de prueba
      unlink(html_files)
      unlink("test_encoding_evaluacion", recursive = TRUE)
    }
    
  }, error = function(e) {
    cat("❌ Error al generar HTML:", e$message, "\n")
  })
  
} else {
  cat("❌ Archivo de prueba no encontrado:", archivo_test, "\n")
}

cat("\n=== FIN DE PRUEBA ===\n")
