#!/usr/bin/env Rscript

# Script para verificar la solución DEFINITIVA del problema de encoding

# Configuración global de encoding UTF-8 DEFINITIVA
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setlocale("LC_CTYPE", "es_ES.UTF-8")
options(encoding = "UTF-8")
options(OutDec = ".")
options(useFancyQuotes = FALSE)

library(exams)

cat("=================================================================\n")
cat("  PRUEBA DEFINITIVA DE ENCODING PARA 'Evaluación'\n")
cat("=================================================================\n\n")

cat("Configuración actual:\n")
cat("  Locale LC_ALL:", Sys.getlocale("LC_ALL"), "\n")
cat("  Locale LC_CTYPE:", Sys.getlocale("LC_CTYPE"), "\n")
cat("  Encoding:", getOption("encoding"), "\n\n")

# Cambiar al directorio correcto
setwd("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4")

# Archivo de prueba
archivo_test <- "156-tabla_evaluaciones.Rmd"

if (file.exists(archivo_test)) {
  cat("Probando archivo:", archivo_test, "\n\n")
  
  # Generar PDF
  cat("Generando PDF...\n")
  tryCatch({
    exams2pdf(archivo_test,
              n = 1,
              name = "test_encoding_pdf",
              dir = ".",
              edir = ".",
              encoding = "UTF-8",
              template = "pcielo.tex",
              verbose = FALSE)
    cat("✅ PDF generado exitosamente\n\n")
  }, error = function(e) {
    cat("❌ Error al generar PDF:", e$message, "\n\n")
  })
  
  # Generar DOCX
  cat("Generando DOCX...\n")
  tryCatch({
    exams2pandoc(archivo_test,
                 n = 1,
                 name = "test_encoding_docx",
                 dir = ".",
                 edir = ".",
                 encoding = "UTF-8",
                 template = "pcielo.tex",
                 type = "docx",
                 verbose = FALSE)
    cat("✅ DOCX generado exitosamente\n\n")
  }, error = function(e) {
    cat("❌ Error al generar DOCX:", e$message, "\n\n")
  })
  
  # Generar HTML
  cat("Generando HTML...\n")
  tryCatch({
    exams2html(archivo_test,
               n = 1,
               name = "test_encoding_html",
               dir = ".",
               edir = ".",
               encoding = "UTF-8",
               verbose = FALSE)
    cat("✅ HTML generado exitosamente\n\n")
  }, error = function(e) {
    cat("❌ Error al generar HTML:", e$message, "\n\n")
  })
  
  # Verificar contenido del HTML
  html_files <- list.files(pattern = "test_encoding_html.*\\.html$")
  if (length(html_files) > 0) {
    html_content <- readLines(html_files[1], warn = FALSE, encoding = "UTF-8")
    html_text <- paste(html_content, collapse = " ")
    
    cat("Verificando contenido HTML:\n")
    if (grepl("Evaluación", html_text, fixed = TRUE)) {
      cat("  ✅ La palabra 'Evaluación' aparece correctamente\n")
    } else if (grepl("Evaluaci.*U\\+00F3", html_text)) {
      cat("  ❌ La palabra 'Evaluación' SIGUE con problemas de encoding\n")
    } else {
      cat("  ⚠️  No se encontró la palabra 'Evaluación'\n")
    }
  }
  
  cat("\n")
  cat("Archivos generados:\n")
  cat("  - test_encoding_pdf1.pdf\n")
  cat("  - test_encoding_docx1.docx\n")
  cat("  - test_encoding_html1.html\n")
  cat("\n")
  cat("Por favor, abre estos archivos y verifica que 'Evaluación'\n")
  cat("aparece correctamente (NO como 'Evaluaci-U+00F3>n')\n")
  
} else {
  cat("❌ Archivo de prueba no encontrado:", archivo_test, "\n")
}

cat("\n=================================================================\n")
cat("  FIN DE PRUEBA\n")
cat("=================================================================\n")
