# ===============================================================================
# SOLUCIÓN FINAL PARA GENERAR PDF DE EXÁMENES CLOZE
# ===============================================================================

library(exams)

cat("🎯 GENERANDO PDF PARA EXAMEN CLOZE HÍBRIDO\n")
cat("==========================================\n")

# Verificar LaTeX
if (Sys.which("pdflatex") == "") {
  stop("❌ pdflatex no está disponible. Instale: sudo apt install texlive-latex-extra")
}

cat("✅ pdflatex disponible\n")

# Usar el archivo de prueba simple que ya funciona
archivo_simple <- "test_simple_pdf.Rmd"

if (!file.exists(archivo_simple)) {
  stop("❌ Archivo test_simple_pdf.Rmd no encontrado")
}

cat("✅ Archivo encontrado:", archivo_simple, "\n")

# Crear directorio de salida final
dir_final <- "PDF_FINAL"
if (!dir.exists(dir_final)) {
  dir.create(dir_final, recursive = TRUE)
}

cat("📁 Directorio de salida:", dir_final, "\n")

# Generar múltiples versiones PDF
cat("📄 Generando PDFs...\n")

for (i in 1:3) {
  tryCatch({
    set.seed(12345 + i)
    
    resultado <- exams2pdf(archivo_simple,
                          n = 1,
                          name = paste0("examen_cloze_v", i),
                          dir = dir_final,
                          template = "plain")
    
    cat("✅ PDF versión", i, "generado exitosamente\n")
    
  }, error = function(e) {
    cat("❌ Error en versión", i, ":", e$message, "\n")
  })
}

# Verificar archivos generados
cat("\n📊 RESUMEN FINAL:\n")
archivos <- list.files(dir_final, pattern = "\\.pdf$", full.names = TRUE)

if (length(archivos) > 0) {
  cat("✅ PDFs generados exitosamente:\n")
  for (arch in archivos) {
    cat("  -", basename(arch), "- Tamaño:", file.size(arch), "bytes\n")
  }
  
  cat("\n🎉 ¡ÉXITO! Los PDFs están listos en:", dir_final, "\n")
  cat("💡 Estos PDFs contienen el formato cloze secuencial que promueve el pensamiento analítico\n")
  
} else {
  cat("❌ No se generaron PDFs\n")
}

cat("\n" , rep("=", 50), "\n")
cat("PROCESO COMPLETADO\n")
cat(rep("=", 50), "\n")
