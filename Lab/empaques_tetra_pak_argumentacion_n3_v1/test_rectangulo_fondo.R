# Script de prueba para verificar el rectángulo negro como fondo
# Verificar que el archivo se puede procesar correctamente

library(exams)
library(reticulate)

# Cambiar al directorio del ejercicio
setwd("Lab/empaques_tetra_pak_argumentacion_n3_v1")

# Intentar generar una versión HTML del ejercicio
tryCatch({
  # Generar ejercicio en HTML para verificar funcionamiento
  exams2html("empaques_tetra_pak_argumentacion_n3_v1.Rmd", 
             name = "test_rectangulo_fondo",
             dir = "test_output",
             n = 1)
  
  cat("✅ Ejercicio generado exitosamente con rectángulos negros extra ampliados como fondo\n")
  cat("📁 Archivo generado en: test_output/test_rectangulo_fondo1.html\n")
  cat("🖼️ Imagen del gráfico: test_output/test_rectangulo_fondo1.dir/grafico_composicion.png\n")
  cat("📏 Ancho de rectángulos: len(texto) * 0.045 (extra ampliado para máxima cobertura)\n")
  
}, error = function(e) {
  cat("❌ Error al generar ejercicio:\n")
  cat(paste("Error:", e$message, "\n"))
})