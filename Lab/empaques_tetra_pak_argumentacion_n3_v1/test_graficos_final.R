# Script de prueba para verificar que las gráficas funcionan correctamente
# en el ejercicio ICFES de empaques Tetra Pak

# Configurar modo generación
.exams_generation_mode <- TRUE

# Cargar librería
library(exams)

# Generar una versión HTML
cat("Generando versión HTML con gráficas...\n")
tryCatch({
  exams2html("empaques_tetra_pak_argumentacion_n3_v1.Rmd",
             n = 1,
             name = "prueba_graficas_final",
             verbose = FALSE)
}, error = function(e) {
  if(grepl("browser", e$message)) {
    cat("⚠️  Error cosmético del navegador (ignorado)\n")
  } else {
    cat("❌ Error real:", e$message, "\n")
  }
})

# Verificar que se generó el archivo de imagen
if(file.exists("grafico_composicion.png")) {
  cat("✅ Archivo de imagen generado correctamente\n")
  cat("   Tamaño:", file.size("grafico_composicion.png"), "bytes\n")
} else {
  cat("❌ Error: No se generó el archivo de imagen\n")
}

# Verificar que se generó el HTML
html_files <- list.files(pattern = "prueba_graficas_final.*\\.html")
if(length(html_files) > 0) {
  cat("✅ Archivo HTML generado:", html_files[1], "\n")
  cat("   Tamaño:", file.size(html_files[1]), "bytes\n")
} else {
  cat("❌ Error: No se generó el archivo HTML\n")
}

cat("\n🎯 Prueba completada. Las gráficas deberían visualizarse correctamente en R-exams.\n")
