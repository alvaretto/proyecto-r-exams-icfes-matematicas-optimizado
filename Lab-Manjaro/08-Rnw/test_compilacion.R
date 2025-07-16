# Script para probar la compilación del ejercicio .Rnw
library(exams)

# Configurar directorio de trabajo
setwd("/home/personal/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Lab-Manjaro/08-Rnw")

cat("=== PRUEBA DE COMPILACIÓN ===\n")
cat("Directorio de trabajo:", getwd(), "\n")

# Verificar que el archivo existe
archivo_rnw <- "Ahorro_opciones_porcentaje_interpretacion_representacion_n2_v1.Rnw"
if(file.exists(archivo_rnw)) {
  cat("✅ Archivo .Rnw encontrado:", archivo_rnw, "\n")
} else {
  cat("❌ Archivo .Rnw no encontrado:", archivo_rnw, "\n")
  stop("Archivo no encontrado")
}

# Crear directorio de salida si no existe
if(!dir.exists("salida")) {
  dir.create("salida")
  cat("📁 Directorio 'salida' creado\n")
}

cat("\n=== COMPILACIÓN HTML ===\n")
tryCatch({
  # Compilar a HTML
  exams2html(archivo_rnw, 
             n = 3,  # Generar 3 versiones
             name = "ahorro_test",
             dir = "salida",
             encoding = "UTF-8")
  cat("✅ Compilación HTML exitosa\n")
}, error = function(e) {
  cat("❌ Error en compilación HTML:", e$message, "\n")
})

cat("\n=== COMPILACIÓN PDF ===\n")
tryCatch({
  # Compilar a PDF
  exams2pdf(archivo_rnw, 
            n = 2,  # Generar 2 versiones
            name = "ahorro_test_pdf",
            dir = "salida",
            encoding = "UTF-8")
  cat("✅ Compilación PDF exitosa\n")
}, error = function(e) {
  cat("❌ Error en compilación PDF:", e$message, "\n")
})

cat("\n=== VERIFICACIÓN DE ARCHIVOS GENERADOS ===\n")
archivos_salida <- list.files("salida", full.names = FALSE)
cat("Archivos generados:\n")
for(archivo in archivos_salida) {
  cat("  -", archivo, "\n")
}

cat("\n=== PRUEBA COMPLETADA ===\n")
