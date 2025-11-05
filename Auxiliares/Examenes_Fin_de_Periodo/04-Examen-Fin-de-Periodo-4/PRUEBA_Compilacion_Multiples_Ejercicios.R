#!/usr/bin/env Rscript
# 🧪 SCRIPT DE PRUEBA: Compilación de Múltiples Ejercicios con Imágenes Python
# 
# Este script prueba que la solución implementada con include_supplement()
# funciona correctamente para compilar múltiples ejercicios juntos.
#
# Fecha: 2025-11-05
# Solución: include_supplement() para registrar imágenes Python en r-exams

# Limpiar entorno
rm(list = ls())

# Cargar librerías necesarias
library(exams)
library(reticulate)

# Configurar Python
use_python("/usr/bin/python3", required = TRUE)

# Configuración de directorios
dir_actual <- getwd()
dir_salida <- "salida_prueba"
dir.create(dir_salida, showWarnings = FALSE, recursive = TRUE)

# Definir archivos de ejercicios
ejercicios <- c(
  "01-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd",
  "02-probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd",
  "03-ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd"
)

# Verificar que los archivos existen
cat("\n📋 VERIFICACIÓN DE ARCHIVOS\n")
cat("=" , rep("=", 60), "\n", sep = "")
for (archivo in ejercicios) {
  existe <- file.exists(archivo)
  simbolo <- if (existe) "✅" else "❌"
  cat(simbolo, archivo, "\n")
  if (!existe) {
    stop(paste("Error: No se encontró el archivo", archivo))
  }
}

# Configuración de semilla para reproducibilidad
semilla <- 101
set.seed(semilla)

cat("\n🧪 PRUEBA 1: Compilación Individual de Cada Ejercicio\n")
cat("=" , rep("=", 60), "\n", sep = "")

for (i in seq_along(ejercicios)) {
  cat("\nCompilando:", ejercicios[i], "\n")
  
  tryCatch({
    # Compilar a PDF
    exams2pdf(ejercicios[i],
              n = 1,
              name = paste0("prueba_individual_", i, "_"),
              dir = dir_salida,
              edir = ".",
              verbose = FALSE,
              quiet = TRUE)
    
    cat("✅ Compilación individual exitosa\n")
    
  }, error = function(e) {
    cat("❌ Error en compilación individual:\n")
    cat("   ", e$message, "\n")
  })
}

cat("\n🧪 PRUEBA 2: Compilación de los 3 Ejercicios Juntos (PDF)\n")
cat("=" , rep("=", 60), "\n", sep = "")

set.seed(semilla)  # Restablecer semilla

tryCatch({
  exams2pdf(ejercicios,
            n = 1,
            name = "prueba_multiples_ejercicios_",
            dir = dir_salida,
            edir = ".",
            verbose = TRUE,
            quiet = FALSE)
  
  cat("\n✅ ¡ÉXITO! Compilación de múltiples ejercicios completada\n")
  
}, error = function(e) {
  cat("\n❌ Error en compilación de múltiples ejercicios:\n")
  cat("   ", e$message, "\n")
  cat("\n⚠️  La solución con include_supplement() puede necesitar ajustes\n")
})

cat("\n🧪 PRUEBA 3: Compilación de los 3 Ejercicios Juntos (DOCX)\n")
cat("=" , rep("=", 60), "\n", sep = "")

set.seed(semilla)  # Restablecer semilla

tryCatch({
  exams2pandoc(ejercicios,
               n = 1,
               name = "prueba_multiples_ejercicios_docx",
               type = "docx",
               template = "plain.tex",
               dir = dir_salida,
               edir = ".",
               verbose = TRUE,
               quiet = FALSE)
  
  cat("\n✅ ¡ÉXITO! Compilación DOCX de múltiples ejercicios completada\n")
  
}, error = function(e) {
  cat("\n❌ Error en compilación DOCX de múltiples ejercicios:\n")
  cat("   ", e$message, "\n")
})

cat("\n📊 RESUMEN DE PRUEBAS\n")
cat("=" , rep("=", 60), "\n", sep = "")
cat("Directorio de salida:", dir_salida, "\n")
cat("Archivos generados:\n")

archivos_salida <- list.files(dir_salida, full.names = FALSE)
if (length(archivos_salida) > 0) {
  for (archivo in archivos_salida) {
    cat("  -", archivo, "\n")
  }
} else {
  cat("  (ninguno)\n")
}

cat("\n✅ Script de prueba completado\n")
cat("=" , rep("=", 60), "\n", sep = "")

