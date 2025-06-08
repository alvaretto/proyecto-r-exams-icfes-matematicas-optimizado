#!/usr/bin/env Rscript

# Script para instalar paquetes Python en el entorno de reticulate
library(reticulate)

cat("=== Instalando paquetes Python para reticulate ===\n")

# Instalar pandas
cat("\n1. Instalando pandas...\n")
tryCatch({
  py_install("pandas")
  cat("✓ pandas instalado correctamente\n")
}, error = function(e) {
  cat("✗ Error al instalar pandas:", e$message, "\n")
})

# Instalar matplotlib
cat("\n2. Instalando matplotlib...\n")
tryCatch({
  py_install("matplotlib")
  cat("✓ matplotlib instalado correctamente\n")
}, error = function(e) {
  cat("✗ Error al instalar matplotlib:", e$message, "\n")
})

# Verificar instalaciones
cat("\n3. Verificando instalaciones...\n")

tryCatch({
  pd <- import("pandas")
  cat("✓ pandas importado correctamente, versión:", pd$`__version__`, "\n")
}, error = function(e) {
  cat("✗ Error al importar pandas:", e$message, "\n")
})

tryCatch({
  matplotlib <- import("matplotlib")
  cat("✓ matplotlib importado correctamente, versión:", matplotlib$`__version__`, "\n")
}, error = function(e) {
  cat("✗ Error al importar matplotlib:", e$message, "\n")
})

cat("\n=== Instalación completada ===\n")
