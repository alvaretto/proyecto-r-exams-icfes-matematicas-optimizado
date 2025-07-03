#!/usr/bin/env Rscript

library(reticulate)

# Configurar Python del sistema
python_path <- "/usr/bin/python3"
if (file.exists(python_path)) {
  use_python(python_path, required = TRUE)
  cat("✓ Python configurado en:", python_path, "\n")
  
  # Verificar configuración
  config <- py_config()
  cat("Versión de Python:", config$version, "\n")
  cat("Ejecutable:", config$python, "\n")
} else {
  cat("✗ No se encontró Python en", python_path, "\n")
}
