#!/usr/bin/env Rscript

# Script para verificar configuración UTF-8
cat("=== VERIFICACIÓN UTF-8 ===\n")
cat("Locale:", Sys.getlocale("LC_CTYPE"), "\n")
cat("Encoding:", getOption("encoding"), "\n")
cat("Prueba caracteres: áéíóú ñ ¿¡\n")
cat("========================\n")
