#!/usr/bin/env Rscript
# =============================================================================
# SCRIPT DE ACCESO RÁPIDO AL SISTEMA DE TESTING .RNW
# =============================================================================
# Este script permite ejecutar el sistema de testing desde el directorio raíz
# del proyecto sin necesidad de navegar a la carpeta Testing_Rnw
#
# Uso desde el directorio raíz:
#   Rscript test-rnw.R archivo.Rnw                    # Testing rápido
#   Rscript test-rnw.R archivo.Rnw --completo         # Testing completo
#   Rscript test-rnw.R --configurar                   # Configurar sistema
#   Rscript test-rnw.R --ayuda                        # Mostrar ayuda
#
# Autor: Sistema de Testing ICFES
# Fecha: 2025-01-11
# =============================================================================

# Función para mostrar ayuda
mostrar_ayuda <- function() {
  cat("🧪 SISTEMA DE TESTING PARA ARCHIVOS .RNW\n")
  cat("========================================\n\n")
  cat("USO:\n")
  cat("  Rscript test-rnw.R archivo.Rnw                # Testing rápido\n")
  cat("  Rscript test-rnw.R archivo.Rnw --completo     # Testing completo\n")
  cat("  Rscript test-rnw.R --configurar               # Configurar sistema\n")
  cat("  Rscript test-rnw.R --ayuda                    # Mostrar esta ayuda\n\n")
  cat("EJEMPLOS:\n")
  cat("  # Testing rápido de un archivo específico\n")
  cat("  Rscript test-rnw.R \"06-Estadística-Y-Probabilidad/.../archivo.Rnw\"\n\n")
  cat("  # Testing completo con compilación\n")
  cat("  Rscript test-rnw.R \"archivo.Rnw\" --completo\n\n")
  cat("  # Configurar el sistema por primera vez\n")
  cat("  Rscript test-rnw.R --configurar\n\n")
  cat("UBICACIÓN DE ARCHIVOS:\n")
  cat("  📁 Sistema completo: Auxiliares/Testing_Rnw/\n")
  cat("  📖 Documentación: Auxiliares/Testing_Rnw/00-README.md\n")
  cat("  📚 Guía detallada: Auxiliares/Testing_Rnw/07-FLUJO_TRABAJO_TESTING_RNW.md\n\n")
}

# Función principal
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  # Verificar que estamos en el directorio correcto
  if (!dir.exists("Auxiliares/Testing_Rnw")) {
    cat("❌ Error: No se encontró el directorio Auxiliares/Testing_Rnw\n")
    cat("💡 Asegúrese de ejecutar este script desde el directorio raíz del proyecto\n")
    quit(status = 1)
  }
  
  # Procesar argumentos
  if (length(args) == 0) {
    mostrar_ayuda()
    quit(status = 0)
  }
  
  primer_arg <- args[1]
  
  # Comandos especiales
  if (primer_arg == "--ayuda" || primer_arg == "--help" || primer_arg == "-h") {
    mostrar_ayuda()
    quit(status = 0)
  }
  
  if (primer_arg == "--configurar") {
    cat("🔧 Ejecutando configuración del sistema...\n")
    setwd("Auxiliares/Testing_Rnw")
    source("08-configurar_testing.R")
    quit(status = 0)
  }
  
  # Guardar nombre del archivo
  archivo_rnw <- primer_arg

  # Determinar tipo de testing
  testing_completo <- length(args) > 1 && args[2] == "--completo"

  # Cambiar al directorio de testing
  directorio_original <- getwd()
  setwd("Auxiliares/Testing_Rnw")

  # Construir ruta relativa al archivo desde Testing_Rnw
  archivo_relativo <- file.path("../..", archivo_rnw)

  # Verificar que el archivo existe desde el directorio de testing
  if (!file.exists(archivo_relativo)) {
    cat("❌ Error: No se puede acceder al archivo\n")
    cat("📁 Ruta esperada:", archivo_relativo, "\n")
    cat("💡 Verificar que la ruta del archivo sea correcta\n")
    setwd(directorio_original)
    quit(status = 1)
  }
  
  # Ejecutar testing
  if (testing_completo) {
    cat("🧪 Ejecutando testing completo...\n")
    cat("📁 Archivo:", archivo_rnw, "\n")
    cat("🔄 Cambiando a directorio de testing...\n\n")
    
    # Cargar y ejecutar testing completo
    source("06-test_rnw_completo.R")
    resultados <- ejecutar_test_suite_completa(archivo_relativo)
    
    # Determinar código de salida
    if (resultados$porcentaje_exito >= 80 && length(resultados$errores_criticos) == 0) {
      codigo_salida <- 0
    } else {
      codigo_salida <- 1
    }
    
  } else {
    cat("⚡ Ejecutando testing rápido...\n")
    cat("📁 Archivo:", archivo_rnw, "\n")
    cat("🔄 Cambiando a directorio de testing...\n\n")
    
    # Cargar y ejecutar testing simple
    source("05-test_rnw_simple.R")
    resultados <- test_basico_rnw(archivo_relativo)
    
    # Determinar código de salida
    if (resultados$porcentaje_exito >= 80) {
      codigo_salida <- 0
    } else {
      codigo_salida <- 1
    }
  }
  
  # Volver al directorio original
  setwd(directorio_original)
  
  cat("\n🏁 Testing completado desde directorio raíz\n")
  cat("📊 Estado final:", if(codigo_salida == 0) "✅ EXITOSO" else "❌ FALLÓ", "\n")
  
  quit(status = codigo_salida)
}

# Ejecutar solo si no estamos en modo interactivo
if (!interactive()) {
  main()
} else {
  cat("🧪 Script de acceso rápido al sistema de testing .Rnw cargado\n")
  cat("💡 Uso: main() o consultar --ayuda\n")
}
