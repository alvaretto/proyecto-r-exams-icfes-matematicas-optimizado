#!/usr/bin/env Rscript
# =============================================================================
# CONFIGURACIÓN INICIAL DEL SISTEMA DE TESTING .RNW
# =============================================================================
# Este script configura el entorno y verifica que todas las dependencias
# estén disponibles para el sistema de testing de archivos .Rnw
#
# Uso: Rscript 08-configurar_testing.R
#
# Autor: Sistema de Testing ICFES
# Fecha: 2025-01-11
# =============================================================================

# Operador auxiliar
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

cat("🔧 CONFIGURANDO SISTEMA DE TESTING PARA ARCHIVOS .RNW\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# =============================================================================
# FUNCIÓN: verificar_paquetes_r
# =============================================================================
verificar_paquetes_r <- function() {
  cat("📦 Verificando paquetes R...\n")
  
  paquetes_requeridos <- c("testthat", "exams", "knitr", "reticulate", "stringr", "tools")
  paquetes_faltantes <- c()
  
  for (paquete in paquetes_requeridos) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      paquetes_faltantes <- c(paquetes_faltantes, paquete)
      cat("  ❌", paquete, "- NO INSTALADO\n")
    } else {
      cat("  ✅", paquete, "- OK\n")
    }
  }
  
  if (length(paquetes_faltantes) > 0) {
    cat("\n⚠️ Paquetes faltantes detectados. ¿Instalar automáticamente? (y/n): ")
    
    if (interactive()) {
      respuesta <- readline()
      if (tolower(respuesta) %in% c("y", "yes", "s", "si", "sí")) {
        cat("📥 Instalando paquetes faltantes...\n")
        install.packages(paquetes_faltantes)
        cat("✅ Instalación completada\n")
      }
    } else {
      cat("\n💡 Para instalar automáticamente, ejecute:\n")
      cat("install.packages(c(", paste0("'", paquetes_faltantes, "'", collapse = ", "), "))\n")
    }
  }
  
  return(length(paquetes_faltantes) == 0)
}

# =============================================================================
# FUNCIÓN: verificar_python
# =============================================================================
verificar_python <- function() {
  cat("\n🐍 Verificando configuración Python...\n")
  
  # Verificar que reticulate esté disponible
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cat("  ❌ reticulate no está disponible\n")
    return(FALSE)
  }
  
  library(reticulate, quietly = TRUE)
  
  # Verificar Python
  if (!py_available()) {
    cat("  ❌ Python no está disponible\n")
    cat("  💡 Configurar con: use_python('/ruta/a/python')\n")
    return(FALSE)
  } else {
    cat("  ✅ Python disponible:", py_config()$python, "\n")
  }
  
  # Verificar librerías Python
  librerias_python <- c("matplotlib", "numpy")
  python_ok <- TRUE
  
  for (lib in librerias_python) {
    if (py_module_available(lib)) {
      cat("  ✅", lib, "- OK\n")
    } else {
      cat("  ❌", lib, "- NO DISPONIBLE\n")
      python_ok <- FALSE
    }
  }
  
  if (!python_ok) {
    cat("  💡 Instalar con: pip install matplotlib numpy\n")
  }
  
  return(python_ok)
}

# =============================================================================
# FUNCIÓN: verificar_latex
# =============================================================================
verificar_latex <- function() {
  cat("\n📄 Verificando LaTeX...\n")
  
  if (Sys.which("pdflatex") != "") {
    cat("  ✅ pdflatex disponible:", Sys.which("pdflatex"), "\n")
    return(TRUE)
  } else {
    cat("  ❌ pdflatex no encontrado\n")
    cat("  💡 Instalar LaTeX:\n")
    cat("    - Ubuntu/Debian: sudo apt-get install texlive-latex-base\n")
    cat("    - macOS: brew install --cask mactex\n")
    cat("    - Windows: https://miktex.org/\n")
    return(FALSE)
  }
}

# =============================================================================
# FUNCIÓN: verificar_pandoc
# =============================================================================
verificar_pandoc <- function() {
  cat("\n📝 Verificando Pandoc...\n")
  
  if (Sys.which("pandoc") != "") {
    cat("  ✅ pandoc disponible:", Sys.which("pandoc"), "\n")
    return(TRUE)
  } else {
    cat("  ❌ pandoc no encontrado\n")
    cat("  💡 Instalar Pandoc:\n")
    cat("    - Ubuntu/Debian: sudo apt-get install pandoc\n")
    cat("    - macOS: brew install pandoc\n")
    cat("    - Windows: https://pandoc.org/installing.html\n")
    return(FALSE)
  }
}

# =============================================================================
# FUNCIÓN: probar_sistema
# =============================================================================
probar_sistema <- function() {
  cat("\n🧪 Probando sistema de testing...\n")
  
  tryCatch({
    # Cargar utilidades básicas
    source("01-testing_rnw_utils.R")
    cat("  ✅ Utilidades básicas cargadas\n")
    
    # Probar función básica
    if (exists("validar_estructura_rnw")) {
      cat("  ✅ Funciones de validación disponibles\n")
    } else {
      cat("  ❌ Funciones de validación no encontradas\n")
      return(FALSE)
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("  ❌ Error cargando sistema:", e$message, "\n")
    return(FALSE)
  })
}

# =============================================================================
# FUNCIÓN: crear_alias_utiles
# =============================================================================
crear_alias_utiles <- function() {
  cat("\n🔗 Creando alias útiles...\n")
  
  # Crear script de alias
  alias_script <- c(
    "#!/bin/bash",
    "# Alias útiles para testing de archivos .Rnw",
    "",
    "# Función para testing rápido",
    "test-rnw() {",
    "  if [ -z \"$1\" ]; then",
    "    echo \"Uso: test-rnw archivo.Rnw\"",
    "    return 1",
    "  fi",
    "  cd \"$(dirname \"$0\")\" && Rscript 05-test_rnw_simple.R \"$1\"",
    "}",
    "",
    "# Función para testing completo",
    "test-rnw-full() {",
    "  if [ -z \"$1\" ]; then",
    "    echo \"Uso: test-rnw-full archivo.Rnw\"",
    "    return 1",
    "  fi",
    "  cd \"$(dirname \"$0\")\" && Rscript 06-test_rnw_completo.R \"$1\"",
    "}",
    "",
    "echo \"Alias cargados: test-rnw, test-rnw-full\""
  )
  
  writeLines(alias_script, "testing_aliases.sh")
  cat("  ✅ Archivo de alias creado: testing_aliases.sh\n")
  cat("  💡 Para usar: source testing_aliases.sh\n")
}

# =============================================================================
# FUNCIÓN: generar_reporte_configuracion
# =============================================================================
generar_reporte_configuracion <- function(resultados) {
  cat("\n📊 RESUMEN DE CONFIGURACIÓN\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  total_checks <- length(resultados)
  checks_ok <- sum(unlist(resultados))
  porcentaje <- round((checks_ok / total_checks) * 100, 1)
  
  cat("✅ Verificaciones exitosas:", checks_ok, "/", total_checks, "\n")
  cat("📈 Porcentaje de configuración:", porcentaje, "%\n")
  
  if (porcentaje >= 80) {
    cat("🎯 Estado: ✅ SISTEMA LISTO PARA USAR\n")
    cat("\n🚀 PRÓXIMOS PASOS:\n")
    cat("1. Ejecutar: Rscript 05-test_rnw_simple.R archivo.Rnw\n")
    cat("2. Para testing completo: Rscript 06-test_rnw_completo.R archivo.Rnw\n")
    cat("3. Leer documentación: 00-README.md\n")
  } else if (porcentaje >= 60) {
    cat("🎯 Estado: ⚠️ CONFIGURACIÓN PARCIAL\n")
    cat("\n⚠️ ACCIONES REQUERIDAS:\n")
    if (!resultados$paquetes_r) cat("- Instalar paquetes R faltantes\n")
    if (!resultados$python) cat("- Configurar Python y librerías\n")
    if (!resultados$latex) cat("- Instalar LaTeX (opcional para PDF)\n")
    if (!resultados$pandoc) cat("- Instalar Pandoc (opcional)\n")
  } else {
    cat("🎯 Estado: ❌ CONFIGURACIÓN INCOMPLETA\n")
    cat("\n❌ PROBLEMAS CRÍTICOS DETECTADOS\n")
    cat("Revisar las verificaciones anteriores y resolver los problemas.\n")
  }
  
  # Crear archivo de configuración
  config_info <- list(
    timestamp = Sys.time(),
    resultados = resultados,
    porcentaje_configuracion = porcentaje,
    sistema_operativo = Sys.info()["sysname"],
    version_r = R.version.string
  )
  
  saveRDS(config_info, "configuracion_testing.rds")
  cat("\n💾 Información de configuración guardada en: configuracion_testing.rds\n")
}

# =============================================================================
# EJECUCIÓN PRINCIPAL
# =============================================================================

# Ejecutar verificaciones
resultados <- list(
  paquetes_r = verificar_paquetes_r(),
  python = verificar_python(),
  latex = verificar_latex(),
  pandoc = verificar_pandoc(),
  sistema = probar_sistema()
)

# Crear alias útiles
crear_alias_utiles()

# Generar reporte final
generar_reporte_configuracion(resultados)

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🎉 CONFIGURACIÓN COMPLETADA\n")
cat("📖 Consultar 00-README.md para más información\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Retornar código de salida apropiado
if (!interactive()) {
  total_checks <- length(resultados)
  checks_ok <- sum(unlist(resultados))
  porcentaje <- (checks_ok / total_checks) * 100
  
  if (porcentaje >= 80) {
    quit(status = 0)  # Configuración exitosa
  } else {
    quit(status = 1)  # Configuración incompleta
  }
}
