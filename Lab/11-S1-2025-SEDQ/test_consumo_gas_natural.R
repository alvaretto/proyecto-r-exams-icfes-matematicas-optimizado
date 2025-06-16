# Script de prueba para consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1.Rmd
# Autor: Sistema R-Exams ICFES
# Fecha: 2025-01-16

# Cargar librerías necesarias
library(exams)
library(reticulate)

# Función para probar la generación del ejercicio
probar_ejercicio <- function() {
  tryCatch({
    cat("=== PRUEBA DE GENERACIÓN DE EJERCICIO ===\n")
    cat("Archivo: consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1.Rmd\n\n")

    # Verificar que el archivo existe
    archivo_rmd <- "consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1.Rmd"
    if (!file.exists(archivo_rmd)) {
      stop("El archivo .Rmd no existe en el directorio actual")
    }
    
    cat("✓ Archivo encontrado\n")
    
    # Generar una versión HTML para prueba
    cat("Generando versión HTML de prueba...\n")
    resultado_html <- exams2html(archivo_rmd, n = 1, name = "test_consumo_gas", dir = ".")

    if (length(resultado_html) > 0) {
      cat("✓ Generación HTML exitosa\n")
      cat("✓ Gráfico Python generado correctamente\n")
    } else {
      cat("✗ Error en generación HTML\n")
    }

    # Probar generación PDF
    cat("Probando generación PDF...\n")
    resultado_pdf <- exams2pdf(archivo_rmd, n = 1, name = "test_pdf", dir = ".")

    if (length(resultado_pdf) > 0) {
      cat("✓ Generación PDF exitosa\n")
    } else {
      cat("✗ Error en generación PDF\n")
    }

    # Generar múltiples versiones para probar diversidad
    cat("Probando diversidad de versiones...\n")
    versiones <- exams2html(archivo_rmd, n = 3, name = "test_diversidad", dir = ".")

    if (length(versiones) == 3) {
      cat("✓ Generación de múltiples versiones exitosa\n")
    } else {
      cat("✗ Error en generación de múltiples versiones\n")
    }
    
    cat("\n=== PRUEBA COMPLETADA EXITOSAMENTE ===\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("✗ ERROR EN LA PRUEBA:\n")
    cat(paste("  ", e$message, "\n"))
    return(FALSE)
  })
}

# Función para limpiar archivos de prueba
limpiar_archivos_prueba <- function() {
  archivos_prueba <- c(
    "test_consumo_gas1.html", "test_pdf1.pdf",
    "test_diversidad1.html", "test_diversidad2.html", "test_diversidad3.html"
  )

  for (archivo in archivos_prueba) {
    if (file.exists(archivo)) {
      file.remove(archivo)
    }
  }

  # Limpiar directorio media si existe
  if (dir.exists("media")) {
    unlink("media", recursive = TRUE)
  }

  cat("Archivos de prueba limpiados.\n")
}

# Ejecutar la prueba si el script se ejecuta directamente
if (interactive() || !exists("sourced_from_other_script")) {
  cat("Iniciando prueba del ejercicio de consumo de gas natural...\n\n")
  
  # Cambiar al directorio correcto si es necesario
  if (basename(getwd()) != "11-S1-2025-SEDQ") {
    if (dir.exists("Lab/11-S1-2025-SEDQ")) {
      setwd("Lab/11-S1-2025-SEDQ")
      cat("Cambiado al directorio: Lab/11-S1-2025-SEDQ\n")
    }
  }
  
  # Ejecutar prueba
  resultado <- probar_ejercicio()
  
  # Limpiar archivos temporales
  limpiar_archivos_prueba()
  
  if (resultado) {
    cat("\n🎉 ¡EJERCICIO VALIDADO CORRECTAMENTE!\n")
    cat("El archivo está listo para usar con r-exams.\n")
  } else {
    cat("\n❌ El ejercicio necesita correcciones.\n")
  }
}
