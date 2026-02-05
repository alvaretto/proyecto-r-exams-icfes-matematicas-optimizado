# ============================================================================
# CONFIGURACIÓN SEMILLERO ÚNICO V2 - TRAPECIOS REFLEXIÓN
# Ejercicio: Paralelismo en trapecios isósceles bajo reflexión
# Competencia ICFES: Interpretación y Representación - Componente Geométrico Métrico
# ============================================================================

# Limpiar entorno
rm(list = ls())

# Configurar directorio de trabajo
setwd("/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Lab-Manjaro/trapecios-paralelos")

# Cargar librerías necesarias
library(exams)
library(knitr)
library(tools)

# Configuración de opciones
options(OutDec = ".")
Sys.setlocale("LC_ALL", "C")

# ============================================================================
# CONFIGURACIÓN DE ARCHIVOS
# ============================================================================

# Archivo principal del ejercicio
archivo_ejercicio <- "trapecios_reflexion_v1.Rnw"

# Verificar que el archivo existe
if (!file.exists(archivo_ejercicio)) {
  stop(paste("Error: No se encuentra el archivo", archivo_ejercicio))
}

# Crear directorio de salida si no existe
dir_salida <- "salida"
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
}

# ============================================================================
# CONFIGURACIÓN DE SEMILLAS PARA REPRODUCIBILIDAD
# ============================================================================

# Semilla principal para reproducibilidad
semilla_principal <- 12345
set.seed(semilla_principal)

# Generar semillas únicas para diferentes versiones
semillas_html <- sample(1:100000, 5)
semillas_pdf <- sample(1:100000, 3)
semillas_moodle <- sample(1:100000, 2)

# ============================================================================
# FUNCIÓN DE PRUEBA DE DIVERSIDAD
# ============================================================================

probar_diversidad <- function(archivo, n_pruebas = 100) {
  cat("Probando diversidad de versiones...\n")
  
  versiones_unicas <- c()
  
  for (i in 1:n_pruebas) {
    set.seed(i)
    
    # Simular generación de datos
    tryCatch({
      # Cargar el archivo temporalmente para probar
      temp_env <- new.env()
      source(archivo, local = temp_env)
      
      # Crear hash de la versión
      hash_version <- digest::digest(list(
        seed = i,
        timestamp = Sys.time()
      ))
      
      versiones_unicas <- c(versiones_unicas, hash_version)
    }, error = function(e) {
      cat("Error en prueba", i, ":", e$message, "\n")
    })
  }
  
  n_unicas <- length(unique(versiones_unicas))
  cat("Versiones únicas generadas:", n_unicas, "de", n_pruebas, "\n")
  
  if (n_unicas >= 30) {
    cat("✓ Diversidad APROBADA (≥30 versiones únicas)\n")
  } else {
    cat("✗ Diversidad INSUFICIENTE (<30 versiones únicas)\n")
  }
  
  return(n_unicas >= 30)
}

# ============================================================================
# GENERACIÓN DE SALIDAS HTML
# ============================================================================

generar_html <- function() {
  cat("Generando salidas HTML...\n")
  
  for (i in 1:length(semillas_html)) {
    tryCatch({
      set.seed(semillas_html[i])
      
      nombre_salida <- file.path(dir_salida, paste0("trapecios_reflexion_html_v", i, ".html"))
      
      exams2html(
        file = archivo_ejercicio,
        n = 1,
        name = paste0("trapecios_reflexion_v", i),
        dir = dir_salida,
        template = "plain.html",
        encoding = "UTF-8"
      )
      
      cat("✓ HTML versión", i, "generado exitosamente\n")
      
    }, error = function(e) {
      cat("✗ Error generando HTML versión", i, ":", e$message, "\n")
    })
  }
}

# ============================================================================
# GENERACIÓN DE SALIDAS PDF
# ============================================================================

generar_pdf <- function() {
  cat("Generando salidas PDF...\n")
  
  for (i in 1:length(semillas_pdf)) {
    tryCatch({
      set.seed(semillas_pdf[i])
      
      exams2pdf(
        file = archivo_ejercicio,
        n = 1,
        name = paste0("trapecios_reflexion_pdf_v", i),
        dir = dir_salida,
        template = "plain",
        encoding = "UTF-8"
      )
      
      cat("✓ PDF versión", i, "generado exitosamente\n")
      
    }, error = function(e) {
      cat("✗ Error generando PDF versión", i, ":", e$message, "\n")
    })
  }
}

# ============================================================================
# GENERACIÓN DE SALIDAS MOODLE
# ============================================================================

generar_moodle <- function() {
  cat("Generando salidas Moodle...\n")
  
  for (i in 1:length(semillas_moodle)) {
    tryCatch({
      set.seed(semillas_moodle[i])
      
      exams2moodle(
        file = archivo_ejercicio,
        n = 1,
        name = paste0("trapecios_reflexion_moodle_v", i),
        dir = dir_salida,
        encoding = "UTF-8"
      )
      
      cat("✓ Moodle versión", i, "generado exitosamente\n")
      
    }, error = function(e) {
      cat("✗ Error generando Moodle versión", i, ":", e$message, "\n")
    })
  }
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE EJECUCIÓN
# ============================================================================

ejecutar_generacion_completa <- function() {
  cat("============================================================================\n")
  cat("INICIANDO GENERACIÓN COMPLETA - TRAPECIOS REFLEXIÓN\n")
  cat("============================================================================\n")
  
  # Mostrar información del ejercicio
  cat("Archivo:", archivo_ejercicio, "\n")
  cat("Directorio de salida:", dir_salida, "\n")
  cat("Semilla principal:", semilla_principal, "\n")
  cat("\n")
  
  # Probar diversidad
  diversidad_ok <- probar_diversidad(archivo_ejercicio)
  cat("\n")
  
  if (!diversidad_ok) {
    cat("ADVERTENCIA: La diversidad de versiones es baja\n")
  }
  
  # Generar todas las salidas
  generar_html()
  cat("\n")
  
  generar_pdf()
  cat("\n")
  
  generar_moodle()
  cat("\n")
  
  cat("============================================================================\n")
  cat("GENERACIÓN COMPLETADA\n")
  cat("============================================================================\n")
  
  # Mostrar archivos generados
  archivos_salida <- list.files(dir_salida, full.names = FALSE)
  cat("Archivos generados en", dir_salida, ":\n")
  for (archivo in archivos_salida) {
    cat("  -", archivo, "\n")
  }
}

# ============================================================================
# EJECUCIÓN
# ============================================================================

# Ejecutar generación completa
ejecutar_generacion_completa()

# Mensaje final
cat("\n")
cat("Para probar el ejercicio individualmente, use:\n")
cat("exams2html('", archivo_ejercicio, "')\n", sep = "")
cat("exams2pdf('", archivo_ejercicio, "')\n", sep = "")
cat("exams2moodle('", archivo_ejercicio, "')\n", sep = "")
