#!/usr/bin/env Rscript

# Script para configurar el entorno de trabajo para el proyecto RepositorioMatematicasICFES_R_Exams

# Función para verificar y cargar paquetes
check_and_load_packages <- function() {
  # Lista de paquetes necesarios
  packages <- c(
    # Paquetes principales
    "exams",       # Paquete principal para R-exams
    "knitr",       # Para formateo de documentos
    "rmarkdown",   # Para procesamiento de archivos Rmd
    
    # Paquetes para visualización
    "ggplot2",     # Para gráficos avanzados
    "scales",      # Para escalas en gráficos
    
    # Paquetes para manipulación de datos
    "dplyr",       # Para manipulación de datos
    "tidyr",       # Para limpieza de datos
    "reshape2",    # Para manipulación de datos
    "data.table",  # Para manipulación eficiente de datos
    
    # Paquetes para estadísticas
    "testthat",    # Para pruebas unitarias
    "digest",      # Para diversidad de versiones
    
    # Paquetes para integración con otros lenguajes
    "reticulate",  # Para integración con Python
    
    # Paquetes adicionales
    "readxl",      # Para leer archivos Excel
    "datasets"     # Para conjuntos de datos de ejemplo
  )
  
  # Verificar si los paquetes están instalados
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(missing_packages) > 0) {
    cat("Los siguientes paquetes no están instalados:\n")
    cat(paste(" -", missing_packages), sep = "\n")
    cat("\nEjecute el script 'install_packages.R' para instalarlos.\n")
    return(FALSE)
  } else {
    cat("Todos los paquetes necesarios están instalados.\n")
    
    # Cargar los paquetes
    for(pkg in packages) {
      library(pkg, character.only = TRUE)
      cat(paste("Paquete cargado:", pkg), "\n")
    }
    return(TRUE)
  }
}

# Función para configurar el directorio de trabajo
setup_working_directory <- function() {
  # Obtener la ruta del script actual
  script_path <- getwd()
  
  # Verificar si estamos en el directorio del proyecto
  if(basename(script_path) == "RepositorioMatematicasICFES_R_Exams") {
    cat("Directorio de trabajo configurado correctamente.\n")
  } else {
    # Intentar encontrar el directorio del proyecto
    if(dir.exists("RepositorioMatematicasICFES_R_Exams")) {
      setwd("RepositorioMatematicasICFES_R_Exams")
      cat("Directorio de trabajo cambiado a RepositorioMatematicasICFES_R_Exams.\n")
    } else {
      cat("ADVERTENCIA: No se pudo encontrar el directorio del proyecto.\n")
      cat("Por favor, ejecute este script desde el directorio del proyecto o su directorio padre.\n")
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Función para verificar la estructura del proyecto
check_project_structure <- function() {
  # Verificar carpetas principales
  main_folders <- c(
    "01-Numeros-Reales",
    "02-Funciones",
    "03-Razones-Trigonometricas",
    "04-Funciones-Identidades-Trigonometricas",
    "05-Geometria-Analitica",
    "06-Estadística-Y-Probabilidad",
    "Lab"
  )
  
  missing_folders <- main_folders[!dir.exists(main_folders)]
  
  if(length(missing_folders) > 0) {
    cat("ADVERTENCIA: Faltan algunas carpetas principales del proyecto:\n")
    cat(paste(" -", missing_folders), sep = "\n")
    cat("\nConsidere crear estas carpetas para mantener la estructura del proyecto.\n")
  } else {
    cat("La estructura principal del proyecto está completa.\n")
  }
  
  # Verificar archivos importantes
  important_files <- c(
    "README.md",
    "matriz_alineacion_icfes.md",
    "plantilla_metadatos_icfes.md",
    "plantilla_ejercicio_icfes.Rmd"
  )
  
  missing_files <- important_files[!file.exists(important_files)]
  
  if(length(missing_files) > 0) {
    cat("ADVERTENCIA: Faltan algunos archivos importantes del proyecto:\n")
    cat(paste(" -", missing_files), sep = "\n")
  } else {
    cat("Todos los archivos importantes del proyecto están presentes.\n")
  }
}

# Función principal
main <- function() {
  cat("=== Configuración del entorno para RepositorioMatematicasICFES_R_Exams ===\n\n")
  
  # Configurar directorio de trabajo
  if(!setup_working_directory()) {
    return(1)
  }
  
  cat("\n")
  
  # Verificar y cargar paquetes
  if(!check_and_load_packages()) {
    return(1)
  }
  
  cat("\n")
  
  # Verificar estructura del proyecto
  check_project_structure()
  
  cat("\n=== Configuración completada ===\n")
  cat("El entorno está listo para trabajar con el proyecto RepositorioMatematicasICFES_R_Exams.\n")
  cat("Para comenzar a trabajar, abra RStudio y cargue este proyecto.\n")
  
  return(0)
}

# Ejecutar la función principal
main()
