#!/usr/bin/env Rscript

# Script para instalar todos los paquetes necesarios para el proyecto RepositorioMatematicasICFES_R_Exams

# Función para instalar paquetes si no están instalados
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    cat("Instalando los siguientes paquetes:\n")
    cat(paste(" -", new_packages), sep = "\n")
    
    # Instalar paquetes faltantes
    install.packages(new_packages, dependencies = TRUE, repos = "https://cloud.r-project.org")
    
    # Verificar si se instalaron correctamente
    still_missing <- new_packages[!(new_packages %in% installed.packages()[,"Package"])]
    
    if(length(still_missing) > 0) {
      cat("\nLos siguientes paquetes no se pudieron instalar:\n")
      cat(paste(" -", still_missing), sep = "\n")
      cat("\nPor favor, intenta instalarlos manualmente desde RStudio.\n")
    } else {
      cat("\nTodos los paquetes se instalaron correctamente.\n")
    }
  } else {
    cat("Todos los paquetes ya están instalados.\n")
  }
}

# Lista de paquetes necesarios
paquetes <- c(
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

  # Paquetes para procesamiento de PDF y LaTeX
  "pdftools",    # Para manipulación de PDF
  "qpdf",        # Para manipulación de PDF
  "tinytex",     # Para integración con LaTeX

  # Paquetes adicionales
  "readxl",      # Para leer archivos Excel
  "datasets",    # Para conjuntos de datos de ejemplo
  "magick",      # Para procesamiento de imágenes
  "webshot"      # Para capturas de pantalla de HTML
)

# Instalar paquetes faltantes
install_if_missing(paquetes)

# Configurar TinyTeX si no está instalado
if(!tinytex::is_tinytex()) {
  cat("Instalando TinyTeX...\n")
  tinytex::install_tinytex()
  cat("TinyTeX instalado correctamente.\n")
} else {
  cat("TinyTeX ya está instalado.\n")
}

# Configurar reticulate para Python
if("reticulate" %in% installed.packages()[,"Package"]) {
  library(reticulate)
  
  # Intentar encontrar Python
  python_path <- try(py_config()$python, silent = TRUE)
  
  if(inherits(python_path, "try-error") || is.null(python_path)) {
    cat("No se pudo encontrar una instalación de Python. Configurando...\n")
    
    # Intentar usar el Python del sistema
    system_python <- Sys.which("python")
    if(system_python != "") {
      use_python(system_python, required = TRUE)
      cat("Usando Python del sistema:", system_python, "\n")
    } else {
      cat("No se encontró Python en el sistema. Por favor, instala Python manualmente.\n")
    }
  } else {
    cat("Usando Python en:", python_path, "\n")
  }
  
  # Instalar paquetes Python necesarios
  cat("Instalando paquetes Python necesarios...\n")
  py_install(c("numpy", "matplotlib", "pandas", "scipy"), pip = TRUE)
  cat("Paquetes Python instalados correctamente.\n")
}

cat("\n=== Configuración completada ===\n")
cat("El entorno está listo para trabajar con el proyecto RepositorioMatematicasICFES_R_Exams.\n")
