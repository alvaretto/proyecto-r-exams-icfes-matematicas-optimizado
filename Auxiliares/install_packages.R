# Script para instalar todos los paquetes necesarios para el proyecto RepositorioMatematicasICFES_R_Exams

# Este script debe ejecutarse desde RStudio

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
paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]

if(length(paquetes_faltantes) > 0) {
  cat("Instalando los siguientes paquetes:\n")
  cat(paste(" -", paquetes_faltantes), sep = "\n")

  # Instalar paquetes faltantes
  install.packages(paquetes_faltantes, dependencies = TRUE)

  # Verificar si se instalaron correctamente
  paquetes_aun_faltantes <- paquetes_faltantes[!(paquetes_faltantes %in% installed.packages()[,"Package"])]

  if(length(paquetes_aun_faltantes) > 0) {
    cat("\nLos siguientes paquetes no se pudieron instalar:\n")
    cat(paste(" -", paquetes_aun_faltantes), sep = "\n")
    cat("\nPor favor, intenta instalarlos manualmente desde RStudio.\n")
  } else {
    cat("\nTodos los paquetes se instalaron correctamente.\n")
  }
} else {
  cat("Todos los paquetes necesarios ya están instalados.\n")
}
