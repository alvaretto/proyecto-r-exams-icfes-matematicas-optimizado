#!/usr/bin/env Rscript
# Script para ejecutar las pruebas unitarias del ejercicio grafico_circular_bienes_v0.Rmd

# Definir la ruta completa al archivo
ruta_base <- "/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams"
archivo_rmd <- file.path(ruta_base, "Lab/01-S2-2025-SEDQ/grafico_circular_bienes_v0.Rmd")

# Verificar que el archivo existe
if (!file.exists(archivo_rmd)) {
  # Intentar buscar el archivo en el directorio actual y sus subdirectorios
  cat("No se encontró el archivo en la ruta predefinida. Buscando en otras ubicaciones...\n")

  # Intentar encontrar el archivo en el directorio actual
  if (file.exists("grafico_circular_bienes_v0.Rmd")) {
    archivo_rmd <- "grafico_circular_bienes_v0.Rmd"
    cat("Archivo encontrado en el directorio actual.\n")
  } else if (file.exists("Lab/01-S2-2025-SEDQ/grafico_circular_bienes_v0.Rmd")) {
    archivo_rmd <- "Lab/01-S2-2025-SEDQ/grafico_circular_bienes_v0.Rmd"
    cat("Archivo encontrado en Lab/01-S2-2025-SEDQ/.\n")
  } else {
    # Buscar en todo el sistema de archivos
    posibles_rutas <- list.files(pattern = "grafico_circular_bienes_v0.Rmd",
                                recursive = TRUE, full.names = TRUE)

    if (length(posibles_rutas) > 0) {
      archivo_rmd <- posibles_rutas[1]
      cat("Archivo encontrado en:", archivo_rmd, "\n")
    } else {
      stop("No se pudo encontrar el archivo grafico_circular_bienes_v0.Rmd en ninguna ubicación.")
    }
  }
}

# Establecer la variable global con la ruta del archivo
options(ruta_archivo_rmd = archivo_rmd)
cat("Usando archivo:", archivo_rmd, "\n")

# Cargar bibliotecas necesarias
if (!require("testthat")) install.packages("testthat")
if (!require("exams")) install.packages("exams")
if (!require("reticulate")) install.packages("reticulate")
if (!require("digest")) install.packages("digest")

# Ejecutar las pruebas
cat("Ejecutando pruebas unitarias para grafico_circular_bienes_v0.Rmd...\n")

# Definir la ruta completa al archivo de pruebas
archivo_pruebas <- file.path(ruta_base, "Lab/01-S2-2025-SEDQ/pruebas_unitarias_grafico_circular_bienes.R")

# Verificar que el archivo de pruebas existe
if (!file.exists(archivo_pruebas)) {
  # Intentar buscar el archivo en el directorio actual y sus subdirectorios
  cat("No se encontró el archivo de pruebas en la ruta predefinida. Buscando en otras ubicaciones...\n")

  # Intentar encontrar el archivo en el directorio actual
  if (file.exists("pruebas_unitarias_grafico_circular_bienes.R")) {
    archivo_pruebas <- "pruebas_unitarias_grafico_circular_bienes.R"
    cat("Archivo de pruebas encontrado en el directorio actual.\n")
  } else if (file.exists("Lab/01-S2-2025-SEDQ/pruebas_unitarias_grafico_circular_bienes.R")) {
    archivo_pruebas <- "Lab/01-S2-2025-SEDQ/pruebas_unitarias_grafico_circular_bienes.R"
    cat("Archivo de pruebas encontrado en Lab/01-S2-2025-SEDQ/.\n")
  } else {
    # Buscar en todo el sistema de archivos
    posibles_rutas <- list.files(pattern = "pruebas_unitarias_grafico_circular_bienes.R",
                                recursive = TRUE, full.names = TRUE)

    if (length(posibles_rutas) > 0) {
      archivo_pruebas <- posibles_rutas[1]
      cat("Archivo de pruebas encontrado en:", archivo_pruebas, "\n")
    } else {
      stop("No se pudo encontrar el archivo pruebas_unitarias_grafico_circular_bienes.R en ninguna ubicación.")
    }
  }
}

# Ejecutar el archivo de pruebas
cat("Usando archivo de pruebas:", archivo_pruebas, "\n")
source(archivo_pruebas)
