#!/usr/bin/env Rscript
# Script para ejecutar las pruebas unitarias del ejercicio grafico_circular_bienes_v1.Rmd

# Verificar que estamos en el directorio correcto
if (!file.exists("Lab/01-S2-2025-SEDQ/grafico_circular_bienes_v1.Rmd")) {
  # Intentar cambiar al directorio raíz del repositorio
  setwd("../../")
  
  # Verificar nuevamente
  if (!file.exists("Lab/01-S2-2025-SEDQ/grafico_circular_bienes_v1.Rmd")) {
    stop("No se pudo encontrar el archivo grafico_circular_bienes_v1.Rmd. ",
         "Asegúrese de ejecutar este script desde el directorio raíz del repositorio.")
  }
}

# Cargar bibliotecas necesarias
if (!require("testthat")) install.packages("testthat")
if (!require("exams")) install.packages("exams")
if (!require("reticulate")) install.packages("reticulate")
if (!require("digest")) install.packages("digest")

# Ejecutar las pruebas
cat("Ejecutando pruebas unitarias para grafico_circular_bienes_v1.Rmd...\n")
source("Lab/01-S2-2025-SEDQ/pruebas_unitarias_grafico_circular_bienes.R")
