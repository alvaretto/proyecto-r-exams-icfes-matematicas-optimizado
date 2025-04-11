#!/usr/bin/env Rscript

# Script para ejecutar ejemplos de R-exams

# Cargar librerías necesarias
library(exams)
library(knitr)

# Función para ejecutar un ejemplo
run_example <- function(rmd_file, output_dir = "output", n_copies = 1) {
  # Verificar que el archivo existe
  if(!file.exists(rmd_file)) {
    stop(paste("El archivo", rmd_file, "no existe."))
  }
  
  # Crear directorio de salida si no existe
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Obtener nombre base del archivo
  base_name <- tools::file_path_sans_ext(basename(rmd_file))
  
  # Generar diferentes formatos de salida
  cat("Generando PDF...\n")
  exams2pdf(rmd_file, 
            n = n_copies,
            name = base_name,
            dir = output_dir,
            template = c("plain", "solution"),
            encoding = "UTF-8")
  
  cat("Generando HTML...\n")
  exams2html(rmd_file,
             n = n_copies,
             name = base_name,
             dir = output_dir,
             mathjax = TRUE,
             solution = TRUE)
  
  cat("Generando DOCX...\n")
  exams2pandoc(rmd_file, 
               n = n_copies,
               name = base_name,
               dir = output_dir,
               type = "docx",
               template = NULL)
  
  cat("\nEjemplos generados en el directorio", output_dir, "\n")
}

# Verificar argumentos de línea de comandos
args <- commandArgs(trailingOnly = TRUE)

if(length(args) == 0) {
  cat("Uso: Rscript run_example.R <archivo.Rmd> [directorio_salida] [num_copias]\n")
  cat("Ejemplo: Rscript run_example.R plantilla_ejercicio_icfes.Rmd output 3\n")
} else {
  rmd_file <- args[1]
  output_dir <- if(length(args) >= 2) args[2] else "output"
  n_copies <- if(length(args) >= 3) as.numeric(args[3]) else 1
  
  run_example(rmd_file, output_dir, n_copies)
}
