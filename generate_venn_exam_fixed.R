#!/usr/bin/env Rscript

# Script para generar exámenes en formato PDF y DOCX a partir del archivo Rmd corregido

# Cargar librerías necesarias
library(exams)

# Archivo de entrada
input_file <- "ExamenFinPeriodo-1/schoice-DVenn_All_GenMus_01_fixed.Rmd"

# Directorio de salida
output_dir <- "output"

# Crear directorio de salida si no existe
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Nombre base para los archivos de salida
base_name <- "DiagramaVenn_GenerosMusicales_Fixed"

# Generar PDF
cat("Generando PDF...\n")
tryCatch({
  exams2pdf(input_file, 
            n = 1,
            name = base_name,
            dir = output_dir,
            template = c("plain", "solution"),
            encoding = "UTF-8")
  cat("PDF generado correctamente.\n")
}, error = function(e) {
  cat("Error al generar PDF:", e$message, "\n")
})

# Generar DOCX
cat("Generando DOCX...\n")
tryCatch({
  exams2pandoc(input_file, 
               n = 1,
               name = base_name,
               dir = output_dir,
               type = "docx",
               template = NULL)
  cat("DOCX generado correctamente.\n")
}, error = function(e) {
  cat("Error al generar DOCX:", e$message, "\n")
})

cat("\nProceso completado. Verifica los archivos generados en el directorio", output_dir, "\n")
