#!/usr/bin/env Rscript

# Script simplificado para probar la configuración de LaTeX y TikZ con R-exams
# Este script no requiere pdftools

# Cargar librerías necesarias
library(exams)

# Generar solo formatos que no requieren pdftools
cat("Generando HTML...\n")
exams2html("test_latex_tikz.Rmd",
           n = 1,
           name = "test_latex_tikz",
           dir = ".",
           mathjax = TRUE,
           solution = TRUE)

cat("Generando DOCX...\n")
exams2pandoc("test_latex_tikz.Rmd", 
             n = 1,
             name = "test_latex_tikz",
             dir = ".",
             type = "docx",
             template = NULL)

cat("\nPrueba completada. Verifica los archivos generados.\n")
