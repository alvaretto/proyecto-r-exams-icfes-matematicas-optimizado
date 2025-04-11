#!/usr/bin/env Rscript

# Script para probar la configuración de LaTeX y TikZ con R-exams

# Cargar librerías necesarias
library(exams)

# Generar diferentes formatos de salida
cat("Generando PDF...\n")
exams2pdf("test_latex_tikz.Rmd", 
          n = 1,
          name = "test_latex_tikz",
          dir = ".",
          template = c("plain", "solution"),
          encoding = "UTF-8")

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
