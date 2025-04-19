#!/usr/bin/env Rscript

# Script para probar el archivo geometria_parabrisas_plantillas_v1.Rmd

# Cargar librerías necesarias
library(exams)

# Generar versión HTML
exams2html("geometria_parabrisas_plantillas_v1.Rmd",
           n = 1,
           name = "test_parabrisas",
           dir = ".",
           mathjax = TRUE,
           solution = TRUE)

cat("\nPrueba completada. Verifica el archivo test_parabrisas1.html generado.\n")
