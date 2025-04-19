#!/usr/bin/env Rscript

# Script para probar la aleatorización del archivo geometria_parabrisas_plantillas_v1.Rmd

# Cargar librerías necesarias
library(exams)

# Generar 5 versiones HTML
exams2html("geometria_parabrisas_plantillas_v1.Rmd",
           n = 5,
           name = "test_aleatorización",
           dir = ".",
           mathjax = TRUE,
           solution = TRUE)

cat("\nPrueba completada. Se han generado 5 versiones aleatorias del ejercicio.\n")
