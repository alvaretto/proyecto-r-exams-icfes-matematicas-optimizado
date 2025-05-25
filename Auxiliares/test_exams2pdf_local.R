#!/usr/bin/env Rscript

# Cargar la biblioteca exams
library(exams)

# Compilar el archivo Rmd con exams2pdf
exams2pdf("Lab/12-S2-2025-SEDQ/crecimiento_exponencial_valor_inicial_v1.Rmd",
          n = 1,
          name = "test_exams2pdf")
