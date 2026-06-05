#!/usr/bin/env Rscript
# Render 4 formatos del CLOZE rango_colesterol — dispara hook FASE 2A-2K
suppressPackageStartupMessages(library(exams))
set.seed(20260604)

rmd <- "rango_colesterol_metacognitivo_interpretacion_n3_cloze_v1.Rmd"
dir.create("salida", showWarnings = FALSE)

cat("=== HTML ===\n")
r1 <- tryCatch({ exams2html(rmd, n = 1, dir = "salida", name = "html_test"); "OK" },
               error = function(e) paste("ERROR:", conditionMessage(e)))
cat(r1, "\n")

cat("=== PDF ===\n")
r2 <- tryCatch({ exams2pdf(rmd, n = 1, dir = "salida", name = "pdf_test"); "OK" },
               error = function(e) paste("ERROR:", conditionMessage(e)))
cat(r2, "\n")

cat("=== DOCX ===\n")
r3 <- tryCatch({ exams2pandoc(rmd, n = 1, dir = "salida", name = "docx_test", type = "docx"); "OK" },
               error = function(e) paste("ERROR:", conditionMessage(e)))
cat(r3, "\n")

cat("=== NOPS ===\n")
r4 <- tryCatch({ exams2nops(rmd, n = 1, dir = "salida", name = "nops_test"); "OK" },
               error = function(e) paste("ERROR:", conditionMessage(e)))
cat(r4, "\n")

cat("\n=== RESUMEN ===\n")
cat("HTML:", r1, "| PDF:", r2, "| DOCX:", r3, "| NOPS:", r4, "\n")
