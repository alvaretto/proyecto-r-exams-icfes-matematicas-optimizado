## volcar_semillas.R — Paso 8, r10, .Rmd md5 e16842c1…: vuelca lo que R/exams emite por semilla. Directorio de trabajo FUERA del repo;
## escribe solo en el directorio de salida (arg 1), que debe ser la carpeta coherencias_r10 o un subdirectorio.
## Uso: Rscript --no-init-file volcar_semillas.R <dir_salida> semillas...
suppressMessages(library(exams))
args0 <- commandArgs(FALSE); dir_s <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", args0, value = TRUE))))
F <- normalizePath(file.path(dir_s, "..", "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
a <- commandArgs(trailingOnly = TRUE); dsal <- a[1]; sem <- as.integer(a[-1])
ex <- xexams(F, n = length(sem), seed = sem, driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"), read = NULL, transform = NULL, write = NULL))
dir.create(dsal, showWarnings = FALSE)
for (i in seq_along(sem)) { e <- ex[[i]][[1]]; out <- file.path(dsal, sprintf("volcado_semilla_%d.txt", sem[i])); sink(out)
  cat("SEMILLA", sem[i], "\n\n== PREGUNTA ==\n"); cat(e$question, sep = "\n"); cat("\n== OPCIONES ==\n")
  for (j in seq_along(e$questionlist)) cat(sprintf("[%02d] %s\n", j, e$questionlist[j]))
  cat("\n== exsolution ==\n"); str(e$metainfo$solution); cat("\n== SOLUTION ==\n"); cat(e$solution, sep = "\n"); cat("\n== RETRO ==\n")
  for (j in seq_along(e$solutionlist)) cat(sprintf("[%02d] %s\n", j, e$solutionlist[j])); sink() }
