## volcar_semillas.R — Paso 8 (coherencias_5), ronda 3, md5 42f725bf…
## Vuelca, para cada semilla, lo que R/exams ve tras ejecutar el .Rmd: pregunta (Markdown), opciones tras exshuffle,
## vectores de solución, Solution y retroalimentación. No escribe fuera de su carpeta ni toca el .Rmd.
## Uso (desde esta carpeta): TMPDIR=<tmp> Rscript --no-init-file volcar_semillas.R 7 3 2 1 4
suppressMessages(library(exams))
F <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
sem <- as.integer(commandArgs(trailingOnly = TRUE))
ex <- xexams(F, n = length(sem), seed = sem,
             driver = list(sweave = list(quiet = TRUE, pdf = FALSE, png = TRUE, encoding = "UTF-8"),
                           read = NULL, transform = NULL, write = NULL))
for (i in seq_along(sem)) {
  e <- ex[[i]][[1]]
  out <- sprintf("volcado_semilla_%d.txt", sem[i])
  sink(out)
  cat("SEMILLA", sem[i], "\n\n== PREGUNTA (Markdown) ==\n"); cat(e$question, sep = "\n")
  cat("\n== OPCIONES (questionlist, tras exshuffle) ==\n")
  for (j in seq_along(e$questionlist)) cat(sprintf("[%02d] %s\n", j, e$questionlist[j]))
  cat("\n== exsolution (metainfo$solution) ==\n"); str(e$metainfo$solution)
  cat("\n== exclozetype / extol / expoints ==\n")
  print(e$metainfo$clozetype); print(e$metainfo$tolerance); print(e$metainfo$points)
  cat("\n== SOLUTION (Markdown) ==\n"); cat(e$solution, sep = "\n")
  cat("\n== RETROALIMENTACIÓN (solutionlist) ==\n")
  for (j in seq_along(e$solutionlist)) cat(sprintf("[%02d] %s\n", j, e$solutionlist[j]))
  sink()
  saveRDS(e, sprintf("exam_semilla_%d.rds", sem[i]))
  cat("escrito", out, "\n")
}
