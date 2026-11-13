## Umbral del profesor (2026-11-13): al menos 100 versiones distintas de 150 renders. Render real con exams2html(n = 150)
## y comparación del HTML de la pregunta (enunciado + opciones, sin la Solution).
library(exams)
rmd <- list.files(".", pattern = "_schoice_v1\\.Rmd$")[1]; out <- file.path(tempdir(), "u150"); unlink(out, recursive = TRUE)
x <- exams2html(rmd, n = 150, dir = out, seed = 1:150, solution = FALSE, quiet = TRUE)
q <- vapply(x, function(e) paste(c(e[[1]]$question, e[[1]]$questionlist), collapse = "|"), "")
cat(sprintf("VERSIONES_UNICAS: %d de %d renders (criterio del profesor: >= 100 de 150)\n", length(unique(q)), length(q)))
