## render_pos100.R — 6b r8: 100 versiones HTML (semillas 1:100, N = 100 de la regla #23) solo para medir la posición de la clave de la Parte 2.
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida/6b_r8" = basename(getwd()) == "6b_r8")
F <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd", mustWork = TRUE)
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
unlink("html100", recursive = TRUE); dir.create("html100", showWarnings = FALSE)
res <- tryCatch({ exams2html(F, n = 100, dir = "html100", name = "v", seed = 1:100, mathjax = TRUE); "OK" }, error = function(e) paste("FALLO:", conditionMessage(e)))
cat("exams2html(n = 100, seed = 1:100):", res, "| archivos:", length(list.files("html100", pattern = "^v[0-9]+\\.html$")), "\n")
