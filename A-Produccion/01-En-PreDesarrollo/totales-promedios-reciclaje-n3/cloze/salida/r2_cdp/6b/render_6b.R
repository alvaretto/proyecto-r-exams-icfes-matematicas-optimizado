## render_6b.R — Paso 6b (auditoría visual masiva) del CLOZE. Correr desde cloze/salida/6b:
##   cd cloze/salida/6b && Rscript --no-init-file render_6b.R
## exams2html nombra los archivos v1.html ... v24.html (SIN ceros): el mapa a semillas va en orden NUMÉRICO.
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida/6b" = basename(getwd()) == "6b")
F <- normalizePath("../../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd", mustWork = TRUE)
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
SEEDS <- c(7L, 3L, 2L, 1L, 4L, 5L, 6L, 8:24)
stopifnot(length(SEEDS) == 24, !anyDuplicated(SEEDS))
unlink("html", recursive = TRUE); dir.create("html", showWarnings = FALSE)
t0 <- Sys.time()
res <- tryCatch({
  exams2html(F, n = length(SEEDS), dir = "html", name = "v", seed = SEEDS, mathjax = TRUE)
  "OK"
}, error = function(e) paste("FALLO:", conditionMessage(e)))
cat("exams2html(n = 24, seed = <vector>, mathjax = TRUE):", res, "\n")
cat("tiempo (s):", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "\n")
fs  <- list.files("html", pattern = "^v[0-9]+\\.html$")
idx <- as.integer(sub("^v([0-9]+)\\.html$", "\\1", fs)); o <- order(idx); fs <- fs[o]; idx <- idx[o]
cat("archivos html:", length(fs), "\n")
writeLines(sprintf("%s\tsemilla %d", fs, SEEDS[idx]), "mapa_semillas.txt")
