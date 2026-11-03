# 100 versiones (semillas 1..100, una por semilla) del .Rmd del ciclo 2026-11-02 (copia de salida/r4b/generar_moodle_100.R con el md5 nuevo), en XML de Moodle, para medir_qtext_moodle.py.
# Uso: Rscript generar_moodle_100.R <ruta del .Rmd> <dir de salida>
suppressMessages(library(exams))
args <- commandArgs(TRUE)
rmd <- args[1]
out <- args[2]
stopifnot(unname(tools::md5sum(rmd)) == "4cffbe8b1924b749c36d2b92369e4a83")
dir.create(out, showWarnings = FALSE)
for (s in 1:100) {
  set.seed(s)
  invisible(capture.output(exams::exams2moodle(rmd, n = 1, name = sprintf("s%03d", s), dir = out, verbose = FALSE)))
}
cat("archivos:", length(list.files(out, "\\.xml$")), "\n")
