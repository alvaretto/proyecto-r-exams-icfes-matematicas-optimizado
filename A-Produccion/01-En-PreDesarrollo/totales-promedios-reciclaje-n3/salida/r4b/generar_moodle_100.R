# 100 versiones (semillas 1..100, una por semilla) del .Rmd aprobado, en XML de Moodle, para medir_qtext_moodle.py.
# Uso: Rscript generar_moodle_100.R <ruta del .Rmd> <dir de salida>
suppressMessages(library(exams))
args <- commandArgs(TRUE)
rmd <- args[1]
out <- args[2]
stopifnot(unname(tools::md5sum(rmd)) == "e293d5c812b1f9790e2bb777b62de9a5")
dir.create(out, showWarnings = FALSE)
for (s in 1:100) {
  set.seed(s)
  invisible(capture.output(exams::exams2moodle(rmd, n = 1, name = sprintf("s%03d", s), dir = out, verbose = FALSE)))
}
cat("archivos:", length(list.files(out, "\\.xml$")), "\n")
