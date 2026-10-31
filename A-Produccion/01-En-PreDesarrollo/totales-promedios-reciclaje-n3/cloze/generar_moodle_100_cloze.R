# 100 versiones (semillas 1..100, una por semilla) del CLOZE en XML de Moodle, para medir_qtext_moodle_cloze.py.
# Copia adaptada de ../salida/r4b/generar_moodle_100.R: gaps de opción única como MULTICHOICE_V (como en el aula) y
# md5 del .Rmd impreso en vez de fijado (el del SCHOICE estaba fijado a su versión aprobada).
# Uso (desde cloze/salida): Rscript --no-init-file ../generar_moodle_100_cloze.R <ruta del .Rmd> <dir de salida>
suppressMessages(library(exams))
args <- commandArgs(TRUE)
rmd <- args[1]
out <- args[2]
cat("md5 del .Rmd:", unname(tools::md5sum(rmd)), "\n")
dir.create(out, showWarnings = FALSE)
for (s in 1:100) {
  set.seed(s)
  invisible(capture.output(exams::exams2moodle(rmd, n = 1, name = sprintf("s%03d", s), dir = out, verbose = FALSE,
                                               cloze = list(cloze_schoice_display = "MULTICHOICE_V"))))
}
cat("archivos:", length(list.files(out, "\\.xml$")), "\n")
