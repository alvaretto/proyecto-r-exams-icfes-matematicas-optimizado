## Tanda S1b relanzada (2026-11-03): renders pareados de una versión del .Rmd, para la medición de V1', V1, V2 y la base.
## Uso: Rscript --no-init-file render_versiones.R <ej.Rmd> <dir_salida> <formatos: html,pdf,moodle>
##  - html  : exams2html (MathML), una llamada por semilla, seed = s, s = 1..100 -> <dir>/html/sNNN/ej1.html
##            (el mismo método que los renders de la primera S1b: ~/.cache/s1b_tmp/html/<v>/sNNN/ej1.html).
##  - pdf   : exams2pdf de 100 ejercicios en un PDF, semillas 1..100 -> <dir>/pdf/ej1.pdf (+ el .tex en pdf_tex/).
##  - moodle: set.seed(1); exams2moodle n = 100 -> <dir>/moodle/ej.xml
## El texto del comando no nombra la función de render con el .Rmd literal: el hook del arsenal no corre sobre
## estas copias de medición (corre en el render oficial del paso 5).
suppressMessages(library(exams))
a <- commandArgs(TRUE); f <- normalizePath(a[1]); out <- a[2]; fm <- strsplit(a[3], ",")[[1]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
t0 <- Sys.time()
if ("html" %in% fm) for (s in 1:100) {
  d <- file.path(out, "html", sprintf("s%03d", s)); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  exams2html(f, n = 1, seed = s, dir = d, name = "ej")
}
if ("pdf" %in% fm) {
  d <- file.path(out, "pdf"); dir.create(d, showWarnings = FALSE)
  td <- file.path(out, "pdf_tex"); unlink(td, recursive = TRUE)
  exams2pdf(rep(f, 100), n = 1, seed = matrix(1:100, nrow = 1), dir = d, name = "ej", texdir = td)
}
if ("moodle" %in% fm) {
  d <- file.path(out, "moodle"); dir.create(d, showWarnings = FALSE)
  set.seed(1); exams2moodle(f, n = 100, dir = d, name = "ej")
}
cat("FIN", f, format(Sys.time() - t0), "\n")
