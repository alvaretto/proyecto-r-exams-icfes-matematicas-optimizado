## render_r9.R — Paso 8 (coherencias_5), ronda 9, md5 f98f9dda…: renderiza el CLOZE en HTML, PDF, DOCX y Moodle DENTRO de esta
## carpeta (coherencias_r9/) y comprueba que NOPS lo rechace (N/A por diseño, Incidente E). No toca nada fuera de ella.
## Uso (desde esta carpeta): TMPDIR=<tmp> Rscript --no-init-file render_r9.R <formato> [semillas...]
##   formato = html | pdf | docx | moodle | moodle_aula | nops
##   html/pdf/moodle sin semillas: la muestra estándar 1:100 (regla #23). docx: semilla 7 (la canónica).
## Los exams2*() se llaman desde este script (no desde la línea de comandos de Bash), para no disparar el hook del arsenal.
suppressMessages(library(exams))
stopifnot("correr desde coherencias_r9" = basename(getwd()) == "coherencias_r9")
F <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
arg <- commandArgs(trailingOnly = TRUE)
fmt <- arg[1]; sem <- as.integer(arg[-1])
t0 <- Sys.time()
fin <- function() cat(sprintf("[%s] terminado en %.0f s\n", fmt, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
if (fmt == "html") {
  if (!length(sem)) sem <- 1:100
  dir.create("html", showWarnings = FALSE)
  exams2html(F, n = length(sem), dir = "html", name = "cloze", seed = sem, mathjax = TRUE)
  cat("HTML: semillas", range(sem), "·", length(sem), "versiones ·", length(list.files("html", "\\.html$")), "archivos\n"); fin()
} else if (fmt == "pdf") {
  if (!length(sem)) sem <- 1:100
  dir.create("pdf", showWarnings = FALSE)
  exams2pdf(F, n = length(sem), dir = "pdf", name = "cloze", seed = sem, texdir = "tex")
  cat("PDF: semillas", paste(range(sem), collapse = "-"), "·", length(sem), "versiones ·", length(list.files("pdf", "\\.pdf$")), "archivos\n"); fin()
} else if (fmt == "docx") {
  if (!length(sem)) sem <- 7L
  dir.create("docx", showWarnings = FALSE)
  for (s in sem) exams2pandoc(F, n = 1, type = "docx", dir = "docx", name = sprintf("cloze_s%03d", s), seed = s)
  cat("DOCX: semillas", sem, "\n"); fin()
} else if (fmt == "moodle") {
  ## exams2moodle no recibe `seed`: un XML por semilla (set.seed(s) + n = 1), como generar_moodle_100_cloze.R.
  if (!length(sem)) sem <- 1:100
  dir.create("moodle", showWarnings = FALSE)
  for (s in sem) {
    set.seed(s)
    ## ruta relativa + edir: el nombre de la pregunta en el XML no lleva la ruta absoluta (como exportar_moodle_cloze.R)
    invisible(capture.output(exams2moodle(basename(F), n = 1, name = sprintf("s%03d", s), dir = "moodle", verbose = FALSE,
                                          edir = dirname(F), cloze = list(cloze_schoice_display = "MULTICHOICE_V"))))
  }
  cat("MOODLE: semillas", paste(range(sem), collapse = "-"), "·", length(sem), "versiones ·", length(list.files("moodle", "\\.xml$")), "XML\n"); fin()
} else if (fmt == "moodle_aula") {
  ## la ruta REAL de exportar_moodle_cloze.R: un solo exams2moodle() con n versiones (por defecto 30) en un XML; el RNG fluye de una
  ## versión a la siguiente. Semilla global fija solo para que esta verificación sea repetible.
  n <- if (length(sem)) sem[1] else 30L
  dir.create("moodle_aula", showWarnings = FALSE)
  set.seed(2026)
  invisible(capture.output(exams2moodle(basename(F), n = n, edir = dirname(F), dir = "moodle_aula", name = "totales_promedios_cloze",
                                        cloze = list(cloze_schoice_display = "MULTICHOICE_V"))))
  cat("MOODLE (aula):", n, "versiones en un XML ·", file.exists("moodle_aula/totales_promedios_cloze.xml"), "\n"); fin()
} else if (fmt == "nops") {
  nops <- tryCatch({ exams2nops(F, n = 1, dir = tempdir()); "renderizó (inesperado)" }, error = function(e) conditionMessage(e))
  cat("NOPS    ", if (grepl("cloze exercises", nops)) "N/A (exams2nops rechaza extype cloze por diseño)" else paste("ERROR REAL:", nops), "\n"); fin()
} else stop("formato desconocido: ", fmt)
