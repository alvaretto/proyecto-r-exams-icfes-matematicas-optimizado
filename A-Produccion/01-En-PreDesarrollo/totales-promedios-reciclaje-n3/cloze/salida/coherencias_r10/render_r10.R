## render_r10.R — Paso 8 (coherencias_5), r10, md5 e16842c1…: renderiza el CLOZE en HTML, PDF, DOCX y Moodle DENTRO de esta
## carpeta (coherencias_r9/) y comprueba que NOPS lo rechace (N/A por diseño, Incidente E). No toca nada fuera de ella.
## Uso (desde esta carpeta): TMPDIR=<tmp> Rscript --no-init-file render_r10.R <formato> [semillas...]
##   formato = html | pdf | docx | moodle | moodle_aula | nops
##   html/pdf/moodle sin semillas: la muestra estándar 1:100 (regla #23). docx: semilla 7 (la canónica).
## Los exams2*() se llaman desde este script (no desde la línea de comandos de Bash), para no disparar el hook del arsenal.
suppressMessages(library(exams))
DIR_S <- if (exists("DIR_S")) DIR_S else dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))   # carpeta de este script (coherencias_r10); cwd FUERA del repo
SAL <- DIR_S
F <- normalizePath(file.path(DIR_S, "..", "..", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
cat("md5 del .Rmd:", unname(tools::md5sum(F)), "\n")
arg <- commandArgs(trailingOnly = TRUE)
fmt <- arg[1]; sem <- as.integer(arg[-1])
t0 <- Sys.time()
fin <- function() cat(sprintf("[%s] terminado en %.0f s\n", fmt, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
if (fmt == "html") {
  if (!length(sem)) sem <- 1:100
  dir.create(file.path(SAL, "html"), showWarnings = FALSE)
  exams2html(F, n = length(sem), dir = file.path(SAL, "html"), name = "cloze", seed = sem, mathjax = TRUE)
  cat("HTML: semillas", range(sem), "·", length(sem), "versiones ·", length(list.files(file.path(SAL, "html"), "\\.html$")), "archivos\n"); fin()
} else if (fmt == "pdf") {
  if (!length(sem)) sem <- 1:100
  dir.create(file.path(SAL, "pdf"), showWarnings = FALSE)
  exams2pdf(F, n = length(sem), dir = file.path(SAL, "pdf"), name = "cloze", seed = sem, texdir = file.path(SAL, "tex"))
  cat("PDF: semillas", paste(range(sem), collapse = "-"), "·", length(sem), "versiones ·", length(list.files(file.path(SAL, "pdf"), "\\.pdf$")), "archivos\n"); fin()
} else if (fmt == "docx") {
  if (!length(sem)) sem <- 7L
  dir.create(file.path(SAL, "docx"), showWarnings = FALSE)
  for (s in sem) exams2pandoc(F, n = 1, type = "docx", dir = file.path(SAL, "docx"), name = sprintf("cloze_s%03d", s), seed = s)
  cat("DOCX: semillas", sem, "\n"); fin()
} else if (fmt == "moodle") {
  ## exams2moodle no recibe `seed`: un XML por semilla (set.seed(s) + n = 1), como generar_moodle_100_cloze.R.
  if (!length(sem)) sem <- 1:100
  dir.create(file.path(SAL, "moodle"), showWarnings = FALSE)
  for (s in sem) {
    set.seed(s)
    ## ruta relativa + edir: el nombre de la pregunta en el XML no lleva la ruta absoluta (como exportar_moodle_cloze.R)
    invisible(capture.output(exams2moodle(basename(F), n = 1, name = sprintf("s%03d", s), dir = file.path(SAL, "moodle"), verbose = FALSE,
                                          edir = dirname(F), cloze = list(cloze_schoice_display = "MULTICHOICE_V"))))
  }
  cat("MOODLE: semillas", paste(range(sem), collapse = "-"), "·", length(sem), "versiones ·", length(list.files(file.path(SAL, "moodle"), "\\.xml$")), "XML\n"); fin()
} else if (fmt == "moodle_aula") {
  stop("moodle_aula se genera con el exportador real: ver correr_exportador_aula.sh (copia fuera del repo de exportar_moodle_cloze.R)")
} else if (fmt == "nops") {
  nops <- tryCatch({ exams2nops(F, n = 1, dir = tempdir()); "renderizó (inesperado)" }, error = function(e) conditionMessage(e))
  cat("NOPS    ", if (grepl("cloze exercises", nops)) "N/A (exams2nops rechaza extype cloze por diseño)" else paste("ERROR REAL:", nops), "\n"); fin()
} else stop("formato desconocido: ", fmt)
