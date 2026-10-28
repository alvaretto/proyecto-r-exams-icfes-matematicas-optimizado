## render_cloze.R — renderiza el CLOZE en los cuatro formatos aplicables (HTML, PDF, DOCX, Moodle) y
## comprueba que NOPS lo rechace por ser cloze (N/A esperado). Correr SIEMPRE desde cloze/salida:
##   cd cloze/salida && Rscript --no-init-file ../render_cloze.R
## (el data_generation escribe la tabla PNG en el directorio de trabajo de knitr).
## Moodle: exportador propio con los gaps schoice como MULTICHOICE_V (botones en vertical, legibles a
## 360 px); por defecto R/exams los exporta como menú desplegable (MULTICHOICE). F-6, aprobado por el
## coordinador como propuesta técnica.
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
F <- normalizePath("../probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd")
lin <- readLines(F, encoding = "UTF-8"); a <- grep("^```\\{r data_generation", lin)[1]
b <- which(lin == "```" & seq_along(lin) > a)[1]
cod <- parse(text = paste(lin[(a + 1):(b - 1)], collapse = "\n"))
es_can <- function(s) { set.seed(s); e <- new.env(); eval(cod, envir = e); isTRUE(e$es_canonica) }
s_can <- which(vapply(1:200, es_can, NA))[1]
SEM <- c(s_can, 2L, 3L)
cat("Semillas:", SEM, "(la primera es canónica)\n")
r <- function(lbl, ex) {
  out <- tryCatch({ ex(); "OK" }, error = function(e) paste("FALLO:", conditionMessage(e)))
  cat(sprintf("%-8s %s\n", lbl, out)); invisible(out)
}
r("HTML",   function() exams2html(F, n = 3, dir = "html", name = "cloze", seed = SEM, mathjax = TRUE))
r("PDF",    function() exams2pdf(F, n = 3, dir = "pdf", name = "cloze", seed = SEM, texdir = "tex"))
r("DOCX",   function() exams2pandoc(F, n = 1, type = "docx", dir = "docx", name = "cloze", seed = SEM[1]))
r("MOODLE", function() { set.seed(SEM[1])    # exams2moodle no recibe seed: se fija el RNG global
  exams2moodle(F, n = 5, dir = "moodle", name = "cloze", cloze = list(cloze_schoice_display = "MULTICHOICE_V")) })
nops <- tryCatch({ exams2nops(F, n = 1, dir = "nops"); "renderizó (inesperado)" },
                 error = function(e) conditionMessage(e))
cat("NOPS    ", if (grepl("cloze exercises", nops)) "N/A (exams2nops rechaza extype cloze por diseño)" else
  paste("ERROR REAL:", nops), "\n")
