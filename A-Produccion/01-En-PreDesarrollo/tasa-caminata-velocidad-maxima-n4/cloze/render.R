library(exams)
f <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
ok <- function(tag, expr) {
  r <- tryCatch({expr; "OK"}, error=function(e) paste("FALLA:", conditionMessage(e)))
  cat(sprintf("%-12s %s\n", tag, r)); invisible(r)
}
set.seed(101); ok("html",  exams2html(f, n=1, dir="salida"))
set.seed(102); ok("pdf",   exams2pdf(f, n=1, dir="salida"))
set.seed(103); ok("pdf+sol", exams2pdf(f, n=1, dir="salida", template="solution"))
set.seed(104); ok("docx",  exams2pandoc(f, n=1, dir="salida", type="docx"))
set.seed(105); ok("moodle", exams2moodle(f, n=1, dir="salida"))
set.seed(106)
r <- tryCatch({exams2nops(f, n=1, dir="salida"); "OK (INESPERADO)"},
              error=function(e) conditionMessage(e))
cat("nops        ", if (grepl("cloze exercises", r)) "N/A esperado (rechaza extype cloze)" else paste("ERROR REAL:", r), "\n")
