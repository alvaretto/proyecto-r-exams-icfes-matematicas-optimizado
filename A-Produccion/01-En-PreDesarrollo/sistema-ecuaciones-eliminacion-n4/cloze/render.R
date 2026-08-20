library(exams)
f <- "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
dir.create("salida", showWarnings = FALSE)
r <- function(lbl, expr) {
  cat("== ", lbl, ": ", sep = "")
  z <- tryCatch({ expr; "OK" }, error = function(e) paste("FALLO -", conditionMessage(e)))
  cat(z, "\n")
  invisible(z)
}
set.seed(20260820)
r("HTML",   exams2html(f, n = 3, dir = "salida", edir = "."))
r("PDF",    exams2pdf(f, n = 3, dir = "salida", edir = "."))
r("DOCX",   exams2pandoc(f, n = 2, type = "docx", dir = "salida", edir = "."))
r("MOODLE", exams2moodle(f, n = 3, dir = "salida", edir = ".",
                         name = "cloze_sistema_ecuaciones"))
msg <- tryCatch({ exams2nops(f, n = 1, dir = "salida", edir = "."); "SIN RECHAZO (revisar)" },
                error = function(e) conditionMessage(e))
cat("== NOPS: ", msg, "\n")
cat("== NOPS motivo esperado ('cloze exercises'): ",
    grepl("cloze exercises", msg, fixed = TRUE), "\n")
