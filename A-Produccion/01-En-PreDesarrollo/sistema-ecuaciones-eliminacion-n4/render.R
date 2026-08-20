f <- "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_schoice_v1.Rmd"
set.seed(20260819)
ok <- function(lbl, expr) {
  r <- tryCatch({ expr; "OK" }, error=function(e) paste("FALLO:", conditionMessage(e)))
  cat(sprintf("%-10s %s\n", lbl, r))
}
ok("html",  exams::exams2html(f, n=3, dir="salida/html"))
ok("pdf",   exams::exams2pdf(f, n=3, dir="salida/pdf"))
ok("docx",  exams::exams2pandoc(f, n=2, type="docx", dir="salida/docx"))
ok("nops",  exams::exams2nops(f, n=2, dir="salida/nops", language="es",
                              institution="IE Pedacito de Cielo"))
ok("moodle",exams::exams2moodle(f, n=2, dir="salida/moodle"))
