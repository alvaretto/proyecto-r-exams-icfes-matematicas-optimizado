F <- "informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd"
r <- function(lbl, ex) {
  out <- tryCatch({ ex(); "OK" }, error = function(e) paste("FALLO:", conditionMessage(e)))
  cat(sprintf("%-8s %s\n", lbl, out))
}
r("HTML",   function() exams::exams2html(F, n = 2, dir = "out_html"))
r("PDF",    function() exams::exams2pdf(F,  n = 2, dir = "out_pdf"))
r("DOCX",   function() exams::exams2pandoc(F, n = 1, type = "docx", dir = "out_docx"))
r("MOODLE", function() exams::exams2moodle(F, n = 2, dir = "out_moodle"))
r("NOPS",   function() exams::exams2nops(F, n = 1, dir = "out_nops"))
