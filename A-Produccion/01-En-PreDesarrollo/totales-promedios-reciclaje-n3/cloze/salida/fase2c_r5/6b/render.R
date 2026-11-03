suppressMessages(library(exams)); B <- path.expand("~/.cache/fase2c_r5_cloze/6b")
stopifnot(unname(tools::md5sum(file.path(B, "rmd_n", "ej.Rmd"))) == "6f6f1b1311b0d681bd974c719ecd75d5")
for (s in 1:100) { d <- file.path(B, "html_n", sprintf("s%03d", s)); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  setwd(file.path(B, "rmd_n")); set.seed(s)
  tryCatch(exams2html("ej.Rmd", n = 1, dir = d, mathjax = TRUE, name = "ej"), error = function(e) cat("ERR", s, conditionMessage(e), "\n")) }
cat("html:", length(list.files(file.path(B, "html_n"), pattern = "ej1.html$", recursive = TRUE)), "\n")
