suppressMessages(library(exams))
O <- commandArgs(TRUE)[1]
nuevo <- commandArgs(TRUE)[2]; ref <- commandArgs(TRUE)[3]
for (v in c("nuevo","ref")) {
  rmd <- if (v=="nuevo") nuevo else ref
  work <- file.path(O, paste0("src_", v)); dir.create(work, FALSE)
  file.copy(rmd, file.path(work, "ej.Rmd"), overwrite=TRUE)
  dir.create(file.path(O, "html", v), recursive=TRUE, FALSE)
  for (s in 1:100) {
    d <- file.path(O, "html", v, sprintf("s%03d", s)); if (file.exists(file.path(d,"ej1.html"))) next
    dir.create(d, FALSE)
    r <- try(suppressWarnings(suppressMessages(exams2html(file.path(work,"ej.Rmd"), n=1, seed=s, solution=TRUE, name="ej", dir=d, mathjax=FALSE, template="plain", base64=TRUE, quiet=TRUE))), silent=TRUE)
    if (inherits(r,"try-error")) cat("FAIL", v, s, "\n")
  }
}
