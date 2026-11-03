## Tanda S1b: renders pareados (semillas 1..100) de base (4cffbe8b), ref (e293d5c8), V1 y V2.
## Uso: Rscript --no-init-file render_variantes.R <dir_scratch> <formato: html|pdf|moodle> <variantes...>
suppressMessages(library(exams))
a <- commandArgs(TRUE); SCR <- a[1]; fmt <- a[2]; vs <- a[-(1:2)]
for (v in vs) {
  rmd <- file.path(SCR, v, "ej.Rmd")
  if (fmt == "html") for (s in 1:100) {
    d <- file.path(SCR, "html", v, sprintf("s%03d", s)); if (file.exists(file.path(d, "ej1.html"))) next
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    r <- try(suppressWarnings(suppressMessages(exams2html(rmd, n = 1, seed = s, solution = TRUE, name = "ej", dir = d,
              mathjax = FALSE, template = "plain", base64 = TRUE, quiet = TRUE))), silent = TRUE)
    if (inherits(r, "try-error")) cat("FAIL html", v, s, "\n")
  }
  if (fmt == "pdf") {
    d <- file.path(SCR, "pdf", v); dir.create(d, recursive = TRUE, showWarnings = FALSE)
    r <- try(exams2pdf(rep(rmd, 100), n = 1, seed = matrix(1:100, nrow = 1), name = "ej", dir = d, quiet = TRUE, template = "plain"), silent = TRUE)
    if (inherits(r, "try-error")) cat("FAIL pdf", v, conditionMessage(attr(r, "condition")), "\n") else cat("pdf ok", v, "\n")
  }
  if (fmt == "moodle") {
    d <- file.path(SCR, "moodle", v); dir.create(d, recursive = TRUE, showWarnings = FALSE)
    set.seed(1); r <- try(exams2moodle(rmd, n = 100, name = "ej", dir = d), silent = TRUE)  # misma corriente RNG en todas (cambio solo de texto)
    if (inherits(r, "try-error")) cat("FAIL moodle", v, conditionMessage(attr(r, "condition")), "\n") else cat("moodle ok", v, "\n")
  }
}
cat("FIN", fmt, "\n")
