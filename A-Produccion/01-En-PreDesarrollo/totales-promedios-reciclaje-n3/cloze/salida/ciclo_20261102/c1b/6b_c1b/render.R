a <- commandArgs(TRUE); v <- a[1]; s0 <- as.integer(a[2]); s1 <- as.integer(a[3])
S <- normalizePath(dirname(sub("--file=","",grep("--file=",commandArgs(FALSE),value=TRUE))))
library(exams)
for (s in s0:s1) { d <- file.path(S, paste0("html_", v), sprintf("s%03d", s)); dir.create(d, recursive=TRUE, showWarnings=FALSE)
  setwd(file.path(S, paste0("rmd_", v))); set.seed(s)
  tryCatch(exams2html("ej.Rmd", n=1, dir=d, mathjax=TRUE, name="ej"), error=function(e) cat("ERR", v, s, conditionMessage(e), "\n")) }
