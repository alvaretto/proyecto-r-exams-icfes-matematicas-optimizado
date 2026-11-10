suppressMessages({library(exams); library(png)})
args <- commandArgs(TRUE); dir.create("lect", showWarnings = FALSE)
for (f in args) {
  d <- file.path("lect", sub("\\.png$", "", basename(f))); unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE); file.copy(f, d)
  o <- setwd(d)
  r <- tryCatch({ suppressWarnings(nops_scan(images = basename(f), file = "z", dir = ".", verbose = FALSE, rotate = FALSE)); unzip("z.zip", files = "Daten.txt"); strsplit(readLines("Daten.txt")[1], " ")[[1]] }, error = function(e) { if (file.exists("z.zip")) { unzip("z.zip", files = "Daten.txt"); strsplit(readLines("Daten.txt")[1], " ")[[1]] } else "ERROR" })
  setwd(o)
  cat(sprintf("%-28s %s\n", basename(f), if (length(r) < 9) paste(r, collapse = " ") else paste("id", r[2], "reg", r[6], "resp", paste(r[7:9], collapse = " "))))
}
