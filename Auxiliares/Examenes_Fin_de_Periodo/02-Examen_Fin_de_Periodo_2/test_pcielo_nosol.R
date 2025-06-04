library(exams)

# Test del nuevo template pcielo_nosol.tex
try({
  result <- exams2pandoc("11_C01_G09_2020_Tipo1.Rmd",
                         n = 1,
                         name = "test_nosol",
                         encoding = "UTF-8",
                         template = "pcielo_nosol.tex",
                         quiet = FALSE,
                         verbose = TRUE,
                         dir = "test_output",
                         edir = ".",
                         type = "docx")
  cat("¡Éxito con pcielo_nosol.tex!\n")
}, silent = FALSE)
