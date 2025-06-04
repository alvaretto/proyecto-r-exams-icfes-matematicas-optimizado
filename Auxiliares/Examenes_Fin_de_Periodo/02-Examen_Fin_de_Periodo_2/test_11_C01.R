library(exams)

# Test with just the problematic file
try({
  result <- exams2pandoc("11_C01_G09_2020_Tipo1.Rmd",
                         n = 1,
                         name = "test_11_C01",
                         encoding = "UTF-8",
                         template = "pcielo.tex",
                         quiet = FALSE,
                         verbose = TRUE,
                         dir = "test_output",
                         edir = ".",
                         type = "docx")
  cat("Success!\n")
}, silent = FALSE)
