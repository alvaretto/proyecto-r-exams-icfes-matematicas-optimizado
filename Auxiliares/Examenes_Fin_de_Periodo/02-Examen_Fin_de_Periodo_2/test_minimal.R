library(exams)

# Test with just two files to see where the conflict is
archivo_examen <- c("11_C01_G09_2020_Tipo1.Rmd", "vaso-cilindrico-v6.Rmd")

set.seed(123)
try({
  exams2pandoc(archivo_examen,
               n = 1,
               name = "test_minimal",
               encoding = "UTF-8",
               template = "pcielo_nosol.tex",
               quiet = FALSE,
               verbose = TRUE,
               dir = "test_output",
               edir = ".",
               type = "docx")
  cat("Success with two files!\n")
}, silent = FALSE)
