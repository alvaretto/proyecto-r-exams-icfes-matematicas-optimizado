library(exams)

# Test with PDF instead of pandoc
archivo_examen <- c("11_C01_G09_2020_Tipo1.Rmd", "vaso-cilindrico-v6.Rmd")

set.seed(123)
try({
  exams2pdf(archivo_examen,
            n = 1,
            name = "test_pdf",
            encoding = "UTF-8",
            template = "exam",
            verbose = TRUE,
            dir = "test_output",
            edir = ".")
  cat("Success with PDF!\n")
}, silent = FALSE)
