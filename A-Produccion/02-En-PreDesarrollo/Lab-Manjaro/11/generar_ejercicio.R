library(exams)

# Ruta al archivo Rmd
archivo_rmd <- "vaso-cilindrico.Rmd"

# Generar versión HTML
exams2html(archivo_rmd, 
           n = 1,
           name = "vaso-cilindrico",
           dir = ".",
           edir = ".",
           verbose = TRUE)

# Generar versión PDF (usando pdflatex)
exams2pdf(archivo_rmd, 
          n = 1,
          name = "vaso-cilindrico",
          dir = ".",
          edir = ".",
          verbose = TRUE)

# Generar versión DOCX
exams2pandoc(archivo_rmd, 
             n = 1,
             name = "vaso-cilindrico",
             dir = ".",
             edir = ".",
             type = "docx",
             verbose = TRUE)

# Generar versión NOPS (para exámenes escaneables)
exams2nops(archivo_rmd,
           n = 1,
           name = "vaso-cilindrico",
           dir = ".",
           edir = ".",
           verbose = TRUE)
