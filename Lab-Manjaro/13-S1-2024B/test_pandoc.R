# Test script para verificar exams2pandoc con imágenes
library(exams)

# Configurar modo generación de exámenes
.exams_generation_mode <- TRUE

# Configuración
archivo_examen <- "13.Rmd"
copias <- 1
numpreg <- 1
semilla <- 12345
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "."

# Nombre del archivo
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_test_")

# Generación con configuración similar al archivo de referencia
set.seed(semilla)
exams2pandoc(rep(archivo_examen, numpreg),
             n = copias,
             name = nombre_arch,
             encoding = "UTF-8",
             template = "plain.tex",
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = FALSE,
             resolution = 100,
             width = 4,
             height = 4,
             svg = FALSE,  # Usar PNG en lugar de SVG
             dir = dir_salida,
             edir = dir_ejercicios,
             tdir = NULL,
             sdir = NULL,
             verbose = TRUE,
             points = NULL,
             exshuffle = NULL,
             type = "docx")
