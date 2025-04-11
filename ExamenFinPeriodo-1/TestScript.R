library(exams)

# Definición del archivo de examen y configuración inicial
archivo_examen <- "schoice-DVenn_All_GenMus_02.Rmd"
copias <- 1
numpreg <- 3
semilla <- 12345
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

# Generación de n copias en un solo archivo de salida para PDF
exams2pdf(rep(archivo_examen, numpreg),
          n = copias,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios)
