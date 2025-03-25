# Carga de la librería r-exams
library(exams)

# Definición del archivo de examen y configuración inicial
archivo_examen <- c("DVenn_All_GenMus_03.Rmd")
copias <- 1
numpreg_por_archivo <- 15  # n preguntas por cada archivo
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- "05-Diagramas-de-Venn-sol"  # Cambiado a un nombre genérico para el taller
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión con soluciones)

set.seed(semilla)
exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),  # n preguntas de cada archivo
          n = copias,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios)

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión de examen)

set.seed(semilla)
exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),  # n preguntas de cada archivo
          n = copias,
          name = "05-Diagramas-de-Venn-espitia",  # Corregido: nombre como string
          encoding = "UTF-8",
          template = "exam",
          dir = dir_salida,
          edir = dir_ejercicios)
