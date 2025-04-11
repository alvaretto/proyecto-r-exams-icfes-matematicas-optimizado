# Carga de la librería r-exams
library(exams)

# Definición del archivo de examen y configuración inicial

archivo_examen <- c(
  "2023-Matematicas-11-2-04-Op-B-V2.Rmd", "2023-Matematicas-11-2-09-Opc-A.Rmd",
  "2023-Matematicas-11-2-04-Op-C.Rmd", "2023-Matematicas-11-2-09-Opc-B.Rmd",
  "2023-Matematicas-11-2-04-Op-C-V2.Rmd", "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd",
 "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd",
 "DVenn_All_GenMus_01.Rmd", "DVenn_All_GenMus_01.Rmd",
  "2023-Matematicas-11-2-04-Op-D.Rmd",
 "2023-Matematicas-11-2-04-Op-D-V2.Rmd", "2023-Matematicas-11-2-04-Op-D-V2.Rmd")
copias <- 1
numpreg_por_archivo <- 1  # n preguntas por cada archivo
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- "ExamenFinPeriodo1-sol"  # Cambiado a un nombre genérico para el taller
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
          name = "ExamenFinPeriodo1",  # Corregido: nombre como string
          encoding = "UTF-8",
          template = "exam",
          dir = dir_salida,
          edir = dir_ejercicios)
