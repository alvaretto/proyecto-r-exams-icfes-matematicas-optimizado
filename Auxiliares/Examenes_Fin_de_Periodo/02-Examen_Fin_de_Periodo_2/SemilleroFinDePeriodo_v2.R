# Carga de la librería r-exams
library(exams)

bezes <- 3

# Definición del archivo de examen y configuración inicial
preg01 <- sample(c("2023-Matematicas-11-2-09-Opc-A.Rmd",
                    "2023-Matematicas-11-2-09-Opc-B.Rmd",
                    "2023-Matematicas-11-2-09-Op-C.Rmd",
                    "2023-Matematicas-11-2-09-Op-D.Rmd"), bezes)

preg02 <- sample(c("fracciones_reparto_premio_v1.Rmd",
                   "fracciones_reparto_premio_v2.Rmd",
                   "fracciones_reparto_premio_v3.Rmd",
                   "fracciones_reparto_premio_v4.Rmd"), bezes)

preg03 <- rep(c("vaso-cilindrico-v6.Rmd"), bezes)

preg04 <- rep(c("01-S2-2025-SEDQ-grafico_circular_bienes_v0.Rmd"), bezes)

preg05 <- rep(c("11_C01_G09_2020_Tipo1.Rmd"), bezes)

archivo_examen <- c(preg01, preg02, preg03, preg04, preg05)
archivo_examen <- sample(archivo_examen)

copias <- 1
numpreg_por_archivo <- 1  # 3 preguntas por cada archivo
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- "10A-Evaluacion_Fin_de_Periodo_2"  # Cambiado a un nombre genérico para el taller
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión con soluciones)

set.seed(semilla)
exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),  # 3 preguntas de cada archivo
          n = copias,
          name = "10A-Evaluacion_Fin_de_Periodo_2_sol",
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión de examen)

set.seed(semilla)
exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),  # 3 preguntas de cada archivo
          n = copias,
          name = "10A-Evaluacion_Fin_de_Periodo_2",  # Corregido: nombre como string
          encoding = "UTF-8",
          template = "exam",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo .docx

set.seed(semilla)
exams2pandoc(rep(archivo_examen, numpreg),
             n = copias,
             name = "10A-Evaluacion_Fin_de_Periodo_2-docx",
             encoding = "UTF-8",
             template = "pcielo.tex",
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = TRUE, # Consider removing or setting to FALSE if verbose is TRUE
             resolution = 100,
             width = 4,
             height = 4,
             svg = TRUE,
             dir = dir_salida,
             edir = dir_ejercicios,
             tdir = NULL,
             sdir = NULL,
             verbose = TRUE, # Added verbose
             points = NULL,
             exshuffle = NULL,
             type = "docx")

# Generación de n copias, sin Solution, en un solo archivo .docx

set.seed(semilla)
exams2pandoc(rep(archivo_examen, numpreg),
             n = copias,
             name = "10A-Evaluacion_Fin_de_Periodo_2-docx_sin_sol",
             encoding = "UTF-8",
             template = "pcielo_sin_sol.tex",
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = TRUE, # Consider removing or setting to FALSE if verbose is TRUE
             resolution = 100,
             width = 4,
             height = 4,
             svg = TRUE,
             dir = dir_salida,
             edir = dir_ejercicios,
             tdir = NULL,
             sdir = NULL,
             verbose = TRUE, # Added verbose
             points = NULL,
             exshuffle = NULL,
             type = "docx")
