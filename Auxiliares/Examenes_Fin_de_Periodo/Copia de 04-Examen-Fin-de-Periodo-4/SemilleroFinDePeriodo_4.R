# Carga de la librería r-exams
library(exams)

# Definiciónn del archivo de examen y configuración inicial
preg01 <- sample(c("cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd"))

archivo_examen <- preg01

copias <- 1
numpreg_por_archivo <- 1
#semilla <- sample(100:1e8, 1)
#set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "."

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- "Matematicas_Evaluacion_Fin_de_Periodo_4"
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación de n copias en un solo archivo .docx

#set.seed(semilla)
exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = "Matematicas_Evaluacion_Fin_de_Periodo_4-docx",
             encoding = "UTF-8",
             template = "pcielo.tex",
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = FALSE,
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

################################################################################
# Generación de n copias, sin Solution, en un solo archivo .docx

#set.seed(semilla)
exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = "Matematicas_Evaluacion_Fin_de_Periodo_4_sin_sol",
             encoding = "UTF-8",
             template = "pcielo_nosol.tex",
             solution = FALSE,  # Desactivar completamente las soluciones
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = FALSE, # Consider removing or setting to FALSE if verbose is TRUE
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


################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión con soluciones)

#set.seed(semilla)
exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
          n = copias,
          name = "Matematicas_Evaluacion_Fin_de_Periodo_4_sol",
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión de examen)

#set.seed(semilla)
exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
          n = copias,
          name = "Matematicas_Evaluacion_Fin_de_Periodo_4_sin_sol",  # Corregido: nombre como string
          encoding = "UTF-8",
          template = "exam",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)
