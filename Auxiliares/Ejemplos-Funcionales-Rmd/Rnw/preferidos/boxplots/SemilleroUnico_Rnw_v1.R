# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

# Definición del archivo de examen y configuración inicial
archivo_examen <- "boxplots.Rnw"
copias <- 1
numpreg <- 5
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "."  # Cambiado de "ejercicios" a "." (directorio actual)

# Nombre del archivo sin la extensión .Rnw
nombre_sin_extension <- sub("\\.Rnw$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación de copias individuales para PDF, sólo 'copias', no importa 'numpreg'

# for(i in 1:copias) {
#   nombre_archivo <- sprintf("%s_copia%d_", nombre_sin_extension, i)
#   exams2pdf(archivo_examen,
#             n = 1,
#             name = nombre_archivo,
#             encoding = "UTF-8",
#             template = "solpcielo",
#             dir = dir_salida,
#             edir = dir_ejercicios,
#             verbose = TRUE)
# }

################################################################################
# Generación de copias individuales para Pandoc (docx), sólo 'copias',
# no importa 'numpreg

# for(i in 1:copias) {
#   nombre_archivo <- sprintf("%s_copia%d_", nombre_sin_extension, i)
#   exams2pandoc(archivo_examen,
#                n = 1,
#                name = nombre_archivo,
#                encoding = "UTF-8",
#                template = "plain.tex",
#                dir = dir_salida,
#                edir = dir_ejercicios,
#                format = "docx",
#                verbose = TRUE)
# }

################################################################################
# Creación del examen en formato HTML, sólo 'numpreg', 'copias' = 1

exams2html(rep(archivo_examen, numpreg),
           svg = FALSE,
           verbose = TRUE,
           template = "plain",
           name = paste0(nombre_sin_extension, "_semillero"))

#################################################################################
# Generación de n copias en un solo archivo de salida para PDF

exams2pdf(rep(archivo_examen, numpreg),
          n = copias,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo .docx

exams2pandoc(rep(archivo_examen, numpreg),
             n = copias,
             name = nombre_arch,
             encoding = "UTF-8",
             template = "pcielo.tex",
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = TRUE,
             resolution = 100,
             width = 4,
             height = 4,
             svg = TRUE,
             dir = dir_salida,
             edir = dir_ejercicios,
             tdir = NULL,
             sdir = NULL,
             verbose = TRUE,
             points = NULL,
             exshuffle = NULL,
             type = "docx")

################################################################################
# Generación para Moodle, solo configura manualmente 'copias'
# no importa 'numpreg'

set.seed(semilla)
exams2moodle(archivo_examen,
             n = copias,
             svg = TRUE,
             name = nombre_arch,
             encoding = "UTF-8",
             dir = "salida",
             edir = dir_ejercicios,  # Cambiado de "ejercicios" a dir_ejercicios
             mchoice = list(shuffle = TRUE,
                            answernumbering = "ABCD",
                            eval = list(partial = TRUE,
                                       rule = "none")),
             verbose = TRUE)

################################################################################
# Generación para NOPS (exámenes escaneables)

set.seed(semilla)
exams2nops(rep(archivo_examen, numpreg),
           n = copias,
           name = paste0(nombre_sin_extension, "_nops_"),
           encoding = "UTF-8",
           dir = dir_salida,
           edir = dir_ejercicios,
           language = "es",
           title = "Evaluación de Matemáticas",
           institution = "I. E. Pedacito de Cielo",
           logo = NULL,
           date = Sys.Date(),
           replacement = FALSE,
           blank = 0,
           duplex = TRUE,
           pages = NULL,
           points = NULL,
           showpoints = FALSE,
           verbose = TRUE)

################################################################################
# Crear el directorio de salida si no existe
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida)
  cat("Directorio de salida creado:", dir_salida, "\n")
}
