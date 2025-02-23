# Carga de la librería r-exams
library(exams)

# Definición del archivo de examen y configuración inicial
archivo_examen <- "p25_SAI_CFA11S123.Rmd"
copias <- 1
numpreg <- 3
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Creación del examen en formato HTML, sólo 'numpreg', 'copias' = 1

exams2html(rep(archivo_examen, numpreg),
           svg = FALSE)

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
             edir = "ejercicios",
             mchoice = list(shuffle = TRUE,
                            answernumbering = "ABCD",
                            eval = list(partial = TRUE,
                                        rule = "none")))

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
#             edir = dir_ejercicios)
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
#                format = "docx")
# }

#################################################################################
# Generación de n copias en un solo archivo de salida para PDF

# exams2pdf(rep(archivo_examen, numpreg),
#           n = copias,
#           name = nombre_arch,
#           encoding = "UTF-8",
#           template = "solpcielo",
#           dir = dir_salida,
#           edir = dir_ejercicios)

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
             verbose = FALSE,
             points = NULL,
             exshuffle = NULL,
             type = "docx")

###############################################################################