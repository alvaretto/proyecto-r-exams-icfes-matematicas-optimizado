# Carga de la librería r-exams
library(exams)

# Definición del archivo de examen y configuración inicial
archivo_examen <- "probabilidad_condicional_tabla_contingencia_razonamiento_nivel3_v1.Rmd"
copias <- 1
numpreg <- 5
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
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
           verbose = TRUE)

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

################################################################################
# Generación para Moodle, solo configura manualmente 'copias'
# no importa 'numpreg'

# set.seed(semilla)
# exams2moodle(archivo_examen,
#              n = copias,
#              svg = TRUE,
#              name = nombre_arch,
#              encoding = "UTF-8",
#              dir = "salida",
#              edir = "ejercicios",
#              mchoice = list(shuffle = TRUE,
#                             answernumbering = "ABCD",
#                             eval = list(partial = TRUE,
#                                         rule = "none")),
#              verbose = TRUE)

################################################################################
# Generación para NOPS (exámenes escaneables)

set.seed(semilla)
exams2nops(rep(archivo_examen, numpreg),
           n = copias,
           name = paste0(nombre_sin_extension, "_nops_"),
           encoding = "UTF-8",
           dir = dir_salida,
           edir = dir_ejercicios,
           language = "es",                      # Idioma español
           title = "Evaluación de Matemáticas",  # Título del examen
           institution = "I. E. Pedacito de Cielo", # Nombre de la institución
           logo = NULL,                         # Sin logo (opcional)
           date = Sys.Date(),                   # Fecha actual
           replacement = FALSE,                 # Sin preguntas de reemplazo
           blank = 0,                           # Sin páginas adicionales
           duplex = TRUE,                       # Impresión a doble cara
           pages = NULL,                        # Número de páginas automático
           points = NULL,                       # Puntos por pregunta automático
           showpoints = FALSE,                  # No mostrar puntos en el examen
           verbose = TRUE)

################################################################################
