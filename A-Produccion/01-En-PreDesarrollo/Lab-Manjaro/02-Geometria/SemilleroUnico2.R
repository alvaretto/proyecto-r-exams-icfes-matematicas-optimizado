# Carga de la librería r-exams
library(exams)

# Definición del archivo de examen y configuración inicial
archivo_examen <- "Geometria_vuelo.Rmd"
copias <- 5  # Generar 5 copias para verificar la aleatorización
numpreg <- 1  # Solo usar 1 pregunta por copia para facilitar la revisión
semilla <- sample(100:1e8, 1)
cat("Usando semilla:", semilla, "\n")  # Mostrar la semilla utilizada
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
# Saltamos la creación de HTML por ahora debido a problemas con las gráficas
# exams2html(rep(archivo_examen, numpreg),
#            n = 1,
#            name = nombre_arch,
#            solution = TRUE,
#            mathjax = TRUE,
#            resolution = 100,
#            width = 600,
#            height = 400,
#            encoding = "UTF-8",
#            dir = dir_salida)

################################################################################
# Generación de n copias en un solo archivo de salida para PDF

exams2pdf(rep(archivo_examen, numpreg),
          n = copias,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios)

################################################################################
# Generación de n copias en un solo archivo .docx

exams2pandoc(rep(archivo_examen, numpreg),
             n = copias,
             name = nombre_arch,
             encoding = "UTF-8",
             template = "pcielo.tex",
             header = list(Date = Sys.Date()),
             quiet = TRUE,
             resolution = 100,
             width = 4,
             height = 4,
             svg = TRUE,
             dir = dir_salida,
             edir = dir_ejercicios,
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
#                                         rule = "none")))

################################################################################
