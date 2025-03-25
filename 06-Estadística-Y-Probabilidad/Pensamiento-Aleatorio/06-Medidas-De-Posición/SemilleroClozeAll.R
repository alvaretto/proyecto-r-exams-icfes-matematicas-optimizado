library(exams)
library(knitr)

# Definición del archivo de examen y configuración inicial
archivo_examen <- "schoice-cuartil-estatura-03-py.Rmd"
archivos <- 1
numpreg <- 5
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

# Configurar opciones de knitr
opts_knit$set(concordance = TRUE)

################################################################################
# Generación de archivos individuales para PDF, sólo 'archivos', no importa 'numpreg'

# for(i in 1:archivos) {
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
# Generación de archivos individuales para Pandoc (docx), sólo 'archivos', 
# no importa 'numpreg'

# for(i in 1:archivos) {
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

################################################################################
# Generación de n archivos en un solo archivo de salida para PDF

exams2pdf(rep(archivo_examen, numpreg),
          n = archivos,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios,
          engine = "knitr",
          envir = new.env())  # Nuevo entorno para evitar interferencias

################################################################################
# Creación del examen en formato HTML

exams2html(rep(archivo_examen, numpreg),
           n = archivos,
           name = nombre_arch,
           solution = TRUE,  # Mostrar soluciones
           mathjax = TRUE,   # Habilitar MathJax para fórmulas
           svg = TRUE,       # Usar SVG para gráficos
           dir = dir_salida,
           edir = dir_ejercicios,
           engine = "knitr",
           envir = new.env())  # Nuevo entorno para evitar interferencias

################################################################################
# Generación de n archivos en un solo archivo .docx

# exams2pandoc(rep(archivo_examen, numpreg),
#              n = archivos,
#              name = nombre_arch,
#              encoding = "UTF-8",
#              template = "pcielo.tex",
#              header = list(Date = Sys.Date()),
#              inputs = NULL,
#              options = NULL,
#              quiet = TRUE,
#              resolution = 100,
#              width = 4,
#              height = 4,
#              svg = TRUE,
#              dir = dir_salida,
#              edir = dir_ejercicios,
#              tdir = NULL,
#              sdir = NULL,
#              verbose = FALSE,
#              points = NULL,
#              exshuffle = NULL,
#              type = "docx",
#              engine = "knitr",
#              envir = new.env())  # Nuevo entorno para evitar interferencias

