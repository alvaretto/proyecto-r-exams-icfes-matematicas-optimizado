library(exams)
library(knitr)  # Explicitly load knitr

# Definición del archivo de examen y configuración inicial
archivo_examen <- "DVenn_All_GenMus_01.Rmd"
archivos <- 1
numpreg <- 10
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

# Configurar el motor LaTeX y el entorno
# Cambiamos de xelatex a pdflatex que tiene mejor compatibilidad
options(tinytex.latex_engine = "pdflatex")
Sys.setenv(LANG = "es_ES.UTF-8")

# Configurar opciones de knitr
opts_knit$set(concordance = TRUE)

################################################################################
# Generación de n archivos en un solo archivo de salida para PDF

exams2pdf(rep(archivo_examen, numpreg),
          n = archivos,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "plain",
          dir = dir_salida,
          edir = dir_ejercicios,
          engine = "knitr")  # Explicitly set to knitr

################################################################################
# Generación de n archivos en un solo archivo .docx

exams2pandoc(rep(archivo_examen, numpreg),
             n = archivos,
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
             type = "docx",
             engine = "knitr")  # Corrected from "xelatex" to "knitr"

################################################################################
# Creación del examen en formato HTML, sólo 'numpreg', 'archivos' = 1

exams2html(rep(archivo_examen, numpreg),
           svg = FALSE)

################################################################################
