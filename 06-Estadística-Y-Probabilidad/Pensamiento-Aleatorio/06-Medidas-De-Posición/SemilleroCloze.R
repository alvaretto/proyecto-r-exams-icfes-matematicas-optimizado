library(exams)

# Definición del archivo de examen y configuración
archivo_examen <- "schoice-cuartil-estatura-01-py.Rmd" 
archivos <- 10
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "ejercicios"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación para Moodle - configuración básica

set.seed(semilla)
exams2moodle(archivo_examen,
             n = archivos,
             svg = TRUE,
             name = nombre_arch,
             encoding = "UTF-8",
             dir = "salida",
             edir = "ejercicios")
################################################################################