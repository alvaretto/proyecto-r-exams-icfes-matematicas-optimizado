# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

# Definición del archivo de examen y configuración inicial
archivo_examen <- "confint3.Rnw"
copias <- 1
numpreg <- 2
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "."  # Directorio actual

# Nombre del archivo sin la extensión .Rnw
nombre_sin_extension <- sub("\\.Rnw$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

# Función para adaptar el archivo Rnw según el formato de salida
adaptar_archivo_para_formato <- function(archivo, formato) {
  # Leer el archivo original
  contenido <- readLines(archivo)

  # Verificar si es un archivo cloze con tipo verbatim
  es_cloze <- any(grepl("\\\\extype\\{cloze\\}", contenido))
  tiene_verbatim <- any(grepl("\\\\exclozetype\\{.*verbatim.*\\}", contenido))

  # Si es PDF y tiene verbatim, crear versión temporal adaptada
  if (formato == "pdf" && es_cloze && tiene_verbatim) {
    cat("Detectado tipo 'verbatim' no compatible con PDF. Adaptando archivo...\n")

    # Buscar líneas de solución para extraer valores
    linea_sol <- grep("\\\\exsolution\\{.*\\}", contenido, value = TRUE)

    # Modificar el tipo de pregunta para PDF
    contenido <- gsub("\\\\exclozetype\\{.*verbatim.*\\}",
                     "\\\\exclozetype{num|num}",
                     contenido)

    # Si hay una referencia a sol[1] y sol[2], reemplazarla con LBt y UBt
    if (grepl("sol\\[1\\]", linea_sol) && grepl("sol\\[2\\]", linea_sol)) {
      contenido <- gsub("\\\\exsolution\\{.*\\}",
                       "\\\\exsolution{\\\\Sexpr{LBt}|\\\\Sexpr{UBt}}",
                       contenido)
    }

    # Crear archivo temporal
    archivo_temp <- paste0(tempdir(), "/", basename(archivo), "_temp")
    writeLines(contenido, archivo_temp)
    return(archivo_temp)
  }

  # Si no necesita adaptación, devolver el archivo original
  return(archivo)
}

# Crear el directorio de salida si no existe
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida)
  cat("Directorio de salida creado:", dir_salida, "\n")
}

################################################################################
# Creación del examen en formato HTML, sólo 'numpreg', 'copias' = 1

cat("\n--- Generando versión HTML ---\n")
exams2html(rep(archivo_examen, numpreg),
           svg = FALSE,
           verbose = TRUE,
           template = "plain",
           name = paste0(nombre_sin_extension, "_semillero"))

#################################################################################
# Generación de n copias en un solo archivo de salida para PDF

cat("\n--- Generando versión PDF ---\n")
# Adaptar archivo para PDF si es necesario
archivo_pdf <- adaptar_archivo_para_formato(archivo_examen, "pdf")

exams2pdf(rep(archivo_pdf, numpreg),
          n = copias,
          name = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo",
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

# Eliminar archivo temporal si se creó uno
if (archivo_pdf != archivo_examen) {
  file.remove(archivo_pdf)
}

################################################################################
# Generación de n copias en un solo archivo .docx

cat("\n--- Generando versión DOCX ---\n")
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

cat("\n--- Generando versión Moodle XML ---\n")
set.seed(semilla)
exams2moodle(archivo_examen,
             n = copias,
             svg = TRUE,
             name = nombre_arch,
             encoding = "UTF-8",
             dir = dir_salida,
             edir = dir_ejercicios,
             mchoice = list(shuffle = TRUE,
                            answernumbering = "ABCD",
                            eval = list(partial = TRUE,
                                       rule = "none")),
             verbose = TRUE)

################################################################################
# Generación para NOPS (exámenes escaneables)

# set.seed(semilla)
# exams2nops(rep(archivo_examen, numpreg),
#            n = copias,
#            name = paste0(nombre_sin_extension, "_nops_"),
#            encoding = "UTF-8",
#            dir = dir_salida,
#            edir = dir_ejercicios,
#            language = "es",
#            title = "Evaluación de Matemáticas",
#            institution = "I. E. Pedacito de Cielo",
#            logo = NULL,
#            date = Sys.Date(),
#            replacement = FALSE,
#            blank = 0,
#            duplex = TRUE,
#            pages = NULL,
#            points = NULL,
#            showpoints = FALSE,
#            verbose = TRUE)

cat("\n¡Proceso completado! Todos los formatos han sido generados en la carpeta:", dir_salida, "\n")
