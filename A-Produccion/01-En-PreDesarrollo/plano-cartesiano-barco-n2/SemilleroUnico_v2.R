# ============================================================
# SemilleroUnico_v2.R — Exportación multi-formato
# Ejercicio: coordenadas_vertices_plano_cartesiano_metacognitivo_
#            interpretacion_n2_schoice_v1
# ============================================================

library(exams)
library(exams2forms)

# Evitar que se ejecuten bloques test_that dentro del .Rmd durante generación
.exams_generation_mode <- TRUE

# --- Configuración inicial ---
archivo_examen  <- "coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd"
copias          <- 1   # Versiones a generar (relevante para Moodle y NOPS)
numpreg         <- 5   # Preguntas por examen (relevante para PDF, DOCX, HTML)
dir_salida      <- "salida"
dir_ejercicios  <- "."

nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch          <- paste0(nombre_sin_extension, "_")

# Crear directorio de salida si no existe
dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)

################################################################################
# PDF  (template "solpcielo")
################################################################################

exams2pdf(rep(archivo_examen, numpreg),
          n        = copias,
          name     = nombre_arch,
          encoding = "UTF-8",
          template = "solpcielo",
          dir      = dir_salida,
          edir     = dir_ejercicios,
          verbose  = TRUE)

################################################################################
# DOCX  (template "pcielo.tex")
################################################################################

exams2pandoc(rep(archivo_examen, numpreg),
             n          = copias,
             name       = nombre_arch,
             encoding   = "UTF-8",
             template   = "pcielo.tex",
             header     = list(Date = Sys.Date()),
             resolution = 100,
             width      = 4,
             height     = 4,
             svg        = TRUE,
             type       = "docx",
             dir        = dir_salida,
             edir       = dir_ejercicios,
             verbose    = TRUE)

################################################################################
# Moodle XML
################################################################################

exams2moodle(archivo_examen,
             n        = copias,
             svg      = TRUE,
             name     = nombre_arch,
             encoding = "UTF-8",
             dir      = dir_salida,
             edir     = dir_ejercicios,
             mchoice  = list(shuffle         = TRUE,
                             answernumbering = "ABCD",
                             eval            = list(partial = TRUE,
                                                    rule    = "none")),
             verbose  = TRUE)

################################################################################
# NOPS  (exámenes escaneables)
################################################################################

exams2nops(rep(archivo_examen, numpreg),
           n           = copias,
           name        = paste0(nombre_sin_extension, "_nops_"),
           encoding    = "UTF-8",
           dir         = dir_salida,
           edir        = dir_ejercicios,
           language    = "es",
           title       = "Evaluación de Matemáticas",
           institution = "I. E. Pedacito de Cielo",
           date        = Sys.Date(),
           duplex      = TRUE,
           verbose     = TRUE)

################################################################################
# HTML interactivo  (exams2forms / webquiz)
################################################################################

exams2webquiz(archivo_examen,
              n        = copias * numpreg,
              dir      = dir_salida,
              name     = paste0(nombre_sin_extension, "_interactivo"),
              edir     = dir_ejercicios,
              encoding = "UTF-8",
              title    = "Evaluación Interactiva de Matemáticas ICFES",
              solution = TRUE,
              shuffle  = TRUE,
              mathjax  = TRUE,
              browse   = TRUE)

################################################################################
# Alternativas comentadas
################################################################################

# PDF por copia individual (útil para personalizar nombre de cada copia):
# for (i in seq_len(copias)) {
#   exams2pdf(archivo_examen, n = 1,
#             name     = sprintf("%s_copia%d_", nombre_sin_extension, i),
#             encoding = "UTF-8", template = "solpcielo",
#             dir      = dir_salida, edir = dir_ejercicios, verbose = TRUE)
# }

# HTML plano (sin interactividad):
# exams2html(rep(archivo_examen, numpreg),
#            svg      = FALSE,
#            verbose  = TRUE,
#            template = "plain",
#            name     = paste0(nombre_sin_extension, "_semillero"))
