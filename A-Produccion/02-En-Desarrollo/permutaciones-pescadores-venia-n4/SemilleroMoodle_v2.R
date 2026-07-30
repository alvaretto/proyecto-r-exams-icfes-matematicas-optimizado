# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

# Definición del archivo de examen y configuración inicial
archivo_examen <- "permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd"
# 100 copias — TAMAÑO DEL BANCO DECIDIDO POR EL USUARIO (2026-07-30).
#
# No confundir con la regla #3 de .claude/rules/codigo-rmd.md ("NO crear
# ejercicios con < 200 versiones únicas"). Esa regla habla de la CAPACIDAD del
# ejercicio y se valida con `exams2html(archivo, n = 200)`; aquí está cumplida y
# medida: 298/300 versiones únicas del data_generation. `copias` es otra cosa —
# cuántas preguntas se exportan al banco de Moodle— y la regla no la gobierna.
#
# El code-review del 2026-07-29 marcó `copias <- 100` como defecto en el hermano
# `desplazamiento-avion-aeropuerto`, pero lo hizo leyendo la regla #3 como si
# aplicara al banco. El defecto real de aquel caso era otro: el valor se cambió
# SIN comentario, así que nadie podía saber si era una decisión o un descuido.
#
# Verificado sobre el banco de 100 exportado: 100/100 preguntas con la clave = n!,
# 4 opciones distintas, I-7 respetada (la clave nunca es la mayor) y 99/100
# preguntas completas distintas. Suficiente para un curso: un banco de 100 con
# 99 variantes cubre cualquier grupo real sin repetir.
copias <- 100
numpreg <- 1
semilla_base <- sample(100:1e8, 1)
# NO establecer semilla fija - cada versión usará semilla diferente
dir_salida <- "salida"
dir_ejercicios <- "."

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

#################################################################################
# Generación de n copias en un solo archivo de salida para PDF

# exams2pdf(rep(archivo_examen, numpreg),
#           n = copias,
#           name = nombre_arch,
#           encoding = "UTF-8",
#           template = "solpcielo",
#           dir = dir_salida,
#           edir = dir_ejercicios,
#           verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo .docx

# exams2pandoc(rep(archivo_examen, numpreg),
#              n = copias,
#              name = nombre_arch,
#              encoding = "UTF-8",
#              template = "pcielo.tex",
#              header = list(Date = Sys.Date()),
#              inputs = NULL,
#              options = NULL,
#              quiet = TRUE, # Consider removing or setting to FALSE if verbose is TRUE
#              resolution = 100,
#              width = 4,
#              height = 4,
#              svg = TRUE,
#              dir = dir_salida,
#              edir = dir_ejercicios,
#              tdir = NULL,
#              sdir = NULL,
#              verbose = TRUE, # Added verbose
#              points = NULL,
#              exshuffle = NULL,
#              type = "docx")

################################################################################
# Creación del examen en formato HTML, sólo 'numpreg', 'copias' = 1

# exams2html(rep(archivo_examen, numpreg),
#            svg = FALSE,
#            verbose = TRUE)

################################################################################
# Generación para Moodle, solo configura manualmente 'copias'
# no importa 'numpreg'

# NO usar set.seed aquí - exams2moodle manejará las semillas internamente
exams2moodle(archivo_examen,
             n = copias,
             svg = TRUE,
             name = nombre_arch,
             encoding = "UTF-8",
             dir = "salida",
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
#            language = "es",                      # Idioma español
#            title = "Evaluación de Matemáticas",  # Título del examen
#            institution = "I. E. Pedacito de Cielo", # Nombre de la institución
#            logo = NULL,                         # Sin logo (opcional)
#            date = Sys.Date(),                   # Fecha actual
#            replacement = FALSE,                 # Sin preguntas de reemplazo
#            blank = 0,                           # Sin páginas adicionales
#            duplex = TRUE,                       # Impresión a doble cara
#            pages = NULL,                        # Número de páginas automático
#            points = NULL,                       # Puntos por pregunta automático
#            showpoints = FALSE,                  # No mostrar puntos en el examen
#            verbose = TRUE)

################################################################################
