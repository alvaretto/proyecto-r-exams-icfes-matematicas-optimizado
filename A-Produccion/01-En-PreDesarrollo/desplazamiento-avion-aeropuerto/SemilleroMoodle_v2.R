# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

# Definición del archivo de examen y configuración inicial
archivo_examen <- "desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd"
# 300 = TAMANO DEL BANCO que se exporta a Moodle. Es el valor original del
# script y se documenta aqui porque estuvo a punto de perderse: alguien lo bajo
# a 100 sin explicacion y el code-review del 2026-07-29 tuvo que revertirlo
# (defecto #8). El problema no era el numero, sino que un valor cambiado SIN
# comentario es indistinguible de un descuido. Si lo cambias, deja escrito por que.
#
# NO confundir con la regla #3 de .claude/rules/codigo-rmd.md ("NO crear
# ejercicios con < 200 versiones unicas"): esa regla habla de la CAPACIDAD del
# ejercicio y se valida con exams2html(archivo, n = 200) — aqui esta medida y
# cumplida con holgura (xexams(n = 200) -> 200/200 unicas el 2026-07-28, sobre un
# espacio sustantivo de 1332 enunciados distintos; ver HANDOFF.md §5). Cuantas
# preguntas se exportan al banco es una decision de uso que ninguna regla fija.
# (El README del repo dice "minima de 300 versiones unicas"; esta desactualizado
# —el umbral vigente es 200+— y es probablemente el origen historico de este 300.)
#
# Ojo al leer el XML resultante: 300 copias NO son 300 preguntas distintas. El
# muestreo es CON reemplazo sobre un espacio finito, asi que por el problema del
# cumpleanos aparecen algunas colisiones; eso es esperado y no indica falta de
# variedad del generador.
copias <- 300
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
