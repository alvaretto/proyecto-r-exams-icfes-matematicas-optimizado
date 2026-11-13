# Semillero de serie-seno-aproximacion-n3 (SCHOICE, MAT-2026-07-017)
# Copia de A-Produccion/perifericos/SemilleroUnico_v2.R con dos cambios, para imprimir en hojas oficio (216 x 330 mm):
#   1. archivo_examen apunta al .Rmd de este ejercicio.
#   2. Al final del bloque NOPS, un paso con pdfjam centra cada hoja de respuestas (A4 de exams2nops) en oficio, sin escalar.
# El ejercicio no tiene opciones gráficas: no hacen falta twocolumn, samepage ni plantilla de una pregunta por página.
# Regla #26 (.claude/rules/material-aula.md). La guarda es salida/verificar_uso_semillero.R. No reemplazar por la copia genérica.
# Ejecutar con el directorio de trabajo en esta carpeta.
# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

# Definición del archivo de examen y configuración inicial
archivo_examen <- "serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd"
copias <- 1  # Número de versiones a generar
stopifnot("copias: máximo 100 (decisión del profesor)" = copias <= 100)
numpreg <- 10
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

################################################################################
# Creación del examen en formato HTML, sólo 'numpreg', 'copias' = 1

# exams2html(rep(archivo_examen, numpreg),
#            svg = FALSE,
#            verbose = TRUE,
#            template = "plain",
#            name = paste0(nombre_sin_extension, "_semillero"))

#################################################################################
# Generación de n copias en un solo archivo de salida para PDF

# NO establecer semilla fija - cada versión usará semilla diferente
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

# NO establecer semilla fija - cada versión usará semilla diferente
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
# seed: la versión s usa la semilla s, la misma que valida validar_stresstest.R en el paso 9; se exporta lo validado.
exams2moodle(archivo_examen,
             n = copias,
             seed = matrix(seq_len(copias), ncol = 1),
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

# NO establecer semilla fija - cada versión usará semilla diferente
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

# exams2nops() siempre genera A4. Aquí se centra cada página en una hoja oficio, sin escalar: esos
# *_oficio.pdf se imprimen a tamaño real. Con otra hoja o colocación, pasar antes los escaneos por
# Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py.
for (pdf in list.files(dir_salida, paste0("^", nombre_sin_extension, "_nops_[0-9]+\\.pdf$"), full.names = TRUE)) {
  estado <- system2("pdfjam", c("--papersize", shQuote("{216mm,330mm}"), "--noautoscale", "true", "--quiet",
                                shQuote(pdf), "-o", shQuote(sub("\\.pdf$", "_oficio.pdf", pdf))))
  stopifnot("pdfjam no generó la versión en oficio" = estado == 0L)
}

################################################################################
# Generación para exams2forms (formularios HTML interactivos)
# NOTA: Requiere instalar el paquete exams2forms (ejecutar solo una vez)
# install.packages("exams2forms")

library(exams2forms)

# Cuestionario para celulares sin internet (2026-11-09, decisión del profesor). exams2webquiz() solo carga
# MathJax desde internet, y sin red las fórmulas salen en TeX crudo. .claude/scripts/webquiz_autocontenido.R lo
# incrusta y comprueba en un navegador sin red que todo se compone (exit 0 = WEBQUIZ_OK). Conserva el botón de
# solución; genera copias × numpreg versiones y, en RStudio, abre el cuestionario al terminar.
raiz <- tryCatch(suppressWarnings(system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE, stderr = FALSE)),
                 error = function(e) character(0))
script_quiz <- file.path(raiz[1], ".claude", "scripts", "webquiz_autocontenido.R")
nombre_quiz <- paste0(nombre_sin_extension, "_interactivo")
if (length(raiz) == 1L && file.exists(script_quiz)) {
  estado_quiz <- system2(file.path(R.home("bin"), "Rscript"),
                         c("--no-init-file", shQuote(script_quiz), shQuote(archivo_examen), "--n", copias * numpreg,
                           "--dir", shQuote(dir_salida), "--nombre", shQuote(nombre_quiz),
                           "--titulo", shQuote("Evaluación Interactiva de Matemáticas ICFES")))
  if (estado_quiz != 0L) {
    warning("El cuestionario interactivo no pasó la comprobación sin internet (exit ", estado_quiz, "): no llevarlo al aula")
  } else if (interactive()) {
    html_quiz <- list.files(dir_salida, paste0("^", nombre_quiz, ".*\\.html$"), full.names = TRUE)
    if (length(html_quiz)) utils::browseURL(html_quiz[1])
  }
} else {
  warning("No se encontró .claude/scripts/webquiz_autocontenido.R: no se generó el cuestionario interactivo")
}

# # Opción 2: Generar archivos HTML en subdirectorio para embeber
# # Útil para integrar en documentos Rmd/Quarto o sitios web
# dir_forms_embed <- file.path(dir_salida, "forms_embed")
# dir.create(dir_forms_embed, recursive = TRUE, showWarnings = FALSE)
#
# exams2forms(archivo_examen,
#             n = 3,  # Generar 3 variaciones
#             dir = dir_forms_embed,
#             edir = dir_ejercicios,
#             encoding = "UTF-8",
#             title = "Ejercicio Interactivo",
#             verbose = TRUE,
#             solution = TRUE,
#             shuffle = TRUE,
#             mathjax = TRUE)
#
# # Copiar archivos CSS y JS al subdirectorio
# file.copy(system.file("webex.css", package = "exams2forms"),
#           file.path(dir_forms_embed, "webex.css"), overwrite = TRUE)
# file.copy(system.file("webex.js", package = "exams2forms"),
#           file.path(dir_forms_embed, "webex.js"), overwrite = TRUE)

################################################################################
