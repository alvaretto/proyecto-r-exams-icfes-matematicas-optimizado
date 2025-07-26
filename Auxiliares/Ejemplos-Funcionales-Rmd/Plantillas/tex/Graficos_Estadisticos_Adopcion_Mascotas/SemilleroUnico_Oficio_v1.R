# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

# Definición del archivo de examen y configuración inicial
archivo_examen <- "I_1796473-Opc-A2v2_optimizado.Rmd"
copias <- 1
numpreg <- 5
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"
dir_ejercicios <- "."


# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_oficio_")

#################################################################################
# FUNCIÓN AUXILIAR: Generar versiones DOCENTE y ESTUDIANTE automáticamente
generar_versiones_duales <- function(archivo_examen, numpreg, copias, nombre_arch,
                                   dir_salida, semilla, dir_ejercicios = ".") {

  cat("🚀 Iniciando generación de versiones duales...\n")
  cat("📄 Archivo:", archivo_examen, "\n")
  cat("🔢 Preguntas:", numpreg, "| Copias:", copias, "\n\n")

  # Versión DOCENTE - Con retroalimentación Y claves
  cat("🎓 Generando versión DOCENTE (con retroalimentación y claves)...\n")
  set.seed(semilla)  # Restablecer la misma semilla para consistencia
  exams2pdf(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_arch, "_CON_CLAVES_DOCENTE_"),
            encoding = "UTF-8",
            template = "oficio_solpcielo_margenes_estrechos",  # CON soluciones y claves
            dir = paste0(dir_salida, "_CON_CLAVES_DOCENTE"),
            edir = dir_ejercicios,
            verbose = FALSE)

  # Versión ESTUDIANTE - Sin retroalimentación NI claves
  cat("📚 Generando versión ESTUDIANTE (sin retroalimentación ni claves)...\n")
  set.seed(semilla)  # Restablecer la misma semilla para consistencia
  exams2pdf(rep(archivo_examen, numpreg),
            n = copias,
            name = paste0(nombre_arch, "_SIN_CLAVES_ESTUDIANTE_"),
            encoding = "UTF-8",
            template = "oficio_solpcielo_margenes_estrechos_SIN_SOLUCION",  # SIN soluciones ni claves
            dir = paste0(dir_salida, "_SIN_CLAVES_ESTUDIANTE"),
            edir = dir_ejercicios,
            verbose = FALSE)

  cat("\n✅ ¡Ambas versiones generadas exitosamente!\n")
  cat("📁 Versión DOCENTE en:", paste0(dir_salida, "_CON_CLAVES_DOCENTE"), "\n")
  cat("📁 Versión ESTUDIANTE en:", paste0(dir_salida, "_SIN_CLAVES_ESTUDIANTE"), "\n")
  cat("🎯 Listo para usar en clase!\n\n")
}

################################################################################
# Generación de copias individuales para PDF, sólo 'copias', no importa 'numpreg'

# for(i in 1:copias) {
#   nombre_archivo <- sprintf("%s_copia%d_oficio_", nombre_sin_extension, i)
#   exams2pdf(archivo_examen,
#             n = 1,
#             name = nombre_archivo,
#             encoding = "UTF-8",
#             template = "oficio_solpcielo",
#             dir = dir_salida,
#             edir = dir_ejercicios,
#             verbose = TRUE)
# }

################################################################################
# Generación de copias individuales para Pandoc (docx), sólo 'copias',
# no importa 'numpreg

# for(i in 1:copias) {
#   nombre_archivo <- sprintf("%s_copia%d_oficio_", nombre_sin_extension, i)
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
           verbose = TRUE,
           template = "plain",
           name = paste0(nombre_sin_extension, "_semillero_oficio"))

#################################################################################
# GENERACIÓN DE DOS VERSIONES: DOCENTE (con soluciones) y ESTUDIANTE (sin soluciones)

# Versión DOCENTE - Con retroalimentación Y claves
cat("🎓 Generando versión DOCENTE (con retroalimentación y claves)...\n")
set.seed(semilla)  # Restablecer la misma semilla para consistencia
exams2pdf(rep(archivo_examen, numpreg),
          n = copias,
          name = paste0(nombre_arch, "_CON_CLAVES_DOCENTE_"),
          encoding = "UTF-8",
          template = "oficio_solpcielo_margenes_estrechos",  # CON soluciones y claves
          dir = paste0(dir_salida, "_CON_CLAVES_DOCENTE"),
          edir = dir_ejercicios,
          verbose = TRUE)

# Versión ESTUDIANTE - Sin retroalimentación NI claves
cat("📚 Generando versión ESTUDIANTE (sin retroalimentación ni claves)...\n")
set.seed(semilla)  # Restablecer la misma semilla para consistencia
exams2pdf(rep(archivo_examen, numpreg),
          n = copias,
          name = paste0(nombre_arch, "_SIN_CLAVES_ESTUDIANTE_"),
          encoding = "UTF-8",
          template = "oficio_solpcielo_margenes_estrechos_SIN_SOLUCION",  # SIN soluciones ni claves
          dir = paste0(dir_salida, "_SIN_CLAVES_ESTUDIANTE"),
          edir = dir_ejercicios,
          verbose = TRUE)

cat("✅ Ambas versiones generadas exitosamente!\n")
cat("📁 Versión DOCENTE en:", paste0(dir_salida, "_CON_CLAVES_DOCENTE"), "\n")
cat("📁 Versión ESTUDIANTE en:", paste0(dir_salida, "_SIN_CLAVES_ESTUDIANTE"), "\n")

#################################################################################
# ALTERNATIVA SIMPLE: Usar la función auxiliar (descomenta para usar)
# generar_versiones_duales(archivo_examen, numpreg, copias, nombre_arch,
#                          dir_salida, semilla, dir_ejercicios)

#################################################################################
# Generación alternativa para documentos con muchas tablas (FORMATO LEGAL)

# exams2pdf(rep(archivo_examen, numpreg),
#           n = copias,
#           name = paste0(nombre_arch, "tablas_"),
#           encoding = "UTF-8",
#           template = "oficio_solpcielo_tablas",
#           dir = dir_salida,
#           edir = dir_ejercicios,
#           verbose = TRUE)

#################################################################################
# Generación para tablas con texto muy grande - MÁXIMA COMPACIDAD

# exams2pdf(rep(archivo_examen, numpreg),
#           n = copias,
#           name = paste0(nombre_arch, "micro_"),
#           encoding = "UTF-8",
#           template = "oficio_solpcielo_micro",
#           dir = dir_salida,
#           edir = dir_ejercicios,
#           verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo .docx (FORMATO LEGAL - DOS COLUMNAS)

# exams2pandoc(rep(archivo_examen, numpreg),
#              n = copias,
#              name = nombre_arch,
#              encoding = "UTF-8",
#              template = "oficio_pcielo_pandoc_optimizado.tex",
#              header = list(Date = Sys.Date()),
#              inputs = NULL,
#              options = NULL,
#              quiet = TRUE, # Consider removing or setting to FALSE if verbose is TRUE
#              resolution = 100,
#              width = 3.8,  # Optimizado para columnas estrechas
#              height = 3.2, # Optimizado para columnas estrechas
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
# Generación para Moodle, solo configura manualmente 'copias'
# no importa 'numpreg'

# set.seed(semilla)
# exams2moodle(archivo_examen,
#              n = copias,
#              svg = TRUE,
#              name = paste0(nombre_arch, "moodle_"),
#              encoding = "UTF-8",
#              dir = "salida",
#              edir = "ejercicios",
#              mchoice = list(shuffle = TRUE,
#                             answernumbering = "ABCD",
#                             eval = list(partial = TRUE,
#                                         rule = "none")),
#              verbose = TRUE)

################################################################################
# Generación para NOPS (exámenes escaneables) - FORMATO LEGAL
# NOTA: NOPS usa plantillas internas, para formato legal usar exams2pdf con nops_oficio.tex

#set.seed(semilla)
# Opción 1: NOPS estándar (no soporta formato legal directamente)
# exams2nops(rep(archivo_examen, numpreg),
#            n = copias,
#            name = paste0(nombre_sin_extension, "_nops_estandar_"),
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

# Opción 2: Simulación de NOPS con formato legal usando exams2pdf
# exams2pdf(rep(archivo_examen, numpreg),
#           n = copias,
#           name = paste0(nombre_sin_extension, "_nops_legal_"),
#           encoding = "UTF-8",
#           template = "nops_oficio",
#           dir = dir_salida,
#           edir = dir_ejercicios,
#           verbose = TRUE)

################################################################################
