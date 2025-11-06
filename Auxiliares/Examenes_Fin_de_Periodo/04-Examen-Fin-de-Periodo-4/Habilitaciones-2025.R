# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

#preg01 <- sample(c("03-ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd"))

# Definición del archivo de examen y configuración inicial
preg01 <- "cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd"
preg03 <- "ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd"
preg04 <- "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd"

# 20 preguntas
archivo_examen <- sample(c(
  preg01, preg01, preg01, preg01, preg01, preg01,          # 6 repeticiones de pregunta 1
  preg03, preg03, preg03, preg03, preg03, preg03, preg03,  # 7 repeticiones de pregunta 2
  preg04, preg04, preg04, preg04, preg04, preg04, preg04   # 7 repeticiones de pregunta 3
))

copias <- 1
numpreg_por_archivo <- 1

# SOLUCIÓN CRÍTICA: Establecer semilla global ÚNICA para todas las compilaciones
# Esto garantiza que exams2pandoc y exams2pdf generen exactamente los mismos datos
semilla <- 101  # Semilla fija para reproducibilidad entre versiones
set.seed(semilla)

dir_salida <- "salida"
dir_ejercicios <- "."

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- "Matematicas_Evaluacion_Fin_de_Periodo_4"
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación de n copias en un solo archivo .docx

# Restablecer semilla antes de cada generación para consistencia
set.seed(semilla)
exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = "Matematicas_Habiliatciones-2025-docx",
             encoding = "UTF-8",
             template = "pcielo.tex",
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = FALSE,
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
# Generación de n copias, sin Solution, en un solo archivo .docx

# Restablecer semilla antes de cada generación para consistencia
set.seed(semilla)
exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = "Matematicas_Habiliatciones-2025-docx_sin_sol",
             encoding = "UTF-8",
             template = "pcielo_nosol.tex",
             solution = FALSE,  # Desactivar completamente las soluciones
             header = list(Date = Sys.Date()),
             inputs = NULL,
             options = NULL,
             quiet = FALSE, # Consider removing or setting to FALSE if verbose is TRUE
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
# Generación de n copias en un solo archivo de salida para PDF (versión con soluciones)

# Restablecer semilla antes de cada generación para consistencia
# set.seed(semilla)
# exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
#           n = copias,
#           name = "Matematicas_Habiliatciones-2025-docx_sol",
#           encoding = "UTF-8",
#           template = "solpcielo",
#           dir = dir_salida,
#           edir = dir_ejercicios,
#           verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión de examen)

# Restablecer semilla antes de cada generación para consistencia
# set.seed(semilla)
# exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
#           n = copias,
#           name = "Matematicas_Habiliatciones-2025-docx_sin_sol",  # Corregido: nombre como string
#           encoding = "UTF-8",
#           template = "exam",
#           dir = dir_salida,
#           edir = dir_ejercicios,
#           verbose = TRUE)
