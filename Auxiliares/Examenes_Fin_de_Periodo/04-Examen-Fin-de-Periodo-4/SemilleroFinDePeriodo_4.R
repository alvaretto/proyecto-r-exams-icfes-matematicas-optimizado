# Carga de la librería r-exams
library(exams)

# Configurar modo generación de exámenes para evitar pruebas test_that
.exams_generation_mode <- TRUE

#preg01 <- sample(c("03-ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd"))

# Definición del archivo de examen y configuración inicial
preg01 <- "cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd"
preg03 <- "ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd"
preg04 <- "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd"

# SOLUCIÓN CRÍTICA: Establecer semilla global ÚNICA para todas las compilaciones
# Esto garantiza que exams2pandoc y exams2pdf generen exactamente los mismos datos
# Y que diferentes semillas generen diferentes órdenes de preguntas
semilla <- sample(100:1e8, 1) # Cambia este número para generar diferentes versiones del examen
set.seed(semilla)

# Duplicar cada pregunta múltiples veces para generar 20 preguntas en total
# Orden: 5 repeticiones de preg01, 7 de preg03, 8 de preg04
# IMPORTANTE: sample() se ejecuta DESPUÉS de set.seed() para que la semilla afecte el orden
archivo_examen <- sample(c(
  rep(preg01, 5),  # repeticiones de pregunta 1
  rep(preg03, 7),  # repeticiones de pregunta 2
  rep(preg04, 8)   # repeticiones de pregunta 3
))

copias <- 1
numpreg_por_archivo <- 1

dir_salida <- "salida"
dir_ejercicios <- "."

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- "Matematicas_Evaluacion_Fin_de_Periodo_4"
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# Generación de n copias en un solo archivo .docx

# Restablecer semilla antes de cada generación para consistencia
# set.seed(semilla)
# exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
#              n = copias,
#              name = "Matematicas_Evaluacion_Fin_de_Periodo_4-docx",
#              encoding = "UTF-8",
#              template = "pcielo.tex",
#              header = list(Date = Sys.Date()),
#              inputs = NULL,
#              options = NULL,
#              quiet = FALSE,
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
# Generación de n copias, sin Solution, en un solo archivo .docx

# Restablecer semilla antes de cada generación para consistencia
# set.seed(semilla)
# exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
#              n = copias,
#              name = "Matematicas_Evaluacion_Fin_de_Periodo_4_sin_sol",
#              encoding = "UTF-8",
#              template = "pcielo_nosol.tex",
#              solution = FALSE,  # Desactivar completamente las soluciones
#              header = list(Date = Sys.Date()),
#              inputs = NULL,
#              options = NULL,
#              quiet = FALSE, # Consider removing or setting to FALSE if verbose is TRUE
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
# LIMPIEZA DE CACHÉ: Eliminar archivos temporales de compilaciones anteriores
# Esto garantiza que cada ejecución genere ejercicios frescos sin reutilizar caché

# Limpiar directorio temporal de exams si existe
if (dir.exists(file.path(dir_salida, ".exams"))) {
  unlink(file.path(dir_salida, ".exams"), recursive = TRUE)
  cat("Caché de exams eliminado\n")
}

# Limpiar archivos .rds que puedan contener ejercicios cacheados
archivos_rds <- list.files(dir_salida, pattern = "\\.rds$", full.names = TRUE)
if (length(archivos_rds) > 0) {
  file.remove(archivos_rds)
  cat("Archivos RDS eliminados:", length(archivos_rds), "\n")
}

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión con soluciones)

# IMPORTANTE: NO restablecer semilla aquí para mantener la secuencia aleatoria
# La semilla ya fue establecida en la línea 18 y debe continuar su secuencia
exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
          n = copias,
          name = "Matematicas_Evaluacion_Fin_de_Periodo_4_sol",
          encoding = "UTF-8",
          template = "solpcielo",
          header = list(ID = semilla, Date = Sys.Date()),
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)

################################################################################
# Generación de n copias en un solo archivo de salida para PDF (versión de examen)

# CRÍTICO: Restablecer semilla para que versión sin soluciones sea IDÉNTICA a versión con soluciones
# Esto garantiza que ambos PDFs tengan exactamente las mismas preguntas y datos
set.seed(semilla)
# Regenerar archivo_examen con la misma semilla para mantener consistencia
archivo_examen <- sample(c(
  rep(preg01, 5),
  rep(preg03, 7),
  rep(preg04, 8)
))

exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
          n = copias,
          name = "Matematicas_Evaluacion_Fin_de_Periodo_4_sin_sol",
          encoding = "UTF-8",
          template = "exam",
          header = list(ID = semilla, Date = Sys.Date()),
          dir = dir_salida,
          edir = dir_ejercicios,
          verbose = TRUE)
