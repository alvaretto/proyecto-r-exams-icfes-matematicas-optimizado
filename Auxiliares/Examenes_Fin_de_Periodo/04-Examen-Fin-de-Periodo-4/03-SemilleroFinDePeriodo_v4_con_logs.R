################################################################################
# SCRIPT: SemilleroFinDePeriodo_v4_con_logs.R
# DESCRIPCIÓN: Generación automática de examen de fin de período CON REGISTRO DE ERRORES
#              Selecciona aleatoriamente 15 ejercicios de todos los .Rmd disponibles
# AUTOR: Sistema ICFES R-Exams
# FECHA: 2025
################################################################################

# Carga de la librería r-exams
library(exams)

################################################################################
# CONFIGURACIÓN DE LOGS
################################################################################

# Crear archivo de log
log_file <- "log_generacion_examenes.txt"
cat("", file = log_file)  # Limpiar archivo de log

# Función para escribir en el log
log_msg <- function(msg, tipo = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- sprintf("[%s] [%s] %s\n", timestamp, tipo, msg)
  cat(log_line, file = log_file, append = TRUE)
  cat(log_line)  # También mostrar en consola
}

log_msg("Iniciando generación de exámenes", "INFO")

################################################################################
# CONFIGURACIÓN INICIAL
################################################################################

# Establecer el directorio de trabajo al directorio del script
get_script_dir <- function() {
  # Intentar varios métodos para obtener el directorio del script
  
  # Método 1: Para source() en RStudio
  if (exists("ofile") && !is.null(ofile <- sys.frame(1)$ofile)) {
    return(dirname(ofile))
  }
  
  # Método 2: Para Rscript
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg)
    return(dirname(normalizePath(script_path)))
  }
  
  # Método 3: Usar directorio actual
  return(getwd())
}

# Intentar establecer el directorio de trabajo
tryCatch({
  script_dir <- get_script_dir()
  if (!is.null(script_dir) && script_dir != "") {
    setwd(script_dir)
  }
}, error = function(e) {
  # Si falla, continuar con el directorio actual
})

log_msg(sprintf("Directorio de trabajo: %s", getwd()), "INFO")

# Número de ejercicios a seleccionar para el examen
NUM_EJERCICIOS <- 15

# Configuración de directorios
dir_salida <- "salida"
dir_ejercicios <- "."

# Configuración de copias y semilla
# Generar 1 versión del examen con 15 preguntas seleccionadas aleatoriamente
copias <- 1
numpreg_por_archivo <- 1
semilla <- sample(100:1e8, 1)
set.seed(semilla)

log_msg(sprintf("Semilla aleatoria: %d", semilla), "INFO")
log_msg(sprintf("Número de versiones a generar: %d", copias), "INFO")

# Nombre del examen
nombre_sin_extension <- "Evaluacion_Fin_de_Periodo_4"
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# DETECCIÓN AUTOMÁTICA DE ARCHIVOS .RMD DISPONIBLES
################################################################################

log_msg("Detectando archivos .Rmd disponibles", "INFO")

# Listar todos los archivos .Rmd en el directorio actual
todos_los_rmd <- list.files(path = dir_ejercicios,
                             pattern = "\\.Rmd$",
                             full.names = FALSE)

log_msg(sprintf("Total de archivos .Rmd encontrados: %d", length(todos_los_rmd)), "INFO")

# Excluir archivos que no sean ejercicios
ejercicios_disponibles <- todos_los_rmd[grepl("^[0-9]{3}-", todos_los_rmd)]

log_msg(sprintf("Total de ejercicios .Rmd disponibles (con prefijo numérico): %d", length(ejercicios_disponibles)), "INFO")

################################################################################
# VALIDACIÓN: Verificar que hay suficientes ejercicios
################################################################################

if (length(ejercicios_disponibles) < NUM_EJERCICIOS) {
  msg <- sprintf("ERROR: Se requieren al menos %d ejercicios, pero solo hay %d disponibles.",
                 NUM_EJERCICIOS, length(ejercicios_disponibles))
  log_msg(msg, "ERROR")
  stop(msg)
}

################################################################################
# SELECCIÓN ALEATORIA DE EJERCICIOS
################################################################################

log_msg(sprintf("Seleccionando aleatoriamente %d ejercicios", NUM_EJERCICIOS), "INFO")

# Seleccionar aleatoriamente NUM_EJERCICIOS archivos sin repetición
set.seed(semilla)
archivo_examen <- sample(ejercicios_disponibles, NUM_EJERCICIOS, replace = FALSE)

# Mezclar el orden de los ejercicios seleccionados
archivo_examen <- sample(archivo_examen)

log_msg("Ejercicios seleccionados:", "INFO")
for (i in 1:length(archivo_examen)) {
  log_msg(sprintf("  %2d. %s", i, archivo_examen[i]), "INFO")
}

################################################################################
# GENERACIÓN DE EXAMEN - FORMATO DOCX (CON SOLUCIONES)
################################################################################

log_msg("Generando examen en formato DOCX (con soluciones)", "INFO")

tryCatch({
  set.seed(semilla)
  exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
               n = copias,
               name = paste0(nombre_sin_extension, "-docx"),
               encoding = "UTF-8",
               template = "pcielo_pandoc.tex",
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
               verbose = TRUE,
               points = NULL,
               exshuffle = NULL,
               type = "docx")
  
  log_msg("✓ Examen DOCX (con soluciones) generado exitosamente", "SUCCESS")
  
}, error = function(e) {
  log_msg(sprintf("✗ ERROR al generar examen DOCX (con soluciones): %s", e$message), "ERROR")
  log_msg(sprintf("Traceback: %s", paste(capture.output(traceback()), collapse = "\n")), "ERROR")
})

################################################################################
# GENERACIÓN DE EXAMEN - FORMATO DOCX (SIN SOLUCIONES)
################################################################################

log_msg("Generando examen en formato DOCX (sin soluciones)", "INFO")

tryCatch({
  set.seed(semilla)
  exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
               n = copias,
               name = paste0(nombre_sin_extension, "_sin_sol"),
               encoding = "UTF-8",
               template = "pcielo_nosol.tex",
               solution = FALSE,
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
               verbose = TRUE,
               points = NULL,
               exshuffle = NULL,
               type = "docx")
  
  log_msg("✓ Examen DOCX (sin soluciones) generado exitosamente", "SUCCESS")
  
}, error = function(e) {
  log_msg(sprintf("✗ ERROR al generar examen DOCX (sin soluciones): %s", e$message), "ERROR")
  log_msg(sprintf("Traceback: %s", paste(capture.output(traceback()), collapse = "\n")), "ERROR")
})

################################################################################
# GENERACIÓN DE EXAMEN - FORMATO PDF (CON SOLUCIONES)
################################################################################

log_msg("Generando examen en formato PDF (con soluciones)", "INFO")

tryCatch({
  set.seed(semilla)
  exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
            n = copias,
            name = paste0(nombre_sin_extension, "_sol"),
            encoding = "UTF-8",
            template = "solpcielo",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = TRUE)
  
  log_msg("✓ Examen PDF (con soluciones) generado exitosamente", "SUCCESS")
  
}, error = function(e) {
  log_msg(sprintf("✗ ERROR al generar examen PDF (con soluciones): %s", e$message), "ERROR")
  log_msg(sprintf("Traceback: %s", paste(capture.output(traceback()), collapse = "\n")), "ERROR")
})

################################################################################
# GENERACIÓN DE EXAMEN - FORMATO PDF (SIN SOLUCIONES)
################################################################################

log_msg("Generando examen en formato PDF (sin soluciones)", "INFO")

tryCatch({
  set.seed(semilla)
  exams2pdf(rep(archivo_examen, each = numpreg_por_archivo),
            n = copias,
            name = nombre_sin_extension,
            encoding = "UTF-8",
            template = "exam",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = TRUE)
  
  log_msg("✓ Examen PDF (sin soluciones) generado exitosamente", "SUCCESS")
  
}, error = function(e) {
  log_msg(sprintf("✗ ERROR al generar examen PDF (sin soluciones): %s", e$message), "ERROR")
  log_msg(sprintf("Traceback: %s", paste(capture.output(traceback()), collapse = "\n")), "ERROR")
})

################################################################################
# GENERACIÓN DE EXAMEN - FORMATO NOPS (CON SOLUCIONES)
################################################################################

log_msg("Generando examen en formato NOPS (con soluciones)", "INFO")

tryCatch({
  set.seed(semilla)
  exams2nops(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = paste0(nombre_sin_extension, "_nops_sol"),
             encoding = "UTF-8",
             dir = dir_salida,
             edir = dir_ejercicios,
             verbose = TRUE,
             language = "es",
             title = "Evaluación Fin de Período 4",
             institution = "Sistema ICFES R-Exams",
             logo = NULL,
             date = Sys.Date(),
             replacement = FALSE,
             intro = "Por favor, responda las siguientes preguntas marcando la opción correcta.",
             blank = 0,
             duplex = TRUE,
             pages = NULL,
             usepackage = NULL,
             header = NULL,
             samepage = FALSE,
             twocolumn = FALSE,
             reglength = 2,
             points = NULL,
             showpoints = TRUE,
             solution = TRUE)
  
  log_msg("✓ Examen NOPS (con soluciones) generado exitosamente", "SUCCESS")
  
}, error = function(e) {
  log_msg(sprintf("✗ ERROR al generar examen NOPS (con soluciones): %s", e$message), "ERROR")
  log_msg(sprintf("Traceback: %s", paste(capture.output(traceback()), collapse = "\n")), "ERROR")
})

################################################################################
# GENERACIÓN DE EXAMEN - FORMATO NOPS (SIN SOLUCIONES)
################################################################################

log_msg("Generando examen en formato NOPS (sin soluciones)", "INFO")

tryCatch({
  set.seed(semilla)
  exams2nops(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = paste0(nombre_sin_extension, "_nops"),
             encoding = "UTF-8",
             dir = dir_salida,
             edir = dir_ejercicios,
             verbose = TRUE,
             language = "es",
             title = "Evaluación Fin de Período 4",
             institution = "Sistema ICFES R-Exams",
             logo = NULL,
             date = Sys.Date(),
             replacement = FALSE,
             intro = "Por favor, responda las siguientes preguntas marcando la opción correcta.",
             blank = 0,
             duplex = TRUE,
             pages = NULL,
             usepackage = NULL,
             header = NULL,
             samepage = FALSE,
             twocolumn = FALSE,
             reglength = 2,
             points = NULL,
             showpoints = TRUE,
             solution = FALSE)
  
  log_msg("✓ Examen NOPS (sin soluciones) generado exitosamente", "SUCCESS")
  
}, error = function(e) {
  log_msg(sprintf("✗ ERROR al generar examen NOPS (sin soluciones): %s", e$message), "ERROR")
  log_msg(sprintf("Traceback: %s", paste(capture.output(traceback()), collapse = "\n")), "ERROR")
})

################################################################################
# RESUMEN FINAL
################################################################################

log_msg("Generación de exámenes completada", "INFO")
log_msg(sprintf("Revise el archivo de log: %s", log_file), "INFO")

cat("\n")
cat("================================================================================\n")
cat("  GENERACIÓN DE EXAMEN COMPLETADA\n")
cat("================================================================================\n")
cat("\n")
cat(sprintf("Semilla utilizada: %d\n", semilla))
cat(sprintf("Número de ejercicios por versión: %d\n", NUM_EJERCICIOS))
cat(sprintf("Número de versiones generadas: %d\n", copias))
cat(sprintf("Directorio de salida: %s\n", dir_salida))
cat(sprintf("Archivo de log: %s\n", log_file))
cat("\n")
cat("Revise el archivo de log para ver detalles de errores (si los hubo)\n")
cat("\n")
cat("================================================================================\n")
cat("\n")

