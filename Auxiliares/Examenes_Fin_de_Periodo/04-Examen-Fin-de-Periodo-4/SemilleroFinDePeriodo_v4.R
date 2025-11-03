################################################################################
# SCRIPT: SemilleroFinDePeriodo_v4.R
# DESCRIPCIÓN: Generación automática de examen de fin de período
#              Selecciona aleatoriamente 15 ejercicios de todos los .Rmd disponibles
# AUTOR: Sistema ICFES R-Exams
# FECHA: 2025
################################################################################

# Carga de la librería r-exams
library(exams)

################################################################################
# CONFIGURACIÓN INICIAL
################################################################################

# Establecer el directorio de trabajo al directorio del script
# Esto asegura que el script funcione correctamente sin importar desde dónde se ejecute
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

cat(sprintf("Directorio de trabajo: %s\n\n", getwd()))

# Número de ejercicios a seleccionar para el examen
NUM_EJERCICIOS <- 15

# Configuración de directorios
dir_salida <- "salida"
dir_ejercicios <- "."

# Configuración de copias y semilla
copias <- 1
numpreg_por_archivo <- 1
semilla <- sample(100:1e8, 1)
set.seed(semilla)

# Nombre del examen
nombre_sin_extension <- "Evaluacion_Fin_de_Periodo_4"
nombre_arch <- paste0(nombre_sin_extension, "_")

################################################################################
# DETECCIÓN AUTOMÁTICA DE ARCHIVOS .RMD DISPONIBLES
################################################################################

cat("\n")
cat("================================================================================\n")
cat("  GENERACIÓN DE EXAMEN DE FIN DE PERÍODO 4\n")
cat("================================================================================\n")
cat("\n")

# Listar todos los archivos .Rmd en el directorio actual
todos_los_rmd <- list.files(path = dir_ejercicios,
                             pattern = "\\.Rmd$",
                             full.names = FALSE)

cat(sprintf("Total de archivos .Rmd encontrados: %d\n", length(todos_los_rmd)))

# Excluir archivos que no sean ejercicios (por ejemplo, archivos de configuración)
# Filtrar solo archivos que comienzan con números (formato: 001-..., 002-..., etc.)
ejercicios_disponibles <- todos_los_rmd[grepl("^[0-9]{3}-", todos_los_rmd)]

cat(sprintf("Total de ejercicios .Rmd disponibles (con prefijo numérico): %d\n", length(ejercicios_disponibles)))

# Si no se encuentran ejercicios con prefijo, mostrar ayuda
if (length(ejercicios_disponibles) == 0 && length(todos_los_rmd) > 0) {
  cat("\n⚠️  ADVERTENCIA: Se encontraron archivos .Rmd pero ninguno con prefijo numérico (001-, 002-, etc.)\n")
  cat("   Primeros archivos encontrados:\n")
  for (i in 1:min(5, length(todos_los_rmd))) {
    cat(sprintf("   - %s\n", todos_los_rmd[i]))
  }
}

cat("\n")

################################################################################
# VALIDACIÓN: Verificar que hay suficientes ejercicios
################################################################################

if (length(ejercicios_disponibles) < NUM_EJERCICIOS) {
  stop(sprintf("ERROR: Se requieren al menos %d ejercicios, pero solo hay %d disponibles.\n",
               NUM_EJERCICIOS, length(ejercicios_disponibles)))
}

################################################################################
# SELECCIÓN ALEATORIA DE EJERCICIOS
################################################################################

cat(sprintf("Seleccionando aleatoriamente %d ejercicios...\n", NUM_EJERCICIOS))

# Seleccionar aleatoriamente NUM_EJERCICIOS archivos sin repetición
set.seed(semilla)
archivo_examen <- sample(ejercicios_disponibles, NUM_EJERCICIOS, replace = FALSE)

# Mezclar el orden de los ejercicios seleccionados
archivo_examen <- sample(archivo_examen)

cat("\n")
cat("Ejercicios seleccionados para el examen:\n")
cat("----------------------------------------\n")
for (i in 1:length(archivo_examen)) {
  cat(sprintf("%2d. %s\n", i, archivo_examen[i]))
}
cat("\n")
cat(sprintf("Semilla aleatoria utilizada: %d\n", semilla))
cat("\n")

################################################################################
# GENERACIÓN DE EXAMEN - FORMATO DOCX (CON SOLUCIONES)
################################################################################

cat("================================================================================\n")
cat("  GENERANDO EXAMEN EN FORMATO DOCX (CON SOLUCIONES)\n")
cat("================================================================================\n")
cat("\n")

tryCatch({
  set.seed(semilla)
  exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
               n = copias,
               name = paste0(nombre_sin_extension, "-docx"),
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
               verbose = TRUE,
               points = NULL,
               exshuffle = NULL,
               type = "docx")

  cat("\n✓ Examen DOCX (con soluciones) generado exitosamente\n\n")

}, error = function(e) {
  cat("\n✗ ERROR al generar examen DOCX (con soluciones):\n")
  cat(sprintf("  %s\n\n", e$message))
  cat("  Continuando con los siguientes formatos...\n\n")
})

################################################################################
# GENERACIÓN DE EXAMEN - FORMATO DOCX (SIN SOLUCIONES)
################################################################################

cat("================================================================================\n")
cat("  GENERANDO EXAMEN EN FORMATO DOCX (SIN SOLUCIONES)\n")
cat("================================================================================\n")
cat("\n")

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

  cat("\n✓ Examen DOCX (sin soluciones) generado exitosamente\n\n")

}, error = function(e) {
  cat("\n✗ ERROR al generar examen DOCX (sin soluciones):\n")
  cat(sprintf("  %s\n\n", e$message))
  cat("  Continuando con los siguientes formatos...\n\n")
})


################################################################################
# GENERACIÓN DE EXAMEN - FORMATO PDF (CON SOLUCIONES)
################################################################################

cat("================================================================================\n")
cat("  GENERANDO EXAMEN EN FORMATO PDF (CON SOLUCIONES)\n")
cat("================================================================================\n")
cat("\n")

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

  cat("\n✓ Examen PDF (con soluciones) generado exitosamente\n\n")

}, error = function(e) {
  cat("\n✗ ERROR al generar examen PDF (con soluciones):\n")
  cat(sprintf("  %s\n\n", e$message))
  cat("  Continuando con los siguientes formatos...\n\n")
})

################################################################################
# GENERACIÓN DE EXAMEN - FORMATO PDF (SIN SOLUCIONES)
################################################################################

cat("================================================================================\n")
cat("  GENERANDO EXAMEN EN FORMATO PDF (SIN SOLUCIONES)\n")
cat("================================================================================\n")
cat("\n")

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

  cat("\n✓ Examen PDF (sin soluciones) generado exitosamente\n\n")

}, error = function(e) {
  cat("\n✗ ERROR al generar examen PDF (sin soluciones):\n")
  cat(sprintf("  %s\n\n", e$message))
  cat("  Continuando...\n\n")
})

################################################################################
# RESUMEN FINAL
################################################################################

cat("\n")
cat("================================================================================\n")
cat("  GENERACIÓN DE EXAMEN COMPLETADA\n")
cat("================================================================================\n")
cat("\n")
cat(sprintf("Semilla utilizada: %d\n", semilla))
cat(sprintf("Número de ejercicios: %d\n", NUM_EJERCICIOS))
cat(sprintf("Directorio de salida: %s\n", dir_salida))
cat("\n")
cat("Archivos generados:\n")
cat("  - Evaluacion_Fin_de_Periodo_4-docx1.docx (con soluciones)\n")
cat("  - Evaluacion_Fin_de_Periodo_4_sin_sol1.docx (sin soluciones)\n")
cat("  - Evaluacion_Fin_de_Periodo_4_sol1.pdf (con soluciones)\n")
cat("  - Evaluacion_Fin_de_Periodo_41.pdf (sin soluciones)\n")
cat("\n")
cat("================================================================================\n")
cat("\n")
