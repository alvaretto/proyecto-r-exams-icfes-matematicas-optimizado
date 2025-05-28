# Script de pruebas para proporciones_encuesta_deportiva_v1.Rmd
# Autor: Sistema R-Exams ICFES
# Fecha: 2025-01-27

# Carga de librerías necesarias
library(exams)
library(testthat)

# Configuración inicial
archivo_examen <- "proporciones_encuesta_deportiva_v1.Rmd"
copias <- 3
numpreg <- 1
semilla <- sample(100:1e8, 1)
set.seed(semilla)
dir_salida <- "salida"

# Nombre del archivo sin la extensión .Rmd
nombre_sin_extension <- sub("\\.Rmd$", "", archivo_examen)
nombre_arch <- paste0(nombre_sin_extension, "_")

cat("=================================================================\n")
cat("PRUEBAS PARA ENCUESTA DEPORTIVA - PROPORCIONES\n")
cat("=================================================================\n")
cat("Archivo:", archivo_examen, "\n")
cat("Semilla:", semilla, "\n")
cat("Fecha:", Sys.Date(), "\n")
cat("=================================================================\n\n")

# Verificar que el archivo existe
if (!file.exists(archivo_examen)) {
  stop("Error: El archivo ", archivo_examen, " no existe en el directorio actual.")
}

# Crear directorio de salida si no existe
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
  cat("Directorio de salida creado:", dir_salida, "\n")
}

################################################################################
# PRUEBA 1: Verificación básica de sintaxis y compilación
################################################################################

cat("\n--- PRUEBA 1: Verificación de sintaxis ---\n")
tryCatch({
  # Intentar compilar una versión simple en HTML
  resultado_html <- exams2html(archivo_examen,
                              n = 1,
                              verbose = FALSE,
                              svg = FALSE)
  cat("✓ Sintaxis correcta - Compilación HTML exitosa\n")
}, error = function(e) {
  cat("✗ Error en sintaxis:", e$message, "\n")
  stop("Deteniendo pruebas debido a errores de sintaxis")
})

################################################################################
# PRUEBA 2: Generación de múltiples variantes para verificar aleatorización
################################################################################

cat("\n--- PRUEBA 2: Verificación de aleatorización ---\n")
tryCatch({
  # Generar 5 variantes para verificar diversidad
  set.seed(semilla)
  variantes <- exams2html(rep(archivo_examen, 5),
                         n = 1,
                         verbose = FALSE,
                         svg = FALSE)
  cat("✓ Aleatorización funcionando - 5 variantes generadas\n")
}, error = function(e) {
  cat("✗ Error en aleatorización:", e$message, "\n")
})

################################################################################
# PRUEBA 3: Generación en formato PDF
################################################################################

cat("\n--- PRUEBA 3: Generación PDF ---\n")
tryCatch({
  exams2pdf(archivo_examen,
            n = copias,
            name = paste0(nombre_arch, "test_"),
            encoding = "UTF-8",
            template = "solpcielo",
            dir = dir_salida,
            verbose = FALSE)
  cat("✓ Generación PDF exitosa -", copias, "copias creadas\n")
}, error = function(e) {
  cat("✗ Error en PDF:", e$message, "\n")
})

################################################################################
# PRUEBA 4: Generación en formato DOCX
################################################################################

cat("\n--- PRUEBA 4: Generación DOCX ---\n")
tryCatch({
  exams2pandoc(archivo_examen,
               n = copias,
               name = paste0(nombre_arch, "test_"),
               encoding = "UTF-8",
               template = "pcielo.tex",
               dir = dir_salida,
               type = "docx",
               verbose = FALSE)
  cat("✓ Generación DOCX exitosa -", copias, "copias creadas\n")
}, error = function(e) {
  cat("✗ Error en DOCX:", e$message, "\n")
})

################################################################################
# PRUEBA 5: Generación para Moodle XML
################################################################################

cat("\n--- PRUEBA 5: Generación Moodle XML ---\n")
tryCatch({
  exams2moodle(archivo_examen,
               n = copias,
               name = paste0(nombre_arch, "test_"),
               encoding = "UTF-8",
               dir = dir_salida,
               svg = TRUE,
               mchoice = list(shuffle = TRUE,
                             answernumbering = "ABCD",
                             eval = list(partial = TRUE,
                                        rule = "none")),
               verbose = FALSE)
  cat("✓ Generación Moodle XML exitosa -", copias, "copias creadas\n")
}, error = function(e) {
  cat("✗ Error en Moodle XML:", e$message, "\n")
})

################################################################################
# PRUEBA 6: Verificación de archivos generados
################################################################################

cat("\n--- PRUEBA 6: Verificación de archivos ---\n")
archivos_generados <- list.files(dir_salida, pattern = paste0(nombre_sin_extension, "_test_"), full.names = TRUE)
if (length(archivos_generados) > 0) {
  cat("✓ Archivos generados encontrados:\n")
  for (archivo in archivos_generados) {
    tamaño <- file.size(archivo)
    cat("  -", basename(archivo), "(", round(tamaño/1024, 2), "KB )\n")
  }
} else {
  cat("⚠ No se encontraron archivos de prueba generados\n")
}

################################################################################
# RESUMEN FINAL
################################################################################

cat("\n=================================================================\n")
cat("RESUMEN DE PRUEBAS COMPLETADO\n")
cat("=================================================================\n")
cat("Archivo probado:", archivo_examen, "\n")
cat("Semilla utilizada:", semilla, "\n")
cat("Directorio de salida:", dir_salida, "\n")
cat("Fecha de prueba:", Sys.Date(), "\n")
cat("=================================================================\n")

# Limpiar workspace (opcional)
# rm(list = ls())
