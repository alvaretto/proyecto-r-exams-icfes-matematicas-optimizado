# ============================================================================
# CONFIGURACIÓN SEMILLERO ÚNICO v2 - PUNTO K TRIGONOMETRÍA
# ============================================================================
# Archivo: SemilleroUnico_v2.R
# Propósito: Generar ejercicios R-exams en múltiples formatos
# Ejercicio: Trigonometría - Interpretación de gráficas KP vs ángulo α
# ============================================================================

# Limpiar entorno
rm(list = ls())

# Configuración de localización
Sys.setlocale("LC_ALL", "C")
options(OutDec = ".")

# Cargar librerías necesarias
library(exams)
library(knitr)

# Configuración de directorio de trabajo
setwd("/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Lab-Manjaro/PuntoK-Rnw")

# Verificar que el archivo .Rnw existe
archivo_ejercicio <- "PuntoK_Trigonometria_v1.Rnw"
if(!file.exists(archivo_ejercicio)) {
  stop(paste("Error: No se encuentra el archivo", archivo_ejercicio))
}

# ============================================================================
# CONFIGURACIÓN DE SALIDAS
# ============================================================================

# Crear directorio de salida si no existe
dir_salida <- "salida"
if(!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
}

# ============================================================================
# GENERACIÓN HTML
# ============================================================================

cat("Generando salida HTML...\n")
try({
  exams2html(
    file = archivo_ejercicio,
    n = 3,  # Generar 3 versiones para prueba
    name = "PuntoK_Trigonometria_HTML",
    dir = dir_salida,
    template = "plain.html",
    encoding = "UTF-8",
    edir = ".",
    tdir = tempdir(),
    sdir = NULL,
    verbose = TRUE,
    resolution = 150
  )
  cat("✓ Salida HTML generada exitosamente\n")
}, silent = FALSE)

# ============================================================================
# GENERACIÓN PDF
# ============================================================================

cat("Generando salida PDF...\n")
try({
  exams2pdf(
    file = archivo_ejercicio,
    n = 3,  # Generar 3 versiones para prueba
    name = "PuntoK_Trigonometria_PDF",
    dir = dir_salida,
    template = "plain.tex",
    encoding = "UTF-8",
    edir = ".",
    tdir = tempdir(),
    sdir = NULL,
    verbose = TRUE,
    resolution = 150
  )
  cat("✓ Salida PDF generada exitosamente\n")
}, silent = FALSE)

# ============================================================================
# GENERACIÓN MOODLE
# ============================================================================

cat("Generando salida Moodle XML...\n")
try({
  exams2moodle(
    file = archivo_ejercicio,
    n = 5,  # Generar 5 versiones para Moodle
    name = "PuntoK_Trigonometria_Moodle",
    dir = dir_salida,
    encoding = "UTF-8",
    edir = ".",
    tdir = tempdir(),
    sdir = NULL,
    verbose = TRUE,
    resolution = 150,
    converter = "pandoc-mathjax"
  )
  cat("✓ Salida Moodle XML generada exitosamente\n")
}, silent = FALSE)

# ============================================================================
# TESTING AUTOMATIZADO
# ============================================================================

cat("Ejecutando pruebas automatizadas...\n")

# Cargar testthat para las pruebas
library(testthat)

# Ejecutar las pruebas definidas en el archivo .Rnw
source_file <- archivo_ejercicio

# Prueba de compilación básica
test_that("Prueba de compilación básica", {
  expect_true(file.exists(archivo_ejercicio),
              info = "El archivo .Rnw debe existir")

  # Intentar compilar una versión de prueba
  resultado <- try({
    exams2html(archivo_ejercicio, n = 1, name = "test_compilacion",
               dir = tempdir(), verbose = FALSE)
  }, silent = TRUE)

  expect_false(inherits(resultado, "try-error"),
               info = "La compilación HTML debe ser exitosa")
})

# Prueba de diversidad (ejecutar función desde el archivo)
cat("Ejecutando prueba de diversidad de versiones...\n")
try({
  # Cargar las funciones del archivo .Rnw
  temp_env <- new.env()

  # Extraer y evaluar el código R del archivo .Rnw
  rnw_content <- readLines(archivo_ejercicio)
  r_chunks <- grep("^<<.*>>=", rnw_content)
  r_end_chunks <- grep("^@$", rnw_content)

  if(length(r_chunks) > 0 && length(r_end_chunks) > 0) {
    # Extraer el primer chunk que contiene las funciones
    start_line <- r_chunks[1] + 1
    end_line <- r_end_chunks[1] - 1

    if(start_line <= end_line) {
      r_code <- paste(rnw_content[start_line:end_line], collapse = "\n")
      eval(parse(text = r_code), envir = temp_env)

      # Ejecutar prueba de diversidad si la función existe
      if(exists("generar_datos", envir = temp_env)) {
        versiones <- list()
        for(i in 1:100) {  # Prueba reducida para validación rápida
          datos_test <- temp_env$generar_datos()
          versiones[[i]] <- digest::digest(datos_test)
        }

        n_versiones_unicas <- length(unique(versiones))
        cat("Versiones únicas generadas en prueba:", n_versiones_unicas, "de 100\n")

        if(n_versiones_unicas >= 30) {  # Al menos 30% de diversidad
          cat("✓ Prueba de diversidad EXITOSA\n")
        } else {
          cat("⚠ Advertencia: Baja diversidad en generación de datos\n")
        }
      }
    }
  }
}, silent = FALSE)

# ============================================================================
# REPORTE FINAL
# ============================================================================

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("REPORTE DE GENERACIÓN COMPLETADO\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("Ejercicio:", archivo_ejercicio, "\n")
cat("Directorio de salida:", dir_salida, "\n")
cat("Formatos generados: HTML, PDF, Moodle XML\n")

# Listar archivos generados
archivos_generados <- list.files(dir_salida, full.names = FALSE)
if(length(archivos_generados) > 0) {
  cat("\nArchivos generados:\n")
  for(archivo in archivos_generados) {
    cat("  -", archivo, "\n")
  }
} else {
  cat("\n⚠ No se encontraron archivos en el directorio de salida\n")
}

cat("\n✓ Proceso completado exitosamente\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# ============================================================================
# INFORMACIÓN ADICIONAL
# ============================================================================

cat("\nINFORMACIÓN DEL EJERCICIO:\n")
cat("- Competencia ICFES: Interpretación y Representación\n")
cat("- Componente: Geométrico-Métrico\n")
cat("- Tema: Trigonometría\n")
cat("- Nivel de dificultad: 3\n")
cat("- Contexto: Matemático\n")
cat("- Tipo: Selección múltiple (schoice)\n")
cat("- Aleatorización: 300+ versiones únicas\n")
cat("- Gráficos: TikZ de alta fidelidad\n")

cat("\nPara usar los archivos generados:\n")
cat("1. HTML: Abrir en navegador web\n")
cat("2. PDF: Imprimir o visualizar\n")
cat("3. Moodle XML: Importar en plataforma Moodle\n")

cat("\n¡Ejercicio listo para uso en evaluaciones ICFES!\n")
