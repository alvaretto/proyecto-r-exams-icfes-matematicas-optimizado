# SemilleroUnico_v2.R - Configuracion para ejercicio costo boleta
# Ejercicio: Interpretacion de representaciones graficas - Relacion lineal decreciente
# Competencia: interpretacion_representacion
# Componente: numerico_variacional
# Nivel: 2
# Contexto: familiar (entretenimiento)

# Cargar librerias necesarias
library(exams)
library(digest)
library(testthat)
library(knitr)

# Configuracion para eliminar notacion cientifica
options(scipen = 999)
options(digits = 10)
Sys.setlocale(category = "LC_NUMERIC", locale = "C")

# Configuracion de directorio de trabajo
setwd("/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Lab-Manjaro/09-costo-boleta")

# Archivo del ejercicio
archivo_ejercicio <- "costo_boleta_interpretacion_representacion_n2_v1.Rnw"

# Verificar que el archivo existe
if(!file.exists(archivo_ejercicio)) {
  stop("El archivo ", archivo_ejercicio, " no existe en el directorio actual.")
}

# Configuracion de salidas
dir.create("salida", showWarnings = FALSE)

# Funcion para generar HTML
generar_html <- function() {
  cat("Generando salida HTML...\n")
  
  tryCatch({
    exams2html(archivo_ejercicio, 
               n = 3,
               name = "costo_boleta_test",
               dir = "salida",
               template = "plain",
               encoding = "UTF-8")
    cat("✓ HTML generado exitosamente en salida/\n")
  }, error = function(e) {
    cat("✗ Error al generar HTML:", e$message, "\n")
  })
}

# Funcion para generar PDF
generar_pdf <- function() {
  cat("Generando salida PDF...\n")
  
  tryCatch({
    exams2pdf(archivo_ejercicio,
              n = 3,
              name = "costo_boleta_test",
              dir = "salida",
              template = "plain",
              encoding = "UTF-8")
    cat("✓ PDF generado exitosamente en salida/\n")
  }, error = function(e) {
    cat("✗ Error al generar PDF:", e$message, "\n")
  })
}

# Funcion para generar Moodle
generar_moodle <- function() {
  cat("Generando salida Moodle...\n")
  
  tryCatch({
    exams2moodle(archivo_ejercicio,
                 n = 3,
                 name = "costo_boleta_test",
                 dir = "salida",
                 encoding = "UTF-8")
    cat("✓ Moodle XML generado exitosamente en salida/\n")
  }, error = function(e) {
    cat("✗ Error al generar Moodle:", e$message, "\n")
  })
}

# Funcion para ejecutar pruebas de diversidad
ejecutar_pruebas_diversidad <- function() {
  cat("Ejecutando pruebas de diversidad de versiones...\n")
  
  # Cargar el archivo para acceder a la funcion generar_datos
  source_code <- readLines(archivo_ejercicio)
  
  # Extraer y evaluar solo la parte de R
  r_code_start <- which(grepl("<<echo=FALSE, results=hide>>=", source_code, fixed = TRUE))
  r_code_end <- which(grepl("^@$", source_code))
  
  if(length(r_code_start) > 0 && length(r_code_end) > 0) {
    # Encontrar el primer bloque de codigo R
    first_block_start <- r_code_start[1] + 1
    first_block_end <- r_code_end[r_code_end > first_block_start][1] - 1
    
    r_code <- source_code[first_block_start:first_block_end]
    
    # Evaluar el codigo R
    eval(parse(text = paste(r_code, collapse = "\n")))
    
    # Ejecutar prueba de diversidad
    versiones <- list()
    for(i in 1:1000) {
      datos_test <- generar_datos()
      versiones[[i]] <- digest::digest(datos_test)
    }
    
    n_versiones_unicas <- length(unique(versiones))
    cat("Versiones únicas generadas:", n_versiones_unicas, "de 1000\n")
    
    if(n_versiones_unicas >= 300) {
      cat("✓ Prueba de diversidad EXITOSA (≥300 versiones únicas)\n")
    } else {
      cat("✗ Prueba de diversidad FALLIDA (<300 versiones únicas)\n")
    }
  } else {
    cat("✗ No se pudo extraer el código R del archivo .Rnw\n")
  }
}

# Funcion para validar estructura del archivo
validar_estructura <- function() {
  cat("Validando estructura del archivo .Rnw...\n")
  
  contenido <- readLines(archivo_ejercicio)
  
  # Verificaciones basicas
  checks <- list(
    "\\documentclass" = any(grepl("\\\\documentclass", contenido)),
    "\\begin{document}" = any(grepl("\\\\begin\\{document\\}", contenido)),
    "\\end{document}" = any(grepl("\\\\end\\{document\\}", contenido)),
    "\\begin{question}" = any(grepl("\\\\begin\\{question\\}", contenido)),
    "\\end{question}" = any(grepl("\\\\end\\{question\\}", contenido)),
    "\\begin{solution}" = any(grepl("\\\\begin\\{solution\\}", contenido)),
    "\\end{solution}" = any(grepl("\\\\end\\{solution\\}", contenido)),
    "\\exname" = any(grepl("\\\\exname", contenido)),
    "\\extype" = any(grepl("\\\\extype", contenido)),
    "\\exsolution" = any(grepl("\\\\exsolution", contenido)),
    "generar_datos" = any(grepl("generar_datos", contenido)),
    "include_tikz" = any(grepl("include_tikz", contenido))
  )
  
  cat("Verificaciones de estructura:\n")
  for(check_name in names(checks)) {
    status <- if(checks[[check_name]]) "✓" else "✗"
    cat(sprintf("  %s %s\n", status, check_name))
  }
  
  all_passed <- all(unlist(checks))
  if(all_passed) {
    cat("✓ Estructura del archivo VÁLIDA\n")
  } else {
    cat("✗ Estructura del archivo INCOMPLETA\n")
  }
  
  return(all_passed)
}

# Funcion principal para ejecutar todo
ejecutar_todo <- function() {
  cat("=== INICIANDO PROCESAMIENTO DEL EJERCICIO COSTO BOLETA ===\n\n")
  
  # Validar estructura
  if(!validar_estructura()) {
    cat("Deteniendo ejecución debido a problemas de estructura.\n")
    return(FALSE)
  }
  
  cat("\n")
  
  # Ejecutar pruebas
  ejecutar_pruebas_diversidad()
  
  cat("\n")
  
  # Generar salidas
  generar_html()
  cat("\n")
  
  generar_pdf()
  cat("\n")
  
  generar_moodle()
  
  cat("\n=== PROCESAMIENTO COMPLETADO ===\n")
  
  # Mostrar archivos generados
  if(dir.exists("salida")) {
    archivos <- list.files("salida", full.names = FALSE)
    if(length(archivos) > 0) {
      cat("Archivos generados en salida/:\n")
      for(archivo in archivos) {
        cat("  -", archivo, "\n")
      }
    }
  }
  
  return(TRUE)
}

# Funciones individuales para uso manual
html <- function() generar_html()
pdf <- function() generar_pdf()
moodle <- function() generar_moodle()
pruebas <- function() ejecutar_pruebas_diversidad()
validar <- function() validar_estructura()
todo <- function() ejecutar_todo()

# Mensaje de ayuda
cat("=== SEMILLERO ÚNICO V2 - COSTO BOLETA ===\n")
cat("Funciones disponibles:\n")
cat("  html()     - Generar salida HTML\n")
cat("  pdf()      - Generar salida PDF\n")
cat("  moodle()   - Generar salida Moodle XML\n")
cat("  pruebas()  - Ejecutar pruebas de diversidad\n")
cat("  validar()  - Validar estructura del archivo\n")
cat("  todo()     - Ejecutar todo el proceso\n")
cat("\nEjemplo de uso: todo()\n")
cat("=====================================\n\n")
