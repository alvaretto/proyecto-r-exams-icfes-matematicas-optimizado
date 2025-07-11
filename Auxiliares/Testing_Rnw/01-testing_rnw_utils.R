#!/usr/bin/env Rscript
# =============================================================================
# UTILIDADES PARA TESTING DE ARCHIVOS .RNW
# =============================================================================
# Este archivo contiene funciones auxiliares para extraer y testear código
# de archivos .Rnw (LaTeX + R) utilizados en el proyecto ICFES R-exams
#
# Autor: Sistema de Testing ICFES
# Fecha: 2025-01-11
# =============================================================================

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(testthat)
  library(knitr)
  library(stringr)
  library(reticulate)
  library(exams)
})

# =============================================================================
# FUNCIÓN: extraer_codigo_r_desde_rnw
# =============================================================================
# Extrae todos los chunks de código R de un archivo .Rnw
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @param incluir_setup: Si incluir chunks de configuración (echo=FALSE)
# @param incluir_python: Si incluir chunks de Python
# @return: Lista con el código R extraído y metadatos
# =============================================================================
extraer_codigo_r_desde_rnw <- function(archivo_rnw, incluir_setup = TRUE, incluir_python = FALSE) {
  
  # Verificar que el archivo existe
  if (!file.exists(archivo_rnw)) {
    stop(paste("El archivo no existe:", archivo_rnw))
  }
  
  # Leer el contenido del archivo
  contenido <- readLines(archivo_rnw, warn = FALSE, encoding = "UTF-8")
  
  # Inicializar variables
  chunks_r <- list()
  chunks_python <- list()
  lineas_codigo <- c()
  en_chunk <- FALSE
  tipo_chunk <- NULL
  chunk_actual <- c()
  numero_chunk <- 0
  
  # Patrones para identificar chunks
  patron_inicio_r <- "^<<.*>>=$"
  patron_inicio_python <- "^<<.*engine\\s*=\\s*['\"]python['\"].*>>=$"
  patron_fin <- "^@$"
  
  # Procesar línea por línea
  for (i in seq_along(contenido)) {
    linea <- contenido[i]
    
    # Detectar inicio de chunk R
    if (grepl(patron_inicio_r, linea) && !grepl("engine\\s*=\\s*['\"]python['\"]", linea)) {
      en_chunk <- TRUE
      tipo_chunk <- "R"
      numero_chunk <- numero_chunk + 1
      chunk_actual <- c()
      
      # Extraer opciones del chunk
      opciones_chunk <- gsub("^<<(.*)>>=$", "\\1", linea)
      
    # Detectar inicio de chunk Python
    } else if (grepl(patron_inicio_python, linea)) {
      en_chunk <- TRUE
      tipo_chunk <- "Python"
      numero_chunk <- numero_chunk + 1
      chunk_actual <- c()
      
      # Extraer opciones del chunk
      opciones_chunk <- gsub("^<<(.*)>>=$", "\\1", linea)
      
    # Detectar fin de chunk
    } else if (grepl(patron_fin, linea) && en_chunk) {
      
      # Guardar el chunk según su tipo
      if (tipo_chunk == "R" && incluir_setup) {
        chunks_r[[length(chunks_r) + 1]] <- list(
          numero = numero_chunk,
          codigo = chunk_actual,
          opciones = if(exists("opciones_chunk")) opciones_chunk else "",
          linea_inicio = i - length(chunk_actual) - 1,
          linea_fin = i
        )
        lineas_codigo <- c(lineas_codigo, chunk_actual)
        
      } else if (tipo_chunk == "Python" && incluir_python) {
        chunks_python[[length(chunks_python) + 1]] <- list(
          numero = numero_chunk,
          codigo = chunk_actual,
          opciones = if(exists("opciones_chunk")) opciones_chunk else "",
          linea_inicio = i - length(chunk_actual) - 1,
          linea_fin = i
        )
      }
      
      # Resetear variables
      en_chunk <- FALSE
      tipo_chunk <- NULL
      chunk_actual <- c()
      
    # Agregar línea al chunk actual
    } else if (en_chunk) {
      chunk_actual <- c(chunk_actual, linea)
    }
  }
  
  # Crear código R completo para ejecución
  codigo_completo <- paste(lineas_codigo, collapse = "\n")
  
  # Retornar resultado
  return(list(
    archivo = archivo_rnw,
    chunks_r = chunks_r,
    chunks_python = chunks_python,
    codigo_r_completo = codigo_completo,
    total_chunks_r = length(chunks_r),
    total_chunks_python = length(chunks_python),
    lineas_totales = length(contenido)
  ))
}

# =============================================================================
# FUNCIÓN: ejecutar_codigo_r_extraido
# =============================================================================
# Ejecuta el código R extraído en un entorno controlado
# 
# @param codigo_extraido: Resultado de extraer_codigo_r_desde_rnw()
# @param entorno_limpio: Si usar un entorno limpio para la ejecución
# @return: Lista con resultado de la ejecución y posibles errores
# =============================================================================
ejecutar_codigo_r_extraido <- function(codigo_extraido, entorno_limpio = TRUE) {
  
  # Crear entorno de ejecución
  if (entorno_limpio) {
    env_ejecucion <- new.env()
    # Cargar librerías básicas en el entorno
    eval(quote({
      library(testthat)
      library(exams)
      library(reticulate)
      library(knitr)
    }), envir = env_ejecucion)
  } else {
    env_ejecucion <- .GlobalEnv
  }
  
  # Variables para capturar resultados
  resultado <- list(
    exito = FALSE,
    error = NULL,
    warnings = c(),
    variables_creadas = c(),
    tiempo_ejecucion = NULL
  )
  
  # Ejecutar código con manejo de errores
  tiempo_inicio <- Sys.time()
  
  tryCatch({
    # Capturar warnings
    warnings_capturados <- c()
    withCallingHandlers({
      
      # Ejecutar el código R
      eval(parse(text = codigo_extraido$codigo_r_completo), envir = env_ejecucion)
      
    }, warning = function(w) {
      warnings_capturados <<- c(warnings_capturados, w$message)
      invokeRestart("muffleWarning")
    })
    
    # Si llegamos aquí, la ejecución fue exitosa
    resultado$exito <- TRUE
    resultado$warnings <- warnings_capturados
    resultado$variables_creadas <- ls(envir = env_ejecucion)
    
  }, error = function(e) {
    resultado$error <- e$message
  })
  
  resultado$tiempo_ejecucion <- as.numeric(difftime(Sys.time(), tiempo_inicio, units = "secs"))
  
  # Agregar el entorno para inspección posterior
  resultado$entorno <- env_ejecucion
  
  return(resultado)
}

# =============================================================================
# FUNCIÓN: validar_estructura_rnw
# =============================================================================
# Valida la estructura básica de un archivo .Rnw
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @return: Lista con resultados de validación
# =============================================================================
validar_estructura_rnw <- function(archivo_rnw) {
  
  contenido <- readLines(archivo_rnw, warn = FALSE, encoding = "UTF-8")
  
  validacion <- list(
    archivo = archivo_rnw,
    tiene_documentclass = any(grepl("\\\\documentclass", contenido)),
    tiene_begin_document = any(grepl("\\\\begin\\{document\\}", contenido)),
    tiene_end_document = any(grepl("\\\\end\\{document\\}", contenido)),
    tiene_question = any(grepl("\\\\begin\\{question\\}", contenido)),
    tiene_solution = any(grepl("\\\\begin\\{solution\\}", contenido)),
    tiene_chunks_r = any(grepl("^<<.*>>=$", contenido)),
    tiene_meta_info = any(grepl("%%\\s*\\\\ex", contenido)),
    chunks_balanceados = TRUE,  # Se calculará después
    errores = c(),
    warnings = c()
  )
  
  # Verificar balance de chunks
  inicios_chunk <- sum(grepl("^<<.*>>=$", contenido))
  fines_chunk <- sum(grepl("^@$", contenido))
  
  if (inicios_chunk != fines_chunk) {
    validacion$chunks_balanceados <- FALSE
    validacion$errores <- c(validacion$errores, 
                           paste("Chunks desbalanceados:", inicios_chunk, "inicios vs", fines_chunk, "finales"))
  }
  
  # Verificar estructura mínima
  if (!validacion$tiene_documentclass) {
    validacion$errores <- c(validacion$errores, "Falta \\documentclass")
  }
  
  if (!validacion$tiene_begin_document) {
    validacion$errores <- c(validacion$errores, "Falta \\begin{document}")
  }
  
  if (!validacion$tiene_end_document) {
    validacion$errores <- c(validacion$errores, "Falta \\end{document}")
  }
  
  if (!validacion$tiene_question) {
    validacion$warnings <- c(validacion$warnings, "No se encontró \\begin{question}")
  }
  
  # Calcular puntuación de validez
  elementos_requeridos <- c("tiene_documentclass", "tiene_begin_document", "tiene_end_document", "chunks_balanceados")
  elementos_validos <- sum(sapply(elementos_requeridos, function(x) validacion[[x]]))
  validacion$puntuacion_validez <- elementos_validos / length(elementos_requeridos)
  
  return(validacion)
}

# =============================================================================
# FUNCIÓN: crear_test_suite_rnw
# =============================================================================
# Crea una suite de tests completa para un archivo .Rnw
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @param nombre_test: Nombre descriptivo para la suite de tests
# @return: Función que ejecuta todos los tests
# =============================================================================
crear_test_suite_rnw <- function(archivo_rnw, nombre_test = NULL) {
  
  if (is.null(nombre_test)) {
    nombre_test <- paste("Tests para", basename(archivo_rnw))
  }
  
  # Función que ejecuta la suite completa
  function() {
    
    cat("\n" %R% 80, "\n")
    cat("🧪 EJECUTANDO SUITE DE TESTS:", nombre_test, "\n")
    cat("📁 Archivo:", archivo_rnw, "\n")
    cat("⏰ Fecha:", Sys.time(), "\n")
    cat("=" %R% 80, "\n\n")
    
    # Test 1: Validación de estructura
    test_that("Estructura del archivo .Rnw es válida", {
      validacion <- validar_estructura_rnw(archivo_rnw)
      
      expect_true(validacion$tiene_documentclass)
      expect_true(validacion$tiene_begin_document)
      expect_true(validacion$tiene_end_document)
      expect_true(validacion$chunks_balanceados)
      expect_gte(validacion$puntuacion_validez, 0.8)
    })
    
    # Test 2: Extracción de código
    test_that("Extracción de código R funciona correctamente", {
      codigo_extraido <- extraer_codigo_r_desde_rnw(archivo_rnw)
      
      expect_gt(codigo_extraido$total_chunks_r, 0)
      expect_gt(nchar(codigo_extraido$codigo_r_completo), 0)
    })
    
    # Test 3: Ejecución de código R
    test_that("El código R se ejecuta sin errores", {
      codigo_extraido <- extraer_codigo_r_desde_rnw(archivo_rnw)
      resultado <- ejecutar_codigo_r_extraido(codigo_extraido)
      
      expect_true(resultado$exito)
      expect_lt(resultado$tiempo_ejecucion, 30)
    })
    
    cat("\n✅ SUITE DE TESTS COMPLETADA\n")
    cat("=" %R% 80, "\n")
  }
}

# =============================================================================
# OPERADOR AUXILIAR
# =============================================================================
# Operador para repetir strings (usado en formateo)
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

cat("📦 Utilidades para testing de archivos .Rnw cargadas correctamente\n")
cat("🔧 Funciones disponibles:\n")
cat("   - extraer_codigo_r_desde_rnw()\n")
cat("   - ejecutar_codigo_r_extraido()\n")
cat("   - validar_estructura_rnw()\n")
cat("   - crear_test_suite_rnw()\n")
