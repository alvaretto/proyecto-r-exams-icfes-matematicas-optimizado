#!/usr/bin/env Rscript
# =============================================================================
# TESTING DE CHUNKS DE PYTHON EN ARCHIVOS .RNW
# =============================================================================
# Este archivo contiene funciones especializadas para testear chunks de Python
# que utilizan matplotlib, numpy y otras librerías en archivos .Rnw
#
# Autor: Sistema de Testing ICFES
# Fecha: 2025-01-11
# =============================================================================

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(testthat)
  library(reticulate)
  library(stringr)
})

# =============================================================================
# FUNCIÓN: configurar_python_testing
# =============================================================================
# Configura el entorno Python para testing
# 
# @param python_path: Ruta específica a Python (opcional)
# @return: TRUE si la configuración fue exitosa
# =============================================================================
configurar_python_testing <- function(python_path = NULL) {
  
  cat("🐍 Configurando entorno Python para testing...\n")
  
  tryCatch({
    # Configurar Python si se especifica una ruta
    if (!is.null(python_path)) {
      use_python(python_path, required = TRUE)
    } else {
      # Usar Python del sistema
      use_python(Sys.which("python"), required = TRUE)
    }
    
    # Verificar que Python está disponible
    py_available <- py_available()
    if (!py_available) {
      stop("Python no está disponible")
    }
    
    # Verificar librerías esenciales
    librerías_requeridas <- c("matplotlib", "numpy", "random")
    for (lib in librerías_requeridas) {
      if (!py_module_available(lib)) {
        warning(paste("Librería Python no disponible:", lib))
      } else {
        cat("✅", lib, "disponible\n")
      }
    }
    
    # Configurar matplotlib para modo no interactivo
    py_run_string("
import matplotlib
matplotlib.use('Agg')  # Backend no interactivo
import matplotlib.pyplot as plt
import numpy as np
import random
")
    
    cat("✅ Configuración Python completada\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error configurando Python:", e$message, "\n")
    return(FALSE)
  })
}

# =============================================================================
# FUNCIÓN: extraer_chunks_python
# =============================================================================
# Extrae específicamente los chunks de Python de un archivo .Rnw
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @return: Lista con chunks de Python y metadatos
# =============================================================================
extraer_chunks_python <- function(archivo_rnw) {
  
  if (!file.exists(archivo_rnw)) {
    stop(paste("El archivo no existe:", archivo_rnw))
  }
  
  contenido <- readLines(archivo_rnw, warn = FALSE, encoding = "UTF-8")
  
  chunks_python <- list()
  en_chunk_python <- FALSE
  chunk_actual <- c()
  numero_chunk <- 0
  
  # Patrones para chunks de Python
  patron_inicio_python <- "^<<.*engine\\s*=\\s*['\"]python['\"].*>>=$"
  patron_fin <- "^@$"
  
  for (i in seq_along(contenido)) {
    linea <- contenido[i]
    
    # Detectar inicio de chunk Python
    if (grepl(patron_inicio_python, linea)) {
      en_chunk_python <- TRUE
      numero_chunk <- numero_chunk + 1
      chunk_actual <- c()
      
      # Extraer opciones del chunk
      opciones_chunk <- gsub("^<<(.*)>>=$", "\\1", linea)
      
    # Detectar fin de chunk
    } else if (grepl(patron_fin, linea) && en_chunk_python) {
      
      # Guardar el chunk
      chunks_python[[length(chunks_python) + 1]] <- list(
        numero = numero_chunk,
        codigo = chunk_actual,
        opciones = opciones_chunk,
        linea_inicio = i - length(chunk_actual) - 1,
        linea_fin = i,
        tiene_matplotlib = any(grepl("matplotlib|plt\\.", chunk_actual)),
        tiene_numpy = any(grepl("numpy|np\\.", chunk_actual)),
        genera_archivo = any(grepl("savefig|plt\\.save", chunk_actual)),
        nombre_archivo = extraer_nombre_archivo_python(chunk_actual)
      )
      
      # Resetear variables
      en_chunk_python <- FALSE
      chunk_actual <- c()
      
    # Agregar línea al chunk actual
    } else if (en_chunk_python) {
      chunk_actual <- c(chunk_actual, linea)
    }
  }
  
  return(list(
    archivo = archivo_rnw,
    chunks_python = chunks_python,
    total_chunks = length(chunks_python),
    chunks_con_matplotlib = sum(sapply(chunks_python, function(x) x$tiene_matplotlib)),
    chunks_con_numpy = sum(sapply(chunks_python, function(x) x$tiene_numpy)),
    chunks_generan_archivos = sum(sapply(chunks_python, function(x) x$genera_archivo))
  ))
}

# =============================================================================
# FUNCIÓN: extraer_nombre_archivo_python
# =============================================================================
# Extrae el nombre del archivo que genera un chunk de Python
# 
# @param codigo_chunk: Vector con las líneas de código del chunk
# @return: Nombre del archivo o NULL si no genera archivo
# =============================================================================
extraer_nombre_archivo_python <- function(codigo_chunk) {
  
  # Buscar líneas con savefig
  lineas_savefig <- grep("savefig|plt\\.save", codigo_chunk, value = TRUE)
  
  if (length(lineas_savefig) > 0) {
    # Extraer el nombre del archivo de la primera línea encontrada
    linea <- lineas_savefig[1]
    
    # Patrones comunes para savefig
    patrones <- c(
      "savefig\\s*\\(\\s*['\"]([^'\"]+)['\"]",
      "plt\\.savefig\\s*\\(\\s*['\"]([^'\"]+)['\"]"
    )
    
    for (patron in patrones) {
      match <- str_match(linea, patron)
      if (!is.na(match[1, 2])) {
        return(match[1, 2])
      }
    }
  }
  
  return(NULL)
}

# =============================================================================
# FUNCIÓN: ejecutar_chunk_python
# =============================================================================
# Ejecuta un chunk específico de Python y valida su funcionamiento
# 
# @param chunk_python: Chunk extraído con extraer_chunks_python()
# @param limpiar_archivos: Si limpiar archivos generados después del test
# @return: Lista con resultado de la ejecución
# =============================================================================
ejecutar_chunk_python <- function(chunk_python, limpiar_archivos = TRUE) {
  
  resultado <- list(
    exito = FALSE,
    error = NULL,
    warnings = c(),
    archivo_generado = NULL,
    archivo_existe = FALSE,
    tiempo_ejecucion = NULL
  )
  
  # Verificar que Python está configurado
  if (!py_available()) {
    resultado$error <- "Python no está disponible"
    return(resultado)
  }
  
  tiempo_inicio <- Sys.time()
  
  tryCatch({
    # Ejecutar el código Python
    codigo_completo <- paste(chunk_python$codigo, collapse = "\n")
    py_run_string(codigo_completo)
    
    # Verificar si se generó un archivo
    if (!is.null(chunk_python$nombre_archivo)) {
      resultado$archivo_generado <- chunk_python$nombre_archivo
      resultado$archivo_existe <- file.exists(chunk_python$nombre_archivo)
      
      # Limpiar archivo si se solicita
      if (limpiar_archivos && resultado$archivo_existe) {
        file.remove(chunk_python$nombre_archivo)
      }
    }
    
    resultado$exito <- TRUE
    
  }, error = function(e) {
    resultado$error <- e$message
  })
  
  resultado$tiempo_ejecucion <- as.numeric(difftime(Sys.time(), tiempo_inicio, units = "secs"))
  
  return(resultado)
}

# =============================================================================
# FUNCIÓN: validar_chunks_python_rnw
# =============================================================================
# Valida todos los chunks de Python en un archivo .Rnw
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @return: Lista con resultados de validación
# =============================================================================
validar_chunks_python_rnw <- function(archivo_rnw) {
  
  cat("🐍 Validando chunks de Python en:", basename(archivo_rnw), "\n")
  
  # Configurar Python
  python_ok <- configurar_python_testing()
  if (!python_ok) {
    return(list(
      archivo = archivo_rnw,
      python_disponible = FALSE,
      chunks_validados = 0,
      chunks_exitosos = 0,
      errores = "Python no disponible"
    ))
  }
  
  # Extraer chunks de Python
  chunks_info <- extraer_chunks_python(archivo_rnw)
  
  if (chunks_info$total_chunks == 0) {
    cat("ℹ️ No se encontraron chunks de Python\n")
    return(list(
      archivo = archivo_rnw,
      python_disponible = TRUE,
      chunks_validados = 0,
      chunks_exitosos = 0,
      chunks_info = chunks_info
    ))
  }
  
  cat("📊 Encontrados", chunks_info$total_chunks, "chunks de Python\n")
  
  # Validar cada chunk
  resultados_chunks <- list()
  chunks_exitosos <- 0
  
  for (i in seq_along(chunks_info$chunks_python)) {
    chunk <- chunks_info$chunks_python[[i]]
    cat("  🧪 Validando chunk", i, "...")
    
    resultado <- ejecutar_chunk_python(chunk)
    resultados_chunks[[i]] <- resultado
    
    if (resultado$exito) {
      chunks_exitosos <- chunks_exitosos + 1
      cat(" ✅\n")
    } else {
      cat(" ❌ Error:", resultado$error, "\n")
    }
  }
  
  # Resumen final
  porcentaje_exito <- round((chunks_exitosos / chunks_info$total_chunks) * 100, 1)
  cat("📈 Resultado:", chunks_exitosos, "/", chunks_info$total_chunks, 
      "chunks exitosos (", porcentaje_exito, "%)\n")
  
  return(list(
    archivo = archivo_rnw,
    python_disponible = TRUE,
    chunks_validados = chunks_info$total_chunks,
    chunks_exitosos = chunks_exitosos,
    porcentaje_exito = porcentaje_exito,
    chunks_info = chunks_info,
    resultados_chunks = resultados_chunks
  ))
}

# =============================================================================
# FUNCIÓN: crear_test_python_rnw
# =============================================================================
# Crea tests específicos para chunks de Python en archivos .Rnw
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @return: Función que ejecuta los tests de Python
# =============================================================================
crear_test_python_rnw <- function(archivo_rnw) {
  
  function() {
    
    cat("\n🐍 TESTING DE CHUNKS PYTHON:", basename(archivo_rnw), "\n")
    cat("=" %R% 60, "\n")
    
    # Test 1: Python disponible
    test_that("Python está disponible y configurado", {
      python_ok <- configurar_python_testing()
      expect_true(python_ok, info = "Python debe estar disponible")
      expect_true(py_available(), info = "reticulate debe detectar Python")
    })
    
    # Test 2: Chunks de Python válidos
    test_that("Chunks de Python se ejecutan correctamente", {
      validacion <- validar_chunks_python_rnw(archivo_rnw)
      
      if (validacion$chunks_validados > 0) {
        expect_gte(validacion$porcentaje_exito, 80, 
                   info = "Al menos 80% de chunks deben ejecutarse exitosamente")
        expect_gt(validacion$chunks_exitosos, 0, 
                  info = "Al menos un chunk debe ejecutarse exitosamente")
      } else {
        skip("No hay chunks de Python para validar")
      }
    })
    
    cat("✅ Tests de Python completados\n")
  }
}

# Operador auxiliar
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

cat("🐍 Utilidades para testing de chunks Python cargadas correctamente\n")
