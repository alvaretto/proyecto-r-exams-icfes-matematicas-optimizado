#!/usr/bin/env Rscript
# =============================================================================
# TESTING DE COMPILACIÓN DE ARCHIVOS .RNW
# =============================================================================
# Este archivo contiene funciones para testear la compilación de archivos .Rnw
# a diferentes formatos (HTML, PDF, Moodle) usando el paquete exams
#
# Autor: Sistema de Testing ICFES
# Fecha: 2025-01-11
# =============================================================================

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(testthat)
  library(exams)
  library(tools)
})

# =============================================================================
# FUNCIÓN: configurar_entorno_compilacion
# =============================================================================
# Configura el entorno para compilación de archivos .Rnw
# 
# @param directorio_trabajo: Directorio donde realizar las compilaciones
# @return: TRUE si la configuración fue exitosa
# =============================================================================
configurar_entorno_compilacion <- function(directorio_trabajo = NULL) {
  
  cat("⚙️ Configurando entorno de compilación...\n")
  
  # Establecer directorio de trabajo
  if (!is.null(directorio_trabajo)) {
    if (!dir.exists(directorio_trabajo)) {
      dir.create(directorio_trabajo, recursive = TRUE)
    }
    setwd(directorio_trabajo)
  }
  
  # Crear directorio temporal para outputs
  dir_temp <- file.path(getwd(), "test_outputs")
  if (!dir.exists(dir_temp)) {
    dir.create(dir_temp)
  }
  
  # Configurar opciones de exams
  options(exams_dir = dir_temp)
  
  # Verificar dependencias
  dependencias <- list(
    latex = Sys.which("pdflatex") != "",
    pandoc = Sys.which("pandoc") != "",
    exams = requireNamespace("exams", quietly = TRUE)
  )
  
  cat("📋 Estado de dependencias:\n")
  for (dep in names(dependencias)) {
    estado <- if (dependencias[[dep]]) "✅" else "❌"
    cat("  ", estado, dep, "\n")
  }
  
  return(all(unlist(dependencias)))
}

# =============================================================================
# FUNCIÓN: compilar_a_html
# =============================================================================
# Compila un archivo .Rnw a formato HTML
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @param n_versiones: Número de versiones a generar
# @param nombre_salida: Nombre base para los archivos de salida
# @return: Lista con resultado de la compilación
# =============================================================================
compilar_a_html <- function(archivo_rnw, n_versiones = 1, nombre_salida = "test_html") {
  
  resultado <- list(
    exito = FALSE,
    error = NULL,
    archivos_generados = c(),
    tiempo_compilacion = NULL,
    warnings = c()
  )
  
  tiempo_inicio <- Sys.time()
  
  tryCatch({
    # Capturar warnings
    warnings_capturados <- c()
    withCallingHandlers({
      
      # Compilar a HTML
      archivos <- exams2html(
        archivo_rnw,
        n = n_versiones,
        name = nombre_salida,
        dir = "test_outputs",
        mathjax = TRUE,
        solution = TRUE,
        encoding = "UTF-8"
      )
      
      resultado$archivos_generados <- archivos
      resultado$exito <- TRUE
      
    }, warning = function(w) {
      warnings_capturados <<- c(warnings_capturados, w$message)
      invokeRestart("muffleWarning")
    })
    
    resultado$warnings <- warnings_capturados
    
  }, error = function(e) {
    resultado$error <- e$message
  })
  
  resultado$tiempo_compilacion <- as.numeric(difftime(Sys.time(), tiempo_inicio, units = "secs"))
  
  return(resultado)
}

# =============================================================================
# FUNCIÓN: compilar_a_pdf
# =============================================================================
# Compila un archivo .Rnw a formato PDF
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @param n_versiones: Número de versiones a generar
# @param nombre_salida: Nombre base para los archivos de salida
# @return: Lista con resultado de la compilación
# =============================================================================
compilar_a_pdf <- function(archivo_rnw, n_versiones = 1, nombre_salida = "test_pdf") {
  
  resultado <- list(
    exito = FALSE,
    error = NULL,
    archivos_generados = c(),
    tiempo_compilacion = NULL,
    warnings = c()
  )
  
  # Verificar que LaTeX está disponible
  if (Sys.which("pdflatex") == "") {
    resultado$error <- "pdflatex no está disponible"
    return(resultado)
  }
  
  tiempo_inicio <- Sys.time()
  
  tryCatch({
    warnings_capturados <- c()
    withCallingHandlers({
      
      # Compilar a PDF
      archivos <- exams2pdf(
        archivo_rnw,
        n = n_versiones,
        name = nombre_salida,
        dir = "test_outputs",
        template = c("plain", "solution"),
        encoding = "UTF-8"
      )
      
      resultado$archivos_generados <- archivos
      resultado$exito <- TRUE
      
    }, warning = function(w) {
      warnings_capturados <<- c(warnings_capturados, w$message)
      invokeRestart("muffleWarning")
    })
    
    resultado$warnings <- warnings_capturados
    
  }, error = function(e) {
    resultado$error <- e$message
  })
  
  resultado$tiempo_compilacion <- as.numeric(difftime(Sys.time(), tiempo_inicio, units = "secs"))
  
  return(resultado)
}

# =============================================================================
# FUNCIÓN: compilar_a_moodle
# =============================================================================
# Compila un archivo .Rnw a formato Moodle XML
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @param n_versiones: Número de versiones a generar
# @param nombre_salida: Nombre base para los archivos de salida
# @return: Lista con resultado de la compilación
# =============================================================================
compilar_a_moodle <- function(archivo_rnw, n_versiones = 1, nombre_salida = "test_moodle") {
  
  resultado <- list(
    exito = FALSE,
    error = NULL,
    archivos_generados = c(),
    tiempo_compilacion = NULL,
    warnings = c()
  )
  
  tiempo_inicio <- Sys.time()
  
  tryCatch({
    warnings_capturados <- c()
    withCallingHandlers({
      
      # Compilar a Moodle
      archivos <- exams2moodle(
        archivo_rnw,
        n = n_versiones,
        name = nombre_salida,
        dir = "test_outputs",
        encoding = "UTF-8"
      )
      
      resultado$archivos_generados <- archivos
      resultado$exito <- TRUE
      
    }, warning = function(w) {
      warnings_capturados <<- c(warnings_capturados, w$message)
      invokeRestart("muffleWarning")
    })
    
    resultado$warnings <- warnings_capturados
    
  }, error = function(e) {
    resultado$error <- e$message
  })
  
  resultado$tiempo_compilacion <- as.numeric(difftime(Sys.time(), tiempo_inicio, units = "secs"))
  
  return(resultado)
}

# =============================================================================
# FUNCIÓN: validar_archivos_generados
# =============================================================================
# Valida que los archivos generados sean válidos
# 
# @param archivos: Vector con rutas de archivos generados
# @param tipo_formato: Tipo de formato ("html", "pdf", "moodle")
# @return: Lista con resultados de validación
# =============================================================================
validar_archivos_generados <- function(archivos, tipo_formato) {
  
  validacion <- list(
    archivos_existen = TRUE,
    archivos_no_vacios = TRUE,
    contenido_valido = TRUE,
    errores = c(),
    tamaños_archivos = c()
  )
  
  for (archivo in archivos) {
    # Verificar existencia
    if (!file.exists(archivo)) {
      validacion$archivos_existen <- FALSE
      validacion$errores <- c(validacion$errores, paste("Archivo no existe:", archivo))
      next
    }
    
    # Verificar tamaño
    tamaño <- file.size(archivo)
    validacion$tamaños_archivos <- c(validacion$tamaños_archivos, tamaño)
    
    if (tamaño == 0) {
      validacion$archivos_no_vacios <- FALSE
      validacion$errores <- c(validacion$errores, paste("Archivo vacío:", archivo))
      next
    }
    
    # Validaciones específicas por formato
    if (tipo_formato == "html") {
      contenido <- readLines(archivo, warn = FALSE)
      if (!any(grepl("<html", contenido, ignore.case = TRUE))) {
        validacion$contenido_valido <- FALSE
        validacion$errores <- c(validacion$errores, paste("HTML inválido:", archivo))
      }
      
    } else if (tipo_formato == "pdf") {
      # Para PDF, verificar que el archivo tenga la cabecera correcta
      con <- file(archivo, "rb")
      cabecera <- readBin(con, "raw", 4)
      close(con)
      
      if (!identical(cabecera, as.raw(c(0x25, 0x50, 0x44, 0x46)))) {  # %PDF
        validacion$contenido_valido <- FALSE
        validacion$errores <- c(validacion$errores, paste("PDF inválido:", archivo))
      }
      
    } else if (tipo_formato == "moodle") {
      contenido <- readLines(archivo, warn = FALSE)
      if (!any(grepl("<quiz", contenido, ignore.case = TRUE))) {
        validacion$contenido_valido <- FALSE
        validacion$errores <- c(validacion$errores, paste("XML Moodle inválido:", archivo))
      }
    }
  }
  
  return(validacion)
}

# =============================================================================
# FUNCIÓN: test_compilacion_completa
# =============================================================================
# Ejecuta tests de compilación completos para un archivo .Rnw
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @param formatos: Vector con formatos a testear ("html", "pdf", "moodle")
# @param n_versiones: Número de versiones a generar por formato
# @return: Lista con resultados de todos los tests
# =============================================================================
test_compilacion_completa <- function(archivo_rnw, formatos = c("html", "pdf", "moodle"), n_versiones = 1) {
  
  cat("🔨 Testing de compilación para:", basename(archivo_rnw), "\n")
  cat("📋 Formatos a testear:", paste(formatos, collapse = ", "), "\n")
  
  # Configurar entorno
  entorno_ok <- configurar_entorno_compilacion()
  if (!entorno_ok) {
    return(list(
      archivo = archivo_rnw,
      entorno_configurado = FALSE,
      resultados = list(),
      error = "No se pudo configurar el entorno de compilación"
    ))
  }
  
  resultados <- list()
  
  # Testear cada formato
  for (formato in formatos) {
    cat("  🔄 Compilando a", formato, "...")
    
    resultado <- switch(formato,
      "html" = compilar_a_html(archivo_rnw, n_versiones, paste0("test_", formato)),
      "pdf" = compilar_a_pdf(archivo_rnw, n_versiones, paste0("test_", formato)),
      "moodle" = compilar_a_moodle(archivo_rnw, n_versiones, paste0("test_", formato))
    )
    
    if (resultado$exito) {
      # Validar archivos generados
      validacion <- validar_archivos_generados(resultado$archivos_generados, formato)
      resultado$validacion_archivos <- validacion
      
      if (validacion$archivos_existen && validacion$archivos_no_vacios && validacion$contenido_valido) {
        cat(" ✅\n")
      } else {
        cat(" ⚠️ (archivos con problemas)\n")
      }
    } else {
      cat(" ❌\n")
      cat("    Error:", resultado$error, "\n")
    }
    
    resultados[[formato]] <- resultado
  }
  
  # Calcular estadísticas generales
  formatos_exitosos <- sum(sapply(resultados, function(x) x$exito))
  porcentaje_exito <- round((formatos_exitosos / length(formatos)) * 100, 1)
  
  cat("📊 Resultado:", formatos_exitosos, "/", length(formatos), 
      "formatos compilados exitosamente (", porcentaje_exito, "%)\n")
  
  return(list(
    archivo = archivo_rnw,
    entorno_configurado = TRUE,
    formatos_testeados = formatos,
    formatos_exitosos = formatos_exitosos,
    porcentaje_exito = porcentaje_exito,
    resultados = resultados
  ))
}

# =============================================================================
# FUNCIÓN: crear_test_compilacion_rnw
# =============================================================================
# Crea tests de compilación para archivos .Rnw
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @param formatos: Formatos a testear
# @return: Función que ejecuta los tests de compilación
# =============================================================================
crear_test_compilacion_rnw <- function(archivo_rnw, formatos = c("html", "moodle")) {
  
  function() {
    
    cat("\n🔨 TESTING DE COMPILACIÓN:", basename(archivo_rnw), "\n")
    cat("=" %R% 60, "\n")
    
    # Ejecutar tests de compilación
    resultados <- test_compilacion_completa(archivo_rnw, formatos)
    
    # Test 1: Entorno configurado
    test_that("El entorno de compilación está configurado", {
      expect_true(resultados$entorno_configurado,
                  info = "El entorno debe estar correctamente configurado")
    })
    
    # Test 2: Compilación exitosa
    test_that("Los archivos se compilan exitosamente", {
      expect_gte(resultados$porcentaje_exito, 80,
                 info = "Al menos 80% de formatos deben compilar exitosamente")
      expect_gt(resultados$formatos_exitosos, 0,
                info = "Al menos un formato debe compilar exitosamente")
    })
    
    # Test 3: Archivos válidos
    for (formato in names(resultados$resultados)) {
      test_that(paste("Archivos", formato, "son válidos"), {
        resultado <- resultados$resultados[[formato]]
        
        if (resultado$exito) {
          validacion <- resultado$validacion_archivos
          expect_true(validacion$archivos_existen,
                      info = paste("Archivos", formato, "deben existir"))
          expect_true(validacion$archivos_no_vacios,
                      info = paste("Archivos", formato, "no deben estar vacíos"))
          expect_true(validacion$contenido_valido,
                      info = paste("Contenido", formato, "debe ser válido"))
        } else {
          skip(paste("Compilación", formato, "falló"))
        }
      })
    }
    
    cat("✅ Tests de compilación completados\n")
  }
}

# Operador auxiliar
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

cat("🔨 Utilidades para testing de compilación .Rnw cargadas correctamente\n")
