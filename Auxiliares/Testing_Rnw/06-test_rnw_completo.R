#!/usr/bin/env Rscript
# =============================================================================
# SCRIPT PRINCIPAL DE TESTING PARA ARCHIVOS .RNW
# =============================================================================
# Este script ejecuta una suite completa de tests para archivos .Rnw
# incluyendo validación de estructura, ejecución de código R y Python,
# coherencia matemática y compilación a diferentes formatos.
#
# Uso:
#   Rscript test_rnw_completo.R archivo.Rnw
#   Rscript test_rnw_completo.R  # Para testear el archivo actual
#
# Autor: Sistema de Testing ICFES
# Fecha: 2025-01-11
# =============================================================================

# Cargar utilidades de testing
source("01-testing_rnw_utils.R")
source("02-testing_python_chunks.R")
source("03-testing_coherencia_matematica.R")
source("04-testing_compilacion_rnw.R")

# =============================================================================
# FUNCIÓN PRINCIPAL: ejecutar_test_suite_completa
# =============================================================================
# Ejecuta la suite completa de tests para un archivo .Rnw
# 
# @param archivo_rnw: Ruta al archivo .Rnw a testear
# @param incluir_compilacion: Si incluir tests de compilación (más lentos)
# @param formatos_compilacion: Formatos a testear en compilación
# @param generar_reporte: Si generar un reporte detallado
# @return: Lista con todos los resultados
# =============================================================================
ejecutar_test_suite_completa <- function(archivo_rnw, 
                                        incluir_compilacion = TRUE,
                                        formatos_compilacion = c("html", "moodle"),
                                        generar_reporte = TRUE) {
  
  # Verificar que el archivo existe
  if (!file.exists(archivo_rnw)) {
    stop(paste("El archivo no existe:", archivo_rnw))
  }
  
  cat("\n")
  cat("🧪" %R% 80, "\n")
  cat("🧪 SUITE COMPLETA DE TESTING PARA ARCHIVOS .RNW\n")
  cat("🧪" %R% 80, "\n")
  cat("📁 Archivo:", archivo_rnw, "\n")
  cat("⏰ Inicio:", Sys.time(), "\n")
  cat("🔧 Compilación:", if(incluir_compilacion) "SÍ" else "NO", "\n")
  cat("📋 Formatos:", if(incluir_compilacion) paste(formatos_compilacion, collapse = ", ") else "N/A", "\n")
  cat("=" %R% 80, "\n\n")
  
  # Inicializar resultados
  resultados_completos <- list(
    archivo = archivo_rnw,
    timestamp = Sys.time(),
    tests_ejecutados = c(),
    tests_exitosos = c(),
    errores_criticos = c(),
    warnings_generales = c()
  )
  
  tiempo_inicio_total <- Sys.time()
  
  # ==========================================================================
  # FASE 1: VALIDACIÓN DE ESTRUCTURA
  # ==========================================================================
  cat("📋 FASE 1: VALIDACIÓN DE ESTRUCTURA\n")
  cat("-" %R% 40, "\n")
  
  tryCatch({
    validacion_estructura <- validar_estructura_rnw(archivo_rnw)
    resultados_completos$validacion_estructura <- validacion_estructura
    resultados_completos$tests_ejecutados <- c(resultados_completos$tests_ejecutados, "estructura")
    
    if (validacion_estructura$puntuacion_validez >= 0.8) {
      resultados_completos$tests_exitosos <- c(resultados_completos$tests_exitosos, "estructura")
      cat("✅ Estructura válida (", round(validacion_estructura$puntuacion_validez * 100, 1), "%)\n")
    } else {
      cat("❌ Problemas de estructura detectados\n")
      for (error in validacion_estructura$errores) {
        cat("  -", error, "\n")
      }
    }
  }, error = function(e) {
    resultados_completos$errores_criticos <- c(resultados_completos$errores_criticos, 
                                              paste("Error validando estructura:", e$message))
    cat("❌ Error crítico en validación de estructura:", e$message, "\n")
  })
  
  # ==========================================================================
  # FASE 2: EXTRACCIÓN Y EJECUCIÓN DE CÓDIGO R
  # ==========================================================================
  cat("\n📝 FASE 2: EXTRACCIÓN Y EJECUCIÓN DE CÓDIGO R\n")
  cat("-" %R% 40, "\n")
  
  tryCatch({
    codigo_extraido <- extraer_codigo_r_desde_rnw(archivo_rnw)
    resultados_completos$codigo_extraido <- codigo_extraido
    resultados_completos$tests_ejecutados <- c(resultados_completos$tests_ejecutados, "extraccion_r")
    
    cat("📊 Chunks R encontrados:", codigo_extraido$total_chunks_r, "\n")
    cat("🐍 Chunks Python encontrados:", codigo_extraido$total_chunks_python, "\n")
    
    if (codigo_extraido$total_chunks_r > 0) {
      # Ejecutar código R
      resultado_ejecucion <- ejecutar_codigo_r_extraido(codigo_extraido)
      resultados_completos$ejecucion_r <- resultado_ejecucion
      
      if (resultado_ejecucion$exito) {
        resultados_completos$tests_exitosos <- c(resultados_completos$tests_exitosos, "ejecucion_r")
        cat("✅ Código R ejecutado exitosamente (", round(resultado_ejecucion$tiempo_ejecucion, 2), "s)\n")
        
        # Guardar entorno para tests posteriores
        entorno_ejecucion <- resultado_ejecucion$entorno
        
      } else {
        cat("❌ Error ejecutando código R:", resultado_ejecucion$error, "\n")
      }
    } else {
      cat("⚠️ No se encontraron chunks de R para ejecutar\n")
    }
    
  }, error = function(e) {
    resultados_completos$errores_criticos <- c(resultados_completos$errores_criticos, 
                                              paste("Error extrayendo/ejecutando R:", e$message))
    cat("❌ Error crítico en extracción/ejecución R:", e$message, "\n")
  })
  
  # ==========================================================================
  # FASE 3: VALIDACIÓN DE CHUNKS PYTHON
  # ==========================================================================
  cat("\n🐍 FASE 3: VALIDACIÓN DE CHUNKS PYTHON\n")
  cat("-" %R% 40, "\n")
  
  tryCatch({
    validacion_python <- validar_chunks_python_rnw(archivo_rnw)
    resultados_completos$validacion_python <- validacion_python
    resultados_completos$tests_ejecutados <- c(resultados_completos$tests_ejecutados, "python")
    
    if (validacion_python$chunks_validados > 0) {
      if (validacion_python$porcentaje_exito >= 80) {
        resultados_completos$tests_exitosos <- c(resultados_completos$tests_exitosos, "python")
      }
    } else {
      resultados_completos$tests_exitosos <- c(resultados_completos$tests_exitosos, "python")
      cat("ℹ️ No hay chunks de Python (OK)\n")
    }
    
  }, error = function(e) {
    resultados_completos$errores_criticos <- c(resultados_completos$errores_criticos, 
                                              paste("Error validando Python:", e$message))
    cat("❌ Error crítico en validación Python:", e$message, "\n")
  })
  
  # ==========================================================================
  # FASE 4: COHERENCIA MATEMÁTICA
  # ==========================================================================
  cat("\n🔢 FASE 4: COHERENCIA MATEMÁTICA\n")
  cat("-" %R% 40, "\n")
  
  if (exists("entorno_ejecucion")) {
    tryCatch({
      validacion_coherencia <- validar_coherencia_completa(entorno_ejecucion)
      resultados_completos$validacion_coherencia <- validacion_coherencia
      resultados_completos$tests_ejecutados <- c(resultados_completos$tests_ejecutados, "coherencia")
      
      if (validacion_coherencia$coherencia_general) {
        resultados_completos$tests_exitosos <- c(resultados_completos$tests_exitosos, "coherencia")
      }
      
    }, error = function(e) {
      resultados_completos$errores_criticos <- c(resultados_completos$errores_criticos, 
                                                paste("Error validando coherencia:", e$message))
      cat("❌ Error crítico en validación de coherencia:", e$message, "\n")
    })
  } else {
    cat("⚠️ No se puede validar coherencia (código R no ejecutado)\n")
  }
  
  # ==========================================================================
  # FASE 5: COMPILACIÓN (OPCIONAL)
  # ==========================================================================
  if (incluir_compilacion) {
    cat("\n🔨 FASE 5: COMPILACIÓN A DIFERENTES FORMATOS\n")
    cat("-" %R% 40, "\n")
    
    tryCatch({
      resultados_compilacion <- test_compilacion_completa(archivo_rnw, formatos_compilacion)
      resultados_completos$compilacion <- resultados_compilacion
      resultados_completos$tests_ejecutados <- c(resultados_completos$tests_ejecutados, "compilacion")
      
      if (resultados_compilacion$porcentaje_exito >= 80) {
        resultados_completos$tests_exitosos <- c(resultados_completos$tests_exitosos, "compilacion")
      }
      
    }, error = function(e) {
      resultados_completos$errores_criticos <- c(resultados_completos$errores_criticos, 
                                                paste("Error en compilación:", e$message))
      cat("❌ Error crítico en compilación:", e$message, "\n")
    })
  }
  
  # ==========================================================================
  # RESUMEN FINAL
  # ==========================================================================
  tiempo_total <- as.numeric(difftime(Sys.time(), tiempo_inicio_total, units = "secs"))
  
  cat("\n")
  cat("📊" %R% 80, "\n")
  cat("📊 RESUMEN FINAL\n")
  cat("📊" %R% 80, "\n")
  
  total_tests <- length(resultados_completos$tests_ejecutados)
  tests_exitosos <- length(resultados_completos$tests_exitosos)
  porcentaje_exito <- if (total_tests > 0) round((tests_exitosos / total_tests) * 100, 1) else 0
  
  cat("⏱️ Tiempo total:", round(tiempo_total, 2), "segundos\n")
  cat("🧪 Tests ejecutados:", total_tests, "\n")
  cat("✅ Tests exitosos:", tests_exitosos, "\n")
  cat("📈 Porcentaje de éxito:", porcentaje_exito, "%\n")
  
  if (length(resultados_completos$errores_criticos) > 0) {
    cat("❌ Errores críticos:", length(resultados_completos$errores_criticos), "\n")
  }
  
  # Determinar estado general
  estado_general <- if (porcentaje_exito >= 80 && length(resultados_completos$errores_criticos) == 0) {
    "✅ APROBADO"
  } else if (porcentaje_exito >= 60) {
    "⚠️ APROBADO CON OBSERVACIONES"
  } else {
    "❌ REPROBADO"
  }
  
  cat("\n🎯 ESTADO GENERAL:", estado_general, "\n")
  
  # Agregar información final a resultados
  resultados_completos$tiempo_total <- tiempo_total
  resultados_completos$total_tests <- total_tests
  resultados_completos$tests_exitosos_count <- tests_exitosos
  resultados_completos$porcentaje_exito <- porcentaje_exito
  resultados_completos$estado_general <- estado_general
  
  # Generar reporte si se solicita
  if (generar_reporte) {
    generar_reporte_testing(resultados_completos)
  }
  
  cat("=" %R% 80, "\n")
  
  return(resultados_completos)
}

# =============================================================================
# FUNCIÓN: generar_reporte_testing
# =============================================================================
# Genera un reporte detallado de los resultados del testing
# 
# @param resultados: Resultados completos del testing
# =============================================================================
generar_reporte_testing <- function(resultados) {
  
  nombre_archivo <- paste0("REPORTE_TESTING_", 
                          tools::file_path_sans_ext(basename(resultados$archivo)), 
                          "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".md")
  
  cat("\n📄 Generando reporte:", nombre_archivo, "\n")
  
  # Contenido del reporte (se implementará en la siguiente función)
  # Por ahora, solo crear el archivo básico
  writeLines(c(
    paste("# Reporte de Testing:", basename(resultados$archivo)),
    paste("**Fecha:** ", resultados$timestamp),
    paste("**Estado:** ", resultados$estado_general),
    paste("**Porcentaje de éxito:** ", resultados$porcentaje_exito, "%"),
    "",
    "## Resumen de Tests",
    paste("- Tests ejecutados:", resultados$total_tests),
    paste("- Tests exitosos:", resultados$tests_exitosos_count),
    paste("- Tiempo total:", round(resultados$tiempo_total, 2), "segundos")
  ), nombre_archivo)
  
  cat("✅ Reporte generado exitosamente\n")
}

# =============================================================================
# EJECUCIÓN PRINCIPAL
# =============================================================================
# Si el script se ejecuta directamente, procesar argumentos de línea de comandos

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    # Buscar archivo .Rnw en el directorio actual
    archivos_rnw <- list.files(pattern = "\\.Rnw$", full.names = TRUE)
    
    if (length(archivos_rnw) == 0) {
      cat("❌ No se encontraron archivos .Rnw en el directorio actual\n")
      cat("💡 Uso: Rscript test_rnw_completo.R archivo.Rnw\n")
      quit(status = 1)
    } else if (length(archivos_rnw) == 1) {
      archivo_objetivo <- archivos_rnw[1]
      cat("🎯 Archivo detectado automáticamente:", archivo_objetivo, "\n")
    } else {
      cat("⚠️ Múltiples archivos .Rnw encontrados:\n")
      for (i in seq_along(archivos_rnw)) {
        cat("  ", i, ":", archivos_rnw[i], "\n")
      }
      archivo_objetivo <- archivos_rnw[1]
      cat("🎯 Usando el primero:", archivo_objetivo, "\n")
    }
  } else {
    archivo_objetivo <- args[1]
  }
  
  # Ejecutar la suite completa
  resultados <- ejecutar_test_suite_completa(archivo_objetivo)
  
  # Salir con código apropiado
  if (resultados$porcentaje_exito >= 80 && length(resultados$errores_criticos) == 0) {
    quit(status = 0)  # Éxito
  } else {
    quit(status = 1)  # Fallo
  }
}

# Operador auxiliar
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

cat("🧪 Script principal de testing para archivos .Rnw cargado correctamente\n")
cat("💡 Uso: ejecutar_test_suite_completa('archivo.Rnw')\n")
