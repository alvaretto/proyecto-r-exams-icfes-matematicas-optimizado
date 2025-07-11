#!/usr/bin/env Rscript
# =============================================================================
# SCRIPT SIMPLE DE TESTING PARA ARCHIVOS .RNW
# =============================================================================
# Versión simplificada que se enfoca en la validación básica sin compilación
# Ideal para testing rápido durante el desarrollo
#
# Uso: Rscript test_rnw_simple.R archivo.Rnw
#
# Autor: Sistema de Testing ICFES
# Fecha: 2025-01-11
# =============================================================================

# Cargar solo las utilidades básicas
source("01-testing_rnw_utils.R")

# =============================================================================
# FUNCIÓN: test_basico_rnw
# =============================================================================
# Ejecuta tests básicos sin compilación
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# @return: Lista con resultados básicos
# =============================================================================
test_basico_rnw <- function(archivo_rnw) {
  
  cat("\n🧪 TESTING BÁSICO PARA:", basename(archivo_rnw), "\n")
  cat("=" %R% 60, "\n")
  
  resultados <- list(
    archivo = archivo_rnw,
    timestamp = Sys.time(),
    tests_exitosos = 0,
    total_tests = 3
  )
  
  # Test 1: Estructura
  cat("📋 Test 1: Validando estructura...\n")
  tryCatch({
    validacion <- validar_estructura_rnw(archivo_rnw)
    if (validacion$puntuacion_validez >= 0.8) {
      cat("  ✅ Estructura válida (", round(validacion$puntuacion_validez * 100, 1), "%)\n")
      resultados$tests_exitosos <- resultados$tests_exitosos + 1
    } else {
      cat("  ❌ Problemas de estructura\n")
      for (error in validacion$errores) {
        cat("    -", error, "\n")
      }
    }
    resultados$validacion_estructura <- validacion
  }, error = function(e) {
    cat("  ❌ Error:", e$message, "\n")
  })
  
  # Test 2: Extracción de código
  cat("📝 Test 2: Extrayendo código R...\n")
  tryCatch({
    codigo_extraido <- extraer_codigo_r_desde_rnw(archivo_rnw)
    cat("  📊 Chunks R encontrados:", codigo_extraido$total_chunks_r, "\n")
    cat("  🐍 Chunks Python encontrados:", codigo_extraido$total_chunks_python, "\n")
    
    if (codigo_extraido$total_chunks_r > 0) {
      cat("  ✅ Extracción exitosa\n")
      resultados$tests_exitosos <- resultados$tests_exitosos + 1
    } else {
      cat("  ⚠️ No se encontraron chunks de R\n")
    }
    resultados$codigo_extraido <- codigo_extraido
  }, error = function(e) {
    cat("  ❌ Error:", e$message, "\n")
  })
  
  # Test 3: Ejecución de código R
  if (exists("codigo_extraido") && codigo_extraido$total_chunks_r > 0) {
    cat("⚡ Test 3: Ejecutando código R...\n")
    tryCatch({
      resultado_ejecucion <- ejecutar_codigo_r_extraido(codigo_extraido)
      
      if (resultado_ejecucion$exito) {
        cat("  ✅ Código ejecutado exitosamente (", round(resultado_ejecucion$tiempo_ejecucion, 2), "s)\n")
        cat("  📊 Variables creadas:", length(resultado_ejecucion$variables_creadas), "\n")
        resultados$tests_exitosos <- resultados$tests_exitosos + 1
        
        # Mostrar algunas variables importantes
        env <- resultado_ejecucion$entorno
        vars_importantes <- c()
        
        for (var in ls(envir = env)) {
          if (grepl("porcentaje|correcto|opcion", var, ignore.case = TRUE)) {
            valor <- get(var, envir = env)
            if (is.numeric(valor) && length(valor) <= 5) {
              vars_importantes <- c(vars_importantes, paste(var, "=", paste(valor, collapse = ", ")))
            }
          }
        }
        
        if (length(vars_importantes) > 0) {
          cat("  🔍 Variables clave:\n")
          for (var_info in head(vars_importantes, 5)) {
            cat("    -", var_info, "\n")
          }
        }
        
      } else {
        cat("  ❌ Error en ejecución:", resultado_ejecucion$error, "\n")
      }
      resultados$ejecucion_r <- resultado_ejecucion
    }, error = function(e) {
      cat("  ❌ Error:", e$message, "\n")
    })
  } else {
    cat("⚠️ Test 3: Saltado (no hay código R para ejecutar)\n")
  }
  
  # Resumen
  porcentaje_exito <- round((resultados$tests_exitosos / resultados$total_tests) * 100, 1)
  cat("\n📊 RESUMEN:\n")
  cat("  ✅ Tests exitosos:", resultados$tests_exitosos, "/", resultados$total_tests, "\n")
  cat("  📈 Porcentaje de éxito:", porcentaje_exito, "%\n")
  
  estado <- if (porcentaje_exito >= 80) {
    "✅ APROBADO"
  } else if (porcentaje_exito >= 60) {
    "⚠️ APROBADO CON OBSERVACIONES"
  } else {
    "❌ REPROBADO"
  }
  
  cat("  🎯 Estado:", estado, "\n")
  
  resultados$porcentaje_exito <- porcentaje_exito
  resultados$estado <- estado
  
  cat("=" %R% 60, "\n")
  
  return(resultados)
}

# =============================================================================
# FUNCIÓN: ejecutar_tests_con_testthat
# =============================================================================
# Ejecuta tests usando el framework testthat
# 
# @param archivo_rnw: Ruta al archivo .Rnw
# =============================================================================
ejecutar_tests_con_testthat <- function(archivo_rnw) {
  
  cat("\n🧪 EJECUTANDO TESTS CON TESTTHAT\n")
  cat("=" %R% 50, "\n")
  
  # Crear suite de tests
  test_suite <- crear_test_suite_rnw(archivo_rnw, paste("Tests para", basename(archivo_rnw)))
  
  # Ejecutar la suite
  test_suite()
}

# =============================================================================
# EJECUCIÓN PRINCIPAL
# =============================================================================

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    # Buscar archivo .Rnw en el directorio actual
    archivos_rnw <- list.files(pattern = "\\.Rnw$", full.names = TRUE)
    
    if (length(archivos_rnw) == 0) {
      cat("❌ No se encontraron archivos .Rnw en el directorio actual\n")
      cat("💡 Uso: Rscript test_rnw_simple.R archivo.Rnw\n")
      quit(status = 1)
    } else if (length(archivos_rnw) == 1) {
      archivo_objetivo <- archivos_rnw[1]
      cat("🎯 Archivo detectado automáticamente:", archivo_objetivo, "\n")
    } else {
      archivo_objetivo <- archivos_rnw[1]
      cat("🎯 Usando el primer archivo encontrado:", archivo_objetivo, "\n")
    }
  } else {
    archivo_objetivo <- args[1]
  }
  
  # Verificar que el archivo existe
  if (!file.exists(archivo_objetivo)) {
    cat("❌ El archivo no existe:", archivo_objetivo, "\n")
    quit(status = 1)
  }
  
  # Ejecutar tests básicos
  resultados <- test_basico_rnw(archivo_objetivo)
  
  # Ejecutar tests con testthat si el archivo es válido
  if (resultados$porcentaje_exito >= 60) {
    cat("\n🔄 Ejecutando tests adicionales con testthat...\n")
    tryCatch({
      ejecutar_tests_con_testthat(archivo_objetivo)
    }, error = function(e) {
      cat("⚠️ Error en tests testthat:", e$message, "\n")
    })
  }
  
  # Salir con código apropiado
  if (resultados$porcentaje_exito >= 80) {
    quit(status = 0)  # Éxito
  } else {
    quit(status = 1)  # Fallo
  }
}

# Operador auxiliar
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

cat("🧪 Script simple de testing para archivos .Rnw cargado correctamente\n")
cat("💡 Uso: test_basico_rnw('archivo.Rnw')\n")
