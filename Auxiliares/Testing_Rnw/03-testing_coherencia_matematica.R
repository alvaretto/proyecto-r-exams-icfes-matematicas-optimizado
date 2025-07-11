#!/usr/bin/env Rscript
# =============================================================================
# VALIDACIÓN DE COHERENCIA MATEMÁTICA EN ARCHIVOS .RNW
# =============================================================================
# Este archivo contiene funciones para validar la coherencia matemática
# de los ejercicios generados en archivos .Rnw del proyecto ICFES
#
# Autor: Sistema de Testing ICFES
# Fecha: 2025-01-11
# =============================================================================

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(testthat)
  library(stringr)
})

# =============================================================================
# FUNCIÓN: extraer_variables_matematicas
# =============================================================================
# Extrae variables matemáticas clave de un entorno de ejecución
# 
# @param entorno: Entorno donde se ejecutó el código R
# @return: Lista con variables matemáticas identificadas
# =============================================================================
extraer_variables_matematicas <- function(entorno) {
  
  variables <- ls(envir = entorno)
  variables_matematicas <- list()
  
  # Categorías de variables a buscar
  patrones_variables <- list(
    porcentajes = c("porcentaje", "porcent", "percent"),
    consumos = c("consumo", "gasto", "uso"),
    precios = c("precio", "costo", "valor", "tarifa"),
    cantidades = c("cantidad", "numero", "total", "suma"),
    opciones = c("opcion", "respuesta", "alternativa"),
    distractores = c("distractor", "incorrecto"),
    correctas = c("correcto", "solucion", "answer")
  )
  
  for (categoria in names(patrones_variables)) {
    variables_categoria <- c()
    
    for (patron in patrones_variables[[categoria]]) {
      vars_encontradas <- variables[grepl(patron, variables, ignore.case = TRUE)]
      variables_categoria <- c(variables_categoria, vars_encontradas)
    }
    
    # Obtener valores de las variables
    valores <- list()
    for (var in unique(variables_categoria)) {
      tryCatch({
        valor <- get(var, envir = entorno)
        if (is.numeric(valor) || is.logical(valor) || is.character(valor)) {
          valores[[var]] <- valor
        }
      }, error = function(e) {
        # Ignorar variables que no se pueden obtener
      })
    }
    
    variables_matematicas[[categoria]] <- valores
  }
  
  return(variables_matematicas)
}

# =============================================================================
# FUNCIÓN: validar_rangos_numericos
# =============================================================================
# Valida que los valores numéricos estén en rangos razonables
# 
# @param variables: Lista de variables extraídas
# @return: Lista con resultados de validación
# =============================================================================
validar_rangos_numericos <- function(variables) {
  
  validacion <- list(
    porcentajes_validos = TRUE,
    precios_validos = TRUE,
    cantidades_validas = TRUE,
    errores = c(),
    warnings = c()
  )
  
  # Validar porcentajes (0-100)
  if (length(variables$porcentajes) > 0) {
    for (var_name in names(variables$porcentajes)) {
      valor <- variables$porcentajes[[var_name]]
      if (is.numeric(valor)) {
        if (any(valor < 0 | valor > 100)) {
          validacion$porcentajes_validos <- FALSE
          validacion$errores <- c(validacion$errores, 
                                 paste("Porcentaje fuera de rango [0,100]:", var_name, "=", paste(valor, collapse=", ")))
        }
      }
    }
  }
  
  # Validar precios (positivos y razonables)
  if (length(variables$precios) > 0) {
    for (var_name in names(variables$precios)) {
      valor <- variables$precios[[var_name]]
      if (is.numeric(valor)) {
        if (any(valor < 0)) {
          validacion$precios_validos <- FALSE
          validacion$errores <- c(validacion$errores, 
                                 paste("Precio negativo:", var_name, "=", paste(valor, collapse=", ")))
        }
        if (any(valor > 1000000)) {  # Precios muy altos
          validacion$warnings <- c(validacion$warnings, 
                                  paste("Precio muy alto:", var_name, "=", paste(valor, collapse=", ")))
        }
      }
    }
  }
  
  # Validar cantidades (no negativas)
  if (length(variables$cantidades) > 0) {
    for (var_name in names(variables$cantidades)) {
      valor <- variables$cantidades[[var_name]]
      if (is.numeric(valor)) {
        if (any(valor < 0)) {
          validacion$cantidades_validas <- FALSE
          validacion$errores <- c(validacion$errores, 
                                 paste("Cantidad negativa:", var_name, "=", paste(valor, collapse=", ")))
        }
      }
    }
  }
  
  return(validacion)
}

# =============================================================================
# FUNCIÓN: validar_coherencia_opciones
# =============================================================================
# Valida la coherencia entre respuestas correctas y distractores
# 
# @param variables: Lista de variables extraídas
# @return: Lista con resultados de validación
# =============================================================================
validar_coherencia_opciones <- function(variables) {
  
  validacion <- list(
    opciones_unicas = TRUE,
    respuesta_en_opciones = TRUE,
    distractores_diferentes = TRUE,
    errores = c(),
    warnings = c()
  )
  
  # Buscar variables de opciones
  todas_variables <- c(variables$opciones, variables$correctas, variables$distractores)
  
  # Buscar específicamente variables comunes
  vars_opciones <- c()
  vars_correcta <- c()
  vars_distractores <- c()
  
  for (var_name in names(todas_variables)) {
    if (grepl("opcion|alternativa", var_name, ignore.case = TRUE)) {
      vars_opciones <- c(vars_opciones, var_name)
    }
    if (grepl("correcto|solucion", var_name, ignore.case = TRUE)) {
      vars_correcta <- c(vars_correcta, var_name)
    }
    if (grepl("distractor", var_name, ignore.case = TRUE)) {
      vars_distractores <- c(vars_distractores, var_name)
    }
  }
  
  # Validar unicidad de opciones
  if (length(vars_opciones) > 0) {
    for (var_name in vars_opciones) {
      opciones <- todas_variables[[var_name]]
      if (is.vector(opciones) && length(opciones) > 1) {
        if (length(unique(opciones)) != length(opciones)) {
          validacion$opciones_unicas <- FALSE
          validacion$errores <- c(validacion$errores, 
                                 paste("Opciones duplicadas en:", var_name))
        }
      }
    }
  }
  
  # Validar que la respuesta correcta esté en las opciones
  if (length(vars_correcta) > 0 && length(vars_opciones) > 0) {
    respuesta_correcta <- todas_variables[[vars_correcta[1]]]
    opciones <- todas_variables[[vars_opciones[1]]]
    
    if (is.numeric(respuesta_correcta) && is.numeric(opciones)) {
      if (!any(abs(opciones - respuesta_correcta) < 1e-10)) {
        validacion$respuesta_en_opciones <- FALSE
        validacion$errores <- c(validacion$errores, 
                               "La respuesta correcta no está en las opciones")
      }
    }
  }
  
  return(validacion)
}

# =============================================================================
# FUNCIÓN: validar_calculos_matematicos
# =============================================================================
# Valida cálculos matemáticos específicos del ejercicio
# 
# @param entorno: Entorno de ejecución
# @return: Lista con resultados de validación
# =============================================================================
validar_calculos_matematicos <- function(entorno) {
  
  validacion <- list(
    calculos_coherentes = TRUE,
    errores = c(),
    warnings = c()
  )
  
  variables <- ls(envir = entorno)
  
  # Buscar patrones de cálculo comunes
  
  # 1. Validar cálculos de porcentajes
  vars_porcentaje <- variables[grepl("porcentaje", variables, ignore.case = TRUE)]
  vars_consumo <- variables[grepl("consumo", variables, ignore.case = TRUE)]
  vars_maximo <- variables[grepl("maximo", variables, ignore.case = TRUE)]
  
  if (length(vars_porcentaje) > 0 && length(vars_consumo) > 0 && length(vars_maximo) > 0) {
    tryCatch({
      porcentaje <- get(vars_porcentaje[1], envir = entorno)
      consumo <- get(vars_consumo[1], envir = entorno)
      maximo <- get(vars_maximo[1], envir = entorno)
      
      if (is.numeric(porcentaje) && is.numeric(consumo) && is.numeric(maximo)) {
        # Verificar que porcentaje = (consumo/maximo) * 100
        porcentaje_calculado <- round((consumo / maximo) * 100)
        if (abs(porcentaje - porcentaje_calculado) > 1) {
          validacion$calculos_coherentes <- FALSE
          validacion$errores <- c(validacion$errores, 
                                 paste("Cálculo de porcentaje incoherente:", 
                                       porcentaje, "vs", porcentaje_calculado))
        }
      }
    }, error = function(e) {
      validacion$warnings <- c(validacion$warnings, 
                              "No se pudo validar cálculo de porcentaje")
    })
  }
  
  # 2. Validar cálculos económicos
  vars_total <- variables[grepl("total", variables, ignore.case = TRUE)]
  vars_fijo <- variables[grepl("fijo|cargo", variables, ignore.case = TRUE)]
  vars_variable <- variables[grepl("variable", variables, ignore.case = TRUE)]
  
  if (length(vars_total) > 0 && length(vars_fijo) > 0 && length(vars_variable) > 0) {
    tryCatch({
      total <- get(vars_total[1], envir = entorno)
      fijo <- get(vars_fijo[1], envir = entorno)
      variable <- get(vars_variable[1], envir = entorno)
      
      if (is.numeric(total) && is.numeric(fijo) && is.numeric(variable)) {
        total_calculado <- fijo + variable
        if (abs(total - total_calculado) > 1) {
          validacion$calculos_coherentes <- FALSE
          validacion$errores <- c(validacion$errores, 
                                 paste("Cálculo de total incoherente:", 
                                       total, "vs", total_calculado))
        }
      }
    }, error = function(e) {
      validacion$warnings <- c(validacion$warnings, 
                              "No se pudo validar cálculo económico")
    })
  }
  
  return(validacion)
}

# =============================================================================
# FUNCIÓN: validar_coherencia_completa
# =============================================================================
# Ejecuta todas las validaciones de coherencia matemática
# 
# @param entorno: Entorno donde se ejecutó el código R
# @return: Lista con resultados completos de validación
# =============================================================================
validar_coherencia_completa <- function(entorno) {
  
  cat("🔢 Validando coherencia matemática...\n")
  
  # Extraer variables
  variables <- extraer_variables_matematicas(entorno)
  
  # Ejecutar validaciones
  validacion_rangos <- validar_rangos_numericos(variables)
  validacion_opciones <- validar_coherencia_opciones(variables)
  validacion_calculos <- validar_calculos_matematicos(entorno)
  
  # Consolidar resultados
  resultado_final <- list(
    variables_extraidas = variables,
    validacion_rangos = validacion_rangos,
    validacion_opciones = validacion_opciones,
    validacion_calculos = validacion_calculos,
    coherencia_general = TRUE,
    errores_totales = c(),
    warnings_totales = c()
  )
  
  # Consolidar errores y warnings
  resultado_final$errores_totales <- c(
    validacion_rangos$errores,
    validacion_opciones$errores,
    validacion_calculos$errores
  )
  
  resultado_final$warnings_totales <- c(
    validacion_rangos$warnings,
    validacion_opciones$warnings,
    validacion_calculos$warnings
  )
  
  # Determinar coherencia general
  resultado_final$coherencia_general <- (
    validacion_rangos$porcentajes_validos &&
    validacion_rangos$precios_validos &&
    validacion_rangos$cantidades_validas &&
    validacion_opciones$opciones_unicas &&
    validacion_opciones$respuesta_en_opciones &&
    validacion_calculos$calculos_coherentes
  )
  
  # Mostrar resumen
  if (resultado_final$coherencia_general) {
    cat("✅ Coherencia matemática validada\n")
  } else {
    cat("❌ Problemas de coherencia detectados:\n")
    for (error in resultado_final$errores_totales) {
      cat("  -", error, "\n")
    }
  }
  
  if (length(resultado_final$warnings_totales) > 0) {
    cat("⚠️ Advertencias:\n")
    for (warning in resultado_final$warnings_totales) {
      cat("  -", warning, "\n")
    }
  }
  
  return(resultado_final)
}

# =============================================================================
# FUNCIÓN: crear_test_coherencia_rnw
# =============================================================================
# Crea tests de coherencia matemática para archivos .Rnw
# 
# @param entorno: Entorno de ejecución del código R
# @return: Función que ejecuta los tests de coherencia
# =============================================================================
crear_test_coherencia_rnw <- function(entorno) {
  
  function() {
    
    cat("\n🔢 TESTING DE COHERENCIA MATEMÁTICA\n")
    cat("=" %R% 50, "\n")
    
    # Ejecutar validación completa
    validacion <- validar_coherencia_completa(entorno)
    
    # Test 1: Rangos numéricos válidos
    test_that("Los rangos numéricos son válidos", {
      expect_true(validacion$validacion_rangos$porcentajes_validos,
                  info = "Los porcentajes deben estar entre 0 y 100")
      expect_true(validacion$validacion_rangos$precios_validos,
                  info = "Los precios deben ser positivos")
      expect_true(validacion$validacion_rangos$cantidades_validas,
                  info = "Las cantidades deben ser no negativas")
    })
    
    # Test 2: Coherencia de opciones
    test_that("Las opciones son coherentes", {
      expect_true(validacion$validacion_opciones$opciones_unicas,
                  info = "Las opciones deben ser únicas")
      expect_true(validacion$validacion_opciones$respuesta_en_opciones,
                  info = "La respuesta correcta debe estar en las opciones")
    })
    
    # Test 3: Cálculos matemáticos
    test_that("Los cálculos matemáticos son coherentes", {
      expect_true(validacion$validacion_calculos$calculos_coherentes,
                  info = "Los cálculos matemáticos deben ser coherentes")
    })
    
    # Test 4: Coherencia general
    test_that("La coherencia general es satisfactoria", {
      expect_true(validacion$coherencia_general,
                  info = paste("Errores encontrados:", 
                              paste(validacion$errores_totales, collapse = "; ")))
      expect_lt(length(validacion$errores_totales), 3,
                info = "No debe haber más de 2 errores de coherencia")
    })
    
    cat("✅ Tests de coherencia matemática completados\n")
  }
}

# Operador auxiliar
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

cat("🔢 Utilidades para validación de coherencia matemática cargadas correctamente\n")
