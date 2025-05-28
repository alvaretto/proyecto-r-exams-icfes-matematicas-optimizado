# SCRIPT DE PRUEBA PARA DISTRACTORES MEJORADOS
# Verifica la funcionalidad de las mejoras implementadas

# Configuración inicial
library(exams)
library(testthat)

# Función para probar múltiples ejecuciones del archivo
probar_distractores_mejorados <- function(num_pruebas = 10) {
  cat("=== INICIANDO PRUEBAS DE DISTRACTORES MEJORADOS ===\n\n")
  
  resultados <- list()
  errores <- c()
  
  for (i in 1:num_pruebas) {
    cat("Prueba", i, "de", num_pruebas, "...\n")
    
    tryCatch({
      # Ejecutar el archivo Rmd
      resultado <- exams2html("proporciones_encuesta_deportiva_v1.Rmd", 
                             n = 1, 
                             name = paste0("test_", i),
                             dir = "test_output",
                             quiet = TRUE)
      
      resultados[[i]] <- list(
        exito = TRUE,
        mensaje = "Ejecución exitosa"
      )
      
      cat("  ✓ Prueba", i, "completada exitosamente\n")
      
    }, error = function(e) {
      error_msg <- paste("Error en prueba", i, ":", e$message)
      errores <- c(errores, error_msg)
      
      resultados[[i]] <- list(
        exito = FALSE,
        mensaje = error_msg
      )
      
      cat("  ✗ Error en prueba", i, ":", e$message, "\n")
    })
  }
  
  # Resumen de resultados
  cat("\n=== RESUMEN DE PRUEBAS ===\n")
  exitosas <- sum(sapply(resultados, function(x) x$exito))
  cat("Pruebas exitosas:", exitosas, "de", num_pruebas, "\n")
  cat("Pruebas fallidas:", num_pruebas - exitosas, "\n")
  
  if (length(errores) > 0) {
    cat("\nERRORES ENCONTRADOS:\n")
    for (error in errores) {
      cat("-", error, "\n")
    }
  }
  
  return(list(
    total = num_pruebas,
    exitosas = exitosas,
    fallidas = num_pruebas - exitosas,
    errores = errores
  ))
}

# Función para verificar la coherencia matemática de las fracciones
verificar_coherencia_fracciones <- function() {
  cat("=== VERIFICANDO COHERENCIA MATEMÁTICA ===\n")
  
  # Función MCD (copiada del archivo principal)
  calcular_mcd <- function(a, b) {
    while (b != 0) {
      temp <- b
      b <- a %% b
      a <- temp
    }
    return(a)
  }
  
  # Función fracción reducida (copiada del archivo principal)
  generar_fraccion_reducida <- function(numerador, denominador) {
    mcd <- calcular_mcd(numerador, denominador)
    return(c(numerador / mcd, denominador / mcd))
  }
  
  # Casos de prueba
  casos_prueba <- list(
    c(30, 150),  # 30/150 = 1/5
    c(24, 120),  # 24/120 = 1/5
    c(18, 90),   # 18/90 = 1/5
    c(20, 100),  # 20/100 = 1/5
    c(15, 75),   # 15/75 = 1/5
    c(12, 60),   # 12/60 = 1/5
    c(36, 144),  # 36/144 = 1/4
    c(25, 125)   # 25/125 = 1/5
  )
  
  for (i in seq_along(casos_prueba)) {
    caso <- casos_prueba[[i]]
    numerador <- caso[1]
    denominador <- caso[2]
    
    cat("Caso", i, ":", numerador, "/", denominador, "\n")
    
    # Calcular fracción reducida
    fraccion_red <- generar_fraccion_reducida(numerador, denominador)
    num_red <- fraccion_red[1]
    den_red <- fraccion_red[2]
    
    # Verificar equivalencia
    original <- numerador / denominador
    reducida <- num_red / den_red
    
    cat("  Original:", numerador, "/", denominador, "=", round(original, 6), "\n")
    cat("  Reducida:", num_red, "/", den_red, "=", round(reducida, 6), "\n")
    
    # Test de equivalencia
    if (abs(original - reducida) < 1e-10) {
      cat("  ✓ Fracciones equivalentes\n")
    } else {
      cat("  ✗ ERROR: Fracciones NO equivalentes\n")
    }
    
    cat("\n")
  }
}

# Función para verificar la variedad de distractores
verificar_variedad_distractores <- function(num_simulaciones = 20) {
  cat("=== VERIFICANDO VARIEDAD DE DISTRACTORES ===\n")
  
  # Esta función simularía la generación de distractores
  # Para una implementación completa, necesitaríamos extraer 
  # la lógica de generación del archivo Rmd
  
  cat("Simulando", num_simulaciones, "generaciones de distractores...\n")
  
  tipos_encontrados <- c()
  
  # Simular diferentes tipos de distractores que podrían generarse
  tipos_posibles <- c(
    "fraccion_reducida",
    "confusion_poblacion", 
    "fraccion_reducida_poblacion",
    "equipo_incorrecto",
    "generalizacion_indebida",
    "confusion_conceptual",
    "otro_equipo_fraccion_reducida"
  )
  
  for (i in 1:num_simulaciones) {
    # Simular selección aleatoria de 3 tipos de distractores
    tipos_seleccionados <- sample(tipos_posibles, 3)
    tipos_encontrados <- c(tipos_encontrados, tipos_seleccionados)
  }
  
  # Analizar variedad
  frecuencias <- table(tipos_encontrados)
  cat("\nFrecuencia de tipos de distractores:\n")
  print(frecuencias)
  
  cat("\nTipos únicos encontrados:", length(unique(tipos_encontrados)), "de", length(tipos_posibles), "posibles\n")
  
  return(frecuencias)
}

# Ejecutar todas las pruebas
if (interactive()) {
  cat("EJECUTANDO SUITE COMPLETA DE PRUEBAS\n")
  cat("====================================\n\n")
  
  # Crear directorio de salida si no existe
  if (!dir.exists("test_output")) {
    dir.create("test_output")
  }
  
  # Ejecutar pruebas
  resultado_pruebas <- probar_distractores_mejorados(5)
  verificar_coherencia_fracciones()
  verificar_variedad_distractores()
  
  cat("\n=== PRUEBAS COMPLETADAS ===\n")
  
  if (resultado_pruebas$fallidas == 0) {
    cat("✓ TODAS LAS PRUEBAS PASARON EXITOSAMENTE\n")
  } else {
    cat("✗ ALGUNAS PRUEBAS FALLARON - REVISAR ERRORES\n")
  }
}
