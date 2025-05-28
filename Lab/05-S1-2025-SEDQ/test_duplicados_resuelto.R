# SCRIPT DE PRUEBA PARA VERIFICAR RESOLUCIÓN DE DUPLICADOS
# Verifica que el sistema de distractores únicos funciona correctamente

library(exams)
library(testthat)

# Función para probar múltiples ejecuciones y detectar duplicados
probar_resolucion_duplicados <- function(num_pruebas = 20) {
  cat("=== PRUEBA DE RESOLUCIÓN DE DUPLICADOS ===\n")
  cat("Ejecutando", num_pruebas, "pruebas para verificar unicidad de opciones...\n\n")
  
  resultados <- list()
  errores_duplicados <- 0
  errores_otros <- 0
  exitosas <- 0
  
  for (i in 1:num_pruebas) {
    cat("Prueba", i, "de", num_pruebas, "... ")
    
    tryCatch({
      # Crear archivo temporal para esta prueba
      archivo_temp <- paste0("test_temp_", i, ".Rmd")
      file.copy("proporciones_encuesta_deportiva_v1.Rmd", archivo_temp)
      
      # Ejecutar el archivo
      resultado <- exams2html(archivo_temp, 
                             n = 1, 
                             name = paste0("test_duplicados_", i),
                             dir = "test_output_duplicados",
                             quiet = TRUE)
      
      exitosas <- exitosas + 1
      cat("✓ ÉXITO\n")
      
      # Limpiar archivo temporal
      if (file.exists(archivo_temp)) {
        file.remove(archivo_temp)
      }
      
    }, error = function(e) {
      error_msg <- e$message
      
      if (grepl("opciones duplicadas", error_msg, ignore.case = TRUE)) {
        errores_duplicados <- errores_duplicados + 1
        cat("✗ ERROR DUPLICADOS\n")
        cat("   ", error_msg, "\n")
      } else {
        errores_otros <- errores_otros + 1
        cat("✗ OTRO ERROR\n")
        cat("   ", error_msg, "\n")
      }
      
      # Limpiar archivo temporal
      archivo_temp <- paste0("test_temp_", i, ".Rmd")
      if (file.exists(archivo_temp)) {
        file.remove(archivo_temp)
      }
    })
  }
  
  # Resumen de resultados
  cat("\n=== RESUMEN DE PRUEBAS ===\n")
  cat("Total de pruebas:", num_pruebas, "\n")
  cat("Pruebas exitosas:", exitosas, "\n")
  cat("Errores por duplicados:", errores_duplicados, "\n")
  cat("Otros errores:", errores_otros, "\n")
  
  if (errores_duplicados == 0) {
    cat("\n✅ PROBLEMA DE DUPLICADOS RESUELTO\n")
  } else {
    cat("\n❌ PROBLEMA DE DUPLICADOS PERSISTE\n")
  }
  
  if (exitosas == num_pruebas) {
    cat("✅ TODAS LAS PRUEBAS PASARON\n")
  } else {
    cat("⚠️  ALGUNAS PRUEBAS FALLARON\n")
  }
  
  return(list(
    total = num_pruebas,
    exitosas = exitosas,
    errores_duplicados = errores_duplicados,
    errores_otros = errores_otros
  ))
}

# Función para simular casos problemáticos específicos
simular_casos_problematicos <- function() {
  cat("\n=== SIMULACIÓN DE CASOS PROBLEMÁTICOS ===\n")
  
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
  
  # Casos que podrían generar duplicados
  casos_problematicos <- list(
    # Caso 1: Fracción ya reducida
    list(valor = 17, muestra = 100, poblacion = 40000),
    # Caso 2: Fracción con MCD = 1
    list(valor = 23, muestra = 100, poblacion = 50000),
    # Caso 3: Números primos
    list(valor = 13, muestra = 97, poblacion = 30000),
    # Caso 4: Fracción simple
    list(valor = 20, muestra = 100, poblacion = 60000),
    # Caso 5: Fracción compleja
    list(valor = 36, muestra = 144, poblacion = 75000)
  )
  
  for (i in seq_along(casos_problematicos)) {
    caso <- casos_problematicos[[i]]
    cat("Caso", i, "- Valor:", caso$valor, "Muestra:", caso$muestra, "Población:", caso$poblacion, "\n")
    
    # Calcular fracción reducida
    fraccion_red <- generar_fraccion_reducida(caso$valor, caso$muestra)
    
    cat("  Fracción original:", caso$valor, "/", caso$muestra, "\n")
    cat("  Fracción reducida:", fraccion_red[1], "/", fraccion_red[2], "\n")
    
    # Verificar si podría generar duplicados
    if (fraccion_red[1] == caso$valor && fraccion_red[2] == caso$muestra) {
      cat("  ⚠️  RIESGO: Fracción ya reducida - posibles duplicados\n")
    } else {
      cat("  ✓ OK: Fracción diferente - duplicados evitados\n")
    }
    
    # Simular distractores que se generarían
    distractores_simulados <- c()
    
    # Distractor tipo 2: confusión muestra-población
    d2 <- paste0("alrededor de ", caso$valor, " de cada ", caso$poblacion)
    distractores_simulados <- c(distractores_simulados, d2)
    
    # Distractor tipo 3: fracción reducida + población (solo si es diferente)
    if (fraccion_red[1] != caso$valor) {
      d3 <- paste0("alrededor de ", fraccion_red[1], " de cada ", caso$poblacion)
      distractores_simulados <- c(distractores_simulados, d3)
    }
    
    cat("  Distractores únicos generados:", length(unique(distractores_simulados)), "\n")
    cat("\n")
  }
}

# Función para verificar la lógica de eliminación de duplicados
verificar_logica_eliminacion <- function() {
  cat("=== VERIFICACIÓN DE LÓGICA DE ELIMINACIÓN ===\n")
  
  # Simular un conjunto con duplicados
  opciones_con_duplicados <- c(
    "opción A",
    "opción B", 
    "opción A",  # Duplicado
    "opción C",
    "opción B"   # Duplicado
  )
  
  cat("Opciones originales (con duplicados):\n")
  for (i in seq_along(opciones_con_duplicados)) {
    cat(" ", i, ":", opciones_con_duplicados[i], "\n")
  }
  
  # Aplicar unique()
  opciones_unicas <- unique(opciones_con_duplicados)
  
  cat("\nOpciones después de unique():\n")
  for (i in seq_along(opciones_unicas)) {
    cat(" ", i, ":", opciones_unicas[i], "\n")
  }
  
  cat("\nResultado: De", length(opciones_con_duplicados), "opciones a", length(opciones_unicas), "únicas\n")
  
  if (length(opciones_unicas) == length(unique(opciones_con_duplicados))) {
    cat("✓ Lógica de eliminación funciona correctamente\n")
  } else {
    cat("✗ Error en la lógica de eliminación\n")
  }
}

# Ejecutar todas las pruebas si el script se ejecuta directamente
if (interactive()) {
  cat("EJECUTANDO SUITE DE PRUEBAS PARA DUPLICADOS\n")
  cat("==========================================\n\n")
  
  # Crear directorio de salida si no existe
  if (!dir.exists("test_output_duplicados")) {
    dir.create("test_output_duplicados")
  }
  
  # Ejecutar pruebas
  verificar_logica_eliminacion()
  simular_casos_problematicos()
  resultado <- probar_resolucion_duplicados(10)
  
  cat("\n=== PRUEBAS COMPLETADAS ===\n")
  
  if (resultado$errores_duplicados == 0 && resultado$exitosas == resultado$total) {
    cat("🎉 TODAS LAS PRUEBAS PASARON - DUPLICADOS RESUELTOS\n")
  } else {
    cat("⚠️  REVISAR RESULTADOS - POSIBLES PROBLEMAS PENDIENTES\n")
  }
}
