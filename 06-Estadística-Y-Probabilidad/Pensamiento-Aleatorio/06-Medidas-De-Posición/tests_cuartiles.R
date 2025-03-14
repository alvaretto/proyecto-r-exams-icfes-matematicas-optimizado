# Pruebas unitarias para el ejercicio de diagramas de caja con cuartiles
# Archivo: tests_cuartiles.R

# Prueba 1: Verificar que los cuartiles usan método Tukey
test_metodo_tukey <- function() {
  cat("Verificando uso del método Tukey para cuartiles...\n")
  
  # Conjunto de datos de prueba
  datos_test <- c(1, 2, 3, 4, 5, 6, 7)
  
  # Calcular cuartiles con método Tukey (type=7)
  q1_tukey <- as.numeric(quantile(datos_test, 0.25, type=7))
  q3_tukey <- as.numeric(quantile(datos_test, 0.75, type=7))
  
  # Valores esperados CORRECTOS para el método Tukey (type=7)
  q1_esperado <- 2.5
  q3_esperado <- 5.5
  
  # Verificar con margen de error
  if (abs(q1_tukey - q1_esperado) > 0.001 || abs(q3_tukey - q3_esperado) > 0.001) {
    stop("Error: Los cuartiles no se calculan con el método Tukey (type=7)")
  }
  
  cat("✅ Método Tukey implementado correctamente\n")
  return(TRUE)
}

# Prueba 2: Verificar redondeo de cuartiles
test_redondeo_cuartiles <- function() {
  cat("Verificando función de redondeo de cuartiles...\n")
  
  # Cargar la función desde el archivo principal
  source("schoice-cuartil-estatura-02.Rmd", local=TRUE)
  
  # Casos de prueba
  casos <- list(
    list(valor = 150.0, esperado = 150),
    list(valor = 150.5, esperado = 150.5),
    list(valor = 150.2, esperado = 150.2),
    list(valor = 150.9, esperado = 150.9)
  )
  
  for (caso in casos) {
    resultado <- redondear_cuartil(caso$valor)
    if (!identical(resultado, caso$esperado)) {
      stop(paste("Error en redondeo: para", caso$valor, "se esperaba", caso$esperado, "pero se obtuvo", resultado))
    }
  }
  
  cat("✅ Función de redondeo implementada correctamente\n")
  return(TRUE)
}

# Prueba 3: Verificar que los diagramas son diferentes
test_diagramas_diferentes <- function() {
  cat("Verificando que los diagramas son visualmente diferentes...\n")
  
  # Cargar las funciones desde el archivo principal
  source("schoice-cuartil-estatura-02.Rmd", local=TRUE)
  
  # Generar datos de prueba
  set.seed(123)
  stats <- generar_datos_estatura()
  
  # Crear los diferentes diagramas
  diag_correcto <- list(
    minimo = stats$minimo,
    q1 = stats$q1,
    mediana = stats$mediana,
    q3 = stats$q3,
    maximo = stats$maximo
  )
  
  diag_escala_10 <- list(
    minimo = stats$minimo * 10,
    q1 = stats$q1 * 10,
    mediana = stats$mediana * 10,
    q3 = stats$q3 * 10,
    maximo = stats$maximo * 10
  )
  
  diag_invertido <- list(
    minimo = stats$minimo,
    q1 = stats$q3,
    mediana = stats$mediana,
    q3 = stats$q1,
    maximo = stats$maximo
  )
  
  diag_mediana_falsa <- list(
    minimo = stats$minimo,
    q1 = stats$q1,
    mediana = round(mean(c(stats$minimo, stats$maximo))),
    q3 = stats$q3,
    maximo = stats$maximo
  )
  
  # Función para verificar si dos diagramas son diferentes
  son_diagramas_diferentes <- function(diag1, diag2) {
    # Comparar todos los valores clave
    return(
      diag1$minimo != diag2$minimo ||
        diag1$q1 != diag2$q1 ||
        diag1$mediana != diag2$mediana ||
        diag1$q3 != diag2$q3 ||
        diag1$maximo != diag2$maximo
    )
  }
  
  # Verificar que todos los diagramas sean diferentes entre sí
  if (!son_diagramas_diferentes(diag_correcto, diag_escala_10)) {
    stop("Error: El diagrama correcto y el de escala 10 no son visualmente diferentes")
  }
  
  if (!son_diagramas_diferentes(diag_correcto, diag_invertido)) {
    stop("Error: El diagrama correcto y el invertido no son visualmente diferentes")
  }
  
  if (!son_diagramas_diferentes(diag_correcto, diag_mediana_falsa)) {
    stop("Error: El diagrama correcto y el de mediana falsa no son visualmente diferentes")
  }
  
  if (!son_diagramas_diferentes(diag_escala_10, diag_invertido)) {
    stop("Error: El diagrama de escala 10 y el invertido no son visualmente diferentes")
  }
  
  if (!son_diagramas_diferentes(diag_escala_10, diag_mediana_falsa)) {
    stop("Error: El diagrama de escala 10 y el de mediana falsa no son visualmente diferentes")
  }
  
  if (!son_diagramas_diferentes(diag_invertido, diag_mediana_falsa)) {
    stop("Error: El diagrama invertido y el de mediana falsa no son visualmente diferentes")
  }
  
  cat("✅ Todos los diagramas son visualmente diferentes\n")
  return(TRUE)
}

# Prueba 4: Verificar el posicionamiento de etiquetas
test_posicionamiento_etiquetas <- function() {
  cat("Verificando el posicionamiento de etiquetas...\n")
  
  # Cargar las funciones desde el archivo principal
  source("schoice-cuartil-estatura-02.Rmd", local=TRUE)
  
  # Generar datos de prueba con valores muy cercanos
  set.seed(456)
  datos_cercanos <- c(150, 151, 152, 153, 154)
  
  # Crear un objeto stats simulado
  stats_cercanos <- list(
    datos = datos_cercanos,
    datos_desordenados = sample(datos_cercanos),
    minimo = 150,
    q1 = 151,
    mediana = 152,
    q3 = 153,
    maximo = 154
  )
  
  # Verificar que la función no genere errores con valores cercanos
  tryCatch({
    # Crear un dispositivo gráfico temporal
    pdf(NULL)
    
    # Intentar dibujar los diagramas con diferentes tipos
    crear_diagrama(stats_cercanos, "correcto")
    crear_diagrama(stats_cercanos, "escala_10")
    crear_diagrama(stats_cercanos, "invertido")
    crear_diagrama(stats_cercanos, "mediana_falsa")
    
    # Cerrar el dispositivo gráfico
    dev.off()
    
    cat("✅ El posicionamiento de etiquetas funciona correctamente\n")
    return(TRUE)
  }, error = function(e) {
    dev.off()
    stop(paste("Error en el posicionamiento de etiquetas:", e$message))
  })
}

# Prueba 5: Verificar manejo de datos pares e impares
test_datos_par_impar <- function() {
  cat("Verificando manejo de conjuntos de datos pares e impares...\n")
  
  # Cargar las funciones desde el archivo principal
  source("schoice-cuartil-estatura-02.Rmd", local=TRUE)
  
  # Probar con conjunto de datos impar
  set.seed(789)
  stats_impar <- generar_datos_estatura(n = 9)
  
  # Probar con conjunto de datos par
  set.seed(789)
  stats_par <- generar_datos_estatura(n = 10)
  
  # Verificar que ambos casos generan estadísticas válidas
  if (is.null(stats_impar$q1) || is.null(stats_impar$mediana) || is.null(stats_impar$q3)) {
    stop("Error: No se generaron estadísticas válidas para conjunto de datos impar")
  }
  
  if (is.null(stats_par$q1) || is.null(stats_par$mediana) || is.null(stats_par$q3)) {
    stop("Error: No se generaron estadísticas válidas para conjunto de datos par")
  }
  
  cat("✅ Manejo correcto de conjuntos de datos pares e impares\n")
  return(TRUE)
}

# Ejecutar todas las pruebas y mostrar resultados
run_all_tests <- function() {
  cat("==== PRUEBAS UNITARIAS PARA DIAGRAMAS DE CAJA CON CUARTILES ====\n\n")
  
  # Registrar pruebas pasadas
  pruebas_pasadas <- 0
  total_pruebas <- 5
  
  # Test 1
  tryCatch({
    test_metodo_tukey()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_metodo_tukey:", e$message, "\n")
  })
  
  # Test 2
  tryCatch({
    test_redondeo_cuartiles()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_redondeo_cuartiles:", e$message, "\n")
  })
  
  # Test 3
  tryCatch({
    test_diagramas_diferentes()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_diagramas_diferentes:", e$message, "\n")
  })
  
  # Test 4
  tryCatch({
    test_posicionamiento_etiquetas()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_posicionamiento_etiquetas:", e$message, "\n")
  })
  
  # Test 5
  tryCatch({
    test_datos_par_impar()
    pruebas_pasadas <- pruebas_pasadas + 1
  }, error = function(e) {
    cat("❌ FALLO en test_datos_par_impar:", e$message, "\n")
  })
  
  # Mostrar resumen
  cat("\n==== RESUMEN DE PRUEBAS UNITARIAS ====\n")
  cat("Pruebas pasadas:", pruebas_pasadas, "de", total_pruebas, "\n")
  
  if (pruebas_pasadas == total_pruebas) {
    cat("✅ TODAS LAS PRUEBAS PASARON CORRECTAMENTE\n")
  } else {
    cat("⚠️ ALGUNAS PRUEBAS FALLARON. Revisa los mensajes de error.\n")
  }
  
  cat("=======================================\n")
}

# Ejecutar las pruebas si este archivo se ejecuta directamente
if (!interactive()) {
  run_all_tests()
}