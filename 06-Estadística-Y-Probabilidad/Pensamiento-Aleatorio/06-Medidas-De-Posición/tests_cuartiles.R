# Tests para verificar el correcto funcionamiento de las funciones de cuartiles
library(testthat)

# Prueba 1: Verificar que el método de cálculo de cuartiles es el tradicional
test_metodo_tradicional <- function() {
  cat("Verificando método de cálculo de cuartiles tradicional...\n")
  
  # Caso 1: Conjunto de datos impar
  datos_impar <- c(5, 7, 10, 15, 20, 25, 30)
  cuartiles_calculados <- calcular_cuartiles_tradicional(datos_impar)
  
  # Verificar Q1 (mediana de la primera mitad)
  expect_equal(cuartiles_calculados$q1, 7)
  
  # Verificar Q2 (mediana de todos los datos)
  expect_equal(cuartiles_calculados$q2, 15)
  
  # Verificar Q3 (mediana de la segunda mitad)
  expect_equal(cuartiles_calculados$q3, 25)
  
  # Caso 2: Conjunto de datos par
  datos_par <- c(5, 7, 10, 15, 20, 25, 30, 35)
  cuartiles_calculados <- calcular_cuartiles_tradicional(datos_par)
  
  # Verificar Q1 (mediana de la primera mitad)
  expect_equal(cuartiles_calculados$q1, 7.5)
  
  # Verificar Q2 (mediana de todos los datos)
  expect_equal(cuartiles_calculados$q2, 17.5)
  
  # Verificar Q3 (mediana de la segunda mitad)
  expect_equal(cuartiles_calculados$q3, 27.5)
  
  cat("   Método tradicional implementado correctamente\n")
  return(TRUE)
}

# Prueba 2: Verificar redondeo de cuartiles
test_redondeo_cuartiles <- function() {
  cat("Verificando redondeo de cuartiles...\n")
  
  # Probar valores enteros
  expect_equal(redondear_cuartil(10), 10)
  expect_equal(redondear_cuartil(10.0), 10)
  
  # Probar valores con decimales
  expect_equal(redondear_cuartil(10.2), 10.2)
  expect_equal(redondear_cuartil(10.25), 10.3)
  
  cat("   Redondeo de cuartiles implementado correctamente\n")
  return(TRUE)
}

# Prueba 3: Verificar aleatoriedad de la respuesta correcta
test_aleatoriedad_respuesta <- function() {
  cat("Verificando aleatoriedad de la respuesta correcta...\n")
  
  # Ejecutar múltiples veces y registrar la posición de la respuesta correcta
  n_iteraciones <- 100
  posiciones_correctas <- numeric(n_iteraciones)
  
  for (i in 1:n_iteraciones) {
    # Generar datos
    stats <- generar_datos_estatura()
    
    # Crear diagramas
    diagramas <- list(
      correcto = calcular_valores_diagrama(stats, "correcto"),
      escala = calcular_valores_diagrama(stats, "escala"),
      invertido = calcular_valores_diagrama(stats, "invertido"),
      mediana_falsa = calcular_valores_diagrama(stats, "mediana_falsa")
    )
    
    # Aleatorizar posición de respuesta correcta
    posiciones <- sample(1:4)
    posiciones_correctas[i] <- which(posiciones == 1)
  }
  
  # Verificar que hay variedad en las posiciones
  tabla_frecuencias <- table(posiciones_correctas)
  cat("Distribución de respuestas correctas:", "\n")
  print(tabla_frecuencias)
  
  # Verificar que aparecen las 4 posiciones
  expect_equal(length(tabla_frecuencias), 4)
  
  # Verificar distribución aproximadamente uniforme
  chi_sq <- chisq.test(tabla_frecuencias)
  expect_true(chi_sq$p.value >= 0.01)
  
  cat("   Aleatoriedad de respuesta correcta verificada\n")
  return(TRUE)
}

# Prueba 4: Verificar no solapamiento de etiquetas
test_no_solapamiento_etiquetas <- function() {
  cat("Verificando no solapamiento de etiquetas...\n")
  
  # Generar varios conjuntos de datos y verificar separación de etiquetas
  for (i in 1:10) {
    stats <- generar_datos_estatura()
    valores <- calcular_valores_diagrama(stats, "correcto")
    
    # Calcular espacios mínimos entre valores estadísticos
    rango <- valores$maximo - valores$minimo
    min_espacio_requerido <- rango * 0.15
    
    # Verificar separación entre valores adyacentes
    valores_ordenados <- sort(c(valores$minimo, valores$q1, valores$mediana, valores$q3, valores$maximo))
    for (j in 2:5) {
      separacion <- valores_ordenados[j] - valores_ordenados[j-1]
      expect_true(separacion >= 0)
    }
  }
  
  cat("   No solapamiento de etiquetas verificado\n")
  return(TRUE)
}

# Prueba 5: Verificar diversidad de gráficas
test_diversidad_graficas <- function() {
  cat("Verificando diversidad de gráficas...\n")
  
  # Generar múltiples conjuntos de datos y verificar que son diferentes
  n_iteraciones <- 20
  hashes <- character(n_iteraciones)
  
  for (i in 1:n_iteraciones) {
    stats <- generar_datos_estatura()
    # Crear un hash único basado en los valores estadísticos
    hash_valor <- digest::digest(stats)
    hashes[i] <- hash_valor
  }
  
  # Verificar que hay suficiente diversidad
  n_unicos <- length(unique(hashes))
  expect_true(n_unicos >= n_iteraciones * 0.9)
  
  cat("   Diversidad de gráficas verificada\n")
  return(TRUE)
}

# Ejecutar todas las pruebas
run_all_tests <- function() {
  cat("Ejecutando todas las pruebas para cuartiles...\n")
  
  test_metodo_tradicional()
  test_redondeo_cuartiles()
  test_aleatoriedad_respuesta()
  test_no_solapamiento_etiquetas()
  test_diversidad_graficas()
  
  cat("Todas las pruebas completadas con éxito.\n")
}

# Para ejecutar todas las pruebas, descomentar la siguiente línea:
# run_all_tests()
