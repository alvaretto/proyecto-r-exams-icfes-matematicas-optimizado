# Tests para schoice-cuartil-estatura-01.Rmd

# Cargar librerías necesarias
library(testthat)
library(exams)
library(knitr)

# Definir funciones de prueba que no dependen de cargar el archivo Rmd

# Función para calcular cuartiles usando el método tradicional (corregida)
calcular_cuartiles_tradicional <- function(datos) {
    n <- length(datos)
    datos_ordenados <- sort(datos)
    
    # Calcular Q2 (mediana)
    if (n %% 2 == 0) {
        # Número par de elementos
        pos_med1 <- n/2
        pos_med2 <- pos_med1 + 1
        q2 <- (datos_ordenados[pos_med1] + datos_ordenados[pos_med2]) / 2
        
        # Dividir en dos mitades
        primera_mitad <- datos_ordenados[1:pos_med1]
        segunda_mitad <- datos_ordenados[pos_med2:n]
    } else {
        # Número impar de elementos
        pos_med <- ceiling(n/2)
        q2 <- datos_ordenados[pos_med]
        
        # Dividir en dos mitades
        primera_mitad <- datos_ordenados[1:(pos_med-1)]
        segunda_mitad <- datos_ordenados[(pos_med+1):n]
    }
    
    # Calcular Q1 (mediana de la primera mitad)
    q1 <- median(primera_mitad)
    
    # Calcular Q3 (mediana de la segunda mitad)
    q3 <- median(segunda_mitad)
    
    return(list(q1 = q1, q2 = q2, q3 = q3))
}

# Prueba 1: Verificar cálculo correcto de cuartiles (ajustada)
test_calculo_cuartiles <- function() {
  cat("Verificando cálculo de cuartiles...\n")
  
  # Casos de prueba con resultados conocidos
  # Caso 1: Número impar de elementos
  datos_impar <- c(10, 20, 30, 40, 50, 60, 70)
  cuartiles_impar <- calcular_cuartiles_tradicional(datos_impar)
  expect_equal(cuartiles_impar$q1, 20)
  expect_equal(cuartiles_impar$q2, 40)
  expect_equal(cuartiles_impar$q3, 60)
  
  # Caso 2: Número par de elementos
  datos_par <- c(10, 20, 30, 40, 50, 60, 70, 80)
  cuartiles_par <- calcular_cuartiles_tradicional(datos_par)
  expect_equal(cuartiles_par$q1, 25)  # Corregido de 20 a 25
  expect_equal(cuartiles_par$q2, 45)
  expect_equal(cuartiles_par$q3, 65)  # Corregido de 70 a 65
  
  cat("Prueba de cálculo de cuartiles completada.\n")
}

# Prueba 2: Verificar que los cuartiles estén en orden correcto
test_orden_cuartiles <- function() {
  cat("Verificando orden de cuartiles...\n")
  
  # Generar varios conjuntos de datos aleatorios
  for (i in 1:10) {
    # Generar datos aleatorios
    n <- sample(9:25, 1)
    datos <- sort(round(runif(n, 150, 190)))
    
    # Calcular cuartiles
    cuartiles <- calcular_cuartiles_tradicional(datos)
    
    # Verificar orden
    minimo <- min(datos)
    maximo <- max(datos)
    
    expect_true(minimo <= cuartiles$q1)
    expect_true(cuartiles$q1 <= cuartiles$q2)
    expect_true(cuartiles$q2 <= cuartiles$q3)
    expect_true(cuartiles$q3 <= maximo)
  }
  
  cat("Prueba de orden de cuartiles completada.\n")
}

# Prueba 3: Verificar que los cuartiles sean valores válidos
test_valores_cuartiles <- function() {
  cat("Verificando valores de cuartiles...\n")
  
  # Generar varios conjuntos de datos aleatorios
  for (i in 1:10) {
    # Generar datos aleatorios
    n <- sample(9:25, 1)
    datos <- sort(round(runif(n, 150, 190)))
    
    # Calcular cuartiles
    cuartiles <- calcular_cuartiles_tradicional(datos)
    
    # Verificar que los cuartiles sean valores numéricos
    expect_true(is.numeric(cuartiles$q1))
    expect_true(is.numeric(cuartiles$q2))
    expect_true(is.numeric(cuartiles$q3))
    
    # Verificar que los cuartiles estén dentro del rango de datos
    expect_true(cuartiles$q1 >= min(datos) && cuartiles$q1 <= max(datos))
    expect_true(cuartiles$q2 >= min(datos) && cuartiles$q2 <= max(datos))
    expect_true(cuartiles$q3 >= min(datos) && cuartiles$q3 <= max(datos))
  }
  
  cat("Prueba de valores de cuartiles completada.\n")
}

# Prueba para verificar que no hay duplicidad en las opciones de respuesta
test_no_duplicidad_opciones <- function() {
  cat("Verificando que no hay opciones de respuesta duplicadas...\n")
  
  # Generar varias opciones de respuesta
  stats <- generar_datos_estatura()
  
  # Crear diferentes diagramas para las opciones
  diag_correcto <- calcular_valores_diagrama(stats, "correcto")
  diag_escala <- calcular_valores_diagrama(stats, "escala")
  diag_invertido <- calcular_valores_diagrama(stats, "invertido")
  diag_mediana_falsa <- calcular_valores_diagrama(stats, "mediana_falsa")
  
  # Verificar que todos son diferentes entre sí
  expect_true(son_diagramas_diferentes(diag_correcto, diag_escala))
  expect_true(son_diagramas_diferentes(diag_correcto, diag_invertido))
  expect_true(son_diagramas_diferentes(diag_correcto, diag_mediana_falsa))
  expect_true(son_diagramas_diferentes(diag_escala, diag_invertido))
  expect_true(son_diagramas_diferentes(diag_escala, diag_mediana_falsa))
  expect_true(son_diagramas_diferentes(diag_invertido, diag_mediana_falsa))
  
  cat("Prueba de no duplicidad completada.\n")
}

# Prueba para verificar la diversidad de diagramas
test_diversidad_diagramas <- function() {
  cat("Verificando diversidad de diagramas...\n")
  
  # Usar un número menor de simulaciones para la prueba
  diversidad <- verificar_diversidad_diagramas(n_simulaciones = 100)
  
  # Verificar que hay suficiente diversidad
  expect_true(diversidad$suficiente_diversidad, 
              info = paste("Solo se pueden generar", diversidad$total_unicos, 
                          "diagramas diferentes. Se requieren al menos 300."))
  
  cat("Prueba de diversidad completada.\n")
}

# Prueba para verificar que no hay gráficas vacías
test_no_graficas_vacias <- function() {
  cat("Verificando que no hay gráficas vacías...\n")
  
  # Generar varios conjuntos de datos
  for (i in 1:10) {
    stats <- generar_datos_estatura()
    
    # Verificar que todos los valores clave están presentes
    expect_false(is.null(stats$minimo) || is.na(stats$minimo))
    expect_false(is.null(stats$q1) || is.na(stats$q1))
    expect_false(is.null(stats$mediana) || is.na(stats$mediana))
    expect_false(is.null(stats$q3) || is.na(stats$q3))
    expect_false(is.null(stats$maximo) || is.na(stats$maximo))
    
    # Verificar que hay datos
    expect_true(length(stats$datos) > 0)
  }
  
  cat("Prueba de no gráficas vacías completada.\n")
}

# Añadir las nuevas pruebas a la función principal
run_all_tests <- function() {
  cat("Ejecutando pruebas para funciones de cálculo de cuartiles...\n")
  
  # Pruebas existentes
  test_calculo_cuartiles()
  test_orden_cuartiles()
  test_valores_cuartiles()
  
  # Nuevas pruebas
  test_no_duplicidad_opciones()
  test_diversidad_diagramas()
  test_no_graficas_vacias()
  
  cat("Todas las pruebas completadas.\n")
}

# Ejecutar todas las pruebas si este script se ejecuta directamente
if (!interactive()) {
  run_all_tests()
}

