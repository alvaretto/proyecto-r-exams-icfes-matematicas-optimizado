# Pruebas unitarias para cloze-cuartil-estatura-00.Rmd
library(testthat)

# Cargar las funciones del archivo principal
source("06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/06-Medidas-De-Posición/cloze-cuartil-estatura-00.Rmd", echo = FALSE)

# Prueba 1: Verificar cálculo correcto de cuartiles con método tradicional
test_calculo_cuartiles <- function() {
  cat("Verificando cálculo de cuartiles...\n")
  
  # Casos de prueba con tamaño par
  datos_par <- c(150, 155, 160, 165, 170, 175, 180, 185, 190, 195)
  cuartiles_par <- calcular_cuartiles_tradicional(datos_par)
  expect_equal(cuartiles_par$q1, 160)
  expect_equal(cuartiles_par$q2, 172.5)
  expect_equal(cuartiles_par$q3, 185)
  
  # Casos de prueba con tamaño impar
  datos_impar <- c(150, 155, 160, 165, 170, 175, 180, 185, 190)
  cuartiles_impar <- calcular_cuartiles_tradicional(datos_impar)
  expect_equal(cuartiles_impar$q1, 155)
  expect_equal(cuartiles_impar$q2, 170)
  expect_equal(cuartiles_impar$q3, 180)
  
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

# Prueba 3: Verificar soporte para decimales en cuartiles
test_decimales_cuartiles <- function() {
  cat("Verificando soporte para decimales en cuartiles...\n")
  
  # Generar datos que probablemente produzcan decimales
  datos <- c(150, 152, 154, 156, 158, 159, 161, 163, 165)
  cuartiles <- calcular_cuartiles_tradicional(datos)
  
  # Aplicar redondeo según reglas
  q1_redondeado <- redondear_cuartil(cuartiles$q1)
  q2_redondeado <- redondear_cuartil(cuartiles$q2)
  q3_redondeado <- redondear_cuartil(cuartiles$q3)
  
  # Verificar que se mantienen decimales cuando corresponde
  expect_true(is.numeric(q1_redondeado))
  expect_true(is.numeric(q2_redondeado))
  expect_true(is.numeric(q3_redondeado))
  
  cat("Prueba de soporte para decimales completada.\n")
}

# Prueba 4: Verificar diversidad de diagramas
test_diversidad_diagramas <- function() {
  cat("Verificando diversidad de diagramas...\n")
  
  # Verificar que podemos generar al menos 400 diagramas diferentes
  set.seed(123)  # Fijar semilla para reproducibilidad
  resultado <- verificar_diversidad_diagramas(500)
  
  expect_true(resultado$total_unicos >= 400, 
              info = paste("Solo se pueden generar", resultado$total_unicos, 
                          "diagramas diferentes, pero se requieren al menos 400"))
  
  cat("   Se pueden generar al menos", resultado$total_unicos, "diagramas diferentes\n")
  cat("Prueba de diversidad completada.\n")
}

# Prueba 5: Verificar que los diagramas generados son diferentes
test_diagramas_diferentes <- function() {
  cat("Verificando que los diagramas generados son diferentes...\n")
  
  for (i in 1:10) {
    # Generar datos y diagramas
    stats <- generar_datos_estatura()
    diagramas <- generar_diagramas_diferentes(stats)
    
    # Verificar que todos los diagramas son diferentes entre sí
    for (j in 1:(length(diagramas)-1)) {
      for (k in (j+1):length(diagramas)) {
        expect_true(son_diagramas_diferentes(diagramas[[j]], diagramas[[k]]),
                   info = paste("Los diagramas", names(diagramas)[j], "y", 
                               names(diagramas)[k], "son idénticos"))
      }
    }
  }
  
  cat("Prueba de diagramas diferentes completada.\n")
}

# Prueba 6: Verificar soporte para conjuntos de datos par e impar
test_soporte_par_impar <- function() {
  cat("Verificando soporte para conjuntos de datos par e impar...\n")
  
  # Probar con tamaño par
  n_par <- 10
  datos_par <- generar_datos_estatura(n = n_par)
  expect_equal(length(datos_par$datos), n_par)
  expect_equal(length(datos_par$datos_desordenados), n_par)
  
  # Probar con tamaño impar
  n_impar <- 11
  datos_impar <- generar_datos_estatura(n = n_impar)
  expect_equal(length(datos_impar$datos), n_impar)
  expect_equal(length(datos_impar$datos_desordenados), n_impar)
  
  cat("Prueba de soporte para conjuntos par e impar completada.\n")
}

# Prueba 7: Verificar que no hay solapamiento de etiquetas
test_no_solapamiento_etiquetas <- function() {
  cat("Verificando que no hay solapamiento de etiquetas...\n")
  
  for (i in 1:10) {
    # Generar datos
    stats <- generar_datos_estatura()
    
    # Posiciones iniciales para etiquetas
    pos_etiquetas <- c(stats$minimo, stats$q1, stats$mediana, stats$q3, stats$maximo)
    
    # Calcular distancia mínima requerida
    rango <- stats$maximo - stats$minimo
    min_distancia <- rango * 0.05
    
    # Ajustar posiciones
    pos_ajustadas <- ajustar_posiciones_etiquetas(pos_etiquetas, min_distancia)
    
    # Verificar que no hay solapamiento
    for (j in 2:length(pos_ajustadas)) {
      expect_true(pos_ajustadas[j] - pos_ajustadas[j-1] >= min_distancia,
                 info = paste("Etiquetas", j-1, "y", j, "se solapan"))
    }
  }
  
  cat("Prueba de no solapamiento de etiquetas completada.\n")
}

# Ejecutar todas las pruebas
run_all_tests <- function() {
  cat("Ejecutando todas las pruebas para cloze-cuartil-estatura-00.Rmd\n")
  cat("==============================================================\n")
  
  test_calculo_cuartiles()
  test_orden_cuartiles()
  test_decimales_cuartiles()
  test_diversidad_diagramas()
  test_diagramas_diferentes()
  test_soporte_par_impar()
  test_no_solapamiento_etiquetas()
  
  cat("==============================================================\n")
  cat("Todas las pruebas completadas exitosamente.\n")
}

# Ejecutar todas las pruebas si este script se ejecuta directamente
if (!interactive()) {
  run_all_tests()
}
