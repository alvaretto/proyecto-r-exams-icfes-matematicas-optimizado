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
  
  # Número de conjuntos de datos a probar
  n_pruebas <- 50
  
  for (i in 1:n_pruebas) {
    # Generar datos aleatorios
    stats <- generar_datos_estatura()
    
    # Crear diferentes diagramas para las opciones
    diagramas <- list(
      correcto = calcular_valores_diagrama(stats, "correcto"),
      escala = calcular_valores_diagrama(stats, "escala"),
      invertido = calcular_valores_diagrama(stats, "invertido"),
      mediana_falsa = calcular_valores_diagrama(stats, "mediana_falsa")
    )
    
    # Verificar que todos los pares de diagramas son diferentes entre sí
    nombres <- names(diagramas)
    for (j in 1:(length(nombres)-1)) {
      for (k in (j+1):length(nombres)) {
        diag1 <- diagramas[[nombres[j]]]
        diag2 <- diagramas[[nombres[k]]]
        
        # Verificar que los diagramas son diferentes
        if (!son_diagramas_diferentes(diag1, diag2)) {
          stop(paste("Error: Se encontraron diagramas idénticos para las opciones", 
                     nombres[j], "y", nombres[k], 
                     "con los valores:", 
                     "Min:", diag1$minimo, 
                     "Q1:", diag1$q1, 
                     "Med:", diag1$mediana, 
                     "Q3:", diag1$q3, 
                     "Max:", diag1$maximo))
        }
      }
    }
  }
  
  cat("   No se encontraron opciones de respuesta duplicadas en", n_pruebas, "pruebas\n")
  return(TRUE)
}

# Función mejorada para verificar si dos diagramas son diferentes
son_diagramas_diferentes <- function(diag1, diag2) {
  # Verificar si al menos uno de los valores es diferente
  if (identical(diag1$minimo, diag2$minimo) && 
      identical(diag1$q1, diag2$q1) && 
      identical(diag1$mediana, diag2$mediana) && 
      identical(diag1$q3, diag2$q3) && 
      identical(diag1$maximo, diag2$maximo)) {
    return(FALSE)
  }
  return(TRUE)
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
  
  # Nueva prueba de no duplicidad
  test_no_duplicidad_opciones()
  
  cat("Todas las pruebas completadas.\n")
}

# Ejecutar todas las pruebas si este script se ejecuta directamente
if (!interactive()) {
  run_all_tests()
}

# Añadir las funciones necesarias del archivo Rmd

# Función para generar datos aleatorios de estatura
generar_datos_estatura <- function(
    base_min = sample(145:165, 1), 
    base_max = sample(168:188, 1),
    # Permitir tamaños de muestra pares o impares
    n = sample(9:25, 1)
) {
    variacion <- sample(5:15, 1)
    min_valor <- base_min - sample(0:variacion, 1)
    max_valor <- base_max + sample(0:variacion, 1)
    
    # Generar datos aleatorios y ordenarlos
    datos <- sort(round(runif(n, min_valor, max_valor), digits = 0))

    # Calcular estadísticas usando el método tradicional
    cuartiles <- calcular_cuartiles_tradicional(datos)
    
    stats <- list(
        datos = datos,
        datos_desordenados = sample(datos),
        minimo = min(datos),
        q1 = cuartiles$q1,
        mediana = cuartiles$q2,
        q3 = cuartiles$q3,
        maximo = max(datos)
    )
    
    # Redondear cuartiles para permitir un decimal si no es .0
    stats$q1 <- redondear_cuartil(stats$q1)
    stats$mediana <- redondear_cuartil(stats$mediana)
    stats$q3 <- redondear_cuartil(stats$q3)
    
    return(stats)
}

# Función para calcular cuartiles usando el método tradicional
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

# Función para redondear cuartiles según las reglas especificadas
redondear_cuartil <- function(valor) {
    valor_redondeado <- round(valor, 1)
    if (valor_redondeado %% 1 == 0) {
        return(as.integer(valor_redondeado))
    } else {
        return(valor_redondeado)
    }
}

# Función para calcular valores para los diagramas
calcular_valores_diagrama <- function(stats, tipo = "correcto") {
    # Valores base
    valores <- list(
        minimo = stats$minimo,
        q1 = stats$q1,
        mediana = stats$mediana,
        q3 = stats$q3,
        maximo = stats$maximo
    )
    
    # Modificar según el tipo de diagrama
    if (tipo == "escala") {
        # Multiplicar todos los valores por 10
        valores$minimo <- valores$minimo * 10
        valores$q1 <- valores$q1 * 10
        valores$mediana <- valores$mediana * 10
        valores$q3 <- valores$q3 * 10
        valores$maximo <- valores$maximo * 10
    } else if (tipo == "invertido") {
        # Invertir Q1 y Q3
        temp <- valores$q1
        valores$q1 <- valores$q3
        valores$q3 <- temp
    } else if (tipo == "mediana_falsa") {
        # Usar la media como mediana
        valores$mediana <- round(mean(stats$datos))
    }
    
    return(valores)
}

# Función mejorada para verificar si dos diagramas son diferentes
son_diagramas_diferentes <- function(diag1, diag2) {
    # Verificar si al menos uno de los valores es diferente
    if (identical(diag1$minimo, diag2$minimo) && 
        identical(diag1$q1, diag2$q1) && 
        identical(diag1$mediana, diag2$mediana) && 
        identical(diag1$q3, diag2$q3) && 
        identical(diag1$maximo, diag2$maximo)) {
        return(FALSE)
    }
    return(TRUE)
}

# Prueba para verificar que no hay duplicidad en las opciones de respuesta
test_no_duplicidad_opciones <- function() {
    cat("Verificando que no hay opciones de respuesta duplicadas...\n")
    
    # Número de conjuntos de datos a probar
    n_pruebas <- 50
    
    for (i in 1:n_pruebas) {
        # Generar datos aleatorios
        stats <- generar_datos_estatura()
        
        # Crear diferentes diagramas para las opciones
        diagramas <- list(
            correcto = calcular_valores_diagrama(stats, "correcto"),
            escala = calcular_valores_diagrama(stats, "escala"),
            invertido = calcular_valores_diagrama(stats, "invertido"),
            mediana_falsa = calcular_valores_diagrama(stats, "mediana_falsa")
        )
        
        # Verificar que todos los pares de diagramas son diferentes entre sí
        nombres <- names(diagramas)
        for (j in 1:(length(nombres)-1)) {
            for (k in (j+1):length(nombres)) {
                diag1 <- diagramas[[nombres[j]]]
                diag2 <- diagramas[[nombres[k]]]
                
                # Verificar que los diagramas son diferentes
                if (!son_diagramas_diferentes(diag1, diag2)) {
                    stop(paste("Error: Se encontraron diagramas idénticos para las opciones", 
                               nombres[j], "y", nombres[k], 
                               "con los valores:", 
                               "Min:", diag1$minimo, 
                               "Q1:", diag1$q1, 
                               "Med:", diag1$mediana, 
                               "Q3:", diag1$q3, 
                               "Max:", diag1$maximo))
                }
            }
        }
    }
    
    cat("   No se encontraron opciones de respuesta duplicadas en", n_pruebas, "pruebas\n")
    return(TRUE)
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
  
  # Nueva prueba de no duplicidad
  test_no_duplicidad_opciones()
  
  cat("Todas las pruebas completadas.\n")
}

# Ejecutar todas las pruebas si este script se ejecuta directamente
if (!interactive()) {
  run_all_tests()
}

