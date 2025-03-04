library(testthat)
library(exams)

# Función que simula la generación de datos del ejercicio
simular_generacion_datos <- function() {
  # Tamaño del conjunto de datos (entre 7 y 12 valores)
  n <- sample(7:12, 1)  # Según el código del ejercicio
  
  # Generar datos con distribución que tenga una moda definida
  # Generamos valores enteros entre 1 y 20
  datos_base <- sample(1:20, n-1, replace = TRUE)
  
  # Aseguramos que haya un valor que se repita (la moda)
  moda_valor <- sample(1:20, 1)
  # Determinar cuántas veces aparecerá la moda (entre 2 y 3 veces)
  moda_repeticiones <- sample(2:3, 1)
  
  # Combinar los datos base con las repeticiones de la moda
  datos <- c(datos_base, rep(moda_valor, moda_repeticiones))
  # Mezclar el conjunto de datos
  datos <- sample(datos, length(datos))
  
  # Calcular la media, mediana y moda
  media <- mean(datos)
  mediana <- median(datos)
  
  # Función para encontrar la moda
  encontrar_moda <- function(x) {
    tabla <- table(x)
    modas <- as.numeric(names(tabla[tabla == max(tabla)]))
    # Si hay más de un valor con la misma frecuencia máxima, devolvemos el primero
    return(modas[1])
  }
  
  moda <- encontrar_moda(datos)
  
  # Asegurar que los tres valores sean distintos para mayor claridad
  while (length(unique(c(round(media, 2), mediana, moda))) < 3) {
    # Regenerar datos si la media, mediana y moda son iguales
    datos_base <- sample(1:20, n-1, replace = TRUE)
    moda_valor <- sample(1:20, 1)
    datos <- c(datos_base, rep(moda_valor, moda_repeticiones))
    datos <- sample(datos, length(datos))
    
    media <- mean(datos)
    mediana <- median(datos)
    moda <- encontrar_moda(datos)
  }
  
  # Redondear la media a 2 decimales para la solución
  media_redondeada <- round(media, 2)
  
  # Soluciones para el formato cloze
  sol_media <- as.character(media_redondeada)
  sol_mediana <- as.character(mediana)
  sol_moda <- as.character(moda)
  
  return(list(
    datos = datos,
    n = length(datos),
    media = media_redondeada,
    mediana = mediana,
    moda = moda,
    soluciones = c(sol_media, sol_mediana, sol_moda)
  ))
}

# Test para verificar el rango de tamaño del conjunto de datos
test_that("Verificar rango del tamaño del conjunto de datos", {
  tamaños <- numeric(100)
  for (i in 1:100) {
    set.seed(i)
    datos <- simular_generacion_datos()
    tamaños[i] <- length(datos$datos)
  }
  
  # En el algoritmo, el tamaño final depende de la muestra inicial (n-1) + repeticiones de la moda (2-3)
  # Por lo tanto, el rango esperado es entre 8-14 (7-1+2 hasta 12-1+3)
  expect_true(all(tamaños >= 8 & tamaños <= 14),
             info = "Algunos conjuntos de datos tienen tamaños fuera del rango esperado")
  
  # Verificar que se utilizan diferentes tamaños (diversidad)
  expect_true(length(unique(tamaños)) >= 3,
             info = "Se deberían usar al menos 3 tamaños diferentes de conjuntos de datos")
  
  cat("\nDistribución de tamaños de conjuntos de datos:\n")
  print(table(tamaños))
})

# Test para verificar que la moda tiene la frecuencia adecuada
test_that("Verificar frecuencia de la moda", {
  frecuencias_moda <- numeric(100)
  for (i in 1:100) {
    set.seed(i)
    datos <- simular_generacion_datos()
    tabla <- table(datos$datos)
    frecuencias_moda[i] <- max(tabla)
  }
  
  # Verificar que todas las frecuencias de la moda son al menos 2
  # Pueden ser mayores debido a coincidencias aleatorias en los datos
  expect_true(all(frecuencias_moda >= 2),
             info = "Algunas modas tienen frecuencias menores a 2")
  
  cat("\nDistribución de frecuencias de la moda:\n")
  print(table(frecuencias_moda))
})

# Test para verificar que los valores generados están en el rango correcto
test_that("Verificar rango de valores generados", {
  for (i in 1:10) {
    set.seed(i)
    datos <- simular_generacion_datos()
    
    # Verificar que todos los valores están en el rango de 1 a 20
    expect_true(all(datos$datos >= 1 & datos$datos <= 20),
               info = "Algunos datos están fuera del rango 1-20")
  }
})

# Test para verificar que el formato de las soluciones es correcto
test_that("Verificar formato de las soluciones", {
  for (i in 1:10) {
    set.seed(i)
    datos <- simular_generacion_datos()
    soluciones <- datos$soluciones
    
    # Verificar que la media tiene 2 decimales si es necesario
    media_str <- soluciones[1]
    if (grepl("\\.", media_str)) {
      decimales <- nchar(sub(".*\\.", "", media_str))
      expect_true(decimales <= 2,
                 info = paste("La media tiene más de 2 decimales:", media_str))
    }
    
    # Verificar que la moda es entero (la mediana puede ser decimal)
    expect_true(as.numeric(soluciones[3]) == round(as.numeric(soluciones[3])),
               info = paste("La moda no es un número entero:", soluciones[3]))
    
    # Verificar si la mediana es decimal, en caso un conjunto con número par de elementos
    if (as.numeric(soluciones[2]) != round(as.numeric(soluciones[2]))) {
      # Si es decimal, verificar que termina en .5 (promedio de dos enteros consecutivos)
      expect_true(abs(as.numeric(soluciones[2]) - round(as.numeric(soluciones[2]))) == 0.5,
                 info = paste("La mediana decimal no termina en .5:", soluciones[2]))
    }
  }
})

# Ejecutar las pruebas
cat("\n======= PRUEBAS ADICIONALES PARA Media-Mediana-Moda.Rmd =======\n")