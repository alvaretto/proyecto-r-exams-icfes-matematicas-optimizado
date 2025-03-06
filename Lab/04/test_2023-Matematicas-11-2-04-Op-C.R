library(testthat)

# Cargar el archivo Rmd
#source("04/2023-Matematicas-11-2-04-Op-C.Rmd")

test_that("La mediana del modelo Z es igual a la del modelo X y menor que la del modelo Y", {
  # Ejecutar el código de generación de datos 500 veces
  for (i in 1:500) {
    #source("2023-Matematicas-11-2-04-Op-C.Rmd") # Esto no funciona porque source ejecuta el Rmd completo, no solo la parte de data generation
    
    # Simular la generación de datos (copiando el código del Rmd)
    modelos <- sample(c("Modelo A", "Modelo B", "Modelo C", "Modelo D", "ModeloE", 
                        "Modelo F", "Modelo G", "Modelo H", "Modelo J", "ModeloK", 
                        "Modelo L", "Modelo M", "Modelo N", "Modelo P", "Modelo Q", 
                        "Modelo R", "Modelo S", "Modelo T", "Modelo U", "Modelo V", 
                        "Modelo W"), 3)
    
    base_valores <- sample(500:800, 15, replace = TRUE)
    matriz_datos <- matrix(base_valores, nrow = 3, ncol = 5)
    
    # Ajustamos los valores para que el modelo Z tenga la mediana igual a X y menor que Y
    mediana_deseada_x <- median(sort(matriz_datos[1,]))
    mediana_deseada_y <- median(sort(matriz_datos[2,]))
    mediana_deseada_z <- mediana_deseada_x
    
    while (mediana_deseada_y <= mediana_deseada_x) {
      matriz_datos[2,] <- matriz_datos[2,] + 10
      mediana_deseada_y <- median(sort(matriz_datos[2,]))
    }
    
    # Aseguramos que Z tenga la misma mediana que X
    matriz_datos[3,] <- sort(matriz_datos[1,])
    matriz_datos[3,] <- matriz_datos[3,] + sample(-20:20, 5, replace = TRUE)
    while (median(sort(matriz_datos[3,])) != mediana_deseada_x) {
      matriz_datos[3,] <- sort(matriz_datos[1,])
      matriz_datos[3,] <- matriz_datos[3,] + sample(-20:20, 5, replace = TRUE)
    }
    
    # Aleatorizar el orden de las columnas de la tabla
    orden_aleatorio <- sample(1:3)
    
    # Creamos la tabla de datos
    tabla <- data.frame(
      Prueba = paste("Prueba", 1:5),
      Modelo1 = matriz_datos[1,],
      Modelo2 = matriz_datos[2,],
      Modelo3 = matriz_datos[3,]
    )
    
    # Reordenar las columnas de la tabla
    tabla <- tabla[, c("Prueba", paste0("Modelo", orden_aleatorio))]
    
    # Renombrar las columnas de la tabla con los nombres de los modelos
    colnames(tabla) <- c("Prueba", modelos)

    # Calculamos las medianas para verificación
    med1 <- median(tabla[, modelos[1]])
    med2 <- median(tabla[, modelos[2]])
    med3 <- median(tabla[, modelos[3]])
    
    # Verificar que la mediana del modelo Z sea igual a la del modelo X
    expect_equal(med3, med1)
    
    # Verificar que la mediana del modelo Z sea menor que la del modelo Y
    expect_lt(med3, med2)
  }
})