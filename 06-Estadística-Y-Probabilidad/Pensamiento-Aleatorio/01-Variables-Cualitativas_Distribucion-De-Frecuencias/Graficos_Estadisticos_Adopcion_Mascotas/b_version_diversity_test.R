library(testthat)
library(digest)

# Prueba de diversidad de versiones
test_that("Prueba de diversidad de versiones", {
  # Generar y almacenar 1000 versiones
  versiones <- list()
  
  # Código de DefiniciónDeVariables chunk
  mascotas <- c('loro', 'perro', 'gato', 'gallina', 'hamster', 'cerdo', 
                'ternero', 'caballo', 'cabra')
  numeros <- seq(60, 300, 10)
  numeros_sin_100 <- setdiff(numeros, 100)
  
  # Código de DefiniciónDeVariables2 chunk
  generar_vector_unico <- function() {
    num1 <- sample(1:99, 1)
    num2 <- sample(1:99, 1)
    while (num1 + num2 >= 100) {
      num1 <- sample(1:99, 1)
      num2 <- sample(1:99, 1)
    }
    num3 <- 100 - num1 - num2
    return(c(num1, num2, num3))
  }
  
  for(i in 1:1000) {
    # Create a new environment for each iteration
    local_env <- new.env(parent = emptyenv())
    
    # Muestreo de variables
    mascotas_sample <- sample(mascotas)
    selmascota <- sample(mascotas_sample, 3)
    enkuestados <- sample(numeros_sin_100, 1)
    vector_resultado <- generar_vector_unico()
    
    maskota1 <- (enkuestados*vector_resultado[1])/100
    maskota2 <- (enkuestados*vector_resultado[2])/100
    maskota3 <- (enkuestados*vector_resultado[3])/100
    
    datos_test <- list(enkuestados = enkuestados, 
                       vector_resultado = vector_resultado, 
                       selmascota = selmascota, 
                       maskota1 = maskota1, 
                       maskota2 = maskota2, 
                       maskota3 = maskota3)
    versiones[[i]] <- digest::digest(datos_test)  # Crear hash de los datos
  }

  # Verificar al menos 750 versiones únicas
  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 750,
              info = paste("Solo se generaron", n_versiones_unicas,
                          "versiones únicas. Se requieren al menos 750."))
})