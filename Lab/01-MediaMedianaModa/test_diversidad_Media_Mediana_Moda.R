library(testthat)
library(digest)
library(exams)

# Función que simula la generación de datos del ejercicio
simular_generacion_datos <- function() {
  # Tamaño del conjunto de datos (entre 7 y 12 valores)
  n <- sample(7:12, 1)
  
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
    media = media_redondeada,
    mediana = mediana,
    moda = moda,
    soluciones = c(sol_media, sol_mediana, sol_moda)
  ))
}

# Test para verificar que se generan al menos 500 versiones diferentes
test_that("Verificar diversidad de al menos 500 versiones diferentes", {
  # Número de iteraciones
  n_iteraciones <- 1000
  
  # Almacenar hashes de cada versión
  versiones <- vector("character", n_iteraciones)
  
  # Ejecutar las iteraciones
  for (i in 1:n_iteraciones) {
    # Generar datos para esta versión
    set.seed(i)  # Para reproducibilidad en las pruebas
    datos_version <- simular_generacion_datos()
    
    # Crear un hash único para esta versión
    version_hash <- digest::digest(datos_version)
    versiones[i] <- version_hash
  }
  
  # Contar versiones únicas
  versiones_unicas <- unique(versiones)
  n_unicas <- length(versiones_unicas)
  
  # Guardar resultados para análisis posterior
  resultados <- data.frame(
    total_versiones = n_iteraciones,
    versiones_unicas = n_unicas,
    porcentaje = round(n_unicas/n_iteraciones*100, 2)
  )
  write.table(resultados, 
             "resultados_diversidad_Media_Mediana_Moda.txt", 
             row.names = FALSE, quote = FALSE)
  
  # Verificar que hay al menos 500 versiones diferentes
  expect_true(n_unicas >= 500,
             info = paste("Se generaron", n_unicas, 
                         "versiones únicas. Se requieren al menos 500."))
  
  # Imprimir resultados para información
  cat("\nResultados de diversidad:\n")
  cat("Total de versiones generadas:", n_iteraciones, "\n")
  cat("Versiones únicas:", n_unicas, "\n")
  cat("Porcentaje de diversidad:", round(n_unicas/n_iteraciones*100, 2), "%\n")
})

# Test para verificar que las soluciones sean correctas
test_that("Verificar exactitud de cálculos", {
  for (i in 1:10) {  # Probar 10 casos aleatorios
    set.seed(i)
    datos <- simular_generacion_datos()
    
    # Verificar cálculo de la media
    expect_equal(
      datos$media,
      round(mean(datos$datos), 2),
      info = "El cálculo de la media es incorrecto"
    )
    
    # Verificar cálculo de la mediana
    expect_equal(
      datos$mediana,
      median(datos$datos),
      info = "El cálculo de la mediana es incorrecto"
    )
    
    # Verificar cálculo de la moda
    tabla <- table(datos$datos)
    modas <- as.numeric(names(tabla[tabla == max(tabla)]))
    expect_equal(
      datos$moda,
      modas[1],
      info = "El cálculo de la moda es incorrecto"
    )
  }
})

# Test para verificar que la media, mediana y moda son diferentes entre sí
test_that("Verificar que media, mediana y moda sean diferentes", {
  for (i in 1:10) {  # Probar 10 casos aleatorios
    set.seed(i)
    datos <- simular_generacion_datos()
    
    # Verificar que los tres valores son diferentes
    expect_equal(
      length(unique(c(datos$media, datos$mediana, datos$moda))),
      3,
      info = "La media, mediana y moda no son todas diferentes entre sí"
    )
  }
})

# Ejecutar el archivo Rmd directamente para ver si compila correctamente
test_that("El archivo Rmd compila correctamente", {
  rmd_path <- "/home/manjaro_lenovo/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Media-Mediana-Moda.Rmd"
  
  # Verificar que el archivo existe
  expect_true(
    file.exists(rmd_path),
    info = "El archivo Rmd no existe en la ruta especificada"
  )
  
  # Comprobar que se puede compilar al menos a un examen
  tryCatch({
    # Crear directorio temporal para pruebas
    temp_dir <- tempdir()
    
    # Intentar compilar el archivo a un formato HTML
    exams2html(rmd_path, n = 1, dir = temp_dir)
    
    # Si llegamos aquí, la compilación fue exitosa
    expect_true(TRUE, info = "La compilación del Rmd fue exitosa")
  }, error = function(e) {
    fail(paste("Error al compilar el archivo Rmd:", e$message))
  })
})