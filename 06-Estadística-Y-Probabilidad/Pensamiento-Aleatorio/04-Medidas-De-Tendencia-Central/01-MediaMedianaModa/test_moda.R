library(testthat)
library(knitr)

# Cargar el archivo Rmd que contiene la función encontrar_moda
# source("Media-Mediana-Moda.Rmd")

# Función auxiliar para extraer la función encontrar_moda del entorno cargado
get_encontrar_moda <- function() {
  env <- new.env()
  knitr::knit("Media-Mediana-Moda.Rmd", envir = env, quiet = TRUE)
  return(env$encontrar_moda)
}

encontrar_moda <- get_encontrar_moda()

test_that("encontrar_moda devuelve un único valor cuando la moda es única", {
  datos <- c(1, 2, 3, 4, 5, 5)
  expect_equal(encontrar_moda(datos), 5)
  
  datos <- c(10, 20, 30, 40, 50, 50, 50)
  expect_equal(encontrar_moda(datos), 50)
})

test_that("encontrar_moda devuelve un error o NA si hay múltiples modas", {
  datos <- c(1, 2, 3, 4, 5, 5, 6, 6)
  # Aquí, verificamos si la función devuelve NA o produce un error
  # Ajusta la expectativa según el comportamiento deseado
  expect_true(is.na(encontrar_moda(datos))) # Espera NA
  # expect_error(encontrar_moda(datos)) # Espera un error
  
  datos <- c(1, 1, 2, 2, 3, 3)
   # Aquí, verificamos si la función devuelve NA o produce un error
  # Ajusta la expectativa según el comportamiento deseado
  expect_true(is.na(encontrar_moda(datos))) # Espera NA
  # expect_error(encontrar_moda(datos)) # Espera un error
})

test_that("encontrar_moda funciona con datos generados aleatoriamente", {
  # Esta prueba genera datos aleatorios similares a los del archivo Rmd
  # y verifica que la moda calculada sea consistente
  n <- sample(7:12, 1)
  moda_valor <- sample(1:20, 1)
  moda_repeticiones <- sample(2:3, 1)
  datos_base <- sample(1:100, n - 1, replace = TRUE)
  datos <- c(datos_base, rep(moda_valor, moda_repeticiones))
  datos <- sample(datos, length(datos))
  
  tabla <- table(datos)
  modas <- as.numeric(names(tabla[tabla == max(tabla)]))
  
  # Si hay múltiples modas, la prueba debe omitirse o fallar
  if (length(modas) > 1) {
    skip("Múltiples modas encontradas, omitiendo la prueba")
  } else {
    expect_equal(encontrar_moda(datos), modas[1])
  }
})