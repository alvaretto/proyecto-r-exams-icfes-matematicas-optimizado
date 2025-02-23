library(testthat)
library(knitr)

# Función para ejecutar el chunk de generación de datos del Rmd
ejecutar_generacion_datos <- function(rmd_file) {
  # Leemos el archivo completo
  rmd_code <- readLines(rmd_file, warn = FALSE)
  
  # Encontramos el inicio del chunk de generación de datos
  start <- grep("```\\{r data generation", rmd_code)
  
  # Encontramos el final del chunk
  end <- grep("```", rmd_code[(start+1):length(rmd_code)])[1] + start
  
  # Extraemos el código del chunk
  generation_code <- paste(rmd_code[(start+1):(end-1)], collapse = "\n")
  
  # Ejecutamos el código en un entorno nuevo
  env <- new.env()
  eval(parse(text = generation_code), envir = env)
  as.list(env)
}

# Pruebas unitarias
test_that("Todos los valores son enteros", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-E(a).Rmd")
  
  # Verificamos cada valor individualmente
  expect_true(is.numeric(datos$duracion) && datos$duracion %% 1 == 0, 
              info = sprintf("duracion: %s", datos$duracion))
  expect_true(is.numeric(datos$rapidez) && datos$rapidez %% 1 == 0, 
              info = sprintf("rapidez: %s", datos$rapidez))
  expect_true(is.numeric(datos$distancia) && datos$distancia %% 1 == 0, 
              info = sprintf("distancia: %s", datos$distancia))
  expect_true(is.numeric(datos$pos_inicial) && datos$pos_inicial %% 1 == 0, 
              info = sprintf("pos_inicial: %s", datos$pos_inicial))
  expect_true(is.numeric(datos$pos_final) && datos$pos_final %% 1 == 0, 
              info = sprintf("pos_final: %s", datos$pos_final))
  expect_true(is.numeric(datos$hora_inicial) && datos$hora_inicial %% 1 == 0, 
              info = sprintf("hora_inicial: %s", datos$hora_inicial))
  expect_true(is.numeric(datos$hora_final) && datos$hora_final %% 1 == 0, 
              info = sprintf("hora_final: %s", datos$hora_final))
  expect_true(is.numeric(datos$tiempo) && datos$tiempo %% 1 == 0, 
              info = sprintf("tiempo: %s", datos$tiempo))
  expect_true(is.numeric(datos$distractor1) && datos$distractor1 %% 1 == 0, 
              info = sprintf("distractor1: %s", datos$distractor1))
  expect_true(is.numeric(datos$distractor2) && datos$distractor2 %% 1 == 0, 
              info = sprintf("distractor2: %s", datos$distractor2))
  expect_true(is.numeric(datos$distractor3) && datos$distractor3 %% 1 == 0, 
              info = sprintf("distractor3: %s", datos$distractor3))
  expect_true(is.numeric(datos$distractor4) && datos$distractor4 %% 1 == 0, 
              info = sprintf("distractor4: %s", datos$distractor4))
})

test_that("Distancia es múltiplo exacto de la duración", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-E(a).Rmd")
  expect_equal(datos$distancia %% datos$duracion, 0)
})

test_that("Valores están dentro de los rangos especificados", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-E(a).Rmd")
  expect_true(datos$duracion %in% 2:4)
  expect_true(datos$rapidez %in% 30:100)
  expect_true(datos$pos_inicial %in% 10:50)
  expect_true(datos$hora_inicial %in% 6:10)
})

test_that("Cálculos son consistentes", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-E(a).Rmd")
  expect_equal(datos$distancia, datos$rapidez * datos$duracion)
  expect_equal(datos$pos_final, datos$pos_inicial + datos$distancia)
  expect_equal(datos$hora_final, datos$hora_inicial + datos$duracion)
})

test_that("Distractores son válidos", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-E(a).Rmd")
  expect_equal(datos$distractor1, datos$pos_final - datos$pos_inicial)
  expect_equal(datos$distractor2, datos$distancia %/% (datos$hora_final - datos$hora_inicial + 1))
  expect_equal(datos$distractor3, datos$pos_inicial - datos$pos_final)
  expect_true(datos$distractor4 %in% -10:10)
})

# Nueva prueba para verificar que la primera opción sea siempre correcta
test_that("Primera opción es siempre correcta", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-E(a).Rmd")
  
  # Verificar que el cálculo en el Paso 1 sea incorrecto
  expect_false(datos$pos_final - datos$pos_inicial == datos$distancia - datos$distractor4,
               info = "El cálculo en el Paso 1 debe ser incorrecto para que la primera opción sea la correcta")
  
  # Verificar que los otros pasos sean correctos
  expect_equal(datos$hora_final - datos$hora_inicial, datos$duracion,
               info = "El cálculo en el Paso 2 debe ser correcto")
  expect_equal(datos$distancia / datos$duracion, datos$rapidez,
               info = "El cálculo en el Paso 3 debe ser correcto")
})