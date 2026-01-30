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
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-M.Rmd")
  
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
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-M.Rmd")
  expect_equal(datos$distancia %% datos$duracion, 0)
})

test_that("Valores están dentro de los rangos especificados", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-M.Rmd")
  expect_true(datos$duracion %in% 2:6)
  expect_true(datos$rapidez %in% 30:120)
  expect_true(datos$pos_inicial %in% 10:80)
  expect_true(datos$hora_inicial %in% 6:12)
})

test_that("Cálculos son consistentes", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-M.Rmd")
  expect_equal(datos$distancia, datos$rapidez * datos$duracion)
  expect_equal(datos$pos_final, datos$pos_inicial + datos$distancia)
  expect_equal(datos$hora_final, datos$hora_inicial + datos$duracion)
})

test_that("Distractores son válidos", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-M.Rmd")
  expect_equal(datos$distractor1, datos$pos_final - datos$pos_inicial)
  expect_equal(datos$distractor2, datos$distancia %/% (datos$hora_final - datos$hora_inicial + sample(c(-1,1), 1)))
  expect_equal(datos$distractor3, datos$pos_inicial - datos$pos_final)
  expect_true(datos$distractor4 %in% -20:20)
})

# Pruebas para múltiples respuestas correctas
test_that("Errores en múltiples pasos", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-M.Rmd")
  
  # Verificar error en el Paso 1
  expect_false(datos$distancia_incorrecta == datos$distancia,
               info = "El cálculo en el Paso 1 debe ser incorrecto")
  
  # Verificar error en el Paso 2
  expect_false(datos$tiempo_incorrecto == datos$duracion,
               info = "El cálculo en el Paso 2 debe ser incorrecto")
  
  # Verificar que el Paso 3 sea incorrecto basado en los errores previos
  expect_false(datos$distancia_incorrecta / datos$tiempo_incorrecto == datos$rapidez,
               info = "El cálculo en el Paso 3 debe ser incorrecto")
})

test_that("Respuestas correctas son consistentes", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-M.Rmd")
  
  # Verificar que la primera respuesta sea correcta
  expect_true(datos$distancia_incorrecta != datos$distancia,
              info = "La primera respuesta debe ser correcta")
  
  # Verificar que la segunda respuesta sea correcta
  expect_true(datos$tiempo_incorrecto != datos$duracion,
              info = "La segunda respuesta debe ser correcta")
  
  # Verificar que las otras respuestas sean incorrectas
  expect_false(datos$distractor3 == datos$distancia,
               info = "La tercera respuesta debe ser incorrecta")
  expect_false(datos$distractor4 == datos$distancia,
               info = "La cuarta respuesta debe ser incorrecta")
})

test_that("Tipos de errores son válidos", {
  datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-M.Rmd")
  expect_true(datos$tipo_error %in% c("distancia", "tiempo", "ambos"))
  
  if(datos$tipo_error == "distancia") {
    expect_false(datos$distancia_incorrecta == datos$distancia)
    expect_equal(datos$tiempo_incorrecto, datos$duracion)
  } else if(datos$tipo_error == "tiempo") {
    expect_equal(datos$distancia_incorrecta, datos$distancia)
    expect_false(datos$tiempo_incorrecto == datos$duracion)
  } else {
    expect_false(datos$distancia_incorrecta == datos$distancia)
    expect_false(datos$tiempo_incorrecto == datos$duracion)
  }
})

test_that("Variabilidad es suficiente", {
  set.seed(123)  # Para reproducibilidad
  tipos_error <- replicate(1000, {
    datos <- ejecutar_generacion_datos("2023-Matematicas-11-2-09-Op-M.Rmd")
    datos$tipo_error
  })
  
  expect_true(length(unique(tipos_error)) == 3, 
              info = "Deben generarse los 3 tipos de errores")
  
  expect_true(sum(tipos_error == "distancia") >= 300, 
              info = "Debe haber suficientes errores de distancia")
  expect_true(sum(tipos_error == "tiempo") >= 300, 
              info = "Debe haber suficientes errores de tiempo")
  expect_true(sum(tipos_error == "ambos") >= 300, 
              info = "Debe haber suficientes errores de ambos")
})