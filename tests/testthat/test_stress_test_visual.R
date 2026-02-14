# =============================================================================
# test_stress_test_visual.R
# Tests unitarios para el Stress Test Visual Multi-Semilla
# =============================================================================

# Cargar el script principal
source_paths <- c(
  "SOURCES/scripts_validacion/stress_test_visual.R",
  "../../SOURCES/scripts_validacion/stress_test_visual.R"
)
for (p in source_paths) {
  if (file.exists(p)) { source(p); break }
}

library(testthat)

# =============================================================================
# Tests de extraer_datos_semilla()
# =============================================================================

test_that("extraer_datos_semilla detecta opciones por convención de nombres", {
  env <- new.env(parent = globalenv())
  env$opciones_mezcladas <- list(A = 10, B = 20, C = 30, D = 40)
  env$sol <- c(0, 1, 0, 0)
  env$valor_correcto <- 20

  datos <- extraer_datos_semilla(env)
  expect_equal(datos$opciones_var, "opciones_mezcladas")
  expect_equal(datos$posicion_correcta, 2)
  expect_equal(datos$valor_correcto, 20)
})

test_that("extraer_datos_semilla maneja entorno vacío sin error", {
  env <- new.env(parent = globalenv())
  datos <- extraer_datos_semilla(env)
  expect_null(datos$opciones)
  expect_null(datos$sol)
  expect_null(datos$valor_correcto)
  expect_equal(length(datos$posicion_correcta), 0)
})

test_that("extraer_datos_semilla detecta múltiples convenciones de valor_correcto", {
  env <- new.env(parent = globalenv())
  env$mediana_calc <- 15.5
  datos <- extraer_datos_semilla(env)
  expect_equal(datos$valor_correcto, 15.5)

  env2 <- new.env(parent = globalenv())
  env2$respuesta_correcta <- 42
  datos2 <- extraer_datos_semilla(env2)
  expect_equal(datos2$valor_correcto, 42)
})

# =============================================================================
# Tests de serializar_opciones()
# =============================================================================

test_that("serializar_opciones convierte lista numérica a strings", {
  opts <- list(A = 10.5, B = 20.3, C = 30.1, D = 40.7)
  result <- serializar_opciones(opts)
  expect_equal(length(result), 4)
  expect_equal(result[1], "10.5")
})

test_that("serializar_opciones maneja vectores numéricos", {
  opts <- c(10.5, 20.3, 30.1, 40.7)
  result <- serializar_opciones(opts)
  expect_equal(length(result), 4)
  expect_equal(result[1], "10.5")
})

test_that("serializar_opciones maneja NULL", {
  expect_null(serializar_opciones(NULL))
})

test_that("serializar_opciones usa digest para objetos complejos", {
  opts <- list(
    A = list(min = 1, max = 10),
    B = list(min = 2, max = 20)
  )
  result <- serializar_opciones(opts)
  expect_equal(length(result), 2)
  # Los hashes deben ser diferentes porque los objetos son diferentes
  expect_false(result[1] == result[2])
})

# =============================================================================
# Tests de analizar_datos()
# =============================================================================

test_that("analizar_datos detecta opciones duplicadas", {
  resultados <- list(
    list(
      semilla = 7919, indice = 1, estado = "exitosa", png_paths = "test.png",
      errores = character(0),
      datos = list(
        opciones = list(A = 10, B = 10, C = 30, D = 40),  # A y B duplicadas
        sol = c(0, 0, 1, 0),
        valor_correcto = 30,
        enunciado = "Test 1",
        datos = c(10, 20, 30, 40),
        posicion_correcta = 3
      )
    ),
    list(
      semilla = 15838, indice = 2, estado = "exitosa", png_paths = "test2.png",
      errores = character(0),
      datos = list(
        opciones = list(A = 10, B = 20, C = 30, D = 40),  # Todas diferentes
        sol = c(0, 1, 0, 0),
        valor_correcto = 20,
        enunciado = "Test 2",
        datos = c(10, 20, 30, 40),
        posicion_correcta = 2
      )
    )
  )

  anomalias <- analizar_datos(resultados, "test.Rmd")
  tipos <- sapply(anomalias, function(a) a$tipo)
  expect_true("ANOM_DUP_OPT" %in% tipos)
})

test_that("analizar_datos detecta distractor igual a correcto", {
  resultados <- list(
    list(
      semilla = 7919, indice = 1, estado = "exitosa", png_paths = "test.png",
      errores = character(0),
      datos = list(
        opciones = list(A = 10, B = 20, C = 20, D = 40),  # C == B (correcto)
        sol = c(0, 1, 0, 0),
        valor_correcto = 20,
        enunciado = "Test",
        datos = c(10, 20, 30, 40),
        posicion_correcta = 2
      )
    ),
    list(
      semilla = 15838, indice = 2, estado = "exitosa", png_paths = "test2.png",
      errores = character(0),
      datos = list(
        opciones = list(A = 10, B = 20, C = 30, D = 40),
        sol = c(0, 1, 0, 0),
        valor_correcto = 20,
        enunciado = "Test 2",
        datos = c(10, 20, 30, 40),
        posicion_correcta = 2
      )
    )
  )

  anomalias <- analizar_datos(resultados, "test.Rmd")
  tipos <- sapply(anomalias, function(a) a$tipo)
  expect_true("ANOM_DIST_EQ_CORR" %in% tipos)
})

test_that("analizar_datos detecta posición correcta fija", {
  # 9 de 10 semillas con correcta en posición 3
  resultados <- lapply(1:10, function(i) {
    list(
      semilla = i * 7919, indice = i, estado = "exitosa", png_paths = "test.png",
      errores = character(0),
      datos = list(
        opciones = list(A = 10, B = 20, C = 30, D = 40),
        sol = if (i <= 9) c(0, 0, 1, 0) else c(1, 0, 0, 0),  # 9/10 en pos 3
        valor_correcto = if (i <= 9) 30 else 10,
        enunciado = paste("Test", i),
        datos = c(10, 20, 30, 40),
        posicion_correcta = if (i <= 9) 3 else 1
      )
    )
  })

  anomalias <- analizar_datos(resultados, "test.Rmd")
  tipos <- sapply(anomalias, function(a) a$tipo)
  expect_true("ANOM_POS_FIJA" %in% tipos)
})

test_that("analizar_datos detecta semillas fallidas", {
  resultados <- list(
    list(
      semilla = 7919, indice = 1, estado = "fallida", png_paths = character(0),
      errores = c("Error: algo falló"),
      datos = NULL
    ),
    list(
      semilla = 15838, indice = 2, estado = "exitosa", png_paths = "test.png",
      errores = character(0),
      datos = list(
        opciones = list(A = 10, B = 20, C = 30, D = 40),
        sol = c(0, 1, 0, 0),
        valor_correcto = 20,
        enunciado = "Test",
        datos = c(10, 20, 30, 40),
        posicion_correcta = 2
      )
    ),
    list(
      semilla = 23757, indice = 3, estado = "exitosa", png_paths = "test.png",
      errores = character(0),
      datos = list(
        opciones = list(A = 11, B = 21, C = 31, D = 41),
        sol = c(1, 0, 0, 0),
        valor_correcto = 11,
        enunciado = "Test 3",
        datos = c(11, 21, 31, 41),
        posicion_correcta = 1
      )
    )
  )

  anomalias <- analizar_datos(resultados, "test.Rmd")
  tipos <- sapply(anomalias, function(a) a$tipo)
  expect_true("ANOM_COMPILE" %in% tipos)
})

test_that("analizar_datos detecta baja variabilidad de valor correcto", {
  resultados <- lapply(1:10, function(i) {
    list(
      semilla = i * 7919, indice = i, estado = "exitosa", png_paths = "test.png",
      errores = character(0),
      datos = list(
        opciones = list(A = 10, B = 20.001, C = 30, D = 40),
        sol = c(0, 1, 0, 0),
        valor_correcto = 20 + runif(1, -0.001, 0.001),  # Casi invariante
        enunciado = paste("Test", i),
        datos = c(10, 20, 30, 40),
        posicion_correcta = 2
      )
    )
  })

  anomalias <- analizar_datos(resultados, "test.Rmd")
  tipos <- sapply(anomalias, function(a) a$tipo)
  expect_true("ANOM_BAJA_VAR" %in% tipos)
})

test_that("analizar_datos reporta pocas exitosas como anomalía", {
  resultados <- list(
    list(semilla = 7919, indice = 1, estado = "fallida", datos = NULL,
         png_paths = character(0), errores = "Error")
  )
  anomalias <- analizar_datos(resultados, "test.Rmd")
  tipos <- sapply(anomalias, function(a) a$tipo)
  expect_true("ANOM_POCAS_EXITOSAS" %in% tipos)
})

# =============================================================================
# Tests de clasificar_semillas()
# =============================================================================

test_that("clasificar_semillas separa correctamente", {
  resultados <- list(
    list(semilla = 100, estado = "exitosa"),
    list(semilla = 200, estado = "exitosa"),
    list(semilla = 300, estado = "fallida")
  )
  anomalias <- list(
    list(tipo = "ANOM_DUP_OPT", semilla = 200, severidad = "critica")
  )

  clasif <- clasificar_semillas(resultados, anomalias)
  expect_equal(clasif$normales, 100)
  expect_equal(clasif$sospechosas, 200)
  expect_equal(clasif$fallidas, 300)
})

test_that("clasificar_semillas sin anomalías pone todo en normales", {
  resultados <- list(
    list(semilla = 100, estado = "exitosa"),
    list(semilla = 200, estado = "exitosa")
  )
  clasif <- clasificar_semillas(resultados, list())
  expect_equal(length(clasif$normales), 2)
  expect_equal(length(clasif$sospechosas), 0)
  expect_equal(length(clasif$fallidas), 0)
})
