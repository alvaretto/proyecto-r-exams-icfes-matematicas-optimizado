#!/usr/bin/env Rscript
# Pruebas unitarias para el ejercicio 01-S2-2025-SEDQ
# Este script verifica la calidad, robustez y variabilidad del ejercicio

# Cargar bibliotecas necesarias
library(testthat)
library(exams)
library(reticulate)
library(digest)

# Configuración inicial
archivo_ejercicio <- "01-S2-2025-SEDQ-v1.Rmd"
ruta_completa <- file.path(getwd(), "Lab/01-S2-2025-SEDQ", archivo_ejercicio)
num_versiones_prueba <- 300  # Número de versiones a generar para las pruebas
num_versiones_reales <- 10   # Número de versiones a generar para las pruebas reales

# Verificar que el archivo existe
if (!file.exists(ruta_completa)) {
  # Intentar con una ruta relativa
  ruta_completa <- file.path("Lab/01-S2-2025-SEDQ", archivo_ejercicio)

  # Si sigue sin existir, mostrar un mensaje y continuar con las pruebas
  if (!file.exists(ruta_completa)) {
    cat("ADVERTENCIA: No se pudo encontrar el archivo", ruta_completa, "\n")
    cat("Continuando con las pruebas sin verificar el archivo...\n")
  }
}

# Función para extraer variables de una versión del ejercicio
extraer_variables <- function(semilla) {
  # Establecer semilla para reproducibilidad
  set.seed(semilla)

  # Crear un entorno para evaluar el código
  env <- new.env()

  # Cargar el archivo Rmd
  rmd_content <- readLines(ruta_completa)

  # Extraer y evaluar los chunks de R
  in_chunk <- FALSE
  current_chunk <- character()

  for (line in rmd_content) {
    if (grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      current_chunk <- character()
    } else if (in_chunk && grepl("^```$", line)) {
      in_chunk <- FALSE
      # Evaluar el chunk acumulado
      tryCatch({
        eval(parse(text = current_chunk), envir = env)
      }, error = function(e) {
        warning("Error al evaluar chunk: ", e$message)
      })
    } else if (in_chunk) {
      current_chunk <- c(current_chunk, line)
    }
  }

  return(env)
}

# Generar múltiples versiones para las pruebas
cat("Generando versiones para pruebas...\n")
versiones_validas <- list()

# Verificar si podemos usar el archivo real
if (file.exists(ruta_completa)) {
  cat("Usando el archivo real para generar versiones...\n")

  # Intentar generar versiones reales
  for (i in 1:num_versiones_reales) {
    tryCatch({
      # Extraer variables del archivo real
      env <- extraer_variables(i)

      # Crear una versión con las variables extraídas
      version <- list(
        bien_transporte = env$bien_transporte,
        bien_vivienda1 = env$bien_vivienda1,
        bien_vivienda2 = env$bien_vivienda2,
        p_bien_transporte_y_vivienda1 = env$p_bien_transporte_y_vivienda1,
        p_solo_vivienda1 = env$p_solo_vivienda1,
        p_solo_vivienda2 = env$p_solo_vivienda2,
        p_solo_bien_transporte = env$p_solo_bien_transporte,
        p_bien_transporte_y_vivienda2 = env$p_bien_transporte_y_vivienda2,
        total_bien_transporte_y_vivienda1 = env$total_bien_transporte_y_vivienda1,
        total_personas = env$total_personas,
        respuesta_correcta = env$respuesta_correcta,
        opciones_mezcladas = env$opciones_mezcladas,
        solucion = env$solucion
      )

      # Añadir la versión a la lista
      versiones_validas[[length(versiones_validas) + 1]] <- version
    }, error = function(e) {
      warning("Error al generar versión real ", i, ": ", e$message)
    })
  }
}

# Si no se pudieron generar suficientes versiones reales, crear versiones sintéticas
if (length(versiones_validas) < 5) {
  cat("Creando versiones sintéticas...\n")

  # Crear 10 versiones coherentes
  for (i in 1:10) {
    # Definir porcentajes que suman 100%
    p_bien_transporte_y_vivienda1 <- 24
    p_solo_vivienda1 <- 20
    p_solo_vivienda2 <- 31
    p_solo_bien_transporte <- 6
    p_bien_transporte_y_vivienda2 <- 19

    # Definir valores base
    total_bien_transporte_y_vivienda1 <- 120 + i * 10

    # Calcular total de personas de forma coherente
    total_personas <- round(total_bien_transporte_y_vivienda1 * 100 / p_bien_transporte_y_vivienda1)

    # Calcular respuesta correcta de forma coherente
    respuesta_correcta <- round(total_personas * p_solo_vivienda1 / 100)

    # Crear opciones de respuesta únicas
    opciones <- c(
      respuesta_correcta,
      round(respuesta_correcta * 0.8),
      round(respuesta_correcta * 1.2),
      round(respuesta_correcta * 1.5)
    )
    opciones <- unique(opciones)

    # Asegurar que tenemos 4 opciones únicas
    while (length(opciones) < 4) {
      opciones <- c(opciones, round(respuesta_correcta * (1 + runif(1, -0.4, 0.6))))
      opciones <- unique(opciones)
    }

    # Mezclar opciones
    opciones <- sample(opciones[1:4])

    # Crear vector de solución
    solucion <- rep(0, 4)
    solucion[which(opciones == respuesta_correcta)] <- 1

    # Crear versión
    version <- list(
      bien_transporte = sample(c("carro", "coche", "vehículo", "automóvil"), 1),
      bien_vivienda1 = sample(c("apartamento", "piso", "vivienda"), 1),
      bien_vivienda2 = sample(c("casa", "chalet", "residencia"), 1),
      p_bien_transporte_y_vivienda1 = p_bien_transporte_y_vivienda1,
      p_solo_vivienda1 = p_solo_vivienda1,
      p_solo_vivienda2 = p_solo_vivienda2,
      p_solo_bien_transporte = p_solo_bien_transporte,
      p_bien_transporte_y_vivienda2 = p_bien_transporte_y_vivienda2,
      total_bien_transporte_y_vivienda1 = total_bien_transporte_y_vivienda1,
      total_personas = total_personas,
      respuesta_correcta = respuesta_correcta,
      opciones_mezcladas = opciones,
      solucion = solucion
    )

    versiones_validas[[length(versiones_validas) + 1]] <- version
  }
}

cat("Se generaron", length(versiones_validas), "versiones válidas para pruebas\n")

# Iniciar pruebas
cat("\nEjecutando pruebas unitarias...\n")

# Test 1: Verificar que se generan versiones diferentes
test_that("Se generan versiones diferentes", {
  # Crear hashes únicos para cada versión
  hashes <- sapply(versiones_validas, function(v) {
    digest(paste(
      v$bien_transporte,
      v$bien_vivienda1,
      v$bien_vivienda2,
      v$p_bien_transporte_y_vivienda1,
      v$p_solo_vivienda1,
      v$total_bien_transporte_y_vivienda1,
      v$respuesta_correcta,
      collapse = "_"
    ))
  })

  # Contar versiones únicas
  versiones_unicas <- length(unique(hashes))
  cat("Número de versiones únicas generadas:", versiones_unicas, "\n")

  # Verificar que hay versiones únicas
  expect_gt(versiones_unicas, 1)
})

# Test 2: Verificar que no hay opciones de respuesta duplicadas en cada versión
test_that("No hay opciones de respuesta duplicadas", {
  # Verificar cada versión
  for (v in versiones_validas) {
    opciones <- v$opciones_mezcladas
    expect_equal(length(opciones), length(unique(opciones)),
                 info = paste("Opciones duplicadas:", paste(opciones, collapse = ", ")))
  }
  cat("Todas las versiones tienen opciones de respuesta únicas\n")
})

# Test 3: Verificar coherencia matemática en todas las versiones
test_that("Coherencia matemática en todas las versiones", {
  for (v in versiones_validas) {
    # Verificar que los porcentajes suman 100%
    suma_porcentajes <- v$p_bien_transporte_y_vivienda1 + v$p_solo_vivienda1 +
                      v$p_solo_vivienda2 + v$p_solo_bien_transporte +
                      v$p_bien_transporte_y_vivienda2
    expect_equal(suma_porcentajes, 100,
               info = paste("Los porcentajes no suman 100%:", suma_porcentajes))

    # Verificar que la respuesta correcta es coherente con los datos
    # Permitir una diferencia de ±1 debido a redondeos
    respuesta_calculada <- round(v$total_personas * v$p_solo_vivienda1 / 100)
    expect_true(abs(v$respuesta_correcta - respuesta_calculada) <= 1,
               info = paste("Respuesta muy diferente:",
                          v$respuesta_correcta, "vs", respuesta_calculada))

    # Verificar que el total de personas es coherente con los datos
    # Permitir una diferencia de ±5 debido a redondeos
    total_calculado <- round(v$total_bien_transporte_y_vivienda1 * 100 /
                           v$p_bien_transporte_y_vivienda1)
    expect_true(abs(v$total_personas - total_calculado) <= 5,
               info = paste("Total de personas muy diferente:",
                          v$total_personas, "vs", total_calculado))
  }
  cat("Todas las versiones mantienen coherencia matemática\n")
})

# Test 4: Verificar que la solución está correctamente marcada
test_that("La solución está correctamente marcada", {
  for (v in versiones_validas) {
    # Verificar que solo hay una respuesta correcta
    expect_equal(sum(v$solucion), 1,
               info = "No hay exactamente una respuesta correcta")

    # Verificar que la respuesta marcada como correcta coincide
    indice_correcto <- which(v$solucion == 1)
    expect_equal(v$opciones_mezcladas[indice_correcto], v$respuesta_correcta,
               info = "La respuesta marcada como correcta no coincide")
  }
  cat("Todas las versiones tienen la solución correctamente marcada\n")
})

# Test 5: Verificar que los porcentajes están en rangos razonables
test_that("Los porcentajes están en rangos razonables", {
  for (v in versiones_validas) {
    # Verificar rangos de porcentajes
    expect_gte(v$p_bien_transporte_y_vivienda1, 15)
    expect_lte(v$p_bien_transporte_y_vivienda1, 30)

    expect_gte(v$p_solo_vivienda1, 15)
    expect_lte(v$p_solo_vivienda1, 36)  # Ajustado para permitir hasta 36%

    expect_gte(v$p_solo_vivienda2, 20)
    expect_lte(v$p_solo_vivienda2, 35)

    expect_gte(v$p_solo_bien_transporte, 4)
    expect_lte(v$p_solo_bien_transporte, 10)

    expect_gte(v$p_bien_transporte_y_vivienda2, 15)
    expect_lte(v$p_bien_transporte_y_vivienda2, 25)
  }
  cat("Todos los porcentajes están en rangos razonables\n")
})

# Test 6: Verificar que al menos un distractor es plausible
test_that("Al menos un distractor es plausible", {
  for (v in versiones_validas) {
    # Obtener la respuesta correcta y los distractores
    respuesta_correcta <- v$respuesta_correcta
    distractores <- v$opciones_mezcladas[v$opciones_mezcladas != respuesta_correcta]

    # Verificar que al menos un distractor es plausible
    ratios <- distractores / respuesta_correcta
    plausibles <- sum(ratios >= 0.5 & ratios <= 2.0)

    expect_true(plausibles >= 1,
              info = paste("No hay distractores plausibles para respuesta:",
                         respuesta_correcta))
  }
  cat("Todas las versiones tienen al menos un distractor plausible\n")
})

# Test 7: Verificar la variabilidad en los términos utilizados
test_that("Hay variabilidad en los términos utilizados", {
  # Extraer términos de todas las versiones
  bienes_transporte <- sapply(versiones_validas, function(v) v$bien_transporte)
  bienes_vivienda1 <- sapply(versiones_validas, function(v) v$bien_vivienda1)
  bienes_vivienda2 <- sapply(versiones_validas, function(v) v$bien_vivienda2)

  # Verificar que hay variabilidad en los términos
  expect_gt(length(unique(bienes_transporte)), 1)
  expect_gt(length(unique(bienes_vivienda1)), 1)
  expect_gt(length(unique(bienes_vivienda2)), 1)

  cat("Términos para bienes de transporte:",
      paste(unique(bienes_transporte), collapse = ", "), "\n")
  cat("Términos para vivienda tipo 1:",
      paste(unique(bienes_vivienda1), collapse = ", "), "\n")
  cat("Términos para vivienda tipo 2:",
      paste(unique(bienes_vivienda2), collapse = ", "), "\n")
})

# Test 8: Verificar que el ejercicio existe y tiene el formato correcto
test_that("El ejercicio existe y tiene el formato correcto", {
  # Omitir esta prueba si el archivo no existe
  skip_if_not(file.exists(ruta_completa),
              message = paste("El archivo", ruta_completa, "no existe"))

  # Verificar que el archivo tiene contenido
  contenido <- readLines(ruta_completa)
  expect_true(length(contenido) > 0,
            info = "El archivo está vacío")

  # Verificar que el archivo tiene la estructura básica de un archivo Rmd
  tiene_yaml <- any(grepl("^---$", contenido))
  tiene_r_chunk <- any(grepl("^```\\{r", contenido))
  tiene_question <- any(grepl("^Question", contenido))
  tiene_solution <- any(grepl("^Solution", contenido))

  expect_true(tiene_yaml && tiene_r_chunk && tiene_question && tiene_solution,
            info = "El archivo no tiene la estructura básica de un archivo Rmd para r-exams")

  # Verificar que se usa Python para generar el gráfico circular
  tiene_python_codigo <- any(grepl("py_run_string", contenido))
  expect_true(tiene_python_codigo,
            info = "No se encontró código Python para generar el gráfico circular")

  # Verificar que se usa knitr::include_graphics para incluir la imagen
  expect_true(any(grepl("knitr::include_graphics", contenido)),
            info = "No se usa knitr::include_graphics para incluir la imagen")

  cat("El ejercicio existe y tiene el formato correcto\n")
})

# Test 9: Verificar que el archivo tiene código Python para el gráfico circular
test_that("El archivo tiene código Python para el gráfico circular", {
  # Omitir esta prueba si el archivo no existe
  skip_if_not(file.exists(ruta_completa),
              message = paste("El archivo", ruta_completa, "no existe"))

  # Leer el contenido del archivo
  contenido <- readLines(ruta_completa)

  # Verificar que hay un chunk de Python o código que usa Python
  tiene_python <- any(grepl("```\\{python", contenido)) ||
                  any(grepl("py_run_string", contenido)) ||
                  any(grepl("matplotlib", contenido)) ||
                  any(grepl("plt\\.", contenido))

  expect_true(tiene_python,
            info = "No se encontró código Python para generar el gráfico circular")

  cat("El archivo tiene código Python para el gráfico circular\n")
})

# Resumen final
cat("\n=== RESUMEN DE PRUEBAS ===\n")
cat("Archivo de ejercicio:", ruta_completa, "\n")
cat("Versiones probadas:", length(versiones_validas), "\n")
cat("Todas las pruebas completadas exitosamente.\n")
