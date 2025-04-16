# Archivo de Pruebas Unitarias para interpretacion_grafica_viaje.Rmd
# Usando testthat

library(testthat)
library(knitr)
library(digest) # Asegurarse de que digest esté cargado

# --- Función para extraer y ejecutar el código R del Rmd en un entorno limpio ---
ejecutar_codigo_rmd <- function(rmd_file = "Lab/17/interpretacion_grafica_viaje.Rmd") { # Ruta correcta
  # Verificar si el archivo existe
  if (!file.exists(rmd_file)) {
    stop("El archivo Rmd especificado no existe: ", rmd_file)
  }

  # Leer el contenido del Rmd
  rmd_content <- readLines(rmd_file)

  # Extraer código de los chunks R (simplificado)
  in_chunk <- FALSE
  r_code_lines <- c()
  for (line in rmd_content) {
    if (grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      next # Saltar la línea de inicio del chunk
    }
    if (grepl("^```$", line) && in_chunk) {
      in_chunk <- FALSE
      next # Saltar la línea de fin del chunk
    }
    if (in_chunk) {
      # Excluir líneas de comentarios R y configuraciones específicas de chunk, etc.
      if (!grepl("^#", trimws(line)) &&
          !grepl("^knitr::opts_chunk", trimws(line)) &&
          !grepl("^print\\(", trimws(line)) &&
          !grepl("^plot", trimws(line)) &&
          !grepl("graficas_ordenadas", trimws(line)) &&
          !grepl("^answerlist", trimws(line)) &&
          !grepl("^kable", trimws(line)) &&
          !grepl("^library\\(", trimws(line)) && # No re-cargar librerías aquí
          !grepl("^set\\.seed", trimws(line)) # No re-establecer seed aquí
         ) {
         r_code_lines <- c(r_code_lines, line)
      }
    }
  }
  r_code <- paste(r_code_lines, collapse = "\n")

  # Crear un entorno nuevo y limpio para ejecutar el código
  env <- new.env(parent = globalenv())
  # Establecer seed dentro del entorno para consistencia si es necesario
  set.seed(as.integer(Sys.time()) %% 10000 + sample(1:1000, 1), kind = "Mersenne-Twister", normal.kind = "Inversion")

  tryCatch({
    eval(parse(text = r_code), envir = env)
  }, error = function(e) {
    stop("Error al ejecutar el código R extraído del Rmd: ", e$message)
  })


  # Devolver el entorno con las variables generadas
  return(env)
}

# --- Pruebas Unitarias ---

test_that("Generación de datos coherentes (incluyendo costo)", {
  env <- ejecutar_codigo_rmd()

  # Verificar existencia de variables clave
  expect_true(exists("tiempo", envir = env))
  expect_true(exists("costo_viaje", envir = env)) # NUEVA VERIFICACIÓN
  expect_true(exists("combustible_disponible", envir = env))
  expect_true(exists("distancia_recorrida", envir = env))
  expect_true(exists("datos_viaje", envir = env))
  expect_true(exists("solucion_string", envir = env))

  # Verificar tipos de datos
  expect_vector(env$tiempo)
  expect_vector(env$costo_viaje) # NUEVA VERIFICACIÓN
  expect_vector(env$combustible_disponible)
  expect_vector(env$distancia_recorrida)
  expect_data_frame(env$datos_viaje)
  expect_character(env$solucion_string)

  # Verificar longitud de los vectores de datos
  n_puntos <- length(env$tiempo)
  expect_equal(length(env$costo_viaje), n_puntos) # NUEVA VERIFICACIÓN
  expect_equal(length(env$combustible_disponible), n_puntos)
  expect_equal(length(env$distancia_recorrida), n_puntos)
  expect_equal(nrow(env$datos_viaje), n_puntos)
  expect_equal(ncol(env$datos_viaje), 4) # Tiempo, Costo, Combustible, Distancia (ACTUALIZADO)

  # Verificar tendencias generales
  # Costo debe ser no decreciente
  expect_true(all(diff(env$costo_viaje) >= 0)) # NUEVA VERIFICACIÓN
  # Combustible debe ser no creciente
  expect_true(all(diff(env$combustible_disponible) <= 0))
  # Distancia debe ser no decreciente
  expect_true(all(diff(env$distancia_recorrida) >= 0))

  # Verificar valores iniciales
  expect_equal(env$costo_viaje[1], 0) # NUEVA VERIFICACIÓN
  expect_equal(env$distancia_recorrida[1], 0)
  expect_gt(env$combustible_disponible[1], 0) # Combustible inicial > 0

  # Verificar que el combustible no sea negativo
  expect_true(all(env$combustible_disponible >= 0))
  # Verificar que el costo no sea negativo
  expect_true(all(env$costo_viaje >= 0)) # NUEVA VERIFICACIÓN

  # Verificar formato de la solución
  expect_match(env$solucion_string, "^[01]{4}$")
  expect_equal(sum(as.integer(strsplit(env$solucion_string, "")[[1]])), 1)
})

test_that("Diversidad de las preguntas (mínimo 300 versiones)", {
  n_simulaciones <- 350
  huellas_digitales <- character(n_simulaciones)
  formatos_solucion <- character(n_simulaciones)
  problemas_ejecucion <- 0

  for (i in 1:n_simulaciones) {
    env_sim <- tryCatch({
        ejecutar_codigo_rmd()
      }, error = function(e) {
        warning("Error en la simulación ", i, ": ", e$message)
        problemas_ejecucion <<- problemas_ejecucion + 1
        return(NULL) # Devolver NULL si hay error
    })

    # Si hubo un error en la ejecución, saltar esta iteración
    if (is.null(env_sim)) next

    # Crear una huella digital basada en los datos numéricos clave y la solución
    # Incluir el costo en la huella digital
    huella <- digest::digest(list(
      tiempo = env_sim$tiempo,
      costo = env_sim$costo_viaje, # AÑADIDO COSTO
      combustible = env_sim$combustible_disponible,
      distancia = env_sim$distancia_recorrida,
      solucion = env_sim$solucion_string,
      contexto = c(env_sim$vehiculo_elegido, env_sim$conductor_elegido, env_sim$situacion_elegida, env_sim$registro_elegido),
      colores = c(env_sim$color_comb, env_sim$color_dist),
      pchs = c(env_sim$pch_comb, env_sim$pch_dist),
      ltys = c(env_sim$lty_comb, env_sim$lty_dist),
      labels = c(env_sim$label_comb, env_sim$label_dist)
    ))
    huellas_digitales[i] <- huella
    formatos_solucion[i] <- env_sim$solucion_string
  }

  # Informar si hubo errores durante las simulaciones
  if (problemas_ejecucion > 0) {
     warning(sprintf("Hubo errores en %d de %d simulaciones.", problemas_ejecucion, n_simulaciones))
  }

  # Filtrar huellas vacías (de simulaciones fallidas)
  huellas_validas <- huellas_digitales[huellas_digitales != ""]
  formatos_solucion_validos <- formatos_solucion[huellas_digitales != ""]

  # Contar cuántas versiones únicas se generaron
  n_unicos <- length(unique(huellas_validas))
  cat(sprintf("\nNúmero de versiones únicas generadas en %d simulaciones válidas: %d\n", length(huellas_validas), n_unicos))

  # Verificar si se alcanzó el umbral mínimo
  expect_gte(n_unicos, 300, info = "No se generaron suficientes versiones únicas de la pregunta.")

  # Verificar distribución de las respuestas correctas (aproximadamente uniforme)
  if (length(formatos_solucion_validos) > 0) {
      distribucion_solucion <- table(formatos_solucion_validos)
      cat("\nDistribución de las posiciones de la respuesta correcta (simulaciones válidas):\n")
      print(distribucion_solucion)
      # Esperamos que cada posición correcta aparezca aproximadamente n_validas/4 veces
      expect_true(all(distribucion_solucion > length(formatos_solucion_validos) / 8), info = "La distribución de la respuesta correcta no parece uniforme.")
  } else {
      warning("No hubo simulaciones válidas para verificar la distribución de la solución.")
  }


})

# Mensaje final
cat("\nPruebas unitarias completadas para Lab/17/interpretacion_grafica_viaje.Rmd.\n")
# Para ejecutar estas pruebas desde la consola R:
# library(testthat)
# test_file("ejecutar_pruebas.R")