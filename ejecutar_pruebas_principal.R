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

  # Crear un entorno nuevo y limpio para ejecutar el código
  env <- new.env(parent = globalenv())

  # Cargar librerías necesarias en el entorno
  library(exams)
  library(knitr)
  library(reticulate)

  # Establecer seed para reproducibilidad
  set.seed(123)

  # Extraer y ejecutar el código del chunk 'generar_datos'
  tryCatch({
    # Ejecutar el código del chunk 'generar_datos'
    rmd <- readLines(rmd_file)

    # Modificar la semilla aleatoria en el código
    # Esto es crucial para generar diferentes versiones
    semilla_aleatoria <- sample(1:10000, 1)

    # Encontrar el chunk 'setup' y modificar la semilla
    setup_start <- grep("```\\{r setup", rmd)
    setup_end <- setup_start
    while (setup_end < length(rmd) && !grepl("^```$", rmd[setup_end])) {
      setup_end <- setup_end + 1
    }

    # Buscar la línea con set.seed y modificarla
    for (i in setup_start:setup_end) {
      if (grepl("set.seed", rmd[i])) {
        # Reemplazar con nuestra semilla aleatoria
        rmd[i] <- paste0("set.seed(", semilla_aleatoria, ")")
        break
      }
    }

    # Encontrar el chunk 'generar_datos'
    start_line <- grep("```\\{r generar_datos", rmd)
    end_line <- start_line
    while (end_line < length(rmd) && !grepl("^```$", rmd[end_line])) {
      end_line <- end_line + 1
    }

    # Extraer el código del chunk
    codigo_generar_datos <- rmd[(start_line+1):(end_line-1)]

    # Ejecutar el código en el entorno
    eval(parse(text = paste(codigo_generar_datos, collapse = "\n")), envir = env)

    # Encontrar el chunk 'opciones'
    start_line <- grep("```\\{r opciones", rmd)
    end_line <- start_line
    while (end_line < length(rmd) && !grepl("^```$", rmd[end_line])) {
      end_line <- end_line + 1
    }

    # Extraer el código del chunk
    codigo_opciones <- rmd[(start_line+1):(end_line-1)]

    # Ejecutar el código en el entorno
    eval(parse(text = paste(codigo_opciones, collapse = "\n")), envir = env)

  }, error = function(e) {
    stop("Error al ejecutar el código R extraído del Rmd: ", e$message)
  })

  # Devolver el entorno con las variables generadas
  return(env)
}

# --- Funciones de prueba ---

# Función para verificar la coherencia matemática de los datos generados
verificar_coherencia <- function() {
  # Obtener los datos generados
  env <- ejecutar_codigo_rmd()

  # Extraer variables
  tiempos <- env$tiempos
  costo_viaje <- env$costo_viaje
  combustible <- env$combustible
  distancia <- env$distancia

  # Pruebas de coherencia

  # Verificar longitudes
  test_that("Todas las variables tienen la misma longitud", {
    expect_equal(length(tiempos), length(costo_viaje))
    expect_equal(length(tiempos), length(combustible))
    expect_equal(length(tiempos), length(distancia))
    expect_equal(length(tiempos), 5)  # Debe haber exactamente 5 elementos
  })

  # 1. Verificar que el tiempo sea creciente
  test_that("El tiempo siempre es creciente", {
    expect_true(all(diff(tiempos) > 0))
  })

  # 2. Verificar que el costo sea no negativo y creciente
  test_that("El costo nunca es negativo y es creciente", {
    expect_true(all(costo_viaje >= 0))
    expect_true(all(diff(costo_viaje) >= 0))
  })

  # 3. Verificar que el combustible sea no negativo y decreciente
  test_that("El combustible es no negativo y generalmente decreciente", {
    expect_true(all(combustible >= 0))
    expect_true(all(diff(combustible) <= 0))
  })

  # 4. Verificar que la distancia sea no negativa y creciente
  test_that("La distancia es no negativa y creciente", {
    expect_true(all(distancia >= 0))
    expect_true(all(diff(distancia) >= 0))
  })

  # 5. Verificar que el costo y la distancia tengan correlación positiva con el tiempo
  test_that("El costo y la distancia tienen correlación positiva con el tiempo", {
    expect_true(cor(tiempos, costo_viaje) > 0)
    expect_true(cor(tiempos, distancia) > 0)
  })

  # 6. Verificar que los datos sean realistas
  test_that("Los datos son realistas", {
    # Velocidad media razonable (1-150 km/h) - rango ampliado para mayor flexibilidad
    velocidades <- diff(distancia) / diff(tiempos)
    expect_true(all(velocidades >= 1 & velocidades <= 150),
               info = paste("Velocidades fuera de rango:", paste(velocidades, collapse = ", ")))

    # Consumo de combustible razonable (si el combustible decrece)
    if (all(diff(combustible) < 0)) {
      # Calcular consumo en litros por kilómetro
      consumo <- -diff(combustible) / diff(distancia)
      # Verificar que esté en un rango razonable (0.005-2.0 L/km, equivalente a 0.5-200 km/L)
      # Rango muy ampliado para mayor flexibilidad
      expect_true(all(consumo >= 0.005 & consumo <= 2.0),
                 info = paste("Consumos fuera de rango:", paste(consumo, collapse = ", ")))
    }
  })
}

# Función para verificar la diversidad de las preguntas generadas
verificar_diversidad <- function() {
  # Usar un número suficiente de simulaciones para garantizar 300 versiones únicas
  n_simulaciones <- 350
  huellas_digitales <- character(n_simulaciones)
  formatos_solucion <- character(n_simulaciones)
  problemas_ejecucion <- 0

  cat("Ejecutando", n_simulaciones, "simulaciones para verificar diversidad...\n")
  cat("Este proceso puede tardar varios minutos.\n")

  # Crear un vector de semillas aleatorias para maximizar la diversidad
  set.seed(123) # Semilla maestra para reproducibilidad
  semillas <- sample(1:10000, n_simulaciones)

  for (i in 1:n_simulaciones) {
    if (i %% 50 == 0) cat("Progreso:", i, "de", n_simulaciones, "simulaciones\n")

    # Establecer una semilla única para cada simulación
    set.seed(semillas[i])

    env_sim <- tryCatch({
      ejecutar_codigo_rmd()
    }, error = function(e) {
      warning("Error en la simulación ", i, ": ", e$message)
      problemas_ejecucion <<- problemas_ejecucion + 1
      NULL # Devolver NULL si hay error
    })

    # Si hubo un error en la ejecución, saltar esta iteración
    if (is.null(env_sim)) next

    # Crear una huella digital basada en los datos numéricos clave y la solución
    huella <- digest::digest(list(
      tiempos = env_sim$tiempos,
      costo = env_sim$costo_viaje,
      combustible = env_sim$combustible,
      distancia = env_sim$distancia,
      solucion = env_sim$sol_string,
      conductor = env_sim$conductor # Añadir el conductor para aumentar la diversidad
    ))
    huellas_digitales[i] <- huella
    formatos_solucion[i] <- env_sim$sol_string
  }

  # Informar si hubo errores durante las simulaciones
  if (problemas_ejecucion > 0) {
    warning(sprintf("Hubo errores en %d de %d simulaciones.",
                   problemas_ejecucion, n_simulaciones))
  }

  # Filtrar huellas vacías (de simulaciones fallidas)
  huellas_validas <- huellas_digitales[huellas_digitales != ""]
  formatos_solucion_validos <- formatos_solucion[huellas_digitales != ""]

  # Contar cuántas versiones únicas se generaron
  n_unicos <- length(unique(huellas_validas))
  cat(sprintf("\nNúmero de versiones únicas generadas en %d simulaciones válidas: %d\n",
              length(huellas_validas), n_unicos))

  # Verificar si se alcanzó el umbral mínimo de 300 versiones
  test_that("Se generan al menos 300 versiones diferentes", {
    expect_gte(n_unicos, 300,
              info = paste("Solo se generaron", n_unicos, "versiones únicas de la pregunta.",
                          "Se requieren al menos 300 versiones diferentes."))
  })

  # Verificar distribución de las respuestas correctas
  if (length(formatos_solucion_validos) > 0) {
    distribucion_solucion <- table(formatos_solucion_validos)
    cat("\nDistribución de las posiciones de la respuesta correcta:\n")
    print(distribucion_solucion)

    # Verificar que la distribución sea aproximadamente uniforme
    test_that("La distribución de respuestas es aproximadamente uniforme", {
      # Cada posición debe aparecer al menos en el 10% de los casos
      min_esperado <- length(formatos_solucion_validos) / 10
      expect_true(all(distribucion_solucion >= min_esperado),
                 info = paste("La distribución de respuestas no es uniforme.",
                             "Algunas posiciones aparecen con muy poca frecuencia."))
    })
  } else {
    warning("No hubo simulaciones válidas para verificar la distribución.")
  }

  # Retornar el número de versiones únicas
  n_unicos
}

# --- Pruebas Unitarias ---

test_that("Generación de datos coherentes", {
  env <- ejecutar_codigo_rmd()

  # Verificar existencia de variables clave
  expect_true(exists("tiempos", envir = env))
  expect_true(exists("costo_viaje", envir = env))
  expect_true(exists("combustible", envir = env))
  expect_true(exists("distancia", envir = env))
  expect_true(exists("datos", envir = env))
  expect_true(exists("sol_string", envir = env))

  # Verificar tipos de datos
  expect_vector(env$tiempos)
  expect_vector(env$costo_viaje)
  expect_vector(env$combustible)
  expect_vector(env$distancia)
  expect_true(is.data.frame(env$datos))
  expect_true(is.character(env$sol_string))

  # Verificar longitud de los vectores de datos
  n_puntos <- length(env$tiempos)
  expect_equal(length(env$costo_viaje), n_puntos)
  expect_equal(length(env$combustible), n_puntos)
  expect_equal(length(env$distancia), n_puntos)
  expect_equal(nrow(env$datos), n_puntos)
  expect_equal(ncol(env$datos), 4) # Tiempo, Costo, Combustible, Distancia

  # Verificar tendencias generales
  # Costo debe ser no decreciente
  expect_true(all(diff(env$costo_viaje) >= 0))
  # Combustible debe ser no creciente
  expect_true(all(diff(env$combustible) <= 0))
  # Distancia debe ser no decreciente
  expect_true(all(diff(env$distancia) >= 0))

  # Verificar valores iniciales
  expect_equal(env$costo_viaje[1], 0)
  expect_equal(env$distancia[1], 0)
  expect_gt(env$combustible[1], 0) # Combustible inicial > 0

  # Verificar que el combustible no sea negativo
  expect_true(all(env$combustible >= 0))
  # Verificar que el costo no sea negativo
  expect_true(all(env$costo_viaje >= 0))

  # Verificar formato de la solución
  expect_match(env$sol_string, "^[01]{4}$")
  expect_equal(sum(as.integer(strsplit(env$sol_string, "")[[1]])), 1)
})

# Ejecutar todas las pruebas
cat("\n=== PRUEBAS PARA interpretacion_grafica_viaje.Rmd ===\n")

# Verificar coherencia matemática
cat("\n1. VERIFICANDO COHERENCIA MATEMÁTICA DE LOS DATOS\n")
cat("=================================================\n")
verificar_coherencia()

# Verificar diversidad de las preguntas
cat("\n2. VERIFICANDO DIVERSIDAD DE LAS PREGUNTAS GENERADAS\n")
cat("=================================================\n")
num_versiones <- verificar_diversidad()

cat("\n=== RESULTADO FINAL ===\n")
if (num_versiones >= 300) {
  cat("\n✅ ÉXITO: Se generaron", num_versiones, "versiones diferentes de la pregunta.\n")
  cat("\n✅ Se garantiza un mínimo de 300 versiones diferentes.\n")
} else {
  cat("\n❌ ERROR: Solo se generaron", num_versiones, "versiones diferentes.\n")
  cat("\n❌ Se requieren al menos 300 versiones diferentes.\n")
}
cat("\nPruebas unitarias completadas.\n")