# ejecutar_pruebas.R

library(testthat)
library(rmarkdown)
library(exams)

# Función para extraer datos y verificar coherencia matemática
verificar_coherencia <- function() {
  # Establecer semilla aleatoria (para reproducibilidad de la prueba)
  set.seed(123)

  # Ejecutar el chunk de definición de variables para obtener los datos
  source_env <- new.env()
  eval(parse(text = readLines("interpretacion_grafica_viaje.Rmd")[49:145]), envir = source_env)

  # Extraer variables
  tiempo <- source_env$tiempo
  costo_viaje <- source_env$costo_viaje
  combustible <- source_env$combustible
  distancia <- source_env$distancia

  # Pruebas de coherencia

  # Verificar longitudes
  test_that("Todas las variables tienen la misma longitud", {
    expect_equal(length(tiempo), length(costo_viaje))
    expect_equal(length(tiempo), length(combustible))
    expect_equal(length(tiempo), length(distancia))
    expect_equal(length(tiempo), 6)  # Debe haber exactamente 6 elementos
  })

  # 1. Verificar que el tiempo sea creciente
  test_that("El tiempo siempre es creciente", {
    expect_true(all(diff(tiempo) > 0))
  })

  # 2. Verificar que el costo sea no negativo
  test_that("El costo nunca es negativo", {
    expect_true(all(costo_viaje >= 0))
  })

  # 3. Verificar que el combustible sea positivo
  test_that("El combustible siempre es positivo", {
    expect_true(all(combustible > 0))
  })

  # 4. Verificar que la distancia sea no negativa y creciente
  test_that("La distancia es no negativa y creciente", {
    expect_true(all(distancia >= 0))
    expect_true(all(diff(distancia) >= 0))
  })

  # 5. Verificar que el costo y la distancia tengan correlación positiva
  test_that("El costo y la distancia tienen correlación positiva", {
    expect_true(cor(tiempo, costo_viaje) > 0)
    expect_true(cor(tiempo, distancia) > 0)
  })

  # 6. Verificar que los datos sean realistas
  test_that("Los datos son realistas", {
    # Velocidad media razonable (10-120 km/h)
    velocidades <- diff(distancia) / diff(tiempo)
    expect_true(all(velocidades >= 5 & velocidades <= 120))

    # Consumo de combustible razonable
    if (all(diff(combustible) < 0)) {  # Si el combustible decrece
      consumo <- -diff(combustible) / diff(distancia[2:length(distancia)])
      expect_true(all(consumo >= 0.01 & consumo <= 0.25))  # 4-100 km/litro
    }
  })

  # 7. Verificar que la tendencia del combustible se respete
  test_that("La tendencia del combustible se respeta", {
    if (source_env$tendencia_combustible == "decreciente") {
      expect_true(combustible[1] >= combustible[length(combustible)])
    } else if (source_env$tendencia_combustible == "oscilante") {
      # Debe haber al menos una subida y una bajada
      diffs <- diff(combustible)
      expect_true(any(diffs > 0) && any(diffs < 0))
    } else if (source_env$tendencia_combustible == "primero_crece_luego_decrece") {
      # Debe crecer al principio y decrecer al final
      expect_true(combustible[2] > combustible[1] &&
                    combustible[length(combustible)] < combustible[length(combustible)-1])
    }
  })
}

# Función para verificar diversidad de ejercicios generados
verificar_diversidad <- function(n_simulaciones = 300) {
  # Vectores para almacenar "huellas digitales" de cada generación
  huellas_digitales <- character(n_simulaciones)

  for (i in 1:n_simulaciones) {
    # Establecer semilla aleatoria
    set.seed(i)

    # Ejecutar el chunk de definición de variables
    source_env <- new.env()
    eval(parse(text = readLines("interpretacion_grafica_viaje.Rmd")[49:145]), envir = source_env)

    # Crear una "huella digital" de esta generación
    huella <- paste(
      paste(source_env$tiempo, collapse = ","),
      paste(source_env$combustible, collapse = ","),
      paste(source_env$distancia, collapse = ","),
      source_env$tendencia_combustible,
      source_env$tendencia_distancia,
      source_env$grafica_correcta,
      collapse = "|"
    )

    huellas_digitales[i] <- huella
  }

  # Contar cuántas huellas únicas hay
  n_unicas <- length(unique(huellas_digitales))

  test_that("Se generan al menos 300 versiones diferentes", {
    expect_gte(n_unicas, 300)
  })

  # Retornar el número de huellas únicas
  return(n_unicas)
}

# Función para verificar la correcta generación de Python
verificar_generacion_python <- function() {
  test_that("El código Python genera gráficas válidas", {
    # Verificar que el código sea válido ejecutándolo
    source_env <- new.env()
    eval(parse(text = readLines("interpretacion_grafica_viaje.Rmd")[176:291]), envir = source_env)

    # Verificar que se han generado los archivos de imagen
    expect_true(file.exists("grafica_1.png"))
    expect_true(file.exists("grafica_2.png"))
    expect_true(file.exists("grafica_3.png"))
    expect_true(file.exists("grafica_4.png"))
  })
}

# Función para verificar la compatibilidad del ejercicio con r-exams
verificar_compatibilidad_exams <- function() {
  test_that("El ejercicio es compatible con r-exams", {
    # Intentar generar un PDF con el ejercicio
    resultado <- try(exams2pdf("interpretacion_grafica_viaje.Rmd", dir = ".", template = "plain"))
    expect_true(!inherits(resultado, "try-error"))

    # Verificar que se ha generado un archivo PDF
    archivos_pdf <- list.files(pattern = "\\.pdf$")
    expect_true(length(archivos_pdf) > 0)
  })
}

# Ejecutar todas las pruebas
verificar_coherencia()
num_versiones <- verificar_diversidad()
cat("Número de versiones únicas generadas:", num_versiones, "\n")
verificar_generacion_python()
verificar_compatibilidad_exams()
