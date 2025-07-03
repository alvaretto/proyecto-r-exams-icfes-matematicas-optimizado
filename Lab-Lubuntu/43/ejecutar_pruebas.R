# --- Test Setup ---
library(testthat)
library(knitr)
library(reticulate)
library(exams)

# Define la ruta del archivo Rmd
rmd_file <- "probabilidad_extraccion_bolas_v1.Rmd"
# Define el número de versiones a probar
N_VERSIONS <- 160 # Un poco más de 150 por seguridad

# --- Funciones Auxiliares ---

# Función para extraer y ejecutar código R del Rmd en un entorno controlado
run_rmd_r_code <- function(file_path) {
  # Crea un archivo temporal para el código R extraído
  temp_r_file <- tempfile(fileext = ".R")
  # Asegura la limpieza del archivo R temporal
  on.exit(unlink(temp_r_file, force = TRUE), add = TRUE)

  # Extrae los chunks de código R usando purl
  tryCatch({
    # Redirige la salida a NULL para suprimir posible salida markdown durante purl
    suppressMessages(utils::capture.output(
      knitr::purl(file_path, output = temp_r_file, documentation = 0, quiet = TRUE)
    ))
  }, error = function(e) {
    stop("Falló al extraer (purl) el código R del Rmd: ", e$message)
  })

  # Crea un nuevo entorno para ejecutar el código
  test_env <- new.env(parent = globalenv())
  source_error <- NULL # Para almacenar error de source

  # Ejecuta (source) el código R extraído en el nuevo entorno
  tryCatch({
    # También captura la salida durante source para evitar desorden en la consola
    suppressMessages(utils::capture.output(
      source(temp_r_file, local = test_env, echo = FALSE)
    ))
  }, error = function(e) {
    # Guarda el error si source() falla
    source_error <<- e
  })

  # Devuelve el entorno, el error (si ocurrió)
  return(list(env = test_env, error = source_error))
}

# --- Pruebas Unitarias ---

# Test 1: Verificar que el código R se ejecuta sin errores
test_that("El código R se ejecuta sin errores", {
  result <- run_rmd_r_code(rmd_file)
  expect_null(result$error,
              info = paste("Error al ejecutar el código R:",
                           if(!is.null(result$error)) result$error$message else ""))
})

# Test 2: Verificar la coherencia matemática de los cálculos de probabilidad
test_that("Los cálculos de probabilidad son matemáticamente coherentes", {
  result <- run_rmd_r_code(rmd_file)
  env <- result$env

  # Extraer variables relevantes
  total_bolas <- env$total_bolas
  bolas_principal <- env$bolas_principal
  bolas_secundario <- env$bolas_secundario
  bolas_extraer <- env$bolas_extraer
  min_bolas_ganar <- env$min_bolas_ganar
  probabilidad_correcta <- env$probabilidad_correcta
  casos_favorables <- env$casos_favorables
  casos_posibles <- env$casos_posibles

  # Verificar que las cantidades de bolas son coherentes
  expect_equal(bolas_principal + bolas_secundario, total_bolas)

  # Verificar que el número de bolas a extraer es válido
  expect_true(bolas_extraer <= total_bolas)

  # Verificar que el número mínimo de bolas para ganar es válido
  expect_true(min_bolas_ganar <= bolas_extraer)
  expect_true(min_bolas_ganar <= bolas_principal)

  # Recalcular la probabilidad correcta para verificar
  recalc_casos_favorables <- 0
  for (i in min_bolas_ganar:min(bolas_extraer, bolas_principal)) {
    recalc_casos_favorables <- recalc_casos_favorables +
      choose(bolas_principal, i) * choose(bolas_secundario, bolas_extraer - i)
  }
  recalc_casos_posibles <- choose(total_bolas, bolas_extraer)
  recalc_probabilidad <- recalc_casos_favorables / recalc_casos_posibles

  # Verificar que los cálculos son correctos
  expect_equal(casos_favorables, recalc_casos_favorables,
               info = "El cálculo de casos favorables es incorrecto")
  expect_equal(casos_posibles, recalc_casos_posibles,
               info = "El cálculo de casos posibles es incorrecto")
  expect_equal(probabilidad_correcta, recalc_probabilidad,
               info = "El cálculo de la probabilidad es incorrecto")

  # Verificar que la probabilidad está en el rango [0,1]
  expect_true(probabilidad_correcta >= 0)
  expect_true(probabilidad_correcta <= 1)
})

# Test 3: Verificar que las opciones de respuesta son distintas
test_that("Las opciones de respuesta son distintas", {
  result <- run_rmd_r_code(rmd_file)
  env <- result$env

  # Extraer opciones
  opciones_ordenadas <- env$opciones_ordenadas

  # Verificar que hay 4 opciones
  expect_equal(length(opciones_ordenadas), 4,
               info = "Debe haber exactamente 4 opciones de respuesta")

  # Verificar que todas las opciones son distintas
  expect_equal(length(unique(opciones_ordenadas)), 4,
               info = "Todas las opciones de respuesta deben ser distintas")

  # Verificar que todas las opciones están en el rango [0,1]
  expect_true(all(opciones_ordenadas >= 0 & opciones_ordenadas <= 1),
              info = "Todas las probabilidades deben estar en el rango [0,1]")
})

# Test 4: Verificar que la respuesta correcta está marcada correctamente
test_that("La respuesta correcta está marcada correctamente", {
  result <- run_rmd_r_code(rmd_file)
  env <- result$env

  # Extraer variables relevantes
  probabilidad_correcta <- env$probabilidad_correcta
  opciones_ordenadas <- env$opciones_ordenadas
  indice_correcto <- env$indice_correcto
  solucion <- env$solucion

  # Verificar que la opción marcada como correcta es realmente la correcta
  expect_equal(opciones_ordenadas[indice_correcto], probabilidad_correcta)

  # Verificar que el vector de solución tiene un único 1 y el resto son 0
  expect_equal(sum(solucion), 1)
  expect_equal(solucion[indice_correcto], 1)
})

# Test 5: Verificar la diversidad de versiones generadas
test_that("Se generan al menos 150 versiones diferentes", {
  # Inicializar vector para almacenar "huellas digitales" de cada versión
  huellas_digitales <- character(N_VERSIONS)

  # Generar N_VERSIONS versiones diferentes
  for (i in 1:N_VERSIONS) {
    # Establecer semilla aleatoria
    set.seed(i)

    # Ejecutar el código R
    result <- run_rmd_r_code(rmd_file)
    env <- result$env

    # Crear una "huella digital" de esta versión
    huella <- paste(
      env$total_bolas,
      env$bolas_principal,
      env$bolas_secundario,
      env$bolas_extraer,
      env$min_bolas_ganar,
      env$color_principal,
      env$color_secundario,
      env$contexto,
      env$tipo_concurso,
      env$verbo_extraccion,
      env$frase_intento,
      env$frase_ganar,
      env$frase_al_menos,
      env$frase_calcular,
      env$probabilidad_correcta,
      paste(env$opciones_ordenadas, collapse = ","),
      env$indice_correcto,
      collapse = "|"
    )

    huellas_digitales[i] <- huella
  }

  # Contar cuántas huellas únicas hay
  n_unicas <- length(unique(huellas_digitales))

  # Verificar que hay al menos 150 versiones únicas
  expect_true(n_unicas >= 150)

  # Imprimir el número de versiones únicas
  cat("\nNúmero de versiones únicas generadas:", n_unicas, "de", N_VERSIONS, "intentos\n")
})

# Test 6: Verificar la compatibilidad con r-exams
test_that("El ejercicio es compatible con r-exams", {
  # Intentar generar un HTML con el ejercicio
  temp_dir <- tempdir()

  tryCatch({
    # Redirigir la salida para evitar desorden en la consola
    suppressMessages(utils::capture.output(
      exams2html(rmd_file, n = 1, dir = temp_dir)
    ))

    # Verificar que se ha generado un archivo HTML
    html_files <- list.files(temp_dir, pattern = "\\.html$", full.names = TRUE)
    expect_gte(length(html_files), 1,
               info = "No se generó ningún archivo HTML")

  }, error = function(e) {
    fail(paste("Error al generar HTML con r-exams:", e$message))
  }, finally = {
    # Limpiar archivos temporales
    unlink(list.files(temp_dir, full.names = TRUE), recursive = TRUE, force = TRUE)
  })
})

# --- Ejecutar todas las pruebas ---
cat("\n=== Ejecutando pruebas para", rmd_file, "===\n\n")

# Ejecutar pruebas individuales para identificar errores específicos
cat("Test 1: Verificar que el código R se ejecuta sin errores\n")
test_that("El código R se ejecuta sin errores", {
  result <- run_rmd_r_code(rmd_file)
  expect_null(result$error,
              info = paste("Error al ejecutar el código R:",
                           if(!is.null(result$error)) result$error$message else ""))
})

cat("Test 2: Verificar la coherencia matemática\n")
test_that("Los cálculos de probabilidad son matemáticamente coherentes", {
  result <- run_rmd_r_code(rmd_file)
  env <- result$env

  # Extraer variables relevantes
  total_bolas <- env$total_bolas
  bolas_principal <- env$bolas_principal
  bolas_secundario <- env$bolas_secundario
  bolas_extraer <- env$bolas_extraer
  min_bolas_ganar <- env$min_bolas_ganar
  probabilidad_correcta <- env$probabilidad_correcta
  casos_favorables <- env$casos_favorables
  casos_posibles <- env$casos_posibles

  # Imprimir valores para depuración
  cat("  Total bolas:", total_bolas, "\n")
  cat("  Bolas principales:", bolas_principal, "\n")
  cat("  Bolas secundarias:", bolas_secundario, "\n")
  cat("  Bolas a extraer:", bolas_extraer, "\n")
  cat("  Mínimo para ganar:", min_bolas_ganar, "\n")
  cat("  Casos favorables:", casos_favorables, "\n")
  cat("  Casos posibles:", casos_posibles, "\n")
  cat("  Probabilidad correcta:", probabilidad_correcta, "\n")

  # Verificar que las cantidades de bolas son coherentes
  cat("  Verificando: bolas_principal + bolas_secundario == total_bolas\n")
  expect_equal(bolas_principal + bolas_secundario, total_bolas)

  # Verificar que el número de bolas a extraer es válido
  cat("  Verificando: bolas_extraer <= total_bolas\n")
  expect_true(bolas_extraer <= total_bolas)

  # Verificar que el número mínimo de bolas para ganar es válido
  cat("  Verificando: min_bolas_ganar <= bolas_extraer\n")
  expect_true(min_bolas_ganar <= bolas_extraer)

  cat("  Verificando: min_bolas_ganar <= bolas_principal\n")
  expect_true(min_bolas_ganar <= bolas_principal)

  # Recalcular la probabilidad correcta para verificar
  recalc_casos_favorables <- 0
  for (i in min_bolas_ganar:min(bolas_extraer, bolas_principal)) {
    recalc_casos_favorables <- recalc_casos_favorables +
      choose(bolas_principal, i) * choose(bolas_secundario, bolas_extraer - i)
  }
  recalc_casos_posibles <- choose(total_bolas, bolas_extraer)
  recalc_probabilidad <- recalc_casos_favorables / recalc_casos_posibles

  cat("  Recalculado - Casos favorables:", recalc_casos_favorables, "\n")
  cat("  Recalculado - Casos posibles:", recalc_casos_posibles, "\n")
  cat("  Recalculado - Probabilidad:", recalc_probabilidad, "\n")

  # Verificar que los cálculos son correctos
  expect_equal(casos_favorables, recalc_casos_favorables,
               info = "El cálculo de casos favorables es incorrecto")
  expect_equal(casos_posibles, recalc_casos_posibles,
               info = "El cálculo de casos posibles es incorrecto")
  expect_equal(probabilidad_correcta, recalc_probabilidad,
               info = "El cálculo de la probabilidad es incorrecto")

  # Verificar que la probabilidad está en el rango [0,1]
  cat("  Verificando: probabilidad_correcta >= 0\n")
  expect_true(probabilidad_correcta >= 0)

  cat("  Verificando: probabilidad_correcta <= 1\n")
  expect_true(probabilidad_correcta <= 1)
})

cat("Test 3: Verificar opciones de respuesta\n")
test_that("Las opciones de respuesta son distintas", {
  result <- run_rmd_r_code(rmd_file)
  env <- result$env

  # Extraer opciones
  opciones_ordenadas <- env$opciones_ordenadas

  # Imprimir para depuración
  cat("  Opciones de respuesta:", paste(opciones_ordenadas, collapse = ", "), "\n")

  # Verificar que hay 4 opciones
  expect_equal(length(opciones_ordenadas), 4,
               info = "Debe haber exactamente 4 opciones de respuesta")

  # Verificar que todas las opciones son distintas
  expect_equal(length(unique(opciones_ordenadas)), 4,
               info = "Todas las opciones de respuesta deben ser distintas")

  # Verificar que todas las opciones están en el rango [0,1]
  expect_true(all(opciones_ordenadas >= 0 & opciones_ordenadas <= 1),
              info = "Todas las probabilidades deben estar en el rango [0,1]")
})

cat("Test 4: Verificar respuesta correcta\n")
test_that("La respuesta correcta está marcada correctamente", {
  result <- run_rmd_r_code(rmd_file)
  env <- result$env

  # Extraer variables relevantes
  probabilidad_correcta <- env$probabilidad_correcta
  opciones_ordenadas <- env$opciones_ordenadas
  indice_correcto <- env$indice_correcto
  solucion <- env$solucion

  # Imprimir para depuración
  cat("  Probabilidad correcta:", probabilidad_correcta, "\n")
  cat("  Índice correcto:", indice_correcto, "\n")
  cat("  Opción en índice correcto:", opciones_ordenadas[indice_correcto], "\n")
  cat("  Vector solución:", paste(solucion, collapse = ", "), "\n")

  # Verificar que la opción marcada como correcta es realmente la correcta
  cat("  Verificando: opciones_ordenadas[indice_correcto] == probabilidad_correcta\n")
  expect_equal(opciones_ordenadas[indice_correcto], probabilidad_correcta)

  # Verificar que el vector de solución tiene un único 1 y el resto son 0
  cat("  Verificando: sum(solucion) == 1\n")
  expect_equal(sum(solucion), 1)

  cat("  Verificando: solucion[indice_correcto] == 1\n")
  expect_equal(solucion[indice_correcto], 1)
})

# Ejecutar prueba de diversidad solo si las anteriores pasan
cat("Test 5: Verificar diversidad (versión reducida para depuración)\n")
test_that("Se generan versiones diferentes", {
  # Usar menos versiones para depuración
  n_test <- 10
  huellas_digitales <- character(n_test)

  # Generar versiones diferentes
  for (i in 1:n_test) {
    set.seed(i)
    result <- run_rmd_r_code(rmd_file)
    env <- result$env

    huella <- paste(
      env$total_bolas,
      env$bolas_principal,
      env$color_principal,
      env$probabilidad_correcta,
      collapse = "|"
    )

    huellas_digitales[i] <- huella
  }

  n_unicas <- length(unique(huellas_digitales))
  cat("  Versiones únicas generadas:", n_unicas, "de", n_test, "\n")

  # Verificar que hay versiones únicas
  expect_gt(n_unicas, 1,
            info = "No se generaron versiones únicas")
})

cat("\n=== Fin de las pruebas ===\n")
