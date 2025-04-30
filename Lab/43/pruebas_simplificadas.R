library(testthat)
library(knitr)
library(exams)

# Definir archivo a probar
rmd_file <- "probabilidad_extraccion_bolas_v1.Rmd"

# Función para extraer y ejecutar código R del Rmd
run_rmd_code <- function(file_path) {
  # Crear archivo temporal para el código R
  temp_r_file <- tempfile(fileext = ".R")
  on.exit(unlink(temp_r_file, force = TRUE), add = TRUE)
  
  # Extraer código R
  suppressMessages(
    knitr::purl(file_path, output = temp_r_file, documentation = 0, quiet = TRUE)
  )
  
  # Ejecutar código en un nuevo entorno
  env <- new.env()
  source(temp_r_file, local = env)
  
  return(env)
}

# Prueba 1: Verificar coherencia matemática
test_that("Coherencia matemática", {
  # Ejecutar código
  env <- run_rmd_code(rmd_file)
  
  # Extraer variables
  total_bolas <- env$total_bolas
  bolas_principal <- env$bolas_principal
  bolas_secundario <- env$bolas_secundario
  bolas_extraer <- env$bolas_extraer
  min_bolas_ganar <- env$min_bolas_ganar
  probabilidad_correcta <- env$probabilidad_correcta
  
  # Imprimir valores para depuración
  cat("\nValores generados:\n")
  cat("Total bolas:", total_bolas, "\n")
  cat("Bolas principales:", bolas_principal, "\n")
  cat("Bolas secundarias:", bolas_secundario, "\n")
  cat("Bolas a extraer:", bolas_extraer, "\n")
  cat("Mínimo para ganar:", min_bolas_ganar, "\n")
  cat("Probabilidad correcta:", probabilidad_correcta, "\n")
  
  # Verificaciones básicas
  expect_true(bolas_principal + bolas_secundario == total_bolas)
  expect_true(bolas_extraer <= total_bolas)
  expect_true(min_bolas_ganar <= bolas_extraer)
  expect_true(min_bolas_ganar <= bolas_principal)
  expect_true(probabilidad_correcta >= 0 && probabilidad_correcta <= 1)
})

# Prueba 2: Verificar opciones de respuesta
test_that("Opciones de respuesta", {
  # Ejecutar código
  env <- run_rmd_code(rmd_file)
  
  # Extraer opciones
  opciones_ordenadas <- env$opciones_ordenadas
  
  # Imprimir opciones
  cat("\nOpciones de respuesta:", paste(opciones_ordenadas, collapse = ", "), "\n")
  
  # Verificaciones
  expect_true(length(opciones_ordenadas) == 4)
  expect_true(length(unique(opciones_ordenadas)) == 4)
  expect_true(all(opciones_ordenadas >= 0 & opciones_ordenadas <= 1))
})

# Prueba 3: Verificar diversidad (versión reducida)
test_that("Diversidad de versiones", {
  # Usar menos versiones para prueba rápida
  n_test <- 10
  huellas <- character(n_test)
  
  for (i in 1:n_test) {
    set.seed(i)
    env <- run_rmd_code(rmd_file)
    
    # Crear huella digital simplificada
    huella <- paste(
      env$total_bolas,
      env$bolas_principal,
      env$color_principal,
      env$probabilidad_correcta,
      collapse = "|"
    )
    
    huellas[i] <- huella
  }
  
  n_unicas <- length(unique(huellas))
  cat("\nVersiones únicas:", n_unicas, "de", n_test, "\n")
  
  # Verificar que hay al menos algunas versiones únicas
  expect_true(n_unicas > 1)
})

# Ejecutar pruebas
cat("\n=== Ejecutando pruebas para", rmd_file, "===\n")
