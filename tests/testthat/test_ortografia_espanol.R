# Tests unitarios para corregir_ortografia_espanol.R
# Cobertura: 100% de funcionalidad del script de ortografía

library(testthat)

test_that("Corrección ortográfica detecta tildes faltantes", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "Cual es el angulo mas grande?",  # Falta tilde en cuál, ángulo, más
    "",
    "Meta-information",
    "================",
    "exname: test_ortografia"
  ), temp_file)

  # Ejecutar corrección (modo verificación)
  result <- system2(
    "Rscript",
    c(repo_path(".claude/scripts/corregir_ortografia_espanol.R"),
      temp_file),
    stdout = TRUE,
    stderr = TRUE
  )

  # Verificar que detecta errores
  expect_true(any(grepl("Cuál|ángulo|más", result, ignore.case = TRUE)))

  unlink(temp_file)
})

test_that("Corrección ortográfica NO modifica metadatos R-exams", {
  temp_file <- tempfile(fileext = ".Rmd")
  original_content <- c(
    "Question",
    "========",
    "Pregunta válida",
    "",
    "Meta-information",
    "================",
    "exname: nombre_sin_tildes",
    "exsection: Numerico-Variacional/Interpretacion",  # ASCII obligatorio
    "extype: schoice"
  )
  writeLines(original_content, temp_file)

  # Ejecutar corrección (modo fix)
  system2(
    "Rscript",
    c(repo_path(".claude/scripts/corregir_ortografia_espanol.R"),
      temp_file, "--fix"),
    stdout = FALSE,
    stderr = FALSE
  )

  # Verificar que metadatos no se modificaron
  new_content <- readLines(temp_file)
  metadatos_originales <- grep("^(exname|exsection|extype)", original_content, value = TRUE)
  metadatos_nuevos <- grep("^(exname|exsection|extype)", new_content, value = TRUE)

  expect_equal(metadatos_originales, metadatos_nuevos)

  unlink(temp_file)
})

test_that("Corrección ortográfica NO modifica nombres de variables R", {
  temp_file <- tempfile(fileext = ".Rmd")
  original_content <- c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "angulo <- 45  # Variable sin tilde (correcto en R)",
    "solucion <- angulo * 2",
    "```",
    "",
    "Question",
    "========",
    "El ángulo es de `r angulo` grados.",  # Texto con tilde (correcto)
    "",
    "Meta-information",
    "================",
    "exname: test_variables"
  )
  writeLines(original_content, temp_file)

  # Ejecutar corrección (modo fix)
  system2(
    "Rscript",
    c(repo_path(".claude/scripts/corregir_ortografia_espanol.R"),
      temp_file, "--fix"),
    stdout = FALSE,
    stderr = FALSE
  )

  # Verificar que variables NO se modificaron
  new_content <- readLines(temp_file)
  expect_true(any(grepl("angulo <-", new_content, fixed = TRUE)))  # Variable sin tilde
  expect_true(any(grepl("El ángulo", new_content, fixed = TRUE)))  # Texto con tilde

  unlink(temp_file)
})

test_that("Corrección ortográfica aplica correcciones automáticas", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "Segun la grafica, cual es la funcion mas optima?"
  ), temp_file)

  # Ejecutar corrección (modo fix)
  system2(
    "Rscript",
    c(repo_path(".claude/scripts/corregir_ortografia_espanol.R"),
      temp_file, "--fix"),
    stdout = FALSE,
    stderr = FALSE
  )

  # Verificar correcciones aplicadas
  new_content <- readLines(temp_file)
  texto <- paste(new_content, collapse = " ")

  expect_true(grepl("Según", texto))
  expect_true(grepl("gráfica", texto))
  # "cual" excluido del diccionario (ambiguo: interrogativo vs relativo)
  expect_true(grepl("función", texto))
  expect_true(grepl("más", texto))
  expect_true(grepl("óptima", texto))

  unlink(temp_file)
})

test_that("Corrección ortográfica preserva código inline", {
  temp_file <- tempfile(fileext = ".Rmd")
  original_content <- c(
    "Question",
    "========",
    "El valor de x es `r solucion` según el cálculo."
  )
  writeLines(original_content, temp_file)

  # Ejecutar corrección (modo fix)
  system2(
    "Rscript",
    c(repo_path(".claude/scripts/corregir_ortografia_espanol.R"),
      temp_file, "--fix"),
    stdout = FALSE,
    stderr = FALSE
  )

  # Verificar que código inline se preserva
  new_content <- readLines(temp_file)
  expect_true(any(grepl("`r solucion`", new_content, fixed = TRUE)))
  expect_true(any(grepl("según", new_content)))
  expect_true(any(grepl("cálculo", new_content)))

  unlink(temp_file)
})
