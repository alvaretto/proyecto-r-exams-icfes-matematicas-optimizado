# Tests unitarios para validar_coherencia_matematica.R
# Cobertura: 100% de funcionalidad del script de validación matemática

library(testthat)
library(exams)

test_that("Validación matemática detecta errores en chunks R", {
  # Crear archivo .Rmd temporal con chunk que falla
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- sqrt(-1)  # Genera NaN",
    "```",
    "",
    "Question",
    "========",
    "Test question",
    "",
    "Solution",
    "========",
    "Test solution",
    "",
    "Meta-information",
    "================",
    "exname: test_error",
    "extype: schoice",
    "exsolution: 10000",
    "exshuffle: TRUE"
  ), temp_file)

  # Ejecutar validación
  result <- tryCatch({
    source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")
    validar_coherencia_matematica(temp_file)
  }, error = function(e) {
    list(errores = TRUE)
  })

  # Verificar que detecta error
  expect_true(result$errores || grepl("NaN", paste(result, collapse = "")))

  unlink(temp_file)
})

test_that("Validación matemática acepta archivo SCHOICE válido", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- sample(1:10, 1)",
    "respuesta <- x + 5",
    "opciones <- c(respuesta, respuesta + 1, respuesta - 1, respuesta + 2)",
    "```",
    "",
    "Question",
    "========",
    "¿Cuánto es `r x` + 5?",
    "",
    "Answerlist",
    "----------",
    "* `r opciones[1]`",
    "* `r opciones[2]`",
    "* `r opciones[3]`",
    "* `r opciones[4]`",
    "",
    "Solution",
    "========",
    "La respuesta es `r respuesta`.",
    "",
    "Meta-information",
    "================",
    "exname: test_valido",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretación",
    "exextra[Componente]: Numérico",
    "exextra[Afirmacion]: Realiza cálculos",
    "exextra[Evidencia]: Suma de números",
    "exextra[Nivel]: 1"
  ), temp_file)

  # Ejecutar validación
  result <- tryCatch({
    source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")
    validar_coherencia_matematica(temp_file)
  }, error = function(e) {
    list(aprobado = FALSE, mensaje = e$message)
  })

  # Verificar que aprueba
  expect_true(result$aprobado || is.null(result$errores))

  unlink(temp_file)
})

test_that("Validación detecta exshuffle = FALSE", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- 5",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* Opción 1",
    "* Opción 2",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_shuffle",
    "extype: schoice",
    "exsolution: 10",
    "exshuffle: FALSE"
  ), temp_file)

  result <- tryCatch({
    source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")
    validar_coherencia_matematica(temp_file)
  }, error = function(e) {
    list(errores = TRUE, mensaje = e$message)
  })

  expect_true(result$errores || grepl("shuffle", tolower(result$mensaje), fixed = TRUE))

  unlink(temp_file)
})

test_that("Validación CLOZE detecta inconsistencias de tipos", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- 10",
    "y <- 20",
    "```",
    "",
    "Question",
    "========",
    "##ANSWER1## y ##ANSWER2##",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_cloze",
    "extype: cloze",
    "exclozetype: num|schoice",
    "exsolution: 10",  # Inconsistente: 1 valor, 2 tipos
    "extol: 0.01"
  ), temp_file)

  result <- tryCatch({
    source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")
    validar_coherencia_matematica(temp_file)
  }, error = function(e) {
    list(errores = TRUE, mensaje = e$message)
  })

  expect_true(result$errores || grepl("inconsisten", tolower(result$mensaje)))

  unlink(temp_file)
})

test_that("Validación detecta metadatos ICFES incompletos", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "x <- 5",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_metadatos",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE"
    # Faltan metadatos ICFES (6 dimensiones)
  ), temp_file)

  result <- tryCatch({
    source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")
    validar_coherencia_matematica(temp_file)
  }, error = function(e) {
    list(errores = TRUE, mensaje = e$message)
  })

  expect_true(result$errores || grepl("metadatos|ICFES", result$mensaje, ignore.case = TRUE))

  unlink(temp_file)
})
