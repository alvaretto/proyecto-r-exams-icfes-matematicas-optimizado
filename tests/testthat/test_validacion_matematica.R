# Tests unitarios para validar_coherencia_matematica.R
# Cobertura: 100% de funcionalidad del script de validación matemática

library(testthat)
library(exams)

# Cargar funciones UNA vez (source() es seguro gracias al guard sys.nframe())
source(repo_path(".claude/scripts/validar_coherencia_matematica.R"))

test_that("Validación matemática detecta errores en chunks R", {
  # Crear archivo .Rmd temporal con chunk que genera NaN
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

  result <- validar_coherencia_matematica(temp_file)

  # Verificar que detecta error (NaN en variable x)
  expect_false(result$aprobado)
  expect_true(length(result$errores) > 0)

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
    "Cuanto es `r x` + 5?",
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
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Realiza calculos",
    "exextra[Evidencia]: Suma de numeros",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)

  # Verificar que aprueba (archivo SCHOICE válido con metadatos completos)
  expect_true(result$aprobado)

  unlink(temp_file)
})

test_that("Validación detecta exshuffle = FALSE en opciones de texto", {
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
    "* Opcion 1",
    "* Opcion 2",
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

  result <- validar_coherencia_matematica(temp_file)

  expect_false(result$aprobado)
  expect_true(any(grepl("shuffle", result$errores, ignore.case = TRUE)))

  unlink(temp_file)
})

test_that("Validación acepta exshuffle = FALSE en SCHOICE con opciones gráficas PNG", {
  # Excepción: SCHOICE con opciones gráficas individuales (diagrama_*.png)
  # usa sample() interno + exshuffle:FALSE porque TRUE rompería la referencia
  # a letra_correcta en Solution. Ver .claude/rules/graficos-como-opciones.md
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
    "* ![](diagrama_a.png){width=60%}",
    "* ![](diagrama_b.png){width=60%}",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_shuffle_graficos",
    "extype: schoice",
    "exsolution: 10",
    "exshuffle: FALSE"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)

  # No debe reportar error de exshuffle porque tiene opciones gráficas PNG
  expect_false(any(grepl("exshuffle", result$errores, ignore.case = TRUE)),
    info = "exshuffle:FALSE debe ser aceptado en SCHOICE con opciones gráficas PNG")

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

  result <- validar_coherencia_matematica(temp_file)

  expect_false(result$aprobado)
  expect_true(length(result$errores) > 0)

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

  result <- validar_coherencia_matematica(temp_file)

  expect_false(result$aprobado)
  expect_true(any(grepl("ICFES", result$errores)))

  unlink(temp_file)
})
