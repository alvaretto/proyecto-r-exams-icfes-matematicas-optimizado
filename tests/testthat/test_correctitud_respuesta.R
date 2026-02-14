# Tests unitarios para Nivel 5: Validación de Correctitud de Respuesta
# Cobertura: 5A (exsolution dinámico), 5B (cross-check), 5C (unicidad),
#            5D (rangos), 5E (distractor vs correcto)

library(testthat)
library(exams)

# Cargar funciones
source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")

# ============================================================
# Nivel 5A: Evaluación de exsolution dinámico
# ============================================================

test_that("5A: Detecta exsolution dinámico con 0 correctas en SCHOICE", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "sol <- '0000'",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* A",
    "* B",
    "* C",
    "* D",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_5a_zero",
    "extype: schoice",
    "exsolution: `r sol`",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("ERR_ANS_A", result$errores)),
    info = "Debe detectar exsolution dinámico '0000' sin correctas")
  unlink(temp_file)
})

test_that("5A: Acepta exsolution dinámico válido en SCHOICE", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "sol <- '0100'",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* A",
    "* B",
    "* C",
    "* D",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_5a_ok",
    "extype: schoice",
    "exsolution: `r sol`",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_false(any(grepl("ERR_ANS_A", result$errores)),
    info = "No debe reportar error para exsolution dinámico válido '0100'")
  unlink(temp_file)
})

test_that("5A: Detecta exsolution dinámico con múltiples correctas", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "sol <- '1010'",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* A",
    "* B",
    "* C",
    "* D",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_5a_multi",
    "extype: schoice",
    "exsolution: `r sol`",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("ERR_ANS_A", result$errores)),
    info = "Debe detectar exsolution dinámico '1010' con 2 correctas")
  unlink(temp_file)
})

# ============================================================
# Nivel 5B: Cross-check respuesta marcada vs valor correcto
# ============================================================

test_that("5B: Detecta respuesta marcada incorrecta", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "valor_correcto <- 42",
    "opciones_mezcladas <- c(10, 20, 30, 40)",
    "sol <- c(1, 0, 0, 0)  # Marca posición 1 (valor=10) como correcta, pero correcto es 42",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* 10",
    "* 20",
    "* 30",
    "* 40",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_5b_wrong",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("ERR_ANS_B", result$errores)),
    info = "Debe detectar que opción marcada (10) ≠ valor_correcto (42)")
  unlink(temp_file)
})

test_that("5B: Aprueba respuesta marcada correcta", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "valor_correcto <- 30",
    "opciones_mezcladas <- c(10, 20, 30, 40)",
    "sol <- c(0, 0, 1, 0)  # Marca posición 3 (valor=30) = valor_correcto",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* 10",
    "* 20",
    "* 30",
    "* 40",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_5b_ok",
    "extype: schoice",
    "exsolution: 0010",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_false(any(grepl("ERR_ANS_B", result$errores)),
    info = "No debe reportar error cuando opción marcada = valor_correcto")
  unlink(temp_file)
})

# ============================================================
# Nivel 5C: Unicidad de opciones en runtime
# ============================================================

test_that("5C: Detecta opciones duplicadas en SCHOICE", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "opciones_mezcladas <- c(25, 25, 30, 40)  # Opciones 1 y 2 son iguales",
    "sol <- c(0, 0, 1, 0)",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* 25",
    "* 25",
    "* 30",
    "* 40",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_5c_dup",
    "extype: schoice",
    "exsolution: 0010",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("ERR_ANS_C", result$errores)),
    info = "Debe detectar opciones duplicadas (25, 25)")
  unlink(temp_file)
})

test_that("5C: Aprueba opciones únicas", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "opciones_mezcladas <- c(10, 20, 30, 40)  # Todas únicas",
    "sol <- c(0, 1, 0, 0)",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* 10",
    "* 20",
    "* 30",
    "* 40",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_5c_ok",
    "extype: schoice",
    "exsolution: 0100",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_false(any(grepl("ERR_ANS_C", result$errores)),
    info = "No debe reportar error para opciones únicas")
  unlink(temp_file)
})

# ============================================================
# Nivel 5D: Validación de rangos matemáticos
# ============================================================

test_that("5D: Detecta mediana fuera de rango", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "datos_ord <- c(10, 20, 30, 40, 50)",
    "mediana_calc <- 999  # Fuera del rango [10, 50]",
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
    "exname: test_5d_mediana",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("ERR_ANS_D", result$errores)),
    info = "Debe detectar mediana_calc=999 fuera de [10, 50]")
  unlink(temp_file)
})

test_that("5D: Detecta probabilidad negativa", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "prob_exito <- -0.5  # Probabilidad negativa",
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
    "exname: test_5d_prob",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("ERR_ANS_D", result$errores)),
    info = "Debe detectar probabilidad negativa (-0.5)")
  unlink(temp_file)
})

test_that("5D: Detecta cuartiles desordenados", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "datos_ord <- c(10, 20, 30, 40, 50, 60, 70, 80)",
    "cuartiles_correctos <- list(q1 = 50, mediana = 30, q3 = 20)  # Desordenados",
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
    "exname: test_5d_quartiles",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("ERR_ANS_D", result$errores)),
    info = "Debe detectar cuartiles desordenados (Q1=50 > Q2=30 > Q3=20)")
  unlink(temp_file)
})

# ============================================================
# Nivel 5E: Distractor ≠ respuesta correcta
# ============================================================

test_that("5E: Detecta distractor igual a respuesta correcta", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "opciones_mezcladas <- list(10, 20, 10, 40)  # Opción 1 (correcta) = Opción 3 (distractor)",
    "sol <- c(1, 0, 0, 0)",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* 10",
    "* 20",
    "* 10",
    "* 40",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_5e_dup",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("ERR_ANS_E", result$errores)),
    info = "Debe detectar distractor (pos 3) idéntico a correcta (pos 1)")
  unlink(temp_file)
})

test_that("5E: Aprueba distractores diferentes", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "opciones_mezcladas <- list(10, 20, 30, 40)  # Todos diferentes",
    "sol <- c(1, 0, 0, 0)",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* 10",
    "* 20",
    "* 30",
    "* 40",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_5e_ok",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_false(any(grepl("ERR_ANS_E", result$errores)),
    info = "No debe reportar error para distractores diferentes")
  unlink(temp_file)
})

# ============================================================
# Test de integración: Nivel 5 completo
# ============================================================

test_that("Nivel 5 completo: Archivo válido pasa todas las validaciones", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "datos_ord <- c(10, 20, 30, 40, 50)",
    "mediana_calc <- 30",
    "valor_correcto <- 30",
    "opciones_mezcladas <- c(20, 30, 40, 25)",
    "sol <- c(0, 1, 0, 0)",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* 20",
    "* 30",
    "* 40",
    "* 25",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_n5_ok",
    "extype: schoice",
    "exsolution: 0100",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)
  expect_false(any(grepl("ERR_ANS", result$errores)),
    info = paste("No debe haber errores Nivel 5. Errores:", paste(result$errores, collapse = "; ")))
  unlink(temp_file)
})

test_that("Nivel 5 completo: Múltiples errores simultáneos", {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "datos_ord <- c(10, 20, 30, 40, 50)",
    "mediana_calc <- 999  # ERR_ANS_D: fuera de rango",
    "valor_correcto <- 30",
    "opciones_mezcladas <- c(10, 20, 10, 40)  # ERR_ANS_C: duplicados + ERR_ANS_B: pos 1 ≠ 30",
    "sol <- c(1, 0, 0, 0)",
    "prob_exito <- -0.5  # ERR_ANS_D: probabilidad negativa",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    "* 10",
    "* 20",
    "* 10",
    "* 40",
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_n5_multi",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE",
    "exextra[Type]: SCHOICE",
    "exextra[Competencia]: Interpretacion",
    "exextra[Componente]: Numerico",
    "exextra[Afirmacion]: Test",
    "exextra[Evidencia]: Test",
    "exextra[Nivel]: 1"
  ), temp_file)

  result <- validar_coherencia_matematica(temp_file)

  # Debe detectar al menos 3 tipos de error Nivel 5
  err_n5 <- result$errores[grepl("ERR_ANS", result$errores)]
  expect_true(length(err_n5) >= 3,
    info = paste("Se esperan ≥3 errores Nivel 5, encontrados:", length(err_n5),
                 "—", paste(err_n5, collapse = "; ")))
  unlink(temp_file)
})
