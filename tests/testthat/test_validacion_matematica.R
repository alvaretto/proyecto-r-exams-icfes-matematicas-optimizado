# Tests unitarios para validar_coherencia_matematica.R
# Cobertura: 100% de funcionalidad del script de validación matemática

library(testthat)
library(exams)

# Cargar funciones UNA vez (source() es seguro gracias al guard sys.nframe())
source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")

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

# La excepción de exshuffle = FALSE (regla #4) debe sobrevivir al texto
# alternativo de las opciones. Un test por forma de alt: con las dos formas en
# el mismo archivo, any() dejaba pasar la que fallaba.
rmd_opciones_graficas <- function(lineas_answerlist) {
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data generation, echo = FALSE, results = \"hide\"}",
    "alt_op <- c(\"Gráfica de barras agrupadas\", \"Gráfica de barras apiladas\")",
    "```",
    "",
    "Question",
    "========",
    "Test",
    "",
    "Answerlist",
    "----------",
    lineas_answerlist,
    "",
    "Solution",
    "========",
    "Test",
    "",
    "Meta-information",
    "================",
    "exname: test_shuffle_graficos_alt",
    "extype: schoice",
    "exsolution: 10",
    "exshuffle: FALSE"
  ), temp_file)
  temp_file
}

test_that("Excepción de exshuffle con alt calculado en línea (`r alt_op[i]`)", {
  temp_file <- rmd_opciones_graficas(c(
    "* ![`r alt_op[1]`](diagrama_a.png){width=60%}&#8203;",
    "* ![`r alt_op[2]`](diagrama_b.png){width=60%}&#8203;"
  ))
  result <- validar_coherencia_matematica(temp_file)
  expect_false(any(grepl("exshuffle", result$errores, ignore.case = TRUE)),
    info = "el alt `r alt_op[i]` no debe desactivar la excepción de la regla #4")
  unlink(temp_file)
})

test_that("Excepción de exshuffle con alt literal", {
  temp_file <- rmd_opciones_graficas(c(
    "* ![Gráfica de barras agrupadas](diagrama_a.png){width=60%}&#8203;",
    "* ![Gráfica de barras apiladas](diagrama_b.png){width=60%}&#8203;"
  ))
  result <- validar_coherencia_matematica(temp_file)
  expect_false(any(grepl("exshuffle", result$errores, ignore.case = TRUE)),
    info = "el alt literal no debe desactivar la excepción de la regla #4")
  unlink(temp_file)
})

test_that("Una imagen diagrama_* que no es opción no activa la excepción de exshuffle", {
  # Control negativo: SCHOICE de TEXTO con exshuffle: FALSE y una figura de contexto
  # en el enunciado. Debe seguir dando ERR_C4; mata a los mutantes que aceptan
  # cualquier imagen o cualquier "diagrama_".
  temp_file <- rmd_opciones_graficas(c("* Opción 1", "* Opción 2"))
  lineas <- readLines(temp_file, encoding = "UTF-8")
  i <- which(lineas == "Test")[1]
  lineas <- append(lineas, "![Mapa del barrio](diagrama_contexto.png){width=60%}&#8203;", after = i)
  writeLines(lineas, temp_file)
  result <- validar_coherencia_matematica(temp_file)
  expect_true(any(grepl("exshuffle", result$errores, ignore.case = TRUE)),
    info = "una figura de contexto no convierte un SCHOICE de texto en uno de opciones gráficas")
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
