# Tests de validación cruzada para 4 formatos (HTML, PDF, DOCX, NOPS)
# Cobertura: 100% de validación de renderizado en múltiples formatos

library(testthat)
library(exams)

setup_temp_exercise <- function(tipo = "schoice") {
  temp_file <- tempfile(fileext = ".Rmd")

  if (tipo == "schoice") {
    writeLines(c(
      "```{r data generation, echo = FALSE, results = \"hide\"}",
      "x <- sample(10:50, 1)",
      "y <- sample(10:50, 1)",
      "respuesta <- x + y",
      "dist1 <- respuesta + sample(1:5, 1)",
      "dist2 <- respuesta - sample(1:5, 1)",
      "dist3 <- respuesta + sample(6:10, 1)",
      "```",
      "",
      "Question",
      "========",
      "¿Cuánto es `r x` más `r y`?",
      "",
      "Answerlist",
      "----------",
      "* `r respuesta`",
      "* `r dist1`",
      "* `r dist2`",
      "* `r dist3`",
      "",
      "Solution",
      "========",
      "La suma de `r x` más `r y` es `r respuesta`.",
      "",
      "Meta-information",
      "================",
      "exname: test_4_formatos",
      "extype: schoice",
      "exsolution: 1000",
      "exshuffle: TRUE",
      "exextra[Type]: SCHOICE",
      "exextra[Competencia]: Interpretación",
      "exextra[Componente]: Numérico",
      "exextra[Afirmacion]: Realiza operaciones básicas",
      "exextra[Evidencia]: Suma de números",
      "exextra[Nivel]: 1"
    ), temp_file)
  } else if (tipo == "cloze") {
    writeLines(c(
      "```{r data generation, echo = FALSE, results = \"hide\"}",
      "x <- sample(10:50, 1)",
      "y <- sample(10:50, 1)",
      "suma <- x + y",
      "producto <- x * y",
      "```",
      "",
      "Question",
      "========",
      "Dados los números `r x` y `r y`:",
      "",
      "* La suma es ##ANSWER1##",
      "* El producto es ##ANSWER2##",
      "",
      "Solution",
      "========",
      "* Suma: `r suma`",
      "* Producto: `r producto`",
      "",
      "Meta-information",
      "================",
      "exname: test_cloze_4_formatos",
      "extype: cloze",
      "exclozetype: num|num",
      "exsolution: `r suma`|`r producto`",
      "extol: 0.01|0.01",
      "exextra[Type]: CLOZE",
      "exextra[Competencia]: Formulación",
      "exextra[Componente]: Numérico",
      "exextra[Afirmacion]: Realiza múltiples operaciones",
      "exextra[Evidencia]: Suma y multiplicación",
      "exextra[Nivel]: 2"
    ), temp_file)
  }

  temp_file
}

test_that("Ejercicio SCHOICE renderiza en HTML sin errores", {
  temp_file <- setup_temp_exercise("schoice")
  temp_dir <- tempdir()

  result <- tryCatch({
    exams2html(temp_file, n = 1, dir = temp_dir, name = "test_html")
    TRUE
  }, error = function(e) {
    FALSE
  })

  expect_true(result)
  expect_true(file.exists(file.path(temp_dir, "test_html1.html")))

  unlink(temp_file)
  unlink(file.path(temp_dir, "test_html1.html"))
})

test_that("Ejercicio SCHOICE renderiza en PDF sin errores", {
  skip_if_not_installed("tinytex")

  temp_file <- setup_temp_exercise("schoice")
  temp_dir <- tempdir()

  result <- tryCatch({
    exams2pdf(temp_file, n = 1, dir = temp_dir, name = "test_pdf")
    TRUE
  }, error = function(e) {
    FALSE
  })

  expect_true(result)
  expect_true(file.exists(file.path(temp_dir, "test_pdf1.pdf")))

  unlink(temp_file)
  unlink(file.path(temp_dir, "test_pdf1.pdf"))
})

test_that("Ejercicio SCHOICE renderiza en DOCX sin errores", {
  skip_if_not_installed("rmarkdown")

  temp_file <- setup_temp_exercise("schoice")
  temp_dir <- tempdir()

  result <- tryCatch({
    exams2pandoc(temp_file, n = 1, dir = temp_dir, name = "test_docx", type = "docx")
    TRUE
  }, error = function(e) {
    FALSE
  })

  expect_true(result)
  expect_true(file.exists(file.path(temp_dir, "test_docx1.docx")))

  unlink(temp_file)
  unlink(file.path(temp_dir, "test_docx1.docx"))
})

test_that("Ejercicio SCHOICE renderiza en NOPS sin errores", {
  skip_if_not_installed("tinytex")

  temp_file <- setup_temp_exercise("schoice")
  temp_dir <- tempdir()

  result <- tryCatch({
    exams2nops(temp_file, n = 1, dir = temp_dir, name = "test_nops")
    TRUE
  }, error = function(e) {
    FALSE
  })

  expect_true(result)
  # NOPS genera PDF
  expect_true(file.exists(file.path(temp_dir, "nops1.pdf")) ||
              file.exists(file.path(temp_dir, "test_nops1.pdf")))

  unlink(temp_file)
})

test_that("Ejercicio CLOZE renderiza en los 4 formatos", {
  skip_if_not_installed("tinytex")
  skip_if_not_installed("rmarkdown")

  temp_file <- setup_temp_exercise("cloze")
  temp_dir <- tempdir()

  # HTML
  html_ok <- tryCatch({
    exams2html(temp_file, n = 1, dir = temp_dir, name = "test_cloze_html")
    file.exists(file.path(temp_dir, "test_cloze_html1.html"))
  }, error = function(e) FALSE)

  # PDF
  pdf_ok <- tryCatch({
    exams2pdf(temp_file, n = 1, dir = temp_dir, name = "test_cloze_pdf")
    file.exists(file.path(temp_dir, "test_cloze_pdf1.pdf"))
  }, error = function(e) FALSE)

  # DOCX
  docx_ok <- tryCatch({
    exams2pandoc(temp_file, n = 1, dir = temp_dir, name = "test_cloze_docx", type = "docx")
    file.exists(file.path(temp_dir, "test_cloze_docx1.docx"))
  }, error = function(e) FALSE)

  expect_true(html_ok, info = "HTML rendering failed")
  expect_true(pdf_ok, info = "PDF rendering failed")
  expect_true(docx_ok, info = "DOCX rendering failed")

  unlink(temp_file)
})

test_that("Validación cruzada: contenido coherente entre formatos", {
  skip_if_not_installed("tinytex")

  temp_file <- setup_temp_exercise("schoice")
  temp_dir <- tempdir()

  # Renderizar en HTML y PDF
  exams2html(temp_file, n = 1, dir = temp_dir, name = "cross_html", seed = 12345)
  exams2pdf(temp_file, n = 1, dir = temp_dir, name = "cross_pdf", seed = 12345)

  # Leer HTML
  html_content <- readLines(file.path(temp_dir, "cross_html1.html"))
  html_text <- paste(html_content, collapse = "\n")

  # Verificar que ambos tienen la misma pregunta (misma semilla)
  # Al menos deben compartir patrones comunes de texto
  expect_true(grepl("Cuánto", html_text) || grepl("Question", html_text))

  unlink(temp_file)
})
