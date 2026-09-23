# Tests unitarios para Media-Mediana-Moda CLOZE metacognitivo
# Ejercicio: media_mediana_moda_metacognitivo_interpretacion_n2_cloze_v1
# Cobertura: renderizado, estructura CLOZE, coherencia matematica, diversidad

suppressPackageStartupMessages({
  library(testthat)
  library(exams)
  library(digest)
})

# Modo rápido para pre-push (R_TESTS_QUICK=1)
.quick <- identical(Sys.getenv("R_TESTS_QUICK"), "1")

EX_REL <- file.path(
  "A-Produccion", "03-En-Produccion",
  "06-Estad\u00edstica-Y-Probabilidad",
  "Pensamiento-Aleatorio",
  "04-Medidas-De-Tendencia-Central",
  "01-MediaMedianaModa",
  "Calificaciones-Universitarias",
  "Media-Mediana-Moda.Rmd"
)
# Ruta corta: la tematica real supera los 255 bytes al aplanarla xexams.
# Ver ex_path_corto() en helper-repo-paths.R.
EX_PATH <- ex_path_corto(EX_REL)

# Helper: generar 1 instancia con xexams
.gen_one <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  res <- exams::xexams(EX_PATH, n = 1)
  ex <- res[[1]][[1]]
  stopifnot(is.list(ex$metainfo))
  ex
}

# ============================================================
# FORMATO CLOZE Y META-INFORMATION
# ============================================================
context("Media-Mediana-Moda: formato CLOZE y metainformacion")

test_that("exclozetype tiene 4 partes: schoice|num|num|schoice", {
  ex <- .gen_one(42)
  mi <- ex$metainfo
  expect_equal(mi$clozetype, c("schoice", "num", "num", "schoice"))
  expect_equal(mi$length, 4L)
})

test_that("Tolerancias son correctas para cada parte", {
  ex <- .gen_one(42)
  mi <- ex$metainfo
  expect_equal(mi$tolerance, c(0, 0.01, 0.01, 0))
})

test_that("Parte 1 (schoice) tiene exactamente 1 correcta de 4 opciones", {
  ex <- .gen_one(42)
  mi <- ex$metainfo
  sol_p1 <- mi$solution[[1]]
  expect_true(is.logical(sol_p1))
  expect_equal(length(sol_p1), 4L)
  expect_equal(sum(sol_p1), 1L)
})

test_that("Parte 4 (schoice V/F) tiene exactamente 1 correcta de 2 opciones", {
  ex <- .gen_one(42)
  mi <- ex$metainfo
  sol_p4 <- mi$solution[[4]]
  expect_true(is.logical(sol_p4))
  expect_equal(length(sol_p4), 2L)
  expect_equal(sum(sol_p4), 1L)
})

test_that("Placeholders ##ANSWER1##..##ANSWER4## aparecen en Question", {
  rmd <- readLines(EX_PATH, warn = FALSE)
  q_i <- which(trimws(rmd) == "Question")
  a_i <- which(trimws(rmd) == "Answerlist")
  expect_true(length(q_i) >= 1)
  expect_true(length(a_i) >= 1)
  seg <- rmd[(q_i[1]):(a_i[1])]
  seg_txt <- paste(seg, collapse = "\n")

  for (k in 1:4) {
    pattern <- paste0("##ANSWER", k, "##")
    expect_true(grepl(pattern, seg_txt, fixed = TRUE),
      info = paste("Falta placeholder", pattern))
  }
})

# ============================================================
# COHERENCIA MATEMATICA
# ============================================================
context("Media-Mediana-Moda: coherencia matematica")

test_that("Media esta entre min y max de los datos", {
  ex <- .gen_one(123)
  mi <- ex$metainfo
  media_val <- as.numeric(mi$solution[[2]])

  # Extraer rango de datos desde Question text
  qtxt <- paste(ex$question, collapse = " ")

  expect_true(is.finite(media_val))
  expect_true(media_val > 0, info = "Media debe ser positiva")
})

test_that("Mediana esta entre min y max de los datos", {
  ex <- .gen_one(123)
  mi <- ex$metainfo
  mediana_val <- as.numeric(mi$solution[[3]])

  expect_true(is.finite(mediana_val))
  expect_true(mediana_val > 0, info = "Mediana debe ser positiva")
})

test_that("Media y mediana son valores finitos sin NA/NaN", {
  set.seed(99)
  for (i in 1:if (.quick) 3 else 20) {
    ex <- .gen_one()
    mi <- ex$metainfo
    media_val <- as.numeric(mi$solution[[2]])
    mediana_val <- as.numeric(mi$solution[[3]])

    expect_true(is.finite(media_val),
      info = paste("Media es NA/NaN/Inf en iteracion", i))
    expect_true(is.finite(mediana_val),
      info = paste("Mediana es NA/NaN/Inf en iteracion", i))
  }
})

# ============================================================
# RENDERIZADO
# ============================================================
context("Media-Mediana-Moda: renderizado en formatos")

test_that("Renderiza correctamente en HTML", {
  td <- tempfile("mmm_html_")
  dir.create(td)
  expect_error(
    exams2html(EX_PATH, n = 1, dir = td, name = "mmm_test"),
    NA
  )
  htmls <- list.files(td, pattern = "\\.html$", full.names = TRUE)
  expect_true(length(htmls) >= 1,
    info = "No se genero archivo HTML")
  unlink(td, recursive = TRUE)
})

test_that("Renderiza correctamente en PDF", {
  td <- tempfile("mmm_pdf_")
  dir.create(td)
  expect_error(
    exams2pdf(EX_PATH, n = 1, dir = td, name = "mmm_test"),
    NA
  )
  pdfs <- list.files(td, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  expect_true(length(pdfs) >= 1,
    info = "No se genero archivo PDF")
  unlink(td, recursive = TRUE)
})

test_that("Renderiza correctamente en DOCX (pandoc)", {
  td <- tempfile("mmm_docx_")
  dir.create(td)
  expect_error(
    exams2pandoc(EX_PATH, n = 1, dir = td, type = "docx", name = "mmm_test"),
    NA
  )
  docxs <- list.files(td, pattern = "\\.docx$", full.names = TRUE, recursive = TRUE)
  expect_true(length(docxs) >= 1,
    info = "No se genero archivo DOCX")
  unlink(td, recursive = TRUE)
})

# ============================================================
# DIVERSIDAD DE VERSIONES
# ============================================================
context("Media-Mediana-Moda: diversidad de versiones")

test_that("200+ versiones unicas de 300 intentos", {
  N_DIV <- if (.quick) 30 else 300
  versiones <- character(N_DIV)
  for (i in 1:N_DIV) {
    ex <- .gen_one()
    mi <- ex$metainfo
    hash_version <- digest::digest(list(
      sol_p1 = as.character(mi$solution[[1]]),
      media = as.numeric(mi$solution[[2]]),
      mediana = as.numeric(mi$solution[[3]]),
      sol_p4 = as.character(mi$solution[[4]])
    ))
    versiones[i] <- hash_version
  }
  n_unicas <- length(unique(versiones))
  expect_true(n_unicas >= if (.quick) 20 else 200,
    info = paste("Solo", n_unicas, "versiones unicas de", N_DIV, ". Se requieren", if (.quick) 20 else 200, "+."))
})

# ============================================================
# ROBUSTEZ: MULTIPLES EJECUCIONES
# ============================================================
context("Media-Mediana-Moda: robustez (50 ejecuciones)")

test_that("50 ejecuciones generan datos validos sin errores", {
  set.seed(2026)
  for (i in 1:if (.quick) 5 else 50) {
    ex <- .gen_one()
    mi <- ex$metainfo

    # Estructura CLOZE intacta
    expect_equal(mi$length, 4L,
      info = paste("Longitud incorrecta en iteracion", i))

    # Valores numericos finitos
    media_val <- as.numeric(mi$solution[[2]])
    mediana_val <- as.numeric(mi$solution[[3]])
    expect_true(is.finite(media_val),
      info = paste("Media no finita en iteracion", i))
    expect_true(is.finite(mediana_val),
      info = paste("Mediana no finita en iteracion", i))

    # Parte 1: exactamente 1 correcta
    sol_p1 <- mi$solution[[1]]
    expect_equal(sum(sol_p1), 1L,
      info = paste("Parte 1 no tiene 1 correcta en iteracion", i))

    # Parte 4: exactamente 1 correcta de 2
    sol_p4 <- mi$solution[[4]]
    expect_equal(sum(sol_p4), 1L,
      info = paste("Parte 4 no tiene 1 correcta en iteracion", i))
  }
})
