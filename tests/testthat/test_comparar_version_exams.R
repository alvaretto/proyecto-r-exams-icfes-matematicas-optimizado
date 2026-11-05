# =============================================================================
# Test del comparador de versiones de R/exams (comparar_version_exams.R)
# =============================================================================
# El comparador responde «¿qué cambia si actualizo?» antes de actualizar. Sus dos
# riesgos: atribuir a la versión lo que es azar del ejercicio (de ahí NO_DETERMINISTA,
# medido en mediana_salas: set.seed con Sys.time) y dar por buena una candidata que
# no llegó a cargarse.
#
# Para no depender de tener dos versiones instaladas, la «candidata» es un directorio
# vacío con un archivo MARCADOR. Los fixtures miran si está en `.libPaths()[1]`: así
# un mismo ejercicio se comporta distinto con cada biblioteca, de forma determinista.

library(testthat)

RAIZ <- tryCatch({
  r <- suppressWarnings(system("git rev-parse --show-toplevel", intern = TRUE, ignore.stderr = TRUE))
  if (length(r) == 1L && dir.exists(r)) r else normalizePath(file.path("..", ".."))
}, error = function(e) normalizePath(file.path("..", "..")))

SCRIPT <- file.path(RAIZ, ".claude", "scripts", "comparar_version_exams.R")
RSCRIPT <- file.path(R.home("bin"), "Rscript")
E <- new.env()
sys.source(SCRIPT, envir = E)

j <- function(...) list(semillas = lapply(c(...), function(h) list(huella = h)))

test_that("clasificar_par: las seis clases", {
  expect_equal(E$clasificar_par(j("a", "b"), j("a", "b")), "IDENTICO")
  expect_equal(E$clasificar_par(j("a", "b"), j("a", "c"), j("a", "b")), "CAMBIA")
  expect_equal(E$clasificar_par(j("a", "b"), j("a", "c"), j("x", "y")), "NO_DETERMINISTA")
  expect_equal(E$clasificar_par(j("a"), j(NA_character_)), "FALLA_SOLO_CANDIDATA")
  expect_equal(E$clasificar_par(j(NA_character_), j("a")), "FALLA_SOLO_BASE")
  expect_equal(E$clasificar_par(j(NA_character_), j(NA_character_)), "FALLA_EN_AMBAS")
  # Sin JSON (el proceso murió) no es IDENTICO, aunque coincidan los NULL.
  expect_equal(E$clasificar_par(NULL, NULL), "FALLA_EN_AMBAS")
  expect_equal(E$clasificar_par(j("a"), NULL), "FALLA_SOLO_CANDIDATA")
})

test_that("veredicto_contenido: solo CAMBIA y FALLA_SOLO_CANDIDATA son regresión", {
  expect_equal(E$veredicto_contenido(c("IDENTICO", "NO_DETERMINISTA", "FALLA_SOLO_BASE", "FALLA_EN_AMBAS")), 0L)
  expect_equal(E$veredicto_contenido(c("IDENTICO", "CAMBIA")), 1L)
  expect_equal(E$veredicto_contenido(c("FALLA_SOLO_CANDIDATA")), 1L)
})

test_that("clasificar_formato: compila, no compila, texto y no aplica", {
  ok <- function(h) list(ok = TRUE, huella = h)
  no <- list(ok = FALSE, huella = NA_character_)
  expect_equal(E$clasificar_formato(NULL, NULL), "NO_APLICA")
  expect_equal(E$clasificar_formato(ok("a"), ok("a")), "IDENTICO")
  expect_equal(E$clasificar_formato(ok("a"), ok("b")), "TEXTO_DISTINTO")
  expect_equal(E$clasificar_formato(ok("a"), ok("b"), ok("a")), "TEXTO_DISTINTO")
  expect_equal(E$clasificar_formato(ok("a"), ok("b"), ok("c")), "NO_DETERMINISTA")
  expect_equal(E$clasificar_formato(ok("a"), no), "FALLA_SOLO_CANDIDATA")
  expect_equal(E$clasificar_formato(no, ok("a")), "FALLA_SOLO_BASE")
  expect_equal(E$clasificar_formato(no, no), "FALLA_EN_AMBAS")
})

test_that("veredicto_formatos: un cambio de texto pide revisión; un arreglo no", {
  expect_equal(E$veredicto_formatos(c("IDENTICO", "NO_APLICA", "NO_DETERMINISTA", "FALLA_SOLO_BASE", "FALLA_EN_AMBAS")), 0L)
  expect_equal(E$veredicto_formatos(c("IDENTICO", "TEXTO_DISTINTO")), 1L)
  expect_equal(E$veredicto_formatos(c("FALLA_SOLO_CANDIDATA")), 1L)
})

test_that("normalizar_texto: quita lo volátil y conserva el contenido", {
  expect_equal(E$normalizar_texto("<img src=\"data:image/png;base64,iVBOR/+=\">"), "<img src=\"data:image/png;base64,\">")
  expect_equal(E$normalizar_texto("<name>exam1234</name>"), "<name>ID</name>")
  # Imagen en Moodle: mismo archivo con otros bytes (el PNG guarda su fecha).
  a <- "<file name=\"d.png\" encoding=\"base64\">iVBORw0KAAAA</file>"
  b <- "<file name=\"d.png\" encoding=\"base64\">iVBORw0KBBBB</file>"
  expect_identical(E$normalizar_texto(a), E$normalizar_texto(b))
  # Otro nombre de archivo sí es un cambio.
  expect_false(identical(E$normalizar_texto(a), E$normalizar_texto(sub("d.png", "e.png", b, fixed = TRUE))))
  # pdftotext -layout reparte los espacios según la línea.
  expect_identical(E$normalizar_texto("001     26010100001"), E$normalizar_texto("001  26010100001"))
  expect_false(identical(E$normalizar_texto("La respuesta es 3"), E$normalizar_texto("La respuesta es 4")))
})

test_that("es_ejercicio: fuera lo generado, las copias y lo que no tiene metadatos", {
  d <- tempfile("corpus_")
  dir.create(file.path(d, "salida"), recursive = TRUE)
  meta <- c("Question", "========", "x", "", "Meta-information", "================", "extype: num", "exsolution: 1")
  e <- function(n) { f <- file.path(d, n); writeLines(meta, f); f }
  expect_true(E$es_ejercicio(e("ejercicio.Rmd")))
  expect_false(E$es_ejercicio(e("ejercicio_interactivo.Rmd")))
  expect_false(E$es_ejercicio(e(".backup_2.Rmd")))
  expect_false(E$es_ejercicio(e(file.path("salida", "otro.Rmd"))))
  f <- file.path(d, "demo.Rmd")
  writeLines(c("---", "title: demo", "---", "texto"), f)
  expect_false(E$es_ejercicio(f))
  expect_equal(length(E$listar_ejercicios(d)), 1L)
})

test_that("elegir_muestra: reproducible, por zona y sin pasarse del corpus", {
  rutas <- c(sprintf("A-Produccion/01-En-PreDesarrollo/e%d.Rmd", 1:60),
             sprintf("A-Produccion/03-En-Produccion/e%d.Rmd", 1:40))
  a <- E$elegir_muestra(rutas, 10L, 1L)
  expect_equal(length(a), 10L)
  expect_identical(a, E$elegir_muestra(rutas, 10L, 1L))
  expect_equal(sum(grepl("01-En-PreDesarrollo", a)), 6L)
  expect_equal(sum(grepl("03-En-Produccion", a)), 4L)
  expect_equal(length(E$elegir_muestra(rutas, 500L, 1L)), 100L)
})

test_that("sin --lib válido: NO VERIFICABLE, nunca sin regresiones", {
  s <- suppressWarnings(system2(RSCRIPT, shQuote(SCRIPT), stdout = TRUE, stderr = TRUE))
  expect_equal(attr(s, "status"), 2L)
  expect_true(any(s == "COMPARAR_NO_VERIFICABLE"))
  s <- suppressWarnings(system2(RSCRIPT, c(shQuote(SCRIPT), "--lib", shQuote(file.path(tempdir(), "no_hay"))),
                                stdout = TRUE, stderr = TRUE))
  expect_equal(attr(s, "status"), 2L)
})

escribir <- function(d, nombre, datos) {
  writeLines(c(
    "```{r data_generation, echo=FALSE, results=\"hide\"}",
    "marcador <- file.exists(file.path(.libPaths()[1], 'MARCADOR'))",
    datos,
    "```",
    "",
    "Question",
    "========",
    "El dato es `r x`.",
    "",
    "Meta-information",
    "================",
    paste0("exname: ", nombre),
    "extype: num",
    "exsolution: `r x`",
    "extol: 0"
  ), file.path(d, paste0(nombre, ".Rmd")))
}

test_that("de punta a punta: cada clase con su ejercicio", {
  skip_if_not_installed("jsonlite")
  corpus <- tempfile("corpus_e2e_")
  dir.create(corpus)
  escribir(corpus, "igual", "x <- sample(1:9, 1)")
  escribir(corpus, "cambia", "x <- sample(1:9, 1) + marcador * 100")
  escribir(corpus, "azar", c("set.seed((as.numeric(Sys.time()) * 1000) %% 100000)", "x <- sample(1e6, 1)"))
  escribir(corpus, "rompe", c("if (marcador) stop('solo con la candidata')", "x <- 1"))
  lib <- tempfile("lib_cand_")
  dir.create(lib)
  file.create(file.path(lib, "MARCADOR"))
  s <- suppressWarnings(system2(RSCRIPT, c(shQuote(SCRIPT), "--lib", shQuote(lib), "--raiz", shQuote(corpus),
                                           "--todos", "--n", 2, "--jobs", 2), stdout = TRUE, stderr = TRUE))
  texto <- paste(s, collapse = "\n")
  expect_equal(attr(s, "status"), 1L)
  expect_true(any(s == "COMPARAR_REVISAR"))
  expect_true(grepl("Corpus: 4 ejercicios", texto, fixed = TRUE))
  expect_true(grepl("CAMBIA: .*cambia\\.Rmd", texto))
  expect_true(grepl("FALLA_SOLO_CANDIDATA: .*rompe\\.Rmd", texto))
  expect_true(grepl("NO_DETERMINISTA: .*azar\\.Rmd", texto))
  expect_false(grepl("(CAMBIA|NO_DETERMINISTA): .*igual\\.Rmd", texto))
})

test_that("candidata inocua: sin regresiones y con versión declarada", {
  skip_if_not_installed("jsonlite")
  corpus <- tempfile("corpus_ok_")
  dir.create(corpus)
  escribir(corpus, "igual", "x <- sample(1:9, 1)")
  lib <- tempfile("lib_vacia_")
  dir.create(lib)
  s <- suppressWarnings(system2(RSCRIPT, c(shQuote(SCRIPT), "--lib", shQuote(lib), "--raiz", shQuote(corpus),
                                           "--todos", "--n", 2, "--jobs", 1), stdout = TRUE, stderr = TRUE))
  expect_null(attr(s, "status"))
  expect_true(any(s == "COMPARAR_SIN_REGRESIONES"))
  v <- as.character(utils::packageVersion("exams"))
  expect_true(any(grepl(sprintf("exams base %s · candidata %s", v, v), s, fixed = TRUE)))
})

# --- Lo que no se repite no es regresión ------------------------------------------
# El diferencial de 2.4-2 a 2.4-5 (2026-11-05) dio dos FALLA_SOLO_CANDIDATA y un
# TEXTO_DISTINTO que eran fallos sueltos de LaTeX: no reaparecieron en tres pasadas, y
# el comparador solo repetía la base ante un texto distinto. Una falla o una diferencia
# cuenta si se repite; si no, queda en el informe como NO_SE_REPITE.

test_that("clasificar_par: una anomalía se confirma repitiendo los dos lados", {
  fallo <- j(NA_character_)
  expect_equal(E$clasificar_par(j("a"), fallo, j("a"), j("a")), "NO_SE_REPITE")
  expect_equal(E$clasificar_par(j("a"), fallo, j("a"), fallo), "FALLA_SOLO_CANDIDATA")
  expect_equal(E$clasificar_par(j("a"), fallo, j("a"), NULL), "FALLA_SOLO_CANDIDATA")
  expect_equal(E$clasificar_par(fallo, j("a"), j("a"), j("a")), "NO_SE_REPITE")
  expect_equal(E$clasificar_par(fallo, j("a"), fallo, j("a")), "FALLA_SOLO_BASE")
  expect_equal(E$clasificar_par(j("a"), j("b"), j("a"), j("a")), "NO_SE_REPITE")
  expect_equal(E$clasificar_par(j("a"), j("b"), j("a"), j("b")), "CAMBIA")
  expect_equal(E$clasificar_par(j("a"), j("b"), j("a"), j("c")), "NO_DETERMINISTA")
  expect_equal(E$clasificar_par(j("a"), j("b"), j("x"), j("b")), "NO_DETERMINISTA")
  expect_equal(E$clasificar_par(fallo, fallo, j("a"), j("a")), "FALLA_EN_AMBAS")
})

test_that("clasificar_formato: una anomalía se confirma repitiendo los dos lados", {
  ok <- function(h) list(ok = TRUE, huella = h)
  no <- list(ok = FALSE, huella = NA_character_, error = "LaTeX")
  expect_equal(E$clasificar_formato(ok("a"), no, ok("a"), ok("a")), "NO_SE_REPITE")
  expect_equal(E$clasificar_formato(ok("a"), no, ok("a"), no), "FALLA_SOLO_CANDIDATA")
  expect_equal(E$clasificar_formato(ok("a"), no, ok("a"), NULL), "FALLA_SOLO_CANDIDATA")
  expect_equal(E$clasificar_formato(no, ok("a"), ok("a"), ok("a")), "NO_SE_REPITE")
  expect_equal(E$clasificar_formato(no, ok("a"), no, ok("a")), "FALLA_SOLO_BASE")
  # El caso real de Moodle: base y su repetición iguales; la candidata difirió una vez.
  expect_equal(E$clasificar_formato(ok("a"), ok("b"), ok("a"), ok("a")), "NO_SE_REPITE")
  expect_equal(E$clasificar_formato(ok("a"), ok("b"), ok("a"), ok("b")), "TEXTO_DISTINTO")
  expect_equal(E$clasificar_formato(ok("a"), ok("b"), ok("a"), ok("c")), "NO_DETERMINISTA")
  expect_equal(E$clasificar_formato(ok("a"), ok("b"), ok("x"), ok("b")), "NO_DETERMINISTA")
})

test_that("NO_SE_REPITE no pide revisión; una anomalía confirmada sí", {
  expect_equal(E$veredicto_contenido(c("IDENTICO", "NO_SE_REPITE")), 0L)
  expect_equal(E$veredicto_formatos(c("IDENTICO", "NO_SE_REPITE")), 0L)
  expect_equal(E$veredicto_contenido(c("NO_SE_REPITE", "FALLA_SOLO_CANDIDATA")), 1L)
  expect_equal(E$veredicto_formatos(c("NO_SE_REPITE", "TEXTO_DISTINTO")), 1L)
})

test_that("de punta a punta: un fallo suelto se repite, se informa y deja su log", {
  skip_if_not_installed("jsonlite")
  corpus <- tempfile("corpus_suelto_")
  dir.create(corpus)
  estado <- tempfile("ya_fallo_")
  escribir(corpus, "suelto", c(
    "hijo_formatos <- '--hijo-formatos' %in% commandArgs()",
    sprintf("estado <- '%s'", estado),
    "if (marcador && hijo_formatos && !file.exists(estado)) {",
    "  file.create(estado)",
    "  stop('fallo suelto')",
    "}",
    "x <- 1"))
  lib <- tempfile("lib_cand_")
  dir.create(lib)
  file.create(file.path(lib, "MARCADOR"))
  salida <- tempfile("salida_suelto_")
  s <- suppressWarnings(system2(RSCRIPT, c(shQuote(SCRIPT), "--lib", shQuote(lib), "--raiz", shQuote(corpus),
                                           "--todos", "--n", 2, "--jobs", 1, "--formatos", 1,
                                           "--salida", shQuote(salida)), stdout = TRUE, stderr = TRUE))
  texto <- paste(s, collapse = "\n")
  expect_true(file.exists(estado))  # sin esto, el fallo no ocurrió y la prueba no prueba nada
  expect_true(grepl("NO_SE_REPITE: .*suelto\\.Rmd", texto))
  expect_false(grepl("FALLA_SOLO_CANDIDATA", texto, fixed = TRUE))
  expect_null(attr(s, "status"))
  expect_true(any(s == "COMPARAR_SIN_REGRESIONES"))
  for (etq in c("base", "cand", "base2", "cand2")) {
    expect_true(file.exists(file.path(salida, sprintf("f00001_%s.log", etq))), info = etq)
  }
  expect_true(any(grepl("fallo suelto", readRDS(file.path(salida, "f00001_cand.rds"))$formatos$html$error, fixed = TRUE)))
})
