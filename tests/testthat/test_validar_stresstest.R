# =============================================================================
# Test del validador de compilación completa multi-semilla (validar_stresstest.R)
# =============================================================================
# Origen (2026-11-04): `exams::stresstest_exercise()` no servía tal cual. Se midió
# que su `timeout` no corta `system()` ni Python, que su `grepl` en inglés no ve el
# mensaje bajo es_CO y que aborta al final si el ejercicio deja un objeto que
# `as.data.frame()` no admite. El barrido de 373 ejercicios dio dos defectos del
# validador que sus fixtures no veían: `Inf` legítimo en un bloque verbatim y el
# aviso de `use_python`. Los dos tienen aquí su caso.
#
# Los N de estos fixtures son pequeños a propósito: prueban el mecanismo con semillas
# conocidas, no miden un ejercicio (regla #23). Con set.seed(s), `sample(1:3, 1)` da
# 1, 1, 1, 3, 2, 1 para s = 1..6: la semilla 4 es x == 3 y la semilla 5 es x == 2.

library(testthat)

RAIZ <- tryCatch({
  r <- suppressWarnings(system("git rev-parse --show-toplevel", intern = TRUE, ignore.stderr = TRUE))
  if (length(r) == 1L && dir.exists(r)) r else normalizePath(file.path("..", ".."))
}, error = function(e) normalizePath(file.path("..", "..")))

SCRIPT <- file.path(RAIZ, ".claude", "scripts", "validar_stresstest.R")
RSCRIPT <- file.path(R.home("bin"), "Rscript")
E <- new.env()
sys.source(SCRIPT, envir = E)

ejercicio <- function(nombre, datos, cuerpo = "Cuanto es `r x` por 2?", solucion = "Es `r sol`.") {
  d <- tempfile("stress_fx_")
  dir.create(d)
  ruta <- file.path(d, paste0(nombre, ".Rmd"))
  writeLines(c(
    "```{r data_generation, echo=FALSE, results=\"hide\"}",
    "x <- sample(1:3, 1)",
    datos,
    "sol <- x * 2",
    "```",
    "",
    "Question",
    "========",
    cuerpo,
    "",
    "Solution",
    "========",
    solucion,
    "",
    "Meta-information",
    "================",
    paste0("exname: ", nombre),
    "extype: num",
    "exsolution: `r sol`",
    "extol: 0"
  ), ruta)
  ruta
}

correr <- function(rmd, ..., script = SCRIPT) {
  args <- c(shQuote(script), if (length(rmd)) shQuote(rmd), ...)
  salida <- suppressWarnings(system2(RSCRIPT, args, stdout = TRUE, stderr = TRUE))
  list(estado = if (is.null(attr(salida, "status"))) 0L else attr(salida, "status"),
       texto = paste(salida, collapse = "\n"))
}

tiene <- function(r, patron) grepl(patron, r$texto, fixed = TRUE)

# --- Funciones puras -------------------------------------------------------------

test_that("buscar_fugas: detecta los valores de R y nada más", {
  for (v in c("NA", "NaN", "Inf", "-Inf", "NULL", "numeric(0)", "character(0)", "integer(0)", "logical(0)")) {
    expect_equal(E$buscar_fugas(list(paste("El valor es", v, "kilos"))), v, info = v)
  }
  expect_equal(E$buscar_fugas(list(c("NA"))), "NA")
  negativos <- c("NASA gana", "INFORME", "Info", "NA2", "NULLO", "Infinito", "x_NA", "valor.NA",
                 "![](NA.png){width=50%}", "`NA` en código", "<span class=\"NA\">a</span>",
                 "\u00c9NA", "PI\u00d1NA", "NA\u00d1O")
  for (v in negativos) expect_equal(length(E$buscar_fugas(list(v))), 0L, info = v)
})

test_that("buscar_fugas: un bloque verbatim no es texto (falso positivo de ttest.Rmd)", {
  verbatim <- c("Resultado:", "```", " -1.145    Inf", "```", "Fin")
  expect_equal(length(E$buscar_fugas(list(verbatim))), 0L)
  expect_equal(E$buscar_fugas(list(c(verbatim, "Sale Inf"))), "Inf")
  # La línea que sigue al bloque cuenta: el cierre apaga el estado.
  expect_equal(E$buscar_fugas(list(c("```", "NA", "```", "NaN"))), "NaN")
})

test_that("huella_texto: ignora los anclajes y no el contenido", {
  a <- E$huella_texto(list("### Análisis {#afirmacion-d92ac822}", "Texto"))
  expect_identical(a, E$huella_texto(list("### Análisis {#afirmacion-413e3989}", "Texto")))
  expect_false(identical(a, E$huella_texto(list("### Análisis {#afirmacion-413e3989}", "Otro"))))
  expect_match(a, "^[0-9a-f]{32}$")
})

test_that("agrupar_mensajes: une lo que solo difiere en cifras", {
  g <- E$agrupar_mensajes(c("length 4 and 5", "length 3 and 5", "otro"), c(1L, 2L, 3L))
  expect_equal(length(g), 2L)
  expect_equal(g[[1L]]$n, 2L)
  expect_equal(g[[1L]]$semillas, c(1L, 2L))
})

test_that("prueba_posicion: n < 20 no concluye y la clave fija se detecta", {
  expect_null(E$prueba_posicion(rep(1L, 19L), 4L))
  expect_true(E$prueba_posicion(rep(1L, 100L), 4L)$sesgada)
  expect_false(E$prueba_posicion(rep(1:4, 25L), 4L)$sesgada)
  expect_equal(E$prueba_posicion(rep(1:4, 25L), 4L)$tabla, c(25L, 25L, 25L, 25L))
})

semilla_sintetica <- function(s, ...) {
  base <- list(semilla = s, segundos = 0.1, avisos = character(0), error = NA_character_,
               timeout = FALSE, tipo = "schoice", posicion = NA_integer_, fugas = character(0),
               huella = paste0("h", s), n_opciones = 4L)
  utils::modifyList(base, list(...))
}

test_that("resumir_stress: tabla de códigos y de salida", {
  limpio <- lapply(1:3, semilla_sintetica)
  r <- E$resumir_stress(limpio, 3L, 60)
  expect_equal(r$codigos, character(0))
  expect_equal(r$salida, 0L)

  # El aviso de use_python no cuenta; cualquier otro sí.
  a <- limpio
  a[[1L]]$avisos <- "The request to `use_python(\"/usr/bin/python\")` will be ignored"
  a[[2L]]$avisos <- "Previous request to `use_python(\"/usr/bin/python3\", required = TRUE)` will be ignored"
  expect_equal(E$resumir_stress(a, 3L, 60)$codigos, character(0))
  a[[3L]]$avisos <- "NAs introducidos"
  r <- E$resumir_stress(a, 3L, 60)
  expect_equal(r$codigos, "WARN_STRESS_AVISO")
  expect_equal(r$salida, 0L)

  e <- limpio
  e[[2L]]$error <- "falló"
  r <- E$resumir_stress(e, 3L, 60)
  expect_equal(r$codigos, "ERR_STRESS_ERROR")
  expect_equal(r$salida, 1L)

  t <- limpio
  t[[2L]]$error <- "reached elapsed time limit"
  t[[2L]]$timeout <- TRUE
  r <- E$resumir_stress(t, 3L, 60)
  expect_equal(r$codigos, "ERR_STRESS_TIMEOUT")
  expect_equal(r$timeouts, 2L)

  f <- limpio
  f[[3L]]$fugas <- "NA"
  expect_equal(E$resumir_stress(f, 3L, 60)$codigos, "ERR_STRESS_TEXTO")

  # Incompleta sin cuelgue demostrado: no es PASS.
  r <- E$resumir_stress(limpio[1:2], 3L, 60)
  expect_equal(r$codigos, "WARN_STRESS_INDET")
  expect_equal(r$salida, 2L)

  # Corte duro con la semilla abierta más allá del límite: cuelgue, no INDET.
  r <- E$resumir_stress(limpio[1:2], 3L, 60, list(semilla = 3L, estado = 124L, cuelgue = 3L))
  expect_equal(r$codigos, "ERR_STRESS_TIMEOUT")
  expect_equal(r$salida, 1L)

  # El peor caso: se cuelga la primera semilla y no hay ningún resultado.
  r <- E$resumir_stress(list(), 3L, 60, list(semilla = 1L, estado = 124L, cuelgue = 1L))
  expect_equal(r$codigos, "ERR_STRESS_TIMEOUT")
  expect_equal(r$n_hechas, 0L)
  expect_true(is.na(r$tipo))
  expect_output(E$imprimir_resumen(r), "Semillas: 0/3")
})

test_that("resumir_stress: reproducibilidad con su mutante", {
  limpio <- lapply(1:3, semilla_sintetica)
  expect_equal(E$resumir_stress(limpio, 3L, 60, NULL, list(huella = "h1"))$codigos, character(0))
  r <- E$resumir_stress(limpio, 3L, 60, NULL, list(huella = "otra"))
  expect_equal(r$codigos, "WARN_STRESS_NO_DETERMINISTA")
  expect_equal(r$salida, 0L)
  expect_false(r$reproducible)
  # Sin repetición no se afirma nada: NA, no TRUE.
  expect_true(is.na(E$resumir_stress(limpio, 3L, 60)$reproducible))
})

test_that("los dos orquestadores lo citan en el paso 9, con el código que bloquea", {
  for (o in c("orquestador-schoice.md", "orquestador-cloze.md")) {
    txt <- paste(readLines(file.path(RAIZ, ".claude", "agents", o), encoding = "UTF-8"), collapse = "\n")
    expect_true(grepl("Rscript .claude/scripts/validar_stresstest.R <ruta_al_.Rmd>", txt, fixed = TRUE), info = o)
    expect_true(grepl("ERR_STRESS_TEXTO", txt, fixed = TRUE), info = o)
    fila9 <- regmatches(txt, regexpr("\\| 9 \\| validar_diversidad \\|[^\n]*", txt))
    expect_true(grepl("validar_stresstest.R", fila9, fixed = TRUE), info = o)
  }
})

# --- Interfaz de línea de órdenes --------------------------------------------------

test_that("uso: sin archivo o con un archivo inexistente sale con 2", {
  r <- correr(character(0))
  expect_equal(r$estado, 2L)
  expect_true(tiene(r, "Uso:"))
  r <- correr(file.path(tempdir(), "no_existe.Rmd"))
  expect_equal(r$estado, 2L)
})

test_that("control negativo: ejercicio sano da PASS, md5 y 6/6", {
  rmd <- ejercicio("sano", character(0))
  r <- correr(rmd, "--n", 6, "--limite", 20)
  expect_equal(r$estado, 0L)
  expect_true(tiene(r, "PASS:"))
  expect_true(tiene(r, "Semillas: 6/6"))
  expect_true(tiene(r, paste0("md5 del .Rmd: ", unname(tools::md5sum(rmd)))))
})

test_that("cuelgue en R: nombra la semilla y sigue con las demás", {
  rmd <- ejercicio("cuelga", "if (x == 2) { i <- 0; repeat { i <- i + 1 } }")
  r <- correr(rmd, "--n", 6, "--limite", 2, "--total", 40)
  expect_equal(r$estado, 1L)
  expect_true(tiene(r, "ERR_STRESS_TIMEOUT: se excedió el límite en las semillas 5"))
  expect_true(tiene(r, "Semillas: 6/6"))
})

test_that("mutante: sin setTimeLimit el cuelgue ya no deja medir las demás semillas", {
  rmd <- ejercicio("cuelga2", "if (x == 2) { i <- 0; repeat { i <- i + 1 } }")
  mut <- file.path(tempdir(), "validar_stresstest_mut1.R")
  fuente <- readLines(SCRIPT, encoding = "UTF-8")
  linea <- grep("setTimeLimit(elapsed = limite, transient = TRUE)", fuente, fixed = TRUE)
  expect_equal(length(linea), 1L)
  writeLines(fuente[-linea], mut)
  r <- correr(rmd, "--n", 6, "--limite", 2, "--total", 16, script = mut)
  expect_false(tiene(r, "Semillas: 6/6"))
  expect_true(tiene(r, "Semillas: 4/6"))
})

test_that("corte duro: system() colgado no bloquea y se conservan las semillas hechas", {
  skip_if(!nzchar(Sys.which("timeout")), "sin timeout")
  rmd <- ejercicio("cuelga_system", "if (x == 2) system('sleep 60')")
  t0 <- Sys.time()
  r <- correr(rmd, "--n", 6, "--limite", 2, "--total", 16)
  segundos <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_equal(r$estado, 1L)
  expect_true(tiene(r, "Corte duro en la semilla 5"))
  expect_true(tiene(r, "Semillas: 4/6"))
  expect_true(tiene(r, "ERR_STRESS_TIMEOUT"))
  expect_lt(segundos, 40)
})

test_that("muestra incompleta sin cuelgue: exit 2 y nunca PASS", {
  skip_if(!nzchar(Sys.which("timeout")), "sin timeout")
  rmd <- ejercicio("lento", "Sys.sleep(1)")
  r <- correr(rmd, "--n", 60, "--limite", 20, "--total", 12)
  expect_equal(r$estado, 2L)
  expect_true(tiene(r, "WARN_STRESS_INDET"))
  expect_false(tiene(r, "PASS:"))
})

test_that("error y aviso solo en el texto: los ve con su semilla exacta", {
  rmd <- ejercicio("inline", character(0),
                   cuerpo = "Cuanto es `r if (x == 3) stop('falla con x=3') else x` por 2?",
                   solucion = "Es `r if (x == 2) warning('aviso con x=2') else sol`.")
  r <- correr(rmd, "--n", 6, "--limite", 20)
  expect_equal(r$estado, 1L)
  expect_true(tiene(r, "ERR_STRESS_ERROR"))
  expect_true(tiene(r, "1 semilla(s), p. ej. 4: falla con x=3"))
  expect_true(tiene(r, "WARN_STRESS_AVISO"))
  expect_true(tiene(r, "1 semilla(s), p. ej. 5: aviso con x=2"))
})

test_that("fuga de NA al enunciado en una sola semilla", {
  rmd <- ejercicio("fuga", "y <- if (x == 3) NA else x", cuerpo = "El dato es `r y`.")
  r <- correr(rmd, "--n", 6, "--limite", 20)
  expect_equal(r$estado, 1L)
  expect_true(tiene(r, "ERR_STRESS_TEXTO"))
  expect_true(tiene(r, "1 semilla(s), p. ej. 4: NA"))
})

test_that("no determinista: set.seed con el reloj se avisa y el sano no", {
  rmd <- ejercicio("reloj", c("set.seed((as.numeric(Sys.time()) * 1000) %% 100000)", "z <- sample(1e6, 1)"),
                   cuerpo = "El dato es `r z`.")
  r <- correr(rmd, "--n", 3, "--limite", 20)
  expect_equal(r$estado, 0L)
  expect_true(tiene(r, "WARN_STRESS_NO_DETERMINISTA"))
})

test_that("objeto no convertible a data.frame: la razón de no envolver stresstest_exercise", {
  skip_if_not_installed("reticulate")
  skip_if(!nzchar(Sys.which("python3")), "sin python3")
  rmd <- ejercicio("modulo_py", "modulo <- reticulate::import('builtins')")
  r <- correr(rmd, "--n", 3, "--limite", 30)
  expect_equal(r$estado, 0L)
  expect_true(tiene(r, "Semillas: 3/3"))
})

test_that("--json: códigos como arreglo aunque sea uno, y una huella por semilla", {
  skip_if_not_installed("jsonlite")
  rmd <- ejercicio("json", "y <- if (x == 3) NA else x", cuerpo = "El dato es `r y`.")
  f <- tempfile(fileext = ".json")
  r <- correr(rmd, "--n", 6, "--limite", 20, "--json", shQuote(f))
  expect_equal(r$estado, 1L)
  j <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_equal(length(j$codigos), 1L)
  expect_equal(j$codigos[[1L]], "ERR_STRESS_TEXTO")
  expect_equal(length(j$semillas), 6L)
  expect_equal(j$md5, unname(tools::md5sum(normalizePath(rmd))))
  huellas <- vapply(j$semillas, function(s) s$huella, character(1))
  expect_true(all(grepl("^[0-9a-f]{32}$", huellas)))
  expect_true(isTRUE(j$reproducible))
})

huellas_del_lote <- function(rmd, k) {
  lote <- exams::xexams(basename(rmd), n = k, edir = dirname(rmd), seed = matrix(seq_len(k), ncol = 1L))
  vapply(lote, function(v) {
    ex <- v[[1L]]
    m <- ex$metainfo
    E$huella_texto(c(list(ex$question, ex$questionlist, ex$solution, ex$solutionlist),
                     m$type, unlist(m$solution), unlist(m$clozetype)))
  }, character(1), USE.NAMES = FALSE)
}

test_that("la semilla s es la versión s de una exportación con seed: lo validado es lo exportado", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("exams")
  # Con exshuffle: xexams() vuelve a sembrar antes de leer, que es donde se mezclan las
  # opciones. En un ejercicio `num` set.seed() y seed = dan lo mismo y el mutante no moriría.
  d <- tempfile("stress_exporta_")
  dir.create(d)
  rmd <- file.path(d, "exporta.Rmd")
  writeLines(c(
    "```{r data_generation, echo=FALSE, results=\"hide\"}",
    "z <- sample(1e6, 1)",
    "```", "",
    "Question", "========", "El dato es `r z`.", "",
    "Answerlist", "----------", "* uno", "* dos", "* tres", "* cuatro", "",
    "Meta-information", "================",
    "exname: exporta", "extype: schoice", "exsolution: 1000", "exshuffle: TRUE"
  ), rmd)
  f <- tempfile(fileext = ".json")
  r <- correr(rmd, "--n", 4, "--limite", 20, "--json", shQuote(f))
  expect_equal(r$estado, 0L)
  j <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  huellas <- vapply(j$semillas, function(s) s$huella, character(1))
  expect_identical(huellas, huellas_del_lote(rmd, 4L))
  expect_equal(length(unique(huellas)), 4L)

  # Mutante: con set.seed() la mezcla de opciones sigue la secuencia y deja de coincidir.
  mut <- file.path(tempdir(), "validar_stresstest_mut2.R")
  fuente <- readLines(SCRIPT, encoding = "UTF-8")
  n_seed <- grep("seed = matrix(semilla),", fuente, fixed = TRUE)
  expect_equal(length(n_seed), 1L)
  fuente[n_seed] <- sub("seed = matrix(semilla),", "", fuente[n_seed], fixed = TRUE)
  llamada <- grep("setTimeLimit(elapsed = limite, transient = TRUE)", fuente, fixed = TRUE)
  fuente[llamada] <- paste0("  set.seed(semilla)\n", fuente[llamada])
  writeLines(fuente, mut)
  g <- tempfile(fileext = ".json")
  correr(rmd, "--n", 4, "--limite", 20, "--json", shQuote(g), script = mut)
  m <- jsonlite::fromJSON(g, simplifyVector = FALSE)
  mutadas <- vapply(m$semillas, function(s) s$huella, character(1))
  expect_false(identical(mutadas, huellas_del_lote(rmd, 4L)))
})


# --- El backend de matplotlib no cambia los decimales de R (Error 41) -----------------
# Con el backend Qt, la primera figura fija LC_NUMERIC en el locale del sistema para todo
# el proceso: R pasa a escribir 12,5, y una exportación con n > 1 mezcla punto y coma sin
# error. Se prueba con un entorno hostil: .Rprofile debe imponerse a un MPLBACKEND=qtagg.

test_that("Error 41: .Rprofile fija Agg y la primera figura no cambia los decimales", {
  rprofile <- readLines(file.path(RAIZ, ".Rprofile"), warn = FALSE)
  expect_true(any(grepl('^Sys\\.setenv\\(MPLBACKEND = "Agg"\\)', rprofile)))
  skip_if_not_installed("reticulate")
  guion <- tempfile(fileext = ".R")
  writeLines(c(
    "library(reticulate)",
    "py_run_string('import matplotlib.pyplot as plt\\nfig = plt.figure()\\nplt.close()')",
    "cat('DECIMAL', as.character(12.5), Sys.getlocale('LC_NUMERIC'), '\\n')"), guion)
  viejo <- setwd(RAIZ)
  on.exit(setwd(viejo))
  s <- suppressWarnings(system2(RSCRIPT, shQuote(guion), stdout = TRUE, stderr = TRUE, env = "MPLBACKEND=qtagg"))
  if (!any(grepl("^DECIMAL", s))) skip("matplotlib no disponible en este entorno")
  expect_true(any(grepl("^DECIMAL 12\\.5 C", s)), info = paste(utils::tail(s, 3), collapse = " | "))
})
