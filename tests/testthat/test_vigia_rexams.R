# =============================================================================
# test_vigia_rexams.R — vigía de versiones de R/exams
# =============================================================================
# .claude/scripts/vigia_rexams.R responde con fuentes primarias qué versión de
# R/exams hay instalada, cuál hay en CRAN y en desarrollo, y qué ítems del NEWS
# tocan el pipeline del repo. Estos tests corren SIEMPRE con --offline contra
# los fixtures de tests/testthat/fixtures/vigia_rexams/: nada de red.
#
# La ruta del script se puede sustituir con la variable VIGIA_REXAMS_SCRIPT
# (mismo patrón que DOC_COBERTURA / RUNNER_SUITES / CATALOGOS_MAT); por defecto
# es la canónica, .claude/scripts/vigia_rexams.R.
#
# El último test es un mutante: una copia del script en tempdir() donde
# version_mayor compara cadenas (`a > b`) debe SUSPENDER la comprobación de
# 2.4-10 — y este test lo demuestra ejecutándola contra la copia.

library(testthat)

.raiz_repo <- function() {
  r <- tryCatch(
    suppressWarnings(system("git rev-parse --show-toplevel",
                            intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) character(0))
  if (length(r) == 1L && nzchar(r)) return(r)
  normalizePath(".")
}

RAIZ <- .raiz_repo()
SCRIPT <- Sys.getenv("VIGIA_REXAMS_SCRIPT",
                     unset = file.path(RAIZ, ".claude", "scripts", "vigia_rexams.R"))
FX <- file.path(RAIZ, "tests", "testthat", "fixtures", "vigia_rexams")
RS <- file.path(R.home("bin"), "Rscript")

skip_if_not_installed("jsonlite")

# Carga el script con source() en un entorno propio. La guarda del CLI
# (!interactive() && sys.nframe() == 0L) impide que se ejecute nada al cargar:
# source() siempre corre dentro del frame de esta función.
.cargar <- function() {
  e <- new.env(parent = baseenv())
  source(SCRIPT, local = e)
  e
}

.leer_news <- function(nombre) {
  e <- .cargar()
  e$parsear_news(readLines(file.path(FX, nombre), warn = FALSE))
}

.correr <- function(args) {
  out <- suppressWarnings(system2(RS, c("--no-init-file", shQuote(SCRIPT), args),
                                  stdout = TRUE, stderr = TRUE))
  list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"),
       texto = paste(out, collapse = "\n"))
}

# Última línea VIGIA_* de la salida. --no-init-file salta el .Rprofile (que
# contamina stdout con mensajes de carga) pero lee .Renviron, donde esta
# máquina declara R_LIBS_USER: con --vanilla jsonlite no aparecería y todo el
# CLI fallaría con «no hay paquete llamado jsonlite» (verificado el 2026-10-22).
.ultima_vigia <- function(texto) {
  lineas <- strsplit(texto, "\n", fixed = TRUE)[[1]]
  lineas <- lineas[nzchar(lineas)]
  tail(grep("^VIGIA_", lineas, value = TRUE), 1)
}

# --- Funciones ---------------------------------------------------------------

test_that("version_mayor: 2.4-10 > 2.4-5 y 2.4.2 == 2.4-2", {
  e <- .cargar()
  expect_true(e$version_mayor("2.4-10", "2.4-5"))
  expect_false(e$version_mayor("2.4.2", "2.4-2"))
  expect_false(e$version_mayor("2.4-2", "2.4-5"))
})

test_that("parsear_news (exams): conteos exactos, ítem multilínea unido, sub-ítem sin fila propia", {
  news <- .leer_news("exams_NEWS.md")
  expect_identical(names(news), c("paquete", "version", "item"))
  expect_equal(nrow(news), 9L)
  expect_equal(sum(news$version == "2.4-5"), 4L)
  expect_equal(sum(news$version == "2.4-4"), 3L)
  expect_equal(sum(news$version == "2.4-3"), 2L)
  expect_true(all(news$paquete == "exams"))

  i1 <- news$item[news$version == "2.4-5"][1]
  # La frase cruza el salto de línea: prueba que el ítem quedó en UNA cadena.
  expect_true(grepl("templates de Pandoc para evitar conflictos con LTcaptype",
                    i1, fixed = TRUE))

  # Los dos sub-ítems viven DENTRO del ítem padre (no crean filas propias).
  i2 <- news$item[news$version == "2.4-5"][2]
  expect_true(grepl("El supplement ya no duplica la imagen del enunciado",
                    i2, fixed = TRUE))
  expect_true(grepl("Se respeta el ancho declarado en el template de cada figura",
                    i2, fixed = TRUE))

  # El preámbulo sin encabezado no genera filas de ningún paquete.
  expect_true(all(!is.na(news$paquete)))
})

test_that("parsear_news (exams2forms): dos versiones, conteos exactos", {
  news <- .leer_news("exams2forms_NEWS.md")
  expect_equal(nrow(news), 3L)
  expect_equal(sum(news$version == "0.2-2"), 2L)
  expect_equal(sum(news$version == "0.2-1"), 1L)
  expect_true(all(news$paquete == "exams2forms"))
})

test_that("items_nuevos: 10 con instalada 2.4-3/0.2-0 y cero con 2.4-5/0.2-2", {
  e <- .cargar()
  news <- rbind(.leer_news("exams_NEWS.md"), .leer_news("exams2forms_NEWS.md"))

  nuevos_3 <- e$items_nuevos(news, c(exams = "2.4-3", exams2forms = "0.2-0"))
  expect_equal(nrow(nuevos_3), 10L)
  expect_equal(sum(nuevos_3$paquete == "exams"), 7L)
  expect_equal(sum(nuevos_3$paquete == "exams2forms"), 3L)

  nuevos_5 <- e$items_nuevos(news, c(exams = "2.4-5", exams2forms = "0.2-2"))
  expect_equal(nrow(nuevos_5), 0L)
})

test_that("clasificar_item: temas exactos de cada ítem del fixture", {
  e <- .cargar()
  items <- .leer_news("exams_NEWS.md")$item
  expect_identical(e$clasificar_item(items[1]), c("pandoc-latex", "plantillas"))
  expect_identical(e$clasificar_item(items[2]), c("plantillas", "figuras"))
  expect_identical(e$clasificar_item(items[3]), c("moodle", "cloze", "metadatos"))
  expect_identical(e$clasificar_item(items[4]), character(0))
  expect_identical(e$clasificar_item(items[5]), c("nops", "stress"))
  expect_identical(e$clasificar_item(items[6]), "helpers-rmd")
  expect_identical(e$clasificar_item(items[7]), character(0))
  expect_identical(e$clasificar_item(items[8]), "nops")
  expect_identical(e$clasificar_item(items[9]), character(0))

  fitems <- .leer_news("exams2forms_NEWS.md")$item
  expect_identical(e$clasificar_item(fitems[1]), "webquiz")
  expect_identical(e$clasificar_item(fitems[2]), c("metadatos", "helpers-rmd"))
  expect_identical(e$clasificar_item(fitems[3]), "moodle")
})

test_that("leer_lock: versiones exactas y NULL si el archivo no existe", {
  e <- .cargar()
  lock <- e$leer_lock(file.path(FX, "renv.lock"))
  expect_identical(lock[["exams"]], "2.4-5")
  expect_identical(lock[["exams2forms"]], "0.2-1")
  expect_null(e$leer_lock(file.path(FX, "no_existe.lock")))
})

test_that("informe: columnas exactas, temas primero y códigos 0/1/2", {
  e <- .cargar()
  news <- rbind(.leer_news("exams_NEWS.md"), .leer_news("exams2forms_NEWS.md"))
  cran <- c(exams = "2.4-5", exams2forms = "0.2-2")
  lock <- e$leer_lock(file.path(FX, "renv.lock"))

  inf <- e$informe(c(exams = "2.4-3", exams2forms = "0.2-0"), lock, cran, cran, news)
  expect_identical(names(inf), c("versiones", "nuevos", "codigo"))
  expect_identical(names(inf$versiones), c("paquete", "instalada", "lock", "cran", "dev"))
  expect_identical(inf$versiones$paquete, c("exams", "exams2forms"))
  expect_identical(inf$versiones$instalada, c("2.4-3", "0.2-0"))
  expect_identical(inf$versiones$lock, c("2.4-5", "0.2-1"))
  expect_identical(inf$versiones$cran, c("2.4-5", "0.2-2"))
  expect_identical(inf$versiones$dev, c("2.4-5", "0.2-2"))
  expect_equal(inf$codigo, 0L)
  expect_identical(names(inf$nuevos), c("paquete", "version", "item", "temas"))
  expect_equal(nrow(inf$nuevos), 10L)

  # Primero los ítems con tema: la secuencia TRUE…FALSE no puede crecer.
  # Con tema son 8 de 10: todos menos «Corrección menor…» y «Ajuste interno…».
  con_tema <- vapply(seq_len(nrow(inf$nuevos)), function(i) {
    length(inf$nuevos$temas[[i]]) > 0L
  }, logical(1))
  expect_equal(sum(con_tema), 8L)
  expect_false(any(diff(con_tema) > 0))

  # --exigir-al-dia con CRAN mayor que la instalada → 1; sin versión nueva → 0.
  inf1 <- e$informe(c(exams = "2.4-3", exams2forms = "0.2-0"), lock, cran, cran,
                    news, exigir_al_dia = TRUE)
  expect_equal(inf1$codigo, 1L)
  inf0 <- e$informe(c(exams = "2.4-5", exams2forms = "0.2-2"), lock, cran, cran,
                    news, exigir_al_dia = TRUE)
  expect_equal(inf0$codigo, 0L)

  # Una fuente fallida → 2, sin importar las versiones.
  inf2 <- e$informe(c(exams = "2.4-3", exams2forms = "0.2-0"), lock, NULL, cran,
                    news, fallas = "desarrollo")
  expect_equal(inf2$codigo, 2L)
})

# --- CLI en modo --offline ---------------------------------------------------

.ARGS_BASE <- c("--offline", FX, "--lock", file.path(FX, "renv.lock"))

test_that("CLI --offline: instalada igual a CRAN → exit 0 y VIGIA_AL_DIA", {
  r <- .correr(c(.ARGS_BASE, "--instalada", "exams=2.4-5,exams2forms=0.2-2"))
  expect_equal(r$status, 0L, info = r$texto)
  expect_equal(.ultima_vigia(r$texto), "VIGIA_AL_DIA")
  expect_false(grepl("VIGIA_HAY_VERSION_NUEVA", r$texto, fixed = TRUE))
  expect_false(grepl("VIGIA_NO_VERIFICABLE", r$texto, fixed = TRUE))
  # El lock del fixture diverge solo en exams2forms (0.2-1 vs 0.2-2).
  expect_match(r$texto, "LOCK_DIVERGENTE: exams2forms instalada 0.2-2, renv.lock 0.2-1",
               fixed = TRUE)
  expect_false(grepl("LOCK_DIVERGENTE: exams ", r$texto, fixed = TRUE))
})

test_that("CLI --offline: instalada menor → exit 0 y VIGIA_HAY_VERSION_NUEVA", {
  r <- .correr(c(.ARGS_BASE, "--instalada", "exams=2.4-3,exams2forms=0.2-0"))
  expect_equal(r$status, 0L, info = r$texto)
  expect_equal(.ultima_vigia(r$texto), "VIGIA_HAY_VERSION_NUEVA")
  expect_match(r$texto,
    "Antes de actualizar: Rscript .claude/scripts/comparar_version_exams.R --lib <biblioteca-con-la-version-nueva>",
    fixed = TRUE)
  expect_match(r$texto, "LOCK_DIVERGENTE: exams instalada 2.4-3, renv.lock 2.4-5",
               fixed = TRUE)
  expect_match(r$texto, "LOCK_DIVERGENTE: exams2forms instalada 0.2-0, renv.lock 0.2-1",
               fixed = TRUE)

  # Las 10 novedades, con los ítems con tema primero (la primera lleva corchete,
  # la última no) y en el orden exacto del NEWS.
  lineas <- strsplit(r$texto, "\n", fixed = TRUE)[[1]]
  inicio <- grep("Novedades posteriores", lineas, fixed = TRUE)
  fin <- grep("^Antes de actualizar:", lineas)
  expect_length(inicio, 1L)
  expect_length(fin, 1L)
  items <- lineas[(inicio + 1L):(fin - 1L)]
  items <- items[nzchar(items)]
  expect_equal(length(items), 10L)
  expect_match(items[1], "[pandoc-latex plantillas]", fixed = TRUE)
  expect_match(items[1], "exams 2.4-5", fixed = TRUE)
  # Los dos últimos son los ítems sin tema, al final y sin corchete (la
  # «portada del manual» es de 2.4-3 y no aparece: no es posterior a 2.4-3).
  expect_false(grepl("[", items[9], fixed = TRUE))
  expect_match(items[9], "Corrección menor de redacción", fixed = TRUE)
  expect_false(grepl("[", items[10], fixed = TRUE))
  expect_match(items[10], "Ajuste interno sin efecto visible para el usuario",
               fixed = TRUE)
})

test_that("CLI: 0.2.0 instalada y 0.2-0 en el lock no es divergencia (falso positivo real)", {
  # packageVersion() escribe con puntos y renv.lock con guion: la primera corrida contra
  # las fuentes reales marcó como divergente una versión idéntica.
  r <- .correr(c("--offline", shQuote(FX), "--instalada", "exams=2.4.5,exams2forms=0.2.1",
                 "--lock", shQuote(file.path(FX, "renv.lock"))))
  expect_false(grepl("LOCK_DIVERGENTE", r$texto, fixed = TRUE))
})

test_that("CLI --offline --exigir-al-dia: exit 1 solo con versión de CRAN mayor", {
  r1 <- .correr(c(.ARGS_BASE, "--instalada", "exams=2.4-3,exams2forms=0.2-0",
                  "--exigir-al-dia"))
  expect_equal(r1$status, 1L, info = r1$texto)
  expect_equal(.ultima_vigia(r1$texto), "VIGIA_HAY_VERSION_NUEVA")

  r0 <- .correr(c(.ARGS_BASE, "--instalada", "exams=2.4-5,exams2forms=0.2-2",
                  "--exigir-al-dia"))
  expect_equal(r0$status, 0L, info = r0$texto)
  expect_equal(.ultima_vigia(r0$texto), "VIGIA_AL_DIA")
})

test_that("CLI --json: una sola línea de JSON con las tres claves", {
  r <- .correr(c(.ARGS_BASE, "--instalada", "exams=2.4-3,exams2forms=0.2-0", "--json"))
  expect_equal(r$status, 0L, info = r$texto)
  lineas <- strsplit(r$texto, "\n", fixed = TRUE)[[1]]
  lineas <- lineas[nzchar(lineas)]
  expect_length(lineas, 1L)
  j <- jsonlite::fromJSON(lineas[1])
  expect_identical(names(j), c("versiones", "nuevos", "codigo"))
  expect_equal(j$codigo, 0L)
  expect_equal(nrow(j$nuevos), 10L)
  expect_identical(j$versiones$paquete, c("exams", "exams2forms"))
})

test_that("CLI: argumentos inválidos → exit 2, jamás VIGIA_AL_DIA", {
  r <- .correr(c("--instalada", "exams=2.4-3,mal"))
  expect_equal(r$status, 2L)
  expect_match(r$texto, "VIGIA_NO_VERIFICABLE: argumentos", fixed = TRUE)
  expect_false(grepl("VIGIA_AL_DIA", r$texto, fixed = TRUE))

  r2 <- .correr(c("--bandera-inexistente"))
  expect_equal(r2$status, 2L)
  expect_equal(.ultima_vigia(r2$texto), "VIGIA_NO_VERIFICABLE")
  expect_false(grepl("VIGIA_AL_DIA", r2$texto, fixed = TRUE))
})

# --- Mutantes -----------------------------------------------------------------

test_that("mutante: sin dev.json → exit 2 y VIGIA_NO_VERIFICABLE, jamás AL_DIA", {
  tmp <- tempfile("vigia_fx_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- file.copy(list.files(FX, full.names = TRUE), tmp)
  expect_true(all(ok))
  unlink(file.path(tmp, "dev.json"))

  # instalada igual a CRAN: sin el dev.json faltante el script diría AL_DIA;
  # la regla clave exige NO VERIFICABLE ante cualquier fuente ilegible.
  r <- .correr(c("--offline", tmp, "--lock", file.path(tmp, "renv.lock"),
                 "--instalada", "exams=2.4-5,exams2forms=0.2-2"))
  expect_equal(r$status, 2L)
  expect_equal(.ultima_vigia(r$texto), "VIGIA_NO_VERIFICABLE")
  expect_match(r$texto, "VIGIA_NO_VERIFICABLE: dev.json", fixed = TRUE)
  expect_false(grepl("VIGIA_AL_DIA", r$texto, fixed = TRUE))
})

test_that("mutante: sin exams2forms_NEWS.md → exit 2", {
  tmp <- tempfile("vigia_fx_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- file.copy(list.files(FX, full.names = TRUE), tmp)
  expect_true(all(ok))
  unlink(file.path(tmp, "exams2forms_NEWS.md"))

  r <- .correr(c("--offline", tmp, "--lock", file.path(tmp, "renv.lock"),
                 "--instalada", "exams=2.4-5,exams2forms=0.2-2"))
  expect_equal(r$status, 2L)
  expect_match(r$texto, "VIGIA_NO_VERIFICABLE: NEWS de exams2forms", fixed = TRUE)
  expect_false(grepl("VIGIA_AL_DIA", r$texto, fixed = TRUE))
})

test_that("mutante: version_mayor comparando cadenas falla la comprobación de 2.4-10", {
  tmp <- tempfile("vigia_mut_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mut <- file.path(tmp, "vigia_rexams_mutado.R")

  lineas <- readLines(SCRIPT, warn = FALSE)
  objetivo <- "  package_version(a) > package_version(b)"
  hit <- which(lineas == objetivo)
  expect_length(hit, 1L)   # la mutación debe tocar la función real
  lineas[hit] <- "  a > b"
  writeLines(lineas, mut)

  e <- new.env(parent = baseenv())
  source(mut, local = e)
  # El mutante compara cadenas: "2.4-10" > "2.4-5" es FALSE («1» < «5»), de modo
  # que SUSPENDE la comprobación exacta que esta suite exige al original.
  expect_false(e$version_mayor("2.4-10", "2.4-5"))
  expect_true(e$version_mayor("2.4-9", "2.4-8"))
})

test_that("el script se carga con source() sin ejecutar el CLI", {
  e <- new.env(parent = baseenv())
  expect_error(source(SCRIPT, local = e), NA)
  expect_true(is.function(e$version_mayor))
  expect_true(is.function(e$parsear_news))
  expect_true(is.function(e$items_nuevos))
  expect_true(is.function(e$clasificar_item))
  expect_true(is.function(e$leer_lock))
  expect_true(is.function(e$informe))
})
