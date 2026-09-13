# =============================================================================
# render_html_shots.R — punto ciego del arsenal visual: capturas truncadas
# en silencio. .claude/scripts/render_html_shots.R
#
# Por que existe: la funcion shot() capturaba con `--window-size=w,h` fijo en
# h=6000L. Chromium `--headless --screenshot` recorta SIEMPRE al tamano EXACTO
# de --window-size (verificado: el PNG crudo mide wxh pase lo que pase con el
# contenido real de la pagina; no existe "full-page screenshot" via CLI). Un
# auditor visual midio con `magick identify` que el contenido real de un CLOZE
# de 6 partes a 360px de ancho mide 6511-6869px de alto segun la version --
# por encima del limite de 6000 -> las capturas moviles se truncaban en
# silencio y omitian el final del documento (Estrategia para evitar el error /
# Reflexion, feedback por opcion). Un auditor podia emitir APTO_VISUAL sin
# haber visto el pie del documento: un gate que da verde sin haber mirado,
# mismo modo de fallo que la FASE 2G y la FASE 2I de este repositorio.
#
# El fix tiene DOS partes, y esta suite prueba las dos: (1) subir el default
# h=6000L -> h=8000L; (2) un detector de truncamiento que compara, tras
# recortar el blanco sobrante con `magick -trim`, el alto resultante contra
# el alto de ventana `h` -- si el contenido llega a menos de TRUNC_MARGIN_PX
# del borde inferior, no se puede distinguir "el documento termina justo ahi"
# de "se corto en silencio", y se marca como POSIBLE TRUNCAMIENTO. Un limite
# mas alto SIN detector solo mueve el problema a un documento mas largo.
#
# El detector se EXTRAE del script real (bloque TRUNC_MARGIN_PX + shot()) y
# se evalua tal cual, en vez de reimplementarlo aqui: una copia paralela
# podria divergir del script real sin que nada lo notara (mismo criterio que
# test_fase2i_pandocbounded_detector.R y test_muestra_estandar.R). Los
# fixtures HTML viven en tempdir(); nunca se muta el script real (invariante
# de "mutar copias, no originales" documentado en varias suites de este repo).
# =============================================================================
library(testthat)

repo_root <- local({
  r <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(r) && length(r) == 1 && dir.exists(r)) r else normalizePath("../..")
})

SCRIPT <- file.path(repo_root, ".claude", "scripts", "render_html_shots.R")

# --- Herramientas externas: si faltan, esta suite se salta declarandolo -----
.browser <- Sys.which("chromium")
if (!nzchar(.browser)) .browser <- Sys.which("google-chrome-stable")
if (!nzchar(.browser)) .browser <- Sys.which("google-chrome")
.magick <- Sys.which("magick")
.tools_ok <- nzchar(.browser) && nzchar(.magick)

# --- Extraccion del bloque real (constante + shot()) del script fuente ------
# NUNCA se copia el codigo del detector a mano: se extrae literal del archivo
# real, entre la definicion de TRUNC_MARGIN_PX y el cierre de shot().
extraer_bloque_shot <- function(script_path) {
  stopifnot(file.exists(script_path))
  ln <- readLines(script_path, warn = FALSE)
  i_const <- grep("^TRUNC_MARGIN_PX <-", ln)
  i_fun   <- grep("^shot <- function", ln)
  stopifnot(length(i_const) == 1, length(i_fun) == 1, i_fun > i_const)
  # Cierre: primera linea "}" en columna 1 despues del inicio de la funcion.
  candidatas <- which(ln == "}")
  i_cierre <- candidatas[candidatas > i_fun][1]
  stopifnot(!is.na(i_cierre))
  paste(ln[i_const:i_cierre], collapse = "\n")
}

cargar_shot <- function(script_path, browser, magick) {
  bloque <- extraer_bloque_shot(script_path)
  e <- new.env(parent = globalenv())
  assign("browser", browser, envir = e)
  assign("magick", magick, envir = e)
  eval(parse(text = bloque), envir = e)
  stopifnot(is.function(get("shot", envir = e)))
  e
}

# --- Fixture: contenido realista ~6900px de alto a 360px de ancho -----------
# Calibrado el 2026-08-17 para caer en el mismo rango (6511-6869px) que midio
# el auditor visual sobre el CLOZE real.
crear_fixture_grande <- function(dir) {
  parrafo <- paste0(
    "Parrafo de prueba numero %d con contenido de longitud media para forzar ",
    "wrap en pantallas estrechas de movil, simulando retroalimentacion ",
    "cientifica de un ejercicio CLOZE con seis partes y feedback por opcion."
  )
  parrafos <- vapply(1:65, function(i) sprintf(paste0("<p>", parrafo, "</p>"), i),
                      character(1))
  html <- paste0(
    "<html><body style='margin:0;padding:10px;background:white;",
    "font-family:sans-serif;font-size:16px;'>",
    paste(parrafos, collapse = ""),
    "<div style='background:#eee;border:2px solid black;padding:10px;",
    "margin-top:10px;'>Estrategia para evitar el error / Reflexion final ",
    "(pie del documento)</div></body></html>"
  )
  f <- file.path(dir, "grande.html")
  writeLines(html, f)
  normalizePath(f)
}

# --- Fixture: contenido corto, con margen amplio bajo cualquier h razonable -
crear_fixture_corto <- function(dir) {
  html <- paste0(
    "<html><body style='margin:0;padding:10px;background:white;",
    "font-family:sans-serif;font-size:16px;'>",
    "<p>Documento corto: una sola version de examen sin desbordes.</p>",
    "<div style='background:#eee;border:2px solid black;padding:10px;'>",
    "Pie del documento corto</div></body></html>"
  )
  f <- file.path(dir, "corto.html")
  writeLines(html, f)
  normalizePath(f)
}

test_that("El default de h en shot() es >= 8000 (regla anti-truncamiento)", {
  ln <- readLines(SCRIPT, warn = FALSE)
  firma <- grep("^shot <- function", ln, value = TRUE)
  expect_length(firma, 1)
  m <- regmatches(firma, regexpr("h\\s*=\\s*[0-9]+L", firma))
  expect_length(m, 1)
  h_default <- as.integer(sub("h\\s*=\\s*", "", sub("L$", "", m)))
  # expect_gte() NO acepta info= en esta version de testthat (lección
  # repetida en test_bateria_eliminacion.R): se envuelve en expect_true.
  expect_true(h_default >= 8000L,
              info = sprintf("default h=%dL insuficiente (medido: 6511-6869px reales a 360px de ancho)",
                             h_default))
})

test_that("El detector de truncamiento existe en el archivo fuente (no solo en memoria)", {
  txt <- paste(readLines(SCRIPT, warn = FALSE), collapse = "\n")
  expect_true(grepl("TRUNC_MARGIN_PX", txt, fixed = TRUE))
  expect_true(grepl("identify", txt, fixed = TRUE))
  expect_true(grepl("truncado", txt, fixed = TRUE))
})

test_that("El aviso de truncamiento es VISIBLE: manifest.txt y consola lo reportan", {
  txt <- paste(readLines(SCRIPT, warn = FALSE), collapse = "\n")
  expect_true(grepl("POSIBLE TRUNCAMIENTO", txt, fixed = TRUE),
              info = "el manifiesto no declara una linea de truncamiento")
  expect_true(grepl("ADVERTENCIA", txt, fixed = TRUE),
              info = "no hay aviso de consola visible cuando hay truncamiento")
})

test_that("CONTROL POSITIVO: el detector real dispara con h=6000L (el default viejo)", {
  skip_if_not(.tools_ok, "chromium/google-chrome + magick no disponibles en este entorno")
  e <- cargar_shot(SCRIPT, .browser, .magick)
  tmp <- file.path(tempdir(), paste0("shots_trunc_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  html <- crear_fixture_grande(tmp)
  png  <- file.path(tmp, "v01_360_h6000.png")
  r <- get("shot", envir = e)(html, png, w = 360L, h = 6000L)

  expect_true(r$ok, info = "la captura debió generarse aunque esté truncada")
  expect_true(isTRUE(r$truncado),
              info = "con h=6000L (default viejo) el fixture de ~6900px debía marcarse truncado")
})

test_that("CONTROL NEGATIVO: el MISMO fixture, con el default REAL del script, no se marca truncado", {
  skip_if_not(.tools_ok, "chromium/google-chrome + magick no disponibles en este entorno")
  e <- cargar_shot(SCRIPT, .browser, .magick)
  tmp <- file.path(tempdir(), paste0("shots_trunc_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  html <- crear_fixture_grande(tmp)
  png  <- file.path(tmp, "v01_360_hdefault.png")
  shot_fn <- get("shot", envir = e)
  # Sin pasar `h`: usa el default declarado en el archivo real (>= 8000L,
  # verificado por el primer test de esta suite).
  r <- shot_fn(html, png, w = 360L)

  expect_true(r$ok)
  expect_false(isTRUE(r$truncado),
               info = "con el default real (>=8000L) el mismo fixture de ~6900px NO debía truncarse")
})

test_that("CONTROL NEGATIVO: documento corto con margen amplio nunca se marca truncado", {
  skip_if_not(.tools_ok, "chromium/google-chrome + magick no disponibles en este entorno")
  e <- cargar_shot(SCRIPT, .browser, .magick)
  tmp <- file.path(tempdir(), paste0("shots_trunc_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  html <- crear_fixture_corto(tmp)
  png  <- file.path(tmp, "v01_360_corto.png")
  r <- get("shot", envir = e)(html, png, w = 360L, h = 3000L)

  expect_true(r$ok)
  expect_false(isTRUE(r$truncado),
               info = "un documento corto con margen amplio no debe generar falso positivo")
})

test_that("TRUNC_MARGIN_PX es un margen positivo y razonable (extraido del script real)", {
  ln <- readLines(SCRIPT, warn = FALSE)
  m <- grep("^TRUNC_MARGIN_PX <- [0-9]+L", ln, value = TRUE)
  expect_length(m, 1)
  margen <- as.integer(sub("^TRUNC_MARGIN_PX <- ", "", sub("L$", "", m)))
  # Calibrado contra margenes medidos: ~14-27px en capturas TRUNCADAS frente
  # a >1000px en capturas SANAS. Un margen fuera de este rango o bien no
  # discrimina (demasiado grande) o es demasiado laxo (demasiado pequeño).
  expect_true(margen >= 10L)
  expect_true(margen <= 500L)
})
