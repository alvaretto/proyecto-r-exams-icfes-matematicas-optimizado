# =============================================================================
# Test del cuestionario HTML sin internet (webquiz_autocontenido.R)
# =============================================================================
# Origen (2026-11-04): el HTML de `exams2webquiz()` carga MathJax desde un CDN; sin
# red las fórmulas salen como TeX crudo. En el aula no hay internet, y el material
# del estudiante debe ser autocontenido. El argumento `mathjax = TRUE` del Semillero
# no lo resuelve.
#
# La prueba que cuenta es la del navegador sin red: la de punta a punta salta sin
# Chromium, exams2forms o la copia de MathJax en caché, y en ese caso la suite no
# acredita la composición, solo las funciones puras.

library(testthat)

RAIZ <- tryCatch({
  r <- suppressWarnings(system("git rev-parse --show-toplevel", intern = TRUE, ignore.stderr = TRUE))
  if (length(r) == 1L && dir.exists(r)) r else normalizePath(file.path("..", ".."))
}, error = function(e) normalizePath(file.path("..", "..")))

SCRIPT <- file.path(RAIZ, ".claude", "scripts", "webquiz_autocontenido.R")
RSCRIPT <- file.path(R.home("bin"), "Rscript")
E <- new.env()
sys.source(SCRIPT, envir = E)

CARGADOR <- paste(
  "<!-- dynamically load mathjax for compatibility with self-contained -->",
  "<script>",
  "  (function () {",
  "    var script = document.createElement(\"script\");",
  "    script.src  = \"https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\";",
  "    document.getElementsByTagName(\"head\")[0].appendChild(script);",
  "  })();",
  "</script>",
  sep = "\n")

test_that("incrustar_mathjax: sustituye el cargador exactamente una vez", {
  html <- paste0("<head>antes\n", CARGADOR, "\ndespués</head>")
  nuevo <- E$incrustar_mathjax(html, "var MJ = 1;")
  expect_false(grepl("mathjax.rstudio.com", nuevo, fixed = TRUE))
  expect_true(grepl("<script>var MJ = 1;</script>", nuevo, fixed = TRUE))
  expect_true(startsWith(nuevo, "<head>antes\n"))
  expect_true(endsWith(nuevo, "\ndespués</head>"))
  expect_equal(lengths(regmatches(nuevo, gregexpr("window.MathJax=", nuevo, fixed = TRUE))), 1L)
})

test_that("incrustar_mathjax: el JavaScript no se reinterpreta (barras y grupos)", {
  html <- paste0("a", CARGADOR, "b")
  js <- "var r = /\\\\(x\\\\)/; var s = '\\\\1';"
  expect_true(grepl(js, E$incrustar_mathjax(html, js), fixed = TRUE))
})

test_that("incrustar_mathjax: falla si cambia la plantilla o el JavaScript no se puede incrustar", {
  expect_error(E$incrustar_mathjax("<html>sin cargador</html>", "x"), "plantilla")
  expect_error(E$incrustar_mathjax(CARGADOR, "x</SCRIPT>y"), "incrustar")
  # Dos cargadores: el segundo quedaría pidiendo la red.
  expect_error(E$incrustar_mathjax(paste0(CARGADOR, "\n", CARGADOR), "x"), "referencia")
})

test_that("recursos_externos: lo que pediría la red, y un enlace no lo es", {
  positivos <- c(
    "<script src=\"https://cdn.x/a.js\"></script>",
    "<script src='//cdn.x/a.js'></script>",
    "<img src=\"http://x/y.png\">",
    "<link rel=\"stylesheet\" href=\"https://x/y.css\">",
    "<style>body{background:url(https://x/y.png)}</style>",
    "<style>@import \"https://x/y.css\";</style>",
    "<iframe src=\"https://x\"></iframe>"
  )
  for (h in positivos) expect_equal(length(E$recursos_externos(h)), 1L, info = h)
  negativos <- c(
    "<a href=\"https://www.icfes.gov.co\">ICFES</a>",
    "<img src=\"data:image/png;base64,AAAA\">",
    "<script>var u = 'https://cdn.jsdelivr.net/npm/mathjax@3/es5/x.js';</script>",
    "<style>body{color:red}</style>"
  )
  for (h in negativos) expect_equal(length(E$recursos_externos(h)), 0L, info = h)
})

test_that("medir_dom: cuenta fórmulas compuestas, crudas y con error", {
  dom <- "<p><mjx-container>a</mjx-container> y \\(x^2\\) y \\[y\\]</p><script>var s='\\(no';</script>"
  m <- E$medir_dom(dom)
  expect_equal(m$compuestas, 1L)
  expect_equal(m$tex_crudo, 2L)
  expect_equal(m$con_error, 0L)
  expect_equal(E$medir_dom("<mjx-container data-mjx-error=\"x\">a</mjx-container>")$con_error, 1L)
  # Un desplegable no puede componer: no se cuenta como fórmula sin componer.
  expect_equal(E$medir_dom("<select><option>\\(x\\)</option></select>")$tex_crudo, 0L)
})

test_that("evaluar_medicion: cualquier resto es fallo", {
  bien <- list(compuestas = 5L, tex_crudo = 0L, con_error = 0L)
  expect_equal(length(E$evaluar_medicion(bien, character(0))), 0L)
  expect_equal(length(E$evaluar_medicion(utils::modifyList(bien, list(tex_crudo = 1L)), character(0))), 1L)
  expect_equal(length(E$evaluar_medicion(utils::modifyList(bien, list(con_error = 2L)), character(0))), 1L)
  expect_equal(length(E$evaluar_medicion(bien, "<script src=\"http://x\">")), 1L)
})

test_that("asegurar_mathjax: rechaza una copia con otro hash", {
  skip_if_not_installed("digest")
  ruta <- tempfile(fileext = ".js")
  writeLines("no es MathJax", ruta)
  expect_error(E$asegurar_mathjax(ruta, url = paste0("file://", ruta), sha = "0000"), "SHA-256")
})

test_that("uso: sin ejercicio sale NO VERIFICABLE", {
  s <- suppressWarnings(system2(RSCRIPT, shQuote(SCRIPT), stdout = TRUE, stderr = TRUE))
  expect_equal(attr(s, "status"), 2L)
  expect_true(any(s == "WEBQUIZ_NO_VERIFICABLE"))
})

test_that("de punta a punta: sin red las fórmulas se componen; sin MathJax no", {
  skip_if_not_installed("exams2forms")
  skip_if_not_installed("digest")
  skip_if(!nzchar(E$navegador()), "sin Chromium")
  cache <- path.expand(E$ruta_mathjax())
  skip_if(!file.exists(cache), "sin la copia de MathJax en caché")

  ej <- system.file("exercises", "deriv2.Rmd", package = "exams")
  d <- tempfile("webquiz_e2e_")
  s <- suppressWarnings(system2(RSCRIPT, c(shQuote(SCRIPT), shQuote(ej), "--n", 1, "--dir", shQuote(d), "--nombre", "q"),
                                stdout = TRUE, stderr = TRUE))
  expect_null(attr(s, "status"))
  expect_true(any(s == "WEBQUIZ_OK"))
  html <- file.path(d, "q.html")
  expect_true(file.exists(html))
  texto <- E$leer_html(html)
  expect_false(grepl("mathjax.rstudio.com", texto, fixed = TRUE))
  expect_equal(length(E$recursos_externos(texto)), 0L)
  m <- E$medir_dom(E$dom_sin_red(html))
  expect_equal(m$tex_crudo, 0L)
  expect_equal(m$con_error, 0L)
  # deriv2: enunciado y cinco opciones numéricas, más la solución.
  expect_true(m$compuestas >= 7L)

  # Mutante: sin MathJax dentro, la misma medición debe ver el TeX crudo.
  sin_mj <- gsub(CARGADOR, "", gsub("(?s)<script>window\\.MathJax=.*?</script>\\s*<script>.*?</script>", "", texto, perl = TRUE), fixed = TRUE)
  f <- tempfile(fileext = ".html")
  writeLines(sin_mj, f, useBytes = TRUE)
  m2 <- E$medir_dom(E$dom_sin_red(f))
  expect_equal(m2$compuestas, 0L)
  expect_gt(m2$tex_crudo, 0L)
  expect_equal(length(E$evaluar_medicion(m2, character(0))), 1L)
})
