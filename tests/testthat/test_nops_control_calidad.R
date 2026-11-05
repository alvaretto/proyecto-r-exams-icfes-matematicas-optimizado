# =============================================================================
# Test de regresión — Triaje de calidad de escaneos NOPS previo a nops_eval()
# =============================================================================
# Prueba nops_control_calidad.R con un Daten.txt sintético construido en
# tempdir() y comprimido con utils::zip(flags = "-jq"). No hace falta LaTeX
# ni el paquete exams: los casos se resuelven a nivel de función (leer_daten,
# triaje, recomendar) y de CLI (códigos de salida con Rscript). Los conteos
# son EXACTOS (expect_equal), nunca "al menos N".

library(testthat)

repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE)
  if (length(r) == 1 && dir.exists(r)) r else getwd()
}, error = function(e) getwd())

SCRIPT <- file.path(repo_root, "Auxiliares", "exams2nops-escaner", "nops_control_calidad.R")
RS     <- file.path(R.home("bin"), "Rscript")

env <- new.env(parent = globalenv())
# Cargar el script sin ejecutar el CLI: el guard !interactive() && sys.nframe()
# lo impide dentro de source().
source(SCRIPT, local = env)

zip_disponible <- nzchar(Sys.which("zip"))

# --- utilidades de fixtures -------------------------------------------------
# Hoja base válida: S0000001.PNG 26110400001 00 005 0 0157193 más 5 respuestas
# válidas 10000 y 40 00000. (type 005 = 5 preguntas.)
linea <- function(file, id, type = "005", reg, respuestas) {
  paste(c(file, id, "00", type, "0", reg, respuestas), collapse = " ")
}

hoja_base <- function(file = "S0000001.PNG", id = "26110400001", type = "005",
                      reg = "0157193", respuestas = c(rep("10000", 5), rep("00000", 40))) {
  linea(file, id, type, reg, respuestas)
}

crear_zip <- function(lineas, nombre = "nops_scan_20261104.zip") {
  dir <- tempfile("nops_fixture_")
  dir.create(dir)
  writeLines(lineas, file.path(dir, "Daten.txt"), useBytes = TRUE)
  zipfile <- file.path(dir, nombre)
  utils::zip(zipfile = zipfile, files = file.path(dir, "Daten.txt"), flags = "-jq")
  zipfile
}

correr_cli <- function(args, wd = NULL, script = SCRIPT) {
  if (is.null(wd)) {
    # suppressWarnings: system2 avisa cuando el comando sale con estatus != 0,
    # y aquí los códigos 1 y 2 son parte del contrato que se está probando.
    out <- suppressWarnings(system2(RS, c(script, args), stdout = TRUE, stderr = TRUE))
  } else {
    cmd <- paste("cd", shQuote(wd), "&&", shQuote(RS), shQuote(script),
                 paste(shQuote(args), collapse = " "))
    # Igual que arriba: el estatus 2 es parte del contrato, no un problema.
    out <- suppressWarnings(system(cmd, intern = TRUE))
  }
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else status, texto = paste(out, collapse = "\n"))
}

ultima_linea <- function(texto) {
  lineas <- strsplit(texto, "\n")[[1]]
  lineas <- lineas[nzchar(trimws(lineas))]
  tail(lineas, 1)
}

context("nops_control_calidad.R — lectura y triaje")

test_that("hoja limpia: cero hallazgos, exit 0 y última línea NOPS_LISTO_PARA_EVALUAR", {
  skip_if(!zip_disponible)
  z <- crear_zip(hoja_base())
  l <- env$leer_daten(z)
  expect_equal(nrow(l$hojas), 1L)
  expect_equal(l$ilegibles, character())
  # Los ceros a la izquierda importan: el registro no pierde el cero inicial.
  expect_equal(l$hojas$registration, "0157193")
  expect_equal(l$hojas$type, "005")
  h <- env$triaje(l)
  expect_equal(names(h), c("file", "fila", "problema", "detalle"))
  expect_equal(nrow(h), 0L)
  r <- correr_cli(z)
  expect_equal(r$status, 0L)
  expect_equal(ultima_linea(r$texto), "NOPS_LISTO_PARA_EVALUAR")
  expect_false(grepl("NOPS_REVISAR", r$texto))
})

test_that("un hallazgo por cada valor de problema, con fila y detalle exactos", {
  skip_if(!zip_disponible)
  r5 <- function(a1, a2 = "10000", a3 = "10000", a4 = "10000", a5 = "10000") {
    c(a1, a2, a3, a4, a5, rep("00000", 40))
  }
  z <- crear_zip(c(
    hoja_base(),                                                    # fila 1: limpia
    linea("S0000002.PNG", "2611040000X", reg = "0157194", respuestas = r5("10000")),  # fila 2: id_invalido
    linea("S0000003.PNG", "26110400002", type = "00X", reg = "0157195", respuestas = r5("11000")),  # fila 3: tipo_invalido (respuestas sin revisar)
    linea("S0000004.PNG", "26110400003", reg = "XXXXXXX", respuestas = r5("10000")),  # fila 4: registro_invalido
    linea("S0000005.PNG", "26110400004", reg = "0157196", respuestas = r5("11000")),  # fila 5: multimarca "1"
    linea("S0000006.PNG", "26110400005", reg = "0157197", respuestas = r5("10000", "00000", "10000", "00000", "10000")),  # fila 6: sin_marcar "2,4"
    linea("S0000007.PNG", "26110400004", reg = "0157198", respuestas = r5("10000")),  # fila 7: id_duplicado (id de la fila 5)
    linea("S0000008.PNG", "26110400006", reg = "0157197", respuestas = r5("10000")),  # fila 8: registro_duplicado (registro de la fila 6)
    linea("S0000009.PNG", "26110400007", reg = "0157199", respuestas = r5("12X00")),  # fila 9: respuesta_invalida "1"
    "S0000010.PNG ERROR 26110400009",                               # fila 10: ilegible (menos campos)
    hoja_base("S0000011.PNG", "26110400008", reg = "0157200")       # fila 11: limpia tras el ERROR
  ))
  l <- env$leer_daten(z)
  h <- env$triaje(l)
  esperado <- data.frame(
    file = c("S0000002.PNG", "S0000003.PNG", "S0000004.PNG", "S0000005.PNG", "S0000006.PNG",
             "S0000007.PNG", "S0000008.PNG", "S0000009.PNG", "S0000010.PNG"),
    fila = c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L),
    problema = c("id_invalido", "tipo_invalido", "registro_invalido", "multimarca", "sin_marcar",
                 "id_duplicado", "registro_duplicado", "respuesta_invalida", "ilegible"),
    detalle = c("2611040000X", "00X", "XXXXXXX", "1", "2,4", "26110400004", "0157197", "1", ""),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(h), 9L)
  expect_equal(h, esperado)
})

test_that("ceros a la izquierda: el registro 0157193 no pierde el cero ni es inválido", {
  skip_if(!zip_disponible)
  z <- crear_zip(hoja_base())
  l <- env$leer_daten(z)
  expect_equal(l$hojas$registration, "0157193")
  expect_equal(l$ilegibles, character())
  h <- env$triaje(l)
  expect_equal(nrow(h), 0L)
  expect_equal(any(h$problema == "registro_invalido"), FALSE)
})

test_that("línea ERROR con menos campos: no rompe la lectura y las filas conservan su número", {
  skip_if(!zip_disponible)
  z <- crear_zip(c(
    hoja_base("S0000031.PNG", "26110400021", reg = "0157211"),
    "S0000032.PNG ERROR",
    linea("S0000033.PNG", "26110400022", reg = "0157212",
          respuestas = c("11000", rep("10000", 4), rep("00000", 40)))
  ))
  l <- env$leer_daten(z)
  expect_equal(nrow(l$hojas), 3L)
  expect_equal(l$ilegibles, "S0000032.PNG")
  expect_true(is.na(l$hojas$id[2L]))
  h <- env$triaje(l)
  expect_equal(nrow(h), 2L)
  expect_equal(h$file, c("S0000032.PNG", "S0000033.PNG"))
  expect_equal(h$fila, c(2L, 3L))
  expect_equal(h$problema, c("ilegible", "multimarca"))
  expect_equal(h$detalle, c("", "1"))
})

test_that("00000 en la pregunta 6 de una hoja de 5 preguntas NO es sin_marcar", {
  skip_if(!zip_disponible)
  z <- crear_zip(hoja_base())  # a6..a45 son 00000 y la hoja tiene 5 preguntas
  l <- env$leer_daten(z)
  expect_equal(l$hojas$a6, "00000")
  h <- env$triaje(l)
  expect_equal(nrow(h), 0L)
})

test_that("mchoice: dos marcas no son anómalas, cinco sí (todas_marcadas)", {
  skip_if(!zip_disponible)
  z <- crear_zip(c(
    linea("S0000021.PNG", "26110400011", reg = "0157201",
          respuestas = c("11000", rep("10000", 4), rep("00000", 40))),
    linea("S0000022.PNG", "26110400012", reg = "0157202",
          respuestas = c("11111", rep("10000", 4), rep("00000", 40)))
  ))
  l <- env$leer_daten(z)
  h <- env$triaje(l, tipo = "mchoice")
  expect_equal(nrow(h), 1L)
  expect_equal(h$fila, 2L)
  expect_equal(h$problema, "todas_marcadas")
  expect_equal(h$detalle, "1")
  # La misma hoja de dos marcas SÍ es multimarca en schoice.
  h2 <- env$triaje(l, tipo = "schoice")
  expect_equal(h2$problema[h2$fila == 1L], "multimarca")
  expect_equal(h2$detalle[h2$fila == 1L], "1")
})

test_that("--preguntas 3 restringe la revisión a tres preguntas", {
  skip_if(!zip_disponible)
  respuestas <- c(rep("10000", 3), "00000", "11000", rep("00000", 40))
  z <- crear_zip(linea("S0000041.PNG", "26110400031", reg = "0157221", respuestas = respuestas))
  l <- env$leer_daten(z)
  h <- env$triaje(l)  # sin --preguntas: revisa las 5 del type
  expect_equal(nrow(h), 2L)
  expect_equal(h$problema, c("multimarca", "sin_marcar"))
  expect_equal(h$detalle, c("5", "4"))
  h3 <- env$triaje(l, preguntas = 3L)
  expect_equal(nrow(h3), 0L)
  r <- correr_cli(c(z, "--preguntas", "3"))
  expect_equal(r$status, 0L)
  expect_equal(ultima_linea(r$texto), "NOPS_LISTO_PARA_EVALUAR")
})

test_that("--lista: registro fuera de la lista detectado; ceros a la izquierda del CSV conservados", {
  skip_if(!zip_disponible)
  z <- crear_zip(c(
    hoja_base("S0000051.PNG", "26110400041", reg = "0157193"),  # está en la lista
    hoja_base("S0000052.PNG", "26110400042", reg = "0157199")   # no está en la lista
  ))
  csv <- data.frame(registration = c("0157193", "0157197"), stringsAsFactors = FALSE)
  ruta_csv <- tempfile(fileext = ".csv")
  utils::write.csv2(csv, ruta_csv, row.names = FALSE)
  # El CSV crudo conserva el cero a la izquierda (sin comillas de por medio).
  expect_true(any(grepl("0157193", readLines(ruta_csv), fixed = TRUE)))
  # Nivel función: fila y detalle exactos.
  l <- env$leer_daten(z)
  h <- env$triaje(l, lista = data.frame(registration = c("0157193", "0157197"), stringsAsFactors = FALSE))
  expect_equal(nrow(h), 1L)
  expect_equal(h$problema, "registro_fuera_de_lista")
  expect_equal(h$fila, 2L)
  expect_equal(h$detalle, "0157199")
  # Nivel CLI: el registro 0157193 (en la lista, con cero inicial) NO se marca;
  # si el CSV se leyera como número, parecería ajeno y serían 2 hallazgos.
  r <- correr_cli(c(z, "--lista", ruta_csv))
  expect_equal(r$status, 1L)
  expect_match(r$texto, "registro_fuera_de_lista: 1")
  expect_equal(ultima_linea(r$texto), "NOPS_REVISAR 1 hallazgo en 1 hoja")
  # Sin --lista la misma hoja es limpia.
  expect_equal(correr_cli(z)$status, 0L)
})

context("nops_control_calidad.R — recomendaciones y CLI")

test_that("recomendar: órdenes exactas (filas 2 y 5) y comentarios de revisión a mano", {
  h <- data.frame(
    file = c("S0000002.PNG", "S0000005.PNG", "S0000005.PNG", "S0000006.PNG", "S0000008.PNG",
             "S0000007.PNG", "S0000009.PNG"),
    fila = c(2L, 5L, 5L, 6L, 8L, 7L, 9L),
    problema = c("ilegible", "id_invalido", "multimarca", "sin_marcar", "todas_marcadas",
                 "registro_fuera_de_lista", "id_duplicado"),
    detalle = c("", "2611040000X", "1,3", "4", "2", "9999999", "26110400001"),
    stringsAsFactors = FALSE
  )
  ords <- env$recomendar(h, "nops_scan_prueba.zip")
  expect_equal(ords, c(
    'nops_fix("nops_scan_prueba.zip", display = "interactive")  # hoja(s) ilegible(s), fila(s) de Daten.txt: 2',
    'nops_fix("nops_scan_prueba.zip", exam = c(5))',
    'nops_fix("nops_scan_prueba.zip", check = "schoice")',
    'nops_fix("nops_scan_prueba.zip", check = "missing")',
    'nops_fix("nops_scan_prueba.zip", check = "mchoice")',
    '# revisar a mano: registro_fuera_de_lista en la fila 7 (detalle: 9999999)',
    '# revisar a mano: id_duplicado en la fila 9 (detalle: 26110400001)'
  ))
  # Sin hallazgos no hay órdenes.
  vacio <- data.frame(file = character(), fila = integer(), problema = character(),
                      detalle = character(), stringsAsFactors = FALSE)
  expect_equal(env$recomendar(vacio, "z.zip"), character())
})

test_that("CLI: ZIP con hallazgos da exit 1 y NOPS_REVISAR 2 hallazgos en 2 hojas", {
  skip_if(!zip_disponible)
  z <- crear_zip(c(
    hoja_base("S0000061.PNG", "26110400051", reg = "0157231"),                       # limpia
    linea("S0000062.PNG", "2611040005X", reg = "0157232", respuestas = c(rep("10000", 5), rep("00000", 40))),  # id_invalido
    linea("S0000063.PNG", "26110400052", reg = "0157233", respuestas = c("11000", rep("10000", 4), rep("00000", 40)))  # multimarca
  ))
  r <- correr_cli(z)
  expect_equal(r$status, 1L)
  expect_match(r$texto, "id_invalido: 1")
  expect_match(r$texto, "multimarca: 1")
  expect_equal(ultima_linea(r$texto), "NOPS_REVISAR 2 hallazgos en 2 hojas")
})

test_that("CLI: sin ZIP en un directorio vacío da exit 2 y NOPS_NO_VERIFICABLE", {
  dir_vacia <- tempfile("nops_vacia_")
  dir.create(dir_vacia)
  r <- correr_cli(character(), wd = dir_vacia)
  expect_equal(r$status, 2L)
  expect_equal(ultima_linea(r$texto), "NOPS_NO_VERIFICABLE")
})

test_that("CLI: ZIP sin Daten.txt da exit 2", {
  skip_if(!zip_disponible)
  dir <- tempfile("nops_sin_daten_")
  dir.create(dir)
  writeLines("no es un Daten", file.path(dir, "otro.txt"))
  zipfile <- file.path(dir, "nops_scan_20261104.zip")
  utils::zip(zipfile = zipfile, files = file.path(dir, "otro.txt"), flags = "-jq")
  r <- correr_cli(zipfile)
  expect_equal(r$status, 2L)
  expect_equal(ultima_linea(r$texto), "NOPS_NO_VERIFICABLE")
})

test_that("CLI: sin ZIP explícito usa el patrón nops_scan_*.zip del directorio actual", {
  skip_if(!zip_disponible)
  z <- crear_zip(hoja_base(), nombre = "nops_scan_20261104.zip")
  r <- correr_cli(character(), wd = dirname(z))
  expect_equal(r$status, 0L)
  expect_equal(ultima_linea(r$texto), "NOPS_LISTO_PARA_EVALUAR")
})

test_that("CLI: --tipo inválido da exit 2", {
  skip_if(!zip_disponible)
  z <- crear_zip(hoja_base())
  r <- correr_cli(c(z, "--tipo", "nope"))
  expect_equal(r$status, 2L)
  expect_equal(ultima_linea(r$texto), "NOPS_NO_VERIFICABLE")
})

test_that("CLI: --json imprime un JSON parseable con el veredicto", {
  skip_if(!zip_disponible)
  z <- crear_zip(c(
    hoja_base("S0000071.PNG", "26110400061", reg = "0157241"),
    linea("S0000072.PNG", "26110400062", reg = "0157242",
          respuestas = c("11000", rep("10000", 4), rep("00000", 40)))
  ), nombre = "nops_scan_20261106.zip")
  r <- correr_cli(c(z, "--json"))
  expect_equal(r$status, 1L)
  # El .Rprofile del usuario puede imprimir un banner antes del JSON; se
  # localiza el JSON desde la primera línea que empieza por "{".
  lineas <- strsplit(r$texto, "\n")[[1]]
  ini <- which(startsWith(trimws(lineas), "{"))
  expect_true(length(ini) >= 1L)
  js <- jsonlite::fromJSON(paste(lineas[ini[1]:length(lineas)], collapse = "\n"))
  expect_equal(js$estado, "REVISAR")
  expect_equal(js$archivos$zip, z)
  expect_equal(nrow(js$archivos$hallazgos[[1]]), 1L)
  expect_equal(js$archivos$hallazgos[[1]]$problema, "multimarca")
  expect_equal(js$archivos$hallazgos[[1]]$detalle, "1")
  # Hoja limpia: LISTO y exit 0.
  z2 <- crear_zip(hoja_base(), nombre = "nops_scan_20261107.zip")
  r2 <- correr_cli(c(z2, "--json"))
  expect_equal(r2$status, 0L)
  lineas2 <- strsplit(r2$texto, "\n")[[1]]
  ini2 <- which(startsWith(trimws(lineas2), "{"))
  js2 <- jsonlite::fromJSON(paste(lineas2[ini2[1]:length(lineas2)], collapse = "\n"))
  expect_equal(js2$estado, "LISTO")
})

test_that("mutante: sin colClasses la comprobación de ceros a la izquierda falla", {
  skip_if(!zip_disponible)
  z <- crear_zip(hoja_base())
  csv <- data.frame(registration = "0157193", stringsAsFactors = FALSE)
  ruta_csv <- tempfile(fileext = ".csv")
  utils::write.csv2(csv, ruta_csv, row.names = FALSE)
  args <- c(z, "--lista", ruta_csv)
  # Original: el registro 0157193 está en la lista y la hoja es limpia.
  expect_equal(correr_cli(args)$status, 0L)
  # Copia del script en tempdir() con el colClasses del read.table eliminado.
  txt <- readLines(SCRIPT, warn = FALSE)
  patron <- 'colClasses = "character", fill = TRUE'
  i_patron <- grep(patron, txt, fixed = TRUE)
  expect_length(i_patron, 1L)
  txt[i_patron] <- sub(patron, "fill = TRUE", txt[i_patron], fixed = TRUE)
  mutante <- tempfile(fileext = ".R")
  writeLines(txt, mutante, useBytes = TRUE)
  r <- correr_cli(args, script = mutante)
  expect_equal(r$status, 1L)
  # El registro pierde el cero inicial y ya no coincide con la lista; el type
  # 005 también se lee como número y se vuelve inválido.
  expect_match(r$texto, "registro_fuera_de_lista")
  expect_match(r$texto, "tipo_invalido")
  expect_false(grepl("NOPS_LISTO_PARA_EVALUAR", r$texto))
})

# --- De punta a punta: hojas reales -------------------------------------------
# Los casos anteriores prueban la lectura de un Daten.txt escrito a partir de la
# especificación. Aquí el Daten.txt lo escribe nops_scan() a partir de hojas
# generadas por exams2nops(), marcadas con cruces en sus coordenadas reales. Las
# anomalías se siembran y el triaje debe hallar exactamente esas.
# Medido el 2026-11-04 con exams 2.4-2 y 2.4-5.

test_that("de punta a punta: exams2nops, marcas, nops_scan y triaje", {
  skip_if_not_installed("exams")
  skip_if_not_installed("png")
  skip_if(!nzchar(Sys.which("pdflatex")), "sin pdflatex")
  skip_if(!nzchar(Sys.which("pdftoppm")), "sin pdftoppm")
  skip_if(!nzchar(Sys.which("zip")), "sin zip")
  skip_if(!exists("trim_nops_scan", envir = asNamespace("exams"), inherits = FALSE),
          "exams ya no expone trim_nops_scan")

  d <- tempfile("nops_e2e_")
  dir.create(d)
  viejo <- setwd(d)
  on.exit(setwd(viejo), add = TRUE)
  # Sin tablas: una tabla Markdown no compila con exams < 2.4-4 y pandoc >= 3.8.1.
  writeLines(c(
    "Question", "========", "Elija una opción.", "",
    "Answerlist", "----------", "* a", "* b", "* c", "* d", "* e", "",
    "Meta-information", "================",
    "exname: hoja", "extype: schoice", "exsolution: 10000", "exshuffle: FALSE"
  ), "hoja.Rmd")
  set.seed(403)
  suppressMessages(utils::capture.output(
    exams::exams2nops(rep("hoja.Rmd", 5L), n = 4L, dir = ".", name = "demo",
                      date = "2026-11-04", language = "es")
  ))
  pdfs <- sort(list.files(".", "^demo[0-9]+\\.pdf$"))
  expect_equal(length(pdfs), 4L)
  pngs <- sprintf("hoja%d.png", seq_along(pdfs))
  for (i in seq_along(pdfs)) {
    system2("pdftoppm", c("-r", "300", "-f", "1", "-l", "1", "-singlefile", "-gray", "-png",
                          pdfs[i], sub("\\.png$", "", pngs[i])))
  }

  limites <- function(f) {
    g <- get("trim_nops_scan", envir = asNamespace("exams"))
    b <- body(g)
    b[[length(b)]] <- quote(c(rt, rb, cl, cr))
    body(g) <- b
    environment(g) <- asNamespace("exams")
    g(f)
  }
  cruz <- function(m, cen, r = 11L) {
    for (k in -r:r) for (g in 0:2) {
      a <- cen + c(k, k + g)
      b <- cen + c(k, -k + g)
      m[a[1L], a[2L]] <- 0
      m[b[1L], b[2L]] <- 0
    }
    m
  }
  marcar <- function(f, registro, respuestas) {
    m <- png::readPNG(f)
    if (length(dim(m)) > 2L) m <- m[, , 1L]
    l <- limites(f)
    px <- function(rr, cc) c(l[1L] + round(rr * (l[2L] - l[1L] + 1L)), l[3L] + round(cc * (l[4L] - l[3L] + 1L)))
    digitos <- strsplit(registro, "")[[1L]]
    for (j in seq_along(digitos)) if (digitos[j] != "-") {
      m <- cruz(m, px(0.166 + as.integer(digitos[j]) * 0.027, 0.681 + (j - 1L) * 0.047))
    }
    for (q in seq_along(respuestas)) for (ch in respuestas[[q]]) {
      bloque <- (q - 1L) %/% 5L
      fila <- (q - 1L) %% 5L
      m <- cruz(m, px(0.5532 + bloque * 0.148 + fila * 0.027, 0.04125 + (ch - 1L) * 0.047))
    }
    png::writePNG(m, f)
  }
  marcar(pngs[1L], "0157193", list(1, 2, 3, 4, 5))
  marcar(pngs[2L], "02-1234", list(c(1, 3), 2, integer(0), 4, 5))
  marcar(pngs[3L], "0157193", list(5, 4, 3, 2, 1))
  png::writePNG(matrix(1, 3508L, 2480L), pngs[4L])

  z <- "nops_scan_20261104120000.zip"
  suppressWarnings(suppressMessages(utils::capture.output(
    exams::nops_scan(images = pngs, file = z, dir = ".", rotate = FALSE, verbose = FALSE)
  )))
  expect_true(file.exists(z))

  lect <- env$leer_daten(z)
  expect_equal(nrow(lect$hojas), 4L)
  expect_equal(lect$ilegibles, "hoja4.png")
  h <- lect$hojas
  # La hoja limpia se lee tal como se dibujó, con el cero a la izquierda.
  expect_equal(h$registration[1L], "0157193")
  expect_equal(unlist(h[1L, paste0("a", 1:5)], use.names = FALSE),
               c("10000", "01000", "00100", "00010", "00001"))
  expect_equal(h$type[1L], "005")

  t <- env$triaje(lect)
  esperado <- data.frame(
    file = c("hoja2.png", "hoja2.png", "hoja2.png", "hoja3.png", "hoja4.png"),
    fila = c(2L, 2L, 2L, 3L, 4L),
    problema = c("registro_invalido", "multimarca", "sin_marcar", "registro_duplicado", "ilegible"),
    detalle = c("0000000", "1", "3", "0157193", ""),
    stringsAsFactors = FALSE
  )
  expect_equal(t, esperado)
})
