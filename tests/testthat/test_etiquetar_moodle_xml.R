# =============================================================================
# test_etiquetar_moodle_xml.R — etiquetar_moodle_xml(): exextra -> <tags> en el XML de Moodle
# =============================================================================
#
# ORIGEN (2026-10-21): exams2moodle() descarta los exextra, así que ninguna pregunta
# llegaba etiquetada a icfes y hubo que etiquetar el banco directamente en la base.
# etiquetar_moodle_xml() (A-Produccion/perifericos/) escribe las etiquetas en el XML.
#
# QUÉ FIJA ESTE TEST
#   1. Una pregunta con los tres exextra recibe area/competencia/nivel/componente.
#   2. Al .Rmd al que le falta un exextra no se le inventa la etiqueta; sin ninguno,
#      solo queda area::Matematicas. Un valor fuera del vocabulario avisa y se omite.
#   3. Las categorías quedan sin <tags>.
#   4. Un XML que ya trae <tags> no se duplica (ni distinguiendo mayúsculas) y no pierde
#      las suyas; correr el paso dos veces deja el archivo idéntico.
#   5. Las tildes quedan intactas en UTF-8 (bytes, no solo lo que lee R).
#   6. Canario: el nombre «R1 Q1 : <archivo>» que escribe el exams2moodle() REAL sigue
#      siendo el que el cruce espera.
#
# Los fixtures van EN LÍNEA (no en tests/testthat/fixtures/): *.xml está en .gitignore
# y en LFS, y un .Rmd suelto en el repo caería bajo el gate y la nomenclatura de .Rmd.
# =============================================================================

library(testthat)

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE)[1],
  error = function(e) normalizePath("../..", mustWork = FALSE))
if (is.na(repo_root) || !nzchar(repo_root)) repo_root <- normalizePath("../..", mustWork = FALSE)

FUNCION <- file.path(repo_root, "A-Produccion", "perifericos", "etiquetar_moodle_xml.R")
if (file.exists(FUNCION)) source(FUNCION, local = TRUE, encoding = "UTF-8")

# --- Fixtures ----------------------------------------------------------------

escribir_utf8 <- function(ruta, lineas) {
  con <- file(ruta, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(lineas), con, useBytes = TRUE)
  invisible(ruta)
}

# .Rmd mínimo de R/exams; `extras` son las líneas exextra[...] de Meta-information y
# `decoy` va ANTES de Meta-information (no debe leerse).
crear_rmd <- function(dir, nombre, extras = character(0), decoy = character(0)) {
  ruta <- file.path(dir, paste0(nombre, ".Rmd"))
  escribir_utf8(ruta, c(
    "Question", "========", "Texto de la pregunta.", decoy, "",
    "Answerlist", "----------", "* Uno", "* Dos", "", "Solution", "========", "Es dos.", "",
    "Meta-information", "================",
    paste0("exname: ", nombre), "extype: schoice", "exsolution: 01", "exshuffle: FALSE", extras))
  ruta
}

# Pregunta tal como la deja exams2moodle(): nombre «R1 Q1 : título» con espacios dentro de <text>.
TEXTO_PREGUNTA <- "<p>¿Cuánto es 2 &amp; 3?</p>"  # ¿Cuánto es 2 &amp; 3?
pregunta_xml <- function(nombre, tipo = "multichoice", tags = NULL) {
  c(sprintf('<question type="%s">', tipo),
    "<name>", sprintf("<text> %s </text>", nombre), "</name>",
    '<questiontext format="html">',
    sprintf("<text><![CDATA[%s]]></text>", TEXTO_PREGUNTA),
    "</questiontext>",
    '<answer fraction="100" format="html">', "<text><![CDATA[<p>5</p>]]></text>", "</answer>",
    if (!is.null(tags)) c("<tags>", sprintf("<tag><text>%s</text></tag>", tags), "</tags>"),
    "</question>", "")
}
categoria_xml <- function(texto = "$course$/lote/Ejercicio 1") {
  c('<question type="category">', "<category>", sprintf("<text>%s</text>", texto),
    "</category>", "</question>", "")
}
crear_xml <- function(dir, ..., archivo = "lote_.xml") {
  ruta <- file.path(dir, archivo)
  escribir_utf8(ruta, c('<?xml version="1.0" encoding="UTF-8"?>', "<quiz>", "", ..., "</quiz>"))
  ruta
}

# Corre el paso recogiendo TODOS los warnings (se afirman uno a uno, no se dejan caer).
correr <- function(xml, rmd, ...) {
  avisos <- character(0)
  res <- withCallingHandlers(
    suppressMessages(etiquetar_moodle_xml(xml, rmd, ...)),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  list(res = res, avisos = avisos)
}

# Etiquetas de la(s) pregunta(s) con ese nombre, en el orden en que están en el archivo.
tags_de <- function(ruta, nombre) {
  doc <- xml2::read_xml(ruta)
  q <- xml2::xml_find_all(doc, sprintf("//question[normalize-space(name/text)='%s']", nombre))
  lapply(q, function(x) xml2::xml_text(xml2::xml_find_all(x, "./tags/tag/text")))
}

COMP_ARG <- "competencia::Argumentación"            # competencia::Argumentación
COMPO_NUM <- "componente::Numérico-variacional"      # componente::Numérico-variacional

# -----------------------------------------------------------------------------
test_that("la función existe y se carga", {
  expect_true(file.exists(FUNCION), info = paste("falta la función:", FUNCION))
  expect_true(is.function(etiquetar_moodle_xml))
  skip_if_not_installed("xml2")
})

test_that("una pregunta con los tres exextra recibe las cuatro etiquetas del banco", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_completo", c("exextra[Competencia]: Argumentacion", "exextra[Nivel]: 3",
                                  "exextra[Componente]: Numerico-Variacional"))
  xml <- crear_xml(dir, categoria_xml(), pregunta_xml("R1 Q1 : ej_completo"),
                   pregunta_xml("R2 Q1 : ej_completo"))
  antes <- unname(tools::md5sum(xml))
  out <- file.path(dir, "etiquetado.xml")

  r <- correr(xml, file.path(dir, "ej_completo.Rmd"), salida = out)

  esperadas <- c("area::Matematicas", COMP_ARG, "nivel::3", COMPO_NUM)
  expect_equal(tags_de(out, "R1 Q1 : ej_completo")[[1]], esperadas)
  expect_equal(tags_de(out, "R2 Q1 : ej_completo")[[1]], esperadas)
  expect_equal(unname(r$res), c(2L, 8L, 0L))
  expect_length(r$avisos, 0L)
  expect_equal(unname(tools::md5sum(xml)), antes, info = "con `salida` el original no se toca")
})

test_that("el exextra se lee con tipo opcional y espacios sobrantes", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_sintaxis",
            extras = c("exextra[Competencia, character]:   Formulacion y Ejecucion  ",
                       "exextra[Nivel, numeric]:  2 ", "exextra[Componente]: Geometrico-Metrico"))
  xml <- crear_xml(dir, pregunta_xml("R1 Q1 : ej_sintaxis"))
  r <- correr(xml, file.path(dir, "ej_sintaxis.Rmd"))
  expect_equal(tags_de(xml, "R1 Q1 : ej_sintaxis")[[1]],
               c("area::Matematicas", "competencia::Formulación y ejecución", "nivel::2",
                 "componente::Geométrico-métrico"))
  expect_length(r$avisos, 0L)
})

test_that("un exextra que aparece fuera de Meta-information no se lee", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_decoy", extras = "exextra[Competencia]: Argumentacion",
            decoy = c("exextra[Nivel]: 4", "exextra[Componente]: Aleatorio"))
  xml <- crear_xml(dir, pregunta_xml("R1 Q1 : ej_decoy"))
  correr(xml, file.path(dir, "ej_decoy.Rmd"))
  expect_equal(tags_de(xml, "R1 Q1 : ej_decoy")[[1]], c("area::Matematicas", COMP_ARG))
})

test_that("si falta algún exextra solo sale lo que existe; sin ninguno, solo area::Matematicas", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_parcial", "exextra[Competencia]: Interpretacion")
  crear_rmd(dir, "ej_vacio", "exextra[Nivel]:")   # clave presente pero sin valor = falta
  xml <- crear_xml(dir, pregunta_xml("R1 Q1 : ej_parcial"), pregunta_xml("R1 Q2 : ej_vacio", "cloze"))
  r <- correr(xml, file.path(dir, c("ej_parcial.Rmd", "ej_vacio.Rmd")))
  expect_equal(tags_de(xml, "R1 Q1 : ej_parcial")[[1]],
               c("area::Matematicas", "competencia::Interpretación y representación"))
  expect_equal(tags_de(xml, "R1 Q2 : ej_vacio")[[1]], "area::Matematicas")
  expect_length(r$avisos, 0L)
})

test_that("un valor fuera del vocabulario o ambiguo no se copia ni se inventa: avisa y se omite", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_raro", c("exextra[Competencia]: Argumentacion y Formulacion",  # dos valores
                              "exextra[Nivel]: 9", "exextra[Componente]: Razonamiento"))
  xml <- crear_xml(dir, pregunta_xml("R1 Q1 : ej_raro"))
  r <- correr(xml, file.path(dir, "ej_raro.Rmd"))
  expect_equal(tags_de(xml, "R1 Q1 : ej_raro")[[1]], "area::Matematicas")
  expect_length(r$avisos, 3L)
  expect_true(all(grepl("no se reconoce", r$avisos)))
})

test_that("las categorías quedan sin tags", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_cat", c("exextra[Competencia]: Argumentacion", "exextra[Nivel]: 3"))
  xml <- crear_xml(dir, categoria_xml("$course$/lote/Estadística"), pregunta_xml("R1 Q1 : ej_cat"),
                   categoria_xml())
  cats_antes <- as.character(xml2::xml_find_all(xml2::read_xml(xml), "//question[@type='category']"))
  correr(xml, file.path(dir, "ej_cat.Rmd"))
  doc <- xml2::read_xml(xml)
  expect_length(xml2::xml_find_all(doc, "//question[@type='category']"), 2L)
  expect_length(xml2::xml_find_all(doc, "//question[@type='category']//tags"), 0L)
  expect_equal(as.character(xml2::xml_find_all(doc, "//question[@type='category']")), cats_antes)
  expect_length(xml2::xml_find_all(doc, "//question[not(@type='category')]//tags"), 1L)
})

test_that("un XML que ya trae <tags> no recibe duplicados y conserva lo que tenía", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_con_tags", c("exextra[Competencia]: Argumentacion", "exextra[Nivel]: 2",
                                  "exextra[Componente]: Aleatorio"))
  # Area::Matematicas ya está, con otra capitalización: Moodle no distingue mayúsculas.
  xml <- crear_xml(dir, pregunta_xml("R1 Q1 : ej_con_tags", tags = c("Area::Matematicas", "mi-etiqueta-propia")))
  r <- correr(xml, file.path(dir, "ej_con_tags.Rmd"))
  doc <- xml2::read_xml(xml)
  expect_length(xml2::xml_find_all(doc, "//question/tags"), 1L)   # no se crea un segundo <tags>
  expect_equal(tags_de(xml, "R1 Q1 : ej_con_tags")[[1]],
               c("Area::Matematicas", "mi-etiqueta-propia", COMP_ARG, "nivel::2", "componente::Aleatorio"))
  expect_equal(unname(r$res[["agregadas"]]), 3L)
})

test_that("las tildes quedan intactas en UTF-8 (bytes)", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_tildes", c("exextra[Competencia]: Interpretación y representación",
                                "exextra[Nivel]: 2", "exextra[Componente]: Numérico variacional"))
  xml <- crear_xml(dir, pregunta_xml("R1 Q1 : ej_tildes"))
  correr(xml, file.path(dir, "ej_tildes.Rmd"))
  txt <- rawToChar(readBin(xml, "raw", file.size(xml)))
  Encoding(txt) <- "UTF-8"
  expect_true(grepl("<text>competencia::Interpretación y representación</text>", txt, fixed = TRUE))
  expect_true(grepl("<text>componente::Numérico-variacional</text>", txt, fixed = TRUE))
  expect_true(grepl(TEXTO_PREGUNTA, txt, fixed = TRUE), info = "el CDATA con tildes y entidad queda igual")
  expect_false(grepl("Ã", txt, fixed = TRUE), info = "sin doble codificación (Ã©)")
  expect_match(readLines(xml, n = 1L, warn = FALSE), 'encoding="UTF-8"', fixed = TRUE)
})

test_that("correrlo dos veces deja el archivo idéntico y no agrega nada", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_idem", c("exextra[Competencia]: Argumentacion", "exextra[Nivel]: 3"))
  xml <- crear_xml(dir, pregunta_xml("R1 Q1 : ej_idem"), pregunta_xml("R2 Q1 : ej_idem"))
  primera <- correr(xml, file.path(dir, "ej_idem.Rmd"))
  md5 <- unname(tools::md5sum(xml))
  segunda <- correr(xml, file.path(dir, "ej_idem.Rmd"))
  expect_equal(unname(primera$res[["agregadas"]]), 6L)
  expect_equal(unname(segunda$res[["agregadas"]]), 0L)
  expect_equal(unname(tools::md5sum(xml)), md5)
})

test_that("una pregunta sin .Rmd correspondiente queda intacta y avisa", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_propio", "exextra[Nivel]: 3")
  xml <- crear_xml(dir, pregunta_xml("R1 Q1 : ej_propio"), pregunta_xml("R1 Q2 : ej_ajeno"))
  r <- correr(xml, file.path(dir, "ej_propio.Rmd"))
  expect_length(tags_de(xml, "R1 Q2 : ej_ajeno")[[1]], 0L)
  expect_equal(unname(r$res[["sin_fuente"]]), 1L)
  expect_length(r$avisos, 1L)
  expect_match(r$avisos, "ej_ajeno", fixed = TRUE)
})

test_that("si el .Rmd define extitle, ese es el título con el que se cruza (como en exams2moodle)", {
  skip_if_not_installed("xml2")
  dir <- withr::local_tempdir()
  crear_rmd(dir, "ej_con_titulo", c("extitle: Titulo Propio del Ejercicio", "exextra[Nivel]: 4"))
  xml <- crear_xml(dir, pregunta_xml("R1 Q1 : Titulo Propio del Ejercicio"),
                   pregunta_xml("R1 Q2 : ej_con_titulo"))   # el nombre del archivo ya no es su título
  r <- correr(xml, file.path(dir, "ej_con_titulo.Rmd"))
  expect_equal(tags_de(xml, "R1 Q1 : Titulo Propio del Ejercicio")[[1]], c("area::Matematicas", "nivel::4"))
  expect_length(tags_de(xml, "R1 Q2 : ej_con_titulo")[[1]], 0L)
  expect_equal(unname(r$res[["sin_fuente"]]), 1L)
})

test_that("canario: el nombre que escribe exams2moodle() real es el que el cruce espera", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("exams")
  base <- withr::local_tempdir()
  ej <- file.path(base, "ej"); sal <- file.path(base, "salida")
  dir.create(ej); dir.create(sal)
  crear_rmd(ej, "canario_tags", c("exextra[Competencia]: Argumentación", "exextra[Nivel]: 3",
                                  "exextra[Componente]: Aleatorio"))
  suppressWarnings(suppressMessages(
    exams::exams2moodle("canario_tags.Rmd", n = 2, name = "canario_", dir = sal, edir = ej)))
  xml <- file.path(sal, "canario_.xml")
  expect_true(file.exists(xml))
  r <- correr(xml, file.path(ej, "canario_tags.Rmd"))
  expect_length(r$avisos, 0L)
  expect_equal(unname(r$res[["sin_fuente"]]), 0L)
  expect_equal(unname(r$res[["agregadas"]]), 8L)
})
