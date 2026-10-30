# =============================================================================
# test_tabla_responsiva_pdf.R — tabla_responsiva() no deja una tabla fuera del
# margen del PDF (Familia F2, regla #21; Incidente Z de los orquestadores)
# =============================================================================
#
# ORIGEN (2026-10-19, informacion-insuficiente-lote-n4): un CLOZE aprobado llevaba
# dos meses con su «Resumen» —una tabla pipe con frases en las celdas— fuera del
# margen del PDF en 6 de 6 versiones, sin ningún error. exams:::pandoc() añade
# --columns=99999, así que pandoc no reparte anchos relativos y la tabla sale con
# columnas l/c/r que no parten el texto; las plantillas de exams tampoco cargan
# array ni calc, que los anchos p{...} de pandoc 3 necesitan.
#
# QUÉ FIJA ESTE TEST
#   1. Una tabla corta sale como antes (fenced div + tabla pipe), sin error.
#   2. Si la línea pipe supera ancho_max_pdf (85), la función aborta y el mensaje
#      remite a lista_responsiva(). El límite es exacto: 85 pasa, 86 aborta.
#   3. ancho_max_pdf = NULL desactiva la guarda.
#   4. lista_responsiva() emite una línea por fila con el formato verificado.
#   5. Con el pipeline real de R/exams (si hay pdflatex): la tabla numérica de 85
#      caracteres no desborda, la de 97 sí, y la lista de frases largas tampoco
#      desborda. Es la calibración del 2026-10-22; si exams o pandoc cambian, este
#      bloque avisa.
#   6. Separación de cabeceras (2026-10-29, ítem 50), OPCIONAL: por defecto la tabla
#      sale como siempre, sin <span> (la variante desborda en otros ejercicios); con
#      separacion = "0.5em", cada celda va en un <span> con padding-left desde la
#      columna 2 y las cabeceras al 90 %, y la columna 1 no lleva relleno.
#   7. La guarda del PDF mide el texto, no el <span>: con la separación, 85 sigue
#      pasando y 86 abortando, aunque la línea emitida sea mucho más larga.
#   8. Con pandoc (si está): el <span> llega al HTML y desaparece en LaTeX y en DOCX,
#      que conservan el texto de las celdas.
# =============================================================================

library(testthat)

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE)[1],
  error = function(e) normalizePath("../..", mustWork = FALSE))
if (is.na(repo_root) || !nzchar(repo_root)) repo_root <- normalizePath("../..", mustWork = FALSE)

SNIPPETS <- file.path(repo_root, ".claude", "scripts", "snippets_familias_rmd.R")
fam <- new.env()
sys.source(SNIPPETS, envir = fam)

# Largo de la línea pipe que emite kable SIN `align`: la medida de la calibración.
ancho_pipe <- function(df) max(nchar(knitr::kable(df, format = "markdown")))
# Ancho de contenido que mide el helper: texto más largo de cada columna + 2 por
# columna + 1. Coincide con ancho_pipe() y no depende del relleno que añade `align`.
ancho_contenido <- function(df) {
  sum(vapply(seq_along(df), function(j) max(nchar(c(names(df)[j], as.character(df[[j]])))),
             numeric(1))) + 2L * ncol(df) + 1L
}

# Tabla de una columna cuyo ancho de contenido es exactamente n.
tabla_de_ancho <- function(n) {
  df <- data.frame(x = "a")
  names(df) <- strrep("b", n - 3L)
  stopifnot(ancho_contenido(df) == n)
  df
}

# Serie numérica de la calibración: k columnas «Medición i» con números de 4 cifras.
numerica <- function(k) {
  df <- as.data.frame(matrix(sprintf("%d", 1000 + seq_len(3 * k)), nrow = 3))
  names(df) <- paste("Medición", seq_len(k))
  df
}

context("Familia F2: tabla_responsiva() y el margen del PDF")

test_that("una tabla corta sale como antes, sin error ni <span> (por defecto)", {
  df <- data.frame(Mes = c("Enero", "Febrero"), Vasos = c(3, 4))
  out <- fam$tabla_responsiva(df, align = c("l", "c"))
  expect_true(grepl("::: {style=", out, fixed = TRUE))
  expect_true(grepl("|Mes", gsub(" ", "", out), fixed = TRUE))
  expect_false(grepl("<span", out, fixed = TRUE))
})

## Celdas de la línea pipe i (1 = cabecera, 2 = separador, 3.. = cuerpo).
celdas_pipe <- function(out, i) {
  l <- grep("^\\|", strsplit(out, "\n", fixed = TRUE)[[1]], value = TRUE)[i]
  trimws(strsplit(sub("^\\|", "", sub("\\|$", "", l)), "|", fixed = TRUE)[[1]])
}

test_that("separación de cabeceras (opcional): <span> por celda, relleno desde la columna 2, cabeceras al 90 %", {
  df <- data.frame(Sede = c("Norte", "Sur"), Cursos = c("6 cursos", "5 cursos"),
                   Promedio = c("40 kilos", "45 kilos"))
  out <- fam$tabla_responsiva(df, align = c("c", "c", "c"), separacion = "0.5em")
  cab <- celdas_pipe(out, 1); cue <- celdas_pipe(out, 3)
  expect_identical(cab[1], '<span style="display:inline-block; font-size:0.9em;">Sede</span>')
  expect_identical(cab[2], '<span style="display:inline-block; padding-left:0.5em; font-size:0.9em;">Cursos</span>')
  expect_identical(cue[1], '<span style="display:inline-block;">Norte</span>')
  expect_identical(cue[3], '<span style="display:inline-block; padding-left:0.5em;">40 kilos</span>')
  expect_identical(lengths(regmatches(out, gregexpr("<span", out, fixed = TRUE))), 9L)
  # Los parámetros mandan, y tam_cabecera = NULL no deja un «font-size:;» vacío.
  out2 <- fam$tabla_responsiva(df, separacion = "1em", tam_cabecera = NULL)
  expect_true(grepl("padding-left:1em;", out2, fixed = TRUE))
  expect_false(grepl("font-size", out2, fixed = TRUE))
  # La negrita de bold_rows queda dentro del <span>.
  out3 <- fam$tabla_responsiva(df, bold_rows = 1L, separacion = "0.5em")
  expect_true(grepl('<span style="display:inline-block;">**Norte**</span>', out3, fixed = TRUE))
})

test_that("la guarda del PDF mide el texto, no el <span>: 85 pasa y 86 aborta igual", {
  out <- expect_silent(fam$tabla_responsiva(tabla_de_ancho(85), separacion = "0.5em"))
  expect_gt(max(nchar(strsplit(out, "\n", fixed = TRUE)[[1]])), 85L)   # la línea emitida es más larga
  expect_error(fam$tabla_responsiva(tabla_de_ancho(86), separacion = "0.5em"), "mide 86 caracteres")
  expect_silent(fam$tabla_responsiva(numerica(7), separacion = "0.5em"))
})

test_that("con pandoc: el <span> llega al HTML y desaparece en LaTeX y DOCX", {
  pandoc <- Sys.which("pandoc")
  skip_if(!nzchar(pandoc), "sin pandoc")
  df <- data.frame(Sede = c("El Bosque", "Sur"), Cursos = c("6 cursos", "5 cursos"), check.names = FALSE)
  md <- tempfile(fileext = ".md"); writeLines(fam$tabla_responsiva(df, separacion = "0.5em"), md, useBytes = TRUE)
  conv <- function(to, ...) system2(pandoc, c("-f", "markdown", "-t", to, "--wrap=none", md, ...), stdout = TRUE)
  html <- paste(conv("html"), collapse = "\n")
  expect_true(grepl('<span style="display:inline-block; padding-left:0.5em; font-size:0.9em;">Cursos</span>', html, fixed = TRUE))
  tex <- paste(conv("latex"), collapse = "\n")
  expect_false(any(vapply(c("span", "padding", "font-size", "inline-block"), grepl, NA, tex, fixed = TRUE)))
  expect_true(grepl("El~Bosque", tex, fixed = TRUE) && grepl("6 cursos", tex, fixed = TRUE))
  docx <- tempfile(fileext = ".docx"); conv("docx", "-o", docx)
  d <- tempfile(); utils::unzip(docx, "word/document.xml", exdir = d)
  xml <- paste(readLines(file.path(d, "word", "document.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "")
  expect_false(any(vapply(c("span", "padding", "font-size"), grepl, NA, xml, fixed = TRUE)))
  expect_true(grepl("6 cursos", xml, fixed = TRUE) && grepl("Cursos", xml, fixed = TRUE))
})

test_that("la serie numérica de la calibración: 85 caracteres pasa, 97 aborta", {
  expect_identical(ancho_pipe(numerica(7)), 85L)
  expect_identical(ancho_pipe(numerica(8)), 97L)
  expect_equal(ancho_contenido(numerica(7)), 85)
  expect_equal(ancho_contenido(numerica(8)), 97)
  expect_silent(fam$tabla_responsiva(numerica(7)))
  expect_error(fam$tabla_responsiva(numerica(8)), "lista_responsiva")
})

test_that("el `align` no cambia el veredicto (solo cambia el relleno de la línea)", {
  expect_silent(fam$tabla_responsiva(numerica(7), align = rep("c", 7)))
  expect_error(fam$tabla_responsiva(numerica(8), align = rep("c", 8)), "mide 97 caracteres")
})

test_that("el límite es exacto: 85 pasa y 86 aborta", {
  expect_silent(fam$tabla_responsiva(tabla_de_ancho(85)))
  expect_error(fam$tabla_responsiva(tabla_de_ancho(86)), "mide 86 caracteres")
})

test_that("ancho_max_pdf = NULL desactiva la guarda", {
  out <- fam$tabla_responsiva(numerica(8), ancho_max_pdf = NULL)
  expect_true(is.character(out) && length(out) == 1L)
})

test_that("lista_responsiva(): una línea por fila, con el formato verificado", {
  df <- data.frame(Pregunta = c("¿A?", "¿B?"), Veredicto = c("No", "Sí"),
                   Respuesta = c("r1", "r2"), check.names = FALSE)
  expect_identical(fam$lista_responsiva(df), "- ¿A? — **No**: r1\n- ¿B? — **Sí**: r2\n")
  expect_identical(fam$lista_responsiva(df[1:2]), "- ¿A? — **No**\n- ¿B? — **Sí**\n")
  df4 <- cbind(df, Extra = c("e1", "e2"))
  expect_identical(fam$lista_responsiva(df4), "- ¿A? — **No**: r1; e1\n- ¿B? — **Sí**: r2; e2\n")
  expect_identical(fam$lista_responsiva(df, negrita = NULL), "- ¿A? — No: r1\n- ¿B? — Sí: r2\n")
})

test_that("calibración con el pipeline real de R/exams (plantilla plain)", {
  skip_if(!nzchar(Sys.which("pdflatex")), "sin pdflatex")
  skip_if_not_installed("exams")
  suppressMessages(library(exams))
  # Desbordes de alineación (tablas) y totales de un ejercicio mínimo con `cuerpo`.
  medir <- function(cuerpo, etiqueta) {
    d <- tempfile("f2_"); dir.create(d)
    rmd <- file.path(d, paste0(etiqueta, ".Rmd"))
    writeLines(c("Question", "========",
                 "```{=latex}", "\\makeatletter\\@ifundefined{c@none}{\\newcounter{none}}{}\\makeatother", "```",
                 "", cuerpo, "", "Answerlist", "----------", "* A", "* B", "",
                 "Meta-information", "================", paste0("exname: ", etiqueta),
                 "extype: schoice", "exsolution: 10", "exshuffle: FALSE"), rmd)
    td <- file.path(d, "tex"); od <- file.path(d, "out"); dir.create(td); dir.create(od)
    suppressWarnings(suppressMessages(exams2pdf(rmd, n = 1, dir = od, texdir = td, verbose = FALSE)))
    texs <- list.files(td, pattern = "\\.tex$", full.names = TRUE)
    main <- texs[vapply(texs, function(t) any(grepl("\\\\documentclass", readLines(t, warn = FALSE))), logical(1))]
    old <- setwd(td); on.exit(setwd(old), add = TRUE)
    system2("pdflatex", c("-interaction=nonstopmode", basename(main)), stdout = FALSE, stderr = FALSE)
    ll <- readLines(sub("\\.tex$", ".log", main), warn = FALSE)
    c(alineacion = sum(grepl("^Overfull \\\\hbox .* in alignment", ll)),
      total = sum(grepl("^Overfull \\\\hbox", ll)))
  }
  tabla_md <- function(df) paste(knitr::kable(df, format = "markdown"), collapse = "\n")
  expect_identical(unname(medir(tabla_md(numerica(7)), "f2_85")["alineacion"]), 0L)
  expect_gt(unname(medir(tabla_md(numerica(8)), "f2_97")["alineacion"]), 0L)
  frases <- data.frame(
    Pregunta = "¿Cuál es la ubicación que le corresponde a cada hermano dentro del lote?",
    Veredicto = "No",
    Respuesta = "El enunciado no dice por dónde pasan los cortes, así que ni siquiera está definida la porción que da a la carretera",
    check.names = FALSE)
  expect_identical(unname(medir(fam$lista_responsiva(frases), "f2_lista")["total"]), 0L)
})
