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

test_that("una tabla corta sale como antes, sin error", {
  df <- data.frame(Mes = c("Enero", "Febrero"), Vasos = c(3, 4))
  out <- fam$tabla_responsiva(df, align = c("l", "c"))
  expect_true(grepl("::: {style=", out, fixed = TRUE))
  expect_true(grepl("|Mes", gsub(" ", "", out), fixed = TRUE))
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
