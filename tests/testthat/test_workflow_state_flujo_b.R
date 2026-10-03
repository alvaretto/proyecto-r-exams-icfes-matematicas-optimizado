# =============================================================================
# test_workflow_state_flujo_b.R — el Flujo B no se sella con la sola decisión
# =============================================================================
#
# ORIGEN (2026-10-02, barras-campeonato-baloncesto-n3): el orquestador registró
# la respuesta del WAIT_USER #1 con
#
#   workflow-state.sh complete <dir> flujo_b --requerido true
#
# y la CLI marcó `flujo_b.completado = true` en ese mismo acto, ANTES de generar
# TikZ/Python/R y de que el usuario eligiera lenguaje (WAIT_USER #2). Con eso el
# gate `pre-write-rmd-gate.sh` quedaba abierto: se podía escribir el .Rmd sin
# gráfico elegido. El orquestador lo revirtió a mano; nada lo impedía.
#
# QUÉ FIJA ESTE TEST
#   1. `--requerido true` sin `--lenguaje` registra la decisión y deja el paso
#      PENDIENTE (exit 0, compatible con las llamadas existentes).
#   2. Con el paso pendiente, el gate BLOQUEA la escritura del .Rmd (exit 2).
#   3. `--lenguaje tikz|python|r` sella el paso y abre el gate.
#   4. Un lenguaje fuera de {tikz, python, r} se rechaza sin tocar el estado.
#   5. `--requerido false` sigue sellando en el acto (no hay gráfico que elegir).
#   6. Completar flujo_b sin decisión (requerido = null) es un error.
# =============================================================================

library(testthat)

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE)[1],
  error = function(e) normalizePath("../..", mustWork = FALSE))
if (is.na(repo_root) || !nzchar(repo_root)) repo_root <- normalizePath("../..", mustWork = FALSE)

CLI  <- file.path(repo_root, ".claude", "scripts", "workflow-state.sh")
GATE <- file.path(repo_root, ".claude", "hooks", "pre-write-rmd-gate.sh")

# El gate sólo actúa bajo A-Produccion/01-En-PreDesarrollo: se replica esa
# ruta dentro de un directorio temporal.
nuevo_ejercicio <- function(tipo = "schoice") {
  dir <- file.path(tempfile("wf_"), "A-Produccion", "01-En-PreDesarrollo", "ej")
  dir.create(dir, recursive = TRUE)
  cli(c("init", dir, "--tipo", tipo))
  dir
}

# Exit REAL del proceso (sin tuberías, que enmascaran el código de salida).
cli <- function(args) {
  out <- suppressWarnings(system2("bash", c(CLI, shQuote(args)),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  list(exit = if (is.null(status)) 0L else status, out = out)
}

flujo_b <- function(dir) {
  jsonlite::fromJSON(file.path(dir, "ejercicio_state.json"))$pasos$flujo_b
}

# Nombre conforme a la nomenclatura: si no, el gate bloquea por el NOMBRE y la
# prueba pasaría por la razón equivocada (medido al escribir este test).
RMD <- "ej_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd"

gate <- function(dir) {
  entrada <- tempfile(fileext = ".json")
  writeLines(sprintf('{"tool_input": {"file_path": "%s"}}',
                     file.path(dir, RMD)), entrada)
  out <- suppressWarnings(system2("bash", GATE, stdin = entrada,
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  list(exit = if (is.null(status)) 0L else status, out = out)
}

marcar_analisis <- function(dir) cli(c("complete", dir, "analisis_icfes"))

test_that("la decisión requerido=true NO sella flujo_b y el gate sigue cerrado", {
  dir <- nuevo_ejercicio()
  marcar_analisis(dir)
  r <- cli(c("complete", dir, "flujo_b", "--requerido", "true"))
  expect_equal(r$exit, 0L)
  expect_true(any(grepl("PENDIENTE", r$out)))
  fb <- flujo_b(dir)
  expect_true(fb$requerido)
  expect_false(fb$completado)
  expect_equal(cli(c("check", dir, "flujo_b"))$exit, 1L)
  g <- gate(dir)
  expect_equal(g$exit, 2L)
  expect_true(any(grepl("Flujo B pendiente", g$out)))
})

test_that("--lenguaje sella flujo_b y abre el gate", {
  dir <- nuevo_ejercicio()
  marcar_analisis(dir)
  cli(c("complete", dir, "flujo_b", "--requerido", "true"))
  r <- cli(c("complete", dir, "flujo_b", "--lenguaje", "tikz"))
  expect_equal(r$exit, 0L)
  fb <- flujo_b(dir)
  expect_true(fb$completado)
  expect_equal(fb$lenguaje, "tikz")
  expect_equal(cli(c("check", dir, "flujo_b"))$exit, 0L)
  expect_equal(gate(dir)$exit, 0L)
})

test_that("decisión y lenguaje en una sola llamada también sellan", {
  for (leng in c("tikz", "python", "r")) {
    dir <- nuevo_ejercicio()
    r <- cli(c("complete", dir, "flujo_b", "--requerido", "true", "--lenguaje", leng))
    expect_equal(r$exit, 0L)
    expect_true(flujo_b(dir)$completado)
  }
})

test_that("un lenguaje inválido se rechaza sin tocar el estado", {
  dir <- nuevo_ejercicio()
  cli(c("complete", dir, "flujo_b", "--requerido", "true"))
  antes <- readLines(file.path(dir, "ejercicio_state.json"))
  r <- cli(c("complete", dir, "flujo_b", "--lenguaje", "cobol"))
  expect_equal(r$exit, 2L)
  expect_identical(readLines(file.path(dir, "ejercicio_state.json")), antes)
})

test_that("requerido=false sigue sellando en el acto", {
  dir <- nuevo_ejercicio("cloze")
  marcar_analisis(dir)
  r <- cli(c("complete", dir, "flujo_b", "--requerido", "false"))
  expect_equal(r$exit, 0L)
  expect_true(flujo_b(dir)$completado)
  expect_equal(gate(dir)$exit, 0L)
})

test_that("completar flujo_b sin decisión es un error", {
  dir <- nuevo_ejercicio()
  r <- cli(c("complete", dir, "flujo_b"))
  expect_equal(r$exit, 3L)
  expect_false(flujo_b(dir)$completado)
})
