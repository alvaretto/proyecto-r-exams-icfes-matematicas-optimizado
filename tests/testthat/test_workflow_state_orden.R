# =============================================================================
# test_workflow_state_orden.R — `complete` avisa del sello fuera de orden
# =============================================================================
#
# ORIGEN (2026-11-03): `complete` aceptó en silencio los pasos 5 y 6 con el 4 pendiente.
# Un sello fuera de orden es señal de que el paso se marcó sin su artefacto previo
# (regla #16: en orden y cada uno tras su artefacto). El sello se registra igual (no se
# bloquea el flujo), pero `complete` lo avisa por stderr, con los pendientes en el orden
# del workflow.
#
# QUÉ FIJA ESTE TEST
#   1. En orden, del 1 al 11: ningún aviso en ningún paso.
#   2. Sellar el paso 5 con el 4 pendiente: exit 0, aviso exacto que nombra los pendientes
#      en orden, y el JSON queda con el paso 5 `completado`.
#   3. Tras completar el 4, sellar el 6: sin aviso.
#   4. flujo_b --requerido true (solo decisión): sin aviso (esa rama no sella).
#   5. Mutante (quitar el aviso): sobre una copia del CLI, el caso del paso 5 deja de
#      avisar pero sigue sellando, así que el test (2) lo atraparía. WORKFLOW_STATE_CLI
#      apunta a otra copia del CLI (con él puesto el mutante embebido no se aplica).
# =============================================================================

library(testthat)

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE)[1],
  error = function(e) normalizePath("../..", mustWork = FALSE))
if (is.na(repo_root) || !nzchar(repo_root)) repo_root <- normalizePath("../..", mustWork = FALSE)

CLI_EXTERNO <- nzchar(Sys.getenv("WORKFLOW_STATE_CLI", ""))
CLI <- if (CLI_EXTERNO) Sys.getenv("WORKFLOW_STATE_CLI") else
  file.path(repo_root, ".claude", "scripts", "workflow-state.sh")
stopifnot(file.exists(CLI))

# Exit REAL del proceso (sin tuberías, que enmascaran el código de salida).
cli <- function(args, cli_path = CLI) {
  out <- suppressWarnings(system2("bash", c(shQuote(cli_path), shQuote(args)), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  list(exit = if (is.null(status)) 0L else status, out = out)
}
estado <- function(dir) jsonlite::fromJSON(file.path(dir, "ejercicio_state.json"), simplifyVector = FALSE)
contiene <- function(x, patron) any(grepl(patron, x, fixed = TRUE, useBytes = TRUE))

# Un estado recién creado con la propia CLI (init), en un directorio temporal.
nuevo_estado <- function() {
  dir <- file.path(tempfile("wfs_orden_"), "ej")
  dir.create(dir, recursive = TRUE)
  stopifnot(cli(c("init", dir, "--tipo", "schoice"))$exit == 0L)
  dir
}

AVISO_PASO5 <- paste0("⚠ Sello fuera de orden: 'renderizado_4_formatos' se selló con pasos ",
                      "anteriores pendientes: retroalimentacion. Regla #16: en orden y cada uno ",
                      "tras su artefacto.")

context("workflow-state.sh complete: aviso de sello fuera de orden")

test_that("en orden, del 1 al 11: ningun aviso en ningun paso", {
  dir <- nuevo_estado()
  pasos <- list(
    c("analisis_icfes", "--resultado", "Clasificado N2"),
    c("flujo_b", "--requerido", "false"),
    c("generacion_rmd", "--archivo", "ejemplo.Rmd"),
    c("retroalimentacion"),
    c("renderizado_4_formatos", "--nota", "HTML, PDF, DOCX y Moodle revisados"),
    c("arsenal_post_render", "--nota", "informe revisado: 0 errores"),
    c("detractor_fase2c", "--veredicto", "APROBAR"),
    c("coherencias_5"),
    c("validar_diversidad", "--versiones_unicas", "42"),
    c("validar_icfes"),
    c("aprobacion_usuario"))
  for (p in pasos) {
    r <- cli(c("complete", dir, p))
    expect_equal(r$exit, 0L, info = paste("paso", p[1], ":", paste(r$out, collapse = "\n")))
    expect_false(contiene(r$out, "Sello fuera de orden"),
                 info = paste("aviso inesperado al sellar", p[1], ":", paste(r$out, collapse = "\n")))
  }
  expect_match(paste(cli(c("status", dir))$out, collapse = "\n"), "Progreso: 11/11", fixed = TRUE)
})

test_that("sellar el paso 5 con el 4 pendiente: exit 0, aviso exacto y el sello queda registrado", {
  dir <- nuevo_estado()
  cli(c("complete", dir, "analisis_icfes"))
  cli(c("complete", dir, "flujo_b", "--requerido", "false"))
  cli(c("complete", dir, "generacion_rmd", "--archivo", "ejemplo.Rmd"))
  r <- cli(c("complete", dir, "renderizado_4_formatos"))
  expect_equal(r$exit, 0L, info = paste(r$out, collapse = "\n"))
  expect_true(contiene(r$out, AVISO_PASO5), info = paste(r$out, collapse = "\n"))
  expect_equal(sum(grepl(AVISO_PASO5, r$out, fixed = TRUE, useBytes = TRUE)), 1L,
               info = paste(r$out, collapse = "\n"))
  pasos <- estado(dir)$pasos
  expect_true(pasos$renderizado_4_formatos$completado)
  expect_false(pasos$retroalimentacion$completado)
})

test_that("el aviso va solo por stderr: stdout queda como antes", {
  dir <- nuevo_estado()
  r <- suppressWarnings(system2("bash", c(shQuote(CLI), shQuote(c("complete", dir, "coherencias_5"))),
                                stdout = TRUE, stderr = FALSE))
  expect_false(contiene(r, "Sello fuera de orden"), info = paste(r, collapse = "\n"))
  expect_true(contiene(r, "marcado como completado"), info = paste(r, collapse = "\n"))
})

test_that("el aviso nombra los pendientes en el orden del workflow", {
  dir <- nuevo_estado()
  r <- cli(c("complete", dir, "coherencias_5"))
  expect_equal(r$exit, 0L, info = paste(r$out, collapse = "\n"))
  aviso <- paste0("⚠ Sello fuera de orden: 'coherencias_5' se selló con pasos anteriores pendientes: ",
                  "analisis_icfes, flujo_b, generacion_rmd, retroalimentacion, renderizado_4_formatos, ",
                  "arsenal_post_render, detractor_fase2c. Regla #16: en orden y cada uno tras su artefacto.")
  expect_true(contiene(r$out, aviso), info = paste(r$out, collapse = "\n"))
})

test_that("tras completar el 4, sellar el 6: sin aviso", {
  dir <- nuevo_estado()
  cli(c("complete", dir, "analisis_icfes"))
  cli(c("complete", dir, "flujo_b", "--requerido", "false"))
  cli(c("complete", dir, "generacion_rmd", "--archivo", "ejemplo.Rmd"))
  cli(c("complete", dir, "renderizado_4_formatos"))   # fuera de orden: aquí sí avisa (no se comprueba)
  cli(c("complete", dir, "retroalimentacion"))
  r <- cli(c("complete", dir, "arsenal_post_render"))
  expect_equal(r$exit, 0L, info = paste(r$out, collapse = "\n"))
  expect_false(contiene(r$out, "Sello fuera de orden"), info = paste(r$out, collapse = "\n"))
})

test_that("flujo_b --requerido true (solo decision): registra sin aviso y sin sellar", {
  dir <- nuevo_estado()
  r <- cli(c("complete", dir, "flujo_b", "--requerido", "true"))
  expect_equal(r$exit, 0L, info = paste(r$out, collapse = "\n"))
  expect_false(contiene(r$out, "Sello fuera de orden"), info = paste(r$out, collapse = "\n"))
  pasos <- estado(dir)$pasos
  expect_true(pasos$flujo_b$requerido)
  expect_false(pasos$flujo_b$completado)
})

test_that("mutante sin el aviso: sellar el 5 con el 4 pendiente deja de avisar", {
  skip_if(CLI_EXTERNO, "WORKFLOW_STATE_CLI apunta a otra copia: el mutante se aplica sobre el CLI vivo")
  lineas <- readLines(CLI, warn = FALSE, encoding = "UTF-8")
  i <- grep("Sello fuera de orden", lineas, fixed = TRUE)
  expect_equal(length(i), 1L, info = "el print del aviso debe aparecer exactamente una vez en el CLI")
  mut <- lineas
  mut[i] <- sub("Sello fuera de orden", "MUTANTE-SIN-AVISO", mut[i], fixed = TRUE)
  mutante <- tempfile(fileext = ".sh")
  writeLines(mut, mutante, useBytes = TRUE)
  dir_vivo <- nuevo_estado()
  cli(c("complete", dir_vivo, "analisis_icfes"))
  cli(c("complete", dir_vivo, "flujo_b", "--requerido", "false"))
  cli(c("complete", dir_vivo, "generacion_rmd", "--archivo", "ejemplo.Rmd"))
  expect_true(contiene(cli(c("complete", dir_vivo, "renderizado_4_formatos"), CLI)$out,
                       "Sello fuera de orden"))
  dir_mut <- nuevo_estado()
  cli(c("complete", dir_mut, "analisis_icfes"))
  cli(c("complete", dir_mut, "flujo_b", "--requerido", "false"))
  cli(c("complete", dir_mut, "generacion_rmd", "--archivo", "ejemplo.Rmd"))
  r <- cli(c("complete", dir_mut, "renderizado_4_formatos"), mutante)
  expect_equal(r$exit, 0L, info = paste(r$out, collapse = "\n"))
  expect_false(contiene(r$out, "Sello fuera de orden"), info = paste(r$out, collapse = "\n"))
  expect_true(estado(dir_mut)$pasos$renderizado_4_formatos$completado)
})
