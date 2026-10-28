# =============================================================================
# test_workflow_state_status_sello.R — `status` avisa de la firma del auto-sello del hook v6.2
# =============================================================================
#
# ORIGEN (2026-10-28): hasta la v6.2 el hook post-render sellaba, tras cualquier render con 0 errores,
# renderizado_4_formatos y arsenal_post_render sobre el ejercicio_state.json del cwd de la sesión, uno
# tras otro y sin nota. Esos dos sellos no prueban que alguien renderizara los formatos ni revisara el
# informe, y se midieron en el repo (20 estados sin ninguna nota; ver HANDOFF_REPO.md). Se corrigió el
# hook (v6.3: ya no sella) pero los estados viejos siguen ahí: `status` los marca para que se
# re-verifiquen por artefacto al retomarlos.
#
# QUÉ FIJA ESTE TEST
#   1. Con los pasos 5 y 6 completados, el 6 de 40 a 80 ms después del 5 y sin más claves que
#      completado y timestamp: `status` imprime el aviso (la línea exacta, con el ⚠).
#   2. Bordes y aritmética: 40,000 y 80,000 ms avisan; 39,999 y 80,001 no; cruzar el segundo o expresar
#      el 6 en otro huso horario (mismo instante) no cambia el veredicto.
#   3. Con nota (o cualquier clave extra) en cualquiera de los dos pasos: sin aviso.
#   4. Δ de 0 ms, de 500 ms o con el orden invertido: sin aviso.
#   5. Pasos sin completar, solo uno completado o timestamps ilegibles/mezclados: sin aviso y sin error.
#   6. Solo informa: `status` no toca el archivo; `check`, `next` y `complete` no cambian ni llevan
#      el aviso.
#   7. Mutante (quitar el aviso): se aplica sobre una copia del CLI y el caso con firma deja de avisar,
#      así que el test (1) lo atraparía. WORKFLOW_STATE_CLI apunta a otra copia del CLI (con él puesto
#      el mutante embebido no se aplica: mutaría una copia ya mutada).
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

AVISO <- "pasos 5 y 6: firma del auto-sello del hook post-render (v6.2 o anterior), sin nota: re-verificar por artefacto antes de confiar en ellos"
LINEA_AVISO <- paste0("⚠ ", AVISO)

# Exit REAL del proceso (sin tuberías, que enmascaran el código de salida).
cli <- function(args, cli_path = CLI) {
  out <- suppressWarnings(system2("bash", c(shQuote(cli_path), shQuote(args)), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  list(exit = if (is.null(status)) 0L else status, out = out)
}
estado <- function(dir) jsonlite::fromJSON(file.path(dir, "ejercicio_state.json"), simplifyVector = FALSE)
bytes <- function(dir) { f <- file.path(dir, "ejercicio_state.json"); readBin(f, "raw", file.size(f)) }
contiene <- function(x, patron) any(grepl(patron, x, fixed = TRUE, useBytes = TRUE))

# Un estado real (hecho con la propia CLI) con los pasos 5 y 6 reescritos a mano: son fixtures JSON
# porque `complete` sella con la hora de ahora y aquí hay que fijar la diferencia entre los dos.
fixture <- function(t5, t6, extra5 = list(), extra6 = list(), completado5 = TRUE, completado6 = TRUE) {
  dir <- file.path(tempfile("wfs_"), "ej"); dir.create(dir, recursive = TRUE)
  cli(c("init", dir, "--tipo", "schoice"))
  f <- file.path(dir, "ejercicio_state.json")
  est <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  est$pasos$renderizado_4_formatos <- c(list(completado = completado5, timestamp = t5), extra5)
  est$pasos$arsenal_post_render    <- c(list(completado = completado6, timestamp = t6), extra6)
  jsonlite::write_json(est, f, auto_unbox = TRUE, pretty = TRUE, null = "null")
  dir
}

muestra_aviso <- function(dir, cli_path = CLI) {
  r <- cli(c("status", dir), cli_path)
  list(exit = r$exit, aviso = contiene(r$out, LINEA_AVISO), out = r$out)
}

T5      <- "2026-10-27T23:07:18.528788-05:00"
T6_REAL <- "2026-10-27T23:07:18.586313-05:00"        # los valores medidos en un estado real: 57,525 ms

context("workflow-state.sh status: firma del auto-sello")

test_that("con la firma (6 unos 57 ms despues del 5, sin claves extra): status avisa con la linea exacta", {
  r <- muestra_aviso(fixture(T5, T6_REAL))
  expect_equal(r$exit, 0L)
  expect_true(r$aviso, info = paste(r$out, collapse = "\n"))
  expect_equal(sum(grepl(AVISO, r$out, fixed = TRUE, useBytes = TRUE)), 1L)   # una sola vez
})

test_that("bordes y aritmética: 40,000 y 80,000 ms avisan; 39,999 y 80,001 no; segundo y huso no importan", {
  avisan <- list(
    "40,000 ms"                  = c(T5, "2026-10-27T23:07:18.568788-05:00"),
    "80,000 ms"                  = c(T5, "2026-10-27T23:07:18.608788-05:00"),
    "cruza el segundo (50 ms)"   = c("2026-10-27T23:07:18.990000-05:00", "2026-10-27T23:07:19.040000-05:00"),
    "otro huso, mismo instante"  = c(T5, "2026-10-28T04:07:18.586313+00:00"))
  for (n in names(avisan)) expect_true(muestra_aviso(fixture(avisan[[n]][1], avisan[[n]][2]))$aviso, info = n)
  no_avisan <- list(
    "39,999 ms" = c(T5, "2026-10-27T23:07:18.568787-05:00"),
    "80,001 ms" = c(T5, "2026-10-27T23:07:18.608789-05:00"))
  for (n in names(no_avisan)) expect_false(muestra_aviso(fixture(no_avisan[[n]][1], no_avisan[[n]][2]))$aviso, info = n)
})

test_that("con nota (o cualquier clave extra) en cualquiera de los dos pasos: sin aviso", {
  nota <- list(nota = "Reapertura 8. Revisado a mano contra salida/r6/.")
  expect_false(muestra_aviso(fixture(T5, T6_REAL, extra5 = nota))$aviso, info = "nota en el paso 5")
  expect_false(muestra_aviso(fixture(T5, T6_REAL, extra6 = nota))$aviso, info = "nota en el paso 6")
  expect_false(muestra_aviso(fixture(T5, T6_REAL, extra5 = nota, extra6 = nota))$aviso, info = "nota en los dos")
  expect_false(muestra_aviso(fixture(T5, T6_REAL, extra6 = list(resultado = "x")))$aviso, info = "otra clave extra")
})

test_that("diferencia de 0 ms, de 500 ms o con el orden invertido: sin aviso", {
  casos <- list(
    "0 ms"                   = T5,
    "500 ms"                 = "2026-10-27T23:07:19.028788-05:00",
    "orden invertido (-58 ms)" = "2026-10-27T23:07:18.470788-05:00")
  for (n in names(casos)) expect_false(muestra_aviso(fixture(T5, casos[[n]]))$aviso, info = n)
})

test_that("pasos sin completar, solo uno completado o timestamps ilegibles: sin aviso y sin error", {
  casos <- list(
    "ninguno completado"   = fixture(NULL, NULL, completado5 = FALSE, completado6 = FALSE),
    "solo el 5 completado" = fixture(T5, NULL, completado6 = FALSE),
    "solo el 6 completado" = fixture(NULL, T6_REAL, completado5 = FALSE),
    # Con fecha de firma pero sin completar (un JSON editado a mano): solo `completado` los distingue.
    "5 con fecha, sin completar" = fixture(T5, T6_REAL, completado5 = FALSE),
    "6 con fecha, sin completar" = fixture(T5, T6_REAL, completado6 = FALSE),
    "timestamp ilegible"   = fixture(T5, "no-es-una-fecha"),
    "timestamp nulo en el 6" = fixture(T5, NULL),
    "uno ingenuo y otro con huso" = fixture("2026-10-27T23:07:18.528788", T6_REAL))
  for (n in names(casos)) {
    r <- muestra_aviso(casos[[n]])
    expect_equal(r$exit, 0L, info = n)
    expect_false(r$aviso, info = n)
  }
})

test_that("status solo informa: no toca el archivo y check, next y complete no cambian ni llevan el aviso", {
  dir <- fixture(T5, T6_REAL)
  antes <- bytes(dir)
  expect_true(muestra_aviso(dir)$aviso)
  expect_identical(bytes(dir), antes)                        # status es de solo lectura
  expect_equal(cli(c("check", dir, "renderizado_4_formatos"))$exit, 0L)
  expect_equal(cli(c("check", dir, "arsenal_post_render"))$exit, 0L)
  expect_equal(cli(c("check", dir, "detractor_fase2c"))$exit, 1L)
  salidas <- list(cli(c("check", dir, "arsenal_post_render"))$out, cli(c("next", dir))$out)
  for (s in salidas) expect_false(contiene(s, "firma del auto-sello"))
  r <- cli(c("complete", dir, "detractor_fase2c", "--veredicto", "APROBAR"))
  expect_equal(r$exit, 0L)
  expect_false(contiene(r$out, "firma del auto-sello"))
  est <- estado(dir)$pasos                                   # los pasos 5 y 6 siguen como estaban
  expect_equal(est$renderizado_4_formatos$timestamp, T5)
  expect_equal(est$arsenal_post_render$timestamp, T6_REAL)
})

test_that("el aviso no aparece en un estado recien creado ni en uno sellado con la CLI y nota", {
  dir <- file.path(tempfile("wfs_"), "ej"); dir.create(dir, recursive = TRUE)
  cli(c("init", dir, "--tipo", "cloze"))
  expect_false(muestra_aviso(dir)$aviso)
  cli(c("complete", dir, "renderizado_4_formatos", "--nota", "HTML, PDF, DOCX y Moodle revisados"))
  cli(c("complete", dir, "arsenal_post_render", "--nota", "informe revisado: 0 errores"))
  expect_false(muestra_aviso(dir)$aviso)
})

test_that("mutante sin el aviso: el caso con firma deja de avisar (el primer test lo atraparia)", {
  skip_if(CLI_EXTERNO, "WORKFLOW_STATE_CLI apunta a otra copia: el mutante se aplica sobre el CLI vivo")
  txt <- paste(readLines(CLI, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ancla <- 'if firma_autosello(state["pasos"]):'
  partes <- strsplit(paste0(txt, "\001FIN"), ancla, fixed = TRUE)[[1]]
  expect_equal(length(partes) - 1L, 1L, info = "el ancla del aviso debe aparecer exactamente una vez")
  mutante <- tempfile(fileext = ".sh")
  writeLines(paste(partes, collapse = "if False:") |> sub(pattern = "\001FIN$", replacement = ""), mutante, useBytes = TRUE)
  dir <- fixture(T5, T6_REAL)
  expect_true(muestra_aviso(dir, CLI)$aviso)                 # el CLI vivo avisa
  expect_false(muestra_aviso(dir, mutante)$aviso)            # el mutante no avisa: el test 1 fallaría
})
