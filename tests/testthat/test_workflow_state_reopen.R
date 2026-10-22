# =============================================================================
# test_workflow_state_reopen.R — reabrir un ejercicio sellado sin editar el JSON
# =============================================================================
#
# ORIGEN (2026-10-19, informacion-insuficiente-lote-n4): un detractor
# independiente dio RECHAZAR sobre un CLOZE ya aprobado (11/11). Para reabrirlo
# hubo que poner `validar_icfes` y `aprobacion_usuario` en false editando
# ejercicio_state.json a mano: la CLI no tenía comando para des-sellar, y nada
# dejaba constancia de quién reabrió ni por qué.
#
# QUÉ FIJA ESTE TEST
#   1. `reopen` sin --motivo, con un motivo en blanco, sin --desde, con un paso
#      inexistente o con analisis_icfes / flujo_b falla y deja el archivo intacto.
#   2. `--desde validar_icfes` des-sella ese paso y los posteriores (completado
#      false, timestamp null), no toca los anteriores y registra la reapertura
#      (motivo, reporte, pasos des-sellados y estado previo). `check`, `next` y
#      `status` lo reflejan.
#   3. `--desde generacion_rmd` des-sella los nueve pasos reabribles; conserva
#      `archivo`, vuelve a null `veredicto` y `versiones_unicas` (las claves que
#      el schema exige) y guarda los valores viejos en estado_previo.
#   4. Si no queda nada sellado desde ese paso: exit 1 y archivo intacto.
#   5. Tras reabrir, `complete` vuelve a sellar con normalidad, y una segunda
#      reapertura se acumula en la lista.
#   6. Cada registro y cada paso des-sellado cumplen el schema (no hay validador
#      JSON Schema instalado ni en R ni en Python: se contrasta leyendo el schema).
# =============================================================================

library(testthat)

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE)[1],
  error = function(e) normalizePath("../..", mustWork = FALSE))
if (is.na(repo_root) || !nzchar(repo_root)) repo_root <- normalizePath("../..", mustWork = FALSE)

CLI    <- file.path(repo_root, ".claude", "scripts", "workflow-state.sh")
SCHEMA <- file.path(repo_root, ".claude", "schemas", "ejercicio_state.schema.json")
PASOS  <- c("analisis_icfes", "flujo_b", "generacion_rmd", "retroalimentacion",
            "renderizado_4_formatos", "arsenal_post_render", "detractor_fase2c",
            "coherencias_5", "validar_diversidad", "validar_icfes", "aprobacion_usuario")
REABRIBLES <- PASOS[3:11]

# Exit REAL del proceso (sin tuberías, que enmascaran el código de salida).
cli <- function(args) {
  out <- suppressWarnings(system2("bash", c(CLI, shQuote(args)),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  list(exit = if (is.null(status)) 0L else status, out = out)
}
estado <- function(dir) jsonlite::fromJSON(file.path(dir, "ejercicio_state.json"),
                                           simplifyVector = FALSE)
bytes  <- function(dir) {
  f <- file.path(dir, "ejercicio_state.json")
  readBin(f, "raw", file.size(f))
}

# Un ejercicio con los 11 pasos sellados, hecho con la propia CLI.
sellado_completo <- function() {
  dir <- file.path(tempfile("wfr_"), "ej")
  dir.create(dir, recursive = TRUE)
  cli(c("init", dir, "--tipo", "cloze"))
  cli(c("complete", dir, "analisis_icfes"))
  cli(c("complete", dir, "flujo_b", "--requerido", "false"))
  cli(c("complete", dir, "generacion_rmd", "--archivo", "x.Rmd"))
  for (p in c("retroalimentacion", "renderizado_4_formatos", "arsenal_post_render"))
    cli(c("complete", dir, p))
  cli(c("complete", dir, "detractor_fase2c", "--veredicto", "APROBAR"))
  cli(c("complete", dir, "coherencias_5"))
  cli(c("complete", dir, "validar_diversidad", "--versiones_unicas", "300"))
  cli(c("complete", dir, "validar_icfes"))
  cli(c("complete", dir, "aprobacion_usuario"))
  stopifnot(all(vapply(estado(dir)$pasos[PASOS], function(x) isTRUE(x$completado), logical(1))))
  dir
}

context("workflow-state.sh reopen")

test_that("invocaciones inválidas fallan y dejan el archivo intacto", {
  dir <- sellado_completo()
  antes <- bytes(dir)
  casos <- list(
    c("reopen", dir, "--desde", "validar_icfes"),
    c("reopen", dir, "--desde", "validar_icfes", "--motivo", "   "),
    c("reopen", dir, "--motivo", "x"),
    c("reopen", dir, "--desde", "paso_inexistente", "--motivo", "x"),
    c("reopen", dir, "--desde", "analisis_icfes", "--motivo", "x"),
    c("reopen", dir, "--desde", "flujo_b", "--motivo", "x"))
  for (a in casos) {
    r <- cli(a)
    expect_true(r$exit != 0L, info = paste(a[-2], collapse = " "))
    expect_identical(bytes(dir), antes, info = paste(a[-2], collapse = " "))
  }
})

test_that("--desde validar_icfes des-sella los dos últimos pasos y registra la reapertura", {
  dir <- sellado_completo()
  e0 <- estado(dir)
  r <- cli(c("reopen", dir, "--desde", "validar_icfes",
             "--motivo", "RECHAZAR del detractor independiente", "--reporte", "R.md"))
  expect_equal(r$exit, 0L)
  e <- estado(dir)
  for (p in c("validar_icfes", "aprobacion_usuario")) {
    expect_false(e$pasos[[p]]$completado, info = p)
    expect_null(e$pasos[[p]]$timestamp, info = p)
  }
  for (p in PASOS[1:9]) expect_identical(e$pasos[[p]], e0$pasos[[p]], info = p)
  expect_length(e$reaperturas, 1L)
  re <- e$reaperturas[[1]]
  expect_identical(re$desde, "validar_icfes")
  expect_identical(re$motivo, "RECHAZAR del detractor independiente")
  expect_identical(re$reporte, "R.md")
  expect_identical(unlist(re$pasos_desellados), c("validar_icfes", "aprobacion_usuario"))
  expect_identical(re$estado_previo$validar_icfes$timestamp, e0$pasos$validar_icfes$timestamp)
  expect_equal(cli(c("check", dir, "validar_icfes"))$exit, 1L)
  expect_true(any(grepl("validar icfes", cli(c("next", dir))$out, fixed = TRUE)))
  expect_true(any(grepl("Reaperturas: 1", cli(c("status", dir))$out, fixed = TRUE)))
})

test_that("--desde generacion_rmd des-sella los nueve y guarda lo que tenían", {
  dir <- sellado_completo()
  r <- cli(c("reopen", dir, "--desde", "generacion_rmd", "--motivo", "clave falsa en la Parte 6"))
  expect_equal(r$exit, 0L)
  e <- estado(dir)
  for (p in REABRIBLES) expect_false(e$pasos[[p]]$completado, info = p)
  expect_true(e$pasos$analisis_icfes$completado)
  expect_true(e$pasos$flujo_b$completado)
  expect_identical(e$pasos$generacion_rmd$archivo, "x.Rmd")
  expect_null(e$pasos$detractor_fase2c$veredicto)
  expect_null(e$pasos$validar_diversidad$versiones_unicas)
  re <- e$reaperturas[[1]]
  expect_identical(unlist(re$pasos_desellados), REABRIBLES)
  expect_null(re$reporte)
  expect_identical(re$estado_previo$detractor_fase2c$veredicto, "APROBAR")
  expect_equal(re$estado_previo$validar_diversidad$versiones_unicas, 300)
  expect_identical(re$estado_previo$generacion_rmd$archivo, "x.Rmd")

  # Nada que reabrir: exit 1 y archivo intacto.
  antes <- bytes(dir)
  r2 <- cli(c("reopen", dir, "--desde", "generacion_rmd", "--motivo", "otra vez"))
  expect_equal(r2$exit, 1L)
  expect_identical(bytes(dir), antes)
})

test_that("tras reabrir, complete vuelve a sellar y las reaperturas se acumulan", {
  dir <- sellado_completo()
  cli(c("reopen", dir, "--desde", "validar_icfes", "--motivo", "primera"))
  r <- cli(c("complete", dir, "validar_icfes"))
  expect_equal(r$exit, 0L)
  expect_true(estado(dir)$pasos$validar_icfes$completado)
  cli(c("reopen", dir, "--desde", "generacion_rmd", "--motivo", "segunda"))
  e <- estado(dir)
  expect_length(e$reaperturas, 2L)
  expect_identical(vapply(e$reaperturas, function(x) x$motivo, character(1)), c("primera", "segunda"))
  # La segunda solo des-sella lo que estaba sellado: aprobacion_usuario seguía abierto.
  expect_identical(unlist(e$reaperturas[[2]]$pasos_desellados), REABRIBLES[1:8])
  expect_true(any(grepl("Reaperturas: 2", cli(c("status", dir))$out, fixed = TRUE)))
})

test_that("cada registro y cada paso des-sellado cumplen el schema", {
  dir <- sellado_completo()
  cli(c("reopen", dir, "--desde", "renderizado_4_formatos", "--motivo", "x", "--reporte", "R.md"))
  e <- estado(dir)
  re <- e$reaperturas[[1]]
  sch <- jsonlite::fromJSON(SCHEMA, simplifyVector = FALSE)
  rs <- sch$properties$reaperturas
  expect_identical(rs$type, "array")
  expect_false(rs$items$additionalProperties)
  expect_true(all(names(re) %in% names(rs$items$properties)))
  expect_true(all(unlist(rs$items$required) %in% names(re)))
  expect_identical(sort(unlist(rs$items$properties$desde$enum)), sort(REABRIBLES))
  expect_identical(sort(unlist(rs$items$properties$pasos_desellados$items$enum)), sort(REABRIBLES))
  props <- sch$properties$pasos$properties
  for (p in unlist(re$pasos_desellados)) {
    expect_true(all(unlist(props[[p]]$required) %in% names(e$pasos[[p]])), info = p)
    expect_true(all(names(e$pasos[[p]]) %in% names(props[[p]]$properties)), info = p)
  }
})
