#!/usr/bin/env Rscript
# =============================================================================
# test_diversidad_sustantiva.R
# Suite #20 — Regla #22: Diversidad Sustantiva
# =============================================================================
# Verifica que el script validar_diversidad_sustantiva.R detecte correctamente:
#   - ERR_DIV_COSMETICA (exit 1): respuesta correcta FIJA (diversidad cosmética)
#   - PASS (exit 0):              respuesta correcta varía suficientemente
#
# Los fixtures se escriben en tempdir() por lo que no contaminan el repo.
# =============================================================================

library(testthat)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE),
  error = function(e) getwd()
)
script_path <- file.path(repo_root, ".claude", "scripts", "validar_diversidad_sustantiva.R")
rscript_bin  <- file.path(R.home("bin"), "Rscript")

# Escribe un .Rmd fixture mínimo y retorna su ruta
escribir_fixture <- function(data_generation_code) {
  tmp <- tempfile(pattern = "fixture_divsust_", fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo=FALSE, results='hide'}",
    data_generation_code,
    "```",
    "",
    "Question",
    "========",
    "Pregunta de prueba.",
    "",
    "Answerlist",
    "----------",
    "* Opción A",
    "* Opción B",
    "* Opción C",
    "* Opción D",
    "",
    "Solution",
    "========",
    "La respuesta correcta varía.",
    "",
    "Meta-information",
    "================",
    "exname: fixture_test",
    "extype: schoice",
    "exsolution: 1000",
    "exshuffle: TRUE"
  ), con = tmp)
  tmp
}

# Ejecuta el script y retorna lista(exit, stdout)
ejecutar_script <- function(rmd_path, n = 10L) {
  out <- tryCatch(
    system2(rscript_bin,
            args   = c(script_path, rmd_path, "--n", as.character(n)),
            stdout = TRUE,
            stderr = TRUE),
    error = function(e) character(0)
  )
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(exit = as.integer(status), stdout = paste(out, collapse = "\n"))
}

# ---------------------------------------------------------------------------
# Fixture A — respuesta correcta FIJA (hardcoded)
# Simula el incidente desplazamiento-avion-aeropuerto:
#   los parámetros de la respuesta son siempre las mismas constantes.
# ---------------------------------------------------------------------------
FIXTURE_FIJO <- '
distancia_total    <- 100   # HARDCODED — siempre 100
angulo             <- 50    # HARDCODED — siempre 50
distancia_avanzada <- 30    # HARDCODED — siempre 30

valor_correcto <- distancia_total - distancia_avanzada  # siempre 70

opciones <- list(
  A = list(tipo = "correcta", valor = valor_correcto),
  B = list(tipo = "error",    valor = valor_correcto + 10),
  C = list(tipo = "error",    valor = valor_correcto + 20),
  D = list(tipo = "error",    valor = valor_correcto - 10)
)
sol <- c(1, 0, 0, 0)
'

# ---------------------------------------------------------------------------
# Fixture B — respuesta correcta ALEATORIA
# Los parámetros se aleatorízan con sample(), por lo que la respuesta varía.
# ---------------------------------------------------------------------------
FIXTURE_ALEATORIO <- '
distancia_total    <- sample(60:150, 1)
angulo             <- sample(25:70, 1)
distancia_avanzada <- sample(10:(distancia_total - 10), 1)

valor_correcto <- distancia_total - distancia_avanzada

opciones <- list(
  A = list(tipo = "correcta", valor = valor_correcto),
  B = list(tipo = "error",    valor = valor_correcto + sample(c(10, 15, 20), 1)),
  C = list(tipo = "error",    valor = valor_correcto + sample(c(25, 30, 35), 1)),
  D = list(tipo = "error",    valor = valor_correcto - sample(c(5, 8, 12), 1))
)
# Evitar colisiones (distractor = correcto)
if (any(sapply(opciones[-1], function(x) x$valor == valor_correcto))) {
  opciones$B$valor <- valor_correcto + 40L
}
sol <- c(1, 0, 0, 0)
'

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("el script validar_diversidad_sustantiva.R existe y es ejecutable", {
  expect_true(file.exists(script_path),
    info = paste("No se encontró:", script_path))
})

test_that("fixture FIJO produce ERR_DIV_COSMETICA (exit 1)", {
  skip_if(!file.exists(script_path), "script no disponible")
  rmd <- escribir_fixture(FIXTURE_FIJO)
  on.exit(unlink(rmd), add = TRUE)
  res <- ejecutar_script(rmd, n = 20L)
  expect_equal(res$exit, 1L,
    info = paste("Se esperaba exit 1 (ERR_DIV_COSMETICA). stdout:", res$stdout))
  expect_true(grepl("ERR_DIV_COSMETICA", res$stdout),
    info = paste("No aparece ERR_DIV_COSMETICA en stdout. stdout:", res$stdout))
})

test_that("fixture ALEATORIO produce PASS (exit 0)", {
  skip_if(!file.exists(script_path), "script no disponible")
  rmd <- escribir_fixture(FIXTURE_ALEATORIO)
  on.exit(unlink(rmd), add = TRUE)
  res <- ejecutar_script(rmd, n = 20L)
  expect_equal(res$exit, 0L,
    info = paste("Se esperaba exit 0 (PASS/WARN). stdout:", res$stdout))
  expect_true(grepl("PASS", res$stdout),
    info = paste("No aparece PASS en stdout. stdout:", res$stdout))
})

test_that("fixture FIJO no produce PASS en stdout", {
  skip_if(!file.exists(script_path), "script no disponible")
  rmd <- escribir_fixture(FIXTURE_FIJO)
  on.exit(unlink(rmd), add = TRUE)
  res <- ejecutar_script(rmd, n = 20L)
  expect_false(grepl("^PASS", res$stdout),
    info = paste("PASS no debería aparecer para fixture fijo. stdout:", res$stdout))
})

test_that("ERR_DIV_COSMETICA referencia la regla #22", {
  skip_if(!file.exists(script_path), "script no disponible")
  rmd <- escribir_fixture(FIXTURE_FIJO)
  on.exit(unlink(rmd), add = TRUE)
  res <- ejecutar_script(rmd, n = 20L)
  expect_true(grepl("diversidad-sustantiva\\.md", res$stdout),
    info = paste("La salida debería referenciar diversidad-sustantiva.md. stdout:", res$stdout))
})

test_that("fixture con .Rmd inexistente retorna exit 0 (WARN_DIV_INDET, no bloqueo)", {
  skip_if(!file.exists(script_path), "script no disponible")
  res <- ejecutar_script("/ruta/que/no/existe.Rmd", n = 10L)
  expect_equal(res$exit, 0L,
    info = paste("Un .Rmd inexistente debe retornar exit 0 (WARN_DIV_INDET). stdout:", res$stdout))
  expect_true(grepl("WARN_DIV_INDET", res$stdout),
    info = paste("Debe emitir WARN_DIV_INDET para .Rmd inexistente. stdout:", res$stdout))
})

test_that("la regla diversidad-sustantiva.md existe en .claude/rules/", {
  regla_path <- file.path(repo_root, ".claude", "rules", "diversidad-sustantiva.md")
  expect_true(file.exists(regla_path),
    info = paste("No se encontró la regla:", regla_path))
})

test_that("la regla diversidad-sustantiva.md menciona ERR_DIV_COSMETICA", {
  regla_path <- file.path(repo_root, ".claude", "rules", "diversidad-sustantiva.md")
  skip_if(!file.exists(regla_path), "regla no disponible")
  contenido <- paste(readLines(regla_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("ERR_DIV_COSMETICA", contenido),
    info = "La regla debe mencionar ERR_DIV_COSMETICA")
  expect_true(grepl("WARN_DIV_ESTATICA", contenido),
    info = "La regla debe mencionar WARN_DIV_ESTATICA")
})
