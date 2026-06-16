#!/usr/bin/env Rscript
# Ejecuta UNA suite de tests en un proceso R aislado y devuelve el veredicto vía
# exit status (0 = todo verde; 1 = hay failed/error o el script reventó).
#
# Usado por run_all_tests.R para AISLAR el estado global entre suites: una sola
# sesión R compartida deja filtrar estado de Python/reticulate (un único
# intérprete por proceso), RNG, options, variables globales, etc. — lo que causaba
# que test_cloze_n3 (reticulate) fallara solo al correr DESPUÉS de otra suite que
# usa Python, pese a pasar en aislamiento. El ejercicio fuente está en
# 03-En-Produccion (inmutable), así que la isolación se garantiza aquí.
#
# Uso: Rscript tests/run_one_suite.R <ruta_al_test_file.R>

suppressMessages({
  library(testthat)
  library(exams)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || !nzchar(args[1])) {
  message("run_one_suite.R: falta la ruta de la suite")
  quit(status = 2)
}
archivo <- args[1]

# Correr desde la raíz del repo (los tests resuelven rutas relativas a ella)
repo_root <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                      error = function(e) getwd())
if (length(repo_root) == 1 && nzchar(repo_root)) setwd(repo_root)
Sys.setenv(TESTING = "TRUE")

res <- tryCatch(
  as.data.frame(test_file(archivo, reporter = "summary", stop_on_failure = FALSE)),
  error = function(e) { message("SCRIPT ERROR: ", conditionMessage(e)); NULL }
)
if (is.null(res)) quit(status = 1)

n_failed <- if ("failed" %in% names(res)) sum(res$failed, na.rm = TRUE) else 0L
n_error  <- if ("error"  %in% names(res)) sum(as.logical(res$error), na.rm = TRUE) else 0L

if (n_failed > 0 || n_error > 0) quit(status = 1) else quit(status = 0)
