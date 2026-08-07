# Tests de `.claude/scripts/validar_diagnosticidad.R` (sonda V9).
#
# Lo que estos tests protegen es el MARGEN RELATIVO de la sonda H1, añadido el
# 2026-08-06. La v1 de H1 solo miraba el orden ("¿es la correcta la única más
# larga?") y no la distancia: un gap cuyas opciones ya se habían igualado
# deliberadamente —8 caracteres medianos sobre 115, el 7%— seguía reportando
# 100% y quedaba bloqueado por una diferencia que ningún estudiante puede usar
# como heurística. Sin estos tests, el criterio puede revertirse sin que nada
# avise, en cualquiera de las dos direcciones:
#   - demasiado laxo  -> vuelve a colarse el ítem resoluble "por la más larga";
#   - demasiado estricto -> vuelve a bloquearse un ítem correcto.

library(testthat)

RAIZ <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
SCRIPT_DIAG <- file.path(RAIZ, ".claude/scripts/validar_diagnosticidad.R")

# Construye un .Rmd mínimo con un gap schoice de 4 opciones cuya opción correcta
# lleva `extra_chars` caracteres de más respecto de las otras tres (que miden
# `base_chars` y son de longitud pareja entre sí).
fixture_rmd <- function(base_chars, extra_chars) {
  relleno <- function(n) paste(rep("a", n), collapse = "")
  f <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo = FALSE, results = \"hide\"}",
    "v <- sample(100:999, 1)",
    sprintf('opciones_p1 <- c(paste0("correcta ", v, " %s"),', relleno(base_chars + extra_chars)),
    sprintf('                 paste0("erronea1 ", v, " %s"),', relleno(base_chars)),
    sprintf('                 paste0("erronea2 ", v, " %s"),', relleno(base_chars)),
    sprintf('                 paste0("erronea3 ", v, " %s"))', relleno(base_chars)),
    "sol_p1 <- c(TRUE, FALSE, FALSE, FALSE)",
    "```",
    "",
    "Question",
    "========",
    "**Parte 1.** Pregunta.",
    "##ANSWER1##"
  ), f)
  f
}

correr <- function(rmd, n = 20) {
  salida <- suppressWarnings(system2("Rscript", c(SCRIPT_DIAG, rmd, "--n", n),
                                     stdout = TRUE, stderr = TRUE))
  list(texto = paste(salida, collapse = "\n"),
       exit = attr(salida, "status") %||% 0L)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("El script existe y es ejecutable", {
  expect_true(file.exists(SCRIPT_DIAG))
})

test_that("H1 caza la correcta MUCHO más larga (margen muy por encima del 15%)", {
  # 100 de base + 40 extra = 40% de margen: la heurística "la más larga" resuelve
  # el ítem sin leerlo.
  r <- correr(fixture_rmd(base_chars = 100, extra_chars = 40))
  expect_match(r$texto, "ERR_DIAG_SUPERFICIAL")
  expect_equal(r$exit, 1L)
})

test_that("H1 NO caza opciones ya igualadas (margen muy por debajo del 15%)", {
  # 100 de base + 5 extra = 5% de margen. La correcta SIGUE siendo la única más
  # larga (el orden no cambia), pero por una diferencia inservible.
  r <- correr(fixture_rmd(base_chars = 100, extra_chars = 5))
  expect_false(grepl("ERR_DIAG_SUPERFICIAL", r$texto))
  expect_equal(r$exit, 0L)
})

test_that("El PASS por margen deja constancia explícita en la NOTA DE ORDEN", {
  # Que no dispare no puede leerse como "no hay señal": el script debe decir que
  # la correcta ocupa el extremo en el 100% de las versiones y por cuánto.
  r <- correr(fixture_rmd(base_chars = 100, extra_chars = 5))
  expect_match(r$texto, "NOTA DE ORDEN")
  expect_match(r$texto, "unica mas larga en el 100%")
})

test_that("El umbral del margen es configurable con --margen", {
  rmd <- fixture_rmd(base_chars = 100, extra_chars = 5)
  salida <- suppressWarnings(system2("Rscript", c(SCRIPT_DIAG, rmd, "--n", 20, "--margen", 2),
                                     stdout = TRUE, stderr = TRUE))
  # Con el umbral bajado al 2%, el mismo archivo que pasa al 15% ahora dispara.
  expect_match(paste(salida, collapse = "\n"), "ERR_DIAG_SUPERFICIAL")
})

test_that("Sin gaps de selección única el veredicto es indeterminado, no un falso PASS", {
  f <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo = FALSE, results = \"hide\"}",
    "x <- sample(1:100, 1)",
    "```",
    "",
    "Question",
    "========",
    "¿Cuánto vale x?"
  ), f)
  r <- correr(f, n = 5)
  expect_match(r$texto, "WARN_DIAG_INDET")
  expect_equal(r$exit, 0L)
  unlink(f)
})
