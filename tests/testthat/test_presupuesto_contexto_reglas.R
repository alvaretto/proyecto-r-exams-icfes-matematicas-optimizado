# =============================================================================
# test_presupuesto_contexto_reglas.R — las reglas de .claude/rules/ caben en contexto
# =============================================================================
#
# ORIGEN (2026-09-29): Claude Code avisó al arrancar
#
#   "28 instruction files add up to 394.1k chars, over the 150.0k-char total
#    limit · largest: diversidad-sustantiva.md (62.0k), detractor-obligatorio.md
#    (34.0k), ejercicios-metacognitivos.md (26.2k)"
#
# Claude Code carga TODO `.claude/rules/*.md` en cada sesión y en cada
# subagente. Las 24 reglas sumaban 349 KB porque cada ciclo de corrección les
# añadía origen, mediciones e historial de versiones — contenido valioso, pero
# que no hace falta en contexto permanente.
#
# Arreglo (política de contexto v3.28.0 de .claude/CLAUDE.md):
#   .claude/rules/<regla>.md        versión COMPACTA, normativa, cargada siempre
#   .claude/docs/reglas/<regla>.md  texto ÍNTEGRO, leído bajo demanda
#
# QUÉ FIJA ESTE TEST
#   1. Presupuesto: las reglas compactas suman <= 60 000 caracteres y ninguna
#      pasa de 5 000. Con CLAUDE.md raíz + .claude/CLAUDE.md + CLAUDE.local.md,
#      lo cargado desde el repo no pasa de 110 000 — deja ~40 000 para el
#      ~/.claude/CLAUDE.md global, bajo el límite de 150 000 de Claude Code.
#   2. Pareo: cada regla compacta tiene su texto íntegro, y lo cita.
#   3. Control positivo: el detector caza una regla inflada y una sin gemelo.
#
# Si una regla necesita crecer, lo que crece es su texto íntegro en docs/reglas;
# la compacta sólo recoge la norma. Subir estos topes para que el test pase es
# reintroducir el problema.
# =============================================================================

library(testthat)

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE)[1],
  error = function(e) getwd())
if (is.na(repo_root) || !dir.exists(repo_root)) repo_root <- getwd()

TOPE_TOTAL_REGLAS <- 60000L
TOPE_POR_REGLA    <- 5000L
TOPE_REPO         <- 110000L

n_chars <- function(f) {
  sum(nchar(readLines(f, warn = FALSE, encoding = "UTF-8"), type = "chars")) +
    length(readLines(f, warn = FALSE))
}

# Devuelve los problemas encontrados en un par de directorios (compactas, íntegras).
auditar_reglas <- function(dir_rules, dir_docs) {
  reglas <- list.files(dir_rules, pattern = "\\.md$", full.names = TRUE,
                       recursive = TRUE)
  tam <- vapply(reglas, n_chars, numeric(1))
  problemas <- character(0)
  if (sum(tam) > TOPE_TOTAL_REGLAS)
    problemas <- c(problemas, sprintf("total %d > %d", as.integer(sum(tam)),
                                      TOPE_TOTAL_REGLAS))
  gordas <- tam[tam > TOPE_POR_REGLA]
  for (i in seq_along(gordas))
    problemas <- c(problemas, sprintf("%s: %d > %d", basename(names(gordas)[i]),
                                      as.integer(gordas[i]), TOPE_POR_REGLA))
  for (r in reglas) {
    b <- basename(r)
    if (!file.exists(file.path(dir_docs, b)))
      problemas <- c(problemas, paste0(b, ": sin texto íntegro en docs/reglas"))
    else if (!any(grepl(paste0("docs/reglas/", b), readLines(r, warn = FALSE),
                        fixed = TRUE)))
      problemas <- c(problemas, paste0(b, ": no cita su texto íntegro"))
  }
  problemas
}

DIR_RULES <- file.path(repo_root, ".claude", "rules")
DIR_DOCS  <- file.path(repo_root, ".claude", "docs", "reglas")

test_that("las reglas compactas caben en el presupuesto y tienen su texto íntegro", {
  skip_if_not(dir.exists(DIR_RULES))
  problemas <- auditar_reglas(DIR_RULES, DIR_DOCS)
  expect_equal(length(problemas), 0,
    info = paste0("Presupuesto de contexto de .claude/rules/ roto:\n  - ",
                  paste(problemas, collapse = "\n  - ")))
})

test_that("lo cargado desde el repo deja margen para el CLAUDE.md global", {
  skip_if_not(dir.exists(DIR_RULES))
  fijos <- file.path(repo_root, c("CLAUDE.md", ".claude/CLAUDE.md", "CLAUDE.local.md"))
  fijos <- fijos[file.exists(fijos)]
  reglas <- list.files(DIR_RULES, pattern = "\\.md$", full.names = TRUE,
                       recursive = TRUE)
  total <- sum(vapply(c(fijos, reglas), n_chars, numeric(1)))
  expect_lte(total, TOPE_REPO)
})

test_that("CONTROL POSITIVO: el detector caza una regla inflada y una sin gemelo", {
  d <- file.path(tempdir(), paste0("presupuesto_", as.integer(Sys.time())))
  dr <- file.path(d, "rules"); dd <- file.path(d, "docs")
  dir.create(dr, recursive = TRUE); dir.create(dd, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  writeLines(c("# sana", "ver docs/reglas/sana.md"), file.path(dr, "sana.md"))
  writeLines("# sana íntegra", file.path(dd, "sana.md"))
  expect_equal(length(auditar_reglas(dr, dd)), 0)

  writeLines(c("# inflada", "ver docs/reglas/inflada.md",
               strrep("x", TOPE_POR_REGLA + 10)), file.path(dr, "inflada.md"))
  writeLines("# íntegra", file.path(dd, "inflada.md"))
  writeLines("# huérfana sin cita", file.path(dr, "huerfana.md"))

  p <- auditar_reglas(dr, dd)
  expect_true(any(grepl("^inflada.md: ", p)))
  expect_true(any(grepl("^huerfana.md: sin texto íntegro", p)))
})
