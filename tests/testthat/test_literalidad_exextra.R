# =============================================================================
# Test de regresión — Literalidad de los exextra contra el catálogo canónico (Mat)
# =============================================================================
# Origen: tanda 9b del CLOZE de totales-promedios-reciclaje-n3 (2026-11-02). Ningún
# validador certificaba que los valores oficiales de exextra fueran literales del
# catálogo ICFES (regla #7); el paso 10 lo hacía a mano. Ahora lo hace
# .claude/scripts/validar_literalidad_exextra.py (solo Matemáticas).
# Los fixtures llevan un minicatálogo con entradas copiadas literales del real, para
# que el test corra en CI sin Todo-Pajaro.

library(testthat)

repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE)
  if (length(r) == 1 && dir.exists(r)) r else getwd()
}, error = function(e) getwd())

SCRIPT <- file.path(repo_root, ".claude", "scripts", "validar_literalidad_exextra.py")
FX     <- file.path(repo_root, "tests", "testthat", "fixtures", "literalidad_exextra")
CAT    <- file.path(FX, "catalogos")
REAL   <- "/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Matematicas/catalogos-oficiales-mat"
PY     <- Sys.which("python3")

correr <- function(args) {
  out <- suppressWarnings(system2(PY, c("-I", shQuote(SCRIPT), args), stdout = TRUE, stderr = TRUE,
                                  env = "CATALOGOS_MAT="))
  list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"),
       texto = paste(out, collapse = "\n"))
}
fx <- function(n) shQuote(file.path(FX, n))

context("validar_literalidad_exextra.py — fixtures")

test_that("el script existe y python3 está disponible", {
  expect_true(file.exists(SCRIPT))
  expect_true(nzchar(PY))
})

test_that("valores literales con minicatálogo: exit 0", {
  r <- correr(c(fx("ok.Rmd"), "--catalogos", shQuote(CAT)))
  expect_equal(r$status, 0L)
  expect_match(r$texto, "md5 del .Rmd:")
  expect_match(r$texto, "\\[OK\\] Afirmacion")
})

test_that("con --item igual a la ficha: exit 0", {
  r <- correr(c(fx("ok.Rmd"), "--catalogos", shQuote(CAT), "--item", fx("item_ok.md")))
  expect_equal(r$status, 0L)
})

# Sin el separador se compara el valor entero (2026-11-02). El barrido de los .Rmd en desarrollo
# marcaba FALLA un Estándar literal del catálogo solo porque no llevaba « (código)».
test_that("Estándar literal sin «(código)»: exit 0; Descriptor reducido a su código: exit 1", {
  base <- readLines(file.path(FX, "ok.Rmd"), warn = FALSE, encoding = "UTF-8")
  i_est <- grep("^exextra\\[Estandar\\]:", base)
  i_des <- grep("^exextra\\[Descriptor\\]:", base)
  expect_length(i_est, 1)
  expect_length(i_des, 1)
  sin_codigo <- base
  sin_codigo[i_est] <- sub(" \\([^()]*\\)$", "", base[i_est])
  expect_false(grepl(" (", sin_codigo[i_est], fixed = TRUE), info = "el caso no quitó el código del Estándar")
  f1 <- tempfile(fileext = ".Rmd"); writeLines(sin_codigo, f1, useBytes = TRUE)
  r1 <- correr(c(shQuote(f1), "--catalogos", shQuote(CAT)))
  expect_equal(r1$status, 0L, info = r1$texto)
  expect_match(r1$texto, "\\[OK\\] Estandar")
  solo_codigo <- base
  solo_codigo[i_des] <- "exextra[Descriptor]: D3.10"
  f2 <- tempfile(fileext = ".Rmd"); writeLines(solo_codigo, f2, useBytes = TRUE)
  r2 <- correr(c(shQuote(f2), "--catalogos", shQuote(CAT)))
  expect_equal(r2$status, 1L, info = r2$texto)
  expect_match(r2$texto, "SIN coincidencia literal")
})

test_that("mutante: Afirmación sin una tilde: exit 1 con su motivo", {
  r <- correr(c(fx("mutante_afirmacion_sin_tilde.Rmd"), "--catalogos", shQuote(CAT)))
  expect_equal(r$status, 1L)
  expect_match(r$texto, "\\[FALLA\\] Afirmacion")
  # Regla #7 íntegra § 2b: solo tildes = FORMATO, con el literal que hay que copiar (2026-11-02).
  expect_match(r$texto, "FORMATO \\(solo tildes o mayúsculas\\)")
  expect_false(grepl("SIN coincidencia literal", r$texto, fixed = TRUE))
})

test_that("mutante: Afirmación con una palabra cambiada: exit 1", {
  r <- correr(c(fx("mutante_afirmacion_parafraseada.Rmd"), "--catalogos", shQuote(CAT)))
  expect_equal(r$status, 1L)
  expect_match(r$texto, "\\[FALLA\\] Afirmacion")
  # Una palabra cambiada es contenido distinto, no FORMATO.
  expect_match(r$texto, "SIN coincidencia literal")
  expect_false(grepl("FORMATO", r$texto, fixed = TRUE))
})

test_that("valor literal en catálogo pero distinto de la ficha: exit 1", {
  r <- correr(c(fx("ok.Rmd"), "--catalogos", shQuote(CAT), "--item", fx("item_distinto.md")))
  expect_equal(r$status, 1L)
  expect_match(r$texto, "distinto de la ficha")
})

test_that("obligatorio ausente: exit 1 con NO DECLARADO", {
  r <- correr(c(fx("sin_evidencia.Rmd"), "--catalogos", shQuote(CAT)))
  expect_equal(r$status, 1L)
  expect_match(r$texto, "\\[NO DECLARADO\\] Evidencia")
})

test_that("directorio de catálogos inexistente: exit 2 NO VERIFICABLE, nunca PASS", {
  r <- correr(c(fx("ok.Rmd"), "--catalogos", shQuote(file.path(FX, "no_existe"))))
  expect_equal(r$status, 2L)
  expect_match(r$texto, "NO VERIFICABLE")
  expect_false(grepl("\\[OK\\]", r$texto))
})

test_that("falta un JSON del catálogo: exit 2", {
  tmp <- tempfile("cat"); dir.create(tmp)
  file.copy(list.files(CAT, full.names = TRUE)[1:3], tmp)
  r <- correr(c(fx("ok.Rmd"), "--catalogos", shQuote(tmp)))
  expect_equal(r$status, 2L)
  expect_match(r$texto, "NO VERIFICABLE")
})

test_that(".Rmd sin Meta-information: exit 2", {
  r <- correr(c(fx("sin_meta.Rmd"), "--catalogos", shQuote(CAT)))
  expect_equal(r$status, 2L)
  expect_match(r$texto, "NO VERIFICABLE")
})

context("validar_literalidad_exextra.py — caso real")

test_that("CLOZE y SCHOICE de totales-promedios-reciclaje-n3 contra el catálogo real", {
  skip_if_not(dir.exists(REAL))
  base <- file.path(repo_root, "A-Produccion", "01-En-PreDesarrollo", "totales-promedios-reciclaje-n3")
  item <- file.path(base, "ITEM_OFICIAL.md")
  skip_if_not(file.exists(item))
  cloze <- file.path(base, "cloze", "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
  sch   <- file.path(base, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd")
  skip_if_not(file.exists(cloze) && file.exists(sch))
  expect_equal(correr(c(shQuote(cloze), "--catalogos", shQuote(REAL), "--item", shQuote(item)))$status, 0L)
  expect_equal(correr(c(shQuote(sch), "--catalogos", shQuote(REAL), "--item", shQuote(item)))$status, 0L)
})
