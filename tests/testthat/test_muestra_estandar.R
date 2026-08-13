# =============================================================================
# Regla #23 — Muestra Estandar de Validacion: N = 100
# .claude/rules/muestra-estandar-validacion.md
#
# Por que existe: hasta el 2026-08-13 el repositorio tenia CINCO tamanos de
# muestra rivales (5, 10, 20, 30, 40) repartidos por defaults de scripts,
# reglas, agentes, comandos y hooks, y ninguno era fuente unica. Cada agente
# elegia el suyo -- algunos 400, muy por encima de lo pedido. La instruccion
# verbal del profesor no se sostuvo porque no vivia en nada ejecutable.
#
# El detector es una FUNCION DE LA RUTA para poder ejercitarlo con mutantes en
# tempdir(); mutar los archivos reales ya dejo una vez el arsenal roto en disco
# (validar_multisemilla.R, 2026-08-09).
# =============================================================================
library(testthat)

repo_root <- local({
  r <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(r) && length(r) == 1 && dir.exists(r)) r else normalizePath("../..")
})

N_ESTANDAR <- 100L
# Familia A: miden sobre data_generation, NO renderizan -> N=100 obligatorio.
FAMILIA_A <- c("validar_diagnosticidad.R", "validar_diversidad_sustantiva.R",
               "validar_multisemilla.R")
# Familia B: renderizan de verdad (1 PDF / 1 captura por unidad). Su N se
# declara en el reporte; esta suite NO lo fija.
FAMILIA_B <- c("stress_test_visual.R", "auditor-visual-html")

# --- Detector 1: invocaciones --n con un valor != 100 junto a Familia A ------
detectar_invocaciones <- function(base_dir) {
  dirs <- file.path(base_dir, ".claude", c("agents", "commands", "rules", "hooks", "skills"))
  dirs <- dirs[dir.exists(dirs)]
  if (!length(dirs)) return(character(0))
  archivos <- list.files(dirs, pattern = "\\.(md|sh|json|R)$", recursive = TRUE,
                         full.names = TRUE)
  # La regla se documenta a si misma citando los valores viejos: se excluye.
  archivos <- archivos[basename(archivos) != "muestra-estandar-validacion.md"]
  malos <- character(0)
  for (a in archivos) {
    ln <- tryCatch(readLines(a, warn = FALSE), error = function(e) character(0))
    if (!length(ln)) next
    for (i in seq_along(ln)) {
      m <- regmatches(ln[i], regexpr("--n\\s+[0-9]+", ln[i]))
      if (!length(m)) next
      val <- as.integer(sub("--n\\s+", "", m))
      # Ventana +/-2 lineas: los agentes parten el comando en varias lineas.
      ven <- paste(ln[max(1, i - 2):min(length(ln), i + 2)], collapse = " ")
      es_a <- any(vapply(FAMILIA_A, function(v) grepl(v, ven, fixed = TRUE), logical(1)))
      es_b <- any(vapply(FAMILIA_B, function(v) grepl(v, ven, fixed = TRUE), logical(1)))
      if (es_a && !identical(val, N_ESTANDAR)) {
        malos <- c(malos, sprintf("%s:%d: --n %d (deberia ser %d)",
                                  sub(paste0(base_dir, "/"), "", a, fixed = TRUE),
                                  i, val, N_ESTANDAR))
      }
      if (es_b && !es_a) next  # Familia B: fuera del alcance de este detector
    }
  }
  malos
}

# --- Detector 2: defaults de los tres validadores ----------------------------
detectar_defaults <- function(base_dir) {
  chequeos <- list(
    list(f = file.path(base_dir, ".claude/scripts/validar_diagnosticidad.R"),
         re = "^n <- 100L"),
    list(f = file.path(base_dir, ".claude/scripts/validar_diversidad_sustantiva.R"),
         re = "^n <- 100L"),
    # OJO: el de .claude/scripts es un SYMLINK (invariante I-10); el real vive aqui.
    list(f = file.path(base_dir, "SOURCES/scripts_validacion/validar_multisemilla.R"),
         re = "n_semillas <- 100")
  )
  malos <- character(0)
  for (ch in chequeos) {
    if (!file.exists(ch$f)) { malos <- c(malos, paste("NO EXISTE:", ch$f)); next }
    ln <- readLines(ch$f, warn = FALSE)
    if (!any(grepl(ch$re, ln))) {
      malos <- c(malos, sprintf("%s: no se encontro el default '%s'",
                                basename(ch$f), ch$re))
    }
  }
  malos
}

# --- Detector 3: timeout del hook post-exams2 --------------------------------
timeout_hook <- function(base_dir) {
  sj <- file.path(base_dir, ".claude/settings.json")
  if (!file.exists(sj)) return(NA_integer_)
  txt <- paste(readLines(sj, warn = FALSE), collapse = "\n")
  m <- regmatches(txt, regexpr(
    "post-exams2-validation\\.sh[^}]*\"timeout\"\\s*:\\s*[0-9]+", txt))
  if (!length(m)) return(NA_integer_)
  as.integer(sub(".*\"timeout\"\\s*:\\s*", "", m))
}

test_that("Regla #23: toda invocacion de la Familia A usa --n 100", {
  malos <- detectar_invocaciones(repo_root)
  expect_equal(length(malos), 0,
    info = paste0("Invocaciones con N != 100:\n  ", paste(malos, collapse = "\n  ")))
})

test_that("Regla #23: los defaults de los tres validadores son 100", {
  malos <- detectar_defaults(repo_root)
  expect_equal(length(malos), 0,
    info = paste0("Defaults incorrectos:\n  ", paste(malos, collapse = "\n  ")))
})

test_that("Regla #23: el timeout del hook admite la muestra estandar", {
  tmo <- timeout_hook(repo_root)
  expect_false(is.na(tmo), info = "No se pudo leer el timeout del hook post-exams2")
  # 170 s medidos (54+55+61) + el resto de fases. Bajar de 300 s corta la FASE 2G.
  expect_gte(tmo, 300000)
})

test_that("CONTROL POSITIVO: el detector caza un --n fuera de estandar", {
  fake <- file.path(tempdir(), paste0("n23_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(file.path(fake, ".claude", "agents"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Paso 9:",
               "Rscript .claude/scripts/validar_diversidad_sustantiva.R x.Rmd --n 40"),
             file.path(fake, ".claude", "agents", "mutante.md"))
  malos <- detectar_invocaciones(fake)
  expect_gte(length(malos), 1)
  expect_true(any(grepl("--n 40", malos)), info = "El detector no nombro el valor infractor")

  # Control NEGATIVO 1: con 100 no debe disparar.
  writeLines(c("Paso 9:",
               "Rscript .claude/scripts/validar_diversidad_sustantiva.R x.Rmd --n 100"),
             file.path(fake, ".claude", "agents", "mutante.md"))
  expect_equal(length(detectar_invocaciones(fake)), 0)

  # Control NEGATIVO 2: Familia B con N distinto NO debe disparar (su N se
  # declara en el reporte, no lo fija esta suite).
  writeLines(c("Stress visual:",
               "Rscript SOURCES/scripts_validacion/stress_test_visual.R x.Rmd --n 10"),
             file.path(fake, ".claude", "agents", "mutante.md"))
  expect_equal(length(detectar_invocaciones(fake)), 0)

  unlink(fake, recursive = TRUE)
})

test_that("CONTROL POSITIVO: el detector caza un default degradado", {
  fake <- file.path(tempdir(), paste0("n23d_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(file.path(fake, ".claude", "scripts"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fake, "SOURCES", "scripts_validacion"), recursive = TRUE,
             showWarnings = FALSE)
  writeLines("n <- 40L", file.path(fake, ".claude/scripts/validar_diagnosticidad.R"))
  writeLines("n <- 100L", file.path(fake, ".claude/scripts/validar_diversidad_sustantiva.R"))
  writeLines("n_semillas <- 100",
             file.path(fake, "SOURCES/scripts_validacion/validar_multisemilla.R"))
  malos <- detectar_defaults(fake)
  expect_gte(length(malos), 1)
  expect_true(any(grepl("validar_diagnosticidad", malos)),
              info = "El detector no nombro el archivo con el default degradado")
  unlink(fake, recursive = TRUE)
})
