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
# validar_stresstest.R compila el ejercicio completo, pero sin LaTeX: una unidad cuesta
# segundos, no un PDF, y por eso va aquí y no en la familia B.
FAMILIA_A <- c("validar_diagnosticidad.R", "validar_diversidad_sustantiva.R",
               "validar_multisemilla.R", "validar_stresstest.R")
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
         re = "n_semillas <- 100"),
    list(f = file.path(base_dir, ".claude/scripts/validar_stresstest.R"),
         re = "argumento\\(args, \"--n\", 100L\\)")
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

# =============================================================================
# Umbral de producto (regla #3 de codigo-rmd.md), vigente desde 2026-11-13:
# al menos 100 versiones unicas sobre 150 renders. Sustituye a "250 de 300",
# "200 de 200" y "200 de 300", que convivian. Es un umbral distinto del N = 100
# de las mediciones estadisticas (arriba) y del maximo de 100 exportadas.
# =============================================================================
RE_CIFRA_RIVAL <- paste0(
  "250\\+|250 versiones|250 \u00fanicas|250 unicas|de 300 intentos|umbral 250|",
  "200\\+ versiones|>= ?250|< ?250|250 (de|sobre) 300|200 de (200|300)")
# Lineas que nombran la cifra vieja como historia, no como norma.
RE_HISTORIA <- "antes|sustituye|288/300|Hasta el"

# Archivos normativos: lista de (ruta relativa, filtro de lineas o NULL = todas).
archivos_normativos <- function(base_dir) {
  rel <- function(p) sub(paste0("^", base_dir, "/"), "", p)
  listar <- function(d, pat) {
    if (!dir.exists(file.path(base_dir, d))) return(character(0))
    rel(list.files(file.path(base_dir, d), pattern = pat, recursive = TRUE,
                   full.names = TRUE))
  }
  sk <- listar(".claude/skills", "\\.md$")
  sk <- sk[grepl("^\\.claude/skills/[^/]+/SKILL\\.md$", sk) |
           grepl("^\\.claude/skills/[^/]+/references/[^/]+\\.md$", sk)]
  fijos <- c(".claude/CLAUDE.md", ".claude/commands/validate.md")
  fijos <- fijos[file.exists(file.path(base_dir, fijos))]
  list(
    completos = c(listar(".claude/rules", "\\.md$")[!grepl("/", sub("^\\.claude/rules/", "",
                    listar(".claude/rules", "\\.md$")))], sk, fijos),
    orquestadores = c(".claude/agents/orquestador-schoice.md",
                      ".claude/agents/orquestador-cloze.md")
  )
}

detectar_cifras_rivales <- function(base_dir) {
  n <- archivos_normativos(base_dir)
  malos <- character(0)
  revisar <- function(ruta, solo_lineas = NULL) {
    f <- file.path(base_dir, ruta)
    if (!file.exists(f)) return(character(0))
    ln <- readLines(f, warn = FALSE, encoding = "UTF-8")
    idx <- which(grepl(RE_CIFRA_RIVAL, ln, perl = TRUE) &
                 !grepl(RE_HISTORIA, ln, perl = TRUE))
    if (!is.null(solo_lineas)) idx <- intersect(idx, solo_lineas(ln))
    if (!length(idx)) return(character(0))
    sprintf("%s:%d: %s", ruta, idx, trimws(ln[idx]))
  }
  for (r in n$completos) malos <- c(malos, revisar(r))
  for (r in n$orquestadores) {
    malos <- c(malos, revisar(r, function(ln) {
      which(grepl("^\\| 9 \\| validar_diversidad \\|", ln) | grepl("Diversidad: ?NNN/", ln))
    }))
  }
  malos
}

test_that("Umbral de producto: test-diversidad.R usa 100 de 150", {
  f <- file.path(repo_root, ".claude/skills/generar-schoice/scripts/test-diversidad.R")
  expect_true(file.exists(f))
  ln <- readLines(f, warn = FALSE)
  expect_true(any(grepl("^if \\(n_unicas >= 100\\) \\{", ln)),
              info = "El umbral de aprobacion no es n_unicas >= 100")
  expect_true(any(grepl("as\\.integer\\(args\\[2\\]\\) else 150", ln)),
              info = "El default de intentos no es 150")
  expect_false(any(grepl("250|else 300", ln)), info = "Quedan cifras viejas (250/300)")
})

test_that("Umbral de producto: la compacta de codigo-rmd.md declara 100 sobre 150", {
  f <- file.path(repo_root, ".claude/rules/codigo-rmd.md")
  expect_true(file.exists(f))
  txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("100 versiones \u00fanicas sobre 150", txt),
              info = "codigo-rmd.md no declara '100 versiones \u00fanicas sobre 150'")
})

test_that("Umbral de producto: ningun archivo normativo trae una cifra rival", {
  malos <- detectar_cifras_rivales(repo_root)
  expect_equal(length(malos), 0,
    info = paste0("Cifras rivales del umbral de producto:\n  ",
                  paste(malos, collapse = "\n  ")))
})

test_that("CONTROL POSITIVO: el detector de cifras rivales caza SKILL.md y respeta la historia", {
  fake <- file.path(tempdir(), paste0("umb_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(file.path(fake, ".claude", "skills", "x"), recursive = TRUE, showWarnings = FALSE)
  sk <- file.path(fake, ".claude", "skills", "x", "SKILL.md")
  writeLines(c("# x", "Exigir 250+ versiones \u00fanicas de 300 intentos.", "otra"), sk)
  malos <- detectar_cifras_rivales(fake)
  expect_equal(length(malos), 1)
  expect_true(grepl("^\\.claude/skills/x/SKILL\\.md:2:", malos),
              info = paste("No nombro archivo y linea exactos:", paste(malos, collapse = "|")))

  # Control negativo: la misma cifra como historia no se detecta.
  writeLines(c("# x", "antes 250 sobre 300 (250+ versiones)", "otra"), sk)
  expect_equal(length(detectar_cifras_rivales(fake)), 0)

  # Orquestador: solo cuentan la fila del paso 9 y la plantilla Diversidad.
  dir.create(file.path(fake, ".claude", "agents"), recursive = TRUE, showWarnings = FALSE)
  orq <- file.path(fake, ".claude", "agents", "orquestador-schoice.md")
  writeLines(c("texto libre con 250+ versiones",
               "| 9 | validar_diversidad | exige 250+ versiones |"), orq)
  malos <- detectar_cifras_rivales(fake)
  expect_equal(length(malos), 1)
  expect_true(grepl("orquestador-schoice\\.md:2:", malos))
  unlink(fake, recursive = TRUE)
})
