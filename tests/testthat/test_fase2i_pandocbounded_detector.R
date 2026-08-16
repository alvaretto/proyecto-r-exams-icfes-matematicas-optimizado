# =============================================================================
# FASE 2I / Error 16 - el detector de \pandocbounded debe distinguir
# DEFINICION de USO.
# .claude/hooks/post-exams2-validation.sh (bloque 2I.2)
# .claude/rules/markdown-imagenes-pdf.md (regla #18)
#
# Por que existe: hasta el 2026-08-15 el bloque 2I.2 usaba
#     grep -l 'pandocbounded' "$tex"
# que NO distingue la definicion del uso. Desde R/exams 2.4-1 TODAS las
# plantillas LaTeX del paquete traen la definicion no-op
#     \providecommand{\pandocbounded}[1]{#1}
# en exams/tex/{plain,plain8,exam,form,solution,plain-highlight}.tex, asi que
# ese grep casaba en CUALQUIER .tex, tuviera o no imagenes, y la FASE 2I
# marcaba ERROR 16 bloqueante en todo render del repositorio.
#
# Medido el 2026-08-15 sobre pendiente-rectas-paralelas-n4 (ejercicio SIN una
# sola imagen): 6 de sus 8 .tex disparaban el detector viejo; usos reales = 0.
# Y sobre un fixture con un chunk que dibuja: 2 disparos, ambos inocuos.
# Un gate que siempre falla se aprende a ignorar -- misma patologia que la
# FASE 2G en falso rojo permanente.
#
# El detector es una FUNCION DE LA RUTA y las dos firmas se EXTRAEN del hook
# real, no se copian aqui: una copia paralela podria divergir del hook sin que
# nada lo notara. Los fixtures viven en tempdir(); mutar archivos reales ya
# dejo una vez el arsenal roto en disco (validar_multisemilla.R, 2026-08-09).
# =============================================================================
library(testthat)

repo_root <- local({
  r <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(r) && length(r) == 1 && dir.exists(r)) r else normalizePath("../..")
})

HOOK <- file.path(repo_root, ".claude", "hooks", "post-exams2-validation.sh")

# --- Extraccion de las firmas REALES del hook -------------------------------
extraer_firma <- function(hook_path, nombre) {
  stopifnot(file.exists(hook_path))
  ln <- grep(paste0("^", nombre, "="), readLines(hook_path, warn = FALSE), value = TRUE)
  if (length(ln) == 0) return(NA_character_)
  sub("'\\s*$", "", sub(paste0("^", nombre, "='"), "", ln[1]))
}

# Aplica las firmas del hook a un directorio de .tex y devuelve el veredicto
# con la MISMA logica que 2I.2: uso sin definicion en ninguna parte del render.
veredicto_2i2 <- function(dir_tex, uso_re, def_re) {
  texs <- list.files(dir_tex, pattern = "\\.tex$", full.names = TRUE, recursive = TRUE)
  hay_uso <- FALSE; hay_def <- FALSE
  for (tx in texs) {
    l <- readLines(tx, warn = FALSE)
    if (any(grepl(uso_re, l))) hay_uso <- TRUE
    if (any(grepl(def_re, l))) hay_def <- TRUE
  }
  if (hay_uso && !hay_def) "ERROR_16" else if (hay_uso) "USO_DEFINIDO_OK" else "SIN_USO"
}

# Detector VIEJO, reproducido solo para demostrar que era el que fallaba.
veredicto_viejo <- function(dir_tex) {
  texs <- list.files(dir_tex, pattern = "\\.tex$", full.names = TRUE, recursive = TRUE)
  sum(vapply(texs, function(tx)
    any(grepl("pandocbounded", readLines(tx, warn = FALSE), fixed = TRUE)), logical(1)))
}

# --- Fixtures en tempdir() ---------------------------------------------------
DEF_LINE <- "\\providecommand{\\pandocbounded}[1]{#1}"
USO_LINE <- "\\pandocbounded{\\includegraphics[keepaspectratio]{fig.png}}"

crear_caso <- function(nombre, preambulo, cuerpo) {
  d <- file.path(tempdir(), "fase2i", nombre)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(preambulo, file.path(d, "plain1.tex"))
  writeLines(cuerpo,    file.path(d, "exercise1.tex"))
  d
}

# Replica de la disposicion real de exams: preambulo en plain*.tex, uso en
# exercise*.tex. Por eso la definicion se busca en TODO el conjunto.
CASO_POS <- crear_caso("pos_uso_sin_def",
                       c("\\documentclass{article}", "\\usepackage{graphicx}"),
                       c("\\begin{question}", USO_LINE, "\\end{question}"))
CASO_REAL <- crear_caso("real_uso_con_def",
                        c("\\documentclass{article}", DEF_LINE),
                        c("\\begin{question}", USO_LINE, "\\end{question}"))
CASO_SOLO_DEF <- crear_caso("solo_definicion",
                            c("\\documentclass{article}", DEF_LINE),
                            c("\\begin{question}", "Cuanto es 2 + 3?", "\\end{question}"))
CASO_LIMPIO <- crear_caso("sin_nada",
                          c("\\documentclass{article}"),
                          c("\\begin{question}",
                            "\\includegraphics[width=0.6\\linewidth]{fig.png}",
                            "\\end{question}"))

# =============================================================================
test_that("El hook declara las dos firmas y ya no usa el grep ingenuo", {
  expect_true(file.exists(HOOK))
  txt <- readLines(HOOK, warn = FALSE)

  expect_false(is.na(extraer_firma(HOOK, "PB_USO_RE")),
               info = "El hook debe declarar PB_USO_RE para que el test lo extraiga")
  expect_false(is.na(extraer_firma(HOOK, "PB_DEF_RE")),
               info = "El hook debe declarar PB_DEF_RE para que el test lo extraiga")

  # El grep ingenuo casa la DEFINICION -> falso rojo permanente. No debe volver.
  ingenuo <- grep("grep\\s+-l\\s+'pandocbounded'", txt, value = TRUE)
  expect_equal(length(ingenuo), 0L,
               info = paste("Reaparecio el detector ingenuo:", paste(ingenuo, collapse = " | ")))
})

test_that("CONTROL POSITIVO: uso SIN definicion dispara ERROR 16", {
  uso <- extraer_firma(HOOK, "PB_USO_RE"); def <- extraer_firma(HOOK, "PB_DEF_RE")
  expect_equal(veredicto_2i2(CASO_POS, uso, def), "ERROR_16",
               info = "Un \\pandocbounded{...} sin \\providecommand debe bloquear")
})

test_that("CONTROL NEGATIVO: solo la definicion NO dispara", {
  uso <- extraer_firma(HOOK, "PB_USO_RE"); def <- extraer_firma(HOOK, "PB_DEF_RE")
  expect_equal(veredicto_2i2(CASO_SOLO_DEF, uso, def), "SIN_USO",
               info = "La linea del template de R/exams no es un defecto")
  expect_equal(veredicto_2i2(CASO_LIMPIO, uso, def), "SIN_USO")
})

test_that("CASO REAL de R/exams >= 2.4-1: uso + definicion NO bloquea", {
  uso <- extraer_firma(HOOK, "PB_USO_RE"); def <- extraer_firma(HOOK, "PB_DEF_RE")
  # Todo ejercicio con un chunk que dibuja produce este caso: pandoc envuelve
  # el \includegraphics y el template de exams define el macro como no-op.
  expect_equal(veredicto_2i2(CASO_REAL, uso, def), "USO_DEFINIDO_OK")
})

test_that("El detector VIEJO fallaba en los tres casos inocuos (regresion)", {
  # Deja constancia medible de por que se cambio: 3 de 4 casos eran falsos rojos.
  expect_gt(veredicto_viejo(CASO_SOLO_DEF), 0)
  expect_gt(veredicto_viejo(CASO_REAL), 0)
  expect_equal(veredicto_viejo(CASO_LIMPIO), 0)
})

test_that("La firma de USO no casa la linea de definicion", {
  uso <- extraer_firma(HOOK, "PB_USO_RE")
  # \providecommand{\pandocbounded}[1]{#1} lleva '}' tras el nombre, no '{'.
  expect_false(grepl(uso, DEF_LINE),
               info = "PB_USO_RE no debe casar la definicion")
  expect_true(grepl(uso, USO_LINE),
              info = "PB_USO_RE debe casar el uso real")
})

test_that("La firma de DEFINICION cubre las cuatro formas de declararla", {
  def <- extraer_firma(HOOK, "PB_DEF_RE")
  for (v in c("\\providecommand{\\pandocbounded}[1]{#1}",
              "\\newcommand{\\pandocbounded}[1]{#1}",
              "\\renewcommand{\\pandocbounded}[1]{#1}",
              "\\def\\pandocbounded#1{#1}")) {
    expect_true(grepl(def, v), info = paste("No reconoce la definicion:", v))
  }
  expect_false(grepl(def, USO_LINE),
               info = "PB_DEF_RE no debe casar un uso")
})
