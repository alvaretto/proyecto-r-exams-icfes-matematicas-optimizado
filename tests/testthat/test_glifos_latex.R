# =============================================================================
# test_glifos_latex.R — glifos Unicode que ROMPEN pdflatex (FASE 2O)
# =============================================================================
#
# ORIGEN (2026-08-15): un ejercicio de `03-En-Produccion/` llevaba tiempo sin
# compilar en PDF y nada lo detectaba. Causa medida: el carácter `✓` (U+2713)
# LITERAL en encabezados Markdown de la sección Solution.
#
#   ! LaTeX Error: Unicode character ✓ (U+2713) not set up for use with LaTeX
#
# El defecto es INVISIBLE en HTML (no pasa por LaTeX) y ningún validador del
# arsenal miraba los caracteres del fuente.
#
# QUÉ FIJA ESTE TEST
#   1. Ningún `.Rmd` nuevo de 01/02/03 introduce glifos rompedores en Markdown.
#   2. El detector dispara con glifo visible (control positivo).
#   3. NO dispara con glifo sólo en comentario R (control negativo) — medido:
#      un `.Rmd` así COMPILA.
#   4. NO dispara con tildes españolas ni con los glifos medidos que sí
#      compilan (× ÷ ° ² → — •) — control negativo contra falsos positivos.
#      Esto importa: las tildes son OBLIGATORIAS por la regla #7, así que un
#      detector que las marcara sería peor que no tener detector.
#
# El allowlist `glifos-latex-legacy.txt` NO ADMITE ALTAS y sólo puede decrecer.
# =============================================================================

library(testthat)

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE)[1],
  error = function(e) normalizePath("../..", mustWork = FALSE))
if (is.na(repo_root) || !nzchar(repo_root)) repo_root <- normalizePath("../..", mustWork = FALSE)

DETECTOR <- file.path(repo_root, ".claude", "scripts", "validar_glifos_latex.R")
ALLOWLIST <- file.path(repo_root, "tests", "testthat", "glifos-latex-legacy.txt")

# Invoca el detector como proceso aparte y devuelve el exit REAL (sin tuberías,
# que enmascararían el código de salida — trampa documentada en el repo).
correr_detector <- function(ruta) {
  salida <- tempfile()
  st <- system2("Rscript", c("--vanilla", shQuote(DETECTOR), shQuote(ruta)),
                stdout = salida, stderr = salida)
  list(exit = st, texto = paste(readLines(salida, warn = FALSE), collapse = "\n"))
}

escribir_rmd <- function(ruta, cuerpo) {
  con <- file(ruta, open = "w", encoding = "UTF-8")
  writeLines(cuerpo, con); close(con)
  ruta
}

# El barrido usa el MISMO motor cargado en proceso: lanzar un Rscript por
# archivo son ~200 arranques de R y la suite pasa de segundos a minutos. La
# guarda `sys.nframe() == 0L` del detector impide que source() dispare su CLI.
# Los controles de arriba SÍ usan system2 a propósito: son los que verifican el
# contrato del CLI y el exit REAL, que es lo que consume el hook.
if (file.exists(DETECTOR)) source(DETECTOR, local = TRUE, encoding = "UTF-8")

bloquea <- function(ruta) length(analizar_glifos(ruta)$bloqueantes) > 0

META <- paste(
  "", "Meta-information", "================", "exname: fx",
  "extype: schoice", "exsolution: 10", "exshuffle: FALSE", sep = "\n")

# -----------------------------------------------------------------------------
test_that("el detector existe y es invocable", {
  expect_true(file.exists(DETECTOR), info = paste("falta el detector:", DETECTOR))
  expect_true(file.exists(ALLOWLIST), info = paste("falta el allowlist:", ALLOWLIST))
})

# -----------------------------------------------------------------------------
# CONTROL POSITIVO: glifo rompedor en texto Markdown visible -> DEBE disparar.
# Sin este control, un detector roto que nunca dispara pasaría por "todo limpio".
test_that("CONTROL POSITIVO: glifo en Markdown visible dispara ERR_GLIFO_LATEX", {
  d <- tempfile(); dir.create(d)
  f <- escribir_rmd(file.path(d, "visible.Rmd"), paste0(
    "Question\n========\n### Paso 1 verificado ✓\n\n",
    "Texto con π y ≤ visibles.\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))

  r <- correr_detector(f)
  expect_equal(r$exit, 1L,
    info = paste0("El detector NO disparó con U+2713/U+03C0/U+2264 en Markdown. ",
                  "Salida:\n", r$texto))
  expect_true(grepl("ERR_GLIFO_LATEX", r$texto, fixed = TRUE),
    info = "Falta el código de error ERR_GLIFO_LATEX en la salida")
  expect_true(grepl("U+2713", r$texto, fixed = TRUE),
    info = "El detector no nombra el codepoint que rompió")
})

# -----------------------------------------------------------------------------
# CONTROL NEGATIVO 1: glifo SOLO en comentario R -> NO debe disparar.
# Medido: un .Rmd así COMPILA. Bloquearlo sería un falso positivo.
test_that("CONTROL NEGATIVO: glifo sólo en comentario R no dispara", {
  d <- tempfile(); dir.create(d)
  f <- escribir_rmd(file.path(d, "comentario.Rmd"), paste0(
    "```{r datos, echo=FALSE}\n",
    "# ✓ verificado: π y ≤ solo en comentario\n",
    "v <- 1\n```\n\n",
    "Question\n========\nTexto limpio. Valor: `r v`.\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))

  r <- correr_detector(f)
  expect_equal(r$exit, 0L,
    info = paste0("Disparó por un glifo en COMENTARIO R, que está medido como ",
                  "inocuo (compila). Salida:\n", r$texto))
  expect_true(grepl("INFO_GLIFO_COMENTARIO", r$texto, fixed = TRUE),
    info = "Debería informar del comentario sin contarlo como error")
})

# -----------------------------------------------------------------------------
# CONTROL NEGATIVO 2: tildes españolas + glifos MEDIDOS que sí compilan.
# Un detector que marcara las tildes contradiría la regla #7, que las EXIGE.
test_that("CONTROL NEGATIVO: tildes y glifos que sí compilan no disparan", {
  d <- tempfile(); dir.create(d)
  f <- escribir_rmd(file.path(d, "limpio.Rmd"), paste0(
    "Question\n========\n",
    "Tildes obligatorias (regla #7): ángulo, función, gráfica, ",
    "ñame, pingüino, ¿cuál?, ¡ojo!\n\n",
    "Glifos medidos que SÍ compilan: × ÷ ° ² ³ º ",
    "§ « » · ± — – … ‰ • ",
    "← ↑ → £ €\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))

  r <- correr_detector(f)
  expect_equal(r$exit, 0L,
    info = paste0("FALSO POSITIVO: marcó tildes españolas o glifos medidos como ",
                  "compilables (× ÷ ° ² → — • ...). Salida:\n", r$texto))
  expect_true(grepl("OK:", r$texto, fixed = TRUE),
    info = paste0("Debería reportar OK. Salida:\n", r$texto))
})

# -----------------------------------------------------------------------------
# CONTROL DE COBERTURA: las flechas ← ↑ → son excepciones MEDIDAS (compilan),
# pero ↔ ⇒ ⇔ ↺ del mismo bloque Unicode NO. Fija que la excepción no se coma
# el rango entero — sería el modo de fallo silencioso más probable aquí.
test_that("las excepciones de flechas no desactivan el resto del bloque", {
  d <- tempfile(); dir.create(d)
  f_ok <- escribir_rmd(file.path(d, "flechas_ok.Rmd"), paste0(
    "Question\n========\nFlechas que compilan: ← ↑ →\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))
  f_mal <- escribir_rmd(file.path(d, "flechas_mal.Rmd"), paste0(
    "Question\n========\nFlechas que rompen: ↔ ⇒ ⇔ ↺\n\n",
    "Answerlist\n----------\n* A\n* B\n\nSolution\n========\nSol.\n\n",
    "Answerlist\n----------\n* Correcto\n* Incorrecto\n", META))

  expect_equal(correr_detector(f_ok)$exit, 0L,
    info = "← ↑ → están MEDIDOS como compilables: no deben disparar")
  expect_equal(correr_detector(f_mal)$exit, 1L,
    info = "↔ ⇒ ⇔ ↺ están MEDIDOS como rompedores: deben disparar")
})

# -----------------------------------------------------------------------------
# BARRIDO DEL REPOSITORIO
test_that("ningún .Rmd fuera del allowlist tiene glifos rompedores en Markdown", {
  dirs <- file.path(repo_root, "A-Produccion",
                    c("01-En-PreDesarrollo", "02-En-Desarrollo", "03-En-Produccion"))
  dirs <- dirs[dir.exists(dirs)]
  skip_if(length(dirs) == 0, "no existen los directorios de A-Produccion")

  archivos <- unlist(lapply(dirs, list.files, pattern = "\\.Rmd$",
                            recursive = TRUE, full.names = TRUE))
  skip_if(length(archivos) == 0, "no se encontraron .Rmd")

  legacy <- readLines(ALLOWLIST, warn = FALSE, encoding = "UTF-8")
  legacy <- trimws(legacy[!grepl("^\\s*#", legacy) & nzchar(trimws(legacy))])

  rel <- function(p) sub(paste0("^", repo_root, "/"), "", normalizePath(p, mustWork = FALSE))

  infractores <- character(0)
  for (a in archivos) {
    if (bloquea(a) && !(rel(a) %in% legacy))
      infractores <- c(infractores, rel(a))
  }

  expect_equal(length(infractores), 0L,
    info = paste0(
      "Estos .Rmd tienen glifos que ROMPEN pdflatex en texto Markdown visible ",
      "y no están en el allowlist legacy:\n  ",
      paste(infractores, collapse = "\n  "),
      "\n\nNo compilarán en PDF/NOPS (HTML no lo detecta: no pasa por LaTeX).",
      "\nFix: sustituir por ASCII o por el comando LaTeX en math ($\\le$, $\\pi$).",
      "\nDiagnóstico: Rscript .claude/scripts/validar_glifos_latex.R <archivo>",
      "\nEl allowlist NO ADMITE ALTAS: sólo puede decrecer."))
})

# -----------------------------------------------------------------------------
# HIGIENE DEL ALLOWLIST: si un archivo listado ya se corrigió o desapareció,
# debe salir de la lista. Es lo que hace que "sólo puede decrecer" sea real y
# no una frase en un comentario.
test_that("el allowlist no contiene entradas obsoletas", {
  legacy <- readLines(ALLOWLIST, warn = FALSE, encoding = "UTF-8")
  legacy <- trimws(legacy[!grepl("^\\s*#", legacy) & nzchar(trimws(legacy))])
  skip_if(length(legacy) == 0, "allowlist vacío")

  inexistentes <- legacy[!file.exists(file.path(repo_root, legacy))]
  expect_equal(length(inexistentes), 0L,
    info = paste0("Entradas del allowlist que ya no existen:\n  ",
                  paste(inexistentes, collapse = "\n  "), "\nBorrarlas."))

  vivos <- legacy[file.exists(file.path(repo_root, legacy))]
  ya_limpios <- vivos[!vapply(vivos, function(p)
    bloquea(file.path(repo_root, p)), logical(1))]
  expect_equal(length(ya_limpios), 0L,
    info = paste0("Estos archivos del allowlist YA están limpios; sacarlos ",
                  "para que la lista decrezca:\n  ",
                  paste(ya_limpios, collapse = "\n  ")))
})

# -----------------------------------------------------------------------------
test_that("el hook post-exams2 cablea la FASE 2O", {
  hook <- file.path(repo_root, ".claude", "hooks", "post-exams2-validation.sh")
  skip_if_not(file.exists(hook), "no existe el hook")
  txt <- paste(readLines(hook, warn = FALSE), collapse = "\n")

  expect_true(grepl("FASE 2O", txt, fixed = TRUE),
    info = "El hook no declara la FASE 2O")
  expect_true(grepl("validar_glifos_latex.R", txt, fixed = TRUE),
    info = "El hook no invoca el detector compartido (¿implementación duplicada?)")
  expect_true(grepl("ERR_GLIFO_LATEX", txt, fixed = TRUE),
    info = "El hook no usa el código de error ERR_GLIFO_LATEX")

  # BUG REAL cazado el 2026-08-15 por el control end-to-end del hook:
  # la fase se escribió con `if [ -f "$RMD_FILE" ]`, que es la ruta RELATIVA al
  # cwd del comando. El hook no hace cd a ese cwd, así que la condición era
  # falsa siempre que el usuario invocara exams2pdf("archivo.Rmd") con ruta
  # relativa — el caso normal. La fase entera quedaba MUDA sin que nada avisara:
  # un gate que no se ejecuta se ve exactamente igual que un gate que no
  # encuentra nada. La variable correcta es $RMD_PATH (absoluta).
  # Recorte POR LÍNEAS: en R el `.` de una regex no cruza saltos de línea, así
  # que un sub(".*FASE 2O...") devolvería el archivo entero — y entonces el
  # check daría un falso positivo por la FASE 2K, que sí usa $RMD_FILE.
  lineas <- readLines(hook, warn = FALSE)
  ini <- grep("FASE 2O: GLIFOS UNICODE", lineas)[1]
  fin <- grep("RESUMEN FINAL Y ACCIONES", lineas)
  fin <- fin[fin > ini][1]
  expect_false(is.na(ini), info = "No se localizó el bloque de la FASE 2O en el hook")
  bloque_lineas <- lineas[ini:(fin - 1)]
  # Sólo CÓDIGO: el bloque lleva un comentario que ADVIERTE contra `-f "$RMD_FILE"`,
  # y un discriminador que no distinga "usa el patrón" de "advierte contra el
  # patrón" castiga justo a la documentación que arregla el problema. Es el
  # mismo bug de discriminador que el repo ya cazó en la v3.20.8.
  bloque <- paste(grep("^\\s*#", bloque_lineas, invert = TRUE, value = TRUE),
                  collapse = "\n")

  expect_true(grepl('-f "$RMD_PATH"', bloque, fixed = TRUE),
    info = paste0("La FASE 2O debe condicionar sobre $RMD_PATH (absoluta). ",
                  "Con $RMD_FILE (relativa) la fase no se ejecuta nunca y el ",
                  "gate queda mudo."))
  expect_false(grepl('-f "$RMD_FILE"', bloque, fixed = TRUE),
    info = "La FASE 2O sigue condicionando sobre $RMD_FILE (ruta relativa)")
})
