# =============================================================================
# Hook post-render: ninguna decision puede depender de una carrera de SIGPIPE.
# .claude/hooks/post-exams2-validation.sh (activacion, bloque 2N, bloque 2O)
#
# Por que existe: el hook corre con `set -o pipefail` y decidia con
#     if ! echo "$TEXTO" | grep -q 'patron'; then ...
# grep -q sale en la primera coincidencia; si el texto pasa de unos 30 KB y la
# coincidencia esta arriba, echo recibe SIGPIPE, la tuberia devuelve 141 y el
# `if` lo lee como «no coincide». Con texto de muchas lineas, 20/20 fallos
# desde 37 KB y 0/20 hasta 16 KB. Tres sitios tenian el patron:
#
#   activacion  `echo "$COMMAND" | grep -q 'exams2'`: con un comando Bash de
#               37 KB el arsenal entero se salto 19 de 20 veces, sin avisar
#               (medido el 2026-10-28).
#   FASE 2N     «sin aleatorizacion» falso, y la señal 1 (file.copy de PNG)
#               callada. Medido en probabilidad-fichas-tabla-doble-entrada-n2:
#               el mismo bloque (37 KB) paso en una corrida y fallo en la
#               siguiente (2026-10-27).
#   FASE 2O     `echo "$GLIFOS_OUT" | grep -q`: un WARN_GLIFO_LATEX salia
#               como «sin glifos» 20 de 20 veces con 37 KB (2026-10-28). El
#               detector real escribe menos de 1 KB cuando sale con 0, asi que
#               aqui responde un detector falso: se prueba la decision del
#               hook, no el tamaño de la salida de hoy.
#
# El codigo se EXTRAE del hook real y se ejecuta en bash con pipefail, como en
# el hook: una copia paralela podria divergir sin que nada lo notara.
# HOOK_PIPEFAIL permite apuntar a otra copia (p. ej. un respaldo previo, para
# comprobar que este test lo caza). Los fixtures viven en tempdir().
# =============================================================================
library(testthat)

repo_root <- local({
  r <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(r) && length(r) == 1 && dir.exists(r)) r else normalizePath("../..")
})

HOOK <- Sys.getenv("HOOK_PIPEFAIL", file.path(repo_root, ".claude", "hooks", "post-exams2-validation.sh"))

leer_hook <- function(hook_path) {
  stopifnot(file.exists(hook_path))
  readLines(hook_path, warn = FALSE, encoding = "UTF-8")
}

# Ejecuta lineas de bash; devuelve stdout y stderr juntos.
correr_bash <- function(lineas, stdin = "") {
  sh <- tempfile(fileext = ".sh")
  writeLines(lineas, sh, useBytes = TRUE)
  paste(suppressWarnings(system2("bash", sh, stdout = TRUE, stderr = TRUE, stdin = stdin)),
        collapse = "\n")
}

# `n` lineas de ~80 caracteres (1200 lineas ~ 100 KB).
relleno <- function(n, prefijo = "## relleno") {
  if (n > 0) sprintf("%s %06d %s", prefijo, seq_len(n), strrep("x", 64)) else character(0)
}

# --- Activacion: el hook REAL desde la primera linea hasta el `fi` del chequeo de exams2 ---
extraer_activacion <- function(hook_path) {
  l <- leer_hook(hook_path)
  codigo <- grep("^\\s*#", l, invert = TRUE)          # la cabecera nombra exams2 en comentarios
  chk <- codigo[grepl("exams2", l[codigo])][1]
  fin <- chk - 1 + match("fi", l[chk:length(l)])
  stopifnot(!is.na(chk), !is.na(fin))
  l[1:fin]
}

# El hook recibe por stdin el JSON de la llamada a Bash, como se lo pasa Claude Code.
correr_activacion <- function(comando, hook = HOOK) {
  json <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(tool_input = list(command = paste(comando, collapse = "\n")),
                                   cwd = tempdir()), auto_unbox = TRUE), json, useBytes = TRUE)
  correr_bash(c(extraer_activacion(hook), 'echo "ACTIVADO"'), stdin = json)
}

# --- Bloques 2N y 2O: el `if` de primer nivel entre su cabecera y la siguiente ---
extraer_bloque <- function(hook_path, cabecera, siguiente, inicio) {
  l <- leer_hook(hook_path)
  h <- tail(grep(cabecera, l), 1); s <- tail(grep(siguiente, l), 1)   # la cabecera del hook también lista las fases
  stopifnot(length(h) == 1, length(s) == 1, s > h)
  ini <- h - 1 + grep(inicio, l[h:s])[1]
  fin <- h - 1 + max(grep("^fi$", l[h:s]))
  stopifnot(!is.na(ini), fin > ini)
  l[ini:fin]
}

correr_2n <- function(rmd, hook = HOOK) {
  correr_bash(c("set -o pipefail", "ADVERTENCIAS_TOTALES=0", sprintf("RMD_FILE='%s'", rmd),
                extraer_bloque(hook, "^# FASE 2N:", "^# FASE 2O:", '^if \\[ -n "\\$RMD_FILE" \\]; then'),
                'echo "ADVERTENCIAS=$ADVERTENCIAS_TOTALES"'))
}

# .Rmd minimo: chunk data_generation con las lineas dadas y `n` lineas de relleno detras.
fixture <- function(lineas_dg, n = 0) {
  f <- tempfile(fileext = ".Rmd")
  writeLines(c('```{r data_generation, echo = FALSE, results = "hide"}', lineas_dg, relleno(n), "```",
               "", "Question", "========", "", "Texto."), f, useBytes = TRUE)
  f
}

# La 2O con un detector falso que imprime `salida` y sale con 0, como el real sin bloqueantes.
correr_2o <- function(salida, hook = HOOK) {
  proy <- tempfile("proy"); dir.create(file.path(proy, ".claude", "scripts"), recursive = TRUE)
  txt <- file.path(proy, "salida.txt"); writeLines(salida, txt, useBytes = TRUE)
  writeLines(sprintf('writeLines(readLines("%s")); quit(status = 0)', txt),
             file.path(proy, ".claude", "scripts", "validar_glifos_latex.R"))
  rmd <- tempfile(fileext = ".Rmd"); writeLines("x", rmd)
  correr_bash(c("set -o pipefail", "ERRORES_TOTALES=0", "ADVERTENCIAS_TOTALES=0",
                sprintf("PROJECT_DIR='%s'", proy), sprintf("RMD_PATH='%s'", rmd),
                extraer_bloque(hook, "^# FASE 2O:", "^# RESUMEN FINAL", '^if \\[ -f "\\$RMD_PATH" \\]; then'),
                'echo "ADVERTENCIAS=$ADVERTENCIAS_TOTALES"'))
}

# --- Activacion ----------------------------------------------------------------
test_that("activacion: un comando de 100 KB con exams2 arriba activa el hook (5 corridas)", {
  cmd <- c("Rscript -e 'exams::exams2html(\"x.Rmd\")'", relleno(1200, "# relleno"))
  expect_gt(sum(nchar(cmd, type = "bytes")), 60000)
  for (i in 1:5) expect_match(correr_activacion(cmd), "ACTIVADO", fixed = TRUE)
})

test_that("control: la activacion depende de exams2, con comando chico y grande", {
  expect_match(correr_activacion("Rscript -e 'exams::exams2html(\"x.Rmd\")'"), "ACTIVADO", fixed = TRUE)
  for (n in c(0, 1200))
    expect_false(grepl("ACTIVADO", correr_activacion(c("ls -la", relleno(n, "# relleno"))), fixed = TRUE))
})

# --- FASE 2N -------------------------------------------------------------------
test_that("bloque grande con sample() arriba: la 2N ve la aleatorizacion (5 corridas)", {
  f <- fixture(c("x <- sample(1:3, 1)", "y <- x + 1"), n = 1200)
  expect_gt(file.size(f), 60000)
  for (i in 1:5) {
    out <- correr_2n(f)
    expect_match(out, "FASE 2N: señales estáticas OK", fixed = TRUE)
    expect_false(grepl("no se detecta ninguna función de aleatorización", out, fixed = TRUE))
  }
})

test_that("control: sin aleatorizacion la 2N advierte, con bloque chico y grande", {
  for (n in c(0, 1200)) {
    out <- correr_2n(fixture(c("x <- 3", "y <- x + 1"), n = n))
    expect_match(out, "no se detecta ninguna función de aleatorización", fixed = TRUE)
    expect_match(out, "ADVERTENCIAS=1", fixed = TRUE)
  }
})

test_that("file.copy de PNG se advierte tambien con bloque grande (5 corridas)", {
  f <- fixture(c("file.copy('a.png', 'b.png')", "x <- sample(1:3, 1)"), n = 1200)
  for (i in 1:5) expect_match(correr_2n(f), "se usa file.copy() para PNGs", fixed = TRUE)
})

# --- FASE 2O -------------------------------------------------------------------
test_that("2O: WARN_GLIFO_LATEX en una salida de 100 KB suma la advertencia (5 corridas)", {
  salida <- c("WARN_GLIFO_LATEX: 3 linea(s) con glifos rompedores en codigo R / metadatos.",
              relleno(1200, "  relleno"))
  expect_gt(sum(nchar(salida, type = "bytes")), 60000)
  for (i in 1:5) {
    out <- correr_2o(salida)
    expect_match(out, "ADVERTENCIAS=1", fixed = TRUE)
    expect_false(grepl("sin glifos que rompan pdflatex", out, fixed = TRUE))
  }
})

test_that("2O: INFO_GLIFO_COMENTARIO en una salida de 100 KB se informa (5 corridas)", {
  salida <- c("INFO_GLIFO_COMENTARIO: 3 linea(s) con glifos rompedores en comentarios R",
              relleno(1200, "  relleno"))
  for (i in 1:5) {
    out <- correr_2o(salida)
    expect_match(out, "No suma nada: los comentarios R no llegan a LaTeX", fixed = TRUE)
    expect_match(out, "ADVERTENCIAS=0", fixed = TRUE)
  }
})

test_that("control: sin WARN ni INFO la 2O da verde, con salida chica y grande", {
  for (n in c(0, 1200)) {
    out <- correr_2o(c("OK: sin glifos que rompan pdflatex", relleno(n, "  relleno")))
    expect_match(out, "FASE 2O: sin glifos que rompan pdflatex", fixed = TRUE)
    expect_match(out, "ADVERTENCIAS=0", fixed = TRUE)
  }
})

# --- Guardia estatica ----------------------------------------------------------
# grep -q no produce nada mas que su codigo de salida, y detras de una tuberia
# con pipefail ese codigo ya no es solo el suyo: toda aparicion es el defecto.
test_that("ninguna decision del hook pasa texto por una tuberia a grep -q", {
  l <- leer_hook(HOOK)
  codigo <- l[!grepl("^\\s*#", l)]
  expect_equal(grep("\\|\\s*grep\\s+(-[A-Za-z]*q|--quiet|--silent)", codigo, value = TRUE), character(0))
})
