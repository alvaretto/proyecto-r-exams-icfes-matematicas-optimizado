# =============================================================================
# Guardia del documento de cobertura: no puede divergir del runner ni del hook.
#   .claude/docs/VALIDATOR_COBERTURA_ICFES.md
#     <- tests/run_all_tests.R                    (bloque SUITES: generado)
#     <- .claude/hooks/post-exams2-validation.sh  (fases y versión: a mano)
#
# Por qué existe: el documento decía «19 suites» cuando el runner tenía 42, y
# describía el hook v6.0 con 10 fases cuando ya tenía 15 (corregido a mano el
# 2026-10-28). Una cifra copiada a mano en prosa se desactualiza sin que nada
# falle. La parte derivada del runner ahora se GENERA con
# tests/sincronizar_doc_suites.R; este test falla si el documento no coincide.
#
# Qué fija
#   (a) El bloque SUITES del documento (tabla, cifras, frase del modo rápido)
#       es idéntico al que genera el script desde el runner REAL.
#   (b) Esa comparación MUERE ante mutantes sobre copias en tempdir(): quitar
#       una fila, cambiar un archivo, alterar una cifra, quitar un marcador...
#       y ante la deriva del runner (suite añadida, quitada, `watch` cambiado).
#       Una guardia que nadie mata no es guardia. Los mutantes parten de un
#       documento en sincronía construido aquí, NO del estado en vivo: si el
#       documento real está desfasado solo falla (a), con su mensaje accionable.
#   (c) La tabla de fases de la sección del hook declara exactamente las fases
#       de la cabecera del hook (`# FASE 2X:`).
#   (d) La sección del hook menciona la versión vigente del hook (la de su
#       cabecera, `# post-exams2-validation.sh (vX.Y`).
#   (e) En los documentos vivos que nombran el arsenal entero («FASES 2A-2X»),
#       X es la última fase del hook. Una docena de documentos seguía en 2A-2H
#       o 2A-2J con el hook en 2O (barrido del 2026-10-28).
#
# (d) mira SOLO la sección del hook y no todo el documento por una trampa
# medida: la tabla generada copia el `nombre` de cada suite, y el de la
# #43 (Hook post-render ... (v6.2)) ya lleva una versión; buscar en todo el
# documento daba verde aunque la sección del hook siguiera diciendo v6.1.
#
# Las rutas se pueden cambiar con DOC_COBERTURA, RUNNER_SUITES y
# HOOK_POST_EXAMS2 (p. ej. para probar la guardia contra una copia vieja).
# =============================================================================
library(testthat)

repo_root <- local({
  r <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(r) && length(r) == 1 && dir.exists(r)) r else normalizePath("../..")
})

en_entorno <- function(variable, defecto) {
  v <- Sys.getenv(variable, "")
  if (nzchar(v)) v else defecto
}

SCRIPT  <- file.path(repo_root, "tests", "sincronizar_doc_suites.R")
DOC     <- en_entorno("DOC_COBERTURA", file.path(repo_root, ".claude", "docs", "VALIDATOR_COBERTURA_ICFES.md"))
RUNNER  <- en_entorno("RUNNER_SUITES", file.path(repo_root, "tests", "run_all_tests.R"))
HOOK    <- en_entorno("HOOK_POST_EXAMS2", file.path(repo_root, ".claude", "hooks", "post-exams2-validation.sh"))
COMANDO <- "Rscript tests/sincronizar_doc_suites.R"

# El generador se carga con source(): solo define funciones (su CLI corre únicamente bajo Rscript).
gen <- new.env()
source(SCRIPT, local = gen, encoding = "UTF-8")
gen$asegurar_utf8()   # el documento, el runner y el hook son UTF-8: en LC_ALL=C los regex dan falsos rojos

suites_reales <- gen$leer_suites(RUNNER)
n_reales      <- gen$contar_test_that(suites_reales$archivo, repo_root)
esperado_real <- gen$generar_bloque(suites_reales, n_reales)

# --- Copias en tempdir() -----------------------------------------------------------
escribir_copia <- function(lineas, ext = ".md") {
  ruta <- tempfile(fileext = ext)
  writeLines(lineas, ruta, useBytes = TRUE)
  ruta
}

# El mutante se escribe en una copia y se vuelve a leer con la lectura del generador:
# pasa por el mismo camino de E/S que el documento real.
copia_leida <- function(lineas) gen$leer_lineas(escribir_copia(lineas))

# Documento en sincronía con el runner real, sea cual sea el estado en vivo del bloque.
doc_en_sincronia <- function() gen$reescribir(gen$leer_lineas(DOC), esperado_real)

fila <- function(l, k) grep(paste0("^\\| ", k, " \\|"), l)
cualquier_fila <- function(l) grep("^\\| [0-9]+ \\|", l)

cambiar_cifra <- function(l, patron, nuevo) {
  i <- which(startsWith(l, gen$PREFIJO_CIFRAS))
  l[i] <- sub(patron, nuevo, l[i])
  l
}

# Mutantes del documento. `msg` es un fragmento ASCII del diagnóstico que debe aparecer;
# `reparable`: TRUE = reescribir() lo repara; FALSE = reescribir() debe negarse (marcadores
# rotos: no adivina); NA = no se prueba la reparación.
MUTANTES <- list(
  "quitar una fila" = list(
    f = function(l) l[-fila(l, 5)],
    msg = "falta la fila 5", reparable = TRUE),
  "cambiar el archivo de una fila" = list(
    f = function(l) { i <- fila(l, 7); l[i] <- sub("`[^`]+`", "`test_otro.R`", l[i]); l },
    msg = "la fila 7 difiere", reparable = TRUE),
  "cambiar el texto de 'Que valida' de una fila" = list(
    f = function(l) { i <- fila(l, 3); l[i] <- sub(" \\|$", " (editado a mano) |", l[i]); l },
    msg = "la fila 3 difiere", reparable = TRUE),
  "anadir una fila que el runner no tiene" = list(
    f = function(l) append(l, "| 9999 | `test_fantasma.R` | Suite inexistente |", after = max(cualquier_fila(l))),
    msg = "sobra la fila 9999", reparable = TRUE),
  "duplicar una fila" = list(
    f = function(l) { i <- fila(l, 4); append(l, l[i], after = i) },
    msg = "la fila 4 aparece 2 veces", reparable = TRUE),
  "alterar la cifra de suites" = list(
    f = function(l) cambiar_cifra(l, ": [0-9]+ suites?,", ": 1000 suites,"),
    msg = "cifras desactualizadas", reparable = TRUE),
  "alterar la cifra de bloques test_that" = list(
    f = function(l) cambiar_cifra(l, " [0-9]+ bloques? `test_that`", " 9999 bloques `test_that`"),
    msg = "cifras desactualizadas", reparable = TRUE),
  "alterar la cifra de suites sin watch" = list(
    f = function(l) cambiar_cifra(l, " y [0-9]+ suites? sin", " y 1000 suites sin"),
    msg = "cifras desactualizadas", reparable = TRUE),
  "quitar la linea de cifras" = list(
    f = function(l) l[!startsWith(l, gen$PREFIJO_CIFRAS)],
    msg = "de cifras; el runner da", reparable = TRUE),
  "editar la frase del modo rapido" = list(
    f = function(l) { i <- which(startsWith(l, gen$PREFIJO_FRASE)); l[i] <- paste0(l[i], " Editado a mano."); l },
    msg = "frase del modo", reparable = TRUE),
  "quitar la frase del modo rapido" = list(
    f = function(l) l[!startsWith(l, gen$PREFIJO_FRASE)],
    msg = "falta la frase del modo", reparable = TRUE),
  "permutar dos filas" = list(
    f = function(l) { i <- fila(l, 3); j <- fila(l, 4); l[c(i, j)] <- l[c(j, i)]; l },
    msg = "el bloque difiere del generado", reparable = TRUE),
  "insertar una linea suelta dentro del bloque" = list(
    f = function(l) append(l, "Nota anadida a mano.", after = fila(l, 2)),
    msg = "el bloque difiere del generado", reparable = TRUE),
  "editar el marcador de inicio" = list(
    f = function(l) { i <- which(startsWith(l, gen$PREFIJO_INICIO)); l[i] <- sub(" -->$", " (editado) -->", l[i]); l },
    msg = "el marcador de inicio no es el generado", reparable = TRUE),
  "editar el marcador de fin" = list(
    f = function(l) { i <- which(startsWith(l, gen$PREFIJO_FIN)); l[i] <- "<!-- SUITES:FIN (editado) -->"; l },
    msg = "el bloque difiere del generado", reparable = TRUE),
  "quitar el marcador de inicio" = list(
    f = function(l) l[!startsWith(l, gen$PREFIJO_INICIO)],
    msg = "falta el marcador de inicio", reparable = FALSE),
  "quitar el marcador de fin" = list(
    f = function(l) l[!startsWith(l, gen$PREFIJO_FIN)],
    msg = "falta el marcador de fin", reparable = FALSE),
  "repetir el marcador de inicio" = list(
    f = function(l) { i <- which(startsWith(l, gen$PREFIJO_INICIO)); append(l, l[i], after = i) },
    msg = "una sola vez", reparable = FALSE),
  "invertir los dos marcadores" = list(
    f = function(l) {
      i <- which(startsWith(l, gen$PREFIJO_INICIO)); j <- which(startsWith(l, gen$PREFIJO_FIN))
      l[c(i, j)] <- l[c(j, i)]; l
    },
    msg = "antes que el de inicio", reparable = FALSE),
  "quitar los dos marcadores" = list(
    f = function(l) l[!startsWith(l, gen$PREFIJO_INICIO) & !startsWith(l, gen$PREFIJO_FIN)],
    msg = "no tiene el bloque generado", reparable = NA)
)

# El documento como estaba antes del generador: sin marcadores ni línea de cifras (ni su línea vacía).
a_formato_legado <- function(l) {
  l <- l[!startsWith(l, gen$PREFIJO_INICIO) & !startsWith(l, gen$PREFIJO_FIN)]
  i <- which(startsWith(l, gen$PREFIJO_CIFRAS))
  l[-c(i, i + 1L)]
}

# Copia del runner real con una suite de más, puesta la primera de la lista.
runner_con_suite_extra <- function() {
  txt <- readLines(RUNNER, warn = FALSE, encoding = "UTF-8")
  i <- grep("^suites\\s*<-\\s*list\\(\\s*$", txt)
  stopifnot("el runner debe abrir la lista con una linea 'suites <- list('" = length(i) == 1L)
  escribir_copia(append(txt, '  list(nombre = "Suite nueva", archivo = "tests/run_all_tests.R", critico = TRUE),',
                        after = i), ".R")
}

# Invoca la CLI como proceso aparte y devuelve el exit REAL (sin tuberías que lo enmascaren).
correr_cli <- function(args, doc, runner = RUNNER) {
  salida <- tempfile()
  st <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(SCRIPT), args),
                stdout = salida, stderr = salida,
                env = c(paste0("DOC_COBERTURA=", shQuote(doc)), paste0("RUNNER_SUITES=", shQuote(runner))))
  list(exit = st, texto = paste(readLines(salida, warn = FALSE), collapse = "\n"))
}

# --- El generador: lectura del runner, cuenta de bloques, bloque generado -------------
test_that("leer_suites lee la lista del runner sin ejecutar nada más del runner", {
  runner <- escribir_copia(c(
    'stop("el runner no debe ejecutarse")',
    'f <- function() { suites <- list(list(nombre = "falsa", archivo = "x.R")) }',
    'suites <- list(',
    '  list(nombre = "Uno _a_", archivo = "tests/testthat/test_uno.R", critico = TRUE),',
    '  list(nombre = "Dos", archivo = "tests/testthat/test_dos.R", critico = TRUE, watch = c("dos")),',
    '  list(nombre = "Tres", archivo = "tests/testthat/test_tres.R", critico = TRUE)',
    ')',
    'quit(status = 9)'), ".R")
  s <- gen$leer_suites(runner)
  expect_equal(nrow(s), 3L)
  expect_equal(s$nombre, c("Uno _a_", "Dos", "Tres"))
  expect_equal(s$archivo[2], "tests/testthat/test_dos.R")
  expect_equal(s$con_watch, c(FALSE, TRUE, FALSE))
})

test_that("leer_suites exige UNA asignación de nivel superior a `suites` y suites bien formadas", {
  una <- 'suites <- list(list(nombre = "a", archivo = "a.R"))'
  expect_error(gen$leer_suites(escribir_copia(c("x <- 1", "f <- function() { suites <- list() }"), ".R")),
               "nivel superior")
  expect_error(gen$leer_suites(escribir_copia(c(una, una), ".R")), "nivel superior")
  expect_error(gen$leer_suites(escribir_copia('suites <- list(list(nombre = "a"))', ".R")),
               "no tiene `nombre` y `archivo`", fixed = TRUE)
  expect_error(gen$leer_suites(escribir_copia("suites <- list()", ".R")), "al menos una suite")
  expect_error(gen$leer_suites(file.path(tempdir(), "no-existe.R")), "no existe el runner")
})

test_that("contar_test_that cuenta las líneas test_that( de cada archivo, una vez por archivo", {
  raiz <- tempfile("raiz")
  dir.create(file.path(raiz, "tests", "testthat"), recursive = TRUE)
  writeLines(c('test_that("a", {', '  expect_true(TRUE)', '})', '  test_that("b", {', '})',
               '# test_that("comentado", {', 'x <- "test_that(en una cadena)"'),
             file.path(raiz, "tests", "testthat", "test_a.R"))
  writeLines(c('test_that("c", {', '})'), file.path(raiz, "tests", "testthat", "test_b.R"))
  expect_equal(gen$contar_test_that(c("tests/testthat/test_a.R", "tests/testthat/test_b.R"), raiz), 3L)
  expect_equal(gen$contar_test_that(rep("tests/testthat/test_a.R", 2), raiz), 2L)
  expect_error(gen$contar_test_that("tests/testthat/test_no_existe.R", raiz), "no existen")
})

test_that("generar_bloque: marcadores, cifras, tabla con \\_ y | escapados, frase del modo rápido", {
  s <- data.frame(nombre = c("Suite_uno _neg_", "Dos | pipe", "Tres", "Cuatro", "Cinco"),
                  archivo = c("tests/testthat/test_a.R", "tests/testthat/sub/test_b.R", "test_c.R", "d.R", "e.R"),
                  con_watch = c(FALSE, TRUE, FALSE, TRUE, FALSE), stringsAsFactors = FALSE)
  b <- gen$generar_bloque(s, 12L)
  expect_equal(b[1], "<!-- SUITES:INICIO — generado por tests/sincronizar_doc_suites.R; no editar a mano -->")
  expect_equal(b[length(b)], "<!-- SUITES:FIN -->")
  expect_equal(b[2], "Cifras (derivadas del runner): 5 suites, 12 bloques `test_that` y 3 suites sin `watch`.")
  expect_equal(b[4:5], c("| # | Archivo | Qué valida |", "|---|---------|-----------|"))
  expect_equal(b[6:10], c("| 1 | `test_a.R` | Suite\\_uno \\_neg\\_ |", "| 2 | `test_b.R` | Dos \\| pipe |",
                          "| 3 | `test_c.R` | Tres |", "| 4 | `d.R` | Cuatro |", "| 5 | `e.R` | Cinco |"))
  expect_equal(b[length(b) - 1L], paste0(
    "En modo rápido (`R_TESTS_QUICK=1`, el del pre-push) corren siempre las 3 suites sin `watch` (1, 3 y 5). ",
    "Las otras 2 corren solo si cambió algún archivo que vigilan."))

  fr <- gen$frase_modo_rapido
  expect_match(fr(4, integer(0)), "ninguna suite corre siempre: todas corren solo si", fixed = TRUE)
  expect_match(fr(4, 2L), "corre siempre la única suite sin `watch` (2). Las otras 3 corren", fixed = TRUE)
  expect_match(fr(2, 1L), "La otra corre solo si cambió algún archivo que vigila.", fixed = TRUE)
  expect_false(grepl("Las otras", fr(3, 1:3), fixed = TRUE))   # todas sin watch: no hay «otras»
  expect_match(gen$generar_bloque(s[1, ], 1L)[2], "1 suite, 1 bloque `test_that` y 1 suite sin `watch`", fixed = TRUE)
})

# --- (a) El documento real frente al runner real -------------------------------------------
test_that("(a) el bloque SUITES del documento es idéntico al que genera el runner real", {
  cmp <- gen$comparar(gen$leer_lineas(DOC), esperado_real)
  expect_true(cmp$ok, info = paste0(
    "El bloque SUITES de ", basename(DOC), " no coincide con ", basename(RUNNER), ":\n  ",
    paste(cmp$diferencias, collapse = "\n  "), "\n\nEjecutar: ", COMANDO))
})

# --- (b) La comparación muere ante mutantes -------------------------------------------------
test_that("(b) la comparación MUERE ante cada mutante del documento (copias en tempdir)", {
  base <- doc_en_sincronia()
  expect_true(gen$comparar(copia_leida(base), esperado_real)$ok, info = "control: la copia sin mutar debe pasar")
  for (nombre in names(MUTANTES)) {
    mut <- copia_leida(MUTANTES[[nombre]]$f(base))
    expect_false(identical(mut, base), info = paste0("el mutante '", nombre, "' no cambió nada: no probaría nada"))
    cmp <- gen$comparar(mut, esperado_real)
    expect_false(cmp$ok, info = nombre)
    expect_match(paste(cmp$diferencias, collapse = "\n"), MUTANTES[[nombre]]$msg, fixed = TRUE, info = nombre)
  }
})

test_that("(b) reescribir repara los mutantes, es idempotente, inserta el bloque la primera vez y exige marcadores completos", {
  base <- doc_en_sincronia()
  expect_identical(gen$reescribir(base, esperado_real), base)
  for (nombre in names(MUTANTES)) {
    m <- MUTANTES[[nombre]]
    mut <- copia_leida(m$f(base))
    if (isTRUE(m$reparable)) {
      reparado <- gen$reescribir(mut, esperado_real)
      expect_true(gen$comparar(reparado, esperado_real)$ok, info = nombre)
      expect_identical(reparado, base, info = nombre)
    } else if (identical(m$reparable, FALSE)) {
      expect_error(gen$reescribir(mut, esperado_real), "marcador", info = nombre)
    }
  }
  # Primera vez: el documento sin marcadores recupera exactamente el mismo bloque, en el mismo sitio.
  legado <- a_formato_legado(base)
  expect_false(gen$comparar(legado, esperado_real)$ok)
  expect_identical(gen$reescribir(legado, esperado_real), base)
  expect_error(gen$reescribir(c("# Titulo", "", "texto sin tabla"), esperado_real), "tabla anterior")
  expect_error(gen$reescribir(legado[!startsWith(legado, gen$PREFIJO_FRASE)], esperado_real), "frase del modo")
})

test_that("(b) deriva del runner: suite añadida, quitada o con `watch` cambiado dejan el documento viejo en rojo", {
  base <- doc_en_sincronia()
  n <- nrow(suites_reales)
  en_rojo <- function(suites, que) {
    cmp <- gen$comparar(copia_leida(base), gen$generar_bloque(suites, n_reales))
    expect_false(cmp$ok, info = que)
    paste(cmp$diferencias, collapse = "\n")
  }
  # Suite añadida: copia del runner real pasada por leer_suites()
  s_mas <- gen$leer_suites(runner_con_suite_extra())
  expect_equal(nrow(s_mas), n + 1L)
  expect_match(en_rojo(s_mas, "suite añadida"), paste0("falta la fila ", n + 1L), fixed = TRUE)
  # Suite quitada
  expect_match(en_rojo(suites_reales[-5, , drop = FALSE], "suite quitada"), paste0("sobra la fila ", n), fixed = TRUE)
  # Una suite con watch lo pierde / una suite sin watch lo gana: cambian las cifras y la frase
  con <- which(suites_reales$con_watch)[1L]
  sin <- which(!suites_reales$con_watch)[1L]
  s_pierde <- suites_reales; s_pierde$con_watch[con] <- FALSE
  s_gana <- suites_reales; s_gana$con_watch[sin] <- TRUE
  for (que in c("pierde", "gana")) {
    msg <- en_rojo(if (que == "pierde") s_pierde else s_gana, paste("watch", que))
    expect_match(msg, "cifras desactualizadas", fixed = TRUE)
    expect_match(msg, "frase del modo", fixed = TRUE)
  }
})

test_that("(b) CLI: --check no escribe y sale 0/1; sin argumentos reescribe; error de uso o de lectura sale 2", {
  base <- doc_en_sincronia()
  n <- nrow(suites_reales)
  ok <- escribir_copia(base)
  r <- correr_cli("--check", ok)
  expect_equal(r$exit, 0L, info = r$texto)

  malo <- escribir_copia(MUTANTES[["quitar una fila"]]$f(base))
  antes <- unname(tools::md5sum(malo))
  r <- correr_cli("--check", malo)
  expect_equal(r$exit, 1L, info = r$texto)
  expect_match(r$texto, "falta la fila 5", fixed = TRUE)
  expect_match(r$texto, COMANDO, fixed = TRUE)
  expect_equal(unname(tools::md5sum(malo)), antes, info = "--check no debe escribir nada")

  r <- correr_cli(character(0), malo)
  expect_equal(r$exit, 0L, info = r$texto)
  expect_identical(gen$leer_lineas(malo), base)
  expect_equal(correr_cli("--check", malo)$exit, 0L)
  despues <- unname(tools::md5sum(malo))
  r <- correr_cli(character(0), malo)
  expect_match(r$texto, "Sin cambios", fixed = TRUE)
  expect_equal(unname(tools::md5sum(malo)), despues, info = "segunda corrida: idempotente")

  expect_equal(correr_cli("--desconocido", ok)$exit, 2L)
  expect_equal(correr_cli("--check", file.path(tempdir(), "no-existe.md"))$exit, 2L)
  expect_equal(correr_cli(character(0), escribir_copia(c("# Titulo", "", "texto")))$exit, 2L)

  # El runner manda: con una suite más, el documento en sincronía queda en rojo y la regeneración lo sigue.
  runner_mas <- runner_con_suite_extra()
  r <- correr_cli("--check", ok, runner = runner_mas)
  expect_equal(r$exit, 1L, info = r$texto)
  expect_match(r$texto, paste0("falta la fila ", n + 1L), fixed = TRUE)
  expect_equal(correr_cli(character(0), ok, runner = runner_mas)$exit, 0L)
  expect_equal(correr_cli("--check", ok, runner = runner_mas)$exit, 0L)
  expect_equal(correr_cli("--check", ok)$exit, 1L)   # frente al runner real, el documento regenerado ya no coincide
})

# --- Hook y documento: helpers de (c) y (d) ---------------------------------------------------
hook_vivo <- function() gen$leer_lineas(HOOK)
doc_vivo  <- function() gen$leer_lineas(DOC)

# Cabecera del hook: las líneas de comentario/vacías del principio, hasta la primera de código.
cabecera_hook <- function(l) {
  codigo <- which(!grepl("^\\s*(#|$)", l))[1L]
  if (is.na(codigo)) l else l[seq_len(codigo - 1L)]
}

fases_del_hook <- function(l) {
  cab <- cabecera_hook(l)
  sub("^# FASE (2[A-Z]):.*$", "\\1", grep("^# FASE 2[A-Z]:", cab, value = TRUE))
}

version_del_hook <- function(l) {
  cab <- cabecera_hook(l)
  m <- Filter(length, regmatches(cab, regexec("^#\\s*post-exams2-validation\\.sh\\s*\\(v([0-9]+(\\.[0-9]+)*)", cab)))
  if (length(m)) m[[1L]][2L] else NA_character_
}

# La sección del documento que describe el hook: desde su encabezado `## ...post-exams2...` hasta el siguiente `## `.
seccion_del_hook <- function(doc) {
  ini <- grep("^##\\s.*post-exams2", doc)
  if (length(ini) != 1L) return(NULL)
  sig <- which(grepl("^##\\s", doc) & seq_along(doc) > ini)
  doc[ini:(if (length(sig)) sig[1L] - 1L else length(doc))]
}

# Filas `| 2A | ...` y `| 2C-2F | ...` de la tabla de fases; un rango declara todas sus fases.
fases_del_documento <- function(seccion) {
  filas <- grep("^\\|\\s*2[A-Z](\\s*-\\s*2[A-Z])?\\s*\\|", seccion, value = TRUE)
  unlist(lapply(filas, function(f) {
    celda <- gsub("\\s", "", strsplit(f, "|", fixed = TRUE)[[1L]][2L])
    letras <- regmatches(celda, gregexpr("[A-Z]", celda))[[1L]]
    paste0("2", LETTERS[seq(match(letras[1L], LETTERS), match(letras[length(letras)], LETTERS))])
  }))
}

comparar_fases <- function(doc, hook) {
  sec <- seccion_del_hook(doc)
  if (is.null(sec))
    return(list(ok = FALSE, error = "no encuentro la sección del hook en el documento (un encabezado '## ...post-exams2...')"))
  d <- fases_del_documento(sec)
  h <- fases_del_hook(hook)
  list(ok = length(h) > 0L && length(d) > 0L && !anyDuplicated(d) && !anyDuplicated(h) && setequal(d, h),
       error = NULL, faltan = setdiff(h, d), sobran = setdiff(d, h), repetidas = unique(d[duplicated(d)]))
}

informe_fases <- function(cmp, hook) {
  if (!is.null(cmp$error)) return(cmp$error)
  paste0("Fases de la cabecera del hook: ", paste(fases_del_hook(hook), collapse = " "),
         "\nFaltan en la tabla del documento: ", paste(cmp$faltan, collapse = " "),
         "\nSobran en la tabla del documento: ", paste(cmp$sobran, collapse = " "),
         if (length(cmp$repetidas)) paste0("\nRepetidas en la tabla: ", paste(cmp$repetidas, collapse = " ")),
         "\nActualizar a mano la tabla de fases de la sección del hook en ", basename(DOC), ".")
}

# `v<version>` como palabra completa: no `v6.10`, no `v6.1.2`, no `v16.1`, no `rev6.1`.
menciona_version <- function(lineas, version) {
  v <- gsub("\\.", "\\\\.", version)
  any(grepl(paste0("(?<![0-9A-Za-z.])v", v, "(?![0-9]|\\.[0-9])"), lineas, perl = TRUE))
}

version_en_seccion <- function(doc, version) {
  sec <- seccion_del_hook(doc)
  !is.null(sec) && !is.na(version) && menciona_version(sec, version)
}

# --- (c) Fases -------------------------------------------------------------------------------
test_that("(c) la tabla de fases de la sección del hook declara exactamente las fases de la cabecera del hook", {
  doc <- doc_vivo(); hook <- hook_vivo()
  expect_gt(length(fases_del_hook(hook)), 0L)   # un lector que no halla nada no probaría nada
  cmp <- comparar_fases(doc, hook)
  expect_true(cmp$ok, info = informe_fases(cmp, hook))
})

test_that("(c) la comparación de fases MUERE ante mutantes del documento y del hook (copias en tempdir)", {
  doc <- doc_vivo(); hook <- hook_vivo()
  if (!comparar_fases(doc, hook)$ok)
    skip("(c) en vivo está en rojo: los mutantes se verifican cuando el documento y el hook coinciden")
  fila_fase <- function(l, f) grep(paste0("^\\|\\s*", f, "\\s*\\|"), l)
  inicio_seccion <- grep("^##\\s.*post-exams2", doc)
  muere <- function(l, h = hook) {
    cmp <- comparar_fases(copia_leida(l), copia_leida(h))
    expect_false(cmp$ok)
    cmp
  }
  vive <- function(l, h = hook) {
    cmp <- comparar_fases(copia_leida(l), copia_leida(h))
    expect_true(cmp$ok, info = informe_fases(cmp, h))
  }

  # Documento
  expect_equal(muere(doc[-fila_fase(doc, "2K")])$faltan, "2K")
  expect_equal(muere(append(doc, "| 2P | Fase nueva | Aviso |", after = fila_fase(doc, "2O")))$sobran, "2P")
  recortado <- sub("^\\| 2C-2F \\|", "| 2C-2E |", doc)
  expect_false(identical(recortado, doc))
  expect_equal(muere(recortado)$faltan, "2F")
  expect_equal(muere(append(doc, doc[fila_fase(doc, "2B")], after = fila_fase(doc, "2B")))$repetidas, "2B")
  renombrado <- doc; renombrado[inicio_seccion] <- sub("post-exams2", "postexams", renombrado[inicio_seccion])
  expect_false(is.null(muere(renombrado)$error))
  # Controles: lo equivalente o lo irrelevante NO es una divergencia
  rango <- fila_fase(doc, "2C-2F")
  sueltas <- c(doc[seq_len(rango - 1L)], paste0("| ", c("2C", "2D", "2E", "2F"), " | x | y |"), doc[(rango + 1L):length(doc)])
  vive(sueltas)                                            # el rango y las filas sueltas declaran lo mismo
  vive(c("| 2Z | fila fuera de la sección del hook | no cuenta |", doc))

  # Hook
  h2o <- grep("^# FASE 2O:", hook)[1L]
  expect_equal(muere(doc, append(hook, "# FASE 2P: Fase nueva", after = h2o))$faltan, "2P")
  expect_equal(muere(doc, hook[-grep("^# FASE 2M:", hook)[1L]])$sobran, "2M")
  vive(doc, append(hook, "# FASE 2Z: titulo de una seccion del cuerpo", after = length(hook)))   # solo cuenta la cabecera
})

# --- (d) Versión -----------------------------------------------------------------------------
test_that("(d) la sección del hook del documento menciona la versión vigente del hook", {
  hook <- hook_vivo(); doc <- doc_vivo()
  ver <- version_del_hook(hook)
  expect_false(is.na(ver), info = "no pude leer la versión de la cabecera del hook (# post-exams2-validation.sh (vX.Y ...)")
  expect_false(is.null(seccion_del_hook(doc)), info = "no encuentro la sección del hook en el documento")
  expect_true(version_en_seccion(doc, ver), info = paste0(
    "El hook declara v", ver, " en su cabecera (", basename(HOOK), ") y la sección del hook de ", basename(DOC),
    " no la menciona. Actualizar a mano esa sección y la línea Versión."))
})

test_that("(d) menciona_version distingue la versión exacta de las parecidas", {
  casos <- c("hook v6.1 vigente" = TRUE, "(v6.1)" = TRUE, "`v6.1`" = TRUE, "termina en v6.1." = TRUE,
             "v6.1, v6.0" = TRUE, "hook v6.1: el informe" = TRUE,
             "v6.10" = FALSE, "v6.1.2" = FALSE, "v16.1" = FALSE, "v6x1" = FALSE,
             "hook v6.0" = FALSE, "6.1" = FALSE, "rev6.1" = FALSE)
  for (texto in names(casos))
    expect_identical(menciona_version(texto, "6.1"), unname(casos[texto]), info = texto)
  expect_true(menciona_version(c("nada", "hook v6.1.2"), "6.1.2"))
})

test_that("(d) la comprobación de versión MUERE ante mutantes del documento y del hook (copias en tempdir)", {
  hook <- hook_vivo(); doc <- doc_vivo()
  ver <- version_del_hook(hook)
  expect_false(is.na(ver))
  ini <- grep("^##\\s.*post-exams2", doc)
  expect_length(ini, 1L)
  fin <- ini + length(seccion_del_hook(doc)) - 1L

  # Documento en sincronía por construcción: la sección del hook menciona la versión vigente.
  doc_ok <- copia_leida(append(doc, paste0("Hook v", ver, "."), after = ini))
  expect_true(version_en_seccion(doc_ok, ver), info = "control: el documento con la versión vigente debe pasar")

  # 1. El hook sube de versión y el documento no (la versión se lee del hook, no está cableada).
  i <- grep("^#\\s*post-exams2-validation\\.sh\\s*\\(v", hook)[1L]
  hook_nuevo <- hook; hook_nuevo[i] <- sub("\\(v[0-9]+(\\.[0-9]+)*", "(v9.9", hook[i])
  ver_nueva <- version_del_hook(copia_leida(hook_nuevo))
  expect_equal(ver_nueva, "9.9")
  expect_false(version_en_seccion(doc_ok, ver_nueva))
  expect_true(version_en_seccion(copia_leida(append(doc_ok, "Hook v9.9.", after = ini)), ver_nueva))

  # 2. La versión vigente se borra de la sección: aunque aparezca fuera (otra sección, la tabla
  #    generada, el encabezado del documento) no cuenta.
  sin_mencion <- doc
  sin_mencion[ini:fin] <- gsub("v[0-9]+(\\.[0-9]+)*", "v0.0", doc[ini:fin])
  expect_false(identical(sin_mencion, doc))
  expect_false(version_en_seccion(copia_leida(sin_mencion), ver))
  fuera <- c(paste0("Hook v", ver, " (mención fuera de la sección)"), sin_mencion)
  expect_false(version_en_seccion(copia_leida(fuera), ver))
  tabla_generada <- append(sin_mencion, paste0("| 99 | `test_x.R` | Suite del hook (v", ver, ") |"), after = max(cualquier_fila(sin_mencion)))
  expect_false(version_en_seccion(copia_leida(tabla_generada), ver))

  # 3. Una versión parecida no vale: v<ver>0 (6.1 -> v6.10), v<ver>.1 (6.1 -> v6.1.1) y la misma sin la «v».
  for (parecida in c(paste0("v", ver, "0"), paste0("v", ver, ".1"), ver)) {
    mut <- copia_leida(append(sin_mencion, paste0("Hook ", parecida, "."), after = ini))
    expect_false(version_en_seccion(mut, ver), info = parecida)
  }

  # 4. Sin la sección del hook no hay nada que comparar: falla en vez de pasar en vacío.
  renombrado <- doc_ok; renombrado[ini] <- sub("post-exams2", "postexams", renombrado[ini])
  expect_false(version_en_seccion(copia_leida(renombrado), ver))
})

# --- (e) Rangos de fases en los documentos vivos ---------------------------------------------
# Donde un documento dice «FASES 2A-2X» para nombrar el arsenal entero, X tiene que ser la última
# fase del hook. La lista es cerrada a propósito: los orquestadores, los casos resueltos y las
# reglas íntegras narran incidentes con el rango de su fecha («2A-2J originales»), y
# stress-test-visual dice «si las fases 2A-2G pasan», un subrango correcto.
DOCS_CON_RANGO <- file.path(repo_root, c(
  "CLAUDE.md", ".claude/CLAUDE.md",
  ".claude/docs/VALIDATOR_COBERTURA_ICFES.md", ".claude/docs/HOOKS_Y_TESTING.md",
  ".claude/docs/FLUJO_AUTOMATICO_TESTING.md", ".claude/docs/ECOSISTEMA_TESTING.md",
  ".claude/docs/ESTRUCTURA_REPOSITORIO.md", ".claude/docs/MANUAL_USUARIO.md",
  ".claude/docs/COMANDOS_Y_SKILLS.md",
  ".claude/skills/generar-schoice/SKILL.md", ".claude/skills/generar-cloze/SKILL.md",
  ".claude/skills/revisar-schoice/SKILL.md", ".claude/skills/revisar-cloze/SKILL.md"))

# Rangos «2A-2X» (también con raya o espacios) que no terminan en `ultima`: «archivo:línea: rango».
rangos_viejos <- function(archivos, ultima) {
  unlist(lapply(archivos, function(f) {
    l <- gen$leer_lineas(f)
    m <- regmatches(l, gregexpr("2A ?[-–] ?2[A-Z]\\b", l))
    unlist(lapply(seq_along(m), function(i) {
      malos <- m[[i]][sub(".*2", "2", m[[i]]) != ultima]
      if (length(malos)) paste0(sub(paste0("^", repo_root, "/"), "", f), ":", i, ": ", malos)
    }))
  }))
}

test_that("(e) los documentos vivos nombran el arsenal entero con la última fase del hook", {
  ultima <- tail(sort(fases_del_hook(hook_vivo())), 1L)
  expect_true(all(file.exists(DOCS_CON_RANGO)), info = paste(DOCS_CON_RANGO[!file.exists(DOCS_CON_RANGO)], collapse = ", "))
  viejos <- rangos_viejos(DOCS_CON_RANGO, ultima)
  expect_true(length(viejos) == 0L,
              info = paste(c(paste0("Rangos que no terminan en ", ultima, ", la última fase del hook:"), viejos),
                           collapse = "\n  "))
})

test_that("(e) la comprobación de rangos MUERE ante mutantes (copias en tempdir)", {
  hook <- hook_vivo()
  ultima <- tail(sort(fases_del_hook(hook)), 1L)
  raiz <- gen$leer_lineas(file.path(repo_root, "CLAUDE.md"))
  expect_true(any(grepl(paste0("2A-", ultima), raiz, fixed = TRUE)), info = "control: el CLAUDE.md raíz nombra el rango")
  expect_length(rangos_viejos(escribir_copia(raiz), ultima), 0L)

  # 1. Un documento que vuelve al rango viejo.
  viejo <- escribir_copia(sub(paste0("2A-", ultima), "2A-2H", raiz, fixed = TRUE))
  expect_gt(length(rangos_viejos(viejo, ultima)), 0L)

  # 2. Variantes tipográficas del mismo rango viejo.
  for (v in c("FASES 2A–2H", "FASES 2A - 2H", "FASES 2A-2J."))
    expect_length(rangos_viejos(escribir_copia(v), ultima), 1L)

  # 3. El hook gana una fase y los documentos no: todos los rangos vivos quedan viejos.
  i <- max(grep("^# FASE 2[A-Z]:", cabecera_hook(hook)))
  hook_mas <- append(hook, "# FASE 2Z: fase de prueba", after = i)
  ultima_nueva <- tail(sort(fases_del_hook(copia_leida(hook_mas))), 1L)
  expect_equal(ultima_nueva, "2Z")
  expect_gt(length(rangos_viejos(DOCS_CON_RANGO, ultima_nueva)), 0L)
})
