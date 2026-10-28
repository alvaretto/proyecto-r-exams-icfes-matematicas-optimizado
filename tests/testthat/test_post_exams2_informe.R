# =============================================================================
# Hook post-render: el informe del arsenal tiene que llegarle al modelo.
# .claude/hooks/post-exams2-validation.sh (v6.2, entrega en JSON)
#
# Por qué existe: un PostToolUse que sale con 0 y escribe texto plano no le
# muestra nada al modelo. Claude Code manda ese stdout al log de depuración
# (https://code.claude.com/docs/en/hooks.md#exit-code-0). Hasta la v6.1 el
# arsenal imprimía su informe así: 116 líneas y 6 errores que nadie leía
# (verificado el 2026-10-28 con el atime de un .Rmd de prueba). Desde la v6.2
# el hook escribe en stdout un único objeto JSON:
#   - hookSpecificOutput.additionalContext, con el informe por debajo del tope
#     de 10 000 caracteres de Claude Code;
#   - decision "block" + reason si hay errores;
#   - systemMessage para el usuario.
#
# Cubre también dos silencios que la v6.1 tenía:
#   - un .Rmd que no se encuentra hacía salir al hook sin avisar;
#   - con ruta relativa y el proceso fuera del cwd de la sesión, las fases
#     2I-2L callaban: 0 errores donde la ruta absoluta daba 2.
#
# Se ejecuta el hook REAL con un proyecto falso (CLAUDE_PROJECT_DIR) sin los
# validadores R, salvo un arsenal de mentira para el informe enorme: corren las
# fases grep y el tiempo es de segundos. HOOK_INFORME permite apuntar a otra
# copia. Contra la v6.1 (.claude/backups/2026-10-28-informe-visible/) este test
# tiene que fallar.
# =============================================================================
library(testthat)

repo_root <- local({
  r <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(r) && length(r) == 1 && dir.exists(r)) r else normalizePath("../..")
})

HOOK <- Sys.getenv("HOOK_INFORME", file.path(repo_root, ".claude", "hooks", "post-exams2-validation.sh"))

# Tope de Claude Code para additionalContext y systemMessage, en unidades UTF-16.
TOPE_CLAUDE_CODE <- 10000L

RMD_CON_ERRORES <- c("Question", "========", "![](g.png)", "",          # 2I: imagen sin width
                     "Solution", "========", "La respuesta correcta es la Opción A.")  # 2J: letra
RMD_LIMPIO <- c("```{r data generation, echo = FALSE, results = \"hide\"}",
                "x <- sample(1:10, 1)", "```", "",
                "Question", "========", "![](g.png){width=60%}", "",
                "Solution", "========", "La respuesta correcta es la que conserva la suma.")

nuevo_caso <- function(rmd, arsenal_falso = NULL) {
  base <- tempfile("informe_")
  caso <- list(base = base, proy = file.path(base, "proy"), ej = file.path(base, "ej"),
               informes = file.path(base, "informes"), otro = file.path(base, "otro"))
  dir.create(file.path(caso$proy, ".claude", "scripts"), recursive = TRUE)
  for (d in c(caso$ej, caso$informes, caso$otro)) dir.create(d)
  caso$rmd <- file.path(caso$ej, "prueba.Rmd")
  writeLines(rmd, caso$rmd, useBytes = TRUE)
  if (!is.null(arsenal_falso))
    writeLines(arsenal_falso, file.path(caso$proy, ".claude", "scripts", "arsenal_validacion_completa.R"))
  caso
}

render <- function(ruta) sprintf("Rscript -e 'exams::exams2html(\"%s\")'", ruta)

# Corre el hook como lo corre Claude Code: JSON por stdin. `proceso_en` es el
# directorio del proceso del hook, que puede no ser el cwd de la sesión.
correr_hook <- function(caso, comando, proceso_en = caso$otro, hook = HOOK) {
  json <- tempfile(fileext = ".json"); out <- tempfile(); err <- tempfile()
  writeLines(jsonlite::toJSON(list(tool_input = list(command = comando), cwd = caso$ej),
                              auto_unbox = TRUE), json, useBytes = TRUE)
  viejo <- setwd(proceso_en); on.exit(setwd(viejo))
  ec <- suppressWarnings(system2("bash", shQuote(hook), stdin = json, stdout = out, stderr = err,
                                 env = c(paste0("CLAUDE_PROJECT_DIR=", shQuote(caso$proy)),
                                         paste0("POST_EXAMS2_INFORME_DIR=", shQuote(caso$informes)))))
  salida <- readLines(out, warn = FALSE, encoding = "UTF-8")
  list(ec = ec, stdout = salida, stderr = readLines(err, warn = FALSE, encoding = "UTF-8"),
       json = if (length(salida))
         tryCatch(jsonlite::fromJSON(paste(salida, collapse = "\n"), simplifyVector = FALSE),
                  error = function(e) NULL))
}

contexto <- function(r) r$json$hookSpecificOutput$additionalContext
largo_utf16 <- function(x) { cp <- utf8ToInt(enc2utf8(x)); sum(ifelse(cp > 0xFFFF, 2L, 1L)) }

test_that("un comando sin exams2 no escribe nada en stdout", {
  r <- correr_hook(nuevo_caso(RMD_LIMPIO), "ls -la")
  expect_equal(r$ec, 0L)
  expect_length(r$stdout, 0)
})

test_that("con errores: un solo JSON con el informe, decision block y reason", {
  caso <- nuevo_caso(RMD_CON_ERRORES)
  r <- correr_hook(caso, render(caso$rmd))
  expect_equal(r$ec, 0L)
  expect_length(r$stdout, 1)
  expect_false(is.null(r$json), info = "El stdout del hook no es JSON: Claude Code lo ignora entero")
  expect_equal(r$json$hookSpecificOutput$hookEventName, "PostToolUse")
  expect_equal(r$json$decision, "block")
  expect_match(r$json$reason, "2 error\\(es\\)")
  expect_match(contexto(r), "ERROR 16", fixed = TRUE)
  expect_match(contexto(r), "ERR_SOL_LETRA_LITERAL", fixed = TRUE)
  expect_match(r$json$systemMessage, "2 error\\(es\\)")
  expect_length(r$stderr, 0)
})

test_that("ruta relativa con el proceso fuera del cwd: 2I y 2J no callan", {
  caso <- nuevo_caso(RMD_CON_ERRORES)
  relativa <- correr_hook(caso, render("prueba.Rmd"))
  absoluta <- correr_hook(caso, render(caso$rmd))
  expect_equal(relativa$json$decision, "block",
               info = "Con ruta relativa el hook no vio los errores (v6.1: fases 2I-2L mudas)")
  expect_equal(relativa$json$reason, absoluta$json$reason)
})

test_that("un .Rmd que no existe produce un aviso, no silencio", {
  caso <- nuevo_caso(RMD_LIMPIO)
  r <- correr_hook(caso, render("no_existe.Rmd"))
  expect_equal(r$ec, 0L)
  expect_false(is.null(r$json), info = "La v6.1 salía callada: el arsenal no corría y nadie lo sabía")
  expect_match(contexto(r), "NO corrió", fixed = TRUE)
  expect_match(contexto(r), "no_existe.Rmd", fixed = TRUE)
  expect_null(r$json$decision)
})

test_that("sin errores: sin decision, y el informe completo queda en disco", {
  caso <- nuevo_caso(RMD_LIMPIO)
  r <- correr_hook(caso, render(caso$rmd))
  expect_false(is.null(r$json))
  expect_null(r$json$decision)
  expect_match(r$json$systemMessage, "0 error\\(es\\)")
  expect_false(grepl("═", contexto(r), fixed = TRUE), info = "Los marcos de las cajas gastan contexto")
  informe <- file.path(caso$informes, "prueba.txt")
  expect_true(file.exists(informe))
  expect_true(any(grepl("RESUMEN FINAL DEL ARSENAL", readLines(informe, warn = FALSE, encoding = "UTF-8"),
                        fixed = TRUE)))
  expect_match(contexto(r), informe, fixed = TRUE)
})

test_that("un informe enorme se resume por debajo del tope de Claude Code", {
  arsenal_falso <- c('for (i in 1:3000) cat(sprintf("  \\u274c ERROR %04d: detalle de la falla %s\\n", i, strrep("x", 40)))',
                     'cat("Total ERRORES: 3000\\n")', "quit(status = 1)")
  caso <- nuevo_caso(RMD_LIMPIO, arsenal_falso)
  r <- correr_hook(caso, render(caso$rmd))
  expect_false(is.null(r$json))
  expect_gt(file.size(file.path(caso$informes, "prueba.txt")), 100000)
  expect_lte(largo_utf16(contexto(r)), TOPE_CLAUDE_CODE)
  expect_match(contexto(r), "Resumido", fixed = TRUE)
  expect_match(contexto(r), "RESUMEN FINAL DEL ARSENAL", fixed = TRUE)
  expect_match(r$json$reason, "3000 error\\(es\\)")
  expect_lte(largo_utf16(r$json$systemMessage), TOPE_CLAUDE_CODE)
})

test_that("la FASE 2G informa el N que corre (regla #23)", {
  hook <- readLines(HOOK, warn = FALSE, encoding = "UTF-8")
  expect_false(any(grepl("20/20|20 semillas", hook)),
               info = "La 2G corre --n 100 y decía «20/20 semillas aprobadas»")
  # v6.3: los validadores reciben la ruta absoluta y canónica (RMD_ABS), no la que vino en el comando.
  expect_true(any(grepl("\"$SCRIPT_MULTISEMILLA\" \"$RMD_ABS\" --n 100", hook, fixed = TRUE)))
  expect_true(any(grepl("100/100 semillas aprobadas", hook, fixed = TRUE)))
})
