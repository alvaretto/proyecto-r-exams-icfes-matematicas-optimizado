# =============================================================================
# Test de regresión — Citas verificables del detractor (Regla #9 v1.6)
# =============================================================================
# Origen: el 2026-06-16 un detractor reportó un «bug crítico» sobre código que
# reconstruyó de memoria: citó un campo, pools y un grupo que no existían en el
# .Rmd (memoria feedback_detractor_alucina_codigo). La defensa era que el
# coordinador revisara a mano. Desde la v1.6 la defensa es mecánica:
#   - el reporte trae el bloque CITAS_DETRACTOR … FIN_CITAS_DETRACTOR;
#   - .claude/scripts/verificar_citas_detractor.py contrasta cada cita con el
#     archivo real;
#   - como hook SubagentStop, no deja terminar al detractor mientras una cita
#     no coincida (tope de 3 bloqueos);
#   - detractor-hetero.sh no acepta un reporte cuyas citas fallan.
# Cada caso inventado de abajo DEBE fallar: si alguno pasa, el verificador dejó
# de proteger y el test lo pone en rojo.

library(testthat)

repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE)
  if (length(r) == 1 && dir.exists(r)) r else getwd()
}, error = function(e) getwd())

SCRIPT   <- file.path(repo_root, ".claude", "scripts", "verificar_citas_detractor.py")
FIXTURES <- file.path(repo_root, "tests", "testthat", "fixtures", "citas_detractor")
PY       <- Sys.which("python3")

correr <- function(args, input = NULL, env = character()) {
  old <- setwd(repo_root)
  on.exit(setwd(old))
  out <- suppressWarnings(system2(PY, c("-I", shQuote(SCRIPT), args), stdout = TRUE, stderr = TRUE,
                                  input = input, env = env))
  list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"),
       texto = paste(out, collapse = "\n"))
}
reporte <- function(nombre) file.path(FIXTURES, paste0("reporte_", nombre, ".md"))

# =============================================================================
context("verificar_citas_detractor.py — reportes")

test_that("el verificador y los fixtures existen", {
  expect_true(nzchar(PY), info = "python3 no está en el PATH")
  expect_true(file.exists(SCRIPT))
  expect_true(file.exists(file.path(FIXTURES, "fuente.js")))
})

test_that("un reporte con citas reales verifica (incluye rango, entidad HTML y AUSENTE verdadero)", {
  r <- correr(reporte("ok"))
  expect_equal(r$status, 0L, info = r$texto)
  expect_match(r$texto, "CITAS OK: 4 cita")
})

test_that("sin hallazgos, el bloque con NINGUNA verifica", {
  r <- correr(reporte("sin_hallazgos"))
  expect_equal(r$status, 0L, info = r$texto)
})

casos_que_fallan <- list(
  inventada     = "el fragmento NO está en el archivo",
  linea         = "sí aparece desde la\\(s\\) línea\\(s\\) 4",
  ausente_falso = "se afirma AUSENTE",
  sin_cita      = "el hallazgo H4 no tiene ninguna cita",
  sin_bloque    = "falta el bloque CITAS_DETRACTOR",
  archivo       = "el archivo no existe"
)
for (caso in names(casos_que_fallan)) {
  local({
    nombre <- caso
    esperado <- casos_que_fallan[[caso]]
    test_that(paste0("caso inventado «", nombre, "» falla con su motivo"), {
      r <- correr(reporte(nombre))
      expect_equal(r$status, 1L, info = r$texto)
      expect_match(r$texto, esperado, info = r$texto)
    })
  })
}

# =============================================================================
context("verificar_citas_detractor.py --hook (SubagentStop)")

entrada_hook <- function(archivo, agente) {
  jsonlite::toJSON(list(hook_event_name = "SubagentStop", agent_id = agente,
                        agent_type = "AgenteDetractor", cwd = repo_root, stop_hook_active = FALSE,
                        last_assistant_message = paste(readLines(archivo, warn = FALSE), collapse = "\n")),
                   auto_unbox = TRUE)
}

test_that("con citas inventadas bloquea 3 veces y luego deja terminar, registrándolo", {
  skip_if_not_installed("jsonlite")
  estado <- tempfile("citas-estado-")
  dir.create(estado)
  # Ventana 0: cada invocación cuenta (simula reenvíos separados en el tiempo).
  env <- c(paste0("CITAS_DETRACTOR_ESTADO=", estado), "CITAS_DETRACTOR_VENTANA=0")
  for (i in 1:3) {
    r <- correr("--hook", input = entrada_hook(reporte("inventada"), "agente-test"), env = env)
    expect_equal(r$status, 0L)
    d <- jsonlite::fromJSON(r$texto)
    expect_equal(d$decision, "block", info = paste("intento", i))
    expect_match(d$reason, "NO está en el archivo")
    expect_match(d$reason, "CITAS_DETRACTOR")
  }
  r <- correr("--hook", input = entrada_hook(reporte("inventada"), "agente-test"), env = env)
  expect_equal(r$texto, "", info = "tras el tope no debe bloquear")
  log <- paste(readLines(file.path(estado, "citas-detractor.log")), collapse = "\n")
  expect_match(log, "TOPE ALCANZADO")
})

test_that("dos copias del hook (repo y usuario) sobre el mismo mensaje evalúan una sola vez", {
  skip_if_not_installed("jsonlite")
  estado <- tempfile("citas-estado-")
  dir.create(estado)
  env <- paste0("CITAS_DETRACTOR_ESTADO=", estado)
  r1 <- correr("--hook", input = entrada_hook(reporte("inventada"), "agente-doble"), env = env)
  r2 <- correr("--hook", input = entrada_hook(reporte("inventada"), "agente-doble"), env = env)
  expect_equal(jsonlite::fromJSON(r1$texto)$decision, "block")
  expect_equal(r2$texto, "", info = "la segunda copia, en la misma ventana, no debe volver a bloquear")
  expect_equal(readLines(file.path(estado, "citas-detractor-agente-doble.count")), "1")
})

test_that("con citas reales no bloquea y registra la entrega verificada", {
  skip_if_not_installed("jsonlite")
  estado <- tempfile("citas-estado-")
  dir.create(estado)
  r <- correr("--hook", input = entrada_hook(reporte("ok"), "agente-ok"),
              env = paste0("CITAS_DETRACTOR_ESTADO=", estado))
  expect_equal(r$texto, "")
  expect_match(paste(readLines(file.path(estado, "citas-detractor.log")), collapse = "\n"),
               "ENTREGA VERIFICADA")
})

test_that("un subagente que no es detractor (sin marcador) no se toca", {
  r <- correr("--hook", input = '{"agent_id":"x","last_assistant_message":"Encontré 3 archivos.","cwd":"/tmp"}')
  expect_equal(r$status, 0L)
  expect_equal(r$texto, "")
})

test_that("una entrada ilegible no bloquea (falla abierto)", {
  r <- correr("--hook", input = "esto no es json")
  expect_equal(r$status, 0L)
  expect_equal(r$texto, "")
})

# --- Entrega por SubagentHandback (2026-11-02) ---------------------------------
# Los detractores r4 y r4b del SCHOICE de totales-promedios-reciclaje-n3 (2026-10-29)
# entregaron con la herramienta SubagentHandback: el reporte iba en input.message, el
# hook solo leía el texto final, salió con 0 y no dejó registro. La transcripción de
# abajo copia la forma de aquellos registros: un texto anterior y, al final, el handback.
transcripcion_handback <- function(archivo) {
  f <- tempfile("transcripcion-", fileext = ".jsonl")
  rep <- paste(readLines(archivo, warn = FALSE), collapse = "\n")
  regs <- list(
    list(type = "assistant", message = list(role = "assistant",
         content = list(list(type = "text", text = "Empiezo la auditoría.")))),
    list(type = "user", message = list(role = "user",
         content = list(list(type = "tool_result", tool_use_id = "t0", content = "ok")))),
    list(type = "assistant", message = list(role = "assistant",
         content = list(list(type = "tool_use", id = "t1", name = "SubagentHandback",
                             input = list(message = rep)))))
  )
  writeLines(vapply(regs, function(r) as.character(jsonlite::toJSON(r, auto_unbox = TRUE)), ""), f)
  f
}
entrada_handback <- function(transcripcion, agente) {
  jsonlite::toJSON(list(hook_event_name = "SubagentStop", agent_id = agente, agent_type = "AgenteDetractor",
                        cwd = repo_root, stop_hook_active = FALSE, last_assistant_message = "",
                        agent_transcript_path = transcripcion), auto_unbox = TRUE)
}

test_that("un reporte entregado con SubagentHandback también se evalúa", {
  skip_if_not_installed("jsonlite")
  estado <- tempfile("citas-estado-")
  dir.create(estado)
  env <- c(paste0("CITAS_DETRACTOR_ESTADO=", estado), "CITAS_DETRACTOR_VENTANA=0")
  r <- correr("--hook", input = entrada_handback(transcripcion_handback(reporte("inventada")), "agente-hb"), env = env)
  expect_equal(jsonlite::fromJSON(r$texto)$decision, "block", info = r$texto)
  r_ok <- correr("--hook", input = entrada_handback(transcripcion_handback(reporte("ok")), "agente-hb-ok"), env = env)
  expect_equal(r_ok$texto, "")
  expect_match(paste(readLines(file.path(estado, "citas-detractor.log")), collapse = "\n"),
               "agente=agente-hb-ok .*ENTREGA VERIFICADA")
})

# Mutante sobre una COPIA: sin la lectura del handback, el mismo reporte inventado pasa sin
# evaluarse y sin registro, como el 2026-10-29. Si el mutante bloqueara, el test de arriba no
# probaría la lectura del handback.
test_that("sin la lectura del handback, el reporte pasa sin evaluar (mutante)", {
  skip_if_not_installed("jsonlite")
  src <- readLines(SCRIPT, warn = FALSE)
  i <- grep("p.get('name') == 'SubagentHandback'", src, fixed = TRUE)
  expect_length(i, 1)
  mut <- src
  mut[i] <- sub("'SubagentHandback'", "'NoExiste'", mut[i], fixed = TRUE)
  dir_m <- tempfile("citas-mut-")
  dir.create(dir_m)
  script_m <- file.path(dir_m, "verificar_citas_detractor.py")
  writeLines(mut, script_m)
  estado <- tempfile("citas-estado-")
  dir.create(estado)
  old <- setwd(repo_root)
  on.exit(setwd(old), add = TRUE)
  out <- suppressWarnings(system2(PY, c("-I", shQuote(script_m), "--hook"), stdout = TRUE, stderr = TRUE,
    input = entrada_handback(transcripcion_handback(reporte("inventada")), "agente-mut"),
    env = c(paste0("CITAS_DETRACTOR_ESTADO=", estado), "CITAS_DETRACTOR_VENTANA=0")))
  expect_equal(paste(out, collapse = "\n"), "",
    info = "MUTANTE no aislado: sin la lectura del handback debía dejar pasar el reporte")
  expect_false(file.exists(file.path(estado, "citas-detractor.log")),
    info = "sin la lectura del handback el hook no debía registrar nada")
})

# =============================================================================
context("Cableado de la defensa (estático)")

leer <- function(...) paste(readLines(file.path(repo_root, ...), warn = FALSE), collapse = "\n")

test_that("settings.json engancha el verificador como hook SubagentStop", {
  skip_if_not_installed("jsonlite")
  s <- jsonlite::fromJSON(file.path(repo_root, ".claude", "settings.json"), simplifyVector = FALSE)
  cmds <- unlist(lapply(s$hooks$SubagentStop, function(g) vapply(g$hooks, function(h) h$command, "")))
  expect_true(any(grepl("verificar_citas_detractor.py\" --hook", cmds, fixed = TRUE)))
})

test_that("el agente y la regla documentan el bloque y el verificador", {
  agente <- leer(".claude", "agents", "agente-detractor.md")
  regla <- leer(".claude", "rules", "detractor-obligatorio.md")
  for (t in list(agente, regla)) {
    expect_match(t, "CITAS_DETRACTOR", fixed = TRUE)
    expect_match(t, "verificar_citas_detractor.py", fixed = TRUE)
  }
})

test_that("detractor-hetero.sh solo acepta reportes cuyas citas verifican", {
  hetero <- leer(".claude", "scripts", "detractor-hetero.sh")
  expect_match(hetero, "entregado\\(\\) \\{ con_marcador \"\\$1\" && citas_ok \"\\$1\"; \\}")
  expect_match(hetero, "verificar_citas_detractor.py", fixed = TRUE)
})
