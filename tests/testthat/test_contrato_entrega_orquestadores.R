# =============================================================================
# Test de regresión — Contrato de entrega de los DOS orquestadores
# Gemelo de `test_contrato_detractor.R`, para `orquestador-schoice` y
# `orquestador-cloze`.
# =============================================================================
# Origen: el incidente 2026-08-09 (`excedente-almuerzo-proporcional-n4`) hizo
# que la v3.20.2 cableara contrato de entrega + marcador verificable en
# `agente-detractor.md`. La defensa NO se propagó a los orquestadores, así que
# el mismo modo de fallo siguió abierto en ellos — y volvió a ocurrir el
# 2026-08-10: un dry-run de `MAT-2026-1-010` terminó dos veces en notificación
# de "disponible" sin reporte. En dry-run el daño es total, porque el plan
# auditado es el ÚNICO producto: no deja rastro en disco con el que reconstruir
# qué se comprobó.
#
# Una lección aplicada a un solo agente no es una defensa: es una anécdota.
# Este test fija la paridad entre los tres agentes de reporte.
#
# El test es ESTÁTICO: verifica que las defensas siguen escritas donde deben.
# No puede comprobar que un subagente las obedezca en runtime; para eso está el
# marcador, que sí es verificable por quien invoca.
#
# NOTA DE DISEÑO — el detector es una FUNCIÓN de la ruta (`verificar_contrato`),
# no un bloque que lea rutas fijas. Eso permite verificarlo por mutación sobre
# un fixture en `tempdir()` sin tocar los archivos reales del repo. Mutar el
# archivo real y restaurarlo después dejó una vez el arsenal roto en disco
# durante minutos (sesión 2026-08-09, `validar_multisemilla.R`).

library(testthat)

# --- Raíz del repo (robusto desde cualquier cwd) ---
repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE)
  if (length(r) == 1 && dir.exists(r)) r else getwd()
}, error = function(e) getwd())

AGENTS_DIR <- file.path(repo_root, ".claude", "agents")

# El marcador es el contrato entre el agente y quien lo invoca. Los DOS
# orquestadores usan el mismo (son gemelos y quien invoca no debería tener que
# recordar cuál es cuál); el detractor usa el suyo, distinto a propósito,
# porque su vocabulario de veredicto es otro.
MARCADOR_ORQ <- "VEREDICTO_ORQUESTADOR:"
MARCADOR_DET <- "VEREDICTO_DETRACTOR:"

# La cláusula que extiende el contrato al modo dry-run, literal.
FRASE_DRY_RUN <- 'también aplica en `modo: "dry-run"`'

ORQUESTADORES <- c("orquestador-schoice", "orquestador-cloze")

leer <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

extraer_valores <- function(txt, patron) {
  m <- regmatches(txt, regexpr(patron, txt, perl = TRUE))
  if (length(m) != 1) return(character(0))
  crudo <- sub(patron, "\\1", m, perl = TRUE)
  sort(trimws(strsplit(crudo, "|", fixed = TRUE)[[1]]))
}

# --- DETECTOR (parametrizado: mutación-testeable sobre un fixture) ----------
verificar_contrato <- function(path) {
  if (!file.exists(path)) return(list(existe = FALSE))
  txt <- leer(path)
  lineas <- readLines(path, warn = FALSE)

  fin <- which(lineas == "---")
  frontmatter <- if (length(fin) >= 2) lineas[(fin[1] + 1):(fin[2] - 1)] else character(0)

  # Vocabulario declarado en el marcador vs. en el campo exit_status.
  vals_marcador <- extraer_valores(txt, "VEREDICTO_ORQUESTADOR:\\s*([^\\n]+)")
  vals_exit     <- extraer_valores(txt, "\"exit_status\":\\s*\"([^\"]+)\"")

  list(
    existe            = TRUE,
    tiene_seccion     = grepl("## Contrato de entrega", txt, fixed = TRUE),
    tiene_marcador    = grepl(MARCADOR_ORQ, txt, fixed = TRUE),
    prohibe_silencio  = grepl("NUNCA[^\n]*termino mi turno sin", txt),
    # Comparación LITERAL (fixed=TRUE): la frase lleva backticks y asteriscos de
    # Markdown, que como regex son operadores de repetición inválidos. Escribirla
    # como patrón costó un falso rojo la primera vez que se ejecutó este test.
    cubre_dry_run     = grepl(FRASE_DRY_RUN, txt, fixed = TRUE),
    declara_maxturns  = any(grepl("^maxTurns:\\s*[0-9]+\\s*$", frontmatter)),
    vals_marcador     = vals_marcador,
    vals_exit         = vals_exit,
    vocabulario_ok    = length(vals_marcador) > 0 &&
                        identical(vals_marcador, vals_exit)
  )
}

# --- DETECTOR del pre-flight 23 con carpeta padre (orquestador-cloze) --------
# El check 23 y la «Defensa preventiva» del Incidente O son párrafos de una sola
# línea en el agente: se localizan por su comienzo. Si algún día se parten en
# varias líneas, el control negativo del test lo avisa.
MARCA_CHECK23   <- "23. **Reglas locales del subproyecto**"
MARCA_DEFENSA_O <- "**Defensa preventiva**: pre-flight check 23"
MARCA_PADRE_23  <- " **Carpeta padre (ejercicio hermano"
MARCA_PADRE_O   <- " Si el destino es la carpeta de un ejercicio hermano"

verificar_check23_padre <- function(path) {
  if (!file.exists(path)) return(list(existe = FALSE))
  lineas  <- readLines(path, warn = FALSE)
  check23 <- lineas[startsWith(lineas, MARCA_CHECK23)]
  defensa <- lineas[startsWith(lineas, MARCA_DEFENSA_O)]
  check23 <- if (length(check23) == 1) check23 else ""
  defensa <- if (length(defensa) == 1) defensa else ""
  list(
    existe         = TRUE,
    hay_check23    = nzchar(check23),
    hay_defensa    = nzchar(defensa),
    padre_claude   = grepl("../.claude/CLAUDE.md", check23, fixed = TRUE),
    padre_handoff  = grepl("../HANDOFF.md", check23, fixed = TRUE),
    padre_item     = grepl("../ITEM_OFICIAL.md", check23, fixed = TRUE),
    criterio_padre = grepl("ejercicio_state.json", check23, fixed = TRUE),
    defensa_padre  = grepl("../.claude/", defensa, fixed = TRUE)
  )
}

# =============================================================================
context("Contrato de entrega de los orquestadores (paridad con el detractor)")

for (agente in ORQUESTADORES) {
  path <- file.path(AGENTS_DIR, paste0(agente, ".md"))

  test_that(paste0(agente, ": el archivo existe"), {
    expect_true(file.exists(path),
                info = paste("Falta", path, "— invariante I-5 (10 agentes)"))
  })

  test_that(paste0(agente, ": declara el contrato de entrega"), {
    skip_if_not(file.exists(path))
    r <- verificar_contrato(path)

    expect_true(r$tiene_seccion,
      info = paste0("Falta la sección '## Contrato de entrega' en ", agente, ".md. ",
                    "Sin ella, un turno que acaba en silencio es indistinguible ",
                    "de uno que trabajó y no emitió."))

    expect_true(r$tiene_marcador,
      info = paste("El agente no declara el marcador", MARCADOR_ORQ,
                   "— sin él, quien invoca no puede distinguir mecánicamente",
                   "'reporte completo' de 'reporte truncado o ausente'"))

    expect_true(r$prohibe_silencio,
      info = "El agente no prohíbe explícitamente terminar el turno sin reporte")

    expect_true(r$cubre_dry_run,
      info = paste("El contrato no dice explícitamente que también aplica en",
                   "modo dry-run. Es justo el caso donde el silencio duele más:",
                   "el plan auditado es el único producto y no deja rastro en disco."))
  })

  test_that(paste0(agente, ": declara maxTurns en el frontmatter"), {
    skip_if_not(file.exists(path))
    expect_true(verificar_contrato(path)$declara_maxturns,
      info = paste(agente, "no declara maxTurns. Un agente de reporte sin",
                   "presupuesto declarado puede agotarse sin llegar a emitir."))
  })

  # El check que de verdad previene la deriva: el marcador y el `exit_status`
  # del contrato de salida son DOS sitios espejo del mismo vocabulario. Si uno
  # gana un valor y el otro no, quien invoca valida contra una lista incompleta
  # y un estado legítimo se lee como reporte corrupto.
  test_that(paste0(agente, ": marcador y exit_status comparten vocabulario"), {
    skip_if_not(file.exists(path))
    r <- verificar_contrato(path)
    expect_gt(length(r$vals_marcador), 0)
    expect_gt(length(r$vals_exit), 0)
    expect_identical(r$vals_marcador, r$vals_exit,
      info = paste0("Divergen los valores del marcador y de `exit_status` en ",
                    agente, ".md\n  marcador: ", paste(r$vals_marcador, collapse = ", "),
                    "\n  exit_status: ", paste(r$vals_exit, collapse = ", ")))
  })
}

# =============================================================================
context("Coherencia entre los tres agentes de reporte")

test_that("los dos orquestadores usan EL MISMO marcador", {
  paths <- file.path(AGENTS_DIR, paste0(ORQUESTADORES, ".md"))
  skip_if_not(all(file.exists(paths)))
  rs <- lapply(paths, verificar_contrato)
  expect_true(all(vapply(rs, function(r) r$tiene_marcador, logical(1))),
    info = "Los gemelos deben compartir marcador: quien invoca no debería recordar cuál es cuál")
  expect_identical(rs[[1]]$vals_marcador, rs[[2]]$vals_marcador,
    info = "SCHOICE y CLOZE declaran vocabularios distintos para el mismo marcador")
})

test_that("el detractor conserva su propio marcador, distinto", {
  det <- file.path(AGENTS_DIR, "agente-detractor.md")
  skip_if_not(file.exists(det))
  txt <- leer(det)
  expect_true(grepl(MARCADOR_DET, txt, fixed = TRUE),
    info = "El detractor perdió su marcador (regresión sobre la v3.20.2)")
  expect_false(grepl(MARCADOR_ORQ, txt, fixed = TRUE),
    info = paste("El detractor adoptó el marcador de los orquestadores.",
                 "Son vocabularios distintos (APROBAR/RECHAZAR vs completado/parcial/...):",
                 "unificarlos rompe la validación de quien invoca a cualquiera de los dos."))
})

# =============================================================================
context("El detector dispara (control positivo sobre fixture temporal)")

# Sin esto, un 'no hay defecto' es indistinguible de 'el patrón no coincide'.
# Se muta una COPIA en tempdir(), nunca el archivo real del repo.
test_that("verificar_contrato() detecta un contrato ausente o mutilado", {
  origen <- file.path(AGENTS_DIR, "orquestador-schoice.md")
  skip_if_not(file.exists(origen))

  fixture_dir <- file.path(tempdir(), "contrato-fixture")
  dir.create(fixture_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(fixture_dir, recursive = TRUE), add = TRUE)

  txt <- leer(origen)

  # Control negativo: la copia intacta pasa. Se comprueba sub-check por
  # sub-check y NO con un `&&` encadenado: la primera versión de este test usaba
  # un AND único y, al fallar, señaló al sub-check equivocado en el mensaje
  # (culpaba a `vocabulario_ok` cuando el roto era `cubre_dry_run`).
  sano <- file.path(fixture_dir, "sano.md")
  writeLines(txt, sano)
  r_sano <- verificar_contrato(sano)
  for (k in c("tiene_seccion", "tiene_marcador", "prohibe_silencio",
              "cubre_dry_run", "vocabulario_ok")) {
    expect_true(isTRUE(r_sano[[k]]),
      info = paste0("La copia INTACTA falla en `", k, "`: el detector está mal ",
                    "calibrado, no el archivo. Arregla el detector, no el agente."))
  }

  # Mutante 1 — sin sección de contrato (el estado previo a este fix).
  m1 <- file.path(fixture_dir, "m1.md")
  writeLines(sub("## Contrato de entrega", "## Notas sueltas", txt, fixed = TRUE), m1)
  expect_false(verificar_contrato(m1)$tiene_seccion,
    info = "MUTANTE 1 no cazado: el detector no ve la ausencia de la sección")

  # Mutante 2 — sin marcador final.
  m2 <- file.path(fixture_dir, "m2.md")
  writeLines(gsub(MARCADOR_ORQ, "Estado final:", txt, fixed = TRUE), m2)
  expect_false(verificar_contrato(m2)$tiene_marcador,
    info = "MUTANTE 2 no cazado: el detector no ve la ausencia del marcador")

  # Mutante 3 — vocabulario divergente entre marcador y exit_status. Es el
  # fallo silencioso: el contrato "está", pero valida contra otra lista.
  m3 <- file.path(fixture_dir, "m3.md")
  writeLines(sub("VEREDICTO_ORQUESTADOR: completado | parcial | abortado | dry_run | preflight_failed",
                 "VEREDICTO_ORQUESTADOR: ok | ko",
                 txt, fixed = TRUE), m3)
  r3 <- verificar_contrato(m3)
  expect_true(r3$tiene_marcador,
    info = "El mutante 3 debía CONSERVAR el marcador (si no, no prueba lo que dice probar)")
  expect_false(r3$vocabulario_ok,
    info = "MUTANTE 3 no cazado: el detector no ve la divergencia de vocabulario")

  # Mutante 4 — contrato que no cubre dry-run. `fixed = TRUE` obligatorio: la
  # frase contiene ** y backticks (ver nota en FRASE_DRY_RUN).
  m4 <- file.path(fixture_dir, "m4.md")
  writeLines(sub(FRASE_DRY_RUN, "aplica solo en ejecución normal", txt, fixed = TRUE), m4)
  r4 <- verificar_contrato(m4)
  expect_true(r4$tiene_seccion,
    info = "El mutante 4 debía CONSERVAR la sección (si no, no prueba lo que dice probar)")
  expect_false(r4$cubre_dry_run,
    info = "MUTANTE 4 no cazado: el detector no ve que el contrato excluya dry-run")
})

# =============================================================================
context("Pre-flight 23 del orquestador-cloze: también la carpeta padre (2026-10-30)")

# Con `ruta_destino` = `<SUB>/cloze`, el check 23 solo miraba `<ruta_destino>/.claude/`
# y `<ruta_destino>/HANDOFF.md`, que en un CLOZE hermano no existen: las invariantes
# del SCHOICE y las firmas del CLOZE viven en `../.claude/CLAUDE.md` (convención de
# `probabilidad-fichas-tabla-doble-entrada-n2/cloze` y de
# `totales-promedios-reciclaje-n3/cloze`). El check pasaba sin haber leído ninguna
# invariante, y el coordinador lo suplía pegando esas rutas en el prompt.
test_that("orquestador-cloze: el check 23 lee también la carpeta padre", {
  path <- file.path(AGENTS_DIR, "orquestador-cloze.md")
  skip_if_not(file.exists(path))
  r <- verificar_check23_padre(path)
  for (k in c("hay_check23", "hay_defensa", "padre_claude", "padre_handoff",
              "padre_item", "criterio_padre", "defensa_padre")) {
    expect_true(isTRUE(r[[k]]),
      info = paste0("Pre-flight 23 del orquestador-cloze: falla `", k, "`. Con el destino ",
                    "en `<SUB>/cloze`, sin la carpeta padre el check pasa sin haber visto ",
                    "ninguna invariante del ejercicio hermano."))
  }
})

# Control positivo: se restituye el texto anterior al 2026-10-30 en una COPIA en
# tempdir() (se corta la cláusula de la carpeta padre de cada línea) y el detector
# tiene que verlo.
test_that("verificar_check23_padre() caza el check 23 sin la carpeta padre", {
  origen <- file.path(AGENTS_DIR, "orquestador-cloze.md")
  skip_if_not(file.exists(origen))

  fixture_dir <- file.path(tempdir(), "check23-fixture")
  dir.create(fixture_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(fixture_dir, recursive = TRUE), add = TRUE)

  lineas <- readLines(origen, warn = FALSE)

  # Control negativo, sub-check por sub-check (ver la nota del mutante 3 de arriba).
  sano <- file.path(fixture_dir, "sano.md")
  writeLines(lineas, sano)
  r_sano <- verificar_check23_padre(sano)
  for (k in c("hay_check23", "hay_defensa", "padre_claude", "padre_handoff",
              "padre_item", "criterio_padre", "defensa_padre")) {
    expect_true(isTRUE(r_sano[[k]]),
      info = paste0("La copia INTACTA falla en `", k, "`: el detector está mal ",
                    "calibrado, no el archivo. Arregla el detector, no el agente."))
  }

  i23 <- which(startsWith(lineas, MARCA_CHECK23))
  iO  <- which(startsWith(lineas, MARCA_DEFENSA_O))
  expect_length(i23, 1)
  expect_length(iO, 1)
  p23 <- regexpr(MARCA_PADRE_23, lineas[i23], fixed = TRUE)
  pO  <- regexpr(MARCA_PADRE_O, lineas[iO], fixed = TRUE)
  expect_true(p23 > 0 && pO > 0,
    info = "El mutante no encuentra las cláusulas de la carpeta padre: no prueba lo que dice probar")

  m <- lineas
  m[i23] <- substr(m[i23], 1, p23 - 1)
  m[iO]  <- substr(m[iO], 1, pO - 1)
  mut <- file.path(fixture_dir, "mutante.md")
  writeLines(m, mut)
  r_m <- verificar_check23_padre(mut)

  expect_true(r_m$hay_check23 && r_m$hay_defensa,
    info = "El mutante debía CONSERVAR el check 23 y la defensa (si no, no prueba lo que dice probar)")
  expect_false(r_m$padre_claude,
    info = "MUTANTE no cazado: el detector no ve que el check 23 ignore `../.claude/CLAUDE.md`")
  expect_false(r_m$padre_handoff,
    info = "MUTANTE no cazado: el detector no ve que el check 23 ignore `../HANDOFF.md`")
  expect_false(r_m$defensa_padre,
    info = "MUTANTE no cazado: el detector no ve la defensa del Incidente O sin la carpeta padre")
})
