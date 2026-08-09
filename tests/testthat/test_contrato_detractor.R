# =============================================================================
# Test de regresión — Contrato de entrega e independencia del detractor
# Regla #9 (`.claude/rules/detractor-obligatorio.md` v1.2)
# =============================================================================
# Origen: incidente 2026-08-09 (`excedente-almuerzo-proporcional-n4`). Tres
# invocaciones consecutivas de un agente detractor terminaron emitiendo sólo una
# notificación de "disponible", sin reporte. El coordinador acabó haciendo la
# revisión adversarial sobre código que él mismo había escrito — es decir, el
# sesgo de confirmación que el detractor existe para romper.
#
# Dos defensas quedaron cableadas y este test las fija:
#   (1) CONTRATO DE ENTREGA: el agente sabe que su texto final ES el reporte y
#       lo cierra con un marcador verificable mecánicamente.
#   (2) INDEPENDENCIA + PROTOCOLO DE NO-ENTREGA: la regla prohíbe sustituir el
#       detractor por la auditoría propia del coordinador y sellar la FASE 2C.
#
# El test es ESTÁTICO: verifica que las defensas siguen escritas donde deben.
# No puede comprobar que un subagente las obedezca en runtime; para eso está el
# marcador, que sí es verificable por quien invoca.

library(testthat)

# --- Raíz del repo (robusto desde cualquier cwd) ---
repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE)
  if (length(r) == 1 && dir.exists(r)) r else getwd()
}, error = function(e) getwd())

AGENTE <- file.path(repo_root, ".claude", "agents", "agente-detractor.md")
REGLA  <- file.path(repo_root, ".claude", "rules", "detractor-obligatorio.md")

# El marcador es el contrato entre el agente y quien lo invoca: si cambia en un
# sitio y no en el otro, la comprobación mecánica de entrega deja de funcionar
# en silencio. Por eso se verifica en AMBOS archivos.
MARCADOR <- "VEREDICTO_DETRACTOR:"

leer <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

# =============================================================================
context("Contrato de entrega del AgenteDetractor")

test_that("el archivo del agente detractor existe", {
  expect_true(file.exists(AGENTE),
              info = paste("Falta", AGENTE, "— invariante I-5 (10 agentes)"))
})

test_that("el agente declara el contrato de entrega", {
  skip_if_not(file.exists(AGENTE))
  txt <- leer(AGENTE)

  expect_true(grepl("Contrato de entrega", txt, fixed = TRUE),
    info = "Falta la sección '## Contrato de entrega' en agente-detractor.md")

  expect_true(grepl(MARCADOR, txt, fixed = TRUE),
    info = paste("El agente no declara el marcador", MARCADOR,
                 "— sin él, quien invoca no puede distinguir",
                 "'reporte completo' de 'reporte truncado o ausente'"))

  # La instrucción operativa que evita el fallo observado: no basta con
  # mencionar el reporte, hay que prohibir terminar sin él.
  expect_true(grepl("NUNCA[^\n]*termino mi turno sin", txt),
    info = "El agente no prohíbe explícitamente terminar el turno sin reporte")
})

test_that("el agente declara maxTurns en el frontmatter", {
  skip_if_not(file.exists(AGENTE))
  fm <- readLines(AGENTE, warn = FALSE)
  fin <- which(fm == "---")
  skip_if(length(fin) < 2, "frontmatter YAML no delimitado")
  frontmatter <- fm[(fin[1] + 1):(fin[2] - 1)]

  expect_true(any(grepl("^maxTurns:\\s*[0-9]+\\s*$", frontmatter)),
    info = paste("agente-detractor.md no declara maxTurns.",
                 "Un agente de reporte sin presupuesto declarado puede agotarse",
                 "sin llegar a emitir (el `adversario` global sí lo declara)."))
})

# =============================================================================
context("Independencia y protocolo de no-entrega (regla #9)")

test_that("la regla del detractor existe y está en v1.2 o superior", {
  expect_true(file.exists(REGLA), info = paste("Falta", REGLA))
  skip_if_not(file.exists(REGLA))
  txt <- leer(REGLA)
  v <- regmatches(txt, regexpr("\\*\\*Versi[oó]n\\*\\*:\\s*([0-9]+)\\.([0-9]+)", txt))
  expect_true(length(v) == 1, info = "No se pudo leer la versión de la regla")
  num <- as.numeric(sub(".*?([0-9]+)\\.([0-9]+).*", "\\1.\\2", v))
  expect_gte(num, 1.2)
})

test_that("la regla exige independencia del detractor", {
  skip_if_not(file.exists(REGLA))
  txt <- leer(REGLA)

  expect_true(grepl("Independencia del detractor", txt, fixed = TRUE),
    info = "Falta la sección '## Independencia del detractor'")

  expect_true(grepl("distinto del que escribi[oó] o corrigi[oó]", txt),
    info = paste("La regla no exige que el detractor sea un agente DISTINTO",
                 "del que escribió el artefacto — es el núcleo del incidente"))
})

test_that("la regla define el protocolo de no-entrega con el mismo marcador", {
  skip_if_not(file.exists(REGLA))
  txt <- leer(REGLA)

  expect_true(grepl("Protocolo de no-entrega", txt, fixed = TRUE),
    info = "Falta la sección '## Protocolo de no-entrega'")

  expect_true(grepl(MARCADOR, txt, fixed = TRUE),
    info = paste("La regla no menciona el marcador", MARCADOR,
                 "— agente y regla deben usar el MISMO, o la comprobación",
                 "mecánica de entrega se rompe en silencio"))
})

test_that("la regla prohíbe sellar FASE 2C con auditoría propia", {
  skip_if_not(file.exists(REGLA))
  txt <- leer(REGLA)

  # Las tres prohibiciones que cierran el agujero: sustituir, sellar, avanzar.
  expect_true(grepl("PROHIBIDO", txt, fixed = TRUE) &&
              grepl("detractor_fase2c", txt, fixed = TRUE),
    info = paste("La regla no prohíbe explícitamente sellar `detractor_fase2c`",
                 "cuando el detractor no entregó reporte"))

  expect_true(grepl("no independiente", txt, fixed = TRUE),
    info = paste("La regla no obliga a declarar la revisión propia como",
                 "NO INDEPENDIENTE cuando sustituye al detractor"))
})

# =============================================================================
context("Coherencia cruzada agente <-> regla")

test_that("agente y regla se referencian mutuamente", {
  skip_if_not(file.exists(AGENTE) && file.exists(REGLA))
  expect_true(grepl("detractor-obligatorio.md", leer(AGENTE), fixed = TRUE),
    info = "El agente no apunta a la regla que lo gobierna")
  expect_true(grepl("agente-detractor.md", leer(REGLA), fixed = TRUE),
    info = "La regla no apunta al agente")
})
