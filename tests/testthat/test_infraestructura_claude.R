# Tests de Infraestructura .claude/ — Regla #17 (infraestructura-protegida.md)
# Cobertura: 8 invariantes I-1 a I-8
# Versión: 1.1
# Fecha: 2026-07-01
#
# Estos tests verifican que la convivencia Ruflo+ICFES sigue intacta.
# Si fallan, la sesión Ruflo del 2026-04-25 ha vuelto a romper el ecosistema.
# Ver: .claude/rules/infraestructura-protegida.md
#      .claude/docs/ADR/001-convivencia-ruflo-icfes.md

library(testthat)
library(jsonlite)

# Resolver la raíz del repo (los tests pueden correr desde cualquier cwd)
repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE),
  error = function(e) getwd()
)
if (length(repo_root) == 1 && nzchar(repo_root)) setwd(repo_root)

# ============================================================
# I-1: Identidad ICFES en CLAUDE.md raíz
# ============================================================

test_that("I-1: CLAUDE.md raíz identifica el repo como ICFES", {
  ruta <- "CLAUDE.md"
  expect_true(file.exists(ruta), info = "CLAUDE.md raíz no existe")

  # Primera línea no vacía debe identificar el repo
  lineas <- readLines(ruta, n = 50)
  primera_no_vacia <- lineas[which(nchar(trimws(lineas)) > 0)[1]]
  expect_match(primera_no_vacia,
               "(?i)(ICFES|Repositorio ICFES)",
               info = paste("Primera línea no es ICFES:", primera_no_vacia))
})

# ============================================================
# I-2: Índice de 17 reglas en .claude/CLAUDE.md
# ============================================================

test_that("I-2: .claude/CLAUDE.md tiene índice ICFES con >= 17 reglas", {
  ruta <- ".claude/CLAUDE.md"
  expect_true(file.exists(ruta), info = ".claude/CLAUDE.md no existe")

  # Lectura robusta a encoding/locale: leer bytes y partir por líneas.
  # readLines() sobre este archivo (tildes/emojis) devolvía 0 líneas en
  # locales como es_CO.UTF-8 → falso FAIL histórico de I-2 (2026-07-01).
  raw_txt <- readChar(ruta, file.info(ruta)$size, useBytes = TRUE)
  contenido <- strsplit(raw_txt, "\n", fixed = TRUE)[[1]]
  # Buscar entradas numeradas tipo "1. **...**" — patrón del índice
  reglas <- grep("^[0-9]{1,2}\\. \\*\\*", contenido, value = TRUE, useBytes = TRUE)
  expect_gte(length(reglas), 17,
             label = paste("Reglas en índice:", length(reglas)))

  # Identidad ICFES debe estar presente (subcadena ASCII, byte-segura)
  identidad <- grepl("Sistema de Generaci", raw_txt, fixed = TRUE, useBytes = TRUE)
  expect_true(identidad,
              info = ".claude/CLAUDE.md no contiene identidad ICFES")
})

# ============================================================
# I-3: Hooks ICFES enganchados en settings.json
# ============================================================

test_that("I-3: settings.json carga los hooks ICFES (gate + post-exams2)", {
  ruta <- ".claude/settings.json"
  expect_true(file.exists(ruta), info = "settings.json no existe")

  s <- fromJSON(ruta, simplifyVector = FALSE)
  expect_true("hooks" %in% names(s), info = "settings.json sin clave 'hooks'")

  # PreToolUse Write|Edit|MultiEdit debe incluir pre-write-rmd-gate.sh
  pre_we <- character(0)
  for (matcher in s$hooks$PreToolUse) {
    if (matcher$matcher %in% c("Write|Edit|MultiEdit", "Write|Edit")) {
      for (h in matcher$hooks) {
        pre_we <- c(pre_we, h$command)
      }
    }
  }
  expect_true(any(grepl("pre-write-rmd-gate\\.sh", pre_we)),
              info = "Gate ICFES desconectado del PreToolUse Write|Edit")

  # PostToolUse Bash debe incluir post-exams2-validation.sh
  post_b <- character(0)
  for (matcher in s$hooks$PostToolUse) {
    if (matcher$matcher == "Bash") {
      for (h in matcher$hooks) {
        post_b <- c(post_b, h$command)
      }
    }
  }
  expect_true(any(grepl("post-exams2-validation\\.sh", post_b)),
              info = "Hook post-exams2 ICFES desconectado del PostToolUse Bash")
})

# ============================================================
# I-4: Reglas ICFES presentes y no-vacías
# ============================================================

test_that("I-4: las 17 reglas ICFES existen como archivos no vacíos", {
  reglas_esperadas <- c(
    "ciclo-validacion.md",
    "codigo-rmd.md",
    "contextos-narrativos-creativos.md",
    "detractor-obligatorio.md",
    "documentacion-verificada.md",
    "ejercicios-metacognitivos.md",
    "flujo-b-obligatorio.md",
    "graficador-secuencial.md",
    "graficos-como-opciones.md",
    "infraestructura-protegida.md",   # regla #17
    "modelo-routing-obligatorio.md",
    "ortografia-espanol.md",
    "testing-obligatorio.md",
    "validacion-correctitud-respuesta.md",
    "validacion-neg-opciones-repetidas.md",
    "workflow-state-enforcement.md"
  )

  for (regla in reglas_esperadas) {
    ruta <- file.path(".claude/rules", regla)
    expect_true(file.exists(ruta),
                info = paste("Regla faltante:", regla))
    expect_gt(file.info(ruta)$size, 100,
              label = paste("Regla vacía o muy corta:", regla))
  }
})

# ============================================================
# I-5: Agentes ICFES presentes
# ============================================================

test_that("I-5: los 10 agentes ICFES existen", {
  agentes_esperados <- c(
    "clasificador-icfes.md",
    "pedagogo-icfes.md",
    "agente-detractor.md",
    "validador-visual.md",
    "diagnosticador-errores.md",
    "corrector-coherencia.md",
    "adversario.md",
    "orquestador-schoice.md",  # nuevo, sesión 2026-05-03
    "orquestador-cloze.md",    # nuevo, sesión 2026-06-03 (v3.15.0)
    "auditor-visual-html.md"   # nuevo, sesión 2026-06-05 (auditor visual masivo HTML)
  )

  for (agente in agentes_esperados) {
    ruta <- file.path(".claude/agents", agente)
    expect_true(file.exists(ruta),
                info = paste("Agente ICFES faltante:", agente))
  }
})

# ============================================================
# I-6: Hooks ejecutables y sintaxis válida
# ============================================================

test_that("I-6: hooks .sh son ejecutables y tienen sintaxis bash válida", {
  hooks <- c(
    ".claude/hooks/pre-write-rmd-gate.sh",
    ".claude/hooks/post-exams2-validation.sh",
    ".claude/hooks/pre-commit-ortografia.sh"
  )

  for (h in hooks) {
    expect_true(file.exists(h),
                info = paste("Hook faltante:", h))
    # Permisos ejecutables (mode includes any +x bit)
    perms <- file.info(h)$mode
    is_executable <- as.integer(perms) %% 2 == 1 ||
                     bitwAnd(as.integer(perms), as.integer("0111", 8L)) != 0
    expect_true(file.access(h, mode = 1) == 0,
                info = paste("Hook NO ejecutable:", h))

    # Sintaxis bash
    sintaxis_ok <- system2("bash", c("-n", h),
                           stdout = FALSE, stderr = FALSE)
    expect_equal(sintaxis_ok, 0,
                 info = paste("Hook con sintaxis inválida:", h))
  }
})

# ============================================================
# I-7: Backup pre-Ruflo preservado
# ============================================================

test_that("I-7: backup pre-Ruflo existe (red de seguridad)", {
  backup <- ".claude.pre-ruflo-20260425-123652.tar.gz"
  expect_true(file.exists(backup),
              info = paste("Backup faltante:", backup,
                           "— sin él no se puede restaurar el estado pre-Ruflo"))
  expect_gt(file.info(backup)$size, 100000,
            label = "Backup demasiado pequeño (¿corrupto?)")
})

# ============================================================
# I-8: Integridad de helpers Ruflo (autoejecutados en cada tool-use)
# ============================================================

test_that("I-8: helpers Ruflo .cjs coinciden con hashes de referencia", {
  helpers <- c(".claude/helpers/hook-handler.cjs",
               ".claude/helpers/intelligence.cjs",
               ".claude/helpers/statusline.cjs")
  if (!all(file.exists(helpers))) {
    skip("Helpers Ruflo no presentes (Ruflo no instalado en este entorno)")
  }
  ref <- "tests/testthat/ruflo-helpers.sha256"
  expect_true(file.exists(ref),
              info = "Falta el fichero de hashes de referencia de los helpers Ruflo")

  # sha256sum -c resuelve rutas relativas a la raíz del repo (cwd ya = repo_root)
  res <- system2("sha256sum", c("-c", "--status", ref),
                 stdout = FALSE, stderr = FALSE)
  expect_equal(res, 0L,
    info = paste("Un helper Ruflo .cjs cambió respecto al hash de referencia.",
                 "Auditar el diff (red/exec/env) ANTES de aceptar; si es benigno,",
                 "regenerar: sha256sum .claude/helpers/*.cjs > tests/testthat/ruflo-helpers.sha256"))
})

# ============================================================
# Test extra: ADR y docs de la sesión Ruflo presentes
# ============================================================

test_that("EXTRA: ADR-001 y INDICE_LECCIONES.md existen tras consolidación", {
  expect_true(file.exists(".claude/docs/ADR/001-convivencia-ruflo-icfes.md"),
              info = "ADR-001 faltante (regla #17 sin decisión documentada)")

  expect_true(file.exists(".claude/docs/INDICE_LECCIONES.md"),
              info = "INDICE_LECCIONES.md faltante (mapa de lecciones)")

  expect_true(file.exists(".claude/docs/MANUAL_USUARIO.md"),
              info = "MANUAL_USUARIO.md faltante")

  expect_true(file.exists(".claude/docs/templates/retrospectiva-sesion.md"),
              info = "Template de retrospectiva faltante")
})

test_that("EXTRA: patrones-errores-conocidos.md tiene Errores 11-15 (sesión Ruflo)", {
  ruta <- ".claude/docs/patrones-errores-conocidos.md"
  expect_true(file.exists(ruta))
  # Lectura byte-robusta (mismo motivo que I-2: readLines corrompe UTF-8 en locale C)
  contenido <- readChar(ruta, file.info(ruta)$size, useBytes = TRUE)
  for (n in 11:15) {
    expect_match(contenido,
                 paste0("Error ", n, ":"),
                 fixed = TRUE, useBytes = TRUE,
                 info = paste("Falta documentar Error", n,
                              "(lecciones sesión Ruflo 2026-05-03)"))
  }
})
