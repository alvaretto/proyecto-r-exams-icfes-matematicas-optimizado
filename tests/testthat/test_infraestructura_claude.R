# Tests de Infraestructura .claude/ — Regla #17 (infraestructura-protegida.md)
# Cobertura: 9 invariantes I-1 a I-9
# Versión: 1.2
# Fecha: 2026-07-28
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
    ".claude/hooks/pre-commit-ortografia.sh",
    # Canónico versionado del hook git pre-push (2026-07-29). El que git ejecuta
    # es .git/hooks/pre-push, no versionable, que debe delegar aquí.
    ".claude/hooks/pre-push.sh"
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
# I-6b: contrato de stdin del pre-push (anti falso verde)
# ============================================================
# git entrega las refs del push por stdin y ese stdin se lee UNA SOLA VEZ.
# `git lfs pre-push` lo consume hasta EOF: si se le pasa el stdin real, el
# cálculo del rango del push queda ciego, R_TESTS_CHANGED_FILES sale vacío o
# parcial y el modo quick salta las suites del commit reportando
# "TODOS LOS TESTS PASARON / Cobertura 100%". Incidente 2026-07-29 (b2144d2d).

test_that("I-6b: pre-push.sh captura stdin antes de invocar git-lfs", {
  hook <- ".claude/hooks/pre-push.sh"
  expect_true(file.exists(hook), info = paste("Hook faltante:", hook))
  lineas <- readLines(hook, warn = FALSE)
  # Escanear SOLO código: el propio hook documenta la trampa en comentarios que
  # mencionan `git lfs pre-push`, y contarlos daría falsos positivos.
  es_codigo <- !grepl("^\\s*#", lineas)
  grep_codigo <- function(patron) {
    i <- grep(patron, lineas)
    i[es_codigo[i]]
  }

  # 1) Captura de stdin una sola vez
  idx_captura <- grep_codigo("PREPUSH_STDIN=\\$\\(cat")
  expect_gt(length(idx_captura), 0,
            label = "pre-push.sh sin captura de stdin (PREPUSH_STDIN=$(cat ...))")

  # 2) git-lfs alimentado desde la copia, NO del stdin real
  idx_lfs <- grep_codigo("git lfs pre-push")
  expect_gt(length(idx_lfs), 0, label = "pre-push.sh no invoca git lfs pre-push")
  for (i in idx_lfs) {
    expect_match(lineas[i], "PREPUSH_STDIN",
                 info = paste0("Línea ", i, ": `git lfs pre-push` recibe el stdin real; ",
                               "debe alimentarse de la copia capturada. ",
                               "Ver .claude/rules/testing-obligatorio.md §Contrato de stdin"))
  }

  # 3) La captura ocurre ANTES de git-lfs (si no, la copia llega vacía)
  if (length(idx_captura) > 0 && length(idx_lfs) > 0) {
    expect_lt(min(idx_captura), min(idx_lfs),
              label = "La captura de stdin debe preceder a `git lfs pre-push`")
  }

  # 4) El bucle de refs lee de la copia, no de stdin
  idx_while <- grep_codigo("^\\s*while\\s+read")
  expect_gt(length(idx_while), 0, label = "pre-push.sh sin bucle de parseo de refs")
  expect_gt(length(grep_codigo("done\\s*<<<\\s*\"?\\$PREPUSH_STDIN")), 0,
            label = "El bucle de refs debe alimentarse de $PREPUSH_STDIN (here-string)")

  # 5) Detección incompleta NO se exporta como confiable
  idx_export <- grep_codigo("export R_TESTS_CHANGED_FILES")
  expect_gt(length(idx_export), 0, label = "pre-push.sh no exporta R_TESTS_CHANGED_FILES")
  expect_true(any(grepl("REFS_PARSED", lineas[es_codigo])),
              info = paste("El export de R_TESTS_CHANGED_FILES debe estar condicionado a",
                           "haber parseado al menos una ref; una lista parcial es peor",
                           "que ninguna (el runner la trata como confiable)"))
})

test_that("I-6b: .git/hooks/pre-push delega en el canónico versionado", {
  wrapper <- ".git/hooks/pre-push"
  if (!file.exists(wrapper)) {
    skip("Wrapper .git/hooks/pre-push no instalado (clon nuevo: ver testing-obligatorio.md)")
  }
  contenido <- paste(readLines(wrapper, warn = FALSE), collapse = "\n")
  expect_match(contenido, "\\.claude/hooks/pre-push\\.sh",
               info = paste("El wrapper NO delega en .claude/hooks/pre-push.sh:",
                            "el hook corregido no se está aplicando aunque I-6 pase"))
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
# I-9: Herramientas de agentes en PascalCase válido (frontmatter tools:)
# ============================================================
#
# Origen: 2026-07-28 — 6 de 10 agentes .claude/agents/*.md declaraban
# `tools:` en minúscula (ej. [read, glob, grep, bash]). Claude Code no
# reconoce esos nombres y el agente se instancia SIN herramientas
# ("would be spawned with zero tools — refusing"), detectado al intentar
# lanzar AgenteDetractor. Fix: PascalCase canónico en los 6 archivos.
# Esta invariante evita que el drift vuelva a colarse silenciosamente.

test_that("I-9: 'tools:' en agentes .claude/agents/*.md usa nombres PascalCase válidos", {
  herramientas_validas <- c(
    "Read", "Write", "Edit", "MultiEdit", "Bash", "Grep", "Glob",
    "Task", "WebFetch", "WebSearch", "TodoWrite", "NotebookEdit"
  )

  archivos_agentes <- list.files(".claude/agents", pattern = "\\.md$", full.names = TRUE)
  expect_gt(length(archivos_agentes), 0,
            label = "No se encontraron agentes en .claude/agents/")

  for (archivo in archivos_agentes) {
    # Lectura byte-robusta (mismo motivo que I-2: readLines corrompe UTF-8 en locale C)
    raw_txt <- readChar(archivo, file.info(archivo)$size, useBytes = TRUE)
    lineas <- strsplit(raw_txt, "\n", fixed = TRUE)[[1]]

    linea_tools <- grep("^tools:", lineas, value = TRUE, useBytes = TRUE)
    if (length(linea_tools) == 0) next  # agente sin campo tools: en el frontmatter

    # Soporta ambos formatos vistos en el repo: "tools: [Read, Glob]" y
    # "tools: Read, Grep, Glob" (sin corchetes)
    valor <- sub("^tools:\\s*", "", linea_tools[1], useBytes = TRUE)
    valor <- gsub("[][]", "", valor, useBytes = TRUE)
    herramientas <- trimws(strsplit(valor, ",", fixed = TRUE)[[1]])
    herramientas <- herramientas[nzchar(herramientas)]

    expect_gt(length(herramientas), 0,
              label = paste("Agente con 'tools:' vacío o sin parsear:", basename(archivo)))

    for (h in herramientas) {
      # Debe empezar por mayúscula — detecta explícitamente el patrón "minúscula"
      # que causó el incidente, aunque el %in% de abajo ya lo cubriría por
      # sí solo (Claude Code no reconoce "read"/"glob"/etc en minúscula).
      expect_match(h, "^[A-Z]",
        info = paste0("Agente ", basename(archivo), ": herramienta '", h,
                      "' no empieza con mayúscula. Nombres en minúscula NO son ",
                      "reconocidos por Claude Code y el agente se instancia SIN ",
                      "herramientas. Ver .claude/rules/infraestructura-protegida.md (I-9)."))

      expect_true(h %in% herramientas_validas,
        info = paste0("Agente ", basename(archivo), " declara herramienta inválida '", h,
                      "' en 'tools:'. Debe ser una de: ",
                      paste(herramientas_validas, collapse = ", "),
                      ". Ver .claude/rules/infraestructura-protegida.md (I-9)."))
    }
  }
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
