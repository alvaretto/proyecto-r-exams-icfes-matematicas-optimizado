# test_letter_independence.R
# ============================================================
# Suite de regresión para Error 19 (sesión 2026-05-12).
#
# Error 19: Solution referencia opciones por letra/posición
#           (A/B/C/D) directamente. Moodle puede re-shufflear
#           independientemente de exshuffle:FALSE de R-exams,
#           rompiendo coherencia letra <-> contenido para el
#           estudiante en producción.
#
# Esta suite NO renderiza; hace análisis estático de la sección
# `Solution` de cada .Rmd buscando los patrones P1..P4 prohibidos
# por la regla #19 (.claude/rules/solution-letter-independence.md).
# ============================================================

library(testthat)

# Localizar la raíz del repo
.repo_root <- tryCatch({
  trimws(system("git rev-parse --show-toplevel", intern = TRUE))
}, error = function(e) getwd())

# Directorios a auditar
.dirs_auditar <- c(
  file.path(.repo_root, "A-Produccion", "01-En-PreDesarrollo"),
  file.path(.repo_root, "A-Produccion", "02-En-Desarrollo"),
  file.path(.repo_root, "A-Produccion", "03-En-Produccion")
)
.dirs_auditar <- .dirs_auditar[dir.exists(.dirs_auditar)]

# Exclusiones
.excluir_glob <- c(
  "Ejemplos-Funcionales-Rmd",
  "Lab-Manjaro",
  "_legacy_"
)

# Lista de .Rmd legacy conocidos con bug Error 19 al crear la regla (2026-05-12).
# Reportar como WARNING informativo, no como FAIL. Action item: fix uno por uno y
# eliminar de esta lista. Si un .Rmd NUEVO cae aquí (no listado), el test SÍ falla.
.legacy_known_letter_dep <- character(0)
# Historia:
# - 2026-05-12: lista creada con 8 .Rmd legacy (Error 19).
# - 2026-05-12 (commit b5f95e74): 3 versiones Venn en 03-En-Produccion fixeadas.
# - 2026-05-12 (este commit): los 5 restantes fixeados.
#   - Comparacion-Lineas-CLOZE/lineas_recursos_*
#   - migraciones-exteriores-lineas-n2/*
#   - Rango-Dispersion-Botellas-Agua/*
#   - pendiente_relacion_lineal_*/...cloze_v1
#   - probabilidad_condicional_*/seleccion_canciones_cd_diagrama_arbol_*
# Lista vacía: cualquier .Rmd nuevo con patrón Error 19 ahora hace FAIL bloqueante.

listar_rmd_letter <- function() {
  todos <- character(0)
  for (d in .dirs_auditar) {
    encontrados <- list.files(d, pattern = "\\.Rmd$",
                              recursive = TRUE, full.names = TRUE)
    todos <- c(todos, encontrados)
  }
  for (patron in .excluir_glob) {
    todos <- todos[!grepl(patron, todos, fixed = TRUE)]
  }
  todos
}

# Extraer la sección Solution de un .Rmd
extraer_solution <- function(ruta_rmd) {
  if (!file.exists(ruta_rmd)) return(list(lineas = character(0), offsets = integer(0)))
  lineas <- readLines(ruta_rmd, warn = FALSE)
  inicio <- grep("^Solution\\s*$", lineas)
  fin <- grep("^Meta-information\\s*$", lineas)
  if (length(inicio) == 0) return(list(lineas = character(0), offsets = integer(0)))
  i0 <- inicio[1]
  i1 <- if (length(fin) > 0) fin[1] - 1L else length(lineas)
  if (i1 < i0) return(list(lineas = character(0), offsets = integer(0)))
  list(
    lineas = lineas[i0:i1],
    offsets = i0:i1  # numeros de linea absolutos
  )
}

# ---------- Detector P1+P2: r letra_correcta / r letras[...] ----------

detectar_letra_interpolada <- function(ruta_rmd) {
  sol <- extraer_solution(ruta_rmd)
  if (length(sol$lineas) == 0) return(character(0))
  # Patrón: `r letra_correcta` o `r letras[...]` dentro de la sección
  patron <- "`r[[:space:]]+(letra_correcta|letras\\[)"
  matches <- grepl(patron, sol$lineas, perl = TRUE)
  if (!any(matches)) return(character(0))
  idx <- which(matches)
  sprintf("L%d: %s",
          sol$offsets[idx],
          trimws(substr(sol$lineas[idx], 1, 120)))
}

# ---------- Detector P3: cat("**Opcion ", l, ...) ----------

detectar_cat_opcion_letra <- function(ruta_rmd) {
  sol <- extraer_solution(ruta_rmd)
  if (length(sol$lineas) == 0) return(character(0))
  # Patron: cat con "**Opcion " seguido de variable l (loop var)
  patron <- "cat\\([^)]*\"\\*?\\*?Opci[oó]n[[:space:]]+\",[[:space:]]*l[[:space:],)]"
  matches <- grepl(patron, sol$lineas, perl = TRUE)
  if (!any(matches)) return(character(0))
  idx <- which(matches)
  sprintf("L%d: %s",
          sol$offsets[idx],
          trimws(substr(sol$lineas[idx], 1, 120)))
}

# ---------- Detector P4: "Opcion A/B/C/D" literal en Markdown ----------

detectar_opcion_literal <- function(ruta_rmd) {
  sol <- extraer_solution(ruta_rmd)
  if (length(sol$lineas) == 0) return(character(0))
  # Patron: "Opcion A" hasta "Opcion D" como texto Markdown (no en backticks)
  patron <- "(^|[^`])(\\*\\*)?Opci[oó]n[[:space:]]+[A-D]\\b"
  matches <- grepl(patron, sol$lineas, perl = TRUE)
  if (!any(matches)) return(character(0))
  # Filtrar: lineas dentro de chunks R que construyen prosa generica
  # (heuristica: contienen err$, descripcion_corta, descripcion_larga)
  idx <- which(matches)
  filtrar <- function(linea) {
    grepl("err\\$|descripcion_(corta|larga)|errores_conceptuales", linea)
  }
  idx <- idx[!sapply(sol$lineas[idx], filtrar)]
  if (length(idx) == 0) return(character(0))
  sprintf("L%d: %s",
          sol$offsets[idx],
          trimws(substr(sol$lineas[idx], 1, 120)))
}

# ---------- Tests ----------

context("Regla #19: Letter-independence en Solution (Error 19)")

test_that("P1+P2: Solution no interpola `r letra_correcta` ni `r letras[...]`", {
  rmds <- listar_rmd_letter()
  if (length(rmds) == 0) skip("Sin .Rmd auditables")

  todos_hits <- list()
  for (rmd in rmds) {
    hits <- detectar_letra_interpolada(rmd)
    if (length(hits) > 0) {
      rel <- sub(paste0(.repo_root, "/"), "", rmd, fixed = TRUE)
      if (any(sapply(.legacy_known_letter_dep, function(legacy) grepl(legacy, rel, fixed = TRUE)))) {
        message("INFO: .Rmd legacy con Error 19 conocido: ", rel)
      } else {
        todos_hits[[rel]] <- hits
      }
    }
  }

  if (length(todos_hits) > 0) {
    msg <- paste(
      sprintf("\n  %s:\n    %s",
              names(todos_hits),
              sapply(todos_hits, function(h) paste(h, collapse = "\n    "))),
      collapse = "\n")
    fail(sprintf("Solution con `r letra_correcta` o `r letras[...]` (regla #19):%s", msg))
  } else {
    succeed("Ningún .Rmd interpola letra_correcta en Solution")
  }
})

test_that("P3: chunk R no emite 'Opcion <letra>' con variable de loop", {
  rmds <- listar_rmd_letter()
  if (length(rmds) == 0) skip("Sin .Rmd auditables")

  todos_hits <- list()
  for (rmd in rmds) {
    hits <- detectar_cat_opcion_letra(rmd)
    if (length(hits) > 0) {
      rel <- sub(paste0(.repo_root, "/"), "", rmd, fixed = TRUE)
      if (any(sapply(.legacy_known_letter_dep, function(legacy) grepl(legacy, rel, fixed = TRUE)))) {
        message("INFO: .Rmd legacy con Error 19 conocido: ", rel)
      } else {
        todos_hits[[rel]] <- hits
      }
    }
  }

  if (length(todos_hits) > 0) {
    msg <- paste(
      sprintf("\n  %s:\n    %s",
              names(todos_hits),
              sapply(todos_hits, function(h) paste(h, collapse = "\n    "))),
      collapse = "\n")
    fail(sprintf("Chunk R emite 'Opción <letra>' (regla #19, P3):%s", msg))
  } else {
    succeed("Ningún chunk R emite 'Opción ' con variable de letra")
  }
})

test_that("P4: Solution no menciona 'Opcion A/B/C/D' literal", {
  rmds <- listar_rmd_letter()
  if (length(rmds) == 0) skip("Sin .Rmd auditables")

  todos_hits <- list()
  for (rmd in rmds) {
    hits <- detectar_opcion_literal(rmd)
    if (length(hits) > 0) {
      rel <- sub(paste0(.repo_root, "/"), "", rmd, fixed = TRUE)
      if (any(sapply(.legacy_known_letter_dep, function(legacy) grepl(legacy, rel, fixed = TRUE)))) {
        message("INFO: .Rmd legacy con Error 19 conocido: ", rel)
      } else {
        todos_hits[[rel]] <- hits
      }
    }
  }

  if (length(todos_hits) > 0) {
    msg <- paste(
      sprintf("\n  %s:\n    %s",
              names(todos_hits),
              sapply(todos_hits, function(h) paste(h, collapse = "\n    "))),
      collapse = "\n")
    fail(sprintf("Solution menciona 'Opción [A-D]' literal (regla #19, P4):%s", msg))
  } else {
    succeed("Ningún .Rmd menciona 'Opción [A-D]' literal en Solution")
  }
})

# ---------- Self-test del detector ----------

test_that("Self-test: detectores reconocen patrones prohibidos en fixture artificial", {
  # Crear fixture en /tmp con todos los patrones (double quotes — patrón
  # real en .Rmd de producción, ver convención generar-schoice/cloze).
  fixture <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "Test",
    "",
    "Solution",
    "========",
    "",
    "### Respuesta correcta: Opción `r letra_correcta` {#test}",
    "",
    "La **Opción A** es la correcta.",
    "",
    "```{r}",
    'for (l in letras) cat("**Opción ", l, ": blah")',
    "```",
    "",
    "Meta-information",
    "================",
    "extype: schoice"
  ), fixture)

  expect_true(length(detectar_letra_interpolada(fixture)) > 0,
              info = "Debe detectar `r letra_correcta` en fixture")
  expect_true(length(detectar_cat_opcion_letra(fixture)) > 0,
              info = "Debe detectar cat(\"**Opción \", l) en fixture")
  expect_true(length(detectar_opcion_literal(fixture)) > 0,
              info = "Debe detectar 'Opción A' literal en fixture")

  unlink(fixture)
})
