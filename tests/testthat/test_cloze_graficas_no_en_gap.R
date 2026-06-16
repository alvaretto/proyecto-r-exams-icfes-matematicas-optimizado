# =============================================================================
# Test de regresión — Validación CLOZE V5 / Incidente G
# Gráficas-opción NUNCA dentro de un gap CLOZE (regla graficos-como-opciones.md)
# =============================================================================
# En Moodle un gap CLOZE ({1:MULTICHOICE:...}) renderiza sus opciones como texto
# plano y DESCARTA el HTML -> las <img> de las opciones desaparecen ("no se ven
# los gráficos en el Paso N"). Por eso, en un CLOZE las gráficas-opción DEBEN ir
# ROTULADAS (I, II, III…) en el ENUNCIADO + opciones de TEXTO ("Gráfica I"…), y el
# Answerlist del enunciado (= opciones del gap) NO debe contener imágenes Markdown.
# Esto es distinto del SCHOICE puro, donde las imágenes-opción sí funcionan.
#
# Detector = espejo exacto del hook post-exams2-validation.sh FASE 2L:
#   - aplica solo a CLOZE (extype: cloze / exclozetype:)
#   - extrae el PRIMER Answerlist (el del enunciado, antes de ^Solution)
#   - marca violación si ese bloque contiene una imagen Markdown ![](...)
# Las imágenes legítimas del enunciado van ANTES del header "Answerlist", por lo
# que NO son falso positivo.
#
# Política (igual que test_markdown_tablas_none_guard.R / test_letter_independence.R):
# allowlist de violadores LEGACY conocidos; el test FALLA solo ante un violador NUEVO.

library(testthat)

# --- Raíz del repo (robusto desde cualquier cwd) ---
repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE, ignore.stderr = TRUE)
  if (length(r) >= 1 && dir.exists(r[1])) r[1] else NA_character_
}, error = function(e) NA_character_)
if (is.na(repo_root)) {
  cand <- c(getwd(), file.path(getwd(), "..", ".."))
  hit <- cand[file.exists(file.path(cand, ".claude", "rules", "graficos-como-opciones.md"))]
  repo_root <- if (length(hit)) normalizePath(hit[1]) else normalizePath(getwd())
}

# --- Detectores (espejo del hook FASE 2L) ---
es_cloze <- function(txt) {
  any(grepl("^extype:[[:space:]]*cloze", txt)) || any(grepl("^exclozetype:", txt))
}

# Answerlist del ENUNCIADO = primer bloque Answerlist, antes de la sección Solution.
answerlist_enunciado <- function(txt) {
  in_al <- FALSE; seen <- FALSE; out <- character(0)
  for (line in txt) {
    if (grepl("^Solution[[:space:]]*$", line)) { in_al <- FALSE; next }
    if (!seen && grepl("^Answerlist[[:space:]]*$", line)) { in_al <- TRUE; seen <- TRUE; next }
    if (in_al) out <- c(out, line)
  }
  out
}

tiene_img_markdown <- function(lines) {
  length(lines) > 0 && any(grepl("!\\[[^]]*\\]\\(", lines))
}

# Violación V5: CLOZE cuyo Answerlist de enunciado contiene una imagen Markdown.
viola_v5 <- function(txt) {
  es_cloze(txt) && tiene_img_markdown(answerlist_enunciado(txt))
}

# --- Excluir junk (no son deliverables): backups, copias, lab, fixtures ---
es_junk <- function(rel) {
  grepl("Ejemplos-Funcionales|/Lab-Manjaro/|/\\.backup_|Copia de|test_replica|/Avert\\.Rmd$", rel)
}

# --- Allowlist LEGACY (deuda preexistente). Vacío al 2026-06-15: el escaneo del
# repo no encontró ningún CLOZE con imágenes en el Answerlist del enunciado. ---
LEGACY_ALLOWLIST <- character(0)

# --- Recolectar .Rmd de producción/desarrollo ---
dirs <- file.path(repo_root, "A-Produccion",
                  c("01-En-PreDesarrollo", "02-En-Desarrollo", "03-En-Produccion"))
dirs <- dirs[dir.exists(dirs)]
rmd_files <- character(0)
for (d in dirs) {
  rmd_files <- c(rmd_files, list.files(d, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE))
}

rel_path <- function(f) sub("^/", "", sub(repo_root, "", f, fixed = TRUE))

test_that("Los detectores V5 funcionan (sanity: controles positivo y negativo)", {
  # Control POSITIVO: CLOZE con imágenes en el Answerlist del enunciado -> viola
  bad <- c("Question", "========", "**Parte 1.** ¿Cuál gráfico?", "##ANSWER1##",
           "Answerlist", "----------",
           "* ![](diagrama_a.png){width=70%}", "* ![](diagrama_b.png){width=70%}",
           "Solution", "========", "texto",
           "Meta-information", "================", "extype: cloze", "exclozetype: schoice")
  expect_true(es_cloze(bad))
  expect_true(viola_v5(bad))

  # Control NEGATIVO: imágenes solo en el ENUNCIADO (antes del Answerlist) +
  # opciones de TEXTO -> NO viola (es el patrón correcto, Incidente G).
  good <- c("Question", "========", "**Parte 1.** ¿Cuál gráfico?",
            "**Gráfica I:**", "![](diagrama_a.png){width=60%}",
            "**Gráfica II:**", "![](diagrama_b.png){width=60%}", "##ANSWER1##",
            "Answerlist", "----------", "* Gráfica I", "* Gráfica II",
            "Solution", "========", "texto",
            "Meta-information", "================", "extype: cloze", "exclozetype: schoice")
  expect_true(es_cloze(good))
  expect_false(viola_v5(good))

  # SCHOICE puro con imágenes-opción -> V5 no aplica (es legítimo en Moodle).
  schoice <- c("Question", "========", "Answerlist", "----------",
               "* ![](a.png){width=70%}", "Solution", "========",
               "Meta-information", "================", "extype: schoice")
  expect_false(es_cloze(schoice))
  expect_false(viola_v5(schoice))
})

test_that("El ejercicio piloto (grafica_funcion_lineal cloze v1) cumple V5", {
  f <- file.path(repo_root, "A-Produccion", "01-En-PreDesarrollo", "SAI2-PS-26",
                 "Cloze", "grafica_funcion_lineal_metacognitivo_interpretacion_n3_cloze_v1.Rmd")
  skip_if_not(file.exists(f), "ejercicio piloto no presente en este checkout")
  txt <- readLines(f, warn = FALSE)
  expect_true(es_cloze(txt))
  expect_false(viola_v5(txt),
    info = "El CLOZE piloto NO debe tener imágenes en el Answerlist del enunciado (Incidente G/V5)")
})

test_that("Ningún CLOZE NUEVO pone gráficas-opción dentro del gap (V5, Incidente G)", {
  nuevos <- character(0)
  for (f in rmd_files) {
    rel <- rel_path(f)
    if (es_junk(rel)) next
    txt <- readLines(f, warn = FALSE)
    if (viola_v5(txt)) {
      if (!(rel %in% LEGACY_ALLOWLIST)) nuevos <- c(nuevos, rel)
    }
  }
  expect_true(
    length(nuevos) == 0,
    info = paste0(
      "VIOLADOR(ES) NUEVO(S): CLOZE con imágenes Markdown en el Answerlist del ",
      "enunciado (las opciones del gap). En Moodle el gap NO renderiza <img> -> ",
      "el estudiante no ve los gráficos:\n  ",
      paste(nuevos, collapse = "\n  "),
      "\nFix (Incidente G, regla graficos-como-opciones.md): mover las gráficas ",
      "ROTULADAS al ENUNCIADO y dejar las opciones del gap como texto ('Gráfica I'…)."
    )
  )
})

test_that("El allowlist legacy V5 no contiene entradas obsoletas (ya corregidas)", {
  obsoletas <- character(0)
  for (rel in LEGACY_ALLOWLIST) {
    f <- file.path(repo_root, rel)
    if (!file.exists(f)) next
    txt <- readLines(f, warn = FALSE)
    if (!viola_v5(txt)) obsoletas <- c(obsoletas, rel)
  }
  if (length(obsoletas) > 0) {
    message("INFO: ", length(obsoletas),
            " entrada(s) del allowlist V5 ya cumplen; pueden retirarse:\n  ",
            paste(obsoletas, collapse = "\n  "))
  }
  expect_true(TRUE)
})
