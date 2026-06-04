# =============================================================================
# Test de regresión — Regla #20 / Error 21
# Guard del contador 'none' para tablas Markdown (pandoc >=3.7 / RStudio 3.8.3)
# =============================================================================
# Todo .Rmd con tabla Markdown (kable markdown o cat("| ...")) que renderice a
# PDF/NOPS DEBE incluir, al inicio de Question, el guard:
#   ```{=latex}
#   \makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother
#   ```
# Sin él, pandoc >=3.7 emite \def\LTcaptype{none} y exams2pdf/exams2nops fallan
# con "! LaTeX Error: No counter 'none' defined." (Ver markdown-tablas-pandoc.md)
#
# Política (igual que test_letter_independence.R): se mantiene un allowlist de
# violadores LEGACY conocidos (deuda preexistente; 03-En-Produccion es inmutable).
# El test FALLA solo si aparece un violador NUEVO fuera del allowlist.

library(testthat)

# --- Raíz del repo (robusto desde cualquier cwd) ---
repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE, ignore.stderr = TRUE)
  if (length(r) >= 1 && dir.exists(r[1])) r[1] else NA_character_
}, error = function(e) NA_character_)
if (is.na(repo_root)) {
  cand <- c(getwd(), file.path(getwd(), "..", ".."))
  hit <- cand[file.exists(file.path(cand, ".claude", "rules", "markdown-tablas-pandoc.md"))]
  repo_root <- if (length(hit)) normalizePath(hit[1]) else normalizePath(getwd())
}

# --- Detectores (espejo del hook FASE 2K) ---
tiene_tabla_markdown <- function(txt) {
  any(grepl("kable\\(", txt)) ||
    any(grepl('cat\\("\\|', txt)) ||
    any(grepl('format[[:space:]]*=[[:space:]]*"markdown"', txt))
}
tiene_guard_none <- function(txt) {
  any(grepl("newcounter\\{none\\}", txt)) ||
    any(grepl("@ifundefined\\{c@none\\}", txt))
}

# --- Excluir junk (no son deliverables): backups, copias, lab, fixtures ---
es_junk <- function(rel) {
  grepl("Ejemplos-Funcionales|/Lab-Manjaro/|/\\.backup_|Copia de|test_replica|/Avert\\.Rmd$", rel)
}

# --- Allowlist LEGACY (deuda preexistente al 2026-06-03; regla #20). ---
# 03-En-Produccion es inmutable -> permanente. 01/02 -> action item de fix.
LEGACY_ALLOWLIST <- c(
  "A-Produccion/01-En-PreDesarrollo/Comparacion-Lineas-Temporales/Comparacion-Lineas-Temporales-Schoice/comparacion_lineas_temporales_metacognitivo_argumentacion_n3_schoice_v1.Rmd",
  "A-Produccion/01-En-PreDesarrollo/Comparacion-Lineas-Temporales/Lineas-Recursos-Cloze/lineas_recursos_metacognitivo_argumentacion_n3_cloze_v1.Rmd",
  "A-Produccion/01-En-PreDesarrollo/Lentes-radio/error_relativo_procedimiento_metacognitivo_interpretacion_n3_cloze_v1.Rmd",
  "A-Produccion/01-En-PreDesarrollo/Lentes-radio/error_relativo_procedimiento_metacognitivo_interpretacion_n3_schoice_v1.Rmd",
  "A-Produccion/01-En-PreDesarrollo/Lentes-radio/error_relativo_procedimiento_metacognitivo_interpretacion_n3_schoice_v2.Rmd",
  "A-Produccion/01-En-PreDesarrollo/migraciones-exteriores-lineas-n2/migraciones_exteriores_metacognitivo_interpretacion_n2_schoice_v1.Rmd",
  "A-Produccion/01-En-PreDesarrollo/Rango-Dispersion-Botellas-Agua/rango_dispersion_botellas_metacognitivo_argumentacion_n2_schoice_v1.Rmd",
  "A-Produccion/02-En-Desarrollo/comparar_medianas_3_grupos_aleatorio_argumentacion_n2_schoice_v1/comparar_medianas_3_grupos_aleatorio_argumentacion_n2_schoice_v1.Rmd",
  "A-Produccion/02-En-Desarrollo/diagrama_caja_estaturas_metacognitivo_interpretacion/50/diagrama_caja_estaturas_metacognitivo_argumentacion_n3_cloze_v1.Rmd",
  "A-Produccion/02-En-Desarrollo/diagrama_caja_estaturas_metacognitivo_interpretacion/50/diagrama_caja_estaturas_metacognitivo_interpretacion_n2_cloze_neg_v1.Rmd",
  "A-Produccion/02-En-Desarrollo/diagrama_caja_estaturas_metacognitivo_interpretacion/50/diagrama_caja_estaturas_metacognitivo_interpretacion_n2_schoice_neg_v1.Rmd",
  "A-Produccion/02-En-Desarrollo/diagrama_caja_estaturas_metacognitivo_interpretacion/50/diagrama_caja_estaturas_metacognitivo_interpretacion_n2_schoice_v1.Rmd",
  "A-Produccion/02-En-Desarrollo/pendiente_relacion_lineal_numerico_variacional_argumentacion_n2_schoice_v1/pendiente_relacion_lineal_numerico_variacional_argumentacion_n2_cloze_v1.Rmd",
  "A-Produccion/02-En-Desarrollo/pendiente_relacion_lineal_numerico_variacional_argumentacion_n2_schoice_v1/pendiente_relacion_lineal_numerico_variacional_argumentacion_n2_schoice_v1.Rmd",
  "A-Produccion/02-En-Desarrollo/proceso_recaudacion_sitio_turistico_numerico_variacional_argumentacion_n2_v1/proceso_recaudacion_sitio_turistico_numerico_variacional_argumentacion_n2_v1.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/04-Medidas-De-Tendencia-Central/Mediana/Baterías-Celulares/comparar_medianas_baterias_celulares_aleatorio_interpretacion_representacion_n2_schoice_v1.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/04-Medidas-De-Tendencia-Central/Mediana/Baterías-Celulares/comparar_medianas_baterias_celulares_aleatorio_interpretacion_representacion_n2_schoice_v2.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/04-Medidas-De-Tendencia-Central/Mediana/Baterías-Celulares/comparar_medianas_baterias_celulares_aleatorio_interpretacion_representacion_n2_schoice_v3.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/04-Medidas-De-Tendencia-Central/Mediana/Baterías-Celulares/comparar_medianas_baterias_celulares_aleatorio_interpretacion_representacion_n2_schoice_v4.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/04-Medidas-De-Tendencia-Central/Mediana/Baterías-Celulares/comparar_medianas_baterias_celulares_aleatorio_interpretacion_representacion_n2_schoice_v5.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/04-Medidas-De-Tendencia-Central/Mediana/Baterías-Celulares/comparar_medianas_baterias_celulares_aleatorio_interpretacion_representacion_n2_schoice_v6.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/04-Medidas-De-Tendencia-Central/Media/Promedios-Borrados/promedios_borrados_edades_aleatorio_formulacion_n2_cloze_v1.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd",
  "A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd"
)

# --- Recolectar .Rmd de producción/desarrollo ---
dirs <- file.path(repo_root, "A-Produccion",
                  c("01-En-PreDesarrollo", "02-En-Desarrollo", "03-En-Produccion"))
dirs <- dirs[dir.exists(dirs)]
rmd_files <- character(0)
for (d in dirs) {
  rmd_files <- c(rmd_files, list.files(d, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE))
}

rel_path <- function(f) sub("^/", "", sub(repo_root, "", f, fixed = TRUE))

test_that("Los detectores de tabla/guard funcionan (sanity)", {
  expect_true(tiene_tabla_markdown('cat(knitr::kable(df, format = "markdown"))'))
  expect_true(tiene_tabla_markdown('cat("| ", x, " |")'))
  expect_false(tiene_tabla_markdown("solo texto sin tabla"))
  expect_true(tiene_guard_none("\\makeatletter\\@ifundefined{c@none}{\\newcounter{none}}{}\\makeatother"))
  expect_true(tiene_guard_none("\\newcounter{none}"))
  expect_false(tiene_guard_none("texto sin guard"))
})

test_that("Ningún .Rmd NUEVO con tabla Markdown carece del guard 'none' (regla #20)", {
  nuevos <- character(0)
  for (f in rmd_files) {
    rel <- rel_path(f)
    if (es_junk(rel)) next
    txt <- readLines(f, warn = FALSE)
    if (tiene_tabla_markdown(txt) && !tiene_guard_none(txt)) {
      if (!(rel %in% LEGACY_ALLOWLIST)) nuevos <- c(nuevos, rel)
    }
  }
  expect_true(
    length(nuevos) == 0,
    info = paste0(
      "VIOLADOR(ES) NUEVO(S) con tabla Markdown sin guard \\newcounter{none} ",
      "(fallarán en RStudio/pandoc 3.8.3):\n  ",
      paste(nuevos, collapse = "\n  "),
      "\nFix: agregar el bloque {=latex} con \\@ifundefined{c@none}{\\newcounter{none}}{} ",
      "al inicio de Question. Ver .claude/rules/markdown-tablas-pandoc.md"
    )
  )
})

test_that("El allowlist legacy no contiene entradas obsoletas (ya corregidas)", {
  obsoletas <- character(0)
  for (rel in LEGACY_ALLOWLIST) {
    f <- file.path(repo_root, rel)
    if (!file.exists(f)) next  # archivo movido/borrado: no es bloqueante
    txt <- readLines(f, warn = FALSE)
    if (!(tiene_tabla_markdown(txt) && !tiene_guard_none(txt))) {
      obsoletas <- c(obsoletas, rel)
    }
  }
  # Informativo: si hay obsoletas, conviene depurarlas del allowlist (no bloqueante).
  if (length(obsoletas) > 0) {
    message("INFO: ", length(obsoletas),
            " entrada(s) del allowlist ya cumplen el guard; pueden retirarse:\n  ",
            paste(obsoletas, collapse = "\n  "))
  }
  expect_true(TRUE)
})
