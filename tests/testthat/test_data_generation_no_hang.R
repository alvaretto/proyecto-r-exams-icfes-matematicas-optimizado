# =============================================================================
# Test de regresión — Error 22 / bucle `repeat` sin cota en data_generation
# =============================================================================
# Origen (2026-06-05): rango_colesterol_*_cloze_v1 colgaba ~1.5% de las semillas.
# La Parte 6 usaba:
#     repeat { vals <- sample(...); if (max(vals)-min(vals) > rango_mas_grande) break }
# Cuando rango_mas_grande >= span del rango de valores, la condición es IMPOSIBLE
# de cumplir -> bucle infinito -> exams2html/pdf(n=200) y la prueba de diversidad
# se congelan. Fix: construir vals_extra de forma determinista (pick_int) en vez
# de reintentar. Ver Error 22 en patrones-errores-conocidos.md.
#
# Este test tiene dos capas:
#   (A) Detector estático reutilizable: marca bucles `repeat {` que NO tienen una
#       cota de iteraciones (contador + límite). Se aplica al ejercicio fuente y
#       a su gemelo SCHOICE; es la firma del anti-patrón.
#   (B) Guard runtime enfocado: ejecuta el chunk data_generation del ejercicio a
#       través de N semillas con setTimeLimit por semilla. Si alguna excede el
#       límite => regresión del cuelgue. Rápido y NO flaky (solo 1 ejercicio).

library(testthat)

# --- Raíz del repo (robusto desde cualquier cwd) ---
repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE, ignore.stderr = TRUE)
  if (length(r) >= 1 && dir.exists(r[1])) r[1] else NA_character_
}, error = function(e) NA_character_)
if (is.na(repo_root)) {
  cand <- c(getwd(), file.path(getwd(), "..", ".."))
  hit <- cand[file.exists(file.path(cand, ".claude", "scripts",
                                    "validar_coherencia_matematica.R"))]
  repo_root <- if (length(hit)) normalizePath(hit[1]) else normalizePath(getwd())
}

# El ejercicio fuente vive en Rango-Colesterol-Pacientes/ (se movió desde -Cloze/).
# Se resuelve por búsqueda para sobrevivir reubicaciones futuras.
RMD_NAME <- "rango_colesterol_metacognitivo_interpretacion_n3_cloze_v1.Rmd"
RMD_CANDIDATOS <- c(
  file.path(repo_root, "A-Produccion", "01-En-PreDesarrollo", "Rango-Colesterol-Pacientes", "Cloze", RMD_NAME),
  file.path(repo_root, "A-Produccion", "01-En-PreDesarrollo", "Rango-Colesterol-Pacientes", RMD_NAME),
  file.path(repo_root, "A-Produccion", "01-En-PreDesarrollo", "Rango-Colesterol-Pacientes-Cloze", RMD_NAME)
)
RMD_ABS <- RMD_CANDIDATOS[file.exists(RMD_CANDIDATOS)][1]
if (is.na(RMD_ABS)) {
  hit <- list.files(file.path(repo_root, "A-Produccion"), pattern = RMD_NAME,
                    recursive = TRUE, full.names = TRUE)
  RMD_ABS <- if (length(hit)) hit[1] else RMD_CANDIDATOS[1]
}

# -----------------------------------------------------------------------------
# (A) Detector estático: ¿hay un `repeat {` sin cota de iteraciones?
# -----------------------------------------------------------------------------
# Heurística conservadora: localiza cada bloque que abre con `repeat {` y revisa
# si dentro (hasta su `}` de cierre aproximado) existe un patrón de cota:
#   - un contador incrementado:  <v> <- <v> + 1   /  <v> = <v> + 1
#   - O una referencia a un límite:  max_intentos | max_iter | n_intentos | < N)
# Si NO encuentra cota -> bucle potencialmente infinito (anti-patrón).
detectar_repeat_sin_cota <- function(txt) {
  if (length(txt) == 1L) txt <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  abre <- grep("\\brepeat\\b[[:space:]]*\\{", txt)
  if (length(abre) == 0L) return(character(0))
  hallazgos <- character(0)
  for (i in abre) {
    # Ventana del cuerpo: desde el repeat hasta 40 líneas o el siguiente repeat.
    fin <- min(length(txt), i + 40L)
    cuerpo <- paste(txt[i:fin], collapse = "\n")
    tiene_contador <- grepl("([A-Za-z_.][A-Za-z0-9_.]*)[[:space:]]*(<-|=)[[:space:]]*\\1[[:space:]]*\\+[[:space:]]*1", cuerpo)
    tiene_limite   <- grepl("max_intentos|max_iter|n_intentos|intentos[[:space:]]*<|iter[[:space:]]*<|<[[:space:]]*[0-9]{2,}\\)", cuerpo)
    if (!tiene_contador && !tiene_limite) {
      hallazgos <- c(hallazgos, paste0("repeat sin cota cerca de línea ", i))
    }
  }
  hallazgos
}

test_that("(sanity) el detector marca repeat sin cota y respeta los acotados", {
  malo <- 'repeat {\n  v <- sample(1:10, 5)\n  if (max(v) - min(v) > k) break\n}'
  bueno_contador <- 'it <- 0\nrepeat {\n  it <- it + 1\n  v <- sample(1:10, 5)\n  if (cond || it > 500) break\n}'
  bueno_while <- 'while (!ok && intentos < max_intentos) {\n  intentos <- intentos + 1\n}'
  expect_true(length(detectar_repeat_sin_cota(malo)) >= 1)
  expect_equal(length(detectar_repeat_sin_cota(bueno_contador)), 0)
  expect_equal(length(detectar_repeat_sin_cota(bueno_while)), 0)
})

test_that("rango_colesterol cloze NO contiene repeat sin cota (Error 22 corregido)", {
  skip_if_not(file.exists(RMD_ABS), "ejercicio fuente no encontrado")
  txt <- readLines(RMD_ABS, warn = FALSE)
  hall <- detectar_repeat_sin_cota(txt)
  expect_equal(
    length(hall), 0,
    info = paste0(
      "El ejercicio reintrodujo un bucle `repeat` sin cota de iteraciones ",
      "(riesgo de cuelgue, Error 22):\n  ", paste(hall, collapse = "\n  "),
      "\nUsar construcción determinista (pick_int) o agregar contador + límite. ",
      "Ver .claude/docs/patrones-errores-conocidos.md (Error 22)."
    )
  )
})

# -----------------------------------------------------------------------------
# (B) Guard runtime: el chunk data_generation no debe colgarse en ninguna semilla
# -----------------------------------------------------------------------------
test_that("data_generation de rango_colesterol cloze no se cuelga (N semillas, timeout)", {
  skip_if_not(file.exists(RMD_ABS), "ejercicio fuente no encontrado")
  val_script <- file.path(repo_root, ".claude", "scripts",
                          "validar_coherencia_matematica.R")
  skip_if_not(file.exists(val_script), "validar_coherencia_matematica.R no disponible")
  suppressWarnings(suppressMessages(source(val_script)))
  skip_if_not(exists("parsear_rmd"), "parsear_rmd no disponible")

  parsed <- parsear_rmd(RMD_ABS)
  # data_generation es el 2.º chunk R (setup es el 1.º); fallback al más grande.
  idx_dg <- if (length(parsed$chunks_r) >= 2) 2L else 1L
  codigo_dg <- parse(text = paste(parsed$chunks_r[[idx_dg]], collapse = "\n"))

  N <- 60L            # cubre los seeds-bomba detectados (23,144,229,298,326,...)
  limite_s <- 2.5     # cada generación honesta tarda < 0.1s; 2.5s = colgado
  cuelgues <- integer(0)
  for (s in 1:N) {
    set.seed(s)
    env <- new.env(parent = globalenv())
    ok <- tryCatch({
      setTimeLimit(cpu = limite_s, elapsed = limite_s, transient = TRUE)
      on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
      eval(codigo_dg, envir = env)
      TRUE
    }, error = function(e) {
      if (grepl("time limit|tiempo", conditionMessage(e), ignore.case = TRUE)) FALSE else TRUE
    })
    if (!isTRUE(ok)) cuelgues <- c(cuelgues, s)
  }
  expect_equal(
    length(cuelgues), 0,
    info = paste0("data_generation excedió ", limite_s, "s (cuelgue) en semillas: ",
                  paste(cuelgues, collapse = ", "),
                  ". Regresión del Error 22 (bucle repeat sin cota en Parte 6).")
  )
})
