# =============================================================================
# test_permutaciones_invariantes.R
#
# Guarda automática de las invariantes I-1 a I-6 del subproyecto
# `permutaciones-pescadores-venia-n4`.
#
# POR QUÉ EXISTE
# --------------
# La respuesta correcta de ese ejercicio es el VALOR de una fórmula (`n!`)
# evaluada sobre un parámetro sorteado, y sus distractores son otras cuatro
# fórmulas del mismo parámetro. Ningún validador genérico del repo protege esa
# relación:
#   - `validar_coherencia_matematica.R` valida cada error por separado, no el
#     conjunto, y su Capa B (21 keywords semánticas) es específica de
#     estadística descriptiva: en combinatoria no tiene reglas aplicables.
#   - `validar_diversidad_sustantiva.R` mide la variación del VALOR de la
#     respuesta correcta, no la del tipo de distractor.
# Medido por mutación el 2026-07-29: marcar como correcta la opción `n^2` y
# desactivar el `stopifnot` interno produce un `.Rmd` que compila, renderiza en
# los cuatro formatos y recibe APROBADO del validador de coherencia, con la
# clave FALSA. Este test es la red que atrapa ese caso.
#
# Mismo patrón que `test_barco_bbox_invariante.R`: se EXTRAE la definición real
# del `.Rmd` (no se reimplementa aquí) y se enumera el espacio COMPLETO.
#
# Referencias:
#   - A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4/.claude/rules/permutaciones-parametricas.md
#   - .claude/docs/patrones-errores-conocidos.md  (Error 27)
#   - .claude/rules/ejercicios-metacognitivos.md  (pool de 4-6 errores)
#   - .claude/rules/diversidad-sustantiva.md      (P5: distractor por magnitud)
# =============================================================================

library(testthat)

repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE)[1],
  error = function(e) normalizePath("..", mustWork = FALSE)
)

RMD <- file.path(
  repo_root,
  "A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4",
  "permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd"
)

UMBRAL_MAGNITUD <- 15   # razón máx/clave admitida (regla #22, patrón P5)
N_SLOTS         <- 3L   # distractores por versión (SCHOICE de 4 opciones)

# --- Extracción del chunk real del .Rmd --------------------------------------
extraer_entorno <- function(semilla) {
  src <- paste(readLines(RMD, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  chunk <- sub("(?s).*?```\\{r data_generation[^}]*\\}(.*?)```.*", "\\1",
               src, perl = TRUE)
  e <- new.env()
  set.seed(semilla)
  eval(parse(text = chunk), envir = e)
  e
}

test_that("el .Rmd del subproyecto existe", {
  expect_true(file.exists(RMD), info = paste("No se encontró:", RMD))
})

test_that("el pool cumple el mínimo de la regla #1 y hay selección por versión", {
  skip_if_not(file.exists(RMD))
  e <- extraer_entorno(1L)
  expect_gte(length(e$errores_conceptuales), 4L)
  # pool estrictamente mayor que los slots: si fueran iguales, el TIPO de error
  # no variaría nunca entre versiones (Error 27).
  expect_gt(length(e$errores_conceptuales), N_SLOTS)
  expect_equal(length(e$errores_sel), N_SLOTS)
})

test_that("I-1..I-4 se cumplen en TODAS las combinaciones del espacio", {
  skip_if_not(file.exists(RMD))
  e <- extraer_entorno(1L)

  n_pool  <- e$N_POOL
  fórmulas <- lapply(e$errores_conceptuales, function(x) x$calcula)
  combos  <- utils::combn(length(fórmulas), N_SLOTS)

  total <- 0L
  for (n in n_pool) {
    correcta <- factorial(n)
    for (j in seq_len(ncol(combos))) {
      total <- total + 1L
      d <- vapply(combos[, j], function(k) as.numeric(fórmulas[[k]](n)), numeric(1L))
      v <- c(correcta, d)
      etiqueta <- sprintf("n=%d terna=%s", n, paste(combos[, j], collapse = ","))

      # I-1: las 4 opciones son distintas
      expect_equal(length(unique(v)), 4L, info = paste("I-1", etiqueta))
      # I-2: ningún distractor iguala a la correcta
      expect_false(any(d == correcta), info = paste("I-2", etiqueta))
      # I-3: plausibilidad de magnitud (expect_lte no admite `info`)
      expect_true(max(v) / correcta <= UMBRAL_MAGNITUD,
                  info = paste("I-3", etiqueta, "ratio",
                               round(max(v) / correcta, 2)))
      # I-4: enteros positivos
      expect_true(all(v > 0), info = paste("I-4", etiqueta))
      expect_true(all(v == floor(v)), info = paste("I-4", etiqueta))
    }
  }
  # El espacio es pequeño y se enumera entero: 3 valores de n x C(5,3) = 30.
  expect_equal(total, length(n_pool) * ncol(combos))
  expect_gte(total, 30L)
})

test_that("I-5: la opción marcada es siempre n! (barrido de semillas)", {
  skip_if_not(file.exists(RMD))
  for (s in 1:60) {
    e <- extraer_entorno(s)
    expect_equal(sum(e$sol), 1L, info = paste("semilla", s))
    marcada <- e$opciones[which(e$sol == 1L)]
    esperada <- e$fmt(as.integer(factorial(e$n)))
    expect_identical(marcada, esperada,
                     info = sprintf("semilla %d: n=%d marcada=%s esperada=%s",
                                    s, e$n, marcada, esperada))
  }
})

test_that("I-6: la instancia canónica reproduce el ítem ICFES MAT-2026-1-004", {
  skip_if_not(file.exists(RMD))

  # a) el texto del contexto canónico con n = 4 es verbatim
  e <- extraer_entorno(1L)
  enunciado_oficial <- paste0(
    "En una obra de teatro, hay 4 personas que interpretan pescadores. ",
    "Al finalizar la obra, los 4 pescadores deben ubicarse en fila en ",
    "el escenario y hacer una venia ante el público."
  )
  pregunta_oficial <- paste0(
    "¿De cuántas formas pueden ubicarse los cuatro ",
    "pescadores durante la venia final?"
  )
  expect_identical(e$contextos[[1]]$enunciado(4L, "cuatro"), enunciado_oficial)
  expect_identical(e$contextos[[1]]$pregunta(4L, "cuatro"), pregunta_oficial)

  # b) cuando la versión ES la canónica, las 4 opciones son las oficiales
  vistas <- 0L
  for (s in 1:200) {
    ee <- extraer_entorno(s)
    if (isTRUE(ee$es_canonica)) {
      vistas <- vistas + 1L
      valores <- as.integer(gsub("\\.", "", ee$opciones))
      expect_setequal(valores, c(24L, 64L, 16L, 4L))
    }
  }
  # Con 6 contextos y 3 valores de n, la canónica debe aparecer varias veces en
  # 200 semillas; si nunca aparece, la rama `es_canonica` está muerta.
  expect_gt(vistas, 0L)
})

test_that("calcula() de cada error del pool es determinista (función pura)", {
  skip_if_not(file.exists(RMD))
  e <- extraer_entorno(1L)
  prohibidas <- "sample\\(|runif\\(|rnorm\\(|rbinom\\(|rpois\\(|Sys\\.time|set\\.seed"
  for (err in e$errores_conceptuales) {
    cuerpo <- paste(deparse(err$calcula), collapse = " ")
    expect_false(grepl(prohibidas, cuerpo),
                 info = paste(err$codigo, "contiene una función aleatoria"))
    for (n in e$N_POOL) {
      expect_identical(err$calcula(n), err$calcula(n),
                       info = paste(err$codigo, "no es determinista en n =", n))
    }
  }
})

test_that("el chunk no reintroduce set.seed ni entropía externa", {
  skip_if_not(file.exists(RMD))
  src <- paste(readLines(RMD, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  chunk <- sub("(?s).*?```\\{r data_generation[^}]*\\}(.*?)```.*", "\\1",
               src, perl = TRUE)
  # Se excluyen los comentarios: el chunk MENCIONA set.seed en una advertencia.
  codigo <- paste(sub("#.*$", "", strsplit(chunk, "\n")[[1]]), collapse = "\n")
  expect_false(grepl("set\\.seed\\(", codigo))
  expect_false(grepl("Sys\\.time\\(|proc\\.time\\(|Sys\\.Date\\(", codigo))
})

test_that("la Solution es letter-independent (regla #19)", {
  skip_if_not(file.exists(RMD))
  lineas <- readLines(RMD, warn = FALSE, encoding = "UTF-8")
  ini <- grep("^Solution\\s*$", lineas)
  fin <- grep("^Meta-information\\s*$", lineas)
  skip_if(length(ini) == 0 || length(fin) == 0, "no se delimitó la Solution")
  bloque <- lineas[ini[1]:fin[1]]
  expect_false(any(grepl("`r\\s+(letra_correcta|letras\\[)", bloque)))
  expect_false(any(grepl("Opci[oó]n\\s+[A-D]\\b", bloque)))
})
