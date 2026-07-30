# =============================================================================
# test_permutaciones_invariantes.R
#
# Guarda automática de las invariantes I-1 a I-7 del subproyecto
# `permutaciones-pescadores-venia-n4`.
#
# POR QUÉ EXISTE
# --------------
# La respuesta correcta de ese ejercicio es el VALOR de una fórmula (`n!`)
# evaluada sobre un parámetro sorteado, y sus distractores son otras seis
# fórmulas del mismo parámetro (pool de 7, se eligen 3 por versión). Ningún
# validador genérico del repo protege esa relación:
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
# del `.Rmd` (no se reimplementa aquí), se enumera el espacio COMPLETO y el
# `.Rmd` se localiza por NOMBRE, de modo que la suite sobrevive a las promociones
# 01-En-PreDesarrollo -> 02-En-Desarrollo -> 03-En-Produccion.
#
# Referencias:
#   - A-Produccion/01-En-PreDesarrollo/permutaciones-pescadores-venia-n4/.claude/rules/permutaciones-parametricas.md
#   - .claude/docs/patrones-errores-conocidos.md  (Error 27)
#   - .claude/rules/ejercicios-metacognitivos.md  (pool de 4-6 errores)
#   - .claude/rules/diversidad-sustantiva.md      (P5: distractor por magnitud)
# =============================================================================

library(testthat)

# --- Raíz del repo (robusto desde cualquier cwd) ------------------------------
# `system(intern = TRUE)` NO lanza error cuando git falta o el árbol no es un
# checkout: sólo advierte y devuelve character(0), así que un `tryCatch(error=)`
# nunca se dispara y `[1]` daría NA. Se comprueba el resultado y se cae a un
# fallback por archivo marcador (misma forma que test_barco_bbox_invariante.R).
repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE, ignore.stderr = TRUE)
  if (length(r) >= 1 && dir.exists(r[1])) r[1] else NA_character_
}, error = function(e) NA_character_)
if (is.na(repo_root)) {
  cand <- c(getwd(), file.path(getwd(), "..", ".."))
  hit  <- cand[file.exists(file.path(cand, ".claude", "rules", "diversidad-sustantiva.md"))]
  repo_root <- if (length(hit)) normalizePath(hit[1]) else normalizePath(getwd())
}

RMD_NAME <- "permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd"

# El subproyecto migra 01-En-PreDesarrollo -> 02-En-Desarrollo -> 03-En-Produccion
# (su destino en producción ya está reservado en esta misma rama). Se busca por
# NOMBRE para que el test sobreviva a esas promociones: con la ruta fija, el día
# de la promoción esta suite fallaría, `run_all_tests.R` saldría 1 y el hook
# pre-push bloquearía todo push del repo, además de apagar en silencio (vía
# skip_if_not) la guarda I-5, que es la única red contra una clave FALSA.
localizar_rmd <- function() {
  hits <- list.files(file.path(repo_root, "A-Produccion"),
                     pattern = paste0("^", RMD_NAME, "$"),
                     recursive = TRUE, full.names = TRUE)
  hits[!grepl("/_archivo/|/salida/|/revision/|/verif_render/", hits)]
}

RMD <- {
  h <- localizar_rmd()
  if (length(h) == 1L) h[1] else ""   # "" => file.exists() FALSE, sin NA
}

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

test_that("el .Rmd del subproyecto se localiza de forma única", {
  hits <- localizar_rmd()
  expect_equal(
    length(hits), 1L,
    info = paste("Se esperaba exactamente 1 copia de", RMD_NAME,
                 "bajo A-Produccion/; encontradas:", length(hits),
                 if (length(hits)) paste0("(", paste(hits, collapse = ", "), ")") else "")
  )
  expect_true(file.exists(RMD), info = paste("No se localizó:", RMD_NAME))
})

test_that("el pool cumple el mínimo de la regla #1 y hay selección por versión", {
  skip_if_not(file.exists(RMD))
  e <- extraer_entorno(1L)
  expect_gte(length(e$errores_conceptuales), 4L)   # piso duro de la regla #1
  # pool estrictamente mayor que los slots: si fueran iguales, el TIPO de error
  # no variaría nunca entre versiones (Error 27).
  expect_gt(length(e$errores_conceptuales), N_SLOTS)
  expect_equal(length(e$errores_sel), N_SLOTS)
  # 7 es la configuración MEDIDA por el barrido de la decisión D4: es el tamaño
  # mínimo que cierra H1 sin borrar ningún distractor diagnóstico. Encogerlo
  # reintroduce el patrón de magnitud, así que se fija aquí para que reducirlo
  # falle en vez de degradar el ítem en silencio.
  expect_gte(length(e$errores_conceptuales), 7L)
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
  # El espacio es pequeño y se enumera entero: 3 valores de n x C(7,3) = 105.
  # `choose()` (forma cerrada) es una ruta de cálculo INDEPENDIENTE de
  # `ncol(combn())` (enumeración): comparar `total` contra `ncol(combos)` sería
  # tautológico, porque `total` se incrementa una vez por columna de `combos`.
  expect_equal(total, length(n_pool) * choose(length(fórmulas), N_SLOTS))
  expect_gte(total, 105L)
})

test_that("I-7: toda terna legal tiene un distractor > n! y la clave nunca es la mayor", {
  skip_if_not(file.exists(RMD))
  e <- extraer_entorno(1L)

  n_pool   <- e$N_POOL
  fórmulas <- lapply(e$errores_conceptuales, function(x) x$calcula)
  combos   <- utils::combn(length(fórmulas), N_SLOTS)

  # (a) Enumeración del espacio LEGAL (ternas con >= 1 distractor > n!).
  #     Las métricas se miden SOLO ahí: incluir las ilegales daría un falso
  #     verde, porque aportan justo los rangos que la restricción elimina.
  rangos <- integer(0); dominancia <- numeric(0); ilegales <- 0L
  for (n in n_pool) {
    correcta <- factorial(n)
    for (j in seq_len(ncol(combos))) {
      d <- vapply(combos[, j], function(k) as.numeric(fórmulas[[k]](n)), numeric(1L))
      if (!any(d > correcta)) { ilegales <- ilegales + 1L; next }
      v <- c(correcta, d)
      rangos     <- c(rangos, rank(v)[1])
      dominancia <- c(dominancia, correcta / max(d))
    }
  }
  expect_gt(length(rangos), 0L)
  # La clave NUNCA es la opción de mayor magnitud: "elegir el mayor" no acierta.
  expect_false(any(rangos == (N_SLOTS + 1L)),
               info = "hay ternas legales donde la clave es la opción mayor")
  # Y nunca domina al mayor distractor (lo que I-3, unilateral, no puede acotar).
  expect_true(all(dominancia < 1),
              info = sprintf("dominancia máx clave/mayor distractor = %.2fx", max(dominancia)))

  # (b) NO-REGRESIÓN del hallazgo H1. Resultado MEDIDO de la decisión D4
  #     (barrido de 6 configuraciones). Si alguien cambia el pool o la
  #     restricción sin volver a medir, esto FALLA en vez de degradarse callado.
  expect_true(any(rangos <= 2L),
              info = "REGRESIÓN H1: la clave nunca queda entre las 2 opciones menores")
  expect_gt(length(unique(rangos)), 1L)   # ningún patrón posicional puro

  # (c) La SELECCIÓN real del chunk se queda en el espacio legal. (a) mide el
  #     espacio; esto mide la selección. Si se borrara el filtro `legales` del
  #     `.Rmd`, (a) seguiría en verde y sólo este bloque lo atraparía.
  for (s in 1:60) {
    ee   <- extraer_entorno(s)
    d    <- as.numeric(unname(ee$vals))
    corr <- as.numeric(ee$correcta_val)
    expect_true(any(d > corr),
                info = sprintf("semilla %d (n=%d): ninguna opción supera la clave", s, ee$n))
    expect_false(max(c(corr, d)) == corr,
                 info = sprintf("semilla %d (n=%d): la clave es la opción mayor", s, ee$n))
  }
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
