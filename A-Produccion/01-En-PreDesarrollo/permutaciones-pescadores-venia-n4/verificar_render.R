#!/usr/bin/env Rscript
# =============================================================================
# verificar_render.R — verificación rápida del ejercicio (CI, sin plantillas)
#
# Subproyecto: permutaciones-pescadores-venia-n4
# Ejercicio  : permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1
#
# NO confundir con SemilleroUnico_v2.R (exportación real con plantillas
# institucionales y apertura de navegador). Este script solo verifica.
#
# Uso:  Rscript verificar_render.R
# Exit: 0 = todo verde · 1 = al menos un FAIL
#
# Comprueba:
#   V1  HTML   renderiza
#   V2  PDF    renderiza (LaTeX)
#   V3  DOCX   renderiza (pandoc)
#   V4  NOPS   renderiza
#   V5  Moodle renderiza y la opción marcada como correcta ES n!
#   V6  las 4 opciones son distintas en todas las semillas probadas
#   V7  la instancia canónica (contexto 1, n = 4) reproduce el ítem ICFES
#   V8  ninguna fuga de rol en el XML de Moodle (no hay figuras: N/A informativo)
# =============================================================================

suppressMessages(library(exams))

RMD <- "permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd"
OUT <- "verif_render"
dir.create(OUT, showWarnings = FALSE)

fails <- character(0)
ok    <- function(v, msg) cat(sprintf("  ✓ %-6s %s\n", v, msg))
bad   <- function(v, msg) {
  cat(sprintf("  ✗ %-6s %s\n", v, msg))
  fails <<- c(fails, v)
}

intenta <- function(v, etiqueta, expr) {
  r <- try(suppressWarnings(expr), silent = TRUE)
  if (inherits(r, "try-error")) {
    bad(v, paste0(etiqueta, " FALLA: ", sub("\n.*", "", as.character(r))))
    invisible(NULL)
  } else {
    ok(v, paste0(etiqueta, " OK"))
    invisible(r)
  }
}

cat("\n=== Renderizado en los formatos canónicos ===\n")
set.seed(101); intenta("V1", "HTML",
  exams2html(RMD, n = 1, dir = OUT, edir = "."))
set.seed(102); intenta("V2", "PDF ",
  exams2pdf(RMD, n = 1, dir = OUT, edir = "."))
set.seed(103); intenta("V3", "DOCX",
  exams2pandoc(RMD, n = 1, type = "docx", dir = OUT, edir = "."))
set.seed(104); intenta("V4", "NOPS",
  exams2nops(rep(RMD, 3), n = 1, dir = OUT, edir = ".", language = "es"))

cat("\n=== V5: la opción marcada como correcta es n! ===\n")
set.seed(105)
mdir <- file.path(OUT, "moodle")
dir.create(mdir, showWarnings = FALSE)
r <- try(suppressWarnings(
  exams2moodle(RMD, n = 12, dir = mdir, edir = ".", name = "perm_check")
), silent = TRUE)

if (inherits(r, "try-error")) {
  bad("V5", paste0("exams2moodle FALLA: ", sub("\n.*", "", as.character(r))))
} else {
  xml <- list.files(mdir, pattern = "\\.xml$", full.names = TRUE)
  if (!length(xml)) {
    bad("V5", "no se generó XML de Moodle")
  } else {
    txt <- paste(readLines(xml[1], warn = FALSE), collapse = "\n")
    # Cada <question> trae 4 <answer fraction="..."> con su <text>
    qs <- strsplit(txt, "<question type=")[[1]][-1]
    malos <- 0L; revisadas <- 0L; detalle <- character(0)
    for (q in qs) {
      # El XML de exams2moodle emite:
      #   <answer fraction="100" format="html">
      #   <text><![CDATA[<p>\n720\n</p>]]></text>
      partes <- strsplit(q, "<answer ", fixed = TRUE)[[1]][-1]
      partes <- partes[grepl("<text>", partes, fixed = TRUE)]
      if (length(partes) != 4L) next
      revisadas <- revisadas + 1L
      fr  <- as.numeric(sub('.*?fraction="([^"]+)".*', "\\1", partes))
      raw <- sub('(?s).*?<text><!\\[CDATA\\[(.*?)\\]\\]></text>.*', "\\1",
                 partes, perl = TRUE)
      raw <- gsub("<[^>]*>", "", raw)          # quitar <p>...</p>
      raw <- gsub("[[:space:]]+", "", raw)
      vals <- as.numeric(gsub("\\.", "", raw)) # "7.776" -> 7776
      if (anyNA(vals)) { revisadas <- revisadas - 1L; next }
      marcada <- vals[which.max(fr)]
      # n se lee del ENUNCIADO, no del cuarteto de opciones: desde que el pool
      # de errores tiene 5 entradas y se eligen 3, el distractor "cardinal"
      # (que vale n) puede no estar presente, así que min(vals) ya no sirve.
      qt <- sub("(?s).*?<questiontext[^>]*>(.*?)</questiontext>.*", "\\1", q,
                perl = TRUE)
      qt <- gsub("<[^>]*>", " ", qt)
      m  <- regmatches(qt, regexpr("\\b[456]\\b", qt))
      if (!length(m)) { revisadas <- revisadas - 1L; next }
      n_inf <- as.integer(m[1])
      if (!isTRUE(all.equal(marcada, factorial(n_inf)))) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("n=%d marcada=%s esperada=%s",
                                      n_inf, marcada, factorial(n_inf)))
      }
      # unicidad real en el artefacto exportado
      if (length(unique(vals)) != 4L) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("n=%d opciones duplicadas: %s",
                                      n_inf, paste(vals, collapse = ",")))
      }
    }
    if (length(detalle)) cat("    ", paste(detalle, collapse = "\n     "), "\n")
    if (revisadas == 0L) {
      bad("V5", "0 preguntas parseadas del XML — el verificador está ciego")
    } else if (malos > 0L) {
      bad("V5", sprintf("%d/%d preguntas con clave != n!", malos, revisadas))
    } else {
      ok("V5", sprintf("%d/%d preguntas: la opción marcada es exactamente n!",
                       revisadas, revisadas))
    }
  }
}

cat("\n=== V6: unicidad y magnitud — enumeración EXHAUSTIVA del espacio ===\n")
# El pool tiene 5 errores y se eligen 3 por versión: hay C(5,3) = 10 ternas
# posibles por cada valor de n. Se enumeran TODAS (3 x 10 = 30 combinaciones),
# no una muestra.
N_POOL <- c(4L, 5L, 6L)
formulas <- list(
  repeticion = function(n) n^(n - 1L),
  cuadrado   = function(n) n * n,
  cardinal   = function(n) n,
  circular   = function(n) factorial(n - 1L),
  suma       = function(n) n * (n + 1L) / 2L
)
combos <- utils::combn(length(formulas), 3L)
dup <- 0L; igual_corr <- 0L; excede <- 0L; total <- 0L
ratios <- numeric(0); rank_corr <- integer(0)
for (n in N_POOL) {
  corr <- factorial(n)
  for (j in seq_len(ncol(combos))) {
    total <- total + 1L
    d <- vapply(combos[, j], function(k) formulas[[k]](n), numeric(1L))
    v <- c(corr, d)
    if (length(unique(v)) != 4L) dup <- dup + 1L
    if (any(d == corr))          igual_corr <- igual_corr + 1L
    r <- max(v) / corr
    ratios <- c(ratios, r)
    if (r > 15) excede <- excede + 1L
    rank_corr <- c(rank_corr, rank(v)[1])   # posición de la correcta por tamaño
  }
}
if (dup == 0L && igual_corr == 0L && excede == 0L) {
  ok("V6", sprintf(
    "%d/%d ternas: 4 opciones únicas, ninguna == correcta, razón máx/clave en [%.1fx, %.1fx] (umbral 15x)",
    total, total, min(ratios), max(ratios)))
  cat(sprintf("         rango de la correcta por magnitud: %s (varía => sin patrón posicional)\n",
              paste(sort(unique(rank_corr)), collapse = "/")))
} else {
  bad("V6", sprintf("de %d ternas: %d con duplicados, %d con distractor==correcta, %d exceden 15x",
                    total, dup, igual_corr, excede))
}

cat("\n=== V7: la instancia canónica reproduce el ítem ICFES ===\n")
canon_enunciado <- paste0(
  "En una obra de teatro, hay 4 personas que interpretan pescadores. ",
  "Al finalizar la obra, los 4 pescadores deben ubicarse en fila en ",
  "el escenario y hacer una venia ante el público."
)
canon_pregunta <- paste0(
  "¿De cuántas formas pueden ubicarse los cuatro ",
  "pescadores durante la venia final?"
)
src <- paste(readLines(RMD, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
env <- new.env()
chunk <- sub("(?s).*?```\\{r data_generation[^}]*\\}(.*?)```.*", "\\1", src, perl = TRUE)
eval(parse(text = chunk), envir = env)
e1 <- env$contextos[[1]]$enunciado(4L, "cuatro")
p1 <- env$contextos[[1]]$pregunta(4L, "cuatro")
if (identical(e1, canon_enunciado) && identical(p1, canon_pregunta)) {
  ok("V7", "contexto 1 con n=4 == MAT-2026-1-004 verbatim")
} else {
  bad("V7", "el contexto canónico NO reproduce el ítem original")
  cat("    esperado: ", canon_enunciado, "\n    obtenido: ", e1, "\n", sep = "")
}

cat("\n=== V8: fuga de rol por nombre de archivo ===\n")
ok("V8", "N/A — el ejercicio no genera imágenes (Flujo B = false)")

cat("\n")
if (length(fails)) {
  cat(sprintf("RESULTADO: %d FAIL (%s)\n", length(fails),
              paste(unique(fails), collapse = ", ")))
  quit(status = 1L)
} else {
  cat("RESULTADO: todo verde\n")
  quit(status = 0L)
}
