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
N_VERSIONES <- 12L   # versiones que V5 exporta a Moodle Y exige haber parseado
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
  exams2moodle(RMD, n = N_VERSIONES, dir = mdir, edir = ".", name = "perm_check")
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
    } else if (revisadas < N_VERSIONES) {
      # La cobertura NO puede reportarse como "revisadas/revisadas": sería
      # tautológica. Tres rutas descartan una versión en silencio (opciones que
      # no parsean a número, enunciado sin \b[456]\b, cuarteto incompleto), así
      # que una clave mutada confinada a las versiones descartadas —p. ej. sólo
      # en la rama n=6, la única que alcanza la razón extrema 10,8x— se
      # publicaría sin detección mientras el resto sigue en verde.
      bad("V5", sprintf(
        "cobertura incompleta: %d/%d versiones parseadas (%d preguntas en el XML, %d descartadas)",
        revisadas, N_VERSIONES, length(qs), length(qs) - revisadas))
    } else {
      ok("V5", sprintf("%d/%d versiones: la opción marcada es exactamente n!",
                       revisadas, N_VERSIONES))
    }
  }
}

# --- Extracción del chunk real del .Rmd (base de V6 y V7) --------------------
# V6 y V7 deben medir el ejercicio REAL, no una copia. `sub()` devuelve la
# entrada intacta cuando el patrón no coincide, así que un encabezado de chunk
# renombrado haría que `parse()` recibiera el .Rmd completo; y un `stopifnot` de
# invariante que salte dentro del chunk aborta el script. Ambos casos se capturan
# aquí para que el reporte V1-V8 y el resumen final siempre se impriman.
src   <- paste(readLines(RMD, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
chunk <- sub("(?s).*?```\\{r data_generation[^}]*\\}(.*?)```.*", "\\1", src, perl = TRUE)
env   <- NULL
err_extraccion <- NA_character_
if (identical(chunk, src) || !nzchar(trimws(chunk))) {
  err_extraccion <- "no se pudo extraer el chunk `data_generation` (¿cambió su encabezado?)"
} else {
  env <- new.env()
  set.seed(106)
  r_chunk <- try(suppressWarnings(eval(parse(text = chunk), envir = env)),
                 silent = TRUE)
  if (inherits(r_chunk, "try-error")) {
    err_extraccion <- paste0("el chunk data_generation ABORTA: ",
                             sub("\n.*", "", as.character(r_chunk)))
    env <- NULL
  }
}

cat("\n=== V6: unicidad y magnitud — enumeración EXHAUSTIVA del espacio ===\n")
# El pool y el rango de n se EXTRAEN del .Rmd, no se reimplementan aquí: una
# copia local queda obsoleta en silencio en cuanto alguien añade o cambia una
# fórmula, y V6 seguiría anunciando "todo verde" sobre el pool viejo mientras el
# ejercicio real aborta en `exams2moodle` a mitad del lote (regla local:
# "si añades o cambias una fórmula del pool, V6 debe volver a dar 100 %").
if (is.null(env)) {
  bad("V6", err_extraccion)
} else {
  N_POOL   <- env$N_POOL
  formulas <- lapply(env$errores_conceptuales, function(e) e$calcula)
  n_slots  <- length(env$errores_sel)
  combos   <- utils::combn(length(formulas), n_slots)
  dup <- 0L; igual_corr <- 0L; excede <- 0L; total <- 0L
  ratios <- numeric(0); domina <- numeric(0); rank_corr <- integer(0)
  for (n in N_POOL) {
    corr <- factorial(n)
    for (j in seq_len(ncol(combos))) {
      total <- total + 1L
      d <- vapply(combos[, j], function(k) as.numeric(formulas[[k]](n)), numeric(1L))
      v <- c(corr, d)
      if (length(unique(v)) != (n_slots + 1L)) dup <- dup + 1L
      if (any(d == corr))                      igual_corr <- igual_corr + 1L
      r <- max(v) / corr
      ratios <- c(ratios, r)
      if (r > 15) excede <- excede + 1L
      domina    <- c(domina, corr / max(d))    # cuánto domina la clave (I-3 es unilateral)
      rank_corr <- c(rank_corr, rank(v)[1])    # posición de la correcta por tamaño
    }
  }
  if (dup == 0L && igual_corr == 0L && excede == 0L) {
    ok("V6", sprintf(
      "%d/%d ternas (pool=%d, slots=%d, n=%s): 4 opciones únicas, ninguna == correcta, razón máx/clave en [%.1fx, %.1fx] (umbral 15x)",
      total, total, length(formulas), n_slots, paste(N_POOL, collapse = "/"),
      min(ratios), max(ratios)))
  } else {
    bad("V6", sprintf("de %d ternas: %d con duplicados, %d con distractor==correcta, %d exceden 15x",
                      total, dup, igual_corr, excede))
  }
  # Guarda de deriva POSICIONAL sobre la clave (regla #22, patrón P4 aplicado a
  # la respuesta correcta). I-3 sólo acota que un distractor sea muy grande; NO
  # acota que la clave domine a todos. Si además el rango de la correcta fuera
  # siempre el mismo, el ítem sería resoluble por posición sin razonar.
  cat(sprintf("         rango de la correcta por magnitud: %s en %d ternas | clave/mayor distractor hasta %.1fx\n",
              paste(sort(unique(rank_corr)), collapse = "/"), total, max(domina)))
  if (length(unique(rank_corr)) < 2L) {
    bad("V6", sprintf("rango de la correcta FIJO en el %d.º puesto en las %d ternas: patrón posicional",
                      unique(rank_corr)[1], total))
  }
  if (!any(rank_corr <= 2L)) {
    cat("         AVISO: la clave nunca queda entre las 2 opciones menores — descartar las 2 menores\n")
    cat("                deja al estudiante adivinando al 50 %. Ver docs/BACKLOG.md, hallazgo H1.\n")
  }
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
if (is.null(env)) {
  bad("V7", err_extraccion)
} else {
  r7 <- try({
    e1 <- env$contextos[[1]]$enunciado(4L, "cuatro")
    p1 <- env$contextos[[1]]$pregunta(4L, "cuatro")
    list(e1 = e1, p1 = p1)
  }, silent = TRUE)
  if (inherits(r7, "try-error")) {
    bad("V7", paste0("no se pudo evaluar el contexto canónico: ",
                     sub("\n.*", "", as.character(r7))))
  } else if (identical(r7$e1, canon_enunciado) && identical(r7$p1, canon_pregunta)) {
    ok("V7", "contexto 1 con n=4 == MAT-2026-1-004 verbatim")
  } else {
    bad("V7", "el contexto canónico NO reproduce el ítem original")
    cat("    esperado: ", canon_enunciado, "\n    obtenido: ", r7$e1, "\n", sep = "")
  }
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
