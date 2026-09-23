#!/usr/bin/env Rscript
# =============================================================================
# verificar_render.R — verificación rápida de la VARIANTE CLOZE (CI, sin plantillas)
#
# Subproyecto: permutaciones-pescadores-venia-n4 (variante CLOZE)
# Ejercicio  : permutaciones_pescadores_metacognitivo_formulacion_n4_cloze_v1
#
# Gemelo de ../verificar_render.R (que verifica la variante SCHOICE). Comparte
# con él V6/V7 —el contrato paramétrico es el mismo— y añade los chequeos que
# solo tienen sentido en un CLOZE (V8, V10, V11).
#
# NO confundir con SemilleroCloze.R (exportación real con plantillas
# institucionales). Este script solo verifica.
#
# Uso:  Rscript verificar_render.R
# Exit: 0 = todo verde · 1 = al menos un FAIL
#
# Comprueba:
#   V1  HTML   renderiza
#   V2  PDF    renderiza (LaTeX)
#   V3  DOCX   renderiza (pandoc)
#   V4  NOPS   N/A DOCUMENTADO: exams2nops() rechaza extype cloze de plano
#   V5  Moodle: los 6 gaps salen en orden y con el tipo correcto; las tres claves
#       calculables (Parte 1 = n!, Parte 2 = n-1, Parte 4 = n^n) coinciden con el
#       n leído del ENUNCIADO narrativo; la marca de las Partes 3 y 6 se comprueba
#       SEMÁNTICAMENTE contra el valor/factor del enunciado, no solo "hay una
#       marcada"; y la Parte 5 exige 6 opciones distintas y entre 2 y 4 marcas
#   V6  enumeración EXHAUSTIVA del espacio de ternas: unicidad (I-1), magnitud
#       (I-3) y, sobre el espacio LEGAL, el rango de la clave y la no-regresión
#       del hallazgo H1 (la clave nunca es la opción mayor — I-7)
#   V7  la instancia canónica (contexto 1, n = 4) reproduce el ítem ICFES
#   V8  estructura CLOZE: nº de ##ANSWERi## == nº de exclozetype == nº de
#       bloques de exsolution y extol, y los placeholders están EN ORDEN
#   V9  la SELECCIÓN real del chunk se queda en el espacio legal (240 semillas)
#   V10 C-1: para cada n de N_POOL, las 7 fórmulas del pool y n! dan 8 valores
#       distintos dos a dos -> la Parte 3 tiene UNA sola respuesta correcta
#   V11 no-regresión de la decisión D6: cada afirmación de la Parte 5 recibe el
#       MISMO veredicto en la prosa agrupada de la Solution y en su Answerlist,
#       emparejadas POR CONTENIDO (inmune al barajado de exshuffle)
# =============================================================================

suppressMessages(library(exams))

RMD <- "permutaciones_pescadores_metacognitivo_formulacion_n4_cloze_v1.Rmd"
OUT <- "verif_render"
N_VERSIONES <- 12L   # versiones que V5 exporta a Moodle Y exige haber parseado
N_GAPS      <- 6L
TIPOS_GAP   <- c("MULTICHOICE", "NUMERICAL", "MULTICHOICE",
                 "NUMERICAL", "MULTIRESPONSE", "MULTICHOICE")
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

cat("\n=== Renderizado en los formatos aplicables ===\n")
set.seed(101); intenta("V1", "HTML",
  exams2html(RMD, n = 1, dir = OUT, edir = "."))
set.seed(102); intenta("V2", "PDF ",
  exams2pdf(RMD, n = 1, dir = OUT, edir = "."))
set.seed(103); intenta("V3", "DOCX",
  exams2pandoc(RMD, n = 1, type = "docx", dir = OUT, edir = "."))

cat("\n=== V4: NOPS ===\n")
# NO es un fallo del ejercicio y NO depende de los tipos de gap. Verificado en
# el codigo de exams 2.4.2: exams2nops() calcula
#   utype <- sapply(ufile, function(n) x[[n]]$type)
#   wrong_type <- ufile[utype == "cloze"]
#   if (length(wrong_type) > 0L) stop("the following exercises are cloze ...")
# es decir, rechaza CUALQUIER extype cloze antes de mirar exclozetype. Se
# comprueba que el motivo del rechazo sigue siendo ESE y no otro: si algun dia
# exams admite cloze en NOPS, o si falla por otra razon, este bloque lo dira.
set.seed(104)
r4 <- try(suppressWarnings(
  exams2nops(rep(RMD, 3), n = 1, dir = OUT, edir = ".", language = "es")
), silent = TRUE)
if (!inherits(r4, "try-error")) {
  ok("V4", "NOPS renderizó (exams empezó a admitir cloze: revisar la documentación)")
} else if (grepl("cloze exercises", as.character(r4), fixed = TRUE)) {
  ok("V4", "N/A esperado — exams2nops() rechaza extype cloze por diseño")
} else {
  bad("V4", paste0("NOPS falla por un motivo DISTINTO del esperado: ",
                   sub("\n.*", "", as.character(r4))))
}

# --- Extracción del chunk real del .Rmd (base de V5, V6, V7, V9 y V10) -------
# Se mide el ejercicio REAL, no una copia. `sub()` devuelve la entrada intacta
# cuando el patrón no coincide, así que un encabezado de chunk renombrado haría
# que `parse()` recibiera el .Rmd completo; y un `stopifnot` de invariante que
# salte dentro del chunk aborta el script. Ambos casos se capturan aquí para que
# el reporte V1-V11 y el resumen final siempre se impriman.
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

cat("\n=== V5: los 6 gaps de Moodle y sus claves calculables ===\n")
set.seed(105)
mdir <- file.path(OUT, "moodle")
dir.create(mdir, showWarnings = FALSE)
r <- try(suppressWarnings(
  exams2moodle(RMD, n = N_VERSIONES, dir = mdir, edir = ".", name = "permcz_check")
), silent = TRUE)

if (inherits(r, "try-error")) {
  bad("V5", paste0("exams2moodle FALLA: ", sub("\n.*", "", as.character(r))))
} else {
  xml <- list.files(mdir, pattern = "\\.xml$", full.names = TRUE)
  if (!length(xml)) {
    bad("V5", "no se generó XML de Moodle")
  } else {
    txt <- paste(readLines(xml[1], warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
    qs <- strsplit(txt, "<question type=", fixed = TRUE)[[1]][-1]
    qs <- qs[grepl('^"cloze"', qs)]
    malos <- 0L; revisadas <- 0L; detalle <- character(0)
    for (q in qs) {
      qt <- sub("(?s).*?(<questiontext.*?</questiontext>).*", "\\1", q, perl = TRUE)
      qt <- gsub("&lt;", "<", gsub("&gt;", ">", gsub("&amp;", "&", qt)))
      plano <- gsub("<[^>]*>", " ", qt)
      gaps <- regmatches(plano, gregexpr(
        "\\{[0-9]*:(MULTICHOICE|MULTIRESPONSE|NUMERICAL|SHORTANSWER)[^{}]*\\}",
        plano))[[1]]
      if (length(gaps) != N_GAPS) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("%d gaps en vez de %d", length(gaps), N_GAPS))
        next
      }
      tipos <- sub("^\\{[0-9]*:([A-Z]+):.*$", "\\1", gaps)
      if (!identical(tipos, TIPOS_GAP)) {
        malos <- malos + 1L
        detalle <- c(detalle, paste("tipos de gap inesperados:",
                                    paste(tipos, collapse = "|")))
        next
      }
      # n se lee del ENUNCIADO NARRATIVO (lo anterior a "Parte 1."), no de las
      # opciones ni de los gaps: leerlo de la propia respuesta seria circular y
      # una clave mutada pasaria desapercibida.
      narrativo <- sub("(?s)Parte 1\\..*", "", plano, perl = TRUE)
      m <- regmatches(narrativo, regexpr("\\b[456]\\b", narrativo))
      if (!length(m)) { next }   # version descartada; la cobertura lo reporta
      revisadas <- revisadas + 1L
      n_inf <- as.integer(m[1])

      # Parte 1 (schoice): la opcion marcada con "=" debe ser n!
      op1 <- strsplit(sub("^\\{[0-9]*:MULTICHOICE:", "", sub("\\}$", "", gaps[1])),
                      "~", fixed = TRUE)[[1]]
      clave1 <- sub("^=", "", op1[grepl("^=", op1)])
      vals1  <- suppressWarnings(as.numeric(sub("^[=~]", "", op1)))
      if (length(clave1) != 1L ||
          !isTRUE(all.equal(as.numeric(clave1), factorial(n_inf)))) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("n=%d Parte1 marcada=%s esperada=%s",
                                      n_inf, paste(clave1, collapse = ","),
                                      factorial(n_inf)))
      }
      if (anyNA(vals1) || length(unique(vals1)) != 4L) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("n=%d Parte1 opciones no son 4 numeros distintos: %s",
                                      n_inf, paste(op1, collapse = "|")))
      }
      # Parte 2 (num): n-1 · Parte 4 (num): n^n
      num2 <- suppressWarnings(as.numeric(
        sub("^\\{[0-9]*:NUMERICAL:=([^:}]+).*$", "\\1", gaps[2])))
      num4 <- suppressWarnings(as.numeric(
        sub("^\\{[0-9]*:NUMERICAL:=([^:}]+).*$", "\\1", gaps[4])))
      if (!isTRUE(all.equal(num2, n_inf - 1))) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("n=%d Parte2=%s esperada=%d",
                                      n_inf, num2, n_inf - 1L))
      }
      if (!isTRUE(all.equal(num4, n_inf^n_inf))) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("n=%d Parte4=%s esperada=%d",
                                      n_inf, num4, as.integer(n_inf^n_inf)))
      }
      # Parte 3 (schoice): exactamente una marcada
      op3 <- strsplit(sub("^\\{[0-9]*:MULTICHOICE:", "", sub("\\}$", "", gaps[3])),
                      "~", fixed = TRUE)[[1]]
      # Estructura Y semántica: comprobar solo "hay una marcada" dejaba sin
      # verificar lo único que importa —que la marcada sea la correcta—, así que
      # un fallo de exportación que moviera el `=` habría pasado en verde.
      if (sum(grepl("^=", op3)) != 1L || length(unique(op3)) != 4L) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("n=%d Parte3 no tiene 4 opciones distintas con 1 marcada",
                                      n_inf))
      } else if (!is.null(env)) {
        mv       <- regmatches(plano, regexpr("respondió[[:space:]]+[0-9]+", plano))
        marcada3 <- trimws(sub("^=", "", op3[grepl("^=", op3)]))
        if (!length(mv)) {
          malos <- malos + 1L
          detalle <- c(detalle, sprintf("n=%d Parte3: no se pudo leer el valor del enunciado", n_inf))
        } else {
          val3 <- as.numeric(gsub("[^0-9]", "", mv[1]))
          idx  <- which(vapply(env$errores_conceptuales,
                               function(e) identical(trimws(e$descripcion_corta), marcada3),
                               logical(1L)))
          if (length(idx) != 1L) {
            malos <- malos + 1L
            detalle <- c(detalle, sprintf("n=%d Parte3: la opción marcada no corresponde a ningún error del pool", n_inf))
          } else if (!isTRUE(all.equal(as.numeric(env$errores_conceptuales[[idx]]$calcula(n_inf)), val3))) {
            malos <- malos + 1L
            detalle <- c(detalle, sprintf("n=%d Parte3: la marcada (%s) produce %s, pero el enunciado dice %s",
                                          n_inf, env$errores_conceptuales[[idx]]$codigo,
                                          env$errores_conceptuales[[idx]]$calcula(n_inf), val3))
          }
        }
      }

      # Parte 5 (mchoice): este gap NO se miraba en absoluto más allá de su tipo.
      op5 <- strsplit(sub("^\\{[0-9]*:MULTIRESPONSE:", "", sub("\\}$", "", gaps[5])),
                      "~", fixed = TRUE)[[1]]
      n_marcadas5 <- sum(grepl("^=", op5))
      if (length(unique(op5)) != 6L || n_marcadas5 < 2L || n_marcadas5 > 4L) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("n=%d Parte5: %d marcadas (esperado 2-4) y %d opciones distintas (esperado 6)",
                                      n_inf, n_marcadas5, length(unique(op5))))
      }
      # Parte 6 (schoice V/F): exactamente una marcada de dos
      op6 <- strsplit(sub("^\\{[0-9]*:MULTICHOICE:", "", sub("\\}$", "", gaps[6])),
                      "~", fixed = TRUE)[[1]]
      # El enunciado propone un factor k; la afirmación es verdadera si y solo si
      # k == n+1, así que la marca es CALCULABLE y no hace falta creerse el
      # `stopifnot` del .Rmd.
      if (length(op6) != 2L || sum(grepl("^=", op6)) != 1L) {
        malos <- malos + 1L
        detalle <- c(detalle, sprintf("n=%d Parte6 no es V/F con 1 marcada", n_inf))
      } else {
        mk       <- regmatches(plano, regexpr("multiplicado por[[:space:]]+[0-9]+", plano))
        marcada6 <- trimws(sub("^=", "", op6[grepl("^=", op6)]))
        if (!length(mk)) {
          malos <- malos + 1L
          detalle <- c(detalle, sprintf("n=%d Parte6: no se pudo leer el factor del enunciado", n_inf))
        } else {
          k6        <- as.integer(gsub("[^0-9]", "", mk[1]))
          esperada6 <- if (k6 == n_inf + 1L) "Verdadero" else "Falso"
          if (!identical(marcada6, esperada6)) {
            malos <- malos + 1L
            detalle <- c(detalle, sprintf("n=%d Parte6: factor %d -> esperada '%s', marcada '%s'",
                                          n_inf, k6, esperada6, marcada6))
          }
        }
      }
    }
    if (length(detalle)) cat("    ", paste(detalle, collapse = "\n     "), "\n")
    if (revisadas == 0L) {
      bad("V5", "0 preguntas parseadas del XML — el verificador está ciego")
    } else if (malos > 0L) {
      bad("V5", sprintf("%d incoherencias en %d/%d versiones parseadas",
                        malos, revisadas, N_VERSIONES))
    } else if (revisadas < N_VERSIONES) {
      # La cobertura NUNCA se reporta como "revisadas/revisadas": seria
      # tautologica, y una clave mutada confinada a las versiones descartadas se
      # publicaria sin deteccion. Misma leccion que la V5 del SCHOICE hermano.
      bad("V5", sprintf(
        "cobertura incompleta: %d/%d versiones parseadas (%d preguntas cloze en el XML)",
        revisadas, N_VERSIONES, length(qs)))
    } else {
      ok("V5", sprintf(
        "%d/%d versiones: 6 gaps en orden y tipo · P1 = n! · P2 = n-1 · P4 = n^n · P3 marca el error que produce el valor del enunciado · P5 6 opciones y 2-4 marcas · P6 V/F coherente con el factor",
        revisadas, N_VERSIONES))
    }
  }
}

cat("\n=== V6: unicidad y magnitud — enumeración EXHAUSTIVA del espacio ===\n")
# El pool y el rango de n se EXTRAEN del .Rmd, no se reimplementan aquí: una
# copia local queda obsoleta en silencio en cuanto alguien añade o cambia una
# fórmula, y V6 seguiría anunciando "todo verde" sobre el pool viejo.
if (is.null(env)) {
  bad("V6", err_extraccion)
} else {
  N_POOL   <- env$N_POOL
  formulas <- lapply(env$errores_conceptuales, function(e) e$calcula)
  n_slots  <- length(env$errores_sel)
  combos   <- utils::combn(length(formulas), n_slots)
  dup <- 0L; igual_corr <- 0L; excede <- 0L; total <- 0L
  ratios <- numeric(0)
  legal_rank <- integer(0); legal_dom <- numeric(0); n_legal <- 0L
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
      if (any(d > corr)) {                     # terna LEGAL (satisface I-7)
        n_legal    <- n_legal + 1L
        legal_rank <- c(legal_rank, rank(v)[1])
        legal_dom  <- c(legal_dom, corr / max(d))
      }
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
  if (n_legal == 0L) {
    bad("V6", "0 ternas legales: la restricción I-7 dejó el espacio vacío")
  } else {
    pct_baja <- 100 * mean(legal_rank <= 2L)
    pct_max  <- 100 * mean(legal_rank == (n_slots + 1L))
    cat(sprintf("         espacio legal %d/%d (%d descartadas por I-7: sin ningún distractor > n!)\n",
                n_legal, total, total - n_legal))
    cat(sprintf("         rango de la clave: %s | mitad baja %.1f%% | 'elegir el mayor' acierta %.1f%% | clave/mayor distr. máx %.2fx\n",
                paste(sort(unique(legal_rank)), collapse = "/"),
                pct_baja, pct_max, max(legal_dom)))
    if (length(unique(legal_rank)) < 2L) {
      bad("V6", sprintf("rango de la clave FIJO en el %d.º puesto en las %d ternas legales: patrón posicional",
                        unique(legal_rank)[1], n_legal))
    }
    # `pct_max` vale SIEMPRE 0 por construcción, no porque el ejercicio esté sano:
    # `legal_rank` solo acumula ternas que cumplen `any(d > corr)`, y eso implica
    # `max(v) > corr`, luego la clave no puede ser el máximo DENTRO de ese
    # subconjunto. Verificado analíticamente y con 200.000 ternas aleatorias: 0
    # violaciones posibles. Se imprime como DATO DESCRIPTIVO; un `bad()` aquí
    # sería una rama inalcanzable — un guard que aparenta proteger sin poder
    # dispararse nunca, que es justo el patrón que este subproyecto persigue.
    #
    # La protección REAL contra la regresión de H1 la da V9, que mide `clave_max`
    # sobre la selección efectiva SIN pre-filtrar por legalidad. Las otras dos
    # guardas de este bloque (rango no constante, mitad baja > 0) sí son
    # alcanzables y sí fallan.
    if (pct_max > 0) {
      bad("V6", sprintf("imposible por construcción: pct_max = %.1f%% — revisar el filtro `legal`",
                        pct_max))
    }
    if (pct_baja == 0) {
      bad("V6", "REGRESIÓN de H1: la clave nunca queda entre las 2 opciones menores")
    }
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
    ok("V7", "contexto 1 con n=4 == MAT-2026-1-004 verbatim (Parte 1)")
  } else {
    bad("V7", "el contexto canónico NO reproduce el ítem original")
    cat("    esperado: ", canon_enunciado, "\n    obtenido: ", r7$e1, "\n", sep = "")
  }
}

cat("\n=== V8: estructura CLOZE (regla #14) ===\n")
# Análisis ESTÁTICO sobre el .Rmd: los placeholders tienen que estar en orden y
# ser tantos como tipos declara exclozetype. Un ##ANSWER## fuera de orden es el
# Incidente A del orquestador-cloze y no lo detecta ningún render.
ph <- regmatches(src, gregexpr("##ANSWER([0-9]+)##", src))[[1]]
if (!length(ph)) {
  bad("V8", "no hay ningún ##ANSWERi## en el .Rmd")
} else {
  idx <- as.integer(sub("##ANSWER([0-9]+)##", "\\1", ph))
  probs <- character(0)
  if (length(idx) != N_GAPS)
    probs <- c(probs, sprintf("%d placeholders, se esperaban %d", length(idx), N_GAPS))
  if (!identical(idx, seq_len(length(idx))))
    probs <- c(probs, paste("placeholders fuera de orden:", paste(idx, collapse = ",")))
  if (is.null(env)) {
    probs <- c(probs, err_extraccion)
  } else {
    n_tipos <- length(strsplit(env$exclozetype_str, "|", fixed = TRUE)[[1]])
    n_sol   <- length(strsplit(env$exsolution_str,  "|", fixed = TRUE)[[1]])
    n_tol   <- length(strsplit(env$extol_str,       "|", fixed = TRUE)[[1]])
    if (length(unique(c(length(idx), n_tipos, n_sol, n_tol))) != 1L)
      probs <- c(probs, sprintf("descuadre: %d ##ANSWER##, %d exclozetype, %d exsolution, %d extol",
                                length(idx), n_tipos, n_sol, n_tol))
    if (env$n_answerlist_enunciado != 16L || env$n_answerlist_solucion != 18L)
      probs <- c(probs, sprintf("answerlist %d/%d, se esperaban 16/18",
                                env$n_answerlist_enunciado, env$n_answerlist_solucion))
  }
  if (length(probs)) bad("V8", paste(probs, collapse = " · "))
  else ok("V8", sprintf("%d ##ANSWERi## en orden == exclozetype == exsolution == extol; answerlist 16 (enunciado) / 18 (solución)",
                        length(idx)))
}

cat("\n=== V9: la selección del .Rmd se queda en el espacio legal ===\n")
# V6 demuestra que el espacio legal es sano; V9 demuestra que el .Rmd REALMENTE
# se restringe a él. No son redundantes: V6 mide el ESPACIO (enumera combn del
# pool), no la SELECCIÓN. Si alguien borrara el filtro `legales` del chunk, V6
# seguiría en verde mientras el ejercicio vuelve a emitir ternas con la clave
# como opción mayor.
N_EMP <- 240L
if (is.null(env)) {
  bad("V9", err_extraccion)
} else {
  ilegales <- 0L; clave_max <- 0L; evaluadas <- 0L
  ternas_vistas <- character(0); ranks_obs <- integer(0)
  for (s in seq_len(N_EMP)) {
    e9 <- new.env()
    set.seed(20000L + s)
    r9 <- try(suppressWarnings(eval(parse(text = chunk), envir = e9)), silent = TRUE)
    if (inherits(r9, "try-error")) next
    evaluadas <- evaluadas + 1L
    d    <- as.numeric(unname(e9$vals))
    corr <- as.numeric(e9$correcta_val)
    if (!any(d > corr))          ilegales  <- ilegales + 1L
    if (max(c(corr, d)) == corr) clave_max <- clave_max + 1L
    ranks_obs <- c(ranks_obs, rank(c(corr, d))[1])
    ternas_vistas <- c(ternas_vistas, paste0(e9$n, ":", paste(sort(vapply(
      e9$errores_sel, function(x) x$codigo, character(1L))), collapse = "+")))
  }
  if (evaluadas == 0L) {
    bad("V9", "0 evaluaciones del chunk completaron — el chequeo está ciego")
  } else if (evaluadas < N_EMP) {
    bad("V9", sprintf("el chunk abortó en %d de %d semillas — alguna invariante salta",
                      N_EMP - evaluadas, N_EMP))
  } else if (ilegales > 0L || clave_max > 0L) {
    bad("V9", sprintf("%d/%d ternas sin ningún distractor > n! y %d con la clave como opción mayor",
                      ilegales, evaluadas, clave_max))
  } else {
    cat(sprintf("         %d ternas (n:códigos) distintas alcanzadas | rango de la clave observado: %s | mitad baja %.1f%%\n",
                length(unique(ternas_vistas)),
                paste(sort(unique(ranks_obs)), collapse = "/"),
                100 * mean(ranks_obs <= 2L)))
    ok("V9", sprintf("%d/%d versiones: toda terna tiene ≥1 distractor > n! y la clave nunca es la mayor",
                     evaluadas, N_EMP))
  }
}

cat("\n=== V10 (C-1): la Parte 3 tiene UNA sola respuesta correcta ===\n")
# Invariante PROPIA del CLOZE. La Parte 3 muestra el valor de un error y pide
# identificarlo entre 4 descripciones, una de ellas de un error que NO está en
# la Parte 1. Si dos fórmulas del pool coincidieran en valor para algún n, esa
# parte tendría DOS respuestas correctas -- e I-1 no lo vería, porque solo mira
# la terna efectivamente seleccionada.
#
# HONESTIDAD SOBRE LA COBERTURA: V6 ya detectaría esa colisión. Al enumerar TODOS
# los combn(7,3), cualquier par de fórmulas coincide en los 5 combos donde ambas
# coexisten, y esos darían `dup > 0`. Medido por mutación: forzar una colisión
# produce 15 ternas duplicadas (5 combos x 3 valores de n) en la lógica de V6.
# V10 NO añade cobertura hoy. Se conserva por dos razones concretas: (a) su
# mensaje señala la CAUSA —qué dos fórmulas colisionan y en qué n— en vez de un
# conteo agregado de duplicados, y (b) sería la única guarda si `n_slots` bajara
# a 1, porque entonces ningún par coexistiría en una terna.
if (is.null(env)) {
  bad("V10", err_extraccion)
} else {
  N_POOL   <- env$N_POOL
  formulas <- lapply(env$errores_conceptuales, function(e) e$calcula)
  colisiones <- character(0)
  for (n in N_POOL) {
    v <- c(factorial(n), vapply(formulas, function(f) as.numeric(f(n)), numeric(1L)))
    nombres <- c("n!", vapply(env$errores_conceptuales,
                              function(e) e$codigo, character(1L)))
    dupes <- v[duplicated(v)]
    if (length(dupes)) {
      for (d in unique(dupes)) {
        colisiones <- c(colisiones, sprintf("n=%d: %s valen %g", n,
                                            paste(nombres[v == d], collapse = " y "), d))
      }
    }
  }
  if (length(colisiones)) {
    bad("V10", paste("colisión de valores:", paste(colisiones, collapse = " · ")))
  } else {
    ok("V10", sprintf("%d valores por n (n!, %d fórmulas) distintos dos a dos en n=%s",
                      length(formulas) + 1L, length(formulas),
                      paste(N_POOL, collapse = "/")))
  }
}

cat("\n=== V11: coherencia por CONTENIDO de la Parte 5 (decisión D6) ===\n")
# Guarda de NO-REGRESIÓN de la decisión D6, MEDIDA sobre el HTML renderizado.
# `exshuffle: TRUE` reordena los dos Answerlists del cloze pero NO la prosa de
# la Solution, que este .Rmd emite con cat(). Por eso la prosa AGRUPA por valor
# de verdad ("Verdaderas:" / "Falsas:") en vez de reproducir la lista en su
# orden interno: agrupar no afirma nada sobre posiciones.
#
# Lo que se comprueba NO es el orden --seria una condicion falsa bajo shuffle--
# sino que cada afirmacion reciba el MISMO veredicto en los dos sitios,
# emparejadas por CONTENIDO. Falla si alguien restaura la lista ordenada, si el
# agrupamiento se desincroniza de sol_p5, o si el Answerlist se desalinea.
html_file <- file.path(OUT, "plain1.html")
if (!file.exists(html_file)) {
  bad("V11", "no se encontró el HTML de V1 para comparar")
} else {
  h <- paste(readLines(html_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  h <- gsub("(?s)<script.*?</script>", " ", h, perl = TRUE)
  # Las etiquetas se sustituyen por ESPACIO, no por salto de línea: el `**` de
  # markdown se convierte en <strong>, que al partir por líneas separaría la
  # marca de su texto y dejaría el chequeo ciego. Ninguna de las 10 afirmaciones
  # del pool contiene un punto interno, así que "hasta el primer punto" delimita
  # bien cada una.
  h <- gsub("<[^>]*>", " ", h)
  h <- gsub("&#8212;", "—", gsub("&amp;", "&", h))
  h <- gsub("[[:space:]]+", " ", h)

  bloques <- regmatches(h, gregexpr("(?:Verdadera|Falsa)\\. *[^.]*\\.", h,
                                    perl = TRUE))[[1]]
  etiqueta <- sub("^(Verdadera|Falsa)\\..*$", "\\1", bloques)
  texto    <- sub("^(?:Verdadera|Falsa)\\. *", "", bloques)

  i_v  <- regexpr("Verdaderas:", h, fixed = TRUE)
  i_f  <- regexpr("Falsas:",     h, fixed = TRUE)
  i_p6 <- regexpr("Respuesta correcta — Parte 6", h, fixed = TRUE)
  if (length(bloques) != 6L) {
    bad("V11", sprintf("no se pudieron aislar las 6 afirmaciones del Answerlist (halladas %d)",
                       length(bloques)))
  } else if (i_v < 0L || i_f < 0L || i_p6 < 0L || !(i_v < i_f && i_f < i_p6)) {
    bad("V11", "la Solution no agrupa la Parte 5 en 'Verdaderas:' / 'Falsas:' antes de la Parte 6 (¿volvió la lista ordenada?)")
  } else {
    grupo_v <- substr(h, i_v, i_f - 1L)
    grupo_f <- substr(h, i_f, i_p6 - 1L)
    discrepan <- character(0)
    for (k in seq_along(bloques)) {
      en_v <- grepl(texto[k], grupo_v, fixed = TRUE)
      en_f <- grepl(texto[k], grupo_f, fixed = TRUE)
      esperado <- if (etiqueta[k] == "Verdadera") en_v else en_f
      if (!esperado || (en_v && en_f)) {
        discrepan <- c(discrepan, sprintf("«%s…» Answerlist=%s prosa=%s",
                                          substr(texto[k], 1, 40), etiqueta[k],
                                          if (en_v) "Verdaderas" else if (en_f) "Falsas" else "(ausente)"))
      }
    }
    if (length(discrepan)) {
      bad("V11", paste("veredicto incoherente entre prosa y Answerlist:",
                       paste(discrepan, collapse = " · ")))
    } else {
      ok("V11", "6/6 afirmaciones con el mismo veredicto en la prosa agrupada y en el Answerlist")
    }
  }
}

cat("\n")
if (length(fails)) {
  cat(sprintf("RESULTADO: %d FAIL (%s)\n", length(fails),
              paste(unique(fails), collapse = ", ")))
  quit(status = 1L)
} else {
  cat("RESULTADO: todo verde\n")
  quit(status = 0L)
}
