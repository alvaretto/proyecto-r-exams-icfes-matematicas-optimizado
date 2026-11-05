#!/usr/bin/env Rscript
# =============================================================================
# validar_stresstest.R — compila el ejercicio completo N veces con límite de tiempo
# -----------------------------------------------------------------------------
# Qué cubre y nadie más cubre: la FASE 2G (`validar_multisemilla.R`) ejecuta solo
# el chunk `data_generation`, y la FASE 2H compila 10 PDF. Un fallo en un chunk o
# en un `r ...` en línea de Question/Solution, en la lectura de metadatos o una
# fuga de `NA` en el texto, que aparece en pocas semillas, no lo ve ninguna.
#
# Contrato tomado de `exams::stresstest_exercise()` (2.4-2): semillas 1:n,
# `setTimeLimit` por semilla, `num_to_schoice_maxit` negativo y registro de avisos
# y errores sin detenerse. Cada semilla se pasa con `seed =`: la versión medida es la
# que exporta `exams2moodle(seed = matrix(1:100, ncol = 1))` (exams >= 2.4-3).
# NO se envuelve esa función porque, medido el 2026-11-04:
#   - aborta al final, y pierde todas las semillas, si el ejercicio deja un objeto
#     de longitud 1 que `as.data.frame()` no admite (p. ej. un módulo de reticulate);
#   - descarta el texto, y sin él no se pueden medir fugas ni comparar versiones;
#   - corre en `.GlobalEnv`, donde el ejercicio puede pisar al validador.
#
# `setTimeLimit` solo corta código R: un `system()` o una llamada a Python colgados
# no se interrumpen (medido: `sleep 25` con límite de 3 s tardó 25 s). Por eso las
# semillas corren en un proceso hijo vigilado por `timeout` (--total): si muere, las
# semillas ya hechas se conservan y se nombra la que quedó en curso.
#
# Uso:
#   Rscript .claude/scripts/validar_stresstest.R archivo.Rmd
#           [--n 100] [--limite 60] [--total 1200] [--json salida.json] [--lib DIR]
#
#   --n       semillas (regla #23: 100; se declara si se usa otro valor).
#   --limite  segundos por semilla (corte blando, solo código R).
#   --total   segundos para el proceso hijo completo (corte duro).
#   --json    escribe el detalle por semilla (lo usa comparar_version_exams.R).
#   --lib     biblioteca que se antepone a .libPaths() en el hijo (otra versión de
#             exams). `.Rprofile` antepone ~/R/library, así que R_LIBS no basta.
#
# Cómo leer un fallo en la semilla k: las semillas corren en el mismo proceso, igual
# que `exams2moodle(n = 100)`. Un ejercicio que arrastra estado en Python puede fallar
# aquí y pasar con una sola versión en un proceso limpio; el fallo es real para las
# exportaciones con n > 1. Medido en vuelo_acrobatico_mejorado (2026-11-04).
#
# Códigos:
#   ERR_STRESS_TIMEOUT          se excedió el límite (bucle sin cota, Error 22)
#   ERR_STRESS_ERROR            falla el knit o la lectura de metadatos
#   ERR_STRESS_TEXTO            un valor de R llega al texto visible
#   WARN_STRESS_AVISO           avisos de R durante la compilación
#   WARN_STRESS_POSICION        la clave no se reparte entre posiciones (regla #22 P4)
#   WARN_STRESS_NO_DETERMINISTA la misma semilla no da el mismo texto: lo validado no
#                               es lo que se exporta (p. ej. set.seed con Sys.time)
#   WARN_STRESS_INDET           muestra incompleta: no es PASS
#
# Salida: 0 PASS o solo WARN · 1 ERR_* · 2 NO VERIFICABLE (uso, no se pudo medir o
# muestra incompleta sin cuelgue demostrado; nunca se lee como PASS).
# =============================================================================

# --- Patrones calibrados contra el corpus (ver test_validar_stresstest.R) -------
# Valores de R que no deberían llegar nunca al texto visible. Se exige que no estén
# pegados a letras, cifras o guion bajo para no marcar palabras que los contienen.
# \\p{L} y no [[:alnum:]]: bajo PCRE esa clase no incluye las letras acentuadas.
PATRON_FUGA <- paste0(
  "(?<![\\p{L}\\p{N}_.])",
  "(NA|NaN|-?Inf|NULL|numeric\\(0\\)|character\\(0\\)|integer\\(0\\)|logical\\(0\\))",
  "(?![\\p{L}\\p{N}_])"
)
ALFA_POSICION <- 0.001  # binomial por opción, con Bonferroni dentro
# reticulate avisa de que RETICULATE_PYTHON manda sobre `use_python()`: es el
# entorno fijado por .Rprofile, no un defecto. Aparecía en 86 de 373 ejercicios.
AVISOS_BENIGNOS <- "^(The request to|Previous request to) `use_python\\("

# --- Motor (corre en el proceso hijo) -------------------------------------------

quitar_no_texto <- function(x) {
  # Las rutas de imágenes y el código no son texto para el estudiante. Un bloque
  # verbatim puede mostrar `Inf` legítimamente (salida de t.test en ttest.Rmd).
  marcas <- grepl("^[[:space:]]*(```|~~~)", x)
  x <- x[!(marcas | (cumsum(marcas) %% 2L) == 1L)]
  x <- gsub("!\\[[^]]*\\]\\([^)]*\\)(\\{[^}]*\\})?", " ", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x, perl = TRUE)
  x <- gsub("`[^`]*`", " ", x, perl = TRUE)
  x
}

buscar_fugas <- function(textos) {
  texto <- quitar_no_texto(enc2utf8(unlist(textos, use.names = FALSE)))
  texto <- texto[!is.na(texto)]
  if (length(texto) == 0L) return(character(0))
  hits <- regmatches(texto, gregexpr(PATRON_FUGA, texto, perl = TRUE))
  unique(unlist(hits))
}

huella_texto <- function(partes) {
  # Los anclajes `{#afirmacion-d92ac822}` llevan Sys.time() en algunos ejercicios: no
  # son contenido, y sin quitarlos un ejercicio reproducible pasaba por no determinista.
  texto <- gsub("\\{#[^}]*\\}", "", unlist(partes, use.names = FALSE), perl = TRUE)
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(enc2utf8(texto), f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

correr_semilla <- function(archivo, edir, semilla, limite) {
  entorno <- new.env(parent = globalenv())
  avisos <- character(0)
  ganchos <- ls(envir = .userHooksEnv, all.names = TRUE)
  setTimeLimit(elapsed = limite, transient = TRUE)
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(
    withCallingHandlers(
      # `seed =` y no set.seed(): xexams() vuelve a sembrar antes de leer el ejercicio, que
      # es donde `exshuffle` mezcla las opciones. Con set.seed() ese paso sigue la secuencia
      # y la semilla s no sería la versión s de exams2moodle(seed = matrix(1:n, ncol = 1)).
      exams::xexams(archivo, n = 1L, edir = edir, seed = matrix(semilla),
                    driver = list(sweave = list(envir = entorno))),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  segundos <- proc.time()[["elapsed"]] - t0
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  # `evaluate` deja ganchos abiertos si el límite lo corta a mitad (FIXME en exams).
  sobran <- setdiff(ls(envir = .userHooksEnv, all.names = TRUE), ganchos)
  for (g in sobran[!startsWith(sobran, "UserHook::")]) setHook(g, NULL, "replace")

  salida <- list(semilla = semilla, segundos = segundos, avisos = avisos,
                 error = NA_character_, timeout = FALSE, tipo = NA_character_,
                 posicion = NA_integer_, fugas = character(0), huella = NA_character_)
  if (inherits(res, "error")) {
    salida$error <- conditionMessage(res)
    salida$timeout <- segundos >= limite || grepl("time limit", salida$error, fixed = TRUE)
    return(salida)
  }
  ex <- res[[1L]][[1L]]
  meta <- ex$metainfo
  salida$tipo <- meta$type
  textos <- list(ex$question, ex$questionlist, ex$solution, ex$solutionlist)
  salida$fugas <- buscar_fugas(textos)
  salida$huella <- huella_texto(c(textos, meta$type, unlist(meta$solution),
                                  unlist(meta$clozetype)))
  if (identical(meta$type, "schoice") && is.logical(meta$solution)) {
    salida$posicion <- which(meta$solution)[1L]
    salida$n_opciones <- length(meta$solution)
  }
  salida
}

modo_hijo <- function(rmd, n, limite, dir_tmp, lib) {
  if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
  suppressPackageStartupMessages(library(exams))
  options(num_to_schoice_maxit = -10000L)
  writeLines(as.character(utils::packageVersion("exams")), file.path(dir_tmp, "version"))
  archivo <- basename(rmd)
  edir <- dirname(rmd)
  for (s in seq_len(n)) {
    writeLines(sprintf("%d %.0f", s, unclass(Sys.time())), file.path(dir_tmp, "en_curso"))
    r <- correr_semilla(archivo, edir, s, limite)
    saveRDS(r, file.path(dir_tmp, sprintf("s%06d.rds", s)))
  }
  unlink(file.path(dir_tmp, "en_curso"))
  saveRDS(correr_semilla(archivo, edir, 1L, limite), file.path(dir_tmp, "repite.rds"))
}

# --- Resumen (puro: se prueba con resultados sintéticos) -------------------------

agrupar_mensajes <- function(msgs, semillas) {
  if (length(msgs) == 0L) return(list())
  clave <- gsub("[[:space:]]+", " ", gsub("[0-9]+", "#", msgs))
  grupos <- split(seq_along(msgs), clave)
  orden <- order(-lengths(grupos))
  lapply(grupos[orden], function(i) {
    list(mensaje = substr(trimws(msgs[i[1L]]), 1L, 200L), n = length(unique(semillas[i])),
         semillas = utils::head(sort(unique(semillas[i])), 5L))
  })
}

prueba_posicion <- function(posiciones, n_opciones) {
  posiciones <- posiciones[!is.na(posiciones)]
  if (length(posiciones) < 20L || n_opciones < 2L) return(NULL)  # regla #23: n < 20
  tabla <- tabulate(posiciones, nbins = n_opciones)
  p <- vapply(tabla, function(k) {
    stats::binom.test(k, length(posiciones), 1 / n_opciones)$p.value
  }, numeric(1))
  list(tabla = tabla, p_min = min(p), sesgada = min(p) < ALFA_POSICION / n_opciones)
}

resumir_stress <- function(resultados, n, limite, corte_duro = NULL, repeticion = NULL) {
  semillas <- vapply(resultados, function(r) r$semilla, integer(1))
  timeouts <- vapply(resultados, function(r) isTRUE(r$timeout), logical(1))
  errores <- vapply(resultados, function(r) !is.na(r$error) && !isTRUE(r$timeout), logical(1))
  con_error <- which(errores)
  msg_error <- vapply(resultados[con_error], function(r) r$error, character(1))
  resultados <- lapply(resultados, function(r) {
    r$avisos <- r$avisos[!grepl(AVISOS_BENIGNOS, r$avisos)]
    r
  })
  avisos <- unlist(lapply(resultados, function(r) r$avisos))
  sem_avisos <- rep(semillas, vapply(resultados, function(r) length(r$avisos), integer(1)))
  fugas <- unlist(lapply(resultados, function(r) r$fugas))
  sem_fugas <- rep(semillas, vapply(resultados, function(r) length(r$fugas), integer(1)))
  seg <- vapply(resultados, function(r) r$segundos, numeric(1))

  r <- list(
    n_pedidas = n, n_hechas = length(resultados),
    timeouts = semillas[timeouts],
    errores = agrupar_mensajes(msg_error, semillas[con_error]),
    avisos = agrupar_mensajes(avisos, sem_avisos),
    fugas = agrupar_mensajes(fugas, sem_fugas),
    segundos = if (length(seg)) c(mediana = stats::median(seg), maximo = max(seg)) else NULL,
    tipo = c(unique(stats::na.omit(unlist(lapply(resultados, function(x) x$tipo)))), NA_character_)[1L],
    corte_duro = corte_duro, posicion = NULL
  )
  pos <- vapply(resultados, function(x) x$posicion %||% NA_integer_, integer(1))
  nop <- unique(unlist(lapply(resultados, function(x) x$n_opciones)))
  if (length(nop) == 1L) r$posicion <- prueba_posicion(pos, nop)

  codigos <- character(0)
  if (length(r$timeouts) > 0L || !is.null(corte_duro$cuelgue)) codigos <- c(codigos, "ERR_STRESS_TIMEOUT")
  if (length(r$errores) > 0L) codigos <- c(codigos, "ERR_STRESS_ERROR")
  if (length(r$fugas) > 0L) codigos <- c(codigos, "ERR_STRESS_TEXTO")
  incompleta <- r$n_hechas < n && !("ERR_STRESS_TIMEOUT" %in% codigos)
  if (incompleta) codigos <- c(codigos, "WARN_STRESS_INDET")
  if (length(r$avisos) > 0L) codigos <- c(codigos, "WARN_STRESS_AVISO")
  if (isTRUE(r$posicion$sesgada)) codigos <- c(codigos, "WARN_STRESS_POSICION")
  primera <- if (length(resultados) > 0L) resultados[[1L]]$huella else NA_character_
  r$reproducible <- if (is.null(repeticion) || is.na(primera) || is.na(repeticion$huella)) NA else
    identical(primera, repeticion$huella)
  if (isFALSE(r$reproducible)) codigos <- c(codigos, "WARN_STRESS_NO_DETERMINISTA")
  r$codigos <- codigos
  r$salida <- if (any(startsWith(codigos, "ERR_"))) 1L else if (incompleta) 2L else 0L
  r
}

`%||%` <- function(a, b) if (is.null(a)) b else a

imprimir_grupos <- function(grupos, n_max = 8L) {
  for (g in utils::head(grupos, n_max)) {
    cat(sprintf("     %d semilla(s), p. ej. %s: %s\n", g$n, paste(g$semillas, collapse = ","), g$mensaje))
  }
  if (length(grupos) > n_max) cat(sprintf("     ... y %d mensaje(s) más\n", length(grupos) - n_max))
}

imprimir_resumen <- function(r) {
  cat(sprintf("Semillas: %d/%d", r$n_hechas, r$n_pedidas))
  if (!is.null(r$segundos)) cat(sprintf(" · mediana %.2f s · máximo %.2f s", r$segundos[["mediana"]], r$segundos[["maximo"]]))
  if (!is.na(r$tipo)) cat(" · tipo", r$tipo)
  cat("\n")
  if (!is.null(r$posicion)) {
    cat("Posición de la clave:", paste(r$posicion$tabla, collapse = " / "), "\n")
  }
  if ("ERR_STRESS_TIMEOUT" %in% r$codigos) {
    s <- c(r$timeouts, r$corte_duro$cuelgue)
    cat("ERR_STRESS_TIMEOUT: se excedió el límite en las semillas", paste(utils::head(s, 8L), collapse = ", "), "\n")
    cat("  -> bucle sin cota o generación imposible (Error 22, regla #21 F1).\n")
  }
  if ("ERR_STRESS_ERROR" %in% r$codigos) {
    cat("ERR_STRESS_ERROR: falla la compilación completa en algunas semillas\n")
    imprimir_grupos(r$errores)
  }
  if ("ERR_STRESS_TEXTO" %in% r$codigos) {
    cat("ERR_STRESS_TEXTO: un valor de R llega al texto visible\n")
    imprimir_grupos(r$fugas)
  }
  if ("WARN_STRESS_INDET" %in% r$codigos) {
    cat("WARN_STRESS_INDET: muestra incompleta, sin veredicto: el resto de semillas no se midió.\n")
  }
  if ("WARN_STRESS_AVISO" %in% r$codigos) {
    cat("WARN_STRESS_AVISO: avisos de R durante la compilación\n")
    imprimir_grupos(r$avisos)
  }
  if ("WARN_STRESS_NO_DETERMINISTA" %in% r$codigos) {
    cat("WARN_STRESS_NO_DETERMINISTA: la semilla 1 repetida dio otro texto. Lo validado no es lo que se exporta.\n")
    cat("  -> buscar set.seed(), Sys.time() o azar en Python sin sembrar.\n")
  }
  if ("WARN_STRESS_POSICION" %in% r$codigos) {
    cat(sprintf("WARN_STRESS_POSICION: la clave no se reparte entre las posiciones (p = %.2g, regla #22 P4)\n", r$posicion$p_min))
  }
  if (length(r$codigos) == 0L) cat("PASS: compilación completa sin errores, cuelgues ni fugas de texto.\n")
}

a_json <- function(r, resultados, rmd, version) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("--json requiere el paquete jsonlite")
  jsonlite::toJSON(list(
    rmd = rmd, md5 = unname(tools::md5sum(rmd)), exams = version,
    codigos = as.list(r$codigos), n_pedidas = r$n_pedidas, n_hechas = r$n_hechas,
    tipo = r$tipo, reproducible = r$reproducible, timeouts = as.list(r$timeouts),
    errores = unname(r$errores),
    avisos = unname(r$avisos), fugas = unname(r$fugas), segundos = as.list(r$segundos),
    posicion = r$posicion,
    semillas = lapply(resultados, function(x) x[c("semilla", "huella", "tipo", "error", "segundos")])
  ), auto_unbox = TRUE, null = "null", na = "null", digits = 6)
}

# --- CLI ------------------------------------------------------------------------

ruta_script <- function() {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  normalizePath(sub("^--file=", "", a[1L]))
}

argumento <- function(args, nombre, defecto) {
  i <- match(nombre, args)
  if (is.na(i) || i == length(args)) return(defecto)
  args[i + 1L]
}

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)

  if ("--hijo" %in% args) {
    modo_hijo(args[2L], as.integer(args[3L]), as.numeric(args[4L]), args[5L], args[6L])
    quit(status = 0L)
  }

  posicionales <- args[!grepl("^--", args)]
  valores <- c(argumento(args, "--n", ""), argumento(args, "--limite", ""), argumento(args, "--total", ""),
               argumento(args, "--json", ""), argumento(args, "--lib", ""))
  posicionales <- setdiff(posicionales, valores)
  if (length(posicionales) != 1L || !file.exists(posicionales)) {
    cat("Uso: Rscript validar_stresstest.R archivo.Rmd [--n 100] [--limite 60] [--total 1200] [--json f] [--lib dir]\n")
    quit(status = 2L)
  }
  n <- as.integer(argumento(args, "--n", 100L))
  limite <- as.numeric(argumento(args, "--limite", 60))
  total <- as.numeric(argumento(args, "--total", 1200))
  ruta_json <- argumento(args, "--json", "")
  lib <- argumento(args, "--lib", "")
  rmd <- normalizePath(posicionales)
  cat("md5 del .Rmd: ", unname(tools::md5sum(rmd)), " (", rmd, ")\n", sep = "")

  dir_tmp <- tempfile("stress_")
  dir.create(dir_tmp)
  bitacora <- file.path(dir_tmp, "hijo.log")
  rscript <- file.path(R.home("bin"), "Rscript")
  hijo <- c(shQuote(ruta_script()), "--hijo", shQuote(rmd), n, limite, shQuote(dir_tmp), shQuote(lib))
  # LANGUAGE=en fija los mensajes de R: bajo es_CO el del límite sale traducido.
  con_timeout <- nzchar(Sys.which("timeout"))
  orden <- if (con_timeout) "timeout" else rscript
  argumentos <- if (con_timeout) c("-k", "5", total, shQuote(rscript), hijo) else hijo
  estado <- system2(orden, argumentos, stdout = bitacora, stderr = bitacora, env = "LANGUAGE=en")

  archivos <- sort(list.files(dir_tmp, "^s[0-9]+\\.rds$", full.names = TRUE))
  resultados <- lapply(archivos, readRDS)
  version <- if (file.exists(file.path(dir_tmp, "version"))) readLines(file.path(dir_tmp, "version"), 1L) else NA_character_

  if (length(resultados) == 0L && !file.exists(file.path(dir_tmp, "en_curso"))) {
    cat("WARN_STRESS_INDET: el proceso hijo no llegó a medir nada (estado ", estado, ")\n", sep = "")
    cat(utils::tail(readLines(bitacora, warn = FALSE), 6L), sep = "\n")
    quit(status = 2L)
  }

  corte <- NULL
  en_curso <- file.path(dir_tmp, "en_curso")
  if (file.exists(en_curso)) {
    # El hijo murió con una semilla abierta: por `timeout` (124/137) o por un fallo
    # fuera de R. Es cuelgue solo si esa semilla ya superaba el límite por semilla.
    d <- strsplit(readLines(en_curso, 1L), " ", fixed = TRUE)[[1L]]
    abierta <- as.integer(d[1L])
    transcurrido <- as.numeric(unclass(Sys.time())) - as.numeric(d[2L])
    corte <- list(semilla = abierta, estado = estado,
                  cuelgue = if (transcurrido >= limite) abierta else NULL)
    cat(sprintf("Corte duro en la semilla %d tras %.0f s (estado del hijo: %s)\n", abierta, transcurrido, estado))
  }

  ruta_rep <- file.path(dir_tmp, "repite.rds")
  repeticion <- if (file.exists(ruta_rep)) readRDS(ruta_rep) else NULL
  r <- resumir_stress(resultados, n, limite, corte, repeticion)
  imprimir_resumen(r)
  if (nzchar(ruta_json)) writeLines(a_json(r, resultados, rmd, version), ruta_json)
  quit(status = r$salida)
}
