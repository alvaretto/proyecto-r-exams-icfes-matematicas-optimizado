#!/usr/bin/env Rscript
# =============================================================================
# comparar_version_exams.R — ¿qué cambia en los ejercicios si actualizo R/exams?
# -----------------------------------------------------------------------------
# Compila los ejercicios del repo con la versión instalada y con una candidata
# aislada en otra biblioteca, con las mismas semillas, y compara el contenido. No
# toca la biblioteca del usuario. Responde antes de actualizar, no después.
#
# Capa 1 (contenido): `validar_stresstest.R --json` con cada versión. Si la huella
# difiere, la base se repite: si la base no coincide consigo misma, el ejercicio es
# NO_DETERMINISTA y la diferencia no se le atribuye a la versión.
# Capa 2 (formatos, opcional): renderiza HTML, PDF, DOCX, NOPS y Moodle con cada
# versión y compara si compilan y su texto, porque la capa 1 no pasa por LaTeX.
#
# Uso:
#   Rscript .claude/scripts/comparar_version_exams.R --lib DIR
#           [--raiz A-Produccion] [--muestra 40 | --todos] [--n 3] [--jobs 8]
#           [--formatos 12] [--salida DIR] [--semilla 1]
#
#   --lib       biblioteca con la versión candidata. Se crea con:
#               install.packages("exams", lib = "DIR")
#   --muestra   ejercicios elegidos al azar por zona (por defecto 40).
#   --todos     todo el corpus; --n fija las semillas por ejercicio. N < 100 aquí es
#               una comparación pareada, no una medición de la regla #23.
#
# Compara texto, no píxeles: una figura que cambie sin que cambie su nombre no se ve.
#
# Salida: 0 COMPARAR_SIN_REGRESIONES · 1 COMPARAR_REVISAR (hay CAMBIA, TEXTO_DISTINTO o
# FALLA_SOLO_CANDIDATA: un cambio puede ser una mejora, pero lo decide una persona) ·
# 2 COMPARAR_NO_VERIFICABLE (argumentos, o la candidata no se pudo cargar).
# =============================================================================

FORMATOS <- c("html", "pdf", "docx", "nops", "moodle")

# --- Corpus -----------------------------------------------------------------------

es_ejercicio <- function(ruta) {
  # Los *_interactivo.Rmd son fuentes que deja exams2webquiz en salida/, y las
  # .backup_* son copias: compilarlas da rojos que no dicen nada de un ejercicio.
  base <- basename(ruta)
  if (grepl("_interactivo\\.Rmd$", base) || startsWith(base, ".")) return(FALSE)
  if (grepl("/(salida|worktrees|renv|node_modules|moodle_output|rsconnect)/", paste0("/", ruta, "/"))) return(FALSE)
  l <- tryCatch(readLines(ruta, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  any(grepl("^Meta-information[[:space:]]*$", l)) &&
    any(grepl("^(ex)?type:|^extype:", l))
}

listar_ejercicios <- function(raiz) {
  r <- list.files(raiz, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  r[vapply(r, es_ejercicio, logical(1))]
}

zona_de <- function(ruta) {
  m <- regmatches(ruta, regexpr("0[123]-[^/]+", ruta))
  if (length(m)) m else "otros"
}

elegir_muestra <- function(rutas, k, semilla) {
  if (k >= length(rutas)) return(rutas)
  zonas <- vapply(rutas, zona_de, character(1))
  set.seed(semilla)
  cuota <- round(k * table(zonas) / length(rutas))
  cuota[] <- pmax(1L, cuota)  # pmax() suelto pierde los nombres de las zonas
  unlist(lapply(names(cuota), function(z) {
    grupo <- rutas[zonas == z]
    grupo[sample.int(length(grupo), min(cuota[[z]], length(grupo)))]
  }), use.names = FALSE)
}

# --- Clasificación (pura: se prueba con JSON sintéticos) --------------------------

huellas_de <- function(j) {
  if (is.null(j) || is.null(j$semillas)) return(NULL)
  vapply(j$semillas, function(s) if (is.null(s$huella)) NA_character_ else s$huella, character(1))
}

clasificar_par <- function(base, cand, base2 = NULL) {
  h_b <- huellas_de(base)
  h_c <- huellas_de(cand)
  falla <- function(h) is.null(h) || anyNA(h)
  if (falla(h_b) && falla(h_c)) return("FALLA_EN_AMBAS")
  if (falla(h_c)) return("FALLA_SOLO_CANDIDATA")
  if (falla(h_b)) return("FALLA_SOLO_BASE")
  if (identical(h_b, h_c)) return("IDENTICO")
  h_b2 <- huellas_de(base2)
  if (!is.null(h_b2) && !identical(h_b, h_b2)) return("NO_DETERMINISTA")
  "CAMBIA"
}

veredicto_contenido <- function(clases) {
  if (any(clases %in% c("CAMBIA", "FALLA_SOLO_CANDIDATA"))) 1L else 0L
}

clasificar_formato <- function(base, cand, base2 = NULL) {
  # Cada argumento: list(ok, huella) o NULL si el formato no aplica (CLOZE en NOPS).
  # base2 es la base repetida: si no coincide consigo misma, la versión no es la causa.
  if (is.null(base) && is.null(cand)) return("NO_APLICA")
  if (!isTRUE(base$ok) && !isTRUE(cand$ok)) return("FALLA_EN_AMBAS")
  if (!isTRUE(cand$ok)) return("FALLA_SOLO_CANDIDATA")
  if (!isTRUE(base$ok)) return("FALLA_SOLO_BASE")
  if (identical(base$huella, cand$huella)) return("IDENTICO")
  if (!is.null(base2) && !identical(base$huella, base2$huella)) return("NO_DETERMINISTA")
  "TEXTO_DISTINTO"
}

veredicto_formatos <- function(clases) {
  if (any(clases %in% c("TEXTO_DISTINTO", "FALLA_SOLO_CANDIDATA"))) 1L else 0L
}

normalizar_texto <- function(x) {
  # Lo que cambia entre dos corridas sin que cambie el ejercicio. Las imágenes en Base64,
  # porque el PNG guarda su fecha (en Moodle van en <file encoding="base64">, sin el
  # prefijo `base64,` del HTML); los identificadores de sesión de Moodle, y los espacios
  # que `pdftotext -layout` reparte según el ancho de cada línea.
  x <- gsub("base64,[A-Za-z0-9+/=]+", "base64,", x, perl = TRUE)
  x <- gsub("(<file [^>]*encoding=\"base64\"[^>]*>)[^<]*", "\\1", x, perl = TRUE)
  x <- gsub("exam[0-9]+|question[0-9]+", "ID", x, perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
}

# --- Procesos hijo ----------------------------------------------------------------

rscript <- function() file.path(R.home("bin"), "Rscript")

correr_validador <- function(script, rmd, json, n, lib) {
  args <- c(shQuote(script), shQuote(rmd), "--n", n, "--limite", 120, "--total", 600, "--json", shQuote(json))
  if (nzchar(lib)) args <- c(args, "--lib", shQuote(lib))
  system2(rscript(), args, stdout = FALSE, stderr = FALSE)
  if (file.exists(json)) tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE), error = function(e) NULL) else NULL
}

huella_archivo_texto <- function(ruta) {
  tipo <- tolower(tools::file_ext(ruta))
  txt <- switch(tipo,
    pdf = system2("pdftotext", c("-layout", shQuote(ruta), "-"), stdout = TRUE, stderr = FALSE),
    docx = system2(rmarkdown::pandoc_exec(), c("-t", "plain", shQuote(ruta)), stdout = TRUE, stderr = FALSE),
    readLines(ruta, warn = FALSE, encoding = "UTF-8"))
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(enc2utf8(normalizar_texto(txt)), f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

modo_hijo_formatos <- function(rmd, lib, salida) {
  if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
  suppressPackageStartupMessages(library(exams))
  f <- basename(rmd)
  e <- dirname(rmd)
  meta <- paste(readLines(rmd, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  es_cloze <- grepl("(^|\n)extype:[[:space:]]*cloze", meta)
  od <- tempfile("fmt_")
  dir.create(od)
  un_formato <- function(fmt) {
    if (fmt == "nops" && es_cloze) return(NULL)
    sd <- file.path(od, fmt)
    dir.create(sd)
    set.seed(123L)
    r <- tryCatch({
      switch(fmt,
        html = exams2html(f, n = 1L, name = "x", dir = sd, edir = e, quiet = TRUE),
        pdf = exams2pdf(f, n = 1L, name = "x", dir = sd, edir = e, quiet = TRUE),
        docx = exams2pandoc(f, n = 1L, name = "x", dir = sd, edir = e, quiet = TRUE),
        nops = exams2nops(f, n = 1L, name = "x", dir = sd, edir = e, language = "es", date = "2026-01-01"),
        moodle = exams2moodle(f, n = 1L, name = "x", dir = sd, edir = e, quiet = TRUE))
      TRUE
    }, error = function(err) conditionMessage(err))
    salida_fmt <- list.files(sd, "\\.(html|pdf|docx|xml)$", full.names = TRUE)
    if (!isTRUE(r) || length(salida_fmt) == 0L) {
      return(list(ok = FALSE, huella = NA_character_, error = if (isTRUE(r)) "sin archivo" else substr(r, 1L, 160L)))
    }
    list(ok = TRUE, huella = huella_archivo_texto(salida_fmt[1L]))
  }
  res <- stats::setNames(lapply(FORMATOS, un_formato), FORMATOS)
  saveRDS(list(version = as.character(utils::packageVersion("exams")), formatos = res), salida)
}

correr_formatos <- function(script, rmd, lib, salida) {
  args <- c(shQuote(script), "--hijo-formatos", shQuote(rmd), shQuote(lib), shQuote(salida))
  system2("timeout", c("-k", "5", "600", shQuote(rscript()), args), stdout = FALSE, stderr = FALSE)
  if (file.exists(salida)) readRDS(salida) else NULL
}

version_en <- function(lib) {
  cod <- sprintf('if (nzchar("%s")) .libPaths(c("%s", .libPaths())); cat("VERSION=", as.character(packageVersion("exams")), sep = "")', lib, lib)
  s <- suppressWarnings(system2(rscript(), c("-e", shQuote(cod)), stdout = TRUE, stderr = FALSE))
  sub("^VERSION=", "", grep("^VERSION=", s, value = TRUE)[1L])
}

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
  if ("--hijo-formatos" %in% args) {
    modo_hijo_formatos(args[2L], args[3L], args[4L])
    quit(status = 0L)
  }

  lib <- argumento(args, "--lib", "")
  if (!nzchar(lib) || !dir.exists(lib)) {
    cat("Uso: Rscript comparar_version_exams.R --lib DIR [--raiz A-Produccion] [--muestra 40 | --todos] [--n 3] [--jobs 8] [--formatos 12] [--salida DIR]\n")
    cat("COMPARAR_NO_VERIFICABLE\n")
    quit(status = 2L)
  }
  raiz <- argumento(args, "--raiz", "A-Produccion")
  muestra <- as.integer(argumento(args, "--muestra", 40L))
  n <- as.integer(argumento(args, "--n", 3L))
  jobs <- as.integer(argumento(args, "--jobs", 8L))
  k_formatos <- as.integer(argumento(args, "--formatos", 0L))
  semilla <- as.integer(argumento(args, "--semilla", 1L))
  salida <- argumento(args, "--salida", tempfile("comparar_"))
  dir.create(salida, recursive = TRUE, showWarnings = FALSE)
  lib <- normalizePath(lib)

  v_base <- version_en("")
  v_cand <- version_en(lib)
  cat(sprintf("exams base %s · candidata %s (%s)\n", v_base, v_cand, lib))
  if (is.na(v_base) || is.na(v_cand)) {
    cat("No se pudo cargar exams en alguna de las dos bibliotecas.\nCOMPARAR_NO_VERIFICABLE\n")
    quit(status = 2L)
  }

  corpus <- listar_ejercicios(raiz)
  rutas <- if ("--todos" %in% args) corpus else elegir_muestra(corpus, muestra, semilla)
  cat(sprintf("Corpus: %d ejercicios · comparados: %d · %d semillas por ejercicio\n", length(corpus), length(rutas), n))

  validador <- file.path(dirname(ruta_script()), "validar_stresstest.R")
  uno <- function(i) {
    rmd <- rutas[i]
    p <- function(etq) file.path(salida, sprintf("%05d_%s.json", i, etq))
    b <- correr_validador(validador, rmd, p("base"), n, "")
    c <- correr_validador(validador, rmd, p("cand"), n, lib)
    b2 <- NULL
    if (!identical(huellas_de(b), huellas_de(c)) && !is.null(huellas_de(c))) {
      b2 <- correr_validador(validador, rmd, p("base2"), n, "")
    }
    list(rmd = rmd, clase = clasificar_par(b, c, b2))
  }
  res <- parallel::mclapply(seq_along(rutas), uno, mc.cores = jobs, mc.preschedule = FALSE)
  res <- res[vapply(res, is.list, logical(1))]
  clases <- vapply(res, function(x) x$clase, character(1))
  cat("\nContenido (knit + lectura):\n")
  print(table(clases))
  for (cl in c("CAMBIA", "FALLA_SOLO_CANDIDATA", "FALLA_SOLO_BASE", "NO_DETERMINISTA")) {
    for (x in utils::head(res[clases == cl], 12L)) cat(sprintf("  %s: %s\n", cl, x$rmd))
  }
  for (x in res[clases == "FALLA_EN_AMBAS"]) cat(sprintf("  FALLA_EN_AMBAS: %s\n", x$rmd))
  estado <- veredicto_contenido(clases)
  if (length(res) < length(rutas)) estado <- 2L

  if (k_formatos > 0L) {
    cat("\nFormatos:\n")
    rutas_f <- elegir_muestra(rutas, k_formatos, semilla + 1L)
    formato_uno <- function(i) {
      rmd <- rutas_f[i]
      a <- correr_formatos(ruta_script(), rmd, "", file.path(salida, sprintf("f%05d_base.rds", i)))
      b <- correr_formatos(ruta_script(), rmd, lib, file.path(salida, sprintf("f%05d_cand.rds", i)))
      r <- vapply(FORMATOS, function(f) clasificar_formato(a$formatos[[f]], b$formatos[[f]]), character(1))
      if (any(r == "TEXTO_DISTINTO")) {
        a2 <- correr_formatos(ruta_script(), rmd, "", file.path(salida, sprintf("f%05d_base2.rds", i)))
        r <- vapply(FORMATOS, function(f) clasificar_formato(a$formatos[[f]], b$formatos[[f]], a2$formatos[[f]]), character(1))
      }
      as.list(r)
    }
    rf <- parallel::mclapply(seq_along(rutas_f), formato_uno, mc.cores = jobs, mc.preschedule = FALSE)
    rf <- rf[vapply(rf, is.list, logical(1))]
    tabla <- do.call(rbind, lapply(rf, function(x) unlist(x)))
    for (f in FORMATOS) {
      t <- table(tabla[, f])
      cat(sprintf("  %-7s %s\n", f, paste(names(t), t, sep = "=", collapse = " ")))
      for (clase in c("FALLA_SOLO_CANDIDATA", "TEXTO_DISTINTO", "FALLA_SOLO_BASE")) {
        for (j in which(tabla[, f] == clase)) cat(sprintf("    %s: %s\n", clase, rutas_f[j]))
      }
    }
    if (veredicto_formatos(tabla) == 1L && estado == 0L) estado <- 1L
  }

  cat(sprintf("\nDetalle en %s\n", salida))
  cat(if (estado == 0L) "COMPARAR_SIN_REGRESIONES\n" else if (estado == 1L) "COMPARAR_REVISAR\n" else "COMPARAR_NO_VERIFICABLE\n")
  quit(status = estado)
}
