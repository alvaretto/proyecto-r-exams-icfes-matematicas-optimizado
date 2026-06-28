#!/usr/bin/env Rscript
# =============================================================================
# validar_diversidad_sustantiva.R
# -----------------------------------------------------------------------------
# Verifica que la RESPUESTA CORRECTA (su CONTENIDO, no su posición) varíe entre
# versiones — es decir, que la diversidad sea SUSTANTIVA y no solo COSMÉTICA
# (envoltorio narrativo variable + orden de opciones).
#
# MOTIVO (incidente 2026-06-27, desplazamiento-avion-aeropuerto):
#   Un ejercicio reportó "288/300 versiones únicas" y aun así la opción correcta
#   era SIEMPRE el mismo diagrama (parámetros 100/50/30/70 hardcoded + PNGs
#   estáticos). El conteo de versiones del render mide la FORMA (contexto, orden,
#   reflexión), no la SUSTANCIA (datos numéricos / respuesta correcta).
#
# Uso:
#   Rscript validar_diversidad_sustantiva.R archivo.Rmd [--n 40]
#
# Veredictos (stdout + exit status):
#   PASS               (exit 0) -> la respuesta correcta varía lo suficiente
#   ERR_DIV_COSMETICA  (exit 1) -> respuesta correcta INVARIANTE (BLOQUEANTE)
#   WARN_DIV_BAJA      (exit 0) -> varía poco (informativo; revisar rangos)
#   WARN_DIV_INDET     (exit 0) -> no se pudo identificar la respuesta correcta
# =============================================================================

suppressWarnings(suppressMessages(ok_digest <- requireNamespace("digest", quietly = TRUE)))
if (!ok_digest) { cat("WARN_DIV_INDET: paquete 'digest' no disponible\n"); quit(status = 0) }

args <- commandArgs(trailingOnly = TRUE)
rmd <- if (length(args) >= 1) args[1] else NA
n <- 40L
if ("--n" %in% args) { v <- suppressWarnings(as.integer(args[which(args == "--n") + 1])); if (!is.na(v)) n <- v }
if (n < 5L) n <- 40L
if (is.na(rmd) || !file.exists(rmd)) { cat("WARN_DIV_INDET: .Rmd no encontrado:", rmd, "\n"); quit(status = 0) }

rmd <- normalizePath(rmd)
lines <- readLines(rmd, warn = FALSE)

# --- Extraer el chunk data_generation (primer chunk R del ejercicio) ---
dg <- grep("^```\\{r[ ]*data_generation", lines)
allr <- grep("^```\\{r([ ,].*)?\\}?\\s*$|^```\\{r[ ,]", lines)
start <- if (length(dg) > 0) dg[1] else if (length(allr) > 0) allr[1] else NA
fences <- grep("^```\\s*$", lines)
end <- if (!is.na(start)) fences[fences > start][1] else NA
if (is.na(start) || is.na(end)) { cat("WARN_DIV_INDET: no se pudo aislar data_generation\n"); quit(status = 0) }
code <- paste(lines[(start + 1):(end - 1)], collapse = "\n")
expr <- tryCatch(parse(text = code), error = function(e) NULL)
if (is.null(expr)) { cat("WARN_DIV_INDET: data_generation no parseable de forma aislada\n"); quit(status = 0) }

d <- digest::digest
hash_file <- function(f) if (file.exists(f)) d(readBin(f, "raw", n = file.info(f)$size)) else NA_character_

# --- Fingerprint del CONTENIDO de la respuesta correcta (nunca de su posición) ---
fp_respuesta <- function(env) {
  # Estrategia 1: lista de opciones con $tipo == "correcta"
  for (vn in c("opciones_pre_mezcla", "opciones_mezcladas", "opciones")) {
    if (exists(vn, envir = env, inherits = FALSE)) {
      ops <- get(vn, envir = env)
      if (is.list(ops) && length(ops) > 0) {
        tipos <- vapply(ops, function(x) if (is.list(x) && !is.null(x$tipo)) as.character(x$tipo) else NA_character_, character(1))
        idx <- which(tipos == "correcta")
        if (length(idx) >= 1) {
          el <- ops[[idx[1]]]
          parts <- character(0)
          if (!is.null(el$descrip)) parts <- c(parts, as.character(el$descrip))
          if (!is.null(el$valor))   parts <- c(parts, as.character(el$valor))
          if (!is.null(el$archivo)) { h <- hash_file(el$archivo); parts <- c(parts, if (!is.na(h)) h else as.character(el$archivo)) }
          if (length(parts) == 0) parts <- d(el)
          return(d(paste(parts, collapse = "|")))
        }
      }
    }
  }
  # Estrategia 2: vector solución (sol/solucion) + lista de opciones -> contenido del marcado
  for (sn in c("sol", "solucion", "solucion_vector")) {
    if (exists(sn, envir = env, inherits = FALSE)) {
      sol <- get(sn, envir = env)
      if (is.numeric(sol) && sum(sol == 1, na.rm = TRUE) == 1) {
        for (on in c("opciones_mezcladas", "opciones", "opciones_valores", "opciones_texto", "opciones_num")) {
          if (exists(on, envir = env, inherits = FALSE)) {
            ops <- get(on, envir = env)
            if (length(ops) == length(sol)) {
              el <- ops[[which(sol == 1)]]
              if (is.list(el) && !is.null(el$archivo)) { h <- hash_file(el$archivo); return(d(c(if (!is.null(el$descrip)) as.character(el$descrip), if (!is.na(h)) h else as.character(el$archivo)))) }
              return(d(el))
            }
          }
        }
      }
    }
  }
  # Estrategia 3: variables canónicas de "valor correcto"
  for (vn in c("valor_correcto", "respuesta_correcta", "media_correcta", "mediana_correcta",
               "moda_correcta", "mediana_calc", "distancia_restante", "descripcion_correcta")) {
    if (exists(vn, envir = env, inherits = FALSE)) return(d(get(vn, envir = env)))
  }
  return(NA_character_)
}

# --- Ejecutar n veces en entorno + cwd aislados ---
tmp <- file.path(tempdir(), paste0("divsust_", Sys.getpid())); dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
old <- getwd(); on.exit({ setwd(old); unlink(tmp, recursive = TRUE) }, add = TRUE)
fps <- character(0); errs <- 0L; indet <- 0L
for (i in seq_len(n)) {
  setwd(tmp)
  env <- new.env(parent = globalenv())
  assign("n", NA, envir = env)  # 'n' inocua: algunas plantillas la referencian con lazy-eval
  set.seed(i * 7919L + 13L)
  okr <- tryCatch({ suppressWarnings(suppressMessages(eval(expr, envir = env))); TRUE }, error = function(e) FALSE)
  setwd(old)
  if (!okr) { errs <- errs + 1L; next }
  f <- tryCatch(fp_respuesta(env), error = function(e) NA_character_)
  if (is.na(f)) { indet <- indet + 1L } else { fps <- c(fps, f) }
}

# --- Veredicto ---
validas <- length(fps); unicos <- length(unique(fps))
cat(sprintf("Diversidad sustantiva: %d/%d versiones evaluadas | respuesta correcta: %d valor(es) unico(s) | errores=%d indeterminadas=%d\n",
            validas, n, unicos, errs, indet))
if (validas == 0L) { cat("WARN_DIV_INDET: no se pudo identificar/evaluar la respuesta correcta (revisar manualmente)\n"); quit(status = 0) }
umbral <- max(2L, ceiling(validas * 0.30))
if (unicos == 1L) {
  cat("ERR_DIV_COSMETICA: la respuesta correcta es INVARIANTE entre versiones — la diversidad es solo cosmetica.\n")
  cat("  -> Aleatorizar los datos numericos/el contenido de la opcion correcta. PROHIBIDO valores fijos hardcoded o PNGs estaticos como opciones.\n")
  cat("  -> Ver .claude/rules/diversidad-sustantiva.md\n")
  quit(status = 1)
}
if (unicos < umbral) { cat(sprintf("WARN_DIV_BAJA: la respuesta correcta varia poco (%d < umbral %d en %d versiones). Ampliar rangos de aleatorizacion.\n", unicos, umbral, validas)); quit(status = 0) }
cat("PASS: la respuesta correcta varia suficientemente (diversidad sustantiva confirmada).\n")
quit(status = 0)
