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

# --- Fingerprint POR GAP (modo CLOZE) ----------------------------------------
# Un CLOZE tiene N claves, una por gap, y nombra sus variables por parte
# (sol_p3, opciones_p3, exsol_p1...). Las tres estrategias de fp_respuesta()
# buscan nombres SIN sufijo, así que devuelven NA para todo CLOZE y el veredicto
# degenera en WARN_DIV_INDET con 0 versiones evaluadas: la puerta se ejecuta pero
# no mide nada. Esta función cubre ese caso. (Ver regla #22 y el informe de
# auditoría del 2026-08-06.)
fp_gaps <- function(env) {
  nms <- ls(envir = env, all.names = FALSE)
  idx <- unique(sort(as.integer(sub("^(?:sol|solucion|opciones|exsol)_p(\\d+)$", "\\1",
                                    grep("^(?:sol|solucion|opciones|exsol)_p\\d+$", nms, value = TRUE)))))
  if (length(idx) == 0L) return(character(0))
  out <- character(0)
  for (i in idx) {
    key <- NA_character_
    sn <- intersect(paste0(c("sol", "solucion"), "_p", i), nms)
    on <- intersect(paste0("opciones_p", i), nms)
    # (a) vector solución + lista de opciones -> CONTENIDO de la(s) marcada(s).
    #     A diferencia de fp_respuesta(), acepta >1 marca: un gap mchoice tiene varias.
    if (length(sn) && length(on)) {
      sol <- get(sn[1], envir = env); ops <- get(on[1], envir = env)
      marcas <- which(as.logical(sol))
      if (length(marcas) >= 1L && length(ops) == length(sol))
        key <- d(lapply(marcas, function(k) ops[[k]]))
    }
    # (b) gap num/string: el propio valor esperado es la clave.
    if (is.na(key)) {
      en <- intersect(paste0("exsol_p", i), nms)
      if (length(en)) key <- d(get(en[1], envir = env))
    }
    # (c) solo hay opciones (sin vector sol legible): huella del conjunto completo.
    if (is.na(key) && length(on)) key <- d(get(on[1], envir = env))
    if (!is.na(key)) out[paste0("p", i)] <- key
  }
  out
}

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
fps <- character(0); errs <- 0L; indet <- 0L; gap_fps <- list()
for (i in seq_len(n)) {
  setwd(tmp)
  env <- new.env(parent = globalenv())
  assign("n", NA, envir = env)  # 'n' inocua: algunas plantillas la referencian con lazy-eval
  set.seed(i * 7919L + 13L)
  okr <- tryCatch({ suppressWarnings(suppressMessages(eval(expr, envir = env))); TRUE }, error = function(e) FALSE)
  setwd(old)
  if (!okr) { errs <- errs + 1L; next }
  f <- tryCatch(fp_respuesta(env), error = function(e) NA_character_)
  g <- tryCatch(fp_gaps(env), error = function(e) character(0))
  for (nm in names(g)) gap_fps[[nm]] <- c(gap_fps[[nm]], g[[nm]])
  if (is.na(f)) { indet <- indet + 1L } else { fps <- c(fps, f) }
}

# --- Veredicto CLOZE (multi-gap): una clave POR GAP, no una sola ------------
# Se activa solo con >=2 gaps detectados; con 0 o 1 se usa la ruta clasica de
# abajo, que queda intacta para SCHOICE.
if (length(gap_fps) >= 2L) {
  gaps <- names(gap_fps)[order(as.integer(sub("^p", "", names(gap_fps))))]
  # Cobertura explicita: un conteo parcial que parece total es el fallo que este
  # script existe para combatir. Declarados = tipos en exclozetype.
  ectl <- grep("^exclozetype:", lines, value = TRUE)
  ecval <- if (length(ectl)) trimws(sub("^exclozetype:", "", ectl[1])) else NA_character_
  # Si el valor es una expresion R inline (`r ...`) no se puede contar estaticamente:
  # dejarlo indeterminado en vez de contar 1 y anunciar una cobertura falsa.
  n_decl <- if (is.na(ecval) || grepl("`r ", ecval, fixed = TRUE)) NA_integer_
            else length(strsplit(ecval, "|", fixed = TRUE)[[1]])
  fuente_decl <- "exclozetype"
  # Fallback si exclozetype es dinamico: contar los encabezados de parte (como V1).
  if (is.na(n_decl)) {
    np <- length(unique(regmatches(paste(lines, collapse = "\n"),
                                   gregexpr("\\*\\*Parte[[:space:]]+[0-9]+\\.", paste(lines, collapse = "\n")))[[1]]))
    if (np > 0L) { n_decl <- np; fuente_decl <- "encabezados **Parte N.**" }
  }
  cat(sprintf("Diversidad sustantiva (CLOZE): %d gap(s) medidos%s | %d version(es) evaluadas | errores=%d\n",
              length(gaps),
              if (!is.na(n_decl)) sprintf(" de %d declarados (%s)", n_decl, fuente_decl) else "",
              max(lengths(gap_fps)), errs))
  if (!is.na(n_decl) && length(gaps) < n_decl)
    cat(sprintf("  NOTA DE COBERTURA: %d gap(s) sin medir (sus claves no siguen la convencion sol_pN/opciones_pN/exsol_pN, o solo exponen un vector de posiciones, que NO se puede usar como huella sin violar la regla #22). Su diversidad queda SIN VERIFICAR.\n",
                n_decl - length(gaps)))
  fijos <- character(0); bajos <- character(0)
  for (gp in gaps) {
    v <- gap_fps[[gp]]; u <- length(unique(v))
    umb <- max(2L, ceiling(length(v) * 0.30))
    estado <- if (u == 1L) "FIJA" else if (u < umb) "baja" else "ok"
    if (u == 1L) fijos <- c(fijos, gp) else if (u < umb) bajos <- c(bajos, gp)
    cat(sprintf("  gap %-4s clave: %3d valor(es) unico(s) de %3d  [%s]\n", gp, u, length(v), estado))
  }
  if (length(fijos) == length(gaps)) {
    cat("ERR_DIV_COSMETICA: la clave de TODOS los gaps es INVARIANTE entre versiones — la diversidad es solo cosmetica.\n")
    cat("  -> Aleatorizar los datos numericos/el contenido de la opcion correcta. PROHIBIDO valores fijos hardcoded o PNGs estaticos como opciones.\n")
    cat("  -> Ver .claude/rules/diversidad-sustantiva.md\n")
    quit(status = 1)
  }
  if (length(fijos) > 0L) {
    cat(sprintf("WARN_DIV_GAP_FIJO: clave invariante en %s. No bloquea, pero cada gap fijo debe declararse y justificarse en el reporte (V8).\n",
                paste(fijos, collapse = ", ")))
    cat("  -> Ver .claude/rules/diversidad-sustantiva.md\n")
    quit(status = 0)
  }
  if (length(bajos) > 0L) {
    cat(sprintf("WARN_DIV_BAJA: la clave varia poco en %s. Ampliar rangos de aleatorizacion.\n",
                paste(bajos, collapse = ", ")))
    quit(status = 0)
  }
  cat("PASS: la clave de cada gap varia suficientemente (diversidad sustantiva confirmada por gap).\n")
  quit(status = 0)
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
