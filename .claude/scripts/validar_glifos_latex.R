#!/usr/bin/env Rscript
# =============================================================================
# validar_glifos_latex.R — detector de glifos Unicode que ROMPEN pdflatex
# =============================================================================
#
# PROBLEMA QUE RESUELVE
# ---------------------
# Un `.Rmd` con el carácter `✓` (U+2713) literal en texto visible NO compila:
#
#   ! LaTeX Error: Unicode character ✓ (U+2713) not set up for use with LaTeX
#
# El defecto es INVISIBLE en HTML (no pasa por LaTeX) y ningún validador del
# arsenal miraba los caracteres del fuente, así que sobrevivió meses en
# ejercicios de `03-En-Produccion/`.
#
# QUÉ SE MIDIÓ (2026-08-15) — este detector NO se apoya en suposiciones
# ---------------------------------------------------------------------
# Se renderizó `exams2pdf()` con la plantilla por defecto de R/exams sobre un
# `.Rmd` mínimo por cada glifo, en TEXTO VISIBLE: 110 glifos distintos + 2
# controles (ASCII puro y tildes españolas), y 14 repetidos en contexto math.
#
#   COMPILAN (26): × ÷ ² ³ ° º § « » · ± ª — – … ‰ • “ ” ‘ ’ ← ↑ → £ € ã ö ø œ
#                  y las tildes españolas (á é í ó ú ñ ü ¿ ¡) — regla #7.
#   FALLAN  (80+): ✓ ✔ ✗ ✘ ✅ ❌ √ ≤ ≥ ≠ − ∈ ∩ ∪ ⊂ ∑ ∏ ∫ ∞ ≈ ∅ ∀ ∃ ≡ ∝ ∠ ∴
#                  π α β γ θ λ μ σ Ω Δ · ↔ ↺ ⇒ ⇔ · ‣ ▪ · │ ─ ┌ ┐ └ ┘ · ₁ ₂
#                  · ℹ ⚡ · IPA (ɛ ɑ ɔ ɶ ɒ ʌ ɤ ɯ ɨ ʉ) · todos los emoji · U+FE0F
#
# TRES HALLAZGOS QUE CONTRADICEN LA INTUICIÓN:
#   1. Las flechas ← ↑ → COMPILAN, pero ↔ ↺ ⇒ ⇔ NO. No vale "las flechas rompen".
#   2. El modo MATEMÁTICO NO salva: `$a ≤ b$` falla igual que `≤` en prosa.
#      Por eso el detector no exceptúa zonas math.
#   3. Un glifo rompedor SOLO en un comentario R es INOCUO (medido: compila).
#      Por eso la ZONA es parte del veredicto, no un detalle.
#
# CRITERIO DEL DETECTOR
# ---------------------
# Rangos Unicode con al menos un rompedor MEDIDO, menos una allowlist de las
# excepciones MEDIDAS que sí compilan. Cero falsos positivos sobre lo medido.
#
# Uso:
#   Rscript validar_glifos_latex.R <archivo.Rmd> [--json]
# Exit:
#   0 = sin glifos rompedores en zona visible
#   1 = hay glifos rompedores en zona visible (ERR_GLIFO_LATEX)
#   2 = error de invocación
# =============================================================================

# --- Rangos ROMPEDORES (cada uno con >=1 rompedor medido) --------------------
RANGOS_ROMPEDORES <- list(
  c(0x0250, 0x02AF),   # IPA Extensions        — medidos 10/10 fallan
  c(0x0370, 0x03FF),   # Greek and Coptic      — medidos 10/10 fallan
  c(0x2023, 0x2023),   # ‣ hyphen bullet       — medido, falla (• U+2022 NO)
  c(0x2070, 0x209F),   # Super/Subscripts      — ₁ ₂ fallan (² ³ son Latin-1)
  c(0x2100, 0x214F),   # Letterlike Symbols    — ℹ falla
  c(0x2190, 0x21FF),   # Arrows                — con excepciones (ver abajo)
  c(0x2200, 0x22FF),   # Mathematical Operators— medidos 12/12 fallan
  c(0x2500, 0x257F),   # Box Drawing           — medidos 6/6 fallan
  c(0x25A0, 0x25FF),   # Geometric Shapes      — ▪ falla
  c(0x2600, 0x27BF),   # Misc Symbols/Dingbats — ✓ ✔ ✗ ✘ ✅ ❌ ⚡ fallan
  c(0xFE00, 0xFE0F),   # Variation Selectors   — U+FE0F falla
  c(0x1F300, 0x1FAFF)  # Emoji                 — todos los medidos fallan
)

# --- Excepciones MEDIDAS que SÍ compilan dentro de un rango rompedor ---------
EXCEPCIONES_OK <- c(0x2190L, 0x2191L, 0x2192L)   # ← ↑ →

es_rompedor <- function(cp) {
  if (cp %in% EXCEPCIONES_OK) return(FALSE)
  for (r in RANGOS_ROMPEDORES) if (cp >= r[1] && cp <= r[2]) return(TRUE)
  FALSE
}

# --- Clasificación de ZONA y SEVERIDAD --------------------------------------
# La zona NO es un detalle de presentación: es lo que decide si bloquea, y está
# CALIBRADA sobre los 63 archivos del repo que tenían glifos rompedores, midiendo
# `exams2pdf()` del original contra el mismo archivo con los glifos sustituidos
# (prueba causal: sólo cuenta como "roto por glifo" si el original falla Y el
# sustituido compila). Resultado sobre los casos concluyentes:
#
#   ZONA            ROTOS/CONCLUYENTES   PRECISIÓN   SEVERIDAD
#   markdown             16/16             100 %     ERR  (bloqueante)
#   sólo codigo_r         1/24               4 %     WARN (96 % falsos positivos)
#   yaml_meta             0/1                 -      WARN (evidencia insuficiente)
#   comentario_r          -                   -      informativo (medido inocuo)
#
# Por eso `codigo_r` NO bloquea: un glifo dentro de una cadena R puede no
# emitirse nunca. El caso real que lo ilustra, y que justifica avisar igual:
#     if (identical(typ, "pandoc")) "≤" else "\\le"
# el MISMO carácter es inocuo o letal según la rama, y eso no es decidible
# estáticamente. Bloquear ahí habría marcado 23 archivos que sí compilan.
# `fence_otro` (bloque markdown sin chunk R) queda en WARN por falta de medición.
clasificar_zonas <- function(lineas) {
  zonas <- character(length(lineas))
  en_chunk_r <- FALSE; en_fence_otro <- FALSE; en_meta <- FALSE
  for (i in seq_along(lineas)) {
    ln <- lineas[i]
    if (grepl("^Meta-information\\s*$", ln)) en_meta <- TRUE
    if (grepl("^\\s*```", ln)) {
      if (en_chunk_r || en_fence_otro) {
        if (grepl("^\\s*```+\\s*$", ln)) { en_chunk_r <- FALSE; en_fence_otro <- FALSE }
      } else {
        if (grepl("^\\s*```+\\s*\\{", ln)) en_chunk_r <- TRUE else en_fence_otro <- TRUE
      }
      zonas[i] <- "fence_marca"; next
    }
    zonas[i] <- if (en_meta) "yaml_meta"
      else if (en_chunk_r) { if (grepl("^\\s*#", ln)) "comentario_r" else "codigo_r" }
      else if (en_fence_otro) "fence_otro"
      else "markdown"
  }
  zonas
}

analizar_glifos <- function(ruta) {
  con <- file(ruta, open = "r", encoding = "UTF-8")
  lineas <- readLines(con, warn = FALSE); close(con)
  zonas <- clasificar_zonas(lineas)

  bloqueantes <- list(); advertencias <- list(); comentarios <- list()
  for (i in seq_along(lineas)) {
    chars <- strsplit(lineas[i], "")[[1]]
    if (!length(chars)) next
    cps <- utf8ToInt(lineas[i])
    if (is.null(cps)) next
    malos <- unique(chars[vapply(cps, es_rompedor, logical(1))])
    if (!length(malos)) next
    reg <- list(linea = i, zona = zonas[i], glifos = malos,
                codepoints = vapply(malos, function(g) sprintf("U+%04X", utf8ToInt(g)), ""),
                texto = substr(trimws(lineas[i]), 1, 100))
    if (identical(zonas[i], "comentario_r"))      comentarios[[length(comentarios) + 1]] <- reg
    else if (identical(zonas[i], "markdown"))     bloqueantes[[length(bloqueantes) + 1]] <- reg
    else                                          advertencias[[length(advertencias) + 1]] <- reg
  }
  list(bloqueantes = bloqueantes, advertencias = advertencias, comentarios = comentarios)
}

# --- CLI ---------------------------------------------------------------------
if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  ruta <- args[!grepl("^--", args)][1]
  if (is.na(ruta) || !file.exists(ruta)) {
    cat("Uso: Rscript validar_glifos_latex.R <archivo.Rmd> [--json]\n"); quit(status = 2)
  }
  r <- analizar_glifos(ruta)

  if ("--json" %in% args) {
    fmt <- function(l) paste0("[", paste(vapply(l, function(x) sprintf(
      '{"linea":%d,"zona":"%s","codepoints":["%s"]}', x$linea, x$zona,
      paste(x$codepoints, collapse = '","')), ""), collapse = ","), "]")
    cat(sprintf('{"bloqueantes":%s,"advertencias":%s,"comentarios":%s}\n',
                fmt(r$bloqueantes), fmt(r$advertencias), fmt(r$comentarios)))
  } else {
    if (length(r$advertencias))
      cat(sprintf("WARN_GLIFO_LATEX: %d linea(s) con glifos rompedores en codigo R / metadatos.\n  No bloquea: dentro de una cadena R puede no emitirse nunca (medido: solo 1 de 24 rompia).\n  Revisar si esa cadena llega al output en alguna rama.\n",
                  length(r$advertencias)))
    if (length(r$bloqueantes)) {
      cat("ERR_GLIFO_LATEX: glifos que ROMPEN pdflatex en texto Markdown visible\n")
      for (x in r$bloqueantes)
        cat(sprintf("  linea %d [%s]: %s (%s)\n     %s\n", x$linea, x$zona,
                    paste(x$glifos, collapse = " "), paste(x$codepoints, collapse = " "), x$texto))
      cat("  -> exams2pdf()/exams2nops() FALLARAN. HTML no lo detecta (no pasa por LaTeX).\n")
      cat("  -> Fix: sustituir por texto/ASCII o por el comando LaTeX equivalente en math.\n")
      cat("     Equivalencias:  ")
      cat("checkmark -> (quitar) | ")
      cat("<= >= != -> ASCII | ")
      cat("$\\le$ $\\ge$ $\\neq$ $\\pi$ $\\Delta$ $\\sqrt{}$ $\\cap$ $\\in$ en math | ")
      cat("emoji -> quitar\n")
      cat("     OJO: el modo math NO salva al glifo literal: $a <glifo> b$ falla igual (medido).\n")
    }
    if (length(r$comentarios))
      cat(sprintf("INFO_GLIFO_COMENTARIO: %d linea(s) con glifos rompedores en comentarios R (INOCUO, medido: compila)\n",
                  length(r$comentarios)))
    if (!length(r$bloqueantes) && !length(r$advertencias) && !length(r$comentarios))
      cat("OK: sin glifos que rompan pdflatex\n")
  }
  quit(status = if (length(r$bloqueantes)) 1 else 0)
}
