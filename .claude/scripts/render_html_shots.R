#!/usr/bin/env Rscript
# =============================================================================
# render_html_shots.R — Pipeline de captura visual masiva de HTMLs (R-exams)
# Soporte del agente auditor-visual-html (revisión visual de varias decenas de
# versiones para detectar errores de todo tipo, incl. responsividad en móvil).
# =============================================================================
# Uso:
#   Rscript .claude/scripts/render_html_shots.R <archivo.Rmd> [N] [outdir] [viewports]
#     N         : nº de versiones a renderizar (default 24)
#     outdir    : carpeta de salida (default "shots_html" junto al .Rmd)
#     viewports : anchos separados por coma (default "360,1024" = móvil,desktop)
#
# Produce, por cada versión v01..vNN y cada ancho W:
#   <outdir>/vNN_W.png      (screenshot full-page, recortado)
# y contact sheets de triaje:
#   <outdir>/contact_<W>.png
# y un manifiesto:
#   <outdir>/manifest.txt   (versiones OK / fallidas / posible truncamiento)
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Uso: render_html_shots.R <archivo.Rmd> [N] [outdir] [viewports]")
rmd <- normalizePath(args[1], mustWork = TRUE)
N   <- if (length(args) >= 2) as.integer(args[2]) else 24L
outdir <- if (length(args) >= 3 && nzchar(args[3])) args[3] else file.path(dirname(rmd), "shots_html")
viewports <- if (length(args) >= 4) as.integer(strsplit(args[4], ",")[[1]]) else c(360L, 1024L)

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
outdir <- normalizePath(outdir)

# --- Localizar navegador headless y magick ---
browser <- Sys.which("chromium")
if (!nzchar(browser)) browser <- Sys.which("google-chrome-stable")
if (!nzchar(browser)) browser <- Sys.which("google-chrome")
if (!nzchar(browser)) stop("No se encontró chromium ni google-chrome-stable para capturar HTML.")
magick <- Sys.which("magick")

suppressWarnings(suppressMessages(library(exams)))

# Margen (px) bajo la altura de ventana `h`: si tras recortar el blanco
# sobrante el contenido llega a menos de este margen del borde inferior de
# la ventana de captura, no se puede distinguir "el documento termina justo
# ahi" de "el documento sigue y la captura lo trunco en silencio" -- chromium
# --screenshot recorta SIEMPRE al tamano exacto de --window-size (verificado:
# el PNG crudo mide wxh pase lo que pase con el contenido), nunca captura mas
# alla del viewport. Calibrado el 2026-08-17 con un fixture de texto realista
# a 360px de ancho (~6948px de contenido real): a h=6000L (default viejo) el
# margen tras el recorte fue de 14px -> TRUNCADO; a h=8000L (default nuevo)
# el margen fue de 1052px -> OK. Hueco amplio entre ambos casos.
TRUNC_MARGIN_PX <- 50L

shot <- function(html, png, w, h = 8000L, trunc_margin = TRUNC_MARGIN_PX) {
  ok <- system2(browser, c(
    "--headless", "--no-sandbox", "--disable-gpu", "--hide-scrollbars",
    "--force-device-scale-factor=1",
    sprintf("--window-size=%d,%d", w, h),
    sprintf("--screenshot=%s", png),
    sprintf("file://%s", html)
  ), stdout = FALSE, stderr = FALSE)
  truncado <- FALSE
  if (file.exists(png) && nzchar(magick)) {
    # Recortar el blanco sobrante inferior; +repage normaliza el lienzo.
    system2(magick, c(shQuote(png), "-trim", "+repage", shQuote(png)),
            stdout = FALSE, stderr = FALSE)
    # Detector de truncamiento: compara el alto tras el recorte contra el
    # alto de ventana `h`. Ver comentario de TRUNC_MARGIN_PX arriba.
    h_trim <- tryCatch({
      out <- system2(magick, c("identify", "-format", "%h", shQuote(png)),
                      stdout = TRUE, stderr = FALSE)
      as.integer(out[1])
    }, error = function(e) NA_integer_)
    if (!is.na(h_trim) && (h - h_trim) <= trunc_margin) truncado <- TRUE
  }
  list(ok = file.exists(png), truncado = truncado)
}

ok_seeds <- integer(0); fail_seeds <- integer(0); trunc_seeds <- character(0)
cat(sprintf("Renderizando %d versiones de %s\n  viewports: %s\n  salida: %s\n",
            N, basename(rmd), paste(viewports, collapse = ","), outdir))

for (s in seq_len(N)) {
  set.seed(s)
  nm <- sprintf("v%02d", s)
  html <- tryCatch({
    exams2html(rmd, n = 1, dir = outdir, name = nm)
    f <- file.path(outdir, paste0(nm, "1.html"))
    if (file.exists(f)) f else NA_character_
  }, error = function(e) NA_character_)
  if (is.na(html)) { fail_seeds <- c(fail_seeds, s); cat(sprintf("  v%02d: ERROR render\n", s)); next }
  html <- normalizePath(html)
  got <- TRUE
  truncado_en <- character(0)
  for (w in viewports) {
    r <- shot(html, file.path(outdir, sprintf("%s_%d.png", nm, w)), w)
    got <- got && r$ok
    if (isTRUE(r$truncado)) truncado_en <- c(truncado_en, sprintf("%dpx", w))
  }
  if (got) { ok_seeds <- c(ok_seeds, s) } else { fail_seeds <- c(fail_seeds, s) }
  if (length(truncado_en)) trunc_seeds <- c(trunc_seeds, sprintf("%s@%s", nm, paste(truncado_en, collapse = "+")))
  cat(sprintf("  v%02d: %s%s\n", s, if (got) "OK" else "shot FALLÓ",
              if (length(truncado_en)) sprintf("  ⚠ POSIBLE TRUNCAMIENTO (%s) — el contenido llega casi al borde de la ventana, subir h",
                                                paste(truncado_en, collapse = ", ")) else ""))
}

# --- Contact sheets de triaje (uno por viewport) ---
if (nzchar(magick) && length(ok_seeds)) {
  for (w in viewports) {
    imgs <- file.path(outdir, sprintf("v%02d_%d.png", ok_seeds, w))
    imgs <- imgs[file.exists(imgs)]
    if (length(imgs)) {
      contact <- file.path(outdir, sprintf("contact_%d.png", w))
      system2(magick, c("montage", shQuote(imgs),
        "-tile", "6x", "-geometry", "240x900+4+4", "-background", "white",
        "-title", shQuote(sprintf("%s  @%dpx  (%d versiones)", basename(rmd), w, length(imgs))),
        shQuote(contact)), stdout = FALSE, stderr = FALSE)
      if (file.exists(contact)) cat(sprintf("Contact sheet @%dpx: %s\n", w, contact))
    }
  }
}

# --- Manifiesto ---
manifest <- file.path(outdir, "manifest.txt")
writeLines(c(
  sprintf("rmd: %s", rmd),
  sprintf("N: %d  viewports: %s", N, paste(viewports, collapse = ",")),
  sprintf("OK (%d): %s", length(ok_seeds), paste(ok_seeds, collapse = ",")),
  sprintf("FALLIDAS (%d): %s", length(fail_seeds), paste(fail_seeds, collapse = ",")),
  sprintf("POSIBLE TRUNCAMIENTO (%d): %s", length(trunc_seeds), paste(trunc_seeds, collapse = "; "))
), manifest)
cat(sprintf("\nResumen: %d OK, %d fallidas. Manifiesto: %s\n",
            length(ok_seeds), length(fail_seeds), manifest))
if (length(trunc_seeds)) {
  cat(sprintf(paste0("\n⚠ ADVERTENCIA: %d captura(s) posiblemente truncada(s) — el contenido llegó a ",
                      "menos de %dpx del borde inferior de la ventana de captura. La auditoría visual ",
                      "sobre esas capturas puede haber omitido el final del documento. Revisar y ",
                      "considerar subir `h` en shot().\n  %s\n"),
              length(trunc_seeds), TRUNC_MARGIN_PX, paste(trunc_seeds, collapse = "\n  ")))
}
