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
#   <outdir>/manifest.txt   (versiones OK / fallidas)
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

shot <- function(html, png, w, h = 6000L) {
  ok <- system2(browser, c(
    "--headless", "--no-sandbox", "--disable-gpu", "--hide-scrollbars",
    "--force-device-scale-factor=1",
    sprintf("--window-size=%d,%d", w, h),
    sprintf("--screenshot=%s", png),
    sprintf("file://%s", html)
  ), stdout = FALSE, stderr = FALSE)
  if (file.exists(png) && nzchar(magick)) {
    # Recortar el blanco sobrante inferior; +repage normaliza el lienzo.
    system2(magick, c(shQuote(png), "-trim", "+repage", shQuote(png)),
            stdout = FALSE, stderr = FALSE)
  }
  file.exists(png)
}

ok_seeds <- integer(0); fail_seeds <- integer(0)
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
  for (w in viewports) got <- shot(html, file.path(outdir, sprintf("%s_%d.png", nm, w)), w) && got
  if (got) { ok_seeds <- c(ok_seeds, s) } else { fail_seeds <- c(fail_seeds, s) }
  cat(sprintf("  v%02d: %s\n", s, if (got) "OK" else "shot FALLÓ"))
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
  sprintf("FALLIDAS (%d): %s", length(fail_seeds), paste(fail_seeds, collapse = ","))
), manifest)
cat(sprintf("\nResumen: %d OK, %d fallidas. Manifiesto: %s\n",
            length(ok_seeds), length(fail_seeds), manifest))
