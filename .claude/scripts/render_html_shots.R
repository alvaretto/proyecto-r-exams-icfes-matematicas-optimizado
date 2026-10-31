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
if (!requireNamespace("chromote", quietly = TRUE)) stop("No se encontró el paquete R chromote para capturar HTML.")
magick <- Sys.which("magick")

suppressWarnings(suppressMessages(library(exams)))

# Margen (px) para la red de seguridad de truncamiento: se compara el alto
# bruto del PNG con document.documentElement.scrollHeight antes de recortar el
# blanco inferior. Con chromote la captura se pide a altura completa, pero la
# comprobación mantiene visible cualquier regresión de ese contrato.
TRUNC_MARGIN_PX <- 50L

shot <- function(html, png, w, h = 8000L, trunc_margin = TRUNC_MARGIN_PX) {
  # Con --window-size por debajo de 500, Chromium 156 headless carga la página a
  # 500 px: el JavaScript de carga y --dump-dom ven innerWidth = 500, aunque
  # --screenshot vuelve a maquetar a W antes de capturar. Medido el 2026-10-31:
  # una caja al 50 % mide 160 px en la captura de 320, y las capturas de un
  # ejercicio real salen idénticas con la CLI y con CDP. Con CDP la página carga
  # ya a W, el ancho se comprueba en la página y se captura con el mismo
  # ejecutable que el script detectó arriba.
  profile_dir <- tempfile("render_html_shots_chromote_")
  dir.create(profile_dir, recursive = TRUE)
  controller <- NULL
  session <- NULL
  on.exit({
    if (!is.null(controller)) {
      try(controller$close(), silent = TRUE)
    } else if (!is.null(session)) {
      try(session$close(), silent = TRUE)
    }
    unlink(profile_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  chrome_args <- c(
    "--no-sandbox", "--disable-gpu", "--hide-scrollbars",
    "--force-device-scale-factor=1",
    paste0("--user-data-dir=", profile_dir)
  )
  controller <- chromote::Chromote$new(
    browser = chromote::Chrome$new(path = browser, args = chrome_args)
  )
  session <- controller$new_session(width = as.integer(w), height = as.integer(h), wait_ = TRUE)
  mobile <- w < 600L
  emular_viewport <- function(alto) {
    session$Emulation$setDeviceMetricsOverride(
      width = as.integer(w), height = as.integer(alto), deviceScaleFactor = 1,
      mobile = mobile, wait_ = TRUE
    )
    if (mobile) {
      # Chromium 156 ensancha el layout viewport a 980px (o al ancho de un
      # elemento fijo) en páginas móviles sin respuesta. La segunda emulación
      # preserva W como viewport CSS real sin modificar el HTML de R/exams.
      session$Emulation$setDeviceMetricsOverride(
        width = as.integer(w), height = as.integer(alto), deviceScaleFactor = 1,
        mobile = FALSE, wait_ = TRUE
      )
    }
  }
  emular_viewport(h)
  file_url <- paste0("file://", utils::URLencode(normalizePath(html), reserved = FALSE))
  session$go_to(file_url, delay = 0.1, wait_ = TRUE)

  medir_entero <- function(expresion) {
    resultado <- session$Runtime$evaluate(expresion, returnByValue = TRUE, wait_ = TRUE)
    as.integer(resultado$result$value)
  }
  comprobar_ancho <- function() {
    ancho <- medir_entero("window.innerWidth")
    if (length(ancho) != 1L || is.na(ancho) || ancho != w) {
      stop(sprintf("Viewport incorrecto: se pidió %d px y se midió %s px.",
                   w, if (length(ancho) == 1L && !is.na(ancho)) ancho else "NA"))
    }
    ancho
  }

  # Se comprueba antes y después del ajuste final para no ocultar un viewport
  # incorrecto por una página sin meta viewport.
  comprobar_ancho()
  page_height <- medir_entero("document.documentElement.scrollHeight")
  if (length(page_height) != 1L || is.na(page_height) || page_height < 1L) {
    stop("No se pudo medir document.documentElement.scrollHeight para la captura.")
  }
  emular_viewport(page_height)
  viewport_width <- comprobar_ancho()
  captura <- session$Page$captureScreenshot(
    format = "png", fromSurface = TRUE, captureBeyondViewport = TRUE,
    clip = list(x = 0, y = 0, width = as.integer(w), height = page_height, scale = 1),
    wait_ = TRUE
  )
  writeBin(jsonlite::base64_dec(captura$data), png)

  truncado <- FALSE
  if (file.exists(png) && nzchar(magick)) {
    # El detector conserva utilidad como red de seguridad: ahora compara el
    # alto bruto capturado contra la altura real medida de la página, no contra
    # la antigua ventana fija. Después se mantiene el recorte de blanco.
    h_capturado <- tryCatch({
      out <- system2(magick, c("identify", "-format", "%h", shQuote(png)),
                      stdout = TRUE, stderr = FALSE)
      as.integer(out[1])
    }, error = function(e) NA_integer_)
    if (!is.na(h_capturado) && h_capturado + trunc_margin < page_height) truncado <- TRUE
    system2(magick, c(shQuote(png), "-trim", "+repage", shQuote(png)),
            stdout = FALSE, stderr = FALSE)
  }
  list(ok = file.exists(png), truncado = truncado,
       viewport_width = viewport_width, page_height = page_height)
}

ok_seeds <- integer(0); fail_seeds <- integer(0); trunc_seeds <- character(0)
viewports_medidos <- character(0)
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
    viewports_medidos <- c(viewports_medidos,
                           sprintf("%s@%d=%d", nm, w, r$viewport_width))
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
  sprintf("ANCHOS MEDIDOS: %s", paste(viewports_medidos, collapse = "; ")),
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
