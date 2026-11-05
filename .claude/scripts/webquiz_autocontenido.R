#!/usr/bin/env Rscript
# =============================================================================
# webquiz_autocontenido.R — cuestionario HTML que funciona sin internet
# -----------------------------------------------------------------------------
# `exams2forms::exams2webquiz()` deja un HTML con imágenes, estilos y scripts
# incrustados, pero carga MathJax desde https://mathjax.rstudio.com por JavaScript.
# Sin internet las fórmulas salen como TeX crudo: `\(f(x) = x^{2}\)` (medido en
# Chromium sin red, 2026-11-04). El material del aula debe ser autocontenido.
# Pasar `math_method = "mathml"` no basta: las opciones de respuesta van en HTML
# crudo, y pandoc no convierte la matemática que hay dentro.
#
# Este script incrusta MathJax 3 (salida SVG, sin fuentes externas) en el HTML y
# comprueba en un navegador sin red que todas las fórmulas se componen. Un HTML que
# no pasa esa comprobación no se da por bueno aunque exista.
#
# La copia de MathJax no se versiona (2 MB): se descarga una vez a una caché local
# y se acepta solo si su SHA-256 coincide con el fijado aquí.
#
# Uso:
#   Rscript .claude/scripts/webquiz_autocontenido.R archivo.Rmd [archivo2.Rmd ...]
#           [--n 3] [--dir salida] [--nombre quiz] [--titulo "Texto"] [--semilla 1]
#           [--revision]
#
#   --n         versiones por ejercicio.
#   --revision  vista del docente (`auto = TRUE`): respuestas ya marcadas, solución,
#               tolerancias y nombre del archivo. NUNCA entregar a estudiantes.
#
# Salida: 0 WEBQUIZ_OK · 1 WEBQUIZ_FALLA (hay fórmulas sin componer o recursos
# externos) · 2 WEBQUIZ_NO_VERIFICABLE (faltan navegador, MathJax o argumentos).
# =============================================================================

MATHJAX_URL <- "https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-svg.js"
MATHJAX_SHA256 <- "d4295dc33744836935c1399feece5159577b34c5c8ffb9f1c6324cd82e03a882"

# Bloque que escribe rmarkdown::html_document() cuando `self_contained = TRUE`.
PATRON_CARGADOR <- "(?s)<!-- dynamically load mathjax for compatibility with self-contained -->[[:space:]]*<script>.*?</script>"

CONFIG_MATHJAX <- paste0(
  "<script>window.MathJax={",
  "tex:{inlineMath:[['\\\\(','\\\\)']],displayMath:[['\\\\[','\\\\]']]},",
  "svg:{fontCache:'global'}",
  "};</script>"
)

ruta_mathjax <- function() {
  file.path(Sys.getenv("WEBQUIZ_MATHJAX_CACHE", "~/.cache/rexams-webquiz"), "tex-svg-3.2.2.js")
}

sha256_archivo <- function(ruta) digest::digest(file = ruta, algo = "sha256")

asegurar_mathjax <- function(ruta = ruta_mathjax(), url = MATHJAX_URL, sha = MATHJAX_SHA256) {
  ruta <- path.expand(ruta)
  if (file.exists(ruta) && identical(sha256_archivo(ruta), sha)) return(ruta)
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  ok <- tryCatch(utils::download.file(url, tmp, mode = "wb", quiet = TRUE) == 0L, error = function(e) FALSE)
  if (!ok) stop("no se pudo descargar MathJax de ", url)
  if (!identical(sha256_archivo(tmp), sha)) stop("MathJax descargado con SHA-256 distinto del fijado")
  file.copy(tmp, ruta, overwrite = TRUE)
  ruta
}

leer_html <- function(ruta) paste(readLines(ruta, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

incrustar_mathjax <- function(html, js) {
  m <- regexpr(PATRON_CARGADOR, html, perl = TRUE)
  if (m[1L] < 0L) stop("no se encontró el cargador de MathJax: cambió la plantilla de rmarkdown")
  if (grepl("</script", tolower(js), fixed = TRUE)) stop("el JavaScript no se puede incrustar: contiene </script")
  antes <- substr(html, 1L, m[1L] - 1L)
  despues <- substr(html, m[1L] + attr(m, "match.length"), nchar(html))
  nuevo <- paste0(antes, CONFIG_MATHJAX, "\n<script>", js, "</script>", despues)
  if (grepl("mathjax.rstudio.com", paste0(antes, despues), fixed = TRUE)) stop("queda otra referencia a mathjax.rstudio.com")
  nuevo
}

sin_scripts <- function(html) {
  html <- gsub("(?s)<script\\b[^>]*>.*?</script>", " ", html, perl = TRUE, ignore.case = TRUE)
  gsub("(?s)<style\\b[^>]*>.*?</style>", " ", html, perl = TRUE, ignore.case = TRUE)
}

# Recursos que el navegador pediría a la red. Un enlace <a href> no es un recurso.
recursos_externos <- function(html) {
  patrones <- c(
    "<(?:script|img|iframe|audio|video|source|embed)\\b[^>]*\\bsrc=[\"']?(?:https?:)?//[^\"'\\s>]+",
    "<link\\b[^>]*\\bhref=[\"']?(?:https?:)?//[^\"'\\s>]+",
    "url\\([\"']?(?:https?:)?//[^)\"']+",
    "@import\\s+[\"'](?:https?:)?//[^\"']+"
  )
  # Los estilos sí se revisan: un @import o una fuente remota se pedirían a la red.
  sin_js <- gsub("(?s)<script\\b[^>]*>.*?</script>", " ", html, perl = TRUE, ignore.case = TRUE)
  etiquetas <- regmatches(html, gregexpr("<script\\b[^>]*>", html, perl = TRUE, ignore.case = TRUE))[[1L]]
  unique(unlist(c(
    lapply(patrones, function(p) regmatches(sin_js, gregexpr(p, sin_js, perl = TRUE, ignore.case = TRUE))[[1L]]),
    lapply(patrones[1L], function(p) regmatches(etiquetas, regexpr(p, etiquetas, perl = TRUE, ignore.case = TRUE)))
  )))
}

navegador <- function() {
  c <- Sys.which(c("chromium", "chromium-browser", "google-chrome-stable", "google-chrome"))
  c <- c[nzchar(c)]
  if (length(c)) unname(c[1L]) else ""
}

dom_sin_red <- function(html_ruta, nav = navegador()) {
  salida <- tempfile(fileext = ".html")
  on.exit(unlink(salida))
  # Toda petición a un host falla: es lo que ve un celular en modo avión.
  args <- c("--headless=new", "--no-sandbox", "--disable-gpu",
            shQuote("--host-rules=MAP * ~NOTFOUND"), "--virtual-time-budget=20000",
            "--dump-dom", shQuote(paste0("file://", normalizePath(html_ruta))))
  system2(nav, args, stdout = salida, stderr = FALSE)
  leer_html(salida)
}

medir_dom <- function(dom) {
  cuerpo <- gsub("(?s)<(select|textarea)\\b[^>]*>.*?</\\1>", " ", sin_scripts(dom), perl = TRUE, ignore.case = TRUE)
  contar <- function(p, x) {
    m <- gregexpr(p, x, perl = TRUE)[[1L]]
    if (m[1L] < 0L) 0L else length(m)
  }
  list(
    compuestas = contar("<mjx-container\\b", cuerpo),
    con_error = contar("data-mjx-error|<mjx-merror|mathcolor=\"red\"", cuerpo),
    tex_crudo = contar("\\\\\\(|\\\\\\[", gsub("<[^>]+>", " ", cuerpo, perl = TRUE))
  )
}

evaluar_medicion <- function(medida, externos) {
  fallos <- character(0)
  if (length(externos) > 0L) fallos <- c(fallos, paste("recursos externos:", paste(externos, collapse = " ")))
  if (medida$tex_crudo > 0L) fallos <- c(fallos, sprintf("%d fórmula(s) en TeX crudo", medida$tex_crudo))
  if (medida$con_error > 0L) fallos <- c(fallos, sprintf("%d fórmula(s) con error de MathJax", medida$con_error))
  fallos
}

argumento <- function(args, nombre, defecto) {
  i <- match(nombre, args)
  if (is.na(i) || i == length(args)) return(defecto)
  args[i + 1L]
}

salir <- function(codigo, estado) {
  cat(codigo, "\n", sep = "")
  quit(status = estado)
}

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  con_valor <- c("--n", "--dir", "--nombre", "--titulo", "--semilla")
  valores <- unlist(lapply(con_valor, function(a) argumento(args, a, NULL)))
  rmds <- setdiff(args[!grepl("^--", args)], valores)
  if (length(rmds) == 0L || !all(file.exists(rmds))) {
    cat("Uso: Rscript webquiz_autocontenido.R archivo.Rmd [...] [--n 3] [--dir salida] [--nombre quiz] [--titulo t] [--semilla 1] [--revision]\n")
    salir("WEBQUIZ_NO_VERIFICABLE", 2L)
  }
  revision <- "--revision" %in% args
  n <- as.integer(argumento(args, "--n", 3L))
  dir_salida <- argumento(args, "--dir", "salida_webquiz")
  nombre <- argumento(args, "--nombre", if (revision) "quiz_revision" else "quiz")
  titulo <- argumento(args, "--titulo", if (revision) "REVISIÓN DOCENTE: NO ENTREGAR" else "Cuestionario")
  semilla <- as.integer(argumento(args, "--semilla", 1L))

  nav <- navegador()
  if (!nzchar(nav)) {
    cat("Falta un navegador Chromium: sin él no se puede comprobar la composición sin red.\n")
    salir("WEBQUIZ_NO_VERIFICABLE", 2L)
  }
  js_ruta <- tryCatch(asegurar_mathjax(), error = function(e) {
    cat(conditionMessage(e), "\n")
    NULL
  })
  if (is.null(js_ruta)) salir("WEBQUIZ_NO_VERIFICABLE", 2L)

  suppressPackageStartupMessages({
    library(exams)
    library(exams2forms)
  })
  rmds <- normalizePath(rmds)
  if (length(unique(dirname(rmds))) != 1L) {
    cat("Todos los .Rmd deben estar en el mismo directorio.\n")
    salir("WEBQUIZ_NO_VERIFICABLE", 2L)
  }
  dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)
  dir_salida <- normalizePath(dir_salida)
  set.seed(semilla)
  rutas <- exams2webquiz(basename(rmds), n = n, dir = dir_salida, edir = dirname(rmds)[1L],
                         name = nombre, title = titulo, browse = FALSE,
                         auto = revision, quiet = TRUE)
  html <- rutas[[2L]]

  js <- paste(readLines(js_ruta, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  texto <- incrustar_mathjax(leer_html(html), js)
  writeLines(texto, html, useBytes = TRUE)
  unlink(rutas[[1L]])

  externos <- recursos_externos(texto)
  medida <- medir_dom(dom_sin_red(html, nav))
  cat(sprintf("HTML: %s (%.1f MB)\n", html, file.size(html) / 1048576))
  cat(sprintf("Sin red: %d fórmulas compuestas, %d en TeX crudo, %d con error · %d recursos externos\n",
              medida$compuestas, medida$tex_crudo, medida$con_error, length(externos)))
  fallos <- evaluar_medicion(medida, externos)
  if (length(fallos) > 0L) {
    cat(paste0("- ", fallos), sep = "\n")
    salir("WEBQUIZ_FALLA", 1L)
  }
  if (revision) cat("Vista del docente: trae las respuestas marcadas. No entregar a estudiantes.\n")
  salir("WEBQUIZ_OK", 0L)
}
