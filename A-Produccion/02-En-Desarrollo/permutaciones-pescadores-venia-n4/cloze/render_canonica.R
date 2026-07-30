# Localiza una semilla que produzca la instancia canónica (contexto 1, n = 4)
# de la variante CLOZE y la renderiza a HTML + PDF para la revisión humana
# (paso 11 del workflow).
#
# Método: se evalúa el chunk `data_generation` real del .Rmd con set.seed(s)
# para cada s candidato; cuando ctx_idx == 1 y n == 4 se vuelve a fijar la misma
# semilla y se llama exams2html/exams2pdf, que consumen el RNG desde el mismo
# estado. La coincidencia se CONFIRMA buscando el enunciado oficial en el HTML,
# no se da por supuesta.

suppressPackageStartupMessages(library(exams))

RMD <- "permutaciones_pescadores_metacognitivo_formulacion_n4_cloze_v1.Rmd"
OUT <- "verif_render/canonica"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

CANON <- paste0(
  "En una obra de teatro, hay 4 personas que interpretan pescadores. ",
  "Al finalizar la obra, los 4 pescadores deben ubicarse en fila en ",
  "el escenario y hacer una venia ante el público."
)

src   <- paste(readLines(RMD, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
chunk <- sub("(?s).*?```\\{r data_generation[^}]*\\}(.*?)```.*", "\\1", src, perl = TRUE)
stopifnot(!identical(chunk, src), nzchar(trimws(chunk)))
expr <- parse(text = chunk)

candidatas <- integer(0)
for (s in 1:400) {
  e <- new.env()
  set.seed(s)
  r <- try(suppressWarnings(eval(expr, envir = e)), silent = TRUE)
  if (inherits(r, "try-error")) next
  if (isTRUE(e$ctx_idx == 1L) && isTRUE(e$n == 4L)) candidatas <- c(candidatas, s)
  if (length(candidatas) >= 5L) break
}
cat("Semillas candidatas (ctx_idx=1, n=4):", paste(candidatas, collapse = ", "), "\n")
stopifnot(length(candidatas) > 0L)

elegida <- NA_integer_
for (s in candidatas) {
  set.seed(s)
  invisible(suppressWarnings(
    exams2html(RMD, n = 1, dir = OUT, edir = ".", name = "canonica")
  ))
  h <- file.path(OUT, "canonica1.html")
  txt <- paste(readLines(h, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  if (grepl(CANON, txt, fixed = TRUE)) { elegida <- s; break }
}

if (is.na(elegida)) {
  cat("NO se confirmó la canónica en el HTML con ninguna candidata.\n")
  quit(status = 1)
}

set.seed(elegida)
invisible(suppressWarnings(
  exams2pdf(RMD, n = 1, dir = OUT, edir = ".", name = "canonica")
))

cat("CONFIRMADO con semilla", elegida, "\n")
cat("  HTML:", file.path(OUT, "canonica1.html"), "\n")
cat("  PDF :", file.path(OUT, "canonica1.pdf"), "\n")
