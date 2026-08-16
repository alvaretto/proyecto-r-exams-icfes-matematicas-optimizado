# =============================================================================
# HECHO MEDIDO: knitr::is_latex_output() es FALSE en TODOS los pipelines de
# R/exams, exams2pdf() y exams2nops() incluidos.
# .claude/rules/familias-soluciones-rmd.md (regla #21, Familia 2)
# .claude/rules/markdown-imagenes-pdf.md   (regla #18, Patron B retirado)
# .claude/rules/codigo-rmd.md              (regla interna #1)
#
# Por que existe: hasta el 2026-08-15 la regla #21 afirmaba que bajo R-exams
# `knitr::is_latex_output()` era "el UNICO discriminador (TRUE = PDF/NOPS;
# FALSE = HTML/Moodle/DOCX)". Es falso. Medido con una sonda que escribe el
# valor a disco desde data_generation, ejecutada por los cinco pipelines:
#   html FALSE | pdf FALSE | docx FALSE | nops FALSE | moodle FALSE
#
# Causa arquitectonica: R/exams SIEMPRE teje el .Rmd a Markdown y despues
# llama a pandoc para convertir ese .md al destino final. Durante el knit no
# existe todavia un destino LaTeX que knitr pueda detectar. Quien enruta es
# pandoc, por TIPO DE BLOQUE (raw latex sobrevive solo a LaTeX; raw html solo
# a HTML).
#
# Consecuencia que motivo la correccion: todo patron documentado del tipo
#   if (is_latex_output()) <latex> else <html>
# ejecuta SIEMPRE la rama else, y esa rama se DESCARTA al escribir LaTeX ->
# la figura desaparece del PDF sin error ni warning.
#
# Esta suite fija el hecho por dos vias independientes:
#   (1) RUNTIME: lo vuelve a medir con exams (se salta si falta el paquete).
#   (2) DOCUMENTAL: ninguna regla puede volver a mostrar el condicional sin
#       declarar al lado que la condicion es FALSE. La guarda es una FUNCION
#       DE LA RUTA para poder ejercitarla con mutantes en tempdir().
# =============================================================================
library(testthat)

repo_root <- local({
  r <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(r) && length(r) == 1 && dir.exists(r)) r else normalizePath("../..")
})

# --- (1) RUNTIME -------------------------------------------------------------
test_that("RUNTIME: is_latex_output() es FALSE en los pipelines de R/exams", {
  skip_if_not_installed("exams")

  d <- file.path(tempdir(), "islatex_probe")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(d, "valores.tsv")
  if (file.exists(out)) file.remove(out)
  Sys.setenv(HFOUT = out)
  rmd <- file.path(d, "sonda.Rmd")

  writeLines(c(
    "Question",
    "========",
    "",
    "```{r data_generation, echo=FALSE, results='hide'}",
    "val <- tryCatch(knitr::is_latex_output(), error = function(e) NA)",
    "cat(paste0(Sys.getenv('HFPIPE'), '\\t', as.character(val), '\\n'),",
    "    file = Sys.getenv('HFOUT'), append = TRUE)",
    "a <- 2; b <- 3",
    "```",
    "",
    "Cuanto es `r a` + `r b`?",
    "",
    "Solution",
    "========",
    "",
    "Es `r a+b`.",
    "",
    "Meta-information",
    "================",
    "exname: sonda-islatex",
    "extype: num",
    "exsolution: `r a+b`",
    "extol: 0.01"
  ), rmd)

  medido <- character(0)

  Sys.setenv(HFPIPE = "html")
  exams::exams2html(rmd, n = 1, dir = file.path(d, "o_html"))
  medido <- c(medido, "html")

  # exams2pandoc(type='latex') ejerce el destino LaTeX sin necesitar pdflatex.
  Sys.setenv(HFPIPE = "latex")
  exams::exams2pandoc(rmd, n = 1, type = "latex", dir = file.path(d, "o_latex"))
  medido <- c(medido, "latex")

  if (nzchar(Sys.which("pdflatex"))) {
    Sys.setenv(HFPIPE = "pdf")
    exams::exams2pdf(rmd, n = 1, dir = file.path(d, "o_pdf"),
                     texdir = file.path(d, "td_pdf"))
    medido <- c(medido, "pdf")
  }

  expect_true(file.exists(out), info = "La sonda no escribio ningun valor")
  filas <- strsplit(readLines(out, warn = FALSE), "\t")
  valores <- vapply(filas, function(x) x[2], character(1))
  pipes   <- vapply(filas, function(x) x[1], character(1))

  # El nucleo del hecho: NINGUN pipeline devuelve TRUE.
  expect_false(any(valores == "TRUE"),
               info = paste0("Algun pipeline devolvio TRUE: ",
                             paste(paste0(pipes, "=", valores), collapse = ", "),
                             ". Si esto pasa, R/exams cambio de arquitectura y hay que ",
                             "revisar las reglas #18, #21 y codigo-rmd #1."))
  expect_true(all(valores == "FALSE"),
              info = paste("Valores medidos:", paste(paste0(pipes, "=", valores), collapse = ", ")))
  # Y en particular el destino LaTeX, que es donde la creencia falsa hacia dano.
  expect_true("latex" %in% pipes)
  expect_equal(unique(valores[pipes == "latex"]), "FALSE")
})

# --- (2) GUARDA DOCUMENTAL ---------------------------------------------------
# Frases aceptadas como declaracion del hecho. Se admite mas de una redaccion
# a proposito: exigir UNA literal invita a que alguien la copie sin entenderla.
MARCADORES <- c("SIEMPRE FALSE", "es FALSE", "FALSE también en", "FALSE tambien en",
                "RAMA MUERTA", "NO discrimina")

# Devuelve TRUE si el archivo MUESTRA el condicional y NO declara el hecho.
regla_desprotegida <- function(path) {
  l <- readLines(path, warn = FALSE)
  muestra <- any(grepl("is_latex_output\\s*\\(\\s*\\)", l))
  if (!muestra) return(FALSE)
  declara <- any(vapply(MARCADORES, function(m) any(grepl(m, l, fixed = TRUE)), logical(1)))
  !declara
}

test_that("Ninguna regla de .claude/rules/ muestra is_latex_output() sin declarar el hecho", {
  reglas <- list.files(file.path(repo_root, ".claude", "rules"),
                       pattern = "\\.md$", full.names = TRUE)
  expect_gt(length(reglas), 0)
  desprotegidas <- Filter(regla_desprotegida, reglas)
  expect_equal(length(desprotegidas), 0L,
               info = paste("Reglas que muestran el condicional sin advertir que es FALSE:",
                            paste(basename(desprotegidas), collapse = ", ")))
})

test_that("Las tres reglas afectadas siguen documentando el hecho", {
  for (f in c("familias-soluciones-rmd.md", "markdown-imagenes-pdf.md", "codigo-rmd.md")) {
    p <- file.path(repo_root, ".claude", "rules", f)
    expect_true(file.exists(p), info = paste("Falta la regla:", f))
    l <- readLines(p, warn = FALSE)
    expect_true(any(grepl("is_latex_output", l)),
                info = paste(f, "ya no menciona is_latex_output (revisar esta suite)"))
    expect_false(regla_desprotegida(p),
                 info = paste(f, "muestra el condicional sin declarar que es FALSE"))
  }
})

test_that("CONTROL POSITIVO: la guarda caza una regla que reintroduce la creencia falsa", {
  d <- file.path(tempdir(), "reglas_mutantes")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

  # Mutante: reintroduce el condicional SIN declarar el hecho -> debe cazarse.
  mut <- file.path(d, "regla_mutante.md")
  writeLines(c("# Regla inventada", "", "```r",
               "if (knitr::is_latex_output()) {",
               "  cat(\"\\\\includegraphics{g.png}\")",
               "} else {",
               "  cat('<img src=\"g.png\" />')",
               "}", "```"), mut)
  expect_true(regla_desprotegida(mut),
              info = "La guarda NO caza una regla que reintroduce el patron sin advertencia")

  # Control negativo A: mismo condicional pero declarando el hecho.
  ok <- file.path(d, "regla_ok.md")
  writeLines(c("# Regla inventada", "",
               "Aviso: `is_latex_output()` es SIEMPRE FALSE bajo R/exams.", "", "```r",
               "if (knitr::is_latex_output()) { }", "```"), ok)
  expect_false(regla_desprotegida(ok),
               info = "La guarda da falso positivo sobre una regla correctamente advertida")

  # Control negativo B: una regla que ni menciona el tema no debe activarla.
  nada <- file.path(d, "regla_ajena.md")
  writeLines(c("# Otra regla", "", "Nada que ver con formatos de salida."), nada)
  expect_false(regla_desprotegida(nada))
})
