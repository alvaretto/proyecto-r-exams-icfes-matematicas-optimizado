# =============================================================================
# Test de regresión — Numeración de las preguntas en las plantillas oficiales
# solpcielo.tex y solpcielo_cloze.tex (regla #26, Error 45)
# =============================================================================
# Defecto que vigila (Error 45 de .claude/docs/patrones-errores-conocidos.md): pandoc escribe toda lista
# numerada de Markdown como de primer nivel (\def\labelenumi y, si no empieza en 1,
# \setcounter{enumi}{N-1}). Si la plantilla numera las preguntas con `enumerate`, ese contador es el
# de las preguntas: la pregunta siguiente sale con otro número y la lista del ejercicio sale con letras.
# Las dos plantillas oficiales numeran con listas propias de enumitem (\newlist) desde el 2026-11-14.
#
# Dos capas, cada una con su mutante (la misma plantilla devuelta a `enumerate` debe reprobar):
#   1. estática, sin LaTeX: los ejercicios van dentro de la lista propia `preguntas` y las opciones
#      dentro de `opciones`;
#   2. de punta a punta: exams2pdf() con tres ejercicios escritos aquí y lectura, con pdftotext, de los
#      números impresos.
# El caso está construido para poder fallar: las listas van en la PRIMERA pregunta y empiezan en 3
# (enunciado) y en 5 (Solution). Ninguna deja el contador en el valor que le tocaba a la pregunta 2;
# una lista que empezara en 2 escondería el defecto (medido el 2026-11-14).

library(testthat)

repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE)
  if (length(r) == 1 && dir.exists(r)) r else getwd()
}, error = function(e) getwd())

PERIFERICOS <- file.path(repo_root, "A-Produccion", "perifericos")
OFICIALES <- c("solpcielo.tex", "solpcielo_cloze.tex")

leer_tex <- function(f) readLines(f, warn = FALSE, encoding = "UTF-8")

# TRUE si la plantilla define las listas propias y envuelve en ellas los ejercicios y las opciones.
numera_con_listas_propias <- function(lineas) {
  codigo <- !grepl("^\\s*%", lineas)
  hay <- function(patron) any(grepl(patron, lineas[codigo], fixed = TRUE))
  marca <- grep("\\exinput{exercises}", lineas, fixed = TRUE)
  if (length(marca) != 1) return(FALSE)
  entorno <- function(idx, verbo) {
    idx <- idx[codigo[idx] & grepl(paste0("^\\s*\\\\", verbo, "\\{"), lineas[idx])]
    if (!length(idx)) return(NA_character_)
    sub(paste0("^\\s*\\\\", verbo, "\\{([^}]*)\\}.*$"), "\\1", lineas[idx[1]])
  }
  abre   <- entorno(rev(seq_len(marca - 1)), "begin")                 # el \begin más cercano ANTES de los ejercicios
  cierra <- entorno(seq(marca + 1, length(lineas)), "end")            # el \end más cercano DESPUÉS
  identical(abre, "preguntas") && identical(cierra, "preguntas") &&
    hay("\\newlist{preguntas}{enumerate}{1}") && hay("\\newlist{opciones}{enumerate}{1}") &&
    hay("\\newenvironment{answerlist}{\\begin{opciones}}{\\end{opciones}}")
}

# Mutante: la lista `preguntas` vuelve a ser un `enumerate` corriente (comparte el contador enumi).
volver_a_enumerate <- function(lineas) {
  de <- c("\\newlist{preguntas}{enumerate}{1}", "\\setlist[preguntas]{label=\\arabic*.}")
  i <- which(lineas == de[1])
  stopifnot("MUTANTE MAL CONSTRUIDO: no aparece una sola vez la definición de la lista `preguntas`" =
              length(i) == 1 && identical(lineas[i + 1], de[2]))
  c(lineas[seq_len(i - 1)], "\\newenvironment{preguntas}{\\begin{enumerate}}{\\end{enumerate}}",
    lineas[seq(i + 2, length(lineas))])
}

herramientas <- c("pdflatex", "pdftotext", "pandoc")
faltan <- herramientas[!nzchar(Sys.which(herramientas))]
if (!requireNamespace("exams", quietly = TRUE)) faltan <- c(faltan, "exams")
listo <- length(faltan) == 0
motivo <- paste("faltan:", paste(faltan, collapse = ", "))

# Tres ejercicios mínimos. El primero trae una lista «3., 4.» en el enunciado y otra que empieza en 5 en la Solution.
escribir_ejercicios <- function(d) {
  cabeza <- c("```{r, echo = FALSE, results = \"hide\"}", "library(exams)", "sol <- c(TRUE, FALSE, FALSE, FALSE)", "```", "")
  cola <- function(nombre) c("", "Meta-information", "================", paste0("exname: ", nombre), "extype: schoice",
                             "exsolution: `r mchoice2string(sol)`", "exshuffle: TRUE")
  opciones <- function(x) c("", "Answerlist", "----------", paste("*", x))
  lista <- c(cabeza, "Question", "========",
             "Un grupo ya hizo los dos primeros pasos de su plan y le faltan estos:", "",
             "3. Sumar las tres cantidades.", "4. Anotar el total en la cartelera.", "",
             "¿Cuántos pasos le faltan?",
             opciones(c("Dos", "Tres", "Cuatro", "Cinco")), "",
             "Solution", "========", "Le faltan los pasos tres y cuatro, que son dos.", "",
             "5. Como último control, se revisa la cartelera.",
             opciones(c("Verdadero", "Falso", "Falso", "Falso")), cola("lista"))
  texto <- function(nombre, filas) c(cabeza, "Question", "========",
             sprintf("Una caja tiene %d filas de cinco fichas cada una. ¿Cuántas fichas hay en total?", filas),
             opciones(as.character(filas * 5 + c(0, 5, -5, 10))), "",
             "Solution", "========", sprintf("Se multiplica: %d por cinco da %d.", filas, filas * 5),
             opciones(c("Verdadero", "Falso", "Falso", "Falso")), cola(nombre))
  rutas <- file.path(d, c("lista.Rmd", "texto_a.Rmd", "texto_b.Rmd"))
  writeLines(lista, rutas[1]); writeLines(texto("texto_a", 4L), rutas[2]); writeLines(texto("texto_b", 7L), rutas[3])
  rutas
}

texto_pdf <- function(pdf) system2("pdftotext", c("-layout", shQuote(pdf), "-"), stdout = TRUE)

# Número impreso delante de «Escenario:», que es como las dos plantillas encabezan cada pregunta.
numeros_impresos <- function(tx) {
  m <- regmatches(tx, regexec("^ {0,14}([0-9]+)\\. +Escenario:", tx))
  as.integer(vapply(Filter(length, m), `[`, "", 2))
}

compilar_con <- function(plantilla, ejercicios) {
  d <- tempfile("num_"); dir.create(d)
  suppressWarnings(exams::exams2pdf(ejercicios, n = 1, dir = d, name = "x", template = plantilla,
                                    seed = matrix(1:3, nrow = 1), verbose = FALSE))
  pdf <- list.files(d, pattern = "\\.pdf$", full.names = TRUE)
  if (length(pdf)) texto_pdf(pdf[1]) else character(0)
}

# -----------------------------------------------------------------------------
# 1. Capa estática
# -----------------------------------------------------------------------------
test_that("las dos plantillas oficiales existen y numeran preguntas y opciones con listas propias", {
  for (p in OFICIALES) {
    f <- file.path(PERIFERICOS, p)
    expect_true(file.exists(f), info = p)
    expect_true(numera_con_listas_propias(leer_tex(f)), info = p)
  }
})

test_that("mutante estático: la plantilla devuelta a `enumerate` no pasa por lista propia", {
  for (p in OFICIALES) {
    mut <- volver_a_enumerate(leer_tex(file.path(PERIFERICOS, p)))
    expect_false(numera_con_listas_propias(mut), info = p)
  }
  # y un `enumerate` escrito directamente alrededor de los ejercicios tampoco pasa
  lineas <- leer_tex(file.path(PERIFERICOS, OFICIALES[1]))
  directo <- sub("^(\\s*\\\\(begin|end))\\{preguntas\\}", "\\1{enumerate}", lineas)
  expect_false(identical(directo, lineas))
  expect_false(numera_con_listas_propias(directo))
})

# -----------------------------------------------------------------------------
# 2. De punta a punta
# -----------------------------------------------------------------------------
test_that("con las plantillas oficiales, una lista numerada no cambia el número de la pregunta siguiente", {
  skip_if_not(listo, motivo)
  ej <- escribir_ejercicios({d <- tempfile("ej_"); dir.create(d); d})
  for (p in OFICIALES) {
    tx <- compilar_con(file.path(PERIFERICOS, p), ej)
    expect_identical(numeros_impresos(tx), 1:3, info = p)
    todo <- paste(tx, collapse = "\n")
    expect_match(todo, "3\\. +Sumar las tres cantidades", info = p)         # la lista conserva los números del autor
    expect_match(todo, "4\\. +Anotar el total en la cartelera", info = p)
    expect_match(todo, "5\\. +Como último control", info = p)
  }
})

test_that("mutante de punta a punta: con `enumerate`, las preguntas salen 1, 5, 6 y la lista pierde sus números", {
  skip_if_not(listo, motivo)
  dir_ej <- tempfile("ej_"); dir.create(dir_ej)
  ej <- escribir_ejercicios(dir_ej)
  for (p in OFICIALES) {
    f <- file.path(dir_ej, sub("\\.tex$", "_mutante.tex", p))
    writeLines(volver_a_enumerate(leer_tex(file.path(PERIFERICOS, p))), f)
    tx <- compilar_con(f, ej)
    # la lista de la Solution de la pregunta 1 empieza en 5: deja el contador en 4 y la pregunta siguiente sale «5.»
    expect_identical(numeros_impresos(tx), c(1L, 5L, 6L), info = p)
    expect_false(grepl("3\\. +Sumar las tres cantidades", paste(tx, collapse = "\n")), info = p)
  }
})
