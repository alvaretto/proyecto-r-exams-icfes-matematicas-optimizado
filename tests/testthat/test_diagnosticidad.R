# Tests de `.claude/scripts/validar_diagnosticidad.R` (sonda V9).
#
# Lo que estos tests protegen es el MARGEN RELATIVO de la sonda H1, añadido el
# 2026-08-06. La v1 de H1 solo miraba el orden ("¿es la correcta la única más
# larga?") y no la distancia: un gap cuyas opciones ya se habían igualado
# deliberadamente —8 caracteres medianos sobre 115, el 7%— seguía reportando
# 100% y quedaba bloqueado por una diferencia que ningún estudiante puede usar
# como heurística. Sin estos tests, el criterio puede revertirse sin que nada
# avise, en cualquiera de las dos direcciones:
#   - demasiado laxo  -> vuelve a colarse el ítem resoluble "por la más larga";
#   - demasiado estricto -> vuelve a bloquearse un ítem correcto.

library(testthat)

RAIZ <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
SCRIPT_DIAG <- file.path(RAIZ, ".claude/scripts/validar_diagnosticidad.R")

# Construye un .Rmd mínimo con un gap schoice de 4 opciones cuya opción correcta
# lleva `extra_chars` caracteres de más respecto de las otras tres (que miden
# `base_chars` y son de longitud pareja entre sí).
fixture_rmd <- function(base_chars, extra_chars) {
  relleno <- function(n) paste(rep("a", n), collapse = "")
  f <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo = FALSE, results = \"hide\"}",
    "v <- sample(100:999, 1)",
    sprintf('opciones_p1 <- c(paste0("correcta ", v, " %s"),', relleno(base_chars + extra_chars)),
    sprintf('                 paste0("erronea1 ", v, " %s"),', relleno(base_chars)),
    sprintf('                 paste0("erronea2 ", v, " %s"),', relleno(base_chars)),
    sprintf('                 paste0("erronea3 ", v, " %s"))', relleno(base_chars)),
    "sol_p1 <- c(TRUE, FALSE, FALSE, FALSE)",
    "```",
    "",
    "Question",
    "========",
    "**Parte 1.** Pregunta.",
    "##ANSWER1##"
  ), f)
  f
}

correr <- function(rmd, n = 20) {
  salida <- suppressWarnings(system2("Rscript", c(SCRIPT_DIAG, rmd, "--n", n),
                                     stdout = TRUE, stderr = TRUE))
  list(texto = paste(salida, collapse = "\n"),
       exit = attr(salida, "status") %||% 0L)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("El script existe y es ejecutable", {
  expect_true(file.exists(SCRIPT_DIAG))
})

test_that("H1 caza la correcta MUCHO más larga (margen muy por encima del 15%)", {
  # 100 de base + 40 extra = 40% de margen: la heurística "la más larga" resuelve
  # el ítem sin leerlo.
  r <- correr(fixture_rmd(base_chars = 100, extra_chars = 40))
  expect_match(r$texto, "ERR_DIAG_SUPERFICIAL")
  expect_equal(r$exit, 1L)
})

test_that("H1 NO caza opciones ya igualadas (margen muy por debajo del 15%)", {
  # 100 de base + 5 extra = 5% de margen. La correcta SIGUE siendo la única más
  # larga (el orden no cambia), pero por una diferencia inservible.
  r <- correr(fixture_rmd(base_chars = 100, extra_chars = 5))
  expect_false(grepl("ERR_DIAG_SUPERFICIAL", r$texto))
  expect_equal(r$exit, 0L)
})

test_that("El PASS por margen deja constancia explícita en la NOTA DE ORDEN", {
  # Que no dispare no puede leerse como "no hay señal": el script debe decir que
  # la correcta ocupa el extremo en el 100% de las versiones y por cuánto.
  r <- correr(fixture_rmd(base_chars = 100, extra_chars = 5))
  expect_match(r$texto, "NOTA DE ORDEN")
  expect_match(r$texto, "unica mas larga en el 100%")
})

test_that("El umbral del margen es configurable con --margen", {
  rmd <- fixture_rmd(base_chars = 100, extra_chars = 5)
  salida <- suppressWarnings(system2("Rscript", c(SCRIPT_DIAG, rmd, "--n", 20, "--margen", 2),
                                     stdout = TRUE, stderr = TRUE))
  # Con el umbral bajado al 2%, el mismo archivo que pasa al 15% ahora dispara.
  expect_match(paste(salida, collapse = "\n"), "ERR_DIAG_SUPERFICIAL")
})

test_that("Sin gaps de selección única el veredicto es indeterminado, no un falso PASS", {
  f <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r data_generation, echo = FALSE, results = \"hide\"}",
    "x <- sample(1:100, 1)",
    "```",
    "",
    "Question",
    "========",
    "¿Cuánto vale x?"
  ), f)
  r <- correr(f, n = 5)
  expect_match(r$texto, "WARN_DIAG_INDET")
  expect_equal(r$exit, 0L)
  unlink(f)
})

# =============================================================================
# H3b — molde uniforme de opciones ("¿Cuál es ...?")
# =============================================================================
# Origen: dry-run de MAT-2026-1-010 (2026-08-10). Cuando las cuatro opciones
# comparten primera palabra, DOS de las tres sondas quedan ciegas y el script
# lo callaba:
#   - `pw` descarta el "¿" inicial, asi que las 4 opciones dan "cuál";
#   - H2 exige que la clave sea la UNICA con su prefijo -> 0% por construccion;
#   - la guarda de H3 exige >=2 prefijos distintos -> `pwc` queda vacio y, bajo
#     `if (length(pwc) >= 5L)`, su linea NI SIQUIERA SE IMPRIME.
# Resultado: PASS con H2=0% y sin fila H3, que se lee como "sin senal" cuando
# en realidad es "sin medicion". H3b mide lo mismo que H3 (invariancia del tipo
# de clave entre versiones) leyendo el CONTENIDO normalizado en vez del prefijo.

# 4 opciones con molde uniforme. Si `clave_fija`, la clave es siempre el mismo
# tipo; si no, se sortea de un pool de 4 -> ~25-40% esperado.
fixture_molde_uniforme <- function(clave_fija) {
  f <- tempfile(fileext = ".Rmd")
  nd <- c('"¿Cuál es la ubicación que le corresponde a cada hijo dentro del lote?"',
          '"¿Cuál es el perímetro de la porción que recibe cada hijo?"',
          '"¿Cuál es el largo de la porción que recibe cada hijo?"',
          '"¿Cuál es la cantidad de cerca para separar las porciones?"')
  clave <- if (clave_fija) nd[1] else sprintf("sample(c(%s), 1)", paste(nd, collapse = ", "))
  writeLines(c(
    "```{r data_generation, echo = FALSE, results = \"hide\"}",
    "lg <- sample(40:60, 1); an <- sample(100:140, 1); nh <- sample(4:9, 1)",
    "d <- c(paste0(\"¿Cuál es el área del lote de \", lg, \" por \", an, \" metros?\"),",
    "       paste0(\"¿Cuál es el perímetro del lote de \", lg, \" por \", an, \" metros?\"),",
    "       paste0(\"¿Cuál es la fracción del lote que recibe cada uno de los \", nh, \" hijos?\"),",
    "       paste0(\"¿Cuál es el área que recibe cada uno de los \", nh, \" hijos?\"))",
    sprintf("opciones_p1 <- c(%s, sample(d, 3))", clave),
    "sol_p1 <- c(TRUE, FALSE, FALSE, FALSE)",
    "```",
    "",
    "Question",
    "========",
    "**Parte 1.** ¿Cuál pregunta NO es posible responder?",
    "##ANSWER1##"
  ), f)
  f
}

test_that("La ceguera de H2/H3 por prefijo uniforme se DECLARA, no se silencia", {
  r <- correr(fixture_molde_uniforme(clave_fija = FALSE), n = 30)
  # Lo esencial: que el 0% de H2 no pueda leerse como "no hay señal".
  expect_match(r$texto, "H2/H3 CIEGAS")
  expect_match(r$texto, "ausencia de medicion")
  expect_match(r$texto, "no son aplicables")
})

test_that("H3b caza el tipo de clave invariante que H2 y H3 no pueden ver", {
  r <- correr(fixture_molde_uniforme(clave_fija = TRUE), n = 30)
  # Control de que el fixture prueba lo que dice: H2 tiene que salir en 0% y
  # H3 no debe haberse medido. Si alguna de las dos midiera, el caso no sería
  # el de ceguera y el test estaría verificando otra cosa.
  expect_match(r$texto, "H2/H3 CIEGAS")
  expect_false(grepl("H3 veredicto", r$texto, fixed = TRUE))
  # Y la sonda de relevo sí dispara, con exit real 1 (bloqueante).
  expect_match(r$texto, "H3b tipo de clave invariante")
  expect_match(r$texto, "ERR_DIAG_SUPERFICIAL")
  expect_equal(r$exit, 1L)
})

test_that("H3b NO caza cuando el tipo de clave se sortea de un pool", {
  r <- correr(fixture_molde_uniforme(clave_fija = FALSE), n = 30)
  expect_match(r$texto, "H3b contenido")
  expect_false(grepl("ERR_DIAG_SUPERFICIAL", r$texto, fixed = TRUE))
  expect_equal(r$exit, 0L)
})

test_that("H3b NO bloquea cuando H3 sí es aplicable (calibración de relevo)", {
  # Los prefijos del fixture de H1 son distintos (correcta/erronea1/2/3), así
  # que H2 y H3 SÍ aplican ahí y H3b no debe gobernar el caso. Sin esta
  # calibración, H3b ponía en ROJO un fixture que existe para probar que H1
  # no dispara — es decir, cambiaba el veredicto de un caso ya revisado.
  r <- correr(fixture_rmd(base_chars = 100, extra_chars = 5), n = 20)
  expect_false(grepl("H2/H3 CIEGAS", r$texto, fixed = TRUE))
  expect_false(grepl("ERR_DIAG_SUPERFICIAL", r$texto, fixed = TRUE))
  expect_equal(r$exit, 0L)
})
