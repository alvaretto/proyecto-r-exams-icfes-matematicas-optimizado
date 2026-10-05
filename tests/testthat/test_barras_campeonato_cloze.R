# Regresión de la versión CLOZE de barras-campeonato-baloncesto-n3 (6 partes, cloze/).
# Gemelo de test_barras_campeonato_clave.R (SCHOICE). Las seis claves las protege
# cloze/verificar_dibujo_clave_cloze.R, que lee el DIBUJO (TikZ emitido) y el TEXTO tejido,
# no las variables del .Rmd.
#
# Origen (2026-10-04, ciclo del mega-prompt CLOZE):
#  - Los mutantes del ciclo anterior (14) solo constaban en el HANDOFF; ningún test los
#    volvía a correr. Cada mutante de abajo reproduce un defecto posible y DEBE hacer
#    fallar al verificador; si alguno pasa, el verificador dejó de medir lo que promete.
#  - Los Semilleros de cloze/ apuntaban a otros .Rmd (el SCHOICE y un ejercicio de
#    teorema de Pitágoras) y pedían exams2nops, que no admite cloze: el profesor habría
#    impreso otro ejercicio creyendo imprimir el CLOZE.

library(testthat)

raiz <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
dir_cl <- file.path(raiz, "A-Produccion", "01-En-PreDesarrollo", "barras-campeonato-baloncesto-n3", "cloze")
verificador <- file.path(dir_cl, "verificar_dibujo_clave_cloze.R")
rmd <- file.path(dir_cl, "barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd")
`%||%` <- function(a, b) if (is.null(a)) b else a

correr <- function(archivo) {
  out <- suppressWarnings(system2("Rscript", c(shQuote(verificador), shQuote(archivo)), stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, salida = out)
}

test_that("el .Rmd CLOZE vigente pasa el verificador en N = 100", {
  skip_if_not(file.exists(rmd), "subproyecto ausente")
  r <- correr(rmd)
  expect_equal(r$status, 0L, info = paste(tail(r$salida, 12), collapse = "\n"))
  expect_true(any(grepl("RESULTADO: 100/100 versiones sin fallos", r$salida)))
})

mutantes <- list(
  a_clave_p1_en_otra_grafica = list("sol_p1 <- as.integer(sol)\n", "sol_p1 <- as.integer(sol)[c(2, 3, 4, 1)]\n"),
  b_distractor_igual_a_clave = list("e7_op <- c(list(NULL), e7_distr)[orden]\n",
                                    "e7_op <- c(list(NULL), e7_distr)[orden]\nmats_op[[which(!sol)[1]]] <- M\n"),
  c_figura_sin_fig_id        = list('name = paste0("diagrama_", c("a", "b", "c", "d")[i], "_", fig_id)',
                                    'name = paste0("diagrama_", c("a", "b", "c", "d")[i], if (i == 4) "" else paste0("_", fig_id))'),
  d_clave_p2_desplazada      = list("resp_p2 <- M[1, j2] + M[2, j2]\n", "resp_p2 <- M[1, j2] + M[2, j2] + 1L\n"),
  e_p5_verdad_invertida      = list('  c(v = "En una barra apilada, solo el segmento inferior empieza en cero.",\n    f = "En una barra apilada, el dato de un segmento superior es la altura a la que llega su borde superior."),',
                                    '  c(f = "En una barra apilada, solo el segmento inferior empieza en cero.",\n    v = "En una barra apilada, el dato de un segmento superior es la altura a la que llega su borde superior."),'),
  # Detractor FASE 2C ciclo 3, objeción 2: P3, P4 y P6 no tenían mutante permanente.
  f_clave_p3_desplazada      = list("resp_p3 <- b3 - a3\n", "resp_p3 <- b3 - a3 + 1L\n"),
  g_clave_p4_invertida       = list('paste(sol_p4, collapse = "")', 'paste(rev(sol_p4), collapse = "")'),
  h_clave_p6_invertida       = list('paste(sol_p6, collapse = "")', 'paste(rev(sol_p6), collapse = "")'),
  # Objeción 1: la explicación de la Parte 5 debe existir (si no, la invariante no tiene guardia).
  i_p5_sin_por_que           = list('cat("\\n*Por qué:* ", paste(porque_p5, collapse = " "), "\\n\\n", sep = "")', 'cat("\\n")')
)

for (nm in names(mutantes)) {
  test_that(paste("el verificador CLOZE detecta el mutante", nm), {
    skip_if_not(file.exists(rmd), "subproyecto ausente")
    txt <- paste(readLines(rmd, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    m <- mutantes[[nm]]
    expect_equal(lengths(regmatches(txt, gregexpr(m[[1]], txt, fixed = TRUE))), 1L,
                 info = "el ancla del mutante ya no existe en el .Rmd: actualizar el test")
    # Las guardas internas abortan el tejido ante (a) y (b); se retiran en la copia para
    # medir al verificador EXTERNO, que es lo que este test protege.
    txt <- sub("stopifnot(identical(as.vector(mats_op[[which(sol)]]), as.vector(M)))", "", txt, fixed = TRUE)
    txt <- gsub("if (!isTRUE(get0(\".exams_generation_mode\"", "if (FALSE && !isTRUE(get0(\".exams_generation_mode\"", txt, fixed = TRUE)
    f <- file.path(tempdir(), paste0("mutante_cloze_", nm, ".Rmd"))
    writeLines(sub(m[[1]], m[[2]], txt, fixed = TRUE), f, useBytes = TRUE)
    r <- correr(f)
    expect_equal(r$status, 1L, info = paste(tail(r$salida, 6), collapse = "\n"))
    pasan <- as.integer(sub("^RESULTADO: ([0-9]+)/.*$", "\\1", grep("^RESULTADO:", r$salida, value = TRUE)))
    # El mutante (e) solo puede fallar en las versiones que muestran ese par: no se exige 0.
    if (nm == "e_p5_verdad_invertida") expect_true(length(pasan) == 1L && pasan < 100L)
    else expect_identical(pasan, 0L, info = paste("versiones sin fallos con el mutante:", tail(r$salida, 1)))
    unlink(f)
  })
}

test_that("los Semilleros de cloze/ renderizan el CLOZE y no piden NOPS", {
  skip_if_not(dir.exists(dir_cl), "subproyecto ausente")
  for (s in c("SemilleroUnico_v2.R", "SemilleroMoodle_v2.R")) {
    f <- file.path(dir_cl, s)
    skip_if_not(file.exists(f), paste(s, "ausente"))
    L <- readLines(f, encoding = "UTF-8", warn = FALSE)
    arch <- sub('^archivo_examen <- "([^"]+)".*$', "\\1", grep("^archivo_examen <- ", L, value = TRUE))
    expect_identical(arch, basename(rmd), info = s)
    activo <- L[!grepl("^\\s*#", L)]
    expect_false(any(grepl("exams2nops\\(", activo)), info = paste(s, ": exams2nops rechaza cloze"))
    if (any(grepl("exams2pdf\\(", activo))) expect_true(any(grepl('template = "solpcielo_cloze"', activo)), info = s)
  }
})
