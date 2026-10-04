# Regresión de barras-campeonato-baloncesto-n3: la clave y las figuras las protege un
# verificador que lee el DIBUJO (código TikZ emitido) y no las variables del .Rmd.
#
# Origen (2026-10-04): con mutantes, `sol` en la opción equivocada pasó 100/100 por
# validar_multisemilla.R y validar_coherencia_matematica.R, y un distractor con la matriz de
# la clave pasó 75/100. Ese mismo día, el PDF de 10 preguntas del Semillero mostraba en las
# 10 las figuras de una sola versión (falta de fig_id). Cada mutante de abajo reproduce uno de
# esos defectos y DEBE hacer fallar al verificador; si alguno pasa, el verificador dejó de
# medir lo que promete.

library(testthat)

raiz <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
dir_ej <- file.path(raiz, "A-Produccion", "01-En-PreDesarrollo", "barras-campeonato-baloncesto-n3")
verificador <- file.path(dir_ej, "verificar_dibujo_clave.R")
rmd <- file.path(dir_ej, "barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd")

correr <- function(archivo) {
  out <- suppressWarnings(system2("Rscript", c(shQuote(verificador), shQuote(archivo)), stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, salida = out)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("el .Rmd vigente pasa el verificador del dibujo en N = 100", {
  skip_if_not(file.exists(rmd), "subproyecto ausente")
  r <- correr(rmd)
  expect_equal(r$status, 0L, info = paste(tail(r$salida, 12), collapse = "\n"))
  expect_true(any(grepl("RESULTADO: 100/100 versiones sin fallos", r$salida)))
})

ancla <- 'letra_correcta <- c("a", "b", "c", "d")[which(sol)]   # solo para logs; nunca en Solution\n'
mutantes <- list(
  a_clave_en_otra_letra = list(ancla, paste0(ancla, "sol <- c(sol[4], sol[1:3]); solucion <- sol\n")),
  b_segunda_clave       = list(ancla, paste0(ancla, "mats_op[[which(!sol)[1]]] <- mats_op[[which(sol)]]\n")),
  c_sin_fig_id          = list('name = paste0("grafica_solucion_", fig_id)', 'name = "grafica_solucion"'),
  d_solution_valor_falso = list('" (datos de la tabla) y, para el grado ", g2, ", ", fmt_num(M[2, 1])',
                                '" (datos de la tabla) y, para el grado ", g2, ", ", fmt_num(M[2, 1] + 1)'),
  e_figura_solucion_ajena = list("include_tikz(tikz_barras_opcion(mats_op[[k_sol]],",
                                 "include_tikz(tikz_barras_opcion(mats_op[[if (k_sol == 1L) 2L else 1L]],"),
  # Sin el rótulo, la apilada E8 es idéntica a barras superpuestas de los datos correctos.
  f_sin_rotulo          = list('subtitulo_op <- function(f) if (es_canonica) NULL else paste0("(", nombre_formato(f), ")")',
                               'subtitulo_op <- function(f) NULL'),
  g_rotulo_invertido    = list('subtitulo_op <- function(f) if (es_canonica) NULL else paste0("(", nombre_formato(f), ")")',
                               'subtitulo_op <- function(f) if (es_canonica) NULL else paste0("(", nombre_formato(setdiff(c("agrupada", "apilada"), f)), ")")')
)

for (nm in names(mutantes)) {
  test_that(paste("el verificador detecta el mutante", nm), {
    skip_if_not(file.exists(rmd), "subproyecto ausente")
    txt <- paste(readLines(rmd, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    m <- mutantes[[nm]]
    expect_equal(lengths(regmatches(txt, gregexpr(m[[1]], txt, fixed = TRUE))), 1L,
                 info = "el ancla del mutante ya no existe en el .Rmd: actualizar el test")
    # Las guardas internas abortan el render ante (a) y (b); se retiran en la copia para
    # medir al verificador EXTERNO, que es lo que este test protege.
    txt <- sub("stopifnot(identical(as.vector(mats_op[[which(sol)]]), as.vector(M)))", "", txt, fixed = TRUE)
    txt <- sub("stopifnot(length(unique(opciones_mezcladas)) == 4L, sum(sol) == 1L)", "", txt, fixed = TRUE)
    txt <- sub("if (!isTRUE(get0(\".exams_generation_mode\"", "if (FALSE && !isTRUE(get0(\".exams_generation_mode\"", txt, fixed = TRUE)
    f <- file.path(tempdir(), paste0("mutante_", nm, ".Rmd"))
    writeLines(sub(m[[1]], m[[2]], txt, fixed = TRUE), f, useBytes = TRUE)
    r <- correr(f)
    expect_equal(r$status, 1L, info = paste(tail(r$salida, 6), collapse = "\n"))
    pasan <- as.integer(sub("^RESULTADO: ([0-9]+)/.*$", "\\1", grep("^RESULTADO:", r$salida, value = TRUE)))
    # Los mutantes del rótulo no pueden fallar en la canónica (allí el rótulo debe faltar):
    # deben pasar exactamente las versiones canónicas y fallar todas las demás.
    n_canon <- as.integer(sub("^.*TRUE=([0-9]+).*$", "\\1", grep("^  canonica", r$salida, value = TRUE)))
    esperado <- if (startsWith(nm, "f_") || startsWith(nm, "g_")) n_canon else 0L
    expect_identical(pasan, esperado, info = paste("versiones sin fallos con el mutante:", tail(r$salida, 1)))
    unlink(f)
  })
}
