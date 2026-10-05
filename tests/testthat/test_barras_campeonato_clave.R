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

# ---------------------------------------------------------------------------------------
# Cotejo con la ficha de origen (2026-10-05). La instancia canónica reproduce el ítem
# MAT-2026-1-015 del cuadernillo; su ficha de alineación vive en Todo-Pajaro. Ese día se
# cotejaron a mano: la clave coincidía (C, sexto 5/7, séptimo 8/4), pero el «¿Qué evalúa?» de
# la ficha decía «8 ganados, 3 perdidos» y nada lo había detectado. Este test repite el
# cotejo: si el .Rmd o la ficha cambian por separado, falla. Se omite si Todo-Pajaro no está
# (CI); la ruta puede fijarse con TODO_PAJARO_DIR.
# ---------------------------------------------------------------------------------------
ficha_md <- file.path(
  Sys.getenv("TODO_PAJARO_DIR", file.path(dirname(raiz), "Todo-Pajaro")),
  "Alineacion-curricular-de-items", "Matematicas", "Alineacion-Curricular-de-Items-Matematicas-2026-1",
  "Alineacion-curricular-de-items-Matematicas-2026-1.md")

## Lee de la ficha la letra de la clave y los pares (ganados, perdidos) que declaran sus
## campos «Clave» y «¿Qué evalúa?»: sexto primero, séptimo después.
leer_ficha_015 <- function(lineas) {
  ini <- grep("^### MAT-2026-1-015 ", lineas)
  fin <- grep("^### MAT-2026-1-016 ", lineas)
  stopifnot(length(ini) == 1L, length(fin) == 1L, fin > ini)
  sec <- lineas[ini:(fin - 1L)]
  campo <- function(nombre) {
    l <- grep(paste0("^- \\*\\*", nombre, "\\*\\*:"), sec, value = TRUE)
    stopifnot(length(l) == 1L)
    l
  }
  pares <- function(txt) {
    m <- regmatches(txt, gregexpr("([0-9]+) ganados,? y? ?([0-9]+) perdidos", txt))[[1]]
    t(vapply(m, function(s) as.numeric(regmatches(s, gregexpr("[0-9]+", s))[[1]]), numeric(2)))
  }
  clave <- campo("Clave")
  list(letra = sub("^- \\*\\*Clave\\*\\*: *([A-D]).*$", "\\1", clave),
       clave = unname(pares(clave)), que_evalua = unname(pares(campo("¿Qué evalúa\\?"))))
}

test_that("el lector de la ficha detecta el «8 ganados, 3 perdidos» del 2026-10-04", {
  vieja <- c("### MAT-2026-1-015 — x",
             "- **¿Qué evalúa?**: … de la tabla (5 ganados, 7 perdidos) con los de la gráfica original (8 ganados, 3 perdidos) para grado 7°",
             "- **Clave**: C — Grado sexto: 5 ganados y 7 perdidos (tabla); grado séptimo: 8 ganados y 4 perdidos (gráfica).",
             "### MAT-2026-1-016 — y")
  f <- leer_ficha_015(vieja)
  expect_identical(f$letra, "C")
  expect_equal(f$clave, matrix(c(5, 8, 7, 4), 2L))
  expect_false(isTRUE(all.equal(f$que_evalua, f$clave)))   # la incoherencia que se escapó
})

test_that("la instancia canónica coincide con la ficha MAT-2026-1-015 (letra y cuatro valores)", {
  skip_if_not(file.exists(rmd), "subproyecto ausente")
  skip_if_not(file.exists(ficha_md), "Todo-Pajaro ausente (fijar TODO_PAJARO_DIR)")
  f <- leer_ficha_015(readLines(ficha_md, encoding = "UTF-8", warn = FALSE))
  expect_equal(f$que_evalua, f$clave, info = "«¿Qué evalúa?» y «Clave» de la ficha no dicen los mismos valores")

  lin <- readLines(rmd, encoding = "UTF-8", warn = FALSE)
  ini <- grep("^```\\{r data_generation", lin)
  fin <- grep("^```\\s*$", lin)
  fin <- fin[fin > ini][1]
  codigo <- parse(text = lin[(ini + 1L):(fin - 1L)])

  hay_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (hay_seed) seed_previa <- get(".Random.seed", envir = globalenv())
  hay_modo <- exists(".exams_generation_mode", envir = globalenv(), inherits = FALSE)
  if (hay_modo) modo_previo <- get(".exams_generation_mode", envir = globalenv())
  assign(".exams_generation_mode", TRUE, envir = globalenv())   # omite los test_that internos
  on.exit({
    if (hay_seed) assign(".Random.seed", seed_previa, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    if (hay_modo) assign(".exams_generation_mode", modo_previo, envir = globalenv())
    else rm(".exams_generation_mode", envir = globalenv())
  }, add = TRUE)

  e <- NULL
  for (s in 1:200) {
    set.seed(s)
    en <- new.env(parent = globalenv())
    suppressMessages(suppressWarnings(for (x in codigo) eval(x, en)))
    if (isTRUE(en$es_canonica)) { e <- en; break }
  }
  expect_false(is.null(e), info = "ninguna de 200 semillas produjo la instancia canónica")
  expect_identical(toupper(e$letra_correcta), f$letra)
  expect_equal(unname(e$M), f$clave)
  expect_equal(unname(e$mats_op[[which(e$sol)]]), f$clave)
})
