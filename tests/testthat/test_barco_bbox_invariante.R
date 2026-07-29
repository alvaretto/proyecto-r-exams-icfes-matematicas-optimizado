# =============================================================================
# Test de regresión — Invariante I-2 del subproyecto plano-cartesiano-barco-n2
# "El bounding box del casco dibujado ES la respuesta correcta"
# =============================================================================
# POR QUÉ EXISTE ESTE TEST
#
# En ese ejercicio la clave NO es un valor calculado: es el rectángulo que
# encierra al barco que dibuja `dibujar_barco()`. La identidad
#
#     bbox(casco)  ==  [x_min, x_max] x [y_min, y_max]  ==  respuesta correcta
#
# depende de que `prof(t)` valga EXACTAMENTE h/2 en su tramo central (y de que
# x recorra linealmente [x_min, x_max]). Si alguien "suaviza" el perfil para que
# el barco se vea más estilizado y el casco deja de tocar los bordes:
#
#   - exams2html/pdf/docx/nops/moodle siguen compilando sin error,
#   - validar_coherencia_matematica.R sigue dando APROBADO,
#   - validar_diversidad_sustantiva.R sigue dando PASS,
#
# porque NINGUNO de ellos mide el dibujo contra la clave. El ejercicio
# entregaría una respuesta correcta FALSA sin una sola señal roja.
# Este test es la única guarda automática de esa identidad.
#
# Ver: A-Produccion/01-En-PreDesarrollo/plano-cartesiano-barco-n2/
#        .claude/rules/barco-parametrico.md   (contrato C1-C3)
#        docs/BLUEPRINT.md                    (invariantes I-2 e I-9)
#      (el subproyecto migrará a 02-En-Desarrollo/ y 03-En-Produccion/; este test
#       localiza el .Rmd por nombre, así que sobrevive a esas promociones)
#
# DISEÑO: el test NO reimplementa el dibujo. Extrae la geometría del `.Rmd` real
# y la ejecuta, de modo que cualquier edición de `prof()` queda medida aquí.
# El espacio de parámetros sí se reconstruye (no es extraíble mecánicamente),
# y por eso se protege con dos guardas de deriva: los anclajes de texto del
# `.Rmd` y el conteo exacto de combinaciones (318).
# =============================================================================

library(testthat)

# --- Raíz del repo (robusto desde cualquier cwd) ---
repo_root <- tryCatch({
  r <- system("git rev-parse --show-toplevel", intern = TRUE, ignore.stderr = TRUE)
  if (length(r) >= 1 && dir.exists(r[1])) r[1] else NA_character_
}, error = function(e) NA_character_)
if (is.na(repo_root)) {
  cand <- c(getwd(), file.path(getwd(), "..", ".."))
  hit <- cand[file.exists(file.path(cand, ".claude", "rules", "diversidad-sustantiva.md"))]
  repo_root <- if (length(hit)) normalizePath(hit[1]) else normalizePath(getwd())
}

RMD_NAME <- "coordenadas_vertices_plano_cartesiano_metacognitivo_interpretacion_n2_schoice_v1.Rmd"

# El subproyecto migra 01-En-PreDesarrollo -> 02-En-Desarrollo -> 03-En-Produccion:
# se busca por nombre para que el test sobreviva a las promociones.
localizar_rmd <- function() {
  hits <- list.files(file.path(repo_root, "A-Produccion"),
                     pattern = paste0("^", RMD_NAME, "$"),
                     recursive = TRUE, full.names = TRUE)
  hits <- hits[!grepl("/_archivo/|/salida/|/revision/", hits)]
  hits
}

# Espacio de parámetros del ejercicio (espejo del chunk data_generation).
# Cualquier cambio en el .Rmd debe reflejarse aquí Y actualizar N_ESPERADO.
GRID_MAX    <- 10L
N_ESPERADO  <- 318L   # 222 -> 374 (P2.7, sin exclusiones de y_pool) -> 318 (P1.1/A', ratio >= 2)

combinaciones <- function() {
  out <- list(); k <- 0L
  for (ancho in 3:6) {
    alto_pool <- if (ancho >= 4L) 1L:2L else 1L          # A' (P1.1): ratio >= 2
    for (alto in alto_pool) {
      for (x_min in 1:(GRID_MAX - ancho)) {
        for (y_min in 1L:(GRID_MAX - alto)) {            # sin exclusiones (P2.7)
          k <- k + 1L
          out[[k]] <- c(x_min = x_min, x_max = x_min + ancho,
                        y_min = y_min, y_max = y_min + alto)
        }
      }
    }
  }
  do.call(rbind, out)
}

# Extrae de `dibujar_barco()` todo lo anterior al ggplot y devuelve el polígono
# del casco (hull_df). Sin dependencia de ggplot2: solo base R.
extraer_geometria_del_rmd <- function(ruta_rmd) {
  L   <- readLines(ruta_rmd, warn = FALSE)
  ini <- grep("^dibujar_barco <- function", L)
  fin <- grep("^  p <- ggplot\\(\\)", L)
  if (length(ini) != 1L || length(fin) != 1L || fin[1] <= ini[1]) {
    stop("No se pudo extraer dibujar_barco() del .Rmd: cambió su estructura. ",
         "Revisa el .Rmd y este test antes de seguir (invariante I-2).")
  }
  src <- c(L[ini[1]:(fin[1] - 1L)], "  hull_df", "}")
  env <- new.env(parent = baseenv())
  eval(parse(text = paste(src, collapse = "\n")), envir = env)
  env$dibujar_barco
}

# El comprobador de la invariante, aislado para poder verificarlo (test 3).
bbox_coincide <- function(hull, x_min, x_max, y_min, y_max) {
  isTRUE(all.equal(min(hull$x), as.numeric(x_min))) &&
  isTRUE(all.equal(max(hull$x), as.numeric(x_max))) &&
  isTRUE(all.equal(min(hull$y), as.numeric(y_min))) &&
  isTRUE(all.equal(max(hull$y), as.numeric(y_max)))
}

# =============================================================================

test_that("El .Rmd del barco existe y es único", {
  hits <- localizar_rmd()
  expect_equal(length(hits), 1L,
    info = paste("Se esperaba exactamente 1 copia de", RMD_NAME,
                 "- encontradas:", length(hits),
                 if (length(hits)) paste("->", paste(hits, collapse = " | ")) else ""))
})

test_that("El espacio de parámetros del .Rmd no cambió (guarda de deriva)", {
  hits <- localizar_rmd()
  skip_if(length(hits) != 1L, "El .Rmd no se localizó de forma única")
  txt <- paste(readLines(hits[1], warn = FALSE), collapse = "\n")

  anclas <- c(
    "ancho_barco <- pick_int(3L, 6L)"                        = "rango de ancho_barco",
    "alto_pool  <- if (ancho_barco >= 4L) 1L:2L else 1L"     = "restricción A' (ratio >= 2)",
    "stopifnot(ancho_barco / alto_barco >= 2)"               = "assert del ratio",
    "y_pool <- 1L:(grid_max - alto_barco)"                   = "y_pool sin exclusiones (P2.7)",
    "grid_max <- 10L"                                        = "tamaño de la grilla"
  )
  for (a in names(anclas)) {
    expect_true(grepl(a, txt, fixed = TRUE),
      info = paste0("Ancla ausente (", anclas[[a]], "): '", a,
                    "'. El espacio de parámetros cambió: re-deriva `combinaciones()` ",
                    "y N_ESPERADO en este test antes de continuar."))
  }
})

test_that("I-2: el bounding box del casco ES la clave en las 318 combinaciones", {
  hits <- localizar_rmd()
  skip_if(length(hits) != 1L, "El .Rmd no se localizó de forma única")
  geom <- extraer_geometria_del_rmd(hits[1])

  combos <- combinaciones()
  expect_equal(nrow(combos), N_ESPERADO,
    info = paste("El espacio de versiones enumerado aquí es", nrow(combos),
                 "y se esperaban", N_ESPERADO,
                 "- este test se desincronizó del .Rmd."))

  fallos <- character(0)
  claves <- character(nrow(combos))
  for (i in seq_len(nrow(combos))) {
    p <- combos[i, ]
    hull <- geom(p[["x_min"]], p[["x_max"]], p[["y_min"]], p[["y_max"]],
                 GRID_MAX, NA_character_)
    if (!bbox_coincide(hull, p[["x_min"]], p[["x_max"]], p[["y_min"]], p[["y_max"]])) {
      fallos <- c(fallos, sprintf(
        "(x:%d-%d, y:%d-%d) bbox dibujado = x:%.4f-%.4f y:%.4f-%.4f",
        p[["x_min"]], p[["x_max"]], p[["y_min"]], p[["y_max"]],
        min(hull$x), max(hull$x), min(hull$y), max(hull$y)))
    }
    claves[i] <- sprintf("%d|%d|%d|%d", p[["x_min"]], p[["x_max"]],
                         p[["y_min"]], p[["y_max"]])
  }

  expect_equal(length(fallos), 0L,
    info = paste0("INVARIANTE I-2 ROTA en ", length(fallos), "/", nrow(combos),
                  " combinaciones. El casco dibujado ya no coincide con la ",
                  "respuesta correcta: la clave del ejercicio es FALSA en esas ",
                  "versiones. Primeros casos:\n  ",
                  paste(utils::head(fallos, 5), collapse = "\n  ")))

  expect_equal(length(unique(claves)), N_ESPERADO,
    info = "Debe haber una biyección combinación <-> respuesta correcta")
})

test_that("El comprobador detecta una violación de I-2 (verificación del verificador)", {
  hits <- localizar_rmd()
  skip_if(length(hits) != 1L, "El .Rmd no se localizó de forma única")
  geom <- extraer_geometria_del_rmd(hits[1])

  hull <- geom(3L, 7L, 4L, 6L, GRID_MAX, NA_character_)
  expect_true(bbox_coincide(hull, 3L, 7L, 4L, 6L),
    info = "Caso de control: el casco real sí cumple la invariante")

  # Casco encogido un 5% hacia su centro: es EXACTAMENTE lo que produciría
  # "suavizar" prof() para que el barco no toque los bordes.
  cx <- 5; cy <- 5
  hull_malo <- data.frame(x = cx + (hull$x - cx) * 0.95,
                          y = cy + (hull$y - cy) * 0.95)
  expect_false(bbox_coincide(hull_malo, 3L, 7L, 4L, 6L),
    info = "El comprobador es vacuo: no detecta un casco que no toca los bordes")
})
