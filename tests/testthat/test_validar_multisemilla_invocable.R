# =============================================================================
# test_validar_multisemilla_invocable.R
#
# Regresión del Error 31 (2026-08-09).
#
# `validar_multisemilla.R` resolvía su propia ruta con
#   script_dir <- dirname(sys.frame(1)$ofile)
#   if (is.null(script_dir) || script_dir == "") { ...fallback... }
# Bajo `Rscript` no existe el frame 1: `sys.frame(1)` LANZA UN ERROR y la guarda
# de la línea siguiente nunca se evalúa. El fallback era código inalcanzable y el
# script abortaba ante CUALQUIER entrada, incluido sin argumentos. El hook lo
# invoca así en la FASE 2G, de modo que esa fase era un falso ROJO permanente en
# todo el repositorio.
#
# Lo que fija este test:
#   1. El patrón sin proteger no vuelve a aparecer en NINGÚN script del arsenal.
#   2. El script es invocable bajo `Rscript` desde un cwd arbitrario — que es la
#      propiedad que el bug rompía y que un test estático no puede acreditar.
#   3. Si la dependencia no se localiza, se aborta con mensaje explícito en vez
#      de continuar (el segundo defecto de la versión anterior: el bucle podía
#      terminar sin cargar nada y el fallo salía después como «no se pudo
#      encontrar la función»).
# =============================================================================

library(testthat)

.raiz_repo <- function() {
  r <- tryCatch(
    suppressWarnings(system("git rev-parse --show-toplevel",
                            intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) character(0))
  if (length(r) == 1L && nzchar(r)) return(r)
  normalizePath(".")
}

RAIZ <- .raiz_repo()
SCRIPT_SYMLINK <- file.path(RAIZ, ".claude", "scripts", "validar_multisemilla.R")
SCRIPT_REAL <- file.path(RAIZ, "SOURCES", "scripts_validacion", "validar_multisemilla.R")

test_that("Error 31: ningún script del arsenal usa sys.frame() sin proteger", {
  dirs <- c(file.path(RAIZ, ".claude", "scripts"),
            file.path(RAIZ, "SOURCES", "scripts_validacion"))
  archivos <- unique(normalizePath(unlist(lapply(
    dirs[dir.exists(dirs)],
    function(d) list.files(d, pattern = "\\.R$", full.names = TRUE)))))

  expect_gt(length(archivos), 0)

  # El patrón peligroso es el ÍNDICE LITERAL — `sys.frame(1)` —, que bajo Rscript
  # revienta porque ese frame no existe. NO lo es `sys.frame(i)` dentro de un bucle
  # acotado por `seq_len(sys.nframe())`: ahí el cuerpo simplemente no se ejecuta
  # (así lo hace stress_test_visual.R, que es correcto y sale con 2, no con 1).
  # Buscar cualquier `sys.frame(` daría ese falso positivo; comprobado 2026-08-09.
  culpables <- character(0)
  for (f in archivos) {
    lineas <- readLines(f, warn = FALSE)
    idx <- grep("sys\\.frame\\s*\\(\\s*[0-9]+\\s*\\)", lineas)
    idx <- idx[!grepl("tryCatch", lineas[idx])]
    idx <- idx[!grepl("^\\s*#", lineas[idx])]   # los comentarios que lo explican no cuentan
    if (length(idx) > 0) {
      culpables <- c(culpables, sprintf("%s:%s", basename(f), paste(idx, collapse = ",")))
    }
  }
  expect_equal(culpables, character(0),
    info = paste0("sys.frame() sin tryCatch (revienta bajo Rscript) en: ",
                  paste(culpables, collapse = " | ")))
})

test_that("Error 31: el fix declara el fallback explícito y aborta si no encuentra la dependencia", {
  skip_if_not(file.exists(SCRIPT_REAL), "validar_multisemilla.R no encontrado")
  src <- paste(readLines(SCRIPT_REAL, warn = FALSE), collapse = "\n")

  expect_true(grepl("--file=", src, fixed = TRUE),
    info = "debe resolver su ruta desde commandArgs('--file='), el caso real bajo Rscript")
  expect_true(grepl("tryCatch", src, fixed = TRUE),
    info = "toda resolución de la propia ruta debe ir aislada en tryCatch")
  expect_true(grepl("no se pudo localizar", src, fixed = TRUE),
    info = "si la dependencia no aparece debe abortar con mensaje, no continuar en silencio")
})

test_that("Error 31: es invocable bajo Rscript desde un cwd arbitrario", {
  skip_if_not(file.exists(SCRIPT_SYMLINK), "symlink .claude/scripts no encontrado")
  skip_if(nchar(Sys.which("Rscript")) == 0, "Rscript no disponible")

  cwd_previo <- getwd()
  tmp <- tempfile("multisemilla_cwd_"); dir.create(tmp)
  on.exit({ setwd(cwd_previo); unlink(tmp, recursive = TRUE) }, add = TRUE)
  setwd(tmp)   # cwd SIN relación con el repo: mata cualquier fallback por ruta relativa

  for (ruta in c(SCRIPT_SYMLINK, SCRIPT_REAL)) {
    salida <- suppressWarnings(system2("Rscript", shQuote(ruta),
                                       stdout = TRUE, stderr = TRUE))
    estado <- attr(salida, "status"); if (is.null(estado)) estado <- 0L
    texto <- paste(salida, collapse = "\n")

    # La señal del Error 31 es el crash con sys.frame, no un código concreto.
    expect_false(grepl("sys.frame", texto, fixed = TRUE),
      info = paste0("REGRESIÓN del Error 31 en ", basename(ruta), ": ", texto))
    # Sin argumentos el script imprime el uso y sale con 2 POR DISEÑO (su cabecera
    # documenta «2 = Error de ejecución del script»). El crash salía con 1, así que
    # fijar el 2 distingue «llegó a ejecutarse» de «murió resolviendo su ruta».
    expect_equal(estado, 2L,
      info = paste0("debe llegar al bloque CLI y salir con 2. Salida: ", texto))
    expect_true(grepl("Uso:", texto, fixed = TRUE),
      info = paste0("debe imprimir el uso. Salida: ", texto))
  }
})
