# =============================================================================
# Helper: rutas del repositorio independientes de la maquina
# =============================================================================
# testthat ejecuta cada suite con el working directory en tests/testthat/, y
# run_one_suite.R lo situa antes en la raiz del repo. Ninguna de las dos cosas
# es una ruta fija: en un runner de CI el repo vive en /home/runner/work/... y
# en un git worktree en cualquier otro sitio. Hardcodear rutas absolutas de una
# maquina concreta dejaba estas suites en rojo permanente fuera de ella.
#
# repo_root() sube directorios hasta reconocer la raiz por sus marcadores.
# repo_path("a", "b") construye rutas a partir de ella.

repo_root <- local({
  cache <- NULL
  function() {
    if (!is.null(cache) && dir.exists(cache)) return(cache)
    d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    for (i in seq_len(8)) {
      if (dir.exists(file.path(d, ".claude", "scripts")) &&
          file.exists(file.path(d, "tests", "run_all_tests.R"))) {
        cache <<- d
        return(d)
      }
      padre <- dirname(d)
      if (identical(padre, d)) break
      d <- padre
    }
    stop("No se pudo localizar la raiz del repositorio desde ", getwd(),
         call. = FALSE)
  }
})

repo_path <- function(...) file.path(repo_root(), ...)

# -----------------------------------------------------------------------------
# ex_path_corto(): ruta corta y estable hacia un ejercicio .Rmd
# -----------------------------------------------------------------------------
# exams::xexams() aplana la ruta ABSOLUTA del .Rmd para nombrar sus archivos
# temporales (cada "/" pasa a "_"). Las rutas tematicas profundas de
# A-Produccion superan asi el limite de 255 bytes por nombre de fichero del
# sistema: en el runner de CI la del ejercicio Media-Mediana-Moda llega a 297 y
# xexams muere con "cannot create file ... Nombre de fichero demasiado largo".
#
# Se copia el directorio del ejercicio a un temporal de nombre corto y se
# devuelve la ruta al .Rmd dentro de esa copia. El original queda intacto.
ex_path_corto <- function(...) {
  origen <- repo_path(...)
  if (!file.exists(origen)) {
    stop("No existe el ejercicio: ", origen, call. = FALSE)
  }
  destino <- tempfile("ex")
  dir.create(destino, recursive = TRUE)
  file.copy(
    list.files(dirname(origen), full.names = TRUE),
    destino, recursive = TRUE
  )
  normalizePath(file.path(destino, basename(origen)),
                winslash = "/", mustWork = TRUE)
}
