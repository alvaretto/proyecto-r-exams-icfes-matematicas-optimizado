# Prueba: llamar al graficador Python desde R con una matriz de R
library(reticulate)
d <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else getwd() })
source_python(file.path(d, "generador_python.py"))
m <- matrix(c(3, 9, 11, 6), nrow = 2)   # filas = grupos, columnas = categorias
out <- file.path(d, "reticulate_prueba.png")
barras_opcion(m, c("Grado sexto", "Grado séptimo"), c("Partidos ganados", "Partidos perdidos"),
              c("#F2501E", "#1EAAD8"), "agrupada", "Informe de partidos del campeonato",
              "Número de partidos", out)
stopifnot(file.exists(out), file.size(out) > 5000)
cat("RETICULATE_OK", out, file.size(out), "bytes\n")
