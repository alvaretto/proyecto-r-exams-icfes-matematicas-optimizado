# Verifica el CAMINO REAL: exams::include_tikz() -> exams::tex2image() con la cabecera por defecto y
# packages = c("tikz", "xcolor"). Escribe los PNG de salida junto a este script (../) como previews.
# Uso: Rscript --no-init-file herramientas/verificar_tex2image.R
suppressPackageStartupMessages(library(exams))
dir_tikz <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), ".."))
source(file.path(dir_tikz, "output_tikz_v8.R"), encoding = "UTF-8")
casos <- list(
  canonica = list(valores = matrix(c(12, 8, 16, 10, 11, 15, 20, 8), ncol = 2), titulo_columnas = "Figura geométrica",
                  encab_col1 = "Color", encab_col2 = "Categoría", figuras = c("Círculo", "Cuadrado"),
                  colores = c("Blanca", "Negra"), categorias = c("Par", "Impar")),
  extremo = list(valores = matrix(c(1, 28, 100, 7, 105, 9, 64, 12), ncol = 2), titulo_columnas = "Figura geométrica impresa",
                 encab_col1 = "Clasificación", encab_col2 = "Numeración", figuras = c("Rectángulo", "Triángulo"),
                 colores = c("Amarilla", "Azul oscuro"), categorias = c("Primo", "Compuesto")))
owd <- setwd(dir_tikz); on.exit(setwd(owd))
for (nm in names(casos)) {
  cod <- do.call(tikz_tabla_fichas, casos[[nm]])
  salida <- capture.output(include_tikz(cod, name = paste0("preview_tikz_", nm), format = "png", markup = "markdown",
                                        packages = c("tikz", "xcolor"), width = "70%", density = 600, resize = 1600))
  cat(nm, ": código", nchar(cod, type = "bytes"), "bytes ->", salida, "\n")
}
