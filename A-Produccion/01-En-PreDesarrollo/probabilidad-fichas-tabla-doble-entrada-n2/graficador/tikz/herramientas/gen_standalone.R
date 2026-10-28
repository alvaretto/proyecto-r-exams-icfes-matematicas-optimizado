# Uso: Rscript --no-init-file gen_standalone.R <vN> [canonica|extremo]
# Escribe output_tikz_<vN>.tex (documento standalone, border=2mm, cabecera idéntica a exams::tex2image) en ../
args <- commandArgs(trailingOnly = TRUE)
v <- args[1]; caso <- if (length(args) >= 2) args[2] else "canonica"
dir_tikz <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), ".."))
source(file.path(dir_tikz, sprintf("output_tikz_%s.R", v)), encoding = "UTF-8")
if (caso == "canonica") {
  codigo <- tikz_tabla_fichas(valores = matrix(c(12, 8, 16, 10, 11, 15, 20, 8), ncol = 2),
                              titulo_columnas = "Figura geométrica", encab_col1 = "Color", encab_col2 = "Categoría",
                              figuras = c("Círculo", "Cuadrado"), colores = c("Blanca", "Negra"),
                              categorias = c("Par", "Impar"))
  salida <- file.path(dir_tikz, sprintf("output_tikz_%s.tex", v))
} else {
  codigo <- tikz_tabla_fichas(valores = matrix(c(1, 28, 100, 7, 105, 9, 64, 12), ncol = 2),
                              titulo_columnas = "Figura geométrica impresa", encab_col1 = "Clasificación",
                              encab_col2 = "Numeración", figuras = c("Rectángulo", "Triángulo"),
                              colores = c("Amarilla", "Azul oscuro"), categorias = c("Primo", "Compuesto"))
  salida <- file.path(dir_tikz, sprintf("extremo_tikz_%s.tex", v))
}
# misma cabecera que exams::tex2image(packages = c("tikz","xcolor")), con border=2mm
cab <- c("\\documentclass[tikz,border=2mm]{standalone}",
         "\\usepackage{tikz}", "\\usepackage{xcolor}", "\\usepackage{graphicx}", "\\usepackage{url}",
         "\\usepackage{color}", "\\usepackage{varwidth}", "\\usepackage{Sweave}",
         "\\tikzset{font={\\fontsize{12pt}{12}\\selectfont}}",
         "\\usepackage{helvet}", "\\IfFileExists{sfmath.sty}{\\RequirePackage[helvet]{sfmath}}{}",
         "\\renewcommand{\\sfdefault}{phv}", "\\renewcommand{\\rmdefault}{phv}",
         "\\begin{document}")
writeLines(enc2utf8(c(cab, codigo, "\\end{document}")), salida, useBytes = TRUE)
cat(salida, "\n")
