# Diagrama de Venn de tres conjuntos en TikZ (Flujo B, paso 2b). Autocontenido.
GEOM_VENN <- list(
  W = 231, H = 239, margen = 3, r = 67.8,
  circ = list(c1 = list(x = 74.0,  y = 87.0,  color = "4A8FA8"),
              c2 = list(x = 155.5, y = 87.0,  color = "7E1F3E"),
              c3 = list(x = 112.5, y = 164.5, color = "2F7F5F")),
  trazo_circulo = 2.2, trazo_marco = 1.7,
  orden = c("s1", "p12", "s2", "ctr", "p13", "p23", "s3"),
  lsb_numero = 0.6, alto_digito = 14.5,
  anclas = list(s1 = list(x_izq = 47.9, y = 86.2), p12 = list(x_izq = 102.1, y = 81.2), s2 = list(x_izq = 164.8, y = 83.8),
                ctr = list(x_izq = 105.8, y = 116.2), p13 = list(x_izq = 67.4, y = 135.2), p23 = list(x_izq = 143.4, y = 136.9),
                s3 = list(x_izq = 105.8, y = 183.6)),
  rotulos = list(r1 = list(x = 7.5, base = 16.5, ajuste = "izquierda"),
                 r2 = list(x = 221.0, base = 16.5, ajuste = "derecha"),
                 r3 = list(x = 163.5, base = 226.5, ajuste = "izquierda")),
  em_numero = 20, em_rotulo = 18, dpi = 216
)

dibujar_venn <- function(regiones, rotulos, archivo, geom = GEOM_VENN) {
  stopifnot(length(regiones) == 7, is.numeric(regiones), all(regiones == round(regiones)),
            all(regiones >= 0), all(regiones <= 99), length(rotulos) == 3, is.character(rotulos))
  regiones <- as.integer(regiones)
  f <- function(v) formatC(v, format = "f", digits = 2)
  g <- geom; m <- g$margen
  # unidades en bp, y hacia abajo; el origen es la esquina del marco, el lienzo lo rebasa en `margen`
  tik <- c(sprintf("\\begin{tikzpicture}[x=1bp,y=-1bp,every node/.style={inner sep=0pt,outer sep=0pt,text=black}]"),
           sprintf("\\useasboundingbox (%s,%s) rectangle (%s,%s);", f(-m), f(-m), f(g$W + m), f(g$H + m)),
           sprintf("\\fill[white] (%s,%s) rectangle (%s,%s);", f(-m), f(-m), f(g$W + m), f(g$H + m)))
  for (k in names(g$circ)) {
    cc <- g$circ[[k]]
    tik <- c(tik, sprintf("\\definecolor{c%s}{HTML}{%s}\\draw[c%s,line width=%sbp] (%s,%s) circle (%sbp);",
                          k, cc$color, k, f(g$trazo_circulo), f(cc$x), f(cc$y), f(g$r)))
  }
  tik <- c(tik, sprintf("\\draw[black,line width=%sbp] (0,0) rectangle (%s,%s);", f(g$trazo_marco), f(g$W), f(g$H)))
  for (i in 1:7) {
    a <- g$anclas[[g$orden[i]]]
    tik <- c(tik, sprintf("\\node[anchor=base west] at (%s,%s) {\\num{%d}};", f(a$x_izq - g$lsb_numero), f(a$y + g$alto_digito / 2), regiones[i]))
  }
  for (i in 1:3) {
    r <- g$rotulos[[i]]
    an <- if (r$ajuste == "izquierda") "base west" else "base east"
    tik <- c(tik, sprintf("\\node[anchor=%s] at (%s,%s) {\\rot{%s}};", an, f(r$x), f(r$base), rotulos[i]))
  }
  tik <- c(tik, "\\end{tikzpicture}")
  cab <- c("\\usepackage[T1]{fontenc}", "\\usepackage[utf8]{inputenc}", "\\usepackage{helvet}",
           sprintf("\\newcommand{\\num}[1]{{\\fontfamily{phv}\\fontseries{bc}\\fontsize{%d}{%d}\\selectfont #1}}", g$em_numero, g$em_numero),
           sprintf("\\newcommand{\\rot}[1]{{\\fontfamily{phv}\\fontseries{bc}\\fontsize{%d}{%d}\\selectfont #1}}", g$em_rotulo, g$em_rotulo))
  td <- tempfile("venn_"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  tex <- c("\\documentclass[border=0pt]{standalone}", cab, "\\usepackage{tikz}", "\\begin{document}", tik, "\\end{document}")
  writeLines(tex, file.path(td, "v.tex"), useBytes = FALSE)
  old <- setwd(td); on.exit(setwd(old), add = TRUE, after = FALSE)
  st <- system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error", "v.tex"), stdout = FALSE, stderr = FALSE)
  if (st != 0 || !file.exists("v.pdf")) stop("pdflatex falló al dibujar el diagrama")
  st <- system2("pdftoppm", c("-png", "-r", g$dpi, "-singlefile", "v.pdf", "v"), stdout = FALSE, stderr = FALSE)
  if (st != 0) stop("pdftoppm falló")
  setwd(old)
  dir.create(dirname(archivo), showWarnings = FALSE, recursive = TRUE)
  file.copy(file.path(td, "v.png"), archivo, overwrite = TRUE)
  invisible(archivo)
}
