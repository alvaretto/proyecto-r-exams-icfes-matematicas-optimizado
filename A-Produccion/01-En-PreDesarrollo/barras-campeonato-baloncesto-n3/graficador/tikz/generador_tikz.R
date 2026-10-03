# Generadores TikZ (TikZ puro, sin pgfplots) -> devuelven el codigo como string.
# Lienzo fijo 15.24 x 10.67 cm (= 6 x 4.2 in; 900 x 630 px a 150 dpi).
.fmt <- function(x) formatC(x, format = "f", digits = 3)
.hex <- function(h) sub("^#", "", toupper(h))
.W <- 15.24; .H <- 10.67

.preambulo <- function() paste0(
"\\documentclass[border=0pt]{standalone}\n",
"\\usepackage[utf8]{inputenc}\\usepackage[T1]{fontenc}\\usepackage{helvet}\n",
"\\renewcommand{\\familydefault}{\\sfdefault}\\usepackage{tikz}\n")

.tikz_cuerpo <- function(barras, ymax, titulo, etiqueta_y, categorias, leyenda, x1) {
  # barras: lista de list(x0,x1,y0,y1,color) en unidades de datos; x en cm
  x0 <- 1.5; yb <- 1.0; yt <- 9.0
  paso <- if (ymax <= 15) 1 else 2
  esc <- (yt - yb) / ymax
  L <- character()
  L <- c(L, sprintf("\\useasboundingbox (0,0) rectangle (%s,%s);", .fmt(.W), .fmt(.H)))
  L <- c(L, "\\fill[white] (0,0) rectangle (current bounding box.north east);")
  ticks <- seq(0, ymax, by = paso)
  for (t in ticks) {
    y <- yb + t * esc
    if (t > 0) L <- c(L, sprintf("\\draw[gray!55,line width=0.4pt] (%s,%s) -- (%s,%s);", .fmt(x0), .fmt(y), .fmt(x1), .fmt(y)))
    L <- c(L, sprintf("\\node[anchor=east,font=\\fontsize{12}{14}\\selectfont,inner sep=2pt] at (%s,%s) {%d};", .fmt(x0 - 0.05), .fmt(y), as.integer(t)))
  }
  for (b in barras)
    L <- c(L, sprintf("\\fill[fill=%s] (%s,%s) rectangle (%s,%s);",
      paste0("{rgb,255:red,", b$rgb[1], ";green,", b$rgb[2], ";blue,", b$rgb[3], "}"),
      .fmt(b$x0), .fmt(yb + b$y0 * esc), .fmt(b$x1), .fmt(yb + b$y1 * esc)))
  L <- c(L, sprintf("\\draw[line width=0.8pt,black!85] (%s,%s) -- (%s,%s) -- (%s,%s);",
    .fmt(x0), .fmt(yt + 0.35), .fmt(x0), .fmt(yb), .fmt(x1), .fmt(yb)))
  slot <- (x1 - x0) / 2
  for (i in 1:2) L <- c(L, sprintf("\\node[font=\\fontsize{11.5}{13}\\selectfont] at (%s,%s) {%s};", .fmt(x0 + (i - .5) * slot), .fmt(yb - 0.5), categorias[i]))
  L <- c(L, sprintf("\\node[rotate=90,font=\\bfseries\\fontsize{14.5}{16}\\selectfont] at (0.45,%s) {%s};", .fmt((yb + yt) / 2), etiqueta_y))
  L <- c(L, sprintf("\\node[font=\\bfseries\\fontsize{15.5}{18}\\selectfont,align=center] at (%s,%s) {%s};", .fmt((x0 + x1) / 2 + 0.5), .fmt(10.0), titulo))
  if (!is.null(leyenda)) for (i in 1:2) {
    yy <- 5.4 - (i - 1) * 0.95
    L <- c(L, sprintf("\\fill[fill=%s] (%s,%s) rectangle (%s,%s);",
      paste0("{rgb,255:red,", leyenda$rgb[[i]][1], ";green,", leyenda$rgb[[i]][2], ";blue,", leyenda$rgb[[i]][3], "}"),
      .fmt(x1 + 0.4), .fmt(yy - 0.17), .fmt(x1 + 0.74), .fmt(yy + 0.17)))
    L <- c(L, sprintf("\\node[anchor=west,font=\\fontsize{11.5}{13}\\selectfont] at (%s,%s) {%s};", .fmt(x1 + 0.85), .fmt(yy), leyenda$txt[i]))
  }
  paste0(.preambulo(), "\\begin{document}\\begin{tikzpicture}\n", paste(L, collapse = "\n"), "\n\\end{tikzpicture}\\end{document}\n")
}

barras_simples_tikz <- function(valores, categorias, colores, titulo, etiqueta_y) {
  ymax <- as.integer(max(valores)); x0 <- 1.5; x1 <- 14.6; slot <- (x1 - x0) / 2; w <- 0.4 * slot
  barras <- lapply(1:2, function(i) { c0 <- x0 + (i - .5) * slot
    list(x0 = c0 - w / 2, x1 = c0 + w / 2, y0 = 0, y1 = valores[i], rgb = as.integer(col2rgb(colores[i]))) })
  .tikz_cuerpo(barras, ymax, titulo, etiqueta_y, categorias, NULL, x1)
}

# M[grupo, categoria]; grupo 1 = p.ej. sexto, grupo 2 = septimo
barras_opcion_tikz <- function(M, grupos, categorias, colores, modo = c("agrupada", "apilada"), titulo, etiqueta_y, ymax = NULL) {
  modo <- match.arg(modo); x0 <- 1.5; x1 <- 10.9; slot <- (x1 - x0) / 2
  rgbs <- lapply(colores, function(c) as.integer(col2rgb(c)))
  ymax_d <- if (modo == "agrupada") max(M) else max(colSums(M))
  ymax <- if (is.null(ymax)) as.integer(ceiling(ymax_d)) else as.integer(ymax)
  barras <- list()
  for (j in 1:2) { c0 <- x0 + (j - .5) * slot
    if (modo == "agrupada") { w <- 0.3 * slot
      barras[[length(barras) + 1]] <- list(x0 = c0 - w, x1 = c0, y0 = 0, y1 = M[1, j], rgb = rgbs[[1]])
      barras[[length(barras) + 1]] <- list(x0 = c0, x1 = c0 + w, y0 = 0, y1 = M[2, j], rgb = rgbs[[2]])
    } else { w <- 0.36 * slot
      barras[[length(barras) + 1]] <- list(x0 = c0 - w / 2, x1 = c0 + w / 2, y0 = 0, y1 = M[2, j], rgb = rgbs[[2]])
      barras[[length(barras) + 1]] <- list(x0 = c0 - w / 2, x1 = c0 + w / 2, y0 = M[2, j], y1 = M[2, j] + M[1, j], rgb = rgbs[[1]])
    } }
  .tikz_cuerpo(barras, ymax, titulo, etiqueta_y, categorias, list(rgb = rgbs, txt = grupos), x1)
}

# Compila string TikZ a PNG (150 dpi). Usa tempdir para auxiliares.
renderizar_tikz_png <- function(codigo, archivo_png, dpi = 150) {
  d <- tempfile("tikz"); dir.create(d); tex <- file.path(d, "f.tex"); writeLines(codigo, tex)
  r <- system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error", paste0("-output-directory=", d), tex), stdout = TRUE, stderr = TRUE)
  if (!file.exists(file.path(d, "f.pdf"))) stop(paste(tail(r, 15), collapse = "\n"))
  system2("magick", c("-density", dpi, file.path(d, "f.pdf"), "-background", "white", "-alpha", "remove", "-resize", "900x630!", archivo_png))
  invisible(archivo_png)
}
