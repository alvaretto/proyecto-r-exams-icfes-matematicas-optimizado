#!/usr/bin/env Rscript
# Genera código TikZ dinámico con puntos aleatorios

set.seed(42)

# Parámetros
g <- 9.8
v0 <- 11.3
n_puntos <- 99

# Generar datos
angulos <- runif(n_puntos, 0.05, 1.55)
alcance_teorico <- (v0^2 * sin(2 * angulos)) / g
ruido <- rnorm(n_puntos, 0, 0.42 * sqrt(pmax(0.5, alcance_teorico)))
alcance <- pmax(0.3, pmin(13.8, alcance_teorico + ruido))

# Generar coordenadas TikZ
coords <- paste0("    (", sprintf("%.3f", angulos), ", ", sprintf("%.1f", alcance), ")")
coords_str <- paste(coords, collapse = "\n")

# Generar código TikZ completo
tikz_code <- paste0('\\documentclass[border=5pt]{standalone}
\\usepackage{tikz}
\\usepackage{pgfplots}
\\pgfplotsset{compat=1.18}

\\begin{document}
\\begin{tikzpicture}
\\begin{axis}[
    width=12cm,
    height=8cm,
    xlabel={Ángulo (en radianes)},
    ylabel={Alcance horizontal (m)},
    xmin=0, xmax=1.7,
    ymin=0, ymax=15,
    xtick={0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6},
    ytick={0,2,4,6,8,10,12,14},
    grid=major,
    grid style={gray!30},
    axis lines=left,
    tick label style={font=\\small},
    label style={font=\\small},
]

\\addplot[
    only marks,
    mark=diamond*,
    mark size=2pt,
    color=cyan,
    fill=cyan,
] coordinates {
', coords_str, '
};

\\end{axis}
\\end{tikzpicture}
\\end{document}
')

# Guardar archivo
writeLines(tikz_code, "output_tikz_v2.tex")
cat("TikZ dinámico generado: output_tikz_v2.tex\n")
