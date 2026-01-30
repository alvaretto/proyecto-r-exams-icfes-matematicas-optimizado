#!/usr/bin/env Rscript
# TikZ v3 - Ajuste fino para ≥95% similitud

set.seed(42)

# Parámetros ajustados para replicar imagen original exactamente
g <- 9.8
v0 <- 11.2  # Ajustado para alcance max ~12.8m
n_puntos <- 99

# Generar ángulos con distribución similar a la imagen original
# Más concentración en zona central (0.4-1.2)
angulos <- c(
  runif(15, 0.05, 0.25),   # Zona izquierda (pocos puntos)
  runif(25, 0.25, 0.55),   # Transición izq
  runif(30, 0.55, 1.0),    # Zona central (más puntos)
  runif(20, 1.0, 1.35),    # Transición der
  runif(9, 1.35, 1.55)     # Zona derecha (pocos puntos)
)

# Calcular alcance teórico
alcance_teorico <- (v0^2 * sin(2 * angulos)) / g

# Ruido proporcional al alcance - ajustado para imagen original
# Mayor dispersión donde alcance es mayor
ruido_factor <- 0.38 * sqrt(pmax(0.3, alcance_teorico / max(alcance_teorico))) * sqrt(alcance_teorico)
ruido <- rnorm(n_puntos, 0, ruido_factor)
alcance <- alcance_teorico + ruido
alcance <- pmax(0.3, pmin(13.5, alcance))

# Generar coordenadas TikZ
coords <- paste0("    (", sprintf("%.3f", angulos), ", ", sprintf("%.1f", alcance), ")")
coords_str <- paste(coords, collapse = "\n")

# Código TikZ
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

writeLines(tikz_code, "output_tikz_v3.tex")
cat("TikZ v3 generado\n")
