# ============================================================
# Script: tikz_arbol_prob.R
# Descripción: Genera un árbol de probabilidad de 2 niveles
#              usando TikZ dinámico (código generado con paste0 desde R)
# Salida: arbol_prob_tikz.tex → arbol_prob_tikz.pdf → arbol_prob_tikz.png
# ============================================================

# --- Directorio de trabajo ---
output_dir <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/outputs/arbol_prob"
setwd(output_dir)

# ============================================================
# PARÁMETROS (fijos por ahora, parametrizables después)
# ============================================================

# Probabilidades Nivel 1
p_llueve     <- 0.6
p_no_llueve  <- 0.4

# Probabilidades Nivel 2 dado Llueve
p_traf_L     <- 0.7
p_notraf_L   <- 0.3

# Probabilidades Nivel 2 dado No llueve
p_traf_NL    <- 0.3
p_notraf_NL  <- 0.7

# Probabilidades conjuntas (hojas)
pf1 <- p_llueve    * p_traf_L    # 0.42
pf2 <- p_llueve    * p_notraf_L  # 0.18
pf3 <- p_no_llueve * p_traf_NL   # 0.12
pf4 <- p_no_llueve * p_notraf_NL # 0.28

# Función para formatear decimales con coma (estilo español)
fmt <- function(x) gsub("\\.", ",", formatC(x, digits = 2, format = "f"))

# Textos de probabilidades
txt_p_llueve    <- paste0("P = ", fmt(p_llueve))
txt_p_nollueve  <- paste0("P = ", fmt(p_no_llueve))
txt_p_trafL     <- paste0("P = ", fmt(p_traf_L))
txt_p_notrafL   <- paste0("P = ", fmt(p_notraf_L))
txt_p_trafNL    <- paste0("P = ", fmt(p_traf_NL))
txt_p_notrafNL  <- paste0("P = ", fmt(p_notraf_NL))

# Cálculos finales (texto)
txt_pf1 <- paste0(fmt(p_llueve), " $\\times$ ", fmt(p_traf_L),    " = ", fmt(pf1))
txt_pf2 <- paste0(fmt(p_llueve), " $\\times$ ", fmt(p_notraf_L),  " = ", fmt(pf2))
txt_pf3 <- paste0(fmt(p_no_llueve), " $\\times$ ", fmt(p_traf_NL),  " = ", fmt(pf3))
txt_pf4 <- paste0(fmt(p_no_llueve), " $\\times$ ", fmt(p_notraf_NL), " = ", fmt(pf4))

# ============================================================
# GENERACIÓN DINÁMICA DEL CÓDIGO TikZ con paste0()
# ============================================================

# --- Preámbulo LaTeX ---
latex_preambulo <- paste0(
  "\\documentclass[border=10pt]{standalone}\n",
  "\\usepackage{tikz}\n",
  "\\usepackage{xcolor}\n",
  "\\usetikzlibrary{shapes, arrows.meta, calc, positioning}\n",
  "\n",
  "% --- Paleta de colores ---\n",
  "\\definecolor{azulBorde}{HTML}{1971c2}\n",
  "\\definecolor{azulFondo}{HTML}{a5d8ff}\n",
  "\\definecolor{verdeBorde}{HTML}{2f9e44}\n",
  "\\definecolor{verdeFondo}{HTML}{b2f2bb}\n",
  "\\definecolor{naranjaBorde}{HTML}{e8590c}\n",
  "\\definecolor{naranjaFondo}{HTML}{ffd8a8}\n",
  "\\definecolor{verdeclaroBorde}{HTML}{66a80f}\n",
  "\\definecolor{verdeclaroFondo}{HTML}{d8f5a2}\n",
  "\\definecolor{naranjaclaroBorde}{HTML}{f76707}\n",
  "\\definecolor{naranjaclaroFondo}{HTML}{ffe8cc}\n",
  "\n",
  "\\begin{document}\n"
)

# --- Estilos TikZ ---
tikz_estilos <- paste0(
  "\\begin{tikzpicture}[\n",
  "  % Estilos de nodos\n",
  "  nodoInicio/.style={\n",
  "    ellipse, draw=azulBorde, fill=azulFondo,\n",
  "    line width=1.5pt, font=\\bfseries\\large,\n",
  "    minimum width=2.8cm, minimum height=1.2cm\n",
  "  },\n",
  "  nodoLlueve/.style={\n",
  "    rectangle, rounded corners=5pt,\n",
  "    draw=verdeBorde, fill=verdeFondo,\n",
  "    line width=1.5pt, font=\\bfseries,\n",
  "    minimum width=2.5cm, minimum height=1cm\n",
  "  },\n",
  "  nodoNoLlueve/.style={\n",
  "    rectangle, rounded corners=5pt,\n",
  "    draw=naranjaBorde, fill=naranjaFondo,\n",
  "    line width=1.5pt, font=\\bfseries,\n",
  "    minimum width=2.5cm, minimum height=1cm\n",
  "  },\n",
  "  nodoHojaVerde/.style={\n",
  "    rectangle, rounded corners=5pt,\n",
  "    draw=verdeclaroBorde, fill=verdeclaroFondo,\n",
  "    line width=1.2pt, font=\\small\\bfseries,\n",
  "    minimum width=2.4cm, minimum height=0.9cm\n",
  "  },\n",
  "  nodoHojaNaranja/.style={\n",
  "    rectangle, rounded corners=5pt,\n",
  "    draw=naranjaclaroBorde, fill=naranjaclaroFondo,\n",
  "    line width=1.2pt, font=\\small\\bfseries,\n",
  "    minimum width=2.4cm, minimum height=0.9cm\n",
  "  },\n",
  "  nodoProb/.style={\n",
  "    rectangle, rounded corners=3pt,\n",
  "    draw=azulBorde, fill=azulFondo,\n",
  "    font=\\small, text=azulBorde,\n",
  "    minimum width=2.8cm, minimum height=0.8cm,\n",
  "    inner sep=3pt\n",
  "  },\n",
  "  % Estilo de flechas\n",
  "  flecha/.style={\n",
  "    -Latex, thick, draw=gray!60\n",
  "  },\n",
  "  etiqProb/.style={\n",
  "    font=\\small, fill=white, inner sep=2pt\n",
  "  }\n",
  "]\n\n"
)

# --- Título ---
tikz_titulo <- paste0(
  "% --- Título ---\n",
  "\\node[font=\\Large\\bfseries, text=azulBorde]\n",
  "  (titulo) at (5, 1.8) {\\'{A}rbol de Probabilidad};\n\n"
)

# --- Nodo raíz ---
tikz_raiz <- paste0(
  "% --- Nodo raíz ---\n",
  "\\node[nodoInicio] (inicio) at (5, 0) {Inicio};\n\n"
)

# --- Nivel 1: Llueve y No llueve ---
tikz_nivel1 <- paste0(
  "% --- Nivel 1 ---\n",
  "\\node[nodoLlueve]   (llueve)   at (1.5, -2.5) {Llueve};\n",
  "\\node[nodoNoLlueve] (nollueve) at (8.5, -2.5) {No llueve};\n\n"
)

# --- Nivel 2 (desde Llueve) ---
tikz_nivel2_L <- paste0(
  "% --- Nivel 2 desde Llueve ---\n",
  "\\node[nodoHojaVerde] (trafL)    at (-0.5, -5.2) {Tr\\'{a}fico};\n",
  "\\node[nodoHojaVerde] (notrafL)  at ( 3.2, -5.2) {Sin tr\\'{a}fico};\n\n"
)

# --- Nivel 2 (desde No llueve) ---
tikz_nivel2_NL <- paste0(
  "% --- Nivel 2 desde No llueve ---\n",
  "\\node[nodoHojaNaranja] (trafNL)   at (6.8, -5.2) {Tr\\'{a}fico};\n",
  "\\node[nodoHojaNaranja] (notrafNL) at (10.5, -5.2) {Sin tr\\'{a}fico};\n\n"
)

# --- Nodos de probabilidades finales ---
tikz_probs_finales <- paste0(
  "% --- Probabilidades finales (cajas azules debajo de hojas) ---\n",
  "\\node[nodoProb] (pf1) at (-0.5, -6.8) {",  txt_pf1, "};\n",
  "\\node[nodoProb] (pf2) at ( 3.2, -6.8) {",  txt_pf2, "};\n",
  "\\node[nodoProb] (pf3) at ( 6.8, -6.8) {",  txt_pf3, "};\n",
  "\\node[nodoProb] (pf4) at (10.5, -6.8) {",  txt_pf4, "};\n\n"
)

# --- Flechas con etiquetas ---
tikz_flechas <- paste0(
  "% --- Flechas Inicio → Nivel 1 ---\n",
  "\\draw[flecha] (inicio) -- (llueve)\n",
  "  node[etiqProb, pos=0.45, left=2pt] {", txt_p_llueve, "};\n",
  "\\draw[flecha] (inicio) -- (nollueve)\n",
  "  node[etiqProb, pos=0.45, right=2pt] {", txt_p_nollueve, "};\n\n",
  "% --- Flechas Llueve → Hojas ---\n",
  "\\draw[flecha] (llueve) -- (trafL)\n",
  "  node[etiqProb, pos=0.45, left=2pt] {", txt_p_trafL, "};\n",
  "\\draw[flecha] (llueve) -- (notrafL)\n",
  "  node[etiqProb, pos=0.45, right=2pt] {", txt_p_notrafL, "};\n\n",
  "% --- Flechas No llueve → Hojas ---\n",
  "\\draw[flecha] (nollueve) -- (trafNL)\n",
  "  node[etiqProb, pos=0.45, left=2pt] {", txt_p_trafNL, "};\n",
  "\\draw[flecha] (nollueve) -- (notrafNL)\n",
  "  node[etiqProb, pos=0.45, right=2pt] {", txt_p_notrafNL, "};\n\n",
  "% --- Flechas Hojas → Probabilidades finales ---\n",
  "\\draw[flecha, draw=azulBorde!50] (trafL)    -- (pf1);\n",
  "\\draw[flecha, draw=azulBorde!50] (notrafL)  -- (pf2);\n",
  "\\draw[flecha, draw=azulBorde!50] (trafNL)   -- (pf3);\n",
  "\\draw[flecha, draw=azulBorde!50] (notrafNL) -- (pf4);\n\n"
)

# --- Cierre del entorno TikZ y documento ---
tikz_cierre <- paste0(
  "\\end{tikzpicture}\n",
  "\\end{document}\n"
)

# ============================================================
# ENSAMBLAR EL DOCUMENTO COMPLETO
# ============================================================
codigo_latex <- paste0(
  latex_preambulo,
  tikz_estilos,
  tikz_titulo,
  tikz_raiz,
  tikz_nivel1,
  tikz_nivel2_L,
  tikz_nivel2_NL,
  tikz_probs_finales,
  tikz_flechas,
  tikz_cierre
)

# Escribir el archivo .tex
tex_file <- file.path(output_dir, "arbol_prob_tikz.tex")
writeLines(codigo_latex, con = tex_file, useBytes = FALSE)
cat("Archivo .tex generado:", tex_file, "\n")

# ============================================================
# COMPILAR CON pdflatex
# ============================================================
cat("Compilando con pdflatex...\n")
resultado <- system(
  paste0("cd '", output_dir, "' && pdflatex -interaction=nonstopmode arbol_prob_tikz.tex"),
  intern = TRUE
)

pdf_file <- file.path(output_dir, "arbol_prob_tikz.pdf")
if (file.exists(pdf_file)) {
  cat("PDF generado exitosamente:", pdf_file, "\n")
} else {
  cat("ERROR: No se generó el PDF.\n")
  cat("Últimas líneas del log:\n")
  tail_lines <- tail(resultado, 20)
  cat(paste(tail_lines, collapse = "\n"), "\n")
  quit(status = 1)
}

# ============================================================
# CONVERTIR PDF → PNG
# ============================================================
cat("Convirtiendo PDF a PNG con magick...\n")
png_file <- file.path(output_dir, "arbol_prob_tikz.png")
conv_resultado <- system(
  paste0(
    "magick -density 150 '", pdf_file,
    "' -quality 90 '", png_file, "'"
  ),
  intern = TRUE
)

if (file.exists(png_file)) {
  cat("PNG generado exitosamente:", png_file, "\n")
  cat("\n=== RESULTADO FINAL ===\n")
  cat("TEX:", tex_file, "\n")
  cat("PDF:", pdf_file, "\n")
  cat("PNG:", png_file, "\n")
} else {
  cat("ADVERTENCIA: No se generó el PNG. Revisa si magick está instalado.\n")
  cat("El PDF sí está disponible en:", pdf_file, "\n")
}
