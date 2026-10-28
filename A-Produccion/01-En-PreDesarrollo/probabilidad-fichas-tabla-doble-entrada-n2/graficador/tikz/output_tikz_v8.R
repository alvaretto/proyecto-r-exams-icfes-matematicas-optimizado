# Tabla de fichas (doble entrada) en TikZ — v8 (final).
#
# tikz_tabla_fichas() es una función PURA (sin aleatoriedad): devuelve, como una sola cadena, el código de un
# `tikzpicture` con la tabla del ítem MAT-2026-07-032 (Figura geométrica × Color × Categoría).
#
#   valores          matriz 4×2. Filas: color1-categoría1, color1-categoría2, color2-categoría1,
#                    color2-categoría2. Columnas: figura1, figura2.
#   titulo_columnas  rótulo combinado sobre las dos columnas de figuras.
#   encab_col1/2     encabezados de las dos primeras columnas.
#   figuras  c(figura1, figura2)    colores  c(color1, color2)    categorias  c(cat1, cat2)
#   estilo           lista opcional con parámetros de afinado (ver P); por defecto, el estilo del impreso.
#
# Integración en el .Rmd (el PNG se regenera en cada versión):
#   cod <- tikz_tabla_fichas(valores, titulo_columnas, encab_col1, encab_col2, figuras, colores, categorias)
#   exams::include_tikz(cod, name = paste0("tabla_fichas_", fig_id), format = "png", markup = "markdown",
#                       packages = c("tikz", "xcolor"), width = "70%", density = 600, resize = 1600)
#
# Dependencias LaTeX: solo tikz y xcolor, más la cabecera por defecto de exams::tex2image (helvet, con
# Helvetica Narrow `phv` series c / bc, y graphicx). Sin paquetes adicionales.
#
# Diseño. Unidad = 1 px del escaneo original = 0,5 pt (x=0.5pt, y=-0.5pt: la y crece hacia abajo, como en el
# escaneo). Geometría medida sobre `orig_enunciado_tabla.png`: 4 columnas iguales de 127,25 px y 6 filas de
# 35,5 px. Los anchos de columna son ADAPTATIVOS: LaTeX mide cada texto con su fuente real y ensancha las
# cuatro columnas por igual (nunca el cuerpo de letra) si algún texto, más su holgura, no cabe en 127,25 px;
# el rótulo combinado de arriba reparte su ancho entre las dos columnas de figuras.
# Tipografía: Helvetica Narrow comprimida en horizontal (xscale) hasta el ancho del impreso y engrosada con
# trazo de contorno (modo de texto 2 de PDF) más un manchado horizontal de tres copias: así las astas
# verticales alcanzan el grosor del escaneo sin engordar los trazos horizontales. El 1 se estrecha (los
# dígitos de Helvetica son tabulares; los del impreso, proporcionales).
tikz_tabla_fichas <- function(valores, titulo_columnas, encab_col1, encab_col2,
                              figuras, colores, categorias, estilo = list()) {
  stopifnot(is.matrix(valores), all(dim(valores) == c(4L, 2L)),
            length(titulo_columnas) == 1L, length(encab_col1) == 1L, length(encab_col2) == 1L,
            length(figuras) == 2L, length(colores) == 2L, length(categorias) == 2L)

  # --- texto seguro para LaTeX; rompe las ligaduras ff/fi/fl (el trazo engrosado fundiría el punto de la i) ---
  tex_esc <- function(s) {
    tabla <- c("\\" = "\\textbackslash{}", "&" = "\\&", "%" = "\\%", "$" = "\\$", "#" = "\\#",
               "_" = "\\_", "{" = "\\{", "}" = "\\}", "~" = "\\textasciitilde{}",
               "^" = "\\textasciicircum{}")
    ch <- strsplit(as.character(s), "", fixed = TRUE)[[1]]
    gsub("f(?=[fil])", "f{}", paste(ifelse(ch %in% names(tabla), tabla[ch], ch), collapse = ""), perl = TRUE)
  }
  cifras <- function(x) gsub("1", "\\tfU{}", tex_esc(format(x, scientific = FALSE, trim = TRUE)), fixed = TRUE)

  # --- parámetros de estilo (unidad = px del escaneo; tamaños de letra y anchos de línea en pt) -----------
  P <- list(
    relleno = "250,235,249", vino = "112,5,44", linea = "76,11,48", lineaF = "116,58,90",
    cuerpoH = "11.8pt", cuerpoK = "12.2pt", cuerpoR = "11.6pt", cuerpoN = "13.0pt",
    serH = "bc", serK = "bc", serR = "c", serN = "c",
    sxH = "0.915", sxT = "0.938", sxK = "0.918", sxR = "0.916", sxN = "0.825",
    eH = "0.40", eK = "0.32", eR = "0.10", eN = "0.15",
    smH = "0.65", smK = "0.3", smR = "0.7", smN = "0.5", w1 = "0.70",
    gruesaH = "1.55pt", gruesaH2 = "1.30pt", gruesaV = "1.38pt", finaH = "1.28pt", finaV = "1.00pt",
    pad = "9", ancho_col = "127.25", dx = "0.1",
    dyH = "-1.0", dyT = "-1.0", dyK = "-1.0", dyR = "-1.0", dyN = "-1.0"
  )
  P[names(estilo)] <- estilo

  # --- textos: cada llamada \tfT<clase>{x}{línea base}{texto}; x en macros de columna (se definen más abajo) --
  ymed <- c(33.75, 69.25, 104.75, 140.25, 175.75, 211.25)             # centros de fila (px)
  base <- function(i, clase) ymed[i] + 11.7 + as.numeric(P[[paste0("dy", clase)]])
  celda <- function(clase, xmac, y, s) sprintf("\\tfT%s{%s}{%.2f}{%s}", clase, xmac, y, s)
  txt <- c(
    celda("T", "\\tfct", base(1, "T"), tex_esc(titulo_columnas)),
    celda("H", "\\tfca", base(2, "H"), tex_esc(encab_col1)), celda("H", "\\tfcb", base(2, "H"), tex_esc(encab_col2)),
    celda("H", "\\tfcc", base(2, "H"), tex_esc(figuras[1])), celda("H", "\\tfcd", base(2, "H"), tex_esc(figuras[2])),
    celda("K", "\\tfca", (ymed[3] + ymed[4]) / 2 + 12 + as.numeric(P$dyK), tex_esc(colores[1])),
    celda("K", "\\tfca", (ymed[5] + ymed[6]) / 2 + 12 + as.numeric(P$dyK), tex_esc(colores[2])),
    unlist(lapply(1:4, function(i) c(
      celda("R", "\\tfcb", base(2 + i, "R"), tex_esc(categorias[(i - 1) %% 2 + 1])),
      celda("N", "\\tfcc", base(2 + i, "N"), cifras(valores[i, 1])),
      celda("N", "\\tfcd", base(2 + i, "N"), cifras(valores[i, 2])))))
  )
  # --- mediciones (LaTeX mide con la fuente real) que fijan el ancho común de columna -------------------
  med <- unique(c(
    sprintf("\\tfMedT{\\tfFH}{\\tfsxT}{%s}", tex_esc(titulo_columnas)),
    sprintf("\\tfMedC{\\tfFH}{\\tfsxH}{%s}", vapply(c(encab_col1, encab_col2, figuras), tex_esc, "")),
    sprintf("\\tfMedC{\\tfFK}{\\tfsxK}{%s}", vapply(colores, tex_esc, "")),
    sprintf("\\tfMedC{\\tfFR}{\\tfsxR}{%s}", vapply(categorias, tex_esc, "")),
    sprintf("\\tfMedC{\\tfFN}{\\tfsxN}{%s}", vapply(as.vector(valores), cifras, ""))
  ))

  cab <- r"(\begin{tikzpicture}[x=0.5pt,y=-0.5pt]
\makeatletter
\definecolor{tfRelleno}{RGB}{<<relleno>>}
\definecolor{tfVino}{RGB}{<<vino>>}
\definecolor{tfLinea}{RGB}{<<linea>>}
\definecolor{tfLineaF}{RGB}{<<lineaF>>}
\def\tfFH{\fontsize{<<cuerpoH>>}{<<cuerpoH>>}\fontseries{<<serH>>}\pgfutil@selectfont}
\def\tfFK{\fontsize{<<cuerpoK>>}{<<cuerpoK>>}\fontseries{<<serK>>}\pgfutil@selectfont}
\def\tfFR{\fontsize{<<cuerpoR>>}{<<cuerpoR>>}\fontseries{<<serR>>}\pgfutil@selectfont}
\def\tfFN{\fontsize{<<cuerpoN>>}{<<cuerpoN>>}\fontseries{<<serN>>}\pgfutil@selectfont}
\def\tfsxH{<<sxH>>}\def\tfsxT{<<sxT>>}\def\tfsxK{<<sxK>>}\def\tfsxR{<<sxR>>}\def\tfsxN{<<sxN>>}
\def\tfeH{<<eH>>}\def\tfeK{<<eK>>}\def\tfeR{<<eR>>}\def\tfeN{<<eN>>}
\def\tfsmH{<<smH>>}\def\tfsmK{<<smK>>}\def\tfsmR{<<smR>>}\def\tfsmN{<<smN>>}
\def\tfEon#1{\pgfsys@invoke{2 Tr #1 w}}
\def\tfEoff{\pgfsys@invoke{0 Tr}}
\def\tfU{\setbox0\hbox{1}\hbox to <<w1>>\wd0{\hss\box0\hss}}
\def\tfN#1#2#3#4#5#6#7{\begin{scope}[shift={(#1,#2)},xscale=#3,transform shape]\node[anchor=base,inner sep=0pt,outer sep=0pt,font=#4,text=#5] at (0,0) {#6#7\tfEoff};\end{scope}}
\def\tfTgen#1#2#3#4#5#6#7#8{\tfN{#1}{#2}{#3}{#4}{#5}{#6}{#7}\ifdim#8pt>0pt\tfN{#1-#8}{#2}{#3}{#4}{#5}{#6}{#7}\tfN{#1+#8}{#2}{#3}{#4}{#5}{#6}{#7}\fi}
\def\tfTT#1#2#3{\tfTgen{#1}{#2}{\tfsxT}{\tfFH}{tfVino}{\tfEon{\tfeH}}{#3}{\tfsmH}}
\def\tfTH#1#2#3{\tfTgen{#1}{#2}{\tfsxH}{\tfFH}{tfVino}{\tfEon{\tfeH}}{#3}{\tfsmH}}
\def\tfTK#1#2#3{\tfTgen{#1}{#2}{\tfsxK}{\tfFK}{black}{\tfEon{\tfeK}}{#3}{\tfsmK}}
\def\tfTR#1#2#3{\tfTgen{#1}{#2}{\tfsxR}{\tfFR}{black}{\tfEon{\tfeR}}{#3}{\tfsmR}}
\def\tfTN#1#2#3{\tfTgen{#1}{#2}{\tfsxN}{\tfFN}{black}{\tfEon{\tfeN}}{#3}{\tfsmN}}
\ifdefined\tfmw\else\newlength{\tfmw}\fi
\def\tfpad{<<pad>>}\def\tfNecC{0}\def\tfNecT{0}
\def\tfMedC#1#2#3{\settowidth{\tfmw}{#1 #3}\pgfmathsetmacro\tfNecC{max(\tfNecC,2*\tfmw*#2+2*\tfpad)}}
\def\tfMedT#1#2#3{\settowidth{\tfmw}{#1 #3}\pgfmathsetmacro\tfNecT{max(\tfNecT,2*\tfmw*#2+2*\tfpad)}})"
  geo <- r"(\pgfmathsetmacro{\tfW}{max(<<ancho_col>>,\tfNecC,\tfNecT/2)}
\pgfmathsetmacro{\tfxa}{10}\pgfmathsetmacro{\tfxb}{\tfxa+\tfW}\pgfmathsetmacro{\tfxc}{\tfxa+2*\tfW}
\pgfmathsetmacro{\tfxd}{\tfxa+3*\tfW}\pgfmathsetmacro{\tfxe}{\tfxa+4*\tfW}
\pgfmathsetmacro{\tfca}{(\tfxa+\tfxb)/2+<<dx>>}\pgfmathsetmacro{\tfcb}{(\tfxb+\tfxc)/2+<<dx>>}
\pgfmathsetmacro{\tfcc}{(\tfxc+\tfxd)/2+<<dx>>}\pgfmathsetmacro{\tfcd}{(\tfxd+\tfxe)/2+<<dx>>}
\pgfmathsetmacro{\tfct}{\tfxd+<<dx>>}
\useasboundingbox (\tfxa-6,10) rectangle (\tfxe+6,235);
\fill[white] (\tfxa-10,6) rectangle (\tfxe+10,239);
\fill[tfRelleno] (\tfxc,16) rectangle (\tfxe,51.5);
\fill[tfRelleno] (\tfxa,51.5) rectangle (\tfxe,87);
\begin{scope}[tfLineaF,line cap=butt]
\draw[line width=<<finaH>>] (\tfxa,87) -- (\tfxe,87);
\draw[line width=<<finaH>>] (\tfxb,122.5) -- (\tfxe,122.5);
\draw[line width=<<finaH>>] (\tfxa,158) -- (\tfxe,158);
\draw[line width=<<finaH>>] (\tfxb,193.5) -- (\tfxe,193.5);
\draw[line width=<<finaV>>] (\tfxb,51.5) -- (\tfxb,229);
\draw[line width=<<finaV>>] (\tfxc,51.5) -- (\tfxc,229);
\draw[line width=<<finaV>>] (\tfxd,51.5) -- (\tfxd,229);
\end{scope}
\begin{scope}[tfLinea,line cap=projecting]
\draw[line width=<<gruesaH>>] (\tfxc,16) -- (\tfxe,16);
\draw[line width=<<gruesaH>>] (\tfxa,51.5) -- (\tfxc,51.5);
\draw[line width=<<gruesaH2>>] (\tfxc,51.5) -- (\tfxe,51.5);
\draw[line width=<<gruesaH>>] (\tfxa,229) -- (\tfxe,229);
\draw[line width=<<gruesaV>>] (\tfxa,51.5) -- (\tfxa,229);
\draw[line width=<<gruesaV>>] (\tfxc,16) -- (\tfxc,51.5);
\draw[line width=<<gruesaV>>] (\tfxe,16) -- (\tfxe,229);
\end{scope})"
  rellena <- function(s) {
    for (k in names(P)) s <- gsub(paste0("<<", k, ">>"), P[[k]], s, fixed = TRUE)
    s
  }
  paste(c(rellena(cab), med, rellena(geo), txt, "\\makeatother", "\\end{tikzpicture}"), collapse = "\n")
}
