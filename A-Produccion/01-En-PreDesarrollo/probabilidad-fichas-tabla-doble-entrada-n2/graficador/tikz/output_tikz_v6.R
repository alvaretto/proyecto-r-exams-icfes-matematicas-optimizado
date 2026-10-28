# Tabla de fichas (doble entrada) en TikZ -- v6.
# Función PURA (sin aleatoriedad): devuelve el código del `tikzpicture` como una sola cadena.
# valores: matriz 4x2; filas = color1-cat1, color1-cat2, color2-cat1, color2-cat2; columnas = figura1, figura2.
tikz_tabla_fichas <- function(valores, titulo_columnas, encab_col1, encab_col2,
                              figuras, colores, categorias, estilo = list()) {
  stopifnot(is.matrix(valores), all(dim(valores) == c(4L, 2L)),
            length(titulo_columnas) == 1L, length(encab_col1) == 1L, length(encab_col2) == 1L,
            length(figuras) == 2L, length(colores) == 2L, length(categorias) == 2L)
  tex_esc <- function(s) {
    tabla <- c("\\" = "\\textbackslash{}", "&" = "\\&", "%" = "\\%", "$" = "\\$", "#" = "\\#",
               "_" = "\\_", "{" = "\\{", "}" = "\\}", "~" = "\\textasciitilde{}",
               "^" = "\\textasciicircum{}")
    ch <- strsplit(as.character(s), "", fixed = TRUE)[[1]]
    gsub("f(?=[fil])", "f{}", paste(ifelse(ch %in% names(tabla), tabla[ch], ch), collapse = ""), perl = TRUE)
  }
  num <- function(x) tex_esc(format(x, scientific = FALSE, trim = TRUE))
  cifras <- function(x) gsub("1", "\\tfU{}", num(x), fixed = TRUE)

  # --- parámetros de estilo (unidad = 1 px del escaneo original = 0,5 pt) -------------------
  P <- list(
    relleno = "250,235,249", vino = "112,5,44", linea = "76,11,48", lineaF = "116,58,90",
    cuerpoH = "11.8pt", cuerpoK = "12.2pt", cuerpoR = "11.6pt", cuerpoN = "13.0pt",
    serH = "bc", serK = "bc", serR = "c", serN = "c",
    sxH = "0.915", sxK = "0.918", sxR = "0.916", sxN = "0.825",
    eH = "0.45", eK = "0.25", eR = "0.10", eN = "0.15",
    smH = "0.7", smK = "0.3", smR = "0.7", smN = "0.5", w1 = "0.70",
    gruesaH = "1.55pt", gruesaH2 = "1.40pt", gruesaV = "1.38pt", finaH = "1.28pt", finaV = "1.08pt",
    pad = "9", ancho_col = "127.25", dx = "0.1", dyH = "-1.0", dyT = "-2.0", dyK = "-0.5", dyR = "-1.0", dyN = "-1.0"
  )
  P[names(estilo)] <- estilo

  # --- celdas con texto -------------------------------------------------------------------
  ymed <- c(33.75, 69.25, 104.75, 140.25, 175.75, 211.25)   # centros de fila (px)
  base <- function(i, clase) ymed[i] + 11.7 + as.numeric(P[[paste0("dy", clase)]])   # línea base del texto
  txt <- character(0)
  add <- function(clase, xmac, y, s) txt <<- c(txt, sprintf("\\tfT%s{%s}{%.2f}{%s}", clase, xmac, y, tex_esc(s)))
  addn <- function(xmac, y, v) txt <<- c(txt, sprintf("\\tfTN{%s}{%.2f}{%s}", xmac, y, cifras(v)))
  add("H", "\\tfct", base(1, "T"), titulo_columnas)
  add("H", "\\tfca", base(2, "H"), encab_col1);  add("H", "\\tfcb", base(2, "H"), encab_col2)
  add("H", "\\tfcc", base(2, "H"), figuras[1]);  add("H", "\\tfcd", base(2, "H"), figuras[2])
  add("K", "\\tfca", (ymed[3] + ymed[4]) / 2 + 12 + as.numeric(P$dyK), colores[1])
  add("K", "\\tfca", (ymed[5] + ymed[6]) / 2 + 12 + as.numeric(P$dyK), colores[2])
  for (i in 1:4) {
    add("R", "\\tfcb", base(2 + i, "R"), categorias[(i - 1) %% 2 + 1])
    addn("\\tfcc", base(2 + i, "N"), valores[i, 1])
    addn("\\tfcd", base(2 + i, "N"), valores[i, 2])
  }
  # --- mediciones para el ancho de columna (LaTeX mide con la fuente real) ----------------
  med <- c(
    sprintf("\\tfMedT{\\tfFH}{\\tfsxH}{%s}", tex_esc(titulo_columnas)),
    sprintf("\\tfMedC{\\tfFH}{\\tfsxH}{%s}", tex_esc(c(encab_col1, encab_col2, figuras))),
    sprintf("\\tfMedC{\\tfFK}{\\tfsxK}{%s}", tex_esc(colores)),
    sprintf("\\tfMedC{\\tfFR}{\\tfsxR}{%s}", tex_esc(categorias)),
    sprintf("\\tfMedC{\\tfFN}{\\tfsxN}{%s}", vapply(as.vector(valores), cifras, ""))
  )

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
\def\tfsxH{<<sxH>>}\def\tfsxK{<<sxK>>}\def\tfsxR{<<sxR>>}\def\tfsxN{<<sxN>>}
\def\tfeH{<<eH>>}\def\tfeK{<<eK>>}\def\tfeR{<<eR>>}\def\tfeN{<<eN>>}
\def\tfsmH{<<smH>>}\def\tfsmK{<<smK>>}\def\tfsmR{<<smR>>}\def\tfsmN{<<smN>>}
\def\tfEon#1{\pgfsys@invoke{2 Tr #1 w}}
\def\tfEoff{\pgfsys@invoke{0 Tr}}
\def\tfU{\setbox0\hbox{1}\hbox to <<w1>>\wd0{\hss\box0\hss}}
\def\tfN#1#2#3#4#5#6#7{\begin{scope}[shift={(#1,#2)},xscale=#3,transform shape]\node[anchor=base,inner sep=0pt,outer sep=0pt,font=#4,text=#5] at (0,0) {#6#7\tfEoff};\end{scope}}
\def\tfTgen#1#2#3#4#5#6#7#8{\tfN{#1}{#2}{#3}{#4}{#5}{#6}{#7}\ifdim#8pt>0pt\tfN{#1-#8}{#2}{#3}{#4}{#5}{#6}{#7}\tfN{#1+#8}{#2}{#3}{#4}{#5}{#6}{#7}\fi}
\def\tfTH#1#2#3{\tfTgen{#1}{#2}{\tfsxH}{\tfFH}{tfVino}{\tfEon{\tfeH}}{#3}{\tfsmH}}
\def\tfTK#1#2#3{\tfTgen{#1}{#2}{\tfsxK}{\tfFK}{black}{\tfEon{\tfeK}}{#3}{\tfsmK}}
\def\tfTR#1#2#3{\tfTgen{#1}{#2}{\tfsxR}{\tfFR}{black}{\tfEon{\tfeR}}{#3}{\tfsmR}}
\def\tfTN#1#2#3{\tfTgen{#1}{#2}{\tfsxN}{\tfFN}{black}{\tfEon{\tfeN}}{#3}{\tfsmN}}
\ifdefined\tfmw\else\newlength{\tfmw}\fi
\def\tfpad{<<pad>>}\def\tfNecC{0}\def\tfNecT{0}
\def\tfMedC#1#2#3{\settowidth{\tfmw}{#1 #3}\pgfmathsetmacro\tfNecC{max(\tfNecC,2*\tfmw*#2+2*\tfpad)}}
\def\tfMedT#1#2#3{\settowidth{\tfmw}{#1 #3}\pgfmathsetmacro\tfNecT{max(\tfNecT,2*\tfmw*#2+2*\tfpad)}}
)"
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
\end{scope}
)"
  rellena <- function(s) {
    for (k in names(P)) s <- gsub(paste0("<<", k, ">>"), P[[k]], s, fixed = TRUE)
    s
  }
  paste(c(trimws(rellena(cab), "right"), unique(med), rellena(geo), txt, "\\makeatother", "\\end{tikzpicture}"), collapse = "\n")
}
