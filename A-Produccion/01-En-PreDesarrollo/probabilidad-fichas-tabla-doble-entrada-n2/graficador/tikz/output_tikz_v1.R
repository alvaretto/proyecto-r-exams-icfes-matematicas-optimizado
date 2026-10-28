# Tabla de fichas (doble entrada) en TikZ -- v1.
# Función PURA (sin aleatoriedad): devuelve el código del `tikzpicture` como una sola cadena.
# valores: matriz 4x2; filas = color1-cat1, color1-cat2, color2-cat1, color2-cat2; columnas = figura1, figura2.
tikz_tabla_fichas <- function(valores, titulo_columnas, encab_col1, encab_col2,
                              figuras, colores, categorias) {
  stopifnot(is.matrix(valores), all(dim(valores) == c(4L, 2L)),
            length(titulo_columnas) == 1L, length(encab_col1) == 1L, length(encab_col2) == 1L,
            length(figuras) == 2L, length(colores) == 2L, length(categorias) == 2L)
  tex_esc <- function(s) {
    tabla <- c("\\" = "\\textbackslash{}", "&" = "\\&", "%" = "\\%", "$" = "\\$", "#" = "\\#",
               "_" = "\\_", "{" = "\\{", "}" = "\\}", "~" = "\\textasciitilde{}",
               "^" = "\\textasciicircum{}")
    ch <- strsplit(as.character(s), "", fixed = TRUE)[[1]]
    paste(ifelse(ch %in% names(tabla), tabla[ch], ch), collapse = "")
  }
  num <- function(x) tex_esc(format(x, scientific = FALSE, trim = TRUE))

  # --- parámetros de estilo (unidad = 1 px del escaneo original = 0,5 pt) -------------------
  P <- list(
    relleno = "250,235,249", vino = "125,8,52", linea = "80,16,52",
    cuerpo = "12.2pt", sxH = "0.90", sxK = "0.90", sxR = "0.84",
    eH = "0.45", eK = "0.55", eR = "0.45",
    gruesa = "1.7pt", fina = "1.05pt", pad = "9", ancho_col = "127.25"
  )

  # --- celdas con texto -------------------------------------------------------------------
  ymed <- c(33.75, 69.25, 104.75, 140.25, 175.75, 211.25)   # centros de fila (px)
  base <- ymed + 11.7                                       # línea base del texto
  txt <- character(0)
  add <- function(clase, xmac, y, s) txt <<- c(txt, sprintf("\\tfT%s{%s}{%.2f}{%s}", clase, xmac, y, tex_esc(s)))
  add("H", "\\tfct", base[1], titulo_columnas)
  add("H", "\\tfca", base[2], encab_col1);  add("H", "\\tfcb", base[2], encab_col2)
  add("H", "\\tfcc", base[2], figuras[1]);  add("H", "\\tfcd", base[2], figuras[2])
  add("K", "\\tfca", (ymed[3] + ymed[4]) / 2 + 12, colores[1])
  add("K", "\\tfca", (ymed[5] + ymed[6]) / 2 + 12, colores[2])
  for (i in 1:4) {
    add("R", "\\tfcb", base[2 + i], categorias[(i - 1) %% 2 + 1])
    add("R", "\\tfcc", base[2 + i], num(valores[i, 1]))
    add("R", "\\tfcd", base[2 + i], num(valores[i, 2]))
  }
  # --- mediciones para el ancho de columna (LaTeX mide con la fuente real) ----------------
  med <- c(
    sprintf("\\tfMedT{\\tfFB}{\\tfsxH}{%s}", tex_esc(titulo_columnas)),
    sprintf("\\tfMedC{\\tfFB}{\\tfsxH}{%s}", tex_esc(c(encab_col1, encab_col2, figuras))),
    sprintf("\\tfMedC{\\tfFB}{\\tfsxK}{%s}", tex_esc(colores)),
    sprintf("\\tfMedC{\\tfFB}{\\tfsxR}{%s}", vapply(c(categorias, as.vector(valores)), function(s) num(s), ""))
  )
  med <- unlist(lapply(med, identity))

  cab <- r"(\begin{tikzpicture}[x=0.5pt,y=-0.5pt]
\makeatletter
\definecolor{tfRelleno}{RGB}{<<relleno>>}
\definecolor{tfVino}{RGB}{<<vino>>}
\definecolor{tfLinea}{RGB}{<<linea>>}
\def\tfFB{\fontsize{<<cuerpo>>}{<<cuerpo>>}\fontseries{bc}\pgfutil@selectfont}
\def\tfsxH{<<sxH>>}\def\tfsxK{<<sxK>>}\def\tfsxR{<<sxR>>}
\def\tfeH{<<eH>>}\def\tfeK{<<eK>>}\def\tfeR{<<eR>>}
\def\tfEon#1{\pgfsys@invoke{2 Tr #1 w}}
\def\tfEoff{\pgfsys@invoke{0 Tr}}
\def\tfTgen#1#2#3#4#5#6#7{\begin{scope}[shift={(#1,#2)},xscale=#3,transform shape]\node[anchor=base,inner sep=0pt,outer sep=0pt,font=#4,text=#5] at (0,0) {#6#7\tfEoff};\end{scope}}
\def\tfTH#1#2#3{\tfTgen{#1}{#2}{\tfsxH}{\tfFB}{tfVino}{\tfEon{\tfeH}}{#3}}
\def\tfTK#1#2#3{\tfTgen{#1}{#2}{\tfsxK}{\tfFB}{black}{\tfEon{\tfeK}}{#3}}
\def\tfTR#1#2#3{\tfTgen{#1}{#2}{\tfsxR}{\tfFB}{black}{\tfEon{\tfeR}}{#3}}
\ifdefined\tfmw\else\newlength{\tfmw}\fi
\def\tfpad{<<pad>>}\def\tfNecC{0}\def\tfNecT{0}
\def\tfMedC#1#2#3{\settowidth{\tfmw}{#1 #3}\pgfmathsetmacro\tfNecC{max(\tfNecC,2*\tfmw*#2+2*\tfpad)}}
\def\tfMedT#1#2#3{\settowidth{\tfmw}{#1 #3}\pgfmathsetmacro\tfNecT{max(\tfNecT,2*\tfmw*#2+2*\tfpad)}}
)"
  geo <- r"(\pgfmathsetmacro{\tfW}{max(<<ancho_col>>,\tfNecC,\tfNecT/2)}
\pgfmathsetmacro{\tfxa}{10}\pgfmathsetmacro{\tfxb}{\tfxa+\tfW}\pgfmathsetmacro{\tfxc}{\tfxa+2*\tfW}
\pgfmathsetmacro{\tfxd}{\tfxa+3*\tfW}\pgfmathsetmacro{\tfxe}{\tfxa+4*\tfW}
\pgfmathsetmacro{\tfca}{(\tfxa+\tfxb)/2+0.7}\pgfmathsetmacro{\tfcb}{(\tfxb+\tfxc)/2+0.7}
\pgfmathsetmacro{\tfcc}{(\tfxc+\tfxd)/2+0.7}\pgfmathsetmacro{\tfcd}{(\tfxd+\tfxe)/2+0.7}
\pgfmathsetmacro{\tfct}{\tfxd+0.7}
\fill[white] (\tfxa-6,10) rectangle (\tfxe+6,235);
\fill[tfRelleno] (\tfxc,16) rectangle (\tfxe,51.5);
\fill[tfRelleno] (\tfxa,51.5) rectangle (\tfxe,87);
\begin{scope}[tfLinea,line width=<<fina>>,line cap=butt]
\draw (\tfxa,87) -- (\tfxe,87);
\draw (\tfxb,122.5) -- (\tfxe,122.5);
\draw (\tfxa,158) -- (\tfxe,158);
\draw (\tfxb,193.5) -- (\tfxe,193.5);
\draw (\tfxb,51.5) -- (\tfxb,229);
\draw (\tfxc,51.5) -- (\tfxc,229);
\draw (\tfxd,51.5) -- (\tfxd,229);
\end{scope}
\begin{scope}[tfLinea,line width=<<gruesa>>,line join=miter,line cap=butt]
\draw (\tfxc,16) -- (\tfxe,16) -- (\tfxe,229) -- (\tfxa,229) -- (\tfxa,51.5) -- (\tfxc,51.5) -- cycle;
\draw (\tfxc,51.5) -- (\tfxe,51.5);
\end{scope}
)"
  rellena <- function(s) {
    for (k in names(P)) s <- gsub(paste0("<<", k, ">>"), P[[k]], s, fixed = TRUE)
    s
  }
  paste(c(rellena(cab), med, rellena(geo), txt, "\\makeatother", "\\end{tikzpicture}"), collapse = "\n")
}
