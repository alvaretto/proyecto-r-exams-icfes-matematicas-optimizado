# =============================================================================
# Familias de Soluciones Reutilizables para .Rmd — Librería de helpers canónicos
# Regla #21: .claude/rules/familias-soluciones-rmd.md
# =============================================================================
# Fuente de verdad de la versión canónica de cada helper. Por la auto-contención
# de los .Rmd bajo R-exams (edir temporal), se RECOMIENDA COPIAR el helper que se
# necesite dentro del chunk data_generation del .Rmd, no source() por ruta frágil.
#
# Verificado en rango_colesterol_..._cloze_v1 (2026-06-05): HTML/PDF/DOCX/Moodle.
# =============================================================================

# ---- Familia 5: muestreo seguro (evita la trampa sample(escalar, k)) --------
# sample(x, k) reinterpreta x como 1:x cuando length(x)==1. Estas funciones lo evitan.
pick_int <- function(a, b) {
  # Entero uniforme en [a, b]; devuelve a si a >= b (soporte de un solo valor).
  if (a >= b) a else sample(a:b, 1L)
}

safe_sample <- function(x, size, replace = TRUE) {
  # Muestra de tamaño 'size'; si x tiene un solo elemento, repite ese valor.
  # Guarda length-0: NULL o vector vacío (típicamente un campo inexistente en una
  # lista, p.ej. un typo de nombre de campo) hacían caer sample() en
  # sample.int(0, ...) -> "primer argumento inválido", error críptico e intermitente.
  # Aquí falla de inmediato señalando la causa real.
  if (length(x) == 0L)
    stop("safe_sample(): el vector de entrada está vacío o es NULL ",
         "(¿campo inexistente en una lista o nombre de campo mal escrito?).")
  if (length(x) == 1L) rep(x, size) else sample(x, size, replace = replace)
}

# ---- Familia 1: construir un vector con un RANGO objetivo garantizado -------
# Sustituye bucles repeat/while que resamplean hasta cumplir una condición que
# puede ser imposible (causa de cuelgues — Error 22).
construir_valores_con_rango <- function(rango_objetivo, n, lo, hi) {
  # Devuelve n enteros en [lo, hi] cuyo (max - min) == rango_objetivo, mezclados.
  stopifnot(rango_objetivo >= 0, rango_objetivo <= (hi - lo), n >= 2)
  base    <- pick_int(lo, hi - rango_objetivo)
  relleno <- if (rango_objetivo == 0) rep(base, n - 2)
             else sample(base:(base + rango_objetivo), n - 2, replace = TRUE)
  v <- sample(c(base, base + rango_objetivo, relleno))
  stopifnot(max(v) - min(v) == rango_objetivo)
  v
}

# ---- Familia 2: tabla RESPONSIVA cross-formato ------------------------------
# PDF/NOPS: tabla Markdown (longtable; requiere guard \newcounter{none}, regla #20).
# HTML/Moodle: fenced div con scroll horizontal. DOCX: tabla nativa de Word.
# bold_rows: índices de filas a resaltar (negrita Markdown nativa **x**).
tabla_responsiva <- function(df, align = NULL, bold_rows = integer(0)) {
  df2 <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  if (length(bold_rows)) for (r in bold_rows)
    df2[r, ] <- lapply(df2[r, , drop = FALSE], function(x) paste0("**", x, "**"))
  md <- paste(knitr::kable(df2, format = "markdown", align = align), collapse = "\n")
  if (knitr::is_latex_output()) {
    md
  } else {
    paste0("::: {style=\"overflow-x:auto; -webkit-overflow-scrolling:touch; max-width:100%; margin:0.6em 0;\"}\n\n",
           md, "\n\n:::\n")
  }
}

# ---- Familia 3: ecuación display RESPONSIVA ---------------------------------
# 'tex' es el contenido LaTeX SIN los delimitadores $$. En HTML/Moodle se envuelve
# en un fenced div con scroll (MathJax renderiza dentro). OJO: el espacio en "::: {".
eq_display <- function(tex) {
  if (knitr::is_latex_output()) {
    paste0("$$", tex, "$$")
  } else {
    paste0("::: {style=\"overflow-x:auto; -webkit-overflow-scrolling:touch; max-width:100%;\"}\n\n",
           "$$", tex, "$$\n\n:::\n")
  }
}

# ---- Guard regla #20 (para copiar al inicio de la sección Question) ---------
# ```{=latex}
# \makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother
# ```

# ---- Familia 6: opciones gráficas de diagramas vectoriales cardinales -------
# Origen: desplazamiento-avion-aeropuerto (2026-07-28). Cubre el patrón completo de un SCHOICE
# cuyas 4 opciones son diagramas "distancia + dirección" sobre ejes cardinales. Requiere
# library(grid). Consolida cuatro defensas que costaron incidentes documentados:
#   - Error 23  : la etiqueta del ángulo no se solapa con la línea ni con el eje (piso R_fit)
#   - Error 24  : la respuesta correcta no cae siempre en el mismo cuadrante (orientación sorteada)
#   - Error 25  : el nombre del PNG no delata el rol de la opción (renombrado POST-mezcla)
#   - Error 26 + H7: ninguna opción queda degenerada por escala (cascada de ratios de legibilidad)

# 6.1 Pool de orientaciones cardinales. Sortear UNA por versión y aplicarla a las 4 opciones:
# rompe la predictibilidad posicional (regla #22 §P4) preservando la estructura relativa.
# th_axis/dir_sign: eje de referencia y sentido del ángulo (convención matemática, +x = 0°).
# th_perp/sign_perp: eje PERPENDICULAR, para el distractor "ángulo medido desde el eje equivocado".
orientaciones_cardinales <- function() list(
  list(quad = "NE", th_axis = 90,  dir_sign = -1, eje = "norte", lado = "este",  th_perp = 0,   sign_perp =  1),
  list(quad = "NO", th_axis = 90,  dir_sign =  1, eje = "norte", lado = "oeste", th_perp = 180, sign_perp = -1),
  list(quad = "SE", th_axis = 270, dir_sign =  1, eje = "sur",   lado = "este",  th_perp = 360, sign_perp = -1),
  list(quad = "SO", th_axis = 270, dir_sign = -1, eje = "sur",   lado = "oeste", th_perp = 180, sign_perp =  1)
)

# 6.2 Selección de distractores por ENUMERACIÓN + CASCADA de umbrales (regla #21 Familia 1:
# nunca un bucle de reintento sin cota). `es_valida(indices, umbral)` decide si una combinación
# sirve con ese umbral. Se prueba el umbral más exigente y se baja de a un escalón; devuelve la
# combinación elegida junto al umbral que realmente se consiguió. Nunca falla si el último
# escalón admite alguna combinación (elige el último escalón como red de seguridad).
seleccionar_combinacion_con_cascada <- function(n_candidatos, k, es_valida,
                                                umbrales = c(0.40, 0.35, 0.30, 0.25)) {
  combos <- utils::combn(n_candidatos, k, simplify = FALSE)
  for (u in umbrales) {
    validos <- Filter(function(cmb) isTRUE(es_valida(cmb, u)), combos)
    if (length(validos) > 0) {
      return(list(combinacion = validos[[sample(length(validos), 1)]],
                  umbral      = u,
                  n_validos   = length(validos)))
    }
  }
  stop("seleccionar_combinacion_con_cascada(): ninguna combinación válida ni en el umbral mínimo.")
}

# 6.3 Renombrado NEUTRAL de las opciones, obligatoriamente POST-mezcla (regla #22 §P6, Error 25).
# Los archivos se generan con nombres semánticos (depurables) y solo DESPUÉS de sample() se
# renombran a diagrama_a/b/c/d.png. Hacerlo antes vuelve la letra predecible; no hacerlo filtra
# la respuesta en el XML de exams2moodle(), que referencia los recursos por nombre.
renombrar_opciones_neutral <- function(opciones_mezcladas, campo = "archivo",
                                       prefijo = "diagrama_", letras = c("A","B","C","D")) {
  stopifnot(length(opciones_mezcladas) == length(letras))
  for (i in seq_along(letras)) {
    neutral <- paste0(prefijo, tolower(letras[i]), ".png")
    if (file.exists(opciones_mezcladas[[i]][[campo]])) {
      file.rename(opciones_mezcladas[[i]][[campo]], neutral)
    }
    opciones_mezcladas[[i]][[campo]] <- neutral
  }
  names(opciones_mezcladas) <- letras
  opciones_mezcladas
}

# 6.4 Dibujo de un diagrama vectorial sobre ejes cardinales (un PNG por opción, nunca una grilla
# con las cuatro juntas — regla #4). `escala_px_km` debe derivarse del MÁXIMO efectivamente
# dibujado entre las opciones elegidas, nunca del valor de un distractor concreto (Error 26).
dibujar_diagrama_cardinal <- function(archivo, etiqueta_dist, dist_unid, escala_px_unid,
                                      angulo, th_axis, dir_sign,
                                      etiqueta_origen = "Origen", etiqueta_punto = "Punto",
                                      cols = list(slate800 = "#1E293B", slate600 = "#475569",
                                                  teal = "#0F766E", orange = "#EA580C")) {
  png(archivo, width = 460, height = 460, bg = "white", res = 110)
  on.exit(dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(xscale = c(0, 460), yscale = c(460, 0)))
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  cx <- 230; cy <- 230; L <- 150
  ln  <- function(x1,y1,x2,y2,col=cols$slate600,lwd=1.8) grid::grid.lines(grid::unit(c(x1,x2),"native"),grid::unit(c(y1,y2),"native"),gp=grid::gpar(col=col,lwd=lwd,lineend="round"))
  tx  <- function(t,x,y,s=14,col=cols$slate600,j="centre",f=1) grid::grid.text(t,grid::unit(x,"native"),grid::unit(y,"native"),gp=grid::gpar(col=col,fontsize=s,fontface=f),just=j)
  pl  <- function(xs,ys,col) grid::grid.polygon(grid::unit(xs,"native"),grid::unit(ys,"native"),gp=grid::gpar(fill=col,col=col))
  cir <- function(x,y,r,col) grid::grid.circle(grid::unit(x,"native"),grid::unit(y,"native"),r=grid::unit(r,"native"),gp=grid::gpar(fill=col,col=col))
  ln(cx-L,cy,cx+L,cy); ln(cx,cy-L,cx,cy+L)
  pl(c(cx,cx-5,cx+5),c(cy-L,cy-L+11,cy-L+11),cols$slate600)
  pl(c(cx,cx-5,cx+5),c(cy+L,cy+L-11,cy+L-11),cols$slate600)
  pl(c(cx-L,cx-L+11,cx-L+11),c(cy,cy-5,cy+5),cols$slate600)
  pl(c(cx+L,cx+L-11,cx+L-11),c(cy,cy-5,cy+5),cols$slate600)
  tx("N", cx, cy-L-13, 16, cols$slate800, f = 2); tx("S", cx, cy+L+15, 16, cols$slate800, f = 2)
  tx("E", cx+L+15, cy, 16, cols$slate800, f = 2); tx("O", cx-L-15, cy, 16, cols$slate800, f = 2)
  cir(cx,cy,5,cols$slate600)
  Lpx <- dist_unid * escala_px_unid
  th_line <- th_axis + dir_sign*angulo
  a <- th_line*pi/180; dx <- cos(a); dy <- -sin(a)
  ex <- cx + Lpx*dx; ey <- cy + Lpx*dy
  ln(cx,cy,ex,ey,cols$teal,3); cir(ex,ey,6,cols$orange)
  th <- seq(min(th_axis, th_line), max(th_axis, th_line), length.out = 40)*pi/180
  grid::grid.lines(grid::unit(cx+28*cos(th),"native"), grid::unit(cy-28*sin(th),"native"),
                   gp = grid::gpar(col = cols$orange, lwd = 1.8))
  # Etiqueta del ángulo (Error 23): el radio crece con 1/sin(ángulo/2) porque la cuña se estrecha
  # con ángulos pequeños; el PISO 50 cubre el caso opuesto (ángulos grandes, línea casi horizontal
  # que clipaba el texto). Si el punto cae a la altura del radio, se empuja más allá para no solaparlo.
  lab   <- (th_axis + dir_sign*angulo/2)*pi/180
  semi  <- (angulo/2)*pi/180
  R_fit <- max(50, (8 + 11*cos(semi))/sin(semi))
  rang  <- if (abs(Lpx - R_fit) < 22) Lpx + 24 else R_fit
  tx(paste0(angulo,"°"), cx+rang*cos(lab), cy-rang*sin(lab), 13, cols$orange, "centre", 2)
  apx  <- if (dx >= 0) cx-12 else cx+12
  apy  <- if (dy >= 0) cy-9  else cy+13
  tx(etiqueta_origen, apx, apy, 12, cols$slate600, if (dx >= 0) "right" else "left")
  # Etiquetas del extremo: el marcador queda en su posición real y solo el TEXTO se aleja cuando el
  # vector es corto, con guía punteada para no engañar sobre la magnitud representada.
  rtext <- max(Lpx, 58)
  lx <- cx + rtext*dx; ly <- cy + rtext*dy
  if (dy < 0) ly <- min(ly, cy - 32) else ly <- max(ly, cy + 30)
  offx <- if (dx >= 0) 13 else -13; jus <- if (dx >= 0) "left" else "right"
  if (rtext - Lpx > 15) {
    grid::grid.lines(grid::unit(c(ex, lx), "native"), grid::unit(c(ey, ly), "native"),
                     gp = grid::gpar(col = cols$slate600, lwd = 0.6, lty = "dotted"))
  }
  tx(etiqueta_punto, lx+offx, ly+15, 13, cols$slate600, jus)
  tx(etiqueta_dist,  lx+offx, ly-6,  14, cols$teal,     jus, 2)
  invisible(archivo)
}
