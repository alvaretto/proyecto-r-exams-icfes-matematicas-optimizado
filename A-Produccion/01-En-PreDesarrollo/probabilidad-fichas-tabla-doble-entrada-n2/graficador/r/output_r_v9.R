# =====================================================================
# Flujo B (regla #3) - graficador R - iteracion 9
# Tabla de doble entrada de fichas (pregunta 32, SAI Nivelacion 6 grado 10)
# Funcion pura (sin aleatoriedad): base R + grid; opcionales: ragg (borde
# antialias) y systemfonts (texto condensado por contornos de glifos).
# Ejecutar con:  Rscript --no-init-file output_r_v9.R
# =====================================================================

# ---- INICIO dibujar_tabla_fichas ------------------------------------
dibujar_tabla_fichas <- function(valores, titulo_columnas, encab_col1, encab_col2,
                                 figuras, colores, categorias, ruta, dpi = 300) {
  # ---- 0. Validacion --------------------------------------------------
  stopifnot(
    is.matrix(valores), is.numeric(valores), identical(dim(valores), c(4L, 2L)),
    all(is.finite(valores)), all(valores >= 0), all(valores == round(valores)),
    is.character(titulo_columnas), length(titulo_columnas) == 1L,
    is.character(encab_col1), length(encab_col1) == 1L,
    is.character(encab_col2), length(encab_col2) == 1L,
    is.character(figuras), length(figuras) == 2L,
    is.character(colores), length(colores) == 2L,
    is.character(categorias), length(categorias) == 2L,
    is.character(ruta), length(ruta) == 1L,
    is.numeric(dpi), length(dpi) == 1L, dpi >= 72
  )

  # ---- 1. Parametros (unidad u = 0.01 pulgada; a 300 ppp 1 u = 3 px) ---
  # Medidos sobre el impreso (escaneo 530 x 244 px): cuerpo de la tabla
  # 509,4 x 212,0 u; columnas casi iguales; seis filas de 35,3 u.
  cw0 <- c(128.15, 127.06, 127.34, 126.83)            # anchos nominales de columna
  rh <- 35.3                                         # alto de fila
  pad <- 9; marg <- 10.2; ancho_min <- 530           # relleno de celda, margen, ancho minimo del lienzo
  col_rosa <- "#F8E7F6"; col_vino <- "#780034"; col_negro <- "#050505"
  col_ext <- "#52002E"; col_ih <- "#6C345A"; col_iv <- "#7D3E62"
  sans3 <- c("Source Sans 3", "Source Sans Pro")
  # Cada estilo lista sus fuentes candidatas: gana la primera instalada.
  estilos <- list(
    enc = list(cap = 17.8, col = col_vino, bold = TRUE, ink = FALSE, cand = list(     # encabezados
      list(fam = sans3, weight = "heavy", kx = 0.73, stroke = 1.05),
      list(fam = "Nimbus Sans Narrow", weight = "bold", kx = 0.87, stroke = 0.5))),
    lab = list(cap = 17.8, col = col_negro, bold = TRUE, ink = FALSE, cand = list(    # Blanca / Negra
      list(fam = sans3, weight = "heavy", kx = 0.735, stroke = 0.8),
      list(fam = "Nimbus Sans Narrow", weight = "bold", kx = 0.87, stroke = 0.5))),
    reg = list(cap = 17.4, col = col_negro, bold = FALSE, ink = FALSE, cand = list(   # Par / Impar
      list(fam = sans3, weight = "semibold", kx = 0.705, stroke = 1.05),
      list(fam = "Nimbus Sans Narrow", weight = "bold", kx = 0.87, stroke = 0.3))),
    num = list(cap = 18.0, col = col_negro, bold = TRUE, ink = TRUE, gap = 1.5, cand = list(  # cifras
      list(fam = "Nimbus Sans Narrow", weight = "bold", kx = 0.90, stroke = 0.6),
      list(fam = sans3, weight = "bold", kx = 0.78, stroke = 0.4)))
  )

  # ---- 2. Motor de texto: contornos de glifos con escala horizontal ----
  # Ninguna fuente instalada es condensada como la del impreso; por eso se
  # dibujan los contornos (systemfonts) comprimidos en x. Si no hay
  # systemfonts o ninguna candidata, se usa texto del sistema (respaldo).
  usa_sf <- requireNamespace("systemfonts", quietly = TRUE) &&
    exists("glyph_outline", envir = asNamespace("systemfonts"), inherits = FALSE)
  resolver <- function(def) {
    base <- list(cap = def$cap, col = def$col, bold = def$bold, ink = def$ink,
                 gap = if (is.null(def$gap)) 0 else def$gap)
    if (usa_sf) for (cd in def$cand) for (fam in cd$fam) {
      fi <- tryCatch(systemfonts::font_info(family = fam, weight = cd$weight, size = 1000, res = 72),
                     error = function(err) NULL)
      if (is.null(fi) || nrow(fi) < 1L || !grepl(fam, fi$family[1], fixed = TRUE)) next
      ref <- systemfonts::shape_string(if (def$ink) "1" else "H", family = fam, weight = cd$weight,
                                       size = 1000, res = 72)
      o <- systemfonts::glyph_outline(ref$shape$index, fi$path[1], fi$index[1], size = 1000, tolerance = 2)
      alto <- function(ch) {                           # altura (1/1000 em) de un glifo de referencia
        r <- systemfonts::shape_string(ch, family = fam, weight = cd$weight, size = 1000, res = 72)
        max(systemfonts::glyph_outline(r$shape$index, fi$path[1], fi$index[1], size = 1000, tolerance = 2)$y)
      }
      return(c(base, list(fam = fam, weight = cd$weight, kx = cd$kx, stroke = cd$stroke,
                          path = fi$path[1], index = fi$index[1], xh = alto("x"), asc = alto("l"),
                          size = (def$cap - cd$stroke) / (diff(range(o$y)) / 1000))))   # em en u
    }
    c(base, list(fam = NULL, weight = NULL, kx = 1, stroke = 0, size = def$cap / 0.72))
  }
  est <- lapply(estilos, resolver)

  forma <- function(txt, e) {                        # contornos y posiciones (1/1000 em)
    sh <- systemfonts::shape_string(txt, family = e$fam, weight = e$weight, size = 1000, res = 72)
    o <- systemfonts::glyph_outline(sh$shape$index, e$path, e$index, size = 1000, tolerance = 2)
    gl <- o$glyph
    vocal <- c("\u00e1", "\u00e9", "\u00ed", "\u00f3", "\u00fa")
    ac <- sh$shape$glyph[gl] %in% vocal
    if (any(ac)) {                                   # el acento agudo queda casi a la altura del ascendente,
      k <- paste(gl, o$contour)                      # como en el impreso (la fuente lo dibuja mas alto)
      y0 <- tapply(o$y, k, min)[k]; y1 <- tapply(o$y, k, max)[k]
      o$y <- o$y - ifelse(ac & y0 > e$xh + 30, pmax(0, y1 - (e$asc + 14)), 0)
    }
    if (e$ink) {                                     # cifras proporcionales: espaciado por tinta
      xmin <- tapply(o$x, gl, min); xmax <- tapply(o$x, gl, max)
      ng <- nrow(sh$shape); wid <- numeric(ng); izq <- numeric(ng)
      wid[as.integer(names(xmin))] <- xmax - xmin; izq[as.integer(names(xmin))] <- xmin
      gap <- e$gap / (e$size / 1000 * e$kx)
      ini <- c(0, cumsum(wid + gap))[seq_len(ng)]
      px <- (ini - izq)[gl] + o$x; total <- sum(wid) + gap * (ng - 1)
    } else {
      px <- o$x + sh$shape$x_offset[gl]; total <- sh$metrics$width[1]
    }
    list(px = px, py = o$y + sh$shape$y_offset[gl], id = gl, ct = o$contour, total = total,
         adv = total * e$size / 1000 * e$kx + e$stroke)
  }
  abrir_png <- function(archivo, w_px, h_px, res) {  # ragg (antialias) si esta; si no, png cairo
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(archivo, width = w_px, height = h_px, res = res, background = "white")
    } else {
      png(archivo, width = w_px, height = h_px, res = res, type = "cairo", bg = "white")
    }
  }
  ancho_sistema <- function(txt, e) {                 # respaldo: medir con el mismo tipo de dispositivo
    tf <- tempfile(fileext = ".png")
    abrir_png(tf, 400, 400, 72)
    on.exit({ grDevices::dev.off(); unlink(tf) })
    grid::grid.newpage()
    g <- grid::textGrob(txt, gp = grid::gpar(fontfamily = "sans", fontsize = e$size * 0.72,
                                              fontface = if (e$bold) "bold" else "plain"))
    grid::convertWidth(grid::grobWidth(g), "inches", valueOnly = TRUE) * 100
  }
  ancho_txt <- function(txt, e) if (is.null(e$fam)) ancho_sistema(txt, e) else forma(txt, e)$adv

  # ---- 3. Textos y anchos de columna ----------------------------------
  num <- matrix(as.character(as.integer(valores)), nrow = 4L)
  req <- function(txts, e) max(vapply(txts, ancho_txt, numeric(1), e = e)) + 2 * pad
  w_col <- cw0                                       # una columna crece solo si su texto no cabe
  w_col[1] <- max(cw0[1], req(encab_col1, est$enc), req(colores, est$lab))
  w_col[2] <- max(cw0[2], req(encab_col2, est$enc), req(categorias, est$reg))
  w_col[3] <- max(cw0[3], req(figuras[1], est$enc), req(num[, 1], est$num))
  w_col[4] <- max(cw0[4], req(figuras[2], est$enc), req(num[, 2], est$num))
  falta <- req(titulo_columnas, est$enc) - (w_col[3] + w_col[4])
  if (falta > 0) w_col[3:4] <- w_col[3:4] + falta / 2

  # ---- 4. Geometria (u, eje y hacia abajo) ----------------------------
  tw <- sum(w_col); th <- 6 * rh
  W <- max(ancho_min, tw + 2 * marg); H <- th + 2 * marg
  w_px <- as.integer(ceiling(W * 0.01 * dpi)); h_px <- as.integer(ceiling(H * 0.01 * dpi))
  W <- w_px / (0.01 * dpi); H <- h_px / (0.01 * dpi)          # u exactos del lienzo
  X0 <- (W - tw) / 2; Y0 <- (H - th) / 2
  vx <- X0 + c(0, cumsum(w_col))                              # V0..V4 (centros de lineas verticales)
  ly <- Y0 + (0:6) * rh                                       # L0..L6 (centros de lineas horizontales)
  cx <- (vx[1:4] + vx[2:5]) / 2
  rc <- (ly[1:6] + ly[2:7]) / 2

  # ---- 5. Dispositivo PNG (RGB, fondo blanco, sin canal alfa) ----------
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  abrir_png(ruta, w_px, h_px, dpi)
  dev_id <- grDevices::dev.cur()
  on.exit(if (dev_id %in% grDevices::dev.list()) grDevices::dev.off(dev_id), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(xscale = c(0, W), yscale = c(0, H)))
  rect_u <- function(x1, y1, x2, y2, fill) {                 # rectangulo (u, y hacia abajo)
    grid::grid.polygon(x = grid::unit(c(x1, x2, x2, x1), "native"),
                       y = grid::unit(H - c(y1, y1, y2, y2), "native"),
                       gp = grid::gpar(fill = fill, col = NA))
  }
  texto <- function(txt, xc, yc, e) {                        # yc = centro de la altura de mayusculas
    if (is.null(e$fam)) {
      grid::grid.text(txt, x = grid::unit(xc, "native"), y = grid::unit(H - (yc + e$cap / 2), "native"),
                      hjust = 0.5, vjust = 0,
                      gp = grid::gpar(col = e$col, fontfamily = "sans", fontsize = e$size * 0.72,
                                      fontface = if (e$bold) "bold" else "plain"))
      return(invisible())
    }
    f <- forma(txt, e)
    ybase <- yc + (e$cap - e$stroke) / 2
    X <- xc + (f$px - f$total / 2) * e$size / 1000 * e$kx
    Y <- H - (ybase - f$py * e$size / 1000)
    idc <- as.integer(factor(paste(f$id, f$ct), levels = unique(paste(f$id, f$ct))))
    grid::grid.path(grid::unit(X, "native"), grid::unit(Y, "native"), id = idc, pathId = f$id,
                    rule = "winding",
                    gp = grid::gpar(fill = e$col, col = if (e$stroke > 0) e$col else NA,
                                    lwd = e$stroke * 0.96, linejoin = "round", lineend = "round"))
  }

  # ---- 6. Dibujo ------------------------------------------------------
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  rect_u(vx[3], ly[1], vx[5], ly[2], col_rosa)               # celda "Figura geometrica" (columnas 3-4)
  rect_u(vx[1], ly[2], vx[5], ly[3], col_rosa)               # fila de encabezados
  hl <- function(i, j1, j2, w, col) rect_u(vx[j1], ly[i] - w / 2, vx[j2], ly[i] + w / 2, col)
  vl <- function(j, i1, i2, w, col) rect_u(vx[j] - w / 2, ly[i1], vx[j] + w / 2, ly[i2], col)
  # interiores: sin linea entre Par e Impar en la columna 1 (celdas combinadas)
  hl(3, 1, 5, 2.33, col_ih); hl(4, 2, 5, 2.43, col_ih); hl(5, 1, 5, 2.46, col_ih); hl(6, 2, 5, 2.73, col_ih)
  vl(2, 2, 3, 1.9, col_iv); vl(2, 3, 7, 2.2, col_iv)         # algo mas finas en la fila de encabezados
  vl(3, 2, 3, 2.2, col_iv); vl(3, 3, 7, 2.2, col_iv)
  vl(4, 2, 3, 1.57, col_iv); vl(4, 3, 7, 1.95, col_iv)
  # exteriores (encima); sobre las columnas 1-2 de la fila superior no hay celda
  hl(1, 3, 5, 3.1, col_ext); hl(2, 1, 5, 2.85, col_ext); hl(7, 1, 5, 3.15, col_ext)
  vl(1, 2, 7, 2.6, col_ext); vl(5, 1, 7, 2.9, col_ext); vl(3, 1, 2, 2.93, col_ext)
  rect_u(vx[1] - 1.3, ly[2] - 1.425, vx[1] + 1.3, ly[2] + 1.425, col_ext)       # esquinas
  rect_u(vx[1] - 1.3, ly[7] - 1.575, vx[1] + 1.3, ly[7] + 1.575, col_ext)
  rect_u(vx[5] - 1.45, ly[1] - 1.55, vx[5] + 1.45, ly[1] + 1.55, col_ext)
  rect_u(vx[5] - 1.45, ly[7] - 1.575, vx[5] + 1.45, ly[7] + 1.575, col_ext)
  rect_u(vx[3] - 1.465, ly[1] - 1.55, vx[3] + 1.465, ly[1] + 1.55, col_ext)

  texto(titulo_columnas, (vx[3] + vx[5]) / 2, rc[1] + 1.4, est$enc)
  texto(encab_col1, cx[1], rc[2] + 2.0, est$enc)
  texto(encab_col2, cx[2], rc[2] + 2.0, est$enc)
  texto(figuras[1], cx[3], rc[2] + 2.0, est$enc)
  texto(figuras[2], cx[4], rc[2] + 2.0, est$enc)
  texto(colores[1], cx[1], (rc[3] + rc[4]) / 2 + 2.6, est$lab)   # celdas combinadas verticalmente
  texto(colores[2], cx[1], (rc[5] + rc[6]) / 2 + 2.6, est$lab)
  for (i in 1:4) {
    texto(categorias[(i + 1) %% 2 + 1], cx[2], rc[i + 2] + 2.5, est$reg)
    texto(num[i, 1], cx[3], rc[i + 2] + 1.9, est$num)
    texto(num[i, 2], cx[4], rc[i + 2] + 1.9, est$num)
  }
  invisible(ruta)
}
# ---- FIN dibujar_tabla_fichas ---------------------------------------

# ---- Ejecucion como script (instancia canonica del impreso) ---------
if (sys.nframe() == 0L) {
  args <- commandArgs(FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  dir_s <- dirname(normalizePath(f))
  dibujar_tabla_fichas(
    valores = matrix(c(12, 8, 16, 10, 11, 15, 20, 8), ncol = 2),
    titulo_columnas = "Figura geométrica", encab_col1 = "Color", encab_col2 = "Categoría",
    figuras = c("Círculo", "Cuadrado"), colores = c("Blanca", "Negra"), categorias = c("Par", "Impar"),
    ruta = file.path(dir_s, sub("\\.R$", ".png", basename(f))))
}
