# =====================================================================
# Flujo B (regla #3) - graficador R - iteracion 2
# Tabla de doble entrada de fichas (pregunta 32, SAI Nivelacion 6 grado 10)
# Funcion pura (sin aleatoriedad): base R + grid + (opcional) systemfonts
# Ejecutar con:  Rscript --no-init-file output_r_v2.R
# =====================================================================

# ---- INICIO dibujar_tabla_fichas ------------------------------------
dibujar_tabla_fichas <- function(valores, titulo_columnas, encab_col1, encab_col2,
                                 figuras, colores, categorias, ruta, dpi = 300) {
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

  # ---- 1. Parametros (unidad u = 0.01 pulgada; a 300 ppp 1 u = 3 px) --
  cw0 <- c(128.15, 127.06, 127.34, 126.83); rh <- 35.3
  pad <- 9; marg <- 10.2; ancho_min <- 530
  col_rosa <- "#F8E9F7"; col_vino <- "#82003A"; col_negro <- "#050505"
  col_ext <- "#5C0036"; col_int <- "#592848"
  fam_l <- "Source Sans 3"; fam_n <- "Nimbus Sans Narrow"
  estilos <- list(
    enc = list(fam = fam_l, weight = "heavy",    cap = 17.8, stroke = 1.3,  kx = 0.71,  col = col_vino, ink = FALSE),
    lab = list(fam = fam_l, weight = "heavy",    cap = 17.8, stroke = 1.3,  kx = 0.725, col = col_negro, ink = FALSE),
    reg = list(fam = fam_l, weight = "semibold", cap = 17.4, stroke = 1.05, kx = 0.70,  col = col_negro, ink = FALSE),
    num = list(fam = fam_n, weight = "bold",     cap = 18.0, stroke = 0.0,  kx = 1.0,   col = col_negro, ink = TRUE, gap = 1.5)
  )

  # ---- 2. Motor de texto: contornos de glifos (condensado real) -------
  usa_contornos <- requireNamespace("systemfonts", quietly = TRUE) &&
    exists("glyph_outline", envir = asNamespace("systemfonts"), inherits = FALSE)
  for (nm in names(estilos)) {
    e <- estilos[[nm]]; e$fi <- NULL
    if (usa_contornos) {
      fi <- tryCatch(systemfonts::font_info(family = e$fam, weight = e$weight, size = 1000, res = 72),
                     error = function(err) NULL)
      if (!is.null(fi) && nrow(fi) >= 1L && grepl(e$fam, fi$family[1], fixed = TRUE)) {
        e$fi <- list(path = fi$path[1], index = fi$index[1])
        sh <- systemfonts::shape_string(if (e$ink) "1" else "H", family = e$fam, weight = e$weight, size = 1000, res = 72)
        o <- systemfonts::glyph_outline(sh$shape$index, e$fi$path, e$fi$index, size = 1000, tolerance = 2)
        e$size <- (e$cap - e$stroke) / (diff(range(o$y)) / 1000)   # em en u
      } else usa_contornos <- FALSE
    }
    estilos[[nm]] <- e
  }
  if (!usa_contornos) for (nm in names(estilos)) {          # respaldo: texto del sistema
    estilos[[nm]]$fi <- NULL
    estilos[[nm]]$size <- (estilos[[nm]]$cap) / 0.72
  }

  forma <- function(txt, e) {                                # contornos + posiciones (en unidades de 1/1000 em)
    sh <- systemfonts::shape_string(txt, family = e$fam, weight = e$weight, size = 1000, res = 72)
    o <- systemfonts::glyph_outline(sh$shape$index, e$fi$path, e$fi$index, size = 1000, tolerance = 2)
    gl <- o$glyph
    if (e$ink) {                                             # espaciado por tinta (cifras proporcionales)
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
  ancho_txt <- function(txt, e) {
    if (usa_contornos) forma(txt, e)$adv else nchar(txt) * 0.5 * e$size * 0.8
  }

  # ---- 3. Textos y anchos de columna ----------------------------------
  num <- matrix(as.character(as.integer(valores)), nrow = 4L)
  w_col <- cw0
  req <- function(txts, est) max(vapply(txts, ancho_txt, numeric(1), e = est)) + 2 * pad
  w_col[1] <- max(cw0[1], req(encab_col1, estilos$enc), req(colores, estilos$lab))
  w_col[2] <- max(cw0[2], req(encab_col2, estilos$enc), req(categorias, estilos$reg))
  w_col[3] <- max(cw0[3], req(figuras[1], estilos$enc), req(num[, 1], estilos$num))
  w_col[4] <- max(cw0[4], req(figuras[2], estilos$enc), req(num[, 2], estilos$num))
  falta <- req(titulo_columnas, estilos$enc) - (w_col[3] + w_col[4])
  if (falta > 0) w_col[3:4] <- w_col[3:4] + falta / 2

  # ---- 4. Geometria (u, eje y hacia abajo) ----------------------------
  tw <- sum(w_col); th <- 6 * rh
  W <- max(ancho_min, tw + 2 * marg); H <- th + 2 * marg
  w_px <- as.integer(ceiling(W * 0.01 * dpi)); h_px <- as.integer(ceiling(H * 0.01 * dpi))
  W <- w_px / (0.01 * dpi); H <- h_px / (0.01 * dpi)          # u exactos del lienzo
  X0 <- (W - tw) / 2; Y0 <- (H - th) / 2
  vx <- X0 + c(0, cumsum(w_col))                              # V0..V4
  ly <- Y0 + (0:6) * rh                                       # L0..L6
  cx <- (vx[1:4] + vx[2:5]) / 2
  rc <- (ly[1:6] + ly[2:7]) / 2

  # ---- 5. Dispositivo -------------------------------------------------
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  png(ruta, width = w_px, height = h_px, res = dpi, type = "cairo", bg = "white")
  dev_id <- grDevices::dev.cur()
  on.exit(if (dev_id %in% grDevices::dev.list()) grDevices::dev.off(dev_id), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(xscale = c(0, W), yscale = c(0, H)))
  rect_u <- function(x1, y1, x2, y2, fill) {
    grid::grid.rect(x = grid::unit(x1, "native"), y = grid::unit(H - y2, "native"),
                    width = grid::unit(x2 - x1, "native"), height = grid::unit(y2 - y1, "native"),
                    just = c("left", "bottom"), gp = grid::gpar(fill = fill, col = NA))
  }
  texto <- function(txt, xc, yc, e) {                        # yc = centro de la altura de mayusculas
    if (usa_contornos) {
      f <- forma(txt, e)
      ybase <- yc + (e$cap - e$stroke) / 2
      X <- xc + (f$px - f$total / 2) * e$size / 1000 * e$kx
      Y <- H - (ybase - f$py * e$size / 1000)
      idc <- as.integer(factor(paste(f$id, f$ct), levels = unique(paste(f$id, f$ct))))
      grid::grid.path(grid::unit(X, "native"), grid::unit(Y, "native"), id = idc, pathId = f$id, rule = "winding",
                      gp = grid::gpar(fill = e$col, col = if (e$stroke > 0) e$col else NA,
                                      lwd = e$stroke * 0.96, linejoin = "round", lineend = "round"))
    } else {
      grid::grid.text(txt, x = grid::unit(xc, "native"), y = grid::unit(H - (yc + e$cap / 2), "native"),
                      just = c("centre", "baseline"),
                      gp = grid::gpar(col = e$col, fontfamily = "sans", fontsize = e$size * 0.72,
                                      fontface = if (e$weight %in% c("heavy", "bold")) "bold" else "plain"))
    }
  }

  # ---- 6. Dibujo ------------------------------------------------------
  rect_u(0, 0, W, H, "white")
  rect_u(vx[3], ly[1], vx[5], ly[2], col_rosa)                # "Figura geometrica"
  rect_u(vx[1], ly[2], vx[5], ly[3], col_rosa)                # fila de encabezados
  seg <- function(x1, y1, x2, y2, w, col) rect_u(x1, y1, x2, y2, col)
  hl <- function(i, j1, j2, w, col) seg(vx[j1] - 0, ly[i] - w / 2, vx[j2], ly[i] + w / 2, w, col)
  vl <- function(j, i1, i2, w, col) seg(vx[j] - w / 2, ly[i1], vx[j] + w / 2, ly[i2], w, col)
  # interiores
  hl(3, 1, 5, 2.4, col_int); hl(4, 2, 5, 2.3, col_int); hl(5, 1, 5, 2.3, col_int); hl(6, 2, 5, 2.5, col_int)
  vl(2, 2, 7, 2.1, col_int); vl(3, 2, 7, 2.1, col_int); vl(4, 2, 7, 1.8, col_int)
  # exteriores (encima)
  hl(1, 3, 5, 3.2, col_ext); hl(2, 1, 5, 2.85, col_ext); hl(7, 1, 5, 3.3, col_ext)
  vl(1, 2, 7, 2.8, col_ext); vl(5, 1, 7, 3.1, col_ext); vl(3, 1, 2, 3.0, col_ext)
  # esquinas exteriores (cuadrados de union)
  rect_u(vx[1] - 1.4, ly[2] - 1.425, vx[1] + 1.4, ly[2] + 1.425, col_ext)
  rect_u(vx[1] - 1.4, ly[7] - 1.65, vx[1] + 1.4, ly[7] + 1.65, col_ext)
  rect_u(vx[5] - 1.55, ly[1] - 1.6, vx[5] + 1.55, ly[1] + 1.6, col_ext)
  rect_u(vx[5] - 1.55, ly[7] - 1.65, vx[5] + 1.55, ly[7] + 1.65, col_ext)
  rect_u(vx[3] - 1.5, ly[1] - 1.6, vx[3] + 1.5, ly[1] + 1.6, col_ext)

  texto(titulo_columnas, (vx[3] + vx[5]) / 2, rc[1] + 1.4, estilos$enc)
  texto(encab_col1, cx[1], rc[2] + 2.0, estilos$enc)
  texto(encab_col2, cx[2], rc[2] + 2.0, estilos$enc)
  texto(figuras[1], cx[3], rc[2] + 2.0, estilos$enc)
  texto(figuras[2], cx[4], rc[2] + 2.0, estilos$enc)
  texto(colores[1], cx[1], (rc[3] + rc[4]) / 2 + 3.0, estilos$lab)
  texto(colores[2], cx[1], (rc[5] + rc[6]) / 2 + 3.0, estilos$lab)
  for (i in 1:4) {
    texto(categorias[(i + 1) %% 2 + 1], cx[2], rc[i + 2] + 3.0, estilos$reg)
    texto(num[i, 1], cx[3], rc[i + 2] + 2.9, estilos$num)
    texto(num[i, 2], cx[4], rc[i + 2] + 2.9, estilos$num)
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
