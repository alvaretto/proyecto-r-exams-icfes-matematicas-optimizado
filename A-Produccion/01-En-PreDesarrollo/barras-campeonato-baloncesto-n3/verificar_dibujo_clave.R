# verificar_dibujo_clave.R — verificador propio de barras-campeonato-baloncesto-n3
# (checks D-1, D-3, D-4 a nivel de datos, D-9 del mega-prompt; Familia A, N = 100, regla #23)
#
# Por qué existe: los validadores genéricos no protegen la clave de este ejercicio. Medido
# el 2026-10-04 con mutantes: `sol` en la opción equivocada pasó 100/100 por
# validar_multisemilla.R y validar_coherencia_matematica.R; un distractor con la matriz de la
# clave pasó en 75/100. La clave es una propiedad del DIBUJO, y nada miraba el dibujo.
#
# Cómo mide, sin fiarse de las variables del .Rmd (mats_op, M, sol):
#   1. Teje el .Rmd completo con knitr, interceptando include_tikz() para capturar el código
#      TikZ de cada figura sin compilarlo (el render real lo cubren D-5/D-7).
#   2. Lee los valores DEL DIBUJO: escala por las marcas del eje, grado por el color de la
#      leyenda, categoría por la posición de su rótulo, apilado por barras superpuestas.
#   3. Lee los DATOS MOSTRADOS: la fila de la tabla en el Markdown tejido y las dos barras de
#      la gráfica del enunciado.
#   4. Exige: exactamente una opción dibuja los datos mostrados y es la que marca exsolution;
#      la gráfica de Solution es esa misma; el texto de Solution, cada párrafo de distractor,
#      el número de casillas distintas y los totales coinciden con lo dibujado.
#
# Uso: Rscript verificar_dibujo_clave.R [ruta.Rmd] [--n 100] [--estratos]
#      Sin ruta usa el .Rmd del subproyecto. Exit 0 = todo verde; 1 = algún fallo.

suppressMessages(library(exams))
args <- commandArgs(trailingOnly = TRUE)
aqui <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
  dirname(normalizePath(sub("^--file=", "", a))) })
rmd <- if (length(args) && !startsWith(args[1], "--")) normalizePath(args[1]) else
  file.path(aqui, "barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd")
N <- if ("--n" %in% args) as.integer(args[which(args == "--n") + 1]) else 100L
semillas <- seq_len(N) * 7919L + 13L        # mismas semillas que verificar_bateria_p7.R

num <- function(s) as.numeric(sub(",", ".", s, fixed = TRUE))
## Inverso de tex_acentos() del .Rmd: el TikZ escribe s\'{e}ptimo, la tabla séptimo.
destex <- function(s) {
  reps <- c("\\'{a}" = "á", "\\'{e}" = "é", "\\'{\\i}" = "í", "\\'{o}" = "ó", "\\'{u}" = "ú", "\\~{n}" = "ñ",
            "\\'{A}" = "Á", "\\'{E}" = "É", "\\'{I}" = "Í", "\\'{O}" = "Ó", "\\'{U}" = "Ú", "\\~{N}" = "Ñ")
  for (k in names(reps)) s <- gsub(k, reps[[k]], s, fixed = TRUE)
  s
}

## ---- Lectura del TikZ ------------------------------------------------------
leer_tikz <- function(code) {
  L <- strsplit(code, "\n", fixed = TRUE)[[1]]
  rgb_de <- function(s) regmatches(s, regexpr("rgb,255:red,[0-9]+;green,[0-9]+;blue,[0-9]+", s))
  # marcas del eje: \node[anchor=east,...] at (x,y) {t};
  tk <- regmatches(L, regexec("anchor=east.*at \\(([-0-9.]+),([-0-9.]+)\\) \\{([0-9]+)\\};", L))
  tk <- do.call(rbind, lapply(Filter(length, tk), function(m) c(y = num(m[3]), t = num(m[4]))))
  fit <- lm(y ~ t, data = as.data.frame(tk))
  yb <- unname(coef(fit)[1]); esc <- unname(coef(fit)[2])
  # rectángulos rellenos (excluye el fondo blanco y los de la leyenda, que van tras el eje)
  rr <- regmatches(L, regexec("^\\\\fill\\[fill=\\{(rgb[^}]*)\\}\\] \\(([-0-9.]+),([-0-9.]+)\\) rectangle \\(([-0-9.]+),([-0-9.]+)\\);", L))
  idx <- which(lengths(rr) > 0)
  rect <- lapply(idx, function(i) { m <- rr[[i]]
    list(col = m[2], x0 = num(m[3]), y0 = num(m[4]), x1 = num(m[5]), y1 = num(m[6]), linea = i) })
  # rótulos de categoría (debajo del eje) y de la leyenda (anchor=west)
  cat_n <- regmatches(L, regexec("^\\\\node\\[font=\\\\fontsize\\{13.5\\}\\{15\\}\\\\selectfont\\] at \\(([-0-9.]+),([-0-9.]+)\\) \\{(.*)\\};$", L))
  cats <- do.call(rbind, lapply(Filter(length, cat_n), function(m) data.frame(x = num(m[2]), txt = destex(m[4]))))
  ley_n <- regmatches(L, regexec("anchor=west.*at \\(([-0-9.]+),([-0-9.]+)\\) \\{(.*)\\};$", L))
  ley <- do.call(rbind, lapply(Filter(length, ley_n), function(m) data.frame(y = num(m[3]), txt = destex(m[4]))))
  eje_x1 <- num(regmatches(code, regexec("-- \\(([-0-9.]+),[-0-9.]+\\);\n\\\\node\\[font=\\\\fontsize\\{13.5", code))[[1]][2])
  list(yb = yb, esc = esc, ticks = tk, rect = rect, cats = cats, ley = ley, x1 = eje_x1,
       ymax = max(tk[, "t"]))
}

## Tolerancia de lectura (P1, medida): el .Rmd escribe las coordenadas con 3 decimales, así que
## cada valor reconstruido arrastra un error <= 2 * 0,0005 / esc, con esc >= 8/15 = 0,53
## -> <= 0,002 unidades. Los valores legítimos están separados >= 0,5 (el 4,5 de la canónica).
## TOL = 0,01 = 5 veces el error máximo y 50 veces menos que la separación.
TOL <- 0.01
ajustar <- function(v, que) {
  a <- round(v * 2) / 2
  if (any(abs(v - a) > TOL)) stop(que, ": valor dibujado ", paste(signif(v[abs(v - a) > TOL], 6), collapse = ", "), " no es múltiplo de 0,5")
  a
}
## Matriz [grado, categoría] que el dibujo representa (nombres = textos de leyenda/rótulo).
matriz_dibujada <- function(tz) {
  barras <- Filter(function(r) is.na(tz$x1) || r$x0 < tz$x1, tz$rect)
  leyenda <- Filter(function(r) !is.na(tz$x1) && r$x0 > tz$x1, tz$rect)
  if (is.null(tz$ley) || !length(leyenda)) {            # gráfica simple del enunciado
    v <- sapply(barras, function(b) (b$y1 - b$y0) / tz$esc)
    xc <- sapply(barras, function(b) (b$x0 + b$x1) / 2)
    cat_de <- sapply(xc, function(x) tz$cats$txt[which.min(abs(tz$cats$x - x))])
    return(list(simple = setNames(ajustar(v, "enunciado"), cat_de), formato = "simple"))
  }
  ycol <- sapply(leyenda, function(r) (r$y0 + r$y1) / 2)
  grado_de_col <- setNames(sapply(ycol, function(y) tz$ley$txt[which.min(abs(tz$ley$y - y))]),
                           sapply(leyenda, `[[`, "col"))
  X <- matrix(NA_real_, 2, 2, dimnames = list(unique(tz$ley$txt[order(-tz$ley$y)]), tz$cats$txt[order(tz$cats$x)]))
  apilada <- FALSE
  for (b in barras) {
    xc <- (b$x0 + b$x1) / 2
    ca <- tz$cats$txt[which.min(abs(tz$cats$x - xc))]
    gr <- grado_de_col[[b$col]]
    if (abs(b$y0 - tz$yb) > 1e-3) apilada <- TRUE         # segmento que no arranca en cero
    if (!is.na(X[gr, ca])) stop("dos barras para la misma casilla: ", gr, " / ", ca)
    X[gr, ca] <- (b$y1 - b$y0) / tz$esc
  }
  techo <- max(sapply(barras, function(b) (b$y1 - tz$yb) / tz$esc))
  X[] <- ajustar(X, "opción")
  list(X = X, formato = if (apilada) "apilada" else "agrupada", techo = techo, ymax = tz$ymax)
}

## ---- Una versión -------------------------------------------------------------
verificar_version <- function(seed) {
  fallos <- character(0); falla <- function(...) fallos <<- c(fallos, paste0(...))
  figs <- list()
  env <- new.env(parent = globalenv())
  env$include_tikz <- function(tikz, name, ...) { figs[[name]] <<- paste(tikz, collapse = "\n"); invisible(NULL) }
  out <- tempfile(fileext = ".md")
  old <- options(knitr.in.progress = TRUE); on.exit(options(old), add = TRUE)
  set.seed(seed)
  md <- tryCatch({
    suppressMessages(suppressWarnings(knitr::knit(rmd, output = out, envir = env, quiet = TRUE, encoding = "UTF-8")))
    readLines(out, encoding = "UTF-8", warn = FALSE)
  }, error = function(e) { falla("ERROR al tejer: ", conditionMessage(e)); NULL })
  if (is.null(md)) return(list(seed = seed, fallos = fallos))
  txt <- paste(md, collapse = "\n")
  estrato <- list(canonica = isTRUE(env$es_canonica), rama_e8 = isTRUE(env$rama_e8), fmt_clave = env$fmt_op[which(env$sol)],
                  centro = isTRUE(env$centro_es_clave),
                  e7 = any(vapply(env$e7_op, Negate(is.null), logical(1))),
                  cadena = any(lengths(env$rutas_op) == 2L))

  ## Nombres de figura: un mismo fig_id hexadecimal en las seis (regla #4 v6.1)
  nm <- names(figs)
  ids <- unique(sub("^.*_([0-9a-f]+)$", "\\1", nm))
  if (length(figs) != 6L) falla("se capturaron ", length(figs), " figuras, no 6")
  if (length(ids) != 1L || !grepl("^[0-9a-f]{8}$", ids)) falla("fig_id no común o no hexadecimal: ", paste(nm, collapse = ", "))
  esperados <- paste0(c("grafica_enunciado", paste0("diagrama_", letters[1:4]), "grafica_solucion"), "_", ids[1])
  if (!setequal(nm, esperados)) falla("nombres de figura inesperados: ", paste(nm, collapse = ", "))
  for (f in esperados) if (!grepl(paste0("(", f, ".png)"), txt, fixed = TRUE)) falla("el Markdown no referencia ", f, ".png")
  if (!all(esperados %in% nm)) return(list(seed = seed, fallos = fallos, estrato = estrato))   # sin las 6 figuras no hay qué leer

  ## Datos MOSTRADOS: tabla (Markdown tejido) + gráfica del enunciado (dibujo)
  fila <- regmatches(txt, regexec("\\| \\*\\*Grado ([^*]+)\\*\\* \\| ([0-9,]+) \\| ([0-9,]+) \\|", txt))[[1]]
  enc <- regmatches(txt, regexec("\\|  \\| ([^|]+) \\| ([^|]+) \\|", txt))[[1]]
  if (length(fila) < 4 || length(enc) < 3) { falla("no se pudo leer la tabla del enunciado"); return(list(seed = seed, fallos = fallos, estrato = estrato)) }
  g_tabla <- trimws(fila[2]); cats_tabla <- trimws(enc[2:3])
  tz_en <- leer_tikz(figs[[esperados[1]]])
  en <- tryCatch(matriz_dibujada(tz_en)$simple, error = function(e) { falla("lectura del enunciado: ", conditionMessage(e)); NULL })
  if (is.null(en)) return(list(seed = seed, fallos = fallos, estrato = estrato))
  g_graf <- destex(sub("^.*para grado ", "", regmatches(figs[[esperados[1]]], regexpr("para grado ([^}]|\\{[^}]*\\})+", figs[[esperados[1]]]))))
  if (!setequal(names(en), cats_tabla)) falla("categorías del enunciado (", paste(names(en), collapse = "/"), ") ≠ tabla (", paste(cats_tabla, collapse = "/"), ")")
  if (any(en != round(en))) falla("la gráfica del enunciado dibuja valores no enteros: ", paste(en, collapse = ", "))
  if (tz_en$ymax < max(en) || tz_en$ymax > ceiling(max(en))) falla("eje del enunciado hasta ", tz_en$ymax, " con barra máxima ", max(en))
  verdad <- matrix(c(num(fila[3]), num(fila[4]), en[cats_tabla]), 2, byrow = TRUE,
                   dimnames = list(paste("Grado", c(g_tabla, g_graf)), cats_tabla))
  if (g_tabla == g_graf) falla("la tabla y la gráfica son del mismo grado: ", g_tabla)

  ## Opciones DIBUJADAS
  dib <- tryCatch(lapply(esperados[2:5], function(f) matriz_dibujada(leer_tikz(figs[[f]]))),
                  error = function(e) { falla("lectura del dibujo: ", conditionMessage(e)); NULL })
  if (is.null(dib)) return(list(seed = seed, fallos = fallos, estrato = estrato))
  igual <- function(X) !is.null(X$X) && identical(dim(X$X), dim(verdad)) &&
    setequal(rownames(X$X), rownames(verdad)) &&
    max(abs(X$X[rownames(verdad), colnames(verdad)] - verdad)) < 1e-6
  coinciden <- which(vapply(dib, igual, logical(1)))
  ## exsolution tal como lo escribe el Markdown tejido (Meta-information)
  exs <- regmatches(txt, regexec("exsolution: ([01]{4})", txt))[[1]][2]
  sol_md <- which(strsplit(exs, "")[[1]] == "1")
  if (length(coinciden) != 1L) falla("opciones que dibujan los datos mostrados: ", length(coinciden), " (", paste(letters[coinciden], collapse = ","), ")")
  if (length(coinciden) == 1L && !identical(coinciden, sol_md)) falla("la opción que dibuja los datos es ", letters[coinciden], " pero exsolution marca ", paste(letters[sol_md], collapse = ","))
  for (k in 1:4) {
    d <- dib[[k]]
    if (d$techo > d$ymax + TOL) falla("opción ", letters[k], ": barra hasta ", d$techo, " sobre un eje hasta ", d$ymax)
    if (any(d$X < 1 - 1e-9) && !estrato$canonica) falla("opción ", letters[k], ": valor < 1")
    if (!identical(rownames(d$X), rev(rownames(verdad))) && !identical(rownames(d$X), rownames(verdad))) falla("opción ", letters[k], ": leyenda con grados ", paste(rownames(d$X), collapse = "/"))
    if (rownames(d$X)[1] != rownames(verdad)[1]) falla("opción ", letters[k], ": la leyenda no pone arriba al grado de la tabla")
  }
  ejes <- tapply(vapply(dib, `[[`, numeric(1), "ymax"), vapply(dib, `[[`, character(1), "formato"), function(v) length(unique(v)))
  if (any(ejes > 1)) falla("ejes distintos dentro de un mismo formato")
  if (sum(vapply(dib, `[[`, character(1), "formato") == "agrupada") != 2L) falla("reparto de formatos distinto de 2+2")

  ## Solution: la figura y el texto describen la opción que dibuja los datos
  ds <- matriz_dibujada(leer_tikz(figs[[esperados[6]]]))
  if (!igual(ds)) falla("la gráfica de Solution no dibuja los datos mostrados")
  ## Rótulo del tipo (decisión del profesor 2026-10-04): sin él, E8 apilada es idéntica a
  ## barras superpuestas de los datos correctos y sería clave. Fuera de la canónica debe
  ## estar y coincidir con el DIBUJO en las 4 opciones y en la copia de Solution; en la
  ## canónica, ausente (idéntica al impreso).
  rotulo_de <- function(code) { m <- regmatches(code, regexpr("\\{\\(barras (agrupadas|apiladas)\\)\\}", code))
    if (length(m)) sub("^\\{\\(barras (agrupadas|apiladas)\\)\\}$", "\\1", m) else NA_character_ }
  for (k in 1:5) { f <- esperados[k + 1L]; d <- if (k <= 4L) dib[[k]] else ds
    esperado <- if (estrato$canonica) NA_character_ else if (d$formato == "apilada") "apiladas" else "agrupadas"
    r <- rotulo_de(figs[[f]])
    if (!identical(r, esperado)) falla(f, ": rótulo «", r, "» con dibujo ", d$formato, if (estrato$canonica) " (la canónica no lleva rótulo)") }
  if (length(coinciden) == 1L && ds$formato != dib[[coinciden]]$formato) falla("la gráfica de Solution es ", ds$formato, " y la clave es ", dib[[coinciden]]$formato)
  rc <- regmatches(txt, regexec("La gráfica correcta es la de \\*\\*barras (agrupadas|apiladas)\\*\\* que muestra, para el grado ([^,]+), ([0-9,]+) [^0-9]+ y ([0-9,]+) [^(]+\\(datos de la tabla\\) y, para el grado ([^,]+), ([0-9,]+) [^0-9]+ y ([0-9,]+) ", txt))[[1]]
  if (length(rc) < 8) falla("no se pudo leer el párrafo de la respuesta correcta") else {
    if (sub("s$", "", sub("as$", "a", rc[2])) != ds$formato) falla("Solution dice barras ", rc[2], " y la figura es ", ds$formato)
    if (rc[3] != g_tabla || rc[6] != g_graf) falla("Solution nombra los grados ", rc[3], "/", rc[6])
    if (!all(num(rc[c(4, 5, 7, 8)]) == as.vector(t(verdad)))) falla("Solution cita ", paste(rc[c(4, 5, 7, 8)], collapse = "/"), " y los datos son ", paste(as.vector(t(verdad)), collapse = "/"))
  }
  ## Párrafos de distractor: cada uno describe exactamente una opción no clave
  pd <- regmatches(txt, gregexpr("\\*\\*Gráfica de barras (agrupadas|apiladas) que asigna al grado [^*]+\\*\\*", txt))[[1]]
  if (length(pd) != 3L) falla("párrafos de distractor: ", length(pd), " (se esperan 3)")
  desc <- lapply(pd, function(p) { m <- regmatches(p, regexec("barras (agrupadas|apiladas) que asigna al grado ([^ ]+) ([0-9,]+) y ([0-9,]+), y al grado ([^ ]+) ([0-9,]+) y ([0-9,]+)", p))[[1]]
    list(fmt = sub("as$", "a", m[2]), X = matrix(num(m[c(4, 5, 7, 8)]), 2, byrow = TRUE)) })
  no_clave <- setdiff(1:4, coinciden)
  usados <- integer(0)
  for (q in desc) {
    hit <- no_clave[vapply(no_clave, function(k) dib[[k]]$formato == q$fmt &&
      max(abs(dib[[k]]$X[rownames(verdad), colnames(verdad)] - q$X)) < 1e-6, logical(1))]
    if (length(hit) != 1L) falla("un párrafo de distractor no corresponde a ninguna opción dibujada (", q$fmt, " ", paste(as.vector(t(q$X)), collapse = "/"), ")")
    usados <- c(usados, hit)
  }
  if (length(unique(usados)) != 3L) falla("los párrafos de distractor no cubren las tres opciones incorrectas")
  ## Answerlist de Solution: Verdadero solo en la clave; casillas distintas exactas
  al <- regmatches(txt, gregexpr("\\* (Verdadero|Falso)\\. Gráfica de barras (agrupadas|apiladas)[^\n]*", txt))[[1]]
  if (length(al) != 4L) falla("Answerlist de Solution con ", length(al), " líneas") else for (k in 1:4) {
    nd <- sum(abs(dib[[k]]$X[rownames(verdad), colnames(verdad)] - verdad) > 1e-6)
    es_v <- startsWith(al[k], "* Verdadero")
    if (es_v != (k %in% coinciden)) falla("Answerlist línea ", k, ": ", substr(al[k], 1, 40))
    if (!es_v && !grepl(paste0(" en ", nd, " casilla"), al[k])) falla("Answerlist línea ", k, " dice otro número de casillas (real ", nd, ")")
    if (!grepl(sub("a$", "as", dib[[k]]$formato), al[k])) falla("Answerlist línea ", k, ": formato distinto del dibujado")
  }
  ## Caso específico: totales por categoría
  tot <- regmatches(txt, regexec("es ([0-9,]+) \\+ ([0-9,]+) = ([0-9,]+) y el total de [^0-9]+ es ([0-9,]+) \\+ ([0-9,]+) = ([0-9,]+)", txt))[[1]]
  if (length(tot) < 7 || num(tot[4]) != sum(verdad[, 1]) || num(tot[7]) != sum(verdad[, 2])) falla("totales del caso específico no coinciden con los datos")
  ## Sin letras de opción en Solution (regla #19) ni encabezados numerados
  sol_txt <- sub("(?s)^.*\nSolution\n=+\n", "", txt, perl = TRUE)
  if (grepl("[Oo]pci[oó]n [A-Da-d]\\b", sol_txt)) falla("Solution nombra una opción por letra")
  hs <- regmatches(sol_txt, gregexpr("\n### [^\n]+", sol_txt))[[1]]
  ok_h <- grepl("\\{(-|\\.unnumbered) #[a-z-]+-[0-9a-f]{8}\\}\\s*$", hs)
  if (length(hs) != 7L || any(!ok_h)) falla("encabezados de Solution sin número o sin id único por versión: ", paste(hs[!ok_h], collapse = " | "))
  list(seed = seed, fallos = fallos, estrato = estrato)
}

## ---- Ejecución ---------------------------------------------------------------
cat("verificar_dibujo_clave.R —", basename(rmd), "— N =", N, "\n")
res <- lapply(semillas, verificar_version)
malos <- Filter(function(r) length(r$fallos) > 0, res)
for (r in head(malos, 15)) cat("  semilla", r$seed, ":", paste(r$fallos, collapse = " || "), "\n")
est <- do.call(rbind, lapply(Filter(function(r) !is.null(r$estrato), res), function(r) as.data.frame(r$estrato)))
cat("\nCobertura por estrato (n de", nrow(est), "versiones; < 20 = NO CONCLUYENTE, regla #23):\n")
for (v in names(est)) { tb <- table(est[[v]]); cat(sprintf("  %-10s %s\n", v, paste(names(tb), tb, sep = "=", collapse = "  "))) }
cat(sprintf("\nRESULTADO: %d/%d versiones sin fallos\n", N - length(malos), N))
quit(status = if (length(malos)) 1L else 0L)
