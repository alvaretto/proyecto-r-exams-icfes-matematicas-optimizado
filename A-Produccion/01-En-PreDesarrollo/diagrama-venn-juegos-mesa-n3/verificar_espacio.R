#!/usr/bin/env Rscript
# verificar_espacio.R — diagrama-venn-juegos-mesa-n3 (invariante I-4 de .claude/CLAUDE.md local).
#
# Ningún validador del repo lee los números de una figura. Este script usa el código real del .Rmd (no lo reimplementa):
#   espacio  : enumera el espacio de I-3 (assert de tamaño) y prueba que solo la clave reproduce los datos.
#   cableado : N = 100 por rama, sin dibujar. Lo que recibe dibujar_venn, contra los datos LEÍDOS DEL TEXTO del enunciado.
#   figura   : N = 100 por rama, render real. Lee lo IMPRESO con pdftotext -bbox sobre el PDF del que sale el PNG: cada
#              número vale por el lugar donde quedó, y cada rótulo por su círculo más cercano. Exige además que las
#              cuatro figuras sean iguales píxel a píxel fuera de los números (I-1).
#   tinta    : render real. Toda la caja de tinta de cada número, de 0 a 99, en las siete zonas (I-4, I-5), y la
#              distancia de los rótulos al marco sobre el PNG sin recortar.
#   solucion : N = 100 por rama. Ejecuta el chunk solucion: cada párrafo debe casar con una plantilla y sus cifras con la
#              figura y el enunciado, o estar en la lista de frases sin datos, y además coincidir letra a letra con el
#              texto reconstruido desde las viñetas. Cubre también el feedback por opción.
#   moodle   : N = 100 por rama. XML real: tipo, pregunta, fraction, feedback e imagen de la Solution, contra el dibujo calculado
#              desde las viñetas del propio XML. exsolution decide la nota y el chunk feedback llega al estudiante: solo se ven aquí.
#   uso      : las condiciones de uso de SemilleroUnico_v2.R (H31): sus literales (plantilla, twocolumn, samepage, \raggedbottom, papersize de pdfjam),
#              solpcielo_una_por_pagina.tex frente a perifericos/solpcielo.tex (solo el comentario y \clearpage), y un PDF y un NOPS reales de 5 preguntas
#              con las cuatro figuras de cada una en una misma página.
#   papel    : N = 100 PDF reales con perifericos/solpcielo.tex en 216 x 330 mm: los 7 contextos, la canónica y los textos más
#              largos. Las cuatro opciones quedan en una misma página (I-5).
#   mutantes : contrato INC-MUTANTE-SONDA; un mutante que intercambia dos regiones debe hacerlo fallar.
#   bateria  : §P7, N = 100, y la canónica por enumeración exacta de sus 24 órdenes.
#
# Uso: Rscript --no-init-file verificar_espacio.R [todo|espacio|cableado|figura|tinta|solucion|papel|moodle|uso|mutantes|bateria]
# Salida: 1 = alguna sonda en rojo;
#         3 = sin cubrir: FIRMADA es FALSE, el .Rmd no tiene el md5 firmado (RMD_MD5), una regla sobre +8 pp fuera de su lista, una regla por encima
#             de su cifra, una cifra firmada que no se reproduce, alcance sin evaluar, una sección de «todo» que no corrió, o un modo distinto de «todo»;
#         4 = ROJO cubierto por la firma del profesor: solo en modo «todo» y solo con FIRMADA TRUE. Nunca es PASS. 0 no ocurre con este ítem.
#         La decisión es la función pura decidir_exit().

args <- commandArgs(TRUE); modo <- if (length(args)) args[1] else "todo"
archivo_self <- sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))
aqui <- if (length(archivo_self)) dirname(normalizePath(archivo_self)) else getwd()
RMD <- file.path(aqui, "diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd")
lineas <- readLines(RMD, encoding = "UTF-8")
cat("md5 del .Rmd:", unname(tools::md5sum(RMD)), "\n")
cat("exams", as.character(packageVersion("exams")), "\n")

chunk <- function(nombre) {
  i <- grep(paste0("^```\\{r ", nombre), lineas); stopifnot(length(i) == 1L)
  f <- i + which(lineas[(i + 1L):length(lineas)] == "```")[1L]
  lineas[(i + 1L):(f - 1L)]
}
compila <- function(cod) parse(text = paste(cod, collapse = "\n"), encoding = "UTF-8", keep.source = FALSE)
COD_DG <- chunk("data_generation"); COD_DIB <- chunk("dibujo_opciones"); COD_SOL <- chunk("solucion")
DG <- compila(COD_DG); DIB <- compila(COD_DIB); SOL <- compila(COD_SOL)
correr <- function(semilla, dg = DG) { e <- new.env(parent = globalenv()); set.seed(semilla); eval(dg, e); e }
rojos <- character(0)
rojo <- function(seccion, detalle) { rojos <<- c(rojos, seccion); cat("  ROJO", seccion, ":", detalle, "\n") }

## Círculos de cada región: se escriben aquí, desde el significado de la región, y no se leen del .Rmd.
SEM <- list(s1 = 1L, p12 = 1:2, s2 = 2L, ctr = 1:3, p13 = c(1L, 3L), p23 = 2:3, s3 = 3L)
RN <- names(SEM)
zonas_de <- function(v, k) as.integer(v)[vapply(SEM, function(s) k %in% s, TRUE)]     # las zonas del círculo k
## Los siete datos que muestra un vector, desde el significado de cada región; no usa datos_desde del .Rmd.
datos_sem <- function(v) {
  v <- stats::setNames(as.integer(v), RN)
  c(a = v[["p12"]], b = v[["p12"]] + v[["ctr"]], c = v[["p23"]] + v[["ctr"]], d = v[["p13"]] + v[["ctr"]],
    P = sum(zonas_de(v, 1L)), M = sum(zonas_de(v, 2L)), B = sum(zonas_de(v, 3L)))
}
## Semillas de cada rama, una sola vez: N = 100 por rama (regla #23).
semillas_por_rama <- local({
  memo <- NULL
  function(n = 100L, tope = 8000L) {
    if (!is.null(memo)) return(memo)
    s <- list(canon = integer(0), resto = integer(0)); k <- 0L
    while ((length(s$canon) < n || length(s$resto) < n) && k < tope) {
      k <- k + 1L; r <- if (correr(k)$es_canonica) "canon" else "resto"
      if (length(s[[r]]) < n) s[[r]] <- c(s[[r]], k)
    }
    memo <<- s; s
  }
})

## --- espacio ---------------------------------------------------------------------------
TAMANO_ESPACIO <- 11000000
en_rejilla <- function(r) {
  r$ctr %in% 2:12 && r$p12 %in% setdiff(2:12, r$ctr) && (r$s1 - r$ctr) %in% 0:9 && (r$s2 - r$ctr) %in% 0:9 &&
    (r$p13 - max(0L, r$p12 - r$ctr)) %in% 0:9 && (r$p23 - max(0L, r$p12 - r$ctr)) %in% 0:9 && (r$s3 - max(0L, r$ctr - r$p12)) %in% 0:9
}
sonda_rejilla <- function(canon) if (is.null(canon)) "CANON_AUSENTE" else if (!en_rejilla(canon)) "CANON_REJILLA" else character(0)
sec_espacio <- function() {
  cat("\n[espacio] enumeración exacta\n")
  e <- correr(1L); F <- e$FORMULAS
  g <- expand.grid(e1 = 0:9, e2 = 0:9, f13 = 0:9, f23 = 0:9, e3 = 0:9); n <- nrow(g)
  comp <- function(M) list(s1 = M[, 1], p12 = M[, 2], s2 = M[, 3], ctr = M[, 4], p13 = M[, 5], p23 = M[, 6], s3 = M[, 7])
  cuenta <- function(M, d) Reduce(`+`, Map(function(x, y) x == y, e$datos_desde(comp(M)), d))
  total <- 0; mal <- c(negativo = 0, mayor99 = 0, iguales = 0, reproduce = 0, cuentas = 0, totales_A = 0, regiones = 0,
                      falla_A = 0, falla_B = 0, centro_doble = 0, resta_una = 0)
  orden_fijo <- 0; extremo <- 0; vmax <- 0
  for (ctr in 2:12) for (p12 in setdiff(2:12, ctr)) {
    piso <- max(0L, p12 - ctr); piso3 <- max(0L, ctr - p12)
    r <- list(s1 = ctr + g$e1, p12 = rep(p12, n), s2 = ctr + g$e2, ctr = rep(ctr, n),
              p13 = piso + g$f13, p23 = piso + g$f23, s3 = piso3 + g$e3)
    d <- e$datos_desde(r); total <- total + n
    K <- F[["CLAVE"]](d); A <- F[["VN-A-CENTRO"]](d); B <- F[["VN-B-TOTALES"]](d); D <- F[["VN-D-SIN-OPERAR"]](d)
    todos <- list(K, A, B, D)
    mal["regiones"] <- mal["regiones"] + sum(rowSums(K != do.call(cbind, r[e$ORDEN])) > 0)
    mal["negativo"] <- mal["negativo"] + sum(vapply(todos, function(M) sum(M < 0), 0))
    mal["mayor99"]  <- mal["mayor99"]  + sum(vapply(todos, function(M) sum(M > 99), 0))
    for (i in 1:3) for (j in (i + 1L):4) mal["iguales"] <- mal["iguales"] + sum(rowSums(todos[[i]] != todos[[j]]) == 0)
    cu <- vapply(todos, cuenta, numeric(n), d = d)
    mal["reproduce"] <- mal["reproduce"] + sum(cu[, 1] != 7 | rowSums(cu[, 2:4, drop = FALSE] == 7) > 0)
    mal["cuentas"]   <- mal["cuentas"]   + sum(cu[, 2] != 5 | cu[, 3] != 4 | cu[, 4] != 0)
    cA <- e$datos_desde(comp(A))
    mal["totales_A"] <- mal["totales_A"] + sum(cA$P != d$P | cA$M != d$M | cA$B != d$B)
    ## Afirmaciones de la Solution, en todo el espacio (regla #13 v1.1).
    cB <- e$datos_desde(comp(B)); dobles <- d$B - d$d - d$c
    mal["falla_A"] <- mal["falla_A"] + sum(!(cA$a != d$a & cA$b != d$b & cA$c == d$c & cA$d == d$d))
    mal["falla_B"] <- mal["falla_B"] + sum(!(cB$a == d$a & cB$b == d$b & cB$c == d$c & cB$d == d$d & cB$P != d$P & cB$M != d$M & cB$B != d$B))
    mal["centro_doble"] <- mal["centro_doble"] + sum(K[, 7] == dobles | A[, 7] == dobles | B[, 7] == dobles | D[, 7] == dobles)
    mal["resta_una"] <- mal["resta_una"] + sum((d$d > d$b - d$a) != (d$P - d$b != K[, 1]))
    orden_fijo <- orden_fijo + sum(rowSums(A) < rowSums(K) & rowSums(K) < rowSums(B) & rowSums(B) < rowSums(D))
    extremo <- extremo + sum(D[, 2] >= 10 & D[, 4] >= 10 & D[, 5] >= 10 & D[, 6] >= 10)
    vmax <- max(vmax, K, A, B, D)
  }
  cat("  versiones enumeradas:", format(total, scientific = FALSE), "\n")
  if (total != TAMANO_ESPACIO) rojo("espacio", "el tamaño cambió: revisar rangos y este script")
  print(mal)
  if (any(mal > 0)) rojo("espacio", "alguna condición de I-3 o I-4 falla")
  cat(sprintf("  suma de regiones A < clave < B < D: %.0f de %.0f (estructural, §P7-F)\n", orden_fijo, total))
  cat(sprintf("  D con dos cifras en centro y tres dobles: %.1f %% · valor máximo en una zona: %d\n", 100 * extremo / total, vmax))
  ## La rejilla de arriba es una copia de los rangos del .Rmd: cada versión real debe caer dentro.
  fuera <- 0L; n <- 0L; canon <- NULL; s <- 0L
  while ((n < 100L || is.null(canon)) && s < 2000L) {
    s <- s + 1L; x <- correr(s)
    if (x$es_canonica) { if (is.null(canon)) canon <- x$reg; next }
    if (n < 100L) { n <- n + 1L; fuera <- fuera + !en_rejilla(x$reg) }
  }
  cat("  versiones reales fuera de la rejilla (de", n, "):", fuera, "\n")
  if (fuera > 0L || n < 100L) rojo("espacio", "la rejilla no es la del .Rmd")
  sr <- sonda_rejilla(canon)
  cat("  canónica dentro de la rejilla:", !length(sr), "\n")
  if (length(sr)) rojo("espacio", sr)
}

## --- cableado --------------------------------------------------------------------------
nums <- function(s) as.integer(regmatches(s, gregexpr("[0-9]+", s))[[1]])
cap <- function(s) paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))
## La pregunta es la del impreso en todas las versiones (I-1, ITEM_OFICIAL.md). Cambiarla cambia qué opción es correcta.
PREGUNTA_LIT <- "De acuerdo con lo anterior, ¿cuál diagrama de Venn organiza los datos de manera adecuada?"
## Las plantillas de contexto, leídas una por una en la FASE 2C. Si cambian, en rojo hasta revisarlas: una frase puede añadir un dato.
MD5_CONTEXTOS <- "c766ff4f6fbe12d9d62eb367c679d753"
sonda_contextos <- function(cod = COD_DG) {
  i <- grep("^contextos <- list\\(", cod); j <- grep("^canonico <- list\\(", cod)
  tf <- tempfile(); on.exit(unlink(tf)); writeLines(cod[i:(j - 1L)], tf, useBytes = TRUE)
  if (!identical(unname(tools::md5sum(tf)), MD5_CONTEXTOS)) "CONTEXTOS_CONGELADOS" else character(0)
}
sonda_metadatos <- function(l = lineas) if (sum(l == "extype: schoice") != 1L || sum(l == "exshuffle: FALSE") != 1L) "CABLEADO_METADATOS" else character(0)
## Si un proceso de mclapply muere, su resultado es NULL y contaría como versión sin fallas.
sonda_hijos <- function(res) if (any(vapply(res, is.null, TRUE))) "HIJO_MUERTO" else character(0)
ANCHO_CIFRA <- 9.086                                        # unidades por cifra de la fuente de los números, medido con pdftotext -bbox
datos_del_texto <- function(e) {
  a <- nums(e$vineta_1); b <- nums(e$vineta_2); p <- nums(e$vineta_3)
  c(a = a[1], b = b[1], c = b[2], d = b[3], P = p[1], M = p[3], B = p[2])   # el impreso nombra P, B, M
}
sondas_version <- function(e, dib) {
  capt <- list()
  e$dibujar_venn <- function(regiones, rotulos, archivo, geom = NULL) { capt[[length(capt) + 1L]] <<- list(r = regiones, rot = rotulos, f = archivo, geom = geom); invisible(archivo) }
  viejo <- options(knitr.in.progress = TRUE); on.exit(options(viejo))
  eval(dib, e)
  f <- character(0)
  dt <- datos_del_texto(e)
  if (!all(dt == unlist(e$dat)[names(dt)])) f <- c(f, "ORACULO_TEXTO")
  if (!identical(e$pregunta, PREGUNTA_LIT)) f <- c(f, "CABLEADO_PREGUNTA")
  if (length(capt) != 4L || sum(e$sol) != 1L) return(c(f, "CABLEADO_CONTEO"))
  if (!all(vapply(capt, function(x) is.null(x$geom), TRUE))) f <- c(f, "CABLEADO_GEOM")   # I-1: ninguna figura lleva geometría propia
  nombres <- vapply(capt, function(x) x$f, "")
  if (!all(grepl("^diagrama_[a-d]_[0-9a-f]{8}\\.png$", nombres)) || !identical(substr(nombres, 10L, 10L), letters[1:4]) ||
      length(unique(substring(nombres, 12L))) != 1L) f <- c(f, "CABLEADO_NOMBRES")
  cumple <- vapply(capt, function(x) sum(e$datos_de(x$r) == dt[c("a", "b", "c", "d", "P", "M", "B")]), 0L)
  letra <- match(substr(nombres, 10L, 10L), letters[1:4])   # la opción la fija la letra del archivo, no el orden de dibujo
  if (anyNA(letra) || cumple[match(which(e$sol), letra)] != 7L || sum(cumple == 7L) != 1L) f <- c(f, "CABLEADO_CLAVE")
  rots <- unique(lapply(capt, function(x) x$rot))
  ## Orden de los rótulos en la llamada: el primer conjunto y el segundo, en el orden de la viñeta 1, y el tercero tras «pero no».
  ## Dónde quedan impresos lo comprueba la sección figura.
  v1 <- tolower(e$vineta_1); r1 <- tolower(rots[[1L]]); pos <- vapply(r1, function(x) regexpr(x, v1, fixed = TRUE)[1L], 0L)
  if (length(rots) != 1L || any(pos < 0L) || !(pos[1L] < pos[2L]) || !grepl(paste0("pero no ", r1[3L]), v1, fixed = TRUE)) f <- c(f, "CABLEADO_ROTULOS")
  f
}
sec_cableado <- function() {
  cat("\n[cableado] N = 100 por rama, con los datos leídos del texto\n")
  li <- grep("^\\* !\\[Diagrama de Venn\\]\\(diagrama_[a-d]_`r fig_id`\\.png\\)\\{width=5\\.5cm style=\"width:280px!important;max-width:100%\"\\}", lineas)
  if (length(li) != 4L || !identical(sub(".*diagrama_([a-d])_.*", "\\1", lineas[li]), letters[1:4])) rojo("cableado", "el Answerlist no está en orden a, b, c, d")
  sm <- c(sonda_metadatos(), sonda_contextos()); if (length(sm)) rojo("cableado", paste(sm, collapse = ", "))
  cuenta <- c(canon = 0L, resto = 0L); pos <- integer(4); fallas <- character(0); s <- 0L
  while (any(cuenta < 100L) && s < 8000L) {
    s <- s + 1L; e <- correr(s); r <- if (e$es_canonica) "canon" else "resto"
    if (cuenta[r] >= 100L) next
    cuenta[r] <- cuenta[r] + 1L
    fallas <- c(fallas, sondas_version(e, DIB))
    if (r == "resto") pos[which(e$sol)] <- pos[which(e$sol)] + 1L
  }
  cat("  semillas recorridas:", s, "· versiones:", paste(names(cuenta), cuenta, collapse = ", "), "· clave por posición:", paste(pos, collapse = " "), "\n")
  if (any(cuenta < 20L)) cat("  NO CONCLUYENTE: estrato con n < 20\n")
  if (length(fallas)) rojo("cableado", paste(names(table(fallas)), table(fallas), collapse = ", "))
  else cat("  sin fallas\n")
}

## --- figura ----------------------------------------------------------------------------
## Lo impreso. pdflatex deja v.pdf, y de ese PDF sale el PNG con pdftoppm. pdftotext -bbox da cada palabra con su caja, sin OCR.
palabras <- function(pdf, g) {
  x <- system2("pdftotext", c("-bbox", shQuote(pdf), "-"), stdout = TRUE); Encoding(x) <- "UTF-8"
  w <- grep("<word ", x, value = TRUE)
  m <- regmatches(w, regexec('xMin="([0-9.]+)" yMin="([0-9.]+)" xMax="([0-9.]+)" yMax="([0-9.]+)">(.*)</word>', w))
  n <- function(i) as.numeric(vapply(m, `[`, "", i)) - g$margen
  data.frame(x0 = n(2L), y0 = n(3L), x1 = n(4L), y1 = n(5L), txt = vapply(m, `[`, "", 6L), stringsAsFactors = FALSE)
}
## Un número vale por los círculos que contienen el centro de su palabra (SEM), no por las anclas del .Rmd.
## Un rótulo está fuera de los tres círculos y pertenece al más cercano.
leer_figura <- function(pdf, g) {
  w <- palabras(pdf, g); num <- grepl("^[0-9]+$", w$txt)
  cx <- vapply(g$circ, function(k) k$x, 0); cy <- vapply(g$circ, function(k) k$y, 0)
  xc <- (w$x0 + w$x1) / 2; yc <- (w$y0 + w$y1) / 2
  vec <- stats::setNames(rep(NA_integer_, 7L), RN); rot <- rep(NA_character_, 3L); ok <- sum(num) == 7L && sum(!num) == 3L
  for (i in seq_len(nrow(w))) {
    d <- unname(sqrt((xc[i] - cx)^2 + (yc[i] - cy)^2))
    if (num[i]) {
      r <- RN[vapply(SEM, function(s) identical(as.integer(s), which(d < g$r)), TRUE)]
      if (length(r) != 1L || !is.na(vec[[r]])) ok <- FALSE else vec[[r]] <- as.integer(w$txt[i])
    } else {
      k <- which.min(d); if (any(d < g$r) || !is.na(rot[k])) ok <- FALSE else rot[k] <- w$txt[i]
    }
  }
  list(ok = ok && !anyNA(vec) && !anyNA(rot), vec = vec, rot = rot, w = w[num, , drop = FALSE])
}
conjuntos_del_texto <- function(e) {
  tolower(regmatches(e$vineta_1, regexec("([[:alpha:]]+) y ([[:alpha:]]+), pero no ([[:alpha:]]+)\\.$", e$vineta_1))[[1L]][2:4])
}
sondas_figura <- function(e, dib = DIB) {
  g <- e$GEOM_VENN; ultimo <- tempfile(fileext = ".pdf"); real <- e$dibujar_venn
  ## El PDF vive dentro de dibujar_venn y se borra al salir. Se conserva interceptando system2 en su entorno, sin tocar su código.
  e$system2 <- function(command, args, ...) {
    r <- base::system2(command, args, ...)
    if (identical(command, "pdftoppm")) file.copy("v.pdf", ultimo, overwrite = TRUE)
    r
  }
  e$dibujar_venn <- function(regiones, rotulos, archivo, ...) {
    real(regiones, rotulos, archivo, ...); file.copy(ultimo, paste0(archivo, ".pdf"), overwrite = TRUE); invisible(archivo)
  }
  td <- tempfile("fig_"); dir.create(td); ini <- setwd(td); viejo <- options(knitr.in.progress = TRUE)
  on.exit({ options(viejo); setwd(ini); unlink(td, recursive = TRUE); unlink(ultimo) })
  eval(dib, e)
  if (!all(file.exists(e$archivos, paste0(e$archivos, ".pdf")))) return("FIG_ARCHIVO")
  ## La letra de la opción la fija el nombre del archivo, que es lo que cita el Answerlist.
  lec <- lapply(e$archivos, function(a) leer_figura(paste0(a, ".pdf"), g))
  if (!all(vapply(lec, function(l) l$ok, TRUE))) return("FIG_PALABRAS")
  f <- character(0); dt <- datos_del_texto(e); ct <- conjuntos_del_texto(e)
  for (i in 1:4) {
    if (!identical(unname(lec[[i]]$vec), as.integer(e$regiones_mezcladas[[i]]))) f <- c(f, "FIG_NUMERO")
    if (!identical(tolower(lec[[i]]$rot), ct)) f <- c(f, "FIG_ROTULO")
  }
  cumple <- vapply(lec, function(l) sum(datos_sem(l$vec) == dt[c("a", "b", "c", "d", "P", "M", "B")]), 0L)
  if (cumple[which(e$sol)] != 7L || !identical(sort(cumple), c(0L, 4L, 5L, 7L))) f <- c(f, "FIG_CLAVE")
  ## I-1: fuera de los números, las cuatro figuras son iguales píxel a píxel.
  img <- lapply(e$archivos, png::readPNG); u <- g$dpi / 72
  mas <- matrix(FALSE, dim(img[[1L]])[1L], dim(img[[1L]])[2L])
  for (l in lec) for (k in seq_len(nrow(l$w))) {
    fi <- seq(max(1L, floor((l$w$y0[k] + g$margen - 1.5) * u)), min(nrow(mas), ceiling((l$w$y1[k] + g$margen + 1.5) * u)))
    co <- seq(max(1L, floor((l$w$x0[k] + g$margen - 1.5) * u)), min(ncol(mas), ceiling((l$w$x1[k] + g$margen + 1.5) * u)))
    mas[fi, co] <- TRUE
  }
  dif <- vapply(img[-1L], function(a) if (!identical(dim(a), dim(img[[1L]]))) Inf else sum(rowSums(abs(a - img[[1L]]), dims = 2L) > 0 & !mas), 0)
  if (any(dif > 0)) f <- c(f, "I1_PIXELES")
  ## I-1 en los propios números, que la máscara de arriba deja fuera: un solo tamaño de letra y ningún color en las cuatro figuras.
  if (length(unique(unlist(lapply(lec, function(l) round(l$w$y1 - l$w$y0, 2L))))) != 1L) f <- c(f, "I1_TAMANO")
  croma <- 0
  for (i in 1:4) for (k in seq_len(nrow(lec[[i]]$w))) {
    w <- lec[[i]]$w[k, ]; h <- w$y1 - w$y0
    fi <- floor((w$y0 + 0.3 * h + g$margen) * u):ceiling((w$y1 - 0.3 * h + g$margen) * u); co <- floor((w$x0 + g$margen) * u):ceiling((w$x1 + g$margen) * u)
    b <- img[[i]][fi, co, 1:3, drop = FALSE]
    croma <- max(croma, pmax(b[, , 1L], b[, , 2L], b[, , 3L]) - pmin(b[, , 1L], b[, , 2L], b[, , 3L]))
  }
  if (croma > 0) f <- c(f, "I1_COLOR")
  ## Los demás rasgos de los números: una sola fuente, posición, ancho por cifra, tinta negra y nada bajo la línea base.
  fu <- unique(unlist(lapply(e$archivos, function(a) sub("^[A-Z]{6}\\+", "", sub(" .*", "", system2("pdffonts", shQuote(paste0(a, ".pdf")), stdout = TRUE)[-(1:2)])))))
  if (length(fu) != 1L) f <- c(f, "I1_FUENTE")
  ax <- vapply(g$anclas, function(a) a$x_izq, 0); ay <- vapply(g$anclas, function(a) a$y, 0)
  for (i in 1:4) { w <- lec[[i]]$w; for (k in seq_len(nrow(w))) {
    z <- w[k, ]; r <- which.min(abs(z$x0 - ax) + abs((z$y0 + z$y1) / 2 - ay))
    if (abs(z$x0 - ax[r] + g$lsb_numero) > 0.8) f <- c(f, "I1_POS")
    ## La vertical: el centro de la caja de la palabra queda 2,22 unidades bajo el ancla (medido 2,217 a 2,218 en 100 versiones); I1_POS solo ve la horizontal.
    if (abs((z$y0 + z$y1) / 2 - ay[r] - 2.22) > 0.8) f <- c(f, "I1_POSY")
    if (abs((z$x1 - z$x0) / nchar(z$txt) - ANCHO_CIFRA) > 0.05) f <- c(f, "I1_ANCHO")
    base <- ay[r] + g$alto_digito / 2; co <- floor((z$x0 + g$margen) * u):ceiling((z$x1 + g$margen) * u)
    fi <- floor((base - g$alto_digito + g$margen) * u):ceiling((base + g$margen) * u)
    if (min(img[[i]][fi, co, 1:3]) > 0.1) f <- c(f, "I1_GRIS")
    fi <- ceiling((base + 0.9 + g$margen) * u):floor((z$y1 + g$margen) * u)
    if (min(img[[i]][fi, co, 1:3]) < 0.9) f <- c(f, "I1_MARCA")
  } }
  unique(f)
}
sec_figura <- function() {
  cat("\n[figura] N = 100 por rama, render real, leyendo lo impreso con pdftotext -bbox\n")
  s <- semillas_por_rama(); t0 <- proc.time()[["elapsed"]]; todas <- c(s$canon, s$resto)
  res <- parallel::mclapply(todas, function(k) sondas_figura(correr(k)), mc.cores = min(10L, parallel::detectCores()))
  sh <- sonda_hijos(res); if (length(sh)) rojo("figura", sh)
  mal <- unlist(lapply(res, function(x) if (inherits(x, "try-error")) "ERROR" else x))
  cat(sprintf("  versiones: canon %d, resto %d · figuras leídas: %d · con falla: %d · %.0f s\n", length(s$canon), length(s$resto), 4L * length(todas),
              sum(lengths(res) > 0L), proc.time()[["elapsed"]] - t0))
  if (any(lengths(s) < 20L)) cat("  NO CONCLUYENTE: estrato con n < 20\n")
  if (length(mal)) rojo("figura", paste(names(table(mal)), table(mal), collapse = ", ")) else cat("  sin fallas: números, rótulos, clave e I-1\n")
}

## --- tinta -----------------------------------------------------------------------------
tinta_cruda <- function(f, g) {
  a <- png::readPNG(f); mx <- pmax(a[, , 1], a[, , 2], a[, , 3]); mn <- pmin(a[, , 1], a[, , 2], a[, , 3])
  i <- which(mx < 105 / 255 & (mx - mn) < 60 / 255, arr.ind = TRUE); u <- g$dpi / 72
  data.frame(x = (i[, 2] - 0.5) / u - g$margen, y = (i[, 1] - 0.5) / u - g$margen)
}
leer_tinta <- function(f, g) { t <- tinta_cruda(f, g); t[t$x > 3.5 & t$x < g$W - 3.5 & t$y > 3.5 & t$y < g$H - 3.5, ] }
## Distancia de la tinta de los rótulos al eje del marco, sobre el PNG sin recortar. La banda del propio marco no cuenta;
## un rótulo pegado a ella da menos de 2, y uno que la rebasa da negativo.
distancia_marco <- function(f, g) {
  t <- tinta_cruda(f, g); t <- t[t$y < 24 | (t$x > 150 & t$y > 205), ]
  d <- pmin(t$x, g$W - t$x, t$y, g$H - t$y)
  min(d[abs(d) > g$trazo_marco / 2 + 0.6])
}
sondas_bordes <- function(e) {
  g <- e$GEOM_VENN; ctxs <- c(lapply(e$contextos, function(k) k$conjuntos), list(e$canonico$conjuntos))
  d <- vapply(ctxs, function(k) distancia_marco(dibuja(e, rep(88L, 7L), vapply(k, function(z) z$rotulo, ""), g), g), 0)
  list(d = d, fallas = if (any(d < 2)) "BORDE_ROTULO" else character(0))
}
sondas_tinta <- function(t, g) {
  cx <- vapply(g$circ, function(k) k$x, 0); cy <- vapply(g$circ, function(k) k$y, 0)
  f <- character(0); hol <- Inf; usado <- rep(FALSE, nrow(t)); peor_izq <- 0
  for (r in names(SEM)) {
    a <- g$anclas[[r]]
    w <- t$x >= a$x_izq - 9 & t$x <= a$x_izq + 27 & abs(t$y - a$y) <= 12
    if (sum(w) < 20L) { f <- c(f, paste0("AUSENTE_", r)); next }
    usado <- usado | w; tw <- t[w, ]
    iz <- abs(min(tw$x) - a$x_izq); peor_izq <- max(peor_izq, iz)
    if (iz > 1.5) f <- c(f, "IZQ")
    for (k in 1:3) {
      d <- sqrt((tw$x - cx[k])^2 + (tw$y - cy[k])^2)
      if (any((d < g$r) != (k %in% SEM[[r]]))) f <- c(f, "REGION")
      hol <- min(hol, min(abs(d - g$r)) - g$trazo_circulo / 2)
    }
  }
  if (hol < 2) f <- c(f, "HOLGURA")
  rot <- t$y < 24 | (t$x > 150 & t$y > 205)
  if (sum(!usado & !rot) >= 40L) f <- c(f, "TINTA_EXTRA")
  list(fallas = unique(f), holgura = hol, izq = peor_izq)
}
dibuja <- function(e, v, rot, g = e$GEOM_VENN) { f <- tempfile(fileext = ".png"); e$dibujar_venn(v, rot, f, g); f }
sec_tinta <- function() {
  cat("\n[tinta] render real, toda la caja de tinta\n")
  e <- correr(1L); g <- e$GEOM_VENN; rot_largos <- c("Baloncesto", "Atletismo", "Natación")
  t0 <- proc.time()[["elapsed"]]; hol <- Inf; izq <- 0; mal <- character(0)
  for (v in 0:99) {
    r <- sondas_tinta(leer_tinta(dibuja(e, rep(v, 7L), rot_largos), g), g)
    hol <- min(hol, r$holgura); izq <- max(izq, r$izq); if (length(r$fallas)) mal <- c(mal, paste0(v, ":", paste(r$fallas, collapse = "+")))
  }
  cat(sprintf("  valores de 0 a 99 en las siete zonas: %d con falla · holgura mínima número-trazo %.2f unidades · peor borde izquierdo %.2f · %.0f s\n",
              length(mal), hol, izq, proc.time()[["elapsed"]] - t0))
  if (length(mal)) rojo("tinta", paste(head(mal, 6), collapse = ", "))
  mp <- sondas_mapeo(e, g); if (length(mp)) rojo("tinta", paste(mp, collapse = ", ")) else cat("  MAPEO: cada posición del vector cae en su región\n")
  cx <- vapply(g$circ, function(k) k$x, 0); cy <- vapply(g$circ, function(k) k$y, 0)
  ctxs <- c(lapply(e$contextos, function(k) k$conjuntos), list(e$canonico$conjuntos)); peor <- Inf
  for (k in ctxs) {
    rot <- vapply(k, function(z) z$rotulo, ""); t <- leer_tinta(dibuja(e, rep(88L, 7L), rot), g)
    tr <- t[t$y < 24 | (t$x > 150 & t$y > 205), ]
    for (j in 1:3) peor <- min(peor, min(abs(sqrt((tr$x - cx[j])^2 + (tr$y - cy[j])^2) - g$r)) - g$trazo_circulo / 2)
  }
  b <- sondas_bordes(e)
  cat(sprintf("  rótulos de los %d contextos: holgura rótulo-trazo mínima %.2f; distancia mínima al eje del marco, sin recortar, %.2f unidades (rojo por debajo de 2)\n",
              length(ctxs), peor, min(b$d)))
  if (peor < 1) rojo("tinta", "un rótulo toca un trazo")
  if (length(b$fallas)) rojo("tinta", "un rótulo está a menos de 2 unidades del marco")
  sc <- sonda_copia(g, e$dibujar_venn); if (length(sc)) rojo("tinta", paste(sc, collapse = ", ")) else cat("  la copia del .Rmd coincide con tikz/venn_tikz.R y con geom_venn.json\n")
}
## GEOM_VENN y dibujar_venn del .Rmd son copia de salida/flujo_b/tikz/venn_tikz.R; los trazos y el radio, de geom_venn.json.
sonda_copia <- function(g, dv = NULL) {
  e2 <- new.env(); eval(parse(file.path(aqui, "salida/flujo_b/tikz/venn_tikz.R"), encoding = "UTF-8", keep.source = FALSE), e2)
  j <- jsonlite::fromJSON(file.path(aqui, "salida/flujo_b/geom_venn.json"), simplifyVector = FALSE); f <- character(0)
  cuerpo <- function(fn) { x <- deparse(body(fn), width.cutoff = 200L); x[!grepl("file.copy|writeBin|salida <-", x)] }
  if (!identical(g, e2$GEOM_VENN) || (!is.null(dv) && !identical(cuerpo(dv), cuerpo(e2$dibujar_venn)))) f <- c(f, "COPIA_TIKZ")
  if (!identical(c(g$trazo_marco, g$trazo_circulo, g$r), c(j$trazo_marco, j$trazo_circulo, j$r))) f <- c(f, "COPIA_JSON")
  f
}
sondas_mapeo <- function(e, g) {
  ## Una sola zona con dos cifras: la tinta ancha debe caer en la ventana de esa zona.
  f <- character(0)
  for (i in 1:7) {
    v <- rep(1L, 7L); v[i] <- 88L; t <- leer_tinta(dibuja(e, v, c("A", "B", "C"), g), g)
    anchos <- vapply(names(SEM), function(r) { a <- e$GEOM_VENN$anclas[[r]]; w <- t$x >= a$x_izq - 9 & t$x <= a$x_izq + 27 & abs(t$y - a$y) <= 12; if (any(w)) diff(range(t$x[w])) else 0 }, 0)
    if (names(which.max(anchos)) != names(SEM)[i]) f <- c(f, "MAPEO")
  }
  unique(f)
}

## --- solucion ---------------------------------------------------------------------------
## Cuatro defectos de prosa (FASE 2C r1) pasaron porque nada ejecutaba este chunk. Cada párrafo debe casar con una plantilla
## cuyas cifras se contrastan con la figura y el enunciado, o estar en SOL_LITERALES. Un párrafo nuevo o reescrito da
## SOL_SIN_ORACULO: la afirmación que no se haya revisado no pasa en silencio.
SOL_ENCABEZADOS <- paste0("**", c("Respuesta correcta", "Análisis del error", "Procedimiento correcto", "Propiedades del concepto", "Caso específico",
                                  "Análisis de las opciones incorrectas", "Reflexión metacognitiva", "Estrategia para evitar el error"), "**")
## Frases sin datos sorteados, leídas una por una: son ciertas por definición. Las que cuentan zonas se comprueban abajo (inc).
SOL_LITERALES <- c(
  "**Qué evalúa este ítem:** la capacidad de organizar datos de intersección y totales en un diagrama de Venn.",
  "*Causa de fondo:* leer «X y Y» y «X y Y, pero no Z» como si dijeran lo mismo. La primera frase cuenta toda la intersección; la segunda, solo la parte que queda fuera del tercer conjunto.",
  "- Las siete zonas no se solapan: cada elemento se cuenta en una sola.",
  "- La intersección de dos conjuntos se parte en dos zonas: la que queda fuera del tercero y el centro.",
  "- El total de un conjunto es la suma de las cuatro zonas de su círculo.",
  "- Una cifra del enunciado no siempre aparece tal cual en una zona: muchas son la suma de varias.",
  "- Empieza por el centro y avanza hacia afuera: centro, zonas de dos conjuntos y zonas de un solo conjunto.",
  "- Marca en cada frase si habla de una zona o de varias: «pero no» señala una sola zona.",
  "- Antes de elegir, comprueba los siete datos: los tres totales, las tres intersecciones y el dato «pero no».",
  "- Si un diagrama falla en un dato, descártalo; si cumple uno, sigue revisando.")
SOL_LATEX <- "\\nobreak\\makeatletter\\@afterindenttrue\\@afterheading\\makeatother"     # lo único que va en los bloques {=latex}
SOL_REFLEXIONES <- c(
  "Antes de escribir un número en una zona, pregúntate a quiénes cuenta esa zona y a quiénes deja por fuera.",
  "Una frase con «pero no» describe una parte de una intersección: compárala con la frase que habla de la intersección completa.",
  "Que los totales cuadren no prueba que el diagrama esté bien: revisa cada dato del enunciado, no solo algunos.",
  "Si un dato no se deja ubicar en una sola zona, es porque reúne varias: sepáralas antes de escribir.",
  "Cuando dos diagramas se parezcan, busca el dato que los distingue en lugar de mirar el conjunto de los números.")
## Cada párrafo con datos, letra a letra, desde las viñetas que lee el estudiante; no usa el pool del .Rmd.
esperado_solucion <- function(e) {
  m <- regmatches(e$vineta_1, regexec("^([0-9]+) (\\S+) (\\S+) (.+) y (.+), pero no (.+)\\.$", e$vineta_1))[[1]]
  a <- as.integer(m[2]); G <- m[3]; V <- m[4]; T1 <- m[5]; T2 <- m[6]; T3 <- m[7]
  n2 <- nums(e$vineta_2); n3 <- nums(e$vineta_3); b <- n2[1]; cc <- n2[2]; d <- n2[3]; P <- n3[1]; B <- n3[2]; M <- n3[3]; z <- b - a
  s1 <- P-a-d; s2 <- M-a-cc; s3 <- B-d-cc+z; p13 <- d-z; p23 <- cc-z
  Z12 <- paste0(T1," y ",T2," sin ",T3); Z13 <- paste0(T1," y ",T3," sin ",T2); Z23 <- paste0(T2," y ",T3," sin ",T1)
  C1 <- "leer «X y Y» y «X y Y, pero no Z» como si dijeran lo mismo, y repartir los dos datos al revés"
  x <- c(
   sprintf("La respuesta correcta es el diagrama con %d en el centro, %d en la zona de %s y %d en la zona de solo %s: es el único de los cuatro que reproduce los siete datos.", z, a, Z12, s1, T1),
   sprintf("La trampa está en la frase «pero no %s». Ese dato ocupa una zona del diagrama, la de %s, y no el centro. En cambio, el dato de %d %s que %s %s y %s incluye a quienes también %s %s: es esa zona más el centro.", T3, Z12, b, G, V, T1, T2, V, T3),
   sprintf("Revisar solo los totales no basta: uno de los diagramas los conserva todos y aun así falla los dos datos de %s y %s.", T1, T2),
   sprintf("- **Centro.** Quienes están en los tres conjuntos son los de %s y %s menos los que no están en %s: $%d - %d = %d$.", T1, T2, T3, b, a, z),
   sprintf("- **Zonas de dos conjuntos.** %s: $%d$. %s: $%d - %d = %d$. %s: $%d - %d = %d$.", cap(Z12), a, cap(Z23), cc, z, p23, cap(Z13), d, z, p13),
   sprintf("- **Zonas de un solo conjunto.** Solo %s: $%d - %d - %d = %d$. Solo %s: $%d - %d - %d = %d$. Solo %s: $%d - %d - %d + %d = %d$, porque %d y %d cuentan dos veces el centro.", T1,P,a,d,s1,T2,M,a,cc,s2,T3,B,d,cc,z,s3,d,cc),
   sprintf("Con el diagrama correcto, el círculo de %s suma $%d + %d + %d + %d = %d$, el de %s suma $%d + %d + %d + %d = %d$ y el de %s suma $%d + %d + %d + %d = %d$. Además, %s y %s suman $%d + %d = %d$, %s y %s suman $%d + %d = %d$, y %s y %s suman $%d + %d = %d$.",
     T1,s1,a,p13,z,P, T2,s2,a,p23,z,M, T3,s3,p13,p23,z,B, T1,T2,a,z,b, T2,T3,p23,z,cc, T1,T3,p13,z,d),
   paste0("**Poner en el centro el dato «pero no».** Es posible que los estudiantes que eligen el diagrama con ",a," en el centro, ",b," en la zona de ",Z12," y ",P-b-d," en la zona de solo ",T1," tomen a quienes están fuera del tercer conjunto como si fueran el centro. El dato de ",b," ",G," quedó entero en la zona de ",Z12,", y esa zona con el centro suma $",b," + ",a," = ",a+b,"$, no ",b,". Desde ahí las demás restas están bien hechas: por eso los tres círculos suman ",P,", ",M," y ",B,", y revisar solo los totales no lo delata. *Causa de fondo:* ",C1,"."),
   paste0("**Confundir el total de un conjunto con quienes están solo en él.** Es posible que los estudiantes que eligen el diagrama con ",z," en el centro, ",a," en la zona de ",Z12," y ",P," en la zona de solo ",T1," dejen cada total en la zona de un solo conjunto, sin restar lo que ese conjunto comparte. El centro y las zonas de dos conjuntos están bien, pero el círculo de ",T1," suma $",P," + ",a," + ",z," + ",p13," = ",P+a+z+p13,"$, no ",P,". Lo mismo pasa con los círculos de ",T2," y de ",T3,". *Causa de fondo:* creer que el total de un conjunto cuenta solo a quienes no están en ningún otro."),
   paste0("**Poner cada dato en una zona, sin operar.** Es posible que los estudiantes que eligen el diagrama con ",a," en el centro, ",b," en la zona de ",Z12," y ",P," en la zona de solo ",T1," repartan las cifras del enunciado entre las zonas sin hacer ninguna resta. Aquí ",a," también quedó en el centro y ninguna cifra se restó, así que no se cumple ni un solo dato. Por ejemplo, el círculo de ",T1," suma $",P," + ",b," + ",a," + ",d," = ",P+a+b+d,"$, no ",P,". *Causa de fondo:* tratar cada frase del enunciado como el contenido de una zona, cuando seis de los siete datos reúnen varias zonas."))
  otro <- if (e$otro_error$codigo == "VN-X-RESTA-UNA") paste0("**Otro error posible: Restarle al total solo una de las intersecciones.** Es posible que los estudiantes descuenten del total una intersección y olviden la otra. Para llegar a la zona de solo ",T1," no basta con quitarle al total la intersección con ",T2,": $",P," - ",b," = ",P-b,"$ todavía incluye la zona de ",Z13,". *Causa de fondo:* mirar un solo vecino de cada zona en lugar de todas las zonas que comparten el círculo.")
    else paste0("**Otro error posible: Restar dos veces el centro.** Es posible que los estudiantes le quiten al total del tercer conjunto las dos intersecciones y no devuelvan el centro. Para llegar a la zona de solo ",T3," no basta con quitarle al total las dos intersecciones: en $",B," - ",d," - ",cc," = ",B-d-cc,"$ el centro quedó restado dos veces, porque pertenece a las dos, y por eso el resultado se queda corto en ",z,". Hay que sumarlo una vez: $",B-d-cc," + ",z," = ",s3,"$. *Causa de fondo:* olvidar que el centro pertenece a las dos intersecciones que se restan.")
  c(x, otro)
}
texto_solucion <- function(e, sol = SOL) {
  x <- capture.output(eval(sol, e)); en <- FALSE; out <- character(0); ltx <- character(0); valla <- strrep(intToUtf8(96L), 3L)
  for (l in x) { if (startsWith(l, valla)) { en <- !en; next }; if (en) ltx <- c(ltx, l) else if (nzchar(trimws(l))) out <- c(out, l) }
  attr(out, "latex") <- ltx
  out
}
sonda_arit <- function(L) {
  f <- character(0)
  for (l in L) for (x in regmatches(l, gregexpr("\\$[^$]+\\$", l))[[1L]]) {
    p <- strsplit(gsub("$", "", x, fixed = TRUE), "=", fixed = TRUE)[[1L]]
    if (length(p) == 2L && eval(parse(text = p[1L])) != eval(parse(text = p[2L]))) f <- "SOL_ARIT"
  }
  f
}
sondas_solucion <- function(e, sol = SOL) {
  L <- texto_solucion(e, sol); dt <- datos_del_texto(e); ct <- conjuntos_del_texto(e); t1 <- ct[1L]; t2 <- ct[2L]; t3 <- ct[3L]
  rg <- lapply(e$regiones_mezcladas, as.integer); kv <- rg[[which(e$sol)]]
  zn <- function(v, z) v[[match(z, RN)]]; nro <- function(x) as.integer(x); low <- function(x) tolower(x)
  sumandos <- function(s) as.integer(strsplit(s, " + ", fixed = TRUE)[[1L]])
  mismos <- function(x, y) identical(sort(as.integer(x)), sort(as.integer(y)))
  opcion <- function(cc, p12, s1) {
    i <- which(vapply(rg, function(v) zn(v, "ctr") == cc && zn(v, "p12") == p12 && zn(v, "s1") == s1, TRUE))
    if (length(i) == 1L) rg[[i]] else NULL
  }
  medio <- function(v) v[match(c("p12", "ctr", "p13", "p23"), RN)]
  solas <- function(v) v[match(c("s1", "s2", "s3"), RN)]
  ## Las zonas que reúne cada dato, desde el .Rmd y desde el significado de cada región: deben coincidir.
  inc <- vapply(RN, function(z) { r <- stats::setNames(as.list(rep(0L, 7L)), RN); r[[z]] <- 1L; unlist(e$datos_desde(r)) }, numeric(7L))
  inc2 <- vapply(RN, function(z) { v <- stats::setNames(rep(0L, 7L), RN); v[[z]] <- 1L; as.numeric(datos_sem(v)) }, numeric(7L))
  zonas_por_dato <- unname(rowSums(inc))
  f <- character(0)
  if (!all(inc == inc2) || !identical(zonas_por_dato, c(1, 2, 2, 2, 4, 4, 4))) f <- c(f, "SOL_AFIRMACION")
  vistas <- list()
  ver <- function(m) {        # la opción que describe un párrafo de diagnóstico: una sola, distractor y sin repetir
    v <- opcion(nro(m[1L]), nro(m[2L]), nro(m[3L]))
    if (is.null(v) || identical(v, kv) || any(vapply(vistas, identical, TRUE, v))) { f <<- c(f, "SOL_OPCION"); return(NULL) }
    vistas[[length(vistas) + 1L]] <<- v; v
  }
  D_OP <- "Es posible que los estudiantes que eligen el diagrama con (\\d+) en el centro, (\\d+) en la zona de .+ y (\\d+) en la zona de solo .+ "
  SUMA <- "\\$([0-9 +]+) = (\\d+)\\$"
  P <- list(
    respuesta = list(n = 1L, p = "^La respuesta correcta es el diagrama con (\\d+) en el centro, (\\d+) en la zona de .+ y (\\d+) en la zona de solo .+: es el único de los cuatro que reproduce los siete datos\\.$",
      f = function(m) { v <- opcion(nro(m[1L]), nro(m[2L]), nro(m[3L])); rep7 <- vapply(rg, function(x) all(datos_sem(x) == dt), TRUE)
        if (is.null(v) || !identical(v, kv) || sum(rep7) != 1L) "SOL_RESPUESTA" }),
    trampa = list(n = 1L, p = "^La trampa está en la frase «pero no (.+)»\\. Ese dato ocupa una zona del diagrama, la de (.+), y no el centro\\. En cambio, el dato de (\\d+) .+ incluye a quienes también .+: es esa zona más el centro\\.$",
      f = function(m) if (low(m[1L]) != t3 || low(m[2L]) != paste0(t1, " y ", t2, " sin ", t3) || nro(m[3L]) != dt[["b"]] || zn(kv, "ctr") < 1L) "SOL_AFIRMACION"),
    totales = list(n = 1L, p = "^Revisar solo los totales no basta: uno de los diagramas los conserva todos y aun así falla los dos datos de (.+) y (.+)\\.$",
      f = function(m) { x <- vapply(rg, function(v) identical(names(which(datos_sem(v) != dt)), c("a", "b")), TRUE)
        if (low(m[1L]) != t1 || low(m[2L]) != t2 || sum(x) != 1L) "SOL_AFIRMACION" }),
    centro = list(n = 1L, p = "^- \\*\\*Centro\\.\\*\\* Quienes están en los tres conjuntos son los de .+ y .+ menos los que no están en .+: \\$(\\d+) - (\\d+) = (\\d+)\\$\\.$",
      f = function(m) if (!identical(nro(m), c(dt[["b"]], dt[["a"]], zn(kv, "ctr")))) "SOL_PROCEDIMIENTO"),
    dobles = list(n = 1L, p = "^- \\*\\*Zonas de dos conjuntos\\.\\*\\* .+: \\$(\\d+)\\$\\. .+: \\$(\\d+) - (\\d+) = (\\d+)\\$\\. .+: \\$(\\d+) - (\\d+) = (\\d+)\\$\\.$",
      f = function(m) if (!identical(nro(m), c(zn(kv, "p12"), dt[["c"]], zn(kv, "ctr"), zn(kv, "p23"), dt[["d"]], zn(kv, "ctr"), zn(kv, "p13")))) "SOL_PROCEDIMIENTO"),
    solas = list(n = 1L, p = "^- \\*\\*Zonas de un solo conjunto\\.\\*\\* Solo .+: \\$(\\d+) - (\\d+) - (\\d+) = (\\d+)\\$\\. Solo .+: \\$(\\d+) - (\\d+) - (\\d+) = (\\d+)\\$\\. Solo .+: \\$(\\d+) - (\\d+) - (\\d+) \\+ (\\d+) = (\\d+)\\$, porque (\\d+) y (\\d+) cuentan dos veces el centro\\.$",
      f = function(m) if (!identical(nro(m), c(dt[["P"]], dt[["a"]], dt[["d"]], zn(kv, "s1"), dt[["M"]], dt[["a"]], dt[["c"]], zn(kv, "s2"),
                                               dt[["B"]], dt[["d"]], dt[["c"]], zn(kv, "ctr"), zn(kv, "s3"), dt[["d"]], dt[["c"]]))) "SOL_PROCEDIMIENTO"),
    caso = list(n = 1L, p = paste0("^Con el diagrama correcto, el círculo de .+ suma ", SUMA, ", el de .+ suma ", SUMA, " y el de .+ suma ", SUMA, "\\. Además, .+ y .+ suman ", SUMA, ", .+ y .+ suman ", SUMA, ", y .+ y .+ suman ", SUMA, "\\.$"),
      f = function(m) { zz <- list(zonas_de(kv, 1L), zonas_de(kv, 2L), zonas_de(kv, 3L), c(zn(kv, "p12"), zn(kv, "ctr")), c(zn(kv, "p23"), zn(kv, "ctr")), c(zn(kv, "p13"), zn(kv, "ctr")))
        q <- unname(dt[c("P", "M", "B", "b", "c", "d")])
        ok <- vapply(1:6, function(i) mismos(sumandos(m[2L * i - 1L]), zz[[i]]) && nro(m[2L * i]) == q[i], TRUE)
        if (!all(ok)) "SOL_CASO" }),
    op_centro = list(n = 1L, p = paste0("^\\*\\*Poner en el centro el dato «pero no»\\.\\*\\* ", D_OP, "tomen a quienes están fuera del tercer conjunto como si fueran el centro\\. El dato de (\\d+) .+ quedó entero en la zona de .+, y esa zona con el centro suma ", SUMA, ", no (\\d+)\\. Desde ahí las demás restas están bien hechas: por eso los tres círculos suman (\\d+), (\\d+) y (\\d+), y revisar solo los totales no lo delata\\. \\*Causa de fondo:\\* .+\\.$"),
      f = function(m) { v <- ver(m); if (is.null(v)) return(NULL)
        ok <- zn(v, "p12") == dt[["b"]] && nro(m[4L]) == dt[["b"]] && zn(v, "ctr") == dt[["a"]] && mismos(sumandos(m[5L]), c(zn(v, "p12"), zn(v, "ctr"))) && nro(m[6L]) == zn(v, "p12") + zn(v, "ctr") &&
          nro(m[7L]) == dt[["b"]] && nro(m[6L]) != nro(m[7L]) && identical(nro(m[8:10]), unname(dt[c("P", "M", "B")])) && identical(datos_sem(v)[c("P", "M", "B")], dt[c("P", "M", "B")])
        if (!ok) "SOL_CIRCULO" }),
    op_totales = list(n = 1L, p = paste0("^\\*\\*Confundir el total de un conjunto con quienes están solo en él\\.\\*\\* ", D_OP, "dejen cada total en la zona de un solo conjunto, sin restar lo que ese conjunto comparte\\. El centro y las zonas de dos conjuntos están bien, pero el círculo de (.+) suma ", SUMA, ", no (\\d+)\\. Lo mismo pasa con los círculos de (.+) y de (.+)\\. \\*Causa de fondo:\\* .+\\.$"),
      f = function(m) { v <- ver(m); if (is.null(v)) return(NULL)
        ok <- identical(medio(v), medio(kv)) && identical(solas(v), unname(dt[c("P", "M", "B")])) && low(m[4L]) == t1 && mismos(sumandos(m[5L]), zonas_de(v, 1L)) && nro(m[6L]) == sum(zonas_de(v, 1L)) &&
          nro(m[7L]) == dt[["P"]] && nro(m[6L]) != nro(m[7L]) && low(m[8L]) == t2 && low(m[9L]) == t3 && sum(zonas_de(v, 2L)) != dt[["M"]] && sum(zonas_de(v, 3L)) != dt[["B"]]
        if (!ok) "SOL_CIRCULO" }),
    op_sin_operar = list(n = 1L, p = paste0("^\\*\\*Poner cada dato en una zona, sin operar\\.\\*\\* ", D_OP, "repartan las cifras del enunciado entre las zonas sin hacer ninguna resta\\. Aquí (\\d+) también quedó en el centro y ninguna cifra se restó, así que no se cumple ni un solo dato\\. Por ejemplo, el círculo de (.+) suma ", SUMA, ", no (\\d+)\\. \\*Causa de fondo:\\* tratar cada frase del enunciado como el contenido de una zona, cuando seis de los siete datos reúnen varias zonas\\.$"),
      f = function(m) { v <- ver(m); if (is.null(v)) return(NULL)
        ok <- nro(m[4L]) == dt[["a"]] && zn(v, "ctr") == dt[["a"]] && mismos(v, dt) && !any(datos_sem(v) == dt) && low(m[5L]) == t1 && mismos(sumandos(m[6L]), zonas_de(v, 1L)) &&
          nro(m[7L]) == sum(zonas_de(v, 1L)) && nro(m[8L]) == dt[["P"]] && sum(zonas_por_dato > 1) == 6L
        if (!ok) "SOL_CIRCULO" }),
    otro_resta = list(n = NA, p = "^\\*\\*Otro error posible: Restarle al total solo una de las intersecciones\\.\\*\\* Es posible que los estudiantes descuenten del total una intersección y olviden la otra\\. Para llegar a la zona de solo (.+) no basta con quitarle al total la intersección con (.+): \\$(\\d+) - (\\d+) = (\\d+)\\$ todavía incluye la zona de (.+)\\. \\*Causa de fondo:\\* .+\\.$",
      f = function(m) { val <- zn(kv, "s1") + zn(kv, "p13")
        ok <- low(m[1L]) == t1 && low(m[2L]) == t2 && identical(nro(m[3:5]), c(dt[["P"]], dt[["b"]], val)) && low(m[6L]) == paste0(t1, " y ", t3, " sin ", t2) && zn(kv, "p13") > 0L && !val %in% vapply(rg, zn, 0L, "s1")
        if (!ok) "SOL_OTRO" }),
    otro_doble = list(n = NA, p = "^\\*\\*Otro error posible: Restar dos veces el centro\\.\\*\\* Es posible que los estudiantes le quiten al total del tercer conjunto las dos intersecciones y no devuelvan el centro\\. Para llegar a la zona de solo (.+) no basta con quitarle al total las dos intersecciones: en \\$(\\d+) - (\\d+) - (\\d+) = (-?\\d+)\\$ el centro quedó restado dos veces, porque pertenece a las dos, y por eso el resultado se queda corto en (\\d+)\\. Hay que sumarlo una vez: \\$(-?\\d+) \\+ (\\d+) = (\\d+)\\$\\. \\*Causa de fondo:\\* .+\\.$",
      f = function(m) { z <- zn(kv, "ctr"); val <- zn(kv, "s3") - z
        ok <- low(m[1L]) == t3 && identical(nro(m[2:9]), c(dt[["B"]], dt[["d"]], dt[["c"]], val, z, val, z, zn(kv, "s3"))) && z > 0L && !val %in% vapply(rg, zn, 0L, "s3")
        if (!ok) "SOL_OTRO" })
  )
  cuenta <- stats::setNames(integer(length(P)), names(P)); enc <- character(0); img <- 0L; refl <- 0L; lit <- character(0)
  for (l in L) {
    if (l %in% SOL_ENCABEZADOS) { enc <- c(enc, l); next }
    if (l %in% SOL_LITERALES) { lit <- c(lit, l); next }
    if (l %in% SOL_REFLEXIONES) { refl <- refl + 1L; next }
    if (startsWith(l, "![Diagrama de Venn de la respuesta correcta](")) {
      img <- img + 1L
      if (!identical(sub("^!\\[[^]]*\\]\\(([^)]+)\\).*$", "\\1", l), e$archivos[which(e$sol)]) ||
          !grepl("{width=5.5cm style=\"width:280px!important;max-width:100%\"}&#8203;", l, fixed = TRUE)) f <- c(f, "SOL_IMAGEN")
      next
    }
    casadas <- 0L
    for (n in names(P)) {
      m <- regmatches(l, regexec(P[[n]]$p, l, perl = TRUE))[[1L]]
      ## Primero el oráculo y luego la concatenación: ver() añade a f con <<-, y en c(f, oraculo()) f ya estaría evaluada.
      if (length(m)) { casadas <- casadas + 1L; cuenta[n] <- cuenta[n] + 1L; r <- P[[n]]$f(m[-1L]); f <- c(f, r) }
    }
    if (casadas != 1L) f <- c(f, "SOL_SIN_ORACULO")
  }
  fijas <- !vapply(P, function(x) is.na(x$n), TRUE)
  if (!identical(enc, SOL_ENCABEZADOS) || !identical(sort(lit), sort(SOL_LITERALES)) || refl != 1L || img != 1L || any(cuenta[fijas] != 1L) ||
      sum(cuenta[!fijas]) != 1L) f <- c(f, "SOL_MUDO")
  for (i in 1:4) {
    x <- e$feedback[i]; v <- rg[[i]]; m <- regmatches(x, regexec("^Incorrecta\\. Los tres totales cuadran, pero (\\d+) va en la zona de (.+), no en el centro\\.$", x, perl = TRUE))[[1L]]
    ok <- if (x == "Correcta. Este diagrama reproduce los siete datos.") e$sol[i] && all(datos_sem(v) == dt)
      else if (length(m)) !e$sol[i] && nro(m[2L]) == dt[["a"]] && zn(v, "ctr") == dt[["a"]] && low(m[3L]) == paste0(t1, " y ", t2, " sin ", t3) && identical(datos_sem(v)[c("P", "M", "B")], dt[c("P", "M", "B")])
      else if (x == "Incorrecta. El centro y las zonas de dos conjuntos están bien, pero cada total quedó en la zona de un solo conjunto.") !e$sol[i] && identical(medio(v), medio(kv)) && identical(solas(v), unname(dt[c("P", "M", "B")]))
      else if (x == "Incorrecta. Cada dato quedó en una zona sin operar, y ninguno de los siete se cumple.") !e$sol[i] && mismos(v, dt) && !any(datos_sem(v) == dt)
      else FALSE
    if (!isTRUE(ok)) f <- c(f, "SOL_FEEDBACK")
  }
  if (any(grepl("VN-|CLAVE|Opci[oó]n [A-D]", c(L, e$feedback)))) f <- c(f, "SOL_CODIGO")
  esp <- esperado_solucion(e)
  if (length(setdiff(esp, L)) || !setequal(e$reflexiones, SOL_REFLEXIONES)) f <- c(f, "SOL_TEXTO")
  ## Cada párrafo en su sección y en su orden, y la reflexión bajo su encabezado.
  o <- match(esp, L); h <- match(SOL_ENCABEZADOS, L); rf <- which(L %in% SOL_REFLEXIONES)
  if (!anyNA(o) && !anyNA(h) && (is.unsorted(h) || is.unsorted(o) || !identical(findInterval(o, h), c(1L, 2L, 2L, 3L, 3L, 3L, 5L, 6L, 6L, 6L, 6L)) ||
                                 (length(rf) && findInterval(rf[1L], h) != 7L))) f <- c(f, "SOL_ORDEN")
  if (!all(attr(L, "latex") == SOL_LATEX)) f <- c(f, "SOL_LATEX")
  unique(c(f, sonda_arit(L)))
}
## I-3 deja fijos los distractores: lo único que varía la retroalimentación son estos dos sorteos.
sonda_variacion <- function(dg = DG) {
  s <- semillas_por_rama(); o <- character(0); r <- character(0)
  for (k in c(s$canon, s$resto)) { e <- correr(k, dg); r <- c(r, e$reflexion); if (!e$es_canonica && e$dat$d > e$dat$b - e$dat$a) o <- c(o, e$otro_error$codigo) }
  if (length(unique(o)) != 2L || length(unique(r)) < 2L) "SOL_SIN_VARIACION" else character(0)
}
sec_solucion <- function() {
  cat("\n[solucion] N = 100 por rama: se ejecuta el chunk solucion y se contrasta cada párrafo\n")
  s <- semillas_por_rama(); mal <- character(0); otro <- list(canon = character(0), resto = character(0)); ejemplo <- NULL
  for (r in names(s)) for (k in s[[r]]) {
    e <- correr(k); mal <- c(mal, sondas_solucion(e)); otro[[r]] <- c(otro[[r]], e$otro_error$codigo)
  }
  for (r in names(s)) cat(sprintf("  %s (n = %d), otro error emitido: %s\n", r, length(s[[r]]), paste(names(table(otro[[r]])), table(otro[[r]]), collapse = ", ")))
  mal <- c(mal, sonda_variacion())
  if (length(mal)) rojo("solucion", paste(names(table(mal)), table(mal), collapse = ", ")) else cat("  sin fallas: cada párrafo casa con su plantilla, coincide letra a letra y está en su sección, y varían el otro error y la reflexión\n")
}

## --- papel -------------------------------------------------------------------------------
## H9: el ítem se imprime en hojas de 216 x 330 mm con perifericos/solpcielo.tex. Una pregunta que empieza arriba de la página
## deja las cuatro opciones en una misma página (I-5). Rótulos a) a d) con pdftotext -bbox; la página de cada figura, con
## pdfimages -list. La plantilla imprime la Solution después de las opciones: se mide d), no el fondo de la página.
PAPEL_TPL <- file.path(aqui, "..", "..", "perifericos", "solpcielo.tex")
PAPEL_HOJA_MM <- c(216, 330); PAPEL_LIMITE_MM <- 310        # 330 mm menos el margen inferior de 20 mm
sondas_papel <- function(semilla, rmd = RMD, tpl = PAPEL_TPL) {
  d <- tempfile("papel_"); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  ## Desde una copia de ruta corta: R/exams arma un nombre de archivo con toda la ruta, y en una carpeta profunda pasa de 255 caracteres.
  copia <- file.path(d, basename(rmd)); file.copy(rmd, copia, overwrite = TRUE)
  exams::exams2pdf(copia, n = 1L, seed = semilla, name = "x", dir = d, template = tpl, encoding = "UTF-8", quiet = TRUE)
  pdf <- file.path(d, "x1.pdf"); mm <- 25.4 / 72; f <- character(0)
  tl <- grep("^Page size", system2("pdfinfo", shQuote(pdf), stdout = TRUE), value = TRUE)
  tam <- as.numeric(regmatches(tl, gregexpr("[0-9.]+", tl))[[1L]])[1:2] * mm
  if (any(abs(tam - PAPEL_HOJA_MM) > 0.5)) f <- c(f, "PAPEL_TAMANO")
  pg <- as.integer(vapply(strsplit(trimws(system2("pdfimages", c("-list", shQuote(pdf)), stdout = TRUE)[-(1:2)]), " +"), `[`, "", 1L))[1:4]
  x <- system2("pdftotext", c("-bbox", shQuote(pdf), "-"), stdout = TRUE); Encoding(x) <- "UTF-8"; hoja <- cumsum(grepl("<page ", x))
  ## La primera aparición de cada rótulo es la de las opciones; la portada usa (a) con paréntesis, y el feedback de la Solution va después.
  i <- vapply(letters[1:4], function(l) grep(paste0(">", l, ")</word>"), x, fixed = TRUE)[1L], 0L)
  pr <- hoja[i]; base <- as.numeric(sub('.*yMax="([0-9.]+)".*', "\\1", x[i[4L]])) * mm
  if (anyNA(pg) || anyNA(pr) || length(unique(c(pg, pr))) != 1L) f <- c(f, "PAPEL_PARTE")
  if (is.na(base) || base > PAPEL_LIMITE_MM) f <- c(f, "PAPEL_BASE")
  list(fallas = f, pagina = pr[1L], figuras = pg, base = base, hoja = tam)
}
semillas_papel <- function() {
  tipos <- vapply(correr(1L)$contextos, function(k) k$tipo, ""); s <- stats::setNames(rep(NA_integer_, length(tipos) + 1L), c(tipos, "canonica")); k <- 0L
  while (anyNA(s) && k < 2000L) { k <- k + 1L; e <- correr(k); n <- if (e$es_canonica) "canonica" else e$ctx$tipo; if (is.na(s[[n]])) s[[n]] <- k }
  resto <- setdiff(semillas_por_rama()$resto, s)
  largo <- vapply(resto, function(k) { e <- correr(k); nchar(paste(e$enunciado, e$vineta_1, e$vineta_2, e$vineta_3)) }, 0L)
  c(s, resto[order(-largo)])[1:100]                         # N = 100, con los textos más largos primero
}
sec_papel <- function() {
  s <- semillas_papel(); t0 <- proc.time()[["elapsed"]]
  cat(sprintf("\n[papel] N = %d PDF reales: los 7 contextos, la canónica y los textos más largos\n", length(s)))
  if (anyNA(s)) rojo("papel", "falta una semilla de contexto")
  res <- parallel::mclapply(s, sondas_papel, mc.cores = min(8L, parallel::detectCores()))
  sh <- sonda_hijos(res); if (length(sh)) rojo("papel", sh)
  ok <- !vapply(res, function(r) is.null(r) || inherits(r, "try-error"), TRUE); r <- res[ok]; bs <- vapply(r, function(x) x$base, 0)
  en_una <- sum(vapply(r, function(x) length(unique(x$figuras)) == 1L, TRUE)); mal <- unlist(lapply(r, function(x) x$fallas))
  cat(sprintf("  hoja %.0f x %.0f mm · cuatro figuras en una misma página: %d de %d · base de d) %.1f a %.1f mm (límite %d, holgura mínima %.1f) · %.0f s\n",
              r[[1L]]$hoja[1L], r[[1L]]$hoja[2L], en_una, length(s), min(bs), max(bs), PAPEL_LIMITE_MM, PAPEL_LIMITE_MM - max(bs), proc.time()[["elapsed"]] - t0))
  if (sum(ok) != length(s)) rojo("papel", paste(length(s) - sum(ok), "PDF no compilaron"))
  if (length(mal)) rojo("papel", paste(names(table(mal)), table(mal), collapse = ", "))
}

## --- moodle ------------------------------------------------------------------------------
## El último eslabón. La nota la decide exsolution, y lo que lee el estudiante sale del chunk feedback; ninguna otra sección los mira.
## Cada PNG del XML se compara píxel a píxel con el dibujo calculado desde las viñetas del propio XML.
sondas_moodle <- function(semilla, rmd = RMD) {
  td <- tempfile("moodle_"); dir.create(td); on.exit(unlink(td, recursive = TRUE))
  copia <- file.path(td, basename(rmd)); file.copy(rmd, copia, overwrite = TRUE)
  invisible(capture.output(exams::exams2moodle(copia, n = 1L, seed = semilla, name = "q", dir = td)))
  x <- paste(readLines(file.path(td, "q.xml"), encoding = "UTF-8", warn = FALSE), collapse = "\n"); Encoding(x) <- "UTF-8"
  qt <- regmatches(x, regexpr("(?s)<questiontext.*?</questiontext>", x, perl = TRUE))
  li <- regmatches(qt, gregexpr("(?<=<li>).*?(?=</li>)", qt, perl = TRUE))[[1L]]
  ans <- regmatches(x, gregexpr('(?s)<answer fraction="-?[0-9.]+".*?</answer>', x, perl = TRUE))[[1L]]
  if (length(li) != 3L || length(ans) != 4L) return("MOODLE_ESTRUCTURA")
  m <- regmatches(li[1L], regexec("^([0-9]+) (\\S+) (\\S+) (.+) y (.+), pero no (.+)\\.$", li[1L]))[[1L]]
  if (!length(m)) return("MOODLE_ESTRUCTURA")
  a <- as.integer(m[2L]); t <- m[5:7]; n2 <- nums(li[2L]); n3 <- nums(li[3L]); b <- n2[1L]; cc <- n2[2L]; d <- n2[3L]; P <- n3[1L]; B <- n3[2L]; M <- n3[3L]; z <- b - a
  V <- list(K = c(P - a - d, a, M - a - cc, z, d - z, cc - z, B - d - cc + z), A = c(P - b - d, b, M - b - cc, a, d - a, cc - a, B - d - cc + a),
            B = c(P, a, M, z, d - z, cc - z, B), D = c(P, b, M, a, d, cc, B))
  e0 <- correr(1L); ref <- lapply(V, function(v) { p <- tempfile(fileext = ".png"); e0$dibujar_venn(as.integer(v), cap(t), p); png::readPNG(p) })
  imagen <- function(s) {
    k <- regmatches(s, regexec('(?s)<file name="[^"]+"[^>]*encoding="base64">([^<]*)</file>', s, perl = TRUE))[[1L]]
    if (length(k)) png::readPNG(jsonlite::base64_dec(gsub("\\s", "", k[2L])))
  }
  f <- character(0); vistos <- character(0)
  if (!grepl("<single>true</single>", x, fixed = TRUE)) f <- c(f, "MOODLE_TIPO")
  ## La cola tras el último </ul> es la pregunta y nada más: una frase añadida después también la lee el estudiante, y la contención no la ve (H30).
  cola <- trimws(sub("(?s)\\]\\]>.*", "", sub("(?s).*</ul>", "", gsub("\\s+", " ", qt), perl = TRUE), perl = TRUE))
  if (!identical(cola, paste0("<p>", PREGUNTA_LIT, "</p> </p>"))) f <- c(f, "MOODLE_PREGUNTA")   # el segundo </p> lo deja R/exams al cerrar el párrafo de la pregunta
  ## Las cinco figuras llevan el ancho de pantalla y el de papel, con !important delante.
  if (lengths(regmatches(x, gregexpr('style="width:280px!important;max-width:100%;width:5.5cm"', x, fixed = TRUE))) != 5L) f <- c(f, "MOODLE_ESTILO")
  for (s1 in ans) {
    fr <- as.numeric(sub('^<answer fraction="(-?[0-9.]+)".*', "\\1", substr(s1, 1L, 40L)))
    q <- names(ref)[vapply(ref, identical, TRUE, imagen(s1))]
    if (length(q) != 1L) { f <- c(f, "MOODLE_PAREJA"); next }
    vistos <- c(vistos, q)
    if ((fr == 100) != (q == "K")) f <- c(f, "MOODLE_FRACCION")
    fb <- regmatches(s1, regexec("(?s)<feedback format=\"html\">.*?<!\\[CDATA\\[<p>\\s*(.*?)\\s*</p>\\]\\]>", s1, perl = TRUE))[[1L]][2L]
    ex <- switch(q, K = "Correcta. Este diagrama reproduce los siete datos.",
                 A = paste0("Incorrecta. Los tres totales cuadran, pero ", a, " va en la zona de ", t[1L], " y ", t[2L], " sin ", t[3L], ", no en el centro."),
                 B = "Incorrecta. El centro y las zonas de dos conjuntos están bien, pero cada total quedó en la zona de un solo conjunto.",
                 D = "Incorrecta. Cada dato quedó en una zona sin operar, y ninguno de los siete se cumple.")
    if (!identical(fb, ex)) f <- c(f, "MOODLE_FEEDBACK")
  }
  if (length(vistos) == 4L && !setequal(vistos, names(ref))) f <- c(f, "MOODLE_PAREJA")
  gf <- regmatches(x, regexpr("(?s)<generalfeedback.*?</generalfeedback>", x, perl = TRUE))
  if (!identical(imagen(gf), ref$K)) f <- c(f, "MOODLE_SOL_IMG")
  if (grepl("VN-|CLAVE|makeatletter|afterheading", paste(qt, gf))) f <- c(f, "MOODLE_FUGA")
  unique(f)
}
sec_moodle <- function() {
  cat("\n[moodle] N = 100 por rama: XML real, cada PNG contra el dibujo calculado desde las viñetas del XML\n")
  s <- semillas_por_rama(); t0 <- proc.time()[["elapsed"]]; todas <- c(s$canon, s$resto)
  res <- parallel::mclapply(todas, sondas_moodle, mc.cores = min(10L, parallel::detectCores()))
  sh <- sonda_hijos(res); if (length(sh)) rojo("moodle", sh)
  mal <- unlist(lapply(res, function(x) if (inherits(x, "try-error")) "ERROR" else x))
  cat(sprintf("  versiones: canon %d, resto %d · con falla: %d · %.0f s\n", length(s$canon), length(s$resto), sum(lengths(res) > 0L), proc.time()[["elapsed"]] - t0))
  if (length(mal)) rojo("moodle", paste(names(table(mal)), table(mal), collapse = ", ")) else cat("  sin fallas: fraction solo en la clave, feedback de cada opción, imagen de la Solution y sin fugas\n")
}

## --- uso ---------------------------------------------------------------------------------
## H31: el semillero y su plantilla son lo que usa el profesor para imprimir; si se quita samepage o \clearpage, nada de lo anterior se pone en rojo.
## Los literales del semillero se comprueban en su texto, y el render usa esos mismos argumentos, escritos aquí.
USO_SEMILLERO <- file.path(aqui, "SemilleroUnico_v2.R"); USO_PLANTILLA <- file.path(aqui, "solpcielo_una_por_pagina.tex")
USO_LITERALES <- c('template = "solpcielo_una_por_pagina"', "twocolumn = TRUE", "samepage = TRUE", 'header = "\\\\raggedbottom"', '"--papersize", shQuote("{216mm,330mm}")')
USO_ENV <- "\\newenvironment{question}{\\clearpage\\item \\textbf{Escenario:}\\newline}{}"
## La plantilla propia difiere de perifericos/solpcielo.tex en el comentario de cabecera y en \clearpage, y en nada más.
sonda_plantilla <- function(tpl, per) {
  com <- startsWith(tpl, "%% Copia de A-Produccion/perifericos/solpcielo.tex")
  if (sum(tpl == USO_ENV) != 1L || sum(com) != 1L) return("USO_PLANTILLA")
  n <- tpl[!com]; n[n == USO_ENV] <- sub("\\clearpage", "", USO_ENV, fixed = TRUE)
  if (identical(n, per)) character(0) else "USO_PLANTILLA"
}
## Figuras por página con figuras, en el orden de las páginas.
figuras_por_pagina <- function(pdf) {
  x <- trimws(system2("pdfimages", c("-list", shQuote(pdf)), stdout = TRUE)[-(1:2)])
  t <- table(as.integer(vapply(strsplit(x, " +"), `[`, "", 1L)))
  stats::setNames(as.integer(t), names(t))
}
USO_PDF <- rep(c(4L, 1L), 5L)    # medido en el semillero: cuatro opciones en una página y la figura de la Solution sola en la siguiente
USO_NOPS <- rep(4L, 5L)          # medido en el semillero: p3:4 p4:4 p5:4 p6:4 p7:4
sondas_uso <- function(sem = readLines(USO_SEMILLERO, encoding = "UTF-8"), tpl = readLines(USO_PLANTILLA, encoding = "UTF-8"),
                       per = readLines(PAPEL_TPL, encoding = "UTF-8"), render = TRUE, rmd = RMD) {
  f <- character(0); det <- "sin render"
  if (any(vapply(USO_LITERALES, function(l) sum(grepl(l, sem, fixed = TRUE)) != 1L, TRUE))) f <- c(f, "USO_SEMILLERO")
  f <- c(f, sonda_plantilla(tpl, per))
  if (render) {
    d <- tempfile("uso_"); dir.create(d); on.exit(unlink(d, recursive = TRUE))
    copia <- file.path(d, basename(rmd)); file.copy(rmd, copia, overwrite = TRUE)
    tp <- file.path(d, "solpcielo_una_por_pagina.tex"); writeLines(tpl, tp, useBytes = TRUE)
    sd <- matrix(1:5, nrow = 1L)
    pdf <- tryCatch({
      exams::exams2pdf(rep(copia, 5L), n = 1L, seed = sd, name = "p", dir = d, edir = d, template = tp, encoding = "UTF-8", quiet = TRUE); file.path(d, "p1.pdf")
    }, error = function(e) NULL)
    nops <- tryCatch({
      exams::exams2nops(rep(copia, 5L), n = 1L, seed = sd, name = "n", dir = d, edir = d, language = "es", title = "Evaluación de Matemáticas",
                        institution = "I. E. Pedacito de Cielo", logo = NULL, date = "2026-11-06", replacement = FALSE, blank = 0, duplex = TRUE,
                        twocolumn = TRUE, samepage = TRUE, header = "\\raggedbottom", encoding = "UTF-8", quiet = TRUE); file.path(d, "n1.pdf")
    }, error = function(e) NULL)
    if (is.null(pdf) || !file.exists(pdf) || is.null(nops) || !file.exists(nops)) return(c(f, "USO_RENDER"))
    fp <- figuras_por_pagina(pdf); fn <- figuras_por_pagina(nops)
    if (!identical(unname(fp), USO_PDF)) f <- c(f, "USO_FIGURAS_PDF")
    if (!identical(unname(fn), USO_NOPS)) f <- c(f, "USO_FIGURAS_NOPS")
    det <- sprintf("PDF %s · NOPS %s", paste0("p", names(fp), ":", fp, collapse = " "), paste0("p", names(fn), ":", fn, collapse = " "))
  }
  r <- unique(f); attr(r, "detalle") <- det; r
}
sec_uso <- function() {
  cat("\n[uso] SemilleroUnico_v2.R y solpcielo_una_por_pagina.tex: literales, plantilla, y un PDF y un NOPS reales de 5 preguntas\n")
  t0 <- proc.time()[["elapsed"]]; r <- sondas_uso()
  cat(sprintf("  figuras por página con figuras: %s · %.0f s\n", attr(r, "detalle"), proc.time()[["elapsed"]] - t0))
  if (length(r)) rojo("uso", paste(r, collapse = ", ")) else cat("  sin fallas: literales del semillero, plantilla igual a la del repo salvo \\clearpage, y las cuatro figuras de cada pregunta en una página\n")
}

## --- mutantes (INC-MUTANTE-SONDA) --------------------------------------------------------
muta <- function(cod, patron, reemplazo) {
  n <- sum(grepl(patron, cod, fixed = TRUE))
  if (n != 1L) stop("MUTANTE MAL CONSTRUIDO: el patrón aparece ", n, " veces: ", patron)
  sub(patron, reemplazo, cod, fixed = TRUE)
}
aplica <- function(cod, pares) { if (!is.list(pares)) pares <- list(pares); for (p in pares) cod <- muta(cod, p[1L], p[2L]); cod }
veredicto <- function(esperada, reales) {
  if (!length(reales)) "no_detectado" else if (!esperada %in% reales) "cazado_por_otra"
  else if (length(reales) == 1L) "cazado_por_su_sonda" else "cazado_por_su_sonda_y_otra"
}
sec_mutantes <- function() {
  cat("\n[mutantes]\n")
  m <- list(
    list(id = "M1_intercambia_regiones_en_la_figura", sonda = "CABLEADO_CLAVE", tipo = "cableado", dib = c("regiones_mezcladas[[i]]", "regiones_mezcladas[[i]][c(1, 2, 3, 4, 6, 5, 7)]")),
    list(id = "M2_archivos_cruzados", sonda = "CABLEADO_NOMBRES", tipo = "cableado", dib = c("archivos[i]", "rev(archivos)[i]")),
    list(id = "M3_vineta_con_otro_dato", sonda = "ORACULO_TEXTO", tipo = "cableado", dg = c("vineta_1 <- paste0(personas(dat$a,", "vineta_1 <- paste0(personas(dat$b,")),
    list(id = "M4_sin_piso_en_las_dobles", sonda = "I-3: los tres mecanismos", tipo = "guarda", dg = c("piso  <- max(0L, v_p12 - v_ctr)", "piso  <- 0L")),
    list(id = "M17_resta_una_sin_precondicion", sonda = "regla #13: VN-X-RESTA-UNA", tipo = "guarda", dg = c("FALSE, function(p) p$d > p$b - p$a)", "FALSE, function(p) TRUE)")),
    list(id = "M22_A_falla_otros_dos_datos", sonda = "regla #13: A falla exactamente", tipo = "guarda", dg = c("cbind(P - b - d, b, M - b - c, a, d - a, c - a, B - d - c + a)", "cbind(P - a - d - 1L, a + 1L, M - a - c, b - a - 1L, d - (b - a) + 1L, c - (b - a), B - d - c + (b - a))")),
    list(id = "M23_B_falla_otros_tres_datos", sonda = "regla #13: B falla exactamente", tipo = "guarda", dg = c("cbind(P, a, M, b - a, d - (b - a), c - (b - a), B)", "cbind(P - a - d - 1L, a + 1L, M - a - c, b - a, d - (b - a), c - (b - a), B - d - c + (b - a))")),
    list(id = "M24_una_opcion_contiene_el_centro_doble", sonda = "regla #13: ninguna opción contiene", tipo = "guarda", dg = c("cbind(P, a, M, b - a, d - (b - a), c - (b - a), B)", "cbind(P, a, M, b - a, d - (b - a), c - (b - a), pmax(0L, B - d - c))")),
    list(id = "M18_digitos_mas_uno", sonda = "FIG_NUMERO", tipo = "figura", dg = c("f(a$y + g$alto_digito / 2), regiones[i]))", "f(a$y + g$alto_digito / 2), regiones[i] + 1L))")),
    list(id = "M29_un_digito_de_D_mas_uno", sonda = "FIG_NUMERO", tipo = "figura", dg = c("f(a$y + g$alto_digito / 2), regiones[i]))", "f(a$y + g$alto_digito / 2), regiones[i] + 1L * (i == 1L && identical(regiones, vec[[\"VN-D-SIN-OPERAR\"]]))))")),
    list(id = "M19_rotulos_2_y_3_cruzados_en_dibujar_venn", sonda = "FIG_ROTULO", tipo = "figura", dg = c("f(r$base), rotulos[i]))", "f(r$base), rotulos[c(1L, 3L, 2L)[i]]))")),
    list(id = "M20_trazo_distinto_solo_en_la_clave", sonda = "I1_PIXELES", tipo = "figura", dg = c("f(g$trazo_circulo), f(cc$x)", "f(if (identical(regiones, reg_clave)) 3.4 else g$trazo_circulo), f(cc$x)")),
    list(id = "M21_geometria_propia_en_la_clave", sonda = "CABLEADO_GEOM", tipo = "cableado", dib = c("rotulos, archivos[i])", "rotulos, archivos[i], geom = if (sol[i]) utils::modifyList(GEOM_VENN, list(trazo_circulo = 3.4)) else GEOM_VENN)")),
    list(id = "M30_clave_marcada_en_otra_figura", sonda = "FIG_CLAVE", tipo = "figura", dg = c("sol <- vapply(ops, function(o) o$es_clave, TRUE)", "sol <- rev(vapply(ops, function(o) o$es_clave, TRUE))")),
    list(id = "M31_un_total_impreso_de_mas", sonda = "FIG_PALABRAS", tipo = "figura", dg = c('tik <- c(tik, "\\\\end{tikzpicture}")', 'tik <- c(tik, "\\\\node at (30,215) {\\\\num{32}};", "\\\\end{tikzpicture}")')),
    list(id = "M25_canonica_fuera_de_la_rejilla", sonda = "CANON_REJILLA", tipo = "funcion", fn = function() sonda_rejilla(utils::modifyList(correr(7L)$reg, list(s1 = 3L)))),
    list(id = "M27_rotulo_3_contra_el_marco", sonda = "BORDE_ROTULO", tipo = "bordes", dg = c('r3 = list(x = 163.5, base = 226.5, ajuste = "izquierda")', 'r3 = list(x = 195, base = 226.5, ajuste = "izquierda")'), env = function(e) e$GEOM_VENN$rotulos$r3$x == 195),
    list(id = "M28_copia_del_marco_distinta", sonda = "COPIA_TIKZ", tipo = "copia", dg = c("trazo_circulo = 2.2, trazo_marco = 1.7,", "trazo_circulo = 2.2, trazo_marco = 2.4,")),
    list(id = "S1_cuenta_falsa", sonda = "SOL_ARIT", tipo = "funcion", fn = function() sonda_arit("Una cuenta falsa: $2 + 2 = 5$.")),
    list(id = "S2_respuesta_describe_un_distractor", sonda = "SOL_RESPUESTA", tipo = "solucion", sol = c('describe(reg_clave), ": es el único', 'describe(regiones_mezcladas[[which(!sol)[1L]]]), ": es el único')),
    list(id = "S3_procedimiento_con_otra_cifra", sonda = "SOL_PROCEDIMIENTO", tipo = "solucion", sol = c('mat(paste0(dat$c, " - ", z, " = ", reg$p23))', 'mat(paste0(dat$c + 1L, " - ", z, " = ", reg$p23 + 1L))')),
    list(id = "S4_caso_con_otra_cifra", sonda = "SOL_CASO", tipo = "solucion", sol = c("mat(sumatxt(reg$s1, reg$p12, reg$p13, reg$ctr))", "mat(sumatxt(reg$s1 + 1L, reg$p12, reg$p13, reg$ctr))")),
    list(id = "S5_diagnostico_describe_la_clave", sonda = "SOL_OPCION", tipo = "solucion", dg = c("describe(e$calcula(dat))", "describe(reg_clave)")),
    list(id = "S6_circulo_con_otra_suma", sonda = "SOL_CIRCULO", tipo = "solucion", dg = c("mat(sumatxt(dat$P, dat$b, dat$a, dat$d))", "mat(sumatxt(dat$P + 1L, dat$b, dat$a, dat$d))")),
    list(id = "S7_otro_error_con_otra_cifra", sonda = "SOL_OTRO", tipo = "solucion", dg = c('se queda corto en ", dat$b - dat$a,', 'se queda corto en ", dat$b - dat$a + 1L,')),
    list(id = "S8_feedback_cruzado", sonda = "SOL_FEEDBACK", tipo = "solucion", dg = c('feedback <- unname(feedback_por_codigo[vapply(ops, function(o) o$codigo, "")])', 'feedback <- rev(unname(feedback_por_codigo[vapply(ops, function(o) o$codigo, "")]))')),
    list(id = "S9_codigo_interno_visible", sonda = "SOL_CODIGO", tipo = "solucion", dg = c('z23 <- paste0(t2, " y ", t3, " sin ", t1)', 'z23 <- paste0("VN-D ", t2, " y ", t3, " sin ", t1)')),
    list(id = "S10_parrafo_nuevo_con_cifras", sonda = "SOL_SIN_ORACULO", tipo = "solucion", sol = c('encabezado("Análisis del error")', 'cat("Hay ", dat$a, " casos.\\n\\n", sep = ""); encabezado("Análisis del error")')),
    list(id = "S11_parrafo_que_desaparece", sonda = "SOL_MUDO", tipo = "solucion", sol = c('cat("Revisar solo los totales no basta:', 'if (FALSE) cat("Revisar solo los totales no basta:')),
    list(id = "S12_imagen_de_un_distractor", sonda = "SOL_IMAGEN", tipo = "solucion", sol = c("archivos[which(sol)]", "archivos[which(!sol)[1L]]")),
    list(id = "S13_afirmacion_sobre_otro_conjunto", sonda = "SOL_AFIRMACION", tipo = "solucion", sol = c('falla los dos datos de ", t1, " y ", t2,', 'falla los dos datos de ", t1, " y ", t3,')),
    list(id = "S14_vuelve_el_texto_de_H2", sonda = "SOL_SIN_ORACULO", tipo = "solucion", sol = c("**Otro error posible: ", "**Otro error frecuente, que no aparece entre las opciones: ")),
    list(id = "S15_vuelve_el_texto_de_H3", sonda = "SOL_SIN_ORACULO", tipo = "solucion", dg = c("cuando seis de los siete datos reúnen varias zonas", "cuando cada dato reúne varias zonas")),
    list(id = "S16_vuelve_el_texto_de_H4", sonda = "SOL_SIN_ORACULO", tipo = "solucion", sol = c('falla los dos datos de ", t1, " y ", t2, ".\\n\\n"', 'no reproduce las intersecciones.\\n\\n"')),
    list(id = "M32_digitos_azules_solo_en_la_clave", sonda = "I1_COLOR", tipo = "figura", dg = c('tik <- c(tik, sprintf("\\\\node[anchor=base west] at (%s,%s) {\\\\num{%d}};", f(a$x_izq - g$lsb_numero)', 'tik <- c(tik, sprintf(paste0("\\\\node[anchor=base west", if (identical(regiones, reg_clave)) ",text=blue" else "", "] at (%s,%s) {\\\\num{%d}};"), f(a$x_izq - g$lsb_numero)')),
    list(id = "M33_digitos_mayores_solo_en_la_clave", sonda = "I1_TAMANO", tipo = "figura", dg = c('tik <- c(tik, sprintf("\\\\node[anchor=base west] at (%s,%s) {\\\\num{%d}};", f(a$x_izq - g$lsb_numero)', 'tik <- c(tik, sprintf(paste0("\\\\node[anchor=base west] at (%s,%s) {", if (identical(regiones, reg_clave)) "\\\\scalebox{1.12}{\\\\num{%d}}" else "\\\\num{%d}", "};"), f(a$x_izq - g$lsb_numero)')),
    list(id = "X1_dobles_nombres_cruzados", sonda = "SOL_TEXTO", tipo = "solucion", sol = list(c('cap1(z23), ": ", mat(paste0(dat$c,', 'cap1(z13), ": ", mat(paste0(dat$c,'), c('cap1(z13), ": ", mat(paste0(dat$d,', 'cap1(z23), ": ", mat(paste0(dat$d,'))),
    list(id = "X2_solas_nombres_cruzados", sonda = "SOL_TEXTO", tipo = "solucion", sol = list(c('Solo ", t1, ": ", mat(paste0(dat$P,', 'Solo ", t2, ": ", mat(paste0(dat$P,'), c('". Solo ", t2, ": ",', '". Solo ", t1, ": ",'))),
    list(id = "X3_caso_nombres_cruzados", sonda = "SOL_TEXTO", tipo = "solucion", sol = list(c('el círculo de ", t1, " suma ", mat(sumatxt(reg$s1,', 'el círculo de ", t2, " suma ", mat(sumatxt(reg$s1,'), c('", el de ", t2, " suma ",', '", el de ", t1, " suma ",'))),
    list(id = "X4_describe_otra_zona_doble", sonda = "SOL_TEXTO", tipo = "solucion", dg = c('v[["p12"]], " en la zona de ", z12,', 'v[["p12"]], " en la zona de ", z13,')),
    list(id = "X5_describe_otro_conjunto_solo", sonda = "SOL_TEXTO", tipo = "solucion", dg = c('" en la zona de solo ", t1)', '" en la zona de solo ", t3)')),
    list(id = "X6_causa_A_falsa", sonda = "SOL_TEXTO", tipo = "solucion", dg = c('"leer «X y Y» y «X y Y, pero no Z» como si dijeran lo mismo, y repartir los dos datos al revés"', '"creer que el centro siempre es la cifra menor del enunciado"')),
    list(id = "X7_causa_otro_falsa", sonda = "SOL_TEXTO", tipo = "solucion", dg = c('"olvidar que el centro pertenece a las dos intersecciones que se restan"', '"olvidar que el centro nunca pertenece a las intersecciones"')),
    list(id = "X8_reflexion_falsa_en_el_pool", sonda = "SOL_TEXTO", tipo = "solucion", dg = c('"Cuando dos diagramas se parezcan, busca el dato que los distingue en lugar de mirar el conjunto de los números."', '"El diagrama correcto siempre es el que tiene la segunda suma más pequeña."')),
    list(id = "X9_trampa_otro_conjunto", sonda = "SOL_TEXTO", tipo = "solucion", sol = c('" que ", verbo, " ", t1, " y ", t2, " incluye a quienes también ", verbo, " ", t3,', '" que ", verbo, " ", t1, " y ", t3, " incluye a quienes también ", verbo, " ", t2,')),
    list(id = "X10_centro_otros_conjuntos", sonda = "SOL_TEXTO", tipo = "solucion", sol = c('son los de ", t1, " y ", t2, " menos los que no están en ", t3,', 'son los de ", t1, " y ", t3, " menos los que no están en ", t2,')),
    list(id = "X11_opA_zona_equivocada", sonda = "SOL_TEXTO", tipo = "solucion", dg = c('quedó entero en la zona de ", z12,', 'quedó entero en la zona de ", z13,')),
    list(id = "X12_caso_pares_cruzados", sonda = "SOL_TEXTO", tipo = "solucion", sol = list(c('", ", t2, " y ", t3, " suman ", mat(sumatxt(reg$p23', '", ", t1, " y ", t3, " suman ", mat(sumatxt(reg$p23'), c('", y ", t1, " y ", t3, " suman ", mat(sumatxt(reg$p13', '", y ", t2, " y ", t3, " suman ", mat(sumatxt(reg$p13'))),
    list(id = "S17_vuelve_el_texto_de_H15", sonda = "SOL_SIN_ORACULO", tipo = "solucion", dg = c('paste0("Para llegar a la zona de solo ", t3, " no basta con quitarle al total las dos intersecciones: en ",', 'paste0("A la zona de solo ", t3, " no basta con quitarle las dos intersecciones: en ",')),
    list(id = "S18_vuelve_el_texto_de_H16", sonda = "SOL_SIN_ORACULO", tipo = "solucion", sol = c("los tres totales, las tres intersecciones y el dato «pero no».", "los tres totales y las cuatro intersecciones.")),
    list(id = "L1_exsolution_invertida", sonda = "MOODLE_FRACCION", tipo = "moodle", rmd = c("exsolution: `r mchoice2string(sol)`", "exsolution: `r mchoice2string(rev(sol))`")),
    list(id = "L2_chunk_feedback_invertido", sonda = "MOODLE_FEEDBACK", tipo = "moodle", rmd = c('answerlist(feedback, markup = "markdown")', 'answerlist(rev(feedback), markup = "markdown")')),
    list(id = "L3_figura_distinta_de_la_calculada", sonda = "MOODLE_PAREJA", tipo = "moodle", rmd = c("f(a$y + g$alto_digito / 2), regiones[i]))", "f(a$y + g$alto_digito / 2), regiones[i] + 1L))")),
    list(id = "L4_solution_con_un_distractor", sonda = "MOODLE_SOL_IMG", tipo = "moodle", rmd = c("archivos[which(sol)]", "archivos[which(!sol)[1L]]")),
    list(id = "L5_codigo_interno_visible", sonda = "MOODLE_FUGA", tipo = "moodle", rmd = c('paste0("**", e$nombre, ".** Es posible que los estudiantes que eligen ",', 'paste0("**", e$codigo, " ", e$nombre, ".** Es posible que los estudiantes que eligen ",')),
    list(id = "M34_pregunta_negada", sonda = "CABLEADO_PREGUNTA", tipo = "cableado", dg = c("¿cuál diagrama de Venn organiza los datos", "¿cuál diagrama de Venn NO organiza los datos")),
    list(id = "M35_contexto_con_una_restriccion", sonda = "CONTEXTOS_CONGELADOS", tipo = "contextos", dg = c(" abrirá talleres por la tarde y, para armar los grupos,", " abrirá talleres por la tarde, cada estudiante toma un solo taller y, para armar los grupos,")),
    list(id = "M36_exshuffle_activado", sonda = "CABLEADO_METADATOS", tipo = "metadatos", rmd = c("exshuffle: FALSE", "exshuffle: TRUE")),
    list(id = "M37_digitos_grises_solo_en_la_clave", sonda = "I1_GRIS", tipo = "figura", dg = c('tik <- c(tik, sprintf("\\\\node[anchor=base west] at (%s,%s) {\\\\num{%d}};", f(a$x_izq - g$lsb_numero)', 'tik <- c(tik, sprintf(paste0("\\\\node[anchor=base west", if (identical(regiones, reg_clave)) ",text=black!55" else "", "] at (%s,%s) {\\\\num{%d}};"), f(a$x_izq - g$lsb_numero)')),
    list(id = "M38_otra_serie_solo_en_la_clave", sonda = "I1_FUENTE", tipo = "figura", dg = c('tik <- c(tik, sprintf("\\\\node[anchor=base west] at (%s,%s) {\\\\num{%d}};", f(a$x_izq - g$lsb_numero)', 'tik <- c(tik, sprintf(paste0("\\\\node[anchor=base west] at (%s,%s) {", if (identical(regiones, reg_clave)) "{\\\\fontfamily{phv}\\\\fontseries{b}\\\\fontsize{20}{20}\\\\selectfont %d}" else "\\\\num{%d}", "};"), f(a$x_izq - g$lsb_numero)')),
    list(id = "M39_estirada_solo_en_la_clave", sonda = "I1_ANCHO", tipo = "figura", dg = c('tik <- c(tik, sprintf("\\\\node[anchor=base west] at (%s,%s) {\\\\num{%d}};", f(a$x_izq - g$lsb_numero)', 'tik <- c(tik, sprintf(paste0("\\\\node[anchor=base west] at (%s,%s) {", if (identical(regiones, reg_clave)) "\\\\scalebox{1.25}[1]{\\\\num{%d}}" else "\\\\num{%d}", "};"), f(a$x_izq - g$lsb_numero)')),
    list(id = "M40_subrayada_solo_en_la_clave", sonda = "I1_MARCA", tipo = "figura", dg = c('tik <- c(tik, sprintf("\\\\node[anchor=base west] at (%s,%s) {\\\\num{%d}};", f(a$x_izq - g$lsb_numero)', 'tik <- c(tik, sprintf(paste0("\\\\node[anchor=base west] at (%s,%s) {", if (identical(regiones, reg_clave)) "\\\\underline{\\\\num{%d}}" else "\\\\num{%d}", "};"), f(a$x_izq - g$lsb_numero)')),
    list(id = "M41_movida_solo_en_la_clave", sonda = "I1_POS", tipo = "figura", dg = c("f(a$x_izq - g$lsb_numero)", "f(a$x_izq - g$lsb_numero + if (identical(regiones, reg_clave)) 3 else 0)")),
    list(id = "M42_hijo_muerto", sonda = "HIJO_MUERTO", tipo = "funcion", fn = function() sonda_hijos(list(character(0), NULL))),
    list(id = "S19_diagnosticos_invertidos", sonda = "SOL_ORDEN", tipo = "solucion", sol = c('cat(paste(diagnostico, collapse = "\\n\\n"), "\\n\\n", sep = "")', 'cat(paste(rev(diagnostico), collapse = "\\n\\n"), "\\n\\n", sep = "")')),
    list(id = "S20_reflexion_bajo_caso", sonda = "SOL_ORDEN", tipo = "solucion", sol = c('encabezado("Análisis de las opciones incorrectas")', 'cat(reflexion, "\\n\\n", sep = ""); encabezado("Análisis de las opciones incorrectas")')),
    list(id = "S21_letra_en_el_bloque_latex", sonda = "SOL_LATEX", tipo = "solucion", sol = c('\\\\makeatother\\n"', '\\\\makeatother Opcion A\\n"')),
    list(id = "S22_otro_error_sin_sortear", sonda = "SOL_SIN_VARIACION", tipo = "variacion", dg = c("otro_error <- pick1(Filter(function(e) !e$ofrecible && isTRUE(e$precondicion(dat)), errores_conceptuales))", "otro_error <- Filter(function(e) !e$ofrecible && isTRUE(e$precondicion(dat)), errores_conceptuales)[[1L]]")),
    list(id = "S23_reflexion_sin_sortear", sonda = "SOL_SIN_VARIACION", tipo = "variacion", dg = c("reflexion <- pick(reflexiones)", "reflexion <- reflexiones[1L]")),
    list(id = "L6_pregunta_negada_en_el_xml", sonda = "MOODLE_PREGUNTA", tipo = "moodle", rmd = c("¿cuál diagrama de Venn organiza los datos", "¿cuál diagrama de Venn NO organiza los datos")),
    list(id = "L7_extype_mchoice", sonda = "MOODLE_TIPO", tipo = "moodle", rmd = c("extype: schoice", "extype: mchoice")),
    list(id = "L8_una_figura_sin_important", sonda = "MOODLE_ESTILO", tipo = "moodle", rmd = c('diagrama_a_`r fig_id`.png){width=5.5cm style="width:280px!important;max-width:100%"}', 'diagrama_a_`r fig_id`.png){width=5.5cm style="max-width:100%"}')),
    list(id = "F0_control_todo_en_orden_cubre", sonda = "EXIT_CUBIERTO", tipo = "funcion", fn = function() prueba_firma()),
    list(id = "F1_regla_fuera_de_la_lista", sonda = "FIRMA_FUERA_DE_LISTA", tipo = "funcion", fn = function() prueba_firma(mv = c(FIRMA_VARIANTES, `LEX-con-repetidos` = 45))),
    list(id = "F2_firmada_en_canonica_no_cubre_variantes", sonda = "FIRMA_FUERA_DE_LISTA", tipo = "funcion", fn = function() prueba_firma(mv = c(FIRMA_VARIANTES, `FOR-cadena-corta` = 100))),
    list(id = "F3_regla_sobre_su_cifra_variantes", sonda = "FIRMA_SOBRE_CIFRA", tipo = "funcion", fn = function() { m <- FIRMA_VARIANTES; m[["EST-menos-cifras-del-enunciado"]] <- 69.6; prueba_firma(mv = m) }),
    list(id = "F4_regla_sobre_su_cifra_canonica", sonda = "FIRMA_SOBRE_CIFRA", tipo = "funcion", fn = function() { m <- FIRMA_CANONICA; m[["MAG-suma-no-extrema"]] <- 62.5; prueba_firma(mc = m) }),
    list(id = "F5_cifra_firmada_que_no_se_reproduce", sonda = "FIRMA_NO_REPRODUCE", tipo = "funcion", fn = function() { m <- FIRMA_VARIANTES; m[["EST-menos-cifras-del-enunciado"]] <- 65.3; prueba_firma(mv = m) }),
    list(id = "F6_regla_firmada_sin_medir", sonda = "FIRMA_NO_REPRODUCE", tipo = "funcion", fn = function() prueba_firma(mv = FIRMA_VARIANTES[names(FIRMA_VARIANTES) != "MAG-menor-maximo"])),
    list(id = "F7_rmd_distinto", sonda = "FIRMA_MD5", tipo = "funcion", fn = function() { f <- tempfile(fileext = ".Rmd"); on.exit(unlink(f)); writeLines(c(lineas, ""), f, useBytes = TRUE); prueba_firma(md5_ok = md5_coincide(f)) }),
    list(id = "F8_firmada_en_false", sonda = "FIRMA_SIN_FIRMAR", tipo = "funcion", fn = function() prueba_firma(firmada = FALSE)),
    list(id = "F9_modo_parcial", sonda = "FIRMA_MODO", tipo = "funcion", fn = function() prueba_firma(modo = "bateria")),
    list(id = "F10_alcance_sin_evaluar", sonda = "FIRMA_SIN_EVALUAR", tipo = "funcion", fn = function() prueba_firma(evaluado = FALSE)),
    list(id = "F11_una_sonda_en_rojo_gana_a_la_firma", sonda = "ROJO_GANA", tipo = "funcion", fn = function() if (decidir_exit(1L, TRUE, character(0), TRUE, "todo", TRUE, SECCIONES) == 1L) "ROJO_GANA" else character(0)),
    list(id = "M43_arriba_1_5_solo_en_la_clave", sonda = "I1_POSY", tipo = "figura", dg = c("f(a$y + g$alto_digito / 2), regiones[i]))", "f(a$y + g$alto_digito / 2 + if (identical(regiones, reg_clave)) (-1.5) else 0), regiones[i]))")),
    list(id = "M44_arriba_3_solo_en_la_clave", sonda = "I1_POSY", tipo = "figura", dg = c("f(a$y + g$alto_digito / 2), regiones[i]))", "f(a$y + g$alto_digito / 2 + if (identical(regiones, reg_clave)) (-3) else 0), regiones[i]))")),
    list(id = "L9_frase_tras_la_pregunta_en_el_xml", sonda = "MOODLE_PREGUNTA", tipo = "moodle", rmd = c('cat(pregunta, "\\n", sep = "")', 'cat(pregunta, "\\n\\n", sep = ""); cat("Marque el diagrama que NO los organiza.\\n\\n", sep = "")')),
    list(id = "E1_exacto_con_otro_md5", sonda = "EXACTO_MD5", tipo = "funcion", fn = function() {
      d <- tempfile("ex_"); dir.create(d); on.exit(unlink(d, recursive = TRUE)); f <- file.path(d, "exacto.py")
      writeLines(c(readLines(RUTA_EXACTO, warn = FALSE), "# otro contenido"), f); as.character(attr(tasas_exactas(f), "razon")) }),
    list(id = "E2_python_sale_con_1", sonda = "EXACTO_PYTHON", tipo = "funcion", fn = function() {
      f <- tempfile(fileext = ".py"); on.exit(unlink(f))
      writeLines(c("print('versiones 11000000 seg 0')", "print('MAG-suma-max   10.000 %')", "import sys", "sys.exit(1)"), f)
      as.character(attr(tasas_exactas(f, unname(tools::md5sum(f))), "razon")) }),
    list(id = "E3_regla_repetida_en_la_salida_de_python", sonda = "EXACTO_REPETIDA", tipo = "funcion",
         fn = function() as.character(attr(parsear_tasas(c("versiones 11000000 seg 1", "MAG-suma-max   10.000 %", "MAG-suma-max   90.000 %")), "razon"))),
    list(id = "E4_regla_no_firmada_retirada", sonda = "BATERIA_CONGELADA", tipo = "funcion", fn = function() {
      nom <- nombres_bateria(); m <- stats::setNames(rep(25, length(nom)), nom); bateria_evaluable(nom[nom != "SIG-sin-cero"], m, m) }),
    list(id = "E5_regla_repetida_en_la_bateria", sonda = "BATERIA_CONGELADA", tipo = "funcion", fn = function() {
      nom <- nombres_bateria(); m <- stats::setNames(rep(25, length(nom)), nom); bateria_evaluable(c(nom, nom[1L]), m, m) }),
    list(id = "E6_control_la_bateria_real_es_evaluable", sonda = "BATERIA_EVALUABLE", tipo = "funcion", fn = function() {
      nom <- nombres_bateria(); m <- stats::setNames(rep(25, length(nom)), nom); if (length(nom) == 34L && !length(bateria_evaluable(nom, m, m))) "BATERIA_EVALUABLE" else character(0) }),
    list(id = "F12_seccion_sin_correr", sonda = "SECCION_SIN_CORRER", tipo = "funcion", fn = function() prueba_firma(corridas = setdiff(SECCIONES, "uso"))),
    list(id = "F13_control_corre_registra_la_seccion", sonda = "CORRE_REGISTRA", tipo = "funcion", fn = function() {
      viejo <- corridas; on.exit(corridas <<- viejo); corridas <<- character(0); corre("x", function() NULL); if (identical(corridas, "x")) "CORRE_REGISTRA" else character(0) }),
    list(id = "U1_samepage_quitado_del_semillero", sonda = "USO_SEMILLERO", tipo = "uso", semillero = c("samepage = TRUE,", "samepage = FALSE,")),
    list(id = "U2_plantilla_sin_clearpage", sonda = "USO_PLANTILLA", tipo = "uso", plantilla = c("{\\clearpage\\item", "{\\item"), render = TRUE),
    list(id = "U3_plantilla_que_cambia_otra_linea", sonda = "USO_PLANTILLA", tipo = "uso", plantilla = c("\\columnseprule}{0.2pt}", "\\columnseprule}{0.3pt}")),
    list(id = "P1_ancho_6_5_cm", sonda = "PAPEL_PARTE", tipo = "papel", ancho = "6.5cm"),
    list(id = "P2_ancho_6_cm", sonda = "PAPEL_BASE", tipo = "papel", ancho = "6cm"),
    list(id = "P3_hoja_de_356_mm", sonda = "PAPEL_TAMANO", tipo = "papel", hoja = "papersize={216mm,356mm}"),
    list(id = "M9_distractor_fuera_de_rango", sonda = "I-3: enteros de 0 a 99", tipo = "guarda", dg = c('"VN-D-SIN-OPERAR"    = function(p) with(p, cbind(P, b, M, a, d, c, B))', '"VN-D-SIN-OPERAR"    = function(p) with(p, cbind(P, b, M, a, d, c, B + 100L))')),
    list(id = "M10_A_pierde_un_total", sonda = "I-4: solo la clave reproduce", tipo = "guarda", dg = c('c - a, B - d - c + a))', 'c - a, B - d - c + a + 1L))')),
    list(id = "M11_rotulos_cruzados", sonda = "CABLEADO_ROTULOS", tipo = "cableado", dib = c("rotulos, archivos[i]", "rev(rotulos), archivos[i]")),
    list(id = "M12_figura_de_menos", sonda = "CABLEADO_CONTEO", tipo = "cableado", dib = c("seq_along(archivos)", "1:3")),
    list(id = "M13_numero_casi_toca_un_trazo", sonda = "HOLGURA", tipo = "tinta", dg = c("p12 = list(x_izq = 102.1, y = 81.2)", "p12 = list(x_izq = 102.1, y = 88)"), env = function(e) e$GEOM_VENN$anclas$p12$y == 88),
    list(id = "M14_numero_en_otra_region_sin_tocar_trazos", sonda = "REGION", tipo = "tinta", dg = c("s1 = list(x_izq = 47.9, y = 86.2)", "s1 = list(x_izq = 175, y = 120)"), env = function(e) e$GEOM_VENN$anclas$s1$x_izq == 175),
    list(id = "M15_un_total_fuera_de_los_circulos", sonda = "TINTA_EXTRA", tipo = "tinta", dg = c('tik <- c(tik, "\\\\end{tikzpicture}")', 'tik <- c(tik, "\\\\node at (30,215) {\\\\num{32}};", "\\\\end{tikzpicture}")')),
    list(id = "M16_falta_un_numero", sonda = "AUSENTE_s3", tipo = "tinta", dg = c("for (i in 1:7) {", "for (i in 1:6) {")),
    list(id = "M5_intercambia_anclas_en_dibujar_venn", sonda = "MAPEO", tipo = "mapeo", dg = c("a <- g$anclas[[g$orden[i]]]", "a <- g$anclas[[g$orden[c(1, 2, 3, 4, 6, 5, 7)[i]]]]")),
    list(id = "M6_ancla_del_centro_fuera", sonda = "REGION", tipo = "tinta", dg = c("ctr = list(x_izq = 105.8, y = 116.2)", "ctr = list(x_izq = 105.8, y = 99)"), env = function(e) e$GEOM_VENN$anclas$ctr$y == 99),
    list(id = "M7_letra_mayor", sonda = "HOLGURA", tipo = "tinta", dg = c("em_numero = 20, em_rotulo = 18, dpi = 216", "em_numero = 30, em_rotulo = 18, dpi = 216"), env = function(e) e$GEOM_VENN$em_numero == 30),
    list(id = "M8_numero_centrado", sonda = "IZQ", tipo = "tinta", dg = c("f(a$x_izq - g$lsb_numero)", "f(a$x_izq + 4.4)"))
  )
  res <- vapply(m, function(x) {
    dg <- if (!is.null(x$dg)) compila(aplica(COD_DG, x$dg)) else DG
    dib <- if (!is.null(x$dib)) compila(aplica(COD_DIB, x$dib)) else DIB
    sol <- if (!is.null(x$sol)) compila(aplica(COD_SOL, x$sol)) else SOL
    if (x$tipo == "funcion") {
      real <- x$fn(); sem <- NA
    } else if (x$tipo == "contextos") {
      real <- sonda_contextos(aplica(COD_DG, x$dg)); sem <- NA
    } else if (x$tipo == "metadatos") {
      real <- sonda_metadatos(muta(lineas, x$rmd[1L], x$rmd[2L])); sem <- NA
    } else if (x$tipo == "variacion") {
      real <- sonda_variacion(dg); sem <- NA
    } else if (x$tipo == "moodle") {
      d <- tempfile("mut_moodle_"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
      f1 <- file.path(d, basename(RMD)); writeLines(muta(lineas, x$rmd[1L], x$rmd[2L]), f1, useBytes = TRUE)
      sem <- 1L; real <- sondas_moodle(sem, f1)
    } else if (x$tipo == "uso") {
      sem <- NA; sl <- readLines(USO_SEMILLERO, encoding = "UTF-8"); tl <- readLines(USO_PLANTILLA, encoding = "UTF-8")
      if (!is.null(x$semillero)) sl <- muta(sl, x$semillero[1L], x$semillero[2L])
      if (!is.null(x$plantilla)) tl <- muta(tl, x$plantilla[1L], x$plantilla[2L])
      real <- as.character(sondas_uso(sl, tl, render = isTRUE(x$render)))
    } else if (x$tipo == "papel") {
      ## Se muta una copia del .Rmd o de la plantilla: el ancho vive en cinco líneas, y el papel en la plantilla.
      d <- tempfile("mut_papel_"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE); l <- lineas; tp <- readLines(PAPEL_TPL, encoding = "UTF-8")
      if (!is.null(x$ancho)) {
        n <- sum(grepl("width=5.5cm", l, fixed = TRUE)); if (n != 5L) stop("MUTANTE MAL CONSTRUIDO: el ancho aparece ", n, " veces, y son 5")
        l <- gsub("width=5.5cm", paste0("width=", x$ancho), l, fixed = TRUE)
      }
      if (!is.null(x$hoja)) {
        n <- sum(grepl("papersize={216mm,330mm}", tp, fixed = TRUE)); if (n != 1L) stop("MUTANTE MAL CONSTRUIDO: el papel aparece ", n, " veces")
        tp <- gsub("papersize={216mm,330mm}", x$hoja, tp, fixed = TRUE)
      }
      f1 <- file.path(d, basename(RMD)); f2 <- file.path(d, "solpcielo.tex"); writeLines(l, f1, useBytes = TRUE); writeLines(tp, f2, useBytes = TRUE)
      sem <- 1L; real <- sondas_papel(sem, f1, f2)$fallas
    } else if (x$tipo == "solucion") {
      real <- character(0); sem <- NA
      for (s in 1:600) { r <- sondas_solucion(correr(s, dg), sol); if (length(r)) { real <- r; sem <- s; break } }
    } else if (x$tipo == "figura") {
      real <- character(0); sem <- NA
      for (s in 1:12) { e <- correr(s, dg); if (e$es_canonica) next; r <- sondas_figura(e, dib); if (length(r)) { real <- r; sem <- s; break } }
    } else if (x$tipo %in% c("bordes", "copia")) {
      e <- correr(1L, dg); sem <- 1L
      if (!is.null(x$env) && !isTRUE(x$env(e))) stop("MUTANTE MAL CONSTRUIDO: no llegó al entorno: ", x$id)
      real <- if (x$tipo == "bordes") sondas_bordes(e)$fallas else sonda_copia(e$GEOM_VENN, e$dibujar_venn)
    } else if (x$tipo == "guarda") {
      real <- character(0); sem <- NA
      for (s in 1:600) { r <- tryCatch({ correr(s, dg); NULL }, error = function(e) conditionMessage(e)); if (!is.null(r)) { real <- r; sem <- s; break } }
      real <- if (length(real) && grepl(x$sonda, real, fixed = TRUE)) x$sonda else substr(real, 1L, 44L)
    } else if (x$tipo == "cableado") {
      real <- character(0); sem <- NA
      for (s in 1:600) { e <- correr(s, dg); if (e$es_canonica) next; r <- sondas_version(e, dib); if (length(r)) { real <- r; sem <- s; break } }
    } else {
      e <- correr(1L, dg); sem <- 1L
      if (!is.null(x$env) && !isTRUE(x$env(e))) stop("MUTANTE MAL CONSTRUIDO: no llegó al entorno: ", x$id)
      g <- e$GEOM_VENN
      real <- if (x$tipo == "mapeo") sondas_mapeo(e, g) else unique(unlist(lapply(c(7L, 88L), function(v) sondas_tinta(leer_tinta(dibuja(e, rep(v, 7L), c("Ajedrez", "Dominó", "Cartas")), g), g)$fallas)))
    }
    vd <- veredicto(x$sonda, real)
    cat(sprintf("  %-40s esperada %-24s real %-34s semilla %-4s %s\n", x$id, x$sonda, paste(real, collapse = "+"), sem, vd))
    vd
  }, "")
  if (!all(res %in% c("cazado_por_su_sonda", "cazado_por_su_sonda_y_otra"))) rojo("mutantes", "algún mutante no murió por su sonda")
}

## --- firma §P7-E -------------------------------------------------------------------------
## Firma del profesor del 2026-11-06, transmitida por el coordinador: salida/tabla_firma_p7.md. Cubre las reglas de abajo y nada más: dos listas,
## regla por regla, sin tope por familia ni por estrato. Vale solo para este .Rmd: la enumeración exacta no lee el archivo, y ese hueco lo cubre el md5.
FIRMADA <- TRUE       # con FALSE el exit es 3 aunque todo cuadre
RMD_MD5 <- "f886dfaa45b80a5ee16f9820908049b5"
md5_coincide <- function(ruta = RMD) isTRUE(unname(tools::md5sum(ruta)) == RMD_MD5)
FIRMA_VARIANTES <- c(`MAG-suma-segunda-menor` = 100.000, `EST-menos-cifras-del-enunciado` = 69.553, `MAG-menor-maximo` = 53.032, `MAG-suma-no-extrema` = 50.000,
                     `MAG-sin-el-mayor-numero` = 50.000, `REL-no-extrema-en-coincidencias` = 50.000, `EST-primer-dato-en-la-zona-doble` = 50.000,
                     `EST-total-en-zona-de-un-conjunto` = 49.456)
FIRMA_CANONICA <- c(`MAG-suma-segunda-menor` = 100, `MAG-menor-maximo` = 100, `FOR-menos-dos-cifras` = 100, `FOR-cadena-corta` = 100, `LEX-con-repetidos` = 100,
                    `MAG-suma-no-extrema` = 50, `MAG-sin-el-mayor-numero` = 50, `MAG-centro-mayor` = 50, `DIV-suma-par` = 50, `REL-no-extrema-en-coincidencias` = 50,
                    `EST-total-en-zona-de-un-conjunto` = 50, `EST-primer-dato-en-la-zona-doble` = 50)
TOL_FIRMA <- 0.001    # las cifras firmadas van a tres decimales
## Tasas exactas de las variantes, en %. salida/firma_p7/exacto.py enumera las 11 000 000 versiones sin leer el .Rmd. Se invoca con python3 -I (modo aislado: no
## lee el sitio de usuario, que no trae numpy; numpy sale del sitio del sistema) y tarda alrededor de un minuto. Su md5 está congelado, y con otro contenido, un
## Python que sale con error o una regla repetida las tasas no se evalúan (exit 3, H25). No trae las reglas POS: valen 25 % por simetría, porque la mezcla es uniforme.
MD5_EXACTO <- "1d5f279462e10c2b5a8fa4e8954fdc6c"
RUTA_EXACTO <- file.path(aqui, "salida", "firma_p7", "exacto.py")
sin_tasas <- function(razon) structure(numeric(0), razon = razon)
parsear_tasas <- function(x) {
  if (!any(grepl("^versiones 11000000 ", x))) return(sin_tasas("EXACTO_SALIDA"))
  m <- regmatches(x, regexec("^([A-Z]+-[a-z0-9-]+) +([0-9.]+) %", x)); m <- m[lengths(m) == 3L]
  v <- stats::setNames(as.numeric(vapply(m, `[`, "", 3L)), vapply(m, `[`, "", 2L))
  if (anyDuplicated(names(v))) return(sin_tasas("EXACTO_REPETIDA"))
  c(v, stats::setNames(rep(25, 4L), paste0("POS-", c("primera", "segunda", "tercera", "cuarta"))))
}
tasas_exactas <- function(ruta = RUTA_EXACTO, md5 = MD5_EXACTO) {
  if (!identical(unname(tools::md5sum(ruta)), md5)) return(sin_tasas("EXACTO_MD5"))
  x <- tryCatch(suppressWarnings(system2("python3", c("-I", shQuote(ruta)), stdout = TRUE)), error = function(e) NULL)
  if (is.null(x) || !is.null(attr(x, "status"))) return(sin_tasas("EXACTO_PYTHON"))   # system2 deja el estado de Python en attr(, "status")
  parsear_tasas(x)
}
## Pura: la batería de R y la de Python deben ser la misma lista, sin faltantes, sobrantes ni repetidas: 34 reglas (las 30 de exacto.py y las 4 POS).
N_REGLAS <- 34L
bateria_evaluable <- function(nom, med_var, med_can) {
  if (length(nom) == N_REGLAS && !anyDuplicated(nom) && !anyDuplicated(names(med_var)) && !anyDuplicated(names(med_can)) &&
      setequal(nom, names(med_var)) && setequal(nom, names(med_can))) character(0) else "BATERIA_CONGELADA"
}
nombres_bateria <- function() {
  if (!exists("nueva_regla", mode = "function")) source(file.path(aqui, "..", "..", "..", ".claude", "scripts", "bateria_eliminacion.R"))
  vapply(reglas_p7(), function(x) x$nombre, "")
}
## Pura: compara una lista firmada con lo medido, en %. Una regla firmada debe reproducir su cifra; por encima o por debajo, no hay cobertura.
clasificar_firma <- function(medido, firma, techo, corte = 8) {
  sobre8 <- names(medido)[medido - techo > corte]; comun <- intersect(names(firma), names(medido))
  c(if (length(setdiff(sobre8, names(firma)))) "FIRMA_FUERA_DE_LISTA",
    if (any(medido[comun] > firma[comun] + TOL_FIRMA)) "FIRMA_SOBRE_CIFRA",
    if (length(setdiff(names(firma), names(medido))) || any(medido[comun] < firma[comun] - TOL_FIRMA)) "FIRMA_NO_REPRODUCE")
}
## Las secciones que «todo» debe haber corrido: decidir_exit exige el conjunto completo, y no se queda con que «no hubo rojos» (H25).
SECCIONES <- c("espacio", "cableado", "figura", "tinta", "solucion", "papel", "moodle", "uso", "mutantes", "bateria")
razones_pendiente <- function(evaluado, pendientes, firmada, modo, md5_ok, corridas) {
  c(if (!isTRUE(firmada)) "FIRMA_SIN_FIRMAR", if (!isTRUE(md5_ok)) "FIRMA_MD5", if (!isTRUE(evaluado)) "FIRMA_SIN_EVALUAR", if (modo != "todo") "FIRMA_MODO",
    if (length(setdiff(SECCIONES, corridas))) "SECCION_SIN_CORRER", pendientes)
}
## Decisión de salida, pura. 4 solo en «todo» y solo firmada; nunca 0.
decidir_exit <- function(errores, evaluado, pendientes, firmada, modo, md5_ok, corridas) {
  if (errores > 0L) return(1L)
  if (length(razones_pendiente(evaluado, pendientes, firmada, modo, md5_ok, corridas))) return(3L)
  4L
}
## Para los mutantes: por defecto, todo en orden y cubierto. Devuelve las razones por las que deja de estarlo.
prueba_firma <- function(mv = c(FIRMA_VARIANTES, `LEX-con-repetidos` = 33.418), mc = c(FIRMA_CANONICA, `POS-primera` = 25), firmada = TRUE, modo = "todo", md5_ok = TRUE, evaluado = TRUE, corridas = SECCIONES) {
  p <- unique(c(clasificar_firma(mv, FIRMA_VARIANTES, 32.2), clasificar_firma(mc, FIRMA_CANONICA, 38.5)))
  if (decidir_exit(0L, evaluado, p, firmada, modo, md5_ok, corridas) == 3L) razones_pendiente(evaluado, p, firmada, modo, md5_ok, corridas) else "EXIT_CUBIERTO"
}
informe_firma <- function(medido, firma, techo, rotulo, corte) {
  cat(sprintf("\n  FIRMA, %s (techo nulo %.1f %%): %d reglas firmadas, cifras exactas\n", rotulo, techo, length(firma)))
  for (n in names(firma)) cat(sprintf("    %-36s firmada %7.3f %%  medida %7.3f %%  exceso %+.1f pp\n", n, firma[[n]], unname(medido[n]), unname(medido[n]) - techo))
  cat(sprintf("    reglas sobre +8 pp fuera de la lista: %d\n", length(setdiff(names(medido)[medido - techo > corte], names(firma)))))
}

## --- batería §P7 --------------------------------------------------------------------------
reglas_p7 <- function() {
  mx <- function(o) t(vapply(strsplit(o, " ", fixed = TRUE), as.integer, integer(7)))
  conj <- function(x) if (any(x) && !all(x)) x else NA
  L <- function(nom, fam, fn, est = FALSE) nueva_regla(nom, fam, fn, usa_estimulo = est)
  sm <- function(o) rowSums(mx(o)); gr <- function(o) apply(mx(o), 1L, max); ct <- function(o) mx(o)[, 4L]
  dosc <- function(o) rowSums(mx(o) >= 10L); rep_int <- function(o) apply(mx(o), 1L, function(v) length(v) - length(unique(v)))
  conv <- function(o) { M <- mx(o); vapply(1:4, function(i) sum(vapply(setdiff(1:4, i), function(j) sum(M[i, ] == M[j, ]), 0L)), 0L) }
  lit <- function(o, e) rowSums(matrix(mx(o) %in% e, nrow = 4L))
  list(
    L("MAG-suma-max", "magnitud", function(o) conj(sm(o) == max(sm(o)))), L("MAG-suma-min", "magnitud", function(o) conj(sm(o) == min(sm(o)))),
    L("MAG-suma-segunda-menor", "magnitud", function(o) conj(rank(sm(o), ties.method = "min") == 2L)),
    L("MAG-suma-segunda-mayor", "magnitud", function(o) conj(rank(-sm(o), ties.method = "min") == 2L)),
    L("MAG-suma-no-extrema", "magnitud", function(o) conj(sm(o) != max(sm(o)) & sm(o) != min(sm(o)))),
    L("MAG-mayor-numero", "magnitud", function(o) conj(gr(o) == max(gr(o)))), L("MAG-menor-maximo", "magnitud", function(o) conj(gr(o) == min(gr(o)))),
    L("MAG-sin-el-mayor-numero", "magnitud", function(o) conj(gr(o) != max(gr(o)))),
    L("MAG-centro-mayor", "magnitud", function(o) conj(ct(o) == max(ct(o)))), L("MAG-centro-menor", "magnitud", function(o) conj(ct(o) == min(ct(o)))),
    L("DIV-suma-par", "divisibilidad", function(o) conj(sm(o) %% 2L == 0L)), L("DIV-suma-impar", "divisibilidad", function(o) conj(sm(o) %% 2L == 1L)),
    L("DIV-suma-mult5", "divisibilidad", function(o) conj(sm(o) %% 5L == 0L)),
    L("DIV-centro-par", "divisibilidad", function(o) conj(ct(o) %% 2L == 0L)), L("DIV-centro-impar", "divisibilidad", function(o) conj(ct(o) %% 2L == 1L)),
    L("SIG-tiene-cero", "signo", function(o) conj(rowSums(mx(o) == 0L) > 0L)), L("SIG-sin-cero", "signo", function(o) conj(rowSums(mx(o) == 0L) == 0L)),
    L("POS-primera", "posicion", function(o) 1L), L("POS-segunda", "posicion", function(o) 2L), L("POS-tercera", "posicion", function(o) 3L), L("POS-cuarta", "posicion", function(o) 4L),
    L("FOR-mas-dos-cifras", "formato", function(o) conj(dosc(o) == max(dosc(o)))), L("FOR-menos-dos-cifras", "formato", function(o) conj(dosc(o) == min(dosc(o)))),
    L("FOR-cadena-larga", "formato", function(o) conj(nchar(o) == max(nchar(o)))), L("FOR-cadena-corta", "formato", function(o) conj(nchar(o) == min(nchar(o)))),
    L("LEX-con-repetidos", "lexico", function(o) conj(rep_int(o) > 0L)), L("LEX-sin-repetidos", "lexico", function(o) conj(rep_int(o) == 0L)),
    L("REL-mas-coincidencias", "lexico", function(o) conj(conv(o) == max(conv(o)))), L("REL-menos-coincidencias", "lexico", function(o) conj(conv(o) == min(conv(o)))),
    L("REL-no-extrema-en-coincidencias", "lexico", function(o) conj(conv(o) != max(conv(o)) & conv(o) != min(conv(o)))),
    L("EST-mas-cifras-del-enunciado", "formato", function(o, e) conj(lit(o, e) == max(lit(o, e))), TRUE),
    L("EST-menos-cifras-del-enunciado", "formato", function(o, e) conj(lit(o, e) == min(lit(o, e))), TRUE),
    L("EST-total-en-zona-de-un-conjunto", "formato", function(o, e) conj(rowSums(matrix(mx(o)[, c(1L, 3L, 7L)] %in% e[5:7], nrow = 4L)) == 0L), TRUE),
    L("EST-primer-dato-en-la-zona-doble", "formato", function(o, e) conj(mx(o)[, 2L] == e[1L]), TRUE)
  )
}
sec_bateria <- function() {
  cat("\n[bateria] §P7, congelada antes del primer resultado\n")
  source(file.path(aqui, "..", "..", "..", ".claude", "scripts", "bateria_eliminacion.R"))
  reglas <- reglas_p7(); m <- list(); s <- 0L
  while (length(m) < 100L) { s <- s + 1L; e <- correr(s); if (e$es_canonica) next
    m[[length(m) + 1L]] <- list(ops = e$opciones, key = which(e$sol), est = as.integer(unlist(e$dat)[c("a", "b", "c", "d", "P", "M", "B")])) }
  r <- evaluar_bateria(reglas, lapply(m, `[[`, "ops"), vapply(m, `[[`, 0L, "key"), estimulos = lapply(m, `[[`, "est"))
  imprimir_bateria(r)
  ## Canónica: los 24 órdenes, sin azar.
  perm <- function(v) if (length(v) == 1L) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perm(v[-i]), function(p) c(v[i], p))))
  base <- c("7 3 9 7 0 1 5", "0 10 2 3 4 5 1", "17 3 20 7 0 1 13", "17 10 20 3 7 8 13"); est <- c(3L, 10L, 8L, 7L, 17L, 20L, 13L)
  pp <- perm(1:4)
  rc <- evaluar_bateria(reglas, lapply(pp, function(p) base[p]), vapply(pp, function(p) which(p == 1L), 0L), estimulos = rep(list(est), 24L))
  cat("\n  --- canónica, 24 órdenes ---\n"); imprimir_bateria(rc)
  ## §P7-E: lo que pasa de +8 pp no es PASS ni defecto corregido. Se contrasta, regla por regla, con el alcance firmado y con cifras exactas: las tasas de
  ## N = 100 de arriba pueden quedar hasta 5 pp por debajo de la real, y no deciden nada.
  med_var <- tasas_exactas(); med_can <- stats::setNames(100 * rc$detalle$tasa, rc$detalle$regla); nom <- vapply(reglas, function(x) x$nombre, "")
  ev <- !length(bateria_evaluable(nom, med_var, med_can))
  if (!ev) cat("  Sin evaluar:", paste(c(bateria_evaluable(nom, med_var, med_can), attr(med_var, "razon")), collapse = ", "), "\n")
  tv <- 100 * r$techo_nulo; tc <- 100 * rc$techo_nulo; cv <- 100 * r$corte_canal; cc <- 100 * rc$corte_canal
  pv <- if (ev) clasificar_firma(med_var, FIRMA_VARIANTES, tv, cv) else character(0)
  pc <- clasificar_firma(med_can, FIRMA_CANONICA, tc, cc)
  if (ev) informe_firma(med_var, FIRMA_VARIANTES, tv, "variantes, 11 000 000 de versiones", cv) else cat("\n  FIRMA: sin tasas exactas de las variantes o sin alguna regla\n")
  informe_firma(med_can, FIRMA_CANONICA, tc, "canónica, 24 órdenes", cc)
  firma_estado <<- list(evaluado = ev, pendientes = unique(c(pv, pc)))
  invisible(r)
}

## --- principal ------------------------------------------------------------------------------
firma_estado <- list(evaluado = FALSE, pendientes = character(0))
## Cada sección se anota al terminar; decidir_exit exige el conjunto completo y no confía en que «no hubo rojos».
corridas <- character(0)
corre <- function(nombre, f) { f(); corridas <<- c(corridas, nombre); invisible(NULL) }
if (modo %in% c("todo", "espacio")) corre("espacio", sec_espacio)
if (modo %in% c("todo", "cableado")) corre("cableado", sec_cableado)
if (modo %in% c("todo", "figura")) corre("figura", sec_figura)
if (modo %in% c("todo", "tinta")) corre("tinta", sec_tinta)
if (modo %in% c("todo", "solucion")) corre("solucion", sec_solucion)
if (modo %in% c("todo", "papel")) corre("papel", sec_papel)
if (modo %in% c("todo", "moodle")) corre("moodle", sec_moodle)
if (modo %in% c("todo", "uso")) corre("uso", sec_uso)
if (modo %in% c("todo", "mutantes")) corre("mutantes", sec_mutantes)
if (modo %in% c("todo", "bateria")) corre("bateria", sec_bateria)
cat("\nRESULTADO:", if (length(rojos)) paste("ROJO en", paste(unique(rojos), collapse = ", ")) else "sin rojos en las sondas de I-3, I-4 e I-5", "\n")
md5_ok <- md5_coincide(); salida <- decidir_exit(length(rojos), firma_estado$evaluado, firma_estado$pendientes, FIRMADA, modo, md5_ok, corridas)
if (salida == 4L) cat("BATERÍA §P7: ROJO CUBIERTO POR LA FIRMA del profesor, regla por regla (no es PASS) · pendientes: 0\n")
if (salida == 3L) cat("BATERÍA §P7: sin cubrir:", paste(razones_pendiente(firma_estado$evaluado, firma_estado$pendientes, FIRMADA, modo, md5_ok, corridas), collapse = ", "), "\n")
quit(save = "no", status = salida)
