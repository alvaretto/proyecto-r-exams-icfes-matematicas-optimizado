#!/usr/bin/env Rscript
# verificar_espacio_cloze.R — CLOZE hermano de diagrama-venn-juegos-mesa-n3 (C-1 a C-5 de ../.claude/CLAUDE.md).
#
#   e1       : el bloque P1 del CLOZE es el data_generation del cuerpo firmado del SCHOICE con tres renombres de código
#              (comparación de TEXTO), da los mismos objetos y el mismo .Random.seed en 100 variantes y 20 canónicas, y el
#              chunk dibujo_opciones es idéntico. Dos controles positivos deben dar diferencias.
#   texto    : oráculo desde el TEXTO que lee el estudiante (V10). Cuotas por rama: 100 variantes por cada cruce de la zona
#              de la Parte 3 (2) y la de la Parte 4 (3), y 20 canónicas por cruce. Lee las viñetas, los rótulos y las
#              preguntas con patrones, calcula las zonas con su propia semántica (no usa FORMULAS del .Rmd) y las contrasta
#              con exsolution. Cubre V11 en los dos sentidos y por contenido (C-2), la Parte 6 y la marca de la Parte 1.
#   pesos    : exclozetype, expoints 1|1|1|1|1|1 (C-3), extol, exshuffle y los seis ##ANSWERi## en orden (V1, V2) en el .Rmd.
#              Los pesos del XML los mide la sección moodle sobre un XML propio (FASE 2C r1, H1).
#   cruce    : canales de las Partes 2 a 6 hacia la Parte 1 (§P7-G, reglas del estímulo), N = 100. Sobre +8 pp vuelve
#              al profesor (C-4): exit 3.
#   bateria  : §P7 de la Parte 1 sobre los objetos del CLOZE (C-4): N = 100, la canónica en sus 24 órdenes y las tasas
#              exactas de ../salida/firma_p7/exacto.py. Las funciones salen de ../verificar_espacio.R (md5 congelado).
#   mutantes : contrato INC-MUTANTE-SONDA: sonda declarada, patrón único, materialización en el entorno, semillas hasta la
#              primera muerte (tope 600) y línea base sin mutar en la semilla de la muerte (obligación 8). Desde la tanda 2,
#              cada componente del oráculo tiene un mutante que la mueve sola (obligación 7).
#   v6       : Incidente Q sobre el HTML REAL de exams2html (familia B de la regla #23, N = 100): empareja por contenido las
#              opciones de los gaps 1 y 6 en el orden mostrado, la lista de la Solution y la prosa agrupada. Controles: copias
#              mutadas del .Rmd en tempdir(), nunca el archivo real.
#
#   moodle   : FASE 2C r1, H1. exams2moodle propio (familia B, N = 100) y marcas, NUMERICAL, Solution e imagen de la clave contra
#              la verdad recalculada desde las viñetas del propio XML; controles en copias mutadas del .Rmd en tempdir().
#   solucion : FASE 2C r1, H2 (I-4.7). Chunks solucion y feedback en las cuotas de texto: cada párrafo con su plantilla y sus cifras
#              contra las viñetas, letra a letra en su sección y en su orden, y la retroalimentación de los seis gaps; sin prosa
#              estática fuera de los chunks (FASE 2C r2, H2). 28 mutantes.
#   papel    : FASE 2C r1, H2 (I-5). N = 100 PDF reales en oficio con ../solpcielo_cloze.tex: hoja, las cuatro figuras en una
#              página, nada fuera de la caja de texto en ninguna página y el «Cómo marcar» de la portada; y 34 PDF de tres
#              preguntas con solpcielo_cloze_una_por_pagina.tex (decisión 1 b del profesor tras la tanda 3), la condición de uso de
#              la I-5 (FASE 2C r2, H1): hoja, las cuatro figuras de cada pregunta en una página y «Cómo marcar». 5 mutantes.
#   pantalla : FASE 2C r1, H2 (I-5 a 320 y 360 px). exams2html N = 100 medido en Chromium por CDP: sin desborde, ancho de cada
#              figura, rótulo encima y sin solapes. 5 mutantes.
#   uso      : decisión del profesor tras la tanda 3 («semillero sí»). SemilleroCloze.R de esta carpeta: sus literales, la plantilla
#              local frente a ../solpcielo_cloze.tex (solo el comentario y \clearpage), y sus funciones generar_pdf, generar_html y
#              generar_moodle, extraídas de su texto y ejecutadas en una carpeta temporal: hoja, figuras de cada pregunta en una
#              página, «Cómo marcar», HTML sin ##ANSWER y gaps del XML. Tanda 5 («H1 c · H2 100»): exshuffle = FALSE solo en
#              exams2pdf() y archivos = 100 en el árbol sintáctico, el gap de la Parte 1 en el orden I-IV en el PDF real y la clave que
#              registra exams2pdf() en el diagrama que reproduce las viñetas impresas. 17 mutantes.
#
# Uso: cd Cloze && Rscript --no-init-file verificar_espacio_cloze.R [todo|e1|texto|pesos|cruce|bateria|mutantes|v6|moodle|solucion|papel|pantalla|uso]
# Salida: 1 = alguna sonda en rojo; 3 = sin cubrir (FIRMADA FALSE, md5 del .Rmd distinto de RMD_MD5, firma que no cuadra,
#         canal cruzado sobre +8 pp, sección sin correr o modo distinto de «todo»); 4 = rojo §P7 de la Parte 1 cubierto por la
#         firma (C-4), solo en «todo». Nunca es PASS. 0 no ocurre con este ítem.

args <- commandArgs(TRUE); modo <- if (length(args)) args[1] else "todo"
archivo_self <- sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))
aqui <- if (length(archivo_self)) dirname(normalizePath(archivo_self)) else getwd()
SUB <- normalizePath(file.path(aqui, ".."))
RMD <- file.path(aqui, "diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd")
RMD_S <- file.path(SUB, "diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd")
VERIF_S <- file.path(SUB, "verificar_espacio.R")
suppressMessages(library(exams))
lineas <- readLines(RMD, encoding = "UTF-8")
cat("md5 del .Rmd CLOZE:", unname(tools::md5sum(RMD)), "· exams", as.character(packageVersion("exams")), "· modo", modo, "\n")
rojos <- character(0); pendientes <- character(0); corridas <- character(0)
rojo <- function(sec, det) { rojos <<- c(rojos, sec); cat("  ROJO", sec, ":", det, "\n") }
pendiente <- function(cod, det) { pendientes <<- c(pendientes, cod); cat("  PENDIENTE", cod, ":", det, "\n") }

## --- Firma (C-4) ---------------------------------------------------------------------------------------------
## Firma del profesor del 2026-11-09 (D4 a, H24 a1): la de la Parte 1 del SCHOICE, extendida por alcance, más
## LEX-con-repetidos en las variantes con el techo exacto. RMD_MD5 fija a qué archivo se refiere el resultado: si el .Rmd
## cambia, el exit pasa a 3 hasta medir de nuevo y actualizarlo.
FIRMADA <- TRUE
RMD_MD5 <- "889d828e8129e11b0ec5dceda0cf108b"
MD5_CUERPO <- "f886dfaa45b80a5ee16f9820908049b5"          # cuerpo firmado del SCHOICE
MD5_VERIF_S <- "d7271f811e27f9b12766ead32280fa6d"         # verificar_espacio.R del SCHOICE, de donde salen las funciones §P7
TECHO_EXACTO_VARIANTES <- 25.03                           # H24 (a1): techo nulo exacto de las variantes, en %
CORTE <- 8

## --- Utilidades ------------------------------------------------------------------------------------------------
chunk_de <- function(lin, nombre) {
  i <- grep(paste0("^```\\{r ", nombre, "[ ,}]"), lin); stopifnot(length(i) == 1L)
  f <- i + which(lin[(i + 1L):length(lin)] == "```")[1L]; lin[(i + 1L):(f - 1L)]
}
compila <- function(cod) parse(text = paste(cod, collapse = "\n"), encoding = "UTF-8", keep.source = FALSE)
cuerpo_firmado <- function(ruta) {
  raw <- readBin(ruta, "raw", file.size(ruta)); nl <- which(raw == as.raw(10L))
  lin <- readLines(ruta, encoding = "UTF-8", warn = FALSE); desde <- 1L
  if (length(lin) && lin[1L] == "---") desde <- nl[which(lin == "---")[2L]] + 1L
  tf <- tempfile(fileext = ".Rmd"); writeBin(raw[desde:length(raw)], tf)
  if (unname(tools::md5sum(tf)) != MD5_CUERPO) stop("el cuerpo del SCHOICE no es el firmado")
  readLines(tf, encoding = "UTF-8", warn = FALSE)
}
MAPA <- c(opciones = "opciones_p1", sol = "sol_p1", feedback = "feedback_p1")
renombrar_codigo <- function(blk) {
  pd <- utils::getParseData(parse(text = blk, keep.source = TRUE, encoding = "UTF-8"))
  s <- pd[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB") & pd$text %in% names(MAPA), ]
  stopifnot(nrow(s) == length(MAPA), !anyDuplicated(s$text))
  for (r in seq_len(nrow(s))) { l <- blk[s$line1[r]]
    blk[s$line1[r]] <- paste0(substr(l, 1L, s$col1[r] - 1L), MAPA[[s$text[r]]], substr(l, s$col2[r] + 1L, nchar(l))) }
  blk
}
CUERPO_S <- cuerpo_firmado(RMD_S)
DG_C <- chunk_de(lineas, "data_generation")
i_ini <- which(DG_C == "## @@INICIO_BLOQUE_P1@@"); i_fin <- which(DG_C == "## @@FIN_BLOQUE_P1@@")
stopifnot(length(i_ini) == 1L, length(i_fin) == 1L, i_fin > i_ini + 1L)
BLK_C <- DG_C[(i_ini + 1L):(i_fin - 1L)]
NOMBRES_TEXTO <- c("enunciado", "parte2", "parte3", "parte4", "parte5", "parte6", "answerlist_q")
CH_TEXTO <- lapply(NOMBRES_TEXTO, function(n) compila(chunk_de(lineas, n)))
correr <- function(s, expr) { e <- new.env(parent = globalenv()); set.seed(s); eval(expr, e); e }
texto_de <- function(e, CH = CH_TEXTO) paste(utils::capture.output(for (ex in CH) eval(ex, e)), collapse = "\n")
mutar <- function(cod, viejo, nuevo) {
  n <- sum(lengths(regmatches(cod, gregexpr(viejo, cod, fixed = TRUE))))
  if (n != 1L) return(structure(cod, mal = sprintf("el patrón aparece %d veces", n)))
  sub(viejo, nuevo, cod, fixed = TRUE)
}
ORDEN <- c("s1", "p12", "s2", "ctr", "p13", "p23", "s3")

## --- e1 ----------------------------------------------------------------------------------------------------------
norm <- function(x) {
  if (is.function(x)) return(paste(deparse(x), collapse = "\n"))
  if (is.environment(x)) return("<entorno>")
  if (is.list(x)) { y <- lapply(x, norm); names(y) <- names(x); return(y) }
  x
}
comparar <- function(e0, e1) {
  n0 <- sort(ls(e0, all.names = TRUE))
  for (nm in n0) {
    nm1 <- if (nm %in% names(MAPA)) MAPA[[nm]] else nm
    if (!exists(nm1, envir = e1, inherits = FALSE)) return(paste("falta", nm1))
    if (!identical(norm(get(nm, e0)), norm(get(nm1, e1)))) return(paste("difiere", nm))
  }
  sobran <- setdiff(ls(e1, all.names = TRUE), c(setdiff(n0, names(MAPA)), unname(MAPA[names(MAPA) %in% n0])))
  if (length(sobran)) return(paste("sobran", paste(sobran, collapse = ",")))
  "ok"
}
e1_correr <- function(blk_c, n_var = 100L, n_can = 20L) {
  x0 <- compila(chunk_de(CUERPO_S, "data_generation")); x1 <- compila(blk_c)
  nv <- 0L; nc <- 0L; s <- 0L; fallos <- character(0)
  while ((nv < n_var || nc < n_can) && s < 5000L) {
    s <- s + 1L
    e0 <- correr(s, x0); rs0 <- get(".Random.seed", envir = globalenv())
    can <- isTRUE(e0$es_canonica)
    if ((can && nc >= n_can) || (!can && nv >= n_var)) next
    e1 <- tryCatch(correr(s, x1), error = function(err) NULL)
    v <- if (is.null(e1)) "error al correr" else comparar(e0, e1)
    if (!is.null(e1) && !identical(rs0, get(".Random.seed", envir = globalenv()))) v <- paste(v, "+ .Random.seed distinto")
    if (v != "ok") fallos <- c(fallos, sprintf("semilla %d: %s", s, v))
    if (can) nc <- nc + 1L else nv <- nv + 1L
  }
  list(fallos = fallos, nv = nv, nc = nc, s = s)
}
sec_e1 <- function() {
  cat("\n[e1] trasplante de la Parte 1 (C-2)\n")
  esperado <- renombrar_codigo(chunk_de(CUERPO_S, "data_generation"))
  if (identical(BLK_C, esperado)) cat(sprintf("  texto: el bloque P1 es el data_generation firmado con 3 renombres (%d líneas)\n", length(BLK_C)))
  else rojo("e1", "el bloque P1 no es el data_generation firmado con sus 3 renombres")
  if (identical(chunk_de(lineas, "dibujo_opciones"), chunk_de(CUERPO_S, "dibujo_opciones"))) cat("  dibujo_opciones: idéntico al del SCHOICE\n")
  else rojo("e1", "dibujo_opciones distinto del SCHOICE")
  r <- e1_correr(BLK_C)
  cat(sprintf("  objetos y .Random.seed: %d variantes + %d canónicas (semillas 1..%d): %d con diferencias\n", r$nv, r$nc, r$s, length(r$fallos)))
  if (r$nv < 100L || r$nc < 20L || length(r$fallos)) rojo("e1", paste(c("muestra incompleta o diferencias", head(r$fallos, 3)), collapse = "; "))
  ## Controles positivos: la sonda debe disparar.
  c1 <- mutar(BLK_C, "ops <- ops[sample.int(4L)]", "invisible(runif(1L)); ops <- ops[sample.int(4L)]")
  c2 <- mutar(BLK_C, "sol_p1 <- vapply(ops,", "sol <- vapply(ops,")
  for (cc in list(list("un sorteo de más", c1), list("sin renombrar sol", c2))) {
    if (!is.null(attr(cc[[2]], "mal"))) { rojo("e1", paste("control mal construido:", cc[[1]])); next }
    rc <- e1_correr(cc[[2]], 10L, 2L)
    cat(sprintf("  [control] %-18s %d de %d con diferencias%s\n", cc[[1]], length(rc$fallos), rc$nv + rc$nc,
                if (length(rc$fallos)) paste0(" (", rc$fallos[1], ")") else ""))
    if (!length(rc$fallos)) rojo("e1", paste("el control no disparó:", cc[[1]]))
  }
}

## --- Oráculo desde el texto -----------------------------------------------------------------------------------------
leer_vinetas <- function(v) {
  if (length(v) != 3L) return(NULL)
  m1 <- regmatches(v[1], regexec("^- ([0-9]+) (\\S+) (\\S+) (\\S+) y (\\S+), pero no (\\S+)\\.$", v[1]))[[1]]
  m2 <- regmatches(v[2], regexec(paste0("^- ([0-9]+) \\S+ \\S+ (\\S+) y (\\S+), ([0-9]+) \\S+ \\S+ (\\S+) y (\\S+), ",
                                        "y ([0-9]+) \\S+ \\S+ (\\S+) y (\\S+)\\.$"), v[2]))[[1]]
  m3 <- regmatches(v[3], regexec("^- ([0-9]+) \\S+ \\S+ (\\S+), ([0-9]+) \\S+ \\S+ (\\S+) y ([0-9]+) \\S+ \\S+ (\\S+)\\.$", v[3]))[[1]]
  if (length(m1) != 7L || length(m2) != 10L || length(m3) != 7L) return(NULL)
  X <- m1[5]; Y <- m1[6]; Z <- m1[7]; a <- as.integer(m1[2]); S <- c(X, Y, Z)
  par <- function(p, q) paste(sort(c(p, q)), collapse = "|")
  inter <- stats::setNames(as.integer(m2[c(2, 5, 8)]), c(par(m2[3], m2[4]), par(m2[6], m2[7]), par(m2[9], m2[10])))
  tot <- stats::setNames(as.integer(m3[c(2, 4, 6)]), m3[c(3, 5, 7)])
  if (!setequal(names(inter), c(par(X, Y), par(Y, Z), par(X, Z))) || !setequal(names(tot), S)) return(NULL)
  b <- inter[[par(X, Y)]]; cc <- inter[[par(Y, Z)]]; d <- inter[[par(X, Z)]]
  ctr <- b - a; pYZ <- cc - ctr; pXZ <- d - ctr
  reg <- c(s1 = tot[[X]] - a - pXZ - ctr, p12 = a, s2 = tot[[Y]] - a - pYZ - ctr, ctr = ctr, p13 = pXZ, p23 = pYZ,
           s3 = tot[[Z]] - pXZ - pYZ - ctr)
  list(S = S, reg = reg, datos = c(a, b, cc, d, tot[[X]], tot[[Y]], tot[[Z]]), grupo = m1[3], verbo = m1[4])
}
zona_de <- function(frase, S) {
  if (grepl("^(los|las) tres ", frase)) return("ctr")
  m <- regmatches(frase, regexec("^solo (\\S+) y (\\S+)$", frase))[[1]]
  if (length(m) == 3L) { k <- sort(match(m[2:3], S)); return(switch(paste(k, collapse = ""), "12" = "p12", "13" = "p13", "23" = "p23", NA)) }
  m <- regmatches(frase, regexec("^solo (\\S+)$", frase))[[1]]
  if (length(m) == 2L) return(c("s1", "s2", "s3")[match(m[2], S)])
  NA_character_
}
num <- function(s) as.integer(unlist(regmatches(s, gregexpr("[0-9]+", s))))
oraculo <- function(e, txt) {
  L <- strsplit(txt, "\n", fixed = TRUE)[[1]]; cod <- character(0)
  i1 <- grep("^\\*\\*Parte 1\\.\\*\\*", L); io <- grep("^\\*\\*Las Partes 2 a 6 se refieren a otra encuesta\\.\\*\\*", L)
  ip <- vapply(2:6, function(k) { i <- grep(paste0("^\\*\\*Parte ", k, "\\.\\*\\*"), L); if (length(i) == 1L) i else NA_integer_ }, 0L)
  if (length(i1) != 1L || length(io) != 1L || anyNA(ip)) return("ESTRUCTURA")
  o1 <- leer_vinetas(grep("^- ", L[seq_len(i1 - 1L)], value = TRUE))
  o2 <- leer_vinetas(grep("^- ", L[(io + 1L):(ip[1] - 1L)], value = TRUE))
  if (is.null(o1) || is.null(o2)) return("VINETAS")
  ## Parte 1: rótulo -> archivo en el enunciado; el diagrama de la clave es el que reproduce las viñetas de la Parte 1.
  ## Rótulo inmediatamente antes de su imagen, en la misma figura: «**Diagrama N**», salto de línea y la imagen.
  img <- regmatches(txt, gregexpr("\\*\\*Diagrama (I|II|III|IV)\\*\\*\\\\\n!\\[[^]]*\\]\\((diagrama_[a-d]_[0-9a-f]+\\.png)\\)", txt))[[1]]
  rot <- sub("^\\*\\*Diagrama (I|II|III|IV)\\*\\*.*$", "\\1", img); arch <- sub("^.*\\((diagrama_[^)]+)\\)$", "\\1", img)
  if (length(img) != 4L || !setequal(rot, c("I", "II", "III", "IV"))) return(c(cod, "P1_ROTULOS"))
  reg_img <- lapply(arch, function(a) e$regiones_mezcladas[[match(a, e$archivos)]])
  clave_rot <- rot[vapply(reg_img, function(r) identical(as.integer(r), as.integer(o1$reg)), TRUE)]
  items <- sub("^\\* ", "", grep("^\\* ", L[(ip[5] + 1L):length(L)], value = TRUE))
  ex <- strsplit(e$exsolution_str, "|", fixed = TRUE)[[1]]
  if (length(items) != 8L || length(ex) != 6L) return(c(cod, "ESTRUCTURA"))
  b1 <- strsplit(ex[1], "")[[1]] == "1"
  if (length(clave_rot) != 1L || sum(b1) != 1L || items[which(b1)] != paste("Diagrama", clave_rot) ||
      !setequal(items[1:4], paste("Diagrama", c("I", "II", "III", "IV")))) cod <- c(cod, "P1_MARCA")
  ## Partes 2 a 5: la zona que nombra cada pregunta, calculada con la semántica de arriba.
  pq <- function(k) sub("^\\*\\*Parte [0-9]\\.\\*\\* ", "", L[ip[k - 1L]])
  q2 <- pq(2); q3 <- pq(3); q4 <- pq(4); q5 <- pq(5)
  z3 <- zona_de(sub("^.* (solo \\S+ y \\S+)\\?$", "\\1", q3), o2$S); z4 <- zona_de(sub("^.* (solo \\S+)\\?$", "\\1", q4), o2$S)
  if (!grepl(" (los|las) tres \\S+\\?$", q2) || !grepl(" al menos (uno de los|una de las) tres \\S+\\?$", q5) ||
      !(z3 %in% c("p13", "p23")) || !(z4 %in% c("s1", "s2", "s3"))) return(c(cod, "PREGUNTAS"))
  resp <- c(o2$reg[["ctr"]], o2$reg[[z3]], o2$reg[[z4]], sum(o2$reg))
  if (!identical(as.integer(resp), as.integer(ex[2:5]))) cod <- c(cod, "RESPUESTA")
  ## V11 en los dos sentidos y por contenido (C-2).
  diag1 <- unique(unlist(e$regiones_mezcladas)); impresos1 <- unique(c(num(L[seq_len(i1 - 1L)]), diag1))
  if (any(resp %in% impresos1)) cod <- c(cod, "V11_RESP")
  if (any(o2$datos %in% diag1)) cod <- c(cod, "V11_DATOS")
  if (any(resp %in% o2$datos)) cod <- c(cod, "RESP_IMPRESA")
  if (any(tolower(o2$S) %in% tolower(o1$S))) cod <- c(cod, "V11_CONTENIDO")
  if (any(grepl("pero no", c(L[ip], items[5:8]), fixed = TRUE))) cod <- c(cod, "PERO_NO")
  ## FASE 2C r1, H5: el vocabulario de error es otra componente (C-2 y conflicto 4), con su propio código.
  if (any(grepl("\\b(error|err[oó]ne|equivoc|incorrect|confund)", c(L[ip], items[5:8]), ignore.case = TRUE, perl = TRUE)))
    cod <- c(cod, "VOCAB_ERROR")
  ## Parte 6: cada afirmación, leída y juzgada desde el texto.
  af <- regmatches(items[5:8], regexec("^(Más|Menos) \\S+ \\S+ (.+) que (.+)\\.$", items[5:8]))
  if (any(lengths(af) != 4L)) return(c(cod, "P6_FORMA"))
  zx <- vapply(af, function(m) zona_de(m[3], o2$S), ""); zy <- vapply(af, function(m) zona_de(m[4], o2$S), "")
  if (anyNA(zx) || anyNA(zy)) return(c(cod, "P6_FORMA"))
  verdad <- ifelse(vapply(af, `[`, "", 2L) == "Más", o2$reg[zx] > o2$reg[zy], o2$reg[zx] < o2$reg[zy])
  if (!identical(unname(verdad), strsplit(ex[6], "")[[1]] == "1")) cod <- c(cod, "P6_VERDAD")
  if (any(o2$reg[zx] == o2$reg[zy])) cod <- c(cod, "P6_EMPATE")
  if (all(verdad) || !any(verdad)) cod <- c(cod, "P6_UNANIME")
  if (anyDuplicated(paste(pmin(zx, zy), pmax(zx, zy)))) cod <- c(cod, "P6_COMPLEMENTARIO")
  if (any(grepl("[0-9]", items[5:8]))) cod <- c(cod, "P6_CIFRAS")
  attr(cod, "rama") <- paste(z3, z4); cod
}
## Una versión por semilla: error del .Rmd (guarda) o códigos del oráculo.
juzgar <- function(s, expr, CH = CH_TEXTO) {
  e <- new.env(parent = globalenv()); set.seed(s)
  err <- tryCatch({ eval(expr, e); NULL }, error = function(x) conditionMessage(x))
  if (!is.null(err)) return(list(e = e, cod = paste0("GUARDA_RMD: ", err), can = isTRUE(e$es_canonica), rama = NA, txt = NA_character_))
  txt <- texto_de(e, CH); cod <- oraculo(e, txt)
  list(e = e, cod = as.character(cod), can = isTRUE(e$es_canonica), rama = attr(cod, "rama"), txt = txt)
}
sec_texto <- function() {
  cat("\n[texto] oráculo desde el texto, V11 y Parte 6 (cuotas: 100 variantes y 20 canónicas por rama de las Partes 3 y 4)\n")
  ex <- compila(DG_C); ramas <- as.vector(outer(c("p13", "p23"), c("s1", "s2", "s3"), paste))
  cuenta <- matrix(0L, 2L, 6L, dimnames = list(c("variante", "canonica"), ramas)); cuota <- c(variante = 100L, canonica = 20L)
  malos <- character(0); s <- 0L
  ## Tanda 2: TODA semilla visitada se juzga y cuenta como defecto si da códigos; las cuotas solo deciden cuándo parar.
  ## Antes, una versión con códigos de una rama con la cuota llena se saltaba sin contarse.
  while (any(cuenta < cuota) && s < 20000L) {
    s <- s + 1L; j <- juzgar(s, ex)
    if (length(j$cod)) malos <- c(malos, sprintf("semilla %d: %s", s, paste(j$cod, collapse = ",")))
    if (is.null(j$rama) || is.na(j$rama)) next   # FASE 2C r1, H6: un código temprano deja rama en NULL
    est <- if (j$can) "canonica" else "variante"
    if (cuenta[est, j$rama] < cuota[[est]]) cuenta[est, j$rama] <- cuenta[est, j$rama] + 1L
  }
  print(cuenta); cat(sprintf("  semillas 1..%d, todas juzgadas · versiones con algún código: %d\n", s, length(malos)))
  if (any(cuenta < 20L)) cat("  NO CONCLUYENTE: hay ramas con n < 20\n")
  if (length(malos) || any(cuenta < cuota)) rojo("texto", paste(c(head(malos, 3), if (any(cuenta < cuota)) "cuotas sin llenar"), collapse = "; "))
}

## --- pesos (V1, V2, C-3) --------------------------------------------------------------------------------------------
pesos_de <- function(lin) {
  meta <- function(k) { x <- grep(paste0("^", k, ":"), lin, value = TRUE); if (length(x) == 1L) trimws(sub("^[^:]+:", "", x)) else NA }
  cod <- character(0)
  if (!identical(meta("exclozetype"), "schoice|num|num|num|num|mchoice")) cod <- c(cod, "EXCLOZETYPE")
  if (!identical(meta("expoints"), "1|1|1|1|1|1")) cod <- c(cod, "PESOS")
  if (!identical(meta("extol"), "0|0|0|0|0|0")) cod <- c(cod, "EXTOL")
  if (!identical(meta("exshuffle"), "TRUE")) cod <- c(cod, "EXSHUFFLE")
  a <- grep("^##ANSWER[0-9]+##$", lin); p <- grep("^cat\\(\"\\*\\*Parte [0-9]\\.\\*\\* ", lin)
  if (!identical(as.integer(sub("^##ANSWER([0-9]+)##$", "\\1", lin[a])), 1:6) || length(p) != 6L ||
      !all(p < a) || !all(p[-1] > a[-6])) cod <- c(cod, "ANSWER_ORDEN")
  cod
}
sec_pesos <- function() {
  cat("\n[pesos] exclozetype, expoints, extol, exshuffle y ##ANSWERi## en orden\n")
  cod <- pesos_de(lineas); cat("  .Rmd:", if (length(cod)) paste(cod, collapse = ",") else "OK", "\n")
  if (length(cod)) rojo("pesos", paste(cod, collapse = ","))
  ## FASE 2C r1, H1: el XML de salida/ no está atado al md5; los pesos del XML se miden en la sección moodle, sobre uno propio.
}

## --- muestra de variantes (N = 100) ---------------------------------------------------------------------------------
muestra_var <- function(n = 100L, ex = compila(DG_C)) {
  m <- list(); s <- 0L
  while (length(m) < n && s < 5000L) { s <- s + 1L; e <- correr(s, ex); if (isTRUE(e$es_canonica)) next; m[[length(m) + 1L]] <- e }
  m
}

## --- cruce (§P7-G) ----------------------------------------------------------------------------------------------------
## FASE 2C r1, H3: la tasa se mide solo donde la regla aplica (antes, una regla sin aplicar contaba como 25 %), con n >= 20
## (regla #23); se declara si la regla elige lo mismo que una regla ya firmada de la Parte 1, porque entonces no es un canal
## nuevo; se mide también la canónica (20 versiones); y un control sin la V11 debe volver aplicable la regla de números en común.
REGLAS_CRUCE <- list(
  "X-mas-numeros-en-comun" = function(V, o) { k <- vapply(V, function(v) sum(v %in% o$datos), 0L); k == max(k) },
  "X-menos-numeros-en-comun" = function(V, o) { k <- vapply(V, function(v) sum(v %in% o$datos), 0L); k == min(k) },
  "X-suma-cercana-al-total" = function(V, o) { k <- abs(vapply(V, sum, 0) - o$total); k == min(k) },
  "X-suma-lejana-al-total" = function(V, o) { k <- abs(vapply(V, sum, 0) - o$total); k == max(k) },
  "X-centro-cercano-al-centro" = function(V, o) { k <- abs(vapply(V, function(v) v[[4L]], 0) - o$ctr); k == min(k) },
  "X-maximo-cercano-al-mayor-dato" = function(V, o) { k <- abs(vapply(V, max, 0) - max(o$datos)); k == min(k) })
EQUIV_CRUCE <- list(
  "X-suma-cercana-al-total" = list("MAG-suma-max", function(V) { k <- vapply(V, sum, 0); k == max(k) }),
  "X-suma-lejana-al-total" = list("MAG-suma-min", function(V) { k <- vapply(V, sum, 0); k == min(k) }),
  "X-centro-cercano-al-centro" = list("MAG-centro-mayor", function(V) { k <- vapply(V, function(v) v[[4L]], 0); k == max(k) }),
  "X-maximo-cercano-al-mayor-dato" = list("MAG-mayor-numero", function(V) { k <- vapply(V, max, 0); k == max(k) }))
medir_cruce <- function(muestra) {
  t(vapply(names(REGLAS_CRUCE), function(nm) {
    x <- vapply(muestra, function(e) {
      o <- list(datos = as.integer(unlist(e$d2)), total = sum(e$r2), ctr = e$r2[["ctr"]])
      V <- e$regiones_mezcladas; S <- REGLAS_CRUCE[[nm]](V, o); k <- which(e$sol_p1); ap <- any(S) && !all(S)
      eq <- if (!is.null(EQUIV_CRUCE[[nm]])) identical(S, EQUIV_CRUCE[[nm]][[2]](V)) else FALSE
      c(acierto = if (ap) S[k] / sum(S) else NA_real_, aplica = ap, igual = eq)
    }, numeric(3))
    c(n_aplica = sum(x[2, ] == 1), tasa = if (any(x[2, ] == 1)) 100 * mean(x[1, ], na.rm = TRUE) else NA_real_, igual = 100 * mean(x[3, ]))
  }, numeric(3)))
}
sec_cruce <- function() {
  cat("\n[cruce] canales de las Partes 2 a 6 hacia la Parte 1 (§P7-G): 100 variantes y 20 canónicas; tasa solo donde la regla aplica (azar = 25 %)\n")
  ex <- compila(DG_C); mc <- list(); s <- 0L
  while (length(mc) < 20L && s < 5000L) { s <- s + 1L; e <- correr(s, ex); if (isTRUE(e$es_canonica)) mc[[length(mc) + 1L]] <- e }
  indep <- character(0)
  for (est in list(list("variantes", muestra_var()), list("canónicas", mc))) {
    res <- medir_cruce(est[[2]])
    cat(sprintf("  -- %s (n = %d) --\n", est[[1]], length(est[[2]])))
    for (nm in rownames(res)) {
      r <- res[nm, ]; eqn <- if (!is.null(EQUIV_CRUCE[[nm]])) EQUIV_CRUCE[[nm]][[1]] else "-"
      estado <- if (r[["n_aplica"]] < 20) "no aplica o NO CONCLUYENTE (n < 20)" else if (r[["igual"]] == 100) paste("es la regla firmada", eqn) else "independiente"
      cat(sprintf("  %-32s aplica en %3d · tasa %s · elige lo mismo que %-17s en %5.1f %% · %s\n", nm, as.integer(r[["n_aplica"]]),
                  if (is.na(r[["tasa"]])) "   -   " else sprintf("%5.1f %%", r[["tasa"]]), eqn, r[["igual"]], estado))
      if (r[["n_aplica"]] >= 20 && r[["igual"]] < 100) { indep <- c(indep, nm)
        if (r[["tasa"]] - 25 > CORTE) pendiente("CRUCE_SOBRE_8", paste0("vuelve al profesor (C-4): ", nm, " en ", est[[1]])) }
    }
  }
  cat("  canales cruzados independientes y medibles:", if (length(indep)) paste(unique(indep), collapse = ", ") else "ninguno", "\n")
  ## Control positivo: sin la V11 (filtro y sus dos guardas), la regla de números en común tiene que volverse aplicable.
  cm <- DG_C
  for (cb in list(c("ok_resp & ok_datos & ok_copia & ok_empate", "ok_copia & ok_empate"),
                  c("= !any(resp2 %in% impresos_p1),", "= TRUE,"), c("= !any(datos2 %in% diag_p1),", "= TRUE,"))) {
    cm <- mutar(cm, cb[1], cb[2]); if (!is.null(attr(cm, "mal"))) { rojo("cruce", paste("control mal construido:", attr(cm, "mal"))); return(invisible()) } }
  rc <- medir_cruce(muestra_var(ex = compila(cm)))
  cat(sprintf("  [control] sin la V11: X-mas-numeros-en-comun aplica en %d de 100 (tasa %s)\n", as.integer(rc["X-mas-numeros-en-comun", "n_aplica"]),
              if (is.na(rc["X-mas-numeros-en-comun", "tasa"])) "-" else sprintf("%.1f %%", rc["X-mas-numeros-en-comun", "tasa"])))
  if (rc["X-mas-numeros-en-comun", "n_aplica"] < 20) rojo("cruce", "el control sin la V11 no volvió medible la regla de números en común")
}

## --- bateria (C-4) ------------------------------------------------------------------------------------------------
firma_estado <- list(evaluado = FALSE, pendientes = "FIRMA_SIN_EVALUAR")
cargar_funciones_schoice <- function() {
  if (unname(tools::md5sum(VERIF_S)) != MD5_VERIF_S) return(NULL)
  if (!exists("nueva_regla", mode = "function")) source(file.path(SUB, "..", "..", "..", ".claude", "scripts", "bateria_eliminacion.R"))
  V <- new.env(parent = globalenv()); assign("aqui", SUB, envir = V)
  quiero <- c("reglas_p7", "sin_tasas", "parsear_tasas", "tasas_exactas", "MD5_EXACTO", "RUTA_EXACTO", "N_REGLAS", "bateria_evaluable",
              "clasificar_firma", "TOL_FIRMA", "FIRMA_VARIANTES", "FIRMA_CANONICA")
  for (x in parse(VERIF_S, keep.source = FALSE, encoding = "UTF-8")) {
    if (is.call(x) && identical(x[[1]], as.name("<-")) && is.name(x[[2]]) && as.character(x[[2]]) %in% quiero) eval(x, V)
  }
  if (!all(vapply(quiero, exists, TRUE, envir = V, inherits = FALSE))) return(NULL)
  V
}
sec_bateria <- function() {
  cat("\n[bateria] §P7 de la Parte 1 sobre los objetos del CLOZE (C-4)\n")
  V <- cargar_funciones_schoice()
  if (is.null(V)) { pendiente("FIRMA_SIN_EVALUAR", "verificar_espacio.R del SCHOICE con otro md5 o sin las funciones §P7"); return(invisible()) }
  reglas <- V$reglas_p7(); m <- muestra_var()
  r <- evaluar_bateria(reglas, lapply(m, function(e) e$opciones_p1), vapply(m, function(e) which(e$sol_p1), 0L),
                       estimulos = lapply(m, function(e) as.integer(unlist(e$dat)[c("a", "b", "c", "d", "P", "M", "B")])))
  imprimir_bateria(r)
  ## Canónica: los 24 órdenes de sus cuatro opciones, tomadas de una versión canónica del CLOZE.
  ex <- compila(DG_C); s <- 0L; repeat { s <- s + 1L; ec <- correr(s, ex); if (isTRUE(ec$es_canonica) || s > 2000L) break }
  perm <- function(v) if (length(v) == 1L) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perm(v[-i]), function(p) c(v[i], p))))
  base <- ec$opciones_p1; k0 <- which(ec$sol_p1); pp <- perm(1:4)
  rc <- evaluar_bateria(reglas, lapply(pp, function(p) base[p]), vapply(pp, function(p) which(p == k0), 0L),
                        estimulos = rep(list(as.integer(unlist(ec$dat)[c("a", "b", "c", "d", "P", "M", "B")])), 24L))
  cat("\n  --- canónica, 24 órdenes (semilla", s, ") ---\n"); imprimir_bateria(rc)
  med_var <- V$tasas_exactas(); med_can <- stats::setNames(100 * rc$detalle$tasa, rc$detalle$regla)
  nom <- vapply(reglas, function(x) x$nombre, ""); ev <- !length(V$bateria_evaluable(nom, med_var, med_can))
  fv <- c(V$FIRMA_VARIANTES, `LEX-con-repetidos` = 33.418); fc <- V$FIRMA_CANONICA
  tc <- 100 * rc$techo_nulo; cc <- 100 * rc$corte_canal
  pv <- if (ev) V$clasificar_firma(med_var, fv, TECHO_EXACTO_VARIANTES, CORTE) else "FIRMA_SIN_EVALUAR"
  pc <- V$clasificar_firma(med_can, fc, tc, cc)
  inf <- function(med, firma, techo, rot) {
    cat(sprintf("\n  FIRMA C-4, %s (techo %.2f %%): %d reglas firmadas\n", rot, techo, length(firma)))
    for (n in names(firma)) cat(sprintf("    %-36s firmada %7.3f %%  medida %7.3f %%  exceso %+.2f pp\n", n, firma[[n]], unname(med[n]), unname(med[n]) - techo))
    cat(sprintf("    reglas sobre +8 pp fuera de la lista: %d\n", length(setdiff(names(med)[med - techo > CORTE], names(firma)))))
  }
  if (ev) inf(med_var, fv, TECHO_EXACTO_VARIANTES, "variantes, tasas exactas de exacto.py") else cat("  tasas exactas no disponibles:", attr(med_var, "razon"), "\n")
  inf(med_can, fc, tc, "canónica, 24 órdenes del CLOZE")
  firma_estado <<- list(evaluado = ev, pendientes = unique(c(pv, pc)))
  if (length(firma_estado$pendientes)) cat("  firma:", paste(firma_estado$pendientes, collapse = ", "), "\n") else cat("  firma: dentro de lo firmado\n")
}

## --- mutantes ---------------------------------------------------------------------------------------------------------
MUTANTES <- list(
  list(id = "M1_e1_sorteo_de_mas", sonda = "E1", donde = "bloque", cambios = list(c("ops <- ops[sample.int(4L)]", "invisible(runif(1L)); ops <- ops[sample.int(4L)]"))),
  list(id = "M2_v11_resp_sin_filtro_ni_guarda", sonda = "V11_RESP", donde = "dg",
       cambios = list(c("ok_resp & ok_datos & ok_copia & ok_empate", "ok_datos & ok_copia & ok_empate"),
                      c("= !any(resp2 %in% impresos_p1),", "= TRUE,")),
       materia = function(e) any(e$resp2 %in% e$impresos_p1)),
  list(id = "M3_v11_resp_sin_filtro", sonda = "GUARDA_RMD: V11 por valor: ninguna respuesta", donde = "dg",
       cambios = list(c("ok_resp & ok_datos & ok_copia & ok_empate", "ok_datos & ok_copia & ok_empate")),
       materia = function(e) any(e$resp2 %in% e$impresos_p1)),
  list(id = "M4_v11_datos_sin_filtro_ni_guarda", sonda = "V11_DATOS", donde = "dg",
       cambios = list(c("ok_resp & ok_datos & ok_copia & ok_empate", "ok_resp & ok_copia & ok_empate"),
                      c("= !any(datos2 %in% diag_p1),", "= TRUE,")),
       materia = function(e) any(e$datos2 %in% e$diag_p1)),
  list(id = "M5_p6_sin_ajustar_el_orden", sonda = "P6_VERDAD", donde = "dg",
       cambios = list(c("if (verdad != v6[i]) { t0 <- x; x <- y; y <- t0 }", "NULL"),
                      c("= identical(verdad6, sol_p6),", "= TRUE,")),
       materia = function(e) !identical(e$verdad6, e$sol_p6)),
  list(id = "M6_respuestas_p3_p4_cruzadas", sonda = "RESPUESTA", donde = "dg",
       cambios = list(c("exsolution_str <- paste(mchoice2string(sol_p1), exsol_p2, exsol_p3, exsol_p4,",
                        "exsolution_str <- paste(mchoice2string(sol_p1), exsol_p2, exsol_p4, exsol_p3,")),
       materia = function(e) strsplit(e$exsolution_str, "|", fixed = TRUE)[[1]][3] != as.character(e$exsol_p3)),
  list(id = "M9_p6_unanime", sonda = "P6_UNANIME", donde = "dg",
       cambios = list(c("COMB6[rowSums(COMB6) %in% 1:3, , drop = FALSE]", "COMB6[rowSums(COMB6) %in% 0:4, , drop = FALSE]"),
                      c("= any(sol_p6) && any(!sol_p6),", "= TRUE,")),
       materia = function(e) all(e$sol_p6) || !any(e$sol_p6)),
  list(id = "M10_respuesta_impresa", sonda = "RESP_IMPRESA", donde = "dg",
       cambios = list(c("ok_resp & ok_datos & ok_copia & ok_empate", "ok_resp & ok_datos & ok_empate"),
                      c("= !any(resp2 %in% datos2),", "= TRUE,")),
       materia = function(e) any(e$resp2 %in% e$datos2)),
  list(id = "M11_p6_empate", sonda = "P6_EMPATE", donde = "dg",
       cambios = list(c("ok_resp & ok_datos & ok_copia & ok_empate", "ok_resp & ok_datos & ok_copia"),
                      c("= all(vapply(af6, function(a) r2[[a$x]] != r2[[a$y]], TRUE)),", "= TRUE,"),
                      c("= identical(verdad6, sol_p6),", "= TRUE,")),
       materia = function(e) any(vapply(e$af6, function(a) e$r2[[a$x]] == e$r2[[a$y]], TRUE))),
  list(id = "M7_rotulos_cruzados", sonda = "P1_MARCA", donde = "enunciado",
       cambios = list(c("![Diagrama de Venn](\", archivos[i], \")", "![Diagrama de Venn](\", archivos[5L - i], \")"))),
  list(id = "M8_pesos", sonda = "PESOS", donde = "meta", cambios = list(c("expoints: 1|1|1|1|1|1", "expoints: 2|1|1|1|1|1"))),
  ## --- Tanda 2 (2026-11-09): una prueba de detección propia por componente del oráculo (obligación 7). Cada mutante
  ## quita la guarda del .Rmd que vigila lo mismo, salvo los «_guarda», que prueban que esa guarda también lo caza.
  list(id = "M12_v11_contenido", sonda = "V11_CONTENIDO", donde = "dg",
       cambios = list(c("u <- vapply(ctx2$conjuntos, function(k) k$texto, \"\")", "u <- c(t1, t2, t3)"),
                      c("!any(u %in% c(t1, t2, t3)) && !identical(ctx2$tipo, ctx$tipo),", "TRUE,")),
       materia = function(e) any(e$u %in% c(e$t1, e$t2, e$t3))),
  list(id = "M12b_v11_contenido_guarda", sonda = "GUARDA_RMD: V11 por contenido", donde = "dg",
       cambios = list(c("u <- vapply(ctx2$conjuntos, function(k) k$texto, \"\")", "u <- c(t1, t2, t3)")),
       materia = function(e) any(e$u %in% c(e$t1, e$t2, e$t3))),
  ## PERO_NO tiene dos componentes: las preguntas de las Partes 2 a 6 y las afirmaciones de la Parte 6.
  list(id = "M13_pero_no_en_pregunta", sonda = "PERO_NO", donde = "dg",
       cambios = list(c("texto_p5 <- paste0(cuantos2, \" \", grupo2, \" \", verbo2,", "texto_p5 <- paste0(cuantos2, \" \", grupo2, \", pero no los repetidos, \", verbo2,"),
                      c("!any(grepl(\"pero no|", "TRUE || !any(grepl(\"pero no|")),
       materia = function(e) grepl("pero no", e$texto_p5, fixed = TRUE)),
  ## En la Parte 6 la frase solo cabe sin romper la lectura de las zonas tras «los/las tres …» (zona_de lee el prefijo).
  list(id = "M14_pero_no_en_parte6", sonda = "PERO_NO", donde = "dg",
       cambios = list(c("ZONA2[[y]], \".\"))", "ZONA2[[y]], if (y == \"ctr\") \", pero no menos\" else \"\", \".\"))"),
                      c("!any(grepl(\"pero no|", "TRUE || !any(grepl(\"pero no|")),
       materia = function(e) any(grepl("pero no", e$opciones_p6, fixed = TRUE))),
  list(id = "M15_p6_cifras", sonda = "P6_CIFRAS", donde = "dg",
       cambios = list(c("texto = paste0(w, \" \", grupo2, \" \", verbo2,", "texto = paste0(w, \" \", grupo2, \"1 \", verbo2,"),
                      c("= !any(grepl(\"[0-9]\", opciones_p6)),", "= TRUE,")),
       materia = function(e) any(grepl("[0-9]", e$opciones_p6))),
  list(id = "M16_p6_par_repetido", sonda = "P6_COMPLEMENTARIO", donde = "dg",
       cambios = list(c("pares6 <- PARES6[sample.int(length(PARES6), 4L)]", "pares6 <- PARES6[sample.int(length(PARES6), 4L)[c(1L, 1L, 3L, 4L)]]"),
                      c("!anyDuplicated(vapply(pares6,", "TRUE || !anyDuplicated(vapply(pares6,"),
                      c("= !anyDuplicated(opciones_p6))", "= TRUE)")),
       materia = function(e) anyDuplicated(vapply(e$pares6, function(p) paste(sort(p), collapse = "-"), "")) > 0L),
  ## P6_EMPATE aislado: M11 lo prueba junto con P6_VERDAD, porque un empate con marca «verdadera» rompe también la marca.
  ## Aquí el empate cae en una afirmación con marca «falsa» (par 2 = p13/p23, v6[2] = FALSE), y solo se mueve P6_EMPATE.
  list(id = "M11b_p6_empate_aislado", sonda = "P6_EMPATE", donde = "dg",
       cambios = list(c("p13 != p23 & ctr != p12)", "p13 == p23 & ctr != p12)"),
                      c("pares6 <- PARES6[sample.int(length(PARES6), 4L)]", "pares6 <- PARES6[c(1L, 4L, 2L, 3L)]"),
                      c("v6 <- unname(COMB6[sample.int(nrow(COMB6), 1L), ])", "v6 <- c(TRUE, FALSE, unname(COMB6[sample.int(nrow(COMB6), 1L), ])[3:4])"),
                      c("= all(vapply(af6, function(a) r2[[a$x]] != r2[[a$y]], TRUE)),", "= TRUE,")),
       materia = function(e) e$r2[["p13"]] == e$r2[["p23"]] && !e$sol_p6[2L]),
  ## P1_ROTULOS tiene dos componentes: el conjunto de rótulos y que cada uno vaya pegado a su imagen.
  list(id = "M17_p1_rotulo_repetido", sonda = "P1_ROTULOS", donde = "enunciado",
       cambios = list(c("textos_p1[i]", "textos_p1[min(i, 3L)]")),
       materia = function(e, txt) lengths(regmatches(txt, gregexpr("**Diagrama III**", txt, fixed = TRUE))) == 2L),
  list(id = "M18_p1_falta_una_figura", sonda = "P1_ROTULOS", donde = "enunciado",
       cambios = list(c("img_p1(3L), \" \", img_p1(4L)", "img_p1(3L)")),
       materia = function(e, txt) !grepl("**Diagrama IV**", txt, fixed = TRUE)),
  ## Códigos de lectura del oráculo: falla cerrado si no puede leer el texto.
  list(id = "M19_vinetas_ilegibles", sonda = "VINETAS", donde = "dg",
       cambios = list(c("\", pero no \", u[3], \".\")", "\", sin \", u[3], \".\")")),
       materia = function(e) !grepl("pero no", e$vineta2_1, fixed = TRUE)),
  list(id = "M20_pregunta_ilegible", sonda = "PREGUNTAS", donde = "dg",
       cambios = list(c("verbo2, \" \", los_tres2, \"?\")", "verbo2, \" \", los_tres2, \" a la vez?\")")),
       materia = function(e) grepl("a la vez?", e$texto_p2, fixed = TRUE)),
  list(id = "M21_encabezado_ilegible", sonda = "ESTRUCTURA", donde = "parte3",
       cambios = list(c("\"**Parte 3.** \"", "\"**Parte 3** \"")),
       materia = function(e, txt) !grepl("**Parte 3.**", txt, fixed = TRUE)),
  list(id = "M22_p6_ilegible", sonda = "P6_FORMA", donde = "dg",
       cambios = list(c("ZONA2[[x]], \" que \", ZONA2[[y]]", "ZONA2[[x]], \" frente a \", ZONA2[[y]]")),
       materia = function(e) !any(grepl(" que ", e$opciones_p6, fixed = TRUE))),
  ## FASE 2C r1, H5: vocabulario de error, en el oráculo (sin la guarda) y en la guarda del .Rmd; «erróneo» prueba err[oó]ne.
  list(id = "M23_vocabulario_de_error", sonda = "VOCAB_ERROR", donde = "dg",
       cambios = list(c("texto_p5 <- paste0(cuantos2, \" \", grupo2, \" \", verbo2,", "texto_p5 <- paste0(cuantos2, \" \", grupo2, \" sin error \", verbo2,"),
                      c("!any(grepl(\"pero no|", "TRUE || !any(grepl(\"pero no|")),
       materia = function(e) grepl("sin error", e$texto_p5, fixed = TRUE)),
  list(id = "M23b_vocabulario_de_error_guarda", sonda = "GUARDA_RMD: Partes 2 a 6: sin la frase", donde = "dg",
       cambios = list(c("texto_p5 <- paste0(cuantos2, \" \", grupo2, \" \", verbo2,", "texto_p5 <- paste0(cuantos2, \" \", grupo2, \" sin error \", verbo2,")),
       materia = function(e) grepl("sin error", e$texto_p5, fixed = TRUE)),
  list(id = "M23c_erroneo_guarda", sonda = "GUARDA_RMD: Partes 2 a 6: sin la frase", donde = "dg",
       cambios = list(c("texto_p5 <- paste0(cuantos2, \" \", grupo2, \" \", verbo2,", "texto_p5 <- paste0(cuantos2, \" \", grupo2, \" sin cálculo erróneo \", verbo2,")),
       materia = function(e) grepl("erróneo", e$texto_p5, fixed = TRUE)),
  ## FASE 2C r1, H4: sin el arreglo, la guarda de concordancia del .Rmd cae (rama Parte 4 = s3 con grupo femenino).
  list(id = "M24_concordancia_restados", sonda = "GUARDA_RMD: Concordancia", donde = "dg",
       cambios = list(c("if (fem2) \"restadas\" else \"restados\"", "\"restados\"")),
       materia = function(e) isTRUE(e$fem2) && identical(e$q4, "s3")))
veredicto <- function(cod, sonda) {
  if (!length(cod)) return("no_detectado")
  propio <- startsWith(cod, sonda)
  if (all(propio)) "cazado_por_su_sonda" else if (any(propio)) "cazado_por_su_sonda_y_otra" else "cazado_por_otra"
}
sec_mutantes <- function() {
  cat("\n[mutantes] contrato INC-MUTANTE-SONDA (tope 600 semillas)\n")
  res <- character(0)
  ex0 <- compila(DG_C)
  for (M in MUTANTES) {
    base <- if (M$donde %in% NOMBRES_TEXTO) chunk_de(lineas, M$donde) else switch(M$donde, bloque = BLK_C, dg = DG_C, meta = lineas)
    mut <- base; mal <- NULL
    for (cb in M$cambios) { mut <- mutar(mut, cb[1], cb[2]); if (!is.null(attr(mut, "mal"))) { mal <- attr(mut, "mal"); break } }
    if (!is.null(mal)) { v <- "mal_construido"; cat(sprintf("  %-34s %s (%s)\n", M$id, v, mal)); res <- c(res, v); next }
    cod <- character(0); sem <- NA; mat_ok <- TRUE; base_ok <- TRUE
    if (M$donde == "bloque") {
      rc <- e1_correr(mut, 10L, 2L); if (length(rc$fallos)) { cod <- "E1"; sem <- sub("^semilla ([0-9]+):.*$", "\\1", rc$fallos[1]) }
    } else if (M$donde == "meta") {
      cod <- pesos_de(mut); sem <- "-"; mat_ok <- any(mut == "expoints: 2|1|1|1|1|1"); base_ok <- !length(pesos_de(lineas))
    } else {
      ex <- if (M$donde == "dg") compila(mut) else ex0
      CH <- CH_TEXTO; if (M$donde %in% NOMBRES_TEXTO) CH[[match(M$donde, NOMBRES_TEXTO)]] <- compila(mut)
      for (s in 1:600) { j <- juzgar(s, ex, CH); if (length(j$cod)) { cod <- j$cod; sem <- s
        mat_ok <- if (is.null(M$materia)) TRUE else
          isTRUE(if (length(formals(M$materia)) == 2L) M$materia(j$e, j$txt) else M$materia(j$e))
        ## Obligación 8: la misma semilla sin mutar no da ningún código; si lo da, la muerte no cuenta.
        base_ok <- !length(juzgar(s, ex0, CH_TEXTO)$cod); break } }
    }
    v <- if (!mat_ok || !base_ok) "mal_construido" else veredicto(cod, M$sonda)
    if (!base_ok) cod <- c(cod, "(la línea base sin mutar ya da códigos en esa semilla)")
    cat(sprintf("  %-34s sonda esperada %-22s semilla %-4s %s · %s\n", M$id, substr(M$sonda, 1, 22), sem, v,
                if (length(cod)) substr(paste(cod, collapse = ","), 1, 90) else "-"))
    res <- c(res, v)
  }
  ## Firma: funciones puras (como F1-F11 del SCHOICE).
  V <- cargar_funciones_schoice()
  if (!is.null(V)) {
    fv <- c(V$FIRMA_VARIANTES, `LEX-con-repetidos` = 33.418)
    f1 <- V$clasificar_firma(c(fv, `DIV-suma-par` = 40), fv, TECHO_EXACTO_VARIANTES, CORTE)
    f2 <- V$clasificar_firma(replace(fv, "LEX-con-repetidos", 34), fv, TECHO_EXACTO_VARIANTES, CORTE)
    f3 <- V$clasificar_firma(fv[names(fv) != "LEX-con-repetidos"], fv, TECHO_EXACTO_VARIANTES, CORTE)
    f4 <- decidir_exit(0L, character(0), "todo", SECCIONES, md5_ok = FALSE)
    for (t in list(list("F1_regla_fuera_de_la_lista", "FIRMA_FUERA_DE_LISTA" %in% f1), list("F2_sobre_su_cifra", "FIRMA_SOBRE_CIFRA" %in% f2),
                   list("F3_firmada_sin_medir", "FIRMA_NO_REPRODUCE" %in% f3), list("F4_md5_distinto_da_3", f4 == 3L))) {
      cat(sprintf("  %-34s %s\n", t[[1]], if (isTRUE(t[[2]])) "cazado_por_su_sonda" else "no_detectado"))
      res <- c(res, if (isTRUE(t[[2]])) "cazado_por_su_sonda" else "no_detectado")
    }
  } else res <- c(res, "mal_construido")
  if (!all(res %in% c("cazado_por_su_sonda", "cazado_por_su_sonda_y_otra"))) rojo("mutantes", "algún mutante no murió por su sonda")
}

## --- v6 (Incidente Q) sobre el HTML real ------------------------------------------------------------------------------
## exams2html permuta con la MISMA mezcla las opciones de cada gap y su entrada de la lista de la Solution, pero no la
## prosa. En cada versión se emparejan POR CONTENIDO, nunca por posición:
##   gap 6: opción mostrada j  <->  entrada j de la lista («Verdadera./Falsa.» + justificación)  <->  la misma opción en la
##          prosa, dentro del grupo «verdadera(s)/falsa(s)» y con la misma justificación;
##   gap 1: opción mostrada j  <->  entrada j de la lista («Diagrama N: Correcta/Incorrecta.»)  <->  el rótulo que la prosa da
##          por correcto y los tres que analiza como incorrectos.
## Códigos: V6_ESTRUCTURA (no se pudo leer; falla cerrado), V6_P1, V6_P6.
des_html <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  for (p in list(c("&amp;", "&"), c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"))) x <- gsub(p[1], p[2], x, fixed = TRUE)
  trimws(gsub("\\s+", " ", x))
}
captura <- function(h, re) { m <- regmatches(h, regexec(re, h, perl = TRUE))[[1]]; if (length(m) < 2L) NA_character_ else m[-1] }
v6_version <- function(h) {
  g1 <- captura(h, "<p>([^<]*)</p>\\s*<p><strong>Las Partes 2 a 6 se refieren a otra encuesta\\.</strong>")
  g6 <- captura(h, "<p><strong>Parte 6\\.</strong>[^<]*</p>\\s*<p>([^<]*)</p>")
  ol <- captura(h, "(?s).*<ol type=\"a\">(.*?)</ol>")
  if (anyNA(c(g1, g6, ol))) return(list(cod = "V6_ESTRUCTURA"))
  sl <- vapply(regmatches(ol, gregexpr("(?s)<li>.*?</li>", ol, perl = TRUE))[[1]], des_html, "", USE.NAMES = FALSE)
  o1 <- strsplit(des_html(g1), " / ", fixed = TRUE)[[1]]; o6 <- strsplit(des_html(g6), " / ", fixed = TRUE)[[1]]
  if (length(sl) != 6L) return(list(cod = "V6_ESTRUCTURA"))
  s1 <- strsplit(sl[1], " / ", fixed = TRUE)[[1]]; s6 <- strsplit(sl[6], " / ", fixed = TRUE)[[1]]
  ## Prosa de la Parte 1: rótulo correcto y rótulos analizados como incorrectos.
  corr <- captura(h, "La respuesta correcta es el (Diagrama (?:I|II|III|IV)):")
  inc <- sub("^<p><em>(Diagrama [IV]+)\\.</em>.*$", "\\1", regmatches(h, gregexpr("<p><em>Diagrama [IV]+\\.</em>", h))[[1]])
  ## Prosa de la Parte 6: grupos por veredicto, cada ítem «opción» + justificación.
  gr <- regmatches(h, gregexpr("(?s)<p><strong>Parte 6 · Afirmaci(?:ón|ones) (?:verdaderas?|falsas?)</strong></p>\\s*<ul>.*?</ul>", h, perl = TRUE))[[1]]
  pr <- do.call(rbind, lapply(gr, function(b) {
    ver <- grepl("verdadera", sub("(?s)</strong>.*$", "", b, perl = TRUE), fixed = TRUE)
    li <- vapply(regmatches(b, gregexpr("(?s)<li>.*?</li>", b, perl = TRUE))[[1]], des_html, "", USE.NAMES = FALSE)
    m <- regmatches(li, regexec("^«(.*)» (.*)$", li))
    data.frame(opc = vapply(m, function(x) if (length(x) == 3L) x[2] else NA_character_, ""),
               just = vapply(m, function(x) if (length(x) == 3L) x[3] else NA_character_, ""), ver = rep(ver, length(li)))
  }))
  if (length(o1) != 4L || length(o6) != 4L || length(s1) != 4L || length(s6) != 4L || is.na(corr) || is.null(pr) ||
      nrow(pr) != 4L || anyNA(pr$opc) || anyDuplicated(o6) || !setequal(pr$opc, o6) || anyDuplicated(pr$opc))
    return(list(cod = "V6_ESTRUCTURA"))
  cod <- character(0)
  v1 <- sub("^Diagrama [IV]+: (Correcta|Incorrecta)\\. .*$", "\\1", s1)
  if (!all(startsWith(s1, paste0(o1, ": "))) || !all(v1 %in% c("Correcta", "Incorrecta")) || sum(v1 == "Correcta") != 1L ||
      o1[v1 == "Correcta"] != corr || length(inc) != 3L || !setequal(inc, setdiff(o1, corr))) cod <- c(cod, "V6_P1")
  k <- match(o6, pr$opc); ver_l <- sub("^(Verdadera|Falsa)\\. .*$", "\\1", s6); just_l <- sub("^(Verdadera|Falsa)\\. ", "", s6)
  if (!all(ver_l %in% c("Verdadera", "Falsa")) || !identical(ver_l == "Verdadera", pr$ver[k]) || !identical(just_l, pr$just[k]))
    cod <- c(cod, "V6_P6")
  ## Dientes de la sonda: versiones en que una prosa en el orden de la prosa (sin agrupar) no seguiría el orden mostrado.
  list(cod = cod, orden_distinto = !identical(o6, pr$opc), p1_distinto = !identical(o1, paste("Diagrama", c("I", "II", "III", "IV"))))
}
v6_render <- function(rmd, semillas, dir) {
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  ok <- tryCatch({ suppressMessages(exams2html(rmd, n = length(semillas), seed = semillas, dir = dir, name = "v", mathjax = TRUE)); TRUE },
                 error = function(x) conditionMessage(x))
  if (!isTRUE(ok)) return(structure(character(0), error = ok))
  vapply(seq_along(semillas), function(i) paste(readLines(file.path(dir, paste0("v", i, ".html")), encoding = "UTF-8", warn = FALSE),
                                               collapse = "\n"), "")
}
V6_MUTANTES <- list(
  list(id = "V6M1_grupos_cruzados", sonda = "V6_P6", cambios = list(c("if (g) (if (n_g", "if (!g) (if (n_g"))),
  list(id = "V6M2_lista_en_orden_interno", sonda = "V6_P6",
       cambios = list(c("for (g in c(TRUE, FALSE)) {", "for (g in TRUE) {"), c("for (i in which(sol_p6 == g))", "for (i in seq_along(opciones_p6))"))),
  list(id = "V6M3_p1_rotulo_correcto_otro", sonda = "V6_P1", cambios = list(c("textos_p1[k1], \": \", describe", "textos_p1[k1 %% 4L + 1L], \": \", describe"))),
  list(id = "V6M4_prosa_ilegible", sonda = "V6_ESTRUCTURA", cambios = list(c("encabezado(paste0(\"Parte 6 · \",", "encabezado(paste0(\"Parte seis · \","))))
sec_v6 <- function() {
  N <- 100L
  cat(sprintf("\n[v6] Incidente Q sobre el HTML real: exams2html, N = %d (semillas 1..%d), familia B de la regla #23\n", N, N))
  t0 <- proc.time()[["elapsed"]]
  hs <- v6_render(RMD, seq_len(N), file.path(tempdir(), "v6_real"))
  if (!length(hs)) { rojo("v6", paste("exams2html falló:", attr(hs, "error"))); return(invisible()) }
  r <- lapply(hs, v6_version); cods <- lapply(r, `[[`, "cod")
  malos <- which(lengths(cods) > 0L)
  od <- vapply(r, function(x) isTRUE(x$orden_distinto), TRUE); p1d <- vapply(r, function(x) isTRUE(x$p1_distinto), TRUE)
  cat(sprintf("  %d versiones en %.0f s · con algún código: %d · gap 6 mostrado en otro orden que la prosa: %d de %d · gap 1 barajado: %d de %d\n",
              length(hs), proc.time()[["elapsed"]] - t0, length(malos), sum(od), length(hs), sum(p1d), length(hs)))
  if (length(malos)) rojo("v6", paste(sprintf("semilla %d: %s", head(malos, 3), vapply(cods[head(malos, 3)], paste, "", collapse = ",")), collapse = "; "))
  if (!any(od)) rojo("v6", "la mezcla nunca cambió el orden del gap 6: la sonda no tendría dientes")
  ## Controles: copias mutadas en tempdir(), hasta la primera muerte (tope 3 semillas); línea base = las mismas semillas reales.
  for (M in V6_MUTANTES) {
    mut <- lineas; mal <- NULL
    for (cb in M$cambios) { mut <- mutar(mut, cb[1], cb[2]); if (!is.null(attr(mut, "mal"))) { mal <- attr(mut, "mal"); break } }
    if (!is.null(mal)) { cat(sprintf("  %-34s mal_construido (%s)\n", M$id, mal)); rojo("v6", paste(M$id, "mal construido")); next }
    d <- file.path(tempdir(), paste0("v6_", M$id)); dir.create(d, showWarnings = FALSE)
    f <- file.path(d, basename(RMD)); writeLines(mut, f, useBytes = TRUE)
    hm <- v6_render(f, 1:3, file.path(d, "html"))
    cm <- if (length(hm)) lapply(hm, function(x) v6_version(x)$cod) else list(paste("RENDER:", attr(hm, "error")))
    i <- which(lengths(cm) > 0L)[1]; cod <- if (is.na(i)) character(0) else cm[[i]]
    base_ok <- is.na(i) || !length(cods[[i]])
    v <- if (!base_ok) "mal_construido" else veredicto(cod, M$sonda)
    cat(sprintf("  %-34s sonda esperada %-14s semilla %-3s %s · %s\n", M$id, M$sonda, if (is.na(i)) "-" else i, v,
                if (length(cod)) paste(cod, collapse = ",") else "-"))
    if (!v %in% c("cazado_por_su_sonda", "cazado_por_su_sonda_y_otra")) rojo("v6", paste(M$id, v))
  }
}

## --- moodle (FASE 2C r1, H1; regla #21 F4; I-4.8) --------------------------------------------------------------------
## exams2moodle PROPIO en tempdir() desde el .Rmd vigente (familia B de la regla #23, N = 100, semillas 1..100): el XML de
## salida/ no está atado al md5. En cada pregunta se recalcula la verdad desde las viñetas del PROPIO XML con leer_vinetas()
## y zona_de() (la semántica del oráculo, no FORMULAS del .Rmd) y se compara con lo que puntúa Moodle (el «=» del gap 1, los
## NUMERICAL con tolerancia 0, el «=» de cada afirmación del MULTIRESPONSE) y con lo que lee el estudiante: el rótulo de «La
## respuesta correcta es el …», el único «Diagrama N: Correcta.», la imagen de la Solution = la que va bajo ese rótulo, y las
## tres cifras de esa frase = las regiones de la clave calculadas desde las viñetas. El contenido de esa imagen se contrasta
## con el entorno de su semilla cuando el fig_id coincide (la marca rótulo -> archivo -> regiones la prueba P1_MARCA).
## Códigos: M_ESTRUCTURA (no se pudo leer; falla cerrado), M_PESOS, M_IMAGEN_EN_GAP, M_P1, M_P1_IMAGEN, M_NUM, M_P6.
html_a_lineas <- function(h) {
  h <- gsub("<li>", "\n- ", h, fixed = TRUE)
  h <- gsub("<strong>(Parte [0-9]\\.|Las Partes 2 a 6 se refieren a otra encuesta\\.)</strong>", "\n**\\1**", h)
  h <- gsub("<(p|br|ul|/ul|/li|/p)( [^>]*)?/?>", "\n", h); h <- gsub("<[^>]+>", "", h)
  L <- trimws(strsplit(h, "\n", fixed = TRUE)[[1]])
  for (p in list(c("&amp;", "&"), c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"))) L <- gsub(p[1], p[2], L, fixed = TRUE)
  L[nzchar(L)]
}
moodle_pregunta <- function(q) {
  qt <- captura(q, "(?s)<questiontext format=\"html\">\\s*<text><!\\[CDATA\\[(.*?)\\]\\]></text>")
  gf <- captura(q, "(?s)<generalfeedback format=\"html\">\\s*<text><!\\[CDATA\\[(.*?)\\]\\]></text>")
  if (anyNA(c(qt, gf))) return(list(cod = "M_ESTRUCTURA"))
  gaps <- regmatches(qt, gregexpr("\\{[0-9]+:(MULTICHOICE_V|MULTICHOICE|MULTIRESPONSE|NUMERICAL):[^}]*\\}", qt))[[1]]
  L <- html_a_lineas(gsub("\\{[0-9]+:[A-Z_]+:[^}]*\\}", "", qt))
  i1 <- grep("^\\*\\*Parte 1\\.\\*\\*", L); io <- grep("^\\*\\*Las Partes 2 a 6 se refieren a otra encuesta\\.\\*\\*", L)
  ip <- vapply(2:6, function(k) { i <- grep(paste0("^\\*\\*Parte ", k, "\\.\\*\\*"), L); if (length(i) == 1L) i else NA_integer_ }, 0L)
  if (length(i1) != 1L || length(io) != 1L || anyNA(ip) || length(gaps) != 6L) return(list(cod = "M_ESTRUCTURA"))
  o1 <- leer_vinetas(grep("^- ", L[seq_len(i1 - 1L)], value = TRUE)); o2 <- leer_vinetas(grep("^- ", L[(io + 1L):(ip[1] - 1L)], value = TRUE))
  if (is.null(o1) || is.null(o2)) return(list(cod = "M_ESTRUCTURA"))
  cod <- character(0)
  tipos <- sub("^\\{[0-9]+:([A-Z_]+):.*$", "\\1", gaps); cuerpo <- function(g) sub("^\\{[0-9]+:[A-Z_]+:(.*)\\}$", "\\1", g)
  if (!identical(tipos, c("MULTICHOICE_V", rep("NUMERICAL", 4L), "MULTIRESPONSE"))) return(list(cod = "M_ESTRUCTURA"))
  if (!identical(sub("^\\{([0-9]+):.*$", "\\1", gaps), rep("1", 6L))) cod <- c(cod, "M_PESOS")
  if (any(grepl("<img|@@PLUGINFILE@@", gaps))) cod <- c(cod, "M_IMAGEN_EN_GAP")
  ## Gap 1 y Parte 1 de la Solution.
  op1 <- strsplit(cuerpo(gaps[1]), "~", fixed = TRUE)[[1]]; marc1 <- sub("^=", "", op1[startsWith(op1, "=")])
  corr <- captura(gf, "La respuesta correcta es el (Diagrama (?:I|II|III|IV)):")
  fbc <- regmatches(gf, gregexpr("Diagrama (?:I|II|III|IV): Correcta\\.", gf, perl = TRUE))[[1]]
  img_sol <- captura(gf, "<img src=\"@@PLUGINFILE@@/(diagrama_[a-d]_[0-9a-f]+\\.png)\"")
  img_rot <- if (is.na(corr)) NA_character_ else
    captura(qt, paste0("<strong>", corr, "</strong><br />\\s*<img src=\"@@PLUGINFILE@@/(diagrama_[a-d]_[0-9a-f]+\\.png)\""))
  desc <- suppressWarnings(as.integer(captura(gf, paste0("La respuesta correcta es el Diagrama [IV]+: el diagrama con ([0-9]+) en el centro, ",
                                                        "([0-9]+) en la zona de .*? y ([0-9]+) en la zona de solo "))))
  if (length(op1) != 4L || length(marc1) != 1L || is.na(corr) || marc1 != corr || !identical(fbc, paste0(corr, ": Correcta.")) ||
      is.na(img_sol) || is.na(img_rot) || img_sol != img_rot || !identical(desc, unname(as.integer(o1$reg[c("ctr", "p12", "s1")]))))
    cod <- c(cod, "M_P1")
  ## Partes 2 a 5: NUMERICAL con tolerancia 0 contra la verdad desde las viñetas del XML.
  pq <- function(k) sub("^\\*\\*Parte [0-9]\\.\\*\\* ", "", L[ip[k - 1L]])
  z3 <- zona_de(sub("^.* (solo \\S+ y \\S+)\\?$", "\\1", pq(3)), o2$S); z4 <- zona_de(sub("^.* (solo \\S+)\\?$", "\\1", pq(4)), o2$S)
  if (is.na(z3) || is.na(z4)) return(list(cod = c(cod, "M_ESTRUCTURA")))
  verdad <- as.integer(c(o2$reg[["ctr"]], o2$reg[[z3]], o2$reg[[z4]], sum(o2$reg)))
  if (!identical(vapply(gaps[2:5], cuerpo, "", USE.NAMES = FALSE), paste0("=", verdad, ":0"))) cod <- c(cod, "M_NUM")
  ## Parte 6: el «=» marca exactamente las afirmaciones verdaderas.
  op6 <- strsplit(cuerpo(gaps[6]), "~", fixed = TRUE)[[1]]; marc6 <- startsWith(op6, "="); txt6 <- sub("^(=|%-?[0-9.]+%)", "", op6)
  af <- regmatches(txt6, regexec("^(Más|Menos) \\S+ \\S+ (.+) que (.+)\\.$", txt6))
  if (length(op6) != 4L || any(lengths(af) != 4L)) return(list(cod = c(cod, "M_ESTRUCTURA")))
  zx <- vapply(af, function(m) zona_de(m[3], o2$S), ""); zy <- vapply(af, function(m) zona_de(m[4], o2$S), "")
  if (anyNA(zx) || anyNA(zy)) return(list(cod = c(cod, "M_ESTRUCTURA")))
  v6 <- ifelse(vapply(af, `[`, "", 2L) == "Más", o2$reg[zx] > o2$reg[zy], o2$reg[zx] < o2$reg[zy])
  if (!identical(unname(v6), marc6)) cod <- c(cod, "M_P6")
  list(cod = cod, img = img_rot, reg1 = o1$reg)
}
moodle_render <- function(rmd, semillas, dir) {
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  ok <- tryCatch({ suppressMessages(exams2moodle(rmd, n = length(semillas), seed = semillas, dir = dir, name = "m",
                                                 cloze = list(cloze_schoice_display = "MULTICHOICE_V"))); TRUE },
                 error = function(x) conditionMessage(x))
  if (!isTRUE(ok)) return(structure(character(0), error = ok))
  x <- paste(readLines(file.path(dir, "m.xml"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  strsplit(x, "<question type=\"cloze\">", fixed = TRUE)[[1]][-1]
}
## Entorno de la semilla s tal como lo deja exams (puede consumir sorteos antes del chunk): se prueba con 0 a 3 sorteos
## previos y vale el que reproduce el fig_id del XML.
entorno_moodle <- function(s, fid, ex) {
  for (k in 0:3) { e <- new.env(parent = globalenv()); set.seed(s); if (k) invisible(runif(k)); eval(ex, e)
    if (identical(e$fig_id, fid)) return(list(e = e, k = k)) }
  NULL
}
MOODLE_MUTANTES <- list(
  list(id = "MM1_numericos_p3_p4_cruzados", sonda = "M_NUM",
       cambios = list(c("exsolution_str <- paste(mchoice2string(sol_p1), exsol_p2, exsol_p3, exsol_p4,",
                        "exsolution_str <- paste(mchoice2string(sol_p1), exsol_p2, exsol_p4, exsol_p3,"))),
  list(id = "MM2_marcas_p6_invertidas", sonda = "M_P6",
       cambios = list(c("sol_p6 <- v6", "sol_p6 <- !v6"), c("= identical(verdad6, sol_p6),", "= TRUE,"))),
  list(id = "MM3_p1_prosa_otro_rotulo", sonda = "M_P1",
       cambios = list(c("textos_p1[k1], \": \", describe", "textos_p1[k1 %% 4L + 1L], \": \", describe"))))
sec_moodle <- function() {
  N <- 100L
  cat(sprintf("\n[moodle] exams2moodle propio desde el .Rmd vigente, N = %d (semillas 1..%d), familia B de la regla #23\n", N, N))
  t0 <- proc.time()[["elapsed"]]
  qs <- moodle_render(RMD, seq_len(N), file.path(tempdir(), "moodle_real"))
  if (!length(qs)) { rojo("moodle", paste("exams2moodle falló:", attr(qs, "error"))); return(invisible()) }
  r <- lapply(qs, moodle_pregunta); cods <- lapply(r, `[[`, "cod"); ex <- compila(DG_C); ent <- 0L; ks <- integer(0)
  for (i in seq_along(r)) if (!length(cods[[i]])) {
    en <- entorno_moodle(i, sub("^diagrama_[a-d]_([0-9a-f]+)\\.png$", "\\1", r[[i]]$img), ex)
    if (!is.null(en)) { ent <- ent + 1L; ks <- c(ks, en$k)
      if (!identical(as.integer(en$e$regiones_mezcladas[[match(r[[i]]$img, en$e$archivos)]]), as.integer(r[[i]]$reg1))) cods[[i]] <- "M_P1_IMAGEN" } }
  nombres <- unique(unlist(regmatches(qs, gregexpr("diagrama_[^.\"/]+\\.png", qs))))
  malos <- which(lengths(cods) > 0L)
  cat(sprintf("  %d preguntas en %.0f s · con algún código: %d · imagen de la clave contra el entorno de su semilla: %d de %d (sorteos previos de exams: %s) · nombres no neutrales: %d de %d\n",
              length(qs), proc.time()[["elapsed"]] - t0, length(malos), ent, length(qs), paste(sort(unique(ks)), collapse = ","),
              sum(!grepl("^diagrama_[a-z](_[0-9a-f]+)?\\.png$", nombres)), length(nombres)))
  if (length(qs) != N) rojo("moodle", sprintf("el XML trae %d preguntas, no %d", length(qs), N))
  if (length(malos)) rojo("moodle", paste(sprintf("pregunta %d: %s", head(malos, 3), vapply(cods[head(malos, 3)], paste, "", collapse = ",")), collapse = "; "))
  if (any(!grepl("^diagrama_[a-z](_[0-9a-f]+)?\\.png$", nombres))) rojo("moodle", "nombres de imagen no neutrales (regla #4 v6.1)")
  if (ent < 20L) pendiente("MOODLE_SIN_ENTORNO", sprintf("solo %d preguntas con entorno: el contenido de la imagen de la clave queda sin medir", ent))
  ## Controles: copias mutadas en tempdir(), hasta la primera muerte (tope 3 semillas); línea base = las mismas semillas reales.
  for (M in MOODLE_MUTANTES) {
    mut <- lineas; mal <- NULL
    for (cb in M$cambios) { mut <- mutar(mut, cb[1], cb[2]); if (!is.null(attr(mut, "mal"))) { mal <- attr(mut, "mal"); break } }
    if (!is.null(mal)) { cat(sprintf("  %-34s mal_construido (%s)\n", M$id, mal)); rojo("moodle", paste(M$id, "mal construido")); next }
    d <- file.path(tempdir(), paste0("moodle_", M$id)); dir.create(d, showWarnings = FALSE)
    f <- file.path(d, basename(RMD)); writeLines(mut, f, useBytes = TRUE)
    qm <- moodle_render(f, 1:3, file.path(d, "xml"))
    cm <- if (length(qm)) lapply(qm, function(x) moodle_pregunta(x)$cod) else list(paste("RENDER:", attr(qm, "error")))
    i <- which(lengths(cm) > 0L)[1]; cod <- if (is.na(i)) character(0) else cm[[i]]
    v <- if (!is.na(i) && length(cods[[i]])) "mal_construido" else veredicto(cod, M$sonda)
    cat(sprintf("  %-34s sonda esperada %-6s semilla %-3s %s · %s\n", M$id, M$sonda, if (is.na(i)) "-" else i, v,
                if (length(cod)) paste(cod, collapse = ",") else "-"))
    if (!v %in% c("cazado_por_su_sonda", "cazado_por_su_sonda_y_otra")) rojo("moodle", paste(M$id, v))
  }
}

## --- solucion (I-4.7; FASE 2C r1, H2) ---------------------------------------------------------------------------------
## Port de la sección solucion de ../verificar_espacio.R. Se ejecutan los chunks solucion y feedback del .Rmd en las cuotas de
## texto (100 variantes y 20 canónicas por rama de las Partes 3 y 4). Tres capas:
##   plantilla: cada párrafo casa con UNA plantilla cuyas cifras se contrastan con lo que se calcula desde las viñetas
##              (leer_vinetas, la semántica del oráculo) y con los diagramas tal como los rotula el enunciado, o está en
##              SOL_LITERALES (frases sin datos sorteados, leídas una por una). Un párrafo nuevo o reescrito da SOL_SIN_ORACULO.
##   texto:     cada párrafo coincide letra a letra con el reconstruido desde las viñetas (SOL_TEXTO), en su sección y en su
##              orden (SOL_ORDEN); los dos pools de reflexiones están congelados.
##   feedback:  la retroalimentación de los seis gaps, tal como la emite el chunk feedback, letra a letra (SOL_FEEDBACK).
## Prueba que el texto es el revisado, no que sea verdadero: la verdad la dan los oráculos y la lectura de la FASE 2C. Un
## mutante puede pedir una sola capa (obligación 7): el texto letra a letra cae con cualquier cambio de cifra, así que una
## sonda de cifras solo se prueba sola sin esa capa.
CH_SOL <- compila(chunk_de(lineas, "solucion")); CH_FB <- compila(chunk_de(lineas, "feedback"))
ANCHO_I5 <- "{width=5.5cm style=\"width:280px!important;max-width:100%\"}"
SOL_LATEX <- "\\nobreak\\makeatletter\\@afterindenttrue\\@afterheading\\makeatother"
SOL_ENC_INI <- paste0("**", c("Parte 1 · Respuesta correcta", "Parte 1 · Análisis del error", "Parte 1 · Procedimiento correcto",
                              "Parte 1 · Análisis de los diagramas incorrectos", "Partes 2 a 5 · Procedimiento en la otra encuesta"), "**")
SOL_ENC_FIN <- paste0("**", c("Propiedades del concepto", "Caso específico", "Reflexión metacognitiva", "Estrategia para evitar el error"), "**")
enc_p6 <- function(v, n) paste0("**Parte 6 · ", if (n == 1L) "Afirmación " else "Afirmaciones ",
                                if (v) (if (n == 1L) "verdadera" else "verdaderas") else (if (n == 1L) "falsa" else "falsas"), "**")
## Frases sin datos sorteados, leídas una por una: son ciertas por definición.
SOL_LIT_P1 <- c("**Qué evalúa la Parte 1:** la capacidad de organizar datos de intersección y totales en un diagrama de Venn.",
  "*Causa de fondo:* leer «X y Y» y «X y Y, pero no Z» como si dijeran lo mismo. La primera frase cuenta toda la intersección; la segunda, solo la parte que queda fuera del tercer conjunto.")
SOL_PROPIEDADES <- c("- Las siete zonas no se solapan: cada elemento se cuenta en una sola.",
  "- La intersección de dos conjuntos se parte en dos zonas: la que queda fuera del tercero y el centro.",
  "- El total de un conjunto es la suma de las cuatro zonas de su círculo.",
  "- Una cifra del enunciado no siempre aparece tal cual en una zona: muchas son la suma de varias.")
SOL_ESTRATEGIA <- c("- Empieza por el centro y avanza hacia afuera: centro, zonas de dos conjuntos y zonas de un solo conjunto.",
  "- Marca en cada frase si habla de una zona o de varias: «pero no» señala una sola zona.",
  "- Antes de elegir, comprueba los siete datos: los tres totales, las tres intersecciones y el dato «pero no».",
  "- Si un diagrama falla en un dato, descártalo; si cumple uno, sigue revisando.",
  "- Cuando construyas tú las zonas, como en la otra encuesta, comprueba que las cuatro zonas de cada círculo sumen el total de ese conjunto.")
SOL_LITERALES <- c(SOL_LIT_P1, SOL_PROPIEDADES, SOL_ESTRATEGIA)
SOL_REFL_P1 <- c("Antes de escribir un número en una zona, pregúntate a quiénes cuenta esa zona y a quiénes deja por fuera.",
  "Una frase con «pero no» describe una parte de una intersección: compárala con la frase que habla de la intersección completa.",
  "Que los totales cuadren no prueba que el diagrama esté bien: revisa cada dato del enunciado, no solo algunos.",
  "Si un dato no se deja ubicar en una sola zona, es porque reúne varias: sepáralas antes de escribir.",
  "Cuando dos diagramas se parezcan, busca el dato que los distingue en lugar de mirar el conjunto de los números.")
SOL_REFL_CLOZE <- c("Calcular primero el centro de la otra encuesta deja cada zona de dos conjuntos a una resta: es el mismo orden que sirve en la Parte 1.",
  "Una afirmación sin cifras se decide con cifras: antes de juzgarla, escribe cuántos hay en cada una de las dos zonas que compara.",
  "Si las cuatro zonas de un círculo no suman el total de ese conjunto, alguna está mal: revísala antes de seguir.")
## Concordancia (regla #7): género de cada grupo y de cada categoría de la otra encuesta, leído una vez. Uno nuevo da
## SOL_CONCORDANCIA hasta revisarlo.
GRUPOS2_FEM <- c(estudiantes = FALSE, socios = FALSE, asociados = FALSE, vecinos = FALSE, usuarios = FALSE, asistentes = FALSE, personas = TRUE)
CATEGORIAS2_FEM <- c(talleres = FALSE, deportes = FALSE, cursos = FALSE, platos = FALSE, "géneros" = FALSE, actividades = TRUE)
## Los tres mecanismos de la Guía (I-3, tabla de ../.claude/CLAUDE.md), escritos aquí: no se leen FORMULAS del .Rmd.
MECANISMO <- list(A = function(x) with(as.list(x), c(P - b - d, b, M - b - c, a, d - a, c - a, B - d - c + a)),
                  B = function(x) with(as.list(x), c(P, a, M, b - a, d - (b - a), c - (b - a), B)),
                  D = function(x) with(as.list(x), c(P, b, M, a, d, c, B)))
DATOS <- c("a", "b", "c", "d", "P", "M", "B")
CIRC <- list(s1 = 1L, p12 = 1:2, s2 = 2L, ctr = 1:3, p13 = c(1L, 3L), p23 = 2:3, s3 = 3L)        # círculos de cada zona, en ORDEN
datos_v <- function(v) { v <- stats::setNames(as.integer(v), ORDEN)
  c(a = v[["p12"]], b = v[["p12"]] + v[["ctr"]], c = v[["p23"]] + v[["ctr"]], d = v[["p13"]] + v[["ctr"]],
    P = v[["s1"]] + v[["p12"]] + v[["p13"]] + v[["ctr"]], M = v[["s2"]] + v[["p12"]] + v[["p23"]] + v[["ctr"]],
    B = v[["s3"]] + v[["p13"]] + v[["p23"]] + v[["ctr"]]) }
circ <- function(v, k) as.integer(v)[vapply(CIRC, function(s) k %in% s, TRUE)]
cap1v <- function(s) paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))
st <- function(...) { v <- c(...); paste0(paste(v, collapse = " + "), " = ", sum(v)) }
texto_solucion <- function(e, sol = CH_SOL) {
  x <- utils::capture.output(eval(sol, new.env(parent = e))); en <- FALSE; out <- character(0); ltx <- character(0); valla <- strrep(intToUtf8(96L), 3L)
  for (l in x) { if (startsWith(l, valla)) { en <- !en; next }; if (en) ltx <- c(ltx, l) else if (nzchar(trimws(l))) out <- c(out, l) }
  attr(out, "latex") <- ltx
  out
}
sonda_arit_sol <- function(L) {
  f <- character(0)
  for (l in L) for (x in regmatches(l, gregexpr("\\$[^$]+\\$", l))[[1L]]) {
    p <- strsplit(gsub("$", "", x, fixed = TRUE), "=", fixed = TRUE)[[1L]]
    if (length(p) == 2L && all(grepl("^[0-9 +-]+$", p)) && eval(parse(text = p[1L])) != eval(parse(text = p[2L]))) f <- "SOL_ARIT"
  }
  f
}
## Lo que lee el estudiante: las viñetas de las dos encuestas, el rótulo de cada diagrama en el enunciado y las preguntas.
lectura_sol <- function(e) {
  o1 <- leer_vinetas(paste0("- ", c(e$vineta_1, e$vineta_2, e$vineta_3)))
  o2 <- leer_vinetas(paste0("- ", c(e$vineta2_1, e$vineta2_2, e$vineta2_3)))
  enun <- texto_de(e, CH_TEXTO[1L])
  img <- regmatches(enun, gregexpr("\\*\\*Diagrama (I|II|III|IV)\\*\\*\\\\\n!\\[[^]]*\\]\\((diagrama_[a-d]_[0-9a-f]+\\.png)\\)", enun))[[1]]
  rot <- sub("^\\*\\*Diagrama (I|II|III|IV)\\*\\*.*$", "\\1", img); arch <- sub("^.*\\((diagrama_[^)]+)\\)$", "\\1", img)
  if (is.null(o1) || is.null(o2) || length(img) != 4L || !setequal(rot, c("I", "II", "III", "IV")) || anyNA(match(arch, e$archivos))) return(NULL)
  z3 <- zona_de(sub("^.* (solo \\S+ y \\S+)\\?$", "\\1", e$texto_p3), o2$S); z4 <- zona_de(sub("^.* (solo \\S+)\\?$", "\\1", e$texto_p4), o2$S)
  los_tres <- sub("^.* ((los|las) tres \\S+)\\?$", "\\1", e$texto_p2)
  al_menos <- sub("^.* (al menos (uno de los|una de las) tres \\S+)\\?$", "\\1", e$texto_p5)
  if (!isTRUE(z3 %in% c("p13", "p23")) || !isTRUE(z4 %in% c("s1", "s2", "s3")) || identical(los_tres, e$texto_p2) || identical(al_menos, e$texto_p5)) return(NULL)
  V <- stats::setNames(lapply(arch, function(a) as.integer(e$regiones_mezcladas[[match(a, e$archivos)]])), rot)
  list(o1 = o1, o2 = o2, V = V[c("I", "II", "III", "IV")], arch = stats::setNames(arch, rot)[c("I", "II", "III", "IV")], z3 = z3, z4 = z4,
       los_tres = los_tres, al_menos = al_menos, ex = strsplit(e$exsolution_str, "|", fixed = TRUE)[[1]],
       dt = stats::setNames(as.integer(o1$datos), DATOS), d2 = stats::setNames(as.integer(o2$datos), DATOS))
}
## Cada párrafo de la Solution y cada retroalimentación, letra a letra, desde lo que lee el estudiante; no usa el pool del .Rmd.
esperado_sol <- function(e, x) {
  o1 <- x$o1; o2 <- x$o2; dt <- x$dt; d2 <- x$d2; r <- as.integer(o1$reg); rn <- stats::setNames(r, ORDEN); r2 <- as.integer(o2$reg); r2n <- stats::setNames(r2, ORDEN)
  T1 <- o1$S[1]; T2 <- o1$S[2]; T3 <- o1$S[3]; G <- o1$grupo; Vb <- o1$verbo
  a <- dt[["a"]]; b <- dt[["b"]]; cc <- dt[["c"]]; d <- dt[["d"]]; P <- dt[["P"]]; M <- dt[["M"]]; B <- dt[["B"]]; z <- b - a
  Z12 <- paste0(T1, " y ", T2, " sin ", T3); Z13 <- paste0(T1, " y ", T3, " sin ", T2); Z23 <- paste0(T2, " y ", T3, " sin ", T1)
  rot_de <- function(v) { k <- names(x$V)[vapply(x$V, identical, TRUE, as.integer(v))]; if (length(k) == 1L) k else "?" }
  desc <- function(v) sprintf("el diagrama con %d en el centro, %d en la zona de %s y %d en la zona de solo %s", v[4], v[2], Z12, v[1], T1)
  vA <- as.integer(MECANISMO$A(dt)); vB <- as.integer(MECANISMO$B(dt)); vD <- as.integer(MECANISMO$D(dt)); k <- rot_de(r)
  p1 <- c(SOL_ENC_INI[1],
    sprintf("La respuesta correcta es el Diagrama %s: %s. Es el único de los cuatro que reproduce los siete datos.", k, desc(r)),
    paste0("![Diagrama de Venn de la respuesta correcta](", if (k %in% names(x$arch)) x$arch[[k]] else "?", ")", ANCHO_I5, "&#8203;"),
    SOL_LIT_P1[1], SOL_ENC_INI[2],
    sprintf("La trampa está en la frase «pero no %s». Ese dato ocupa una zona del diagrama, la de %s, y no el centro. En cambio, el dato de %d %s que %s %s y %s incluye a quienes también %s %s: es esa zona más el centro.", T3, Z12, b, G, Vb, T1, T2, Vb, T3),
    sprintf("Revisar solo los totales no basta: uno de los diagramas los conserva todos y aun así falla los dos datos de %s y %s.", T1, T2),
    SOL_LIT_P1[2], SOL_ENC_INI[3],
    sprintf("- **Centro.** Quienes están en los tres conjuntos son los de %s y %s menos los que no están en %s: $%d - %d = %d$.", T1, T2, T3, b, a, z),
    sprintf("- **Zonas de dos conjuntos.** %s: $%d$. %s: $%d - %d = %d$. %s: $%d - %d = %d$.", cap1v(Z12), a, cap1v(Z23), cc, z, rn[["p23"]], cap1v(Z13), d, z, rn[["p13"]]),
    sprintf("- **Zonas de un solo conjunto.** Solo %s: $%d - %d - %d = %d$. Solo %s: $%d - %d - %d = %d$. Solo %s: $%d - %d - %d + %d = %d$, porque %d y %d cuentan dos veces el centro.",
            T1, P, a, d, rn[["s1"]], T2, M, a, cc, rn[["s2"]], T3, B, d, cc, z, rn[["s3"]], d, cc),
    SOL_ENC_INI[4],
    paste0("*Diagrama ", rot_de(vA), ".* **Poner en el centro el dato «pero no».** Es posible que los estudiantes que eligen ", desc(vA),
           " tomen a quienes están fuera del tercer conjunto como si fueran el centro. El dato de ", b, " ", G, " quedó entero en la zona de ", Z12,
           ", y esa zona con el centro suma $", b, " + ", a, " = ", a + b, "$, no ", b, ". Desde ahí las demás restas están bien hechas: por eso los tres círculos suman ",
           P, ", ", M, " y ", B, ", y revisar solo los totales no lo delata. *Causa de fondo:* leer «X y Y» y «X y Y, pero no Z» como si dijeran lo mismo, y repartir los dos datos al revés."),
    paste0("*Diagrama ", rot_de(vB), ".* **Confundir el total de un conjunto con quienes están solo en él.** Es posible que los estudiantes que eligen ", desc(vB),
           " dejen cada total en la zona de un solo conjunto, sin restar lo que ese conjunto comparte. El centro y las zonas de dos conjuntos están bien, pero el círculo de ",
           T1, " suma $", P, " + ", a, " + ", z, " + ", d - z, " = ", P + a + d, "$, no ", P, ". Lo mismo pasa con los círculos de ", T2, " y de ", T3,
           ". *Causa de fondo:* creer que el total de un conjunto cuenta solo a quienes no están en ningún otro."),
    paste0("*Diagrama ", rot_de(vD), ".* **Poner cada dato en una zona, sin operar.** Es posible que los estudiantes que eligen ", desc(vD),
           " repartan las cifras del enunciado entre las zonas sin hacer ninguna resta. Aquí ", a, " también quedó en el centro y ninguna cifra se restó, así que no se cumple ni un solo dato. Por ejemplo, el círculo de ",
           T1, " suma $", P, " + ", b, " + ", a, " + ", d, " = ", P + a + b + d, "$, no ", P, ". *Causa de fondo:* tratar cada frase del enunciado como el contenido de una zona, cuando seis de los siete datos reúnen varias zonas."),
    if (identical(e$otro_error$codigo, "VN-X-RESTA-UNA"))
      paste0("**Otro error posible: Restarle al total solo una de las intersecciones.** Es posible que los estudiantes descuenten del total una intersección y olviden la otra. Para llegar a la zona de solo ",
             T1, " no basta con quitarle al total la intersección con ", T2, ": $", P, " - ", b, " = ", P - b, "$ todavía incluye la zona de ", Z13,
             ". *Causa de fondo:* mirar un solo vecino de cada zona en lugar de todas las zonas que comparten el círculo.")
    else paste0("**Otro error posible: Restar dos veces el centro.** Es posible que los estudiantes le quiten al total del tercer conjunto las dos intersecciones y no devuelvan el centro. Para llegar a la zona de solo ",
             T3, " no basta con quitarle al total las dos intersecciones: en $", B, " - ", d, " - ", cc, " = ", B - d - cc, "$ el centro quedó restado dos veces, porque pertenece a las dos, y por eso el resultado se queda corto en ",
             z, ". Hay que sumarlo una vez: $", B - d - cc, " + ", z, " = ", rn[["s3"]], "$. *Causa de fondo:* olvidar que el centro pertenece a las dos intersecciones que se restan."))
  U1 <- o2$S[1]; U2 <- o2$S[2]; U3 <- o2$S[3]; V2 <- o2$verbo; fem <- isTRUE(GRUPOS2_FEM[o2$grupo])
  art <- if (fem) "las" else "los"; ellos <- if (fem) "ellas" else "ellos"; restad <- if (fem) "restadas" else "restados"
  a2 <- d2[["a"]]; b2 <- d2[["b"]]; c2 <- d2[["c"]]; dd2 <- d2[["d"]]; P2 <- d2[["P"]]; M2 <- d2[["M"]]; B2 <- d2[["B"]]; z2 <- b2 - a2
  ZONA <- c(s1 = paste("solo", U1), p12 = paste("solo", U1, "y", U2), s2 = paste("solo", U2), ctr = x$los_tres,
            p13 = paste("solo", U1, "y", U3), p23 = paste("solo", U2, "y", U3), s3 = paste("solo", U3))
  q3 <- if (x$z3 == "p13") list(v = dd2, par = paste(U1, "y", U3)) else list(v = c2, par = paste(U2, "y", U3))
  q4 <- switch(x$z4,
    s1 = list(t = sprintf("al total de %s, %d, se le quitan %s %d de %s y %s %d que %s %s y %s", U1, P2, art, a2, ZONA[["p12"]], art, dd2, V2, U1, U3), op = sprintf("%d - %d - %d", P2, a2, dd2)),
    s2 = list(t = sprintf("al total de %s, %d, se le quitan %s %d de %s y %s %d que %s %s y %s", U2, M2, art, a2, ZONA[["p12"]], art, c2, V2, U2, U3), op = sprintf("%d - %d - %d", M2, a2, c2)),
    s3 = list(t = sprintf("al total de %s, %d, se le quitan %s %d que %s %s y %s y %s %d que %s %s y %s, y se suman %s %d de %s, que quedaron %s dos veces",
                          U3, B2, art, dd2, V2, U1, U3, art, c2, V2, U2, U3, art, z2, x$los_tres, restad), op = sprintf("%d - %d - %d + %d", B2, dd2, c2, z2)))
  p25 <- c(SOL_ENC_INI[5],
    sprintf("- **Parte 2, %s.** Quienes %s %s y %s son %d, y %d de %s no %s %s: $%d - %d = %d$.", x$los_tres, V2, U1, U2, b2, a2, ellos, V2, U3, b2, a2, z2),
    sprintf("- **Parte 3, %s.** De %s %d que %s %s se quitan %s %d de %s: $%d - %d = %d$.", ZONA[[x$z3]], art, q3$v, V2, q3$par, art, z2, x$los_tres, q3$v, z2, r2n[[x$z3]]),
    sprintf("- **Parte 4, %s.** %s: $%s = %d$.", ZONA[[x$z4]], cap1v(q4$t), q4$op, r2n[[x$z4]]),
    sprintf("- **Parte 5, %s.** Se suman las siete zonas: %s.", x$al_menos, st(r2)))
  af <- regmatches(e$opciones_p6, regexec("^(Más|Menos) \\S+ \\S+ (.+) que (.+)\\.$", e$opciones_p6))
  ok6 <- all(lengths(af) == 4L)
  zx <- if (ok6) vapply(af, function(m) zona_de(m[3], o2$S), "") else rep(NA_character_, 4L)
  zy <- if (ok6) vapply(af, function(m) zona_de(m[4], o2$S), "") else rep(NA_character_, 4L)
  if (anyNA(c(zx, zy))) { ver <- rep(NA, 4L); just <- rep("?", 4L) } else {
    ver <- ifelse(vapply(af, `[`, "", 2L) == "Más", r2n[zx] > r2n[zy], r2n[zx] < r2n[zy])
    just <- sprintf("%s: %d; %s: %d.", cap1v(vapply(af, `[`, "", 3L)), r2n[zx], vapply(af, `[`, "", 4L), r2n[zy]) }
  p6 <- unlist(lapply(c(TRUE, FALSE), function(g) { i <- which(ver %in% g); c(enc_p6(g, length(i)), sprintf("- «%s» %s", e$opciones_p6[i], just[i])) }))
  fin <- c(SOL_ENC_FIN[1], SOL_PROPIEDADES, SOL_ENC_FIN[2],
    sprintf("Con el diagrama correcto de la Parte 1, el círculo de %s suma $%s$, el de %s suma $%s$ y el de %s suma $%s$. Además, %s y %s suman $%s$, %s y %s suman $%s$, y %s y %s suman $%s$.",
            T1, st(rn[["s1"]], rn[["p12"]], rn[["p13"]], rn[["ctr"]]), T2, st(rn[["s2"]], rn[["p12"]], rn[["p23"]], rn[["ctr"]]), T3, st(rn[["s3"]], rn[["p13"]], rn[["p23"]], rn[["ctr"]]),
            T1, T2, st(rn[["p12"]], rn[["ctr"]]), T2, T3, st(rn[["p23"]], rn[["ctr"]]), T1, T3, st(rn[["p13"]], rn[["ctr"]])),
    sprintf("En la otra encuesta pasa lo mismo: el círculo de %s suma $%s$, el de %s suma $%s$ y el de %s suma $%s$.",
            U1, st(r2n[["s1"]], r2n[["p12"]], r2n[["p13"]], r2n[["ctr"]]), U2, st(r2n[["s2"]], r2n[["p12"]], r2n[["p23"]], r2n[["ctr"]]), U3, st(r2n[["s3"]], r2n[["p13"]], r2n[["p23"]], r2n[["ctr"]])),
    SOL_ENC_FIN[3], paste(e$reflexion, e$reflexion_cloze), SOL_ENC_FIN[4], SOL_ESTRATEGIA)
  fbk <- function(v) if (identical(v, r)) "Correcta. Este diagrama reproduce los siete datos." else if (identical(v, vA))
    paste0("Incorrecta. Los tres totales cuadran, pero ", a, " va en la zona de ", Z12, ", no en el centro.") else if (identical(v, vB))
    "Incorrecta. El centro y las zonas de dos conjuntos están bien, pero cada total quedó en la zona de un solo conjunto." else if (identical(v, vD))
    "Incorrecta. Cada dato quedó en una zona sin operar, y ninguno de los siete se cumple." else "?"
  fb <- c(paste0("Diagrama ", names(x$V), ": ", vapply(x$V, fbk, "")),
    sprintf("Quienes %s %s y %s son %d, y %d de %s no %s %s: en %s hay %d - %d = %d.", V2, U1, U2, b2, a2, ellos, V2, U3, x$los_tres, b2, a2, z2),
    sprintf("%s: de %s %d que %s %s se quitan %s %d de %s: %d - %d = %d.", cap1v(ZONA[[x$z3]]), art, q3$v, V2, q3$par, art, z2, x$los_tres, q3$v, z2, r2n[[x$z3]]),
    sprintf("%s: %s: %s = %d.", cap1v(ZONA[[x$z4]]), q4$t, q4$op, r2n[[x$z4]]),
    sprintf("Se suman las siete zonas: %s.", st(r2)),
    paste(ifelse(ver %in% TRUE, "Verdadera.", "Falsa."), just))
  list(sol = c(p1, p25, p6, fin), fb = unname(fb))
}
## Capa plantilla: cada párrafo casa con una sola plantilla y sus cifras se contrastan con las viñetas y los diagramas rotulados.
sondas_sol_plantilla <- function(e, x, L) {
  o1 <- x$o1; o2 <- x$o2; dt <- x$dt; d2 <- x$d2; rg <- unname(x$V); U <- o2$S
  r <- as.integer(o1$reg); rn <- stats::setNames(r, ORDEN); r2 <- as.integer(o2$reg); r2n <- stats::setNames(r2, ORDEN)
  T1 <- o1$S[1]; T2 <- o1$S[2]; T3 <- o1$S[3]; Z12 <- paste0(T1, " y ", T2, " sin ", T3); Z13 <- paste0(T1, " y ", T3, " sin ", T2)
  fem <- GRUPOS2_FEM[o2$grupo]; art <- if (isTRUE(fem)) "las" else "los"; ellos <- if (isTRUE(fem)) "ellas" else "ellos"
  restad <- if (isTRUE(fem)) "restadas" else "restados"; z2 <- d2[["b"]] - d2[["a"]]
  nro <- function(s) as.integer(s); sumandos <- function(s) as.integer(strsplit(s, " + ", fixed = TRUE)[[1L]])
  mismos <- function(p, q) identical(sort(as.integer(p)), sort(as.integer(q)))
  f <- character(0)
  ## Concordancia de la otra encuesta: grupo y categoría conocidos, y sus artículos en las preguntas de las Partes 2 a 5.
  fc <- CATEGORIAS2_FEM[sub("^(los|las) tres ", "", x$los_tres)]
  if (is.na(fem) || is.na(fc) || startsWith(x$los_tres, "las ") != isTRUE(fc) || grepl("una de las", x$al_menos, fixed = TRUE) != isTRUE(fc) ||
      any(startsWith(c(e$texto_p2, e$texto_p3, e$texto_p4, e$texto_p5), "¿Cuántas ") != isTRUE(fem))) f <- c(f, "SOL_CONCORDANCIA")
  ## «Seis de los siete datos reúnen varias zonas»: las zonas de cada dato, desde el .Rmd y desde la semántica de aquí.
  inc <- vapply(ORDEN, function(zz) { q <- stats::setNames(as.list(rep(0L, 7L)), ORDEN); q[[zz]] <- 1L; as.numeric(unlist(e$datos_desde(q))) }, numeric(7L))
  inc2 <- vapply(ORDEN, function(zz) { v <- stats::setNames(rep(0L, 7L), ORDEN); v[[zz]] <- 1L; as.numeric(datos_v(v)) }, numeric(7L))
  if (!all(inc == inc2) || !identical(unname(rowSums(inc2)), c(1, 2, 2, 2, 4, 4, 4))) f <- c(f, "SOL_AFIRMACION")
  vistas <- list()
  ver <- function(rot, m) {     # el diagrama de un párrafo de diagnóstico: uno solo, distractor, sin repetir y bajo su rótulo
    i <- which(vapply(rg, function(v) v[4] == nro(m[1]) && v[2] == nro(m[2]) && v[1] == nro(m[3]), TRUE))
    v <- if (length(i) == 1L) rg[[i]] else NULL
    if (is.null(v) || identical(v, r) || any(vapply(vistas, identical, TRUE, v)) || !identical(x$V[[rot]], v)) { f <<- c(f, "SOL_OPCION"); return(NULL) }
    vistas[[length(vistas) + 1L]] <<- v
    v
  }
  D_OP <- "Es posible que los estudiantes que eligen el diagrama con (\\d+) en el centro, (\\d+) en la zona de .+ y (\\d+) en la zona de solo .+ "
  SUMA <- "\\$([0-9 +]+) = (\\d+)\\$"; ROT <- "^\\*Diagrama (I|II|III|IV)\\.\\* "
  P <- list(
    respuesta = list(n = 1L, p = "^La respuesta correcta es el Diagrama (I|II|III|IV): el diagrama con (\\d+) en el centro, (\\d+) en la zona de (.+) y (\\d+) en la zona de solo (.+)\\. Es el único de los cuatro que reproduce los siete datos\\.$",
      f = function(m) { rep7 <- vapply(rg, function(v) all(datos_v(v) == dt), TRUE)
        if (!identical(x$V[[m[1]]], r) || !identical(nro(m[c(2, 3, 5)]), unname(rn[c("ctr", "p12", "s1")])) || m[4] != Z12 || m[6] != T1 || sum(rep7) != 1L) "SOL_RESPUESTA" }),
    imagen = list(n = 1L, p = "^!\\[Diagrama de Venn de la respuesta correcta\\]\\((diagrama_[a-d]_[0-9a-f]+\\.png)\\)(.*)$",
      f = function(m) { k <- names(x$V)[vapply(x$V, identical, TRUE, r)]
        if (length(k) != 1L || m[1] != x$arch[[k]] || m[2] != paste0(ANCHO_I5, "&#8203;")) "SOL_IMAGEN" }),
    trampa = list(n = 1L, p = "^La trampa está en la frase «pero no (.+)»\\. Ese dato ocupa una zona del diagrama, la de (.+), y no el centro\\. En cambio, el dato de (\\d+) .+ incluye a quienes también .+: es esa zona más el centro\\.$",
      f = function(m) if (m[1] != T3 || m[2] != Z12 || nro(m[3]) != dt[["b"]] || rn[["ctr"]] < 1L) "SOL_AFIRMACION"),
    totales = list(n = 1L, p = "^Revisar solo los totales no basta: uno de los diagramas los conserva todos y aun así falla los dos datos de (.+) y (.+)\\.$",
      f = function(m) { k <- vapply(rg, function(v) identical(names(which(datos_v(v) != dt)), c("a", "b")), TRUE)
        if (m[1] != T1 || m[2] != T2 || sum(k) != 1L) "SOL_AFIRMACION" }),
    centro = list(n = 1L, p = "^- \\*\\*Centro\\.\\*\\* Quienes están en los tres conjuntos son los de .+ y .+ menos los que no están en .+: \\$(\\d+) - (\\d+) = (\\d+)\\$\\.$",
      f = function(m) if (!identical(nro(m), c(dt[["b"]], dt[["a"]], rn[["ctr"]]))) "SOL_PROCEDIMIENTO"),
    dobles = list(n = 1L, p = "^- \\*\\*Zonas de dos conjuntos\\.\\*\\* .+: \\$(\\d+)\\$\\. .+: \\$(\\d+) - (\\d+) = (\\d+)\\$\\. .+: \\$(\\d+) - (\\d+) = (\\d+)\\$\\.$",
      f = function(m) if (!identical(nro(m), c(rn[["p12"]], dt[["c"]], rn[["ctr"]], rn[["p23"]], dt[["d"]], rn[["ctr"]], rn[["p13"]]))) "SOL_PROCEDIMIENTO"),
    solas = list(n = 1L, p = "^- \\*\\*Zonas de un solo conjunto\\.\\*\\* Solo .+: \\$(\\d+) - (\\d+) - (\\d+) = (\\d+)\\$\\. Solo .+: \\$(\\d+) - (\\d+) - (\\d+) = (\\d+)\\$\\. Solo .+: \\$(\\d+) - (\\d+) - (\\d+) \\+ (\\d+) = (\\d+)\\$, porque (\\d+) y (\\d+) cuentan dos veces el centro\\.$",
      f = function(m) if (!identical(nro(m), c(dt[["P"]], dt[["a"]], dt[["d"]], rn[["s1"]], dt[["M"]], dt[["a"]], dt[["c"]], rn[["s2"]],
                                               dt[["B"]], dt[["d"]], dt[["c"]], rn[["ctr"]], rn[["s3"]], dt[["d"]], dt[["c"]]))) "SOL_PROCEDIMIENTO"),
    diag_A = list(n = 1L, p = paste0(ROT, "\\*\\*Poner en el centro el dato «pero no»\\.\\*\\* ", D_OP, "tomen a quienes están fuera del tercer conjunto como si fueran el centro\\. El dato de (\\d+) .+ quedó entero en la zona de .+, y esa zona con el centro suma ", SUMA, ", no (\\d+)\\. Desde ahí las demás restas están bien hechas: por eso los tres círculos suman (\\d+), (\\d+) y (\\d+), y revisar solo los totales no lo delata\\. \\*Causa de fondo:\\* .+\\.$"),
      f = function(m) { v <- ver(m[1], m[2:4]); if (is.null(v)) return(NULL); vn <- stats::setNames(v, ORDEN)
        ok <- vn[["p12"]] == dt[["b"]] && nro(m[5]) == dt[["b"]] && vn[["ctr"]] == dt[["a"]] && mismos(sumandos(m[6]), c(vn[["p12"]], vn[["ctr"]])) &&
          nro(m[7]) == vn[["p12"]] + vn[["ctr"]] && nro(m[8]) == dt[["b"]] && nro(m[7]) != nro(m[8]) && identical(nro(m[9:11]), unname(dt[c("P", "M", "B")])) &&
          identical(unname(datos_v(v)[c("P", "M", "B")]), unname(dt[c("P", "M", "B")]))
        if (!ok) "SOL_CIRCULO" }),
    diag_B = list(n = 1L, p = paste0(ROT, "\\*\\*Confundir el total de un conjunto con quienes están solo en él\\.\\*\\* ", D_OP, "dejen cada total en la zona de un solo conjunto, sin restar lo que ese conjunto comparte\\. El centro y las zonas de dos conjuntos están bien, pero el círculo de (.+) suma ", SUMA, ", no (\\d+)\\. Lo mismo pasa con los círculos de (.+) y de (.+)\\. \\*Causa de fondo:\\* .+\\.$"),
      f = function(m) { v <- ver(m[1], m[2:4]); if (is.null(v)) return(NULL)
        ok <- identical(v[c(2, 4, 5, 6)], r[c(2, 4, 5, 6)]) && identical(v[c(1, 3, 7)], unname(dt[c("P", "M", "B")])) && m[5] == T1 && mismos(sumandos(m[6]), circ(v, 1L)) &&
          nro(m[7]) == sum(circ(v, 1L)) && nro(m[8]) == dt[["P"]] && nro(m[7]) != nro(m[8]) && m[9] == T2 && m[10] == T3 &&
          sum(circ(v, 2L)) != dt[["M"]] && sum(circ(v, 3L)) != dt[["B"]]
        if (!ok) "SOL_CIRCULO" }),
    diag_D = list(n = 1L, p = paste0(ROT, "\\*\\*Poner cada dato en una zona, sin operar\\.\\*\\* ", D_OP, "repartan las cifras del enunciado entre las zonas sin hacer ninguna resta\\. Aquí (\\d+) también quedó en el centro y ninguna cifra se restó, así que no se cumple ni un solo dato\\. Por ejemplo, el círculo de (.+) suma ", SUMA, ", no (\\d+)\\. \\*Causa de fondo:\\* tratar cada frase del enunciado como el contenido de una zona, cuando seis de los siete datos reúnen varias zonas\\.$"),
      f = function(m) { v <- ver(m[1], m[2:4]); if (is.null(v)) return(NULL)
        ok <- nro(m[5]) == dt[["a"]] && v[4] == dt[["a"]] && mismos(v, dt) && !any(datos_v(v) == dt) && m[6] == T1 && mismos(sumandos(m[7]), circ(v, 1L)) &&
          nro(m[8]) == sum(circ(v, 1L)) && nro(m[9]) == dt[["P"]] && sum(rowSums(inc2) > 1) == 6L
        if (!ok) "SOL_CIRCULO" }),
    otro_resta = list(n = NA, p = "^\\*\\*Otro error posible: Restarle al total solo una de las intersecciones\\.\\*\\* Es posible que los estudiantes descuenten del total una intersección y olviden la otra\\. Para llegar a la zona de solo (.+) no basta con quitarle al total la intersección con (.+): \\$(\\d+) - (\\d+) = (\\d+)\\$ todavía incluye la zona de (.+)\\. \\*Causa de fondo:\\* .+\\.$",
      f = function(m) { val <- rn[["s1"]] + rn[["p13"]]
        ok <- m[1] == T1 && m[2] == T2 && identical(nro(m[3:5]), c(dt[["P"]], dt[["b"]], val)) && m[6] == Z13 && rn[["p13"]] > 0L && !val %in% vapply(rg, function(v) v[1], 0L)
        if (!ok) "SOL_OTRO" }),
    otro_doble = list(n = NA, p = "^\\*\\*Otro error posible: Restar dos veces el centro\\.\\*\\* Es posible que los estudiantes le quiten al total del tercer conjunto las dos intersecciones y no devuelvan el centro\\. Para llegar a la zona de solo (.+) no basta con quitarle al total las dos intersecciones: en \\$(\\d+) - (\\d+) - (\\d+) = (-?\\d+)\\$ el centro quedó restado dos veces, porque pertenece a las dos, y por eso el resultado se queda corto en (\\d+)\\. Hay que sumarlo una vez: \\$(-?\\d+) \\+ (\\d+) = (\\d+)\\$\\. \\*Causa de fondo:\\* .+\\.$",
      f = function(m) { zc <- rn[["ctr"]]; val <- rn[["s3"]] - zc
        ok <- m[1] == T3 && identical(nro(m[2:9]), c(dt[["B"]], dt[["d"]], dt[["c"]], val, zc, val, zc, rn[["s3"]])) && zc > 0L && !val %in% vapply(rg, function(v) v[7], 0L)
        if (!ok) "SOL_OTRO" }),
    p2 = list(n = 1L, p = "^- \\*\\*Parte 2, (.+)\\.\\*\\* Quienes \\S+ (\\S+) y (\\S+) son (\\d+), y (\\d+) de (ellos|ellas) no \\S+ (\\S+): \\$(\\d+) - (\\d+) = (\\d+)\\$\\.$",
      f = function(m) c(if (m[1] != x$los_tres || !identical(m[c(2, 3, 7)], U) || !identical(nro(m[c(4, 5, 8, 9, 10)]), c(d2[["b"]], d2[["a"]], d2[["b"]], d2[["a"]], r2n[["ctr"]])) ||
                            r2n[["ctr"]] != nro(x$ex[2])) "SOL_OTRA_ENCUESTA", if (m[6] != ellos) "SOL_CONCORDANCIA")),
    p3 = list(n = 1L, p = "^- \\*\\*Parte 3, (solo \\S+ y \\S+)\\.\\*\\* De (los|las) (\\d+) que \\S+ (\\S+) y (\\S+) se quitan (los|las) (\\d+) de (.+): \\$(\\d+) - (\\d+) = (\\d+)\\$\\.$",
      f = function(m) { zz <- zona_de(m[1], U); par <- if (identical(zz, "p13")) U[c(1, 3)] else U[c(2, 3)]; it <- if (identical(zz, "p13")) d2[["d"]] else d2[["c"]]
        c(if (!identical(zz, x$z3) || !identical(m[4:5], par) || nro(m[3]) != it || nro(m[7]) != z2 || m[8] != x$los_tres ||
              !identical(nro(m[9:11]), c(it, z2, r2n[[x$z3]])) || r2n[[x$z3]] != nro(x$ex[3])) "SOL_OTRA_ENCUESTA",
          if (m[2] != art || m[6] != art) "SOL_CONCORDANCIA") }),
    p4 = list(n = 1L, p = "^- \\*\\*Parte 4, (solo \\S+)\\.\\*\\* Al total de (\\S+), (\\d+), se le quitan (.+): \\$([0-9 +-]+) = (\\d+)\\$\\.$",
      f = function(m) { zz <- zona_de(m[1], U); k <- match(zz, c("s1", "s2", "s3")); if (is.na(k)) return("SOL_OTRA_ENCUESTA")
        tot <- d2[[c("P", "M", "B")[k]]]
        mid <- switch(zz, s1 = c(d2[["a"]], d2[["d"]]), s2 = c(d2[["a"]], d2[["c"]]), s3 = c(d2[["d"]], d2[["c"]], z2))
        op <- switch(zz, s1 = sprintf("%d - %d - %d", tot, d2[["a"]], d2[["d"]]), s2 = sprintf("%d - %d - %d", tot, d2[["a"]], d2[["c"]]),
                     s3 = sprintf("%d - %d - %d + %d", tot, d2[["d"]], d2[["c"]], z2))
        arts <- trimws(regmatches(m[4], gregexpr("\\b(los|las) (?=[0-9])", m[4], perl = TRUE))[[1]])
        c(if (!identical(zz, x$z4) || m[2] != U[k] || nro(m[3]) != tot || !identical(num(m[4]), as.integer(mid)) || m[5] != op ||
              nro(m[6]) != r2n[[zz]] || r2n[[zz]] != nro(x$ex[4])) "SOL_OTRA_ENCUESTA",
          if (!length(arts) || any(arts != art) || (identical(zz, "s3") && !grepl(paste0("que quedaron ", restad, " dos veces"), m[4], fixed = TRUE))) "SOL_CONCORDANCIA") }),
    p5 = list(n = 1L, p = "^- \\*\\*Parte 5, (al menos (?:uno de los|una de las) tres \\S+)\\.\\*\\* Se suman las siete zonas: ([0-9 +]+) = (\\d+)\\.$",
      f = function(m) if (m[1] != x$al_menos || !identical(sumandos(m[2]), r2) || nro(m[3]) != sum(r2) || sum(r2) != nro(x$ex[5])) "SOL_OTRA_ENCUESTA"),
    caso = list(n = 1L, p = paste0("^Con el diagrama correcto de la Parte 1, el círculo de .+ suma ", SUMA, ", el de .+ suma ", SUMA, " y el de .+ suma ", SUMA, "\\. Además, .+ y .+ suman ", SUMA, ", .+ y .+ suman ", SUMA, ", y .+ y .+ suman ", SUMA, "\\.$"),
      f = function(m) { zz <- list(circ(r, 1L), circ(r, 2L), circ(r, 3L), c(rn[["p12"]], rn[["ctr"]]), c(rn[["p23"]], rn[["ctr"]]), c(rn[["p13"]], rn[["ctr"]]))
        q <- unname(dt[c("P", "M", "B", "b", "c", "d")])
        if (!all(vapply(1:6, function(i) mismos(sumandos(m[2L * i - 1L]), zz[[i]]) && nro(m[2L * i]) == q[i], TRUE))) "SOL_CASO" }),
    caso2 = list(n = 1L, p = paste0("^En la otra encuesta pasa lo mismo: el círculo de .+ suma ", SUMA, ", el de .+ suma ", SUMA, " y el de .+ suma ", SUMA, "\\.$"),
      f = function(m) { q <- unname(d2[c("P", "M", "B")])
        if (!all(vapply(1:3, function(i) mismos(sumandos(m[2L * i - 1L]), circ(r2, i)) && nro(m[2L * i]) == q[i], TRUE))) "SOL_CASO" }))
  cuenta <- stats::setNames(integer(length(P)), names(P)); enc <- character(0); lit <- character(0); refl <- 0L
  h6 <- list(); g6 <- NULL; vistos6 <- character(0)
  ENC6 <- "^\\*\\*Parte 6 · (Afirmación|Afirmaciones) (verdadera|verdaderas|falsa|falsas)\\*\\*$"
  REFL <- as.vector(outer(SOL_REFL_P1, SOL_REFL_CLOZE, paste))
  af6 <- regmatches(e$opciones_p6, regexec("^(Más|Menos) \\S+ \\S+ (.+) que (.+)\\.$", e$opciones_p6))
  for (l in L) {
    if (l %in% c(SOL_ENC_INI, SOL_ENC_FIN)) { enc <- c(enc, l); g6 <- NULL; next }
    m6 <- regmatches(l, regexec(ENC6, l, perl = TRUE))[[1L]]
    if (length(m6)) { g6 <- startsWith(m6[3], "verdadera"); h6[[length(h6) + 1L]] <- list(g = g6, plural = m6[2] == "Afirmaciones", n = 0L); next }
    if (l %in% SOL_LITERALES) { lit <- c(lit, l); next }
    if (l %in% REFL) { refl <- refl + 1L; next }
    mi <- regmatches(l, regexec("^- «(.+)» (.+)$", l, perl = TRUE))[[1L]]
    if (length(mi) && !is.null(g6)) {
      h6[[length(h6)]]$n <- h6[[length(h6)]]$n + 1L; vistos6 <- c(vistos6, mi[2])
      j <- match(mi[2], e$opciones_p6); a <- if (is.na(j)) NULL else af6[[j]]
      zx <- if (length(a) == 4L) zona_de(a[3], U) else NA; zy <- if (length(a) == 4L) zona_de(a[4], U) else NA
      ok <- !is.na(zx) && !is.na(zy) && (if (a[2] == "Más") r2n[[zx]] > r2n[[zy]] else r2n[[zx]] < r2n[[zy]]) == g6 &&
        mi[3] == sprintf("%s: %d; %s: %d.", cap1v(a[3]), r2n[[zx]], a[4], r2n[[zy]])
      if (!isTRUE(ok)) f <- c(f, "SOL_P6")
      next
    }
    casadas <- 0L
    for (n in names(P)) {
      m <- regmatches(l, regexec(P[[n]]$p, l, perl = TRUE))[[1L]]
      ## Primero el oráculo y luego la concatenación: ver() añade a f con <<-, y en c(f, oraculo()) f ya estaría evaluada.
      if (length(m)) { casadas <- casadas + 1L; cuenta[n] <- cuenta[n] + 1L; r0 <- P[[n]]$f(m[-1L]); f <- c(f, r0) }
    }
    if (casadas != 1L) f <- c(f, "SOL_SIN_ORACULO")
  }
  fijas <- !vapply(P, function(p) is.na(p$n), TRUE)
  if (!identical(enc, c(SOL_ENC_INI, SOL_ENC_FIN)) || !identical(sort(lit), sort(SOL_LITERALES)) || refl != 1L || any(cuenta[fijas] != 1L) ||
      sum(cuenta[!fijas]) != 1L) f <- c(f, "SOL_MUDO")
  if (length(h6) != 2L || !isTRUE(h6[[1]]$g) || !isFALSE(h6[[2]]$g) || any(vapply(h6, function(h) h$plural != (h$n != 1L), TRUE)) ||
      length(vistos6) != 4L || anyDuplicated(vistos6) || !setequal(vistos6, e$opciones_p6)) f <- c(f, "SOL_P6")
  ltx <- attr(L, "latex")
  if (!length(ltx) || !all(ltx == SOL_LATEX) || length(ltx) != length(enc) + length(h6)) f <- c(f, "SOL_LATEX")
  f
}
sondas_solucion <- function(e, sol = CH_SOL, fbc = CH_FB, capas = c("plantilla", "texto", "feedback")) {
  x <- lectura_sol(e); if (is.null(x)) return("SOL_LECTURA")
  L <- texto_solucion(e, sol); L0 <- as.vector(L); esp <- esperado_sol(e, x); f <- character(0)
  out <- utils::capture.output(eval(fbc, new.env(parent = e))); items <- sub("^\\* ", "", grep("^\\* ", out, value = TRUE))
  if ("plantilla" %in% capas) f <- c(f, sondas_sol_plantilla(e, x, L), sonda_arit_sol(L0),
                                     if (any(grepl("VN-|CLAVE|Opci[oó]n [A-D]", c(L0, items)))) "SOL_CODIGO")
  if ("texto" %in% capas) {
    if (length(setdiff(esp$sol, L0)) || length(setdiff(L0, esp$sol)) || !setequal(e$reflexiones, SOL_REFL_P1) ||
        !setequal(e$reflexiones_cloze, SOL_REFL_CLOZE)) f <- c(f, "SOL_TEXTO") else if (!identical(L0, esp$sol)) f <- c(f, "SOL_ORDEN")
  }
  if ("feedback" %in% capas && !identical(items, esp$fb)) f <- c(f, "SOL_FEEDBACK")
  unique(f)
}
## I-3 deja fijos los distractores: lo que varía la retroalimentación son el otro error y las dos reflexiones.
sonda_variacion_sol <- function(ex = compila(DG_C), n = 100L) {
  o <- character(0); r1 <- character(0); r2 <- character(0)
  for (s in seq_len(n)) { e <- correr(s, ex); r1 <- c(r1, e$reflexion); r2 <- c(r2, e$reflexion_cloze)
    if (e$dat$d > e$dat$b - e$dat$a) o <- c(o, e$otro_error$codigo) }
  if (length(unique(o)) != 2L || length(unique(r1)) < 2L || length(unique(r2)) < 2L) "SOL_SIN_VARIACION" else character(0)
}
SOL_MUTANTES <- list(
  list(id = "S1_respuesta_con_otro_rotulo", sonda = "SOL_RESPUESTA", donde = "solucion", capas = "plantilla",
       cambios = list(c('textos_p1[k1], ": ", describe(reg_clave)', 'textos_p1[k1 %% 4L + 1L], ": ", describe(reg_clave)'))),
  list(id = "S2_imagen_de_un_distractor", sonda = "SOL_IMAGEN", donde = "solucion", capas = "plantilla",
       cambios = list(c('archivos[k1], ")", ANCHO', 'archivos[k1 %% 4L + 1L], ")", ANCHO'))),
  list(id = "S3_procedimiento_con_otra_cifra", sonda = "SOL_PROCEDIMIENTO", donde = "solucion", capas = "plantilla",
       cambios = list(c('mat(paste0(dat$c, " - ", z, " = ", reg$p23))', 'mat(paste0(dat$c + 1L, " - ", z, " = ", reg$p23 + 1L))'))),
  list(id = "S4_caso_con_otra_cifra", sonda = "SOL_CASO", donde = "solucion", capas = "plantilla",
       cambios = list(c("mat(sumatxt(reg$s1, reg$p12, reg$p13, reg$ctr))", "mat(sumatxt(reg$s1 + 1L, reg$p12, reg$p13, reg$ctr))"))),
  list(id = "S5_diagnosticos_con_rotulos_rotados", sonda = "SOL_OPCION", donde = "solucion", capas = "plantilla",
       cambios = list(c("aplicables[[k]]$codigo", "aplicables[[k %% 3L + 1L]]$codigo"))),
  list(id = "S6_circulo_con_otra_suma", sonda = "SOL_CIRCULO", donde = "dg", capas = "plantilla",
       cambios = list(c("mat(sumatxt(dat$P, dat$b, dat$a, dat$d))", "mat(sumatxt(dat$P + 1L, dat$b, dat$a, dat$d))")),
       materia = function(e) grepl(paste0("$", e$dat$P + 1L, " + "), e$errores_conceptuales[["VN-D-SIN-OPERAR"]]$descripcion_larga, fixed = TRUE)),
  list(id = "S7_otro_error_con_otra_cifra", sonda = "SOL_OTRO", donde = "dg", capas = "plantilla",
       cambios = list(c('se queda corto en ", dat$b - dat$a,', 'se queda corto en ", dat$b - dat$a + 1L,')),
       materia = function(e) identical(e$otro_error$codigo, "VN-X-CENTRO-DOBLE")),
  list(id = "S8_feedback_p1_invertido", sonda = "SOL_FEEDBACK", donde = "dg",
       cambios = list(c('fb_p1 <- paste0(textos_p1, ": ", feedback_p1)', 'fb_p1 <- paste0(textos_p1, ": ", rev(feedback_p1))')),
       materia = function(e) !identical(e$fb_p1, paste0(e$textos_p1, ": ", e$feedback_p1))),
  list(id = "S9_codigo_interno_visible", sonda = "SOL_CODIGO", donde = "dg", capas = "plantilla",
       cambios = list(c('z23 <- paste0(t2, " y ", t3, " sin ", t1)', 'z23 <- paste0("VN-D ", t2, " y ", t3, " sin ", t1)')),
       materia = function(e) startsWith(e$z23, "VN-D ")),
  list(id = "S10_parrafo_nuevo_con_cifras", sonda = "SOL_SIN_ORACULO", donde = "solucion", capas = "plantilla",
       cambios = list(c('encabezado("Parte 1 · Análisis del error")', 'cat("Hay ", dat$a, " casos.\\n\\n", sep = ""); encabezado("Parte 1 · Análisis del error")'))),
  list(id = "S11_parrafo_que_desaparece", sonda = "SOL_MUDO", donde = "solucion", capas = "plantilla",
       cambios = list(c('cat("Revisar solo los totales no basta:', 'if (FALSE) cat("Revisar solo los totales no basta:'))),
  list(id = "S12_afirmacion_sobre_otro_conjunto", sonda = "SOL_AFIRMACION", donde = "solucion", capas = "plantilla",
       cambios = list(c('falla los dos datos de ", t1, " y ", t2,', 'falla los dos datos de ", t1, " y ", t3,'))),
  list(id = "S13_letra_en_el_bloque_latex", sonda = "SOL_LATEX", donde = "solucion", capas = "plantilla",
       cambios = list(c('\\\\makeatother\\n", valla', '\\\\makeatother Opcion A\\n", valla'))),
  list(id = "S14_cuenta_falsa", sonda = "SOL_ARIT", tipo = "funcion", fn = function() sonda_arit_sol("Una cuenta falsa: $2 + 2 = 5$.")),
  list(id = "S15_otra_encuesta_con_otra_cifra", sonda = "SOL_OTRA_ENCUESTA", donde = "solucion", capas = "plantilla",
       cambios = list(c('mat(paste0(dato_q3$v, " - ", z2, " = ", exsol_p3))', 'mat(paste0(dato_q3$v + 1L, " - ", z2, " = ", exsol_p3 + 1L))'))),
  list(id = "S16_parte6_grupos_cruzados", sonda = "SOL_P6", donde = "solucion", capas = "plantilla", cambios = list(c("if (g) (if (n_g", "if (!g) (if (n_g"))),
  list(id = "S17_concordancia_ellos", sonda = "SOL_CONCORDANCIA", donde = "dg", capas = "plantilla",
       cambios = list(c('ellos2 <- if (fem2) "ellas" else "ellos"', 'ellos2 <- "ellos"')),
       materia = function(e) isTRUE(e$fem2) && identical(e$ellos2, "ellos")),
  list(id = "S18_trampa_con_otro_conjunto", sonda = "SOL_TEXTO", donde = "solucion",
       cambios = list(c('" que ", verbo, " ", t1, " y ", t2, " incluye a quienes también ", verbo, " ", t3,', '" que ", verbo, " ", t1, " y ", t3, " incluye a quienes también ", verbo, " ", t2,'))),
  list(id = "S19_diagnosticos_en_otro_orden", sonda = "SOL_ORDEN", donde = "solucion",
       cambios = list(c("for (k in seq_along(aplicables)) {", "for (k in rev(seq_along(aplicables))) {"))),
  list(id = "S20_reflexion_cloze_sin_sortear", sonda = "SOL_SIN_VARIACION", donde = "dg", tipo = "variacion",
       cambios = list(c("reflexion_cloze <- pick(reflexiones_cloze)", "reflexion_cloze <- reflexiones_cloze[1L]"))),
  list(id = "S21_ancho_sin_important_en_la_solucion", sonda = "SOL_IMAGEN", donde = "dg", capas = "plantilla",
       cambios = list(c("width:280px!important;max-width", "width:280px;max-width")),
       materia = function(e) !grepl("!important", e$ANCHO, fixed = TRUE)),
  ## FASE 2C r2, H2 (b): una prueba propia para cada plantilla de la otra encuesta y para cada componente de AFIRMACION y CONCORDANCIA.
  list(id = "S22_parte5_un_sumando_mas_uno", sonda = "SOL_OTRA_ENCUESTA", donde = "solucion", capas = "plantilla",
       cambios = list(c('sumatxt(r2[["s1"]], r2[["p12"]], r2[["s2"]], r2[["ctr"]], r2[["p13"]], r2[["p23"]], r2[["s3"]]), ".\\n\\n", sep = "")',
                        'sumatxt(r2[["s1"]] + 1L, r2[["p12"]], r2[["s2"]], r2[["ctr"]], r2[["p13"]], r2[["p23"]], r2[["s3"]]), ".\\n\\n", sep = "")'))),
  list(id = "S23_parte5_total_mas_uno", sonda = "SOL_OTRA_ENCUESTA", donde = "solucion", capas = "plantilla",
       cambios = list(c('sumatxt(r2[["s1"]], r2[["p12"]], r2[["s2"]], r2[["ctr"]], r2[["p13"]], r2[["p23"]], r2[["s3"]]), ".\\n\\n", sep = "")',
                        'sub("= [0-9]+$", paste("=", sum(r2) + 1L), sumatxt(r2[["s1"]], r2[["p12"]], r2[["s2"]], r2[["ctr"]], r2[["p13"]], r2[["p23"]], r2[["s3"]])), ".\\n\\n", sep = "")'))),
  list(id = "S24_parte5_una_zona_omitida", sonda = "SOL_OTRA_ENCUESTA", donde = "solucion", capas = "plantilla",
       cambios = list(c('sumatxt(r2[["s1"]], r2[["p12"]], r2[["s2"]], r2[["ctr"]], r2[["p13"]], r2[["p23"]], r2[["s3"]]), ".\\n\\n", sep = "")',
                        'sumatxt(r2[["s1"]], r2[["p12"]], r2[["s2"]], r2[["ctr"]], r2[["p13"]], r2[["p23"]]), ".\\n\\n", sep = "")'))),
  list(id = "S25_trampa_pero_no_otro_conjunto", sonda = "SOL_AFIRMACION", donde = "solucion", capas = "plantilla",
       cambios = list(c('La trampa está en la frase «pero no ", t3,', 'La trampa está en la frase «pero no ", t2,'))),
  list(id = "S26_restados_solo_en_la_solucion", sonda = "SOL_CONCORDANCIA", donde = "solucion", capas = "plantilla",
       cambios = list(c("cap1(cuenta_q4$txt)", 'cap1(sub("restadas", "restados", cuenta_q4$txt, fixed = TRUE))'))),
  list(id = "S27_ellos_solo_en_la_solucion", sonda = "SOL_CONCORDANCIA", donde = "solucion", capas = "plantilla",
       cambios = list(c('" de ", ellos2, " no ", verbo2,', '" de ", "ellos", " no ", verbo2,'))),
  ## FASE 2C r2, H2 (c): un párrafo estático en la Solution, fuera de los chunks.
  list(id = "S28_parrafo_estatico_en_la_solucion", sonda = "SOL_ESTATICO", tipo = "estatico",
       cambios = list(c('```{r feedback, echo = FALSE, results = "asis"}', 'Un párrafo estático que ningún chunk emite.\n\n```{r feedback, echo = FALSE, results = "asis"}'))))
## FASE 2C r2, H2 (c): la sección solucion ejecuta los chunks, así que no vería un párrafo estático entre «Solution» y
## «Meta-information», fuera de ellos. Ahí solo puede haber líneas en blanco.
sonda_estatico <- function(lin = lineas) {
  i0 <- which(lin == "Solution"); i1 <- which(lin == "Meta-information")
  if (length(i0) != 1L || length(i1) != 1L || i1 < i0 + 2L) return("SOL_ESTATICO")
  dentro <- FALSE; malas <- 0L
  for (l in lin[(i0 + 2L):(i1 - 1L)]) {
    if (!dentro && grepl("^```\\{r ", l)) { dentro <- TRUE; next }
    if (dentro) { if (l == "```") dentro <- FALSE; next }
    if (nzchar(trimws(l))) malas <- malas + 1L
  }
  if (malas > 0L || dentro) "SOL_ESTATICO" else character(0)
}
## FASE 2C r2, H2 (a): «cazado por su sonda y otra» solo cuenta si el mutante declara cuál es la otra (campo otra).
aceptado <- function(v, M) v == "cazado_por_su_sonda" || (v == "cazado_por_su_sonda_y_otra" && !is.null(M$otra))
correr_mutante_sol <- function(M, ex0 = compila(DG_C)) {
  if (identical(M$tipo, "funcion")) { cod <- M$fn(); return(list(v = veredicto(cod, M$sonda), cod = cod, sem = "-")) }
  if (identical(M$tipo, "estatico")) {
    l <- mutar(lineas, M$cambios[[1]][1], M$cambios[[1]][2]); if (!is.null(attr(l, "mal"))) return(list(v = "mal_construido", cod = attr(l, "mal"), sem = "-"))
    cod <- sonda_estatico(unlist(strsplit(l, "\n", fixed = TRUE))); base <- sonda_estatico(lineas)
    return(list(v = if (length(base)) "mal_construido" else veredicto(cod, M$sonda), cod = cod, sem = "-"))
  }
  dg <- DG_C; sol <- chunk_de(lineas, "solucion")
  for (cb in M$cambios) {
    if (M$donde == "dg") dg <- mutar(dg, cb[1], cb[2]) else sol <- mutar(sol, cb[1], cb[2])
    mal <- attr(if (M$donde == "dg") dg else sol, "mal"); if (!is.null(mal)) return(list(v = "mal_construido", cod = mal, sem = "-"))
  }
  ex <- compila(dg); exs <- compila(sol); capas <- if (is.null(M$capas)) c("plantilla", "texto", "feedback") else M$capas
  if (identical(M$tipo, "variacion")) {
    cod <- sonda_variacion_sol(ex); base <- sonda_variacion_sol(ex0)
    return(list(v = if (length(base)) "mal_construido" else veredicto(cod, M$sonda), cod = cod, sem = "1..100"))
  }
  for (s in 1:600) {
    e <- tryCatch(correr(s, ex), error = function(x) NULL); if (is.null(e)) next
    cod <- sondas_solucion(e, exs, CH_FB, capas)
    if (length(cod)) {
      mat_ok <- is.null(M$materia) || isTRUE(M$materia(e))
      base <- sondas_solucion(correr(s, ex0), CH_SOL, CH_FB, capas)          # obligación 8: la misma semilla sin mutar
      return(list(v = if (!mat_ok || length(base)) "mal_construido" else veredicto(cod, M$sonda), cod = cod, sem = s))
    }
  }
  list(v = "no_detectado", cod = character(0), sem = "-")
}
sec_solucion <- function() {
  cat("\n[solucion] I-4.7: chunks solucion y feedback en las cuotas de texto (100 variantes y 20 canónicas por rama de las Partes 3 y 4)\n")
  ex <- compila(DG_C); ramas <- as.vector(outer(c("p13", "p23"), c("s1", "s2", "s3"), paste))
  cuenta <- matrix(0L, 2L, 6L, dimnames = list(c("variante", "canonica"), ramas)); cuota <- c(variante = 100L, canonica = 20L)
  malos <- character(0); s <- 0L; otro <- character(0); t0 <- proc.time()[["elapsed"]]
  ## La rama se decide antes de juzgar y no depende del resultado: la muestra son las primeras 100 (o 20) versiones de cada rama.
  while (any(cuenta < cuota) && s < 20000L) {
    s <- s + 1L; e <- tryCatch(correr(s, ex), error = function(x) NULL)
    if (is.null(e)) { malos <- c(malos, sprintf("semilla %d: GUARDA_RMD", s)); next }
    est <- if (isTRUE(e$es_canonica)) "canonica" else "variante"; rama <- paste(e$q3, e$q4)
    if (cuenta[est, rama] >= cuota[[est]]) next
    cod <- sondas_solucion(e)
    if (length(cod)) malos <- c(malos, sprintf("semilla %d: %s", s, paste(cod, collapse = ",")))
    cuenta[est, rama] <- cuenta[est, rama] + 1L; otro <- c(otro, e$otro_error$codigo)
  }
  print(cuenta)
  cat(sprintf("  semillas 1..%d · %d versiones juzgadas en %.0f s · con algún código: %d · otro error emitido: %s\n", s, sum(cuenta),
              proc.time()[["elapsed"]] - t0, length(malos), paste(names(table(otro)), table(otro), collapse = ", ")))
  if (any(cuenta < 20L)) cat("  NO CONCLUYENTE: hay ramas con n < 20\n")
  v <- sonda_variacion_sol(ex); if (length(v)) malos <- c(malos, v)
  est <- sonda_estatico(); if (length(est)) malos <- c(malos, est)
  if (length(malos) || any(cuenta < cuota)) rojo("solucion", paste(c(head(malos, 3), if (any(cuenta < cuota)) "cuotas sin llenar"), collapse = "; ")) else
    cat("  sin fallas: cada párrafo casa con su plantilla, coincide letra a letra y está en su sección; la retroalimentación de los seis gaps es la reconstruida; varían el otro error y las dos reflexiones\n")
  ## Mutantes (INC-MUTANTE-SONDA): en memoria, hasta la primera muerte (tope 600), con línea base sin mutar en esa semilla.
  res <- character(0); acept <- logical(0)
  for (M in SOL_MUTANTES) {
    r <- correr_mutante_sol(M, ex)
    cat(sprintf("  %-42s sonda esperada %-18s capas %-24s semilla %-7s %s · %s\n", M$id, M$sonda,
                if (is.null(M$capas)) "todas" else paste(M$capas, collapse = "+"), r$sem, r$v, if (length(r$cod)) substr(paste(r$cod, collapse = ","), 1, 80) else "-"))
    res <- c(res, r$v); acept <- c(acept, aceptado(r$v, M))
  }
  cat(sprintf("  mutantes: %d de %d cazados solo por su sonda\n", sum(res == "cazado_por_su_sonda"), length(res)))
  if (!all(acept)) rojo("solucion", "algún mutante no murió solo por su sonda, o murió también por otra sin declararla")
}

## --- papel (I-5; FASE 2C r1, H2) ---------------------------------------------------------------------------------------
## Port de la sección papel de ../verificar_espacio.R. El ítem se imprime en hojas de 216 x 330 mm con ../solpcielo_cloze.tex
## (restricción 5). En cada PDF real: el tamaño de la hoja (pdfinfo); las cuatro figuras de la Parte 1 en una misma página y
## cinco imágenes en total (pdfimages -list; la quinta es la de la Solution); y nada fuera de la caja de texto en ninguna página,
## también las de la Solution: cada página se rasteriza a 40 ppp y las franjas de los cuatro márgenes, menos 1,5 mm, quedan en
## blanco. Así cuentan el texto y las imágenes. N = 100: la canónica, un contexto de cada tipo y los textos de la Parte 1 más
## largos, que son los que empujan las figuras hacia abajo.
## Códigos: PAPEL_RENDER (no compiló; falla cerrado), PAPEL_TAMANO, PAPEL_FIGURAS, PAPEL_MARGEN, PAPEL_COMO_MARCAR (la frase
## «Cómo marcar» de la portada no está en la página 1) y, con varias preguntas, PAPEL_FIGURAS_VARIAS.
PAPEL_TPL <- file.path(SUB, "solpcielo_cloze.tex"); PAPEL_HOJA_MM <- c(216, 330); PAPEL_TOL_MM <- 1.5
PAPEL_MARGEN_MM <- c(izq = 10, der = 10, sup = 15, inf = 20)      # los de \geometry en la plantilla
## La franja superior empieza bajo el encabezado de la plantilla («Saber ICFES Matemáticas: MicroSimulacro» y el número de
## página, de 3 a 6,3 mm), que va en el margen por diseño.
PAPEL_ENCABEZADO_MM <- 7.5
## Decisión 1 b del profesor tras la tanda 3: la portada de las dos plantillas del CLOZE explica cómo marcar las partes con opciones.
COMO_MARCAR <- "Cómo marcar: en cada parte con opciones separadas por «/»"
como_marcar_en_portada <- function(pdf) {
  p1 <- paste(system2("pdftotext", c("-f", "1", "-l", "1", shQuote(pdf), "-"), stdout = TRUE), collapse = " ")
  grepl(COMO_MARCAR, gsub("\\s+", " ", p1), fixed = TRUE)
}
tinta_fuera <- function(png_file, hoja_mm) {
  m <- png::readPNG(png_file); if (length(dim(m)) == 3L) m <- m[, , 1L]
  W <- ncol(m); H <- nrow(m); px <- W / hoja_mm[1]; tinta <- function(x) sum(x < 0.8)
  x1 <- floor((PAPEL_MARGEN_MM[["izq"]] - PAPEL_TOL_MM) * px); x2 <- ceiling((hoja_mm[1] - PAPEL_MARGEN_MM[["der"]] + PAPEL_TOL_MM) * px)
  y0 <- ceiling(PAPEL_ENCABEZADO_MM * px); y1 <- floor((PAPEL_MARGEN_MM[["sup"]] - PAPEL_TOL_MM) * px); y2 <- ceiling((hoja_mm[2] - PAPEL_MARGEN_MM[["inf"]] + PAPEL_TOL_MM) * px)
  tinta(m[, seq_len(x1)]) + (if (x2 <= W) tinta(m[, x2:W]) else 0L) + (if (y1 >= y0) tinta(m[y0:y1, ]) else 0L) + (if (y2 <= H) tinta(m[y2:H, ]) else 0L)
}
sondas_papel <- function(semilla, rmd = RMD, tpl = PAPEL_TPL) {
  d <- tempfile("papel_"); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  ## Desde una copia de ruta corta: R/exams arma un nombre de archivo con toda la ruta.
  copia <- file.path(d, "c.Rmd"); file.copy(rmd, copia, overwrite = TRUE)
  ok <- tryCatch({ suppressMessages(exams2pdf(copia, n = 1L, seed = semilla, name = "x", dir = d, template = tpl, encoding = "UTF-8", quiet = TRUE)); TRUE },
                 error = function(x) conditionMessage(x))
  pdf <- file.path(d, "x1.pdf")
  if (!isTRUE(ok) || !file.exists(pdf)) return(list(fallas = "PAPEL_RENDER", detalle = as.character(ok), figuras = integer(0), fuera = NA, hoja = c(NA, NA)))
  mm <- 25.4 / 72; f <- character(0)
  tl <- grep("^Page size", system2("pdfinfo", shQuote(pdf), stdout = TRUE), value = TRUE)
  hoja <- as.numeric(regmatches(tl, gregexpr("[0-9.]+", tl))[[1L]])[1:2] * mm
  if (any(abs(hoja - PAPEL_HOJA_MM) > 0.5)) f <- c(f, "PAPEL_TAMANO")
  li <- system2("pdfimages", c("-list", shQuote(pdf)), stdout = TRUE)[-(1:2)]
  pg <- as.integer(vapply(strsplit(trimws(li), " +"), `[`, "", 1L))
  if (length(pg) != 5L || length(unique(pg[1:4])) != 1L) f <- c(f, "PAPEL_FIGURAS")
  marcar <- como_marcar_en_portada(pdf); if (!marcar) f <- c(f, "PAPEL_COMO_MARCAR")
  system2("pdftoppm", c("-r", "40", "-gray", "-png", shQuote(pdf), shQuote(file.path(d, "p"))))
  pngs <- sort(list.files(d, "^p-[0-9]+\\.png$", full.names = TRUE))
  fuera <- vapply(pngs, tinta_fuera, 0, hoja_mm = hoja)
  if (!length(pngs) || any(fuera > 0)) f <- c(f, "PAPEL_MARGEN")
  list(fallas = f, figuras = pg, fuera = unname(fuera), hoja = hoja, marcar = marcar)
}
semillas_papel <- function(ex = compila(DG_C)) {
  I <- do.call(rbind, lapply(1:600, function(s) { e <- correr(s, ex)
    data.frame(s = s, tipo = if (isTRUE(e$es_canonica)) "canonica" else e$ctx$tipo, largo = nchar(paste(e$enunciado, e$vineta_1, e$vineta_2, e$vineta_3))) }))
  primeras <- I$s[!duplicated(I$tipo)]
  c(primeras, setdiff(I$s[order(-I$largo)], primeras))[1:100]   # N = 100, la canónica y un contexto de cada tipo primero
}
PAPEL_MUTANTES <- list(
  list(id = "PP1_hoja_de_356_mm", sonda = "PAPEL_TAMANO", tpl = c("papersize={216mm,330mm}", "papersize={216mm,356mm}")),
  list(id = "PP2_salto_de_pagina_entre_las_filas", sonda = "PAPEL_FIGURAS",
       rmd = c('cat(img_p1(1L), " ", img_p1(2L), "\\n\\n", sep = "")', 'cat(img_p1(1L), " ", img_p1(2L), "\\n\\n", bt, "\\\\newpage", bt, "{=latex}\\n\\n", sep = "")')),
  list(id = "PP3_cifra_sin_cortes_en_el_enunciado", sonda = "PAPEL_MARGEN",
       rmd = c('cat(enunciado, "\\n\\n", sep = "")', 'cat(enunciado, " ", strrep("0123456789", 20), "\\n\\n", sep = "")')))
PAPEL_TPL_VARIAS <- file.path(aqui, "solpcielo_cloze_una_por_pagina.tex")   # decisión 1 b del profesor tras la tanda 3
## Medición común de un PDF de varias preguntas (secciones papel y uso): hoja, páginas de las imágenes (cinco por pregunta: las
## cuatro de la Parte 1 y la de la Solution), preguntas con sus cuatro figuras en más de una página y «Cómo marcar» en la portada.
medir_pdf_varias <- function(pdf, nq) {
  tl <- grep("^Page size", system2("pdfinfo", shQuote(pdf), stdout = TRUE), value = TRUE)
  hoja <- as.numeric(regmatches(tl, gregexpr("[0-9.]+", tl))[[1L]])[1:2] * 25.4 / 72
  li <- system2("pdfimages", c("-list", shQuote(pdf)), stdout = TRUE)[-(1:2)]
  pg <- as.integer(vapply(strsplit(trimws(li), " +"), `[`, "", 1L))
  partidas <- if (length(pg) == 5L * nq) sum(vapply(seq_len(nq) - 1L, function(q) length(unique(pg[q * 5L + 1:4])) != 1L, TRUE)) else NA_integer_
  list(hoja = hoja, paginas = pg, partidas = partidas, marcar = como_marcar_en_portada(pdf))
}
sondas_papel_varias <- function(s, rmd = RMD, tpl = PAPEL_TPL_VARIAS) {
  d <- tempfile("papel_v_"); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  copia <- file.path(d, "c.Rmd"); file.copy(rmd, copia, overwrite = TRUE)
  ok <- tryCatch({ suppressMessages(exams2pdf(rep(copia, 3L), n = 1L, seed = matrix(c(s, s + 100L, s + 200L), 1L, 3L), name = "x", dir = d,
                                              template = tpl, encoding = "UTF-8", quiet = TRUE)); TRUE }, error = function(x) conditionMessage(x))
  pdf <- file.path(d, "x1.pdf")
  if (!isTRUE(ok) || !file.exists(pdf)) return(list(fallas = "PAPEL_RENDER", partidas = NA))
  m <- medir_pdf_varias(pdf, 3L); f <- character(0)
  if (any(abs(m$hoja - PAPEL_HOJA_MM) > 0.5)) f <- c(f, "PAPEL_TAMANO")
  if (is.na(m$partidas) || m$partidas > 0L) f <- c(f, "PAPEL_FIGURAS_VARIAS")
  if (!m$marcar) f <- c(f, "PAPEL_COMO_MARCAR")
  list(fallas = f, partidas = m$partidas, paginas = m$paginas, hoja = m$hoja, marcar = m$marcar)
}
sec_papel <- function() {
  s <- semillas_papel(); t0 <- proc.time()[["elapsed"]]
  cat(sprintf("\n[papel] I-5: N = %d PDF reales con %s: la canónica, un contexto de cada tipo y los textos de la Parte 1 más largos\n", length(s), basename(PAPEL_TPL)))
  res <- parallel::mclapply(s, sondas_papel, mc.cores = min(8L, parallel::detectCores()))
  muertos <- vapply(res, function(r) is.null(r) || inherits(r, "try-error"), TRUE)
  if (any(muertos)) rojo("papel", paste(sum(muertos), "procesos sin resultado"))
  r <- res[!muertos]; mal <- unlist(lapply(r, `[[`, "fallas"))
  juntas <- vapply(r, function(x) length(x$figuras) == 5L && length(unique(x$figuras[1:4])) == 1L, TRUE)
  pag <- unlist(lapply(r, function(x) if (length(x$figuras) >= 4L) x$figuras[1] else NULL))
  cat(sprintf("  hoja %s mm · cuatro figuras en una misma página: %d de %d (página %s) · píxeles con tinta fuera de la caja: %d · «Cómo marcar» en la portada: %d de %d · %.0f s\n",
              paste(round(r[[1]]$hoja), collapse = " x "), sum(juntas), length(r), paste(sort(unique(pag)), collapse = ","),
              sum(unlist(lapply(r, `[[`, "fuera")), na.rm = TRUE), sum(vapply(r, function(x) isTRUE(x$marcar), TRUE)), length(r),
              proc.time()[["elapsed"]] - t0))
  if (length(mal)) rojo("papel", paste(names(table(mal)), table(mal), collapse = ", "))
  ## Mutantes: una copia del .Rmd o de la plantilla en tempdir(); se renderiza la primera semilla de la lista, la canónica,
  ## cuya línea base es la medición de arriba (obligación 8).
  base <- if (!muertos[1]) res[[1]]$fallas else "SIN_BASE"
  for (M in PAPEL_MUTANTES) {
    l <- lineas; tp <- readLines(PAPEL_TPL, encoding = "UTF-8")
    if (!is.null(M$rmd)) l <- mutar(l, M$rmd[1], M$rmd[2]); if (!is.null(M$tpl)) tp <- mutar(tp, M$tpl[1], M$tpl[2])
    mal_c <- c(attr(l, "mal"), attr(tp, "mal"))
    if (length(mal_c)) { cat(sprintf("  %-40s mal_construido (%s)\n", M$id, mal_c[1])); rojo("papel", paste(M$id, "mal construido")); next }
    d <- tempfile("mut_papel_"); dir.create(d); f1 <- file.path(d, basename(RMD)); f2 <- file.path(d, "plantilla.tex")
    writeLines(l, f1, useBytes = TRUE); writeLines(tp, f2, useBytes = TRUE)
    rm_ <- sondas_papel(s[1], f1, f2); unlink(d, recursive = TRUE)
    v <- if (length(base)) "mal_construido" else veredicto(rm_$fallas, M$sonda)
    cat(sprintf("  %-40s sonda esperada %-14s semilla %-4d %s · %s\n", M$id, M$sonda, s[1], v, if (length(rm_$fallas)) paste(rm_$fallas, collapse = ",") else "-"))
    if (!aceptado(v, M)) rojo("papel", paste(M$id, v))
  }
  ## FASE 2C r2, H1, y decisión 1 b del profesor tras la tanda 3: la I-5 rige la Parte 1 y fija la condición de uso para varias
  ## preguntas: exams2pdf solo con la plantilla local solpcielo_cloze_una_por_pagina.tex, copia de ../solpcielo_cloze.tex con
  ## \clearpage al empezar cada pregunta. Con ../solpcielo_cloze.tex, la segunda de tres preguntas parte sus cuatro figuras en dos
  ## páginas (5 de 5 en la r2). 34 PDF de tres preguntas (102 preguntas, familia B de la regla #23); en cada uno, la hoja, las cuatro
  ## figuras de cada pregunta en una misma página y el «Cómo marcar» de la portada.
  t1 <- proc.time()[["elapsed"]]
  rv <- parallel::mclapply(1:34, sondas_papel_varias, mc.cores = min(8L, parallel::detectCores()))
  okv <- !vapply(rv, function(x) is.null(x) || inherits(x, "try-error"), TRUE)
  malv <- unlist(lapply(rv[okv], `[[`, "fallas")); part <- sum(unlist(lapply(rv[okv], `[[`, "partidas")), na.rm = TRUE)
  cat(sprintf("  varias preguntas (I-5, condición de uso): %d PDF de tres preguntas con %s (%d preguntas) · preguntas con sus cuatro figuras partidas: %d · «Cómo marcar» en la portada: %d de %d · hoja %s mm · %.0f s\n",
              sum(okv), basename(PAPEL_TPL_VARIAS), 3L * sum(okv), part, sum(vapply(rv[okv], function(x) isTRUE(x$marcar), TRUE)), sum(okv),
              paste(unique(vapply(rv[okv], function(x) paste(round(x$hoja), collapse = " x "), "")), collapse = "; "), proc.time()[["elapsed"]] - t1))
  if (any(!okv)) rojo("papel", paste(sum(!okv), "procesos sin resultado (varias preguntas)"))
  if (length(malv)) rojo("papel", paste(names(table(malv)), table(malv), collapse = ", "))
  ## PP4: con ../solpcielo_cloze.tex las figuras se parten; debe morir por PAPEL_FIGURAS_VARIAS. PP5: la plantilla de varias preguntas
  ## sin la línea «Cómo marcar»; debe morir por PAPEL_COMO_MARCAR. Semillas 1, 101 y 201: el primer PDF del lote, su línea base.
  base4 <- if (okv[1]) rv[[1]]$fallas else "SIN_BASE"
  sin_marcar <- mutar(readLines(PAPEL_TPL_VARIAS, encoding = "UTF-8"), "\\noindent\\textbf{Cómo marcar:}", "%% \\noindent\\textbf{Cómo marcar:}")
  for (M in list(list(id = "PP4_varias_preguntas_con_la_del_cloze", sonda = "PAPEL_FIGURAS_VARIAS", tpl = PAPEL_TPL),
                 list(id = "PP5_varias_preguntas_sin_como_marcar", sonda = "PAPEL_COMO_MARCAR", lineas = sin_marcar))) {
    if (!is.null(M$lineas) && !is.null(attr(M$lineas, "mal"))) {
      cat(sprintf("  %-40s mal_construido (%s)\n", M$id, attr(M$lineas, "mal"))); rojo("papel", paste(M$id, "mal construido")); next }
    tp <- M$tpl
    if (is.null(tp)) { tp <- tempfile("tpl_", fileext = ".tex"); writeLines(M$lineas, tp, useBytes = TRUE) }
    rm_ <- sondas_papel_varias(1L, RMD, tp)
    v <- if (length(base4)) "mal_construido" else veredicto(rm_$fallas, M$sonda)
    cat(sprintf("  %-40s sonda esperada %-20s semillas 1,101,201 %s · %s\n", M$id, M$sonda, v, if (length(rm_$fallas)) paste(rm_$fallas, collapse = ",") else "-"))
    if (v != "cazado_por_su_sonda") rojo("papel", paste(M$id, v))
  }
}

## --- pantalla (I-5 a 320 y 360 px; FASE 2C r1, H2) -------------------------------------------------------------------
## HTML real de exams2html (familia B de la regla #23, N = 100, semillas 1..100), medido en Chromium por CDP (chromote) como
## .claude/scripts/render_html_shots.R: Emulation.setDeviceMetricsOverride deja el ancho pedido como viewport CSS, y cada medición
## comprueba window.innerWidth == W (con --window-size < 500, Chromium 156 carga la página a 500 px; si no coincide, falla cerrado).
## En cada ancho: sin desborde horizontal; cada figura (las cuatro de la Parte 1 y la de la Solution) mide min(280 px, el ancho
## disponible) — sin !important quedaría en unos 208 px (I-5); cada rótulo «Diagrama N», en orden, encima de su figura; y las
## cuatro figuras sin solaparse. Prueba la maqueta, no que los números se lean: eso lo mira la 6b.
## Códigos: PANT_VIEWPORT y PANT_LECTURA (medición inválida; fallan cerrado), PANT_DESBORDE, PANT_ANCHO, PANT_ROTULO, PANT_SOLAPE.
PANT_ANCHOS <- c(320L, 360L); PANT_IMG_PX <- 280
PANT_JS <- paste0(
  "(function(){var R=function(el){var b=el.getBoundingClientRect();return {l:b.left,r:b.right,t:b.top,b:b.bottom,w:b.width};};",
  "var disp=function(el){var cs=getComputedStyle(el);return el.clientWidth-parseFloat(cs.paddingLeft)-parseFloat(cs.paddingRight);};",
  "var im=Array.prototype.slice.call(document.querySelectorAll('img'));",
  "var q=im.filter(function(i){return i.getAttribute('alt')==='Diagrama de Venn';});",
  "var s=im.filter(function(i){return i.getAttribute('alt')==='Diagrama de Venn de la respuesta correcta';});",
  "var fig=q.map(function(i){var sp=i.parentElement,st=sp?sp.querySelector('strong'):null,pc=sp?sp.parentElement:null,ms=sp?getComputedStyle(sp):null;",
  "return {img:R(i),ok:i.complete&&i.naturalWidth>0,rot:st?st.textContent:null,rrot:st?R(st):null,",
  "disp:pc?disp(pc)-parseFloat(ms.marginLeft)-parseFloat(ms.marginRight):-1};});",
  "var sol=s.map(function(i){return {img:R(i),ok:i.complete&&i.naturalWidth>0,disp:disp(i.parentElement)};});",
  "return JSON.stringify({iw:window.innerWidth,sw:document.documentElement.scrollWidth,fig:fig,sol:sol});})()")
pant_abrir <- function() {
  br <- Sys.which("chromium"); if (!nzchar(br) || !requireNamespace("chromote", quietly = TRUE)) return(NULL)
  prof <- tempfile("pant_"); dir.create(prof)
  ctl <- chromote::Chromote$new(browser = chromote::Chrome$new(path = br, args = c("--no-sandbox", "--disable-gpu", "--hide-scrollbars",
                                "--force-device-scale-factor=1", paste0("--user-data-dir=", prof))))
  list(ctl = ctl, ses = ctl$new_session(width = 1024L, height = 3000L, wait_ = TRUE), prof = prof)
}
pant_cerrar <- function(b) { try(b$ctl$close(), silent = TRUE); unlink(b$prof, recursive = TRUE, force = TRUE) }
pant_emular <- function(ses, w) for (mob in c(TRUE, FALSE))       # dos pasos, como render_html_shots.R
  ses$Emulation$setDeviceMetricsOverride(width = w, height = 3000L, deviceScaleFactor = 1, mobile = mob, wait_ = TRUE)
pant_medir <- function(ses, html, w) {
  pant_emular(ses, w)
  ses$go_to(paste0("file://", utils::URLencode(normalizePath(html), reserved = FALSE)), delay = 0.1, wait_ = TRUE)
  v <- tryCatch(ses$Runtime$evaluate(PANT_JS, returnByValue = TRUE, wait_ = TRUE)$result$value, error = function(x) NULL)
  if (is.null(v)) NULL else jsonlite::fromJSON(v, simplifyVector = FALSE)
}
pant_juzgar <- function(m, w) {
  if (is.null(m) || !identical(as.integer(m$iw), as.integer(w))) return("PANT_VIEWPORT")
  if (length(m$fig) != 4L || length(m$sol) != 1L || !all(vapply(c(m$fig, m$sol), function(x) isTRUE(x$ok), TRUE)) ||
      any(vapply(m$fig, function(x) is.null(x$rot) || is.null(x$rrot), TRUE))) return("PANT_LECTURA")
  f <- character(0); ims <- c(lapply(m$fig, `[[`, "img"), list(m$sol[[1]]$img))
  if (m$sw > m$iw || any(vapply(ims, function(b) b$r > m$iw + 0.5 || b$l < -0.5, TRUE))) f <- c(f, "PANT_DESBORDE")
  esp <- pmin(PANT_IMG_PX, c(vapply(m$fig, function(x) as.numeric(x$disp), 0), as.numeric(m$sol[[1]]$disp)))
  if (any(abs(vapply(ims, function(b) as.numeric(b$w), 0) - esp) > 1.5)) f <- c(f, "PANT_ANCHO")
  rot_ok <- vapply(seq_along(m$fig), function(i) { x <- m$fig[[i]]
    identical(x$rot, paste("Diagrama", c("I", "II", "III", "IV")[i])) && x$rrot$b <= x$img$t + 1 && x$rrot$l >= x$img$l - 1 && x$rrot$l < x$img$r }, TRUE)
  if (!all(rot_ok)) f <- c(f, "PANT_ROTULO")
  b <- lapply(m$fig, `[[`, "img"); sol <- FALSE
  for (i in 1:3) for (j in (i + 1L):4) if (min(b[[i]]$r, b[[j]]$r) - max(b[[i]]$l, b[[j]]$l) > 1 && min(b[[i]]$b, b[[j]]$b) - max(b[[i]]$t, b[[j]]$t) > 1) sol <- TRUE
  if (sol) f <- c(f, "PANT_SOLAPE")
  f
}
pant_html <- function(rmd, semillas, dir) {
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  ok <- tryCatch({ suppressMessages(exams2html(rmd, n = length(semillas), seed = semillas, dir = dir, name = "p")); TRUE }, error = function(x) conditionMessage(x))
  if (!isTRUE(ok)) return(structure(character(0), error = ok))
  file.path(dir, paste0("p", seq_along(semillas), ".html"))
}
## Por ancho y por versión: la medición y sus códigos.
pant_todo <- function(ses, htmls) lapply(PANT_ANCHOS, function(w) lapply(htmls, function(h) { m <- pant_medir(ses, h, w); list(m = m, cod = pant_juzgar(m, w)) }))
PANT_MUTANTES <- list(
  list(id = "PM1_ancho_sin_important", sonda = "PANT_ANCHO", cambios = list(c("width:280px!important;max-width", "width:280px;max-width"))),
  list(id = "PM2_cifra_sin_cortes_en_el_enunciado", sonda = "PANT_DESBORDE",
       cambios = list(c('cat(enunciado, "\\n\\n", sep = "")', 'cat(enunciado, " ", strrep("0123456789", 6), "\\n\\n", sep = "")'))),
  list(id = "PM3_rotulo_debajo_de_su_figura", sonda = "PANT_ROTULO",
       cambios = list(c("display:inline-block;vertical-align:top", "display:inline-flex;flex-direction:column-reverse;vertical-align:top"))),
  list(id = "PM4_figuras_solapadas", sonda = "PANT_SOLAPE", cambios = list(c("margin:0 16px 12px 0", "margin:0 16px -150px 0"))),
  ## La regresión de la pasada 2: la suma de la Parte 5 vuelve a MathML y a 320 px se sale de la pantalla.
  list(id = "PM5_suma_de_la_parte5_en_matematicas", sonda = "PANT_DESBORDE",
       cambios = list(c('sumatxt(r2[["s1"]], r2[["p12"]], r2[["s2"]], r2[["ctr"]], r2[["p13"]], r2[["p23"]], r2[["s3"]]), ".\\n\\n", sep = "")',
                        'mat(sumatxt(r2[["s1"]], r2[["p12"]], r2[["s2"]], r2[["ctr"]], r2[["p13"]], r2[["p23"]], r2[["s3"]])), ".\\n\\n", sep = "")'))))
sec_pantalla <- function() {
  N <- 100L; t0 <- proc.time()[["elapsed"]]
  cat(sprintf("\n[pantalla] I-5 a %s px: exams2html N = %d (semillas 1..%d) medido en Chromium por CDP; familia B de la regla #23\n",
              paste(PANT_ANCHOS, collapse = " y "), N, N))
  b <- pant_abrir(); if (is.null(b)) { rojo("pantalla", "sin chromium o sin chromote: no se puede medir"); return(invisible()) }
  on.exit(pant_cerrar(b), add = TRUE)
  hs <- pant_html(RMD, seq_len(N), file.path(tempdir(), "pant_real"))
  if (!length(hs)) { rojo("pantalla", paste("exams2html falló:", attr(hs, "error"))); return(invisible()) }
  real <- pant_todo(b$ses, hs)
  for (k in seq_along(PANT_ANCHOS)) {
    ck <- lapply(real[[k]], `[[`, "cod"); malos <- which(lengths(ck) > 0L)
    w4 <- unlist(lapply(real[[k]], function(x) if (!is.null(x$m)) vapply(x$m$fig, function(y) as.numeric(y$img$w), 0)))
    ws <- unlist(lapply(real[[k]], function(x) if (!is.null(x$m) && length(x$m$sol)) as.numeric(x$m$sol[[1]]$img$w)))
    cat(sprintf("  %d px: %d versiones · figuras de la Parte 1 de %.0f a %.0f px y la de la Solution de %.0f a %.0f px · con algún código: %d%s\n",
                PANT_ANCHOS[k], length(ck), min(w4), max(w4), min(ws), max(ws), length(malos),
                if (length(malos)) paste0(" (", paste(sprintf("semilla %d: %s", head(malos, 3), vapply(ck[head(malos, 3)], paste, "", collapse = ",")), collapse = "; "), ")") else ""))
    if (length(malos)) rojo("pantalla", sprintf("%d px: %d versiones con códigos", PANT_ANCHOS[k], length(malos)))
  }
  cat(sprintf("  %.0f s\n", proc.time()[["elapsed"]] - t0))
  ## Mutantes: copias mutadas del .Rmd en tempdir(), semillas 1..3; la línea base es la medición real de esas semillas.
  for (M in PANT_MUTANTES) {
    mut <- lineas; mal <- NULL
    for (cb in M$cambios) { mut <- mutar(mut, cb[1], cb[2]); if (!is.null(attr(mut, "mal"))) { mal <- attr(mut, "mal"); break } }
    if (!is.null(mal)) { cat(sprintf("  %-40s mal_construido (%s)\n", M$id, mal)); rojo("pantalla", paste(M$id, "mal construido")); next }
    d <- file.path(tempdir(), paste0("pant_", M$id)); dir.create(d, showWarnings = FALSE)
    f <- file.path(d, basename(RMD)); writeLines(mut, f, useBytes = TRUE)
    hm <- pant_html(f, 1:3, file.path(d, "html"))
    if (!length(hm)) { cat(sprintf("  %-40s RENDER: %s\n", M$id, attr(hm, "error"))); rojo("pantalla", paste(M$id, "no renderizó")); next }
    rm_ <- pant_todo(b$ses, hm); cod <- character(0); donde <- "-"; base_ok <- TRUE
    for (k in seq_along(PANT_ANCHOS)) for (i in seq_along(hm)) if (!length(cod) && length(rm_[[k]][[i]]$cod)) {
      cod <- rm_[[k]][[i]]$cod; donde <- sprintf("%d a %d px", i, PANT_ANCHOS[k]); base_ok <- !length(real[[k]][[i]]$cod) }
    v <- if (!base_ok) "mal_construido" else veredicto(cod, M$sonda)
    cat(sprintf("  %-40s sonda esperada %-14s semilla %-10s %s · %s\n", M$id, M$sonda, donde, v, if (length(cod)) paste(cod, collapse = ",") else "-"))
    if (!aceptado(v, M)) rojo("pantalla", paste(M$id, v))
  }
}

## --- uso (decisión del profesor tras la tanda 3: «semillero sí») -------------------------------------------------------
## SemilleroCloze.R de esta carpeta es lo que el profesor ejecuta para generar el lote. Si su PDF vuelve a la plantilla de un solo
## ejercicio, a "plain" (A4) o al archivo de prueba de ../SemilleroCloze.R, ninguna otra sección se pone en rojo. Tres capas:
## (1) literales del semillero, cada uno una vez, y ningún rastro del archivo de prueba; (2) la plantilla local difiere de
## ../solpcielo_cloze.tex solo en su comentario de cabecera y en \clearpage; (3) las funciones generar_pdf(), generar_html() y
## generar_moodle() del semillero, extraídas de su texto (no reescritas aquí), se ejecutan en una carpeta temporal con 3 preguntas
## en el PDF y 2 versiones en HTML y en Moodle, cada formato tras set.seed(semilla): hoja de 216 x 330 mm, las cuatro figuras de
## cada pregunta en una página, «Cómo marcar» en la portada, HTML sin ##ANSWER y, en el XML, los seis gaps en la forma que mide
## la sección moodle (MULTICHOICE_V, cuatro NUMERICAL y MULTIRESPONSE). Los mutantes de la plantilla «en disco» cambian solo el
## archivo que usa el render, no el texto que mira la capa (2): así cada componente tiene un mutante que la mueve sola.
## Códigos: USO_SEMILLERO, USO_PLANTILLA, USO_RENDER (falla cerrado), USO_TAMANO, USO_FIGURAS_PDF, USO_COMO_MARCAR, USO_HTML, USO_MOODLE
## y, desde la tanda 5, USO_EXSHUFFLE, USO_ARCHIVOS, USO_ORDEN_PDF y USO_CLAVE_PDF (falla cerrado).
USO_SEMILLERO <- file.path(aqui, "SemilleroCloze.R"); USO_PLANTILLA <- PAPEL_TPL_VARIAS; USO_TOPE <- 5L
USO_LITERALES <- c(sprintf('archivo_examen <- "%s"', basename(RMD)), 'template = "solpcielo_cloze_una_por_pagina"',
                   "exams2pdf(rep(archivo_examen, config$preguntas_pdf),", "preguntas_pdf = ")
USO_PROHIBIDOS <- c("archivo_simple", "pdf_basico", "test_simple_pdf")
USO_ENV <- "\\newenvironment{question}{\\clearpage\\item \\textbf{Escenario:}\\newline}{}"
USO_COMENTARIO <- "%% Copia de ../solpcielo_cloze.tex. Único cambio: \\clearpage"
sonda_plantilla_uso <- function(tpl, base) {
  com <- startsWith(tpl, USO_COMENTARIO)
  if (sum(tpl == USO_ENV) != 1L || sum(com) != 1L) return("USO_PLANTILLA")
  n <- tpl[!com]; n[n == USO_ENV] <- sub("\\clearpage", "", USO_ENV, fixed = TRUE)
  if (identical(n, base)) character(0) else "USO_PLANTILLA"
}
USO_OBJETOS <- c("archivo_examen", "config", "nombre_sin_extension", "nombre_base", "generar_html", "generar_moodle", "generar_pdf")
cargar_semillero <- function(sem) {
  ex <- parse(text = sem, keep.source = FALSE, encoding = "UTF-8"); es <- new.env(parent = globalenv())
  for (x in ex) if (is.call(x) && identical(x[[1L]], as.name("<-")) && is.name(x[[2L]]) && as.character(x[[2L]]) %in% USO_OBJETOS) eval(x, es)
  falta <- setdiff(USO_OBJETOS, ls(es)); if (length(falta)) stop("el semillero no define ", paste(falta, collapse = ", "))
  es
}
USO_GAPS <- c("MULTICHOICE_V", rep("NUMERICAL", 4L), "MULTIRESPONSE")
gaps_xml <- function(xml) {
  q <- strsplit(paste(readLines(xml, encoding = "UTF-8", warn = FALSE), collapse = "\n"), "<question type=\"cloze\">", fixed = TRUE)[[1]][-1]
  lapply(q, function(x) sub("^\\{[0-9]+:([A-Z_]+):$", "\\1", regmatches(x, gregexpr("\\{[0-9]+:(MULTICHOICE_V|MULTICHOICE|MULTIRESPONSE|NUMERICAL):", x))[[1]]))
}
## Tanda 5 (decisión del profesor tras la tanda 4, respuesta literal «H1 c · H2 100»; ../.claude/CLAUDE.md, «CLOZE hermano»).
## (4) Árbol sintáctico del semillero (los comentarios no cuentan; con o sin «exams::»): toda llamada a exams2pdf() lleva
## exshuffle = FALSE, ninguna otra exams2*() lleva exshuffle (USO_EXSHUFFLE), y config$archivos vale 100 (USO_ARCHIVOS).
## (5) PDF real de generar_pdf(): el gap de la Parte 1 de cada pregunta se imprime «Diagrama I / Diagrama II / Diagrama III /
## Diagrama IV» (USO_ORDEN_PDF). (6) La clave que REGISTRA exams2pdf() (metainfo$solution del gap 1, capturada con trace() a la
## salida de exams2pdf, sin tocar sus argumentos) señala el diagrama correcto: el texto impreso en esa posición del gap, su
## rótulo en el LaTeX compilado, el archivo de ese rótulo y sus regiones (del entorno de la pregunta, capturado con trace() a la
## salida de knitr::knit y emparejado por fig_id) deben ser las que dan las viñetas impresas leídas con leer_vinetas(). No usa
## sol_p1 ni la Solution (USO_CLAVE_PDF, falla cerrado). La muestra es la de la capa (3): 3 preguntas en la semilla 1 (familia B
## de la regla #23, declarada); la medición de N = 100 de la tanda 5 está en el HANDOFF.
USO_ORDEN_GAP <- "Diagrama I / Diagrama II / Diagrama III / Diagrama IV"; USO_ARCHIVOS_N <- 100
llamadas_exams <- function(x, acc = list()) {
  if (is.call(x)) {
    f <- x[[1L]]
    nom <- if (is.name(f)) as.character(f) else if (is.call(f) && identical(f[[1L]], as.name("::")) && identical(f[[2L]], as.name("exams")))
      as.character(f[[3L]]) else ""
    if (startsWith(nom, "exams2")) acc[[length(acc) + 1L]] <- list(f = nom, args = as.list(x)[-1L])
  }
  if (is.call(x) || is.expression(x) || is.pairlist(x) || is.list(x)) {
    el <- as.list(x)
    for (i in seq_along(el)) if (!identical(el[[i]], quote(expr = ))) acc <- llamadas_exams(el[[i]], acc)
  }
  acc
}
sondas_llamadas_uso <- function(sem) {
  ex <- tryCatch(parse(text = sem, keep.source = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(ex)) return(c("USO_EXSHUFFLE", "USO_ARCHIVOS"))
  ll <- llamadas_exams(ex); f <- character(0)
  es_pdf <- vapply(ll, function(l) identical(l$f, "exams2pdf"), TRUE)
  sh <- lapply(ll, function(l) if ("exshuffle" %in% names(l$args)) l$args[["exshuffle"]] else NULL)
  if (!any(es_pdf) || !all(vapply(sh[es_pdf], identical, TRUE, FALSE)) || !all(vapply(sh[!es_pdf], is.null, TRUE))) f <- c(f, "USO_EXSHUFFLE")
  cfg <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) && identical(x[[2L]], as.name("config")), as.list(ex))
  ar <- if (length(cfg) == 1L && is.call(cfg[[1L]][[3L]]) && identical(cfg[[1L]][[3L]][[1L]], as.name("list"))) as.list(cfg[[1L]][[3L]])[["archivos"]]
  if (!(is.numeric(ar) && length(ar) == 1L && isTRUE(ar == USO_ARCHIVOS_N))) f <- c(f, "USO_ARCHIVOS")
  f
}
USO_ESPIA <- c("fig_id", "archivos", "regiones_mezcladas", "sol_p1", "reg_clave", "textos_p1")   # sol_p1 y textos_p1: solo para `materia`
poner_espia_pdf <- function() {
  cap <- new.env(); cap$env <- list(); cap$res <- NULL; cap$llamado <- FALSE; cap$exshuffle <- NULL
  suppressMessages({
    trace("knit", where = asNamespace("knitr"), print = FALSE, exit = bquote(
      assign("env", c(get("env", envir = .(cap)), list(mget(.(USO_ESPIA), envir = envir, ifnotfound = list(NULL)))), envir = .(cap))))
    ## exams2pdf vive en el namespace y, exportada, en package:exams: se traza en los dos (el semillero la llama sin «exams::»).
    ## No se traza xexams: su on.exit(setwd(...)) sin add = TRUE borra la salida de trace().
    for (w in list(asNamespace("exams"), as.environment("package:exams")))
      trace("exams2pdf", where = w, print = FALSE, exit = bquote({
        assign("res", returnValue(), envir = .(cap)); assign("llamado", TRUE, envir = .(cap)); assign("exshuffle", exshuffle, envir = .(cap)) }))
  })
  cap
}
quitar_espia_pdf <- function() suppressMessages({
  untrace("knit", where = asNamespace("knitr"))
  for (w in list(asNamespace("exams"), as.environment("package:exams"))) untrace("exams2pdf", where = w)
})
gap1_pdf <- function(pdf) {
  t <- gsub("\\s+", " ", paste(system2("pdftotext", c(shQuote(pdf), "-"), stdout = TRUE), collapse = " "))
  regmatches(t, gregexpr("Diagrama (I|II|III|IV)( / Diagrama (I|II|III|IV)){3}", t))[[1L]]
}
## Viñetas de la Parte 1 en el LaTeX que compiló exams2pdf: el primer itemize, un \item por viñeta (pandoc lo deja en su línea).
vinetas_latex <- function(L) {
  i0 <- grep("^\\\\begin\\{itemize\\}", L)[1L]; i1 <- grep("^\\\\end\\{itemize\\}", L)[1L]
  if (is.na(i0) || is.na(i1) || i1 <= i0 + 1L) return(character(0))
  it <- trimws(strsplit(sub("^\\\\tightlist\\s*", "", paste(trimws(L[(i0 + 1L):(i1 - 1L)]), collapse = " ")), "\\\\item\\b", perl = TRUE)[[1L]])
  paste0("- ", it[nzchar(it)])
}
sonda_clave_pdf <- function(cap, g, nq) {
  falla <- function(j, por) structure("USO_CLAVE_PDF", detalle = sprintf("pregunta %s: %s", j, por))
  res <- cap$res
  if (!isTRUE(cap$llamado) || !is.list(res) || length(res) != 1L || length(res[[1L]]) != nq) return(falla("-", "sin la salida de exams2pdf()"))
  if (length(g) != nq) return(falla("-", sprintf("%d gaps de la Parte 1 en el PDF", length(g))))
  if (length(cap$env) != nq) return(falla("-", sprintf("%d entornos capturados", length(cap$env))))
  letra <- character(0)
  for (j in seq_len(nq)) {
    q <- res[[1L]][[j]]; s1 <- q$metainfo$solution[[1L]]
    if (!is.logical(s1) || length(s1) != 4L || sum(s1) != 1L) return(falla(j, "la clave registrada no es una de cuatro"))
    k <- which(s1); X <- strsplit(g[j], " / ", fixed = TRUE)[[1L]][k]; letra <- c(letra, letters[k])
    tx <- paste(q$question, collapse = "\n")
    m <- regmatches(tx, gregexpr(paste0("\\\\textbf\\{Diagrama (I|II|III|IV)\\}\\\\\\\\\\s*\\\\includegraphics(\\[[^]]*\\])?",
                                        "\\{[^}]*?diagrama_[a-d]_[0-9a-f]+\\.png\\}"), tx, perl = TRUE))[[1L]]
    rot <- paste("Diagrama", sub("^\\\\textbf\\{Diagrama (I|II|III|IV)\\}.*$", "\\1", m))
    arch <- sub("^.*(diagrama_[a-d]_[0-9a-f]+\\.png)\\}$", "\\1", m)
    if (length(m) != 4L || anyDuplicated(rot) || !(X %in% rot)) return(falla(j, "rótulos de las figuras en el LaTeX"))
    a <- arch[rot == X]; e <- Filter(function(z) identical(z$fig_id, sub("^diagrama_[a-d]_([0-9a-f]+)\\.png$", "\\1", a)), cap$env)
    o1 <- leer_vinetas(vinetas_latex(q$question))
    if (length(e) != 1L || is.null(o1) || !(a %in% e[[1L]]$archivos)) return(falla(j, "sin entorno o sin viñetas legibles"))
    if (!identical(as.integer(e[[1L]]$regiones_mezcladas[[match(a, e[[1L]]$archivos)]]), unname(as.integer(o1$reg))))
      return(falla(j, sprintf("la clave registrada, (%s) = %s, no reproduce las viñetas", letters[k], X)))
  }
  structure(character(0), claves = letra)
}
orden_gap1_xml <- function(xml) {
  q <- strsplit(paste(readLines(xml, encoding = "UTF-8", warn = FALSE), collapse = "\n"), "<question type=\"cloze\">", fixed = TRUE)[[1]][-1]
  vapply(q, function(x) { b <- regmatches(x, regexpr("\\{[0-9]+:MULTICHOICE_V:[^}]*\\}", x)); if (!length(b)) NA_character_ else
    paste(sub("^(=|%-?[0-9.]+%)", "", strsplit(sub("^\\{[0-9]+:MULTICHOICE_V:(.*)\\}$", "\\1", b), "~", fixed = TRUE)[[1]]), collapse = " / ") },
    "", USE.NAMES = FALSE)
}
sondas_uso <- function(sem = readLines(USO_SEMILLERO, encoding = "UTF-8"), tpl = readLines(USO_PLANTILLA, encoding = "UTF-8"),
                       base = readLines(PAPEL_TPL, encoding = "UTF-8"), render = TRUE, tpl_disco = tpl,
                       formatos = c("pdf", "html", "moodle"), semilla = 1L, rmd = RMD) {
  f <- character(0); det <- "sin render"
  if (any(vapply(USO_LITERALES, function(l) sum(lengths(regmatches(sem, gregexpr(l, sem, fixed = TRUE)))) != 1L, TRUE)) ||
      any(vapply(USO_PROHIBIDOS, function(l) any(grepl(l, sem, fixed = TRUE)), TRUE))) f <- c(f, "USO_SEMILLERO")
  f <- c(f, sonda_plantilla_uso(tpl, base), sondas_llamadas_uso(sem))
  captura <- list(archivos = tryCatch(cargar_semillero(sem)$config$archivos, error = function(x) NA))   # para `materia`
  if (render) {
    d <- tempfile("uso_"); dir.create(d); od <- setwd(d); on.exit({ setwd(od); unlink(d, recursive = TRUE) })
    file.copy(rmd, file.path(d, basename(RMD)))                     # el semillero lo busca por su nombre, en edir
    writeLines(tpl_disco, file.path(d, basename(USO_PLANTILLA)), useBytes = TRUE)
    es <- tryCatch(cargar_semillero(sem), error = function(x) conditionMessage(x))
    if (!is.environment(es)) return(structure(unique(c(f, "USO_RENDER")), detalle = paste("el semillero no carga:", es), captura = captura))
    es$config$dir_salida <- file.path(d, "salida_hibrida"); es$config$dir_ejercicios <- d
    es$config$archivos <- 2L; es$config$preguntas_pdf <- 3L
    for (g in c("html", "moodle", "pdf")) dir.create(file.path(es$config$dir_salida, g), recursive = TRUE, showWarnings = FALSE)
    logs <- list()
    corre_g <- function(g) { set.seed(semilla); r_ <- FALSE
      logs[[g]] <<- utils::capture.output(r_ <- tryCatch(es[[paste0("generar_", g)]](), error = function(x) FALSE)); isTRUE(r_) }
    sale <- function(g, pat) list.files(file.path(es$config$dir_salida, g), pat, full.names = TRUE)
    det <- character(0)
    if ("pdf" %in% formatos) {
      cap <- poner_espia_pdf(); hecho <- tryCatch(corre_g("pdf"), finally = quitar_espia_pdf())
      captura$pdf <- list(exshuffle = cap$exshuffle, llamado = cap$llamado, entornos = cap$env,
                          rotulos_latex = if (is.list(cap$res) && length(cap$res)) vapply(cap$res[[1L]], function(q)
                            sum(grepl("\\textbf{Diagrama", q$question, fixed = TRUE)), 0L) else integer(0))
      pdf <- if (hecho) sale("pdf", "\\.pdf$") else character(0)
      if (length(pdf) != 1L) { f <- c(f, "USO_RENDER"); det <- c(det, "generar_pdf() sin PDF") } else {
        m <- medir_pdf_varias(pdf, 3L)
        if (any(abs(m$hoja - PAPEL_HOJA_MM) > 0.5)) f <- c(f, "USO_TAMANO")
        if (is.na(m$partidas) || m$partidas > 0L) f <- c(f, "USO_FIGURAS_PDF")
        if (!m$marcar) f <- c(f, "USO_COMO_MARCAR")
        det <- c(det, sprintf("PDF: hoja %s mm, páginas de las imágenes %s, «Cómo marcar» %s", paste(round(m$hoja), collapse = " x "),
                              paste(m$paginas, collapse = " "), if (m$marcar) "sí" else "no"))
        g1 <- gap1_pdf(pdf)
        if (length(g1) != 3L || any(g1 != USO_ORDEN_GAP)) f <- c(f, "USO_ORDEN_PDF")
        kc <- sonda_clave_pdf(cap, g1, 3L); f <- c(f, kc)
        det <- c(det, sprintf("gap de la Parte 1 en el orden I-IV en %d de %d preguntas", sum(g1 == USO_ORDEN_GAP), length(g1)),
                 if (length(kc)) attr(kc, "detalle") else
                   sprintf("clave registrada por exams2pdf() %s, cada una en el diagrama que reproduce las viñetas impresas",
                           paste0("(", attr(kc, "claves"), ")", collapse = " ")))
      }
    }
    if ("html" %in% formatos) {
      caph <- new.env(); caph$exshuffle <- "sin_llamar"; dondes <- list(asNamespace("exams"), as.environment("package:exams"))
      suppressMessages(for (w in dondes) trace("exams2html", where = w, print = FALSE, exit = bquote(assign("exshuffle", exshuffle, envir = .(caph)))))
      hecho_h <- tryCatch(corre_g("html"), finally = suppressMessages(for (w in dondes) untrace("exams2html", where = w)))
      captura$html_exshuffle <- caph$exshuffle
      html <- if (hecho_h) sale("html", "\\.html$") else character(0)
      bien <- length(html) == 1L && !any(grepl("##ANSWER", readLines(html[1L], warn = FALSE), fixed = TRUE))
      if (!bien) f <- c(f, "USO_HTML"); det <- c(det, sprintf("HTML: %d archivo(s)%s", length(html), if (bien) " sin ##ANSWER" else ""))
    }
    if ("moodle" %in% formatos) {
      xml <- if (corre_g("moodle")) sale("moodle", "\\.xml$") else character(0)
      captura$moodle_log <- logs$moodle
      g <- if (length(xml) == 1L) gaps_xml(xml) else list()
      if (length(xml) == 1L) captura$moodle_orden <- orden_gap1_xml(xml)
      bien <- length(g) == 2L && all(vapply(g, identical, TRUE, USO_GAPS))
      if (!bien) f <- c(f, "USO_MOODLE")
      det <- c(det, sprintf("XML: %d pregunta(s) con gaps %s", length(g), paste(unique(vapply(g, paste, "", collapse = "|")), collapse = "; ")))
    }
    det <- paste(det, collapse = " · ")
  }
  r <- unique(f); attr(r, "detalle") <- det; attr(r, "captura") <- captura; r
}
## `materia` (obligación 3): la mutación llegó a lo que se ejecuta, comprobado sobre lo capturado y no sobre el texto. `otra`: la
## segunda sonda que también debe caer (U10 tumba la estructura y el orden; U13 y U15 mueven cada una sola).
USO_MUTANTES <- list(
  list(id = "U1_semillero_con_plantilla_plain", sonda = "USO_SEMILLERO",
       sem = c('template = "solpcielo_cloze_una_por_pagina"', 'template = "plain"')),
  list(id = "U2_semillero_con_archivo_de_prueba", sonda = "USO_SEMILLERO",
       sem = c('cat("✅ pdflatex disponible\\n")', 'archivo_simple <- "test_simple_pdf.Rmd"; cat("✅ pdflatex disponible\\n")')),
  list(id = "U3_plantilla_que_cambia_otra_linea", sonda = "USO_PLANTILLA",
       tpl = c("\\setlength{\\columnseprule}{0.2pt}", "\\setlength{\\columnseprule}{0.3pt}")),
  list(id = "U4_plantilla_en_disco_sin_clearpage", sonda = "USO_FIGURAS_PDF", formatos = "pdf",
       disco = c("{\\clearpage\\item", "{\\item")),
  list(id = "U5_plantilla_en_disco_sin_como_marcar", sonda = "USO_COMO_MARCAR", formatos = "pdf",
       disco = c("\\noindent\\textbf{Cómo marcar:}", "%% \\noindent\\textbf{Cómo marcar:}")),
  list(id = "U6_plantilla_en_disco_de_356_mm", sonda = "USO_TAMANO", formatos = "pdf",
       disco = c("papersize={216mm,330mm}", "papersize={216mm,356mm}")),
  list(id = "U7_plantilla_en_disco_que_no_compila", sonda = "USO_RENDER", formatos = "pdf",
       disco = c("\\begin{document}", "\\begin{documento}")),
  list(id = "U8_semillero_html_de_otro_archivo", sonda = "USO_HTML", formatos = "html",
       sem = c("exams2html(rep(archivo_examen, config$archivos),", 'exams2html(rep("no_existe.Rmd", config$archivos),')),
  list(id = "U9_semillero_moodle_sin_multichoice_v", sonda = "USO_MOODLE", formatos = "moodle",
       sem = c('cloze_schoice_display = "MULTICHOICE_V"', 'cloze_schoice_display = "MULTICHOICE"')),
  ## Tanda 5 («H1 c · H2 100»).
  list(id = "U10_semillero_sin_exshuffle_en_el_pdf", sonda = "USO_ORDEN_PDF", otra = "USO_EXSHUFFLE", formatos = "pdf",
       sem = c("exshuffle = FALSE,", ""), materia = function(k) isTRUE(k$pdf$llamado) && is.null(k$pdf$exshuffle)),
  ## exams2moodle() de exams 2.4.5 no tiene el argumento exshuffle: con él aborta («argumento no fue usado»), así que U11 también
  ## tumba USO_MOODLE (declarada en `otra`). U16 mueve sola la componente «ninguna otra exams2*() lleva exshuffle»: exams2html() sí
  ## lo acepta y deja de barajar sin avisar.
  list(id = "U11_semillero_moodle_tambien_sin_barajar", sonda = "USO_EXSHUFFLE", otra = "USO_MOODLE", formatos = "moodle",
       sem = c('cloze = list(cloze_schoice_display = "MULTICHOICE_V"),', 'cloze = list(cloze_schoice_display = "MULTICHOICE_V"), exshuffle = FALSE,'),
       materia = function(k) any(grepl("(exshuffle = FALSE)", k$moodle_log, fixed = TRUE))),
  list(id = "U12_semillero_con_250_archivos", sonda = "USO_ARCHIVOS", sem = c("archivos = 100,", "archivos = 250,"),
       materia = function(k) identical(k$archivos, 250)),
  list(id = "U13_rmd_con_rotulos_permutados", sonda = "USO_ORDEN_PDF", formatos = "pdf",
       rmd = list(c('ROMANOS <- c("I", "II", "III", "IV")', 'ROMANOS <- c("II", "I", "III", "IV")')),
       materia = function(k) length(k$pdf$entornos) == 3L &&
         all(vapply(k$pdf$entornos, function(e) identical(e$textos_p1, paste("Diagrama", c("II", "I", "III", "IV"))), TRUE))),
  list(id = "U14_rmd_con_la_clave_desplazada", sonda = "USO_CLAVE_PDF", formatos = "pdf",
       rmd = list(c("sol_p1 <- vapply(ops, function(o) o$es_clave, TRUE)", "sol_p1 <- vapply(ops, function(o) o$es_clave, TRUE)[c(4L, 1L, 2L, 3L)]"),
                  c('identical(regiones_mezcladas[[which(sol_p1)]], reg_clave) && codigos_p1[which(sol_p1)] == "CLAVE")', "TRUE)")),
       materia = function(k) length(k$pdf$entornos) == 3L && all(vapply(k$pdf$entornos, function(e)
         !identical(which(e$sol_p1), which(vapply(e$regiones_mezcladas, identical, TRUE, e$reg_clave))), TRUE))),
  list(id = "U15_semillero_sin_exshuffle_y_rmd_sin_barajar", sonda = "USO_EXSHUFFLE", formatos = "pdf",
       sem = c("exshuffle = FALSE,", ""), rmd = list(c("exshuffle: TRUE", "exshuffle: FALSE")),
       materia = function(k) isTRUE(k$pdf$llamado) && is.null(k$pdf$exshuffle)),
  list(id = "U16_semillero_html_tambien_sin_barajar", sonda = "USO_EXSHUFFLE", formatos = "html",
       sem = c("exams2html(rep(archivo_examen, config$archivos),", "exams2html(rep(archivo_examen, config$archivos), exshuffle = FALSE,"),
       materia = function(k) identical(k$html_exshuffle, FALSE)),
  ## Falla cerrado de USO_CLAVE_PDF: si no puede leer el rótulo de cada figura en el LaTeX, cae; no da la clave por buena.
  list(id = "U17_rmd_con_rotulos_sin_negrita", sonda = "USO_CLAVE_PDF", formatos = "pdf",
       rmd = list(c('"**", textos_p1[i], "**', 'textos_p1[i], "')),
       materia = function(k) length(k$pdf$rotulos_latex) == 3L && all(k$pdf$rotulos_latex == 0L)))
sec_uso <- function() {
  cat("\n[uso] SemilleroCloze.R y", basename(USO_PLANTILLA), ": literales, llamadas exams2*(), plantilla, y generar_pdf/html/moodle del semillero en una carpeta temporal\n")
  t0 <- proc.time()[["elapsed"]]; r <- sondas_uso()
  cat(sprintf("  semilla 1 · %s · %.0f s\n", attr(r, "detalle"), proc.time()[["elapsed"]] - t0))
  if (length(r)) rojo("uso", paste(r, collapse = ", ")) else
    cat("  sin fallas: literales del semillero, exshuffle = FALSE solo en exams2pdf(), archivos = 100, plantilla igual a ../solpcielo_cloze.tex salvo el comentario y \\clearpage, PDF en oficio con las cuatro figuras de cada pregunta en una página, «Cómo marcar», el gap de la Parte 1 en el orden I-IV y la clave registrada en el diagrama correcto, HTML sin ##ANSWER y XML con los seis gaps medidos\n")
  ## Mutantes (contrato INC-MUTANTE-SONDA): patrón único (mutar), sonda declarada, semillas 1 a USO_TOPE hasta la primera muerte,
  ## línea base sin mutar en la semilla de la muerte (obligación 8; en la semilla 1 es la medición de arriba) y, desde la tanda 5,
  ## `materia` sobre lo capturado (obligación 3) y `rmd`: copias mutadas del .Rmd en tempdir(), nunca el archivo real.
  sem0 <- readLines(USO_SEMILLERO, encoding = "UTF-8"); tpl0 <- readLines(USO_PLANTILLA, encoding = "UTF-8")
  res <- parallel::mclapply(USO_MUTANTES, function(M) {
    s <- sem0; t <- tpl0; dk <- tpl0; rl <- lineas; rm_ <- RMD
    if (!is.null(M$sem)) s <- mutar(s, M$sem[1], M$sem[2])
    if (!is.null(M$tpl)) t <- mutar(t, M$tpl[1], M$tpl[2])
    if (!is.null(M$disco)) dk <- mutar(dk, M$disco[1], M$disco[2])
    for (cb in M$rmd) { rl <- mutar(rl, cb[1], cb[2]); if (!is.null(attr(rl, "mal"))) break }
    mal <- c(attr(s, "mal"), attr(t, "mal"), attr(dk, "mal"), attr(rl, "mal"))
    if (length(mal)) return(list(mal = mal[1]))
    if (!is.null(M$rmd)) { rm_ <- tempfile("uso_rmd_", fileext = ".Rmd"); writeLines(rl, rm_, useBytes = TRUE) }
    if (is.null(M$formatos)) { z <- sondas_uso(s, t, render = FALSE)
      return(list(cod = as.character(z), sem = NA_integer_, base = character(0), captura = attr(z, "captura"))) }
    for (sm in seq_len(USO_TOPE)) { z <- sondas_uso(s, t, tpl_disco = dk, formatos = M$formatos, semilla = sm, rmd = rm_); cod <- as.character(z); if (length(cod)) break }
    base <- if (sm == 1L) character(0) else as.character(sondas_uso(formatos = M$formatos, semilla = sm))
    list(cod = cod, sem = sm, base = base, captura = attr(z, "captura"), detalle = attr(z, "detalle"))
  }, mc.cores = min(8L, parallel::detectCores()))
  for (i in seq_along(USO_MUTANTES)) {
    M <- USO_MUTANTES[[i]]; x <- res[[i]]
    v <- if (is.null(x) || inherits(x, "try-error")) "sin_resultado" else if (!is.null(x$mal)) "mal_construido" else
      if (length(r) || length(x$base)) "mal_construido" else
      if (!is.null(M$materia) && !isTRUE(tryCatch(M$materia(x$captura), error = function(e) FALSE))) "mal_construido" else veredicto(x$cod, M$sonda)
    ok <- v == "cazado_por_su_sonda" ||
      (v == "cazado_por_su_sonda_y_otra" && !is.null(M$otra) && all(startsWith(x$cod, M$sonda) | startsWith(x$cod, M$otra)))
    cat(sprintf("  %-46s sonda esperada %-16s semilla %-3s %s%s · %s\n", M$id, M$sonda,
                if (is.list(x) && !is.null(x$sem) && !is.na(x$sem)) x$sem else "-", v,
                if (!is.null(M$otra)) sprintf(" (otra declarada: %s)", M$otra) else "",
                if (is.list(x) && !is.null(x$mal)) x$mal else if (is.list(x) && length(x$cod)) paste(x$cod, collapse = ",") else "-"))
    if (!ok) { rojo("uso", paste(M$id, v)); if (is.list(x) && !is.null(x$detalle)) cat("    detalle:", x$detalle, "\n") }
  }
}
## --- salida ----------------------------------------------------------------------------------------------------------
SECCIONES <- c("e1", "texto", "pesos", "cruce", "bateria", "mutantes", "v6", "moodle", "solucion", "papel", "pantalla", "uso")
decidir_exit <- function(errores, pend, modo, corr, md5_ok = isTRUE(unname(tools::md5sum(RMD)) == RMD_MD5), firmada = FIRMADA) {
  if (errores > 0L) return(1L)
  if (!isTRUE(firmada) || !isTRUE(md5_ok) || modo != "todo" || length(setdiff(SECCIONES, corr)) || length(pend)) return(3L)
  4L
}
corre <- function(nombre, f) { f(); corridas <<- c(corridas, nombre) }
if (modo %in% c("todo", "e1")) corre("e1", sec_e1)
if (modo %in% c("todo", "texto")) corre("texto", sec_texto)
if (modo %in% c("todo", "pesos")) corre("pesos", sec_pesos)
if (modo %in% c("todo", "cruce")) corre("cruce", sec_cruce)
if (modo %in% c("todo", "bateria")) corre("bateria", sec_bateria)
if (modo %in% c("todo", "mutantes")) corre("mutantes", sec_mutantes)
if (modo %in% c("todo", "v6")) corre("v6", sec_v6)
if (modo %in% c("todo", "moodle")) corre("moodle", sec_moodle)
if (modo %in% c("todo", "solucion")) corre("solucion", sec_solucion)
if (modo %in% c("todo", "papel")) corre("papel", sec_papel)
if (modo %in% c("todo", "pantalla")) corre("pantalla", sec_pantalla)
if (modo %in% c("todo", "uso")) corre("uso", sec_uso)
pend <- unique(c(pendientes, if ("bateria" %in% corridas) firma_estado$pendientes))
md5_ok <- isTRUE(unname(tools::md5sum(RMD)) == RMD_MD5)
salida <- decidir_exit(length(rojos), pend, modo, corridas, md5_ok)
cat("\nRESUMEN: rojos", length(rojos), if (length(rojos)) paste0("(", paste(unique(rojos), collapse = ","), ")") else "",
    "· pendientes", if (length(pend)) paste(pend, collapse = ",") else "ninguno", "· md5 cableado", if (md5_ok) "coincide" else "NO coincide",
    "· secciones", paste(corridas, collapse = ","), "\n")
cat("EXIT", salida, switch(as.character(salida), "1" = "(rojo)", "3" = "(sin cubrir)", "4" = "(rojo §P7 de la Parte 1 cubierto por la firma C-4; no es PASS)"), "\n")
quit(status = salida)
