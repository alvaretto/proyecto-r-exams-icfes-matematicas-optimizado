# verificar_dibujo_clave_cloze.R — verificador propio de la versión CLOZE de
# barras-campeonato-baloncesto-n3 (Familia A, N = 100, regla #23).
#
# Por qué existe: en el SCHOICE hermano, `sol` en la opción equivocada pasó 100/100 por
# validar_multisemilla.R (la clave es una propiedad del DIBUJO). En el CLOZE hay seis claves
# y la Parte 1 hereda el mismo punto ciego; las Partes 2-6 tienen claves que ningún validador
# genérico recalcula a partir de lo que el estudiante LEE.
#
# Cómo mide, sin fiarse de las variables del .Rmd:
#   1. Teje el .Rmd con knitr interceptando include_tikz() (mismo método que el verificador del
#      SCHOICE; el lector del TikZ se toma de ../verificar_dibujo_clave.R, fuente única).
#   2. Parte 1: lee los datos mostrados (tabla + gráfica del enunciado) y las cuatro opciones
#      dibujadas; exige una sola opción que los dibuje, que su RÓTULO (Gráfica I-IV, el que
#      precede a su imagen en el enunciado) sea el marcado en exsolution, y que la Solution
#      (párrafo, figura, distractores, Answerlist) hable de esa misma gráfica.
#   3. Partes 2, 3 y 6: relee los números del TEXTO tejido y recalcula la clave.
#   4. Parte 4: relee la tabla correcta y la del estudiante del texto y aplica las seis
#      permutaciones definidas AQUÍ a partir del significado de cada nombre de error; exige que
#      solo una produzca la gráfica del estudiante y que sea la marcada.
#   5. Parte 5: compara cada marca con una tabla de verdad propia de las diez afirmaciones.
#
# Uso: Rscript verificar_dibujo_clave_cloze.R [ruta.Rmd] [--n 100]
#      Exit 0 = todo verde; 1 = algún fallo.

suppressMessages(library(exams))
args <- commandArgs(trailingOnly = TRUE)
aqui <- local({ a <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
  dirname(normalizePath(sub("^--file=", "", a))) })
rmd <- if (length(args) && !startsWith(args[1], "--")) normalizePath(args[1]) else
  file.path(aqui, "barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd")
N <- if ("--n" %in% args) as.integer(args[which(args == "--n") + 1]) else 100L
semillas <- seq_len(N) * 7919L + 13L

## Lector del TikZ: solo las definiciones de ../verificar_dibujo_clave.R (no su ejecución).
local({
  ex <- parse(file.path(dirname(aqui), "verificar_dibujo_clave.R"), encoding = "UTF-8")
  for (e in ex) if (is.call(e) && identical(e[[1]], as.name("<-")) &&
                    as.character(e[[2]]) %in% c("num", "destex", "leer_tikz", "TOL", "ajustar", "matriz_dibujada"))
    eval(e, envir = globalenv())
})
stopifnot(exists("matriz_dibujada"), exists("leer_tikz"))

ROM <- c("I", "II", "III", "IV")
## Errores E1-E6 definidos por su SIGNIFICADO (fila 1 = grado de la tabla, columna 1 = ganados).
perm_error <- list(
  "Grados intercambiados" = function(P) P[2:1, ],
  "Categorías invertidas en el grado de la tabla" = function(P) { P[1, ] <- P[1, 2:1]; P },
  "Categorías invertidas en el grado de la gráfica" = function(P) { P[2, ] <- P[2, 2:1]; P },
  "Cruce gráfica-tabla" = function(P) { t <- P[2, 1]; P[2, 1] <- P[1, 2]; P[1, 2] <- t; P },
  "Cruce tabla-gráfica" = function(P) { t <- P[1, 1]; P[1, 1] <- P[2, 2]; P[2, 2] <- t; P },
  "Categorías invertidas en ambos grados" = function(P) P[, 2:1])
clave_error <- function(nombre) {
  if (grepl("^Cruce entre (los|las) (ganados|ganadas) de la gráfica y (los|las) (perdidos|perdidas) de la tabla$", nombre)) return("Cruce gráfica-tabla")
  if (grepl("^Cruce entre (los|las) (ganados|ganadas) de la tabla y (los|las) (perdidos|perdidas) de la gráfica$", nombre)) return("Cruce tabla-gráfica")
  if (nombre %in% names(perm_error)) return(nombre)
  NA_character_
}
## Tabla de verdad propia de la Parte 5. Límite declarado (detractor FASE 2C, obj. 7): es un
## oráculo HUMANO; detecta que una afirmación cambie de pool o de texto, pero no un juicio de
## verdad equivocado presente aquí y en el .Rmd a la vez. Las 10 las firma el profesor.
VERDAD_P5 <- c(
  "Pasar correctamente de barras agrupadas a barras apiladas no cambia la información representada." = TRUE,
  "Pasar correctamente de barras agrupadas a barras apiladas cambia los datos que se representan." = FALSE,
  "En una barra apilada, solo el segmento inferior empieza en cero." = TRUE,
  "En una barra apilada, el dato de un segmento superior es la altura a la que llega su borde superior." = FALSE,
  "Si los dos grados tienen datos distintos y en la leyenda se intercambian los colores de los grados, sin tocar las barras, la gráfica pasa a representar otra situación." = TRUE,
  "Si los dos grados tienen datos distintos, intercambiar en la leyenda los colores de los grados, sin tocar las barras, no altera la información, porque las alturas no cambian." = FALSE,
  "Una gráfica no puede aceptarse como correcta si ya se sabe que una de las cuatro casillas no coincide con las fuentes." = TRUE,
  "Si la mitad de las cuatro casillas coinciden con las fuentes, la gráfica ya contiene toda la información." = FALSE,
  "En una barra apilada, la altura total de la barra es la suma de los datos de sus segmentos." = TRUE,
  "En una gráfica de barras agrupadas, la altura de cada barra es la suma de los datos de ambos grados." = FALSE)
## Pares del mismo concepto (verdadera, falsa): en una versión nunca deben aparecer los dos.
PARES_P5 <- split(names(VERDAD_P5), rep(1:5, each = 2))

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
  estrato <- list(canonica = isTRUE(env$es_canonica), rama_e8 = isTRUE(env$rama_e8),
                  rotulo_clave = env$rotulo_clave, k4 = env$codigos_pool[env$k4],
                  p6_verdadero = isTRUE(env$es_verdadero_p6))

  ## ---- exsolution / exclozetype del Markdown tejido ----
  exs <- strsplit(regmatches(txt, regexec("\nexsolution: ([^\n]+)", txt))[[1]][2], "|", fixed = TRUE)[[1]]
  ect <- strsplit(regmatches(txt, regexec("\nexclozetype: ([^\n]+)", txt))[[1]][2], "|", fixed = TRUE)[[1]]
  if (!identical(ect, c("schoice", "num", "num", "schoice", "mchoice", "schoice"))) falla("exclozetype inesperado: ", paste(ect, collapse = "|"))
  if (length(exs) != 6L) { falla("exsolution con ", length(exs), " bloques"); return(list(seed = seed, fallos = fallos, estrato = estrato)) }
  q_txt <- sub("(?s)\nSolution\n=+\n.*$", "", txt, perl = TRUE)
  sol_txt <- sub("(?s)^.*\nSolution\n=+\n", "", txt, perl = TRUE)
  for (i in 1:6) if (lengths(regmatches(q_txt, gregexpr(paste0("##ANSWER", i, "##"), q_txt))) != 1L) falla("##ANSWER", i, "## no aparece exactamente una vez")
  pos_ans <- vapply(1:6, function(i) regexpr(paste0("##ANSWER", i, "##"), q_txt)[1], numeric(1))
  if (is.unsorted(pos_ans)) falla("los ##ANSWERi## no están en orden")
  bl <- sub("(?s)\n\\s*$", "", sub("(?s)^.*\nAnswerlist\n-+\n", "", q_txt, perl = TRUE), perl = TRUE)
  al_q <- sub("^\\* ", "", regmatches(bl, gregexpr("(?m)^\\* [^\n]+", bl, perl = TRUE))[[1]])
  if (length(al_q) != 15L) falla("Answerlist del enunciado con ", length(al_q), " líneas (se esperan 15)")

  ## ---- Parte 1: figuras y datos mostrados ----
  ids <- unique(sub("^.*_([0-9a-f]+)$", "\\1", names(figs)))
  if (length(figs) != 6L || length(ids) != 1L || !grepl("^[0-9a-f]{8}$", ids)) falla("figuras o fig_id inesperados: ", paste(names(figs), collapse = ", "))
  esperados <- paste0(c("grafica_enunciado", paste0("diagrama_", letters[1:4]), "grafica_solucion"), "_", ids[1])
  if (!all(esperados %in% names(figs))) return(list(seed = seed, fallos = c(fallos, "faltan figuras"), estrato = estrato))
  fila <- regmatches(txt, regexec("\\| \\*\\*Grado ([^*]+)\\*\\* \\| ([0-9,]+) \\| ([0-9,]+) \\|", txt))[[1]]
  enc <- regmatches(txt, regexec("\\|  \\| ([^|]+) \\| ([^|]+) \\|", txt))[[1]]
  g_tabla <- trimws(fila[2]); cats_tabla <- trimws(enc[2:3])
  en <- matriz_dibujada(leer_tikz(figs[[esperados[1]]]))$simple
  g_graf <- destex(sub("^.*para grado ", "", regmatches(figs[[esperados[1]]], regexpr("para grado ([^}]|\\{[^}]*\\})+", figs[[esperados[1]]]))))
  verdad <- matrix(c(num(fila[3]), num(fila[4]), en[cats_tabla]), 2, byrow = TRUE,
                   dimnames = list(paste("Grado", c(g_tabla, g_graf)), cats_tabla))
  dib <- tryCatch(lapply(esperados[2:5], function(f) matriz_dibujada(leer_tikz(figs[[f]]))),
                  error = function(e) { falla("lectura del dibujo: ", conditionMessage(e)); NULL })
  if (is.null(dib)) return(list(seed = seed, fallos = fallos, estrato = estrato))
  igual <- function(X) !is.null(X$X) && setequal(rownames(X$X), rownames(verdad)) &&
    max(abs(X$X[rownames(verdad), colnames(verdad)] - verdad)) < 1e-6
  coinciden <- which(vapply(dib, igual, logical(1)))
  if (length(coinciden) != 1L) falla("opciones que dibujan los datos mostrados: ", length(coinciden))
  ## Rótulo DIBUJADO en cada figura (no puede separarse de su imagen) y su texto alternativo
  rot_dib <- function(code) { m <- regmatches(destex(code), regexpr("\\{Gráfica (I|II|III|IV)\\};", destex(code)))
    if (length(m)) sub("^\\{Gráfica (I|II|III|IV)\\};$", "\\1", m) else NA_character_ }
  rot_de_fig <- vapply(esperados[2:5], function(f) rot_dib(figs[[f]]), character(1))
  if (!identical(unname(rot_de_fig), ROM)) falla("rótulos dibujados en diagrama_a..d: ", paste(rot_de_fig, collapse = "/"))
  im <- regmatches(q_txt, gregexpr("!\\[Gráfica (I|II|III|IV): [^]]*\\]\\(diagrama_([a-d])_", q_txt))[[1]]
  if (!identical(sub("^!\\[Gráfica (I|II|III|IV):.*$", "\\1", im), ROM) || !identical(sub("^.*diagrama_([a-d])_$", "\\1", im), letters[1:4]))
    falla("imágenes de opciones fuera de orden o sin rótulo en el texto alternativo")
  if (!identical(al_q[1:4], paste("Gráfica", ROM))) falla("opciones del gap 1: ", paste(al_q[1:4], collapse = " / "))
  marca1 <- which(strsplit(exs[1], "")[[1]] == "1")
  if (length(coinciden) == 1L && !identical(marca1, coinciden)) falla("Parte 1: el dibujo correcto es la Gráfica ", ROM[coinciden], " y exsolution marca ", paste(ROM[marca1], collapse = ","))
  ds <- matriz_dibujada(leer_tikz(figs[[esperados[6]]]))
  if (!igual(ds)) falla("la gráfica de Solution no dibuja los datos mostrados")
  if (length(coinciden) == 1L && !identical(rot_dib(figs[[esperados[6]]]), ROM[coinciden])) falla("la gráfica de Solution lleva el rótulo ", rot_dib(figs[[esperados[6]]]), " y la clave es la ", ROM[coinciden])
  rc <- regmatches(sol_txt, regexec("La gráfica correcta es la \\*\\*Gráfica (I|II|III|IV)\\*\\*, de barras (agrupadas|apiladas), que muestra, para el grado ([^,]+), ([0-9,]+) [^0-9]+ y ([0-9,]+) [^(]+\\(datos de la tabla\\) y, para el grado ([^,]+), ([0-9,]+) [^0-9]+ y ([0-9,]+) ", sol_txt))[[1]]
  if (length(rc) < 9) falla("no se pudo leer el párrafo de la respuesta correcta") else {
    if (length(coinciden) == 1L && rc[2] != ROM[coinciden]) falla("Solution dice Gráfica ", rc[2], " y la que dibuja los datos es la ", ROM[coinciden])
    if (sub("as$", "a", rc[3]) != ds$formato) falla("Solution dice barras ", rc[3], " y la figura es ", ds$formato)
    if (!all(num(rc[c(5, 6, 8, 9)]) == as.vector(t(verdad)))) falla("Solution cita otros datos")
  }
  pd <- regmatches(sol_txt, gregexpr("\\*\\*Gráfica (I|II|III|IV), de barras (agrupadas|apiladas): asigna al grado [^ ]+ ([0-9,]+) y ([0-9,]+), y al grado [^ ]+ ([0-9,]+) y ([0-9,]+)", sol_txt))[[1]]
  if (length(pd) != 3L) falla("párrafos de distractor: ", length(pd))
  for (p in pd) {
    m <- regmatches(p, regexec("Gráfica (I|II|III|IV), de barras (agrupadas|apiladas): asigna al grado [^ ]+ ([0-9,]+) y ([0-9,]+), y al grado [^ ]+ ([0-9,]+) y ([0-9,]+)", p))[[1]]
    k <- match(m[2], ROM)
    if (k %in% coinciden) falla("un párrafo de distractor describe la clave")
    if (dib[[k]]$formato != sub("as$", "a", m[3]) || max(abs(dib[[k]]$X[rownames(verdad), colnames(verdad)] - matrix(num(m[4:7]), 2, byrow = TRUE))) > 1e-6)
      falla("el párrafo de la Gráfica ", m[2], " no describe su dibujo")
  }
  al1 <- regmatches(sol_txt, gregexpr("\\* (Correcto|Incorrecto)\\. La Gráfica (I|II|III|IV)[^\n]*", sol_txt))[[1]]
  if (length(al1) != 4L) falla("Answerlist de Solution, Parte 1: ", length(al1), " líneas") else for (k in 1:4) {
    r <- sub("^.*La Gráfica (I|II|III|IV).*$", "\\1", al1[k]); kk <- match(r, ROM)
    es_ok <- startsWith(al1[k], "* Correcto")
    if (es_ok != (kk %in% coinciden)) falla("Answerlist Parte 1: ", substr(al1[k], 1, 50))
    nd <- sum(abs(dib[[kk]]$X[rownames(verdad), colnames(verdad)] - verdad) > 1e-6)
    if (!es_ok && !grepl(paste0(" en ", nd, " casilla"), al1[k])) falla("Answerlist Parte 1, Gráfica ", r, ": casillas (real ", nd, ")")
  }

  ## ---- Parte 2 ----
  p2 <- regmatches(q_txt, regexec("\\*\\*Parte 2\\.\\*\\* [^¿]*¿(?:cuántos|cuántas) (.+?) sumaron entre los grados ([^ ]+) y ([^?]+)\\?", q_txt, perl = TRUE))[[1]]
  if (length(p2) < 4) falla("no se pudo leer la Parte 2") else {
    j <- match(p2[2], tolower(cats_tabla))
    if (is.na(j) || !setequal(paste("Grado", p2[3:4]), rownames(verdad))) falla("Parte 2 pregunta por otra categoría o grados")
    else if (num(exs[2]) != sum(verdad[, j])) falla("Parte 2: exsolution ", exs[2], " y la suma real es ", sum(verdad[, j]))
  }
  ## ---- Parte 3 ----
  p3 <- regmatches(q_txt, regexec("termina en ([0-9]+) y el segmento que está encima de él termina en ([0-9]+)", q_txt))[[1]]
  if (length(p3) < 3) falla("no se pudo leer la Parte 3") else if (num(exs[3]) != num(p3[3]) - num(p3[2])) falla("Parte 3: exsolution ", exs[3], " y el segmento mide ", num(p3[3]) - num(p3[2]))
  ## ---- Parte 4 ----
  p4 <- regmatches(q_txt, regexec("los resultados del grado ([^ ]+) estaban en una tabla y los del grado ([^ ]+) en una gráfica de barras\\. Reunidos correctamente, el grado [^ ]+ tuvo ([0-9]+) [^0-9]+ y ([0-9]+) [^,]+, y el grado [^,]+, ([0-9]+) [^0-9]+ y ([0-9]+) [^.]+\\. Un estudiante [^.]* cuyos datos, leídos correctamente, son: grado [^,]+, ([0-9]+) [^0-9]+ y ([0-9]+) [^;]+; grado [^,]+, ([0-9]+) [^0-9]+ y ([0-9]+) ", q_txt, perl = TRUE))[[1]]
  P <- Q <- NULL
  if (length(p4) < 11) falla("no se pudo leer la Parte 4") else {
    P <- matrix(num(p4[4:7]), 2, byrow = TRUE); Q <- matrix(num(p4[8:11]), 2, byrow = TRUE)
    if (any(p4[2:3] %in% sub("^Grado ", "", rownames(verdad)))) falla("Parte 4 repite un grado del caso oficial")
    ops4 <- al_q[5:8]; claves4 <- vapply(ops4, clave_error, character(1))
    if (anyNA(claves4)) falla("Parte 4: opción sin significado conocido: ", paste(ops4[is.na(claves4)], collapse = " / "))
    else {
      prod <- vapply(claves4, function(k) identical(perm_error[[k]](P), Q), logical(1))
      todos <- vapply(names(perm_error), function(k) identical(perm_error[[k]](P), Q), logical(1))
      marca4 <- which(strsplit(exs[4], "")[[1]] == "1")
      if (sum(todos) != 1L) falla("Parte 4: ", sum(todos), " errores de E1-E6 producen la gráfica del estudiante")
      if (sum(prod) != 1L || !identical(unname(which(prod)), marca4)) falla("Parte 4: el error que produce la gráfica no es el marcado")
    }
  }
  ## ---- Parte 5 ----
  ops5 <- al_q[9:13]; marca5 <- strsplit(exs[5], "")[[1]] == "1"
  if (!all(ops5 %in% names(VERDAD_P5))) falla("Parte 5: afirmación sin valor de verdad conocido: ", paste(setdiff(ops5, names(VERDAD_P5)), collapse = " / "))
  else if (!identical(unname(VERDAD_P5[ops5]), marca5)) falla("Parte 5: marcas distintas de la verdad")
  ## ---- Parte 6 ----
  p6 <- regmatches(q_txt, regexec("con el grado ([^ ]+) en el segmento inferior, la barra de (.+?) llega hasta ([0-9]+) en el eje vertical", q_txt))[[1]]
  if (al_q[14] != "Verdadero" || al_q[15] != "Falso") falla("opciones de la Parte 6: ", paste(al_q[14:15], collapse = "/"))
  if (length(p6) < 4 || is.null(P)) falla("no se pudo leer la Parte 6") else {
    if (p6[2] != p4[3]) falla("Parte 6 pone abajo al grado ", p6[2], " y el de la gráfica es ", p4[3])
    j6 <- match(p6[3], tolower(cats_tabla))
    real <- sum(P[, j6]); es_v <- num(p6[4]) == real
    if (!identical(exs[6], if (es_v) "10" else "01")) falla("Parte 6: exsolution ", exs[6], " y la afirmación es ", es_v, " (real ", real, ")")
    if (num(p6[4]) %in% P) falla("Parte 6: el valor ", p6[4], " aparece en la tabla de la Parte 4")
  }
  ## ---- Solution de las Partes 2, 3, 4 y 6 frente a exsolution (detractor FASE 2C, obj. 2) ----
  s2 <- regmatches(sol_txt, regexec("Parte 2 \\{[^}]*\\}\n\n\\*\\*([0-9]+)\\*\\*", sol_txt))[[1]]
  if (length(s2) < 2 || num(s2[2]) != num(exs[2])) falla("Solution Parte 2 distinta de exsolution")
  s3 <- regmatches(sol_txt, regexec("Parte 3 \\{[^}]*\\}\n\n\\*\\*([0-9]+)\\*\\*", sol_txt))[[1]]
  if (length(s3) < 2 || num(s3[2]) != num(exs[3])) falla("Solution Parte 3 distinta de exsolution")
  s4 <- regmatches(sol_txt, regexec("explica por el error \\*\\*E[1-6] — ([^*]+)\\*\\*", sol_txt))[[1]]
  if (length(s4) < 2 || !identical(s4[2], al_q[5:8][strsplit(exs[4], "")[[1]] == "1"])) falla("Solution Parte 4 nombra otro error")
  s6 <- regmatches(sol_txt, regexec("La afirmación es \\*\\*(verdadera|falsa)\\*\\*", sol_txt))[[1]]
  if (length(s6) < 2 || (s6[2] == "verdadera") != identical(exs[6], "10")) falla("Solution Parte 6 invierte el valor de verdad")
  ## ---- Answerlist de Solution (17 = 4 + 1 + 1 + 4 + 5 + 2) y listas de la Parte 5
  ## (re-auditoría FASE 2C, objeción 1: la retroalimentación por opción no tenía guardia) ----
  bs <- sub("(?s)^.*\nAnswerlist\n-+\n", "", sol_txt, perl = TRUE)
  al_s <- regmatches(bs, gregexpr("(?m)^\\* [^\n]+", bs, perl = TRUE))[[1]]
  if (length(al_s) != 17L) falla("Answerlist de Solution con ", length(al_s), " líneas") else {
    b4 <- strsplit(exs[4], "")[[1]] == "1"; b5 <- strsplit(exs[5], "")[[1]] == "1"; b6 <- strsplit(exs[6], "")[[1]] == "1"
    if (!identical(startsWith(al_s[7:10], "* Correcto"), b4)) falla("Answerlist Solution P4 desalineado")
    if (!identical(startsWith(al_s[11:15], "* Verdadera"), b5)) falla("Answerlist Solution P5 desalineado")
    if (!identical(startsWith(al_s[16:17], "* Correcto"), b6)) falla("Answerlist Solution P6 desalineado")
  }
  lv <- regmatches(sol_txt, regexec("(?s)\\*\\*Verdaderas:\\*\\*\n\n(.*?)\n\n\\*\\*Falsas:\\*\\*\n\n(.*?)\n\n", sol_txt, perl = TRUE))[[1]]
  if (length(lv) < 3) falla("Solution P5: no se pudieron leer las listas") else {
    ver <- sub("^- ", "", strsplit(lv[2], "\n")[[1]]); fal <- sub("^- ", "", strsplit(lv[3], "\n")[[1]])
    marca5 <- strsplit(exs[5], "")[[1]] == "1"
    if (!setequal(ver, al_q[9:13][marca5]) || !setequal(fal, al_q[9:13][!marca5])) falla("Solution P5: listas de verdaderas/falsas distintas de exsolution")
  }
  if (any(vapply(PARES_P5, function(p) all(p %in% al_q[9:13]), logical(1)))) falla("Parte 5 muestra los dos miembros de un par")
  ## Por qué de cada par (FASE 2C ciclo 3, objeción 1): un párrafo tras las falsas, con una
  ## explicación por concepto (5), antes del encabezado de la Parte 6.
  pq <- regmatches(sol_txt, regexec("(?s)\\*\\*Falsas:\\*\\*\n\n.*?\n\n\\*Por qué:\\* ([^\n]+)\n\n### Respuesta correcta — Parte 6", sol_txt, perl = TRUE))[[1]]
  if (length(pq) < 2 || lengths(regmatches(pq[2], gregexpr("[.]( |$)", pq[2]))) != 5L) falla("Solution P5: falta el párrafo «Por qué» con las 5 explicaciones")
  ## ---- Solution: sin letras de opción, encabezados con id por versión ----
  if (grepl("[Oo]pci[oó]n [A-Da-d]\\b", sol_txt)) falla("Solution nombra una opción por letra")
  hs <- regmatches(sol_txt, gregexpr("\n### [^\n]+", sol_txt))[[1]]
  ok_h <- grepl("\\{(-|\\.unnumbered) #[a-z0-9-]+-[0-9a-f]{8}\\}\\s*$", hs)
  if (length(hs) != 12L || any(!ok_h)) falla("encabezados de Solution: ", length(hs), " (12 esperados) o sin id por versión")
  list(seed = seed, fallos = fallos, estrato = estrato)
}

cat("verificar_dibujo_clave_cloze.R —", basename(rmd), "— N =", N, "\n")
res <- lapply(semillas, verificar_version)
malos <- Filter(function(r) length(r$fallos) > 0, res)
for (r in head(malos, 15)) cat("  semilla", r$seed, ":", paste(r$fallos, collapse = " || "), "\n")
est <- do.call(rbind, lapply(Filter(function(r) !is.null(r$estrato), res), function(r) as.data.frame(r$estrato)))
cat("\nCobertura por estrato (n de", nrow(est), "versiones; < 20 = NO CONCLUYENTE, regla #23):\n")
for (v in names(est)) { tb <- table(est[[v]]); cat(sprintf("  %-13s %s\n", v, paste(names(tb), tb, sep = "=", collapse = "  "))) }
cat(sprintf("\nRESULTADO: %d/%d versiones sin fallos\n", N - length(malos), N))
quit(status = if (length(malos)) 1L else 0L)
