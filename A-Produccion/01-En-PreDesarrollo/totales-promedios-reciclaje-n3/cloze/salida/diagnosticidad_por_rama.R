#!/usr/bin/env Rscript
## diagnosticidad_por_rama.R — paso 9 del CLOZE (tanda 3, 2026-10-30): V8 y V9 por gap Y por estrato.
## validar_diagnosticidad.R y validar_diversidad_sustantiva.R miden cada gap sobre todas las versiones juntas, y el
## agregado no acredita las ramas (orquestador-cloze, V9 «El agregado no acredita las ramas»). Aquí, con N = 100 por
## estrato (regla #23; un estrato con n < 20 sería NO CONCLUYENTE):
##   - V9, gaps de selección única (p1, p2): H1 y H2 con las mismas definiciones que validar_diagnosticidad.R (la clave
##     es la ÚNICA más larga o más corta con margen >= 15 % frente a su rival más próximo; la clave es la única con su
##     primera palabra y el único singleton); H3 (la primera palabra de la clave, la misma en todo el estrato); y las
##     reglas de acierto «elige la más larga», «elige la más corta» y «descarta la más larga», contra el 25 % del azar,
##     con el margen máximo de cada una (§P7-B: < 15 % es inexplotable).
##   - Parte 6 (mchoice): verdaderas por papel, por longitud (la más larga y la más corta) y número de verdaderas.
##   - V8: claves distintas por gap en cada estrato.
## Estratos: p1 por la rama de la Parte 1 (CURSOS y PROMEDIO no canónicas, y la canónica); p2 por tipo de pregunta;
## p3 a p6 por la rama del otro colegio; más la mezcla natural (semillas 1..N). Las funciones de lectura (rec, chunk,
## correr) se toman por parse de ../verificar_espacio_cloze.R, sin ejecutarlo.
## Uso (desde cloze/salida): Rscript --no-init-file diagnosticidad_por_rama.R [N]
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
args <- commandArgs(trailingOnly = TRUE); N <- if (length(args)) as.integer(args[1]) else 100L
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
QUIERO <- c("toks", "cif1", "num_all", "tabla_leida", "sujeto", "rec", "chunk", "MARCA", "cap1", "correr")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% QUIERO)
  eval(e, envir = globalenv())
RMD <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(RMD)), "· N =", N, "por estrato\n")
EXPR <- parse(text = chunk(RMD), encoding = "UTF-8")
TOPE <- 20000L
EST <- list(mezcla = list(), p1_CURSOS = list(), p1_PROMEDIO = list(), p1_canonica = list(),
            p2_promedio = list(), p2_comparar = list(), par_CURSOS = list(), par_PROMEDIO = list())
lleno <- function() all(lengths(EST) >= N)
for (z in seq_len(TOPE)) {
  if (lleno()) break
  en <- correr(EXPR, z); if (!is.null(en$.err)) stop("semilla ", z, ": ", en$.err)
  x <- rec(en)
  put <- function(k) if (length(EST[[k]]) < N) EST[[k]][[length(EST[[k]]) + 1L]] <<- x
  if (z <= N) put("mezcla")
  put(if (x$canon) "p1_canonica" else paste0("p1_", x$rama))
  put(paste0("p2_", x$tipo_p2)); put(paste0("par_", x$rama_par))
}
cat("semillas recorridas: 1..", z, " · n por estrato: ", paste(names(EST), lengths(EST), sep = " = ", collapse = " · "), "\n", sep = "")
stopifnot("ningún estrato por debajo de 20 (regla #23)" = all(lengths(EST) >= 20L))

sc <- function(S, k) { if (!any(S) || all(S)) 0.25 else if (S[k]) 1 / sum(S) else 0 }
pw <- function(tx) tolower(sub("^[^[:alnum:]]*([[:alnum:]á-úÁ-Úñü]+).*$", "\\1", tx))
v9 <- function(L, campo_op, campo_k) {
  r <- t(vapply(L, function(x) {
    tx <- x[[campo_op]]; k <- campo_k(x); n <- nchar(tx)
    umax <- n[k] == max(n) && sum(n == max(n)) == 1L; umin <- n[k] == min(n) && sum(n == min(n)) == 1L
    mmax <- if (umax) (n[k] - max(n[-k])) / max(n[-k]) else NA_real_
    mmin <- if (umin) (min(n[-k]) - n[k]) / min(n[-k]) else NA_real_
    p <- pw(tx); tb <- table(p)
    c(h1max = isTRUE(umax && mmax >= 0.15), h1min = isTRUE(umin && mmin >= 0.15),
      h2 = tb[[p[k]]] == 1L && sum(tb == 1L) == 1L, uniforme = length(unique(p)) == 1L,
      larga = sc(n == max(n), k), corta = sc(n == min(n), k), no_larga = sc(n != max(n), k),
      mmax = mmax, mmin = mmin)
  }, numeric(9)))
  pk <- vapply(L, function(x) pw(x[[campo_op]][campo_k(x)]), "")
  c(n = length(L), H1_larga = 100 * mean(r[, "h1max"]), H1_corta = 100 * mean(r[, "h1min"]), H2 = 100 * mean(r[, "h2"]),
    prefijo_uniforme = 100 * mean(r[, "uniforme"]), H3 = 100 * max(table(pk)) / length(pk),
    elige_larga = 100 * mean(r[, "larga"]), elige_corta = 100 * mean(r[, "corta"]), descarta_larga = 100 * mean(r[, "no_larga"]),
    margen_max_larga = 100 * max(c(r[, "mmax"], 0), na.rm = TRUE), margen_max_corta = 100 * max(c(r[, "mmin"], 0), na.rm = TRUE))
}
cat("\n== V9 por estrato, gap p1 (Parte 1, schoice) ==\n")
T1 <- sapply(EST[c("mezcla", "p1_CURSOS", "p1_PROMEDIO", "p1_canonica")], v9, campo_op = "opciones", campo_k = function(x) x$clave)
print(round(T1, 1))
cat("\n== V9 por estrato, gap p2 (Parte 2, schoice) ==\n")
T2 <- sapply(EST[c("mezcla", "p2_promedio", "p2_comparar", "par_CURSOS", "par_PROMEDIO")], v9, campo_op = "opciones_p2", campo_k = function(x) which(x$sol_p2))
print(round(T2, 1))
cat("\n(acierto de «elige la más larga», «elige la más corta» y «descarta la más larga» contra el 25 % del azar; H3 es la\n",
    " cuota de la primera palabra más frecuente de la clave; con prefijo uniforme, H2 y H3 no miden nada)\n", sep = "")

cat("\n== Parte 6 (mchoice) por rama del otro colegio: % de verdaderas ==\n")
p6 <- function(L) {
  por_rol <- vapply(c("hp", "hg", "ventaja", "ganador"), function(rl) 100 * mean(unlist(lapply(L, function(x) x$sol_p6[x$rol_p6 == rl]))), 0)
  larga <- 100 * mean(vapply(L, function(x) { n <- nchar(x$opciones_p6); mean(x$sol_p6[n == max(n)]) }, 0))
  corta <- 100 * mean(vapply(L, function(x) { n <- nchar(x$opciones_p6); mean(x$sol_p6[n == min(n)]) }, 0))
  nv <- table(factor(vapply(L, function(x) sum(x$sol_p6), 0L), levels = 1:3))
  c(n = length(L), por_rol, la_mas_larga = larga, la_mas_corta = corta, todas = 100 * mean(unlist(lapply(L, function(x) x$sol_p6))),
    `1 verdadera` = nv[["1"]], `2 verdaderas` = nv[["2"]], `3 verdaderas` = nv[["3"]])
}
print(round(sapply(EST[c("mezcla", "par_CURSOS", "par_PROMEDIO")], p6), 1))

cat("\n== V8: claves distintas por gap y por estrato ==\n")
v8 <- function(L) c(n = length(L),
  p1 = length(unique(vapply(L, function(x) x$opciones[x$clave], ""))),
  p2 = length(unique(vapply(L, function(x) x$opciones_p2[x$sol_p2], ""))),
  p3 = length(unique(vapply(L, function(x) x$exsol[1], 0))), p4 = length(unique(vapply(L, function(x) x$exsol[2], 0))),
  p5 = length(unique(vapply(L, function(x) x$exsol[3], 0))),
  p6 = length(unique(vapply(L, function(x) paste(sort(x$opciones_p6[x$sol_p6]), collapse = "|"), ""))))
print(sapply(EST, v8))
cat("\n(p1 en la canónica: clave verbatim, una sola por diseño (L-1); p2: clave conceptual fija por tipo, 3 paráfrasis (V8 fija-justificada))\n")
