## medir_p2_redaccion.R — tanda 6, pasada 2 de §P7-D (H1): mide la redacción de la Parte 2 con la batería CONGELADA L-C4
## (enumeración exacta por tipo, como E8 de verificar_espacio_cloze.R) y con cinco sondas EXTRA que no están en la batería
## (la que menos comparte, la que queda fuera del par más parecido, prefijo singleton como H2 de validar_diagnosticidad.R,
## descarta la más corta y las tres primeras palabras): sirven para no abrir un canal inverso al cerrar el directo, y no
## deciden nada. Además, X5, X6 y X10 en la mezcla, por rama (N = 100) y en la canónica (X6 y X10 sobre las 6 claves).
## Solo lee. Uso, desde cloze/salida:  Rscript --no-init-file medir_p2_redaccion.R actual [rapido]
##   RMD=<ruta a otro .Rmd> para medir otra versión (por ejemplo, la de HEAD).
suppressMessages(library(exams))
args <- commandArgs(TRUE); CAND <- args[1]; RAPIDO <- length(args) > 1 && args[2] == "rapido"
DIRC <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze"
RMD <- Sys.getenv("RMD", file.path(DIRC, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
cat("md5 del .Rmd medido:", unname(tools::md5sum(RMD)), "\n")
EB <- new.env(); sys.source(file.path(DIRC, "bateria_partes_2_a_6.R"), envir = EB, keep.source = FALSE)
lin <- readLines(RMD, encoding = "UTF-8"); ini <- grep("^```\\{r data_generation", lin)[1]
fin <- which(lin == "```" & seq_along(lin) > ini)[1]; COD <- paste(lin[(ini + 1):(fin - 1)], collapse = "\n")
if (!is.na(CAND) && CAND != "actual") {
  cand <- paste(readLines(CAND, encoding = "UTF-8"), collapse = "\n")
  i0 <- regexpr("P2 <- list(", COD, fixed = TRUE); i1 <- regexpr("tipo_p2 <- sample(names(P2), 1L)", COD, fixed = TRUE)
  stopifnot(i0 > 0, i1 > i0)
  COD <- paste0(substr(COD, 1, i0 - 1), cand, "\n", substr(COD, i1, nchar(COD)))
}
EXPR <- parse(text = COD, encoding = "UTF-8")
correr <- function(s) { set.seed(s); en <- new.env(); en$.err <- tryCatch({ eval(EXPR, envir = en); NULL }, error = function(e) conditionMessage(e)); en }
toks <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
sc <- function(S, k) { if (!any(S) || all(S)) 0.25 else if (S[k]) 1 / sum(S) else 0 }
mas_comun <- function(o, ref) { r <- toks(paste(ref, collapse = " ")); k <- vapply(o, function(s) length(intersect(toks(s), r)), 0L); k == max(k) }
dentro <- function(S, t) { u <- S & t; if (any(u) && any(S & !u)) u else S }
perm4 <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perm4(v[-i]), function(p) c(v[i], p))))
## (a) Parte 2 exacta, como E8
ENV_T <- list(); for (z in 1:60) { en <- correr(z); if (!is.null(en$.err)) stop("semilla ", z, ": ", en$.err)
  if (is.null(ENV_T[[en$tipo_p2]])) ENV_T[[en$tipo_p2]] <- en; if (length(ENV_T) == 2L) break }
POS <- c("P2-P1", "P2-P2"); peor <- 0
for (tp in c("promedio", "comparar")) {
  en <- ENV_T[[tp]]; q <- en$P2[[tp]]; LE <- list()
  for (cl in q$clave) for (s in utils::combn(length(q$distractores), 3L, simplify = FALSE)) { op0 <- c(cl, unname(q$distractores[s]))
    for (pm in perm4(1:4)) LE[[length(LE) + 1L]] <- list(p2 = list(op = op0[pm], k = which(pm == 1L), tipo = tp, preg = en$texto_p2, otros = c(en$texto_p3, en$texto_p4, en$texto_p5))) }
  M <- EB$medir_p2(LE); M <- M[!M$id %in% POS, ]; peor <- max(peor, M$exceso)
  ## sondas EXTRA (fuera de la batería congelada; solo para no abrir un canal inverso): la que menos comparte, la que
  ## no está en el par más parecido, prefijo singleton (H2 de validar_diagnosticidad.R), la única palabra propia
  ex <- c(
    menos_comparte = mean(vapply(LE, function(x) { s <- rowSums(EB$.pares(x$p2$op)); EB$.sc_op(s == min(s), x$p2$k) }, 0)),
    fuera_del_par = mean(vapply(LE, function(x) { M2 <- EB$.pares(x$p2$op); i <- which(apply(M2 == max(M2), 1, any)); EB$.sc_op(!seq_along(x$p2$op) %in% i, x$p2$k) }, 0)),
    prefijo_singleton = mean(vapply(LE, function(x) { f <- vapply(x$p2$op, function(o) strsplit(o, " ")[[1]][1], ""); s <- f %in% names(which(table(f) == 1)); EB$.sc_op(s, x$p2$k) }, 0)),
    descarta_corta = mean(vapply(LE, function(x) { n <- nchar(x$p2$op); EB$.sc_op(n != min(n), x$p2$k) }, 0)),
    tres_palabras = mean(vapply(LE, function(x) { f <- vapply(x$p2$op, function(o) paste(strsplit(o, " ")[[1]][1:3], collapse = " "), ""); s <- f %in% names(which(table(f) == 1)); EB$.sc_op(s, x$p2$k) }, 0)))
  cat(sprintf("  EXTRA: %s\n", paste(sprintf("%s %+.1f", names(ex), 100 * (ex - .25)), collapse = " · ")))
  cat(sprintf("Parte 2 «%s» exacta (%d): %s\n", tp, length(LE), paste(sprintf("%s %+.1f", M$id, 100 * M$exceso), collapse = " · ")))
  r <- M[M$exceso > 0.08 + 1e-12, ]; if (nrow(r)) cat("  EN ROJO:", paste(sprintf("%s %+.1f", r$id, 100 * r$exceso), collapse = " · "), "\n")
  todas <- c(q$clave, q$distractores); cat("  largos clave:", nchar(q$clave), "· distractores:", nchar(q$distractores), "\n")
  bad <- grepl("[0-9]|:|=|estudiante|error|equivoc|confundi|olvid", todas); if (any(bad)) cat("  PROHIBIDO en:", todas[bad], "\n")
  cat("  pregunta:", en$texto_p2, "\n")
}
cat(sprintf("\nPEOR exceso de la Parte 2: %+.1f pp\n", 100 * peor))
if (RAPIDO) quit(save = "no")
## (b) canónica y muestras: X5, X6, X10
rec <- function(en) with(en, list(op = opciones_p1, k = which(sol_p1), su = vapply(afs[rol_opcion], function(a) a$sede, "") != vapply(afs[rol_opcion], function(a) a$sede, "")[rol_opcion == "psin"],
  ref5 = c(intro_par, texto_p2, opciones_p2, texto_p3, texto_p4, texto_p5, texto_p6, opciones_p6), kp2 = opciones_p2[sol_p2], rama = rama, canon = isTRUE(es_canonica),
  P2 = P2, q2 = q2, cl2 = opciones_p2[sol_p2], resto5 = c(intro_par, texto_p2, texto_p3, texto_p4, texto_p5, texto_p6, opciones_p6)))
xr <- function(x) c(X5 = sc(mas_comun(x$op, x$ref5), x$k), X6 = sc(mas_comun(x$op, x$kp2), x$k), X10 = sc(dentro(x$su, mas_comun(x$op, x$kp2)), x$k))
NAT <- lapply(1:100, function(z) rec(correr(z)))
CU <- list(CURSOS = list(), PROMEDIO = list()); CAN <- list()
for (z in 1000L + 1:4000) { if (length(CU$CURSOS) >= 100 && length(CU$PROMEDIO) >= 100 && length(CAN) >= 20) break
  x <- rec(correr(z)); if (x$canon) { if (length(CAN) < 100) CAN[[length(CAN) + 1]] <- x; next }
  if (length(CU[[x$rama]]) < 100) CU[[x$rama]][[length(CU[[x$rama]]) + 1]] <- x }
for (nm in c("mezcla", "CURSOS", "PROMEDIO")) { L <- switch(nm, mezcla = NAT, CU[[nm]]); V <- rowMeans(sapply(L, xr))
  cat(sprintf("%-9s n = %3d: X5 %+.1f · X6 %+.1f · X10 %+.1f pp\n", nm, length(L), 100 * (V[1] - .25), 100 * (V[2] - .25), 100 * (V[3] - .5))) }
## canónica: X6 y X10 sobre las 6 claves (uniforme, como canon_transcrita); X5 integrando los tríos de distractores
x0 <- CAN[[1]]; claves <- unlist(lapply(x0$P2, function(q) q$clave))
x6 <- mean(vapply(claves, function(kp) sc(mas_comun(x0$op, kp), x0$k), 0)); x10 <- mean(vapply(claves, function(kp) sc(dentro(x0$su, mas_comun(x0$op, kp)), x0$k), 0))
x5 <- mean(vapply(CAN, function(x) mean(vapply(utils::combn(length(x$q2$distractores), 3L, simplify = FALSE), function(s)
  sc(mas_comun(x$op, c(x$resto5, x$cl2, unname(x$q2$distractores[s]))), x$k), 0)), 0))
cat(sprintf("canónica: X6 %+.1f (firmado +16,7) · X10 %+.1f (firmado +16,7) · X5 %+.1f pp (muestra de %d canónicas, tríos integrados)\n",
            100 * (x6 - .25), 100 * (x10 - .5), 100 * (x5 - .25), length(CAN)))
cat("  canónica, Parte 1:", paste0(ifelse(seq_along(x0$op) == x0$k, "[CLAVE] ", ""), x0$op, collapse = " | "), "\n")
for (kp in claves) cat(sprintf("  clave P2 «%s»: comunes con A-D = %s\n", kp, paste(vapply(x0$op, function(o) length(intersect(toks(o), toks(kp))), 0L), collapse = "/")))
