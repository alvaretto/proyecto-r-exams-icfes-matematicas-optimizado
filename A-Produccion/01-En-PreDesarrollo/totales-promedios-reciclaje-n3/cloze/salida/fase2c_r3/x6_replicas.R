## Detractor r3 — L-C2 exacta con código propio: X5, X6, X10 integradas sobre las 30 combinaciones de la Parte 2,
## en la muestra del verificador (réplica 1) y en 4 réplicas disjuntas más de N = 100 por rama (variabilidad por Parte 1).
suppressMessages(library(exams))
RMD <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"
L <- readLines(RMD, encoding = "UTF-8"); ini <- grep("^```\\{r data_generation", L)[1]; ci <- which(L == "```"); fin <- ci[ci > ini][1]
EXPR <- parse(text = paste(L[(ini + 1):(fin - 1)], collapse = "\n"), encoding = "UTF-8")
run <- function(s) { set.seed(s); en <- new.env(); eval(EXPR, envir = en); en }
tk <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
maxcomun <- function(op, ref) { r <- tk(paste(ref, collapse = " ")); k <- vapply(op, function(o) length(intersect(tk(o), r)), 0L); k == max(k) }
punt <- function(S, k) if (!any(S) || all(S)) 0.25 else if (S[k]) 1 / sum(S) else 0
restringe <- function(S, t) { u <- S & t; if (any(u) && any(S & !u)) u else S }
reg <- function(en) with(en, {
  sede <- vapply(afs[rol_opcion], function(a) a$sede, "")
  list(rama = rama, canon = isTRUE(es_canonica), op = opciones_p1, k = which(sol_p1), su = sede != sede[rol_opcion == "psin"],
       resto = c(intro_par, texto_p2, texto_p3, texto_p4, texto_p5, texto_p6, opciones_p6), clave_vista = clave_p2) })
B <- run(1L)$P2$promedio; CL <- unname(B$clave); DI <- unname(B$distractores)
COMB <- list(); for (i in 1:3) for (t in combn(5, 3, simplify = FALSE)) COMB[[length(COMB) + 1]] <- list(ci = i, op = c(CL[i], DI[t]))
medir <- function(V) {
  m <- t(vapply(V, function(x) { r <- vapply(COMB, function(cb) c(
      X5 = punt(maxcomun(x$op, c(x$resto, cb$op)), x$k),
      X6 = punt(maxcomun(x$op, cb$op[1]), x$k),
      X10 = punt(restringe(x$su, maxcomun(x$op, cb$op[1])), x$k)), c(X5 = 0, X6 = 0, X10 = 0))
    ci <- vapply(COMB, function(cb) cb$ci, 0L)
    c(rowMeans(r), X6k1 = mean(r["X6", ci == 1]), X6k2 = mean(r["X6", ci == 2]), X6k3 = mean(r["X6", ci == 3])) }, numeric(6)))
  round(100 * (colMeans(m) - c(0.25, 0.25, 0.50, 0.25, 0.25, 0.25)), 1)
}
NAT <- lapply(1:100, function(s) reg(run(s)))
cat("mezcla (semillas 1:100):", paste(names(medir(NAT)), medir(NAT), collapse = " · "), "\n")
CU <- list(CURSOS = list(), PROMEDIO = list()); z <- 1000L
while ((length(CU$CURSOS) < 500 || length(CU$PROMEDIO) < 500) && z < 4000L) {
  z <- z + 1L; x <- reg(run(z)); if (x$canon) next
  if (length(CU[[x$rama]]) < 500) CU[[x$rama]][[length(CU[[x$rama]]) + 1]] <- x
}
cat("semillas usadas: 1001 ..", z, "\n")
for (b in names(CU)) for (r in 1:5) {
  V <- CU[[b]][(100 * (r - 1) + 1):(100 * r)]
  cat(sprintf("%-8s réplica %d (N = 100): %s\n", b, r, paste(names(medir(V)), medir(V), collapse = " · ")))
}
for (b in names(CU)) cat(sprintf("%-8s las 500 juntas (solo depuración): %s\n", b, paste(names(medir(CU[[b]])), medir(CU[[b]]), collapse = " · ")))
