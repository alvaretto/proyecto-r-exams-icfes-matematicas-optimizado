suppressMessages(library(exams))
DIRS <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3"
chunk <- function(f) { L <- readLines(f, encoding = "UTF-8"); i <- grep("^```\\{r data_generation", L)[1]; ci <- which(L == "```"); L[(i + 1):(ci[ci > i][1] - 1)] }
LS <- chunk(file.path(DIRS, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd"))
LC <- chunk(file.path(DIRS, "cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))
cut <- grep("^## ==== FIN DEL BLOQUE P1 ====", LC)
ES <- parse(text = paste(LS, collapse = "\n"), encoding = "UTF-8"); EP1 <- parse(text = paste(LC[1:(cut - 1)], collapse = "\n"), encoding = "UTF-8")
EC <- parse(text = paste(LC, collapse = "\n"), encoding = "UTF-8")
ev <- function(e, s) { set.seed(s); en <- new.env(); eval(e, envir = en); en$.seed <- .Random.seed; en }
ren <- c(opciones = "opciones_p1", sol = "sol_p1", feedback = "feedback_p1")
dif <- character(0); ncan <- 0; can_seeds <- integer(0)
sem <- c(1:100, 101:400)
for (s in sem) {
  A <- ev(ES, s); if (s > 100 && !isTRUE(A$es_canonica)) next
  if (s > 100) { if (length(can_seeds) >= 20) next; can_seeds <- c(can_seeds, s) }
  ncan <- ncan + isTRUE(A$es_canonica); B <- ev(EP1, s); C <- ev(EC, s)
  for (k in setdiff(ls(A), names(ren))) if (!identical(A[[k]], B[[k]], ignore.environment = TRUE)) dif <- c(dif, paste(s, k))
  for (k in names(ren)) if (!identical(A[[k]], B[[ren[[k]]]]) || !identical(A[[k]], C[[ren[[k]]]])) dif <- c(dif, paste(s, k))
  if (!identical(A$.seed, B$.seed)) dif <- c(dif, paste(s, ".seed"))
  for (k in c("enunciado", "tabla_md", "pregunta", "respuesta_correcta")) if (!identical(A[[k]], C[[k]])) dif <- c(dif, paste(s, k, "en el CLOZE completo"))
}
cat("E1 propio:", 100 + length(can_seeds), "versiones (", ncan, "canónicas); diferencias:", length(dif), "\n"); if (length(dif)) print(head(dif))
## P2-R1, P2-R2, P2-R3 exactas (código propio, misma definición de regla): 3 claves x C(5,3) tríos x 24 órdenes
B <- ev(EC, 1L)$P2$promedio; CL <- unname(B$clave); DI <- unname(B$distractores)
tk <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
M <- function(op) { T <- lapply(op, tk); outer(1:4, 1:4, Vectorize(function(i, j) if (i == j) 0L else length(intersect(T[[i]], T[[j]])))) }
pt <- function(S, k) if (!any(S) || all(S)) 0.25 else if (S[k]) 1 / sum(S) else 0
perms <- function(v) if (length(v) == 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perms(v[-i]), function(p) c(v[i], p))))
P <- perms(1:4); acc <- c(R1 = 0, R2 = 0, R3 = 0); nn <- 0; porclave <- matrix(0, 3, 3, dimnames = list(paste0("k", 1:3), c("R1", "R2", "R3")))
for (i in 1:3) for (t in combn(5, 3, simplify = FALSE)) for (p in P) {
  op <- c(CL[i], DI[t])[p]; k <- which(p == 1); m <- M(op); s <- rowSums(m)
  r <- c(R1 = pt(apply(m == max(m), 1, any), k), R2 = pt(s == max(s), k), R3 = pt(s != min(s), k))
  acc <- acc + r; porclave[i, ] <- porclave[i, ] + r; nn <- nn + 1 }
cat("Parte 2 exacta (720):", paste(names(acc), sprintf("%+.1f", 100 * (acc / nn - 0.25)), collapse = " · "), "\n")
print(round(100 * (porclave / (nn / 3) - 0.25), 1))
