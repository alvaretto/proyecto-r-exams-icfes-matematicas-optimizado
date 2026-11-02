suppressMessages(library(exams))
chunk <- function(f) { L <- readLines(f, encoding = "UTF-8"); i <- grep("^```\\{r data_generation", L)[1]; ci <- which(L == "```"); parse(text = paste(L[(i + 1):(ci[ci > i][1] - 1)], collapse = "\n"), encoding = "UTF-8") }
EN <- chunk("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
EV <- chunk("viejo_898608dd.Rmd")
ev <- function(e, s) { set.seed(s); en <- new.env(); eval(e, envir = en); en$.seed <- .Random.seed; en }
cmp <- c("tabla_par_md", "intro_par", "texto_p3", "texto_p4", "texto_p5", "exsol_p3", "exsol_p4", "exsol_p5", "opciones_p6", "sol_p6", "feedback_p6", "sol_p2", ".seed")
d <- character(0); nprom <- 0; dp2 <- 0
for (s in 1:100) { A <- ev(EN, s); B <- ev(EV, s)
  for (k in cmp) if (!identical(A[[k]], B[[k]])) d <- c(d, paste(s, k))
  if (B$tipo_p2 == "promedio") { nprom <- nprom + 1; if (!identical(A$opciones_p2, B$opciones_p2) || !identical(A$feedback_p2, B$feedback_p2)) dp2 <- dp2 + 1 } }
cat("RNG frente a 898608dd (semillas 1:100):", length(d), "diferencias en Partes 3-6, posición de la clave P2 y .Random.seed final;",
    nprom, "versiones eran «promedio», de ellas", dp2, "con opciones o retro de la Parte 2 distintas\n"); if (length(d)) print(head(d))
## Canónica: X6 y X10 exactas sobre el sorteo de la Parte 2 (código propio)
tk <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
mc <- function(op, ref) { r <- tk(paste(ref, collapse = " ")); k <- vapply(op, function(o) length(intersect(tk(o), r)), 0L); k == max(k) }
pt <- function(S, k) if (!any(S) || all(S)) 0.25 else if (S[k]) 1 / sum(S) else 0
rs <- function(S, t) { u <- S & t; if (any(u) && any(S & !u)) u else S }
CL <- unname(ev(EN, 1L)$P2$promedio$clave)
x6 <- c(); x10 <- c(); n <- 0; s <- 100L
while (n < 30 && s < 2000) { s <- s + 1L; A <- ev(EN, s); if (!isTRUE(A$es_canonica)) next; n <- n + 1
  sede <- vapply(A$afs[A$rol_opcion], function(a) a$sede, ""); su <- sede != sede[A$rol_opcion == "psin"]; k <- which(A$sol_p1)
  x6 <- c(x6, mean(vapply(CL, function(cl) pt(mc(A$opciones_p1, cl), k), 0))); x10 <- c(x10, mean(vapply(CL, function(cl) pt(rs(su, mc(A$opciones_p1, cl)), k), 0))) }
cat(sprintf("canónica (n = %d, exacta sobre la Parte 2): X6 %.1f %% · X10 %.1f %%\n", n, 100 * mean(x6), 100 * mean(x10)))
