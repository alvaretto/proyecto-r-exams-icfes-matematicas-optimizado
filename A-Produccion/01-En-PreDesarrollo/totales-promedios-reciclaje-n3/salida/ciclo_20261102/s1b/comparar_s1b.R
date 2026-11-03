## Tanda S1b: invariancia 4cffbe8b -> nuevo, semillas 1..1000 (RDS de salida/ciclo_20261102/capturar.R).
## La única diferencia admitida: «$» + U+00A0 + unidad pasa a «\ \text{unidad}$» (la unidad dentro de la fórmula).
A <- readRDS(commandArgs(TRUE)[1]); B <- readRDS(commandArgs(TRUE)[2])
ida <- function(s) gsub("\\$\u00a0(kilos?|cursos?)", "\\\\ \\\\text{\\1}$", s, perl = TRUE)
cat("semillas:", length(A), length(B), "\n")
cat(".Random.seed idéntico al final de data_generation:", sum(mapply(function(a, b) identical(a$seed, b$seed), A, B)), "de", length(A), "\n")
for (k in c("enunciado", "opciones", "solucion", "feedback"))
  cat(sprintf("texto %-9s idéntico byte a byte: %4d / %d · idéntico tras pasar la unidad a la fórmula: %4d / %d\n", k,
      sum(mapply(function(a, b) identical(a$txt[[k]], b$txt[[k]]), A, B)), length(A),
      sum(mapply(function(a, b) identical(ida(a$txt[[k]]), b$txt[[k]]), A, B)), length(A)))
cat("tabla_md idéntica byte a byte:", sum(mapply(function(a, b) identical(a$tabla_md, b$tabla_md), A, B)), "\n")
cat("enunciado (objeto) idéntico:", sum(mapply(function(a, b) identical(a$enunciado, b$enunciado), A, B)), "\n")
cat("opciones (objeto) idénticas byte a byte:", sum(mapply(function(a, b) identical(a$opciones, b$opciones), A, B)), "\n")
cat("sol, rama, canónica idénticas:", sum(mapply(function(a, b) identical(a$sol, b$sol) && identical(a$rama, b$rama) && identical(a$canon, b$canon), A, B)), "\n")
cnt <- function(L, p) sum(vapply(L, function(x) { t <- paste(x$txt[c("solucion", "feedback")], collapse = "\n")
  lengths(regmatches(t, gregexpr(p, t, perl = TRUE))) }, 0))
cat(sprintf("Solution + feedback, fórmula que cierra antes de su unidad («$» + espacio + unidad): antes %d (control positivo) · después %d\n",
            cnt(A, "\\$[ \u00a0]+(kilos?|cursos?)\\b"), cnt(B, "\\$[ \u00a0]+(kilos?|cursos?)\\b")))
cat(sprintf("Solution + feedback, unidad dentro de la fórmula: antes %d · después %d\n",
            cnt(A, "\\\\ \\\\text\\{(kilos?|cursos?)\\}\\$"), cnt(B, "\\\\ \\\\text\\{(kilos?|cursos?)\\}\\$")))
cat("U+00A0 restantes (total, todos los textos): antes", sum(vapply(A, function(x) sum(lengths(gregexpr("\u00a0", x$txt, fixed = TRUE))), 0)),
    "· después", sum(vapply(B, function(x) sum(lengths(gregexpr("\u00a0", x$txt, fixed = TRUE))), 0)), "\n")
