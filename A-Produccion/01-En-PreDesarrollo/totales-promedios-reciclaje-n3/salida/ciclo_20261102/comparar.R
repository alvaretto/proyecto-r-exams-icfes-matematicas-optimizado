A <- readRDS(commandArgs(TRUE)[1]); B <- readRDS(commandArgs(TRUE)[2]); NB <- "\u00a0"
norm <- function(s) gsub(NB, " ", s, fixed = TRUE)
cat("semillas:", length(A), length(B), "\n")
cat(".Random.seed idéntico al final de data_generation:", sum(mapply(function(a, b) identical(a$seed, b$seed), A, B)), "de", length(A), "\n")
for (k in c("enunciado", "opciones", "solucion", "feedback"))
  cat(sprintf("texto %-9s idéntico salvo el espacio duro: %d / %d; con diferencias de espacio: %d\n", k,
      sum(mapply(function(a, b) identical(norm(a$txt[[k]]), norm(b$txt[[k]])), A, B)), length(A),
      sum(mapply(function(a, b) !identical(a$txt[[k]], b$txt[[k]]), A, B))))
cat("tabla_md idéntica byte a byte:", sum(mapply(function(a, b) identical(a$tabla_md, b$tabla_md), A, B)), "\n")
cat("sol, rama, canónica idénticas:", sum(mapply(function(a, b) identical(a$sol, b$sol) && identical(a$rama, b$rama) && identical(a$canon, b$canon), A, B)), "\n")
cat("opciones idénticas salvo el espacio:", sum(mapply(function(a, b) identical(norm(a$opciones), norm(b$opciones)), A, B)), "\n")
pat <- "[0-9$] (kilos?|cursos?)\\b|El Bosque|La Pradera"
cuenta <- function(L, k) sum(vapply(L, function(x) lengths(regmatches(x$txt[[k]], gregexpr(pat, x$txt[[k]], perl = TRUE))), 0))
for (k in c("opciones", "solucion", "feedback", "enunciado"))
  cat(sprintf("%-9s cifra-unidad o nombre con espacio normal: antes %5d (control positivo) · después %d\n", k, cuenta(A, k), cuenta(B, k)))
## Cada espacio duro nuevo, en su contexto: lo que lo rodea
ctx <- unlist(lapply(B, function(x) { s <- paste(x$txt[c("enunciado","opciones","solucion","feedback")], collapse = "\n")
  regmatches(s, gregexpr(paste0(".{0,3}", NB, "[[:alpha:]]+"), s, perl = TRUE))[[1]] }))
ctx <- gsub(NB, "_", ctx); ctx <- sub("^.*?([0-9$]|El|La)_", "\\1_", ctx)
print(sort(table(sub("^[0-9]+_", "N_", ctx)), decreasing = TRUE))
