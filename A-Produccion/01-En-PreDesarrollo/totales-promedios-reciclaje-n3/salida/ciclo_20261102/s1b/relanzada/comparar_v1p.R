## Tanda S1b relanzada: invariancia A -> B en las semillas 1..1000 (RDS de salida/ciclo_20261102/capturar.R) y prueba
## sobre el TEXTO EMITIDO (enunciado, opciones, Solution y feedback): tras el cierre de una fórmula que termina en su
## unidad no queda ningún signo. Uso: Rscript comparar_v1p.R <A.rds> <B.rds> <modo: v1p | identico>
##   v1p      : la única diferencia admitida es «\text{unidad}$<signo>» -> «\text{unidad<signo>}$» en Solution y feedback.
##   identico : todo idéntico byte a byte (prueba de código muerto: quitar lado_gdis de la canónica).
a <- commandArgs(TRUE); A <- readRDS(a[1]); B <- readRDS(a[2]); modo <- a[3]
SIG <- "[.,:;!?)\\]»”…]"
ida <- if (modo == "v1p") function(s) gsub(paste0("\\\\text\\{(kilos?|cursos?)\\}\\$(", SIG, ")"), "\\\\text{\\1\\2}$", s, perl = TRUE) else identity
cat("semillas:", length(A), length(B), "· modo:", modo, "\n")
cat(".Random.seed idéntico al final de data_generation:", sum(mapply(function(x, y) identical(x$seed, y$seed), A, B)), "de", length(A), "\n")
for (k in c("enunciado", "opciones", "solucion", "feedback"))
  cat(sprintf("texto %-9s idéntico byte a byte: %4d / %d · idéntico tras normalizar la posición del signo: %4d / %d\n", k,
      sum(mapply(function(x, y) identical(x$txt[[k]], y$txt[[k]]), A, B)), length(A),
      sum(mapply(function(x, y) identical(ida(x$txt[[k]]), y$txt[[k]]), A, B)), length(A)))
for (k in c("tabla_md", "enunciado", "opciones", "feedback", "sol", "rama", "canon", "nomG", "conj_filas"))
  cat(sprintf("objeto %-10s idéntico: %4d / %d\n", k, sum(mapply(function(x, y) identical(if (k == "feedback") ida(x[[k]]) else x[[k]], y[[k]]), A, B)), length(A)))
cnt <- function(L, p, ks = c("enunciado", "opciones", "solucion", "feedback")) sum(vapply(L, function(x) {
  t <- paste(x$txt[ks], collapse = "\n"); lengths(regmatches(t, gregexpr(p, t, perl = TRUE))) }, 0))
por_v <- function(L, p) vapply(L, function(x) { t <- paste(x$txt, collapse = "\n"); lengths(regmatches(t, gregexpr(p, t, perl = TRUE))) }, 0)
P_FUERA <- paste0("\\\\text\\{(kilos?|cursos?)\\}\\$", SIG); P_DENTRO <- paste0("\\\\text\\{(kilos?|cursos?)", SIG, "\\}\\$")
cat(sprintf("TEXTO EMITIDO, signo tras el cierre de una fórmula que termina en su unidad: A %d · B %d\n", cnt(A, P_FUERA), cnt(B, P_FUERA)))
cat(sprintf("TEXTO EMITIDO, unidad con el signo dentro de la fórmula: A %d · B %d (B: en %d de %d versiones)\n",
            cnt(A, P_DENTRO), cnt(B, P_DENTRO), sum(por_v(B, P_DENTRO) > 0), length(B)))
cat(sprintf("TEXTO EMITIDO, fórmula que cierra antes de su unidad («$» + espacio + unidad): A %d · B %d\n",
            cnt(A, "\\$[  ]+(kilos?|cursos?)\\b"), cnt(B, "\\$[  ]+(kilos?|cursos?)\\b")))
## lo que sigue a «\text{unidad...}$» en B, para ver que no queda ningún signo en otra cadena
sig_tras <- function(L) { t <- unlist(lapply(L, function(x) regmatches(x$txt, gregexpr("\\\\text\\{(kilos?|cursos?)[^}]*\\}\\$.{0,2}", x$txt, perl = TRUE))))
  sort(table(sub("^.*\\}\\$", "»", sub("^\\\\text\\{", "", t))), decreasing = TRUE) }
cat("B, lo que sigue al cierre «$» de una fórmula que termina en su unidad (» marca el cierre):\n"); print(sig_tras(B))
cat(sprintf("Informativo: fórmulas que NO terminan en unidad seguidas de un signo: A %d · B %d\n",
            cnt(A, paste0("(?<!\\})\\$", SIG)), cnt(B, paste0("(?<!\\})\\$", SIG))))
cat("U+00A0 restantes (todos los textos): A", sum(vapply(A, function(x) sum(lengths(gregexpr(" ", x$txt, fixed = TRUE))), 0)),
    "· B", sum(vapply(B, function(x) sum(lengths(gregexpr(" ", x$txt, fixed = TRUE))), 0)), "\n")
