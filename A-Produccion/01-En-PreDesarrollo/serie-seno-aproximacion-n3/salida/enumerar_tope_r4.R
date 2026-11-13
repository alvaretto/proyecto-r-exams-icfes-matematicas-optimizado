## Pasada 4 (profesor, 2026-11-13), pendiente 2: por rama (x, n) y por tope de |valor|, cuántos errores del banco quedan
## elegibles ADEMÁS de la trampa SS-C (aplicables, distintos de la clave, de la trampa y entre sí).
suma <- function(x, n, nt, fact, sgn) { D <- factorial(2 * n - 1); if (nt < 1) return(0); k <- 0:(nt - 1); sum(sgn(k) * x^(2 * k + 1) * D / fact(k)) }
alt <- function(k) (-1)^k; fi <- function(k) factorial(2 * k + 1); fp <- function(k) factorial(2 * k)
BANCO <- list(
  A = function(x, n) suma(x, n, n, fp, function(k) ifelse(k == 0, 1, -1)), B = function(x, n) suma(x, n, n, fp, function(k) 1),
  D = function(x, n) suma(x, n, n, fi, function(k) 1), E = function(x, n) suma(x, n, n, function(k) 2 * k + 1, alt),
  F = function(x, n) suma(x, n, n - 1, fi, alt), G = function(x, n) suma(x, n, n, function(k) ifelse(k == 1, 2, factorial(2 * k + 1)), alt),
  H = function(x, n) suma(x, n, n, fp, alt), I = function(x, n) suma(x, n, n, fi, function(k) ifelse(k == 0, 1, -(-1)^k)),
  N = function(x, n) suma(x, n, n, fi, function(k) ifelse(k == n - 1, -(-1)^k, (-1)^k)),
  O = function(x, n) suma(x, n, n, function(k) (2 * k + 1) * (k + 1), alt))
TOPES <- list("<= 1" = function(x) 1, "<= 2" = function(x) 2, "<= 3" = function(x) 3, "<= 4" = function(x) 4,
              "<= |x|" = function(x) abs(x), "<= 1,5|x|" = function(x) 1.5 * abs(x), "<= 2|x|" = function(x) 2 * abs(x))
filas <- list()
for (x in c(2, 3, -2, -3)) for (n in 3:4) {
  D <- factorial(2 * n - 1); kv <- suma(x, n, n, fi, alt); tr <- round(sin(x) * D)
  v <- vapply(BANCO, function(f) f(x, n), 0); v <- v[abs(v - kv) > 0.5 & abs(v - tr) > 0.5]; v <- v[!duplicated(round(v))]
  cnt <- vapply(TOPES, function(t) sum(abs(v / D) <= t(x) + 1e-12), 0)
  filas[[length(filas) + 1]] <- data.frame(rama = sprintf("x=%d,n=%d", x, n), t(cnt), check.names = FALSE,
    valores = paste(sprintf("%s=%.2f", names(v), v / D), collapse = " "))
}
T <- do.call(rbind, filas); print(T[, c("rama", names(TOPES))], row.names = FALSE)
ok <- vapply(names(TOPES), function(t) all(T[[t]] >= 3), TRUE)
cat("\nTope que deja >= 3 errores elegibles además de SS-C en TODAS las ramas:\n"); print(ok)
cat("\nMáximo |valor| que admite cada tope (en la rama más permisiva): ",
    paste(sprintf("%s -> %.1f", names(TOPES), vapply(TOPES, function(t) max(t(c(2, 3))), 0)), collapse = " · "), "\n")
cat("\nValores |v|/D por rama:\n"); for (i in seq_len(nrow(T))) cat(T$rama[i], ":", T$valores[i], "\n")
