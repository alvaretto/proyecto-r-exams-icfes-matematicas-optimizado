#!/usr/bin/env Rscript
## Enumeración exacta del espacio de diseño (paso 3, antes de fijar el .Rmd).
## Uso: Rscript --no-init-file salida/enumerar_diseno.R "<xs separados por coma>" "<ns>" [pool]
## Mide, por rama (x, n) y en agregado, con pesos uniformes por rama y por terna:
##   unicidad, clave única, rango de la clave, y la tasa de acierto (convención 1/|S|) de reglas de eliminación.
args <- commandArgs(TRUE)
xs <- if (length(args) >= 1) eval(parse(text = paste0("c(", args[1], ")"))) else c(1, 2)
ns <- if (length(args) >= 2) eval(parse(text = paste0("c(", args[2], ")"))) else 2:4
pool_sel <- if (length(args) >= 3 && nzchar(args[3])) strsplit(args[3], ",")[[1]] else NULL
BAL <- length(args) >= 4 && args[4] == "balanceado"
IRR <- length(args) >= 5 && args[5] == "irr"

suma <- function(x, n, fact, sgn, pot) {   # numerador sobre D = (2n-1)!, exacto en enteros
  D <- factorial(2 * n - 1); k <- 0:(n - 1)
  sum(sgn(k) * pot(x, k) * D / fact(k))
}
alt <- function(k) (-1)^k
POOL <- list(
  A = list(pre = function(x, n) n >= 2, f = function(x, n) suma(x, n, function(k) factorial(2 * k), function(k) ifelse(k == 0, 1, -1), function(x, k) x^(2 * k + 1))),
  B = list(pre = function(x, n) n >= 2, f = function(x, n) suma(x, n, function(k) factorial(2 * k), function(k) 1, function(x, k) x^(2 * k + 1))),
  D = list(pre = function(x, n) n >= 2, f = function(x, n) suma(x, n, function(k) factorial(2 * k + 1), function(k) 1, function(x, k) x^(2 * k + 1))),
  E = list(pre = function(x, n) n >= 2, f = function(x, n) suma(x, n, function(k) 2 * k + 1, alt, function(x, k) x^(2 * k + 1))),
  F = list(pre = function(x, n) n >= 2, f = function(x, n) suma(x, n - 1, function(k) factorial(2 * k + 1), alt, function(x, k) x^(2 * k + 1)) * factorial(2 * n - 1) / factorial(2 * n - 3)),
  N = list(pre = function(x, n) n >= 3, f = function(x, n) suma(x, n, function(k) factorial(2 * k + 1), function(k) ifelse(k == n - 1, -alt(k), alt(k)), function(x, k) x^(2 * k + 1))),
  J = list(pre = function(x, n) abs(x) >= 2, f = function(x, n) suma(x, n, function(k) factorial(2 * k + 1), alt, function(x, k) (2 * k + 1) * x)),
  I = list(pre = function(x, n) n >= 3, f = function(x, n) suma(x, n, function(k) factorial(2 * k + 1), function(k) ifelse(k == 0, 1, -alt(k)), function(x, k) x^(2 * k + 1))),
  G = list(pre = function(x, n) n >= 3, f = function(x, n) suma(x, n, function(k) ifelse(k == 1, 2, factorial(2 * k + 1)), alt, function(x, k) x^(2 * k + 1))),
  H = list(pre = function(x, n) n >= 3, f = function(x, n) suma(x, n, function(k) factorial(2 * k), alt, function(x, k) x^(2 * k + 1))),
  O = list(pre = function(x, n) n >= 3, f = function(x, n) suma(x, n, function(k) (2 * k + 1) * (k + 1), alt, function(x, k) x^(2 * k + 1)))
)
if (!is.null(pool_sel)) POOL <- POOL[pool_sel]
clave <- function(x, n) suma(x, n, function(k) factorial(2 * k + 1), alt, function(x, k) x^(2 * k + 1))

## Reglas: reciben num (numeradores), D, x. Devuelven lógico (sobrevive).
gcdv <- function(a, b) { a <- abs(a); b <- abs(b); while (b > 0) { t <- b; b <- a %% b; a <- t }; a }
REGLAS <- list(
  mag_mayor      = function(v, D, x) v == max(v),
  mag_menor      = function(v, D, x) v == min(v),
  mag_2a_menor   = function(v, D, x) rank(v) == 2,
  mag_2a_mayor   = function(v, D, x) rank(v) == 3,
  pos_centrales  = function(v, D, x) rank(v) %in% c(2, 3),
  mag_mas_cifras = function(v, D, x) nchar(abs(v)) == max(nchar(abs(v))),
  mag_menos_cifr = function(v, D, x) nchar(abs(v)) == min(nchar(abs(v))),
  div_irreducible= function(v, D, x) vapply(v, gcdv, 0, b = D) == 1,
  div_reducible  = function(v, D, x) vapply(v, gcdv, 0, b = D) > 1,
  div_impar      = function(v, D, x) abs(v) %% 2 == 1,
  div_par        = function(v, D, x) abs(v) %% 2 == 0,
  div_mult5      = function(v, D, x) abs(v) %% 5 == 0,
  div_no_mult5   = function(v, D, x) abs(v) %% 5 != 0,
  div_mult3      = function(v, D, x) abs(v) %% 3 == 0,
  div_no_mult3   = function(v, D, x) abs(v) %% 3 != 0,
  div_term1      = function(v, D, x) abs(v) %% 10 == 1,
  sig_positiva   = function(v, D, x) v > 0,
  sig_negativa   = function(v, D, x) v < 0,
  fmt_cifras_sing= function(v, D, x) { nc <- nchar(abs(v)); tb <- table(nc); nc %in% as.integer(names(tb)[tb == 1]) },
  rel_par_cercano= function(v, D, x) { d <- abs(outer(v, v, "-")); diag(d) <- Inf; m <- min(d); apply(d == m, 1, any) },
  rel_lejana     = function(v, D, x) { d <- abs(outer(v, v, "-")); diag(d) <- NA; s <- rowSums(d, na.rm = TRUE); s == max(s) },
  rel_media      = function(v, D, x) abs(v - mean(v)) == min(abs(v - mean(v))),
  est_rango_seno = function(v, D, x) abs(v) <= D,
  est_fuera_rango= function(v, D, x) abs(v) > D,
  est_cerca_sen  = function(v, D, x) abs(v / D - sin(x)) == min(abs(v / D - sin(x))),
  est_cerca_x    = function(v, D, x) abs(v / D - x) == min(abs(v / D - x)),
  est_signo_x    = function(v, D, x) sign(v) == sign(x)
)
score <- function(s, key = 1L) { if (sum(s) == 0 || all(s)) return(0.25); if (s[key]) 1 / sum(s) else 0 }

filas <- list(); resumen <- list()
for (x in xs) for (n in ns) {
  D <- factorial(2 * n - 1); kv <- clave(x, n)
  ap <- names(POOL)[vapply(POOL, function(e) isTRUE(e$pre(x, n)), TRUE)]
  vals <- vapply(POOL[ap], function(e) e$f(x, n), 0)
  ok <- abs(vals - kv) > 1e-9
  dup <- duplicated(round(vals, 6)) | duplicated(round(vals, 6), fromLast = TRUE)
  cat(sprintf("\n(x=%g, n=%d) D=%d clave=%g (%.4f; sen=%.4f; gcd=%d)  aplicables: %s\n", x, n, D, kv, kv / D, sin(x), gcdv(kv, D),
              paste(sprintf("%s=%g%s%s", ap, vals, ifelse(abs(vals) <= D, "[in]", ""), ifelse(!ok, "[=CLAVE]", ifelse(dup, "[DUP]", ""))), collapse = " ")))
  ap <- ap[ok]; vals <- vals[ok]
  if (length(ap) < 3) { cat("  ** menos de 3 distractores válidos\n"); next }
  ternas <- combn(seq_along(ap), 3, simplify = FALSE)
  abajo <- which(vals < kv); arriba <- which(vals > kv)
  bs <- 0:3; bs <- bs[bs <= length(abajo) & (3 - bs) <= length(arriba)]
  irr_d <- vapply(vals, gcdv, 0, b = D) == 1
  ## cascada: para cada b, si existe alguna terna con >= 1 distractor irreducible, solo esas
  elig <- function(t) TRUE
  if (IRR) {
    tiene <- vapply(ternas, function(t) any(irr_d[t]), TRUE); bt <- vapply(ternas, function(t) sum(vals[t] < kv), 0)
    elig_b <- tapply(tiene, bt, any)
    elig <- function(t) { b <- sum(vals[t] < kv); !isTRUE(elig_b[as.character(b)]) || any(irr_d[t]) }
  }
  n_elig <- function(b) sum(vapply(ternas, function(t) sum(vals[t] < kv) == b && elig(t), TRUE))
  for (t in ternas) {
    v <- c(kv, vals[t])
    b <- sum(vals[t] < kv)
    if (!elig(t)) next
    peso <- if (BAL) { if (!(b %in% bs)) 0 else (1 / length(bs)) / n_elig(b) } else 1 / length(ternas)
    if (anyDuplicated(round(v, 6))) next
    sc <- vapply(REGLAS, function(r) score(r(v, D, x)), 0)
    if (peso > 0) filas[[length(filas) + 1]] <- c(list(x = x, n = n, terna = paste(ap[t], collapse = "+"), rango = rank(v)[1], peso = peso), as.list(sc))
  }
}
T <- do.call(rbind, lapply(filas, as.data.frame))
T$rama <- paste0("x=", T$x, ",n=", T$n)
w <- T$peso / sum(T$peso)
cat("\n== Ternas válidas por rama ==\n"); print(table(T$rama))
cat("\n== Rango de la clave (1 = menor) por rama, % ==\n"); print(round(100 * prop.table(xtabs(peso ~ rama + rango, T), 1), 1))
cat("\n== Frecuencia de cada error (agregado, %) ==\n"); fe <- sapply(names(POOL), function(e) sum(w[grepl(paste0("(^|\\+)", e, "(\\+|$)"), T$terna)])); print(round(100 * fe, 1))
cat("\n== Tasa por regla (pesos uniformes por rama), agregado y peor rama, en pp sobre 25 % ==\n")
reg <- names(REGLAS)
ag <- vapply(reg, function(r) sum(w * T[[r]]), 0)
por_rama <- sapply(reg, function(r) tapply(T$peso * T[[r]], T$rama, sum) / tapply(T$peso, T$rama, sum))
peor <- apply(por_rama, 2, max); cual <- apply(por_rama, 2, function(z) rownames(por_rama)[which.max(z)])
out <- data.frame(regla = reg, agregado_pp = round(100 * (ag - 0.25), 1), peor_rama_pp = round(100 * (peor - 0.25), 1), peor_rama = cual)
print(out[order(-out$agregado_pp), ], row.names = FALSE)
