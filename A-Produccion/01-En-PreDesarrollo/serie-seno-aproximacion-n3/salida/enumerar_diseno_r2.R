#!/usr/bin/env Rscript
## Pasada de corrección 2 (decisiones del profesor del 2026-11-13): enumeración EXACTA del espacio nuevo antes de tocar el .Rmd.
## x en {2, 3, -2, -3}, n en {3, 4}; trampa de calculadora SS-C obligatoria + 2 distractores del banco.
## Políticas: P1 rango balanceado · P2 = P1 + cascada «al menos un distractor más con |v| <= 1» · P3 = P1 + cascada «los otros
## dos se simplifican, como la clave» · P4 = P2 y P3 (primero ambas, luego P2, luego P3, luego P1).
args <- commandArgs(TRUE); pol_sel <- if (length(args)) args[1] else "todas"
suma <- function(x, n, nt, fact, sgn) { D <- factorial(2 * n - 1); if (nt < 1) return(0); k <- 0:(nt - 1); sum(sgn(k) * x^(2 * k + 1) * D / fact(k)) }
alt <- function(k) (-1)^k; fi <- function(k) factorial(2 * k + 1); fp <- function(k) factorial(2 * k)
BANCO <- list(
  A = function(x, n) suma(x, n, n, fp, function(k) ifelse(k == 0, 1, -1)),
  B = function(x, n) suma(x, n, n, fp, function(k) 1),
  D = function(x, n) suma(x, n, n, fi, function(k) 1),
  E = function(x, n) suma(x, n, n, function(k) 2 * k + 1, alt),
  F = function(x, n) suma(x, n, n - 1, fi, alt),
  G = function(x, n) suma(x, n, n, function(k) ifelse(k == 1, 2, factorial(2 * k + 1)), alt),
  H = function(x, n) suma(x, n, n, fp, alt),
  I = function(x, n) suma(x, n, n, fi, function(k) ifelse(k == 0, 1, -(-1)^k)),
  N = function(x, n) suma(x, n, n, fi, function(k) ifelse(k == n - 1, -(-1)^k, (-1)^k)),
  O = function(x, n) suma(x, n, n, function(k) (2 * k + 1) * (k + 1), alt))
gcdv <- function(a, b) { a <- abs(a); b <- abs(b); while (b > 0) { t <- b; b <- a %% b; a <- t }; a }
S2 <- function(x) x - x^3 / 6
REG <- list(
  mag_mayor = function(v, D, x) v == max(v), mag_menor = function(v, D, x) v == min(v),
  mag_2a_menor = function(v, D, x) rank(v) == 2, mag_2a_mayor = function(v, D, x) rank(v) == 3,
  mag_mas_cifras = function(v, D, x) nchar(abs(v)) == max(nchar(abs(v))), mag_menos_cifras = function(v, D, x) nchar(abs(v)) == min(nchar(abs(v))),
  pos_centrales = function(v, D, x) rank(v) %in% c(2, 3),
  div_irreducible = function(v, D, x) vapply(v, gcdv, 0, b = D) == 1, div_reducible = function(v, D, x) vapply(v, gcdv, 0, b = D) > 1,
  div_impar = function(v, D, x) abs(v) %% 2 == 1, div_par = function(v, D, x) abs(v) %% 2 == 0,
  div_mult5 = function(v, D, x) abs(v) %% 5 == 0, div_no_mult5 = function(v, D, x) abs(v) %% 5 != 0,
  div_mult3 = function(v, D, x) abs(v) %% 3 == 0, div_no_mult3 = function(v, D, x) abs(v) %% 3 != 0,
  div_termina_en_1 = function(v, D, x) abs(v) %% 10 == 1, div_termina_1o9 = function(v, D, x) abs(v) %% 10 %in% c(1, 9),
  sig_positiva = function(v, D, x) v > 0, sig_negativa = function(v, D, x) v < 0,
  fmt_cifras_singular = function(v, D, x) { nc <- nchar(abs(v)); tb <- table(nc); nc %in% as.integer(names(tb)[tb == 1]) },
  rel_par_mas_cercano = function(v, D, x) { d <- abs(outer(v, v, "-")); diag(d) <- Inf; apply(d == min(d), 1, any) },
  rel_mas_lejana = function(v, D, x) { d <- abs(outer(v, v, "-")); s <- rowSums(d); s == max(s) },
  rel_cerca_media = function(v, D, x) abs(v - mean(v)) == min(abs(v - mean(v))),
  est_rango_seno = function(v, D, x) abs(v) <= D, est_fuera_rango = function(v, D, x) abs(v) > D,
  est_cerca_sen = function(v, D, x) { d <- abs(v / D - sin(x)); d == min(d) },
  est_no_cerca_sen = function(v, D, x) { d <- abs(v / D - sin(x)); d != min(d) },
  est_cerca_x = function(v, D, x) { d <- abs(v / D - x); d == min(d) },
  est_cerca_S2 = function(v, D, x) { d <- abs(v / D - S2(x)); d == min(d) },
  est_entre_S2_y_x = function(v, D, x) v / D > min(S2(x), x) & v / D < max(S2(x), x),
  est_signo_x = function(v, D, x) sign(v) == sign(x))
score <- function(s) { if (sum(s) == 0 || all(s)) return(0.25); if (s[1]) 1 / sum(s) else 0 }   # la clave va primera
filas <- list(); resumen <- c()
for (x in c(2, 3, -2, -3)) for (n in 3:4) {
  D <- factorial(2 * n - 1); kv <- suma(x, n, n, fi, alt); tr <- round(sin(x) * D)
  v <- vapply(BANCO, function(f) f(x, n), 0); v <- v[abs(v - kv) > 0.5 & abs(v - tr) > 0.5]; v <- v[!duplicated(round(v))]
  resumen <- c(resumen, sprintf("x=%2d n=%d D=%4d clave=%6g (%.4f) red=%s | trampa=%5g (%.4f) red=%s | %s", x, n, D, kv, kv / D, gcdv(kv, D) > 1, tr, tr / D,
    gcdv(tr, D) > 1, paste(sprintf("%s=%g%s%s", names(v), v, ifelse(abs(v) <= D, "[in]", ""), ifelse(vapply(v, gcdv, 0, b = D) > 1, "", "[irr]")), collapse = " ")))
  stopifnot(abs(tr - kv) > 0.5)
  pares <- combn(names(v), 2, simplify = FALSE); tr_abajo <- tr < kv
  for (pol in c("P1", "P2", "P3", "P4")) {
    info <- lapply(pares, function(pp) { vv <- v[pp]; list(pp = pp, b = sum(vv < kv) + tr_abajo, rango = any(abs(vv) <= D), red = all(vapply(vv, gcdv, 0, b = D) > 1)) })
    bs <- sort(unique(vapply(info, `[[`, 0, "b")))
    for (b in bs) {
      cand <- Filter(function(i) i$b == b, info)
      niveles <- switch(pol, P1 = list(function(i) TRUE), P2 = list(function(i) i$rango, function(i) TRUE),
                        P3 = list(function(i) i$red, function(i) TRUE),
                        P4 = list(function(i) i$rango && i$red, function(i) i$rango, function(i) i$red, function(i) TRUE))
      for (f in niveles) { c2 <- Filter(f, cand); if (length(c2)) break }
      for (i in c2) {
        opc <- c(kv, tr, v[i$pp])
        sc <- vapply(REG, function(r) score(r(opc, D, x)), 0)
        filas[[length(filas) + 1]] <- c(list(pol = pol, rama = sprintf("x=%d,n=%d", x, n), terna = paste(c("C", i$pp), collapse = "+"),
          rango = rank(opc)[1], peso = (1 / 8) * (1 / length(bs)) * (1 / length(c2))), as.list(sc))
      }
    }
  }
}
cat(resumen, sep = "\n"); T <- do.call(rbind, lapply(filas, as.data.frame))
for (pol in unique(T$pol)) {
  t <- T[T$pol == pol, ]; w <- t$peso / sum(t$peso)
  cat("\n==================== ", pol, " ====================\n", sep = "")
  cat("ternas con peso > 0:", nrow(t), "| rango de la clave (%):", paste(round(100 * tapply(w, t$rango, sum), 1), collapse = " / "), "\n")
  ag <- vapply(names(REG), function(r) sum(w * t[[r]]), 0)
  pr <- sapply(names(REG), function(r) tapply(t$peso * t[[r]], t$rama, sum) / tapply(t$peso, t$rama, sum))
  out <- data.frame(regla = names(REG), agregado_pp = round(100 * (ag - 0.25), 1), peor_rama_pp = round(100 * (apply(pr, 2, max) - 0.25), 1),
                    peor_rama = apply(pr, 2, function(z) rownames(pr)[which.max(z)]))
  print(out[order(-out$agregado_pp), ][1:12, ], row.names = FALSE)
}
