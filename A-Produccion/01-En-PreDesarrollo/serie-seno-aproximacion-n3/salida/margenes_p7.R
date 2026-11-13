## Margen §P7-B de las reglas de distancia que pasan de +8 pp, sobre las 100 versiones reales de verificar_bateria_p7.R
## (semillas 1..100; sin canónica). Margen = (d2 - d1) / d2: ventaja relativa de la opción elegida sobre la segunda.
rmd <- list.files(".", pattern = "_schoice_v1\\.Rmd$")[1]
l <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", l); i1 <- which(l == "```")[which(l == "```") > i0][1]
code <- parse(text = l[(i0 + 1):(i1 - 1)], encoding = "UTF-8")
marg <- function(d) { s <- sort(d); if (s[2] == 0) 0 else (s[2] - s[1]) / s[2] }
R <- t(vapply(1:100, function(s) { set.seed(s); e <- new.env(); eval(code, e)
  v <- e$numeradores / e$D; k <- which(e$sol == 1L); x <- e$x
  dp <- abs(outer(v, v, "-")); diag(dp) <- Inf; pares <- dp[upper.tri(dp)]
  par_min <- which(dp == min(dp), arr.ind = TRUE)[1, ]
  dx <- abs(v - x); dS2 <- abs(v - (x - x^3 / 6))
  c(par_clave = as.numeric(k %in% par_min), m_par = marg(pares),
    x_clave = as.numeric(which.min(dx) == k), m_x = marg(dx),
    S2_clave = as.numeric(which.min(dS2) == k), m_S2 = marg(dS2)) }, numeric(6)))
f <- function(acierta, m) sprintf("acierta en %d/100; margen mediano cuando acierta %.0f %% (mín %.0f %%); versiones con margen < 15 %%: %d",
  sum(acierta), 100 * median(m[acierta == 1]), 100 * min(m[acierta == 1]), sum(acierta == 1 & m < 0.15))
cat("rel_par_mas_cercano (la clave está en el par más cercano):", f(R[, 1], R[, 2]), "\n")
cat("est_cerca_x (la clave es la más cercana a x):", f(R[, 3], R[, 4]), "\n")
cat("est_cerca_S2 (la clave es la más cercana a x - x^3/6):", f(R[, 5], R[, 6]), "\n")
