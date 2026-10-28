## Réplica independiente del canal de estímulo (H1 del detractor r4).
## Regla: descartar las opciones cuyos dos tipos comparten algún valor de celda; elegir al azar entre las demás.
rmd <- commandArgs(TRUE)[1]
lin <- readLines(rmd, encoding = "UTF-8")
i <- grep("^```\\{r data_generation", lin)[1]; f <- which(lin == "```" & seq_along(lin) > i)[1]
ex <- parse(text = paste(lin[(i + 1):(f - 1)], collapse = "\n"))
res <- list(); clave_coinc <- 0L; canon_acc <- NULL
for (s in 1:3000) {
  set.seed(s); e <- new.env(); eval(ex, e)
  coinc <- vapply(e$pares, function(par) {
    v1 <- as.vector(e$celdas_tipo(e$w, par[[1]])); v2 <- as.vector(e$celdas_tipo(e$w, par[[2]]))
    any(v1 %in% v2) }, NA)
  if (coinc[["clave"]]) clave_coinc <- clave_coinc + 1L
  quedan <- sum(!coinc); acc <- if (coinc[["clave"]]) 0 else 1 / quedan
  if (isTRUE(e$es_canonica)) { canon_acc <- c(canon_acc, acc); next }
  res[[length(res) + 1]] <- data.frame(estrato = paste(e$err_PQ, collapse = ">"), acc = acc,
                                       espejo = coinc[["PROB-SIM"]], quedan = quedan)
}
d <- do.call(rbind, res)
cat("no canónicas:", nrow(d), "| canónicas:", length(canon_acc), "| clave con coincidencia:", clave_coinc, "\n\n")
print(aggregate(cbind(acierto = 100 * acc, espejo_coinc = 100 * espejo, quedan) ~ estrato, data = d, FUN = function(x) round(mean(x), 1)))
nc <- mean(d$acc); cn <- mean(canon_acc)
cat(sprintf("\nno canónica: %.1f %% (%+.1f pp) | canónica: %.1f %% (%+.1f pp) | mezcla 10 %%: %.1f %% (%+.1f pp)\n",
            100 * nc, 100 * (nc - .25), 100 * cn, 100 * (cn - .25), 100 * (.9 * nc + .1 * cn), 100 * (.9 * nc + .1 * cn - .25)))
## Techo nulo: misma regla con la clave permutada al azar dentro de cada versión
set.seed(1); perm <- mean(vapply(seq_len(nrow(d)), function(k) 0.25, 0)); cat("techo nulo (clave permutada, por linealidad): 25 %\n")
