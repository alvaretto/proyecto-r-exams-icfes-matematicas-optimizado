## El gap 6 es `mchoice`: la bateria de eliminacion no aplica igual (hay varias
## correctas). El riesgo propio es que el NUMERO de verdaderas sea predecible.
rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0+1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0+1):(i1-1)], collapse="\n"))
N <- 100L; nv <- integer(N); dup <- 0L; frec <- list()
for (s in seq_len(N)) {
  set.seed(s * 8221L)
  e <- new.env(parent = globalenv()); eval(expr, envir = e)
  nv[s] <- sum(e$sol_p6)
  if (length(unique(e$afirmaciones_p6)) != 6L) dup <- dup + 1L
  for (j in seq_along(e$afirmaciones_p6))
    frec[[e$afirmaciones_p6[j]]] <- c(frec[[e$afirmaciones_p6[j]]], e$sol_p6[j])
}
cat("=== GAP 6 (mchoice) ===\n")
cat("distribucion del numero de VERDADERAS por version:\n"); print(table(nv))
cat("\ncolapso del pool (opciones duplicadas):", dup, "/", N, "\n")
cat("degeneracion a todas-V o todas-F:", sum(nv == 0L | nv == 6L), "/", N, "\n")
# una afirmacion NUNCA puede cambiar de veredicto entre versiones
incoh <- sum(vapply(frec, function(v) length(unique(v)) > 1L, TRUE))
cat("afirmaciones con veredicto INCONSISTENTE entre versiones:", incoh,
    "de", length(frec), "distintas\n")
cat("\nveredicto:", if (dup == 0L && incoh == 0L && all(nv >= 1L & nv <= 5L)) "OK" else "REVISAR", "\n")
