## §P7-B: una frecuencia SIN margen no es un defecto. El umbral con el que este
## repositorio calibro H1 es 15 % de margen relativo sobre el rival mas proximo.
rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0+1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0+1):(i1-1)], collapse="\n"))
N <- 400L
res <- list(p4=list(T=c(),F=c()), p5=list(T=c(),F=c()))
for (s in seq_len(N)) {
  set.seed(s * 6421L)
  e <- new.env(parent = globalenv()); eval(expr, envir = e)
  for (nm in c("p4","p5")) {
    o <- if (nm=="p4") e$opciones_p4 else e$opciones_p5
    k <- which((if (nm=="p4") e$sol_p4 else e$sol_p5) == 1L)
    L <- nchar(o); r <- if (isTRUE(e$excede)) "T" else "F"
    # margen relativo de la clave frente al rival mas proximo en la direccion explotada
    if (r == "T") { # se explota "la mas corta"
      otros <- sort(L[-k]); m <- (otros[1] - L[k]) / otros[1]
    } else {        # se explota "la mas larga"
      otros <- sort(L[-k], decreasing = TRUE); m <- (L[k] - otros[1]) / L[k]
    }
    res[[nm]][[r]] <- c(res[[nm]][[r]], m)
  }
}
cat("=== MARGEN RELATIVO de la clave en la direccion explotada (n=400) ===\n")
cat("umbral de explotabilidad (H1 de este repo): 15 %\n\n")
for (nm in c("p4","p5")) for (r in c("T","F")) {
  v <- res[[nm]][[r]]; et <- if (r=="T") "supera (mas corta)" else "cabe   (mas larga)"
  cat(sprintf("%s rama %-20s n=%3d | margen mediano %+6.1f %% | p90 %+6.1f %% | %% de versiones con margen >= 15 %%: %5.1f %%\n",
      nm, et, length(v), 100*median(v), 100*quantile(v,.9), 100*mean(v >= .15)))
}
