## Canal PROPIO del formato CLOZE, que el arsenal compartido NO puede ver:
## las Partes 2 y 3 entregan `vel` y `cota` ANTES de la Parte 5. ¿Basta con
## "elige la opcion que cita esos dos numeros" para resolver la P5?
## Convencion: score = 1/|S| si la clave sobrevive, 0 si no, 1/n si S vacio
## (nulo exacto 1/n para toda regla, regla #22 v1.6). Abstencion en empates.
rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0+1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0+1):(i1-1)], collapse="\n"))

N <- 100L
sc_cifras <- numeric(N); sc_p1 <- numeric(N); n_surv <- integer(N)
for (s in seq_len(N)) {
  set.seed(s * 5011L)
  e <- new.env(parent = globalenv()); eval(expr, envir = e)
  o <- e$opciones_p5; k <- which(e$sol_p5 == 1L)
  nums <- lapply(o, function(t) as.numeric(regmatches(t, gregexpr("[0-9]+", t))[[1]]))
  # (1) "cita a la vez vel y cota" -- los dos numeros de P2 y P3
  S <- which(vapply(nums, function(v) e$vel %in% v && e$cota %in% v, TRUE))
  n_surv[s] <- length(S)
  sc_cifras[s] <- if (!length(S)) 1/4 else if (k %in% S) 1/length(S) else 0
  # (2) el andamiaje COMPLETO: cita vel y cota Y ademas dice "menos de" (P1 dio el techo)
  S2 <- which(vapply(seq_along(o), function(i)
        e$vel %in% nums[[i]] && e$cota %in% nums[[i]] && grepl("menos de", o[i]), TRUE))
  sc_p1[s] <- if (!length(S2)) 1/4 else if (k %in% S2) 1/length(S2) else 0
}
cat("=== CANAL PROPIO DEL CLOZE en la Parte 5 (n =", N, ") ===\n")
cat("nulo exacto de cualquier regla con 4 opciones: 25.0 %\n\n")
cat(sprintf("(1) 'cita a la vez vel y cota'          : %.1f %%  (exceso %+.1f pp)\n",
            100*mean(sc_cifras), 100*mean(sc_cifras)-25))
cat(sprintf("    tamano mediano del grupo superviviente: %d opciones\n", median(n_surv)))
cat(sprintf("(2) '...y ademas dice \"menos de\"' (P1+P2+P3): %.1f %%  (exceso %+.1f pp)\n",
            100*mean(sc_p1), 100*mean(sc_p1)-25))
cat("\nLectura: (2) es el atajo que el andamiaje habilita de verdad. Un estudiante\n")
cat("que resolvio P1 (techo), P2 (vel) y P3 (cota) llega a la P5 con esa regla.\n")
