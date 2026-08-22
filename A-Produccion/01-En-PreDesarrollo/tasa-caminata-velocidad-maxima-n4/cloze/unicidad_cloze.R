## Regla #3: 250+ versiones únicas sobre 300 intentos. NO es la muestra de
## medición (N=100, regla #23): es el umbral de PRODUCTO, otra cosa.
rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0+1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0+1):(i1-1)], collapse="\n"))
firmas <- character(300)
for (s in seq_len(300L)) {
  set.seed(s * 104729L)                      # serie independiente
  e <- new.env(parent = globalenv()); eval(expr, envir = e)
  firmas[s] <- paste(e$enunciado, paste(e$opciones_p1, collapse="|"),
                     e$resp_p2, e$resp_p3, paste(e$opciones_p4, collapse="|"),
                     paste(e$opciones_p5, collapse="|"),
                     paste(e$afirmaciones_p6, collapse="|"), collapse="~")
}
cat("versiones unicas:", length(unique(firmas)), "/ 300  (umbral 250)\n")
cat("veredicto:", if (length(unique(firmas)) >= 250L) "OK" else "INSUFICIENTE", "\n")
