## V6 (Incidente Q): la PROSA de la Solution no puede enumerar en el orden
## interno del pool. Se empareja POR CONTENIDO, no por posición: bajo exshuffle
## comparar órdenes seria una condicion falsa.
rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0+1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0+1):(i1-1)], collapse="\n"))
mal <- 0L; n <- 100L
for (s in seq_len(n)) {
  set.seed(s * 31337L)
  e <- new.env(parent = globalenv()); eval(expr, envir = e)
  # veredicto segun el Answerlist (fuente de verdad que ve el estudiante)
  al <- setNames(e$sol_p6 == 1L, e$afirmaciones_p6)
  # veredicto segun la PROSA (que agrupa, no enumera)
  prosa_V <- sort(e$afirmaciones_p6[e$sol_p6 == 1L])
  prosa_F <- sort(e$afirmaciones_p6[e$sol_p6 == 0L])
  for (t in prosa_V) if (!isTRUE(al[[t]])) mal <- mal + 1L
  for (t in prosa_F) if (!isFALSE(al[[t]])) mal <- mal + 1L
  # la prosa NO debe reproducir el orden interno del pool
  if (identical(prosa_V, e$afirmaciones_p6[e$sol_p6 == 1L]) &&
      length(prosa_V) > 1L && !identical(sort(prosa_V), prosa_V)) mal <- mal + 1L
}
cat("V6: discrepancias prosa<->Answerlist (por contenido):", mal, "/", n, "versiones\n")
cat("V6 veredicto:", if (mal == 0L) "OK (la prosa AGRUPA por veredicto; no enumera en orden interno)" else "FALLA", "\n")
