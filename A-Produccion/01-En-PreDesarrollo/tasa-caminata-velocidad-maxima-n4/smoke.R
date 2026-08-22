## Smoke: evalúa data_generation N=100 veces y comprueba invariantes propias.
rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_schoice_v1.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0 + 1):length(ln)]))[1]
code <- ln[(i0 + 1):(i1 - 1)]
expr <- parse(text = paste(code, collapse = "\n"))

N <- 100L; errs <- character(0); canon <- 0L; nex <- 0L
firmas <- character(0); largos <- numeric(0)
for (s in seq_len(N)) {
  set.seed(s * 7919L)
  e <- new.env(parent = globalenv())
  ok <- tryCatch({ eval(expr, envir = e); TRUE },
                 error = function(err) { errs <<- c(errs, paste0("s", s, ": ", conditionMessage(err))); FALSE })
  if (!ok) next
  if (isTRUE(e$is_canonical)) canon <- canon + 1L
  if (!isTRUE(e$excede)) nex <- nex + 1L
  ## I-1 corrección de la clave: la cota se deduce del criterio y el veredicto es el real
  if (e$vel * e$tasa != 60L) errs <- c(errs, paste0("s", s, ": I-1 vel"))
  if (e$cota != e$vel * e$H) errs <- c(errs, paste0("s", s, ": I-1 cota"))
  if (!identical(e$excede, e$reporte > e$cota)) errs <- c(errs, paste0("s", s, ": I-1 veredicto"))
  ## I-2 exsolution coherente
  if (sum(e$sol) != 1L || length(e$sol) != 4L) errs <- c(errs, paste0("s", s, ": I-2 sol"))
  k <- e$opciones[which(e$sol == 1L)]
  if (!identical(k, e$texto_clave)) errs <- c(errs, paste0("s", s, ": I-2 clave-marcada"))
  ## I-3 unicidad
  if (length(unique(e$opciones)) != 4L) errs <- c(errs, paste0("s", s, ": I-3 unicidad"))
  ## I-4 la clave menciona la cota correcta y el veredicto correcto
  if (!grepl(paste0("menos de ", e$vel, " kil"), k, fixed = FALSE)) errs <- c(errs, paste0("s", s, ": I-4 vel en clave"))
  if (!grepl(paste0("menos de ", format(e$cota, big.mark = ".", trim = TRUE), " kil"), k)) errs <- c(errs, paste0("s", s, ": I-4 cota en clave"))
  ## I-5 canónica verbatim
  if (isTRUE(e$is_canonical)) {
    if (e$tasa != 6L || e$H != 24L || e$reporte != 300L || e$vel != 10L || e$cota != 240L)
      errs <- c(errs, paste0("s", s, ": I-5 params canonicos"))
    ofic <- "Lo dicho implica que la velocidad máxima es menos de 10 kilómetros por hora y menos de 240 kilómetros al día; un recorrido mayor corresponde entonces a un medio motorizado."
    if (!identical(e$texto_clave, ofic)) errs <- c(errs, paste0("s", s, ": I-5 clave NO verbatim"))
    if (!grepl("En un conocido juego virtual", e$enunciado, fixed = TRUE))
      errs <- c(errs, paste0("s", s, ": I-5 enunciado"))
  }
  firmas <- c(firmas, tolower(gsub("[[:digit:][:punct:]]+", "", k)))
  largos <- c(largos, nchar(k) - max(nchar(e$opciones[e$sol == 0L])))
}
cat("=== SMOKE N =", N, "===\n")
cat("errores:", length(errs), "\n"); if (length(errs)) print(head(unique(errs), 15))
cat("canonicas:", canon, " | rama no_excede:", nex, "\n")
cat("firmas normalizadas distintas de la clave:", length(unique(firmas)), "\n")
tb <- sort(table(firmas), decreasing = TRUE)
cat("frecuencia de la firma mas comun:", round(100 * max(tb) / length(firmas), 1), "%\n")
cat("clave es la mas larga en:", round(100 * mean(largos > 0), 1), "% | delta mediano:", median(largos), "chars\n")
