## Conteo de versiones UNICAS del producto (N = 100, estandar del profesor).
## Huella = enunciado + las 4 opciones de p1 + las 4 de p4 + las 6 de p5 + los 3 num.
f <- "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
src <- readLines(f, warn = FALSE)
i0 <- grep("^```[{]r data_generation", src)[1]
i1 <- grep("^```$", src); i1 <- i1[i1 > i0][1]
ex <- parse(text = paste(src[(i0 + 1):(i1 - 1)], collapse = "\n"))
h <- character(0)
for (s in seq_len(100L)) {
  set.seed(50000L + 13L * s); e <- new.env(parent = globalenv())
  if (inherits(try(eval(ex, envir = e), silent = TRUE), "try-error")) next
  h <- c(h, paste(e$enunciado, paste(e$opciones_p1, collapse = "|"),
                  paste(e$opciones_p4, collapse = "|"),
                  paste(e$afirmaciones_p5, collapse = "|"),
                  e$resp_p2, e$resp_p3, e$resp_p6, collapse = "|"))
}
cat("versiones evaluadas:", length(h), "  UNICAS:", length(unique(h)), "\n")
## huella SOLO de la sustancia (las 6 claves), que es lo que mide la regla #22
k <- character(0)
for (s in seq_len(100L)) {
  set.seed(50000L + 13L * s); e <- new.env(parent = globalenv())
  if (inherits(try(eval(ex, envir = e), silent = TRUE), "try-error")) next
  k <- c(k, paste(e$opciones_p1[which(e$sol_p1 == 1L)],
                  e$opciones_p4[which(e$sol_p4 == 1L)],
                  paste(sort(e$afirmaciones_p5[e$sol_p5 == 1L]), collapse = "~"),
                  e$resp_p2, e$resp_p3, e$resp_p6, collapse = "|"))
}
cat("combinaciones de CLAVES distintas:", length(unique(k)), "de", length(k), "\n")
