f <- "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
src <- readLines(f, warn = FALSE)
i0 <- grep("^```[{]r data_generation", src)[1]
i1 <- grep("^```$", src); i1 <- i1[i1 > i0][1]
ex <- parse(text = paste(src[(i0 + 1):(i1 - 1)], collapse = "\n"))
vis <- character(0)
for (s in 1:120) {
  set.seed(1000L + 7L * s); e <- new.env(parent = globalenv())
  if (inherits(try(eval(ex, envir = e), silent = TRUE), "try-error")) next
  vis <- c(vis, paste0(e$art_un, " ", e$ctx$lote, " ", e$adj_formado,
                       "  /  ", e$art_El, " ", e$ctx$lote, " ", e$adj_nuevo))
}
cat("combinaciones distintas vistas:\n"); print(sort(unique(vis)))
