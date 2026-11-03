## Paso 9 (tanda S2d): versiones únicas sobre 300 semillas (umbral de producto 250/300, regla #3 de codigo-rmd),
## con el método de la r4b («opciones ordenadas»: el conjunto de opciones, sin el orden de la mezcla). Solo
## data_generation, en memoria; no escribe nada ni toca el .Rmd. Semillas 1 a 300, declaradas.
suppressMessages(library(exams))
rmd <- commandArgs(trailingOnly = TRUE)[1]
l <- readLines(rmd, encoding = "UTF-8")
i0 <- grep("^```\\{r data_generation", l)[1]; i1 <- i0 + which(grepl("^```\\s*$", l[(i0 + 1):length(l)]))[1]
stopifnot(!is.na(i0), !is.na(i1))
EXPR <- parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8")
cat("md5", unname(tools::md5sum(rmd)), "\n")
k_op <- k_full <- character(0); n_can <- 0L; err <- 0L
for (s in 1:300) {
  set.seed(s); e <- new.env(parent = globalenv())
  r <- tryCatch({ eval(EXPR, e); NULL }, error = function(z) conditionMessage(z))
  if (!is.null(r)) { err <- err + 1L; next }
  op <- sort(e$opciones); n_can <- n_can + isTRUE(e$es_canonica)
  k_op <- c(k_op, paste(op, collapse = " | "))
  k_full <- c(k_full, paste(c(e$enunciado, e$tabla_md, op), collapse = " | "))
}
cat(sprintf("semillas 1 a 300: %d con error, %d canónicas\n", err, n_can))
cat(sprintf("versiones unicas (opciones ordenadas): %d / 300\n", length(unique(k_op))))
cat(sprintf("versiones unicas (enunciado + tabla + opciones ordenadas): %d / 300\n", length(unique(k_full))))
