## OBJ 1 del detractor: en la rama paso 3 el estimulo imprime ci/coef_final pero
## el procedimiento divide entre par$a. Verificacion INDEPENDIENTE por ejecucion.
f <- "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
src <- readLines(f, warn = FALSE)
i0 <- grep("^```[{]r data_generation", src)[1]; i1 <- grep("^```$", src); i1 <- i1[i1 > i0][1]
ex <- parse(text = paste(src[(i0+1):(i1-1)], collapse = "\n"))
n3 <- 0L; falsas <- 0L; ej <- character(0); p4_falsa <- 0L
for (s in 1:100) {
  set.seed(1000L + 7L*s); e <- new.env(parent = globalenv())
  if (inherits(try(eval(ex, envir=e), silent=TRUE), "try-error")) next
  if (e$err_real$paso != 3L) next
  n3 <- n3 + 1L
  ## lo que el estimulo IMPRIME como denominador
  den_impreso <- if (e$e2s$cR == 0) e$e2s$cL else e$e2s$cR
  cociente_impreso <- e$e2s$ci / den_impreso
  ## el valor que el estimulo declara
  if (cociente_impreso != e$valor_mostrado) {
    falsas <- falsas + 1L
    if (length(ej) < 3L) ej <- c(ej, sprintf("  %s = %s / %s -> imprime %s, pero da %s (cod=%s)",
        e$inc_final, format(e$e2s$ci, big.mark="."), den_impreso,
        format(e$valor_mostrado, big.mark="."), format(cociente_impreso, big.mark="."),
        e$err_real$codigo))
    ## OBJ 1 punto 4: la clave de P4 dice que NO respeta "dividir entre el que queda"
    if (identical(e$props_p4[[which(e$sol_p4==1L)]]$cod, "PROP-COEF")) p4_falsa <- p4_falsa + 1L
  }
}
cat("versiones de la rama paso 3:", n3, "\n")
cat("con division IMPRESA falsa :", falsas, "\n")
cat("de esas, con clave P4 = PROP-COEF (clave falsa):", p4_falsa, "\n")
cat("ejemplos:\n"); cat(paste(ej, collapse="\n"), "\n")
