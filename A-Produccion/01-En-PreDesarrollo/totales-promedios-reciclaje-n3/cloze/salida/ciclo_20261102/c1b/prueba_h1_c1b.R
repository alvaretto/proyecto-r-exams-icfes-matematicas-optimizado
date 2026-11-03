## prueba_h1_c1b.R — prueba rápida de la sonda H1-Sol y de MC50, MC51, MC53 y MC54, con los mismos trozos del verificador
## (cabecera, bloque de L-6 y C1b, y los mutantes de H1-Sol), sin correr E1 a E8. Uso, desde cloze/:
##   Rscript --no-init-file -e 'source("salida/ciclo_20261102/c1b/prueba_h1_c1b.R")'
L <- readLines("verificar_espacio_cloze.R", encoding = "UTF-8")
a1 <- grep("^## ---- Registro de una versión", L) - 1L
b0 <- grep("^## L-6 en la prosa propia de la Parte 1 de la Solution del CLOZE", L); b1 <- grep("^## ---- E5 ", L) - 1L
c0 <- grep("^## MC50, MC51, MC53 y MC54", L); c1 <- grep("^cat\\(\"  Decisión 2 a", L) - 1L
stopifnot(length(a1) == 1, length(b0) == 1, length(b1) == 1, length(c0) == 1, length(c1) == 1)
filas_mut <- list()
for (r in list(1:a1, b0:b1, c0:c1)) eval(parse(text = L[r], encoding = "UTF-8"), envir = globalenv())
TM <- do.call(rbind, filas_mut); print(TM[, c(1, 4, 3)], quote = FALSE)
cat("fallos:", length(fallos), "\n")
