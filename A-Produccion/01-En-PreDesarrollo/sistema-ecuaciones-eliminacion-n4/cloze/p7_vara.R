## §P7 con la VARA CONGELADA (bateria_referencia_icfes.R, B2 = 34 reglas).
## Veredicto por EXCESO sobre el techo nulo, nunca por tasa absoluta (§P7-A).
R <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts"
source(file.path(R, "bateria_eliminacion.R"))
source(file.path(R, "bateria_referencia_icfes.R"))
stopifnot(exists("B2"), length(B2) >= 28L)
cat("Reglas de la vara congelada:", length(B2), "\n\n")
filas <- readRDS("smoke_filas.rds")
medir <- function(nom, getop, getsol) {
  ops <- lapply(filas, getop); sls <- vapply(filas, function(x) which(getsol(x) == 1L)[1], 1L)
  keep <- !is.na(sls) & vapply(ops, function(o) length(o) == 4L, TRUE)
  ops <- ops[keep]; sls <- sls[keep]
  res <- evaluar_bateria(B2, ops, sls,
                         familias_no_aplicables = c(posicion = "el orden lo fija sample() por version"))
  cat("---------- gap ", nom, "  (n = ", length(ops), ") ----------\n", sep = "")
  imprimir_bateria(res)
  cat("\n")
  invisible(res)
}
r1 <- medir("p1 (item oficial)", function(x) x$op1, function(x) x$sol1)
r4 <- medir("p4 (propiedad)",    function(x) x$op4, function(x) x$sol4)
