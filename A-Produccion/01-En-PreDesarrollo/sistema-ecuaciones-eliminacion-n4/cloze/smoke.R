## smoke: evalua data_generation en N=100 versiones y verifica invariantes por ejecucion
suppressMessages(library(exams))
f <- "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_cloze_v1.Rmd"
src <- readLines(f, warn = FALSE)
i0 <- grep("^```\\{r data_generation", src)[1]
i1 <- grep("^```$", src); i1 <- i1[i1 > i0][1]
code <- paste(src[(i0 + 1):(i1 - 1)], collapse = "\n")
ex <- parse(text = code)
N <- 100L; err <- character(0); filas <- list()
for (s in seq_len(N)) {
  set.seed(1000L + 7L * s)
  e <- new.env(parent = globalenv())
  ok <- tryCatch({ eval(ex, envir = e); TRUE },
                 error = function(z) { err <<- c(err, paste0("s", s, ": ", conditionMessage(z))); FALSE })
  if (!ok) next
  filas[[length(filas) + 1L]] <- list(
    s = s, canon = e$is_canonical, paso = e$err_real$paso, cod = e$err_real$codigo,
    op1 = e$opciones_p1, sol1 = e$sol_p1,
    op4 = e$opciones_p4, sol4 = e$sol_p4, cod4 = vapply(e$props_p4, function(p) p$cod, character(1)),
    af5 = e$afirmaciones_p5, sol5 = e$sol_p5,
    r = e$resp_p2, l = e$resp_p3, p6 = e$resp_p6, m1 = e$m1, m2 = e$m2,
    P1 = e$par$P1, P2 = e$par$P2, a = e$par$a, b = e$par$b, cc = e$par$c, d = e$par$d,
    excl = e$exclozetype_str, exsol = e$exsolution_str, extol = e$extol_str,
    vm = e$valor_mostrado)
}
cat("VERSIONES OK:", length(filas), "/", N, "\n")
if (length(err)) { cat("ERRORES:\n"); print(head(err, 5)) }
bad <- 0L
for (x in filas) {
  ## CORRECCION (binario): las 6 claves deben ser verdaderas por ejecucion
  if (x$a * x$l + x$b * x$r != x$P1) { bad <- bad + 1L; cat("FALLA E1 s", x$s, "\n") }
  if (x$cc * x$l + x$d * x$r != x$P2) { bad <- bad + 1L; cat("FALLA E2 s", x$s, "\n") }
  if (x$p6 != x$m1 * x$l + x$m2 * x$r) { bad <- bad + 1L; cat("FALLA P6 s", x$s, "\n") }
  if (sum(x$sol1) != 1L || sum(x$sol4) != 1L) { bad <- bad + 1L; cat("FALLA sol s", x$s, "\n") }
  if (sum(x$sol5) != 3L) { bad <- bad + 1L; cat("FALLA sol5 s", x$s, "\n") }
  if (length(unique(x$op1)) != 4L || length(unique(x$op4)) != 4L ||
      length(unique(x$af5)) != 6L) { bad <- bad + 1L; cat("FALLA unicidad s", x$s, "\n") }
  if (x$vm == x$r) { bad <- bad + 1L; cat("FALLA estimulo-sin-error s", x$s, "\n") }
  parts <- strsplit(x$exsol, "|", fixed = TRUE)[[1]]
  if (length(parts) != 6L) { bad <- bad + 1L; cat("FALLA exsolution s", x$s, "\n") }
  if (nchar(parts[1]) != 4L || nchar(parts[4]) != 4L || nchar(parts[5]) != 6L) {
    bad <- bad + 1L; cat("FALLA long exsolution s", x$s, "\n") }
  if (as.integer(parts[2]) != x$r || as.integer(parts[3]) != x$l ||
      as.integer(parts[6]) != x$p6) { bad <- bad + 1L; cat("FALLA num-gap s", x$s, "\n") }
}
cat("DEFECTOS DE CORRECCION:", bad, "\n")
cat("canonicas:", sum(vapply(filas, function(x) isTRUE(x$canon), logical(1))), "\n")
cat("reparto paso_real:", paste(names(table(vapply(filas, function(x) x$paso, integer(1)))),
    table(vapply(filas, function(x) x$paso, integer(1))), collapse = " | "), "\n")
cat("claves P4 distintas:", length(unique(vapply(filas, function(x) x$cod4[which(x$sol4 == 1L)], character(1)))), "\n")
print(table(vapply(filas, function(x) x$cod4[which(x$sol4 == 1L)], character(1))))
cat("valores unicos R:", length(unique(vapply(filas, function(x) x$r, numeric(1)))),
    " L:", length(unique(vapply(filas, function(x) x$l, numeric(1)))),
    " P6:", length(unique(vapply(filas, function(x) x$p6, numeric(1)))), "\n")
saveRDS(filas, "smoke_filas.rds")
