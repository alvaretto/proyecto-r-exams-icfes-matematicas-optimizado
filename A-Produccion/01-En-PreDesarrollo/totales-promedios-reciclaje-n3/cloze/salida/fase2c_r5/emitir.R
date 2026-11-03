## FASE 2C r5 — emite el texto real (xexams + read_exercise, exams 2.4.2) del CLOZE nuevo (1..205) y del viejo (1..100)
suppressMessages(library(exams))
rmd <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"
old <- path.expand("~/.cache/fase2c_r5_cloze/old/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
stopifnot(unname(tools::md5sum(rmd)) == "6f6f1b1311b0d681bd974c719ecd75d5", unname(tools::md5sum(old)) == "27f789dc6327f7d6bda72cded61502cd")
out <- path.expand("~/.cache/fase2c_r5_cloze"); td <- file.path(out, "tdir"); dir.create(td, showWarnings = FALSE)
emite <- function(f, seeds) {
  res <- list()
  for (s in seeds) {
    t1 <- file.path(td, "t"); s1 <- file.path(td, "s"); unlink(c(t1, s1), recursive = TRUE); dir.create(t1); dir.create(s1)
    x <- tryCatch(xexams(f, n = 1L, driver = list(sweave = list(quiet = TRUE, encoding = "UTF-8")),
                         dir = td, tdir = t1, sdir = s1, seed = matrix(s, 1, 1)), error = function(e) e)
    res[[as.character(s)]] <- if (inherits(x, "error")) list(err = conditionMessage(x)) else
      x[[1]][[1]][c("question", "questionlist", "solution", "solutionlist", "metainfo")]
  }
  res
}
t0 <- Sys.time(); N <- emite(rmd, 1:205); V <- emite(old, 1:100)
cat("tiempo", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s; errores nuevo:", sum(vapply(N, function(r) !is.null(r$err), NA)),
    "viejo:", sum(vapply(V, function(r) !is.null(r$err), NA)), "\n")
if (!is.null(N[[1]]$err)) cat("ERR:", N[[1]]$err, "\n")
saveRDS(list(N = N, V = V), file.path(out, "emit.rds"))
e <- N[["1"]]
cat("==== QUESTION s=1\n"); cat(e$question, sep = "\n")
cat("==== QUESTIONLIST s=1\n"); print(e$questionlist)
cat("==== SOLUTIONLIST s=1\n"); print(e$solutionlist)
cat("==== META s=1\n"); str(e$metainfo[c("type", "clozetype", "solution", "string", "points", "shuffle", "tolerance")])
cat("==== SOLUTION s=1\n"); cat(e$solution, sep = "\n")
