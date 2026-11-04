## FASE 2C r6: emite el texto real del CLOZE e16842c1 y del SCHOICE 63e2713e. Solo escribe en ~/.cache/fase2c_r6_cloze.
suppressMessages({library(exams); library(jsonlite)})
BASE <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3"
rmd <- file.path(BASE, "cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
sch <- file.path(BASE, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd")
stopifnot(unname(tools::md5sum(rmd)) == "e16842c1157b3bffe35b8f21682ed6ad", unname(tools::md5sum(sch)) == "63e2713e6ac0bdbf250f4dc33901b2ec")
out <- path.expand("~/.cache/fase2c_r6_cloze"); td <- file.path(out, "tdir"); dir.create(td, showWarnings = FALSE)
L <- readLines(rmd, encoding = "UTF-8")
chunk <- function(nm) { i0 <- grep(paste0("^```\\{r ", nm), L); stopifnot(length(i0) == 1L); i1 <- which(L == "```" & seq_along(L) > i0)[1]; paste(L[(i0 + 1):(i1 - 1)], collapse = "\n") }
dg <- parse(text = chunk("data_generation"), encoding = "UTF-8", keep.source = FALSE)
so <- parse(text = chunk("solucion"), encoding = "UTF-8", keep.source = FALSE)
t0 <- Sys.time()
crudo <- lapply(1:1000, function(s) { set.seed(s); e <- new.env(parent = globalenv()); eval(dg, e)
  t <- paste(capture.output(eval(so, new.env(parent = e))), collapse = "\n")
  p <- regmatches(t, regexpr("Parte 6\\. Cada hipótesis[^\\n]*", t, perl = TRUE))
  list(s = s, par = p, tabla = e$tabla_par_md, opc6 = e$opciones_p6, ex = e$exsolution_str, rama = e$rama_par, c6 = e$c6$id, can = isTRUE(e$es_canonica), casc = e$cascada_par, rp1 = e$rama) })
write_json(crudo, file.path(out, "crudo_1000.json"), auto_unbox = TRUE, digits = NA)
can <- vapply(crudo, function(x) x$can, NA)
extra <- which(can & seq_along(can) > 100L & seq_along(can) <= 205L)
cat("canónicas en 1..100:", sum(can[1:100]), "· extra hasta 205:", length(extra), "\n"); seeds <- c(1:100, extra)
emite <- function(f, seeds) {
  res <- list()
  for (s in seeds) {
    t1 <- file.path(td, "t"); s1 <- file.path(td, "s"); unlink(c(t1, s1), recursive = TRUE); dir.create(t1); dir.create(s1)
    x <- tryCatch(xexams(f, n = 1L, driver = list(sweave = list(quiet = TRUE, encoding = "UTF-8")), dir = td, tdir = t1, sdir = s1, seed = matrix(s, 1, 1)), error = function(e) e)
    res[[as.character(s)]] <- if (inherits(x, "error")) list(err = conditionMessage(x)) else {
      y <- x[[1]][[1]]; m <- y$metainfo
      list(question = y$question, questionlist = y$questionlist, solution = y$solution, solutionlist = y$solutionlist,
           meta = list(type = m$type, clozetype = m$clozetype, solution = m$solution, string = m$string, points = m$points, tolerance = m$tolerance, shuffle = m$shuffle, name = m$name)) }
  }
  res
}
C <- emite(rmd, seeds); S <- emite(sch, seeds)
cat("errores CLOZE:", sum(vapply(C, function(r) !is.null(r$err), NA)), "· SCHOICE:", sum(vapply(S, function(r) !is.null(r$err), NA)), "\n")
write_json(C, file.path(out, "emit_cloze.json"), auto_unbox = FALSE, digits = NA, null = "null"); write_json(S, file.path(out, "emit_schoice.json"), auto_unbox = FALSE, digits = NA, null = "null")
saveRDS(list(C = C, S = S), file.path(out, "emit.rds"))
## Cadena de invariancia: contra el emitido de 6f6f1b13 (r5, semillas 1..205)
r5 <- readRDS(path.expand("~/.cache/fase2c_r5_cloze/emit.rds"))$N
F_NEW <- "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
F_OLD <- "Por eso basta comparar ese promedio con la diferencia entre los totales: si es mayor, el ganador cambia; si es menor, no."
dif <- c(question = 0, questionlist = 0, solutionlist = 0, solution = 0, frase_nueva = 0, frase_vieja_en_r5 = 0, compar = 0)
for (k in names(C)) { a <- C[[k]]; b <- r5[[k]]; if (is.null(b)) next; dif[["compar"]] <- dif[["compar"]] + 1
  dif[["question"]] <- dif[["question"]] + !identical(a$question, b$question); dif[["questionlist"]] <- dif[["questionlist"]] + !identical(a$questionlist, b$questionlist)
  dif[["solutionlist"]] <- dif[["solutionlist"]] + !identical(a$solutionlist, b$solutionlist)
  ta <- paste(a$solution, collapse = "\n"); tb <- paste(b$solution, collapse = "\n")
  dif[["frase_nueva"]] <- dif[["frase_nueva"]] + grepl(F_NEW, ta, fixed = TRUE); dif[["frase_vieja_en_r5"]] <- dif[["frase_vieja_en_r5"]] + grepl(F_OLD, tb, fixed = TRUE)
  dif[["solution"]] <- dif[["solution"]] + !identical(sub(F_NEW, "@@", ta, fixed = TRUE), sub(F_OLD, "@@", tb, fixed = TRUE)) }
print(dif)
## Moodle n = 100, con los mismos argumentos que exportar_moodle_cloze.R
md <- file.path(out, "moodle"); unlink(md, recursive = TRUE); dir.create(md); t1 <- file.path(td, "tm"); s1 <- file.path(td, "sm"); unlink(c(t1, s1), recursive = TRUE); dir.create(t1); dir.create(s1)
set.seed(20261103L)
exams2moodle(rmd, n = 100L, dir = md, name = "m100", tdir = t1, sdir = s1, cloze = list(cloze_schoice_display = "MULTICHOICE_V"))
cat("moodle:", file.size(file.path(md, "m100.xml")), "bytes; tiempo", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")
e <- C[["1"]]
cat("==== Q\n"); cat(e$question, sep = "\n"); cat("==== QL\n"); str(e$questionlist); cat("==== SL\n"); str(e$solutionlist); cat("==== META\n"); str(e$meta); cat("==== SOL\n"); cat(e$solution, sep = "\n")
