## invariancia_c1e.R — tanda C1e del ciclo 2026-11-02 del CLOZE (2026-11-03). Solo lee. Contra 32243d0a.
##   (a) El .Rmd: deshacer la frase da 32243d0a byte a byte; con eso, data_generation, Question y Meta-information, idénticos.
##   (b) Semillas 1 a 1000: .Random.seed al final del data_generation, igual; todos los objetos, idénticos; ningún objeto nuevo.
##   (c) E1, semillas 1 a 1000: la Parte 1 (bloque P1) = SCHOICE 63e2713e, objetos y .Random.seed al final del bloque.
##   (d) Solution emitida (chunk solucion), semillas 1 a 1000: idéntica salvo la frase; el resto del párrafo, idéntico byte a byte.
##   (e) Texto emitido por R/exams (xexams + read_exercise, después de exshuffle), semillas 1 a 100: enunciado, questionlist,
##       solutionlist y metainformación (salvo file), idénticos; solution, idéntica salvo la frase. Sin escribir en el repo.
##   Control positivo: la frase vieja está en la vieja y no en la nueva, y al revés, en todas las versiones.
## Uso: cd cloze/salida && Rscript --no-init-file ciclo_20261102/c1e/invariancia_c1e.R
suppressMessages(library(exams))
NOMBRE <- "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"
NUEVO <- normalizePath(file.path("..", NOMBRE))
VIEJO <- normalizePath("ciclo_20261102/c1e/cloze_32243d0a.Rmd.txt")
SCH <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd")
cat("CLOZE nuevo", unname(tools::md5sum(NUEVO)), "· CLOZE viejo", unname(tools::md5sum(VIEJO)), "· SCHOICE", unname(tools::md5sum(SCH)), "\n")
stopifnot(unname(tools::md5sum(VIEJO)) == "32243d0a6e6c499a4c6b1213a49e95cd", unname(tools::md5sum(SCH)) == "63e2713e6ac0bdbf250f4dc33901b2ec")
F_VIEJA <- "Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
F_NUEVA <- "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
cuenta <- function(p, t) lengths(regmatches(t, gregexpr(p, t, fixed = TRUE)))
## (a)
crudo <- function(f) { x <- rawToChar(readBin(f, "raw", file.size(f))); Encoding(x) <- "UTF-8"; x }
SRC_N <- '"promedio al total, y un curso menos lo resta. Por eso, para saber si el grado que perdió habría ganado con un ",\n    "curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la ",\n    "diferencia entre los totales: si el promedio es mayor, sí; si es menor, no. ",\n    "Con un curso más, el grado ", per_par,\n'
SRC_V <- '"promedio al total, y un curso menos lo resta. Por eso, si el grado que perdió tiene un curso más o el que ganó ",\n    "tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es ",\n    "mayor, el ganador cambia; si es menor, no. ",\n    "Con un curso más, el grado ", per_par,\n'
tN <- crudo(NUEVO); tV <- crudo(VIEJO)
stopifnot("el patrón fuente aparece una vez en el nuevo" = cuenta(SRC_N, tN) == 1L, "y una vez en el viejo" = cuenta(SRC_V, tV) == 1L)
cat(sprintf("(a) deshacer la frase en el .Rmd nuevo da 32243d0a byte a byte: %s\n", identical(charToRaw(sub(SRC_N, SRC_V, tN, fixed = TRUE)), charToRaw(tV))))
lineas <- function(f) readLines(f, encoding = "UTF-8")
rango <- function(l, nm) { i0 <- grep(paste0("^```\\{r ", nm, "[,}]"), l)[1]; i1 <- which(l == "```" & seq_along(l) > i0)[1]; c(i0, i1) }
chunk <- function(f, nm) { l <- lineas(f); r <- rango(l, nm); paste(l[(r[1] + 1):(r[2] - 1)], collapse = "\n") }
MARCA <- "## ==== FIN DEL BLOQUE P1 ===="
DG_N <- chunk(NUEVO, "data_generation"); DG_V <- chunk(VIEJO, "data_generation"); DG_S <- chunk(SCH, "data_generation")
cat(sprintf("    chunk data_generation idéntico: %s\n", identical(DG_N, DG_V)))
EX_N <- parse(text = DG_N, encoding = "UTF-8"); EX_V <- parse(text = DG_V, encoding = "UTF-8"); EX_S <- parse(text = DG_S, encoding = "UTF-8")
EX_P1 <- parse(text = strsplit(DG_N, MARCA, fixed = TRUE)[[1]][1], encoding = "UTF-8")
SOL_N <- parse(text = chunk(NUEVO, "solucion"), encoding = "UTF-8"); SOL_V <- parse(text = chunk(VIEJO, "solucion"), encoding = "UTF-8")
correr <- function(ex, s) { set.seed(s); en <- new.env(); err <- tryCatch({ eval(ex, envir = en); NULL }, error = function(e) conditionMessage(e))
  list(en = en, err = err, seed = get(".Random.seed", envir = globalenv())) }
emite <- function(ex_sol, en) paste(capture.output(eval(ex_sol, envir = new.env(parent = en))), collapse = "\n")
ig <- function(x, y) identical(x, y, ignore.environment = TRUE, ignore.bytecode = TRUE, ignore.srcref = TRUE)
RE_P6 <- "Parte 6\\. Cada hipótesis[^\\n]*"
f <- list(err = character(0), seed = 0L, e1 = character(0), obj = character(0), nuevos = character(0), sol = 0L, p6 = 0L, ctrl = 0L)
n_can <- 0L; n_obj <- integer(0); ej <- character(0)
for (s in 1:1000) {
  N <- correr(EX_N, s); V <- correr(EX_V, s); S <- correr(EX_S, s); P <- correr(EX_P1, s)
  if (!is.null(N$err) || !is.null(V$err) || !is.null(S$err) || !is.null(P$err)) {
    f$err <- c(f$err, sprintf("s=%d %s|%s|%s|%s", s, format(N$err), format(V$err), format(S$err), format(P$err))); next }
  n_can <- n_can + isTRUE(N$en$es_canonica)
  if (!identical(N$seed, V$seed)) f$seed <- f$seed + 1L
  ## (c) E1 contra el SCHOICE
  REN <- c(opciones = "opciones_p1", sol = "sol_p1", feedback = "feedback_p1")
  nS <- setdiff(ls(S$en), names(REN)); nP <- setdiff(ls(P$en), REN)
  if (!setequal(nS, nP)) f$e1 <- c(f$e1, sprintf("s=%d objetos distintos", s))
  for (k in intersect(nS, nP)) if (!ig(S$en[[k]], P$en[[k]])) f$e1 <- c(f$e1, sprintf("s=%d %s", s, k))
  for (k in names(REN)) if (!ig(S$en[[k]], P$en[[REN[[k]]]])) f$e1 <- c(f$e1, sprintf("s=%d %s", s, REN[[k]]))
  if (!identical(S$seed, P$seed)) f$e1 <- c(f$e1, sprintf("s=%d .Random.seed al final del bloque P1", s))
  ## (b) objetos
  lN <- ls(N$en, all.names = TRUE); lV <- ls(V$en, all.names = TRUE); n_obj <- c(n_obj, length(lN))
  if (!identical(lN, lV)) f$nuevos <- c(f$nuevos, sprintf("s=%d nuevos: %s; faltan: %s", s, paste(setdiff(lN, lV), collapse = ","), paste(setdiff(lV, lN), collapse = ",")))
  for (k in intersect(lN, lV)) if (!ig(N$en[[k]], V$en[[k]])) f$obj <- c(f$obj, sprintf("s=%d %s", s, k))
  ## (d) Solution
  a <- emite(SOL_N, N$en); b <- emite(SOL_V, V$en)
  if (cuenta(F_NUEVA, a) != 1L || cuenta(F_VIEJA, a) != 0L || cuenta(F_VIEJA, b) != 1L || cuenta(F_NUEVA, b) != 0L) f$ctrl <- f$ctrl + 1L
  if (!identical(sub(F_NUEVA, F_VIEJA, a, fixed = TRUE), b)) f$sol <- f$sol + 1L
  pN <- regmatches(a, regexpr(RE_P6, a, perl = TRUE)); pV <- regmatches(b, regexpr(RE_P6, b, perl = TRUE))
  if (length(pN) != 1L || length(pV) != 1L || !identical(sub(F_NUEVA, F_VIEJA, pN, fixed = TRUE), pV)) f$p6 <- f$p6 + 1L
  if (s %in% c(1L, 10L)) ej <- c(ej, sprintf("semilla %d: %s", s, gsub(" ", "~", pN, fixed = TRUE)))
}
cat(sprintf("Semillas 1 a 1000 (%d canónicas); errores de ejecución: %d\n", n_can, length(f$err))); if (length(f$err)) print(head(f$err))
cat(sprintf("(b) .Random.seed final distinto de 32243d0a: %d de 1000 · objetos distintos: %d · versiones con objetos nuevos o que faltan: %d · objetos por versión (incluidos los ocultos): de %d a %d\n",
            f$seed, length(f$obj), length(f$nuevos), min(n_obj), max(n_obj))); if (length(f$obj)) print(head(f$obj)); if (length(f$nuevos)) print(head(f$nuevos))
cat(sprintf("(c) E1 (Parte 1 = SCHOICE 63e2713e, objetos y .Random.seed al final del bloque): %d diferencias\n", length(f$e1))); if (length(f$e1)) print(head(f$e1))
cat(sprintf("(d) Solution emitida distinta de 32243d0a fuera de la frase: %d de 1000 · párrafo «Parte 6.» distinto fuera de la frase: %d de 1000 · control (cada frase solo en su versión) fallido: %d de 1000\n", f$sol, f$p6, f$ctrl))
cat("    Párrafo nuevo, ejemplos (~ = U+00A0):\n"); for (e in ej) cat("    ", e, "\n", sep = "")
## (e) Texto emitido por R/exams
BASE <- path.expand("~/.cache/c1e_cloze"); OLD <- file.path(BASE, "old"); TD <- file.path(BASE, "tdir"); dir.create(OLD, recursive = TRUE, showWarnings = FALSE); dir.create(TD, showWarnings = FALSE)
file.copy(VIEJO, file.path(OLD, NOMBRE), overwrite = TRUE); stopifnot(unname(tools::md5sum(file.path(OLD, NOMBRE))) == "32243d0a6e6c499a4c6b1213a49e95cd")
emx <- function(f, s) { t1 <- file.path(TD, "t"); s1 <- file.path(TD, "s"); unlink(c(t1, s1), recursive = TRUE); dir.create(t1); dir.create(s1)
  x <- xexams(f, n = 1L, driver = list(sweave = list(quiet = TRUE, encoding = "UTF-8")), dir = TD, tdir = t1, sdir = s1, seed = matrix(s, 1, 1))
  x[[1]][[1]][c("question", "questionlist", "solution", "solutionlist", "metainfo")] }
dif <- character(0); nf <- 0L
for (s in 1:100) { a <- emx(NUEVO, s); b <- emx(file.path(OLD, NOMBRE), s)
  for (k in c("question", "questionlist", "solutionlist")) if (!identical(a[[k]], b[[k]])) dif <- c(dif, paste(s, k))
  for (k in setdiff(union(names(a$metainfo), names(b$metainfo)), "file")) if (!identical(a$metainfo[[k]], b$metainfo[[k]])) dif <- c(dif, paste(s, "meta", k))
  sa <- paste(a$solution, collapse = "\n"); sb <- paste(b$solution, collapse = "\n")
  if (cuenta(F_NUEVA, sa) == 1L && cuenta(F_VIEJA, sb) == 1L) nf <- nf + 1L
  if (!identical(sub(F_NUEVA, F_VIEJA, sa, fixed = TRUE), sb)) dif <- c(dif, paste(s, "solution fuera de la frase")) }
cat(sprintf("(e) Texto emitido (xexams, después de exshuffle), semillas 1 a 100: %d diferencias en enunciado, questionlist, solutionlist, metainformación salvo file, o solution fuera de la frase · la frase, una vez en cada versión: %d de 100\n", length(dif), nf))
if (length(dif)) print(head(dif, 10))
