## Coordinador, 2026-11-03: comprobación independiente de la tanda C1e del CLOZE (no reutiliza sus scripts).
## (A) data_generation, semillas 1 a N, del CLOZE nuevo frente al de la C1 (27f789dc):
##     - el .Random.seed final y TODOS los objetos comunes, idénticos;
##     - los únicos objetos nuevos son hp_supera y hg_supera, y coinciden con v_hp y v_hg;
##     - E1: la Parte 1 del CLOZE nuevo, igual al SCHOICE aprobado.
## (B) texto emitido (xexams + read_exercise), semillas 1 a NT:
##     - enunciado, questionlist, solutionlist, metainfo y Solution, idénticos salvo el párrafo «Parte 6.»;
##     - en el párrafo nuevo, las cifras y los grados son los del párrafo viejo de la misma semilla, «más» o «menos»
##       es verdadero leído suelto y la conclusión se sigue de la comparación.
## Uso (desde la raíz del repo):
##   Rscript --no-init-file <este archivo> <schoice.Rmd> <cloze_viejo.Rmd> <cloze_nuevo.Rmd> <cloze_32243d0a.Rmd> [N] [NT]
suppressPackageStartupMessages(library(exams))
a <- commandArgs(trailingOnly = TRUE)
N <- if (length(a) >= 5) as.integer(a[5]) else 1000L
NT <- if (length(a) >= 6) as.integer(a[6]) else 100L
cat("md5:", paste(basename(a[1:4]), unname(tools::md5sum(a[1:4])), collapse = " · "), "\n")

## (A) ---------------------------------------------------------------------------------------------------------------
dg <- function(f) {
  l <- readLines(f, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", l); cs <- which(l == "```")
  i1 <- cs[cs > i0][1]; parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8")
}
ES <- dg(a[1]); EV <- dg(a[2]); EN <- dg(a[3])
corre <- function(E, s) {
  e <- new.env(); set.seed(s)
  err <- tryCatch({ eval(E, envir = e); NULL }, error = function(x) conditionMessage(x))
  list(e = e, err = err, sem = if (is.null(err)) .Random.seed else NULL)
}
igual <- function(x, y) {
  if (is.function(x) || is.function(y))
    return(is.function(x) && is.function(y) && identical(x, y, ignore.environment = TRUE))
  if (is.environment(x) || is.environment(y)) return(is.environment(x) && is.environment(y))
  if (is.list(x) || is.list(y)) {
    if (!(is.list(x) && is.list(y)) || length(x) != length(y) || !identical(attributes(x), attributes(y))) return(FALSE)
    return(all(vapply(seq_along(x), function(i) igual(x[[i]], y[[i]]), TRUE)))
  }
  identical(x, y)
}
P1_S <- c("opciones", "sol", "feedback", "rama", "es_canonica", "tabla_md", "respuesta_correcta")
P1_C <- c("opciones_p1", "sol_p1", "feedback_p1", "rama", "es_canonica", "tabla_md", "respuesta_correcta")
NUEVOS <- c("hg_supera", "hp_supera")
r <- c(err = 0, e1 = 0, sem = 0, obj = 0, nuevos = 0, ata = 0, hp = 0, hg = 0)
dif <- character(0); extra <- character(0); ncomunes <- NA_integer_
for (s in seq_len(N)) {
  S <- corre(ES, s); V <- corre(EV, s); C <- corre(EN, s)
  if (!is.null(S$err) || !is.null(V$err) || !is.null(C$err)) { r["err"] <- r["err"] + 1; next }
  r["e1"] <- r["e1"] + all(mapply(igual, unname(mget(P1_S, envir = S$e)), unname(mget(P1_C, envir = C$e))))
  r["sem"] <- r["sem"] + identical(V$sem, C$sem)
  nv <- ls(V$e, all.names = TRUE); nc <- ls(C$e, all.names = TRUE); com <- intersect(nv, nc)
  ncomunes <- length(com); extra <- union(extra, setdiff(union(nv, nc), com))
  ig <- vapply(com, function(k) igual(get(k, envir = V$e), get(k, envir = C$e)), TRUE)
  if (all(ig)) r["obj"] <- r["obj"] + 1 else dif <- c(dif, com[!ig])
  r["nuevos"] <- r["nuevos"] + (identical(sort(setdiff(nc, nv)), NUEVOS) && length(setdiff(nv, nc)) == 0L)
  r["ata"] <- r["ata"] + (isTRUE(C$e$hp_supera == C$e$v_hp) && isTRUE(C$e$hg_supera == C$e$v_hg))
  r["hp"] <- r["hp"] + isTRUE(C$e$hp_supera); r["hg"] <- r["hg"] + isTRUE(C$e$hg_supera)
}
cat(sprintf("(A) semillas 1 a %d · errores de ejecución: %d\n", N, r[["err"]]))
cat(sprintf("(A) E1, Parte 1 del CLOZE nuevo = SCHOICE (%d objetos): %d / %d\n", length(P1_S), r[["e1"]], N))
cat(sprintf("(A) .Random.seed final idéntico al CLOZE de la C1: %d / %d\n", r[["sem"]], N))
cat(sprintf("(A) los %d objetos comunes, idénticos: %d / %d\n", ncomunes, r[["obj"]], N))
if (length(dif)) print(table(dif))
cat(sprintf("(A) objetos nuevos = {%s} y ninguno quitado: %d / %d (nombres que difieren: %s)\n",
            paste(NUEVOS, collapse = ", "), r[["nuevos"]], N, paste(sort(extra), collapse = ", ")))
cat(sprintf("(A) hp_supera == v_hp y hg_supera == v_hg: %d / %d · hp_supera verdadero en %d, hg_supera en %d\n",
            r[["ata"]], N, r[["hp"]], r[["hg"]]))

## (B) ---------------------------------------------------------------------------------------------------------------
leer <- function(f, s, dir) {
  x <- xexams(f, n = 1L, dir = dir, seed = s,
              driver = list(sweave = list(quiet = TRUE), read = NULL, transform = NULL, write = NULL))
  x[[1]][[1]]
}
nb <- function(v) gsub(" ", " ", v, fixed = TRUE)
P6 <- "^Parte 6\\. Cada hipótesis"
pv <- paste0("Por eso el grado (.+) habría ganado con un curso más si y solo si su promedio por curso, ([0-9]+) kilos, ",
             "es mayor que la diferencia, ([0-9]+) kilos; y el grado (.+) habría perdido con un curso menos si y solo si ",
             "su promedio, ([0-9]+) kilos, es mayor que esa diferencia\\.$")
FN <- "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
FV <- "Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
pn <- paste0("Con un curso más, el grado (.+) habría sumado a su total ([0-9]+) kilos, (más|menos) que la diferencia de ",
             "([0-9]+) kilos: (habría ganado|no le habría alcanzado para ganar)\\. Con un curso menos, el grado (.+) ",
             "habría restado a su total ([0-9]+) kilos, (más|menos) que la diferencia de ([0-9]+) kilos: ",
             "(habría perdido|habría seguido ganando)\\.$")
dV <- tempfile("c1b_v_"); dC <- tempfile("c1b_c_"); dir.create(dV); dir.create(dC)
t <- c(q = 0, ql = 0, sl = 0, meta = 0, sol_fuera = 0, general = 0, forma = 0, cifras = 0, grados = 0,
       comp_ok = 0, concl_ok = 0)
ej <- list()
for (s in seq_len(NT)) {
  V <- leer(a[2], s, dV); C <- leer(a[3], s, dC)
  t["q"] <- t["q"] + identical(V$question, C$question)
  t["ql"] <- t["ql"] + identical(V$questionlist, C$questionlist)
  t["sl"] <- t["sl"] + identical(V$solutionlist, C$solutionlist)
  mV <- V$metainfo; mC <- C$metainfo; mV$file <- NULL; mC$file <- NULL
  t["meta"] <- t["meta"] + identical(mV, mC)
  sV <- nb(V$solution); sC <- nb(C$solution); iV <- grep(P6, sV); iC <- grep(P6, sC)
  if (length(iV) != 1L || length(iC) != 1L) stop("semilla ", s, ": no hay un solo párrafo «Parte 6.»")
  t["sol_fuera"] <- t["sol_fuera"] + identical(sV[-iV], sC[-iC])
  t["general"] <- t["general"] + grepl(FN, sC[iC], fixed = TRUE)
  mv <- regmatches(sV[iV], regexec(pv, sV[iV]))[[1]]; mn <- regmatches(sC[iC], regexec(pn, sC[iC]))[[1]]
  if (length(mv) != 6L) stop("semilla ", s, ": el párrafo viejo no tiene la forma esperada")
  if (length(mn) != 11L) { cat("semilla", s, "· forma inesperada:\n", sC[iC], "\n"); next }
  t["forma"] <- t["forma"] + 1
  per <- mv[2]; aP <- as.integer(mv[3]); D <- as.integer(mv[4]); gan <- mv[5]; aG <- as.integer(mv[6])
  A <- as.integer(mn[3]); D1 <- as.integer(mn[5]); B <- as.integer(mn[8]); D2 <- as.integer(mn[10])
  t["cifras"] <- t["cifras"] + (A == aP && B == aG && D1 == D && D2 == D)
  t["grados"] <- t["grados"] + (mn[2] == per && mn[7] == gan)
  t["comp_ok"] <- t["comp_ok"] + ((mn[4] == "más") == (A > D1)) + ((mn[9] == "más") == (B > D2))
  t["concl_ok"] <- t["concl_ok"] + ((mn[6] == "habría ganado") == (mn[4] == "más")) +
    ((mn[11] == "habría perdido") == (mn[9] == "más"))
  clave <- paste(mn[4], mn[9])
  if (is.null(ej[[clave]])) ej[[clave]] <- sprintf("semilla %d: %s", s, sub("^.*(Con un curso más)", "\\1", sC[iC]))
}
cat(sprintf("\n(B) texto emitido, semillas 1 a %d (xexams)\n", NT))
cat(sprintf("(B) question idéntico: %d · questionlist: %d · solutionlist: %d · metainfo (sin file): %d\n",
            t[["q"]], t[["ql"]], t[["sl"]], t[["meta"]]))
cat(sprintf("(B) Solution idéntica fuera del párrafo «Parte 6.»: %d / %d\n", t[["sol_fuera"]], NT))
cat(sprintf("(B) criterio general sin cifras presente: %d / %d · párrafo con la forma esperada: %d / %d\n",
            t[["general"]], NT, t[["forma"]], NT))
cat(sprintf("(B) cifras = las del párrafo viejo (aP, aG y D): %d / %d · grados iguales: %d / %d\n",
            t[["cifras"]], NT, t[["grados"]], NT))
cat(sprintf("(B) comparaciones verdaderas leídas sueltas: %d / %d · conclusiones que se siguen: %d / %d\n",
            t[["comp_ok"]], 2L * NT, t[["concl_ok"]], 2L * NT))
cat("(B) un ejemplo por combinación (HP · HG):\n"); for (k in names(ej)) cat("  [", k, "] ", ej[[k]], "\n", sep = "")

## (C) contra 32243d0a ---------------------------------------------------------------------------------------------
fuente <- function(f) readLines(f, encoding = "UTF-8")
chunk <- function(l) { i0 <- grep("^```\\{r data_generation", l); cs <- which(l == "```"); l[(i0 + 1):(cs[cs > i0][1] - 1)] }
L6 <- fuente(a[4]); LN <- fuente(a[3])
cat(sprintf("\n(C) contra 32243d0a: data_generation idéntico byte a byte: %s\n", identical(chunk(L6), chunk(LN))))
d6 <- tempfile("c1c_6_"); dN <- tempfile("c1c_n_"); dir.create(d6); dir.create(dN)
tc <- c(q = 0, ql = 0, sl = 0, meta = 0, sol = 0, una = 0)
for (s in seq_len(NT)) {
  V <- leer(a[4], s, d6); C <- leer(a[3], s, dN)
  tc["q"] <- tc["q"] + identical(V$question, C$question)
  tc["ql"] <- tc["ql"] + identical(V$questionlist, C$questionlist)
  tc["sl"] <- tc["sl"] + identical(V$solutionlist, C$solutionlist)
  mV <- V$metainfo; mC <- C$metainfo; mV$file <- NULL; mC$file <- NULL
  tc["meta"] <- tc["meta"] + identical(mV, mC)
  tc["una"] <- tc["una"] + (sum(lengths(regmatches(C$solution, gregexpr(FN, C$solution, fixed = TRUE)))) == 1L)
  tc["sol"] <- tc["sol"] + identical(sub(FN, FV, C$solution, fixed = TRUE), V$solution)
}
cat(sprintf("(C) question: %d · questionlist: %d · solutionlist: %d · metainfo (sin file): %d, de %d\n",
            tc[["q"]], tc[["ql"]], tc[["sl"]], tc[["meta"]], NT))
cat(sprintf("(C) la frase nueva sale una vez: %d / %d · Solution byte a byte igual a 32243d0a al deshacer la frase: %d / %d\n",
            tc[["una"]], NT, tc[["sol"]], NT))
