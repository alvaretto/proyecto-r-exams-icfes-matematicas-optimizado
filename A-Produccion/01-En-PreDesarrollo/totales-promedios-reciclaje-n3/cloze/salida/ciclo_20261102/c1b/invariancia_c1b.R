## invariancia_c1b.R — tanda C1b del ciclo 2026-11-02 del CLOZE (2026-11-03). Solo lee. Semillas 1 a 1000.
##   (a) .Random.seed al final del data_generation: nuevo = 27f789dc.
##   (b) E1: la Parte 1 (bloque P1) = SCHOICE 63e2713e, objetos y .Random.seed al final del bloque.
##   (c) Objetos: todos los de 27f789dc, idénticos; los únicos nuevos, hp_supera y hg_supera, iguales a v_hp y v_hg.
##   (d) Fuera de los chunks data_generation y solucion, el .Rmd es idéntico byte a byte (Question, Meta-information):
##       con (c), el enunciado, las opciones, las marcas y el feedback de las seis partes salen iguales.
##   (e) Solution emitida: idéntica salvo el párrafo «Parte 6.».
##   (f) Párrafo nuevo, leído del texto: cada comparación verdadera, la conclusión se sigue de ella y coincide con la marca
##       de HP y de HG. Control: en 27f789dc, versiones con alguna comparación falsa leída suelta.
## Uso: cd cloze/salida && Rscript --no-init-file ciclo_20261102/c1b/invariancia_c1b.R
suppressMessages(library(exams))
NUEVO <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
VIEJO <- normalizePath("ciclo_20261102/c1b/cloze_27f789dc.Rmd.txt")
SCH <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd")
cat("CLOZE nuevo", unname(tools::md5sum(NUEVO)), "· CLOZE viejo", unname(tools::md5sum(VIEJO)), "· SCHOICE", unname(tools::md5sum(SCH)), "\n")
stopifnot(unname(tools::md5sum(VIEJO)) == "27f789dc6327f7d6bda72cded61502cd", unname(tools::md5sum(SCH)) == "63e2713e6ac0bdbf250f4dc33901b2ec")
NB <- intToUtf8(0xA0)
lineas <- function(f) readLines(f, encoding = "UTF-8")
rango <- function(l, nm) { i0 <- grep(paste0("^```\\{r ", nm, "[,}]"), l)[1]; i1 <- which(l == "```" & seq_along(l) > i0)[1]; c(i0, i1) }
chunk <- function(f, nm) { l <- lineas(f); r <- rango(l, nm); paste(l[(r[1] + 1):(r[2] - 1)], collapse = "\n") }
fuera <- function(f) { l <- lineas(f); a <- rango(l, "data_generation"); b <- rango(l, "solucion"); l[-c(a[1]:a[2], b[1]:b[2])] }
MARCA <- "## ==== FIN DEL BLOQUE P1 ===="
DG_N <- chunk(NUEVO, "data_generation"); DG_V <- chunk(VIEJO, "data_generation"); DG_S <- chunk(SCH, "data_generation")
EX_N <- parse(text = DG_N, encoding = "UTF-8"); EX_V <- parse(text = DG_V, encoding = "UTF-8"); EX_S <- parse(text = DG_S, encoding = "UTF-8")
EX_P1 <- parse(text = strsplit(DG_N, MARCA, fixed = TRUE)[[1]][1], encoding = "UTF-8")
stopifnot(identical(strsplit(DG_N, MARCA, fixed = TRUE)[[1]][1], strsplit(DG_V, MARCA, fixed = TRUE)[[1]][1]))
SOL_N <- parse(text = chunk(NUEVO, "solucion"), encoding = "UTF-8"); SOL_V <- parse(text = chunk(VIEJO, "solucion"), encoding = "UTF-8")
correr <- function(ex, s) { set.seed(s); en <- new.env(); err <- tryCatch({ eval(ex, envir = en); NULL }, error = function(e) conditionMessage(e))
  list(en = en, err = err, seed = get(".Random.seed", envir = globalenv())) }
emite <- function(ex_sol, en) paste(capture.output(eval(ex_sol, envir = new.env(parent = en))), collapse = "\n")
ig <- function(x, y) identical(x, y, ignore.environment = TRUE, ignore.bytecode = TRUE, ignore.srcref = TRUE)
RE_P6 <- "Parte 6\\. Cada hipótesis[^\\n]*"
RE_CMP <- "([0-9]+) kilos[^0-9.:;]*?\\b(más|menos|mayor|menor) que [^0-9.:;]*?([0-9]+) kilos"
CONCL <- list(mas = c("habría ganado" = TRUE, "no le habría alcanzado para ganar" = FALSE),
              menos = c("habría perdido" = TRUE, "habría seguido ganando" = FALSE))
cmp_de <- function(p) { out <- list()
  for (cl in strsplit(gsub(NB, " ", p, fixed = TRUE), "[.;]")[[1]]) { m <- regmatches(cl, regexec(RE_CMP, cl, perl = TRUE))[[1]]
    if (length(m)) out[[length(out) + 1L]] <- list(cl = cl, x = as.numeric(m[2]), y = as.numeric(m[4]), sube = m[3] %in% c("más", "mayor")) }
  out }
cierto <- function(c) if (c$sube) c$x > c$y else c$x < c$y
stopifnot("fuera de los chunks data_generation y solucion, el .Rmd es idéntico" = identical(fuera(NUEVO), fuera(VIEJO)))
f <- list(err = character(0), seed = 0L, e1 = character(0), obj = character(0), nuevos = character(0), sol = 0L, p6_igual = 0L,
          n_cmp = 0L, falsa = 0L, concl = 0L, marca = 0L, papel = 0L)
ramas <- c(hp_si = 0L, hp_no = 0L, hg_si = 0L, hg_no = 0L); n_can <- 0L; viejo_falsa <- 0L; viejo_cmp <- 0L; ej <- list()
for (s in 1:1000) {
  N <- correr(EX_N, s); V <- correr(EX_V, s); S <- correr(EX_S, s); P <- correr(EX_P1, s)
  if (!is.null(N$err) || !is.null(V$err) || !is.null(S$err) || !is.null(P$err)) {
    f$err <- c(f$err, sprintf("s=%d %s|%s|%s|%s", s, format(N$err), format(V$err), format(S$err), format(P$err))); next }
  n_can <- n_can + isTRUE(N$en$es_canonica)
  if (!identical(N$seed, V$seed)) f$seed <- f$seed + 1L
  ## (b) E1 contra el SCHOICE
  REN <- c(opciones = "opciones_p1", sol = "sol_p1", feedback = "feedback_p1")
  nS <- setdiff(ls(S$en), names(REN)); nP <- setdiff(ls(P$en), REN)
  if (!setequal(nS, nP)) f$e1 <- c(f$e1, sprintf("s=%d objetos distintos", s))
  for (k in intersect(nS, nP)) if (!ig(S$en[[k]], P$en[[k]])) f$e1 <- c(f$e1, sprintf("s=%d %s", s, k))
  for (k in names(REN)) if (!ig(S$en[[k]], P$en[[REN[[k]]]])) f$e1 <- c(f$e1, sprintf("s=%d %s", s, REN[[k]]))
  if (!identical(S$seed, P$seed)) f$e1 <- c(f$e1, sprintf("s=%d .Random.seed al final del bloque P1", s))
  ## (c) objetos
  nuevos <- setdiff(ls(N$en), ls(V$en)); if (!setequal(nuevos, c("hp_supera", "hg_supera"))) f$nuevos <- c(f$nuevos, sprintf("s=%d %s", s, paste(nuevos, collapse = ",")))
  if (length(setdiff(ls(V$en), ls(N$en)))) f$obj <- c(f$obj, sprintf("s=%d faltan objetos", s))
  for (k in ls(V$en)) if (!ig(N$en[[k]], V$en[[k]])) f$obj <- c(f$obj, sprintf("s=%d %s", s, k))
  if (!identical(N$en$hp_supera, N$en$v_hp) || !identical(N$en$hg_supera, N$en$v_hg)) f$nuevos <- c(f$nuevos, sprintf("s=%d hp_supera/hg_supera != v_hp/v_hg", s))
  ## (e) Solution
  tN <- emite(SOL_N, N$en); tV <- emite(SOL_V, V$en)
  pN <- regmatches(tN, regexpr(RE_P6, tN, perl = TRUE)); pV <- regmatches(tV, regexpr(RE_P6, tV, perl = TRUE))
  if (length(pN) != 1L || length(pV) != 1L) { f$sol <- f$sol + 1L; next }
  if (!identical(sub(pN, "", tN, fixed = TRUE), sub(pV, "", tV, fixed = TRUE))) f$sol <- f$sol + 1L
  if (identical(pN, pV)) f$p6_igual <- f$p6_igual + 1L
  ## (f) párrafo nuevo, leído del texto, contra la marca de HP y HG
  cs <- cmp_de(pN); f$n_cmp <- f$n_cmp + length(cs)
  s6 <- N$en$sol_p6; r6 <- N$en$rol_p6
  for (c in cs) {
    if (!cierto(c)) f$falsa <- f$falsa + 1L
    pap <- if (grepl("un curso más", c$cl, fixed = TRUE)) "mas" else if (grepl("un curso menos", c$cl, fixed = TRUE)) "menos" else NA
    if (is.na(pap)) { f$papel <- f$papel + 1L; next }
    hay <- vapply(names(CONCL[[pap]]), function(k) grepl(k, c$cl, fixed = TRUE), NA)
    if (sum(hay) != 1L) { f$concl <- f$concl + 1L; next }
    cv <- unname(CONCL[[pap]][hay]); if (cv != c$sube) f$concl <- f$concl + 1L
    rol <- if (pap == "mas") "hp" else "hg"; if (!identical(cv, isTRUE(s6[r6 == rol]))) f$marca <- f$marca + 1L
    k <- paste0(rol, if (cv) "_si" else "_no"); ramas[[k]] <- ramas[[k]] + 1L
    clave_ej <- sprintf("%s · %s", N$en$rama_par, k); if (is.null(ej[[clave_ej]])) ej[[clave_ej]] <- sprintf("semilla %d: %s", s, gsub(NB, " ", pN, fixed = TRUE))
  }
  ## control: en 27f789dc, la comparación leída suelta («su promedio …, a kilos, es mayor que la diferencia, D kilos»)
  cv0 <- cmp_de(pV); viejo_cmp <- viejo_cmp + length(cv0)
  aGv <- as.numeric(regmatches(pV, regexec(paste0("su promedio, ([0-9]+)", NB, "kilos, es mayor que esa"), pV))[[1]][2])
  Dv <- V$en$dp$D
  if (any(!vapply(cv0, cierto, NA)) || !isTRUE(aGv > Dv)) viejo_falsa <- viejo_falsa + 1L
}
cat(sprintf("Semillas 1 a 1000 (%d canónicas); errores de ejecución: %d\n", n_can, length(f$err))); if (length(f$err)) print(head(f$err))
cat(sprintf("(a) .Random.seed final distinto de 27f789dc: %d de 1000\n", f$seed))
cat(sprintf("(b) E1 (Parte 1 = SCHOICE 63e2713e, objetos y .Random.seed al final del bloque): %d diferencias\n", length(f$e1))); if (length(f$e1)) print(head(f$e1))
cat(sprintf("(c) objetos de 27f789dc distintos o ausentes en el nuevo: %d · objetos nuevos distintos de {hp_supera, hg_supera} o distintos de v_hp/v_hg: %d\n",
            length(f$obj), length(f$nuevos))); if (length(f$obj)) print(head(f$obj)); if (length(f$nuevos)) print(head(f$nuevos))
cat("(d) fuera de los chunks data_generation y solucion, el .Rmd es idéntico byte a byte a 27f789dc (stopifnot)\n")
cat(sprintf("(e) Solution distinta de 27f789dc fuera del párrafo «Parte 6.»: %d de 1000 · párrafo «Parte 6.» igual al viejo: %d de 1000\n", f$sol, f$p6_igual))
cat(sprintf("(f) párrafo nuevo, leído del texto: %d comparaciones con cifras (esperadas 2000) · falsas %d · sin «un curso más» ni «un curso menos» %d · conclusión que no se sigue %d · conclusión contra la marca de HP o HG %d\n",
            f$n_cmp, f$falsa, f$papel, f$concl, f$marca))
cat(sprintf("    por rama: HP verdadera %d, HP falsa %d, HG verdadera %d, HG falsa %d\n", ramas[["hp_si"]], ramas[["hp_no"]], ramas[["hg_si"]], ramas[["hg_no"]]))
cat(sprintf("    control, 27f789dc: %d comparaciones con dos cifras (una por versión; la de HG solo cita una cifra) · versiones con alguna comparación falsa leída suelta: %d de 1000\n",
            viejo_cmp, viejo_falsa))
cat("Un ejemplo por rama del otro colegio y veredicto:\n"); for (k in sort(names(ej))) cat("  ", k, " — ", ej[[k]], "\n", sep = "")
