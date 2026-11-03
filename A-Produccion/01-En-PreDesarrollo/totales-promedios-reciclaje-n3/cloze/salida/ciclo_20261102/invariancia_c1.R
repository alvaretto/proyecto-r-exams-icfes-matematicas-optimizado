## invariancia_c1.R — tanda C1 del ciclo 2026-11-02 del CLOZE (2026-11-03). Solo lee. Semillas 1 a 1000.
##   (a) .Random.seed al final del data_generation: CLOZE nuevo = CLOZE f98f9dda (las Partes 2 a 6 sortean igual).
##   (b) Objetos propios del CLOZE (los que el bloque P1 no define): idénticos a f98f9dda; si alguno difiere, se
##       informa y se compara tras normalizar el espacio duro.
##   (c) E1 ampliado: la Parte 1 del CLOZE (bloque P1) = SCHOICE 63e2713e, todos los objetos y .Random.seed.
##   (d) Solution emitida: igual a la de f98f9dda tras deshacer, en el texto, el espacio duro, la unidad y el signo
##       dentro de la fórmula (L-6) y el cambio de H3; cuentas de L-6 en la prosa de la Parte 1 y en la del resto.
##   (e) H3: la marca de HP y HG coincide con «si y solo si su promedio \u2026 es mayor que la diferencia\u00bb, leído del texto.
## Uso: cd cloze/salida && Rscript --no-init-file ciclo_20261102/invariancia_c1.R
suppressMessages(library(exams))
NUEVO <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
VIEJO <- normalizePath("ciclo_20261102/cloze_f98f9dda.Rmd.txt")
SCH <- normalizePath("../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd")
cat("CLOZE nuevo", unname(tools::md5sum(NUEVO)), "· CLOZE viejo", unname(tools::md5sum(VIEJO)), "· SCHOICE", unname(tools::md5sum(SCH)), "\n")
stopifnot(unname(tools::md5sum(VIEJO)) == "f98f9dda47e4f3cafc4e8b4e82cc956b", unname(tools::md5sum(SCH)) == "63e2713e6ac0bdbf250f4dc33901b2ec")
chunk <- function(f, nm) { l <- readLines(f, encoding = "UTF-8"); i0 <- grep(paste0("^```\\{r ", nm, "[,}]"), l)[1]
  i1 <- which(l == "```" & seq_along(l) > i0)[1]; paste(l[(i0 + 1):(i1 - 1)], collapse = "\n") }
MARCA <- "## ==== FIN DEL BLOQUE P1 ===="
DG_N <- chunk(NUEVO, "data_generation"); DG_V <- chunk(VIEJO, "data_generation"); DG_S <- chunk(SCH, "data_generation")
EX_N <- parse(text = DG_N, encoding = "UTF-8"); EX_V <- parse(text = DG_V, encoding = "UTF-8"); EX_S <- parse(text = DG_S, encoding = "UTF-8")
EX_P1 <- parse(text = strsplit(DG_N, MARCA, fixed = TRUE)[[1]][1], encoding = "UTF-8")
SOL_N <- parse(text = chunk(NUEVO, "solucion"), encoding = "UTF-8"); SOL_V <- parse(text = chunk(VIEJO, "solucion"), encoding = "UTF-8")
correr <- function(ex, s) { set.seed(s); en <- new.env(); err <- tryCatch({ eval(ex, envir = en); NULL }, error = function(e) conditionMessage(e))
  list(en = en, err = err, seed = get(".Random.seed", envir = globalenv())) }
emite <- function(ex_sol, en) paste(capture.output(eval(ex_sol, envir = en)), collapse = "\n")
ig <- function(x, y) identical(x, y, ignore.environment = TRUE, ignore.bytecode = TRUE, ignore.srcref = TRUE)
NB <- "\u00a0"
## Deshace L-6 y H3 en el texto emitido, para comparar con f98f9dda.
deshacer <- function(t) {
  t <- gsub("\\\\ \\\\text\\{(kilos?|cursos?)([.,:;!?])\\}\\$", "$ \\1\\2", t, perl = TRUE)
  t <- gsub("\\\\ \\\\text\\{(kilos?|cursos?)\\}\\$", "$ \\1", t, perl = TRUE)
  t <- gsub("\\\\text\\{([.,:;!?])\\}\\$", "$\\1", t, perl = TRUE)
  t <- gsub("si y solo si su promedio", "solo si su promedio", t, fixed = TRUE)
  t <- gsub("kilos, es mayor que", "kilos, fuera mayor que", t, fixed = TRUE)
  gsub(NB, " ", t, fixed = TRUE)
}
RE_UNI_FUERA <- "\\$[^A-Za-z0-9$\\n]{0,3}(kilos?|cursos?)\\b"
RE_SIG_FUERA <- "\\$[^A-Za-z0-9$\\n]{0,3}[.,:;!?)\\]\u00bb\u201d\u2026]"
RE_ESP_NORMAL <- "[0-9] (kilos?|cursos?)\\b|El Bosque|La Pradera"
cuenta <- function(re, t) sum(lengths(regmatches(t, gregexpr(re, t, perl = TRUE))))
## La prosa propia de la Parte 1 en la Solution del CLOZE: el párrafo «Parte 1.\u00bb del procedimiento (hasta «Parte 2.\u00bb),
## el análisis de sus distractores (hasta «Parte 2 ·\u00bb) y el párrafo «En la Parte 1,\u00bb del caso específico.
p1_de_sol <- function(t) {
  a <- regmatches(t, regexpr("Parte 1\\. El concurso premia[\\s\\S]*?(?=\\nParte 2\\. )", t, perl = TRUE))
  b <- regmatches(t, regexpr("\\*\\*Parte 1 · [\\s\\S]*?(?=\\*\\*Parte 2 · )", t, perl = TRUE))
  c <- regmatches(t, regexpr("En la Parte 1, [^\\n]*", t, perl = TRUE))
  if (length(a) != 1 || length(b) != 1 || length(c) != 1) stop("no se pudo aislar la prosa de la Parte 1")
  c(proc = a, dist = b, caso = c)
}
f <- list(seed = 0L, propios = character(0), propios_norm = character(0), e1 = character(0), sol = 0L, h3 = 0L, veredicto6 = 0L, err = character(0))
c_p1 <- c(uni = 0L, sig = 0L, esp = 0L, dentro = 0L, dentro_sig = 0L); c_resto <- c(uni = 0L, sig = 0L, esp = 0L)
c_viejo_p1 <- c(uni = 0L, esp = 0L); n_can <- 0L; obj_dif <- integer(0); n_h3_v <- c(hp = 0L, hg = 0L)
for (s in 1:1000) {
  N <- correr(EX_N, s); V <- correr(EX_V, s); S <- correr(EX_S, s); P <- correr(EX_P1, s)
  if (!is.null(N$err) || !is.null(V$err) || !is.null(S$err) || !is.null(P$err)) { f$err <- c(f$err, sprintf("s=%d %s|%s|%s|%s", s, format(N$err), format(V$err), format(S$err), format(P$err))); next }
  n_can <- n_can + isTRUE(N$en$es_canonica)
  if (!identical(N$seed, V$seed)) f$seed <- f$seed + 1L
  ## (c) E1 ampliado contra el SCHOICE 63e2713e
  REN <- c(opciones = "opciones_p1", sol = "sol_p1", feedback = "feedback_p1")
  nS <- setdiff(ls(S$en), names(REN)); nP <- setdiff(ls(P$en), REN)
  if (!setequal(nS, nP)) f$e1 <- c(f$e1, sprintf("s=%d objetos distintos", s))
  for (k in intersect(nS, nP)) if (!ig(S$en[[k]], P$en[[k]])) f$e1 <- c(f$e1, sprintf("s=%d %s", s, k))
  for (k in names(REN)) if (!ig(S$en[[k]], P$en[[REN[[k]]]])) f$e1 <- c(f$e1, sprintf("s=%d %s", s, REN[[k]]))
  if (!identical(S$seed, P$seed)) f$e1 <- c(f$e1, sprintf("s=%d .Random.seed al final del bloque P1", s))
  ## (b) objetos propios del CLOZE (los que el bloque P1 no define)
  propios <- setdiff(ls(N$en), ls(P$en))
  if (!setequal(propios, setdiff(ls(V$en), ls(P$en)) )) f$propios <- c(f$propios, sprintf("s=%d conjunto de objetos propios distinto", s))
  for (k in propios) if (!ig(N$en[[k]], V$en[[k]])) {
    obj_dif[k] <- (if (is.na(obj_dif[k])) 0L else obj_dif[k]) + 1L
    nn <- rapply(list(N$en[[k]]), function(z) gsub(NB, " ", z, fixed = TRUE), classes = "character", how = "replace")[[1]]
    vv <- rapply(list(V$en[[k]]), function(z) gsub(NB, " ", z, fixed = TRUE), classes = "character", how = "replace")[[1]]
    if (!ig(nn, vv)) f$propios_norm <- c(f$propios_norm, sprintf("s=%d %s", s, k))
  }
  ## Veredictos y clave de la Parte 6 (H3 no los toca)
  if (!ig(N$en$sol_p6, V$en$sol_p6) || !ig(N$en$opciones_p6, V$en$opciones_p6)) f$veredicto6 <- f$veredicto6 + 1L
  ## (d) Solution emitida
  tN <- emite(SOL_N, N$en); tV <- emite(SOL_V, V$en)
  if (!identical(deshacer(tN), gsub(NB, " ", tV, fixed = TRUE))) f$sol <- f$sol + 1L
  p1 <- paste(p1_de_sol(tN), collapse = "\n"); resto <- tN
  for (q in p1_de_sol(tN)) resto <- sub(q, "", resto, fixed = TRUE)
  resto <- sub("- Parte 1\\. La respuesta correcta[^\\n]*", "", resto, perl = TRUE)
  c_p1 <- c_p1 + c(cuenta(RE_UNI_FUERA, p1), cuenta(RE_SIG_FUERA, p1), cuenta(RE_ESP_NORMAL, p1),
                   cuenta("\\\\text\\{(kilos?|cursos?)[.,:;!?]?\\}\\$", p1), cuenta("\\\\text\\{(kilos?|cursos?)[.,:;!?]\\}\\$", p1))
  c_resto <- c_resto + c(cuenta(RE_UNI_FUERA, resto), cuenta(RE_SIG_FUERA, resto), cuenta(RE_ESP_NORMAL, resto))
  pv <- paste(p1_de_sol(tV), collapse = "\n"); c_viejo_p1 <- c_viejo_p1 + c(cuenta(RE_UNI_FUERA, pv), cuenta(RE_ESP_NORMAL, pv))
  ## (e) H3: leído del texto emitido
  m <- regmatches(tN, regexec(paste0("el grado (\\S+) habría ganado con un curso más si y solo si su promedio por curso, ([0-9]+)", NB,
                                     "kilos, es mayor que la diferencia, ([0-9]+)", NB, "kilos; y el grado (\\S+) habría perdido con un ",
                                     "curso menos si y solo si su promedio, ([0-9]+)", NB, "kilos, es mayor que esa diferencia\\."), tN))[[1]]
  if (length(m) != 6L) { f$h3 <- f$h3 + 1L; next }
  aP <- as.numeric(m[3]); D <- as.numeric(m[4]); aG <- as.numeric(m[6])
  r6 <- N$en$rol_p6; s6 <- N$en$sol_p6
  ok_h3 <- s6[r6 == "hp"] == (aP > D) && s6[r6 == "hg"] == (aG > D) && m[2] == N$en$per_par && m[5] == N$en$gan_par && aP != D && aG != D
  if (!isTRUE(ok_h3)) f$h3 <- f$h3 + 1L
  n_h3_v <- n_h3_v + c(hp = s6[r6 == "hp"], hg = s6[r6 == "hg"])
}
cat(sprintf("Semillas 1 a 1000 (%d canónicas); errores de ejecución: %d\n", n_can, length(f$err))); if (length(f$err)) print(head(f$err))
cat(sprintf("(a) .Random.seed final distinto de f98f9dda: %d de 1000\n", f$seed))
cat(sprintf("(b) objetos propios del CLOZE distintos de f98f9dda: %s; tras normalizar el espacio duro: %d diferencias\n",
            if (length(obj_dif)) paste(sprintf("%s (%d)", names(obj_dif), obj_dif), collapse = ", ") else "ninguno", length(f$propios_norm)))
if (length(f$propios_norm)) print(head(f$propios_norm, 10)); if (length(f$propios)) print(head(f$propios))
cat(sprintf("(c) E1 ampliado (Parte 1 = SCHOICE 63e2713e, objetos y .Random.seed): %d diferencias\n", length(f$e1))); if (length(f$e1)) print(head(f$e1, 10))
cat(sprintf("    veredictos u opciones de la Parte 6 distintos de f98f9dda: %d de 1000\n", f$veredicto6))
cat(sprintf("(d) Solution distinta de f98f9dda tras deshacer L-6 y H3: %d de 1000\n", f$sol))
cat(sprintf("    prosa de la Parte 1: unidad fuera de la fórmula %d · signo fuera de una fórmula %d · cifra-unidad o El/La con espacio normal %d · unidades dentro %d (con el signo dentro %d)\n",
            c_p1[["uni"]], c_p1[["sig"]], c_p1[["esp"]], c_p1[["dentro"]], c_p1[["dentro_sig"]]))
cat(sprintf("    control, la misma prosa en f98f9dda: unidad fuera de la fórmula %d · cifra-unidad o El/La con espacio normal %d\n", c_viejo_p1[["uni"]], c_viejo_p1[["esp"]]))
cat(sprintf("    resto de la Solution (Partes 2 a 6, sin tocar): unidad fuera de la fórmula %d · signo fuera %d · espacio normal %d\n",
            c_resto[["uni"]], c_resto[["sig"]], c_resto[["esp"]]))
cat(sprintf("(e) H3: marca de HP y HG = «si y solo si \u2026 es mayor que la diferencia\u00bb, leído del texto: %d discrepancias en 1000 (HP verdadera en %d, HG en %d)\n",
            f$h3, n_h3_v[["hp"]], n_h3_v[["hg"]]))
