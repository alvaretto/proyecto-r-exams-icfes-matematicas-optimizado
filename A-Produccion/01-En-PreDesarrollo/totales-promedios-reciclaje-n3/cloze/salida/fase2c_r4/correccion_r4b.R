## FASE 2C r4b (detractor nuevo): CORRECCIÓN sobre el texto EMITIDO por R/exams (xexams), con funciones de verdad propias.
suppressMessages(library(exams))
RMD <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"
md5 <- unname(tools::md5sum(RMD)); cat("md5", md5, "\n"); stopifnot(md5 == "27f789dc6327f7d6bda72cded61502cd")
nb <- function(s) gsub(" ", " ", s, fixed = TRUE)
lin <- readLines(RMD, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", lin)[1]; i1 <- which(lin == "```" & seq_along(lin) > i0)[1]
EXPR <- parse(text = paste(lin[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8")
dg <- function(s) { set.seed(s); en <- new.env(parent = globalenv()); suppressMessages(eval(EXPR, envir = en)); en }
can100 <- sum(vapply(1:100, function(s) isTRUE(dg(s)$es_canonica), NA)); extra <- integer(0); s <- 100L
while (can100 + length(extra) < 20L && s < 4000L) { s <- s + 1L; if (isTRUE(dg(s)$es_canonica)) extra <- c(extra, s) }
SEEDS <- c(1:100, extra); cat("canónicas en 1:100:", can100, "· extra:", paste(extra, collapse = ","), "\n")
CAN_ENUN <- "En un colegio se realiza un concurso de reciclaje que premiará a la sede que recoja la mayor cantidad de papel. La tabla muestra la cantidad de cursos que participaron y el promedio de papel recogido por curso en la sede primaria y en la sede secundaria."
CAN_PREG <- "De acuerdo con la información anterior, ¿cuál de las siguientes afirmaciones es VERDADERA?"
CAN_HDR <- c("Sede", "Cantidad de cursos", "Promedio de papel recolectado por curso")
CAN_ROWS <- c("Primaria|5 cursos|40 kilos", "Secundaria|4 cursos|45 kilos")
CAN_OP <- c(A = "La sede primaria ganó el concurso porque recogió 40 kilos más que la sede de secundaria.",
  B = "La sede primaria ganó el concurso porque participaron más cursos y aunque el promedio fue menor, el total fue de 20 kilos más.",
  C = "La sede secundaria ganó porque, aunque participaron menos cursos, tuvo mayor promedio con 5 kilos de más.",
  D = "La sede secundaria ganó el concurso porque en ella hubo un curso que recogió más que los otros, aumentando la cantidad de papel recogida en todo el colegio.")
tablas <- function(qtxt) {
  L <- strsplit(qtxt, "\n", fixed = TRUE)[[1]]; r <- grep("^\\|", L); g <- cumsum(c(1, diff(r) != 1))
  lapply(split(r, g), function(ix) { f <- L[ix]; f <- f[!grepl("^\\|[-:| ]+\\|$", f)]
    cel <- lapply(f, function(z) trimws(nb(gsub("<[^>]*>|\\*\\*", "", strsplit(sub("^\\|", "", sub("\\|$", "", z)), "|", fixed = TRUE)[[1]]))))
    list(hdr = cel[[1]], rows = vapply(cel[-1], paste, "", collapse = "|"),
         df = data.frame(nombre = vapply(cel[-1], `[`, "", 1), n = as.numeric(sub(" cursos?$", "", vapply(cel[-1], `[`, "", 2))),
                         a = as.numeric(sub(" kilos?$", "", vapply(cel[-1], `[`, "", 3))), stringsAsFactors = FALSE)) })
}
fila_de <- function(subj, df) { i <- which(endsWith(tolower(subj), tolower(df$nombre))); if (length(i) == 1L) i else NA }
v1 <- function(o, df) {   # Parte 1: verdad de la afirmación leída del texto (lectura «diferencia») y, aparte, la lectura «total»
  o <- nb(o); TT <- df$n * df$a; S <- fila_de(sub(" gan[óo] .*$", "", o), df); if (is.na(S)) return(c(v = NA, tot = NA))
  O <- 3L - S; gana <- TT[S] > TT[O]; per <- TRUE; rec <- FALSE
  if (grepl("participaron más cursos", o, fixed = TRUE) && grepl("promedio fue menor", o, fixed = TRUE)) { per <- df$n[S] > df$n[O] && df$a[S] < df$a[O]; rec <- TRUE }
  if (grepl("participaron menos cursos", o, fixed = TRUE) && grepl("mayor promedio", o, fixed = TRUE)) { per <- df$n[S] < df$n[O] && df$a[S] > df$a[O]; rec <- TRUE }
  neutro <- grepl("todos y cada uno de sus cursos|juntar lo que recogieron todos sus cursos", o)
  sinsus <- grepl("cursos más grandes|hubo un curso que recogió más que los otros|hay más alumnos|durante más días", o)
  cifra_sola <- grepl("recogió [0-9]+ kilos más que", o); if (!(rec || neutro || sinsus || cifra_sola)) return(c(v = NA, tot = NA))
  nums <- as.numeric(regmatches(o, gregexpr("[0-9]+", o))[[1]])
  if (grepl("su total fue más alto que el de .+ en [0-9]+ kilos\\.$", o)) {
    x <- nums[length(nums)]; if (!identical(fila_de(sub("^.*que el de (.+) en [0-9]+ kilos\\.$", "\\1", o), df), O)) return(c(v = NA, tot = NA))
    cif <- TT[S] - TT[O] == x; tot <- TT[S] == x
  } else if (grepl("recogió [0-9]+ kilos más que", o)) { cif <- TT[S] - TT[O] == nums[1]; tot <- FALSE
  } else if (grepl("el total fue de [0-9]+ kilos más", o)) { cif <- TT[S] - TT[O] == nums[1]; tot <- TT[S] == nums[1]
  } else if (grepl("mayor promedio con [0-9]+ kilos de más", o)) { cif <- df$a[S] - df$a[O] == nums[1]; tot <- FALSE
  } else if (!length(nums)) { cif <- TRUE; tot <- TRUE } else return(c(v = NA, tot = NA))
  c(v = gana && per && !sinsus && cif, tot = gana && per && !sinsus && tot)
}
P2T <- c("Lo que recogió ese grado dividido en partes iguales entre sus cursos." = "clave",
         "Lo que recogió ese grado dividido entre el número de sus cursos." = "clave",
         "Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado." = "clave",
         "Lo que recogió ese grado dividido entre el promedio de sus cursos." = "entre_promedio",
         "Lo que recogió ese grado dividido en partes iguales entre sus cursos y luego entre dos." = "luego_dos",
         "Lo que recogió ese grado dividido entre el número de sus cursos al cuadrado." = "cuadrado",
         "Lo que recogió ese grado dividido entre la cantidad de cursos de ambos grados." = "ambos",
         "Lo que recogió la mayoría de los cursos de ese mismo grado." = "moda")
v2 <- function(o, df) { k <- unname(P2T[nb(o)]); if (is.na(k)) return(NA)
  all(vapply(1:2, function(i) { n <- df$n[i]; a <- df$a[i]; T <- n * a; n2 <- df$n[3L - i]
    switch(k, clave = T / n == a, entre_promedio = T / a == a, luego_dos = (T / n / 2 == a) || (T / (n / 2) == a),
           cuadrado = (T / n^2 == a) || ((T / n)^2 == a), ambos = T / (n + n2) == a, moda = FALSE) }, NA)) }
v6 <- function(t, df) { t <- nb(t); TT <- df$n * df$a; W <- which.max(TT); Lo <- 3L - W
  if (grepl("^Ganó el concurso el grado ", t)) return(sub("^Ganó el concurso el grado (.*)\\.$", "\\1", t) == tolower(df$nombre[W]))
  s <- if (grepl("^Si (el|al) grado que perdió", t)) Lo else if (grepl("^Si (el|al) grado que ganó", t)) W else return(NA); o <- 3L - s
  n <- df$n; a <- df$a
  if (grepl("con un curso más, conservando su promedio por curso", t, fixed = TRUE)) n[s] <- n[s] + 1 else
  if (grepl("le hubiera faltado un curso, conservando su promedio por curso", t, fixed = TRUE)) n[s] <- n[s] - 1 else
  if (grepl(paste0("con tantos cursos como el que ", if (o == W) "ganó" else "perdió", ", conservando su promedio"), t)) n[s] <- n[o] else
  if (grepl(paste0("el mismo promedio por curso que el que ", if (o == W) "ganó" else "perdió", ", conservando su cantidad de cursos"), t)) a[s] <- a[o] else return(NA)
  T2 <- n * a
  if (grepl("habría ganado el concurso\\.$", t)) return(T2[s] > T2[o]); if (grepl("habría perdido el concurso\\.$", t)) return(T2[s] < T2[o]); NA }
arit <- function(txt) { txt <- nb(paste(txt, collapse = "\n")); bad <- 0L; nn <- 0L
  for (op in c("\\\\times", "-", "\\+")) { m <- regmatches(txt, gregexpr(paste0("([0-9]+) ", op, " ([0-9]+) = ([0-9]+)"), txt))[[1]]
    for (e in m) { v <- as.numeric(regmatches(e, gregexpr("[0-9]+", e))[[1]]); nn <- nn + 1L
      r <- if (op == "-") v[1] - v[2] else if (op == "\\+") v[1] + v[2] else v[1] * v[2]; if (r != v[3]) bad <- bad + 1L } }
  c(n = nn, mal = bad) }
F <- list(); add <- function(s, m) F[[length(F) + 1L]] <<- sprintf("s=%d %s", s, m)
cont <- c(ver = 0, can = 0, tot_alt = 0, h3_falsa_emb = 0, arit_n = 0, arit_mal = 0, mismo_que_E1 = 0)
for (s in SEEDS) {
  ex <- tryCatch(xexams(RMD, n = 1L, driver = list(sweave = list(quiet = TRUE, encoding = "UTF-8"), read = NULL, transform = NULL, write = NULL),
                        dir = tempdir(), seed = s)[[1]][[1]], error = function(e) e)
  if (inherits(ex, "error")) { add(s, paste("xexams:", conditionMessage(ex))); next }
  cont["ver"] <- cont["ver"] + 1; mi <- ex$metainfo; ms <- mi$solution; ql <- ex$questionlist; sl <- ex$solutionlist
  if (s == 1L) { cat("clozetype:", mi$clozetype, "· lengths sol:", lengths(ms), "· ql:", length(ql), "· sl:", length(sl), "· exsolution:", mi$string, "\n") }
  if (!identical(mi$clozetype, c("schoice", "schoice", "num", "num", "num", "mchoice"))) add(s, "clozetype")
  qtxt <- paste(ex$question, collapse = "\n"); TB <- tablas(qtxt); if (length(TB) != 2L) { add(s, "no hay dos tablas"); next }
  d1 <- TB[[1]]$df; d2 <- TB[[2]]$df
  ## ##ANSWERi## en orden, uno por gap
  an <- as.integer(regmatches(qtxt, gregexpr("(?<=##ANSWER)[0-9]+(?=##)", qtxt, perl = TRUE))[[1]]); if (!identical(an, 1:6)) add(s, paste("ANSWER:", paste(an, collapse = ",")))
  lens <- lengths(ms); ch <- which(mi$clozetype %in% c("schoice", "mchoice"))
  seg <- if (length(ql) == sum(lens)) split(seq_along(ql), rep(seq_along(lens), lens)) else { z <- split(seq_along(ql), rep(seq_along(ch), lens[ch])); w <- vector("list", 6); w[ch] <- z; w }
  o1 <- ql[seg[[1]]]; o2 <- ql[seg[[2]]]; o6 <- ql[seg[[6]]]
  en <- dg(s); if (setequal(nb(o1), nb(en$opciones_p1)) && identical(tolower(TB[[2]]$df$nombre), en$nom_grado) && isTRUE(all(ms[[3]] == en$exsol_p3, ms[[4]] == en$exsol_p4, ms[[5]] == en$exsol_p5))) cont["mismo_que_E1"] <- cont["mismo_que_E1"] + 1
  ## Parte 1
  V1 <- t(vapply(o1, v1, c(v = NA, tot = NA), df = d1)); if (anyNA(V1[, "v"])) add(s, paste("P1 texto no juzgado:", paste(nb(o1[is.na(V1[, "v"])]), collapse = " | ")))
  else { if (sum(V1[, "v"]) != 1L || !identical(unname(as.logical(V1[, "v"])), unname(ms[[1]]))) add(s, "P1 clave != única verdadera")
         if (any(V1[, "tot"] & !V1[, "v"])) cont["tot_alt"] <- cont["tot_alt"] + 1 }
  es_can <- grepl(CAN_ENUN, nb(qtxt), fixed = TRUE)
  if (es_can) { cont["can"] <- cont["can"] + 1
    if (!identical(TB[[1]]$hdr, CAN_HDR) || !identical(TB[[1]]$rows, CAN_ROWS)) add(s, "canónica: tabla != impreso")
    if (!grepl(paste0("**Parte 1.** ", CAN_PREG), nb(qtxt), fixed = TRUE)) add(s, "canónica: pregunta")
    if (!setequal(nb(o1), CAN_OP) || nb(o1[ms[[1]]]) != CAN_OP[["B"]]) add(s, "canónica: opciones o clave != impreso") }
  ## Parte 2
  V2 <- vapply(o2, v2, NA, df = d2); if (anyNA(V2)) add(s, "P2 texto no juzgado") else if (sum(V2) != 1L || !identical(unname(V2), unname(ms[[2]]))) add(s, "P2 clave")
  ## Partes 3 a 5
  pq <- regmatches(nb(qtxt), gregexpr("\\*\\*Parte [3-5]\\.\\*\\* [^\n]*", nb(qtxt)))[[1]]; TT2 <- d2$n * d2$a
  g3 <- sub("^.*el grado (.+)\\?$", "\\1", pq[1]); g4 <- sub("^.*el grado (.+)\\?$", "\\1", pq[2])
  if (!isTRUE(TT2[match(g3, tolower(d2$nombre))] == ms[[3]]) || !isTRUE(TT2[match(g4, tolower(d2$nombre))] == ms[[4]]) || g3 == g4) add(s, "P3/P4")
  if (!isTRUE(abs(diff(TT2)) == ms[[5]])) add(s, "P5")
  ## Parte 6
  V6 <- vapply(o6, v6, NA, df = d2); if (anyNA(V6)) add(s, paste("P6 texto no juzgado:", paste(nb(o6[is.na(V6)]), collapse = " | "))) else
    if (!identical(unname(V6), unname(ms[[6]])) || all(V6) || !any(V6)) add(s, "P6 marcas")
  ## Solution
  sol <- nb(paste(ex$solution, collapse = "\n")); W <- which.max(TT2); Lo <- 3L - W; T1 <- d1$n * d1$a; G1 <- which.max(T1)
  r1 <- sub("^.*- Parte 1\\. La respuesta correcta es la que afirma: «([^»]*)».*$", "\\1", sol); if (r1 != nb(o1[ms[[1]]])) add(s, "Sol P1 respuesta")
  r2 <- sub("^.*- Parte 2\\. «([^»]*)».*$", "\\1", sol); if (r2 != nb(o2[ms[[2]]])) add(s, "Sol P2 respuesta")
  if (!grepl(sprintf("- Parte 3. %d kilos, el total del grado %s.", ms[[3]], g3), sol, fixed = TRUE) ||
      !grepl(sprintf("- Parte 4. %d kilos, el total del grado %s.", ms[[4]], g4), sol, fixed = TRUE) ||
      !grepl(sprintf("- Parte 5. %d kilos.", ms[[5]]), sol, fixed = TRUE)) add(s, "Sol P3-5")
  bv <- sub("^.*- Parte 6\\. Son verdaderas:\n(.*?)\n- Parte 6\\. Son falsas:\n(.*?)\n\n.*$", "\\1", sol)
  bf <- sub("^.*- Parte 6\\. Son verdaderas:\n(.*?)\n- Parte 6\\. Son falsas:\n(.*?)\n\n.*$", "\\2", sol)
  lv <- sub("^ *- «(.*)»$", "\\1", strsplit(bv, "\n")[[1]]); lf <- sub("^ *- «(.*)»$", "\\1", strsplit(bf, "\n")[[1]])
  if (!setequal(lv, nb(o6[V6])) || !setequal(lf, nb(o6[!V6]))) add(s, "Sol P6 listas")
  m <- regmatches(sol, regexec("Ganó (.+), con \\$([0-9]+) - ([0-9]+) = ([0-9]+)\\\\ \\\\text\\{kilos\\}\\$ más\\. (.+?), y eso pesó más\\.", sol))[[1]]
  if (!length(m) || !endsWith(tolower(m[2]), tolower(d1$nombre[G1])) || as.numeric(m[3]) != max(T1) || as.numeric(m[4]) != min(T1)) add(s, "Sol P1 Ganó")
  else { mas_cursos <- d1$n[G1] > d1$n[3L - G1]; frase <- m[6]
         if (mas_cursos != grepl("^Su promedio era menor", frase)) add(s, "Sol P1 frase de rama") }
  if (!grepl(sprintf("solo una cita la diferencia correcta, %d kilos.", max(T1) - min(T1)), sol, fixed = TRUE)) add(s, "Sol P1 «solo una cita»")
  if (sum(vapply(o1, function(o) { S <- fila_de(sub(" gan[óo] .*$", "", nb(o)), d1); x <- as.numeric(regmatches(nb(o), gregexpr("[0-9]+", nb(o)))[[1]])
        isTRUE(S == G1) && length(x) >= 1L && (max(T1) - min(T1)) %in% x }, NA)) != 1L) add(s, "Sol P1: no es una sola de G con la cifra D")
  h3 <- regmatches(sol, regexec("Por eso el grado (\\S+) habría ganado con un curso más si y solo si su promedio por curso, ([0-9]+) kilos, es mayor que la diferencia, ([0-9]+) kilos; y el grado (\\S+) habría perdido con un curso menos si y solo si su promedio, ([0-9]+) kilos, es mayor que esa diferencia\\.", sol))[[1]]
  if (!length(h3)) add(s, "H3 no encontrado") else {
    X <- match(h3[2], tolower(d2$nombre)); Y <- match(h3[5], tolower(d2$nombre)); A <- as.numeric(h3[3]); Dd <- as.numeric(h3[4]); B <- as.numeric(h3[6])
    okh <- isTRUE(X == Lo) && isTRUE(Y == W) && A == d2$a[Lo] && B == d2$a[W] && Dd == abs(diff(TT2)) &&
      ((TT2[Lo] + A > TT2[W]) == (A > Dd)) && ((TT2[W] - B < TT2[Lo]) == (B > Dd)) && A != Dd && B != Dd
    if (!okh) add(s, "H3 falso o con cifras ajenas"); if (!(A > Dd) || !(B > Dd)) cont["h3_falsa_emb"] <- cont["h3_falsa_emb"] + 1 }
  a <- arit(c(ex$solution, sl)); cont["arit_n"] <- cont["arit_n"] + a[["n"]]; cont["arit_mal"] <- cont["arit_mal"] + a[["mal"]]; if (a[["mal"]] > 0) add(s, "aritmética falsa en Solution/feedback")
  ## feedback: prefijo Verdadero/Falso alineado con la marca en los gaps de elección (si sl tiene una entrada por elemento de la solución)
  if (length(sl) == sum(lens)) { sg <- split(seq_along(sl), rep(seq_along(lens), lens))
    for (k in ch) { pre <- startsWith(nb(sl[sg[[k]]]), "Verdadero."); if (!identical(unname(pre), unname(ms[[k]]))) add(s, paste("feedback parte", k)) } }
  else if (s == 1L) cat("solutionlist con", length(sl), "entradas y solution con", sum(lens), "\n")
}
cat("\nResumen:", paste(names(cont), cont, sep = "=", collapse = " · "), "\n")
cat("Fallos:", length(F), "\n"); for (z in head(unlist(F), 40)) cat("  ", z, "\n")
