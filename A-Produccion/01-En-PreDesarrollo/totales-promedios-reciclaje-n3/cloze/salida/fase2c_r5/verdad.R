## FASE 2C r5 — funciones de verdad PROPIAS sobre el texto emitido (xexams/read_exercise) del CLOZE 6f6f1b13
r <- readRDS(path.expand("~/.cache/fase2c_r5_cloze/emit.rds")); N <- r$N; V <- r$V
nb <- function(x) gsub(" ", " ", x)
strip <- function(x) gsub("<span[^>]*>|</span>|\\*\\*", "", x)
tablas <- function(q) {
  L <- strsplit(paste(q, collapse = "\n"), "\n")[[1]]; idx <- grep("^\\|", L)
  bl <- split(idx, cumsum(c(1, diff(idx) != 1)))
  lapply(bl, function(b) { rw <- L[b]; rw <- rw[!grepl("^\\|[-:| ]+\\|$", rw)]
    ce <- lapply(rw, function(z) trimws(nb(strip(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", z)), "|", fixed = TRUE)[[1]]))))
    dat <- ce[-1]
    df <- data.frame(nombre = vapply(dat, `[`, "", 1), n = as.numeric(sub(" cursos?$", "", vapply(dat, `[`, "", 2))),
                     a = as.numeric(sub(" kilos?$", "", vapply(dat, `[`, "", 3))), stringsAsFactors = FALSE)
    attr(df, "hdr") <- ce[[1]]; df })
}
cnt <- new.env(); inc <- function(k, v = 1) assign(k, (if (exists(k, envir = cnt, inherits = FALSE)) get(k, envir = cnt) else 0) + v, envir = cnt)
fallos <- character(0); F <- function(s, ...) fallos <<- c(fallos, sprintf("s=%s %s", s, paste0(...)))
RE_CMP <- "([0-9]+) kilos[^0-9.:;]*?\\b(más|menos|mayor|menor) que [^0-9.:;]*?([0-9]+) kilos"
esq <- character(0); can_ok <- 0; n_can <- 0
P2_CLAVE <- "^Lo que recogió ese grado dividido (en partes iguales entre sus cursos|entre el número de sus cursos|entre la cantidad de cursos de ese mismo grado)\\.$"
for (s in names(N)) {
  e <- N[[s]]; q <- paste(e$question, collapse = "\n"); sol <- strsplit(paste(e$solution, collapse = "\n"), "\n")[[1]]
  ql <- e$questionlist; sl <- e$solutionlist; mi <- e$metainfo; X <- mi$solution
  ## ##ANSWERi## y exsolution
  an <- as.integer(regmatches(q, gregexpr("(?<=##ANSWER)[0-9]+(?=##)", q, perl = TRUE))[[1]])
  if (!identical(an, 1:6)) F(s, "ANSWER: ", paste(an, collapse = ","))
  if (!identical(mi$clozetype, c("schoice", "schoice", "num", "num", "num", "mchoice")) || !identical(as.numeric(mi$points), c(3, 1, 1, 1, 1, 1)) ||
      length(ql) != 15L || length(sl) != 15L || any(ql[9:11] != "")) F(s, "estructura gaps")
  tb <- tablas(e$question); if (length(tb) != 2L) { F(s, "tablas ", length(tb)); next }
  t1 <- tb[[1]]; t2 <- tb[[2]]
  ## ---------- Parte 1 ----------
  T1 <- t1$n * t1$a; w <- which.max(T1); l <- 3L - w; D1 <- T1[w] - T1[l]; nm1 <- tolower(t1$nombre)
  if (T1[1] == T1[2]) F(s, "P1 empate")
  rama1 <- if (t1$n[w] > t1$n[l]) "CURSOS" else "PROMEDIO"
  op1 <- ql[1:4]
  v1 <- vapply(op1, function(o) { lo <- tolower(nb(o)); pos <- vapply(nm1, function(z) { p <- regexpr(z, lo, fixed = TRUE); if (p < 0) Inf else p }, 0)
    suj <- which.min(pos); if (!is.finite(pos[suj])) return(NA)
    if (suj == l) return(FALSE)
    nums <- as.numeric(regmatches(lo, gregexpr("[0-9]+", lo))[[1]])
    if (length(nums) != 1L || nums != D1) return(FALSE)
    if (grepl("más cursos", lo) && !(t1$n[w] > t1$n[l])) return(FALSE)
    if (grepl("menos cursos", lo) && !(t1$n[w] < t1$n[l])) return(FALSE)
    if (grepl("promedio fue menor|menor promedio", lo) && !(t1$a[w] < t1$a[l])) return(FALSE)
    if (grepl("promedio fue mayor|mayor promedio|promedio más alto", lo) && !(t1$a[w] > t1$a[l])) return(FALSE)
    if (grepl("un curso que recogió", lo)) return(FALSE)
    TRUE }, NA)
  if (anyNA(v1)) F(s, "P1 sujeto no hallado")
  if (sum(v1, na.rm = TRUE) != 1L || !identical(unname(v1), X[[1]])) F(s, "P1 clave: verdad ", paste(v1, collapse = ","), " marca ", paste(X[[1]], collapse = ","))
  for (i in 1:4) { lo <- tolower(nb(op1[i])); k <- lo
    for (j in 1:2) k <- gsub(nm1[j], if (j == w) "<W>" else "<L>", k, fixed = TRUE)
    k <- gsub("[0-9]+", "<N>", k); esq <- c(esq, sprintf("%s | v=%s | %s", rama1, v1[i], k))
    pre <- sub("\\..*$", ".", sl[i]); if (!identical(pre == "Verdadero.", isTRUE(v1[i]))) F(s, "P1 feedback ", i, " ", pre) }
  sujetos <- vapply(op1, function(o) { lo <- tolower(nb(o)); which.min(vapply(nm1, function(z) { p <- regexpr(z, lo, fixed = TRUE); if (p < 0) Inf else p }, 0)) }, 0L)
  citaD <- vapply(op1, function(o) D1 %in% as.numeric(regmatches(nb(o), gregexpr("[0-9]+", nb(o)))[[1]]), NA)
  if (sum(sujetos == l) != 2L || sum(sujetos == w) != 2L || sum(citaD & sujetos == w) != 1L) F(s, "P1 Solution «dos y dos / solo una cita D»")
  if (!any(grepl(paste0("La respuesta correcta es la que afirma: «", op1[which(v1)[1]], "»"), sol, fixed = TRUE))) F(s, "P1 Respuestas")
  ## canónica (impreso, H-2)
  if (all(sort(nm1) == c("primaria", "secundaria")) && identical(t1$n[match(c("primaria", "secundaria"), nm1)], c(5, 4)) && identical(t1$a[match(c("primaria", "secundaria"), nm1)], c(40, 45))) {
    n_can <- n_can + 1
    imp <- c("La sede primaria ganó el concurso porque recogió 40 kilos más que la sede de secundaria.",
             "La sede primaria ganó el concurso porque participaron más cursos y aunque el promedio fue menor, el total fue de 20 kilos más.",
             "La sede secundaria ganó porque, aunque participaron menos cursos, tuvo mayor promedio con 5 kilos de más.",
             "La sede secundaria ganó el concurso porque en ella hubo un curso que recogió más que los otros, aumentando la cantidad de papel recogida en todo el colegio.")
    enun <- "En un colegio se realiza un concurso de reciclaje que premiará a la sede que recoja la mayor cantidad de papel. La tabla muestra la cantidad de cursos que participaron y el promedio de papel recogido por curso en la sede primaria y en la sede secundaria."
    okc <- setequal(nb(op1), imp) && nb(op1[X[[1]]]) == imp[2] && grepl(enun, nb(q), fixed = TRUE) &&
      grepl("De acuerdo con la información anterior, ¿cuál de las siguientes afirmaciones es VERDADERA?", q, fixed = TRUE) &&
      identical(attr(t1, "hdr"), c("Sede", "Cantidad de cursos", "Promedio de papel recolectado por curso")) && identical(t1$nombre, c("Primaria", "Secundaria"))
    if (okc) can_ok <- can_ok + 1 else F(s, "CANÓNICA distinta del impreso")
  }
  ## ---------- Parte 2 ----------
  v2 <- grepl(P2_CLAVE, ql[5:8])
  if (sum(v2) != 1L || !identical(v2, X[[2]])) F(s, "P2 clave")
  for (i in 5:8) { pre <- sub("\\..*$", ".", sl[i]); if (!identical(pre == "Verdadero.", v2[i - 4])) F(s, "P2 feedback ", i) }
  if (!any(grepl(paste0("- Parte 2. «", ql[4 + which(v2)], "»"), sol, fixed = TRUE))) F(s, "P2 Respuestas")
  ## ---------- Partes 3 a 5 ----------
  T2 <- t2$n * t2$a; w2 <- which.max(T2); l2 <- 3L - w2; D2 <- T2[w2] - T2[l2]; nm2 <- tolower(t2$nombre)
  if (T2[1] == T2[2]) F(s, "P2-6 empate de totales")
  g3 <- tolower(sub(".*el grado (\\S+)\\?.*", "\\1", regmatches(q, regexpr("\\*\\*Parte 3\\.\\*\\*[^\n]*", q))))
  g4 <- tolower(sub(".*el grado (\\S+)\\?.*", "\\1", regmatches(q, regexpr("\\*\\*Parte 4\\.\\*\\*[^\n]*", q))))
  if (X[[3]] != T2[match(g3, nm2)] || X[[4]] != T2[match(g4, nm2)] || X[[5]] != D2 || g3 == g4) F(s, "P3-5 valores")
  if (!any(grepl(paste0("- Parte 3. ", X[[3]], " kilos, el total del grado ", g3, "."), sol, fixed = TRUE)) ||
      !any(grepl(paste0("- Parte 5. ", D2, " kilos."), sol, fixed = TRUE))) F(s, "P3/P5 Respuestas")
  ## ---------- Parte 6: afirmaciones ----------
  nW <- t2$n[w2]; aW <- t2$a[w2]; nL <- t2$n[l2]; aL <- t2$a[l2]; TW <- T2[w2]; TL <- T2[l2]
  verdad6 <- function(t) switch(t,
    "Si el grado que perdió hubiera participado con un curso más, conservando su promedio por curso, habría ganado el concurso." = (nL + 1) * aL > TW,
    "Si al grado que ganó le hubiera faltado un curso, conservando su promedio por curso, habría perdido el concurso." = (nW - 1) * aW < TL,
    "Si el grado que perdió hubiera participado con tantos cursos como el que ganó, conservando su promedio por curso, habría ganado el concurso." = nW * aL > TW,
    "Si el grado que ganó hubiera participado con tantos cursos como el que perdió, conservando su promedio por curso, habría perdido el concurso." = nL * aW < TL,
    "Si el grado que perdió hubiera tenido el mismo promedio por curso que el que ganó, conservando su cantidad de cursos, habría ganado el concurso." = nL * aW > TW,
    "Si el grado que ganó hubiera tenido el mismo promedio por curso que el que perdió, conservando su cantidad de cursos, habría perdido el concurso." = nW * aL < TL,
    if (grepl("^Ganó el concurso el grado (\\S+)\\.$", t)) tolower(sub("^Ganó el concurso el grado (\\S+)\\.$", "\\1", t)) == nm2[w2] else NA)
  emp <- c((nL + 1) * aL == TW, (nW - 1) * aW == TL, nW * aL == TW, nL * aW == TL, nL * aW == TW, nW * aL == TL)
  if (any(emp)) inc("p6_empates_contrafactuales")
  v6 <- vapply(ql[12:15], verdad6, NA)
  if (anyNA(v6) || !identical(unname(v6), X[[6]]) || !any(v6) || all(v6)) F(s, "P6 marcas ", paste(v6, collapse = ","), " vs ", paste(X[[6]], collapse = ","))
  for (i in 12:15) { pre <- sub("\\..*$", ".", sl[i]); if (!identical(pre == "Verdadero.", isTRUE(v6[i - 11]))) F(s, "P6 feedback prefijo ", i) }
  ## feedback P6: contenido
  for (i in 12:15) { f <- nb(sl[i])
    m <- regmatches(f, regexec("Con un curso más, el grado (\\S+) habría recogido \\$([0-9]+) \\+ ([0-9]+) = ([0-9]+)\\$ kilos, (más que los|que no superan los) ([0-9]+) del grado (\\S+): (habría ganado|habría seguido perdiendo)\\.", f))[[1]]
    if (length(m)) { inc("fb_hp"); tt <- as.numeric(m[c(3, 4, 5, 7)])
      if (m[2] != nm2[l2] || tt[1] != TL || tt[2] != aL || tt[3] != TL + aL || tt[4] != TW || m[8] != nm2[w2] ||
          ((m[6] == "más que los") != (tt[3] > TW)) || ((m[9] == "habría ganado") != (tt[3] > TW))) F(s, "fb HP falso: ", f) }
    m <- regmatches(f, regexec("Con un curso menos, el grado (\\S+) habría recogido \\$([0-9]+) - ([0-9]+) = ([0-9]+)\\$ kilos, (menos que los|que siguen superando los) ([0-9]+) del grado (\\S+): (habría perdido|habría seguido ganando)\\.", f))[[1]]
    if (length(m)) { inc("fb_hg"); tt <- as.numeric(m[c(3, 4, 5, 7)])
      if (m[2] != nm2[w2] || tt[1] != TW || tt[2] != aW || tt[3] != TW - aW || tt[4] != TL || m[8] != nm2[l2] ||
          ((m[6] == "menos que los") != (tt[3] < TL)) || ((m[9] == "habría perdido") != (tt[3] < TL))) F(s, "fb HG falso: ", f) }
    m <- regmatches(f, regexec("Con ese cambio, el grado (\\S+) habría recogido \\$([0-9]+) \\\\times ([0-9]+) = ([0-9]+)\\$ kilos, (más|menos) que los ([0-9]+) del grado (\\S+): habría (ganado|perdido)\\.", f))[[1]]
    if (length(m)) { inc("fb_ventaja"); tt <- as.numeric(m[c(3, 4, 5, 7)]); tx <- ql[i]
      esp <- if (grepl("perdió hubiera participado con tantos", tx)) c(nm2[l2], nW, aL, TW) else if (grepl("ganó hubiera participado con tantos", tx)) c(nm2[w2], nL, aW, TL) else
        if (grepl("perdió hubiera tenido el mismo", tx)) c(nm2[l2], nL, aW, TW) else c(nm2[w2], nW, aL, TL)
      if (m[2] != esp[1] || tt[1] != as.numeric(esp[2]) || tt[2] != as.numeric(esp[3]) || tt[3] != tt[1] * tt[2] || tt[4] != as.numeric(esp[4]) ||
          ((m[6] == "más") != (tt[3] > tt[4])) || ((m[9] == "ganado") != (tt[3] > tt[4]))) F(s, "fb ventaja falso: ", f) }
    m <- regmatches(f, regexec("Ganó el grado (\\S+), con ([0-9]+) kilos frente a los ([0-9]+) del grado (\\S+)\\.", f))[[1]]
    if (length(m)) { inc("fb_ganador"); if (m[2] != nm2[w2] || as.numeric(m[3]) != TW || as.numeric(m[4]) != TL || m[5] != nm2[l2]) F(s, "fb ganador falso") } }
  ## Solution: listas de la Parte 6
  iv <- which(sol == "- Parte 6. Son verdaderas:"); ifa <- which(sol == "- Parte 6. Son falsas:")
  lv <- sub("^    - «(.*)»$", "\\1", sol[(iv + 1):(ifa - 1)]); k <- ifa + 1; lf <- character(0)
  while (k <= length(sol) && grepl("^    - «", sol[k])) { lf <- c(lf, sub("^    - «(.*)»$", "\\1", sol[k])); k <- k + 1 }
  if (!setequal(lv, ql[12:15][v6]) || !setequal(lf, ql[12:15][!v6])) F(s, "P6 Respuestas")
  ## Solution: párrafo «Parte 6.» (función de verdad propia)
  pr <- grep("^Parte 6\\. Cada hipótesis", sol, value = TRUE)
  if (length(pr) != 1L) { F(s, "párrafo P6 ausente"); next }
  x <- nb(pr)
  base <- "Parte 6. Cada hipótesis cambia un solo dato de la tabla. Un curso más con el mismo promedio suma ese promedio al total, y un curso menos lo resta. Por eso basta comparar ese promedio con la diferencia entre los totales: si es mayor, el ganador cambia; si es menor, no. "
  if (!startsWith(x, base)) F(s, "criterio general distinto")
  if (!(((aL > D2) == ((nL + 1) * aL > TW)) && ((aW > D2) == ((nW - 1) * aW < TL))) || aL == D2 || aW == D2) F(s, "criterio general FALSO o empate")
  m1 <- regmatches(x, regexec("Con un curso más, el grado (\\S+) habría sumado a su total ([0-9]+) kilos, (más|menos) que la diferencia de ([0-9]+) kilos: (habría ganado|no le habría alcanzado para ganar)\\. Con un curso menos", x))[[1]]
  m2 <- regmatches(x, regexec("Con un curso menos, el grado (\\S+) habría restado a su total ([0-9]+) kilos, (más|menos) que la diferencia de ([0-9]+) kilos: (habría perdido|habría seguido ganando)\\.$", x))[[1]]
  if (length(m1) != 6L || length(m2) != 6L) { F(s, "párrafo P6 con otra forma: ", x); next }
  hp_txt <- m1[6] == "habría ganado"; hg_txt <- m2[6] == "habría perdido"
  if (m1[2] != nm2[l2] || as.numeric(m1[3]) != aL || as.numeric(m1[5]) != D2) F(s, "P6 párrafo cifra/grado HP")
  if (m2[2] != nm2[w2] || as.numeric(m2[3]) != aW || as.numeric(m2[5]) != D2) F(s, "P6 párrafo cifra/grado HG")
  if ((m1[4] == "más") != (aL > D2)) F(s, "P6 comparación HP falsa"); if ((m2[4] == "más") != (aW > D2)) F(s, "P6 comparación HG falsa")
  if (hp_txt != ((nL + 1) * aL > TW)) F(s, "P6 conclusión HP falsa"); if (hg_txt != ((nW - 1) * aW < TL)) F(s, "P6 conclusión HG falsa")
  if (hp_txt != (m1[4] == "más") || hg_txt != (m2[4] == "más")) F(s, "P6 conclusión no se sigue")
  ihp <- match("Si el grado que perdió hubiera participado con un curso más, conservando su promedio por curso, habría ganado el concurso.", ql[12:15])
  ihg <- match("Si al grado que ganó le hubiera faltado un curso, conservando su promedio por curso, habría perdido el concurso.", ql[12:15])
  if (hp_txt != X[[6]][ihp] || hg_txt != X[[6]][ihg]) F(s, "P6 párrafo contra la marca")
  inc(paste0("ramas_", m1[4], "_", m2[4])); inc(paste0("concl_", hp_txt, "_", hg_txt)); inc(paste0("rama2_", if (nW > nL) "CURSOS" else "PROMEDIO", "_hp", hp_txt, "_hg", hg_txt))
  ## L-6 y espacio duro final; regla #19; #25; ortografía básica
  inc("p6_esp_normal", lengths(regmatches(pr, gregexpr("[0-9] kilos", pr))))
  if (!grepl("(habría perdido|habría seguido ganando)\\.$", pr)) F(s, "espacio duro final")
  if (grepl("Opción|opción [A-D]|\\b[A-D]\\)", pr)) F(s, "regla #19 en el párrafo")
  if (grepl("[^\\x{0000}-\\x{00FF}]", pr, perl = TRUE)) F(s, "glifo fuera de Latin-1 en el párrafo")
  if (grepl("\\b(habria|hipotesis|mas que|segun|tambien)\\b", tolower(x), perl = TRUE)) F(s, "tilde")
  inc("p6_cmp_RE_CMP_nuevo", sum(vapply(strsplit(x, "[.;]")[[1]], function(cl) length(regmatches(cl, regexec(RE_CMP, cl, perl = TRUE))[[1]]) > 0, NA)))
  ## aritmética de todo el texto emitido
  tx <- nb(c(e$question, e$solution, sl))
  ar <- regmatches(tx, gregexpr("([0-9]+) (\\\\times|\\+|-) ([0-9]+) = ([0-9]+)", tx))
  for (a in unlist(ar)) { p <- regmatches(a, regexec("([0-9]+) (\\\\times|\\+|-) ([0-9]+) = ([0-9]+)", a))[[1]]; x1 <- as.numeric(p[2]); x2 <- as.numeric(p[4]); y <- as.numeric(p[5])
    inc("igualdades"); if (y != switch(p[3], "\\times" = x1 * x2, "+" = x1 + x2, "-" = x1 - x2)) F(s, "aritmética ", a) }
  if (any(grepl("\\b(NA|NaN|NULL|Inf)\\b", tx))) F(s, "NA/NULL en el texto")
}
cat("versiones:", length(N), "· canónicas (tabla del impreso):", n_can, "· canónicas = impreso:", can_ok, "\n")
for (k in sort(ls(cnt))) cat(sprintf("  %s = %s\n", k, get(k, envir = cnt)))
cat("FALLOS:", length(fallos), "\n"); if (length(fallos)) cat(head(fallos, 40), sep = "\n")
cat("\n== Esqueletos de la Parte 1 (rama | verdad | texto) ==\n"); tb_e <- sort(table(esq), decreasing = TRUE); for (i in seq_along(tb_e)) cat(sprintf("%4d  %s\n", tb_e[i], names(tb_e)[i]))
## ---------- Invariancia N vs V (1..100) y patrón de la sonda sobre la redacción vieja ----------
dif <- character(0); re_viejo <- 0
for (s in names(V)) { a <- N[[s]]; b <- V[[s]]
  for (k in c("question", "questionlist", "solutionlist")) if (!identical(a[[k]], b[[k]])) dif <- c(dif, paste(s, k))
  mk <- setdiff(union(names(a$metainfo), names(b$metainfo)), character(0)); for (k in mk) if (!identical(a$metainfo[[k]], b$metainfo[[k]])) dif <- c(dif, paste(s, "meta", k))
  sa <- strsplit(paste(a$solution, collapse = "\n"), "\n")[[1]]; sb <- strsplit(paste(b$solution, collapse = "\n"), "\n")[[1]]
  pa <- grep("^Parte 6\\. Cada hipótesis", sa); pb <- grep("^Parte 6\\. Cada hipótesis", sb)
  if (length(pa) != 1 || length(pb) != 1 || !identical(sa[-pa], sb[-pb])) dif <- c(dif, paste(s, "solution fuera del párrafo"))
  xv <- nb(sb[pb]); re_viejo <- re_viejo + sum(vapply(strsplit(xv, "[.;]")[[1]], function(cl) length(regmatches(cl, regexec(RE_CMP, cl, perl = TRUE))[[1]]) > 0, NA))
  na <- as.numeric(regmatches(nb(sa[pa]), gregexpr("[0-9]+", nb(sa[pa])))[[1]]); nbv <- as.numeric(regmatches(xv, gregexpr("[0-9]+", xv))[[1]])
  if (!identical(na[c(1, 2, 3)], nbv[c(1, 2, 3)]) || na[4] != nbv[2]) dif <- c(dif, paste(s, "cifras del párrafo"))
  gn <- regmatches(nb(sa[pa]), gregexpr("el grado \\S+", nb(sa[pa])))[[1]]; gv <- regmatches(xv, gregexpr("el grado \\S+", xv))[[1]]
  if (!identical(gn, gv)) dif <- c(dif, paste(s, "grados del párrafo"))
}
cat("\nInvariancia 6f6f1b13 vs 27f789dc (100 semillas): diferencias", length(dif), "\n"); if (length(dif)) cat(head(dif, 20), sep = "\n")
cat("RE_CMP de la sonda H1-Sol sobre la redacción vieja: comparaciones reconocidas", re_viejo, "en 100 versiones\n")
