## Verificador del paso 9, ronda 4. md5 del .Rmd en la primera línea de la salida.
## Escritura de la ronda 2 restaurada (decisión del profesor del 2026-11-10: «Volver a la de la ronda 2») y tasas por
## ENUMERACIÓN EXACTA del diseño (FASE 2C r3, H1). §P7-D agotado: mide y lleva a firma; no persigue ninguna pista.
## Versiones anteriores: salida/verificar_vueltas_r3.R (ronda 3) y salida/verificar_vueltas_r2.R (ronda 2).
##   1. Muestra por cuotas (regla #23) en dos tandas de semillas (desde 1 y desde 5001): 100 no canónicas, 100 por forma
##      de escritura y la canónica (24 órdenes). Sirve al oráculo de texto, al techo nulo y al control de la enumeración.
##   2. Oráculo independiente (K, d y la letra se leen del TEXTO visible) sobre las dos tandas.
##   3. Enumeración exacta: objeto × distancia × letra × celda, con sus probabilidades; la canónica, en sus 24 órdenes.
##      Toda versión de la muestra está en la enumeración con sus cuatro opciones, y las frecuencias de las celdas cuadran
##      con la tabla de pesos. Oráculo algebraico sobre toda la enumeración.
##   4. Ancho en el render: todas las cadenas (enumeración y muestra) con pdflatex; modelo de la guarda; mutante A1.
##   5. Tasas EXACTAS por familia: B (batería §P7 congelada, más las 2 reglas del estímulo de la ronda 1 y la anchura y la
##      estrechez medidas en el render, en los 24 órdenes de cada combinación); V1 (vector del detractor r1); V2 («contiene
##      ÷» y marcas únicas); V3 (reglas de conjunto del detractor r3, H2: salida/detractor_r3/reglas_r3.R). Techo nulo de
##      cada familia con N = 100 (regla #23): el menor de las dos tandas, que es el más exigente.
##   6. Controles: la guarda del .Rmd (instancia de referencia) coincide celda por celda con la tasa exacta (FASE 2C r3,
##      H4.1); la tasa de cada tanda cae dentro de 4,5 errores típicos de la exacta (estabilidad ante otra semilla).
##   7. Rojos (exceso >= 8 pp sobre el techo nulo de su familia) contra el ALCANCE cableado en salida/alcance_r4.R (§P7-E):
##      rojo cubierto, nunca PASS, si la tasa exacta no pasa de la cableada.
##   8. Mutantes (Incidente P): de guarda, de texto por atajo, pares (nivel, atajo) por mutación del entorno y del alcance.
## Salida: exit 0 sin rojos; exit 3 si todo rojo está dentro del ALCANCE; exit 1 si falla algo.
## Uso: Rscript --no-init-file salida/verificar_vueltas.R <ruta.Rmd> <raíz del repo> [proponer]
##      («proponer»: sin ALCANCE ni mutantes; imprime la propuesta de alcance con las cifras exactas).
args <- commandArgs(TRUE); rmd <- args[1]; raiz <- args[2]; proponer <- length(args) >= 3L && args[3] == "proponer"
t0 <- Sys.time()
cat("md5 del .Rmd:", unname(tools::md5sum(rmd)), "\n")
suppressMessages(library(exams))
L <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", L); i1 <- which(L == "```"); i1 <- i1[i1 > i0][1]
CODE <- paste(L[(i0 + 1):(i1 - 1)], collapse = "\n")
gen <- function(code, s) { set.seed(s); e <- new.env(); eval(parse(text = code), envir = e); e }
fallos <- character(0)
FORMAS_M <- c("fraccion_izq", "numero_izq", "fraccion_unica")
NIVELES <- c("agregado", FORMAS_M, "canonica")
ALCANCE <- list()
if (!proponer) source(file.path(dirname(rmd), "salida", "alcance_r4.R"), local = TRUE)   # define ALCANCE
## ---------- 1. Muestra, en dos tandas ----------
muestra <- function(s0) {
  nc <- list(); porf <- setNames(lapply(FORMAS_M, function(x) list()), FORMAS_M); can <- NULL; s <- s0
  while ((length(nc) < 100L || any(lengths(porf) < 100L) || is.null(can)) && s < s0 + 4000L) {
    s <- s + 1L; e <- gen(CODE, s)
    if (e$es_canonica) { if (is.null(can)) can <- e; next }
    if (length(nc) < 100L) nc[[length(nc) + 1L]] <- e
    if (length(porf[[e$estilo]]) < 100L) porf[[e$estilo]][[length(porf[[e$estilo]]) + 1L]] <- e
  }
  cat(sprintf("MUESTRA (semillas %d-%d): agregado %d | %s | canónica %s\n", s0 + 1L, s, length(nc),
              paste(names(porf), lengths(porf), collapse = " · "), if (is.null(can)) "NO" else "sí"))
  for (f in names(porf)) if (length(porf[[f]]) < 20L) cat("  estrato", f, "NO CONCLUYENTE (n < 20)\n")
  list(nc = nc, porf = porf, can = can)
}
TANDAS <- list(t1 = muestra(0L), t2 = muestra(5000L))
perms <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perms(v[-i]), function(p) c(v[i], p))))
P24 <- perms(1:4)
can <- TANDAS$t1$can
can_vers <- lapply(P24, function(p) { e <- as.list(can); e$opciones <- can$opciones[p]; e$sol <- can$sol[p]; e })
## ---------- 2. Oráculo de texto ----------
oraculo <- function(e) {
  txt <- paste(e$regla, e$pregunta); K <- as.numeric(sub(".*es igual a ([0-9]+) dividido.*", "\\1", txt))
  d <- as.numeric(sub(".*para recorrer ([0-9]+) [^ ]+\\?$", "\\1", txt)); V <- sub(".*de perímetro \\*\\*([Pp])\\*\\*.*", "\\1", txt)
  ev <- function(x, P) { x <- gsub("[$]", "", x); x <- gsub("\\,", "", x, fixed = TRUE)
    for (i in 1:3) x <- gsub("\\\\dfrac\\{([^{}]*)\\}\\{([^{}]*)\\}", "((\\1)/(\\2))", x)
    x <- gsub("\\times", "*", x, fixed = TRUE); x <- gsub("\\div", "/", x, fixed = TRUE)
    eval(parse(text = gsub(V, "x", x, fixed = TRUE)), list(x = P)) }
  Ps <- c(37, 211.5, 1234)
  ok <- vapply(e$opciones, function(o) all(abs(vapply(Ps, function(P) ev(o, P), 0) - K * d / Ps) < 1e-9 * K * d / Ps), TRUE)
  c(sum(ok) == 1, identical(as.integer(ok), as.integer(e$sol))) }
nc_de <- function(T) c(T$nc, unlist(T$porf, recursive = FALSE))
todas <- unlist(lapply(TANDAS, function(T) c(nc_de(T), list(T$can))), recursive = FALSE)
OR <- t(vapply(todas, oraculo, c(TRUE, TRUE)))
cat(sprintf("ORÁCULO de texto: %d versiones | exactamente 1 correcta: %d | la marcada es la correcta: %d\n", nrow(OR), sum(OR[, 1]), sum(OR[, 2])))
if (!all(OR)) fallos <- c(fallos, "oráculo: la clave no es la única correcta en alguna versión")
## ---------- 3. Enumeración exacta ----------
e0 <- TANDAS$t1$nc[[1]]
PES <- e0$PESOS / sum(e0$PESOS)
cmb <- list()
for (oi in seq_along(e0$OBJETOS)) { o <- e0$OBJETOS[[oi]]; K <- e0$SISTEMAS[[o$sis]]$K
  for (d in o$d_cand) for (V in c("P", "p")) for (cel in names(PES)) {
    terna <- sub("\\|.*", "", cel); est <- sub(".*\\|", "", cel)
    cods <- c("C", sub("VL-", "", strsplit(terna, "+", fixed = TRUE)[[1L]], fixed = TRUE))
    KT <- e0$num_tex(K); dT <- e0$num_tex(d)
    cmb[[length(cmb) + 1L]] <- list(w = 1 / length(e0$OBJETOS) / length(o$d_cand) / 2 * PES[[cel]], est = est, cel = cel,
      opciones = paste0("$", unname(vapply(cods, e0$expr_de, "", est = est, k = KT, n = dT, v = V)), "$"),
      sol = c(1L, 0L, 0L, 0L), K = K, d = d, V = V, KT = KT, dT = dT) } }
W <- vapply(cmb, `[[`, 0, "w"); EST <- vapply(cmb, `[[`, "", "est"); CEL <- vapply(cmb, `[[`, "", "cel")
zc <- list(w = 1, est = "canonica", opciones = can$opciones[c(which(can$sol == 1L), which(can$sol != 1L))], sol = c(1L, 0L, 0L, 0L),
           K = can$K, d = can$d, V = can$V, KT = can$KT, dT = can$dT)
cat(sprintf("ENUMERACIÓN: %d combinaciones no canónicas | suma de probabilidades %.9f | con opciones repetidas: %d | canónica con probabilidad %.2f\n",
            length(cmb), sum(W), sum(vapply(cmb, function(z) anyDuplicated(z$opciones) > 0, TRUE)), e0$P_CANON))
if (abs(sum(W) - 1) > 1e-9) fallos <- c(fallos, "la enumeración no suma 1")
llave <- function(cel, K, d, V, ops) paste(cel, K, d, V, paste(sort(ops), collapse = " | "))
LL <- vapply(cmb, function(z) llave(z$cel, z$K, z$d, z$V, z$opciones), "")
cel_de <- function(e) paste(paste(sort(e$sel), collapse = "+"), e$estilo, sep = "|")
nc_t <- unlist(lapply(TANDAS, nc_de), recursive = FALSE)
dentro <- vapply(nc_t, function(e) llave(cel_de(e), e$K, e$d, e$V, e$opciones) %in% LL, TRUE)
cat(sprintf("PERTENENCIA: %d de %d versiones no canónicas de la muestra están en la enumeración con su celda y sus cuatro opciones\n", sum(dentro), length(dentro)))
if (!all(dentro)) fallos <- c(fallos, "una versión de la muestra no está en la enumeración")
obs <- table(factor(vapply(unlist(lapply(TANDAS, `[[`, "nc"), recursive = FALSE), cel_de, ""), levels = names(PES)))
pv <- suppressWarnings(chisq.test(as.numeric(obs), p = as.numeric(PES))$p.value)
cat(sprintf("FRECUENCIAS de las celdas (200 no canónicas sin cuota): chi-cuadrado p = %.3f | %s\n", pv, paste(sprintf("%s %d (esp. %.1f)", names(obs), as.integer(obs), 200 * PES), collapse = " · ")))
if (pv < 0.001) fallos <- c(fallos, "las frecuencias de las celdas no cuadran con la tabla de pesos")
ev_tex <- function(x, V, P) { x <- gsub("[$]", "", x); x <- gsub("\\,", "", x, fixed = TRUE)
  for (i in 1:3) x <- gsub("\\\\dfrac\\{([^{}]*)\\}\\{([^{}]*)\\}", "((\\1)/(\\2))", x)
  x <- gsub("\\times", "*", x, fixed = TRUE); x <- gsub("\\div", "/", x, fixed = TRUE)
  eval(parse(text = gsub(V, "x", x, fixed = TRUE)), list(x = P)) }
Ps <- c(37, 211.5, 1234)
alg <- vapply(c(cmb, list(zc)), function(z) { ok <- vapply(z$opciones, function(o) all(abs(vapply(Ps, function(P) ev_tex(o, z$V, P), 0) -
  z$K * z$d / Ps) < 1e-9 * z$K * z$d / Ps), TRUE); sum(ok) == 1L && ok[1] }, TRUE)
cat(sprintf("ORÁCULO algebraico sobre la enumeración (y la canónica): %d de %d con exactamente una correcta, y es la clave\n", sum(alg), length(alg)))
if (!all(alg)) fallos <- c(fallos, "oráculo algebraico: alguna combinación del diseño no tiene una sola correcta")
## ---------- 4. Ancho en el render de TeX ----------
U <- unique(c(unlist(lapply(cmb, `[[`, "opciones")), unlist(lapply(todas, `[[`, "opciones"))))
td <- tempfile("anchos"); dir.create(td)
writeLines(c("\\documentclass[10pt]{article}", "\\usepackage{amsmath}", "\\newsavebox\\caja", "\\begin{document}",
             sprintf("\\sbox\\caja{%s}\\typeout{ANCHO %d \\the\\wd\\caja}", U, seq_along(U)), "\\end{document}"), file.path(td, "a.tex"))
system2("pdflatex", c("-interaction=batchmode", paste0("-output-directory=", td), file.path(td, "a.tex")), stdout = FALSE)
lg <- readLines(file.path(td, "a.log")); mm <- regmatches(lg, regexec("^ANCHO ([0-9]+) ([0-9.]+)pt", lg)); mm <- mm[lengths(mm) == 3L]
WT <- setNames(rep(NA_real_, length(U)), U); WT[as.integer(vapply(mm, `[`, "", 2))] <- as.numeric(vapply(mm, `[`, "", 3))
if (anyNA(WT)) fallos <- c(fallos, "pdflatex no midió todas las cadenas")
err_mod <- max(abs(vapply(U, e0$ancho_tex, 0) - WT[U]))
ne <- new.env(parent = environment(e0$ancho_tex)); ne$ANCHO_GLIFO <- replace(e0$ANCHO_GLIFO, "P", 6.42)
anch_mut <- e0$ancho_tex; environment(anch_mut) <- ne; ne$ancho_tex <- anch_mut
err_mut <- max(abs(vapply(U, anch_mut, 0) - WT[U]))
cat(sprintf("ANCHO: %d cadenas medidas con pdflatex | modelo de la guarda: error máximo %.4f pt | mutante A1 (P sin corrección itálica): error %.3f pt, %s\n",
            length(U), err_mod, err_mut, if (err_mut > 0.01) "cazado_por_su_sonda" else "no_detectado"))
if (err_mod > 0.01) fallos <- c(fallos, "el modelo de ancho de la guarda no coincide con pdflatex")
if (err_mut <= 0.01) fallos <- c(fallos, "mutante A1 del instrumento de ancho no detectado")
## ---------- 5. Familias y tasas exactas ----------
source(file.path(raiz, ".claude/scripts/bateria_eliminacion.R"), local = (BE <- new.env()))
ex <- parse(file.path(raiz, ".claude/scripts/bateria_referencia_icfes.R")); RE <- new.env()
for (x in ex) { eval(x, RE); if (exists("B2", RE, inherits = FALSE)) break }
num_ops <- function(o) as.numeric(regmatches(gsub("\\,", "", o, fixed = TRUE), gregexpr("[0-9]+", gsub("\\,", "", o, fixed = TRUE)))[[1]])
extra <- list(
  BE$nueva_regla("estimulo: contiene K y d del enunciado", "lexico", function(op, est)
    vapply(op, function(o) all(c(est$K, est$d) %in% num_ops(o)), TRUE), usa_estimulo = TRUE),
  BE$nueva_regla("estimulo: contiene la relacion K/V y otra operacion", "formato", function(op, est)
    vapply(op, function(o) grepl(paste0("\\dfrac{", est$K, "}{", est$V, "}"), o, fixed = TRUE) &&
      grepl("\\\\times|\\\\div", o), TRUE), usa_estimulo = TRUE))
nom <- function(r) r$nombre
B2v <- lapply(RE$B2, function(r) if (nom(r) == "la mas larga (chars)") BE$nueva_regla("la mas ancha (TeX)", "magnitud", function(o) abs(WT[o] - max(WT[o])) < 0.01) else
  if (nom(r) == "la mas corta (chars)") BE$nueva_regla("la mas estrecha (TeX)", "magnitud", function(o) abs(WT[o] - min(WT[o])) < 0.01) else r)
reglas <- c(B2v, extra); chars <- Filter(function(r) grepl("(chars)", nom(r), fixed = TRUE), RE$B2)
op_cl <- function(vers) list(op = lapply(vers, function(e) e$opciones), cl = vapply(vers, function(e) which(e$sol == 1L), 0L),
                             est = lapply(vers, function(e) list(K = e$K, d = e$d, V = e$V)))
bateria <- function(vers) { x <- op_cl(vers); BE$evaluar_bateria(reglas, x$op, x$cl, estimulos = x$est) }
tok <- function(x) { x <- gsub("$", "", x, fixed = TRUE); x <- gsub("\\,", "", x, fixed = TRUE)
  x <- gsub("\\dfrac{", " F( ", x, fixed = TRUE); x <- gsub("}{", " / ", x, fixed = TRUE); x <- gsub("}", " ) ", x, fixed = TRUE)
  x <- gsub("\\times", " x ", x, fixed = TRUE); x <- gsub("\\div", " d ", x, fixed = TRUE); strsplit(trimws(gsub("\\s+", " ", x)), " ")[[1]] }
bigr <- function(t) paste(head(t, -1), tail(t, -1))
sJ_de <- function(o) { ts <- lapply(o, tok); colSums(sapply(ts, function(a) sapply(ts, function(z) length(intersect(bigr(a), bigr(z))) / length(union(bigr(a), bigr(z)))))) - 1 }
sL_de <- function(o) { ts <- lapply(o, tok); u <- unique(unlist(ts)); ch <- intToUtf8(seq_along(u) + 64L, multiple = TRUE)
  rowSums(adist(vapply(ts, function(t) paste(ch[match(t, u)], collapse = ""), ""))) }
partes_fr <- function(x, i) { m <- regmatches(x, gregexpr("\\\\dfrac\\{[^{}]*\\}\\{[^{}]*\\}", x))[[1]]; sub("\\\\dfrac\\{([^{}]*)\\}\\{([^{}]*)\\}", paste0("\\", i), m) }
rasgos <- function(o) { x <- gsub("[$]", "", o); nop <- lengths(regmatches(x, gregexpr("\\\\times|\\\\div", x)))
  cbind(division = grepl("\\div", x, fixed = TRUE), producto = grepl("\\times", x, fixed = TRUE),
        empieza_fraccion = startsWith(x, "\\dfrac"), termina_fraccion = endsWith(x, "}"),
        fraccion_sola = grepl("^\\\\dfrac\\{[^{}]*\\}\\{[^{}]*\\}$", x), dos_operadores = nop == 2L, sin_operador = nop == 0L,
        letra_arriba = vapply(x, function(s) any(grepl("[Pp]", partes_fr(s, 1))), TRUE),
        letra_abajo = vapply(x, function(s) any(grepl("[Pp]", partes_fr(s, 2))), TRUE),
        empieza_numero = grepl("^[0-9]", x), operador_en_fraccion = vapply(x, function(s) any(grepl("times|div", c(partes_fr(s, 1), partes_fr(s, 2)))), TRUE)) }
solo1 <- function(l) if (sum(l) == 1L) which(l) else integer(0)
V1 <- list(centralidad = function(e) { v <- sJ_de(e$opciones); abs(v - max(v)) < 1e-12 },
           medoide = function(e) { v <- sL_de(e$opciones); v == min(v) },
           tres_datos = function(e) vapply(e$opciones, function(o) all(c(gsub("\\,", "", e$KT, fixed = TRUE), gsub("\\,", "", e$dT, fixed = TRUE), e$V) %in% tok(o)), TRUE))
V2 <- c(list(`contiene division` = function(e) grepl("\\div", e$opciones, fixed = TRUE)),
        unlist(lapply(colnames(rasgos("$1$")), function(nmr) setNames(list(
          function(e) solo1(rasgos(e$opciones)[, nmr]), function(e) solo1(!rasgos(e$opciones)[, nmr])),
          paste(c("marca: unica con", "marca: unica sin"), nmr))), recursive = FALSE))
source(file.path(dirname(rmd), "salida", "detractor_r3", "reglas_r3.R"), local = (R3E <- new.env()))
V3N <- setdiff(names(R3E$R3), c("V1 centralidad", "V1 medoide", "usa K, d y la letra", "ancho TeX (tol 0,01 pt)",
                                "estrecha TeX (tol 0,01 pt)", "contiene div (conjunto)"))
V3 <- setNames(lapply(V3N, function(nm) { f <- R3E$R3[[nm]]
  function(e) f(gsub("$", "", e$opciones, fixed = TRUE), list(K = e$KT, d = e$dT, V = e$V, w = unname(WT[e$opciones]))) }), V3N)
FAMS <- list(V1 = V1, V2 = V2, V3 = V3)
conj <- function(r, n = 4L) { if (is.logical(r)) { r[is.na(r)] <- FALSE; return(r) }; s <- rep(FALSE, n); s[r] <- TRUE; s }
cred1 <- function(S) { S <- conj(S); k <- sum(S); if (k == 0L) 0.25 else if (S[1]) 1 / k else 0 }   # la clave va primera
familia <- function(vers, fam, B = 2000L) {
  n <- length(vers); kk <- vapply(vers, function(e) which(e$sol == 1L), 0L)
  SC <- lapply(fam, function(f) t(vapply(vers, function(e) { S <- conj(f(e)); k <- sum(S); if (k == 0L) rep(0.25, 4L) else ifelse(S, 1 / k, 0) }, numeric(4))))
  obs <- vapply(SC, function(M) mean(M[cbind(seq_len(n), kk)]), 0)
  set.seed(99); nul <- replicate(B, { r <- sample.int(4L, n, TRUE); max(vapply(SC, function(M) mean(M[cbind(seq_len(n), r)]), 0)) })
  list(obs = obs, techo = mean(nul), p95 = unname(quantile(nul, 0.95))) }
## Techo nulo (N = 100) y tasas de cada tanda
TECHO <- list(); MUEST <- list()
for (tn in names(TANDAS)) { T <- TANDAS[[tn]]; MS <- c(list(agregado = T$nc), T$porf, list(canonica = can_vers))
  for (m in names(MS)) { vers <- MS[[m]]
    res <- bateria(vers); det <- res$detalle; nmc <- intersect(c("regla", "nombre"), names(det))[1]
    TECHO[[paste(tn, m, "B")]] <- res$techo_nulo; MUEST[[paste(tn, m, "B")]] <- setNames(det$tasa, det[[nmc]])
    if (tn == "t1") cat(sprintf("Familia B con N = %d, %s, tanda 1: veredicto de la batería %s | techo nulo %.1f %% | máximo %.1f %% (%s)\n",
                                length(vers), m, res$veredicto, 100 * res$techo_nulo, 100 * res$max_obs, res$regla_top))
    for (fn in names(FAMS)) { F <- familia(vers, FAMS[[fn]]); TECHO[[paste(tn, m, fn)]] <- F$techo; MUEST[[paste(tn, m, fn)]] <- F$obs } } }
## Tasas exactas
expandir <- function(zs) { n <- length(zs) * 24L; OPS <- vector("list", n); CL <- integer(n); ES <- vector("list", n); WV <- numeric(n); FV <- character(n); j <- 0L
  for (z in zs) for (p in P24) { j <- j + 1L; OPS[[j]] <- z$opciones[p]; CL[j] <- which(p == 1L); ES[[j]] <- list(K = z$K, d = z$d, V = z$V); WV[j] <- z$w / 24; FV[j] <- z$est }
  list(OPS = OPS, CL = CL, ES = ES, WV = WV, FV = FV) }
X <- expandir(cmb); XC <- expandir(list(zc))
porn <- function(cr, wv, fv) c(agregado = sum(wv * cr) / sum(wv), vapply(FORMAS_M, function(f) sum(wv[fv == f] * cr[fv == f]) / sum(wv[fv == f]), 0))
EXB <- t(vapply(reglas, function(r) {
  M <- BE$.matriz_score(r, X$OPS, rep(4L, length(X$OPS)), 4L, X$ES); cr <- M[cbind(seq_along(X$OPS), X$CL)]
  Mc <- BE$.matriz_score(r, XC$OPS, rep(4L, length(XC$OPS)), 4L, XC$ES); crc <- Mc[cbind(seq_along(XC$OPS), XC$CL)]
  c(porn(cr, X$WV, X$FV), canonica = mean(crc)) }, numeric(5)))
rownames(EXB) <- vapply(reglas, nom, ""); EX <- list(B = EXB); CRED <- list()
for (fn in names(FAMS)) { C <- sapply(FAMS[[fn]], function(f) vapply(cmb, function(z) cred1(f(z)), 0))
  CRED[[fn]] <- C; EX[[fn]] <- cbind(t(apply(C, 2, porn, wv = W, fv = EST)), canonica = vapply(FAMS[[fn]], function(f) cred1(f(zc)), 0)) }
## Tabla larga: familia, regla, nivel, tasa exacta, techo (el menor de las dos tandas), exceso
TL <- do.call(rbind, lapply(names(EX), function(fn) do.call(rbind, lapply(NIVELES, function(m) {
  te <- min(TECHO[[paste("t1", m, fn)]], TECHO[[paste("t2", m, fn)]])
  data.frame(familia = fn, regla = rownames(EX[[fn]]), muestra = m, tasa = EX[[fn]][, m], techo = te, exceso = EX[[fn]][, m] - te, stringsAsFactors = FALSE) }))))
write.csv(TL, file.path(dirname(rmd), "salida", "r4_exacto.csv"), row.names = FALSE)
cat("\n=== Techo nulo con N = 100 (el menor de las dos tandas), por familia y nivel ===\n")
for (fn in names(EX)) cat(sprintf("  %-3s %s\n", fn, paste(sprintf("%s %.1f %%", NIVELES, 100 * vapply(NIVELES, function(m) min(TECHO[[paste("t1", m, fn)]], TECHO[[paste("t2", m, fn)]]), 0)), collapse = " · ")))
## H3b exacta (la sonda borra los dígitos y compara la clave): parte de la forma de escribir la clave más frecuente
forma_clave <- function(o) paste(tolower(regmatches(o, gregexpr("[A-Za-z]+", o))[[1]]), collapse = " ")
fk <- c(vapply(cmb, function(z) forma_clave(z$opciones[1]), ""), forma_clave(zc$opciones[1])); wk <- c((1 - e0$P_CANON) * W, e0$P_CANON)
h3b <- tapply(wk, fk, sum); h3b_nc <- tapply(W, fk[seq_along(W)], sum)
cat(sprintf("\nH3b EXACTA (todas las versiones): %s | sin la canónica: %s\n", paste(sprintf("«%s» %.1f %%", names(h3b), 100 * h3b), collapse = " · "),
            paste(sprintf("«%s» %.1f %%", names(h3b_nc), 100 * h3b_nc), collapse = " · ")))
## Márgenes exactos (mediana ponderada) cuando la clave es la única en el extremo
med_w <- function(x, w) { o <- order(x); x <- x[o]; w <- w[o] / sum(w); x[which(cumsum(w) >= 0.5)[1]] }
margen_ex <- function(f, mayor = TRUE, sel = rep(TRUE, length(cmb))) { g <- rep(NA_real_, length(cmb))
  for (i in which(sel)) { v <- f(cmb[[i]]$opciones); o <- sort(v, decreasing = mayor)
    if (abs(v[1] - o[1]) < 1e-9 && sum(abs(v - o[1]) < 1e-9) == 1L) g[i] <- abs(o[1] - o[2]) / abs(if (mayor) o[1] else o[2]) }
  ok <- !is.na(g); if (!any(ok)) return("-"); sprintf("%.0f %% (en el %.1f %% de las versiones; %.1f %% con margen >= 15 %%)", 100 * med_w(g[ok], W[ok]),
    100 * sum(W[ok]) / sum(W[sel]), 100 * sum(W[ok & g >= 0.15]) / sum(W[sel])) }
cat("\n=== Márgenes exactos cuando la clave es la única en el extremo (mediana ponderada) ===\n")
for (m in c("agregado", FORMAS_M)) { sel <- if (m == "agregado") rep(TRUE, length(cmb)) else EST == m
  cat(sprintf("  %-15s ancho (TeX) %s\n  %-15s centralidad %s · medoide %s\n", m, margen_ex(function(o) WT[o], TRUE, sel), "",
              margen_ex(sJ_de, TRUE, sel), margen_ex(sL_de, FALSE, sel))) }
## Vector completo pedido (tasas exactas)
cols <- c(V1 = "centralidad", V1 = "medoide", B = "la que MAS comparte", V1 = "tres_datos", B = "la mas ancha (TeX)",
          V3 = "divide entre una fraccion", V3 = "empieza por la distancia", V3 = "letra arriba (conjunto)", V3 = "contiene x (conjunto)",
          V3 = "un operador", V3 = "NO contiene K/V del enunciado", V3 = "gemelas (1 simbolo o intercambio)", V2 = "contiene division",
          B = "estimulo: contiene la relacion K/V y otra operacion")
cat("\n=== Vector completo, tasas EXACTAS (% de acierto si se aplica el atajo; azar 25 %) ===\n")
for (i in seq_along(cols)) cat(sprintf("  %-3s %-52s %s\n", names(cols)[i], cols[i], paste(sprintf("%s %5.1f", NIVELES, 100 * EX[[names(cols)[i]]][cols[i], NIVELES]), collapse = " · ")))
## ---------- 6. Controles de la enumeración ----------
guard_inst <- function(z) { op <- gsub("$", "", z$opciones, fixed = TRUE)
  ts <- lapply(op, e0$tok_r); u <- unique(unlist(ts)); ch <- intToUtf8(seq_along(u) + 64L, multiple = TRUE)
  sL <- rowSums(adist(vapply(ts, function(t) paste(ch[match(t, u)], collapse = ""), "")))
  J <- sapply(ts, function(a) sapply(ts, function(b) length(intersect(e0$bigr_r(a), e0$bigr_r(b))) / length(union(e0$bigr_r(a), e0$bigr_r(b)))))
  sJ <- colSums(J) - 1
  tk <- lapply(op, function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-z]{3,}", s))[[1L]])))
  sM <- vapply(seq_along(tk), function(i) length(intersect(tk[[i]], unlist(tk[-i]))), 0L); w <- unname(WT[z$opciones])
  c(centralidad = e0$credito(which(abs(sJ - max(sJ)) < 1e-12)), medoide = e0$credito(which(sL == min(sL))),
    tres_datos = e0$credito(which(vapply(ts, function(t) all(c(z$KT, z$dT, z$V) %in% t), TRUE))),
    mas_comparte = e0$credito(which(sM == max(sM))), ancho = e0$credito(which(abs(w - max(w)) < 0.01))) }
GE <- t(vapply(cmb, guard_inst, numeric(5)))
GEc <- rowsum(W * GE, CEL) / as.vector(rowsum(W, CEL))
Aref <- e0$A_cel; rownames(Aref) <- paste(e0$CELDAS$terna[e0$activas], e0$CELDAS$estilo[e0$activas], sep = "|")
dif <- max(abs(GEc[rownames(Aref), colnames(Aref)] - Aref))
cat(sprintf("\nCONTROL guarda = exacta: diferencia máxima por celda entre la instancia de referencia y la enumeración: %.2e (%d celdas × %d atajos)\n",
            dif, nrow(Aref), ncol(Aref)))
if (dif > 1e-9) fallos <- c(fallos, "la guarda (instancia de referencia) no coincide con la tasa exacta por celda")
zs <- NULL
for (tn in names(TANDAS)) for (m in c("agregado", FORMAS_M)) for (fn in names(EX)) {
  s_ <- MUEST[[paste(tn, m, fn)]]; x_ <- EX[[fn]][names(s_), m]; z_ <- (s_ - x_) / sqrt(pmax(x_ * (1 - x_), 0.0025) / 100)
  zs <- rbind(zs, data.frame(tanda = tn, muestra = m, familia = fn, regla = names(s_), z = z_, stringsAsFactors = FALSE)) }
peor <- zs[which.max(abs(zs$z)), ]
cat(sprintf("CONTROL de estabilidad (dos tandas de semillas, %d tasas): |z| máximo %.2f (%s, %s, %s · %s); con |z| > 4,5: %d\n",
            nrow(zs), abs(peor$z), peor$tanda, peor$muestra, peor$familia, peor$regla, sum(abs(zs$z) > 4.5)))
if (any(abs(zs$z) > 4.5)) fallos <- c(fallos, "una tasa de la muestra se aleja de la exacta más de 4,5 errores típicos")
## ---------- 7. Rojos contra el ALCANCE ----------
ROJAS <- TL[TL$exceso >= 0.08 - 1e-12, ]
ROJAS <- ROJAS[order(ROJAS$muestra, -ROJAS$exceso), ]
cubre <- function(m, f, r, t, alc) any(vapply(alc, function(a) a$muestra == m && a$familia == f && a$regla == r && t <= a$tasa + 1e-9, TRUE))
clasificar <- function(R, alc) if (is.null(R) || !nrow(R)) character(0) else
  ifelse(mapply(cubre, R$muestra, R$familia, R$regla, R$tasa, MoreArgs = list(alc = alc)), "ROJO CUBIERTO (alcance a firmar)", "ROJO SIN ALCANCE")
cls <- clasificar(ROJAS, ALCANCE)
cat("\n=== Reglas en rojo: tasa EXACTA con exceso >= 8 pp sobre el techo nulo de su familia (N = 100) ===\n")
if (length(cls)) for (i in seq_along(cls)) cat(sprintf("  %-15s %-3s %-52s %5.1f %% (techo %4.1f %%, %+5.1f pp)  %s\n", ROJAS$muestra[i], ROJAS$familia[i],
  ROJAS$regla[i], 100 * ROJAS$tasa[i], 100 * ROJAS$techo[i], 100 * ROJAS$exceso[i], cls[i])) else cat("  ninguna\n")
if (proponer) {
  cat("\n## PROPUESTA DE ALCANCE (tasa exacta, redondeada hacia arriba a 6 decimales)\n")
  for (i in seq_len(nrow(ROJAS))) cat(sprintf("  a(%-17s %-5s %-55s %.6f),\n", paste0('"', ROJAS$muestra[i], '",'), paste0('"', ROJAS$familia[i], '",'),
    paste0('"', ROJAS$regla[i], '",'), ceiling(ROJAS$tasa[i] * 1e6) / 1e6))
  cat("\ntiempo:", format(Sys.time() - t0), "\n"); quit(status = 0L)
}
vmut <- function(alc, et) { v <- clasificar(ROJAS, alc); ok <- any(v == "ROJO SIN ALCANCE")
  cat(sprintf("MUTANTE %s: %s\n", et, if (ok) "sale del cubierto (cazado_por_su_sonda)" else "NO DETECTADO")); if (!ok) fallos <<- c(fallos, paste("mutante del alcance", et)) }
if (length(ALCANCE)) {
  iB <- which(vapply(ALCANCE, function(a) a$familia == "B", TRUE))[1]; iV <- which(vapply(ALCANCE, function(a) a$familia != "B", TRUE))[1]
  a1 <- ALCANCE; a1[[1]]$tasa <- a1[[1]]$tasa - 0.02; vmut(a1, sprintf("V1 (%s · %s cableada 2 pp por debajo)", ALCANCE[[1]]$muestra, ALCANCE[[1]]$regla))
  if (!is.na(iB)) vmut(ALCANCE[-iB], sprintf("V2 (sin %s · %s, familia B)", ALCANCE[[iB]]$muestra, ALCANCE[[iB]]$regla))
  if (!is.na(iV)) vmut(ALCANCE[-iV], sprintf("V3 (sin %s · %s, familia %s)", ALCANCE[[iV]]$muestra, ALCANCE[[iV]]$regla, ALCANCE[[iV]]$familia))
}
## ---------- 8. Mutantes ----------
primera_muerte <- function(code, tope = 600L) { for (s in seq_len(tope)) { m <- tryCatch({ gen(code, s); NULL }, error = function(err) conditionMessage(err))
  if (!is.null(m)) return(list(s = s, msg = m)) }; NULL }
ctl <- primera_muerte(CODE); cat("\nCONTROL sin mutar (600 semillas):", if (is.null(ctl)) "0 muertes" else paste("MUERE en", ctl$s, ctl$msg), "\n")
if (!is.null(ctl)) fallos <- c(fallos, "el control sin mutar muere")
t3 <- combn(paste0("VL-", c("A", "B", "D", "E", "F", "G")), 3L, simplify = FALSE)
pesos_unif <- paste0("PESOS <- c(", paste(vapply(t3, function(t) paste0('"', paste(t, collapse = "+"), "|", FORMAS_M, '" = 1', collapse = ", "), ""), collapse = ", "), ")\n")
pesos_act <- regmatches(CODE, regexpr("PESOS <- c\\([^)]*\\)\n", CODE))
G <- function(id, sonda, de, a) list(id = id, sonda = sonda, de = de, a = a)
mutantes <- list(
  G("G01", "diseño: toda distancia del banco", "c(200, 220), c(40, 60, 80, 120", "c(7, 11), c(40, 60, 80, 120"),
  G("G02", "tabla de pesos: cada celda con peso existe", 'PESOS <- c("VL-A+VL-D+VL-F|fraccion_izq"', 'PESOS <- c("VL-Z|fraccion_izq" = 3, "VL-A+VL-D+VL-F|fraccion_izq"'),
  G("G02b", "tabla de pesos: todos los pesos son positivos", '"VL-D+VL-E+VL-F|numero_izq"   = 3', '"VL-D+VL-E+VL-F|numero_izq"   = -3'),
  G("G03", "regla #1: banco de al menos 6 errores", 'stopifnot("regla #1: banco de al menos 6 errores"', 'errores_conceptuales[["VL-E"]] <- NULL; stopifnot("regla #1: banco de al menos 6 errores"'),
  G("G04", "regla #1: cada error declara precondición y calcula", 'stopifnot("regla #1: banco de al menos 6 errores"', 'errores_conceptuales[["VL-A"]]$calcula <- NULL; stopifnot("regla #1: banco de al menos 6 errores"'),
  G("G05", "selección: tres distractores del banco", 'sel    <- strsplit(celda$terna, "+", fixed = TRUE)[[1L]]', 'sel    <- strsplit(celda$terna, "+", fixed = TRUE)[[1L]][1:2]'),
  G("G06", "diagnosticidad: tabla de pesos", pesos_act, pesos_unif),
  G("G07", "regla #13: exactamente una clave", 'sol      <- as.integer(codigos == "CLAVE")', 'sol      <- as.integer(codigos %in% c("CLAVE", sel[1L]))'),
  G("G08", "regla #13: el oráculo LaTeX", 'clave_expr <- expr_de("C", estilo, KT, dT, V)', 'clave_expr <- expr_de("G", estilo, KT, dT, V)'),
  G("G09", "opciones distintas", 'expr_sel <- vapply(errores_conceptuales[sel], function(e) e$expr, "")', 'expr_sel <- vapply(errores_conceptuales[sel], function(e) e$expr, ""); expr_sel[2L] <- expr_sel[1L]'),
  G("G10", "Capa C", "calcula = function(p) p$d / p$P)", "calcula = function(p) p$K * p$d / p$P)"),
  G("G11", "precondiciones de los errores mostrados", "precondicion = function(p) p$d != 1,\n    calcula = function(p) p$K / p$P)", "precondicion = function(p) p$d == 1,\n    calcula = function(p) p$K / p$P)"),
  G("G12", "caso específico: número entero de vueltas", "P_ref <- if (es_canonica) 200 else elegir_Pref(o$P_cand, K, d)", "P_ref <- if (es_canonica) 200 else elegir_Pref(o$P_cand, K, d) + 0.5"),
  G("G13", "errata: ninguna versión muestra 101/120", 'expr_sel <- vapply(errores_conceptuales[sel], function(e) e$expr, "")',
    'expr_sel <- sub("\\\\dfrac{100}{150}", "\\\\dfrac{101}{120}", vapply(errores_conceptuales[sel], function(e) e$expr, ""), fixed = TRUE)'),
  G("G14", "canónica: enunciado verbatim", 'regla <- paste0("La cantidad de vueltas que da una llanta para recorrer un metro es igual a 100 dividido',
    'regla <- paste0("La cantidad de vueltas que da una llanta para recorrer un metro es igual a 1000 dividido'),
  G("G15", "canónica: pregunta verbatim", '"llanta de perímetro **P** (en cm), para recorrer 150 metros?")', '"llanta de perímetro **P** (en cm), para recorrer 160 metros?")'),
  G("G16", "canónica: opciones de la Guía", 'GUIA_OFICIAL <- c("VL-A", "VL-B", "VL-D")', 'GUIA_OFICIAL <- c("VL-A", "VL-E", "VL-D")'),
  G("G18", "regla #7: contracciones", 'del_n <- if (o$gen == "f") paste("de la", o$nucleo) else paste("del", o$nucleo)', 'del_n <- paste("de", la_n)'),
  G("G19", "regla #25: sin signo menos", 'regla <- paste0("La cantidad de vueltas que da ", una_n,', 'regla <- paste0("\\u2212La cantidad de vueltas que da ", una_n,'),
  G("G20", "textos sin NA", "contexto <- pick(CONTEXTOS)(o, s, d, pick(NOMBRES))", "contexto <- paste(pick(CONTEXTOS)(o, s, d, pick(NOMBRES)), NA)"))
ATJ <- c(centralidad = "centralidad = credito(which(abs(sJ - max(sJ)) < 1e-12))", medoide = "medoide = credito(which(sL == min(sL)))",
         tres_datos = 'tres_datos = credito(which(vapply(ts, function(t) all(c("100", "150", "P") %in% t), TRUE)))',
         mas_comparte = "mas_comparte = credito(which(sM == max(sM)))", ancho = "ancho = credito(which(abs(w - max(w)) < 0.01))")
for (a in names(ATJ)) mutantes[[length(mutantes) + 1L]] <- list(id = paste0("T-", a), sonda = "diagnosticidad: tabla de pesos", atajo = a,
  de = ATJ[[a]], a = paste0(a, " = 1"))
pares <- function(msg) strsplit(sub(".*por encima del tope: ", "", msg), ", ")[[1]]
cat("\n=== Mutantes de texto (contrato: sonda declarada, patrón único, semillas hasta la primera muerte) ===\n")
for (m in mutantes) { code <- CODE; n <- lengths(regmatches(code, gregexpr(m$de, code, fixed = TRUE)))
  if (n != 1L) { cat(m$id, "| mal_construido (el patrón aparece", n, "veces)\n"); fallos <- c(fallos, paste(m$id, "mal construido")); next }
  code <- sub(m$de, m$a, code, fixed = TRUE); r <- primera_muerte(code)
  v <- if (is.null(r)) "no_detectado" else if (!grepl(m$sonda, r$msg, fixed = TRUE)) "cazado_por_otra" else if (!is.null(m$atajo)) {
    p <- pares(r$msg); otros <- p[sub(".*: ", "", p) != m$atajo]
    if (length(otros)) paste0("cazado_por_su_sonda_y_otra (", paste(otros, collapse = ", "), ")") else
      if (length(p) > 1L) paste0("cazado_por_su_sonda_y_otra (niveles: ", paste(sub(":.*", "", p), collapse = ", "), ")") else "cazado_por_su_sonda" } else "cazado_por_su_sonda"
  if (v %in% c("no_detectado", "cazado_por_otra")) fallos <- c(fallos, paste(m$id, v))
  cat(sprintf("%-16s | sonda: %-47s | %-40s | semilla %s\n", m$id, m$sonda, substr(v, 1, 90), if (is.null(r)) "-" else r$s)) }
cat("sin_prueba_de_deteccion: «canónica: clave en el contenido de la C oficial» (con las opciones de la Guía y el oráculo, la única opción correcta ya es la C oficial: ningún mutante la mueve sola)\n")
## Pares (nivel, atajo) de la guarda, mutando una COPIA del entorno (obligación 7 del contrato). Forma: sube ese atajo
## en las celdas de la forma hasta 0,1 pp por encima de su tope. Agregado: lleva la forma de tope más alto justo por debajo
## de su tope y le da el 99 % del peso; como cambiar la mezcla mueve también el agregado de los demás atajos, a cada uno
## que suba se le resta lo que subió dentro de esa forma (bajar nunca dispara un tope). Con la mezcla fija el agregado no
## se puede mover solo: queda implicado por los topes de cada forma.
est <- e0$CELDAS$estilo[e0$activas]; Ab <- e0$A_cel; wb <- e0$w_cel; TP <- e0$TOPES; fs <- unique(est)
tasa_f <- function(A, w, f) { en <- est == f; colSums(w[en] * A[en, , drop = FALSE]) / sum(w[en]) }
base <- e0$revisar_tabla(Ab, wb, est)
cat(sprintf("\n=== Pares (nivel, atajo) por mutación del entorno | control sin mutar: %s ===\n", if (length(base)) paste(base, collapse = ", ") else "ningún par por encima del tope"))
if (length(base)) fallos <- c(fallos, "la tabla del diseño ya pasa un tope")
res_p <- character(0)
for (a in e0$ATAJOS) for (nv in c("agregado", fs)) {
  A <- Ab; w <- wb
  if (nv == "agregado") { fst <- fs[which.max(TP[fs, a])]; en <- est == fst
    A[en, a] <- A[en, a] + (TP[fst, a] - 1e-6 - tasa_f(Ab, wb, fst)[[a]]); w <- ifelse(en, 0.99 * wb / sum(wb[en]), 0.01 * wb / sum(wb[!en]))
    for (b in setdiff(colnames(A), a)) { exc <- sum(w * A[, b]) - sum(wb * Ab[, b]); if (exc > 0) A[en, b] <- A[en, b] - exc / sum(w[en]) }
    if (sum(w * A[, a]) <= TP["agregado", a] + 1e-9) { res_p <- c(res_p, "implicado"); cat(sprintf("  %-28s implicado por los topes de cada forma\n", paste0(nv, ": ", a))); next } }
  else { en <- est == nv; A[en, a] <- A[en, a] + (TP[nv, a] + 0.001 - tasa_f(Ab, wb, nv)[[a]]) }
  f <- e0$revisar_tabla(A, w, est); esp <- paste0(nv, ": ", a)
  v <- if (!length(f)) "no_detectado" else if (identical(f, esp)) "cazado_por_su_sonda" else if (esp %in% f) "cazado_por_su_sonda_y_otra" else "cazado_por_otra"
  res_p <- c(res_p, v); if (v != "cazado_por_su_sonda") cat(sprintf("  %-28s %s (%s)\n", esp, v, paste(f, collapse = ", "))) }
cat(sprintf("  %d pares: %s\n", length(res_p), paste(names(table(res_p)), table(res_p), collapse = " · ")))
if (any(!res_p %in% c("cazado_por_su_sonda", "implicado"))) fallos <- c(fallos, "algún par (nivel, atajo) no se caza solo")
## ---------- Veredicto ----------
sin_alc <- sum(cls == "ROJO SIN ALCANCE"); cub <- sum(cls == "ROJO CUBIERTO (alcance a firmar)")
cat(sprintf("\nVEREDICTO_VERIFICADOR: %s | fallos: %d | rojos cubiertos: %d | rojos sin alcance: %d | tiempo %s\n",
            if (length(fallos) || sin_alc) "FALLA" else if (cub) "ROJO_CUBIERTO" else "PASS", length(fallos), cub, sin_alc, format(Sys.time() - t0)))
if (length(fallos)) cat("  -", paste(fallos, collapse = "\n  - "), "\n")
quit(status = if (length(fallos) || sin_alc) 1L else if (cub) 3L else 0L)
