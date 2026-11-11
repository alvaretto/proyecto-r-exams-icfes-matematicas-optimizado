args <- commandArgs(TRUE); rmd <- args[1]; RAIZ <- args[2]; DIRS <- args[3]
suppressMessages(library(exams)); source(file.path(DIRS, "reglas_r3.R")); t0 <- Sys.time()
L <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", L); i1 <- which(L == "```"); i1 <- i1[i1 > i0][1]
CG <- parse(text = paste(L[(i0 + 1):(i1 - 1)], collapse = "\n")); set.seed(1); e0 <- new.env(); eval(CG, e0)
cat("md5:", unname(tools::md5sum(rmd)), "\n")
PES <- e0$PESOS / sum(e0$PESOS); cmb <- list()
for (o in e0$OBJETOS) { K <- e0$SISTEMAS[[o$sis]]$K
  for (d in o$d_cand) for (V in c("P", "p")) for (cel in names(PES)) {
    terna <- sub("\\|.*", "", cel); est <- sub(".*\\|", "", cel); cods <- c("C", sub("VL-", "", strsplit(terna, "+", fixed = TRUE)[[1]], fixed = TRUE))
    KT <- e0$num_tex(K); dT <- e0$num_tex(d); ops <- unname(vapply(cods, e0$expr_de, "", est = est, k = KT, n = dT, v = V))
    cmb[[length(cmb) + 1L]] <- list(w = (1 / length(e0$OBJETOS)) * (1 / length(o$d_cand)) * 0.5 * PES[[cel]], est = est, cel = cel, ops = ops,
      x = list(K = KT, d = dT, V = V, w = unname(vapply(ops, e0$ancho_tex, 0))), Kn = K, dn = d, sis = o$sis) } }
W <- vapply(cmb, `[[`, 0, "w"); EST <- vapply(cmb, `[[`, "", "est")
cat("combinaciones:", length(cmb), "| suma de pesos:", sum(W), "| con opciones repetidas:", sum(vapply(cmb, function(z) anyDuplicated(z$ops) > 0, TRUE)), "\n")
FM <- c("cociente", "cociente_b", "fraccion_nv"); cat("peso por forma:", round(100 * tapply(W, EST, sum), 2), "\n")
porf <- function(cr) c(agregado = sum(W * cr), vapply(FM, function(f) sum(W[EST == f] * cr[EST == f]) / sum(W[EST == f]), 0))
tab <- sapply(names(R3), function(rn) vapply(cmb, function(z) cred(R3[[rn]](z$ops, z$x)), 0))
cat("\n=== EXACTO (enumeración de objeto x distancia x letra x celda, pesos del diseño) — atajos propios y V1, % ===\n")
print(round(100 * t(apply(tab, 2, porf)), 1))
## Por celda (para las reglas clave)
cat("\n=== Por celda (crédito medio de la clave), reglas seleccionadas ===\n")
sel <- c("letra arriba (conjunto)", "contiene x (conjunto)", "NO contiene K/V del enunciado", "ancho TeX (tol 0,01 pt)", "ancho TeX (tol 1 pt)", "V1 medoide", "V1 centralidad")
CEL <- vapply(cmb, `[[`, "", "cel"); print(round(100 * sapply(sel, function(rn) tapply(tab[, rn] * W, CEL, sum) / tapply(W, CEL, sum)), 1))
## Batería congelada B2 exacta: cada combinación en sus 24 órdenes (desempates de which.max/min)
source(file.path(RAIZ, ".claude/scripts/bateria_eliminacion.R"), local = (BE <- new.env()))
ex <- parse(file.path(RAIZ, ".claude/scripts/bateria_referencia_icfes.R")); RE <- new.env(); for (xx in ex) { eval(xx, RE); if (exists("B2", RE, inherits = FALSE)) break }
num_ops <- function(o) as.numeric(regmatches(gsub("\\,", "", o, fixed = TRUE), gregexpr("[0-9]+", gsub("\\,", "", o, fixed = TRUE)))[[1]])
extra <- list(BE$nueva_regla("estimulo: contiene K y d del enunciado", "lexico", function(op, est) vapply(op, function(o) all(c(est$K, est$d) %in% num_ops(o)), TRUE), usa_estimulo = TRUE),
  BE$nueva_regla("estimulo: contiene la relacion K/V y otra operacion", "formato", function(op, est) vapply(op, function(o) grepl(paste0("\\dfrac{", est$K, "}{", est$V, "}"), o, fixed = TRUE) && grepl("\\\\times|\\\\div", o), TRUE), usa_estimulo = TRUE),
  BE$nueva_regla("la mas ancha (TeX)", "magnitud", function(o) { w <- vapply(o, e0$ancho_tex, 0); abs(w - max(w)) < 0.01 }),
  BE$nueva_regla("la mas estrecha (TeX)", "magnitud", function(o) { w <- vapply(o, e0$ancho_tex, 0); abs(w - min(w)) < 0.01 }))
reglas <- c(Filter(function(r) r$familia != "posicion", RE$B2), extra)
perms <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perms(v[-i]), function(p) c(v[i], p))))
P24 <- perms(1:4); n <- length(cmb) * 24L; OPS <- vector("list", n); CL <- integer(n); ESTL <- vector("list", n); WV <- numeric(n); FV <- character(n); j <- 0L
for (z in cmb) for (p in P24) { j <- j + 1L; OPS[[j]] <- paste0("$", z$ops[p], "$"); CL[j] <- which(p == 1L); ESTL[[j]] <- list(K = z$Kn, d = z$dn, V = z$x$V); WV[j] <- z$w / 24; FV[j] <- z$est }
B2T <- t(vapply(reglas, function(r) { M <- BE$.matriz_score(r, OPS, rep(4L, n), 4L, ESTL); cr <- M[cbind(seq_len(n), CL)]
  c(agregado = sum(WV * cr), vapply(FM, function(f) sum(WV[FV == f] * cr[FV == f]) / sum(WV[FV == f]), 0)) }, numeric(4)))
rownames(B2T) <- vapply(reglas, function(r) r$nombre, "")
cat("\n=== EXACTO — batería congelada (sin las de posición, que valen 25 %) + estímulo + ancho/estrecha TeX, % (orden de mayor a menor en el agregado) ===\n")
print(round(100 * B2T[order(-apply(B2T, 1, max)), ], 1)[1:20, ])
cat("tiempo:", format(Sys.time() - t0), "\n")
