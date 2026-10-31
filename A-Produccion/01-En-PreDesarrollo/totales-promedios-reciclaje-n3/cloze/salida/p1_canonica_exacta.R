#!/usr/bin/env Rscript
## p1_canonica_exacta.R — tanda 3 (2026-10-30). La Parte 1 en la canónica, regla por regla y EXACTA (24 órdenes de
## sus opciones verbatim, que son fijas), al aplicar la aclaración firmada el 2026-10-30 («firmar el máximo de una
## batería no cubre las demás reglas»). «O-1 corregido» firmó la canónica como un número de estrato, el máximo de la
## batería (0,10 × 75 pp); este script lista TODAS las reglas que pasan +8 pp en ese estrato, con su cifra exacta:
## la batería §P7 del SCHOICE (B2 + I_REGLAS, la misma que E4) y las sondas de «O-1 corregido». Solo lee.
## Uso (desde cloze/salida): Rscript --no-init-file p1_canonica_exacta.R
suppressMessages(library(exams))
DIR <- normalizePath("..")
SUB <- dirname(DIR); REPO <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
V <- parse(file = file.path(DIR, "verificar_espacio_cloze.R"), keep.source = FALSE, encoding = "UTF-8")
QUIERO <- c("toks","cif1","num_all","tabla_leida","sujeto","rec","chunk","MARCA","cap1","correr","perms","sc")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% QUIERO) eval(e, envir = globalenv())
ENV_S <- new.env(); sys.source(file.path(REPO, ".claude/scripts/bateria_referencia_icfes.R"), envir = ENV_S)
EX_S <- parse(file = file.path(SUB, "verificar_espacio.R"), keep.source = FALSE, encoding = "UTF-8")
Q2 <- c("cif", "sc", "h1", "rango_D", "es_suma", "filtra", "atipica", "sondas", "FIRMA_O1", "FIRMA_S1_PROMEDIO", "FIRMA_LARGO", "hasta", "sede_txt", "grupo", "par_por_sede", "toks3", "I_REGLAS", "NA_FAM")
for (e in EX_S) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% Q2) eval(e, envir = ENV_S)
EXPR <- parse(text = chunk(file.path(DIR, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")), encoding = "UTF-8")
CANON <- NULL; for (z in 1001:1400) { en <- correr(EXPR, z); if (isTRUE(en$es_canonica)) { CANON <- rec(en); break } }
cat("canónica, semilla", z, "\n")
CANON_V <- lapply(perms(1:4), function(p) { x <- CANON; x$opciones <- CANON$opciones[p]; x$sede_o <- CANON$sede_o[p]
  x$rol <- CANON$rol[p]; x$nom_o <- CANON$nom_o[p]; x$clave <- which(p == CANON$clave); x })
TABc <- ENV_S$sondas(CANON_V)
REGLAS <- c(ENV_S$B2, ENV_S$I_REGLAS)
scr <- vapply(REGLAS, function(r) mean(vapply(CANON_V, function(x) {
  S <- tryCatch(get(".conjunto_de", envir = ENV_S)(r$fn(x$opciones), 4L), error = function(e) rep(FALSE, 4)); sc(S, x$clave) }, 0)), 0)
names(scr) <- vapply(REGLAS, function(r) r$nombre, "")
cat("\nBatería §P7 del SCHOICE (", length(REGLAS), " reglas) en la canónica, 24 órdenes (exacta); exceso > +8 pp:\n", sep = "")
o <- order(-scr); for (i in o) if (scr[i] - 0.25 > 0.08) cat(sprintf("  %5.1f %%  %+6.1f pp  peso %+5.2f pp  %s\n", 100 * scr[i], 100 * (scr[i] - .25), 10 * (scr[i] - .25), names(scr)[i]))
cat(sprintf("  (reglas de la batería: %d; por encima de +8 pp: %d; máximo %.1f %%)\n", length(scr), sum(scr - 0.25 > 0.08), 100 * max(scr)))
cat("\nSondas de E4 en la canónica (exactas), sin las filas «(frecuencia)», exceso > +8 pp:\n")
TABc <- as.matrix(TABc); for (nm in rownames(TABc)) { v <- TABc[nm, 1]; if (!grepl("frecuencia", nm) && v - 0.25 > 0.08) cat(sprintf("  %5.1f %%  %+6.1f pp  %s\n", 100 * v, 100 * (v - .25), nm)) }
n <- nchar(CANON$opciones); cat("\nlargos de las opciones de la canónica:", n, "· clave:", CANON$clave, "· margen de la más larga sobre la segunda:", round(100 * (max(n) - sort(n, TRUE)[2]) / sort(n, TRUE)[2], 1), "%\n")
## Fuera de la batería congelada (§P7-C: un canal nuevo a mitad de ciclo se declara, no obliga en este ciclo):
nl <- vapply(CANON_V, function(x) { n <- nchar(x$opciones); sc(n != max(n), x$clave) }, 0)
cat(sprintf("\nFuera de la batería (§P7-C): «descarta la más larga» %.1f %% (%+.1f pp; margen de la más larga sobre la segunda en la canónica: %.1f %%)\n",
            100 * mean(nl), 100 * (mean(nl) - .25), 100 * (max(nchar(CANON$opciones)) - sort(nchar(CANON$opciones), TRUE)[2]) / sort(nchar(CANON$opciones), TRUE)[2]))
cat("md5 del .Rmd del CLOZE:", unname(tools::md5sum(file.path(DIR, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"))), "\n")
