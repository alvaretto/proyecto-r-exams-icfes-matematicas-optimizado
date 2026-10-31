#!/usr/bin/env Rscript
## contrastar_h2_ponderacion.R — tanda 5 (2026-10-31), paso A. Dos contrastes de la puerta L-C5 (enumerar_h2_cloze.R):
##   (a) Ponderación: semilla por semilla, la enumeración conducida por el código frente al modelo TRANSCRITO de
##       medir_reparto_h2.R (rama 1/2, combinación 1/3, fila uniforme, fila del ganador 1/2, v_c y redacción de ventaja
##       como el código), ahora CON las exclusiones de V11 de esa Parte 1 y la cascada F1. Sin exclusiones, ese modelo es
##       exactamente V0 (la cifra firmada). Si coinciden, la diferencia con lo firmado sale solo de las exclusiones.
##   (b) Ruido de la Parte 1: la tabla con las semillas 1 a 50 frente a la de 51 a 100.
## Solo lee. Uso (desde cloze/salida): Rscript --no-init-file contrastar_h2_ponderacion.R
suppressMessages(library(exams))
stopifnot(basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% c("chunk", "correr", "MARCA", "FIRMA_H2"))
  eval(e, envir = globalenv())
EB <- new.env(); sys.source("../bateria_partes_2_a_6.R", envir = EB, keep.source = FALSE)
source("../enumerar_h2_cloze.R", encoding = "UTF-8")
RMD <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"); COD <- chunk(RMD)
EXPR <- parse(text = COD, encoding = "UTF-8"); cat("md5 del .Rmd:", unname(tools::md5sum(RMD)), "\n")
perms <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perms(v[-i]), function(p) c(v[i], p))))
transcrito <- function(e1) {
  out <- list()
  for (rama in c("CURSOS", "PROMEDIO")) for (pm in perms(1:3)) {
    for (ip in seq_along(e1$PASOS_CASCADA)) { pc <- e1$PASOS_CASCADA[[ip]]; vh <- e1$COMBOS_H[[rama]][[pm[pc$h]]]
      ep <- e1$espacio_paralelo(rama, pc$n, pc$k); ep <- ep[(ep$aG > ep$D) == vh[["hg"]] & (ep$aP > ep$D) == vh[["hp"]], ]
      ep <- ep[!(ep$nG %in% e1$prohib_tabla_par | ep$nP %in% e1$prohib_tabla_par | ep$aG %in% e1$prohib_tabla_par | ep$aP %in% e1$prohib_tabla_par) &
               !(ep$TG %in% e1$prohib_resp_par | ep$TP %in% e1$prohib_resp_par | ep$D %in% e1$prohib_resp_par), ]
      if (nrow(ep)) break }
    stopifnot(nrow(ep) > 0)
    vcs <- if (vh[["hg"]] == vh[["hp"]]) list(c(v = !vh[["hg"]], w = 1)) else list(c(v = TRUE, w = 0.5), c(v = FALSE, w = 0.5))
    for (vc in vcs) { cand <- Filter(function(z) (z$si_cursos == (rama == "CURSOS")) == as.logical(vc[["v"]]), e1$P6_VENTAJA)
      for (c6 in cand) out[[length(out) + 1L]] <- data.frame(rama = rama, nG = rep(ep$nG, 2), nP = rep(ep$nP, 2), aG = rep(ep$aG, 2), aP = rep(ep$aP, 2),
        gi = rep(1:2, each = nrow(ep)), v_hp = vh[["hp"]], v_hg = vh[["hg"]], v_c = as.logical(vc[["v"]]), v_d = as.logical(vc[["v"]]),
        hp_txt = e1$P6_HP, hg_txt = e1$P6_HG, c6_txt = c6$texto, cascada = ip,
        w = 0.5 / 6 / nrow(ep) / 2 * vc[["w"]] / length(cand), stringsAsFactors = FALSE) }
  }
  do.call(rbind, out)
}
clave <- function(A) paste(A$rama, A$nG, A$nP, A$aG, A$aP, A$gi, A$v_hp, A$v_hg, A$v_c, A$v_d, A$c6_txt)
dmax <- 0; TR <- list(); t0 <- Sys.time()
for (z in 1:100) {
  e1 <- correr(EXPR, z); stopifnot(is.null(e1$.err))
  At <- h2_agregar(transcrito(e1)); Ec <- enumerar_h2(COD, MARCA, z); stopifnot(Ec$masa_err == 0); Ac <- h2_agregar(Ec$filas)
  m <- merge(data.frame(k = clave(At), wt = At$w), data.frame(k = clave(Ac), wc = Ac$w), all = TRUE)
  m$wt[is.na(m$wt)] <- 0; m$wc[is.na(m$wc)] <- 0; dmax <- max(dmax, abs(m$wt - m$wc)); At$z <- z; TR[[z]] <- At
}
cat(sprintf("(a) 100 Partes 1: máxima diferencia de peso por fila, código frente a transcrito con exclusiones = %.3g · %s\n", dmax, format(Sys.time() - t0)))
mitad <- function(zz) { A <- do.call(rbind, TR[zz]); A$w <- A$w / length(zz); h2_agregar(A) }
J1 <- h2_juzgar(h2_tabla(mitad(1:50), EB), FIRMA_H2); J2 <- h2_juzgar(h2_tabla(mitad(51:100), EB), FIRMA_H2)
JJ <- merge(J1[, c("estrato", "base", "papel", "exceso", "firma")], J2[, c("estrato", "base", "papel", "exceso")], by = c("estrato", "base", "papel"))
JJ <- JJ[!is.na(JJ$firma) | pmax(JJ$exceso.x, JJ$exceso.y) > 0.08, ]
JJ$s1_50 <- round(100 * JJ$exceso.x, 1); JJ$s51_100 <- round(100 * JJ$exceso.y, 1); JJ$media <- round(50 * (JJ$exceso.x + JJ$exceso.y), 1)
JJ$semidif <- round(50 * abs(JJ$exceso.x - JJ$exceso.y), 1)
cat("\n(b) Ruido de la Parte 1: semillas 1 a 50 frente a 51 a 100 (pp; semidif ≈ error típico de la media de 100)\n")
print(JJ[order(JJ$estrato, JJ$base, JJ$papel), c("estrato", "base", "papel", "firma", "s1_50", "s51_100", "media", "semidif")], row.names = FALSE)
cat(sprintf("\nsemidiferencia mediana %.1f pp, máxima %.1f pp\n", median(JJ$semidif), max(JJ$semidif)))
