#!/usr/bin/env Rscript
## p7f_h2_espacio.R — tanda 3 (2026-10-31), H2 de la FASE 2C r1: enumeración previa del espacio de diseño (§P7-F)
## antes de rediseñar el sorteo de la tabla del otro colegio. (1) Verifica con N = 100 (semillas 1..100) las cifras de
## H2: R_k (k' >= 2 => ganó el de más cursos), R_hg (HG verdadera <=> k' = 1) y R_delta (delta' <= 4 => CURSOS).
## (2) Cuenta las filas viables de espacio_paralelo() (la función del propio .Rmd) por rama, combinación de veredictos
## (hg, hp), k' y franja de delta', en el espacio base (n' <= 8, k' <= 3) y en el ampliado de la cascada (n' <= 10,
## k' <= 4), sin las exclusiones de V11 (que dependen de la Parte 1 de cada versión: cota superior de lo viable).
## Solo lee. Uso (desde cloze/salida): Rscript --no-init-file p7f_h2_espacio.R
suppressMessages(library(exams))
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% c("chunk", "correr")) eval(e, envir = globalenv())
RMD <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(RMD)), "\n")
EXPR <- parse(text = chunk(RMD), encoding = "UTF-8")
R <- t(vapply(1:100, function(z) { en <- correr(EXPR, z); stopifnot(is.null(en$.err))
  c(cursos = en$rama_par == "CURSOS", k = en$dp$k, delta = en$dp$delta, hg = en$v_hg, hp = en$v_hp) }, numeric(5)))
pk <- ifelse(R[, "k"] >= 2, 1, 0)
cat(sprintf("\n(1) N = 100: R_k %.1f %% (CURSOS %.1f %% n = %d; PROMEDIO %.1f %% n = %d) · R_hg %.1f %% · R_delta: delta' <= 4 en %d versiones, CURSOS en %.1f %%\n",
  100 * mean(pk == R[, "cursos"]), 100 * mean((pk == R[, "cursos"])[R[, "cursos"] == 1]), sum(R[, "cursos"] == 1),
  100 * mean((pk == R[, "cursos"])[R[, "cursos"] == 0]), sum(R[, "cursos"] == 0), 100 * mean((R[, "k"] == 1) == R[, "hg"]),
  sum(R[, "delta"] <= 4), 100 * mean(R[R[, "delta"] <= 4, "cursos"])))
en <- correr(EXPR, 1L)
for (esp in list(list("base (n' <= 8, k' <= 3)", 8L, 3L), list("ampliado (n' <= 10, k' <= 4)", 10L, 4L))) {
  cat("\n(2) Filas viables, espacio", esp[[1]], "— sin exclusiones de V11:\n")
  for (rama in c("CURSOS", "PROMEDIO")) {
    g <- en$espacio_paralelo(rama, esp[[2]], esp[[3]])
    g$combo <- paste0("hg=", substr(g$aG > g$D, 1, 1), " hp=", substr(g$aP > g$D, 1, 1))
    g$franja <- cut(g$delta, c(1, 4, 9, 15), labels = c("d'<=4", "d'5-9", "d'10-15"))
    cat(" rama", rama, "\n"); print(addmargins(table(g$combo, paste0("k'=", g$k))))
    cat("  por franja de delta':\n"); print(table(g$combo, g$franja))
  }
}
cat("\nCombinaciones que sortea el .Rmd: CURSOS {hg=T hp=T, hg=F hp=T, hg=F hp=F}; PROMEDIO {hg=T hp=T, hg=T hp=F, hg=F hp=F}.\n")
