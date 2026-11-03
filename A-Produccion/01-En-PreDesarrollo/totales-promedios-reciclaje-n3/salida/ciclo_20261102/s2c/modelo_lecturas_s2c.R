## Modelo de las tres lecturas de «resta» en un ítem 2 + 2 (explicación, no fuente de cifras): si δ lo cita la opción de G en
## una fracción aG de las versiones y una de P en aP, el acierto es (i) 25 + 75·aG − 25·aP, (ii) 25 + (aG + aP)·25/3 y
## (iii) 25 + 25·(aP − aG). Se coteja con lo medido por verificar_s2c.R (res_<ID>.rds), rama por rama.
ids <- c("B0", "D1c40", "D1q50", "D1q100", "D3", "D3b", "D4")
for (i in ids) { r <- readRDS(sprintf("res_%s.rds", i))
  for (b in c("CURSOS", "PROMEDIO")) { aG <- r$QD["G", b] / 100; aP <- r$QD["P", b] / 100
    pred <- c(i = 25 + 75 * aG - 25 * aP, ii = 25 + (aG + aP) * 25 / 3, iii = 25 + 25 * (aP - aG))
    med <- 100 * c(i = r$TAB_H1[1, b], ii = r$TAB_EST["|a1 - a2| (ii)", b], iii = r$TAB_INV[1, b])
    cat(sprintf("%-6s %-8s aG %3.0f %% aP %3.0f %% · (i) modelo %5.1f medido %5.1f · (ii) %5.1f / %5.1f · (iii) %5.1f / %5.1f\n",
                i, b, 100 * aG, 100 * aP, pred[["i"]], med[["i"]], pred[["ii"]], med[["ii"]], pred[["iii"]], med[["iii"]])) } }
## Con aP = 2·aG, las tres lecturas quedan en 25 + s·25/3 con s = aG + aP: el mínimo común de (i) y (iii).
r0 <- readRDS("lote1/res_B0.rds"); r1 <- readRDS("res_B0.rds")
cat("B0, lote 1 frente a lote 2 (TAB, TAB_H1, TAB_EST, PENDIENTES):", identical(r0$TAB, r1$TAB) && identical(r0$TAB_H1, r1$TAB_H1) &&
    identical(r0$TAB_EST, r1$TAB_EST) && identical(r0$PENDIENTES, r1$PENDIENTES), "\n")
rd <- readRDS("res_D0.rds"); cat("D0 frente a B0 (TAB, TAB_H1, TAB_INV, TAB_EST, TAB_EST3, PENDIENTES):", identical(rd$TAB, r1$TAB) &&
    identical(rd$TAB_H1, r1$TAB_H1) && identical(rd$TAB_INV, r1$TAB_INV) && identical(rd$TAB_EST, r1$TAB_EST) &&
    identical(rd$TAB_EST3, r1$TAB_EST3) && identical(rd$PENDIENTES, r1$PENDIENTES), "\n")
