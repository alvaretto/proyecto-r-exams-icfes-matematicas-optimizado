## Construye tabla_decision_s2c.md a partir de res_<ID>.rds (salidas de verificar_s2c.R). Ninguna cifra se transcribe.
ids <- c("B0", "D0", "D1c40", "D1q50", "D1q100", "D3", "D3b", "D4", "D2")
R <- setNames(lapply(ids, function(i) { f <- sprintf("res_%s.rds", i); if (file.exists(f)) readRDS(f) else NULL }), ids)
R <- Filter(Negate(is.null), R)
p1 <- function(x) ifelse(is.na(x), "—", sprintf("%.1f", 100 * x))
ex <- function(x) ifelse(is.na(x), "—", sprintf("%+.1f", 100 * (x - 0.25)))
cel <- function(r, T, fila, cols = c("mezcla", "CURSOS", "PROMEDIO", "canonica")) paste(p1(T[fila, cols]), collapse = " / ")
RS <- rownames(R[[1]]$TAB_H1); RINV <- rownames(R[[1]]$TAB_INV)
S7 <- "S7 G real + menor cifra del par (cota canal 2)"; C2 <- "canal 2: cifra falsa de G menor que D"
LUG <- c("D es la menor cifra (frecuencia)", "D es la 2.ª cifra (frecuencia)", "D es la 3.ª cifra (frecuencia)", "D es la 4.ª cifra (frecuencia)")
out <- c("# Tanda S2c (§P7-F): tabla de decisión, generada por `tabla_decision_s2c.R` desde `res_<ID>.rds`", "",
  "Acierto en %, mezcla de E1 / rama CURSOS / rama PROMEDIO / canónica exacta (24 órdenes), N = 100 por estrato.", "")
out <- c(out, "## 1. «resta» y sus lecturas", "",
  "| Candidato | RS1 (i) | (ii) descartar la que cita δ | (iii) la sede que cita δ pierde | RS2 S_U + resta | RS7 S_U + 3 pistas + resta | S_U + sin cifra | S_U + 3 pistas + resta + sin cifra |",
  "|---|---|---|---|---|---|---|---|")
for (i in names(R)) { r <- R[[i]]
  out <- c(out, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |", i, cel(r, r$TAB_H1, RS[1]), cel(r, r$TAB_EST, "|a1 - a2| (ii)"),
                        cel(r, r$TAB_INV, RINV[1]), cel(r, r$TAB_H1, RS[2]), cel(r, r$TAB_H1, RS[7]),
                        cel(r, r$TAB_SC, rownames(r$TAB_SC)[1]), cel(r, r$TAB_SC, rownames(r$TAB_SC)[2]))) }
out <- c(out, "", "## 2. Quién cita δ en las no canónicas (%, G / P / ninguna) y tipo de error del distractor de G (%, por rama)", "",
  "| Candidato | Mezcla | CURSOS | PROMEDIO | Distractor de G, CURSOS (DATO/RESTA/PARCIAL/TOTAL) | Distractor de G, PROMEDIO |", "|---|---|---|---|---|---|")
for (i in names(R)) { r <- R[[i]]; q <- round(r$QD, 1); cu <- round(r$CUOTA, 1)
  out <- c(out, sprintf("| %s | %s | %s | %s | %s | %s |", i, paste(q[, "mezcla"], collapse = " / "), paste(q[, "CURSOS"], collapse = " / "),
                        paste(q[, "PROMEDIO"], collapse = " / "), paste(cu[, "CURSOS"], collapse = " / "), paste(cu[, "PROMEDIO"], collapse = " / "))) }
out <- c(out, "", "## 3. Cifras firmadas, canal 2, lugar de D y batería", "",
  "| Candidato | O-1 corregido: cifras por encima de lo firmado (de 13) | Decisión 2 a: iguales (de 24) | S7 canal 2, CURSOS / PROMEDIO (exceso sobre 50 %) | Cifra falsa de G bajo D, CURSOS / PROMEDIO | Lugar de D 1.º-4.º, CURSOS | Lugar de D 1.º-4.º, PROMEDIO | Batería §P7, exceso mezcla / CURSOS / PROMEDIO |",
  "|---|---|---|---|---|---|---|---|")
for (i in names(R)) { r <- R[[i]]; o <- r$O1
  oe <- if (any(o$excede)) paste0(sum(o$excede), ": ", paste(sprintf("%s %s %.1f > %.1f", o$rama[o$excede], sub(" \\(.*$", "", o$regla[o$excede]), o$medido[o$excede], o$firmado[o$excede]), collapse = "; ")) else "0"
  s7 <- sprintf("%+.1f / %+.1f", 100 * (r$TAB[S7, "CURSOS"] - .5), 100 * (r$TAB[S7, "PROMEDIO"] - .5))
  c2 <- sprintf("%.0f / %.0f", 100 * r$TAB[C2, "CURSOS"], 100 * r$TAB[C2, "PROMEDIO"])
  lc <- paste(sprintf("%.0f", 100 * r$TAB[LUG, "CURSOS"]), collapse = "-"); lp <- paste(sprintf("%.0f", 100 * r$TAB[LUG, "PROMEDIO"]), collapse = "-")
  bt <- paste(vapply(c("mezcla natural", "rama CURSOS", "rama PROMEDIO"), function(k) if (is.null(r$BAT[[k]])) "—" else
    sprintf("%+.1f (%s)", 100 * r$BAT[[k]]$exceso, r$BAT[[k]]$veredicto), ""), collapse = " / ")
  out <- c(out, sprintf("| %s | %s | %d | %s | %s | %s | %s | %s |", i, oe, sum(r$D2A), s7, c2, lc, lp, bt)) }
out <- c(out, "", "## 4. Barrido del estímulo, L-2, F1, diversidad y resultado del verificador", "",
  "| Candidato | Barrido (i)/(ii) > +8 pp (sin «resta») | Barrido (iii) > +8 pp | L-2 enumeración (combinaciones / violaciones) | Abortos / semillas | δ en P: padv / psin / cascada | D distintos, mezcla / CURSOS / PROMEDIO | Claves distintas | Pendientes | Fallos | exit |",
  "|---|---|---|---|---|---|---|---|---|---|---|")
for (i in names(R)) { r <- R[[i]]; f <- r$F1; dv <- r$DIV
  out <- c(out, sprintf("| %s | %d | %d | %d / %d | %d / %d | %d / %d / %d | %s | %s | %d | %d | %d |", i, length(r$S_EST), length(r$S_EST3),
    r$E3[["combinaciones"]], r$E3[["violaciones"]], f[["abortos"]], f[["semillas"]], f[["padv_delta"]], f[["psin_delta"]], f[["cascada"]],
    paste(dv["D_distintos", ], collapse = " / "), paste(dv["claves_distintas", ], collapse = " / "), length(r$PENDIENTES), length(r$fallos), r$codigo)) }
out <- c(out, "", "## 5. Detalle: RS1 a RS7 por candidato", "")
for (i in names(R)) { r <- R[[i]]; out <- c(out, sprintf("**%s**", i), "", "| Regla | Mezcla | CURSOS | PROMEDIO | Canónica |", "|---|---|---|---|---|")
  for (k in seq_along(RS)) out <- c(out, sprintf("| RS%d | %s |", k, paste(sprintf("%s (%s)", p1(r$TAB_H1[RS[k], ]), ex(r$TAB_H1[RS[k], ])), collapse = " | ")))
  out <- c(out, "") }
out <- c(out, "## 6. Detalle: cifras del barrido por encima de +8 pp, por candidato", "")
for (i in names(R)) { r <- R[[i]]; out <- c(out, sprintf("- **%s**, (i)/(ii): %s", i, if (length(r$S_EST)) paste(r$S_EST, collapse = "; ") else "ninguna"),
                                          sprintf("  - (iii): %s", if (length(r$S_EST3)) paste(r$S_EST3, collapse = "; ") else "ninguna")) }
out <- c(out, "", "## 7. Fallos del verificador por candidato", "")
for (i in names(R)) { r <- R[[i]]; out <- c(out, sprintf("- **%s**: %s", i, if (length(r$fallos)) paste(r$fallos, collapse = " · ") else "ninguno")) }
writeLines(out, "tabla_decision_s2c.md", useBytes = TRUE)
cat("tabla_decision_s2c.md:", length(out), "líneas,", length(R), "candidatos\n")
