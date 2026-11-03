## ==== S2c (§P7-F): medidas adicionales del candidato ===========================================================
cat("\n[S2c] Medidas adicionales del candidato", S2C_ID, "\n")
lee_iii <- function(x, r) { if (is.na(r)) return(0.25); v <- vapply(x$opciones, cif, 0); h <- which(v %in% r)
  if (length(h) != 1L) return(0.25); sc(x$sede_o != x$sede_o[h], x$clave) }
R_INV <- "resta (iii): la sede de la opción que cita |aG - aP| pierde; azar entre las dos de la otra"
TAB_INV <- mide(setNames(list(function(x) lee_iii(x, delta_t(x))), R_INV))
## D2 deja una opción de G sin cifra: el canal de formato con S_U (descartar la que no cita cifra) y el atajo completo.
R_SC <- c("S_U + descartar la opción sin cifra", "S_U + S1 + suma + atípica + resta + descartar sin cifra")
TAB_SC <- mide(setNames(list(
  function(x) { v <- vapply(x$opciones, cif, 0); sc(filtra(su_set(x), is.na(v)), x$clave) },
  function(x) { v <- vapply(x$opciones, cif, 0); S <- filtra(filtra(filtra(su_set(x), v %in% x$tabla), es_suma(v)), atipica(v))
    sc(filtra(filtra(S, v %in% delta_t(x)), is.na(v)), x$clave) }), R_SC))
writeLines("  S2c · canal de formato con S_U (relevante en D2):"); print(round(100 * TAB_SC, 1))
writeLines("  S2c · lectura (iii) de «resta» (fuera de la batería congelada, §P7-C: solo para comparar candidatos):")
print(round(100 * TAB_INV, 1))
BARR3 <- list()
for (nm_op in names(OPS_EST)) BARR3[[paste(nm_op, "(iii)")]] <- local({ f0 <- OPS_EST[[nm_op]]; function(x) lee_iii(x, f0(celdas(x))) })
TAB_EST3 <- mide(BARR3)
sobre8 <- function(T) { w <- which(T - 0.25 > 0.08 + 1e-9, arr.ind = TRUE); if (!length(w) || !nrow(w)) character(0) else
  sprintf("%s [%s] %.1f %% (%+.1f pp)", rownames(T)[w[, 1]], colnames(T)[w[, 2]], 100 * T[w], 100 * (T[w] - 0.25)) }
S_H1 <- sobre8(TAB_H1); S_INV <- sobre8(TAB_INV)
S_EST <- sobre8(TAB_EST[rownames(TAB_EST) != "|a1 - a2| (i)", , drop = FALSE]); S_EST3 <- sobre8(TAB_EST3)
writeLines(sprintf("  S2c H1 (RS1-RS7): %d cifras > +8 pp", length(S_H1)))
writeLines(sprintf("  S2c barrido (i)/(ii), sin «|a1 - a2| (i)»: %d cifras > +8 pp: %s", length(S_EST), paste(S_EST, collapse = " · ")))
writeLines(sprintf("  S2c barrido (iii): %d cifras > +8 pp: %s", length(S_EST3), paste(S_EST3, collapse = " · ")))
quien_delta <- function(V) { V <- Filter(function(x) !isTRUE(x$canon), V); if (!length(V)) return(c(G = NA, P = NA, ninguna = NA))
  z <- vapply(V, function(x) { v <- vapply(x$opciones, cif, 0); h <- which(v %in% delta_t(x)); if (!length(h)) "ninguna" else x$sede_o[h[1]] }, "")
  100 * c(G = mean(z == "G"), P = mean(z == "P"), ninguna = mean(z == "ninguna")) }
QD <- sapply(list(mezcla = NAT, CURSOS = CU$CURSOS, PROMEDIO = CU$PROMEDIO), quien_delta)
writeLines("  S2c · quién cita δ = |aG - aP| en las no canónicas (%):"); print(round(QD, 1))
TODAS <- c(NAT, CU$CURSOS, CU$PROMEDIO); NC <- Filter(function(x) !isTRUE(x$canon), TODAS)
F1 <- c(semillas = 100 + (S2C_E2_ULT - 1001), abortos = length(grep("^semilla", fallos)),
        padv_delta = sum(vapply(NC, `[[`, TRUE, "s2c_pd")), psin_delta = sum(vapply(NC, `[[`, TRUE, "s2c_sd")),
        cascada = sum(vapply(NC, `[[`, TRUE, "s2c_fb")), no_canonicas = length(NC))
writeLines(sprintf("  S2c F1: %d semillas corridas en E1 y E2; abortos: %d; de las %d no canónicas guardadas, padv con δ: %d, psin con δ: %d, cascada (P sin δ por falta de lado): %d",
                   F1[["semillas"]], F1[["abortos"]], F1[["no_canonicas"]], F1[["padv_delta"]], F1[["psin_delta"]], F1[["cascada"]]))
DIV <- sapply(list(mezcla = Filter(function(x) !isTRUE(x$canon), NAT), CURSOS = CU$CURSOS, PROMEDIO = CU$PROMEDIO), function(V) {
  Dv <- vapply(V, `[[`, 0, "D"); tup <- vapply(V, function(x) paste(x$tabla, collapse = ","), "")
  c(n = length(V), D_distintos = length(unique(Dv)), tablas_distintas = length(unique(tup)),
    D_mas_frecuente_pct = round(100 * max(table(Dv)) / length(Dv), 1), claves_distintas = length(unique(vapply(V, `[[`, "", "clave_txt")))) })
writeLines("  S2c · diversidad sustantiva rápida (no canónicas):"); print(DIV)
O1 <- do.call(rbind, lapply(c("CURSOS", "PROMEDIO"), function(b) {
  fl <- c("S_U la sede de la razón sin sustento pierde", rownames(FIRMA_O1), if (b == "PROMEDIO") "S1 descarta cifras de la tabla")
  lim <- c(0.5, FIRMA_O1[, b], if (b == "PROMEDIO") 0.25 + FIRMA_S1_PROMEDIO)
  data.frame(rama = b, regla = fl, medido = round(100 * TAB[fl, b], 1), firmado = round(100 * lim, 1),
             excede = !hasta(TAB[fl, b], lim), stringsAsFactors = FALSE, row.names = NULL) }))
writeLines(sprintf("  S2c · las 13 cifras de «O-1 corregido»: %d por encima de lo firmado", sum(O1$excede))); print(O1)
D2A <- if (exists("C1")) c(reglas_iguales = sum(vapply(names(FIRMA_P1_CANON), function(nm) isTRUE(round(C1$reglas[[nm]], 1) == FIRMA_P1_CANON[[nm]]), TRUE)),
  sondas_iguales = sum(vapply(names(FIRMA_P1_SONDAS), function(nm) isTRUE(round(C1$sondas[[nm]], 1) == FIRMA_P1_SONDAS[[nm]]), TRUE))) else c(NA, NA)
writeLines(sprintf("  S2c · decisión 2 a: %d de 12 reglas y %d de 12 sondas iguales a lo firmado", D2A[1], D2A[2]))
if (isTRUE(env0$D2_SIN_CIFRA)) {
  v2 <- 0L; n2 <- 0L
  for (b in c("CURSOS", "PROMEDIO")) { esp <- esp_s2c(b); pG <- if (b == "CURSOS") "M" else "P"
    for (i in seq_len(nrow(esp))) { f <- esp[i, ]; d <- list(nG = f$nG, nP = f$nP, aG = f$aG, aP = f$aP)
      for (m in c(if (pG == "M") c("M_a", "M_b") else c("P_a", "P_b"), "N_a", "N_b")) { n2 <- n2 + 1L
        if (env0$verdad(list(molde = m, sede = "G", x = NA_real_, perfil = env0$MOLDES[[m]]$perfil, afirma = "dif_igual_delta"), d)) v2 <- v2 + 1L } } }
  ok(v2 == 0L, sprintf("D2, L-2: la afirmación de ERR-RESTA sin cifra es falsa en las %d configuraciones (%d verdaderas)", n2, v2))
}
codigo_s2c <- if (length(fallos)) 1L else if (length(PENDIENTES)) 3L else if (length(FIRMADOS)) 4L else 0L
saveRDS(list(id = S2C_ID, TAB = TAB, TAB_H1 = TAB_H1, TAB_INV = TAB_INV, TAB_SC = TAB_SC, TAB_EST = TAB_EST, TAB_EST3 = TAB_EST3, APL_EST = APL_EST,
             QD = QD, F1 = F1, DIV = DIV, O1 = O1, D2A = D2A, CUOTA = CUOTA, BAT = BAT_S2C, PENDIENTES = PENDIENTES,
             FIRMADOS = FIRMADOS, fallos = fallos, S_EST = S_EST, S_EST3 = S_EST3, S_H1 = S_H1, S_INV = S_INV,
             E3 = c(combinaciones = sum(filas), violaciones = viol, sin_y = sin_y, fuera_g = fuera_g, textos = n_txt,
                    admiten_total = adm_total, clausula = clausula), codigo = codigo_s2c),
        file.path(S2C_DIR, sprintf("res_%s.rds", S2C_ID)))
cat("\n=== RESULTADO S2c (", S2C_ID, "): ", if (length(fallos)) sprintf("FALLA (%d)", length(fallos)) else if (length(PENDIENTES))
  sprintf("PENDIENTE DE FIRMA (%d)", length(PENDIENTES)) else if (length(FIRMADOS)) "EN ROJO bajo override firmado" else "sin canales en rojo",
  " ===\n", sep = "")
if (length(fallos)) { cat("FALLAS:\n"); cat(paste0("  - ", fallos, "\n"), sep = "") }
quit(status = codigo_s2c)
