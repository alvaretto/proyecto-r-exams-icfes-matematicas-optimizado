## §P7-F, enumeración EXACTA del espacio de diseño de la cifra δ = |aG - aP| (tanda S2c).
## Recorre todas las combinaciones (nG, nP, aG, aP) de cada rama que admite espacio_rama() del .Rmd (md5 63e2713e),
## cada b_debajo (lugar de D) y cada reparto de lados de G y de la razón sin sustento, y cuenta en cuántas puede ir δ
## en cada papel sin romper L-2 (la cifra nunca es D), el canal 8 (cifras distintas), el canal 3 (un solo dato de la
## tabla en kilos), el canal 2 (lado sorteado) ni el lugar de D. Sin RNG: todo es conteo.
rmd <- "../../../totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd"
stopifnot(unname(tools::md5sum(rmd)) == "63e2713e6ac0bdbf250f4dc33901b2ec")
l <- readLines(rmd, encoding = "UTF-8"); i0 <- grep("^```\\{r data_generation", l); ci <- which(l == "```"); i1 <- ci[ci > i0][1]
e <- new.env(); set.seed(1); eval(parse(text = paste(l[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8"), e)
REP <- list(c(gdis = "encima", psin = "encima"), c(gdis = "debajo", psin = "encima"),
            c(gdis = "encima", psin = "debajo"), c(gdis = "debajo", psin = "debajo"))
res <- list()
for (rama in c("CURSOS", "PROMEDIO")) {
  g <- e$espacio_rama(rama); n <- nrow(g)
  ## G sin δ: candidatos de G por lado (como cand_g, sin ERR-RESTA)
  g_sin_d <- function(f, lado) { x <- c(if (!f$p_tabla) c(f$aG, f$aP), f$parcialG, f$TG); x <- x[x != f$D]
    if (lado == "debajo") any(x < f$D) else any(x > f$D) }
  g_con_d <- function(f, lado) { x <- c(if (!f$p_tabla) c(f$aG, f$aP), f$delta, f$parcialG, f$TG); x <- x[x != f$D]
    if (lado == "debajo") any(x < f$D) else any(x > f$D) }
  cnt <- c(versiones_b = 0, original = 0, G_resta_forzosa = 0, psin_delta = 0, D4_factible = 0, padv_delta_mismo_lado = 0)
  for (i in seq_len(n)) { f <- g[i, ]; ld <- if (f$delta < f$D) "debajo" else "encima"; lp <- if (f$parcialP < f$D) "debajo" else "encima"
    for (b in 0:3) {
      faltan <- b - (lp == "debajo"); if (faltan < 0 || faltan > 2) next
      reps <- Filter(function(z) sum(z == "debajo") == faltan, REP)
      ok_o <- Filter(function(z) g_con_d(f, z[["gdis"]]), reps); if (!length(ok_o)) next
      cnt["versiones_b"] <- cnt["versiones_b"] + 1                              # (combinación, b) que el generador usa
      cnt["original"] <- cnt["original"] + length(ok_o) / length(ok_o)
      ## ¿Algún reparto exige ERR-RESTA en G (δ es la única cifra de G de su lado)?
      if (any(vapply(ok_o, function(z) !g_sin_d(f, z[["gdis"]]), TRUE))) cnt["G_resta_forzosa"] <- cnt["G_resta_forzosa"] + 1
      ## D1 (PROMEDIO) / D1 variante: psin = δ con algún reparto compatible y G sin δ de su lado
      if (any(vapply(ok_o, function(z) z[["psin"]] == ld && g_sin_d(f, z[["gdis"]]), TRUE))) cnt["psin_delta"] <- cnt["psin_delta"] + 1
      ## D4: G sin δ en algún reparto
      if (any(vapply(ok_o, function(z) g_sin_d(f, z[["gdis"]]), TRUE))) cnt["D4_factible"] <- cnt["D4_factible"] + 1
      ## padv = δ sin mover el lugar de D: δ del mismo lado de D que parcialP
      if (ld == lp) cnt["padv_delta_mismo_lado"] <- cnt["padv_delta_mismo_lado"] + 1
    } }
  res[[rama]] <- c(combinaciones = n, delta_bajo_D_pct = round(100 * mean(g$delta < g$D), 1),
                   parcialP_bajo_D_pct = round(100 * mean(g$parcialP < g$D), 1),
                   G_bajo_D_sin_delta_pct = round(100 * mean(vapply(seq_len(n), function(i) g_sin_d(g[i, ], "debajo"), TRUE)), 1),
                   cnt, round(100 * cnt[-1] / cnt[["versiones_b"]], 1))
}
out <- do.call(cbind, res)
rownames(out)[(nrow(out) - 4):nrow(out)] <- paste0(rownames(out)[(nrow(out) - 4):nrow(out)], "_pct")
print(out)
## Textos del lado de P que pueden citar δ, por rama, con su función de verdad (deben ser falsos en todo el espacio)
af <- function(m, sede, x) list(molde = m, sede = sede, x = x, perfil = e$MOLDES[[m]]$perfil, afirma = e$MOLDES[[m]]$afirma)
for (rama in c("CURSOS", "PROMEDIO")) { g <- e$espacio_rama(rama); pP <- if (rama == "CURSOS") "P" else "M"
  for (m in c(if (pP == "P") c("P_a", "P_b") else c("M_a", "M_b"), "U_a", "U_b", "U_c", "U_d")) {
    v <- sum(vapply(seq_len(nrow(g)), function(i) e$verdad(af(m, "P", g$delta[i]), as.list(g[i, c("nG", "nP", "aG", "aP")])), TRUE))
    cat(sprintf("rama %-8s P cita δ con el molde %-4s: verdadera en %d de %d combinaciones\n", rama, m, v, nrow(g))) }
  ## La frase de la C del impreso (P_ven, «tuvo mayor promedio con δ kilos de más») atribuida a P en la rama PROMEDIO:
  if (rama == "PROMEDIO") { v <- sum(vapply(seq_len(nrow(g)), function(i) { d <- as.list(g[i, c("nG", "nP", "aG", "aP")])
      d$aP > d$aG }, TRUE)); cat(sprintf("rama PROMEDIO: P tiene el mayor promedio en %d de %d combinaciones (la frase de la C no le sirve)\n", v, nrow(g))) } }
