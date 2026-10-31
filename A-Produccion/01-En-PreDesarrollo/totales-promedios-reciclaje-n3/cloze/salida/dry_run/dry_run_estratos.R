## Seguimiento de solo lectura: factibilidad del caso paralelo por rama de la Parte 1 x rama paralela,
## y con el veredicto de la hipótesis de la Parte 6 sorteado primero (F1). Reutiliza dry_run_espacio.R.
src <- readLines("dry_run_espacio.R", encoding = "UTF-8")
corte <- grep("^## --- Peso de la Parte 1", src) - 1L
eval(parse(text = src[1:corte], encoding = "UTF-8"))
cat("\n=== Por rama de la Parte 1 x rama paralela (válidas tras V11) ===\n")
rP1 <- vapply(p1, `[[`, "", "rama")
for (r1 in c("CURSOS", "PROMEDIO")) for (rp in names(EP)) {
  idx <- which(rP1 == r1)
  ok <- vapply(p1[idx], function(x) factible(x, EP[[rp]])[["ok"]], 0)
  cat(sprintf("  P1 %-8s (n = %3d) x paralela %-8s: mín %4d, p5 %4d, mediana %4d\n", r1, length(idx), rp, min(ok),
              as.integer(quantile(ok, .05)), as.integer(median(ok))))
}
peor <- which.min(vapply(p1, function(x) factible(x, EP$PROMEDIO)[["ok"]], 0))
cat("  Versión más restringida: rama", p1[[peor]]$rama, "canónica", p1[[peor]]$canon, "| cifras de opciones:",
    paste(p1[[peor]]$ops, collapse = ","), "| tabla:", paste(p1[[peor]]$tab, collapse = ","), "\n")
cat("\n=== Veredicto de la hipótesis sorteado primero: combinaciones por versión ===\n")
for (rp in names(EP)) for (h in c("perdedor+1 (aP > D)", "ganador-1 (aG > D)")) for (v in c(TRUE, FALSE)) {
  g <- EP[[rp]]; cond <- if (startsWith(h, "perdedor")) g$aP > g$D else g$aG > g$D
  g <- g[cond == v, ]
  ok <- vapply(p1, function(x) factible(x, g)[["ok"]], 0)
  cat(sprintf("  paralela %-8s %-20s = %-5s: mín %4d, mediana %4d, versiones sin candidatos %d/%d\n",
              rp, h, v, min(ok), as.integer(median(ok)), sum(ok == 0), length(ok)))
}
