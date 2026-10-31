#!/usr/bin/env Rscript
## medir_reparto_h2.R — tanda 4 (2026-10-31), paso B: el reparto autorizado de H2 («1 a»), medido con la batería
## L-C4 CONGELADA (md5 comprobado), ANTES de tocar el .Rmd (§P7-F). Enumeración EXACTA del espacio de
## espacio_paralelo() (la función del propio .Rmd, espacio base n′ ≤ 8, k′ ≤ 3), sin las exclusiones de V11 (que
## dependen de la Parte 1: es la cota de lo viable; la cascada F1 se mide aparte con la Parte 1 real).
## Variantes:
##   V0  diseño actual (md5 96cfcf35): rama 1/2, combinación de veredictos 1/3, fila uniforme (contraste).
##   V1  REPARTO AUTORIZADO, literal: rama 1/2; k′ ∈ {1, 2} con 1/2 (misma masa en las dos ramas); k′ = 1 solo en
##       CURSOS con HG y HP verdaderas y en PROMEDIO con HG y HP falsas; k′ = 2, combinación 1/3; δ′ ≥ 5; fila uniforme.
##   V2  V1 con δ′ equilibrado: dentro de cada k′, δ′ uniforme sobre los valores que tienen TODAS las celdas de ese k′
##       (δ′ independiente de la rama y de los veredictos dado k′); fila uniforme dentro de (celda, δ′).
##   V3  V1 con el número de cursos del grado de menos cursos equilibrado del mismo modo (en lugar de δ′).
## Dentro de cada fila: fila del ganador 1/2; v_c como en el .Rmd (si HG = HP, el contrario; si no, 1/2); plantilla de
## ventaja 1/2 entre sus dos candidatas. Las reglas del ORDEN de las afirmaciones (P6-P2) son exactamente nulas por
## construcción (o6 <- sample.int(4L), uniforme): no se evalúan aquí. Solo lee.
## Uso (desde cloze/salida): Rscript --no-init-file medir_reparto_h2.R
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
MD5_BATERIA <- "de20e0a986f86d44f0d3412517d2f07b"
stopifnot("la batería L-C4 es la congelada" = unname(tools::md5sum("../bateria_partes_2_a_6.R")) == MD5_BATERIA)
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% c("chunk", "correr"))
  eval(e, envir = globalenv())
source("../bateria_partes_2_a_6.R", encoding = "UTF-8")
RMD <- normalizePath("../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
cat("md5 del .Rmd:", unname(tools::md5sum(RMD)), "· batería", BATERIA_VERSION, "md5", MD5_BATERIA, "\n")
EXPR <- parse(text = chunk(RMD), encoding = "UTF-8")
en <- correr(EXPR, 1L); stopifnot(is.null(en$.err))
NOM <- c("sexto", "noveno")
espacio <- function(rama, n_max = 8L, k_max = 3L) { g <- en$espacio_paralelo(rama, n_max, k_max); g$hg <- g$aG > g$D; g$hp <- g$aP > g$D; g$rama <- rama; g }
G <- rbind(espacio("CURSOS"), espacio("PROMEDIO"))
celda <- function(g, rama, hg, hp, k = NULL, dmin = 2L) { s <- g$rama == rama & g$hg == hg & g$hp == hp & g$delta >= dmin; if (!is.null(k)) s <- s & g$k == k; g[s, ] }
## Celdas de cada variante: lista de (filas, masa de la celda).
celdas_V0 <- function(g) unlist(lapply(c("CURSOS", "PROMEDIO"), function(r) lapply(en$COMBOS_H[[r]], function(h)
  list(f = celda(g, r, h[["hg"]], h[["hp"]]), m = 0.5 / 3))), recursive = FALSE)
celdas_V1 <- function(g) {
  out <- list(list(f = celda(g, "CURSOS", TRUE, TRUE, 1L, 5L), m = 0.25), list(f = celda(g, "PROMEDIO", FALSE, FALSE, 1L, 5L), m = 0.25))
  for (r in c("CURSOS", "PROMEDIO")) for (h in en$COMBOS_H[[r]]) out[[length(out) + 1L]] <- list(f = celda(g, r, h[["hg"]], h[["hp"]], 2L, 5L), m = 0.25 / 3)
  out
}
## Equilibrar una columna dentro de cada k′: valor uniforme sobre el soporte común de las celdas de ese k′.
equilibrar <- function(cl, col) {
  ks <- vapply(cl, function(c) c$f$k[1], 0)
  out <- list()
  for (kk in unique(ks)) {
    cc <- cl[ks == kk]; com <- Reduce(intersect, lapply(cc, function(c) unique(c$f[[col]])))
    if (!length(com)) { cat(sprintf("  k′ = %d: las celdas no comparten ningún valor de %s\n", kk, col)); next }
    for (c in cc) for (v in com) out[[length(out) + 1L]] <- list(f = c$f[c$f[[col]] == v, ], m = c$m / length(com))
  }
  out
}
## Registros de la Parte 6 de una celda, con su peso.
registros <- function(cl) {
  L <- list(); W <- numeric(0)
  for (c in cl) { f <- c$f; if (!nrow(f)) { cat("  celda vacía\n"); next }
    for (j in seq_len(nrow(f))) { r <- f[j, ]
      vcs <- if (r$hg == r$hp) list(c(v = !r$hg, w = 1)) else list(c(v = TRUE, w = 0.5), c(v = FALSE, w = 0.5))
      for (gi in 1:2) for (vc in vcs) {
        cand <- Filter(function(z) (z$si_cursos == (r$rama == "CURSOS")) == as.logical(vc[["v"]]), en$P6_VENTAJA)
        for (c6 in cand) {
          gan <- NOM[gi]; per <- NOM[3L - gi]
          L[[length(L) + 1L]] <- list(p6 = list(op = c(en$P6_HP, en$P6_HG, c6$texto, paste0("Ganó el concurso el grado ", if (vc[["v"]]) gan else per, ".")),
                                               v = c(r$hp, r$hg, as.logical(vc[["v"]]), as.logical(vc[["v"]])), rol = c("hp", "hg", "ventaja", "ganador"),
                                               n = if (gi == 1L) c(r$nG, r$nP) else c(r$nP, r$nG), a = if (gi == 1L) c(r$aG, r$aP) else c(r$aP, r$aG), nombres = NOM),
                                     rama = r$rama, k = r$k, delta = r$delta, n_lo = min(r$nG, r$nP))
          W <- c(W, c$m / nrow(f) * 0.5 * vc[["w"]] / length(cand))
        }
      }
    }
  }
  list(L = L, w = W / sum(W))
}
SIN_ORDEN <- c("P6-P2", "P6-P2c")
informe <- function(nombre, cl) {
  R <- registros(cl); L <- R$L; w <- R$w
  cat(sprintf("\n==================== %s · %d registros ====================\n", nombre, length(L)))
  stopifnot("control positivo: la batería lee bien las afirmaciones" = control_p6(L) == 1)
  est <- list(mezcla = rep(TRUE, length(L)), rama_CURSOS = vapply(L, function(x) x$rama == "CURSOS", NA), rama_PROMEDIO = vapply(L, function(x) x$rama == "PROMEDIO", NA),
              k1 = vapply(L, function(x) x$k == 1, NA), k2mas = vapply(L, function(x) x$k >= 2, NA))
  out <- list()
  for (e in names(est)) { s <- est[[e]]; if (!any(s)) next
    M <- medir_p6(L[s], w[s]); M <- M[!M$id %in% SIN_ORDEN, ]
    S <- sobre_umbral(M); out[[e]] <- M
    vv <- function(pp) vapply(L[s], function(x) x$p6$v[x$p6$rol == pp], NA)
    ws <- w[s] / sum(w[s])
    cat(sprintf("\n[%s] masa %.3f · HG verdadera %.1f %% · HP verdadera %.1f %% · ganó el de más cursos %.1f %% · δ′ ≥ 10 en %.1f %%\n", e, sum(w[s]),
                100 * sum(ws * vv("hg")), 100 * sum(ws * vv("hp")), 100 * sum(ws * vapply(L[s], function(x) x$rama == "CURSOS", NA)),
                100 * sum(ws * vapply(L[s], function(x) x$delta >= 10, NA))))
    for (id in c("P6-K1", "P6-HK1", "P6-D1", "P6-D2", "P6-N1", "P6-A1")) {
      m <- M[M$id %in% c(id, paste0(id, "c")) & M$papel %in% (if (id == "P6-HK1") "hg" else c("ventaja", "ganador")), ]
      b <- m[which.max(abs(m$exceso)), ]
      cat(sprintf("  %-6s %-8s %5.1f %% (nulo %4.1f %%, %+5.1f pp, aplica %3.0f %%)  %s\n", b$id, b$papel, 100 * b$acierto, 100 * b$nulo, 100 * b$exceso, 100 * b$aplica, b$nombre))
    }
    Rl <- relacionales_p6(L[s], w[s])
    cat(sprintf("  RL1 par HP = HG: HP = HG en %.1f %%; «adivina una y copia» %.1f %% frente a %.1f %% (%+.1f pp, métrica del par)\n",
                100 * Rl$RL1_par_hp_hg[["p_igual"]], 100 * Rl$RL1_par_hp_hg[["acierto"]], 100 * Rl$RL1_par_hp_hg[["nulo"]], 100 * Rl$RL1_par_hp_hg[["exceso"]]))
    cat(sprintf("  RL2 (condicional al ganador): HP %+.1f pp · HG %+.1f pp\n", 100 * Rl$RL2_hp_contraria_ganador[["exceso"]], 100 * Rl$RL2_hg_contraria_ganador[["exceso"]]))
    cat(sprintf("  reglas por encima de +8 pp: %d\n", nrow(S)))
    for (i in seq_len(nrow(S))) cat(sprintf("    %-7s %-8s %5.1f %% (nulo %4.1f %%, %+5.1f pp, aplica %3.0f %%)  %s\n", S$id[i], S$papel[i], 100 * S$acierto[i], 100 * S$nulo[i],
                                            100 * S$exceso[i], 100 * S$aplica[i], S$nombre[i]))
  }
  ## Por rama y k′ a la vez: P(HG), P(HP) y celdas.
  for (rr in c("CURSOS", "PROMEDIO")) for (kk in 1:2) { s <- vapply(L, function(x) x$rama == rr && x$k == kk, NA); if (!any(s)) next
    ws <- w[s] / sum(w[s]); vv <- function(pp) vapply(L[s], function(x) x$p6$v[x$p6$rol == pp], NA)
    cat(sprintf("  rama %s, k′ = %d: masa %.3f · HG %.1f %% · HP %.1f %% · δ′ medio %.1f · cursos del grado de menos cursos: media %.2f\n", rr, kk, sum(w[s]),
                100 * sum(ws * vv("hg")), 100 * sum(ws * vv("hp")), sum(ws * vapply(L[s], function(x) x$delta, 0)), sum(ws * vapply(L[s], function(x) x$n_lo, 0))))
  }
  invisible(out)
}
## Soporte de δ′ y de n_lo en las celdas de k′ = 1 (por qué no se puede equilibrar todo a la vez).
cat("\nSoporte de las dos celdas de k′ = 1 (espacio base, δ′ ≥ 5): filas por δ′ y por cursos del grado de menos cursos\n")
for (c in celdas_V1(G)[1:2]) {
  f <- c$f; f$n_lo <- pmin(f$nG, f$nP)
  cat(sprintf(" %s HG=%s HP=%s (%d filas)\n", f$rama[1], f$hg[1], f$hp[1], nrow(f))); print(table(n_lo = f$n_lo, delta = f$delta))
}
informe("V0 · diseño actual", celdas_V0(G))
informe("V1 · reparto autorizado (literal)", celdas_V1(G))
informe("V2 · reparto autorizado + δ′ equilibrado dentro de k′", equilibrar(celdas_V1(G), "delta"))
G2 <- G; G2$n_lo <- pmin(G2$nG, G2$nP)
informe("V3 · reparto autorizado + cursos del grado de menos cursos equilibrados dentro de k′", equilibrar(celdas_V1(G2), "n_lo"))

## ---- Cascada F1 del reparto autorizado, con la Parte 1 real (N = 100 semillas, regla #23) ------------------------
cat("\n==================== Cascada F1 (V1, con las exclusiones de V11 de cada Parte 1) ====================\n")
GA <- rbind(espacio("CURSOS", 10L, 4L), espacio("PROMEDIO", 10L, 4L))
sin_base <- 0; sin_ampl <- 0; detalle <- list()
for (z in 1:100) {
  e1 <- correr(EXPR, z); stopifnot(is.null(e1$.err))
  ex <- function(f) f[!(f$nG %in% e1$prohib_tabla_par | f$nP %in% e1$prohib_tabla_par | f$aG %in% e1$prohib_tabla_par | f$aP %in% e1$prohib_tabla_par) &
                        !(f$TG %in% e1$prohib_resp_par | f$TP %in% e1$prohib_resp_par | f$D %in% e1$prohib_resp_par), ]
  cb <- celdas_V1(G); ca <- celdas_V1(GA)
  for (i in seq_along(cb)) {
    nb <- nrow(ex(cb[[i]]$f)); na <- nrow(ex(ca[[i]]$f))
    if (nb == 0L) { sin_base <- sin_base + cb[[i]]$m; detalle[[length(detalle) + 1L]] <- sprintf("s=%d celda %d (%s HG=%s HP=%s k′=%d): base 0, ampliado %d", z, i,
      cb[[i]]$f$rama[1], cb[[i]]$f$hg[1], cb[[i]]$f$hp[1], cb[[i]]$f$k[1], na) }
    if (na == 0L) sin_ampl <- sin_ampl + ca[[i]]$m
  }
}
cat(sprintf("  versiones esperadas (de 100) cuya celda sorteada no tiene candidatos en el espacio base: %.2f; tampoco en el ampliado (n′ ≤ 10): %.2f\n", sin_base, sin_ampl))
if (length(detalle)) cat(paste0("   ", head(detalle, 15), "\n"), sep = "")
