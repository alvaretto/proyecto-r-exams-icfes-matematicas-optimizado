#!/usr/bin/env Rscript
## medir_bateria_cloze.R — tanda 4 (2026-10-31). Mide la batería L-C4 (../bateria_partes_2_a_6.R) sobre el .Rmd del
## CLOZE: Parte 2 por tipo de pregunta (muestra N = 100 y enumeración EXACTA de su espacio finito) y Parte 6 por
## estrato (mezcla 1..N, rama del otro colegio y k′, N = 100 cada uno, regla #23). Imprime las reglas por encima de
## +8 pp, el techo nulo por permutación, las relacionales y el control positivo. Sirve para la línea base (md5 de la
## tanda 3) y para el .Rmd corregido. Solo lee.
## Uso (desde cloze/salida): Rscript --no-init-file medir_bateria_cloze.R [ruta.Rmd]
suppressMessages(library(exams))
stopifnot("correr desde cloze/salida" = basename(getwd()) == "salida" && basename(dirname(getwd())) == "cloze")
args <- commandArgs(trailingOnly = TRUE)
RMD <- normalizePath(if (length(args)) args[1] else "../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
V <- parse(file = "../verificar_espacio_cloze.R", keep.source = FALSE, encoding = "UTF-8")
for (e in V) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% c("chunk", "correr"))
  eval(e, envir = globalenv())
source("../bateria_partes_2_a_6.R", encoding = "UTF-8")
cat("md5 del .Rmd:", unname(tools::md5sum(RMD)), "· batería:", BATERIA_VERSION, "md5", unname(tools::md5sum("../bateria_partes_2_a_6.R")), "\n")
N <- 100L; TOPE <- 30000L
EXPR <- parse(text = chunk(RMD), encoding = "UTF-8")
EST <- list(mezcla = list(), p2_promedio = list(), p2_comparar = list(), par_CURSOS = list(), par_PROMEDIO = list(),
            k1 = list(), k2mas = list())
ENV_TIPO <- list()
for (z in seq_len(TOPE)) {
  if (all(lengths(EST) >= N)) break
  en <- correr(EXPR, z); if (!is.null(en$.err)) stop("semilla ", z, ": ", en$.err)
  x <- registro_bateria(en)
  put <- function(k) if (length(EST[[k]]) < N) EST[[k]][[length(EST[[k]]) + 1L]] <<- x
  if (z <= N) put("mezcla")
  put(paste0("p2_", x$p2$tipo)); put(paste0("par_", x$rama_par)); put(if (x$k == 1) "k1" else "k2mas")
  if (length(ENV_TIPO[[x$p2$tipo]]) < 2L) ENV_TIPO[[x$p2$tipo]] <- c(ENV_TIPO[[x$p2$tipo]], list(en))
}
cat("semillas 1..", z, " · n: ", paste(names(EST), lengths(EST), sep = " = ", collapse = " · "), "\n", sep = "")
imprime <- function(M, cab) {
  cat("\n", cab, "\n", sep = "")
  S <- sobre_umbral(M)
  if (!nrow(S)) cat("  ninguna regla por encima de +8 pp\n") else
    for (i in seq_len(nrow(S))) cat(sprintf("  %-7s %-9s %5.1f %% (nulo %4.1f %%, %+5.1f pp)%s  %s\n", S$id[i], if (is.null(S$papel)) "" else S$papel[i],
                                            100 * S$acierto[i], 100 * S$nulo[i], 100 * S$exceso[i],
                                            if (!is.null(S$aplica)) sprintf(" aplica %3.0f %%", 100 * S$aplica[i]) else "", S$nombre[i]))
  invisible(S)
}

## ---- Parte 2 ----------------------------------------------------------------------------------------------------
cat("\n==================== PARTE 2 (schoice) ====================\n")
perms4 <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perms4(v[-i]), function(p) c(v[i], p))))
PERM <- perms4(1:4)
enum_p2 <- function(en) {
  q <- en$P2[[en$tipo_p2]]; L <- list()
  for (cl in q$clave) for (s in utils::combn(length(q$distractores), 3L, simplify = FALSE)) {
    op0 <- c(cl, unname(q$distractores[s]))
    for (pm in PERM) L[[length(L) + 1L]] <- list(p2 = list(op = op0[pm], k = which(pm == 1L), tipo = en$tipo_p2, preg = en$texto_p2,
                                                          otros = c(en$texto_p3, en$texto_p4, en$texto_p5), enc = names(en$tabla_par_df)[2:3]))
  }
  L
}
RES_P2 <- list()
for (tp in c("promedio", "comparar")) {
  E1 <- enum_p2(ENV_TIPO[[tp]][[1]]); E2 <- enum_p2(ENV_TIPO[[tp]][[2]])
  M1 <- medir_p2(E1); M2 <- medir_p2(E2)
  cat(sprintf("\n[%s] enumeración exacta: %d versiones (3 claves × 10 tríos × 24 órdenes); otro contexto (materia «%s» frente a «%s»): %s\n",
              tp, length(E1), ENV_TIPO[[tp]][[2]]$mat, ENV_TIPO[[tp]][[1]]$mat,
              if (isTRUE(all.equal(M1$acierto, M2$acierto))) "mismas cifras" else "CIFRAS DISTINTAS"))
  RES_P2[[paste0("exacta_", tp)]] <- imprime(M1, sprintf("Parte 2 · %s · exacta: reglas > +8 pp", tp))
  Ls <- EST[[paste0("p2_", tp)]]; Ms <- medir_p2(Ls)
  RES_P2[[paste0("muestra_", tp)]] <- imprime(Ms, sprintf("Parte 2 · %s · muestra n = %d (techo nulo de la batería %.1f %%, máximo observado %.1f %%)",
                                                       tp, length(Ls), 100 * techo_p2(Ls), 100 * max(Ms$acierto)))
}
TAB2 <- merge(medir_p2(enum_p2(ENV_TIPO$promedio[[1]]))[, c("id", "familia", "acierto")], medir_p2(enum_p2(ENV_TIPO$comparar[[1]]))[, c("id", "acierto")], by = "id",
              suffixes = c("_promedio", "_comparar"))
cat("\nParte 2, todas las reglas (exacta), acierto en %: promedio · comparar\n")
for (i in seq_len(nrow(TAB2))) cat(sprintf("  %-6s %-10s %5.1f · %5.1f\n", TAB2$id[i], TAB2$familia[i], 100 * TAB2$acierto_promedio[i], 100 * TAB2$acierto_comparar[i]))

## ---- Parte 6 ----------------------------------------------------------------------------------------------------
cat("\n==================== PARTE 6 (mchoice, por afirmación) ====================\n")
RES_P6 <- list()
for (k in c("mezcla", "par_CURSOS", "par_PROMEDIO", "k1", "k2mas")) {
  L <- EST[[k]]; M <- medir_p6(L); tc <- techo_p6(L)
  ob <- tapply(M$acierto, M$papel, max)[names(tc)]
  RES_P6[[k]] <- imprime(M, sprintf("Parte 6 · %s (n = %d) · máximo observado / techo nulo por papel: %s", k, length(L),
                                     paste(sprintf("%s %.1f/%.1f", names(tc), 100 * ob, 100 * tc), collapse = " · ")))
  R <- relacionales_p6(L)
  cat(sprintf("  relacionales: RL1 par HP=HG %.1f %% (nulo %.1f %%, %+.1f pp; HP = HG en %.1f %%) · RL2 HP %+.1f pp, HG %+.1f pp (condicional al ganador) · RL3 par ventaja=ganador %.1f %% (%+.1f pp; equivalencia lógica)\n",
              100 * R$RL1_par_hp_hg[["acierto"]], 100 * R$RL1_par_hp_hg[["nulo"]], 100 * R$RL1_par_hp_hg[["exceso"]], 100 * R$RL1_par_hp_hg[["p_igual"]],
              100 * R$RL2_hp_contraria_ganador[["exceso"]], 100 * R$RL2_hg_contraria_ganador[["exceso"]],
              100 * R$RL3_par_ventaja_ganador[["acierto"]], 100 * R$RL3_par_ventaja_ganador[["exceso"]]))
  cat(sprintf("  control positivo (calcula los totales): %.1f %% de las versiones con las cuatro afirmaciones bien\n", 100 * control_p6(L)))
}

## ---- Línea base: las sondas de H1 y H2 tienen que salir en rojo --------------------------------------------------
cat("\n==================== SONDAS DE H1 Y H2 ====================\n")
h1 <- c("P2-L1", "P2-L2", "P2-L3")
for (id in h1) {
  v <- vapply(c("exacta_promedio", "exacta_comparar"), function(k) { S <- RES_P2[[k]]; if (id %in% S$id) S$exceso[S$id == id] else NA_real_ }, 0)
  cat(sprintf("  H1 %-6s en rojo (> +8 pp, exacta): promedio %s · comparar %s\n", id,
              if (is.na(v[1])) "no" else sprintf("sí, %+.1f pp", 100 * v[1]), if (is.na(v[2])) "no" else sprintf("sí, %+.1f pp", 100 * v[2])))
}
for (id in c("P6-K1", "P6-HK1", "P6-D1")) {
  v <- vapply(names(RES_P6), function(k) { S <- RES_P6[[k]]; S <- S[S$id %in% c(id, paste0(id, "c")), ]; if (nrow(S)) max(S$exceso) else NA_real_ }, 0)
  cat(sprintf("  H2 %-6s en rojo (> +8 pp) por estrato: %s\n", id, paste(sprintf("%s %s", names(v), ifelse(is.na(v), "no", sprintf("%+.1f", 100 * v))), collapse = " · ")))
}
