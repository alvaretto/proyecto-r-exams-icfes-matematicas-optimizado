## medir_bateria_v2.R — tanda 7 (2026-10-31): la batería L-C4 v2 sobre el histórico (regla #22 §P7-C). Solo lee.
## Uso, desde cloze/salida:  Rscript --no-init-file medir_bateria_v2.R <ruta.Rmd> [<ruta.Rmd> ...]
## Parte 2: EXACTA por tipo (medir_p2_exacta: 3 claves × C(5,3) tríos; reglas del estímulo integradas sobre las 100
## versiones de la muestra estándar como portadores) + control: las reglas de v1 coinciden con la enumeración de 720.
## Parte 6: la enumeración EXACTA de L-C5 (enumerar_h2_cloze.R, 100 Partes 1) con v2; las filas de v1 se comparan con
## las de la batería v1 (BAT_V1=<ruta>) para comprobar que v2 no mueve L-C5.
suppressMessages(library(exams))
DIRC <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze"
BAT <- file.path(DIRC, "bateria_partes_2_a_6.R"); EB <- new.env(); sys.source(BAT, envir = EB, keep.source = FALSE)
BAT_V1 <- Sys.getenv("BAT_V1", ""); EB1 <- NULL
if (nzchar(BAT_V1)) { EB1 <- new.env(); sys.source(BAT_V1, envir = EB1, keep.source = FALSE) }
cat("Batería:", EB$BATERIA_VERSION, "· md5", unname(tools::md5sum(BAT)), if (!is.null(EB1)) paste("· v1 de control:", EB1$BATERIA_VERSION, unname(tools::md5sum(BAT_V1))) else "", "\n")
MARCA <- "## ==== FIN DEL BLOQUE P1 ===="
source(file.path(DIRC, "enumerar_h2_cloze.R"), encoding = "UTF-8")
chunk <- function(f) { lin <- readLines(f, encoding = "UTF-8"); ini <- grep("^```\\{r data_generation", lin)[1]
  fin <- which(lin == "```" & seq_along(lin) > ini)[1]; paste(lin[(ini + 1):(fin - 1)], collapse = "\n") }
correr <- function(expr, s) { set.seed(s); en <- new.env(); en$.err <- tryCatch({ eval(expr, envir = en); NULL }, error = function(e) conditionMessage(e)); en }
perm4 <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perm4(v[-i]), function(p) c(v[i], p))))
h2_exacta <- function(cod, semillas) { FF <- list(); merr <- 0; errs <- character(0); nh <- 0L
  for (z in semillas) { E <- enumerar_h2(cod, MARCA, z); nh <- nh + E$n_hojas; merr <- merr + E$masa_err; errs <- unique(c(errs, E$errores))
    if (is.null(E$filas)) next; f <- E$filas; f$w <- f$w / sum(f$w) / length(semillas); FF[[length(FF) + 1L]] <- f }
  list(A = if (length(FF)) h2_agregar(do.call(rbind, FF)) else NULL, masa_err = merr / length(semillas), errores = errs, n_hojas = nh) }
V1_IDS <- c(paste0("P2-L", 1:6), paste0("P2-F", 1:5), paste0("P2-R", 1:3), paste0("P2-E", 1:4))
fmt <- function(M) paste(sprintf("%s %+.1f", M$id, 100 * M$exceso), collapse = " · ")
for (RMD in commandArgs(TRUE)) {
  COD <- chunk(RMD); EXPR <- parse(text = COD, encoding = "UTF-8")
  cat("\n", strrep("=", 100), "\n.Rmd: ", RMD, "\nmd5: ", unname(tools::md5sum(RMD)), "\n", strrep("=", 100), "\n", sep = "")
  ENV <- lapply(1:100, function(z) correr(EXPR, z)); bad <- which(!vapply(ENV, function(e) is.null(e$.err), NA))
  if (length(bad)) stop("semillas con error: ", paste(bad, collapse = ", "))
  for (tp in c("promedio", "comparar")) {
    banco <- ENV[[1]]$P2[[tp]]; EST <- lapply(ENV, function(en) EB$estimulo_p2(en, preg = banco$pregunta))
    t0 <- Sys.time(); M <- EB$medir_p2_exacta(banco, tp, EST); V <- attr(M, "vocabulario")
    cat(sprintf("\n--- Parte 2 «%s», EXACTA: %d combinaciones × %d portadores; %d reglas (%d tokens); %.0f s\n", tp, attr(M, "n_combos"),
                attr(M, "n_portadores"), nrow(M), nrow(V), as.numeric(Sys.time() - t0, units = "secs")))
    ## control de v1: 720 versiones con un portador, mismas cifras que la medida exacta para las reglas de v1
    LE <- list(); cl <- unname(banco$clave); di <- unname(banco$distractores)
    for (k in cl) for (s in utils::combn(length(di), 3L, simplify = FALSE)) { op0 <- c(k, di[s])
      for (pm in perm4(1:4)) LE[[length(LE) + 1L]] <- list(p2 = c(list(op = op0[pm], k = which(pm == 1L), tipo = tp), EST[[1]])) }
    M720 <- EB$medir_p2(LE, vocab = FALSE); d <- merge(M720[M720$id %in% V1_IDS, c("id", "exceso")], M[M$id %in% V1_IDS, c("id", "exceso")], by = "id")
    cat(sprintf("control: reglas de v1, exacta frente a 720 versiones con un portador: %d reglas, diferencia máxima %.2g pp\n", nrow(d), 100 * max(abs(d$exceso.x - d$exceso.y))))
    cat("v1 (exacta):", fmt(M[match(V1_IDS, M$id), ]), "\n")
    R <- M[M$exceso > 0.08 + 1e-12, ]; R <- R[order(-R$exceso), ]
    cat(sprintf("EN ROJO (> +8 pp): %d reglas\n", nrow(R))); if (nrow(R)) for (i in seq_len(nrow(R))) cat(sprintf("  %-28s %+6.1f pp  %s\n", R$id[i], 100 * R$exceso[i], R$nombre[i]))
    G <- M[M$exceso > 0.053 + 1e-12 & M$exceso <= 0.08 + 1e-12, ]; if (nrow(G)) cat("zona gris (+5,3 a +8 pp):", fmt(G[order(-G$exceso), ]), "\n")
    cat("vocabulario exclusivo de la clave:", paste(V$token[V$k > 0 & V$m == 0], collapse = " · "), "\n")
    cat("vocabulario exclusivo de los distractores:", paste(V$token[V$k == 0 & V$m > 0], collapse = " · "), "\n")
    for (q in c("P2-V «dividido»", "P2-V «entre»", "P2-V «por curso»c", "P2-V «curso»c", "P2-VX", "P2-VXE", "P2-E5", "P2-E7"))
      if (q %in% M$id) cat(sprintf("  %-22s %+6.1f pp\n", q, 100 * M$exceso[M$id == q]))
  }
  t0 <- Sys.time(); H <- h2_exacta(COD, 1:100)
  cat(sprintf("\n--- Parte 6, enumeración exacta de L-C5: %d hojas, masa con error %.4f, %.0f s\n", H$n_hojas, H$masa_err, as.numeric(Sys.time() - t0, units = "secs")))
  EB$fijar_universo_p6(h2_registros(H$A)); T2 <- h2_tabla(H$A, EB); nv <- T2[grepl("^P6-V", T2$base), ]
  cat(sprintf("vocabulario de la Parte 6 (v2): %d celdas (token × papel × estrato); máximo %+.1f pp; > +8 pp: %d\n", nrow(nv), 100 * max(nv$exceso), sum(nv$exceso > 0.08 + 1e-12)))
  top <- nv[order(-nv$exceso), ][1:8, ]; for (i in seq_len(nrow(top))) cat(sprintf("  %-12s %-34s %-8s %+5.1f pp (aplica %.0f %%)\n", top$estrato[i], top$base[i], top$papel[i], 100 * top$exceso[i], 100 * top$aplica[i]))
  ## exploratoria (fuera de la batería): reglas por token de la Parte 6, por estrato
  for (e in names(H2_ESTRATOS)) { sx <- H2_ESTRATOS[[e]](H$A); if (!any(sx)) next
    VT <- EB$vocab_p6_tokens(h2_registros(H$A[sx, , drop = FALSE]), H$A$w[sx] / sum(H$A$w[sx]))
    cat(sprintf("  exploratoria P6 por token, %-9s máx %+5.1f pp (%s)\n", e, 100 * max(VT$exceso), VT$id[which.max(VT$exceso)])) }
  if (!is.null(EB1)) { T1 <- h2_tabla(H$A, EB1); k1 <- paste(T1$estrato, T1$base, T1$papel); k2 <- paste(T2$estrato, T2$base, T2$papel)
    i2 <- match(k1, k2); cat(sprintf("L-C5 con v1 y con v2: %d celdas de v1, %d presentes en v2, diferencia máxima %.2g pp\n", length(k1), sum(!is.na(i2)), 100 * max(abs(T1$exceso - T2$exceso[i2]), na.rm = TRUE))) }
}
