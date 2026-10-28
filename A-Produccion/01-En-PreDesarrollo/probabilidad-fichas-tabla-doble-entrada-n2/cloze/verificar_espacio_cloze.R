#!/usr/bin/env Rscript
## =============================================================================
## verificar_espacio_cloze.R — verificador propio del CLOZE derivado (R13 del encargo, 2026-10-28)
## CLOZE: probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd
##
## Correr con el directorio de trabajo en cloze/salida o en un temporal, NUNCA en el directorio del
## SCHOICE: el data_generation escribe tabla_<id>.png en el directorio de trabajo cuando teje.
##   cd cloze/salida && Rscript --no-init-file ../verificar_espacio_cloze.R
## Solo LEE el .Rmd del SCHOICE (y toma de SUB/verificar_espacio.R la definición de sus reglas E4c);
## no edita nada fuera de cloze/.
##
##   E1  P1 idéntica al SCHOICE (R3): objetos y .Random.seed al final del bloque P1 en 100 semillas,
##       más una cuota de 20 canónicas.
##   E2  Una sola clave por gap (I-5): verdad de las Partes 2 a 6 recalculada desde la tabla y desde el
##       TEXTO, sin las funciones del .Rmd, y enumeración de todas sus elecciones en cada versión medida.
##       La Parte 1 hereda la enumeración de SUB/verificar_espacio.R (E1, 10.401.300 combinaciones): su
##       código es el mismo, y E1 de aquí lo comprueba.
##   E3  V11: sin cifras, una sola dimensión, D independiente de los roles X/Y/Z y reglas que cruzan la
##       Parte 1 con D (batería cruzada congelada al empezar el paso 3, §P7-C).
##   E4  R12: residuos firmados del SCHOICE medidos sobre la Parte 1 del CLOZE, por estrato observable.
##   E5  Versiones únicas (objetivo >= 250 de 300).
##   E6  Mutantes (contrato: sonda propia, materialización en el entorno, patrón único, semillas hasta la
##       primera muerte con tope de 600).
##   E7  Pesos por gap (decisión del profesor, 2026-10-28): expoints: 3|1|1|1|1|1, una entrada por
##       gap. R/exams recicla expoints sin avisar (rep(..., length.out = nº de partes)), así que la
##       longitud se compara con los ##ANSWERi## del .Rmd. Con sus propios mutantes (detractor r4, H3).
##   V10 N/A: ninguna parte modifica la tabla.
## =============================================================================
suppressMessages(library(exams))
args_all <- commandArgs(trailingOnly = FALSE)
f_arg <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
DIR <- if (length(f_arg)) dirname(normalizePath(f_arg)) else getwd()
SUB <- normalizePath(file.path(DIR, ".."))
RMD_C <- file.path(DIR, "probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd")
RMD_S <- file.path(SUB, "probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd")
stopifnot(file.exists(RMD_C), file.exists(RMD_S))
if (normalizePath(getwd()) == SUB) stop("No correr con el directorio de trabajo en el subproyecto del SCHOICE")
## La salida dice qué versión verificó (detractor r4, H5): sin esto, dos corridas idénticas no prueban sobre qué .Rmd fueron.
cat("Versión verificada: ", basename(RMD_C), " · md5 ", unname(tools::md5sum(RMD_C)), "\n", sep = "")
N_EST <- 100L; TOPE <- 4000L; TOPE_MUT <- 600L; DIMS_ORD <- c("color", "categ", "figura")
errores <- character(0)
fallo <- function(...) { m <- paste0(...); errores <<- c(errores, m); cat("  FALLO:", m, "\n") }
bien <- function(...) cat("  OK:", paste0(...), "\n")
titulo <- function(t) cat("\n", strrep("=", 78), "\n", t, "\n", strrep("=", 78), "\n", sep = "")
chunk <- function(f) {
  lin <- readLines(f, encoding = "UTF-8"); ini <- grep("^```\\{r data_generation", lin)[1]
  fin <- which(lin == "```" & seq_along(lin) > ini)[1]; paste(lin[(ini + 1):(fin - 1)], collapse = "\n")
}
COD_S <- chunk(RMD_S); COD_C <- chunk(RMD_C)
MARCA <- "## ==== FIN DEL BLOQUE P1 ===="
stopifnot("el marcador del bloque P1 aparece una sola vez" =
            lengths(regmatches(COD_C, gregexpr(MARCA, COD_C, fixed = TRUE))) == 1L)
p1_de <- function(cod) strsplit(cod, MARCA, fixed = TRUE)[[1]][1]
correr <- function(codigo, s) {
  set.seed(s); en <- new.env()
  en$.err <- tryCatch({ eval(parse(text = codigo), envir = en); NULL }, error = function(e) conditionMessage(e))
  en$.seed <- get(".Random.seed", envir = globalenv()); en
}

## ---- E1 · R3 ------------------------------------------------------------------------------------
titulo("E1 · La Parte 1 es el SCHOICE (R3): 100 semillas + cuota de 20 canónicas")
CAMPOS <- c("es_canonica", "w", "lab", "pares", "opciones_base", "iguales", "cuentas", "codigos_sel", "N",
            "enunciado", "pregunta", "contexto_tipo", "respuesta_correcta", "n_clave", "reflexion", "valores",
            "alt_tabla", "fig_id", "archivo_tabla", "codigo_tabla_py")
REN <- c(opciones = "opciones_p1", sol = "sol_p1", feedback = "feedback_p1")
medir_E1 <- function(cod_c, semillas) {
  f <- character(0); n_can <- 0L; p1 <- p1_de(cod_c)
  for (s in semillas) {
    A <- correr(COD_S, s); B <- correr(p1, s); C <- correr(cod_c, s)
    if (!is.null(A$.err) || !is.null(B$.err) || !is.null(C$.err)) {
      f <- c(f, sprintf("E1 s=%d error: %s / %s / %s", s, format(A$.err), format(B$.err), format(C$.err))); next
    }
    n_can <- n_can + isTRUE(A$es_canonica)
    for (k in CAMPOS) if (!identical(A[[k]], B[[k]])) f <- c(f, sprintf("E1 s=%d %s distinto", s, k))
    for (k in names(REN)) if (!identical(A[[k]], B[[REN[[k]]]])) f <- c(f, sprintf("E1 s=%d %s != %s", s, REN[[k]], k))
    if (!identical(A$.seed, B$.seed)) f <- c(f, sprintf("E1 s=%d .Random.seed distinto al final del bloque P1", s))
    for (k in c(CAMPOS, REN)) if (!identical(B[[k]], C[[k]])) f <- c(f, sprintf("E1 s=%d %s cambia en las Partes 2 a 6", s, k))
  }
  list(f = f, n_can = n_can)
}
r1 <- medir_E1(COD_C, seq_len(N_EST))
can_extra <- integer(0); s <- N_EST
while (r1$n_can + length(can_extra) < 20L && s < TOPE) {
  s <- s + 1L; if (isTRUE(correr(p1_de(COD_C), s)$es_canonica)) can_extra <- c(can_extra, s)
}
r1c <- medir_E1(COD_C, can_extra)
f1 <- c(r1$f, r1c$f)
if (length(f1)) for (x in head(f1, 10)) fallo(x) else
  bien(sprintf("%d semillas (%d canónicas) + %d canónicas de cuota: tabla, opciones, clave, pregunta, fig_id, Python y .Random.seed idénticos al SCHOICE; las Partes 2 a 6 no pisan nada",
               N_EST, r1$n_can, length(can_extra)))

## ---- Muestra por cuotas (regla #23): estratos observables por el estudiante ------------------------
usa_dim <- function(par, d) d %in% par[[1]]$d
estrato <- function(en) {
  if (isTRUE(en$es_canonica)) return("canonica")
  n_usan <- sum(vapply(en$pares, usa_dim, NA, d = en$dim_D))
  if (n_usan == 4L) "D_en_las_4" else if (n_usan == 2L) "D_en_2" else "otro"
}
rol_D <- function(en) if (isTRUE(en$es_canonica)) NA_character_ else c("X", "Y", "Z")[match(en$dim_D, en$perm)]
muestrear <- function(cod, cuota = N_EST, tope = TOPE, extra_nc = 300L) {
  M <- list(canonica = list(), D_en_las_4 = list(), D_en_2 = list(), otro = list()); nc <- list(); errs <- character(0)
  for (s in seq_len(tope)) {
    en <- correr(cod, s)
    if (!is.null(en$.err)) { errs <- c(errs, sprintf("s=%d %s", s, en$.err)); next }
    en$.s <- s; st <- estrato(en)
    if (length(M[[st]]) < cuota) M[[st]][[length(M[[st]]) + 1L]] <- en
    if (!isTRUE(en$es_canonica) && length(nc) < extra_nc) nc[[length(nc) + 1L]] <- en
    if (all(lengths(M[c("canonica", "D_en_las_4", "D_en_2")]) >= cuota) && length(nc) >= extra_nc) break
  }
  list(M = M, nc = nc, errs = errs, s_fin = s)
}
MU <- muestrear(COD_C)
cat("\nMuestra: ", paste(sprintf("%s n=%d", names(MU$M), lengths(MU$M)), collapse = " | "),
    sprintf(" | no canónicas consecutivas n=%d | semillas recorridas %d | errores %d\n", length(MU$nc), MU$s_fin,
            length(MU$errs)), sep = "")
if (length(MU$errs)) fallo("el generador aborta: ", MU$errs[1])
for (st in c("canonica", "D_en_las_4", "D_en_2"))
  if (length(MU$M[[st]]) < 20L) cat("  NO CONCLUYENTE: estrato ", st, " con n < 20\n", sep = "")
TODAS <- c(unlist(MU$M, recursive = FALSE), MU$nc)

## ---- E2 · Una sola clave por gap; verdad desde la tabla y el texto ---------------------------------
titulo("E2 · Una sola clave por gap (I-5): verdad recalculada sin las funciones del .Rmd")
marg <- function(w, d, k) unname(apply(w, match(d, DIMS_ORD), sum)[k])
rx_nivel <- function(x) paste0("\\b", x, "(s|es)?\\b")
pos_niveles <- function(txt, niv) vapply(niv, function(x) {
  r <- regexpr(rx_nivel(x), txt, perl = TRUE); if (r < 0) Inf else as.numeric(r) }, 0)
evento_n <- function(txt_ev, en) {                   # casos favorables leídos del TEXTO del evento
  niv <- en$lab[[en$dim_D]]; pos <- pos_niveles(txt_ev, niv)
  if (sum(is.finite(pos)) != 1L) return(NA_real_)
  k <- which.min(pos); n <- marg(en$w, en$dim_D, k)
  if (grepl("que no (sea|tenga)", txt_ev)) sum(en$w) - n else n
}
verdad_p5 <- function(en) vapply(en$opciones_p5, function(o) {
  k <- which.min(pos_niveles(o, en$lab[[en$dim_D]]))
  marg(en$w, en$dim_D, k) > marg(en$w, en$dim_D, 3L - k) }, NA, USE.NAMES = FALSE)
verdad_p6 <- function(en) vapply(en$opciones_p6, function(o) {
  ev <- strsplit(sub("\\.$", "", o), " y sacar ", fixed = TRUE)[[1]]
  if (length(ev) != 2L) return(NA)
  evento_n(ev[1], en) == evento_n(ev[2], en) }, NA, USE.NAMES = FALSE)
medir_E2 <- function(ens) {
  f <- character(0); n_tie_X <- 0L
  for (en in ens) {
    s <- en$.s; w <- en$w; N <- sum(w)
    if (!identical(as.numeric(en$exsol_p2), as.numeric(N))) f <- c(f, sprintf("E2-num s=%d Parte 2", s))
    for (p in c("p3", "p4")) {
      ev <- sub(".*«sacar (.*)».*", "\\1", en[[paste0("texto_", p)]])
      if (!isTRUE(evento_n(ev, en) == en[[paste0("exsol_", p)]])) f <- c(f, sprintf("E2-num s=%d Parte %s", s, substr(p, 2, 2)))
    }
    v5 <- verdad_p5(en)
    if (anyNA(v5) || sum(v5) != 1L || !identical(as.logical(en$sol_p5), v5)) f <- c(f, sprintf("E2-verdad-P5 s=%d", s))
    v6 <- verdad_p6(en)
    if (anyNA(v6) || !identical(as.logical(en$sol_p6), v6) || sum(v6) < 1L || sum(!v6) < 1L)
      f <- c(f, sprintf("E2-verdad-P6 s=%d", s))
    if (!identical(as.character(en$exsolution_str),
                   paste(mchoice2string(en$sol_p1), N, en$exsol_p3, en$exsol_p4, mchoice2string(v5),
                         mchoice2string(v6), sep = "|"))) f <- c(f, sprintf("E2-exsolution s=%d", s))
    ## Enumeración de todas las elecciones de las Partes 2 a 6 en esta tabla: cada dimensión sin empate
    ## da exactamente una clave en la Parte 5, y cada selección posible de la Parte 6 (1 o 2 iguales)
    ## tiene al menos una verdadera y una falsa.
    for (d in DIMS_ORD) {
      n1 <- marg(w, d, 1L); n2 <- marg(w, d, 2L)
      if (n1 == n2) {
        if (d %in% en$dims_ok) f <- c(f, sprintf("E2-enum s=%d dimensión %s empatada y elegible", s, d))
        if (!isTRUE(en$es_canonica) && rol_D(list(es_canonica = FALSE, dim_D = d, perm = en$perm)) != "X")
          f <- c(f, sprintf("E2-enum s=%d empate en una dimensión que no es X", s))
        n_tie_X <- n_tie_X + 1L; next
      }
      plantillas <- c(si1_no2 = n1 == N - n2, si2_no1 = n2 == N - n1, si1_no1 = n1 == N - n1,
                      si2_no2 = n2 == N - n2, no1_no2 = (N - n1) == (N - n2))
      if (sum(plantillas) != 2L || sum(xor(n1 > n2, n2 > n1)) != 1L) f <- c(f, sprintf("E2-enum s=%d dim %s", s, d))
      for (nT in 1:2) for (tt in combn(which(plantillas), nT, simplify = FALSE))
        for (ff in combn(which(!plantillas), 4L - nT, simplify = FALSE)) {
          sv <- plantillas[c(tt, ff)]; if (sum(sv) < 1L || sum(!sv) < 1L) f <- c(f, sprintf("E2-enum s=%d sel", s))
        }
    }
    if (!isTRUE(en$es_canonica)) {                  # identidad que garantiza dos dimensiones sin empate
      v <- en$v; esp <- (v[1, 1, 1] + v[2, 1, 1]) - (v[1, 2, 2] + v[2, 2, 2])
      dY <- marg(w, en$perm[2], 1L) - marg(w, en$perm[2], 2L); dZ <- marg(w, en$perm[3], 1L) - marg(w, en$perm[3], 2L)
      fl <- en$flip
      if (abs(dY) != abs(esp) || abs(dZ) != abs(esp) || esp == 0)
        f <- c(f, sprintf("E2-enum s=%d identidad Y/Z = espejo", s))
    }
  }
  list(f = f, n_tie_X = n_tie_X)
}
r2 <- medir_E2(TODAS)
if (length(r2$f)) for (x in head(unique(r2$f), 10)) fallo(x) else
  bien(sprintf("%d versiones: N, casos favorables, clave de la Parte 5 y marcas de la Parte 6 coinciden con la verdad leída de la tabla y del texto; exsolution coherente; enumeración de dimensiones y selecciones sin fallos",
               length(TODAS)))
cat(sprintf("  Empates de la dimensión X (excluida del sorteo en esa versión): %d de %d versiones medidas\n",
            r2$n_tie_X, length(TODAS)))
cat("  Parte 1: una sola clave por la enumeración E1 de SUB/verificar_espacio.R (código idéntico, ver E1).\n")

## ---- E3 · V11 ----------------------------------------------------------------------------------------
titulo("E3 · V11: lo que muestran las Partes 2 a 6 no delata la Parte 1")
textos26 <- function(en) c(en$texto_p2, en$texto_p3, en$texto_p4, en$texto_p5, en$opciones_p5, en$texto_p6, en$opciones_p6)
medir_E3_textos <- function(ens) {
  f <- character(0)
  for (en in ens) {
    tx <- textos26(en)
    if (any(grepl("[0-9]", tx))) f <- c(f, sprintf("E3-cifras s=%d", en$.s))
    otros <- unlist(en$lab[setdiff(DIMS_ORD, en$dim_D)], use.names = FALSE)
    if (any(vapply(otros, function(x) any(grepl(rx_nivel(x), tx, perl = TRUE, ignore.case = TRUE)), NA)))
      f <- c(f, sprintf("E3-una-dimension s=%d", en$.s))
    if (!all(grepl("en cada extracción|al extraer una ficha", tolower(c(en$texto_p2, en$texto_p3, en$texto_p4, en$texto_p5, en$texto_p6)))))
      f <- c(f, sprintf("E3-extraccion s=%d", en$.s))
    if (any(en$opciones_p1 %in% tx)) f <- c(f, sprintf("E3-opcion-P1 s=%d", en$.s))
  }
  f
}
f3 <- medir_E3_textos(TODAS)
if (length(f3)) for (x in head(unique(f3), 10)) fallo(x) else
  bien(length(TODAS), " versiones: las Partes 2 a 6 no imprimen cifras, nombran solo la dimensión sorteada, definen el evento en cada extracción y no repiten ninguna opción de la Parte 1")
medir_E3_indep <- function(nc) {
  r <- vapply(nc, rol_D, ""); tb <- table(factor(r, levels = c("X", "Y", "Z"))); p <- tb / sum(tb)
  se <- sqrt((1 / 3) * (2 / 3) / sum(tb))
  list(tb = tb, p = p, ok = all(abs(p - 1 / 3) <= 3 * se + 0.02))
}
r3i <- medir_E3_indep(MU$nc)
cat(sprintf("  Rol de la dimensión sorteada (%d no canónicas consecutivas): X %.1f %% | Y %.1f %% | Z %.1f %%\n",
            sum(r3i$tb), 100 * r3i$p[["X"]], 100 * r3i$p[["Y"]], 100 * r3i$p[["Z"]]))
if (r3i$ok) bien("D es independiente de los roles X/Y/Z (|p - 1/3| <= 3 errores típicos + 2 pp por los empates de X)") else
  fallo("E3-independencia: la dimensión sorteada depende de los roles del generador")
## Batería cruzada (congelada al empezar el paso 3, §P7-C): reglas que usan la Parte 1 Y la dimensión D
reglas_cruce <- function(en) {
  ps <- en$pares; k <- which(names(ps) == "clave")
  usa <- vapply(ps, usa_dim, NA, d = en$dim_D)
  rp <- vapply(ps, function(p) any(en$celdas_tipo(en$w, p[[1]]) %in% en$celdas_tipo(en$w, p[[2]])), NA)
  molde <- vapply(ps, function(p) paste(sort(p[[1]]$d), collapse = "+"), "")
  gem <- vapply(seq_along(ps), function(j) any(rp[molde == molde[j] & seq_along(ps) != j]), NA)
  cel <- lapply(ps, function(p) c(en$celdas_tipo(en$w, p[[1]]), en$celdas_tipo(en$w, p[[2]])))
  rg <- vapply(cel, function(x) diff(range(x)), 0)
  el <- function(keep) { q <- which(keep); if (!length(q)) q <- seq_along(keep); if (k %in% q) 1 / length(q) else 0 }
  jn <- function(keep) { q <- which(keep); if (!length(q)) q <- seq_along(keep); m <- q[rg[q] == min(rg[q])]
                         if (k %in% m) 1 / length(m) else 0 }
  c(nombra_D = el(usa), no_nombra_D = el(!usa), repite_y_nombra_D = el(!rp & usa),
    repite_y_no_nombra_D = el(!rp & !usa), compuesta_y_nombra_D = el(!(rp | gem) & usa),
    compuesta_y_no_nombra_D = el(!(rp | gem) & !usa), juntas_entre_nombra_D = jn(usa),
    juntas_entre_no_nombra_D = jn(!usa))
}
p_can <- 0.10
tabla_reglas <- function(fun, etiqueta) {
  R4 <- t(vapply(MU$M$D_en_las_4, fun, fun(MU$M$D_en_las_4[[1]])))
  R2 <- t(vapply(MU$M$D_en_2, fun, fun(MU$M$D_en_2[[1]])))
  RC <- t(vapply(MU$M$canonica, fun, fun(MU$M$canonica[[1]])))
  RN <- t(vapply(MU$nc, fun, fun(MU$nc[[1]])))
  cat(sprintf("  %-26s %9s %9s %12s %10s %9s\n", etiqueta, "D en 4", "D en 2", "no canónica", "canónica", "mezcla"))
  out <- list()
  for (rg in colnames(RN)) {
    x <- RN[, rg]; mez <- (1 - p_can) * mean(x) + p_can * mean(RC[, rg])
    cat(sprintf("  %-26s %8.1f%% %8.1f%% %8.1f%% (%+5.1f pp) %8.1f%% %+7.1f pp\n", rg, 100 * mean(R4[, rg]),
                100 * mean(R2[, rg]), 100 * mean(x), 100 * (mean(x) - .25), 100 * mean(RC[, rg]), 100 * (mez - .25)))
    out[[rg]] <- c(nc = mean(x), se = sd(x) / sqrt(length(x)), can = mean(RC[, rg]), mez = mez)
  }
  invisible(out)
}
cat("\n  Batería cruzada P1 x D (tasa de acierto en la Parte 1; azar 25 %):\n")
RC3 <- tabla_reglas(reglas_cruce, "regla")
## Criterio (R12: «las otras partes pueden amplificarlos»). Una regla cruzada es su regla base de la
## Parte 1 más un filtro por D. Lo atribuible a las Partes 2 a 6 es lo que el filtro AÑADE sobre la base:
## las reglas solo-D se comparan con el azar (canal si > +8 pp, §P7-A); las demás, con la cifra firmada de
## su base (amplifica si la supera en más de 3 errores típicos, como E4). Corregido en la primera corrida:
## el criterio absoluto marcaba compuesta_y_nombra_D (41,7 %), que es MENOR que la compuesta firmada (1/2):
## exacto, (2/3)(1/2) + (1/3)(1/4) = 5/12, porque con D = X el filtro no deja ninguna opción y se cae al azar.
BASE_CRUCE <- c(nombra_D = .25, no_nombra_D = .25, repite_y_nombra_D = 1 / 3, repite_y_no_nombra_D = 1 / 3,
                compuesta_y_nombra_D = 1 / 2, compuesta_y_no_nombra_D = 1 / 2, juntas_entre_nombra_D = 0.326,
                juntas_entre_no_nombra_D = 0.326)
for (rg in names(RC3)) {
  x <- RC3[[rg]]; base <- BASE_CRUCE[[rg]]
  if (base == .25) {
    if (x[["nc"]] - .25 - 2 * x[["se"]] > .08) fallo("E3-cruce ", rg, ": canal > +8 pp en las no canónicas")
  } else if (x[["nc"]] > base + 3 * x[["se"]]) {
    fallo("E3-cruce ", rg, ": la dimensión D amplifica su regla base firmada (", round(100 * base, 1), " %)")
  }
}
cat(sprintf("  Incremento máximo sobre la base atribuible a D: %+.1f pp (%s)\n",
            100 * max(vapply(names(RC3), function(r) RC3[[r]][["nc"]] - BASE_CRUCE[[r]], 0)),
            names(RC3)[which.max(vapply(names(RC3), function(r) RC3[[r]][["nc"]] - BASE_CRUCE[[r]], 0))]))

## ---- E4 · R12: residuos firmados del SCHOICE sobre la Parte 1 del CLOZE ------------------------------
titulo("E4 · R12: residuos firmados del SCHOICE, medidos sobre la Parte 1 del CLOZE")
reglas_tabla <- function(en) {                       # definiciones de SUB/verificar_espacio.R (E4c, congeladas)
  ps <- en$pares; k <- which(names(ps) == "clave")
  cel <- lapply(ps, function(p) c(en$celdas_tipo(en$w, p[[1]]), en$celdas_tipo(en$w, p[[2]])))
  rp <- vapply(ps, function(p) any(en$celdas_tipo(en$w, p[[1]]) %in% en$celdas_tipo(en$w, p[[2]])), NA)
  molde <- vapply(ps, function(p) paste(sort(p[[1]]$d), collapse = "+"), "")
  gem <- vapply(seq_along(ps), function(j) any(rp[molde == molde[j] & seq_along(ps) != j]), NA)
  el <- function(drop) { q <- which(!drop); if (!length(q)) q <- seq_along(drop); if (k %in% q) 1 / length(q) else 0 }
  rg <- vapply(cel, function(x) diff(range(x)), 0)
  c(repite = el(rp), compuesta_repite_y_molde = el(rp | gem),
    celdas_mas_juntas = { m <- which(rg == min(rg)); if (k %in% m) 1 / length(m) else 0 },
    repite_y_juntas = { q <- which(!rp); if (!length(q)) q <- seq_along(rp); m <- q[rg[q] == min(rg[q])]; if (k %in% m) 1 / length(m) else 0 },
    compuesta_y_juntas = { q <- which(!(rp | gem)); if (!length(q)) q <- seq_along(rp); m <- q[rg[q] == min(rg[q])]; if (k %in% m) 1 / length(m) else 0 })
}
FIRMADAS <- c(repite = 1 / 3, compuesta_repite_y_molde = 1 / 2, celdas_mas_juntas = 0.326, repite_y_juntas = 0.476,
              compuesta_y_juntas = 0.648)
R12 <- tabla_reglas(reglas_tabla, "regla firmada")
for (rg in names(R12)) {
  ok <- R12[[rg]][["nc"]] <= FIRMADAS[[rg]] + 3 * R12[[rg]][["se"]] + 1e-9
  if (!ok) fallo("E4 ", rg, ": por encima de su firma (", round(100 * FIRMADAS[[rg]], 1), " %)") else
    bien(sprintf("%s: %.1f %% en las no canónicas, dentro de su firma (%.1f %%)", rg, 100 * R12[[rg]][["nc"]], 100 * FIRMADAS[[rg]]))
}
cat("  Las Partes 2 a 6 no imprimen cifras ni tipos de dos características: las reglas que ven la tabla\n",
    "  reciben lo mismo que en el SCHOICE. Lo nuevo que traen es la dimensión D, medido en E3.\n", sep = "")

## ---- E5 · Versiones únicas ------------------------------------------------------------------------
titulo("E5 · Versiones únicas sobre 300 semillas (objetivo >= 250)")
huella <- function(en) paste(c(en$enunciado, as.vector(en$w), sort(en$opciones_p1), en$texto_p3, en$texto_p4,
                               en$texto_p5, sort(en$opciones_p5), sort(en$opciones_p6)), collapse = "|")
hs <- vapply(1:300, function(s) { en <- correr(COD_C, s); if (is.null(en$.err)) huella(en) else NA_character_ }, "")
u <- length(unique(hs[!is.na(hs)]))
if (u >= 250L) bien(u, " de 300 versiones únicas (enunciado + tabla + textos de las seis partes)") else
  fallo("E5: solo ", u, " versiones únicas de 300")

## ---- E6 · Mutantes ----------------------------------------------------------------------------------
titulo("E6 · Mutantes (contrato: sonda propia, materialización en el entorno, patrón único)")
MUT <- list(
  list(id = "M1", inv = "V11 por valor (sin cifras)", tipo = "guarda", sonda = "V11 por valor",
       patron = '"extraer una ficha. La respuesta es un número entero.")',
       nuevo = '"extraer una ficha. La respuesta es un número entero, como 7.")',
       mat = function(en) grepl("[0-9]", en$texto_p2)),
  list(id = "M2", inv = "F-3 (una sola dimensión)", tipo = "guarda", sonda = "F-3",
       patron = 'color  = paste0("una ficha ", niv[k]),',
       nuevo = 'color  = paste0("una ficha ", niv[k], " con un ", lab$figura[1]),',
       mat = function(en) en$dim_D == "color" && grepl(lab_rx(en$lab$figura[1]), en$texto_p3, perl = TRUE)),
  list(id = "M3", inv = "F-4 (evento en cada extracción)", tipo = "guarda", sonda = "F-4",
       patron = '"En cada extracción, ¿qué es más probable: sacar "', nuevo = '"¿Qué es más probable: sacar "',
       mat = function(en) !grepl("extracci", en$texto_p5)),
  list(id = "M4", inv = "I-1 y R7 (sin vocabulario de error)", tipo = "guarda", sonda = "I-1 y R7",
       patron = 'paste0("Es más probable sacar "', nuevo = 'paste0("Sin error, es más probable sacar "',
       mat = function(en) any(grepl("error", en$opciones_p5))),
  list(id = "M5", inv = "Parte 5: una sola clave", tipo = "guarda", sonda = "Parte 5: una sola clave",
       patron = "sol_p5 <- nivel_p5 == mayor", nuevo = "sol_p5 <- nivel_p5 >= 1L",
       mat = function(en) sum(en$sol_p5) == 2L),
  list(id = "M6", inv = "Parte 6: al menos una verdadera y una falsa", tipo = "guarda",
       sonda = "Parte 6: al menos una verdadera y una falsa",
       patron = "sol_p6 <- unname(igual_pool[sel6])", nuevo = "sol_p6 <- rep(FALSE, 4L)",
       mat = function(en) sum(en$sol_p6) == 0L),
  list(id = "M7", inv = "opciones únicas (Partes 5 y 6)", tipo = "guarda", sonda = "opciones únicas",
       patron = "sol_p6 <- unname(igual_pool[sel6])",
       nuevo = "opciones_p6[2] <- opciones_p6[1]; sol_p6 <- unname(igual_pool[sel6])",
       mat = function(en) anyDuplicated(en$opciones_p6) > 0L),
  list(id = "M8", inv = "I-7 en las Partes 2 a 6 (n/N con «en cada extracción»)", tipo = "guarda",
       sonda = "n/N con «en cada extracción» (Partes 2 a 6)",
       patron = '" fichas: en cada extracción, $\\\\frac{"', nuevo = '" fichas: $\\\\frac{"',
       mat = function(en) any(grepl("frac{", en$feedback_p6, fixed = TRUE) & !grepl("extracci", en$feedback_p6))),
  list(id = "M9", inv = "Sin empate en la dimensión sorteada", tipo = "guarda", sonda = "n_niv[1] != n_niv[2]",
       patron = "dim_D <- pick(dims_ok)", nuevo = "dim_D <- pick(DIMS)",
       mat = function(en) en$n_tipo(en$w, en$tp(en$dim_D, 1L)) == en$n_tipo(en$w, en$tp(en$dim_D, 2L))),
  list(id = "M10", inv = "Verdad de la Parte 6 (complemento)", tipo = "verificador", sonda = "E2-verdad-P6",
       patron = 'ev_no <- function(k) list(txt = paste0("sacar ", ficha_no(k)), n = N - n_niv[k])',
       nuevo = 'ev_no <- function(k) list(txt = paste0("sacar ", ficha_no(k)), n = n_niv[k])',
       mat = function(en) isTRUE(en$igual_pool[["si1_no1"]])),
  list(id = "M11", inv = "D independiente de los roles X/Y/Z", tipo = "verificador", sonda = "E3-independencia",
       patron = "dim_D <- pick(dims_ok)",
       nuevo = "dim_D <- if (!es_canonica && perm[1] %in% dims_ok) perm[1] else pick(dims_ok)",
       mat = function(en) isTRUE(en$es_canonica) || en$dim_D == en$perm[1] || !(en$perm[1] %in% en$dims_ok)),
  list(id = "M12", inv = "R3: consumo del RNG del bloque P1", tipo = "verificador", sonda = "E1",
       patron = "o <- sample(4)", nuevo = "invisible(runif(1)); o <- sample(4)",
       mat = NULL),
  list(id = "M13", inv = "Verdad de la Parte 5", tipo = "verificador", sonda = "E2-verdad-P5",
       patron = "mayor <- which.max(n_niv)", nuevo = "mayor <- which.min(n_niv)",
       mat = function(en) en$mayor != which.max(en$n_niv)),
  ## Guardas heredadas del SCHOICE, vueltas a probar sobre el código del CLOZE
  list(id = "M14", inv = "P_CANON <= 0,10 (I-8)", tipo = "guarda", sonda = "P_CANON <= 0.1",
       patron = "P_CANON <- 0.10", nuevo = "P_CANON <- 0.20", mat = function(en) isTRUE(en$P_CANON == 0.2)),
  list(id = "M15", inv = "I-7: la pregunta del impreso", tipo = "guarda", sonda = "nchar(pregunta) == 123L",
       patron = 'PREGUNTA <- paste("Si se eligen al azar dos fichas de la bolsa,',
       nuevo = 'PREGUNTA <- paste("Si se elige al azar una ficha de la bolsa,',
       mat = function(en) !grepl("dos fichas", en$pregunta)),
  list(id = "M16", inv = "I-5: una sola clave en la Parte 1", tipo = "guarda", sonda = "sum(iguales) == 1",
       patron = "iguales <- cuentas[, 1] == cuentas[, 2]", nuevo = "iguales <- cuentas[, 1] >= 0",
       mat = function(en) sum(en$iguales) == 4L),
  list(id = "M17", inv = "I-2: canónica verbatim (celdas)", tipo = "guarda", sonda = "identical(as.vector(w)",
       patron = "CANON_W <- array(c(12, 16, 8, 10, 11, 20, 15, 8)", nuevo = "CANON_W <- array(c(12, 16, 8, 10, 11, 20, 15, 9)",
       mat = function(en) isTRUE(en$es_canonica) && en$w[2, 2, 2] == 9),
  list(id = "M18", inv = "I-1: opciones de la Parte 1 sin vocabulario de error", tipo = "guarda", sonda = "estudiante|error",
       patron = 'paste0(cap1(paste(txt_tipo(par[[1]], lab), "y", txt_tipo(par[[2]], lab))), ".")',
       nuevo = 'paste0(cap1(paste(txt_tipo(par[[1]], lab), "y", txt_tipo(par[[2]], lab))), " (sin error).")',
       mat = function(en) any(grepl("error", en$opciones_base)) && !isTRUE(en$es_canonica), solo_nc = TRUE),
  ## Guardas sin mutante en la pasada 1 (detractor FASE 2C r1, H4)
  list(id = "M19", inv = "V11: la pareja directa no entra en el pool de la Parte 6", tipo = "guarda",
       sonda = "pool de la Parte 6", patron = "  no1_no2 = list(ev_no(1L), ev_no(2L)))",
       nuevo = "  no1_no2 = list(ev_no(1L), ev_no(2L)),\n  si1_si2 = list(ev_si(1L), ev_si(2L)))",
       mat = function(en) "si1_si2" %in% names(en$parejas_p6)),
  list(id = "M20", inv = "Seis gaps en exclozetype, exsolution y extol", tipo = "guarda",
       sonda = "lengths(strsplit(c(exclozetype_str", patron = 'exclozetype_str <- "schoice|num|num|num|schoice|mchoice"',
       nuevo = 'exclozetype_str <- "schoice|num|num|schoice|mchoice"',
       mat = function(en) length(strsplit(en$exclozetype_str, "|", fixed = TRUE)[[1]]) == 5L),
  list(id = "M21", inv = "Al menos dos dimensiones sin empate (inalcanzable sin mutar dims_ok)", tipo = "guarda",
       sonda = "al menos dos dimensiones sin empate",
       patron = "dims_ok <- DIMS[vapply(DIMS, function(d) n_nivel(d, 1L) != n_nivel(d, 2L), logical(1))]",
       nuevo = "dims_ok <- DIMS[vapply(DIMS, function(d) n_nivel(d, 1L) != n_nivel(d, 2L), logical(1))][1]",
       mat = function(en) length(en$dims_ok) == 1L),
  list(id = "M22", inv = "Los dos niveles suman N (inalcanzable sin mutar n_niv)", tipo = "guarda", sonda = "sum(n_niv) == N",
       patron = "n_niv <- c(n_nivel(dim_D, 1L), n_nivel(dim_D, 2L))", nuevo = "n_niv <- c(n_nivel(dim_D, 1L), n_nivel(dim_D, 2L) + 1000)",
       mat = function(en) sum(en$n_niv) != sum(en$w)),
  list(id = "M23", inv = "Respuestas num enteras (F-6)", tipo = "guarda", sonda = "== round(c(exsol_p2",
       patron = "exsol_p2 <- N\n", nuevo = "exsol_p2 <- N + 0.5\n", mat = function(en) en$exsol_p2 %% 1 != 0))
lab_rx <- function(x) rx_nivel(x)
veredictos <- data.frame(id = character(0), inv = character(0), sonda_esperada = character(0), sonda_real = character(0),
                         semilla = integer(0), veredicto = character(0), stringsAsFactors = FALSE)
for (mu in MUT) {
  n_pat <- lengths(regmatches(COD_C, gregexpr(mu$patron, COD_C, fixed = TRUE)))
  if (n_pat != 1L) {
    veredictos[nrow(veredictos) + 1L, ] <- list(mu$id, mu$inv, mu$sonda, sprintf("patrón %d veces", n_pat), NA, "mal_construido")
    next
  }
  cod_m <- sub(mu$patron, mu$nuevo, COD_C, fixed = TRUE)
  real <- "-"; semilla <- NA_integer_; ver <- "no_detectado"
  if (mu$tipo == "guarda") {
    for (s in seq_len(TOPE_MUT)) {
      en <- correr(cod_m, s)
      if (isTRUE(mu$solo_nc) && isTRUE(en$es_canonica)) next
      if (!is.null(en$.err)) {
        real <- en$.err; semilla <- s
        ver <- if (!isTRUE(tryCatch(mu$mat(en), error = function(e) FALSE))) "mal_construido" else
               if (grepl(mu$sonda, en$.err, fixed = TRUE)) "cazado_por_su_sonda" else "cazado_por_otra"
        break
      }
    }
  } else {
    if (mu$sonda == "E1") {
      rr <- medir_E1(cod_m, 1:20); real <- if (length(rr$f)) rr$f[1] else "-"
      mat_ok <- !identical(correr(p1_de(cod_m), 1)$.seed, correr(p1_de(COD_C), 1)$.seed)
    } else {
      ens <- list(); for (s in seq_len(TOPE_MUT)) { en <- correr(cod_m, s); if (is.null(en$.err)) { en$.s <- s; ens[[length(ens) + 1L]] <- en }
                                                    if (length(ens) >= 300L) break }
      mat_ok <- if (mu$id == "M11") mean(vapply(ens, mu$mat, NA)) > 0.9 else any(vapply(ens, mu$mat, NA))
      ens_nc <- Filter(function(en) !isTRUE(en$es_canonica), ens)
      rr <- if (mu$sonda == "E3-independencia") { x <- medir_E3_indep(ens_nc); if (x$ok) character(0) else "E3-independencia" } else
            medir_E2(ens)$f
      real <- if (length(rr)) rr[1] else "-"
    }
    ver <- if (!mat_ok) "mal_construido" else if (real == "-") "no_detectado" else
           if (grepl(mu$sonda, real, fixed = TRUE)) "cazado_por_su_sonda" else "cazado_por_otra"
  }
  veredictos[nrow(veredictos) + 1L, ] <- list(mu$id, mu$inv, mu$sonda, substr(gsub("\n", " ", real), 1, 70), semilla, ver)
}
for (i in seq_len(nrow(veredictos))) with(veredictos[i, ], cat(sprintf("  %-4s %-22s %-30s semilla %4s  -> %s\n      sonda real: %s\n",
  id, veredicto, substr(inv, 1, 30), ifelse(is.na(semilla), "-", semilla), sonda_esperada, sonda_real)))
malos <- veredictos$veredicto != "cazado_por_su_sonda"
if (any(malos)) for (i in which(malos)) fallo("E6 ", veredictos$id[i], " ", veredictos$veredicto[i]) else
  bien(nrow(veredictos), "/", nrow(veredictos), " mutantes cazados por su propia sonda")
cat("  Guardas de la Parte 1 sin mutante aquí: las demás del SCHOICE (25/25 en SUB/salida/r6/verificar_espacio.txt);\n",
    "  el código es el mismo (E1), así que esos mutantes valen también para el CLOZE.\n", sep = "")

## ---- E7 · Pesos por gap ---------------------------------------------------------------------------
titulo("E7 · Pesos por gap: expoints 3|1|1|1|1|1, una entrada por gap")
chequeo_pesos <- function(lin) {
  n_gaps <- length(unique(regmatches(lin, regexpr("##ANSWER[0-9]+##", lin))))
  ep <- grep("^expoints:", lin, value = TRUE)
  if (length(ep) != 1L) return(paste0("hay ", length(ep), " líneas expoints (se espera 1)"))
  pesos <- trimws(strsplit(sub("^expoints:", "", ep), "|", fixed = TRUE)[[1]])
  if (length(pesos) != n_gaps) return(paste0(length(pesos), " pesos para ", n_gaps, " gaps"))
  if (!identical(pesos, c("3", "1", "1", "1", "1", "1"))) return(paste0("pesos ", paste(pesos, collapse = "|"), " (firmado: 3|1|1|1|1|1)"))
  NULL
}
LIN_C <- readLines(RMD_C, encoding = "UTF-8")
m7 <- chequeo_pesos(LIN_C)
if (is.null(m7)) bien("expoints 3|1|1|1|1|1 con ", length(unique(regmatches(LIN_C, regexpr("##ANSWER[0-9]+##", LIN_C)))), " gaps") else fallo("E7 ", m7)
## Mutantes del propio E7: cada uno tiene que hacerlo fallar.
mut7 <- list(
  "sin la línea" = LIN_C[!grepl("^expoints:", LIN_C)],
  "Parte 1 a peso 1" = sub("^expoints: 3\\|", "expoints: 1|", LIN_C),
  "un peso de menos" = sub("^expoints: 3\\|1\\|1\\|1\\|1\\|1$", "expoints: 3|1|1|1|1", LIN_C),
  "el 3 en la Parte 2" = sub("^expoints: 3\\|1\\|", "expoints: 1|3|", LIN_C))
for (nm in names(mut7)) {
  if (identical(mut7[[nm]], LIN_C)) { fallo("E7 mutante «", nm, "» mal construido (no cambia el texto)"); next }
  r <- chequeo_pesos(mut7[[nm]])
  if (is.null(r)) fallo("E7 mutante «", nm, "» vivo") else bien("E7 mutante «", nm, "» muerto: ", r)
}

## ---- Resultado -------------------------------------------------------------------------------------
titulo("RESULTADO")
cat("V10: N/A (ninguna parte modifica la tabla ni cita un enunciado modificado).\n")
if (length(errores)) { cat("RESULTADO: RECHAZADO (", length(errores), " fallo(s))\n", sep = ""); quit(status = 1) } else
  cat("RESULTADO: APROBADO (0 fallos)\n")
