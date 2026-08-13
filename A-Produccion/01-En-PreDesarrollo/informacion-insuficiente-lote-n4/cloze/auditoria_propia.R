# Auditoria propia del CLOZE informacion-insuficiente-lote-n4.
# Mide lo que NINGUNA sonda del arsenal mide para este item:
#   A) fuga INTER-PARTE (propia del formato CLOZE), incluye A5 P2/P3->P5
#   B) fuga LEXICA por token exclusivo de la clave (Error 32)
#   C) alcanzabilidad de TODO el banco como clave y como distractor (Error 27)
#   D) H1 / H3b CONDICIONADOS POR RAMA (regla #22 v1.3: el agregado no acredita)
#   E) coherencia interna de las claves de cada parte
suppressMessages(library(exams))
args <- commandArgs(TRUE)
RMD <- if (length(args) >= 1) args[1] else
  "informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd"
# Muestra estandar N=100 (.claude/rules/muestra-estandar-validacion.md, regla #23).
# Antes 400: era uno de los valores ad-hoc que la regla vino a eliminar.
NV <- if (length(args) >= 2) as.integer(args[2]) else 100L

L <- readLines(RMD, warn = FALSE)
i0 <- grep("^```\\{r data_generation", L)[1]
i1 <- which(grepl("^```\\s*$", L) & seq_along(L) > i0)[1]
expr <- parse(text = paste(L[(i0 + 1):(i1 - 1)], collapse = "\n"))

recolectar <- function(nv) {
  out <- list(); errs <- 0L
  for (s in seq_len(nv)) {
    set.seed(s * 7919L + 13L)
    e <- new.env(parent = globalenv())
    ok <- tryCatch({ suppressWarnings(suppressMessages(eval(expr, envir = e))); TRUE },
                   error = function(z) FALSE)
    if (!ok) { errs <- errs + 1L; next }
    g <- function(x) get(x, envir = e)
    out[[length(out) + 1L]] <- list(
      info = g("info_txt"), canon = g("is_canonical"),
      cods = g("cods"),
      cod_p1 = g("codigos_p1"), sol_p1 = g("sol_p1"), op_p1 = g("opciones_p1"),
      clave_p1 = g("clave")$codigo,
      cod_p23 = g("cods")[g("par_23")],
      op_p4 = g("opciones_p4"), sol_p4 = g("sol_p4"),
      cod_p4 = g("cods")[g("idx_p4")],
      op_p5 = g("opciones_p5"), sol_p5 = g("sol_p5"), cod_p5 = g("cods")[g("idx_p5")],
      op_p6 = g("opciones_p6"), sol_p6 = g("sol_p6"),
      cod_p6 = g("cods")[g("idx_p6")], clave_p6 = g("clave_p6")$codigo,
      veto_clave = g("veto_clave"), q_p5 = g("clave_p5q")$codigo,
      p5_fb = g("p5_sin_poda"),
      niv23 = g("nivel_excl_p23"), niv4 = g("nivel_excl_p4"), niv6 = g("nivel_excl_p6"))
  }
  list(v = out, errs = errs)
}
cat("Recolectando", NV, "versiones...\n")
R <- recolectar(NV); V <- R$v
cat(sprintf("Versiones validas: %d/%d (errores de generacion: %d)\n\n", length(V), NV, R$errs))
stopifnot(length(V) > 0)
fallos <- character(0)
add <- function(...) fallos <<- c(fallos, paste0(...))

# --- Minimo por ESTRATO (regla #23) --------------------------------------
# Los bloques B y D condicionan por rama, asi que el N global se reparte entre
# las 5 ramas segun su peso. Con N=100 la rama mas ligera (LT--, peso 8/86)
# recibe n~7: un solo caso mueve la tasa 14 puntos y el umbral del 70 % se cruza
# por ruido. Medido el 2026-08-13: el token 'área' daba 71,4 % (5/7) con N=100 y
# 48,3 % (14/29) con N=400 -- un RECHAZADO falso. Un rojo falso es peor que no
# medir, porque se aprende a ignorarlo.
# Por eso un estrato por debajo de MIN_ESTRATO no se declara ni verde ni rojo:
# se declara NO CONCLUYENTE y se pide el N que lo haria dictaminable.
MIN_ESTRATO <- 20L
cortos <- character(0)
marcar_corto <- function(etq, n) {
  cortos <<- c(cortos, sprintf("%s (n=%d)", etq, n))
  cat(sprintf("  %-22s MUESTRA CORTA (n=%d < %d): NO CONCLUYENTE\n", etq, n, MIN_ESTRATO))
}

# ---------- A) FUGA INTER-PARTE -------------------------------------------
cat("== A) Fuga inter-parte ==\n")
a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- 0L
for (x in V) {
  # La CLAVE de la Parte 1 y su gemela no pueden aparecer en NINGUNA otra parte.
  otras <- unique(c(x$cod_p23, x$cod_p4, x$cod_p6))
  if (any(otras %in% x$veto_clave)) a1 <- a1 + 1L
  # Ninguna opcion de la Parte 1 puede aparecer en las Partes 2, 3 o 4.
  if (any(c(x$cod_p23, x$cod_p4) %in% x$cod_p1)) a2 <- a2 + 1L
  # Lo que se pide CALCULAR (P2/P3) no puede ser opcion de la Parte 4.
  if (any(x$cod_p23 %in% x$cod_p4)) a3 <- a3 + 1L
  # Ninguna parte puede repetir un codigo dentro de si misma.
  if (any(duplicated(x$cod_p1)) || any(duplicated(x$cod_p4)) ||
      any(duplicated(x$cod_p6))) a4 <- a4 + 1L
  # A5. Lo que se pide CALCULAR (P2/P3) tampoco puede ser opcion de la Parte 5:
  # si P3 pide calcular una magnitud, el estudiante ya sabe que esa pregunta SI
  # tiene respuesta y la descarta en P5 sin razonar sobre el hecho callado. Es
  # la misma fuga inter-parte que A3 mide para P4, aplicada a P5 (bloque que
  # auditoria_propia.R no cubria: su bloque A original solo miraba P1->otras y
  # P2/P3->P4, nunca P2/P3->P5).
  if (any(x$cod_p23 %in% x$cod_p5)) a5 <- a5 + 1L
  # A6. P5 compartiendo opcion con P6 NO es fuga y NO se cuenta como fallo: P6
  # es HIPOTETICA (pregunta que dejaria de responderse SI se quitara un dato),
  # no afirma nada sobre el enunciado original, asi que coincidir con ella no
  # revela si esa pregunta tiene o no respuesta hoy. Solo informativo.
  if (any(x$cod_p6 %in% x$cod_p5)) a6 <- a6 + 1L
  # A7. Ninguna opcion de P5 puede coincidir con una opcion de P1 (misma logica
  # que A2, aplicada a P5).
  if (any(x$cod_p1 %in% x$cod_p5)) a7 <- a7 + 1L
  # A8. Lo que ofrece P6 no puede reaparecer en P4. Las cuatro opciones de P6
  # estan TODAS determinadas hoy y su molde lo transparenta, asi que resolver P6
  # acredita que se responden con el enunciado original: cualquiera que reaparezca
  # en P4 se marca "si" sin razonar. El veto existia solo en el escalon 1 de P4
  # (objecion 2 del detractor, 2026-08-13) y NINGUNA sonda lo medía -- se corrigio
  # el veto, se anadio la asercion en C-1 y esta sonda lo deja MEDIDO, para que
  # una regresion futura se vea en cifras y no solo reviente un stopifnot.
  if (any(x$cod_p6 %in% x$cod_p4)) a8 <- a8 + 1L
}
cat(sprintf("  clave P1 o gemela reaparece en otra parte : %d/%d\n", a1, length(V)))
cat(sprintf("  opcion de P1 reaparece en P2/P3/P4        : %d/%d\n", a2, length(V)))
cat(sprintf("  magnitud de P2/P3 reaparece en P4         : %d/%d\n", a3, length(V)))
cat(sprintf("  codigo duplicado dentro de una parte      : %d/%d\n", a4, length(V)))
cat(sprintf("  magnitud de P2/P3 reaparece en P5         : %d/%d\n", a5, length(V)))
cat(sprintf("  opcion de P5 tambien es opcion de P6 (info): %d/%d\n", a6, length(V)))
cat(sprintf("  opcion de P1 reaparece en P5               : %d/%d\n", a7, length(V)))
cat(sprintf("  opcion de P6 reaparece en P4              : %d/%d\n", a8, length(V)))
if (a1 > 0) add("A1: la clave de P1 (o su gemela) reaparece en otra parte")
if (a2 > 0) add("A2: una opcion de P1 reaparece en P2/P3/P4")
if (a3 > 0) add("A3: una magnitud calculada en P2/P3 es opcion de P4")
if (a4 > 0) add("A4: codigo duplicado dentro de una parte")
if (a5 > 0) add("A5: una magnitud calculada en P2/P3 es opcion de P5")
if (a7 > 0) add("A5b: una opcion de P1 reaparece en P5")
if (a8 > 0) add("A8: una opcion de P6 reaparece en P4 (fuga: P6 acredita que se responden)")

# ---------- B) FUGA LEXICA (Error 32) --------------------------------------
# Ningun token de >2 caracteres puede ser exclusivo de la clave en >=70 % de las
# versiones, ni globalmente ni dentro de cada rama.
cat("\n== B) Fuga lexica (Error 32) ==\n")
toks <- function(s) {
  s <- tolower(s); s <- gsub("[^[:alnum:]á-úÁ-Úñü ]+", " ", s)
  t <- unique(strsplit(trimws(gsub("[[:space:]]+", " ", s)), " ")[[1]])
  t[nchar(t) > 2]
}
lex_test <- function(sub, gap_op, gap_sol, etq) {
  if (!length(sub)) return(invisible(NULL))
  # Solo los estratos (los globales llevan el N completo y siempre dictaminan).
  if (grepl(" rama ", etq, fixed = TRUE) && length(sub) < MIN_ESTRATO) {
    marcar_corto(etq, length(sub)); return(invisible(NULL))
  }
  excl <- list()
  for (x in sub) {
    op <- x[[gap_op]]; so <- x[[gap_sol]]
    ic <- which(so == 1L)
    if (length(ic) != 1L) next
    tk <- lapply(op, toks)
    ex <- setdiff(tk[[ic]], unique(unlist(tk[-ic])))
    for (t in ex) excl[[t]] <- c(excl[[t]], 1L)
  }
  n <- length(sub)
  if (!length(excl)) { cat(sprintf("  %-22s sin token exclusivo de la clave\n", etq)); return(invisible(NULL)) }
  tasas <- sort(vapply(excl, function(z) 100 * length(z) / n, numeric(1)), decreasing = TRUE)
  cat(sprintf("  %-22s peor token: '%s' exclusivo de la clave en %.1f%% de %d versiones\n",
              etq, names(tasas)[1], tasas[1], n))
  if (tasas[1] >= 70) add(sprintf("B: token '%s' exclusivo de la clave en %.1f%% (%s)",
                                  names(tasas)[1], tasas[1], etq))
  invisible(NULL)
}
for (gp in list(c("op_p1","sol_p1","P1 global"), c("op_p5","sol_p5","P5 global"),
                c("op_p6","sol_p6","P6 global")))
  lex_test(V, gp[1], gp[2], gp[3])
ramas <- sort(unique(vapply(V, function(x) x$info, character(1))))
for (rr in ramas) {
  sub <- Filter(function(x) x$info == rr, V)
  lex_test(sub, "op_p1", "sol_p1", paste0("P1 rama ", rr))
}

# ---------- C) ALCANZABILIDAD DEL BANCO (Error 27) -------------------------
cat("\n== C) Alcanzabilidad de las 20 preguntas del banco ==\n")
todos <- V[[1]]$cods
como_clave <- table(factor(vapply(V, function(x) x$clave_p1, character(1)), levels = todos))
como_dist  <- table(factor(unlist(lapply(V, function(x) setdiff(x$cod_p1, x$clave_p1))), levels = todos))
en_p4      <- table(factor(unlist(lapply(V, function(x) x$cod_p4)), levels = todos))
en_p23     <- table(factor(unlist(lapply(V, function(x) x$cod_p23)), levels = todos))
en_p6      <- table(factor(unlist(lapply(V, function(x) x$cod_p6)), levels = todos))
tot <- as.integer(como_clave) + as.integer(como_dist) + as.integer(en_p4) +
       as.integer(en_p23) + as.integer(en_p6)
df <- data.frame(codigo = todos, clave_P1 = as.integer(como_clave),
                 dist_P1 = as.integer(como_dist), P2P3 = as.integer(en_p23),
                 P4 = as.integer(en_p4), P6 = as.integer(en_p6), total = tot)
print(df, row.names = FALSE)
nunca <- df$codigo[df$total == 0]
if (length(nunca)) add(paste0("C: preguntas que NUNCA aparecen: ", paste(nunca, collapse = ", ")))
cat(sprintf("  preguntas que nunca aparecen en ninguna parte: %d\n", length(nunca)))
# Toda pregunta que pueda ser indeterminada deberia poder ser clave alguna vez.
cat(sprintf("  preguntas que nunca son clave de P1: %d (%s)\n",
            sum(df$clave_P1 == 0),
            paste(df$codigo[df$clave_P1 == 0], collapse = ", ")))

# ---------- D) H1 / H3b POR RAMA ------------------------------------------
cat("\n== D) H1 y H3b CONDICIONADOS POR RAMA (el agregado no acredita) ==\n")
sonda_rama <- function(sub, gap_op, gap_sol, etq) {
  n <- length(sub)
  if (n < MIN_ESTRATO) { marcar_corto(etq, n); return(invisible()) }
  umax <- umin <- 0L; claves <- character(0)
  for (x in sub) {
    op <- x[[gap_op]]; so <- x[[gap_sol]]; ic <- which(so == 1L)
    if (length(ic) != 1L) next
    nc <- nchar(op)
    if (nc[ic] == max(nc) && sum(nc == max(nc)) == 1L) umax <- umax + 1L
    if (nc[ic] == min(nc) && sum(nc == min(nc)) == 1L) umin <- umin + 1L
    sg <- tolower(gsub("[[:digit:]]+", "", op[ic]))
    claves <- c(claves, trimws(gsub("[^[:alnum:]á-úÁ-Úñü ]+", " ", sg)))
  }
  h3b <- if (length(claves)) 100 * max(table(claves)) / length(claves) else NA
  cat(sprintf("  %-16s n=%3d  unica-mas-larga=%5.1f%%  unica-mas-corta=%5.1f%%  H3b=%5.1f%%\n",
              etq, n, 100 * umax / n, 100 * umin / n, h3b))
  if (100 * umax / n >= 90) add(sprintf("D: clave siempre la mas larga en %s (%.1f%%)", etq, 100 * umax / n))
  if (100 * umin / n >= 90) add(sprintf("D: clave siempre la mas corta en %s (%.1f%%)", etq, 100 * umin / n))
  if (!is.na(h3b) && h3b >= 90) add(sprintf("D: H3b invariante en %s (%.1f%%)", etq, h3b))
  invisible()
}
for (rr in ramas) {
  sub <- Filter(function(x) x$info == rr, V)
  sonda_rama(sub, "op_p1", "sol_p1", paste0("P1 ", rr))
}
cat("  --- Partes 5 y 6 por rama ---\n")
for (rr in ramas) {
  sub <- Filter(function(x) x$info == rr, V)
  sonda_rama(sub, "op_p5", "sol_p5", paste0("P5 ", rr))
  sonda_rama(sub, "op_p6", "sol_p6", paste0("P6 ", rr))
}

# ---------- E) COHERENCIA DE CLAVES ---------------------------------------
cat("\n== E) Coherencia interna ==\n")
e1 <- sum(vapply(V, function(x) sum(x$sol_p1) != 1L, logical(1)))
e2 <- sum(vapply(V, function(x) sum(x$sol_p5) != 1L, logical(1)))
e3 <- sum(vapply(V, function(x) sum(x$sol_p6) != 1L, logical(1)))
e4 <- sum(vapply(V, function(x) sum(x$sol_p4) < 1L || sum(x$sol_p4) >= length(x$sol_p4), logical(1)))
e5 <- sum(vapply(V, function(x) length(unique(x$op_p4)) != length(x$op_p4), logical(1)))
cat(sprintf("  P1 sin clave unica: %d | P5: %d | P6: %d\n", e1, e2, e3))
cat(sprintf("  P4 sin al menos 1 V y 1 F: %d | P4 con opciones repetidas: %d\n", e4, e5))
if (e1 + e2 + e3 + e4 + e5 > 0) add("E: incoherencia en las claves de algun gap")
nv4 <- table(vapply(V, function(x) sum(x$sol_p4), integer(1)))
cat("  reparto del numero de VERDADERAS en P4: "); print(nv4)
cat("  escalones usados -> P2/P3: "); print(table(vapply(V, function(x) x$niv23, integer(1))))
cat("  escalones usados -> P4   : "); print(table(vapply(V, function(x) x$niv4, integer(1))))
cat("  escalones usados -> P6   : "); print(table(vapply(V, function(x) x$niv6, integer(1))))
cat("  ramas: "); print(table(vapply(V, function(x) x$info, character(1))))
cat("  P5 interroga la clave de P1 (fallback): ",
    sum(vapply(V, function(x) isTRUE(x$p5_fb), logical(1))), "/", length(V), "\n", sep="")
cat("  preguntas distintas interrogadas por P5: ",
    length(unique(vapply(V, function(x) x$q_p5, character(1)))), "\n", sep="")

cat("\n==========================================\n")
if (length(cortos)) {
  # El estrato mas pequeno fija el N necesario: N_nec = NV * MIN_ESTRATO / n_min.
  n_min <- min(as.integer(sub(".*\\(n=([0-9]+)\\)$", "\\1", cortos)))
  n_nec <- ceiling(length(V) * MIN_ESTRATO / max(n_min, 1L) / 50) * 50L
  cat("ESTRATOS NO CONCLUYENTES (", length(cortos), "):\n", sep = "")
  for (c in cortos) cat("  ? ", c, "\n", sep = "")
  cat("  -> Con N=", length(V), " la rama mas ligera queda en n=", n_min,
      ". Para dictaminarlos hace falta N>=", n_nec, ":\n", sep = "")
  cat("     Rscript auditoria_propia.R <rmd> ", n_nec, "\n", sep = "")
  cat("  (regla #23: el estandar N=100 es para medidas GLOBALES; el analisis\n")
  cat("   estratificado declara su submuestra y no se sella con estratos sin medir)\n")
}
if (length(fallos)) {
  cat("RESULTADO: RECHAZADO (", length(fallos), " hallazgos)\n", sep = "")
  for (f in fallos) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
} else if (length(cortos)) {
  cat("RESULTADO: SIN VEREDICTO (0 hallazgos en lo medido, ", length(cortos),
      " estratos sin medir)\n", sep = "")
  quit(status = 1)
} else {
  cat("RESULTADO: APROBADO (0 hallazgos, todos los estratos dictaminados)\n")
  quit(status = 0)
}
