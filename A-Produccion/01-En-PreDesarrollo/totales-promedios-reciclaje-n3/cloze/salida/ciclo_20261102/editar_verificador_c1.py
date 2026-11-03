#!/usr/bin/env python3
# editar_verificador_c1.py — tanda C1 del ciclo 2026-11-02 del CLOZE (2026-11-03): cambios de verificar_espacio_cloze.R.
# Cada patrón se exige exactamente las veces indicadas; si no, aborta sin escribir. Los escapes Unicode del código R se
# escriben con marcadores (@U00A0@ -> \u00a0; @BSU00A0@ -> \\u00a0) para que ninguna herramienta los vuelva literales.
# Uso: python3 -I editar_verificador_c1.py <ruta de verificar_espacio_cloze.R>
import hashlib, re, sys
ruta = sys.argv[1]
src = open(ruta, encoding="utf-8").read()
antes = hashlib.md5(src.encode("utf-8")).hexdigest()
BS = "\\"
def esc(t):
    for cp in (0x00a0, 0x2009, 0x202f, 0x2060, 0x00bb, 0x201d, 0x2026):
        t = t.replace("@BSU%04X@" % cp, BS + BS + "u%04x" % cp).replace("@U%04X@" % cp, BS + "u%04x" % cp)
    return t

CAB = r'''##       Tanda C1 del ciclo 2026-11-02 (2026-11-03): la Parte 1 se re-empalma desde el SCHOICE aprobado 63e2713e. rec() ya
##       no lee lado_gdis (S7 lo quitó de la canónica y ningún consumidor del CLOZE usa el campo): MC40. E4 mide la batería
##       §P7 y los márgenes de la Parte 1 con el espacio duro normalizado (decisión (a) del SCHOICE, por F-2; las guardas de
##       texto miran el texto crudo): MC43. MP2, adaptado al espacio duro. La firma de la S2d (41 cifras) y la lectura (iii)
##       se extienden por alcance a la Parte 1 (decisión 3 del 2026-11-03) con el código de SUB/verificar_espacio.R: MC41 y
##       MC42. L-6 en la prosa propia de la Parte 1 de la Solution, con controles exactos: MC45 a MC48. Sin literales U+00A0,
##       U+2009, U+202F ni U+2060 en el .Rmd ni en este verificador: MC44.
'''

NORM = r'''## Decisión (a) del SCHOICE (2026-11-02), extendida a la Parte 1 por F-2 (tanda C1): la batería §P7 y los márgenes de
## largo de la Parte 1 miden el texto que lee el estudiante, con U+00A0 convertido en espacio. Las guardas de texto (L-6,
## S1) miran el texto crudo. MC43 corre el juicio del canal de largo sin normalizar.
norm_ed <- function(o) gsub("@U00A0@", " ", o, fixed = TRUE)
'''

E4C1 = r'''
## ---- E4 · C1 (tanda C1, 2026-11-03): la firma de la S2d y la lectura (iii), extendidas por alcance a la Parte 1 --------
## Decisión 3 del profesor (2026-11-03), literal: «Extender por alcance (Recomendada)». Las 41 cifras de FIRMA_S2D y las
## leyes L-6, L-7 y L-8 rigen la Parte 1 por identidad, como F-2 extendió «O-1 corregido». Se miden con el MISMO código
## del SCHOICE: la región «S2b … S2d» de SUB/verificar_espacio.R se lee por sus marcadores (cada uno una sola vez) y se
## evalúa sobre las muestras de este verificador (NAT, CU y CANON_V), que por E1 son las del SCHOICE (mismas semillas:
## 1..100 y las cuotas desde la 1001). Dentro de su cifra, «ROJO, cubierto por la firma de la S2d» (exit 4); por encima,
## otra regla > +8 pp o una cifra que se mueva de lo firmado: PENDIENTE (exit 3). L-8 no está cableada en el verificador
## del SCHOICE (firmada, pendiente de cablear): aquí no se inventa; se declara. MC41 y MC42 sacan cifras de «cubierto».
titulo("E4 · C1 · Firma de la S2d (41 cifras) y lectura (iii) sobre la Parte 1 (decisión 3 del 2026-11-03)")
L_S <- readLines(VER_S, encoding = "UTF-8")
i_a <- grep("^## ---- S2b \\(FASE 2C r6, H1\\)", L_S)
i_b <- grep('^source\\(file.path\\(REPO, ".claude/scripts/bateria_referencia_icfes.R"\\)\\)', L_S)
stopifnot("región S2b-S2d de SUB/verificar_espacio.R: marcadores únicos" = length(i_a) == 1L && length(i_b) == 1L && i_b > i_a)
REGION_S2D <- parse(text = paste(L_S[i_a:(i_b - 1L)], collapse = "\n"), keep.source = FALSE, encoding = "UTF-8")
cat(sprintf("  región S2b-S2d de SUB/verificar_espacio.R (md5 %s): líneas %d a %d\n", unname(tools::md5sum(VER_S)), i_a, i_b - 1L))
corre_s2d <- function(nat, cu, canv, silencio = FALSE) {
  en <- new.env(parent = ENV_S); en$NAT <- nat; en$CU <- cu; en$CANON_V <- canv
  if (silencio) { nada <- function(...) invisible(NULL); en$ok <- nada; en$pendiente <- nada; en$firmado <- nada
    en$writeLines <- nada; en$print <- nada } else { en$ok <- ok; en$pendiente <- pendiente; en$firmado <- firmado }
  eval(REGION_S2D, envir = en); en
}
S2D <- corre_s2d(NAT, CU, CANON_V)
d_s2d <- S2D$FIRMA_S2D$id[!S2D$ig_s2d]
if (length(d_s2d)) pendiente("C1 · extensión por alcance: ", length(d_s2d), " de las 41 cifras de la S2d se mueven sobre la Parte 1 (",
                             paste(d_s2d, collapse = ", "), "): la identidad ya no rige; vuelve al profesor") else
  ok(TRUE, "C1 · extensión por alcance: las 41 cifras de la S2d, medidas sobre la Parte 1 del CLOZE, son iguales a lo firmado (a un decimal)")
cat("  L-8 (ERR-RESTA llena el hueco por debajo de D): firmada el 2026-11-03 y PENDIENTE DE CABLEAR en SUB/verificar_espacio.R. Este verificador no la inventa: sin guarda, no es una invariante.\n")

## L-6 en la prosa propia de la Parte 1 de la Solution del CLOZE (tanda C1): la unidad y el signo dentro de la fórmula y el
## espacio duro entre cifra y unidad, con las mismas cadenas que la Solution del SCHOICE. Se emite el chunk «solucion» en
## las N versiones de la mezcla y se aísla la prosa de la Parte 1: el párrafo «Parte 1.» del procedimiento (hasta «Parte
## 2.»), el análisis de sus distractores (hasta «Parte 2 ·») y el párrafo «En la Parte 1,» del caso específico. Sondas, que
## tienen que dar 0: unidad fuera de una fórmula y signo fuera de una fórmula (hasta 3 caracteres que no sean letra, cifra,
## $ ni salto de línea), y cifra-unidad o El/La con espacio normal. Controles EXACTOS en la prosa propia (procedimiento y caso
## específico): 6 unidades dentro de la fórmula por versión, 3 con el signo dentro. Las Partes 2 a 6 quedan fuera: su texto
## no cambia, y sus fórmulas con la unidad fuera son el BAJA 1 de la 6b r3, declarado. MC45 a MC48.
SOL_C <- local({ l <- readLines(RMD_C, encoding = "UTF-8"); i0 <- grep("^```\\{r solucion", l); i1 <- which(l == "```" & seq_along(l) > i0[1])[1]
  stopifnot("un solo chunk solucion" = length(i0) == 1L); paste(l[(i0 + 1):(i1 - 1)], collapse = "\n") })
ENVS_NAT <- Filter(function(e) is.null(e$.err), lapply(seq_len(N), function(z) correr(EXPR_C, z)))
p1_de_sol <- function(t) {
  a <- regmatches(t, regexpr("Parte 1\\. El concurso premia[\\s\\S]*?(?=\\nParte 2\\. )", t, perl = TRUE))
  b <- regmatches(t, regexpr("\\*\\*Parte 1 · [\\s\\S]*?(?=\\*\\*Parte 2 · )", t, perl = TRUE))
  c <- regmatches(t, regexpr("En la Parte 1, [^\\n]*", t, perl = TRUE))
  if (length(a) != 1L || length(b) != 1L || length(c) != 1L) return(NULL)
  list(propia = c(a, c), dist = b)
}
RE_UNI <- "\\$[^A-Za-z0-9$\\n]{0,3}(kilos?|cursos?)\\b"
RE_SIG <- "\\$[^A-Za-z0-9$\\n]{0,3}[.,:;!?)\\]@U00BB@@U201D@@U2026@]"
RE_ESP <- "[0-9] (kilos?|cursos?)\\b|El Bosque|La Pradera"
RE_DENTRO <- "\\\\text\\{(kilos?|cursos?)[.,:;!?]?\\}\\$"
RE_DENTRO_SIG <- "\\\\text\\{(kilos?|cursos?)[.,:;!?]\\}\\$"
cuenta_re <- function(re, t) sum(lengths(regmatches(t, gregexpr(re, t, perl = TRUE))))
l6_p1 <- function(sol_txt) {
  ex <- parse(text = sol_txt, encoding = "UTF-8"); z <- c(uni = 0, sig = 0, esp = 0, dentro = 0, dentro_sig = 0, sin_p1 = 0)
  for (en in ENVS_NAT) {
    t <- paste(capture.output(eval(ex, envir = new.env(parent = en))), collapse = "\n"); p <- p1_de_sol(t)
    if (is.null(p)) { z[["sin_p1"]] <- z[["sin_p1"]] + 1; next }
    todo <- c(p$propia, p$dist)
    z <- z + c(cuenta_re(RE_UNI, todo), cuenta_re(RE_SIG, todo), cuenta_re(RE_ESP, todo),
               cuenta_re(RE_DENTRO, p$propia), cuenta_re(RE_DENTRO_SIG, p$propia), 0)
  }
  z
}
L6 <- l6_p1(SOL_C); nv <- length(ENVS_NAT)
ok(L6[["sin_p1"]] == 0 && L6[["uni"]] == 0 && L6[["sig"]] == 0 && L6[["esp"]] == 0,
   sprintf("C1 · L-6 en la prosa de la Parte 1 de la Solution (%d versiones): unidad fuera de una fórmula %d · signo fuera de una fórmula %d · cifra-unidad o El/La con espacio normal %d · versiones sin la prosa de la Parte 1 %d",
           nv, L6[["uni"]], L6[["sig"]], L6[["esp"]], L6[["sin_p1"]]))
ok(L6[["dentro"]] == 6 * nv && L6[["dentro_sig"]] == 3 * nv,
   sprintf("C1 · L-6, controles exactos en la prosa propia de la Parte 1: %d unidades dentro de la fórmula (esperadas %d) y %d con el signo dentro (esperadas %d)",
           L6[["dentro"]], 6 * nv, L6[["dentro_sig"]], 3 * nv))
## Sin literales U+00A0, U+2009, U+202F ni U+2060 en el .Rmd ni en este verificador (L-6: ED es una variable). MC44.
RE_LIT <- "[@U00A0@@U2009@@U202F@@U2060@]"
lit_en <- function(txt) sum(grepl(RE_LIT, txt, perl = TRUE))
ok(lit_en(readLines(RMD_C, encoding = "UTF-8")) == 0L && lit_en(readLines(file.path(DIR, "verificar_espacio_cloze.R"), encoding = "UTF-8")) == 0L,
   "C1 · L-6: ni el .Rmd ni este verificador llevan U+00A0, U+2009, U+202F o U+2060 literales")
'''

MUT = r'''
## ---- Mutantes de la tanda C1 (2026-11-03) -----------------------------------------------------------------------------
## MC40: rec() vuelve a leer lado_gdis. Sonda: rec() sobre una canónica, que desde S7 no lo asigna (el error nombra el objeto).
txt_rec <- local({ l <- readLines(file.path(DIR, "verificar_espacio_cloze.R"), encoding = "UTF-8"); i0 <- which(l == "rec <- function(en) {")
  if (length(i0) != 1L) return(NA_character_); i1 <- which(l == "}" & seq_along(l) > i0)[1]; paste(l[i0:i1], collapse = "\n") })
n40 <- if (is.na(txt_rec)) 0L else lengths(regmatches(txt_rec, gregexpr("gi = gi,", txt_rec, fixed = TRUE)))
corre_rec <- function(f, en) tryCatch({ f(en); "ok" }, error = function(e) conditionMessage(e))
en_can <- correr(EXPR_C, SEM_CAN[1]); en_nc <- Filter(function(e) !isTRUE(e$es_canonica), ENVS_NAT)[[1]]
ok(identical(corre_rec(rec, en_can), "ok") && identical(corre_rec(rec, en_nc), "ok") && !exists("lado_gdis", envir = en_can, inherits = FALSE),
   "C1 · rec() corre en una canónica (semilla ", SEM_CAN[1], ", sin lado_gdis en su entorno) y en una no canónica")
if (n40 != 1L) { falla("MC40 MAL CONSTRUIDO: «gi = gi,» aparece ", n40, " veces en rec()"); filas_mut[["MC40"]] <- c("MC40", "C1: rec() en la canónica", "-", "mal_construido", NA) } else {
  e40 <- new.env(); eval(parse(text = sub("gi = gi,", "gi = gi, lado = lado_gdis,", txt_rec, fixed = TRUE), encoding = "UTF-8"), envir = e40)
  r_can <- corre_rec(e40$rec, en_can); r_nc <- corre_rec(e40$rec, en_nc)
  llego40 <- !identical(deparse(e40$rec), deparse(rec)) && identical(r_nc, "ok")
  ver40 <- if (!llego40) "mal_construido" else if (grepl("lado_gdis", r_can, fixed = TRUE)) "cazado_por_su_sonda" else if (!identical(r_can, "ok")) "cazado_por_otra" else "no_detectado"
  ok(ver40 == "cazado_por_su_sonda", sprintf("MC40 (rec() vuelve a leer lado_gdis): en la canónica (semilla %d) «%s»; en una no canónica «%s» — %s", SEM_CAN[1], r_can, r_nc, ver40))
  filas_mut[["MC40"]] <- c("MC40", "C1: rec() corre en la canónica", r_can, ver40, SEM_CAN[1])
}
## MC41 y MC42: M44 y M45 del SCHOICE, sobre el bloque P1 del CLOZE (en memoria, patrón único), con las mismas cuotas por rama
## (semillas desde la 1001) y la misma puerta, veredicto_s2d() de la región S2b-S2d.
P1_TXT <- p1_de(COD_C)
mutar_p1 <- function(de, a) { n <- lengths(regmatches(P1_TXT, gregexpr(de, P1_TXT, fixed = TRUE)))
  if (n != 1L) return(list(error = sprintf("el patrón «%s» aparece %d veces en el bloque P1 (se exige 1)", substr(de, 1, 50), n)))
  list(expr = parse(text = sub(de, a, P1_TXT, fixed = TRUE), encoding = "UTF-8")) }
rec_p1 <- function(en) with(en, list(canon = isTRUE(es_canonica), rama = rama, opciones = opciones_p1, clave = which(sol_p1), rol = rol_opcion,
  D = d$D, X = afs$gdis$x, cod_gdis = afs$gdis$codigo, sede_o = vapply(afs[rol_opcion], function(a) a$sede, ""),
  tabla = c(d$nG, d$nP, d$aG, d$aP), gi = gi))
cuotas_p1 <- function(expr) { CUm <- list(CURSOS = list(), PROMEDIO = list()); err <- 0L
  for (z in 1000L + seq_len(TOPE)) { if (all(lengths(CUm) >= N)) break
    en <- correr(expr, z); if (!is.null(en$.err)) { err <- err + 1L; next }; if (isTRUE(en$es_canonica)) next
    x <- rec_p1(en); if (length(CUm[[x$rama]]) < N) CUm[[x$rama]][[length(CUm[[x$rama]]) + 1]] <- x }
  list(CU = CUm, err = err) }
m41 <- mutar_p1("cod_gdis <- pick(unique(cand_g$codigo))", 'cod_gdis <- if ("ERR-RESTA" %in% cand_g$codigo) "ERR-RESTA" else pick(unique(cand_g$codigo))')
if (!is.null(m41$error)) { falla("MC41 MAL CONSTRUIDO: ", m41$error); filas_mut[["MC41"]] <- c("MC41", "C1 · S2d: RS1 fuera de «cubierto»", "-", "mal_construido", NA) } else {
  q41 <- cuotas_p1(m41$expr); C41 <- q41$CU
  cuota_r <- function(L) vapply(L, function(V) mean(vapply(V, function(x) identical(x$cod_gdis, "ERR-RESTA"), TRUE)), 0)
  k0 <- cuota_r(CU[c("CURSOS", "PROMEDIO")]); k41 <- cuota_r(C41)
  t0 <- S2D$TAB_H1[S2D$R_RESTA, c("CURSOS", "PROMEDIO")]; t41 <- vapply(C41, function(V) mean(vapply(V, S2D$H1_REGLAS[[S2D$R_RESTA]], 0)), 0)
  v41 <- vapply(c("CURSOS", "PROMEDIO"), function(b) S2D$veredicto_s2d(S2D$R_RESTA, b, t41[[b]]), "")
  ver41 <- if (!all(lengths(C41) >= 20L) || !all(k41 > k0)) "mal_construido" else if (q41$err > 0L) "cazado_por_otra" else
    if (any(v41 == "pendiente")) "cazado_por_su_sonda" else "no_detectado"
  d41 <- sprintf("ERR-RESTA en el distractor de G %.0f -> %.0f %% (CURSOS) y %.0f -> %.0f %% (PROMEDIO); RS1 %.1f -> %.1f %% «%s» y %.1f -> %.1f %% «%s»",
                 100 * k0[["CURSOS"]], 100 * k41[["CURSOS"]], 100 * k0[["PROMEDIO"]], 100 * k41[["PROMEDIO"]],
                 100 * t0[["CURSOS"]], 100 * t41[["CURSOS"]], v41[["CURSOS"]], 100 * t0[["PROMEDIO"]], 100 * t41[["PROMEDIO"]], v41[["PROMEDIO"]])
  ok(ver41 == "cazado_por_su_sonda", "MC41 (M44 del SCHOICE sobre el bloque P1: ERR-RESTA siempre que se pueda): ", d41, " — ", ver41)
  filas_mut[["MC41"]] <- c("MC41", "C1 · S2d: RS1 fuera de «cubierto»", d41, ver41, NA)
}
m42 <- mutar_p1("gi <- pick(1:2)", "gi <- 2L")
if (!is.null(m42$error)) { falla("MC42 MAL CONSTRUIDO: ", m42$error); filas_mut[["MC42"]] <- c("MC42", "C1 · S2d: BE9 y BE11 fuera de «cubierto»", "-", "mal_construido", NA) } else {
  q42 <- cuotas_p1(m42$expr); C42 <- q42$CU
  F42 <- S2D$FIRMA_S2D[S2D$FIRMA_S2D$id %in% c("BE9", "BE10", "BE11", "BE12"), ]
  t42 <- mapply(function(rg, es) if (length(C42[[es]])) mean(vapply(C42[[es]], S2D$BARRIDO_III[[rg]], 0)) else NA_real_, F42$regla, F42$estrato)
  v42 <- mapply(S2D$veredicto_s2d, F42$regla, F42$estrato, t42)
  llego42 <- all(lengths(C42) >= 20L) && all(vapply(c(C42$CURSOS, C42$PROMEDIO), function(x) identical(as.integer(x$gi), 2L), TRUE)) &&
    any(vapply(c(CU$CURSOS, CU$PROMEDIO), function(x) identical(as.integer(x$gi), 1L), TRUE))
  ver42 <- if (!llego42) "mal_construido" else if (q42$err > 0L) "cazado_por_otra" else if (any(v42 == "pendiente")) "cazado_por_su_sonda" else "no_detectado"
  d42 <- paste(sprintf("%s %s %.1f -> %.1f %% «%s»", F42$id, F42$estrato, F42$acierto, 100 * t42, v42), collapse = "; ")
  ok(ver42 == "cazado_por_su_sonda", "MC42 (M45 del SCHOICE sobre el bloque P1: la fila de G siempre abajo, gi = 2): ", d42, " — ", ver42)
  filas_mut[["MC42"]] <- c("MC42", "C1 · S2d: BE9 y BE11 fuera de «cubierto»", d42, ver42, NA)
}
## MC43 (decisión (a), como M28 del SCHOICE): el juicio del canal de largo de la rama PROMEDIO, sin normalizar el espacio duro.
## Con la normalización queda dentro de lo declarado en «O-1 corregido»; sin ella, su margen en palabras tiene que salir.
juicio_largo <- function(L, nf) {
  eb <- get("evaluar_bateria", envir = ENV_S); ops <- lapply(L, function(x) nf(x$opciones)); cl <- vapply(L, function(x) x$clave, 0L)
  res <- eb(REGLAS, ops, cl, familias_no_aplicables = ENV_S$NA_FAM, estimulo_no_aplicable = ESTIMULO_P1)
  mg <- function(f) vapply(L, function(x) { n <- nchar(nf(x$opciones)); k <- x$clave
    if (sum(n == f(n)) != 1 || n[k] != f(n)) return(NA_real_); abs(n[k] - f(n[-k])) / f(n[-k]) }, 0)
  mmax <- max(c(mg(max), mg(min), 0), na.rm = TRUE)
  mt <- vapply(L, function(x) { k <- vapply(nf(x$opciones), ENV_S$ntok, 0L); dv <- abs(k - stats::median(k))
    if (all(dv == 0) || which.max(dv) != x$clave) return(NA_real_)
    cerca <- k[-x$clave][which.min(abs(k[-x$clave] - k[x$clave]))]; abs(k[x$clave] - cerca) / cerca }, 0)
  tmax <- max(c(mt, 0), na.rm = TRUE)
  if (res$exceso <= 0.08) return(list(v = "ok", tmax = tmax, mmax = mmax, ex = res$exceso))
  res2 <- eb(Filter(function(r) !r$nombre %in% LARGO, REGLAS), ops, cl, familias_no_aplicables = ENV_S$NA_FAM, estimulo_no_aplicable = ESTIMULO_P1)
  dentro <- res2$exceso <= 0.08 && hasta(res$exceso, FL[["exceso"]]) && hasta(mmax, FL[["chars"]]) && hasta(tmax, FL[["palabras"]])
  list(v = if (dentro) "declarado" else "pendiente", tmax = tmax, mmax = mmax, ex = res$exceso)
}
j_con <- juicio_largo(CU$PROMEDIO, norm_ed); j_sin <- juicio_largo(CU$PROMEDIO, identity)
ver43 <- if (!identical(j_con$v, "declarado")) "mal_construido" else if (j_sin$v == "pendiente" && !hasta(j_sin$tmax, FL[["palabras"]])) "cazado_por_su_sonda" else
  if (j_sin$v == "pendiente") "cazado_por_otra" else "no_detectado"
d43 <- sprintf("rama PROMEDIO: con la normalización «%s» (%+.1f pp; márgenes %.1f %% y %.1f %%); sin ella «%s» (%+.1f pp; márgenes %.1f %% y %.1f %%; declarado hasta 2,7 %% y 6,5 %%)",
               j_con$v, 100 * j_con$ex, 100 * j_con$mmax, 100 * j_con$tmax, j_sin$v, 100 * j_sin$ex, 100 * j_sin$mmax, 100 * j_sin$tmax)
ok(ver43 == "cazado_por_su_sonda", "MC43 (E4 sin normalizar el espacio duro): ", d43, " — ", ver43)
filas_mut[["MC43"]] <- c("MC43", "C1: canal de largo PROMEDIO, margen en palabras", d43, ver43, NA)
## MC44: un U+00A0 literal en el .Rmd (en ED <- "…", con el mismo valor en R). Sonda: la guarda de literales de L-6.
txt44 <- readLines(RMD_C, encoding = "UTF-8"); i44 <- which(txt44 == 'ED <- "@BSU00A0@"')
if (length(i44) < 1L) { falla("MC44 MAL CONSTRUIDO: no hay ninguna línea ED <- \"\\u00a0\" en el .Rmd"); filas_mut[["MC44"]] <- c("MC44", "C1 · L-6: sin literales", "-", "mal_construido", NA) } else {
  m44 <- txt44; m44[i44[1]] <- sub("@BSU00A0@", "@U00A0@", m44[i44[1]], fixed = TRUE)
  llego44 <- !identical(m44, txt44) && identical(eval(parse(text = m44[i44[1]], encoding = "UTF-8"), envir = new.env()), "@U00A0@")
  ver44 <- if (!llego44) "mal_construido" else if (lit_en(m44) == 1L) "cazado_por_su_sonda" else "no_detectado"
  ok(ver44 == "cazado_por_su_sonda", sprintf("MC44 (U+00A0 literal en la línea %d del .Rmd, mismo valor de ED): la guarda de literales ve %d líneas — %s", i44[1], lit_en(m44), ver44))
  filas_mut[["MC44"]] <- c("MC44", "C1 · L-6: sin literales", sprintf("%d línea con literal", lit_en(m44)), ver44, NA)
}
## MC45 a MC48: L-6 en la prosa propia de la Parte 1 de la Solution. MC45 a MC47 son ADITIVOS (crean una sola clase de
## defecto y dejan quietos los controles); MC48 quita una unidad de un sitio y solo lo ve su control exacto.
mut_l6 <- function(id, de, a, sonda, desc) {
  n <- lengths(regmatches(SOL_C, gregexpr(de, SOL_C, fixed = TRUE)))
  if (n != 1L) { falla(id, " MAL CONSTRUIDO: el patrón aparece ", n, " veces en el chunk solucion"); filas_mut[[id]] <<- c(id, paste("C1 · L-6:", sonda), "-", "mal_construido", NA); return(invisible()) }
  dif <- l6_p1(sub(de, a, SOL_C, fixed = TRUE)) - L6; hits <- names(dif)[dif != 0]
  ver <- if (!length(hits)) "no_detectado" else if (identical(hits, sonda)) "cazado_por_su_sonda" else if (sonda %in% hits) "cazado_por_su_sonda_y_otra" else "cazado_por_otra"
  d <- paste(sprintf("%s %+d", hits, as.integer(dif[hits])), collapse = ", ")
  ok(ver == "cazado_por_su_sonda", sprintf("%s (%s): %s — %s", id, desc, d, ver))
  filas_mut[[id]] <<- c(id, paste("C1 · L-6:", sonda), d, ver, NA)
}
mut_l6("MC45", '", solo una cita la diferencia correcta, ", d$D, ED, "kilos.\\n\\n"', '", solo una cita la diferencia correcta, $", d$D, "$", ED, "kilos.\\n\\n"',
       "uni", "la cifra de «solo una cita la diferencia correcta» pasa a fórmula con la unidad fuera")
mut_l6("MC46", '" = ", d$D, "\\\\ \\\\text{kilos}$ más. "', '" = ", d$D, "\\\\ \\\\text{kilos}$. Más "',
       "sig", "«Ganó …»: un punto tras el cierre de la fórmula")
mut_l6("MC47", '", solo una cita la diferencia correcta, ", d$D, ED, "kilos.\\n\\n"', '", solo una cita la diferencia correcta, ", d$D, " kilos.\\n\\n"',
       "esp", "«solo una cita la diferencia correcta»: espacio normal entre cifra y unidad")
mut_l6("MC48", '"\\\\ \\\\text{kilos}$ a favor de "', '"$ a favor de "',
       "dentro", "caso específico: la unidad desaparece de «a favor de» (solo el control exacto)")
'''

E = [
  ('gsub("' + chr(0xa0) + '", " ", gsub("<span', 'gsub("@U00A0@", " ", gsub("<span', 1),
  ("de L-C7) y el rótulo de L-C2 «muestral sobre la Parte 1» (H1).\n",
   "de L-C7) y el rótulo de L-C2 «muestral sobre la Parte 1» (H1).\n" + CAB, 1),
  ('" (aprobado para el aula: e293d5c812b1f9790e2bb777b62de9a5)\\n"', '" (aprobado para el aula: 63e2713e6ac0bdbf250f4dc33901b2ec; re-empalmado en la tanda C1)\\n"', 1),
  ("cap1 <- function(s) paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))\n",
   "cap1 <- function(s) paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))\n" + NORM, 1),
  ("tabla = c(d$nG, d$nP, d$aG, d$aP), gi = gi, lado = lado_gdis,", "tabla = c(d$nG, d$nP, d$aG, d$aP), gi = gi,", 1),
  ("L <- est[[2]]; ops <- lapply(L, function(x) x$opciones);", "L <- est[[2]]; ops <- lapply(L, function(x) norm_ed(x$opciones));", 1),
  ("mg <- function(f) vapply(L, function(x) { n <- nchar(x$opciones); k <- x$clave", "mg <- function(f) vapply(L, function(x) { n <- nchar(norm_ed(x$opciones)); k <- x$clave", 1),
  ("mt <- vapply(L, function(x) { k <- vapply(x$opciones, ENV_S$ntok, 0L);", "mt <- vapply(L, function(x) { k <- vapply(norm_ed(x$opciones), ENV_S$ntok, 0L);", 1),
  ('get(".conjunto_de", envir = ENV_S)(r$fn(x$opciones), 4L)', 'get(".conjunto_de", envir = ENV_S)(r$fn(norm_ed(x$opciones)), 4L)', 1),
  ('get(".conjunto_de", envir = ENV_S)(rg$fn(x$opciones), 4L)', 'get(".conjunto_de", envir = ENV_S)(rg$fn(norm_ed(x$opciones)), 4L)', 1),
  ("nl_can <- mean(vapply(CANON_V, function(x) { n <- nchar(x$opciones);", "nl_can <- mean(vapply(CANON_V, function(x) { n <- nchar(norm_ed(x$opciones));", 1),
  ('mut_p1("MP2", "regla", "mas digitos", "recogió 40 kilos más", "recogió 4 kilos más")\n',
   'mut_p1("MP2", "regla", "mas digitos", "recogió 40@U00A0@kilos más", "recogió 4@U00A0@kilos más")   # C1: con el espacio duro de L-6\n' + MUT, 1),
  ('"MP1", "MP2"), c("id", "veredicto", "sonda_esperada", "sonda_real")]',
   '"MP1", "MP2", "MC40", "MC41", "MC42", "MC43", "MC44", "MC45", "MC46", "MC47", "MC48"), c("id", "veredicto", "sonda_esperada", "sonda_real")]', 1),
  ("la regla por palabra de la Parte 6 firmada regla por regla (L-C7) por la decisión «Va»; regla #22 §P7-E):",
   "la regla por palabra de la Parte 6 firmada regla por regla (L-C7) por la decisión «Va»; la firma de la S2d (41 cifras), extendida por alcance a la Parte 1 (decisión 3, tanda C1); regla #22 §P7-E):", 1),
  ("la regla por palabra de la Parte 6 firmada regla por regla por la decisión «Va»; 0 fallos, 0 pendientes)",
   "la regla por palabra de la Parte 6 firmada regla por regla por la decisión «Va», y la firma de la S2d extendida a la Parte 1; 0 fallos, 0 pendientes)", 1),
]
for viejo, nuevo, n in E:
    k = src.count(viejo)
    if k != n:
        sys.exit(f"ABORTO sin escribir: el patrón aparece {k} veces (se esperaban {n}): {viejo[:90]!r}")
    src = src.replace(viejo, esc(nuevo))
# E4 · C1, justo después del residuo «descarta la más larga» de la canónica (una sola línea)
m = [x for x in re.finditer(r'^residuo\(sprintf\("canónica de la Parte 1, fuera de la batería congelada.*\n', src, re.M)]
if len(m) != 1:
    sys.exit(f"ABORTO sin escribir: el ancla de E4 · C1 aparece {len(m)} veces")
src = src[:m[0].end()] + esc(E4C1) + src[m[0].end():]
for ch in ("\u00a0", "\u2009", "\u202f", "\u2060"):
    if ch in src:
        sys.exit(f"ABORTO sin escribir: queda un U+{ord(ch):04X} literal en el verificador")
if "@U" in src or "@BSU" in src:
    sys.exit("ABORTO sin escribir: queda un marcador sin sustituir")
open(ruta, "w", encoding="utf-8").write(src)
print(f"{len(E) + 1} cambios aplicados; md5 {antes} -> {hashlib.md5(src.encode('utf-8')).hexdigest()}")
