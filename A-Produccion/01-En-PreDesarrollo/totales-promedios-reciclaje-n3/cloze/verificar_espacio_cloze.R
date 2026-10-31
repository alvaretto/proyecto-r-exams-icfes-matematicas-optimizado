#!/usr/bin/env Rscript
## =============================================================================
## verificar_espacio_cloze.R — verificador propio del CLOZE hermano de totales-promedios-reciclaje-n3
## CLOZE: totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd (F-1 a F-4, firmadas por el
## profesor el 2026-10-30; ver ../.claude/CLAUDE.md § CLOZE hermano).
##
## Correr con el directorio de trabajo en cloze/salida, NUNCA en el subproyecto del SCHOICE:
##   cd cloze/salida && Rscript --no-init-file ../verificar_espacio_cloze.R
## Solo LEE el .Rmd del SCHOICE y SUB/verificar_espacio.R, del que toma por parse (sin ejecutarlo) las sondas y
## las cifras firmadas de «O-1 corregido». No edita nada.
##
##   E1  La Parte 1 es el SCHOICE: todos sus objetos y .Random.seed al final del bloque P1, en 100 semillas más
##       una cuota de 20 canónicas; y las Partes 2 a 6 no tocan ningún objeto de la Parte 1.
##   E2  Una sola clave por gap, con oráculos propios que leen el texto y las tablas MOSTRADOS (sin las funciones
##       del .Rmd). Parte 6: V10 en dos direcciones (estructura leída del texto = la declarada; veredicto leído
##       = marca). La Parte 1 hereda además la enumeración de SUB/verificar_espacio.R (E1 prueba que es el mismo
##       código).
##   E3  V11 por valor (dos sentidos, más «a un paso»), por contenido y por independencia; cascada F1 contada;
##       veredictos de la Parte 6; canal de largo de la Parte 2; batería cruzada CONGELADA el 2026-10-30, antes
##       de medir (§P7-C). En la canónica, las once reglas por ENUMERACIÓN EXACTA conducida por el código
##       (enumerar_canonica_cloze.R, tanda 3), cada una por separado: X6, X9, X10 y X11 contra FIRMA_CANON (L-C3,
##       decisiones 3.2 a y «a · aclaración sí» del 2026-10-30); las demás, contra +8 pp.
##   E4  Canales de «O-1 corregido» sobre la Parte 1, por rama, contra FIRMA_O1, FIRMA_S1_PROMEDIO y FIRMA_LARGO.
##   E5  Versiones únicas sobre 300 semillas (objetivo >= 250) y claves distintas por gap.
##   E6  Mutantes (contrato del orquestador: sonda propia, materialización en el entorno, patrón único, semillas
##       hasta la primera muerte con tope de 600; MC18 a MC21, uno por regla firmada en la canónica, con la
##       enumeración exacta).
##   E7  Pesos: expoints 3|1|1|1|1|1, una entrada por gap, con sus mutantes (F-3).
##   E8  L-C4 (tanda 4): batería CONGELADA por gap (bateria_partes_2_a_6.R, md5 comprobado). Parte 2 exacta (y
##       posición sobre las muestras); Parte 6 por estrato (mezcla, rama del otro colegio, k′), por afirmación,
##       con el p95 del máximo permutado como cota de ruido; relacionales RL1 y RL2. Mutantes MC23 y MC24 (L-C4) y
##       MP1 y MP2 (decisión 2 a). En E4, la canónica de la Parte 1 regla por regla (decisión 2 a, 24 cifras) y el estímulo
##       declarado en su batería (§P7-G). L-C5 (tanda 5, «H2 a»): la Parte 6, salvo el orden de las afirmaciones, por
##       ENUMERACIÓN EXACTA conducida por el código (enumerar_h2_cloze.R, 100 Partes 1, con las exclusiones de V11), celda
##       por celda contra FIRMA_H2; mutantes MC25 a MC27.
## Salida: exit 1 = algo falla; 3 = pendiente de firma (una cifra firmada superada o un canal cruzado > +8 pp
##   fuera de lo firmado); 4 = EN ROJO bajo override firmado («O-1 corregido» extendido por F-2 y X9 en la
##   canónica por L-C3, X6, X9, X10 y X11; la canónica de la Parte 1 regla por regla por la decisión 2 a; todo dentro
##   de lo firmado, y H2 en la Parte 6 por «H2 a», L-C5; regla #22 §P7-E: nunca PASS); 0 = sin rojo.
## =============================================================================
suppressMessages(library(exams))
args_all <- commandArgs(trailingOnly = FALSE)
f_arg <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
DIR <- if (length(f_arg)) dirname(normalizePath(f_arg)) else getwd()
SUB <- normalizePath(file.path(DIR, ".."))
REPO <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
RMD_C <- file.path(DIR, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd")
RMD_S <- file.path(SUB, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd")
VER_S <- file.path(SUB, "verificar_espacio.R")
stopifnot(file.exists(RMD_C), file.exists(RMD_S), file.exists(VER_S))
if (normalizePath(getwd()) == SUB) stop("No correr con el directorio de trabajo en el subproyecto del SCHOICE")
cat("Versión verificada: ", basename(RMD_C), " · md5 ", unname(tools::md5sum(RMD_C)), "\n", sep = "")
cat("SCHOICE leído: md5 ", unname(tools::md5sum(RMD_S)), " (aprobado para el aula: e293d5c812b1f9790e2bb777b62de9a5)\n", sep = "")
N <- 100L; TOPE <- 4000L; TOPE_MUT <- 600L
fallos <- character(0); PENDIENTES <- character(0); FIRMADOS <- character(0); RESIDUOS <- character(0)
falla <- function(...) { m <- paste0(...); fallos <<- c(fallos, m); cat("  FALLA:", m, "\n") }
ok <- function(cond, ...) { m <- paste0(...); if (isTRUE(cond)) cat("  OK:", m, "\n") else falla(m) }
pendiente <- function(...) { m <- paste0(...); PENDIENTES <<- c(PENDIENTES, m); cat("  PENDIENTE DE FIRMA:", m, "\n") }
firmado <- function(..., por = "O-1 corregido (F-2)") { m <- paste0(...); FIRMADOS <<- c(FIRMADOS, paste0("[", por, "] ", m))
  cat("  ROJO, cubierto por ", por, ": ", m, "\n", sep = "") }
residuo <- function(...) { m <- paste0(...); RESIDUOS <<- c(RESIDUOS, m); cat("  RESIDUO DECLARADO:", m, "\n") }
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
EXPR_S <- parse(text = COD_S, encoding = "UTF-8")
EXPR_C <- parse(text = COD_C, encoding = "UTF-8")
correr <- function(expr, s) {
  set.seed(s); en <- new.env()
  en$.err <- tryCatch({ eval(expr, envir = en); NULL }, error = function(e) conditionMessage(e))
  en$.seed <- get(".Random.seed", envir = globalenv()); en
}
cap1 <- function(s) paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))
num_all <- function(s) as.numeric(unlist(regmatches(s, gregexpr("[0-9]+", s))))
cif1 <- function(s) { v <- regmatches(s, gregexpr("[0-9]+", s))[[1]]; if (length(v)) as.numeric(v[1]) else NA_real_ }
toks <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
## Oráculo de tablas, propio: lee las filas de datos de una tabla pipe mostrada (nombre, cursos, kilos).
tabla_leida <- function(md) {
  f <- grep("^\\|", strsplit(md, "\n", fixed = TRUE)[[1]], value = TRUE)
  f <- f[!grepl("^\\|[-:| ]+\\|$", f)]
  cel <- lapply(f, function(r) {
    x <- trimws(strsplit(sub("^\\|", "", sub("\\|$", "", r)), "|", fixed = TRUE)[[1]])
    trimws(gsub(" ", " ", gsub("<span[^>]*>|</span>|\\*\\*", "", x)))
  })
  dat <- cel[-1]
  data.frame(nombre = vapply(dat, `[`, "", 1L),
             n = vapply(dat, function(x) as.numeric(sub(" cursos?$", "", x[2])), 0),
             a = vapply(dat, function(x) as.numeric(sub(" kilos?$", "", x[3])), 0), stringsAsFactors = FALSE)
}
sujeto <- function(s) sub(" gan[oó] .*$", "", s)

## ---- Registro de una versión ---------------------------------------------------------------------
rec <- function(en) {
  with(en, list(
    canon = isTRUE(es_canonica), rama = rama, opciones = opciones_p1, clave = which(sol_p1), rol = rol_opcion,
    D = d$D, X = afs$gdis$x, cod_gdis = afs$gdis$codigo, molde_gdis = afs$gdis$molde,
    sede_o = vapply(afs[rol_opcion], function(a) a$sede, ""),
    nom_o = ifelse(vapply(afs[rol_opcion], function(a) a$sede, "") == "G", nomG, nomP),
    fila1 = conj$nombres[1], tabla = c(d$nG, d$nP, d$aG, d$aP), gi = gi, lado = lado_gdis,
    enunciado = enunciado, pregunta = pregunta, tabla_md = tabla_md,
    rama_par = rama_par, cascada = cascada_par, tabla_par_md = tabla_par_md, intro = intro_par,
    tipo_p2 = tipo_p2, texto_p2 = texto_p2, opciones_p2 = opciones_p2, sol_p2 = sol_p2,
    texto_p3 = texto_p3, texto_p4 = texto_p4, texto_p5 = texto_p5, exsol = c(exsol_p3, exsol_p4, exsol_p5),
    texto_p6 = texto_p6, opciones_p6 = opciones_p6, sol_p6 = sol_p6, rol_p6 = rol_p6, ventaja_id = c6$id,
    v_hp = v_hp, v_hg = v_hg, v_c = v_c, gan_par = gan_par, per_par = per_par,
    exclozetype = exclozetype_str, exsolution = exsolution_str, extol = extol_str))
}

## ---- E1 -----------------------------------------------------------------------------------------
titulo("E1 · La Parte 1 es el SCHOICE: objetos y .Random.seed en 100 semillas + cuota de 20 canónicas")
REN <- c(opciones = "opciones_p1", sol = "sol_p1", feedback = "feedback_p1")
iguales <- function(x, y) identical(x, y, ignore.environment = TRUE, ignore.bytecode = TRUE, ignore.srcref = TRUE)
medir_E1 <- function(cod_c, semillas) {
  f <- character(0); n_can <- 0L; e_c <- parse(text = cod_c, encoding = "UTF-8")
  e_p1 <- parse(text = p1_de(cod_c), encoding = "UTF-8")
  for (s in semillas) {
    A <- correr(EXPR_S, s); B <- correr(e_p1, s); C <- correr(e_c, s)
    if (!is.null(A$.err) || !is.null(B$.err) || !is.null(C$.err)) {
      f <- c(f, sprintf("s=%d error: %s / %s / %s", s, format(A$.err), format(B$.err), format(C$.err))); next
    }
    n_can <- n_can + isTRUE(A$es_canonica)
    nA <- setdiff(ls(A), names(REN)); nB <- setdiff(ls(B), REN)
    if (!setequal(nA, nB)) f <- c(f, sprintf("s=%d objetos distintos: %s", s, paste(c(setdiff(nA, nB), setdiff(nB, nA)), collapse = ",")))
    for (k in intersect(nA, nB)) if (!iguales(A[[k]], B[[k]])) f <- c(f, sprintf("s=%d %s distinto", s, k))
    for (k in names(REN)) if (!iguales(A[[k]], B[[REN[[k]]]])) f <- c(f, sprintf("s=%d %s != %s", s, REN[[k]], k))
    if (!identical(A$.seed, B$.seed)) f <- c(f, sprintf("s=%d .Random.seed distinto al final del bloque P1", s))
    for (k in ls(B)) if (!iguales(B[[k]], C[[k]])) f <- c(f, sprintf("s=%d %s cambia en las Partes 2 a 6", s, k))
  }
  list(f = f, n_can = n_can)
}
can_extra <- integer(0); s <- 100L
n_can_100 <- sum(vapply(1:100, function(z) isTRUE(correr(EXPR_S, z)$es_canonica), NA))
while (n_can_100 + length(can_extra) < 20L && s < TOPE) {
  s <- s + 1L; if (isTRUE(correr(EXPR_S, s)$es_canonica)) can_extra <- c(can_extra, s)
}
r1 <- medir_E1(COD_C, c(1:100, can_extra))
cat(sprintf("  semillas 1..100 (%d canónicas) + %d canónicas de cuota (hasta la semilla %d)\n", n_can_100, length(can_extra), s))
if (length(r1$f)) { for (x in head(r1$f, 10)) cat("   ", x, "\n"); falla("E1: ", length(r1$f), " diferencias") } else
  ok(TRUE, sprintf("E1: %d versiones (%d canónicas): objetos, opciones_p1/sol_p1/feedback_p1 y .Random.seed idénticos al SCHOICE; las Partes 2 a 6 no tocan nada de la Parte 1",
                   100L + length(can_extra), r1$n_can))

## ---- Muestras: mezcla natural (1..100), cuotas por rama de la Parte 1 y canónicas -------------------
NAT <- list(); for (z in 1:N) { en <- correr(EXPR_C, z); if (!is.null(en$.err)) falla("semilla ", z, ": ", en$.err) else NAT[[length(NAT) + 1]] <- rec(en) }
CU <- list(CURSOS = list(), PROMEDIO = list()); CAN <- list(); CANON <- NULL; SEM_CAN <- integer(0)
for (z in 1000L + seq_len(TOPE)) {
  if (length(CU$CURSOS) >= N && length(CU$PROMEDIO) >= N && length(CAN) >= 20L) break
  en <- correr(EXPR_C, z); if (!is.null(en$.err)) { falla("semilla ", z, ": ", en$.err); next }
  x <- rec(en)
  if (x$canon) { if (is.null(CANON)) CANON <- x
    if (length(CAN) < N) { CAN[[length(CAN) + 1]] <- x; SEM_CAN <- c(SEM_CAN, z) }; next }
  if (length(CU[[x$rama]]) < N) CU[[x$rama]][[length(CU[[x$rama]]) + 1]] <- x
}
TODAS <- c(NAT, CU$CURSOS, CU$PROMEDIO, CAN)
cat(sprintf("\nMuestras: mezcla natural n = %d · rama CURSOS n = %d · rama PROMEDIO n = %d · canónicas n = %d (semillas 1001..%d)\n",
            length(NAT), length(CU$CURSOS), length(CU$PROMEDIO), length(CAN), z))
for (b in names(CU)) ok(length(CU[[b]]) >= 20L, "estrato ", b, ": n = ", length(CU[[b]]), " (n < 20 sería NO CONCLUYENTE)")
ok(length(CAN) >= 20L, "canónicas: n = ", length(CAN))

## ---- E2 -----------------------------------------------------------------------------------------
titulo("E2 · Una sola clave por gap, con oráculos que leen el texto y las tablas mostrados")
or_p1 <- function(x) {
  tb <- tabla_leida(x$tabla_md); tt <- tb$n * tb$a; g <- which.max(tt); D <- max(tt) - min(tt)
  if (length(unique(tt)) != 2L) return(FALSE)
  nombra_g <- vapply(x$opciones, function(o) grepl(tolower(tb$nombre[g]), tolower(sujeto(o)), fixed = TRUE), NA)
  cand <- nombra_g & !is.na(vapply(x$opciones, cif1, 0)) & vapply(x$opciones, cif1, 0) == D
  sum(cand) == 1L && which(cand) == x$clave
}
or_p2 <- function(x) {
  ## Pasada 2 (tanda 6): las claves «promedio» dividen lo que recogió el grado entre sus cursos, y ningún distractor de
  ## ese tipo divide. Las claves «comparar» multiplican cursos por promedio o suman lo recogido por los cursos; ningún
  ## distractor de ese tipo lo hace (el de la suma suma cursos y promedio; el del cociente divide el promedio).
  es_clave <- if (x$tipo_p2 == "promedio") grepl("^Lo que recogió (el|ese) grado dividido (entre|en partes iguales entre) ", x$opciones_p2) else
    grepl("producto de cursos por promedio|cursos multiplicada por el promedio|suma de lo recogido por los cursos", x$opciones_p2)
  sum(es_clave) == 1L && identical(es_clave, x$sol_p2)
}
or_p345 <- function(x) {
  tb <- tabla_leida(x$tabla_par_md); tt <- tb$n * tb$a
  g3 <- sub("^.*el grado (.+)\\?$", "\\1", x$texto_p3); g4 <- sub("^.*el grado (.+)\\?$", "\\1", x$texto_p4)
  i3 <- match(g3, tolower(tb$nombre)); i4 <- match(g4, tolower(tb$nombre))
  !is.na(i3) && !is.na(i4) && i3 != i4 && tt[i3] == x$exsol[1] && tt[i4] == x$exsol[2] &&
    abs(diff(tt)) == x$exsol[3] && grepl("ganó", x$texto_p5) && !grepl(tb$nombre[which.max(tt)], x$texto_p5, ignore.case = TRUE)
}
## V10: la estructura de cada afirmación se lee del TEXTO, con patrones, y se evalúa con la tabla mostrada.
PAT6 <- paste0("^Si el grado que (perdió|ganó) hubiera (participado con un curso más|participado con un curso menos|",
               "participado con tantos cursos como el que (?:ganó|perdió)|tenido el mismo promedio por curso que el que ",
               "(?:ganó|perdió)), conservando su (promedio por curso|cantidad de cursos), habría (ganado|perdido) el concurso\\.$")
## Pasada 1: la hipótesis del que ganó dice «le hubiera faltado un curso» (sin «menos»).
PAT6B <- paste0("^Si al grado que (perdió|ganó) (le hubiera faltado un curso), conservando su (promedio por curso|cantidad de cursos), ",
                "habría (ganado|perdido) el concurso\\.$")
lee6 <- function(t) {
  if (grepl("^Ganó el concurso el grado [a-zé]+\\.$", t)) return(list(tipo = "ganador", grado = sub("^Ganó el concurso el grado ([a-zé]+)\\.$", "\\1", t)))
  m <- regmatches(t, regexec(PAT6, t, perl = TRUE))[[1]]
  if (!length(m)) m <- regmatches(t, regexec(PAT6B, t, perl = TRUE))[[1]]
  if (!length(m)) return(list(tipo = "ilegible"))
  list(tipo = "hipotesis", sujeto = m[2], cambio = m[3], conserva = m[4], concl = m[5])
}
## Lo que cada papel declarado DEBE decir (dirección «modelo -> texto»).
ESPERADO6 <- list(hp = c("perdió", "participado con un curso más", "promedio por curso", "ganado"),
                  hg = c("ganó", "le hubiera faltado un curso", "promedio por curso", "perdido"),
                  BC_perdio = c("perdió", "participado con tantos cursos como el que ganó", "promedio por curso", "ganado"),
                  BC_gano = c("ganó", "participado con tantos cursos como el que perdió", "promedio por curso", "perdido"),
                  BP_perdio = c("perdió", "tenido el mismo promedio por curso que el que ganó", "cantidad de cursos", "ganado"),
                  BP_gano = c("ganó", "tenido el mismo promedio por curso que el que perdió", "cantidad de cursos", "perdido"))
verdad6 <- function(a, tb) {
  tt <- tb$n * tb$a; G <- which.max(tt)
  if (a$tipo == "ganador") return(tolower(tb$nombre[G]) == a$grado)
  if (a$tipo != "hipotesis") return(NA)
  S <- if (a$sujeto == "ganó") G else 3L - G; O <- 3L - S
  n <- tb$n; av <- tb$a
  if (a$cambio == "participado con un curso más") {
    n[S] <- n[S] + 1
  } else if (a$cambio == "le hubiera faltado un curso") {
    n[S] <- n[S] - 1
  } else if (startsWith(a$cambio, "participado con tantos cursos")) {
    n[S] <- n[O]
  } else {
    av[S] <- av[O]
  }
  t2 <- n * av
  if (a$concl == "ganado") t2[S] > t2[O] else t2[S] < t2[O]
}
or_p6 <- function(x) {
  tb <- tabla_leida(x$tabla_par_md); L <- lapply(x$opciones_p6, lee6)
  dir1 <- all(vapply(seq_along(L), function(i) {
    a <- L[[i]]; r <- x$rol_p6[i]
    if (r == "ganador") return(a$tipo == "ganador" && a$grado == (if (x$v_c) x$gan_par else x$per_par))
    clave <- if (r == "ventaja") x$ventaja_id else r
    a$tipo == "hipotesis" && identical(c(a$sujeto, a$cambio, a$conserva, a$concl), ESPERADO6[[clave]])
  }, NA))
  ver <- vapply(L, verdad6, NA, tb = tb)
  c(dir_texto_modelo = dir1, marca = identical(unname(ver), unname(x$sol_p6)),
    una_v_una_f = any(x$sol_p6) && any(!x$sol_p6))
}
E2 <- t(vapply(TODAS, function(x) c(p1 = or_p1(x), p2 = or_p2(x), p345 = or_p345(x), or_p6(x)), logical(6)))
for (k in colnames(E2)) ok(all(E2[, k]), sprintf("E2 %s: %d/%d versiones", k, sum(E2[, k]), nrow(E2)))
cat("  Parte 1: además, la enumeración exacta de SUB/verificar_espacio.R (E3 del SCHOICE) vale aquí: E1 prueba que es el mismo código.\n")

## ---- E3 -----------------------------------------------------------------------------------------
titulo("E3 · V11 (valor, contenido, independencia), cascada F1, veredictos de la Parte 6 y batería cruzada")
ops1 <- function(v) { v <- unique(v); if (length(v) < 2) return(numeric(0)); a <- outer(v, v, "+"); b <- abs(outer(v, v, "-")); p <- outer(v, v, "*")
  u <- upper.tri(a); unique(c(a[u], b[u], p[u])) }
v11 <- function(x) {
  tb1 <- tabla_leida(x$tabla_md); tt1 <- tb1$n * tb1$a; resuelve <- c(tt1, max(tt1) - min(tt1))
  muestra <- c(tb1$n, tb1$a, num_all(x$opciones), num_all(x$enunciado), num_all(x$pregunta))
  tb2 <- tabla_leida(x$tabla_par_md)
  txt26 <- gsub("Las Partes 2 a 6", "", c(x$intro, x$texto_p2, x$opciones_p2, x$texto_p3, x$texto_p4, x$texto_p5,
                                           x$texto_p6, x$opciones_p6), fixed = TRUE)
  imprime26 <- c(tb2$n, tb2$a, num_all(txt26))
  nombres1 <- unique(tolower(c(tb1$nombre, sub("^la (sede|jornada de la) ", "", tolower(vapply(x$opciones, sujeto, ""))))))
  nombres1 <- nombres1[nchar(nombres1) > 0]
  pat <- paste0("\\b(", paste(c(nombres1, "sede", "jornada"), collapse = "|"), ")\\b")
  c(resp_en_p1 = any(x$exsol %in% c(muestra, resuelve)),
    p26_imprime_p1 = any(imprime26 %in% c(num_all(x$opciones), resuelve)),
    texto26_con_cifras = any(grepl("[0-9]", txt26)),
    contenido = any(grepl(pat, tolower(c(x$intro, x$texto_p2, x$opciones_p2, x$texto_p3, x$texto_p4, x$texto_p5,
                                          x$texto_p6, x$opciones_p6)), perl = TRUE)),
    paso_D_desde_tabla2 = (max(tt1) - min(tt1)) %in% ops1(c(tb2$n, tb2$a)),
    paso_resp_desde_p1 = any(x$exsol %in% ops1(muestra)))
}
V <- t(vapply(TODAS, v11, logical(6)))
ok(!any(V[, "resp_en_p1"]), "V11 por valor: ninguna respuesta de las Partes 3 a 5 es un número que la Parte 1 muestra o resuelve (", nrow(V), " versiones)")
ok(!any(V[, "p26_imprime_p1"]), "V11 por valor: las Partes 2 a 6 no imprimen ninguna cifra de las opciones de la Parte 1 ni T_G, T_P o D")
ok(!any(V[, "texto26_con_cifras"]), "V11 por valor: los textos de las Partes 2 a 6 no llevan cifras (solo su tabla; «Partes 2 a 6» es un rótulo)")
ok(!any(V[, "contenido"]), "V11 por contenido: las Partes 2 a 6 no nombran sedes ni filas de la Parte 1")
for (est in list(list("mezcla", NAT), list("rama CURSOS", CU$CURSOS), list("rama PROMEDIO", CU$PROMEDIO), list("canónica", CAN))) {
  W <- t(vapply(est[[2]], v11, logical(6)))
  residuo(sprintf("V11 a un paso (%s, n = %d): D de la Parte 1 sale de una operación (+, -, ×) entre dos números de la tabla del otro colegio en %.1f %%; una respuesta de las Partes 3 a 5 sale de una operación entre dos números que muestra la Parte 1 en %.1f %%",
                  est[[1]], nrow(W), 100 * mean(W[, "paso_D_desde_tabla2"]), 100 * mean(W[, "paso_resp_desde_p1"])))
}
## Independencia (V11 por correlación): la rama del otro colegio no depende de la de la Parte 1.
indep <- function(L) { r1 <- vapply(L, function(x) x$rama, ""); r2 <- vapply(L, function(x) x$rama_par, "")
  p <- tapply(r2 == "CURSOS", r1, mean); list(tab = table(P1 = r1, paralela = r2), dif = unname(abs(p["CURSOS"] - p["PROMEDIO"]))) }
I <- indep(c(CU$CURSOS, CU$PROMEDIO)); print(I$tab)
ok(I$dif <= 0.20, sprintf("V11 por independencia: P(paralela CURSOS | P1 CURSOS) - P(paralela CURSOS | P1 PROMEDIO) = %.2f (tolerancia 0,20 con n = 100 por estrato)", I$dif))
## Cascada F1 (F-4): cada paso, contado.
casc <- table(factor(vapply(TODAS, function(x) x$cascada, 0L), levels = 1:4))
cat("  cascada F1, paso usado (1 base · 2 espacio ampliado · 3-4 otra plantilla de veredictos):", paste(names(casc), casc, sep = ": ", collapse = " · "), "\n")
if (sum(casc[2:4]) > 0) residuo(sprintf("cascada F1: %d de %d versiones usaron el paso 2 o siguientes (%s)", sum(casc[2:4]), sum(casc),
                                        paste(names(casc)[2:4], casc[2:4], sep = ": ", collapse = ", ")))
## Veredictos de la Parte 6 (H3): ninguna plantilla con veredicto fijo; las hipótesis cerca del 50 %.
v6 <- function(L, rol) mean(unlist(lapply(L, function(x) x$sol_p6[x$rol_p6 == rol])))
for (rol in c("hp", "hg", "ventaja", "ganador"))
  ok(abs(v6(TODAS, rol) - 0.5) <= 0.12, sprintf("Parte 6, «%s»: verdadera en %.1f %% de %d versiones (50 %% ± 12)", rol, 100 * v6(TODAS, rol), length(TODAS)))
for (rb in c("CURSOS", "PROMEDIO")) {
  Lr <- Filter(function(x) x$rama_par == rb, TODAS)
  cat(sprintf("  rama paralela %s (n = %d): hp %.0f %% · hg %.0f %% · ventaja %.0f %% · ganador %.0f %% verdaderas (diseño: 1/3 o 2/3 en las hipótesis por rama)\n",
              rb, length(Lr), 100 * v6(Lr, "hp"), 100 * v6(Lr, "hg"), 100 * v6(Lr, "ventaja"), 100 * v6(Lr, "ganador")))
}
ids <- table(vapply(TODAS, function(x) x$ventaja_id, ""))
for (id in names(ids)) {
  vv <- mean(unlist(lapply(Filter(function(x) x$ventaja_id == id, TODAS), function(x) x$sol_p6[x$rol_p6 == "ventaja"])))
  ok(vv > 0.2 && vv < 0.8, sprintf("Parte 6, plantilla de ventaja %s: verdadera en %.0f %% de %d (sin veredicto fijo)", id, 100 * vv, ids[[id]]))
}
nv <- table(vapply(TODAS, function(x) sum(x$sol_p6), 0L)); cat("  Parte 6, número de verdaderas por versión:", paste(names(nv), nv, sep = ": ", collapse = " · "), "\n")
## Parte 2: canal de largo (§P7-B: margen del 15 %).
sc <- function(S, k) { if (!any(S) || all(S)) 0.25 else if (S[k]) 1 / sum(S) else 0 }
larg <- function(L, f) mean(vapply(L, function(x) { n <- nchar(x$opciones_p2); sc(n == f(n), which(x$sol_p2)) }, 0))
marg <- function(L, f) { z <- vapply(L, function(x) { n <- nchar(x$opciones_p2); k <- which(x$sol_p2)
  if (sum(n == f(n)) != 1 || n[k] != f(n)) return(NA_real_); abs(n[k] - f(n[-k])) / f(n[-k]) }, 0); z[!is.na(z)] }
for (tp in c("promedio", "comparar")) {
  L2 <- Filter(function(x) x$tipo_p2 == tp, TODAS)
  for (f in list(list("la más larga", max), list("la más corta", min))) {
    t <- larg(L2, f[[2]]); m <- marg(L2, f[[2]]); mx <- if (length(m)) max(m) else 0
    msg <- sprintf("Parte 2 (%s, n = %d): «%s» acierta %.1f %% (%+.1f pp); la clave es la única %s en %d versiones, margen máximo %.1f %%",
                   tp, length(L2), f[[1]], 100 * t, 100 * (t - 0.25), sub("la ", "", f[[1]]), length(m), 100 * mx)
    if (t - 0.25 <= 0.08) ok(TRUE, msg) else if (mx < 0.15) residuo(paste(msg, "(< 15 %: inexplotable, §P7-B)")) else pendiente(paste(msg, "(> +8 pp con margen >= 15 %)"))
  }
}
## Orden de mención: la Parte 3 pregunta por el grado ganador en ~50 %.
p3g <- mean(vapply(TODAS, function(x) grepl(paste0(x$gan_par, "\\?$"), x$texto_p3), NA))
ok(abs(p3g - 0.5) <= 0.12, sprintf("orden de mención: la Parte 3 pregunta por el grado que ganó en %.1f %% (50 %% ± 12)", 100 * p3g))
fila_g <- mean(vapply(TODAS, function(x) { tb <- tabla_leida(x$tabla_par_md); which.max(tb$n * tb$a) == 1L }, NA))
ok(abs(fila_g - 0.5) <= 0.12, sprintf("tabla del otro colegio: el ganador va en la fila 1 en %.1f %% (50 %% ± 12)", 100 * fila_g))

## Batería cruzada (de las Partes 2 a 6 hacia la Parte 1). CONGELADA el 2026-10-30, al empezar el paso 3 y
## antes de medir (§P7-C). Tasa de acierto en la Parte 1 frente al azar (25 %); las reglas que se combinan con
## el atajo firmado S_U se miden frente al 50 % de S_U. Más de +8 pp vuelve al profesor (F-2).
## x$imprime (tanda 3): la enumeración exacta de la canónica inyecta aquí cada fila posible del caso paralelo para
## integrar X1 a X4 sobre todas; sin él, las cifras de la tabla mostrada.
imprime26 <- function(x) if (!is.null(x$imprime)) x$imprime else { tb2 <- tabla_leida(x$tabla_par_md); c(tb2$n, tb2$a) }
cifs <- function(o) vapply(o, cif1, 0)
mas_comun <- function(o, ref) { r <- toks(paste(ref, collapse = " ")); k <- vapply(o, function(s) length(intersect(toks(s), r)), 0L); k == max(k) }
par_su <- function(x) x$sede_o != x$sede_o[x$rol == "psin"]
ventaja_par <- function(x) if (x$rama_par == "CURSOS") "más cursos" else "mayor promedio"
dentro <- function(S, t) { u <- S & t; if (any(u) && any(S & !u)) u else S }
XREG <- list(
  "X1 elige la cifra que imprimen las Partes 2 a 6" = list(base = 0.25, f = function(x) { v <- cifs(x$opciones); !is.na(v) & v %in% imprime26(x) }),
  "X2 descarta la cifra que imprimen las Partes 2 a 6" = list(base = 0.25, f = function(x) { v <- cifs(x$opciones); !(!is.na(v) & v %in% imprime26(x)) }),
  "X3 elige la cifra a una operación de la tabla del otro colegio" = list(base = 0.25, f = function(x) { v <- cifs(x$opciones); !is.na(v) & v %in% ops1(imprime26(x)) }),
  "X4 descarta la cifra a una operación de la tabla del otro colegio" = list(base = 0.25, f = function(x) { v <- cifs(x$opciones); !(!is.na(v) & v %in% ops1(imprime26(x))) }),
  "X5 más palabras en común con todo el texto de las Partes 2 a 6" = list(base = 0.25, f = function(x) mas_comun(x$opciones, c(x$intro, x$texto_p2, x$opciones_p2, x$texto_p3, x$texto_p4, x$texto_p5, x$texto_p6, x$opciones_p6))),
  "X6 más palabras en común con la clave de la Parte 2" = list(base = 0.25, f = function(x) mas_comun(x$opciones, x$opciones_p2[x$sol_p2])),
  "X7 más palabras en común con las verdaderas de la Parte 6" = list(base = 0.25, f = function(x) mas_comun(x$opciones, x$opciones_p6[x$sol_p6])),
  "X8 perfil cruzado: sede de la Parte 1 con la ventaja del grado ganador" = list(base = 0.25, f = function(x) x$sede_o == (if (x$rama == x$rama_par) "G" else "P")),
  "X9 S_U + la que nombra la ventaja del grado ganador" = list(base = 0.50, f = function(x) dentro(par_su(x), grepl(ventaja_par(x), x$opciones, fixed = TRUE))),
  "X10 S_U + más palabras en común con la clave de la Parte 2" = list(base = 0.50, f = function(x) dentro(par_su(x), mas_comun(x$opciones, x$opciones_p2[x$sol_p2]))),
  "X11 S_U + más palabras en común con las verdaderas de la Parte 6" = list(base = 0.50, f = function(x) dentro(par_su(x), mas_comun(x$opciones, x$opciones_p6[x$sol_p6]))))
XT <- sapply(list(mezcla = NAT, CURSOS = CU$CURSOS, PROMEDIO = CU$PROMEDIO, canonica = CAN), function(L)
  vapply(XREG, function(r) mean(vapply(L, function(x) sc(r$f(x), x$clave), 0)), 0))
print(round(100 * XT, 1))
BASES_X <- vapply(XREG, function(r) r$base, 0)
## L-C2 (decisión 3.1 a del profesor, 2026-10-30): un canal cruzado de más de +8 pp que siga abierto no se cierra
## solo: queda PENDIENTE DE FIRMA (exit 3) y vuelve al profesor. Una sola función decide, aquí y en el mutante MC17.
veredicto_cruzado <- function(ex) if (ex <= 0.08) "ok" else "pendiente"
for (i in seq_along(XREG)) for (est in c("mezcla", "CURSOS", "PROMEDIO")) {
  ex <- XT[i, est] - XREG[[i]]$base
  msg <- sprintf("%s, %s: %.1f %% (%+.1f pp sobre %.0f %%)", names(XREG)[i], est, 100 * XT[i, est], 100 * ex, 100 * XREG[[i]]$base)
  if (veredicto_cruzado(ex) == "ok") ok(TRUE, "batería cruzada: ", msg) else pendiente("canal cruzado > +8 pp (F-2 y L-C2: vuelve al profesor): ", msg)
}
## L-C3 (decisión 3.2 a y «a · aclaración sí» del profesor, 2026-10-30): override de X6, X9, X10 y X11 en la
## canónica, con sus cifras de ENUMERACIÓN EXACTA. Cubiertas por dominancia: en la canónica, el atajo firmado de
## «O-1 corregido» ya acierta el 100 %. Firmar el máximo de una batería no cubre las demás reglas: el alcance va
## regla por regla (la tanda 1 solo declaró X9, y X6, X10 y X11 quedaron fuera hasta la tanda 3). La puerta es la
## enumeración exacta conducida por el código (enumerar_canonica_cloze.R) para las ONCE reglas; la columna
## «canonica» de la tabla de arriba (n = 30) es solo informativa. Dentro del alcance, «rojo cubierto» (exit 4),
## nunca PASS (regla #22 §P7-E); por encima, o si cualquier otra regla pasa de +8 pp, PENDIENTE DE FIRMA (exit 3).
## Se comparan redondeadas a un decimal, como se firmaron.
REGLA_X6 <- "X6 más palabras en común con la clave de la Parte 2"
REGLA_X9 <- "X9 S_U + la que nombra la ventaja del grado ganador"
REGLA_X10 <- "X10 S_U + más palabras en común con la clave de la Parte 2"
REGLA_X11 <- "X11 S_U + más palabras en común con las verdaderas de la Parte 6"
FIRMA_CANON <- stats::setNames(c(16.7, 25.0, 16.7, 12.5), c(REGLA_X6, REGLA_X9, REGLA_X10, REGLA_X11))   # pp, exactas
stopifnot("las reglas firmadas en la canónica existen en la batería congelada" = all(names(FIRMA_CANON) %in% names(XREG)))
veredicto_canon <- function(exc) vapply(names(exc), function(r) {
  if (exc[[r]] <= 0.08) return("ok")
  if (r %in% names(FIRMA_CANON) && round(100 * exc[[r]], 1) <= FIRMA_CANON[[r]]) "cubierto" else "pendiente"
}, "")
source(file.path(DIR, "enumerar_canonica_cloze.R"), encoding = "UTF-8")
## Valor de las once reglas en una hoja. X6 a X11 y X8 salen de lo realizado (sus sorteos están controlados). X1 a X4
## se integran exactamente sobre todas las filas del caso paralelo (sorteo uniforme sample.int(nrow(ep), 1L)) y X5
## sobre todos los conjuntos de distractores de la Parte 2; las funciones de las reglas son las de XREG, sin copia.
CACHE_X14 <- list()
f_hoja_canon <- function(enB, ST) {
  x <- rec(enB)
  if (length(intersect(toks(paste(enB$GRADOS, collapse = " ")), unlist(lapply(x$opciones, toks)))))
    stop("los nombres de grado comparten palabras con las opciones de la Parte 1: la enumeración no los integra")
  if (is.null(ST$ep) || is.null(ST$dis)) stop("no se ejecutaron los sorteos de la fila del caso paralelo o de los distractores")
  val <- vapply(XREG, function(r) sc(r$f(x), x$clave), 0)
  ep <- ST$ep; v14 <- NULL
  for (cc in CACHE_X14) if (identical(cc$ep, ep) && identical(cc$op, x$opciones)) { v14 <- cc$v; break }
  if (is.null(v14)) {
    v14 <- vapply(1:4, function(i) mean(vapply(seq_len(nrow(ep)), function(j) {
      y <- x; y$imprime <- c(ep$nG[j], ep$nP[j], ep$aG[j], ep$aP[j]); sc(XREG[[i]]$f(y), y$clave) }, 0)), 0)
    CACHE_X14[[length(CACHE_X14) + 1L]] <<- list(ep = ep, op = x$opciones, v = v14)
  }
  val[1:4] <- v14
  cl2 <- x$opciones_p2[x$sol_p2]; q2 <- get("q2", envir = enB)
  subs <- utils::combn(ST$dis[["n"]], ST$dis[["size"]], simplify = FALSE)
  val[[5]] <- mean(vapply(subs, function(s) { y <- x; y$opciones_p2 <- c(cl2, unname(q2$distractores[s])); sc(XREG[[5]]$f(y), y$clave) }, 0))
  val
}
enum_canon <- function(cod, semilla) { H <- enumerar_canonica(cod, MARCA, semilla, f_hoja_canon); c(resumir_enumeracion(H), list(hojas = H)) }
## Contraste: la transcripción a mano del modelo (salida/x_canonica_exacta.R, tanda 2), para X6 a X11.
canon_transcrita <- function(en) {
  op <- en$opciones_p1; k <- which(en$sol_p1); rol <- en$rol_opcion
  sede_o <- vapply(en$afs[rol], function(z) z$sede, ""); su <- sede_o != sede_o[rol == "psin"]
  claves_p2 <- unlist(lapply(en$P2, function(q) q$clave)); filas <- list()
  for (rp in c("CURSOS", "PROMEDIO")) for (ic in 1:3) {
    h <- en$COMBOS_H[[rp]][[ic]]
    vcs <- if (h[["hg"]] == h[["hp"]]) list(c(v = !h[["hg"]], w = 1)) else list(c(v = TRUE, w = 0.5), c(v = FALSE, w = 0.5))
    for (vc in vcs) {
      cand <- Filter(function(z) (z$si_cursos == (rp == "CURSOS")) == as.logical(vc[["v"]]), en$P6_VENTAJA)
      for (c6 in cand) {
        verdaderas <- c(if (h[["hp"]]) en$P6_HP, if (h[["hg"]]) en$P6_HG, if (as.logical(vc[["v"]])) c(c6$texto, "Ganó el concurso el grado sexto."))
        w <- 0.5 * (1 / 3) * vc[["w"]] * (1 / length(cand)); ventaja <- if (rp == "CURSOS") "más cursos" else "mayor promedio"
        for (kp2 in claves_p2) filas[[length(filas) + 1L]] <- c(w = w / length(claves_p2),
          X6 = sc(mas_comun(op, kp2), k), X7 = sc(mas_comun(op, verdaderas), k), X8 = sc(sede_o == (if (en$rama == rp) "G" else "P"), k),
          X9 = sc(dentro(su, grepl(ventaja, op, fixed = TRUE)), k), X10 = sc(dentro(su, mas_comun(op, kp2)), k),
          X11 = sc(dentro(su, mas_comun(op, verdaderas)), k))
      }
    }
  }
  M <- do.call(rbind, filas); stopifnot("transcripción: pesos que suman 1" = abs(sum(M[, "w"]) - 1) < 1e-9)
  stats::setNames(colSums(M[, -1] * M[, "w"]), names(XREG)[6:11])
}
SEM_ENUM <- SEM_CAN[1:2]
EC <- enum_canon(COD_C, SEM_ENUM[1]); EC2 <- enum_canon(COD_C, SEM_ENUM[2])
ok(EC$masa_err == 0 && abs(EC$masa - 1) < 1e-9,
   sprintf("canónica, enumeración exacta conducida por el código (semilla %d): %d hojas, masa %.9f, masa con error %g%s",
           SEM_ENUM[1], EC$n_hojas, EC$masa, EC$masa_err, if (length(EC$errores)) paste0(" — ", paste(EC$errores, collapse = "; ")) else ""))
ok(EC2$masa_err == 0 && isTRUE(all.equal(EC$valores, EC2$valores, tolerance = 1e-12)),
   sprintf("canónica: la enumeración no depende de la semilla (orden de las opciones de la Parte 1): semillas %d y %d, mismas once cifras", SEM_ENUM[1], SEM_ENUM[2]))
TR <- canon_transcrita(correr(EXPR_C, SEM_ENUM[1]))
ok(isTRUE(all.equal(unname(EC$valores[names(TR)]), unname(TR), tolerance = 1e-9)),
   "canónica: X6 a X11 de la enumeración por código = transcripción del modelo (x_canonica_exacta.R): ",
   paste(sprintf("%s %.1f/%.1f", sub(" .*$", "", names(TR)), 100 * EC$valores[names(TR)], 100 * TR), collapse = " · "))
exc <- EC$valores - BASES_X
VC <- veredicto_canon(exc)
for (r in names(XREG)) {
  msg <- sprintf("%s, canónica exacta: %.1f %% (%+.1f pp sobre %.0f %%; muestra n = %d: %.1f %%); peso en la mezcla 0,10 × %+.1f = %+.2f pp",
                 r, 100 * EC$valores[[r]], 100 * exc[[r]], 100 * BASES_X[[r]], length(CAN), 100 * XT[r, "canonica"], 100 * exc[[r]], 10 * exc[[r]])
  switch(VC[[r]],
         ok = ok(TRUE, "batería cruzada: ", msg),
         cubierto = firmado(msg, sprintf(" (firmado hasta %+.1f pp)", FIRMA_CANON[[r]]), por = "el override de la canónica (L-C3: X6, X9, X10 y X11)"),
         pendiente = pendiente(if (r %in% names(FIRMA_CANON)) sprintf("%s en la canónica, por encima de lo firmado (%+.1f pp): ", sub(" .*$", "", r), FIRMA_CANON[[r]]) else
                                 "canal cruzado en la canónica > +8 pp, fuera del alcance firmado (L-C3 cubre X6, X9, X10 y X11): ", msg))
}
cat("  La transcripción del modelo sigue disponible como contraste: Rscript --no-init-file x_canonica_exacta.R, desde cloze/salida.\n")

## ---- E4 -----------------------------------------------------------------------------------------
titulo("E4 · «O-1 corregido» sobre la Parte 1, por rama (F-2)")
ENV_S <- new.env()
sys.source(file.path(REPO, ".claude/scripts/bateria_referencia_icfes.R"), envir = ENV_S)
EX_S <- parse(file = VER_S, keep.source = FALSE, encoding = "UTF-8")
QUIERO <- c("cif", "sc", "h1", "rango_D", "es_suma", "filtra", "atipica", "sondas", "FIRMA_O1", "FIRMA_S1_PROMEDIO",
            "FIRMA_LARGO", "hasta", "sede_txt", "grupo", "par_por_sede", "toks3", "I_REGLAS", "NA_FAM")
for (e in EX_S) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% QUIERO) eval(e, envir = ENV_S)
stopifnot("definiciones tomadas de SUB/verificar_espacio.R" = all(QUIERO %in% ls(ENV_S)))
perms <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perms(v[-i]), function(p) c(v[i], p))))
CANON_V <- lapply(perms(1:4), function(p) { x <- CANON; x$opciones <- CANON$opciones[p]; x$sede_o <- CANON$sede_o[p]
  x$rol <- CANON$rol[p]; x$nom_o <- CANON$nom_o[p]; x$clave <- which(p == CANON$clave); x })
TAB <- cbind(mezcla = ENV_S$sondas(NAT), CURSOS = ENV_S$sondas(CU$CURSOS), PROMEDIO = ENV_S$sondas(CU$PROMEDIO), canonica = ENV_S$sondas(CANON_V))
print(round(100 * TAB, 1))
F1 <- ENV_S$FIRMA_O1; hasta <- ENV_S$hasta
for (b in c("CURSOS", "PROMEDIO")) {
  su <- TAB["S_U la sede de la razón sin sustento pierde", b]
  m_su <- sprintf("S_U, rama %s: %.1f %%, %+.1f pp atribuibles (firmado: +16,7)", b, 100 * su, 100 * (su - 1 / 3))
  if (su - 1 / 3 <= 0.167 + 1e-9) firmado(m_su) else pendiente(m_su)
  for (s in rownames(F1)) {
    t <- TAB[s, b]; lim <- F1[s, b]
    if (hasta(t, lim)) firmado(sprintf("%s, rama %s: %.1f %% (firmado hasta %.1f %%)", s, b, 100 * t, 100 * lim)) else
      pendiente(sprintf("%s, rama %s: %.1f %%, por encima de lo firmado (%.1f %%): vuelve al profesor", s, b, 100 * t, 100 * lim))
  }
  s1 <- TAB["S1 descarta cifras de la tabla", b] - 0.25
  m_s1 <- sprintf("S1, rama %s: %+.1f pp", b, 100 * s1)
  if (s1 <= 0.08) ok(TRUE, m_s1, " (obliga > +8)") else if (b == "PROMEDIO" && hasta(s1, ENV_S$FIRMA_S1_PROMEDIO)) firmado(m_s1, " (firmado hasta +8,4)") else pendiente(m_s1)
  for (s in c("R_dif la cifra que es diferencia de otras dos", "Atípica: descarta la cifra que dobla a todas las demás")) {
    ex <- TAB[s, b] - 0.25
    if (ex <= 0.08) ok(TRUE, sprintf("%s, rama %s: %+.1f pp (obliga > +8)", s, b, 100 * ex)) else pendiente(sprintf("%s, rama %s: %+.1f pp", s, b, 100 * ex))
  }
}
REGLAS <- c(ENV_S$B2, ENV_S$I_REGLAS)
## Regla #22 §P7-G (v1.10, 2026-10-31): el estímulo es una entrada de la batería. Esta batería (la del SCHOICE, sobre la
## Parte 1) solo recibe las opciones; los canales del enunciado y de la tabla de la Parte 1 se miden aparte, en las sondas
## de E4 (S1 lee las cifras de la tabla; S2 y S3, sus filas; S5, S7 y S_U, las sedes que muestra), y en la canónica, regla
## por regla (decisión 2 a). Declararlo no mueve ninguna cifra (§P7-G).
ESTIMULO_P1 <- paste("los canales del enunciado y de la tabla de la Parte 1 se miden aparte, en las sondas de E4 (S1, S2, S3,",
                     "S5, S7, S_U y sus combinaciones) y en la canónica regla por regla (decisión 2 a)")
LARGO <- c("la mas larga (chars)", "la mas corta (chars)", "molde atipico (n tokens)", "sede de la opción más larga (relacional)")
FL <- ENV_S$FIRMA_LARGO
for (est in list(list("mezcla natural", NAT), list("rama CURSOS", CU$CURSOS), list("rama PROMEDIO", CU$PROMEDIO))) {
  L <- est[[2]]; ops <- lapply(L, function(x) x$opciones); cl <- vapply(L, function(x) x$clave, 0L)
  evaluar_bateria <- get("evaluar_bateria", envir = ENV_S)   # vive en bateria_eliminacion.R (lo carga la referencia)
  res <- evaluar_bateria(REGLAS, ops, cl, familias_no_aplicables = ENV_S$NA_FAM, estimulo_no_aplicable = ESTIMULO_P1)
  cat(sprintf("  batería §P7 (%d reglas) %s n=%d: máx %.1f %% (%s), techo nulo %.1f %%, exceso %+.1f pp -> %s\n",
              length(REGLAS), est[[1]], length(L), 100 * res$max_obs, res$regla_top, 100 * res$techo_nulo, 100 * res$exceso, res$veredicto))
  mg <- function(f) vapply(L, function(x) { n <- nchar(x$opciones); k <- x$clave
    if (sum(n == f(n)) != 1 || n[k] != f(n)) return(NA_real_); abs(n[k] - f(n[-k])) / f(n[-k]) }, 0)
  mmax <- max(c(mg(max), mg(min), 0), na.rm = TRUE)
  mt <- vapply(L, function(x) { k <- vapply(x$opciones, ENV_S$ntok, 0L); dv <- abs(k - stats::median(k))
    if (all(dv == 0) || which.max(dv) != x$clave) return(NA_real_)
    cerca <- k[-x$clave][which.min(abs(k[-x$clave] - k[x$clave]))]; abs(k[x$clave] - cerca) / cerca }, 0)
  tmax <- max(c(mt, 0), na.rm = TRUE)
  if (res$exceso <= 0.08) {
    ok(TRUE, sprintf("§P7-A %s: exceso %+.1f pp (obliga > +8)", est[[1]], 100 * res$exceso))
  } else {
    res2 <- evaluar_bateria(Filter(function(r) !r$nombre %in% LARGO, REGLAS), ops, cl, familias_no_aplicables = ENV_S$NA_FAM,
                            estimulo_no_aplicable = ESTIMULO_P1)
    m_l <- sprintf("canal de largo (%s): %+.1f pp, márgenes %.1f %% (caracteres) y %.1f %% (palabras); sin reglas de largo %+.1f pp; declarado hasta +10,4 pp, 2,7 %% y 6,5 %%",
                   est[[1]], 100 * res$exceso, 100 * mmax, 100 * tmax, 100 * res2$exceso)
    if (res2$exceso <= 0.08 && hasta(res$exceso, FL[["exceso"]]) && hasta(mmax, FL[["chars"]]) && hasta(tmax, FL[["palabras"]]))
      ok(TRUE, "§P7-B, dentro de lo declarado en O-1 corregido: ", m_l) else pendiente(m_l)
  }
}
sc_can <- vapply(REGLAS, function(r) mean(vapply(CANON_V, function(x) {
  S <- tryCatch(get(".conjunto_de", envir = ENV_S)(r$fn(x$opciones), 4L), error = function(e) rep(FALSE, 4)); sc(S, x$clave) }, 0)), 0)
cat(sprintf("  canónica (24 órdenes): máx %.1f %% (%s); peso en la mezcla 0,10 × %+.1f = %+.1f pp (firmado: 0,10 × 75 pp)\n",
            100 * max(sc_can), REGLAS[[which.max(sc_can)]]$nombre, 100 * (max(sc_can) - 0.25), 10 * (max(sc_can) - 0.25)))
## Decisión 2 a del profesor (2026-10-31): override de la canónica de la Parte 1 REGLA POR REGLA, por dominancia (en la
## canónica, el atajo firmado de «O-1 corregido» acierta el 100 %). Cifras EXACTAS de los 24 órdenes de las opciones del
## impreso (salida/p1_canonica_exacta_96cfcf35.txt): 12 reglas de la batería §P7 del SCHOICE y 12 sondas de E4, en pp
## sobre el 25 % del azar, comparadas redondeadas a un decimal, como se firmaron. Dentro: «ROJO, cubierto» (exit 4), nunca
## PASS. Por encima, o cualquier otra regla o sonda > +8 pp en la canónica: PENDIENTE (exit 3). Para la Parte 1 del CLOZE
## sustituye la firma por el máximo (0,10 × 75 pp) de «O-1 corregido» (aclaración ii de §P7-E). Las nueve que están al
## 100 % (+75,0 pp) no pueden subir: sus mutantes no existen y se declara (E8 lleva los de dos reglas al 50 %).
FIRMA_P1_CANON <- c("la que MAS comparte" = 75.0, "quedarse con 'total'" = 75.0, "cifra mediana" = 75.0, "mas digitos" = 25.0,
                    "primer numero PAR" = 25.0, "ultimo multiplo de 10" = 25.0, "quedarse con 'promedio'" = 25.0,
                    "par con más palabras en común (relacional)" = 25.0, "descartar ultimo negativo" = 8.3,
                    "par por sede: menor cifra" = 8.3, "descartar sin cifra" = 8.3, "descartar 'de más'" = 8.3)
FIRMA_P1_SONDAS <- c("S1 descarta cifras de la tabla" = 25.0, "S2 par de la sede de la fila 1" = 25.0,
                     "S3 par de la sede de más cursos" = 25.0, "S5 más cursos + menor cifra del par" = 75.0,
                     "S7 G real + menor cifra del par (cota canal 2)" = 75.0, "S_U la sede de la razón sin sustento pierde" = 25.0,
                     "S_U + S1 (par de G sin la cifra de la tabla)" = 75.0, "S_U + suma (par de G sin la cifra que es suma de otras dos)" = 25.0,
                     "Atípica: descarta la cifra que dobla a todas las demás" = 8.3, "S_U + S1 + suma (las dos pistas)" = 75.0,
                     "S_U + atípica" = 75.0, "S_U + S1 + suma + atípica" = 75.0)
canon_p1 <- function(CV) {
  r <- vapply(REGLAS, function(rg) mean(vapply(CV, function(x) {
    S <- tryCatch(get(".conjunto_de", envir = ENV_S)(rg$fn(x$opciones), 4L), error = function(e) rep(FALSE, 4)); sc(S, x$clave) }, 0)), 0)
  names(r) <- vapply(REGLAS, function(rg) rg$nombre, "")
  s <- as.matrix(ENV_S$sondas(CV))[, 1]; s <- s[!grepl("frecuencia", names(s))]
  list(reglas = 100 * (r - 0.25), sondas = 100 * (s - 0.25))
}
veredicto_p1c <- function(exc, firma) vapply(names(exc), function(nm) { e <- exc[[nm]]
  if (e <= 8 + 1e-9) "ok" else if (nm %in% names(firma) && round(e, 1) <= firma[[nm]]) "cubierto" else "pendiente" }, "")
C1 <- canon_p1(CANON_V)
stopifnot("decisión 2 a: las reglas y sondas firmadas existen en la batería y en E4" =
            all(names(FIRMA_P1_CANON) %in% names(C1$reglas)) && all(names(FIRMA_P1_SONDAS) %in% names(C1$sondas)))
for (cl in list(list("regla", C1$reglas, FIRMA_P1_CANON), list("sonda", C1$sondas, FIRMA_P1_SONDAS))) {
  vv <- veredicto_p1c(cl[[2]], cl[[3]])
  for (nm in names(vv)[vv != "ok"]) {
    m <- sprintf("canónica de la Parte 1, %s «%s»: %+.1f pp exactos (24 órdenes); peso en la mezcla %+.2f pp", cl[[1]], nm, cl[[2]][[nm]], cl[[2]][[nm]] / 10)
    if (vv[[nm]] == "cubierto") firmado(m, sprintf(" (firmado hasta %+.1f pp)", cl[[3]][[nm]]), por = "la decisión 2 a (canónica de la Parte 1, regla por regla)") else
      pendiente(m, if (nm %in% names(cl[[3]])) sprintf(", por encima de lo firmado (%+.1f pp)", cl[[3]][[nm]]) else ", fuera de lo firmado en la decisión 2 a")
  }
  cat(sprintf("  decisión 2 a, %ss firmadas: %d; por encima de +8 pp en la canónica: %d\n", cl[[1]], length(cl[[3]]), sum(vv != "ok")))
}
nl_can <- mean(vapply(CANON_V, function(x) { n <- nchar(x$opciones); sc(n != max(n), x$clave) }, 0))
residuo(sprintf("canónica de la Parte 1, fuera de la batería congelada (§P7-C, decisión 2 a: solo declarada): «descarta la más larga» %.1f %% (%+.1f pp)", 100 * nl_can, 100 * (nl_can - 0.25)))

## ---- E5 -----------------------------------------------------------------------------------------
titulo("E5 · Versiones únicas sobre 300 semillas (objetivo >= 250) y claves por gap")
U <- lapply(1:300, function(z) { en <- correr(EXPR_C, z); if (!is.null(en$.err)) return(NULL); rec(en) })
U <- Filter(Negate(is.null), U)
firma <- vapply(U, function(x) paste(c(x$enunciado, x$tabla_md, sort(x$opciones), x$intro, x$tabla_par_md, x$texto_p2,
  sort(x$opciones_p2), x$texto_p3, x$texto_p4, x$texto_p5, sort(x$opciones_p6)), collapse = "|"), "")
ok(length(unique(firma)) >= 250L, sprintf("E5: %d versiones únicas de %d", length(unique(firma)), length(U)))
claves <- list(p1 = vapply(U, function(x) x$opciones[x$clave], ""), p2 = vapply(U, function(x) x$opciones_p2[x$sol_p2], ""),
               p3 = vapply(U, function(x) x$exsol[1], 0), p4 = vapply(U, function(x) x$exsol[2], 0), p5 = vapply(U, function(x) x$exsol[3], 0),
               p6 = vapply(U, function(x) paste(sort(x$opciones_p6[x$sol_p6]), collapse = "|"), ""))
cat("  claves distintas por gap (300 semillas):", paste(names(claves), vapply(claves, function(v) length(unique(v)), 0L), sep = " ", collapse = " · "), "\n")
cat("  Parte 2: clave conceptual fija por tipo de pregunta (V8, fija-justificada); cambia su redacción (3 paráfrasis por tipo).\n")

## ---- E6 -----------------------------------------------------------------------------------------
titulo("E6 · Mutantes (sonda propia, materialización en el entorno, patrón único, semillas hasta la primera muerte)")
mutar <- function(cambios) {
  txt <- COD_C
  for (cb in cambios) {
    n <- lengths(regmatches(txt, gregexpr(cb[1], txt, fixed = TRUE)))
    if (n != 1L) return(list(error = sprintf("el patrón «%s» aparece %d veces (se exige 1)", substr(cb[1], 1, 60), n)))
    txt <- sub(cb[1], cb[2], txt, fixed = TRUE)
  }
  list(txt = txt, expr = parse(text = txt, encoding = "UTF-8"))
}
MUT <- list(
  list(id = "MC1", sonda = "V11 por valor: la tabla del otro colegio", desc = "sin la exclusión de la tabla paralela",
       cambios = list(c("prohib_tabla_par <- unique(c(cifras_p1, resuelve_p1))", "prohib_tabla_par <- numeric(0)")),
       llego = function(e) length(e$prohib_tabla_par) == 0L),
  list(id = "MC2", sonda = "V11 por valor: las respuestas de las Partes 3 a 5", desc = "sin la exclusión de las respuestas",
       cambios = list(c("prohib_resp_par <- unique(c(muestra_p1, resuelve_p1))", "prohib_resp_par <- numeric(0)")),
       llego = function(e) length(e$prohib_resp_par) == 0L),
  list(id = "MC3", sonda = "V11 por contenido", desc = "la Parte 2 nombra a la sede ganadora de la Parte 1",
       cambios = list(c("texto_p2 <- q2$pregunta", "texto_p2 <- paste(q2$pregunta, cap1(nomG))")),
       llego = function(e) isTRUE(grepl(cap1(e$nomG), e$texto_p2, fixed = TRUE))),
  list(id = "MC4", sonda = "Parte 2: una sola clave", desc = "una segunda paráfrasis de la clave entra como distractor",
       cambios = list(c("dis_p2 <- q2$distractores[sample.int(length(q2$distractores), 3L)]",
                        "dis_p2 <- c(otra = q2$clave[[1]], q2$distractores[sample.int(length(q2$distractores), 2L)])")),
       llego = function(e) isTRUE(sum(e$opciones_p2 %in% e$q2$clave) == 2L)),
  list(id = "MC5", sonda = "Parte 6: al menos una verdadera y una falsa", desc = "si las hipótesis coinciden, las otras dos también",
       cambios = list(c("v_c <- if (v_hg == v_hp) !v_hg else", "v_c <- if (v_hg == v_hp) v_hg else")),
       llego = function(e) isTRUE(e$v_hg == e$v_hp && e$v_c == e$v_hg)),
  list(id = "MC6", sonda = "V10: la marca de cada afirmación", desc = "la hipótesis del que perdió dice «habría perdido» con la marca de «habría ganado»",
       cambios = list(c("un curso más, conservando su promedio por curso, habría ganado", "un curso más, conservando su promedio por curso, habría perdido")),
       llego = function(e) isTRUE(grepl("habría perdido", e$P6_HP))),
  list(id = "MC7", sonda = "L-1 endurecida: sin vocabulario de error", desc = "un distractor de la Parte 2 dice «error»",
       cambios = list(c("Lo que recogió la mayoría de sus cursos en ese grado.", "Lo que recogió la mayoría de sus cursos en ese grado, sin error.")),
       llego = function(e) isTRUE(any(grepl("error", e$opciones_p2)))),
  list(id = "MC8", sonda = "L-3 (caso paralelo): sin dominancia", desc = "permite la dominancia en la rama CURSOS del caso paralelo",
       cambios = list(c('g <- if (rama_x == "CURSOS") g[g$nG > g$nP & g$aG < g$aP, ]',
                        'g <- if (rama_x == "CURSOS") g[g$nG > g$nP & g$aG < g$aP + 12, ]')),
       llego = function(e) isTRUE(e$dp$rama == "CURSOS" && e$dp$aG > e$dp$aP)),
  list(id = "MC9", sonda = "seis gaps", desc = "extol con cinco entradas",
       cambios = list(c('extol_str <- paste(rep("0", 6L), collapse = "|")', 'extol_str <- paste(rep("0", 5L), collapse = "|")')),
       llego = function(e) isTRUE(lengths(strsplit(e$extol_str, "|", fixed = TRUE)) == 5L)),
  list(id = "MC10", sonda = "Partes 3 a 5: respuestas enteras", desc = "la respuesta de la Parte 5 no es entera",
       cambios = list(c("exsol_p5 <- dp$D", "exsol_p5 <- dp$D + 0.5")),
       llego = function(e) isTRUE(e$exsol_p5 != round(e$exsol_p5))),
  list(id = "MC11", sonda = "Parte 6: sin pares complementarios", desc = "L-C1: rompe el atado, la del ganador con veredicto independiente del de la de ventaja",
       cambios = list(c("v_d <- v_c", "v_d <- sample(c(TRUE, FALSE), 1L)")),
       llego = function(e) isTRUE(e$v_d != e$v_c)),
  list(id = "MC16", sonda = "Parte 6: HP, HG, una de ventaja y la del ganador, nada más",
       desc = "L-C1: una afirmación conceptual sustituye a la del ganador",
       cambios = list(c('texto_d <- paste0("Ganó el concurso el grado ", if (v_d) gan_par else per_par, ".")',
                        'texto_d <- "El promedio por curso es el total del grado repartido por igual entre sus cursos."')),
       llego = function(e) isTRUE(length(e$opciones_p6) == 4L && !any(grepl("^Ganó el concurso el grado", e$opciones_p6)))))
filas_mut <- list()
for (mu in MUT) {
  mm <- mutar(mu$cambios)
  if (!is.null(mm$error)) { falla(mu$id, " MAL CONSTRUIDO: ", mm$error); filas_mut[[mu$id]] <- c(mu$id, mu$sonda, "-", "mal_construido", NA); next }
  muerte <- NA; msg <- NA; llego <- FALSE
  for (z in seq_len(TOPE_MUT)) {
    en <- correr(mm$expr, z)
    if (!llego) llego <- isTRUE(tryCatch(mu$llego(en), error = function(e) FALSE))
    if (!is.null(en$.err)) { muerte <- z; msg <- en$.err; break }
  }
  ver <- if (!llego) "mal_construido" else if (is.na(muerte)) "no_detectado" else if (!grepl(mu$sonda, msg, fixed = TRUE)) "cazado_por_otra" else "cazado_por_su_sonda"
  filas_mut[[mu$id]] <- c(mu$id, mu$sonda, if (is.na(msg)) "-" else substr(msg, 1, 90), ver, muerte)
  if (ver == "cazado_por_su_sonda") ok(TRUE, sprintf("%s (%s) cazado por su sonda en la semilla %d", mu$id, mu$desc, muerte)) else
    falla(sprintf("%s (%s): %s%s", mu$id, mu$desc, ver, if (is.na(msg)) "" else paste0(" — ", msg)))
}
## Mutantes de medición: la sonda tiene que dispararse con ellos.
medir_mut <- function(cambios, semillas = 1:200) { mm <- mutar(cambios); if (!is.null(mm$error)) return(mm)
  L <- list(); for (z in semillas) { en <- correr(mm$expr, z); if (is.null(en$.err)) L[[length(L) + 1]] <- rec(en) }; list(L = L) }
m12 <- medir_mut(list(c('rama_par <- sample(c("CURSOS", "PROMEDIO"), 1L)', "rama_par <- rama")))
if (!is.null(m12$error)) falla("MC12 MAL CONSTRUIDO: ", m12$error) else {
  d12 <- indep(Filter(function(x) !x$canon, m12$L))$dif
  ok(d12 > 0.20, sprintf("MC12 (la rama del otro colegio copia la de la Parte 1): la sonda de independencia lo detecta (diferencia %.2f > 0,20)", d12))
  filas_mut[["MC12"]] <- c("MC12", "V11 por independencia", sprintf("diferencia %.2f", d12), if (d12 > 0.20) "cazado_por_su_sonda" else "no_detectado", NA)
}
## MC17 (L-C2): la misma mutación que MC12 abre un canal cruzado (X8: la sede de la Parte 1 con la ventaja del grado
## ganador). Su sonda es la de la batería: veredicto_cruzado() tiene que dejarlo PENDIENTE DE FIRMA.
if (is.null(m12$error)) {
  L17 <- Filter(function(x) !x$canon, m12$L)
  llego17 <- length(L17) >= 20L && all(vapply(L17, function(x) x$rama == x$rama_par, NA))
  ex17 <- vapply(XREG, function(r) mean(vapply(L17, function(x) sc(r$f(x), x$clave), 0)), 0) - BASES_X
  v17 <- vapply(ex17, veredicto_cruzado, "")
  d17 <- sprintf("%s %+.1f pp, n = %d", names(ex17)[which.max(ex17)], 100 * max(ex17), length(L17))
  ver17 <- if (!llego17) "mal_construido" else if (any(v17 == "pendiente")) "cazado_por_su_sonda" else "no_detectado"
  ok(ver17 == "cazado_por_su_sonda", sprintf("MC17 (L-C2: abre un canal cruzado; la rama del otro colegio copia la de la Parte 1): la batería lo deja PENDIENTE DE FIRMA (%s) — %s", d17, ver17))
  filas_mut[["MC17"]] <- c("MC17", "batería cruzada > +8 pp: pendiente (L-C2)", d17, ver17, NA)
}
## MC18 a MC21 (L-C3, tanda 3): un mutante por regla firmada en la canónica, medido con la MISMA enumeración exacta
## que la puerta (sin semillas: no hay «primera muerte» que buscar). Cada uno tiene que sacar SU regla de «cubierto»;
## se informa qué otras reglas salen con él. La materialización se comprueba en el entorno de las hojas.
MUT_CANON <- list(
  list(id = "MC18", regla = REGLA_X9, desc = "la canónica sale siempre con el otro colegio en la rama CURSOS",
       cambios = list(c('rama_par <- sample(c("CURSOS", "PROMEDIO"), 1L)',
                        'rama_par <- if (isTRUE(es_canonica)) "CURSOS" else sample(c("CURSOS", "PROMEDIO"), 1L)')),
       llego = function(H) all(vapply(H, function(h) identical(h$rama_par, "CURSOS") && !"rama_par" %in% names(h$ruta), NA))),
  ## Tanda 6 (pasada 2 de la Parte 2): con la redacción nueva, X6 y X10 bajan a -2,8 y +0,0 pp en la canónica, así que
  ## un solo texto mutado ya no basta para salir de «cubierto»: MC19 muta las tres claves «comparar» y MC20 dos claves
  ## «promedio» (calculado: X6 +25,0 y X10 +33,3 pp). La sonda y la puerta no cambian.
  list(id = "MC19", regla = REGLA_X6, desc = "las tres claves «comparar» de la Parte 2 ganan «aunque», «fue» y «menor» (palabras de la clave de la Parte 1)",
       cambios = list(c('"La suma de lo recogido por los cursos de un grado y la suma del otro grado."',
                        '"La suma de lo recogido por los cursos de un grado y la suma del otro grado aunque fue menor."'),
                      c('"El producto de cursos por promedio de un grado y el del otro grado."',
                        '"El producto de cursos por promedio de un grado y el del otro grado aunque fue menor."'),
                      c('"La cantidad de cursos multiplicada por el promedio de un grado y lo mismo del otro grado."',
                        '"La cantidad de cursos multiplicada por el promedio de un grado y lo mismo del otro grado aunque fue menor."')),
       llego = function(H) any(vapply(H, function(h) isTRUE(grepl("del otro grado aunque fue menor.", h$clave_p2, fixed = TRUE)), NA))),
  list(id = "MC20", regla = REGLA_X10, desc = "dos claves «promedio» de la Parte 2 ganan «aunque», «fue» y «menor» (palabras de la clave de la Parte 1)",
       cambios = list(c('"Lo que recogió ese grado dividido en partes iguales entre sus cursos."',
                        '"Lo que recogió ese grado dividido en partes iguales entre sus cursos aunque fue menor."'),
                      c('"Lo que recogió ese grado dividido entre sus cursos."',
                        '"Lo que recogió ese grado dividido entre sus cursos aunque fue menor."')),
       llego = function(H) any(vapply(H, function(h) isTRUE(grepl("entre sus cursos aunque fue menor.", h$clave_p2, fixed = TRUE)), NA))),
  list(id = "MC21", regla = REGLA_X11, desc = "las dos redacciones de ventaja «por promedio» de la Parte 6 ganan «total» (exclusiva de la clave de la Parte 1)",
       cambios = list(c('"cantidad de cursos, habría ganado el concurso."', '"cantidad de cursos y su total, habría ganado el concurso."'),
                      c('"cantidad de cursos, habría perdido el concurso."', '"cantidad de cursos y su total, habría perdido el concurso."')),
       llego = function(H) all(vapply(H, function(h) sum(grepl("y su total, habría", h$ventaja, fixed = TRUE)) == 2L, NA))))
for (mu in MUT_CANON) {
  mm <- mutar(mu$cambios)
  if (!is.null(mm$error)) { falla(mu$id, " MAL CONSTRUIDO: ", mm$error); filas_mut[[mu$id]] <- c(mu$id, "L-C3", "-", "mal_construido", NA); next }
  E <- enum_canon(mm$txt, SEM_ENUM[1])
  vm <- veredicto_canon(E$valores - BASES_X)
  rr <- sub(" .*$", "", mu$regla); fuera <- sub(" .*$", "", names(vm)[vm == "pendiente"])
  d <- sprintf("%s %+.1f pp (firmado %+.1f) -> %s; salen: %s; %d hojas", rr, 100 * (E$valores[[mu$regla]] - BASES_X[[mu$regla]]),
               FIRMA_CANON[[mu$regla]], vm[[mu$regla]], paste(sprintf("%s %+.1f", fuera, 100 * (E$valores - BASES_X)[vm == "pendiente"]), collapse = ", "), E$n_hojas)
  ver <- if (E$masa_err > 0 || !mu$llego(E$hojas)) "mal_construido" else if (vm[[mu$regla]] == "pendiente") "cazado_por_su_sonda" else "no_detectado"
  ok(ver == "cazado_por_su_sonda", sprintf("%s (L-C3, %s: %s): %s — %s", mu$id, rr, mu$desc, d, ver))
  filas_mut[[mu$id]] <- c(mu$id, sprintf("%s en la canónica: fuera de «cubierto» (L-C3)", rr), d, ver, NA)
}
## MC22 (H4 de la FASE 2C r1, 2026-10-31): un sorteo que decide reglas reescrito con el texto de un sorteo LIBRE
## (v_c como «sample.int(2L, 1L) == 1L», el texto de gi_par y m3). Sin la cuenta de llamadas pasaría como libre, no se
## ramificaría y la cifra «exacta» saldría mal sin ningún error. Su sonda: la cuenta de llamadas libres hace fallar las
## hojas, y la puerta de la canónica deja de certificar (masa con error > 0).
m22 <- mutar(list(c("v_c <- if (v_hg == v_hp) !v_hg else sample(c(TRUE, FALSE), 1L)",
                    "v_c <- if (v_hg == v_hp) !v_hg else sample.int(2L, 1L) == 1L")))
if (!is.null(m22$error)) { falla("MC22 MAL CONSTRUIDO: ", m22$error); filas_mut[["MC22"]] <- c("MC22", "H4", "-", "mal_construido", NA) } else {
  E22 <- enum_canon(m22$txt, SEM_ENUM[1])
  llego22 <- any(vapply(E22$hojas, function(h) sum(h$libres == SORTEOS_LIBRES[["gi_m3"]]) == 3L, NA))
  sonda22 <- E22$masa_err > 0 && any(grepl("número de llamadas distinto", E22$errores, fixed = TRUE))
  ver22 <- if (!llego22) "mal_construido" else if (sonda22) "cazado_por_su_sonda" else "no_detectado"
  d22 <- sprintf("masa con error %.3f en %d hojas", E22$masa_err, E22$n_hojas)
  ok(ver22 == "cazado_por_su_sonda", sprintf("MC22 (H4: v_c reescrito con el texto de un sorteo libre): la cuenta de llamadas libres lo detecta (%s) — %s", d22, ver22))
  filas_mut[["MC22"]] <- c("MC22", "enumeración: llamadas libres (H4)", d22, ver22, NA)
}
m13 <- medir_mut(list(c('ep <- ep[(ep$aG > ep$D) == vh[["hg"]] & (ep$aP > ep$D) == vh[["hp"]], ]', "ep <- ep"),
                      c('v_hg <- vh[["hg"]]', "v_hg <- dp$aG > dp$D"), c('v_hp <- vh[["hp"]]', "v_hp <- dp$aP > dp$D")))
if (!is.null(m13$error)) falla("MC13 MAL CONSTRUIDO: ", m13$error) else {
  p13 <- c(hp = v6(m13$L, "hp"), hg = v6(m13$L, "hg")); peor <- max(abs(p13 - 0.5))
  ok(peor > 0.12, sprintf("MC13 (sin sortear primero el veredicto): la sonda de veredictos lo detecta (hp %.0f %%, hg %.0f %%)", 100 * p13[["hp"]], 100 * p13[["hg"]]))
  filas_mut[["MC13"]] <- c("MC13", "Parte 6: veredicto 50 % ± 12", sprintf("hp %.0f %%, hg %.0f %%", 100 * p13[["hp"]], 100 * p13[["hg"]]), if (peor > 0.12) "cazado_por_su_sonda" else "no_detectado", NA)
}
for (mx in list(list(id = "MC14", c = c(MARCA, paste0("invisible(runif(1L))\n", MARCA)), d = "consume un número aleatorio más dentro del bloque P1"),
                list(id = "MC15", c = c("compruébala con tus cálculos antes de aceptarla.", "compruébala con tus cálculos antes de aceptarla!"), d = "cambia una reflexión de la Parte 1"))) {
  n <- lengths(regmatches(COD_C, gregexpr(mx$c[1], COD_C, fixed = TRUE)))
  if (n != 1L) { falla(mx$id, " MAL CONSTRUIDO: patrón ", n, " veces"); next }
  r <- medir_E1(sub(mx$c[1], mx$c[2], COD_C, fixed = TRUE), 1:5)
  ok(length(r$f) > 0, sprintf("%s (%s): E1 lo detecta (%d diferencias en 5 semillas; p. ej. «%s»)", mx$id, mx$d, length(r$f), if (length(r$f)) r$f[1] else "-"))
  filas_mut[[mx$id]] <- c(mx$id, "E1", if (length(r$f)) substr(r$f[1], 1, 90) else "-", if (length(r$f)) "cazado_por_su_sonda" else "no_detectado", NA)
}
TM <- do.call(rbind, filas_mut); colnames(TM) <- c("id", "sonda_esperada", "sonda_real", "veredicto", "semilla")
cat("\n"); print(TM[, c("id", "veredicto", "semilla", "sonda_esperada")], quote = FALSE)
cat("  Sin mutante propio aquí: las guardas de la Parte 1 (L-1 a L-5, M1-M22 del SCHOICE): el código es el mismo (E1).\n")

## ---- E7 -----------------------------------------------------------------------------------------
titulo("E7 · Pesos por gap: expoints 3|1|1|1|1|1, una entrada por gap (F-3)")
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
ok(is.null(m7), "expoints 3|1|1|1|1|1 con ", length(unique(regmatches(LIN_C, regexpr("##ANSWER[0-9]+##", LIN_C)))), " gaps", if (!is.null(m7)) paste0(" — ", m7) else "")
mut7 <- list("sin la línea" = LIN_C[!grepl("^expoints:", LIN_C)],
             "Parte 1 a peso 1" = sub("^expoints: 3\\|", "expoints: 1|", LIN_C),
             "un peso de menos" = sub("^expoints: 3\\|1\\|1\\|1\\|1\\|1$", "expoints: 3|1|1|1|1", LIN_C),
             "el 3 en la Parte 2" = sub("^expoints: 3\\|1\\|", "expoints: 1|3|", LIN_C))
for (nm in names(mut7)) {
  if (identical(mut7[[nm]], LIN_C)) { falla("E7 mutante «", nm, "» mal construido (no cambia el texto)"); next }
  r <- chequeo_pesos(mut7[[nm]]); ok(!is.null(r), "E7 mutante «", nm, "» muerto: ", format(r))
}

## ---- E8 -----------------------------------------------------------------------------------------
## L-C4 (tanda 4, 2026-10-31; decisión «1 a» del profesor): batería CONGELADA por gap, comprobada por su md5. Parte 2:
## enumeración EXACTA de su espacio de textos (3 claves × 10 tríos × 24 órdenes, con las listas P2 del propio código)
## para todas las reglas salvo las de posición, que se juzgan sobre las muestras (dependen del sorteo o2 del código).
## Parte 6: muestras por estrato (mezcla 1..N, rama del otro colegio y k′, leídas de la tabla MOSTRADA). Con muestras,
## una regla por encima de +8 pp que no supera el p95 del máximo con el veredicto (o la clave) permutado es NO
## CONCLUYENTE (residuo declarado, §P7 exigencia 3); por encima de ese p95, PENDIENTE (exit 3). Exacta: > +8 pp =
## PENDIENTE. Mutantes: MC23 (clave de la Parte 2 siempre primera) y MC24 (ganador del otro colegio siempre en la fila
## 1), cada uno tiene que poner en rojo SU regla (P2-P1, P6-P1) por la sonda de L-C4.
titulo("E8 · L-C4: batería CONGELADA por gap (Parte 2 y Parte 6), regla por regla")
BAT <- file.path(DIR, "bateria_partes_2_a_6.R"); MD5_BAT <- "de20e0a986f86d44f0d3412517d2f07b"
ok(unname(tools::md5sum(BAT)) == MD5_BAT, sprintf("L-C4: la batería es la congelada (%s, md5 %s)", basename(BAT), MD5_BAT))
EB <- new.env(); sys.source(BAT, envir = EB, keep.source = FALSE)
source(file.path(DIR, "enumerar_h2_cloze.R"), encoding = "UTF-8")   # L-C5 (tanda 5): enumerar_h2(), h2_tabla(), h2_juzgar()
## L-C5 (tanda 5, 2026-10-31; decisión «H2 a» del profesor): override de H2 sobre el diseño actual (V0), regla por regla y
## por estrato. Cifras firmadas, en pp de exceso sobre el nulo exacto, tal como se firmaron (un decimal). «base» es la
## regla o su contraria (la que sale positiva); las de ventaja y ganador valen igual. NA = no firmada en ese estrato
## (debe quedar en +8 pp o menos).
## Tanda 6 (decisión «a · iii ampliada sí» del profesor, 2026-10-31): las cifras son las EXACTAS que da el código del
## .Rmd con las exclusiones de V11, sobre las 100 Partes 1 de la muestra estándar: la columna `ex` de la primera tabla de
## salida/h2_exacta_96cfcf35.txt. Sustituyen a las de V0 (salida/reparto_h2_96cfcf35.txt, 14 096 registros, sin las
## exclusiones), que salían de un modelo transcrito. Las dos celdas de A1 en la rama CURSOS quedan en 7.0: por debajo de
## +8 pp, h2_juzgar() las da «ok» antes de mirar la firma.
FIRMA_H2 <- utils::read.table(header = TRUE, stringsAsFactors = FALSE, text = "
base   papel   mezcla CURSOS PROMEDIO k1   k2
P6-K1  ventaja 33.3   29.6   36.9     32.8 33.5
P6-K1  ganador 33.3   29.6   36.9     32.8 33.5
P6-D2  ventaja 24.3   16.1   32.5     32.8 11.9
P6-D2  ganador 24.3   16.1   32.5     32.8 11.9
P6-N1  ventaja 14.8   NA     34.0     28.9 NA
P6-N1  ganador 14.8   NA     34.0     28.9 NA
P6-A1  ventaja 14.2   7.0    35.4     30.0 NA
P6-A1  ganador 14.2   7.0    35.4     30.0 NA
P6-D1  ventaja NA     12.6   NA       NA   NA
P6-D1  ganador NA     12.6   NA       NA   NA
P6-HK1 hg      22.8   27.1   NA       NA   NA
P6-HK2 hp      NA     13.6   NA       NA   NA
P6-HN  hp      17.7   15.2   10.2     16.9 17.0
P6-HN  hg      NA     NA     10.6     14.3 NA
P6-HD  hp      NA     10.3   8.4      23.8 14.0
P6-HD  hg      NA     NA     NA       14.1 15.9
P6-HA  hp      NA     NA     NA       16.6 NA
P6-HA  hg      NA     NA     NA       11.2 9.3
RL1    par     8.3    11.1   11.1     13.0 NA
RL2    hp      33.3   33.3   33.3     37.7 30.3
RL2    hg      33.3   33.3   33.3     33.4 24.6
")
reg_b <- function(x) { tb <- tabla_leida(x$tabla_par_md)
  list(p2 = list(op = x$opciones_p2, k = which(x$sol_p2), tipo = x$tipo_p2, preg = x$texto_p2, otros = c(x$texto_p3, x$texto_p4, x$texto_p5)),
       p6 = list(op = x$opciones_p6, v = x$sol_p6, rol = x$rol_p6, n = tb$n, a = tb$a, nombres = tolower(tb$nombre)),
       rama_par = x$rama_par, k = abs(diff(tb$n))) }
POS_P2 <- c("P2-P1", "P2-P2")
p95_p2 <- function(L, B = 200L) { set.seed(20261031L); S <- lapply(EB$REGLAS_P2, function(r) lapply(L, function(x) r$fn(x$p2)))
  stats::quantile(replicate(B, { kk <- sample.int(4L, length(L), replace = TRUE)
    max(vapply(S, function(Sr) mean(vapply(seq_along(L), function(i) EB$.sc_op(Sr[[i]], kk[i]), 0)), 0)) }), 0.95)[[1]] }
p95_p6 <- function(L, B = 200L) { P <- lapply(EB$REGLAS_P6, function(r) EB$.pred6(L, r$fn))
  ## La misma semilla para cada papel: «ventaja» y «ganador» llevan el mismo veredicto (L-C1) y deben recibir las mismas
  ## permutaciones; con una sola semilla al principio, su p95 difería por ruido y la misma cifra salía roja en un papel y
  ## NO CONCLUYENTE en el otro (r4, rama CURSOS, P6-A1c).
  stats::setNames(vapply(c("hp", "hg", "ventaja", "ganador"), function(pp) { set.seed(20261031L)
    ix <- vapply(L, function(x) which(x$p6$rol == pp), 0L); v <- vapply(seq_along(L), function(i) L[[i]]$p6$v[ix[i]], NA)
    PR <- lapply(P, function(Pr) vapply(seq_along(L), function(i) Pr[[i]][ix[i]], NA))
    stats::quantile(replicate(B, { vp <- sample(v); max(vapply(PR, function(pr) mean(ifelse(is.na(pr), 0.5, as.numeric(pr == vp))), 0)) }), 0.95)[[1]]
  }, 0), c("hp", "hg", "ventaja", "ganador")) }
juzgar <- function(M, cab, p95 = NULL, informar = TRUE) {
  S <- M[M$exceso > 0.08 + 1e-12, , drop = FALSE]; S <- S[order(-S$exceso), , drop = FALSE]; rojo <- character(0)
  for (i in seq_len(nrow(S))) {
    pap <- if (!is.null(S$papel)) S$papel[i] else "clave"
    m <- sprintf("L-C4 %s · %s%s: %.1f %% (nulo %.1f %%, %+.1f pp) — %s", cab, S$id[i], if (pap != "clave") paste0(" ", pap) else "",
                 100 * S$acierto[i], 100 * S$nulo[i], 100 * S$exceso[i], S$nombre[i])
    lim <- if (is.null(p95)) NA_real_ else if (length(p95) == 1L) p95 else p95[[pap]]
    if (!is.na(lim) && S$acierto[i] <= lim) { if (informar) residuo(m, sprintf(" (NO CONCLUYENTE: no supera el p95 del máximo con la clave o el veredicto permutados, %.1f %%)", 100 * lim)) } else {
      rojo <- c(rojo, S$id[i]); if (informar) pendiente(m, " (> +8 pp: vuelve al profesor o a una pasada de §P7-D)") }
  }
  if (informar && !nrow(S)) ok(TRUE, "L-C4 ", cab, ": ninguna regla por encima de +8 pp")
  invisible(rojo)
}
perm4 <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perm4(v[-i]), function(p) c(v[i], p))))
ENV_T <- list(); for (z in 1:60) { en <- correr(EXPR_C, z); if (is.null(ENV_T[[en$tipo_p2]])) ENV_T[[en$tipo_p2]] <- en; if (length(ENV_T) == 2L) break }
for (tp in c("promedio", "comparar")) {
  en <- ENV_T[[tp]]; q <- en$P2[[tp]]; LE <- list()
  for (cl in q$clave) for (s in utils::combn(length(q$distractores), 3L, simplify = FALSE)) { op0 <- c(cl, unname(q$distractores[s]))
    for (pm in perm4(1:4)) LE[[length(LE) + 1L]] <- list(p2 = list(op = op0[pm], k = which(pm == 1L), tipo = tp, preg = en$texto_p2, otros = c(en$texto_p3, en$texto_p4, en$texto_p5))) }
  M <- EB$medir_p2(LE); juzgar(M[!M$id %in% POS_P2, ], sprintf("Parte 2 «%s», exacta (%d versiones)", tp, length(LE)))
  LS <- lapply(Filter(function(x) x$tipo_p2 == tp, TODAS), reg_b); MS <- EB$medir_p2(LS)
  juzgar(MS[MS$id %in% POS_P2, ], sprintf("Parte 2 «%s», posición, muestra n = %d", tp, length(LS)), p95_p2(LS))
}
EST_B <- list(mezcla = lapply(NAT, reg_b), `rama CURSOS` = lapply(Filter(function(x) x$rama_par == "CURSOS", TODAS), reg_b),
              `rama PROMEDIO` = lapply(Filter(function(x) x$rama_par == "PROMEDIO", TODAS), reg_b))
TB <- lapply(TODAS, reg_b); EST_B$`k′ = 1` <- Filter(function(x) x$k == 1, TB); EST_B$`k′ ≥ 2` <- Filter(function(x) x$k >= 2, TB)
for (e in names(EST_B)) { L <- EST_B[[e]]; if (length(L) < 20L) { residuo(sprintf("L-C4 Parte 6, %s: n = %d < 20, NO CONCLUYENTE (regla #23)", e, length(L))); next }
  ok(EB$control_p6(L) == 1, sprintf("L-C4 Parte 6, %s: control positivo (calcula los totales) acierta las cuatro afirmaciones en %d/%d", e, round(EB$control_p6(L) * length(L)), length(L)))
  ## L-C5 (tanda 5): las demás reglas de la Parte 6 y las relacionales RL1 y RL2 se juzgan con la enumeración exacta, abajo.
  ## En la muestra quedan solo las del ORDEN de las afirmaciones (dependen de o6, que L-C5 no ramifica).
  M6 <- EB$medir_p6(L); juzgar(M6[M6$id %in% H2_SIN_ORDEN, , drop = FALSE], sprintf("Parte 6, orden de las afirmaciones, %s, muestra n = %d", e, length(L)), p95_p6(L))
}

## ---- L-C5 (tanda 5, 2026-10-31; decisión «H2 a» del profesor) -------------------------------------------------------
## Override de H2 sobre el diseño actual (V0), regla por regla y por estrato. La Parte 6 (todas sus reglas salvo las del
## orden, más RL1 y RL2) por ENUMERACIÓN EXACTA conducida por el código del .Rmd (enumerar_h2_cloze.R; aclaración iii de
## §P7-E), con las exclusiones de V11 de cada Parte 1, sobre las N Partes 1 de la muestra estándar (regla #23): exacta en
## las Partes 2 a 6, condicionada a esas Partes 1, y determinista. Cada celda (regla, papel, estrato) contra FIRMA_H2:
## dentro de lo firmado, «ROJO, cubierto» (exit 4), nunca PASS; por encima, o > +8 pp en una celda que la firma no cubre,
## PENDIENTE (exit 3). Mutantes: MC25 (K1), MC26 (D2) y MC27 (RL2), medidos con la misma puerta.
h2_exacta <- function(cod, semillas) {
  FF <- list(); merr <- 0; errs <- character(0); nh <- 0L
  for (z in semillas) { E <- enumerar_h2(cod, MARCA, z); nh <- nh + E$n_hojas; merr <- merr + E$masa_err; errs <- unique(c(errs, E$errores))
    if (is.null(E$filas)) next; f <- E$filas; f$w <- f$w / sum(f$w) / length(semillas); FF[[length(FF) + 1L]] <- f }
  list(A = if (length(FF)) h2_agregar(do.call(rbind, FF)) else NULL, masa_err = merr / length(semillas), errores = errs, n_hojas = nh)
}
H2X <- h2_exacta(COD_C, seq_len(N))
ok(H2X$masa_err == 0 && !length(H2X$errores), sprintf("L-C5: enumeración exacta conducida por el código, %d Partes 1 (semillas 1 a %d): %d hojas, %d filas, masa con error %.4f%s",
   N, N, H2X$n_hojas, nrow(H2X$A), H2X$masa_err, if (length(H2X$errores)) paste0(" — ", paste(H2X$errores, collapse = "; ")) else ""))
TH2 <- h2_juzgar(h2_tabla(H2X$A, EB), FIRMA_H2)
ok(all(paste(FIRMA_H2$base, FIRMA_H2$papel) %in% paste(TH2$base, TH2$papel)), "L-C5: las reglas firmadas existen en la batería congelada")
for (i in which(TH2$veredicto != "ok")) {
  m <- sprintf("L-C5 Parte 6, %s · %s %s: %.1f %% (nulo %.1f %%, %+.1f pp exactos) — %s", TH2$estrato[i], TH2$base[i], TH2$papel[i],
               100 * TH2$acierto[i], 100 * TH2$nulo[i], 100 * TH2$exceso[i], TH2$nombre[i])
  if (TH2$veredicto[i] == "cubierto") firmado(m, sprintf(" (firmado hasta %+.1f pp)", TH2$firma[i]), por = "«H2 a» (L-C5, Parte 6 por estrato)") else
    pendiente(m, if (is.na(TH2$firma[i])) ", fuera de lo firmado en «H2 a»" else sprintf(", por encima de lo firmado (%+.1f pp)", TH2$firma[i]))
}
f5 <- !is.na(TH2$firma)
cat(sprintf("  L-C5: %d celdas firmadas; dentro de lo firmado %d; por encima %d; bajaron a +8 pp o menos %d; > +8 pp fuera de la firma %d\n",
            sum(f5), sum(f5 & TH2$veredicto == "cubierto"), sum(f5 & TH2$veredicto == "pendiente"), sum(f5 & TH2$veredicto == "ok"), sum(!f5 & TH2$veredicto == "pendiente")))
MUT_H2 <- list(
  list(id = "MC25", base = "P6-K1", papel = "ventaja", estrato = "PROMEDIO", desc = "la rama PROMEDIO del otro colegio, siempre con un curso de diferencia",
       c = c('g <- if (rama_x == "CURSOS") g[g$nG > g$nP & g$aG < g$aP, ] else g[g$nG < g$nP & g$aG > g$aP, ]', 'g <- if (rama_x == "CURSOS") g[g$nG > g$nP & g$aG < g$aP, ] else g[g$nG + 1L == g$nP & g$aG > g$aP, ]'),
       llego = function(A) all(abs(A$nG - A$nP)[A$rama == "PROMEDIO"] == 1L)),
  list(id = "MC26", base = "P6-D2", papel = "ventaja", estrato = "CURSOS", desc = "la rama CURSOS del otro colegio, siempre con δ′ < 10",
       c = c('g <- if (rama_x == "CURSOS") g[g$nG > g$nP & g$aG < g$aP, ] else g[g$nG < g$nP & g$aG > g$aP, ]', 'g <- if (rama_x == "CURSOS") g[g$nG > g$nP & g$aG < g$aP & g$aP - g$aG < 10L, ] else g[g$nG < g$nP & g$aG > g$aP, ]'),
       llego = function(A) all((A$aP - A$aG)[A$rama == "CURSOS"] < 10L)),
  list(id = "MC27", base = "RL2", papel = "hp", estrato = "mezcla", desc = "«ventaja» y «Ganó…», siempre con el veredicto contrario al de HP",
       c = c("v_c <- if (v_hg == v_hp) !v_hg else sample(c(TRUE, FALSE), 1L)", "v_c <- !v_hp"),
       llego = function(A) all(A$v_c == !A$v_hp)))
for (mx in MUT_H2) {
  mm <- mutar(list(mx$c))
  if (!is.null(mm$error)) { falla(mx$id, " MAL CONSTRUIDO: ", mm$error); filas_mut[[mx$id]] <- c(mx$id, "L-C5", "-", "mal_construido", NA); next }
  muerte <- NA; llego <- FALSE; d <- "-"
  for (k in c(1L, 10L)) {   # semillas hasta la primera muerte: primero 1 Parte 1; si no basta, 10
    HM <- h2_exacta(mm$txt, seq_len(k)); if (is.null(HM$A)) next
    llego <- isTRUE(mx$llego(HM$A)); TM5 <- h2_juzgar(h2_tabla(HM$A, EB), FIRMA_H2)
    cel <- TM5[TM5$base == mx$base & TM5$papel == mx$papel & TM5$estrato == mx$estrato, , drop = FALSE]
    d <- sprintf("%s %s, %s: %+.1f pp (firmado %+.1f) con %d Parte(s) 1, masa con error %.3f -> %s", mx$base, mx$papel, mx$estrato,
                 if (nrow(cel)) 100 * cel$exceso else NA, if (nrow(cel)) cel$firma else NA, k, HM$masa_err, if (nrow(cel)) cel$veredicto else "sin celda")
    if (llego && nrow(cel) && cel$veredicto == "pendiente") { muerte <- k; break }
  }
  ver <- if (!llego) "mal_construido" else if (!is.na(muerte)) "cazado_por_su_sonda" else "no_detectado"
  ok(ver == "cazado_por_su_sonda", sprintf("%s (L-C5: %s): %s — %s", mx$id, mx$desc, d, ver))
  filas_mut[[mx$id]] <- c(mx$id, sprintf("L-C5: %s %s (%s) fuera de «cubierto»", mx$base, mx$papel, mx$estrato), d, ver, muerte)
}
cat("  L-C5: la puerta es una sola función (h2_juzgar) para todas las celdas; MC25, MC26 y MC27 la prueban en K1, D2 y RL2. Las demás celdas firmadas no tienen mutante propio y se declara.\n")
## Mutantes de L-C4: cada uno tiene que poner en rojo SU regla, medida con la misma sonda (juzgar, con su p95).
for (mx in list(list(id = "MC23", c = c("o2 <- sample.int(4L)", "o2 <- 1:4"), regla = "P2-P1", desc = "la clave de la Parte 2, siempre en la primera posición",
                     llego = function(e) identical(as.integer(e$o2), 1:4)),
                list(id = "MC24", c = c("gi_par <- sample.int(2L, 1L)", "gi_par <- 1L"), regla = "P6-P1", desc = "el grado que gana, siempre en la primera fila de la tabla del otro colegio",
                     llego = function(e) identical(e$gi_par, 1L)))) {
  mm <- mutar(list(mx$c))
  if (!is.null(mm$error)) { falla(mx$id, " MAL CONSTRUIDO: ", mm$error); filas_mut[[mx$id]] <- c(mx$id, "L-C4", "-", "mal_construido", NA); next }
  LM <- list(); llego <- TRUE
  for (z in 1:N) { en <- correr(mm$expr, z); if (!is.null(en$.err)) next; llego <- llego && isTRUE(mx$llego(en)); LM[[length(LM) + 1L]] <- reg_b(rec(en)) }
  rojo <- if (startsWith(mx$regla, "P2")) juzgar(EB$medir_p2(LM), "", p95_p2(LM), informar = FALSE) else juzgar(EB$medir_p6(LM), "", p95_p6(LM), informar = FALSE)
  ver <- if (!llego || length(LM) < 20L) "mal_construido" else if (mx$regla %in% rojo) "cazado_por_su_sonda" else "no_detectado"
  d <- sprintf("%d versiones; en rojo: %s", length(LM), if (length(rojo)) paste(unique(rojo), collapse = ", ") else "ninguna")
  ok(ver == "cazado_por_su_sonda", sprintf("%s (L-C4: %s): la batería congelada pone en rojo %s (%s) — %s", mx$id, mx$desc, mx$regla, d, ver))
  filas_mut[[mx$id]] <- c(mx$id, sprintf("L-C4: %s en rojo", mx$regla), d, ver, NA)
}
cat("  Partes 3 a 5 (num): ninguna regla de eliminación aplica (sin opciones, tolerancia 0); sus vías de fuga las mide V11 por valor y «a un paso» (E3).\n")
## Mutantes de la decisión 2 a (canónica de la Parte 1, regla por regla): COPIAS del registro canónico con una opción
## retocada (la canónica es verbatim y su guarda de literales aborta cualquier mutante en el código del bloque P1). Cada
## uno tiene que sacar SU regla de «cubierto», medida con la misma puerta (canon_p1 + veredicto_p1c, 24 órdenes).
mut_p1 <- function(id, clase, regla, de, a) {
  i <- grep(de, CANON$opciones, fixed = TRUE)
  if (length(i) != 1L) { falla(id, " MAL CONSTRUIDO: «", de, "» aparece en ", length(i), " opciones de la canónica"); filas_mut[[id]] <<- c(id, "2 a", "-", "mal_construido", NA); return(invisible()) }
  CM <- CANON; CM$opciones[i] <- sub(de, a, CM$opciones[i], fixed = TRUE)
  CVM <- lapply(perms(1:4), function(p) { x <- CM; x$opciones <- CM$opciones[p]; x$sede_o <- CM$sede_o[p]; x$rol <- CM$rol[p]; x$nom_o <- CM$nom_o[p]; x$clave <- which(p == CM$clave); x })
  CMx <- canon_p1(CVM); ex <- if (clase == "regla") CMx$reglas else CMx$sondas; fi <- if (clase == "regla") FIRMA_P1_CANON else FIRMA_P1_SONDAS
  vv <- veredicto_p1c(ex, fi); llego <- !identical(CM$opciones, CANON$opciones) && identical(CM$clave, CANON$clave)
  ver <- if (!llego) "mal_construido" else if (vv[[regla]] == "pendiente") "cazado_por_su_sonda" else "no_detectado"
  d <- sprintf("%s %+.1f pp (firmado %+.1f) -> %s", regla, ex[[regla]], fi[[regla]], vv[[regla]])
  ok(ver == "cazado_por_su_sonda", sprintf("%s (decisión 2 a; copia de la canónica, «%s» -> «%s»): %s — %s", id, de, a, d, ver))
  filas_mut[[id]] <<- c(id, sprintf("2 a: «%s» fuera de «cubierto»", regla), d, ver, NA)
}
mut_p1("MP1", "regla", "quedarse con 'promedio'", "tuvo mayor promedio", "tuvo mayor media")
mut_p1("MP2", "regla", "mas digitos", "recogió 40 kilos más", "recogió 4 kilos más")
cat("  Decisión 2 a: las nueve reglas y sondas al 100 % (+75,0 pp) no pueden subir; ningún mutante puede sacarlas de «cubierto» y se declara.\n")
TM <- do.call(rbind, filas_mut); colnames(TM) <- c("id", "sonda_esperada", "sonda_real", "veredicto", "semilla")
cat("\n"); print(TM[TM[, "id"] %in% c("MC23", "MC24", "MC25", "MC26", "MC27", "MP1", "MP2"), c("id", "veredicto", "sonda_esperada", "sonda_real")], quote = FALSE)

## ---- Resultado ----------------------------------------------------------------------------------
titulo("RESULTADO")
if (length(FIRMADOS)) cat("En rojo, cubiertos por un override firmado («O-1 corregido» extendido por F-2; X6, X9, X10 y X11 en la canónica por L-C3; la canónica de la Parte 1 regla por regla por la decisión 2 a; H2 en la Parte 6 por «H2 a» (L-C5); regla #22 §P7-E):\n", paste0("  - ", FIRMADOS, "\n"), sep = "")
if (length(RESIDUOS)) cat("\nResiduos declarados:\n", paste0("  - ", RESIDUOS, "\n"), sep = "")
if (length(PENDIENTES)) cat("\nPendientes de firma del profesor (exit 3):\n", paste0("  - ", PENDIENTES, "\n"), sep = "")
cat("\n=== RESULTADO:", if (length(fallos)) sprintf("FALLA (%d)", length(fallos)) else
  if (length(PENDIENTES)) sprintf("PENDIENTE DE FIRMA (%d; 0 fallos)", length(PENDIENTES)) else
  if (length(FIRMADOS)) sprintf("EN ROJO BAJO OVERRIDE FIRMADO (%d cifras dentro de lo firmado: «O-1 corregido», X6, X9, X10 y X11 en la canónica, la canónica de la Parte 1 por la decisión 2 a y H2 por «H2 a»; 0 fallos, 0 pendientes)", length(FIRMADOS)) else
  "APROBADO (0 fallos)", "===\n")
quit(status = if (length(fallos)) 1L else if (length(PENDIENTES)) 3L else if (length(FIRMADOS)) 4L else 0L)
