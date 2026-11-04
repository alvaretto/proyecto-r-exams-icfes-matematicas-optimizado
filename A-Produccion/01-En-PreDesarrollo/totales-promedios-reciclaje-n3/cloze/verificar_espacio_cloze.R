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
##       Tanda 7: la batería es L-C4 v2 (md5 820e5146…): la Parte 2 se mide EXACTA con medir_p2_exacta() (3 claves × C(5,3)
##       tríos; las reglas del estímulo, sobre las N versiones como portadores), con el vocabulario por token y sus
##       contrarias; mutantes MC28 (vocabulario exclusivo de la clave) y MC29 (función de verdad de la Parte 2).
##       Tanda 8 (decisión b del profesor, 2026-10-31): la Parte 2 tiene solo el tipo «promedio»; MC30 se retiró con el
##       tipo quitado (su guarda ya no existe) y MC19 muta las tres claves «promedio». L-C6: el residuo firmado de la
##       Parte 2, regla por regla (FIRMA_P2), mutantes MC31 a MC35. L-C7: la regla por palabra de la Parte 6, firmada
##       regla por regla (firma_p6_palabras_f98f9dda.tsv, con FIRMA_P6_TOKEN como tope del estrato), sobre la
##       enumeración exacta de L-C5, mutantes MC36 y MC38.
##       Tras la tanda 8 (decisión «Va» del profesor, 2026-10-31): X5, X6 y X10 exactas sobre el sorteo de la Parte 2
##       (MC37); L-C7 regla por regla (firma_p6_palabras_f98f9dda.tsv, MC38). Tras la FASE 2C r3: MC39 (H2, la cláusula «o
##       que pase de su cifra» de L-C7) y el rótulo de L-C2 «muestral sobre la Parte 1» (H1).
##       Tanda C1 del ciclo 2026-11-02 (2026-11-03): la Parte 1 se re-empalma desde el SCHOICE aprobado 63e2713e. rec() ya
##       no lee lado_gdis (S7 lo quitó de la canónica y ningún consumidor del CLOZE usa el campo): MC40. E4 mide la batería
##       §P7 y los márgenes de la Parte 1 con el espacio duro normalizado (decisión (a) del SCHOICE, por F-2; las guardas de
##       texto miran el texto crudo): MC43. MP2, adaptado al espacio duro. La firma de la S2d (41 cifras) y la lectura (iii)
##       se extienden por alcance a la Parte 1 (decisión 3 del 2026-11-03) con el código de SUB/verificar_espacio.R: MC41 y
##       MC42. L-6 en la prosa propia de la Parte 1 de la Solution, con controles exactos: MC45 a MC48. Sin literales U+00A0,
##       U+2009, U+202F ni U+2060 en el .Rmd ni en este verificador: MC44.
##       Tanda C1b (2026-11-03): H1 de la FASE 2C r4, la sonda H1-Sol sobre el párrafo «Parte 6.» de la Solution emitida
##       (cada comparación con cifras, verdadera leída del texto; control exacto de dos por versión): MC50, MC51, MC53 y
##       MC54; la guarda del data_generation que ata la comparación a v_hp y v_hg: MC52. H2 de la r4: MC39 con línea base
##       (h2_exacta sin mutar) y con el conteo de los pendientes que abre fuera de j39.
##       Tanda C1c (2026-11-03): H1 de la FASE 2C r5, la sonda Crit-P6 sobre el criterio general del párrafo «Parte 6.» (el
##       literal una vez por versión y ninguna cifra en el tramo general; control exacto): MC56 y MC57. H3 de la r5: mut_h1()
##       con la convención _y_otra y _y_control, y MC55, mutante del entorno que prueba sola la componente «marca».
##       Tanda C1d (2026-11-03): el criterio general cambia otra vez por decisión del profesor («Cambiar la frase (Recomendada)»).
##       Solo cambian los literales que dependen de la frase: P6_CRITERIO, P6_NUEVO (MC51), MC56 (vuelve a la frase de ef2c96dc) y
##       el ancla de MC57. Ninguna sonda, ningún umbral ni ninguna firma.
##       Tanda C1e (2026-11-03): el criterio general cambia otra vez por decisión del profesor («Reformular (Recomendada)»).
##       Solo cambian los literales que dependen de la frase: P6_CRITERIO, P6_NUEVO (MC51), MC56 (vuelve a la frase de 32243d0a) y
##       el ancla de MC57. Ninguna sonda, ningún umbral ni ninguna firma.
## Salida: exit 1 = algo falla; 3 = pendiente de firma (una cifra firmada superada o un canal cruzado > +8 pp
##   fuera de lo firmado); 4 = EN ROJO bajo override firmado («O-1 corregido» extendido por F-2 y X9 en la
##   canónica por L-C3, X6, X9, X10 y X11; la canónica de la Parte 1 regla por regla por la decisión 2 a; todo dentro
##   de lo firmado, H2 en la Parte 6 por «H2 a», L-C5, el residuo de la Parte 2 por la decisión b y la regla por palabra
##   de la Parte 6 firmada regla por regla por la decisión «Va», L-C6 y L-C7; regla #22 §P7-E: nunca PASS); 0 = sin rojo.
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
cat("SCHOICE leído: md5 ", unname(tools::md5sum(RMD_S)), " (aprobado para el aula: 63e2713e6ac0bdbf250f4dc33901b2ec; re-empalmado en la tanda C1)\n", sep = "")
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
## Decisión (a) del SCHOICE (2026-11-02), extendida a la Parte 1 por F-2 (tanda C1): la batería §P7 y los márgenes de
## largo de la Parte 1 miden el texto que lee el estudiante, con U+00A0 convertido en espacio. Las guardas de texto (L-6,
## S1) miran el texto crudo. MC43 corre el juicio del canal de largo sin normalizar.
norm_ed <- function(o) gsub("\u00a0", " ", o, fixed = TRUE)
num_all <- function(s) as.numeric(unlist(regmatches(s, gregexpr("[0-9]+", s))))
cif1 <- function(s) { v <- regmatches(s, gregexpr("[0-9]+", s))[[1]]; if (length(v)) as.numeric(v[1]) else NA_real_ }
toks <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
## Oráculo de tablas, propio: lee las filas de datos de una tabla pipe mostrada (nombre, cursos, kilos).
tabla_leida <- function(md) {
  f <- grep("^\\|", strsplit(md, "\n", fixed = TRUE)[[1]], value = TRUE)
  f <- f[!grepl("^\\|[-:| ]+\\|$", f)]
  cel <- lapply(f, function(r) {
    x <- trimws(strsplit(sub("^\\|", "", sub("\\|$", "", r)), "|", fixed = TRUE)[[1]])
    trimws(gsub("\u00a0", " ", gsub("<span[^>]*>|</span>|\\*\\*", "", x)))
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
    fila1 = conj$nombres[1], tabla = c(d$nG, d$nP, d$aG, d$aP), gi = gi,
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
## Tanda C1: el oráculo lee el texto como se ve, con U+00A0 como espacio (decisión (a) del SCHOICE, por F-2). MC49.
or_p1 <- function(x, nf = norm_ed) {
  tb <- tabla_leida(x$tabla_md); tt <- tb$n * tb$a; g <- which.max(tt); D <- max(tt) - min(tt)
  if (length(unique(tt)) != 2L) return(FALSE)
  nombra_g <- vapply(x$opciones, function(o) grepl(tolower(tb$nombre[g]), tolower(sujeto(nf(o))), fixed = TRUE), NA)
  cand <- nombra_g & !is.na(vapply(x$opciones, cif1, 0)) & vapply(x$opciones, cif1, 0) == D
  sum(cand) == 1L && which(cand) == x$clave
}
or_p2 <- function(x) {
  ## Pasada 3 (tanda 7): una clave divide lo que recogió ese grado, entero y sin más operaciones, entre SUS cursos (tres
  ## redacciones); los distractores dividen con otro operando (el promedio, dos, el cuadrado, los dos grados) o hablan de
  ## la mayoría. Desde la tanda 8 (decisión b) la Parte 2 tiene solo este tipo.
  es_clave <- grepl("^Lo que recogió ese grado dividido (en partes iguales entre sus cursos|entre el número de sus cursos|entre la cantidad de cursos de ese mismo grado)\\.$", x$opciones_p2)
  identical(x$tipo_p2, "promedio") && sum(es_clave) == 1L && identical(es_clave, x$sol_p2)
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
## Decisión b (tanda 8): la Parte 2 es «promedio» en todas las versiones (su pregunta y el molde de sus opciones).
ok(all(vapply(TODAS, function(x) identical(x$tipo_p2, "promedio") && all(startsWith(x$opciones_p2, "Lo que recogió")) &&
  grepl("^¿Qué indica, en la tabla del otro colegio, el promedio de .+ recogido por curso de un grado\\?$", x$texto_p2), NA)),
  sprintf("decisión b: la Parte 2 es «promedio» en las %d versiones de las muestras (su pregunta y opciones con el molde «Lo que recogió»)", length(TODAS)))
for (tp in "promedio") {
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
## Decisión «Va» del profesor (2026-10-31): X5, X6 y X10 se miden EXACTAS sobre el sorteo de la Parte 2. Su banco es fijo
## (tanda 8: solo el tipo «promedio»): 3 claves (paráfrasis) y 5 distractores; cada versión muestra una clave uniforme y un
## trío de distractores (C(5,3) = 10 tríos equiprobables), el mismo supuesto de medir_p2_exacta(). Una sola función mide la
## cifra exacta, aquí y en el mutante MC37; la clave de la combinación va en la posición 1 (las reglas cuentan palabras o
## combinan con S_U, el orden de las opciones no importa).
REGLA_X5 <- "X5 más palabras en común con todo el texto de las Partes 2 a 6"
REGLA_X6 <- "X6 más palabras en común con la clave de la Parte 2"
REGLA_X10 <- "X10 S_U + más palabras en común con la clave de la Parte 2"
BANCO_P2 <- correr(EXPR_C, 1L)$P2$promedio
## sol_p2 es LÓGICO: los distractores son opciones_p2[!sol_p2]. Con «-sol_p2» el índice sería 0/-1 y quitaría siempre la
## opción 1, no la clave (primera corrida de la tanda «Va»: FALLA en las 74 de 100 versiones con la clave en otra posición).
sup_sorteo_p2 <- function(L) all(vapply(L, function(x) {
  k <- x$opciones_p2[x$sol_p2]; d <- x$opciones_p2[!x$sol_p2]
  is.logical(x$sol_p2) && length(k) == 1L && k %in% BANCO_P2$clave && all(d %in% BANCO_P2$distractores) && length(unique(c(k, d))) == 4L }, NA))
ok(sup_sorteo_p2(NAT) && sup_sorteo_p2(CU$CURSOS) && sup_sorteo_p2(CU$PROMEDIO),
   sprintf("sorteo de la Parte 2: en las %d versiones de mezcla, CURSOS y PROMEDIO, la clave mostrada está entre las 3 del banco y los 3 distractores mostrados entre los 5",
           length(NAT) + length(CU$CURSOS) + length(CU$PROMEDIO)))
combinaciones_p2 <- function(banco) { cl <- unique(unname(banco$clave)); di <- unique(unname(banco$distractores))
  combos <- list()
  for (i in seq_along(cl)) for (s in utils::combn(length(di), 3L, simplify = FALSE)) combos[[length(combos) + 1L]] <- c(cl[i], di[s])
  combos }
medir_exacta_p2 <- function(L, regla, banco) mean(vapply(L, function(x) mean(vapply(combinaciones_p2(banco), function(op_p2) {
  y <- x; y$opciones_p2 <- op_p2; y$sol_p2 <- 1L; sc(regla(y), y$clave) }, 0)), 0))
NOM_EXACTAS <- c(REGLA_X5, REGLA_X6, REGLA_X10)
XT_EX <- sapply(list(mezcla = NAT, CURSOS = CU$CURSOS, PROMEDIO = CU$PROMEDIO), function(L)
  vapply(XREG[NOM_EXACTAS], function(r) medir_exacta_p2(L, r$f, BANCO_P2), 0))
cat("  Exactas sobre el sorteo de la Parte 2 (3 claves × C(5,3) tríos = 30 combinaciones equiprobables) y muestrales sobre la Parte 1 (las mismas N = 100 por estrato):\n")
print(round(100 * XT_EX, 1))
## L-C2 (decisión 3.1 a del profesor, 2026-10-30): un canal cruzado de más de +8 pp que siga abierto no se cierra
## solo: queda PENDIENTE DE FIRMA (exit 3) y vuelve al profesor. Una sola función decide, aquí y en el mutante MC17.
veredicto_cruzado <- function(ex) if (ex <= 0.08) "ok" else "pendiente"
for (i in seq_along(XREG)) for (est in c("mezcla", "CURSOS", "PROMEDIO")) {
  ex <- XT[i, est] - XREG[[i]]$base
  msg <- sprintf("%s, %s: %.1f %% (%+.1f pp sobre %.0f %%)", names(XREG)[i], est, 100 * XT[i, est], 100 * ex, 100 * XREG[[i]]$base)
  if (names(XREG)[i] %in% NOM_EXACTAS) {
    ex <- XT_EX[names(XREG)[i], est] - XREG[[i]]$base
    msg <- sprintf("%s, %s: exacta sobre el sorteo de la Parte 2 y muestral sobre la Parte 1 (N = 100; decisión «Va» del profesor, 2026-10-31) %.1f %% (%+.1f pp sobre %.0f %%); en la muestra %.1f %%",
                   names(XREG)[i], est, 100 * XT_EX[names(XREG)[i], est], 100 * ex, 100 * XREG[[i]]$base, 100 * XT[i, est])
  }
  if (veredicto_cruzado(ex) == "ok") ok(TRUE, "batería cruzada: ", msg) else pendiente("canal cruzado > +8 pp (F-2 y L-C2: vuelve al profesor): ", msg)
}
## X6 exacta por clave en cada estrato (el mismo desglose de salida/x6_estratos_t8.txt).
for (est in c("mezcla", "CURSOS", "PROMEDIO")) {
  L <- list(mezcla = NAT, CURSOS = CU$CURSOS, PROMEDIO = CU$PROMEDIO)[[est]]
  pc <- vapply(unique(unname(BANCO_P2$clave)), function(k)
    medir_exacta_p2(L, XREG[[REGLA_X6]]$f, list(clave = k, distractores = BANCO_P2$distractores)), 0)
  cat(sprintf("  X6 exacta sobre la Parte 2 (muestral sobre la Parte 1), %-8s n = %3d: %.1f %% (%+.1f pp); por clave: %s\n", est, length(L),
              100 * XT_EX[REGLA_X6, est], 100 * (XT_EX[REGLA_X6, est] - XREG[[REGLA_X6]]$base),
              paste(sprintf("%+.1f", 100 * (pc - XREG[[REGLA_X6]]$base)), collapse = " / ")))
}
## Su mutante, MC37, está en E6, después de MC17: necesita mutar() y filas_mut, que se definen allí.
## L-C3 (decisión 3.2 a y «a · aclaración sí» del profesor, 2026-10-30): override de X6, X9, X10 y X11 en la
## canónica, con sus cifras de ENUMERACIÓN EXACTA. Cubiertas por dominancia: en la canónica, el atajo firmado de
## «O-1 corregido» ya acierta el 100 %. Firmar el máximo de una batería no cubre las demás reglas: el alcance va
## regla por regla (la tanda 1 solo declaró X9, y X6, X10 y X11 quedaron fuera hasta la tanda 3). La puerta es la
## enumeración exacta conducida por el código (enumerar_canonica_cloze.R) para las ONCE reglas; la columna
## «canonica» de la tabla de arriba (n = 30) es solo informativa. Dentro del alcance, «rojo cubierto» (exit 4),
## nunca PASS (regla #22 §P7-E); por encima, o si cualquier otra regla pasa de +8 pp, PENDIENTE DE FIRMA (exit 3).
## Se comparan redondeadas a un decimal, como se firmaron.
REGLA_X9 <- "X9 S_U + la que nombra la ventaja del grado ganador"
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
  L <- est[[2]]; ops <- lapply(L, function(x) norm_ed(x$opciones)); cl <- vapply(L, function(x) x$clave, 0L)
  evaluar_bateria <- get("evaluar_bateria", envir = ENV_S)   # vive en bateria_eliminacion.R (lo carga la referencia)
  res <- evaluar_bateria(REGLAS, ops, cl, familias_no_aplicables = ENV_S$NA_FAM, estimulo_no_aplicable = ESTIMULO_P1)
  cat(sprintf("  batería §P7 (%d reglas) %s n=%d: máx %.1f %% (%s), techo nulo %.1f %%, exceso %+.1f pp -> %s\n",
              length(REGLAS), est[[1]], length(L), 100 * res$max_obs, res$regla_top, 100 * res$techo_nulo, 100 * res$exceso, res$veredicto))
  mg <- function(f) vapply(L, function(x) { n <- nchar(norm_ed(x$opciones)); k <- x$clave
    if (sum(n == f(n)) != 1 || n[k] != f(n)) return(NA_real_); abs(n[k] - f(n[-k])) / f(n[-k]) }, 0)
  mmax <- max(c(mg(max), mg(min), 0), na.rm = TRUE)
  mt <- vapply(L, function(x) { k <- vapply(norm_ed(x$opciones), ENV_S$ntok, 0L); dv <- abs(k - stats::median(k))
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
  S <- tryCatch(get(".conjunto_de", envir = ENV_S)(r$fn(norm_ed(x$opciones)), 4L), error = function(e) rep(FALSE, 4)); sc(S, x$clave) }, 0)), 0)
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
    S <- tryCatch(get(".conjunto_de", envir = ENV_S)(rg$fn(norm_ed(x$opciones)), 4L), error = function(e) rep(FALSE, 4)); sc(S, x$clave) }, 0)), 0)
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
nl_can <- mean(vapply(CANON_V, function(x) { n <- nchar(norm_ed(x$opciones)); sc(n != max(n), x$clave) }, 0))
residuo(sprintf("canónica de la Parte 1, fuera de la batería congelada (§P7-C, decisión 2 a: solo declarada): «descarta la más larga» %.1f %% (%+.1f pp)", 100 * nl_can, 100 * (nl_can - 0.25)))

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
RE_SIG <- "\\$[^A-Za-z0-9$\\n]{0,3}[.,:;!?)\\]\u00bb\u201d\u2026]"
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
RE_LIT <- "[\u00a0\u2009\u202f\u2060]"
lit_en <- function(txt) sum(grepl(RE_LIT, txt, perl = TRUE))
ok(lit_en(readLines(RMD_C, encoding = "UTF-8")) == 0L && lit_en(readLines(file.path(DIR, "verificar_espacio_cloze.R"), encoding = "UTF-8")) == 0L,
   "C1 · L-6: ni el .Rmd ni este verificador llevan U+00A0, U+2009, U+202F o U+2060 literales")
## ---- C1b (tanda C1b, 2026-11-03): H1 de la FASE 2C r4, el párrafo «Parte 6.» de la Solution ------------------------------
## Sonda H1-Sol, sobre la Solution emitida (el chunk «solucion» en las N versiones de la mezcla, como la de L-6). Lee del TEXTO,
## con U+00A0 como espacio, cada comparación con cifras del párrafo «Parte 6.»: una cifra con «kilos», «más», «menos», «mayor»
## o «menor» seguido de «que» y otra cifra con «kilos», sin cruzar un punto, dos puntos ni punto y coma. La tabla del otro
## colegio se lee como se muestra (tabla_leida(), sin las funciones del .Rmd). Por comparación cuenta:
##   falsa  la desigualdad que dice el texto no se cumple con las dos cifras que cita;
##   cifra  la primera cifra no es el promedio del grado que nombra su frase, la segunda no es la diferencia entre los totales
##          de la tabla, o el grado no es el que perdió («un curso más») o el que ganó («un curso menos»);
##   concl  la conclusión de su frase no se sigue del «más» o «menos» escrito, o falta, o hay dos;
##   marca  la conclusión contradice la marca de HP o de HG, tomada de sol_p6 y rol_p6 del entorno, no del exsolution emitido
##          (esa lectura la hace el oráculo r10 del paso 8; H3 de la FASE 2C r5);
##   esp    en el párrafo, una cifra seguida de «kilos» con espacio normal (L-6, sobre el texto crudo).
## Control EXACTO: dos comparaciones por versión, una con «un curso más» y otra con «un curso menos». Mutantes: MC50 a MC55.
P6_CONCL <- list(mas = c("habría ganado" = TRUE, "no le habría alcanzado para ganar" = FALSE),
                 menos = c("habría perdido" = TRUE, "habría seguido ganando" = FALSE))
RE_CMP <- "([0-9]+) kilos[^0-9.:;]*?\\b(más|menos|mayor|menor) que [^0-9.:;]*?([0-9]+) kilos"
COMP_H1 <- c("falsa", "cifra", "concl", "marca", "esp")
h1_p6 <- function(sol_txt, envs = ENVS_NAT) {
  ex <- parse(text = sol_txt, encoding = "UTF-8")
  z <- c(falsa = 0, cifra = 0, concl = 0, marca = 0, esp = 0, n_cmp = 0, n_mas = 0, n_menos = 0, sin_parrafo = 0, err = 0)
  ramas <- c(mas_si = 0, mas_no = 0, menos_si = 0, menos_no = 0)
  for (en in envs) {
    t <- tryCatch(paste(capture.output(eval(ex, envir = new.env(parent = en))), collapse = "\n"), error = function(e) NA_character_)
    if (is.na(t)) { z[["err"]] <- z[["err"]] + 1; next }
    pr <- regmatches(t, regexpr("Parte 6\\. Cada hipótesis[^\\n]*", t, perl = TRUE))
    if (length(pr) != 1L) { z[["sin_parrafo"]] <- z[["sin_parrafo"]] + 1; next }
    z[["esp"]] <- z[["esp"]] + cuenta_re("[0-9] kilos?\\b", pr)
    tb <- tabla_leida(en$tabla_par_md); tb$T <- tb$n * tb$a; tb$nombre <- tolower(tb$nombre)
    gan <- tb$nombre[which.max(tb$T)]; per <- tb$nombre[which.min(tb$T)]; Dt <- max(tb$T) - min(tb$T)
    for (cl in strsplit(norm_ed(pr), "[.;]")[[1]]) {
      m <- regmatches(cl, regexec(RE_CMP, cl, perl = TRUE))[[1]]
      if (!length(m)) next
      z[["n_cmp"]] <- z[["n_cmp"]] + 1
      x <- as.numeric(m[2]); y <- as.numeric(m[4]); sube <- m[3] %in% c("más", "mayor")
      if (!(if (sube) x > y else x < y)) z[["falsa"]] <- z[["falsa"]] + 1
      papel <- if (grepl("un curso más", cl, fixed = TRUE)) "mas" else if (grepl("un curso menos", cl, fixed = TRUE)) "menos" else NA_character_
      g <- regmatches(cl, regexec("el grado (\\S+)", cl, perl = TRUE))[[1]]
      quien <- if (identical(papel, "mas")) per else if (identical(papel, "menos")) gan else NA_character_
      if (is.na(papel) || length(g) != 2L || !identical(g[2], quien) || !isTRUE(x == tb$a[tb$nombre == quien]) || y != Dt)
        z[["cifra"]] <- z[["cifra"]] + 1
      if (is.na(papel)) { z[["concl"]] <- z[["concl"]] + 1; next }
      z[[paste0("n_", papel)]] <- z[[paste0("n_", papel)]] + 1
      hay <- vapply(names(P6_CONCL[[papel]]), function(k) grepl(k, cl, fixed = TRUE), NA)
      if (sum(hay) != 1L) { z[["concl"]] <- z[["concl"]] + 1; next }
      cv <- unname(P6_CONCL[[papel]][hay])
      if (cv != sube) z[["concl"]] <- z[["concl"]] + 1
      if (!identical(cv, isTRUE(en$sol_p6[en$rol_p6 == if (papel == "mas") "hp" else "hg"]))) z[["marca"]] <- z[["marca"]] + 1
      k <- paste0(papel, if (sube) "_si" else "_no"); ramas[[k]] <- ramas[[k]] + 1
    }
  }
  list(z = z, ramas = ramas)
}
H1S <- h1_p6(SOL_C)
ok(H1S$z[["err"]] == 0 && H1S$z[["sin_parrafo"]] == 0 && all(H1S$z[COMP_H1] == 0),
   sprintf("C1b · H1-Sol, párrafo «Parte 6.» de la Solution (%d versiones), leído del texto: comparaciones falsas %d · cifra o grado que no son los de la tabla %d · conclusión que no se sigue del «más» o «menos» %d · conclusión contra la marca de HP o HG %d · cifra-kilos con espacio normal %d · versiones sin el párrafo %d",
           nv, as.integer(H1S$z[["falsa"]]), as.integer(H1S$z[["cifra"]]), as.integer(H1S$z[["concl"]]), as.integer(H1S$z[["marca"]]),
           as.integer(H1S$z[["esp"]]), as.integer(H1S$z[["sin_parrafo"]])))
ok(H1S$z[["n_cmp"]] == 2 * nv && H1S$z[["n_mas"]] == nv && H1S$z[["n_menos"]] == nv,
   sprintf("C1b · H1-Sol, control exacto: %d comparaciones con cifras (esperadas %d), %d con «un curso más» y %d con «un curso menos» (esperadas %d y %d); por rama: «un curso más» y «más» %d, «un curso más» y «menos» %d, «un curso menos» y «más» %d, «un curso menos» y «menos» %d",
           as.integer(H1S$z[["n_cmp"]]), 2L * nv, as.integer(H1S$z[["n_mas"]]), as.integer(H1S$z[["n_menos"]]), nv, nv,
           as.integer(H1S$ramas[["mas_si"]]), as.integer(H1S$ramas[["mas_no"]]), as.integer(H1S$ramas[["menos_si"]]), as.integer(H1S$ramas[["menos_no"]])))

## ---- C1c (tanda C1c, 2026-11-03): H1 de la FASE 2C r5, el criterio general del párrafo «Parte 6.» ----------------------------
## Sonda Crit-P6, sobre la Solution emitida y sobre el texto crudo. H1-Sol no ve el criterio general, porque solo lee las
## comparaciones con cifras; por eso esta sonda va aparte y su mutante no mueve H1-Sol. Cuenta por versión:
##   falta  el párrafo no contiene el criterio literal exactamente una vez;
##   cifra  el tramo general del párrafo (sin «Parte 6.», hasta «Con un curso más, el grado») lleva alguna cifra.
## Control EXACTO: el literal aparece una vez por versión, nv apariciones en total. Mutantes: MC56 (vuelve a la frase de
## 32243d0a) y MC57 (una cifra en el tramo general, con el literal intacto).
P6_CRITERIO <- "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
COMP_CRIT <- c("falta", "cifra")
crit_p6 <- function(sol_txt, envs = ENVS_NAT, lit = P6_CRITERIO) {
  ex <- parse(text = sol_txt, encoding = "UTF-8")
  z <- c(falta = 0, cifra = 0, n_crit = 0, sin_parrafo = 0, err = 0)
  for (en in envs) {
    t <- tryCatch(paste(capture.output(eval(ex, envir = new.env(parent = en))), collapse = "\n"), error = function(e) NA_character_)
    if (is.na(t)) { z[["err"]] <- z[["err"]] + 1; next }
    pr <- regmatches(t, regexpr("Parte 6\\. Cada hipótesis[^\\n]*", t, perl = TRUE))
    if (length(pr) != 1L) { z[["sin_parrafo"]] <- z[["sin_parrafo"]] + 1; next }
    n <- lengths(regmatches(pr, gregexpr(lit, pr, fixed = TRUE)))
    z[["n_crit"]] <- z[["n_crit"]] + n
    if (n != 1L) z[["falta"]] <- z[["falta"]] + 1
    gen <- sub("Con un curso más, el grado.*$", "", sub("^Parte 6\\. ", "", pr), perl = TRUE)
    if (grepl("[0-9]", gen)) z[["cifra"]] <- z[["cifra"]] + 1
  }
  z
}
CRIT <- crit_p6(SOL_C)
ok(CRIT[["err"]] == 0 && CRIT[["sin_parrafo"]] == 0 && all(CRIT[COMP_CRIT] == 0),
   sprintf("C1c · Crit-P6, criterio general del párrafo «Parte 6.» (%d versiones), leído del texto crudo: sin el literal exactamente una vez %d · con cifras en el tramo general %d · versiones sin el párrafo %d",
           nv, as.integer(CRIT[["falta"]]), as.integer(CRIT[["cifra"]]), as.integer(CRIT[["sin_parrafo"]])))
ok(CRIT[["n_crit"]] == nv, sprintf("C1c · Crit-P6, control exacto: %d apariciones del criterio literal (esperadas %d)", as.integer(CRIT[["n_crit"]]), nv))

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
cat("  Parte 2: clave conceptual fija (V8, fija-justificada; un solo tipo desde la decisión b); cambia su redacción (3 paráfrasis).\n")

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
       cambios = list(c("Lo que recogió la mayoría de los cursos de ese mismo grado.", "Lo que recogió la mayoría de los cursos de ese mismo grado, sin error.")),
       llego = function(e) isTRUE(any(grepl("error", e$opciones_p2)))),
  list(id = "MC29", sonda = "Parte 2: cada distractor «promedio» es falso", desc = "la función de verdad de «luego_dos» describe el promedio (T / n), no la mitad",
       cambios = list(c("luego_dos      = function(n, a, n2, a2) (n * a) / n / 2,", "luego_dos      = function(n, a, n2, a2) (n * a) / n,")),
       llego = function(e) isTRUE(e$valor_p2$luego_dos(3, 40, 4, 45) == 40)),
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
       llego = function(e) isTRUE(length(e$opciones_p6) == 4L && !any(grepl("^Ganó el concurso el grado", e$opciones_p6)))),
  ## MC52 (C1b, H1 de la FASE 2C r4): la comparación de la Solution de la Parte 6 toma el promedio del grado que ganó para
  ## «un curso más». Sonda: la guarda del data_generation que la ata a los veredictos sorteados.
  list(id = "MC52", sonda = "H1 r4: la comparación de la Solution de la Parte 6 coincide",
       desc = "hp_supera compara el promedio del grado que ganó con la diferencia",
       cambios = list(c("hp_supera <- dp$aP > dp$D", "hp_supera <- dp$aG > dp$D")),
       llego = function(e) isTRUE(e$hp_supera == (e$dp$aG > e$dp$D))))
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
## MC37 (L-C2 exacta, decisión «Va» del profesor, 2026-10-31): las tres claves de la Parte 2 ganan «aunque fue menor»
## (las mismas tres sustituciones de MC19). Su sonda es X6 EXACTA sobre el sorteo, con la misma función de la puerta
## (medir_exacta_p2) y el banco del código mutado, en las semillas 1 a 200 (las de medir_mut), y en los MISMOS tres
## estratos que juzga la puerta: la mezcla y cada rama de la Parte 1 (sin la canónica, como CU). veredicto_cruzado() tiene
## que dejarla PENDIENTE DE FIRMA en alguno. Medida solo en la mezcla, la mutación se diluye: abre X6 en la rama CURSOS
## (+38,2 pp) y la vuelve antipredictiva en la PROMEDIO (−25,0 pp), y la mezcla queda en +8,5 pp, medio punto sobre el umbral.
m37 <- mutar(list(
  c('"Lo que recogió ese grado dividido en partes iguales entre sus cursos."',
    '"Lo que recogió ese grado dividido en partes iguales entre sus cursos aunque fue menor."'),
  c('"Lo que recogió ese grado dividido entre el número de sus cursos."',
    '"Lo que recogió ese grado dividido entre el número de sus cursos aunque fue menor."'),
  c('"Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado."',
    '"Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado aunque fue menor."')))
if (!is.null(m37$error)) { falla("MC37 MAL CONSTRUIDO: ", m37$error); filas_mut[["MC37"]] <- c("MC37", "L-C2 exacta", "-", "mal_construido", NA) } else {
  en37 <- correr(m37$expr, 1L); b37 <- if (is.null(en37$.err)) en37$P2$promedio else NULL
  llego37 <- !is.null(b37) && all(endsWith(b37$clave, "aunque fue menor."))
  L37 <- list(); for (z in 1:200) { en <- correr(m37$expr, z); if (is.null(en$.err)) L37[[length(L37) + 1L]] <- rec(en) }
  E37 <- list(mezcla = L37, CURSOS = Filter(function(x) !x$canon && x$rama == "CURSOS", L37),
              PROMEDIO = Filter(function(x) !x$canon && x$rama == "PROMEDIO", L37))
  llego37 <- llego37 && all(lengths(E37) >= 20L)   # regla #23: un estrato con n < 20 no concluye
  ex37 <- if (llego37) vapply(E37, function(L) medir_exacta_p2(L, XREG[[REGLA_X6]]$f, b37) - XREG[[REGLA_X6]]$base, 0) else NA_real_
  v37 <- if (llego37) vapply(ex37, veredicto_cruzado, "") else NA
  d37 <- if (llego37) paste(sprintf("%s %+.1f pp (n = %d) -> %s", names(E37), 100 * ex37, lengths(E37), v37), collapse = "; ") else "-"
  ver37 <- if (!llego37) "mal_construido" else if (any(v37 == "pendiente")) "cazado_por_su_sonda" else "no_detectado"
  ok(ver37 == "cazado_por_su_sonda", sprintf("MC37 (L-C2 exacta: las tres claves de la Parte 2 ganan «aunque fue menor»): la batería deja X6 exacta PENDIENTE DE FIRMA en algún estrato (%s) — %s", d37, ver37))
  filas_mut[["MC37"]] <- c("MC37", "batería cruzada exacta > +8 pp: pendiente (L-C2)", d37, ver37, NA)
}
## MC18 a MC21 (L-C3, tanda 3): un mutante por regla firmada en la canónica, medido con la MISMA enumeración exacta
## que la puerta (sin semillas: no hay «primera muerte» que buscar). Cada uno tiene que sacar SU regla de «cubierto»;
## se informa qué otras reglas salen con él. La materialización se comprueba en el entorno de las hojas.
MUT_CANON <- list(
  list(id = "MC18", regla = REGLA_X9, desc = "la canónica sale siempre con el otro colegio en la rama CURSOS",
       cambios = list(c('rama_par <- sample(c("CURSOS", "PROMEDIO"), 1L)',
                        'rama_par <- if (isTRUE(es_canonica)) "CURSOS" else sample(c("CURSOS", "PROMEDIO"), 1L)')),
       llego = function(H) all(vapply(H, function(h) identical(h$rama_par, "CURSOS") && !"rama_par" %in% names(h$ruta), NA))),
  ## Tanda 6 (pasada 2 de la Parte 2): con la redacción nueva, X6 y X10 bajan en la canónica, así que un solo texto mutado ya
  ## no basta para salir de «cubierto». Tanda 8 (decisión b): la Parte 2 tiene solo el tipo «promedio»; MC19 muta sus tres
  ## claves y MC20 dos de ellas. La sonda y la puerta no cambian.
  list(id = "MC19", regla = REGLA_X6, desc = "las tres claves de la Parte 2 ganan «aunque», «fue» y «menor» (palabras de la clave de la Parte 1)",
       cambios = list(c('"Lo que recogió ese grado dividido en partes iguales entre sus cursos."',
                        '"Lo que recogió ese grado dividido en partes iguales entre sus cursos aunque fue menor."'),
                      c('"Lo que recogió ese grado dividido entre el número de sus cursos."',
                        '"Lo que recogió ese grado dividido entre el número de sus cursos aunque fue menor."'),
                      c('"Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado."',
                        '"Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado aunque fue menor."')),
       llego = function(H) all(vapply(H, function(h) isTRUE(grepl(" aunque fue menor.", h$clave_p2, fixed = TRUE)), NA))),
  list(id = "MC20", regla = REGLA_X10, desc = "dos claves «promedio» de la Parte 2 ganan «aunque», «fue» y «menor» (palabras de la clave de la Parte 1)",
       cambios = list(c('"Lo que recogió ese grado dividido en partes iguales entre sus cursos."',
                        '"Lo que recogió ese grado dividido en partes iguales entre sus cursos aunque fue menor."'),
                      c('"Lo que recogió ese grado dividido entre el número de sus cursos."',
                        '"Lo que recogió ese grado dividido entre el número de sus cursos aunque fue menor."')),
       llego = function(H) any(vapply(H, function(h) isTRUE(grepl("cursos aunque fue menor.", h$clave_p2, fixed = TRUE)), NA))),
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
BAT <- file.path(DIR, "bateria_partes_2_a_6.R"); MD5_BAT <- "820e51464b34deff35b830789584de8a"   # L-C4 v2 (tanda 7); v1: de20e0a9…
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
  list(p2 = c(list(op = x$opciones_p2, k = which(x$sol_p2), tipo = x$tipo_p2), EB$estimulo_p2_rec(x)),
       p6 = list(op = x$opciones_p6, v = x$sol_p6, rol = x$rol_p6, n = tb$n, a = tb$a, nombres = tolower(tb$nombre)),
       rama_par = x$rama_par, k = abs(diff(tb$n))) }
POS_P2 <- c("P2-P1", "P2-P2", "P2-P1c", "P2-P2c")
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
## Decisión b (tanda 8): la Parte 2 tiene solo el tipo «promedio»; su banco es el mismo en todas las versiones. La semilla 1
## da el banco y la pregunta con que se midieron las cifras firmadas (salida/medir_bateria_v2.R usaba ENV[[1]]).
EN_P2 <- correr(EXPR_C, 1L)
ok(identical(names(EN_P2$P2), "promedio") && identical(EN_P2$tipo_p2, "promedio"), "decisión b: el banco de la Parte 2 tiene un solo tipo, «promedio»")
## L-C4 v2 (tanda 7): la Parte 2 EXACTA con medir_p2_exacta(): 3 claves × C(5,3) tríos (las reglas de contenido no dependen
## del orden), con el vocabulario por token y sus contrarias; las reglas del estímulo, integradas sobre las N versiones de la
## muestra estándar como portadores (párrafo, tabla, pregunta, Partes 3 a 6 y Parte 1).
EST_P2 <- lapply(NAT, function(x) EB$estimulo_p2_rec(x))
EST_TIPO <- function(q) lapply(EST_P2, function(e) { e$preg <- q$pregunta; EB$.con_ng(e) })
## L-C6 (tanda 8; decisión b del profesor, 2026-10-31): override del residuo de la Parte 2 «promedio» (§P7-D agotado), regla
## por regla, con las cifras exactas de salida/bateria_v2_898608dd.txt, comparadas a un decimal, como se firmaron. Dentro de
## lo firmado, «ROJO, cubierto» (exit 4), nunca PASS; por encima, o cualquier otra regla de la Parte 2 > +8 pp, PENDIENTE
## (exit 3). Mutantes MC31 a MC35, uno por regla, con la misma medida exacta.
FIRMA_P2 <- c("P2-R2" = 21.9, "P2-R1" = 13.6, "P2-E8c" = 13.5, "P2-E5c" = 11.1, "P2-M3c" = 10.0)
veredicto_p2 <- function(M) { ex <- 100 * M$exceso; f <- unname(FIRMA_P2[M$id])
  ifelse(ex <= 8 + 1e-9, "ok", ifelse(!is.na(f) & round(ex, 1) <= f + 1e-9, "cubierto", "pendiente")) }
Q_P2 <- EN_P2$P2$promedio
M_P2 <- EB$medir_p2_exacta(Q_P2, "promedio", EST_TIPO(Q_P2)); VP_P2 <- veredicto_p2(M_P2)
cab_p2 <- sprintf("Parte 2 «promedio», exacta (%d combinaciones × %d portadores, %d reglas)", attr(M_P2, "n_combos"), attr(M_P2, "n_portadores"), nrow(M_P2))
ok(all(names(FIRMA_P2) %in% M_P2$id), "L-C6: las cinco reglas firmadas existen en la batería congelada")
for (i in order(-M_P2$exceso)) if (VP_P2[i] != "ok") {
  m <- sprintf("L-C4 %s · %s: %.1f %% (nulo %.1f %%, %+.1f pp exactos) — %s", cab_p2, M_P2$id[i], 100 * M_P2$acierto[i], 100 * M_P2$nulo[i], 100 * M_P2$exceso[i], M_P2$nombre[i])
  if (VP_P2[i] == "cubierto") firmado(m, sprintf(" (firmado hasta %+.1f pp)", FIRMA_P2[[M_P2$id[i]]]), por = "decisión b (L-C6, Parte 2 «promedio»)") else
    pendiente(m, if (M_P2$id[i] %in% names(FIRMA_P2)) sprintf(", por encima de lo firmado (%+.1f pp)", FIRMA_P2[[M_P2$id[i]]]) else ", fuera de lo firmado en la decisión b")
}
for (r in names(FIRMA_P2)) cat(sprintf("  L-C6 %-7s %+5.1f pp exactos (firmado %+5.1f) -> %s\n", r, 100 * M_P2$exceso[M_P2$id == r], FIRMA_P2[[r]], VP_P2[M_P2$id == r]))
GRIS_P2 <- M_P2[M_P2$exceso > 0.053 + 1e-12 & M_P2$exceso <= 0.08 + 1e-12, , drop = FALSE]
cat(sprintf("  L-C6: %d reglas medidas; > +8 pp: %d (cubiertas %d, pendientes %d); zona gris (+5,3 a +8 pp): %s\n", nrow(M_P2), sum(VP_P2 != "ok"),
            sum(VP_P2 == "cubierto"), sum(VP_P2 == "pendiente"), if (nrow(GRIS_P2)) paste(sprintf("%s %+.1f", GRIS_P2$id, 100 * GRIS_P2$exceso), collapse = " · ") else "ninguna"))
V2 <- attr(M_P2, "vocabulario"); cat(sprintf("  L-C4 v2, Parte 2 «promedio»: %d tokens; vocabulario exclusivo de la clave: %s\n", nrow(V2),
    if (any(V2$k > 0 & V2$m == 0)) paste(V2$token[V2$k > 0 & V2$m == 0], collapse = " · ") else "ninguno"))
LS <- lapply(TODAS, reg_b); MS <- EB$medir_p2(LS, vocab = FALSE)
juzgar(MS[MS$id %in% POS_P2, ], sprintf("Parte 2 «promedio», posición, muestra n = %d", length(LS)), p95_p2(LS))
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
EB$fijar_universo_p6(h2_registros(H2X$A))   # L-C4 v2: P6-VX sobre todas las versiones
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
    llego <- isTRUE(mx$llego(HM$A)); EB$fijar_universo_p6(h2_registros(HM$A)); TM5 <- h2_juzgar(h2_tabla(HM$A, EB), FIRMA_H2)
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
## ---- L-C7 (tanda 8; decisión b del profesor, 2026-10-31) -----------------------------------------------------------
## Override firmado de la familia «las que dicen t son verdaderas» y su contraria (P6-V «t»), fuera de la batería
## congelada: REGLA POR REGLA, con la lista firma_p6_palabras_f98f9dda.tsv (216 filas: estrato, id, papel, firmado_pp y
## aplica_pct; decisión «Va» del profesor, 2026-10-31, md5 comprobado) y el tope del estrato (FIRMA_P6_TOKEN)
## conservado como cota. Se mide con la MISMA enumeración exacta de L-C5 (H2X) y la misma función que dio las cifras
## firmadas (EB$vocab_p6_tokens, exploratoria de salida/medir_bateria_v2.R). Cada regla por palabra (palabra, papel y
## sentido): +8 pp o menos, «ok»; por encima de +8 pp, dentro de su cifra firmada y del tope de su estrato, «ROJO,
## cubierto» (exit 4), nunca PASS; por encima de la cifra o fuera de la lista, PENDIENTE (exit 3). Mutantes MC36 y MC38.
FIRMA_P6_TOKEN <- c(mezcla = 0.0, CURSOS = 25.0, PROMEDIO = 25.0, k1 = 16.4, k2 = 16.7)
FIRMA_P6_TSV <- file.path(DIR, "firma_p6_palabras_f98f9dda.tsv")
stopifnot("la lista firmada regla por regla está en su md5" = unname(tools::md5sum(FIRMA_P6_TSV)) == "2aa8e7426b325d8fed9f8c6144608164")
FIRMA_P6_PALABRAS <- utils::read.delim(FIRMA_P6_TSV, quote = "", stringsAsFactors = FALSE, encoding = "UTF-8")
ok(nrow(FIRMA_P6_PALABRAS) == 216L, "L-C7: la lista firmada regla por regla tiene 216 filas (estrato, id, papel, firmado_pp, aplica_pct)")
p6_token_tabla <- function(A) do.call(rbind, lapply(names(H2_ESTRATOS), function(e) { sx <- H2_ESTRATOS[[e]](A); if (!any(sx)) return(NULL)
  VT <- EB$vocab_p6_tokens(h2_registros(A[sx, , drop = FALSE]), A$w[sx] / sum(A$w[sx])); VT$estrato <- rep(e, nrow(VT)); VT }))
firma_p6_f <- function(e, id, pp) { j <- which(FIRMA_P6_PALABRAS$estrato == e & FIRMA_P6_PALABRAS$id == id & FIRMA_P6_PALABRAS$papel == pp)
  if (length(j)) FIRMA_P6_PALABRAS$firmado_pp[j[1]] else NA_real_ }
p6_token_juzgar <- function(VT) { ex <- 100 * VT$exceso
  f <- vapply(seq_len(nrow(VT)), function(i) firma_p6_f(VT$estrato[i], VT$id[i], VT$papel[i]), 0)
  fe <- unname(FIRMA_P6_TOKEN[VT$estrato])
  ifelse(ex <= 8 + 1e-9, "ok", ifelse(!is.na(f) & round(ex, 1) <= f + 1e-9 & round(ex, 1) <= fe + 1e-9, "cubierto", "pendiente")) }
## La regla anterior (por estrato), conservada solo para el contraste del mutante MC38.
p6_token_juzgar_estrato <- function(VT) { ex <- 100 * VT$exceso; f <- unname(FIRMA_P6_TOKEN[VT$estrato])
  ifelse(ex <= 8 + 1e-9, "ok", ifelse(!is.na(f) & round(ex, 1) <= f + 1e-9, "cubierto", "pendiente")) }
VT7 <- p6_token_tabla(H2X$A); VT7$veredicto <- p6_token_juzgar(VT7)
ok(setequal(unique(VT7$estrato), names(FIRMA_P6_TOKEN)), "L-C7: los cinco estratos firmados están en la enumeración exacta")
clave_fila <- function(d) paste(d$estrato, d$id, d$papel)
ok(all(clave_fila(FIRMA_P6_PALABRAS) %in% clave_fila(VT7)), "L-C7: las 216 reglas de la lista firmada regla por regla existen en la enumeración exacta")
ok(all(VT7$veredicto[clave_fila(VT7) %in% clave_fila(FIRMA_P6_PALABRAS)] == "cubierto"), "L-C7: las 216 reglas de la lista firmada salen «cubierto»")
ok(!any(VT7$veredicto == "cubierto" & !clave_fila(VT7) %in% clave_fila(FIRMA_P6_PALABRAS)), "L-C7: ninguna «cubierto» fuera de la lista firmada")
for (e in names(FIRMA_P6_TOKEN)) { D <- VT7[VT7$estrato == e, , drop = FALSE]; D <- D[order(-D$exceso), , drop = FALSE]; R8 <- D[D$veredicto != "ok", , drop = FALSE]
  cat(sprintf("  L-C7 %-8s %d reglas por palabra (palabra × papel × sentido); máximo %+.1f pp (%s · %s); > +8 pp: %d (cubiertas %d, pendientes %d); tope del estrato %+.1f pp\n",
              e, nrow(D), 100 * D$exceso[1], D$id[1], D$papel[1], nrow(R8), sum(R8$veredicto == "cubierto"), sum(R8$veredicto == "pendiente"), FIRMA_P6_TOKEN[[e]]))
  for (i in seq_len(nrow(R8))) { m <- sprintf("L-C7 Parte 6 por palabra, %s · %s %s: %+.1f pp exactos (aplica %.0f %%)", e, R8$id[i], R8$papel[i], 100 * R8$exceso[i], 100 * R8$aplica[i])
    f <- firma_p6_f(R8$estrato[i], R8$id[i], R8$papel[i])
    if (R8$veredicto[i] == "cubierto") firmado(m, sprintf(" (firmada regla por regla: %+.1f pp)", f), por = "«Va» (L-C7, regla por regla)") else
      pendiente(m, if (is.na(f)) ", fuera de la lista firmada (L-C7 regla por regla)" else sprintf(", por encima de lo firmado (%+.1f pp)", f)) } }
## MC36 (L-C7): la hipótesis del que perdió dice «sin duda» solo cuando es verdadera. La regla por palabra («duda») no
## está en la lista firmada regla por regla y tiene que salir de «cubierto», medida con la misma puerta (enumeración
## exacta conducida por el código, 1 Parte 1).
local({
  de <- 'P6_HP <- "Si el grado que perdió hubiera participado con un curso más, conservando su promedio por curso, habría ganado el concurso."'
  a <- paste0('P6_HP <- if (v_hp) "Si el grado que perdió hubiera participado con un curso más, conservando su promedio por curso, sin duda habría ganado el concurso." else ',
              sub("^P6_HP <- ", "", de))
  mm <- mutar(list(c(de, a)))
  if (!is.null(mm$error)) { falla("MC36 MAL CONSTRUIDO: ", mm$error); filas_mut[["MC36"]] <<- c("MC36", "L-C7", "-", "mal_construido", NA); return(invisible()) }
  HM <- h2_exacta(mm$txt, 1L)
  llego <- !is.null(HM$A) && HM$masa_err == 0 && any(HM$A$v_hp) && all(grepl("sin duda", HM$A$hp_txt[HM$A$v_hp], fixed = TRUE)) &&
    !any(grepl("sin duda", HM$A$hp_txt[!HM$A$v_hp], fixed = TRUE))
  VM <- if (llego) p6_token_tabla(HM$A) else NULL; if (llego) VM$veredicto <- p6_token_juzgar(VM)
  pend <- if (llego) VM[VM$veredicto == "pendiente", , drop = FALSE] else VT7[0, ]
  pd <- pend[grepl("duda", pend$id, fixed = TRUE), , drop = FALSE]; pd <- pd[order(-pd$exceso), , drop = FALSE]
  ver <- if (!llego) "mal_construido" else if (nrow(pd)) "cazado_por_su_sonda" else if (nrow(pend)) "cazado_por_otra" else "no_detectado"
  d <- if (nrow(pd)) sprintf("%s · %s %s %+.1f pp (fuera de la lista firmada) -> pendiente; reglas por palabra pendientes: %d", pd$estrato[1], pd$id[1], pd$papel[1],
                             100 * pd$exceso[1], nrow(pend)) else sprintf("reglas por palabra pendientes: %d (masa con error %.3f)", nrow(pend), if (is.null(HM$A)) NA else HM$masa_err)
  ok(ver == "cazado_por_su_sonda", sprintf("MC36 (L-C7: HP dice «sin duda» solo si es verdadera): %s — %s", d, ver))
  filas_mut[["MC36"]] <<- c("MC36", "L-C7: regla por palabra fuera de la lista firmada", d, ver, NA) })
## MC38 (L-C7 regla por regla, decisión «Va» del profesor, 2026-10-31): las dos redacciones de ventaja «por promedio»
## (BP_perdio y BP_gano: «hubiera tenido el mismo promedio por curso») ganan «justamente», una palabra que ninguna otra
## afirmación lleva. Esas redacciones salen en las dos ramas del otro colegio (verdaderas en PROMEDIO, falsas en CURSOS), y
## «justamente» hereda la cifra de «tenido» en cada estrato. Las reglas por palabra nuevas («justamente…») no están en la
## lista firmada y tienen que salir PENDIENTES con la puerta nueva; la firma anterior, por estrato, las dejaba «cubierto»
## con su tope: es justo lo que la firma por estrato dejaba pasar. Se mide como MC36: la misma puerta (h2_exacta, que
## ramifica la rama del otro colegio y la redacción de ventaja) sobre la Parte 1 de la semilla 1.
local({
  m38 <- mutar(list(
    c("Si el grado que perdió hubiera tenido el mismo promedio por curso que el que ganó, conservando su",
      "Si el grado que perdió hubiera tenido justamente el mismo promedio por curso que el que ganó, conservando su"),
    c("Si el grado que ganó hubiera tenido el mismo promedio por curso que el que perdió, conservando su",
      "Si el grado que ganó hubiera tenido justamente el mismo promedio por curso que el que perdió, conservando su")))
  if (!is.null(m38$error)) { falla("MC38 MAL CONSTRUIDO: ", m38$error); filas_mut[["MC38"]] <<- c("MC38", "L-C7", "-", "mal_construido", NA); return(invisible()) }
  en38 <- correr(m38$expr, 1L); HM <- h2_exacta(m38$txt, 1L)
  bp38 <- !is.null(HM$A) && any(grepl("hubiera tenido", HM$A$c6_txt, fixed = TRUE))
  tx38 <- if (is.null(en38$.err)) vapply(en38$P6_VENTAJA, function(z) z$texto, "") else character(0)
  si38 <- if (is.null(en38$.err)) vapply(en38$P6_VENTAJA, function(z) isTRUE(z$si_cursos), NA) else logical(0)
  llego <- is.null(en38$.err) && HM$masa_err == 0 && bp38 && length(tx38) == 4L && sum(!si38) == 2L &&
    all(grepl("justamente", HM$A$c6_txt[grepl("hubiera tenido", HM$A$c6_txt, fixed = TRUE)], fixed = TRUE)) &&
    all(grepl("justamente", tx38[!si38], fixed = TRUE)) && !any(grepl("justamente", tx38[si38], fixed = TRUE))
  VM <- if (llego) p6_token_tabla(HM$A) else NULL
  if (llego) { VM$veredicto <- p6_token_juzgar(VM); VM$v_estrato <- p6_token_juzgar_estrato(VM) }
  j38 <- if (llego) VM[grepl("justamente", VM$id, fixed = TRUE) & VM$veredicto == "pendiente" & VM$v_estrato == "cubierto", , drop = FALSE] else VT7[0, ]
  j38 <- j38[order(-j38$exceso), , drop = FALSE]
  ver <- if (!llego) "mal_construido" else if (nrow(j38)) "cazado_por_su_sonda" else if (any(VM$veredicto == "pendiente")) "cazado_por_otra" else "no_detectado"
  d <- if (nrow(j38)) sprintf("%s · %s %s %+.1f pp -> pendiente con la firma regla por regla; la firma por estrato la cubría (tope %+.1f pp); reglas «justamente» pendientes: %d",
                              j38$estrato[1], j38$id[1], j38$papel[1], 100 * j38$exceso[1], FIRMA_P6_TOKEN[[j38$estrato[1]]], nrow(j38)) else
    sprintf("reglas «justamente» pendientes: 0 (masa con error %.3f)", if (is.null(HM$A)) NA else HM$masa_err)
  ok(ver == "cazado_por_su_sonda", sprintf("MC38 (L-C7 regla por regla: la redacción «hubiera tenido el mismo promedio por curso» gana «justamente»): %s — %s", d, ver))
  filas_mut[["MC38"]] <<- c("MC38", "L-C7: regla por palabra fuera de la lista firmada", d, ver, NA) })
## MC39 (L-C7, la cláusula «o que pase de su cifra»; H2 de la FASE 2C r3, 2026-10-31). MC36 y MC38 solo prueban reglas FUERA
## de la lista. Aquí, una regla DE la lista sube por encima de su cifra firmada sin pasar el tope de su estrato: la puerta
## regla por regla la deja pendiente y la de estrato la cubría. Las 32 reglas firmadas a la mitad del tope (8 por estrato) son
## trigramas que lleva una sola de las dos redacciones de su clase de ventaja. «perdió hubiera tenido» solo está en BP_perdio;
## el mutante lo pone también en BP_gano, así que pasa a delatar el veredicto como «tenido»: de +12,5 a +25,0 pp en las dos
## ramas (tope 25,0). Se mide como MC36 y MC38 (h2_exacta sobre la Parte 1 de la semilla 1). Antes, la sonda directa que propuso
## el detractor: no depende del texto y sigue probando la cláusula si otro ciclo cambia las redacciones.
local({
  mitad <- FIRMA_P6_PALABRAS[FIRMA_P6_PALABRAS$firmado_pp < unname(FIRMA_P6_TOKEN[FIRMA_P6_PALABRAS$estrato]) - 1e-9, , drop = FALSE]
  f39 <- VT7[clave_fila(VT7) == clave_fila(mitad[1, , drop = FALSE]), , drop = FALSE]
  f39$exceso <- (mitad$firmado_pp[1] + 1) / 100   # 1 pp sobre su cifra, todavía bajo el tope de su estrato
  ok(nrow(mitad) == 32L && nrow(f39) == 1L && p6_token_juzgar(f39) == "pendiente" && p6_token_juzgar_estrato(f39) == "cubierto",
     sprintf("MC39, sonda directa (L-C7, «o que pase de su cifra»): %d reglas de la lista bajo el tope de su estrato; %s · %s %s a %+.1f pp (firmada %+.1f, tope %+.1f): pendiente con la regla por regla, cubierta con la de estrato",
             nrow(mitad), mitad$estrato[1], mitad$id[1], mitad$papel[1], mitad$firmado_pp[1] + 1, mitad$firmado_pp[1], FIRMA_P6_TOKEN[[mitad$estrato[1]]]))
  m39 <- mutar(list(c("Si el grado que ganó hubiera tenido el mismo promedio por curso que el que perdió, conservando su",
                      "Si el grado que ganó hubiera tenido el mismo promedio por curso que el que perdió hubiera tenido, conservando su")))
  if (!is.null(m39$error)) { falla("MC39 MAL CONSTRUIDO: ", m39$error); filas_mut[["MC39"]] <<- c("MC39", "L-C7", "-", "mal_construido", NA); return(invisible()) }
  HM <- h2_exacta(m39$txt, 1L)
  llego <- !is.null(HM$A) && HM$masa_err == 0 && any(grepl("que perdió hubiera tenido, conservando", HM$A$c6_txt, fixed = TRUE))
  VM <- if (llego) p6_token_tabla(HM$A) else NULL
  if (llego) { VM$veredicto <- p6_token_juzgar(VM); VM$v_estrato <- p6_token_juzgar_estrato(VM) }
  j39 <- if (llego) VM[grepl("«perdió hubiera tenido»", VM$id, fixed = TRUE) & clave_fila(VM) %in% clave_fila(FIRMA_P6_PALABRAS) &
                       VM$veredicto == "pendiente" & VM$v_estrato == "cubierto", , drop = FALSE] else VT7[0, ]
  ## H2 de la FASE 2C r4 (tanda C1b, 2026-11-03): línea base en la misma medida (h2_exacta sin mutar, con la Parte 1 de la
  ## semilla 1), donde las filas de j39 tienen que salir «cubierto»; y el veredicto cuenta los pendientes que el mutante abre
  ## fuera de j39 (los que ya lo eran en la línea base no cuentan), como juzgar_s1() del SCHOICE desde la S2b. El detractor
  ## r4 dedujo de los textos 6 n-gramas fuera de la lista («tenido conservando»… y «perdió conservando»…): el veredicto
  ## esperado es cazado_por_su_sonda_y_otra.
  HB <- h2_exacta(COD_C, 1L)
  VB <- if (is.null(HB$A)) NULL else p6_token_tabla(HB$A); if (!is.null(VB)) VB$veredicto <- p6_token_juzgar(VB)
  b39 <- if (is.null(VB)) VT7[0, ] else VB[clave_fila(VB) %in% clave_fila(j39), , drop = FALSE]
  base_ok <- nrow(j39) > 0L && nrow(b39) == nrow(j39) && all(b39$veredicto == "cubierto")
  pend_b <- if (is.null(VB)) character(0) else clave_fila(VB)[VB$veredicto == "pendiente"]
  o39 <- if (llego) VM[VM$veredicto == "pendiente" & !clave_fila(VM) %in% c(clave_fila(j39), pend_b), , drop = FALSE] else VT7[0, ]
  ver <- if (!llego || (nrow(j39) && !base_ok)) "mal_construido" else if (nrow(j39)) (if (nrow(o39)) "cazado_por_su_sonda_y_otra" else "cazado_por_su_sonda") else
    if (any(VM$veredicto == "pendiente")) "cazado_por_otra" else "no_detectado"
  bx <- function(k, campo) { j <- which(clave_fila(b39) == k); if (!length(j)) return(if (campo == "v") "sin fila" else NA_real_)
    if (campo == "v") b39$veredicto[j[1]] else 100 * b39$exceso[j[1]] }
  d <- if (nrow(j39)) paste(sprintf("%s · %s %s %+.1f pp (firmada %+.1f, tope %+.1f; línea base %+.1f, «%s») -> pendiente con la regla por regla; cubierta con la de estrato",
                                    j39$estrato, j39$id, j39$papel, 100 * j39$exceso,
                                    vapply(seq_len(nrow(j39)), function(i) firma_p6_f(j39$estrato[i], j39$id[i], j39$papel[i]), 0),
                                    unname(FIRMA_P6_TOKEN[j39$estrato]),
                                    vapply(clave_fila(j39), bx, 0, campo = "x"), vapply(clave_fila(j39), bx, "", campo = "v")), collapse = "; ") else
    sprintf("ninguna regla «perdió hubiera tenido» de la lista quedó pendiente y cubierta por estrato (masa con error %.3f)", if (is.null(HM$A)) NA else HM$masa_err)
  d <- paste0(d, sprintf(" · pendientes nuevos fuera de j39: %d%s (la línea base, sin mutar, tiene %d pendientes)", nrow(o39),
                         if (nrow(o39)) paste0(": ", paste(head(unique(o39$id), 8), collapse = ", ")) else "", length(pend_b)))
  ok(ver == "cazado_por_su_sonda_y_otra", sprintf("MC39 (L-C7, «o que pase de su cifra»: BP_gano gana «perdió hubiera tenido»): %s — %s (esperado: cazado_por_su_sonda_y_otra)", d, ver))
  filas_mut[["MC39"]] <<- c("MC39", "L-C7: regla de la lista por encima de su cifra, bajo el tope de su estrato", d, ver, NA) })
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
## MC28 (tanda 7, L-C4 v2): las tres claves «promedio» ganan una palabra que ningún distractor tiene. La sonda de vocabulario
## exclusivo de la clave (P2-VX), calculada sobre los textos, tiene que ponerse en rojo con la misma medida exacta.
local({ mm <- mutar(list(
    c('"Lo que recogió ese grado dividido en partes iguales entre sus cursos."', '"Lo que recogió ese grado dividido en partes iguales entre sus cursos, precisamente."'),
    c('"Lo que recogió ese grado dividido entre el número de sus cursos."', '"Lo que recogió ese grado dividido entre el número de sus cursos, precisamente."'),
    c('"Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado."', '"Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado, precisamente."')))
  if (!is.null(mm$error)) { falla("MC28 MAL CONSTRUIDO: ", mm$error); filas_mut[["MC28"]] <<- c("MC28", "P2-VX", "-", "mal_construido", NA); return(invisible()) }
  en <- NULL; for (z in 1:60) { e <- correr(mm$expr, z); if (is.null(e$.err) && e$tipo_p2 == "promedio") { en <- e; break } }
  llego <- !is.null(en) && all(grepl("precisamente", en$P2$promedio$clave, fixed = TRUE)) && !any(grepl("precisamente", en$P2$promedio$distractores, fixed = TRUE))
  M <- if (llego) EB$medir_p2_exacta(en$P2$promedio, "promedio", EST_TIPO(en$P2$promedio)[1:10]) else NULL
  rojo <- if (llego) juzgar(M, "", informar = FALSE) else character(0)
  ver <- if (!llego) "mal_construido" else if ("P2-VX" %in% rojo) "cazado_por_su_sonda" else if (length(rojo)) "cazado_por_otra" else "no_detectado"
  d <- if (llego) sprintf("P2-VX %+.1f pp; P2-V «precisamente» %+.1f pp", 100 * M$exceso[M$id == "P2-VX"], 100 * M$exceso[M$id == "P2-V «precisamente»"]) else "-"
  ok(ver == "cazado_por_su_sonda", sprintf("MC28 (L-C4 v2: las tres claves «promedio» ganan «precisamente»): %s — %s", d, ver))
  filas_mut[["MC28"]] <<- c("MC28", "L-C4 v2: P2-VX en rojo", d, ver, NA) })
## MC31 a MC35 (L-C6, tanda 8): un mutante por regla firmada de la Parte 2. Cada uno tiene que sacar SU regla de «cubierto»,
## medida con la misma medida exacta (medir_p2_exacta) sobre 10 portadores de la muestra estándar; se informa cuántas reglas
## salen con él. La materialización se comprueba en el banco del entorno mutado, no en el texto.
K_P2 <- c('"Lo que recogió ese grado dividido en partes iguales entre sus cursos."', '"Lo que recogió ese grado dividido entre el número de sus cursos."',
          '"Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado."')
D_P2 <- c('"Lo que recogió ese grado dividido entre el promedio de sus cursos."',
          '"Lo que recogió ese grado dividido en partes iguales entre sus cursos y luego entre dos."',
          '"Lo que recogió ese grado dividido entre el número de sus cursos al cuadrado."',
          '"Lo que recogió ese grado dividido entre la cantidad de cursos de ambos grados."',
          '"Lo que recogió la mayoría de los cursos de ese mismo grado."')
con_p2 <- function(s, extra) sub('\\."$', paste0(extra, '."'), s)
PARRAFO_MC34 <- paste(" según las partes que se refieren a otro colegio donde el concurso se hizo entre dos grados y ganaba el que recogiera más",
                      "la tabla muestra la cantidad de cursos que participaron y el promedio recogido por curso en el grado")
MUT_P2 <- list(
  list(id = "MC31", regla = "P2-R2", desc = "las tres claves repiten palabras de todos los distractores",
       c = lapply(K_P2, function(s) c(s, con_p2(s, " ni entre el promedio ni entre dos ni al cuadrado ni entre ambos grados ni la mayoría de los cursos"))),
       llego = function(q) all(grepl("ni la mayoría de los cursos.", q$clave, fixed = TRUE))),
  ## «y luego entre dos» a secas volvía la clave 1 idéntica al distractor «luego_dos» y la guarda «una sola clave» abortaba
  ## (mutante mal construido, r8 del 2026-10-31): con «veces», ninguna clave repite un distractor (0 errores en 300 semillas).
  list(id = "MC32", regla = "P2-R1", desc = "las tres claves terminan en «y luego entre dos veces», casi como un distractor",
       c = lapply(K_P2, function(s) c(s, con_p2(s, " y luego entre dos veces"))), llego = function(q) all(endsWith(q$clave, "y luego entre dos veces."))),
  list(id = "MC33", regla = "P2-E8c", desc = "cada distractor lleva tres palabras del texto visible que ninguna otra opción tiene",
       c = unname(Map(function(s, w) c(s, paste0('"Lo que recogió ese grado dividido entre sus cursos ', w, '."')), D_P2,
                      c("conservando participado habría", "faltado perdido ganado", "afirmaciones verdaderas puede", "refieren colegio donde", "ganaba recogiera muestra"))),
       llego = function(q) all(grepl("^Lo que recogió ese grado dividido entre sus cursos [a-zíá]+ [a-zíá]+ [a-zíá]+\\.$", q$distractores))),
  list(id = "MC34", regla = "P2-E5c", desc = "los cinco distractores repiten el párrafo del otro colegio",
       c = lapply(D_P2, function(s) c(s, con_p2(s, PARRAFO_MC34))), llego = function(q) all(grepl("ganaba el que recogiera", q$distractores, fixed = TRUE))),
  list(id = "MC35", regla = "P2-M3c", desc = "las claves 1 y 2 terminan como la clave 3 y la moda («de ese mismo grado»)",
       c = lapply(K_P2[1:2], function(s) c(s, con_p2(s, " de ese mismo grado"))), llego = function(q) sum(endsWith(q$clave, "de ese mismo grado.")) == 3L))
for (mx in MUT_P2) {
  mm <- mutar(mx$c)
  if (!is.null(mm$error)) { falla(mx$id, " MAL CONSTRUIDO: ", mm$error); filas_mut[[mx$id]] <- c(mx$id, "L-C6", "-", "mal_construido", NA); next }
  en <- correr(mm$expr, 1L); q <- en$P2$promedio
  llego <- is.null(en$.err) && !is.null(q) && isTRUE(mx$llego(q))
  M <- if (llego) EB$medir_p2_exacta(q, "promedio", EST_TIPO(q)[1:10]) else NULL
  vp <- if (llego) veredicto_p2(M) else character(0); pend <- if (llego) M$id[vp == "pendiente"] else character(0)
  ver <- if (!llego) "mal_construido" else if (mx$regla %in% pend) "cazado_por_su_sonda" else if (length(pend)) "cazado_por_otra" else "no_detectado"
  d <- if (llego) sprintf("%s %+.1f pp (firmado %+.1f) -> %s; reglas pendientes con él: %d", mx$regla, 100 * M$exceso[M$id == mx$regla], FIRMA_P2[[mx$regla]],
                          vp[M$id == mx$regla], length(pend)) else if (!is.null(en$.err)) paste("error:", en$.err) else "-"
  ok(ver == "cazado_por_su_sonda", sprintf("%s (L-C6: %s): %s — %s", mx$id, mx$desc, d, ver))
  filas_mut[[mx$id]] <- c(mx$id, sprintf("L-C6: %s fuera de «cubierto»", mx$regla), d, ver, NA)
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
mut_p1("MP2", "regla", "mas digitos", "recogió 40\u00a0kilos más", "recogió 4\u00a0kilos más")   # C1: con el espacio duro de L-6

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
txt44 <- readLines(RMD_C, encoding = "UTF-8"); i44 <- which(txt44 == 'ED <- "\\u00a0"')
if (length(i44) < 1L) { falla("MC44 MAL CONSTRUIDO: no hay ninguna línea ED <- \"\\u00a0\" en el .Rmd"); filas_mut[["MC44"]] <- c("MC44", "C1 · L-6: sin literales", "-", "mal_construido", NA) } else {
  m44 <- txt44; m44[i44[1]] <- sub("\\u00a0", "\u00a0", m44[i44[1]], fixed = TRUE)
  llego44 <- !identical(m44, txt44) && identical(eval(parse(text = m44[i44[1]], encoding = "UTF-8"), envir = new.env()), "\u00a0")
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
## MC49: el oráculo de la Parte 1 (E2) sin normalizar el espacio duro. La sede que lee de la tabla (sus celdas no llevan
## U+00A0) deja de casar con el sujeto de la opción cuando el nombre tiene dos palabras («El Bosque», «La Pradera»).
ok49 <- vapply(TODAS, or_p1, NA, nf = identity); dos <- vapply(TODAS, function(x) grepl(" ", tabla_leida(x$tabla_md)$nombre[1], fixed = TRUE), NA)
ver49 <- if (!all(vapply(TODAS, or_p1, NA))) "mal_construido" else if (all(ok49)) "no_detectado" else if (identical(unname(!ok49), unname(dos))) "cazado_por_su_sonda" else "cazado_por_otra"
d49 <- sprintf("sin normalizar, el oráculo acierta %d de %d versiones; fallan %d, y las versiones con nombres de dos palabras son %d", sum(ok49), length(ok49), sum(!ok49), sum(dos))
ok(ver49 == "cazado_por_su_sonda", "MC49 (E2 p1 sin normalizar el espacio duro): ", d49, " — ", ver49)
filas_mut[["MC49"]] <- c("MC49", "C1: E2 p1 sin normalizar", d49, ver49, NA)
## MC50, MC51, MC53 y MC54 (C1b): H1-Sol, sobre el chunk «solucion» (en memoria, patrón único). MC50, MC53 y MC54 son
## ADITIVOS: el control exacto no se mueve. MC51 vuelve a la redacción de 27f789dc, que deja una sola comparación con dos
## cifras por versión: muere por H1-Sol y también por el control, y se declara así. El párrafo viejo y el nuevo, literales.
## H3 de la FASE 2C r5 (tanda C1c): el veredicto sigue la convención de mut_l6 (MC45 a MC48). cazado_por_su_sonda, si su
## componente es la única afectada; cazado_por_su_sonda_y_otra, si hay otras; _y_control, si además se mueve el control exacto.
## MC50 mueve falsa y concl; MC54, cifra y falsa; MC51, falsa, marca y el control; MC53, solo esp. La componente «marca» no cae
## sola con ningún mutante de texto: la prueba MC55, sobre una copia de los entornos.
P6_NUEVO <- r"---(cat("Parte 6. Cada hipótesis cambia un solo dato de la tabla. Un curso más con el mismo promedio suma ese ",
    "promedio al total, y un curso menos lo resta. Por eso, para saber si el grado que perdió habría ganado con un ",
    "curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la ",
    "diferencia entre los totales: si el promedio es mayor, sí; si es menor, no. ",
    "Con un curso más, el grado ", per_par,
    " habría sumado a su total ", dp$aP, ED, "kilos, ", if (hp_supera) "más" else "menos", " que la diferencia de ",
    dp$D, ED, "kilos: ", if (hp_supera) "habría ganado" else "no le habría alcanzado para ganar",
    ". Con un curso menos, el grado ", gan_par, " habría restado a su total ", dp$aG, ED, "kilos, ",
    if (hg_supera) "más" else "menos", " que la diferencia de ", dp$D, ED, "kilos: ",
    if (hg_supera) paste0("habría", ED, "perdido") else paste0("habría seguido", ED, "ganando"), ".\n\n", sep = ""))---"
P6_VIEJO <- r"---(cat("Parte 6. Cada hipótesis cambia un solo dato de la tabla. Un curso más con el mismo promedio suma ese ",
    "promedio al total, y un curso menos lo resta. Por eso el grado ", per_par, " habría ganado con un curso más ",
    "si y solo si su promedio por curso, ", dp$aP, ED, "kilos, es mayor que la diferencia, ", dp$D, ED, "kilos; y el grado ",
    gan_par, " habría perdido con un curso menos si y solo si su promedio, ", dp$aG, ED, "kilos, es mayor que esa ",
    "diferencia.\n\n", sep = ""))---"
juzga_h1 <- function(id, R, comp, desc, esperado) {
  dif <- R$z[COMP_H1] - H1S$z[COMP_H1]; hits <- names(dif)[dif != 0]
  ctrl <- R$z[["n_cmp"]] != 2 * nv || R$z[["n_mas"]] != nv || R$z[["n_menos"]] != nv || R$z[["sin_parrafo"]] > 0
  ver <- if (R$z[["err"]] > 0) "cazado_por_otra" else if (!comp %in% hits) (if (length(hits) || ctrl) "cazado_por_otra" else "no_detectado") else
    paste0(if (identical(hits, comp)) "cazado_por_su_sonda" else "cazado_por_su_sonda_y_otra", if (ctrl) "_y_control")
  d <- sprintf("%s; control: %d comparaciones (%d «un curso más», %d «un curso menos»; esperadas %d, %d y %d)",
               if (length(hits)) paste(sprintf("%s %+d", hits, as.integer(dif[hits])), collapse = ", ") else "ninguna componente",
               as.integer(R$z[["n_cmp"]]), as.integer(R$z[["n_mas"]]), as.integer(R$z[["n_menos"]]), 2L * nv, nv, nv)
  ok(ver == esperado, sprintf("%s (%s): %s — %s (esperado: %s)", id, desc, d, ver, esperado))
  filas_mut[[id]] <<- c(id, paste("C1b · H1-Sol:", comp), d, ver, NA)
}
mut_h1 <- function(id, de, a, comp, desc, esperado = "cazado_por_su_sonda") {
  n <- lengths(regmatches(SOL_C, gregexpr(de, SOL_C, fixed = TRUE)))
  if (n != 1L) { falla(id, " MAL CONSTRUIDO: el patrón aparece ", n, " veces en el chunk solucion"); filas_mut[[id]] <<- c(id, paste("C1b · H1-Sol:", comp), "-", "mal_construido", NA); return(invisible()) }
  juzga_h1(id, h1_p6(sub(de, a, SOL_C, fixed = TRUE)), comp, desc, esperado)
}
mut_h1("MC50", 'if (hp_supera) "más" else "menos", " que la diferencia de ",', 'if (hp_supera) "menos" else "menos", " que la diferencia de ",',
       "falsa", "«un curso más» dice «menos» también cuando el promedio supera la diferencia", esperado = "cazado_por_su_sonda_y_otra")
mut_h1("MC51", P6_NUEVO, P6_VIEJO, "falsa", "vuelve a la redacción de 27f789dc", esperado = "cazado_por_su_sonda_y_otra_y_control")
mut_h1("MC53", 'dp$D, ED, "kilos: ", if (hp_supera)', 'dp$D, " kilos: ", if (hp_supera)', "esp",
       "«un curso más»: espacio normal entre la diferencia y «kilos»")
mut_h1("MC54", '" habría sumado a su total ", dp$aP, ED,', '" habría sumado a su total ", dp$aG, ED,', "cifra",
       "«un curso más» cita el promedio del grado que ganó", esperado = "cazado_por_su_sonda_y_otra")
## MC55 (C1c, H3 de la r5): mutante del ENTORNO, no del texto. Voltea la marca de HP en una COPIA de los entornos: la Solution
## emite el mismo párrafo y solo puede caer «marca». Las guardas miran las copias (obligación 3 del contrato de mutación): el
## volteo llegó, a una sola entrada por versión, y los entornos originales siguen intactos.
SOL6_0 <- lapply(ENVS_NAT, function(en) en$sol_p6)
E55 <- lapply(ENVS_NAT, function(en) { e <- list2env(as.list(en, all.names = TRUE), parent = parent.env(en))
  i <- e$rol_p6 == "hp"; e$sol_p6[i] <- !e$sol_p6[i]; e })
llego55 <- all(mapply(function(o, m) { i <- o$rol_p6 == "hp"
  sum(i) == 1L && !identical(o, m) && isTRUE(m$sol_p6[i] == !o$sol_p6[i]) && identical(m$sol_p6[!i], o$sol_p6[!i]) && identical(m$rol_p6, o$rol_p6) }, ENVS_NAT, E55)) &&
  identical(SOL6_0, lapply(ENVS_NAT, function(en) en$sol_p6))
if (!llego55) { falla("MC55 MAL CONSTRUIDO: el volteo de la marca de HP no llegó a la copia o tocó los entornos originales")
  filas_mut[["MC55"]] <- c("MC55", "C1b · H1-Sol: marca", "-", "mal_construido", NA)
} else juzga_h1("MC55", h1_p6(SOL_C, envs = E55), "marca", "la marca de HP, volteada en una copia de los entornos", "cazado_por_su_sonda")
## MC56 y MC57 (C1c, H1 de la r5): Crit-P6, sobre el chunk «solucion» (en memoria, patrón único). Cada uno declara su componente,
## y el veredicto exige además que H1-Sol y su control no se muevan: MC56 toca solo el criterio, no las comparaciones con cifras.
P6_CRIT_VIEJO <- "Por eso, si el grado que perdió tiene un curso más o el que ganó tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es mayor, el ganador cambia; si es menor, no."
mut_crit <- function(id, de, a, comp, desc, lit_mut = NULL) {
  et <- paste("C1c · Crit-P6:", comp); n <- lengths(regmatches(SOL_C, gregexpr(de, SOL_C, fixed = TRUE)))
  if (n != 1L) { falla(id, " MAL CONSTRUIDO: el patrón aparece ", n, " veces en el chunk solucion"); filas_mut[[id]] <<- c(id, et, "-", "mal_construido", NA); return(invisible()) }
  txt <- sub(de, a, SOL_C, fixed = TRUE)
  if (!is.null(lit_mut) && crit_p6(txt, lit = lit_mut)[["n_crit"]] != nv) { falla(id, " MAL CONSTRUIDO: el texto mutado no emite lo que el mutante declara"); filas_mut[[id]] <<- c(id, et, "-", "mal_construido", NA); return(invisible()) }
  C <- crit_p6(txt); R <- h1_p6(txt); dif <- C[COMP_CRIT] - CRIT[COMP_CRIT]; hits <- names(dif)[dif != 0]
  cmp_h1 <- c(COMP_H1, "n_cmp", "n_mas", "n_menos", "sin_parrafo", "err"); h1_mov <- !identical(R$z[cmp_h1], H1S$z[cmp_h1])
  ver <- if (C[["err"]] > 0) "cazado_por_otra" else if (!comp %in% hits) (if (length(hits) || h1_mov) "cazado_por_otra" else "no_detectado") else
    if (identical(hits, comp) && !h1_mov) "cazado_por_su_sonda" else "cazado_por_su_sonda_y_otra"
  d <- sprintf("%s; control: %d apariciones del literal (sin mutar %d); H1-Sol %s",
               if (length(hits)) paste(sprintf("%s %+d", hits, as.integer(dif[hits])), collapse = ", ") else "ninguna componente",
               as.integer(C[["n_crit"]]), as.integer(CRIT[["n_crit"]]), if (h1_mov) "se mueve" else "no se mueve")
  ok(ver == "cazado_por_su_sonda", sprintf("%s (%s): %s — %s", id, desc, d, ver))
  filas_mut[[id]] <<- c(id, et, d, ver, NA)
}
mut_crit("MC56", r"---(Por eso, para saber si el grado que perdió habría ganado con un ",
    "curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la ",
    "diferencia entre los totales: si el promedio es mayor, sí; si es menor, no. ",)---",
  r"---(Por eso, si el grado que perdió tiene un curso más o el que ganó ",
    "tiene uno menos, basta comparar el promedio de ese grado con la diferencia entre los totales: si el promedio es ",
    "mayor, el ganador cambia; si es menor, no. ",)---",
  "falta", "vuelve a la frase de 32243d0a, sin tocar las comparaciones", lit_mut = P6_CRIT_VIEJO)
mut_crit("MC57", '"promedio al total, y un curso menos lo resta. Por eso, para saber', '"promedio al total, y un curso menos lo resta (", dp$D, "). Por eso, para saber',
         "cifra", "una cifra de la versión en el tramo general, con el literal intacto")
cat("  Decisión 2 a: las nueve reglas y sondas al 100 % (+75,0 pp) no pueden subir; ningún mutante puede sacarlas de «cubierto» y se declara.\n")
TM <- do.call(rbind, filas_mut); colnames(TM) <- c("id", "sonda_esperada", "sonda_real", "veredicto", "semilla")
cat("\n"); print(TM[TM[, "id"] %in% c("MC19", "MC20", "MC23", "MC24", "MC25", "MC26", "MC27", "MC28", "MC29", "MC31", "MC32", "MC33", "MC34", "MC35", "MC36", "MC37", "MC38", "MC39", "MP1", "MP2", "MC40", "MC41", "MC42", "MC43", "MC44", "MC45", "MC46", "MC47", "MC48", "MC49", "MC50", "MC51", "MC52", "MC53", "MC54", "MC55", "MC56", "MC57"), c("id", "veredicto", "sonda_esperada", "sonda_real")], quote = FALSE)

## ---- Resultado ----------------------------------------------------------------------------------
titulo("RESULTADO")
if (length(FIRMADOS)) cat("En rojo, cubiertos por un override firmado («O-1 corregido» extendido por F-2; X6, X9, X10 y X11 en la canónica por L-C3; la canónica de la Parte 1 regla por regla por la decisión 2 a; H2 en la Parte 6 por «H2 a» (L-C5); el residuo de la Parte 2 «promedio» (L-C6) y la regla por palabra de la Parte 6 firmada regla por regla (L-C7) por la decisión «Va»; la firma de la S2d (41 cifras), extendida por alcance a la Parte 1 (decisión 3, tanda C1); regla #22 §P7-E):\n", paste0("  - ", FIRMADOS, "\n"), sep = "")
if (length(RESIDUOS)) cat("\nResiduos declarados:\n", paste0("  - ", RESIDUOS, "\n"), sep = "")
if (length(PENDIENTES)) cat("\nPendientes de firma del profesor (exit 3):\n", paste0("  - ", PENDIENTES, "\n"), sep = "")
cat("\n=== RESULTADO:", if (length(fallos)) sprintf("FALLA (%d)", length(fallos)) else
  if (length(PENDIENTES)) sprintf("PENDIENTE DE FIRMA (%d; 0 fallos)", length(PENDIENTES)) else
  if (length(FIRMADOS)) sprintf("EN ROJO BAJO OVERRIDE FIRMADO (%d cifras dentro de lo firmado: «O-1 corregido», X6, X9, X10 y X11 en la canónica, la canónica de la Parte 1 por la decisión 2 a H2 por «H2 a», y la Parte 2 y la regla por palabra de la Parte 6 firmada regla por regla por la decisión «Va», y la firma de la S2d extendida a la Parte 1; 0 fallos, 0 pendientes)", length(FIRMADOS)) else
  "APROBADO (0 fallos)", "===\n")
quit(status = if (length(fallos)) 1L else if (length(PENDIENTES)) 3L else if (length(FIRMADOS)) 4L else 0L)
