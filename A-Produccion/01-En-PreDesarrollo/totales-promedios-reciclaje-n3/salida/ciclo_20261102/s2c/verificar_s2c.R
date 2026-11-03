#!/usr/bin/env Rscript
## =============================================================================
## verificar_espacio.R — verificador propio de totales-promedios-reciclaje-n3
## =============================================================================
## L-2.4: enumera TODO el espacio de parámetros de cada rama (es finito) y comprueba que,
##        con cualquier elección de moldes y cifras del generador, la clave es la única
##        afirmación verdadera.
## L-2.5 / Incidente P: mutantes en memoria sobre el chunk data_generation (no tocan el
##        .Rmd); cada uno declara su sonda, el patrón que muta aparece una sola vez, y se
##        recorren semillas hasta la primera muerte (tope 600).
## Regla #23: N = 100 por estrato (mezcla natural, rama CURSOS, rama PROMEDIO); la
##        canónica se mide aparte por enumeración exacta (24 órdenes).
## Diagnosticidad: batería §P7 congelada al inicio (34 reglas de referencia + 14 del ítem)
##        y sondas que ven la tabla (la batería solo ve las opciones).
## Ronda 2 (tras la FASE 2C, ronda 1): H1 (cifra sin sustento fuera de los cálculos, M13), H5
##        (umbral de S1-S6, cota estructural de S1, margen en palabras de «molde atipico», M15),
##        O-1 (S_U, S_todos), L-1 (un solo verbo, M14), H6 (calcula() fuera de contexto) y M11 rehecho.
## Ronda 3 (variante C): cierre «su total fue más alto que el de O en x kilos», ERR-TOTAL repuesto,
##        lecturas del texto (regla #13 v1.1: enumeración de textos, oráculo léxico propio, exposición
##        a la lectura «total», canónica), mutantes M16-M18 y cuotas del distractor de G por rama.
## Ronda 4 (FASE 2C r4): el alcance firmado de «O-1 corregido» como tabla de cifras (H1, mutante M19)
##        y la canónica cotejada también sobre la tabla mostrada, tabla_md (H3, mutante M20).
## Ronda 4b (FASE 2C r4b, H1): guarda estructural de los encabezados de la Solution (mutantes M21, M22).
## Ciclo 2026-11-02, tanda S1b: la unidad que cierra una fórmula va DENTRO de ella («= T\\ \\text{kilos}$»): sonda
##        S1-Sol fórmula, guarda «L-6: la unidad que cierra una fórmula…» (M30) y M31, que revierte la variante.
## Tanda S1b relanzada (2026-11-03): V1', el signo que sigue a la unidad entra en la fórmula («\\text{kilos.}$»):
##        sonda S1-Sol signo (M32), guarda «L-6: el signo que sigue a la unidad…» (M33) y M30 adaptado. S7 cerrado: la
##        canónica ya no asigna lado_gdis y rasgos() calcula su lado de las cifras.
## Tanda S2a (2026-11-03), FASE 2C r5. H2, decisión del profesor «Cerrarlo ahora»: la sonda S1-Sol signo cubre toda
##        fórmula, con unidad o sin ella (M37 para la guarda del .Rmd; M38 para la sonda). H1: controles positivos
##        exactos (1158, 730 y el signo dentro de una fórmula sin unidad, uno por versión con ERR-RESTA), cada uno con un
##        mutante que quita un solo sitio (M34, M35, M36), y separadores ampliados a todo carácter que no sea letra ni
##        cifra (M39 y M41, con U+2009, U+202F y U+2060). H5: guarda real del canal 2 sobre lado_gdis (M40); fuera el
##        ok() tautológico de S7.
## Tanda S2b (2026-11-03), FASE 2C r6. H1: el canal «resta» (la opción que cita |aG - aP| delata a la sede que gana) y sus
##        combinaciones con las pistas de «O-1 corregido», por estrato y en la canónica exacta; y el barrido de las
##        operaciones de un paso entre dos celdas de la tabla (§P7, cierre por familias; §P7-G). Ninguna está firmada:
##        pendientes de firma (exit 3). M44 fuerza ERR-RESTA; M19 mueve también el barrido. ESTIMULO_SC dice qué mide E4.
##        H2: juzgar_s1() simétrico («cazado_por_su_sonda_y_control») y M42 y M43, aditivos. M28 juzga solo los
##        pendientes nuevos del proceso sin normalizar.
## Ciclo 2026-11-02, tanda S1: espacio duro (S1) en el oráculo léxico, en M16 y en la Solution (sonda S1-Sol,
##        mutantes M23-M27); MOLDES sin el parámetro m (S7); la canónica regla por regla con las 24 cifras de
##        la decisión 2 a y sus mutantes MP1 y MP2 (S3); el estímulo de la batería, declarado (§P7-G, S4).
##        Decisión (a) del profesor: la batería y los márgenes miden con U+00A0 convertido en espacio (mutante M28).
## Uso: Rscript --no-init-file verificar_espacio.R [ruta.Rmd]
##      exit 1 = algo falla; 3 = pendiente de firma (un canal supera lo firmado o es nuevo); 4 = EN ROJO
##      bajo override firmado (todo dentro del alcance de «O-1 corregido» y de la decisión 2 a); 0 = sin canales en rojo.
##      Regla #22 §P7-E: un canal aceptado por override sigue en rojo; nunca se reporta como PASS.
##      VERIF_N=<n> solo para depuración (regla #23): corre E1-E4 hasta las sondas y sale; la cifra
##      oficial es la de N = 100, el default.
## =============================================================================
suppressMessages(library(exams))
args <- commandArgs(trailingOnly = TRUE)
arg_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
aqui <- if (length(arg_file)) dirname(normalizePath(sub("^--file=", "", arg_file[1]))) else getwd()
rmd <- if (length(args)) args[1] else
  file.path(aqui, "totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd")
REPO <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
N <- as.integer(Sys.getenv("VERIF_N", "100")); TOPE <- max(600L, 6L * N)
DEPURACION <- N != 100L
## Decisión (a) del profesor (2026-11-02, tanda S1): la batería §P7 y los márgenes de largo (caracteres y palabras)
## miden el texto que lee el estudiante, con el espacio duro (U+00A0) convertido en espacio. ntok() de
## bateria_referencia_icfes.R cuenta palabras con \\s+, que en R no separa en U+00A0, y subía el margen en palabras
## de la rama PROMEDIO de 6,5 % a 6,7 % sin que cambiara ninguna palabra (HANDOFF_REPO, pendiente 11). Las guardas
## S1 y S1-Sol y los mutantes M23-M27 miran el texto crudo, sin normalizar. M28 corre este mismo verificador sin la
## normalización (VERIF_SIN_NORMALIZAR=1): el margen vuelve a 6,7 % y queda pendiente. Rige también para la Parte 1
## del CLOZE tras el re-empalme (F-2); lo aplica la tanda del CLOZE.
NORMALIZAR_ED <- Sys.getenv("VERIF_SIN_NORMALIZAR") != "1"
norm_ed <- function(o) if (NORMALIZAR_ED) gsub("\u00a0", " ", o, fixed = TRUE) else o
l <- readLines(rmd, encoding = "UTF-8")
i0 <- grep("^```\\{r data_generation", l); cierres <- which(l == "```"); i1 <- cierres[cierres > i0][1]
CODIGO <- paste(l[(i0 + 1):(i1 - 1)], collapse = "\n")
## ---- S2c: mutación del candidato, en memoria (no toca el .Rmd) ----
S2C_ID <- Sys.getenv("S2C_ID", "B0"); S2C_CAND <- Sys.getenv("S2C_CAND", ""); S2C_DIR <- Sys.getenv("S2C_DIR", ".")
reemplazar1 <- function(txt, viejo, nuevo) { pos <- gregexpr(viejo, txt, fixed = TRUE)[[1]]
  if (length(pos) != 1L || pos[1] < 0L) stop(sprintf("S2c: el patrón «%s» aparece %d veces (se exige 1)",
    substr(viejo, 1, 60), if (pos[1] < 0L) 0L else length(pos)))
  paste0(substr(txt, 1L, pos[1] - 1L), nuevo, substr(txt, pos[1] + nchar(viejo), nchar(txt))) }
if (nzchar(S2C_CAND)) { source(S2C_CAND, local = TRUE, encoding = "UTF-8")
  for (cb in CAMBIOS) CODIGO <- reemplazar1(CODIGO, cb[1], cb[2])
  cat(sprintf("S2c: candidato %s, %d cambios aplicados en memoria (patrón único cada uno)\n", S2C_ID, length(CAMBIOS))) }
writeLines(CODIGO, file.path(S2C_DIR, sprintf("codigo_%s.R", S2C_ID)), useBytes = TRUE)
EXPR <- parse(text = CODIGO, encoding = "UTF-8")
fallos <- character(0)
falla <- function(msg) { fallos <<- c(fallos, msg); cat("  FALLA:", msg, "\n") }
ok <- function(cond, msg) { if (isTRUE(cond)) cat("  OK:", msg, "\n") else falla(msg) }
correr <- function(seed, expr = EXPR) {
  env <- new.env(); set.seed(seed)
  err <- tryCatch({ eval(expr, envir = env); NULL }, error = function(e) conditionMessage(e))
  list(env = env, err = err)
}
cif <- function(s) { v <- regmatches(s, gregexpr("[0-9]+", s))[[1]]; if (length(v)) as.numeric(v[1]) else NA_real_ }
## S7 (tanda S1b relanzada, decisión del profesor del 2026-11-03): la canónica ya no asigna lado_gdis, que no usaba.
## H5 de la FASE 2C r5 (tanda S2a): lado es el lado SORTEADO para la cifra falsa de G (lado_gdis), y lo consume la
## guarda real del canal 2 (E2). La canónica no sortea lado: NA, y la guarda la deja fuera.
rasgos <- function(env) with(env, list(
  canon = es_canonica, rama = rama, opciones = opciones, clave = which(sol), rol = rol_opcion,
  D = d$D, X = afs$gdis$x, cod_gdis = afs$gdis$codigo, molde_gdis = afs$gdis$molde,
  molde_clave = afs$clave$molde, molde_padv = afs$padv$molde, molde_psin = afs$psin$molde,
  sede_o = vapply(afs[rol_opcion], `[[`, "", "sede"), nom_o = ifelse(vapply(afs[rol_opcion], `[[`, "", "sede") == "G",
  nomG, nomP), fila1 = conj$nombres[1], tabla = c(d$nG, d$nP, d$aG, d$aP), gi = gi,
  lado = if (isTRUE(es_canonica)) NA_character_ else lado_gdis))
rasgos0 <- rasgos
rasgos <- function(env) c(rasgos0(env), list(
  s2c_fb = isTRUE(get0("s2c_fallback", envir = env, inherits = FALSE)),
  s2c_pd = isTRUE(get0("s2c_padv_delta", envir = env, inherits = FALSE)),
  s2c_sd = isTRUE(get0("s2c_psin_delta", envir = env, inherits = FALSE)), clave_txt = env$afs$clave$texto))

cat("=== verificar_espacio.R —", basename(rmd), "===\n")

## ---- E1. Mezcla natural, N = 100 (semillas 1..100) ---------------------------
cat("\n[E1] Mezcla natural, N = 100\n")
NAT <- list()
for (s in 1:N) { r <- correr(s); if (!is.null(r$err)) falla(paste("semilla", s, ":", r$err)) else NAT[[length(NAT) + 1]] <- rasgos(r$env) }
ok(length(NAT) == N, sprintf("%d/%d versiones sin error", length(NAT), N))
p_cursos <- mean(vapply(NAT, function(x) x$rama == "CURSOS", TRUE))
n_canon <- sum(vapply(NAT, `[[`, TRUE, "canon"))
cat(sprintf("  rama CURSOS (canónica incluida): %.0f %% · canónicas: %d/%d\n", 100 * p_cursos, n_canon, N))
ok(abs(p_cursos - 0.5) <= 0.12, "L-3: frecuencia global de la rama CURSOS cerca de 50 % (tolerancia ±12 pp a N = 100)")
uniq <- length(unique(vapply(NAT, function(x) paste(x$opciones, collapse = "|"), "")))
cat(sprintf("  versiones únicas (opciones): %d/%d\n", uniq, N))

## ---- E2. Cuotas por rama (no canónicas), N = 100 cada una; canónica aparte ----
cat("\n[E2] Cuotas por rama, N = 100 cada una (semillas 1001.., tope", TOPE, ")\n")
CU <- list(CURSOS = list(), PROMEDIO = list()); CANON <- NULL
for (s in 1000L + seq_len(TOPE)) {
  if (length(CU$CURSOS) >= N && length(CU$PROMEDIO) >= N && !is.null(CANON)) break
  r <- correr(s); if (!is.null(r$err)) { falla(paste("semilla", s, ":", r$err)); next }
  x <- rasgos(r$env)
  if (x$canon) { if (is.null(CANON)) CANON <- x; next }
  if (length(CU[[x$rama]]) < N) CU[[x$rama]][[length(CU[[x$rama]]) + 1]] <- x
}
S2C_E2_ULT <- s
for (b in names(CU)) ok(length(CU[[b]]) >= 20, sprintf("estrato %s: n = %d (n < 20 sería NO CONCLUYENTE)", b, length(CU[[b]])))
ok(!is.null(CANON), "se encontró la instancia canónica")
## H5 de la FASE 2C r5 (tanda S2a): el ok() de S7 era tautológico (el lado de la canónica salía de sus cifras, y la
## guarda verbatim ya fija x = 40 y D = 20). Lo sustituye una guarda real del canal 2 en las no canónicas de E1 y de los
## estratos: la cifra falsa de G cae del lado de D que se sorteó para ella (lado_gdis). M40 quita el filtro
## «cand_g <- cand_g[lado_ok, ]» y la tiene que ver esta guarda.
lado_cumple <- function(x) isTRUE(x$canon) || is.na(x$X) || isTRUE((x$X < x$D) == (x$lado == "debajo"))
VL <- Filter(function(x) !isTRUE(x$canon), c(NAT, CU$CURSOS, CU$PROMEDIO))
n_lado_mal <- sum(!vapply(c(NAT, CU$CURSOS, CU$PROMEDIO), lado_cumple, TRUE))
ok(n_lado_mal == 0L, sprintf("canal 2 (H5 r5): en las %d no canónicas de E1 y de los estratos, la cifra falsa de G cae del lado de D sorteado para ella (debajo en %d, encima en %d); fuera de su lado: %d",
                             length(VL), sum(vapply(VL, function(x) identical(x$lado, "debajo"), TRUE)),
                             sum(vapply(VL, function(x) identical(x$lado, "encima"), TRUE)), n_lado_mal))

## ---- E3. Enumeración exacta del espacio (L-2.4) --------------------------------
cat("\n[E3] Enumeración del espacio de parámetros\n")
env0 <- correr(1L)$env
esp_s2c <- function(b) { e <- env0$espacio_rama(b)
  if ("xP_delta" %in% names(formals(env0$espacio_rama))) { e2 <- env0$espacio_rama(b, xP_delta = TRUE)
    cc <- intersect(names(e), names(e2)); e <- unique(rbind(e[, cc], e2[, cc])) }
  rownames(e) <- NULL; e }
viol <- 0L; filas <- c(CURSOS = 0L, PROMEDIO = 0L); trampa_promedio <- 0L; sin_y <- 0L; fuera_g <- 0L
for (b in c("CURSOS", "PROMEDIO")) {
  esp <- esp_s2c(b); filas[[b]] <- nrow(esp)
  ok(nrow(esp) > 0 && any(esp$hay_debajo), sprintf("rama %s: %d combinaciones válidas, %d con cifra de G bajo D",
                                                   b, nrow(esp), sum(esp$hay_debajo)))
  ok(all((esp$nG - esp$nP) * (esp$aG - esp$aP) < 0 & esp$TG > esp$TP), sprintf("L-3 sin dominancia y gana G (%s)", b))
  pG <- if (b == "CURSOS") "M" else "P"; pP <- if (b == "CURSOS") "P" else "M"
  mtot <- function(p) if (p == "M") c("M_a", "M_b") else c("P_a", "P_b")
  for (i in seq_len(nrow(esp))) {
    f <- esp[i, ]; d <- list(nG = f$nG, nP = f$nP, aG = f$aG, aP = f$aP)
    af <- function(m, sede, x) list(molde = m, sede = sede, x = x, perfil = env0$MOLDES[[m]]$perfil,
                                    afirma = env0$MOLDES[[m]]$afirma)
    for (m in c(mtot(pG), "N_a", "N_b")) {
      if (!env0$verdad(af(m, "G", f$D), d)) viol <- viol + 1L                     # la clave, verdadera
      for (x in setdiff(c(f$aG, f$aP, f$delta, f$parcialG, f$TG), f$D))            # toda cifra falsa de G, falsa
        if (env0$verdad(af(m, "G", x), d)) viol <- viol + 1L
    }
    for (m in c(mtot(pP), if (pP == "P") "P_ven"))
      for (x in c(f$delta, f$parcialP, f$k, f$D)) if (env0$verdad(af(m, "P", x), d)) viol <- viol + 1L
    for (m in c("U_a", "U_b", "U_c", "U_d")) if (env0$verdad(af(m, "P", 50), d)) viol <- viol + 1L
    if (b == "PROMEDIO" && env0$verdad(af("P_ven", "G", f$delta), d)) trampa_promedio <- trampa_promedio + 1L
    ## H1: con las exclusiones nuevas, ¿queda cifra para la razón sin sustento a cada lado de D?
    cal <- env0$calculos_tabla(f); excl <- c(f$D, f$parcialP, f$nG, f$nP, f$aG, f$aP, cal)
    if (!length(setdiff(2:(f$D - 1), excl)) || !length(setdiff((f$D + 1):(f$D + 40), excl))) sin_y <- sin_y + 1L
    if (!all(c(f$aG, f$aP, f$delta, f$parcialG, f$TG) %in% c(f$aG, f$aP, cal))) fuera_g <- fuera_g + 1L
  }
}
ok(viol == 0L, sprintf("L-2: en %d combinaciones x todos los moldes y cifras, la clave es la única verdadera (%d violaciones)",
                       sum(filas), viol))
ok(trampa_promedio == filas[["PROMEDIO"]],
   sprintf("L-2.3: en la rama PROMEDIO la frase «G ganó porque ... mayor promedio con δ kilos de más» es verdadera en %d/%d (por eso no se usa para G)",
           trampa_promedio, filas[["PROMEDIO"]]))
ok(sin_y == 0L, sprintf("H1/F1: en las %d combinaciones queda cifra admisible para la razón sin sustento, debajo y encima de D (%d sin ella)",
                        sum(filas), sin_y))
ok(fuera_g == 0L, "H1: toda cifra posible del distractor de G ya la excluyen la tabla o calculos_tabla()")
## Regla #13 v1.1 (ronda 3): lecturas del TEXTO. Se construyen los textos reales de todos los moldes con
## todas las cifras posibles de cada papel y se comprueba (a) que ninguno admite la lectura «total» según
## el generador; (b) con un oráculo léxico propio, más amplio, que ninguna cifra va en una cláusula que
## la presente como cantidad; y (c) cuánto cambiaría si se admitiera la lectura «total» aunque el texto
## no la traiga (exposición declarada, no defecto).
ORACULO_CLAUSULA <- paste0("(fue de|fue|fueron|recogió|recogieron|sumó|sumaron|llegó a|alcanzó|reunió|juntó|",
                           "juntaron|con|de|es|son|hubo) [0-9]+[ \u00a0]kilos")   # S1: espacio normal o duro
n_txt <- 0L; adm_total <- 0L; clausula <- 0L
expo <- c(CURSOS = 0L, PROMEDIO = 0L); expo_tg <- expo; conf_g <- expo
for (b in c("CURSOS", "PROMEDIO")) {
  esp <- esp_s2c(b); pG <- if (b == "CURSOS") "M" else "P"; pP <- if (b == "CURSOS") "P" else "M"
  mtot <- function(p) if (p == "M") c("M_a", "M_b") else c("P_a", "P_b")
  for (i in seq_len(nrow(esp))) {
    f <- esp[i, ]; d <- list(nG = f$nG, nP = f$nP, aG = f$aG, aP = f$aP)
    casos <- c(
      do.call(c, lapply(c(mtot(pG), "N_a", "N_b"), function(m)
        lapply(unique(c(f$D, f$aG, f$aP, f$delta, f$parcialG, f$TG)), function(x) list(m = m, x = x, sede = "G")))),
      do.call(c, lapply(mtot(pP), function(m) lapply(unique(c(f$parcialP, f$delta)), function(x) list(m = m, x = x, sede = "P")))),
      do.call(c, lapply(c("U_a", "U_b", "U_c", "U_d"), function(m) lapply(c(f$D + 1, f$delta), function(x) list(m = m, x = x, sede = "P")))),
      if (isTRUE(env0$D2_SIN_CIFRA)) lapply(c(mtot(pG), "N_a", "N_b"), function(m) list(m = m, x = NA_real_, sede = "G")))
    for (cc in casos) {
      S <- if (cc$sede == "G") "la sede norte" else "la sede sur"; O <- if (cc$sede == "G") "la sede sur" else "la sede norte"
      t <- env0$MOLDES[[cc$m]]$f(S, O, cc$x); n_txt <- n_txt + 1L
      if ("total" %in% env0$lecturas_de(t)) adm_total <- adm_total + 1L
      if (grepl(ORACULO_CLAUSULA, t)) clausula <- clausula + 1L
      if (cc$sede == "G" && isTRUE(cc$x != f$D)) {
        conf_g[[b]] <- conf_g[[b]] + 1L
        a <- list(molde = cc$m, sede = "G", x = cc$x, perfil = env0$MOLDES[[cc$m]]$perfil, afirma = env0$MOLDES[[cc$m]]$afirma)
        if (env0$verdad(a, d, "total")) { expo[[b]] <- expo[[b]] + 1L; if (cc$x == f$TG) expo_tg[[b]] <- expo_tg[[b]] + 1L }
      }
    }
  }
}
ok(adm_total == 0L, sprintf("regla #13 v1.1: de %d textos no canónicos (todos los moldes y cifras), ninguno admite la lectura «total» (%d)",
                            n_txt, adm_total))
ok(clausula == 0L, sprintf("regla #13 v1.1, oráculo léxico propio: ninguna cifra va en una cláusula que la presente como cantidad (%d de %d)",
                           clausula, n_txt))
ok(all(expo == expo_tg), "exposición a la lectura «total»: solo la cifra T_G (ERR-TOTAL) la volvería verdadera")
cat(sprintf(paste0("  exposición declarada (el texto no admite esa lectura): leída «en x kilos» como el total, la cifra falsa de G ",
                   "sería verdadera en %d de %d configuraciones de G en CURSOS y %d de %d en PROMEDIO (todas con x = T_G)\n"),
            expo[["CURSOS"]], conf_g[["CURSOS"]], expo[["PROMEDIO"]], conf_g[["PROMEDIO"]]))
envC <- NULL; for (s in 1:300) { r <- correr(s); if (is.null(r$err) && isTRUE(r$env$es_canonica)) { envC <- r$env; break } }
if (is.null(envC)) falla("canónica no encontrada en 300 semillas") else {
  cv <- vapply(envC$afs, envC$cambia_veredicto, TRUE)
  vt <- vapply(envC$afs, function(a) "total" %in% envC$lecturas_de(a$texto) && envC$verdad(a, envC$d, "total"), TRUE)
  ok(identical(names(cv)[cv], "clave") && !any(vt),
     sprintf(paste0("regla #13 v1.1, canónica (semilla %d): solo la clave verbatim cambia con la lectura «el total fue de 20 kilos», ",
                    "que no vuelve verdadera ninguna opción (cambian: %s; verdaderas con «total»: %d)"),
             s, paste(names(cv)[cv], collapse = ","), sum(vt)))
}
## H3 de la FASE 2C r4: la suma de control de la canónica la calcula el .Rmd sobre tabla_df, antes de
## tabla_responsiva(); aquí se repite sobre lo que se muestra (tabla_md), sin el relleno de la ronda 4,
## la negrita ni el espacio duro. Mismas cifras de ITEM_OFICIAL.md (119 caracteres, 695221).
celdas_md <- function(md) {
  f <- grep("^\\|", strsplit(md, "\n", fixed = TRUE)[[1]], value = TRUE)
  f <- f[!grepl("^\\|[-:| ]+\\|$", f)]
  x <- unlist(lapply(f, function(r) trimws(strsplit(sub("^\\|", "", sub("\\|$", "", r)), "|", fixed = TRUE)[[1]])))
  gsub("\u00a0", " ", gsub("<span[^>]*>|</span>|\\*\\*", "", x))
}
canon_tabla_ok <- function(env) { cm <- paste(celdas_md(env$tabla_md), collapse = "|")
  nchar(cm) == 119L && env$pond(cm) == 695221 }
if (!is.null(envC)) ok(canon_tabla_ok(envC), sprintf("H3 r4: canónica verbatim sobre la tabla mostrada (tabla_md), semilla %d", s))
## H1 de la FASE 2C r4b: la corrección del encabezado huérfano al pie del PDF (ronda 4: 14/60 -> 0/60)
## pasa a guarda estructural. En la Solution, todo encabezado en negrita sale de encabezado(), y
## encabezado() emite el bloque {=latex} con \nobreak y \@afterheading. M21 y M22 la revierten.
encabezados_ok <- function(lin) {
  i0 <- grep("^```\\{r solucion", lin); i1 <- which(lin == "```"); i1 <- i1[i1 > i0][1]
  if (length(i0) != 1L || is.na(i1)) return(FALSE)
  sol <- lin[(i0 + 1):(i1 - 1)]; j <- grep("^encabezado <- function", sol)
  def <- paste(sol[c(j, j + 1L)], collapse = "")
  sueltos <- grep("cat\\(\"\\*\\*[[:alpha:]]", sol)             # cat("**Título**"), fuera de encabezado()
  length(j) == 1L && grepl("{=latex}", def, fixed = TRUE) && grepl("nobreak", def, fixed = TRUE) &&
    grepl("@afterheading", def, fixed = TRUE) && !length(sueltos) && length(grep("^\\s*encabezado\\(", sol)) >= 8L
}
ok(encabezados_ok(l), "H1 r4b: los 8 encabezados de la Solution salen de encabezado(), con \\nobreak y \\@afterheading")
l_m21 <- sub("\\\\nobreak\\\\makeatletter\\\\@afterindenttrue\\\\@afterheading\\\\makeatother", "", l, fixed = TRUE)
ok(!identical(l_m21, l) && !encabezados_ok(l_m21), "M21 (quita \\nobreak y \\@afterheading de encabezado()): la sonda H1 r4b lo detecta")
l_m22 <- sub("encabezado(\"Caso específico\")", "cat(\"**Caso específico**\\n\\n\")", l, fixed = TRUE)
ok(!identical(l_m22, l) && !encabezados_ok(l_m22), "M22 (un encabezado vuelve a cat(\"**…**\")): la sonda H1 r4b lo detecta")
## H6 (FASE 2C, ronda 1): todo calcula() se detiene fuera de contexto (codigo-rmd #8).
p0 <- list(nG = 5, nP = 4, aG = 40, aP = 45, k = 1, delta = 5, TG = 200, TP = 180, D = 20, parcialG = 40,
           parcialP = 20, y = 33, fuente_dato = NA_character_, fuente_padv = NA_character_, rama = "CURSOS")
fuera <- list("ERR-DATO" = p0, "ERR-RESTA" = modifyList(p0, list(delta = 20)),
              "ERR-PARCIAL" = modifyList(p0, list(parcialG = 20)), "ERR-TOTAL" = modifyList(p0, list(TG = 20)),
              "ERR-PROM" = p0, "ERR-CURSOS" = p0, "ERR-INVENTA" = modifyList(p0, list(y = 5)))
para <- vapply(names(fuera), function(cd) inherits(tryCatch(env0$errores_conceptuales[[cd]]$calcula(fuera[[cd]]),
                                                            error = function(e) e), "error"), TRUE)
ok(all(para) && length(para) == length(env0$errores_conceptuales),
   sprintf("H6: los %d calcula() se detienen fuera de contexto%s", length(para),
           if (all(para)) "" else paste0(" (no: ", paste(names(para)[!para], collapse = ", "), ")")))
usa_ven_G <- sum(vapply(c(NAT, CU$CURSOS, CU$PROMEDIO), function(x) x$molde_gdis %in% c("P_ven", "M_ven"), TRUE))
ok(usa_ven_G == 0L, "L-2.3: el distractor de G nunca usa un molde de ventaja")
anchos <- c()
for (cj in env0$CONJUNTOS) for (m in env0$MATERIALES) {
  df <- data.frame(paste0("**", cj$filas, "**"), c("8 cursos", "8 cursos"), c("60 kilos", "60 kilos"))
  names(df) <- c(cj$encab, "Cantidad de cursos", paste("Promedio de", m, "recolectado por curso"))
  a <- vapply(seq_along(df), function(j) max(nchar(c(names(df)[j], df[[j]]))), 0)
  anchos <- c(anchos, sum(a) + 2 * length(a) + 1)
}
ok(max(anchos) <= 85, sprintf("tabla: ancho máximo %d de 85 en las %d combinaciones de contexto y material", max(anchos), length(anchos)))

## ---- E4. Diagnosticidad ----------------------------------------------------------
cat("\n[E4] Diagnosticidad\n")
sc <- function(S, k) { if (!any(S) || all(S)) 0.25 else if (S[k]) 1 / sum(S) else 0 }
h1 <- function(o, k, f = max) {
  L <- nchar(o); m <- f(L); if (sum(L == m) != 1 || which(L == m) != k) return(FALSE)
  otro <- if (identical(f, max)) max(L[-k]) else min(L[-k]); abs(m - otro) / otro >= 0.15
}
rango_D <- function(x) { v <- vapply(x$opciones, cif, 0); sum(v[!is.na(v)] < v[x$clave]) + 1 }
## FASE 2C r3 (H1): pistas combinables. es_suma: la cifra es suma de otras dos; atipica: la cifra que
## dobla a todas las demás (con ERR-TOTAL, T_G); filtra: descarta del conjunto S las marcadas por t
## si queda al menos una.
es_suma <- function(v) vapply(seq_along(v), function(i) { o <- v[-i]; !is.na(v[i]) &&
  isTRUE(any(outer(o, o, "+")[upper.tri(diag(length(o)))] == v[i], na.rm = TRUE)) }, TRUE)
filtra <- function(S, t) { t <- S & t; if (any(t) && any(S & !t)) S & !t else S }
atipica <- function(v) { a <- rep(FALSE, length(v)); i <- which.max(v)
  if (length(i) && sum(!is.na(v)) >= 2 && all(v[i] >= 2 * v[-i], na.rm = TRUE)) a[i] <- TRUE; a }
sondas <- function(V) {
  if (!length(V)) return(NULL)
  m <- function(fn) mean(vapply(V, fn, 0))
  c(
    "H1 clave única más larga (margen 15 %)" = m(function(x) h1(x$opciones, x$clave)),
    "H1 clave única más corta (margen 15 %)" = m(function(x) h1(x$opciones, x$clave, min)),
    "clave es la más larga (sin margen)" = m(function(x) sc(nchar(x$opciones) == max(nchar(x$opciones)), x$clave)),
    "clave es la más corta (sin margen)" = m(function(x) sc(nchar(x$opciones) == min(nchar(x$opciones)), x$clave)),
    "canal 2: cifra falsa de G menor que D" = m(function(x) as.numeric(isTRUE(x$X < x$D))),
    "S1 descarta cifras de la tabla" = m(function(x) sc(!(vapply(x$opciones, cif, 0) %in% x$tabla), x$clave)),
    "S1b se queda con cifras de la tabla" = m(function(x) sc(vapply(x$opciones, cif, 0) %in% x$tabla, x$clave)),
    "S2 par de la sede de la fila 1" = m(function(x) sc(x$nom_o == x$fila1, x$clave)),
    "S3 par de la sede de más cursos" = m(function(x) sc(x$sede_o == (if (x$rama == "CURSOS") "G" else "P"), x$clave)),
    "S4 par de la sede de mayor promedio" = m(function(x) sc(x$sede_o == (if (x$rama == "CURSOS") "P" else "G"), x$clave)),
    "S5 más cursos + menor cifra del par" = m(function(x) {
      S <- x$sede_o == (if (x$rama == "CURSOS") "G" else "P"); v <- vapply(x$opciones, cif, 0)
      i <- which(S & !is.na(v)); if (length(i) == 2) S[i[v[i] != min(v[i])]] <- FALSE; sc(S, x$clave) }),
    "S6 mayor promedio + menor cifra del par" = m(function(x) {
      S <- x$sede_o == (if (x$rama == "CURSOS") "P" else "G"); v <- vapply(x$opciones, cif, 0)
      i <- which(S & !is.na(v)); if (length(i) == 2) S[i[v[i] != min(v[i])]] <- FALSE; sc(S, x$clave) }),
    "S7 G real + menor cifra del par (cota canal 2)" = m(function(x) {
      S <- x$sede_o == "G"; v <- vapply(x$opciones, cif, 0); i <- which(S & !is.na(v))
      S[i[v[i] != min(v[i])]] <- FALSE; sc(S, x$clave) }),
    "S_U la sede de la razón sin sustento pierde" = m(function(x) sc(x$sede_o != x$sede_o[x$rol == "psin"], x$clave)),
    "S_todos elige la opción con «todos»" = m(function(x) sc(grepl("todos", x$opciones, fixed = TRUE), x$clave)),
    ## H1 de la FASE 2C r2: D = parcial_G - parcial_P, así que con ERR-PARCIAL la clave es la diferencia
    ## de otras dos cifras; y combinaciones del atajo de O-1 con pistas superficiales.
    "R_dif la cifra que es diferencia de otras dos" = m(function(x) { v <- vapply(x$opciones, cif, 0)
      S <- vapply(seq_along(v), function(i) !is.na(v[i]) && isTRUE(any(outer(v[-i], v[-i], "-") == v[i], na.rm = TRUE)), TRUE)
      sc(S, x$clave) }),
    "S_U + S1 (par de G sin la cifra de la tabla)" = m(function(x) {
      S <- x$sede_o != x$sede_o[x$rol == "psin"]; v <- vapply(x$opciones, cif, 0); t <- S & v %in% x$tabla
      if (any(t) && any(S & !t)) S <- S & !t; sc(S, x$clave) }),
    "S_U + suma (par de G sin la cifra que es suma de otras dos)" = m(function(x) {
      S <- x$sede_o != x$sede_o[x$rol == "psin"]; v <- vapply(x$opciones, cif, 0)
      su <- vapply(seq_along(v), function(i) { o <- v[-i]; !is.na(v[i]) &&
        isTRUE(any(outer(o, o, "+")[upper.tri(diag(length(o)))] == v[i], na.rm = TRUE)) }, TRUE)
      t <- S & su; if (any(t) && any(S & !t)) S <- S & !t; sc(S, x$clave) }),
    "Atípica: descarta la cifra que dobla a todas las demás" = m(function(x) sc(!atipica(vapply(x$opciones, cif, 0)), x$clave)),
    "S_U + S1 + suma (las dos pistas)" = m(function(x) { v <- vapply(x$opciones, cif, 0)
      S <- filtra(x$sede_o != x$sede_o[x$rol == "psin"], v %in% x$tabla); sc(filtra(S, es_suma(v)), x$clave) }),
    "S_U + atípica" = m(function(x) { v <- vapply(x$opciones, cif, 0)
      sc(filtra(x$sede_o != x$sede_o[x$rol == "psin"], atipica(v)), x$clave) }),
    "S_U + S1 + suma + atípica" = m(function(x) { v <- vapply(x$opciones, cif, 0)
      S <- filtra(x$sede_o != x$sede_o[x$rol == "psin"], v %in% x$tabla); S <- filtra(S, es_suma(v))
      sc(filtra(S, atipica(v)), x$clave) }),
    "D es la menor cifra (frecuencia)" = m(function(x) as.numeric(rango_D(x) == 1)),
    "D es la 2.ª cifra (frecuencia)" = m(function(x) as.numeric(rango_D(x) == 2)),
    "D es la 3.ª cifra (frecuencia)" = m(function(x) as.numeric(rango_D(x) == 3)),
    "D es la 4.ª cifra (frecuencia)" = m(function(x) as.numeric(rango_D(x) == 4)),
    "única opción sin cifra (frecuencia)" = m(function(x) as.numeric(sum(is.na(vapply(x$opciones, cif, 0))) == 1)),
    "clave con token exclusivo (frecuencia)" = m(function(x) {
      T <- lapply(x$opciones, function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]])))
      as.numeric(length(setdiff(T[[x$clave]], unlist(T[-x$clave]))) > 0) }))
}
CANON_V <- if (!is.null(CANON)) {
  perms <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perms(v[-i]), function(p) c(v[i], p))))
  lapply(perms(1:4), function(p) { x <- CANON; x$opciones <- CANON$opciones[p]; x$sede_o <- CANON$sede_o[p]; x$rol <- CANON$rol[p]
    x$nom_o <- CANON$nom_o[p]; x$clave <- which(p == CANON$clave); x })
} else list()
TAB <- cbind(mezcla = sondas(NAT), CURSOS = sondas(CU$CURSOS), PROMEDIO = sondas(CU$PROMEDIO), canonica = sondas(CANON_V))
print(round(100 * TAB, 1))
COD_G <- c("ERR-DATO", "ERR-RESTA", "ERR-PARCIAL", "ERR-TOTAL")
CUOTA <- sapply(c("CURSOS", "PROMEDIO"), function(b)
  100 * prop.table(table(factor(vapply(CU[[b]], `[[`, "", "cod_gdis"), levels = COD_G))))
cat("  cuota del tipo de error del distractor de G, por rama (%):\n"); print(round(CUOTA, 1))
ok(all(CUOTA["ERR-TOTAL", ] > 0), "ERR-TOTAL alcanzable en las dos ramas")
if (DEPURACION) {
  cat(sprintf("\nDEPURACIÓN (regla #23): N = %d por estrato; la cifra oficial es la de N = 100. Sin batería ni mutantes.\n", N))
  quit(status = 0L)
}
for (est in list(list("mezcla", NAT), list("CURSOS", CU$CURSOS), list("PROMEDIO", CU$PROMEDIO))) {
  L <- sapply(est[[2]], function(x) setNames(nchar(x$opciones), x$rol)[c("clave", "gdis", "padv", "psin")])
  cat(sprintf("  largo medio por papel (%s): %s\n", est[[1]], paste(sprintf("%s %.0f", rownames(L), rowMeans(L)), collapse = " · ")))
  ## §P7-B: margen relativo de la clave sobre la segunda cuando es la única más larga / más corta.
  mg <- function(f) { z <- vapply(est[[2]], function(x) { L <- nchar(norm_ed(x$opciones)); k <- x$clave
    if (sum(L == f(L)) != 1 || L[k] != f(L)) return(NA_real_); abs(L[k] - f(L[-k])) / f(L[-k]) }, 0); z[!is.na(z)] }
  ml <- mg(max); mc <- mg(min)
  cat(sprintf("  §P7-B (%s): clave única más larga en %d versiones, margen mediano %.1f %% (máx %.1f %%); única más corta en %d, margen mediano %.1f %%\n",
              est[[1]], length(ml), 100 * stats::median(c(ml, NA), na.rm = TRUE), 100 * max(c(ml, 0)), length(mc), 100 * stats::median(c(mc, NA), na.rm = TRUE)))
}
cat("  (las tasas de acierto se comparan con el azar, 25 %; «canal 2» y las frecuencias no son tasas de acierto)\n")
nc <- c(CU$CURSOS, CU$PROMEDIO)
ok(abs(mean(vapply(nc, function(x) as.numeric(isTRUE(x$X < x$D)), 0)) - 0.5) <= 0.12,
   "canal 2: en las no canónicas la cifra falsa de G queda por debajo de D cerca del 50 %")
for (b in c("CURSOS", "PROMEDIO")) {
  ex <- TAB["S7 G real + menor cifra del par (cota canal 2)", b] - 0.5
  ok(ex <= 0.08, sprintf("canal 2 en la rama %s: «menor cifra del par ganador» %+.1f pp sobre el 50 %% de quien ya sabe qué sede ganó (obliga > +8)", b, 100 * ex))
  for (j in 1:4) { fr <- TAB[sprintf(c("D es la menor cifra (frecuencia)", "D es la 2.ª cifra (frecuencia)", "D es la 3.ª cifra (frecuencia)", "D es la 4.ª cifra (frecuencia)")[j]), b]
    ok(abs(fr - 0.25) <= 0.12, sprintf("lugar de D entre las cifras, rama %s: puesto %d en el %.0f %% (25 %% ± 12)", b, j, 100 * fr)) }
  for (h in c("H1 clave única más larga (margen 15 %)", "H1 clave única más corta (margen 15 %)"))
    ok(TAB[h, b] <= 0.5, sprintf("%s en la rama %s: %.0f %% (bloquea el 100 %%; aviso desde 90 %%)", h, b, 100 * TAB[h, b]))
}

## H5 (FASE 2C, ronda 1): las sondas S1-S6 pasan de documentación a guarda. S1, S1b y S2 las aplica
## el estudiante sin saber la rama: se juzgan por rama. S3-S6 eligen una sede por su perfil y, por la
## alternancia de L-3, dan 50 % o 0 % en cada rama por construcción: se juzgan en la mezcla, ponderada
## con las probabilidades del diseño (0,10 canónica; 0,90 x 4/9 CURSOS; 0,90 x 5/9 PROMEDIO).
## Obliga más de +8 pp sobre el azar (§P7-A).
W <- c(CURSOS = 0.9 * 4 / 9, PROMEDIO = 0.9 * 5 / 9, canonica = 0.10)
pond_mezcla <- function(s) sum(W * TAB[s, names(W)])
RESIDUOS <- character(0); PENDIENTES <- character(0)   # PENDIENTES: > +8 pp sin firma -> exit 3, nunca APROBADO
## Canal 3 (diseño): a lo sumo un distractor cita un dato de la tabla en kilos (aG o aP), y nunca la
## clave. Con eso, descartar «la cifra que está en la tabla» (S1) vale a lo sumo descartar una opción
## de cuatro: 1/3, +8,3 pp. S1 compara números sin unidad, así que una cifra que coincide con una
## cantidad de cursos (p. ej., δ = 5 con 5 cursos) puede sumar algo: se tolera 1 pp por encima.
c3_v <- function(x) { v <- vapply(x$opciones, cif, 0); a <- x$tabla[3:4]
  sum(v %in% a) <= 1 && !(v[x$clave] %in% a) }
ok(all(vapply(c(NAT, CU$CURSOS, CU$PROMEDIO), c3_v, TRUE)),
   "canal 3: en toda versión, a lo sumo un distractor cita un dato de la tabla en kilos, y nunca la clave")
s1_v <- function(x) sc(!(vapply(x$opciones, cif, 0) %in% x$tabla), x$clave)
COTA_S1 <- 1 / 3 - 1 / 4 + 0.01
## H1 de la FASE 2C r4: el alcance firmado de «O-1 corregido» (.claude/CLAUDE.md local, WAIT_USER #3 de
## la ronda 3, md5 cd94b59a…) pasa de prosa a guarda. Las semillas de E1 y E2 son fijas, así que las
## cifras son deterministas: hasta lo firmado es OK; por encima vuelve al profesor (exit 3). Antes se
## comparaba con el 50 % de la premisa tachada, y el exit 3 permanente no distinguía «dentro del
## alcance» de «lo superó». El mutante M19 sube la cuota de ERR-TOTAL y debe salir de lo firmado.
FIRMA_O1 <- rbind(
  "S_U + S1 (par de G sin la cifra de la tabla)"                = c(CURSOS = 0.710, PROMEDIO = 0.550),
  "S_U + suma (par de G sin la cifra que es suma de otras dos)" = c(CURSOS = 0.605, PROMEDIO = 0.620),
  "S_U + S1 + suma (las dos pistas)"                            = c(CURSOS = 0.790, PROMEDIO = 0.670),
  "S_U + atípica"                                               = c(CURSOS = 0.515, PROMEDIO = 0.580),
  "S_U + S1 + suma + atípica"                                   = c(CURSOS = 0.815, PROMEDIO = 0.745))
FIRMA_S1_PROMEDIO <- 0.084                                  # residuo firmado: S1 en la rama PROMEDIO, +8,4 pp
FIRMA_LARGO <- c(exceso = 0.104, chars = 0.027, palabras = 0.065)   # declarado sin firma (§P7-B)
FIRMADOS <- character(0)
## Las cifras se firmaron redondeadas a un decimal (en %); se comparan igual, para que 33,42 % no exceda 33,4 %.
hasta <- function(x, lim) round(100 * x, 1) <= round(100 * lim, 1)
## Regla #22 §P7-E: la batería se deja en rojo; el override no la vuelve PASS. Dentro del alcance firmado
## el canal se reporta como ROJO CUBIERTO (exit 4), y por encima como PENDIENTE DE FIRMA (exit 3).
POR <- character(0)   # qué override cubre cada canal en rojo
firmado <- function(msg, por = "O-1 corregido") { FIRMADOS <<- c(FIRMADOS, msg); POR <<- c(POR, por)
  cat("  ROJO, cubierto por ", por, ": ", msg, "\n", sep = "") }
juzga <- function(s, est, tasa) {
  ex <- tasa - 0.25
  if (ex <= 0.08) return(invisible(ok(TRUE, sprintf("H5: %s, %s: %.1f %% (%+.1f pp; obliga > +8)", s, est, 100 * tasa, 100 * ex))))
  if (s == "S1 descarta cifras de la tabla" && est == "rama PROMEDIO" && hasta(ex, FIRMA_S1_PROMEDIO))
    return(invisible(firmado(sprintf("O-1 corregido: %s, %s: %.1f %% (%+.1f pp; firmado hasta +8,4 pp)",
                                     s, est, 100 * tasa, 100 * ex))))
  if (startsWith(s, "S1 ") && ex <= COTA_S1) {
    ## H2 de la FASE 2C r2 (regla #24 H-5): por encima de +8 pp NO es APROBADO; queda pendiente de firma
    ## y el verificador sale con exit 3. Por encima de la cota del diseño, FALLA.
    PENDIENTES <<- c(PENDIENTES, sprintf("%s, %s: %.1f %% (%+.1f pp); cota del diseño +8,3 pp (+1 pp de coincidencias)",
                                         s, est, 100 * tasa, 100 * ex))
    cat(sprintf("  RESIDUO PENDIENTE DE FIRMA (§P7-D agotado): %s, %s: %.1f %% (%+.1f pp; cota del diseño +8,3 pp)\n",
                s, est, 100 * tasa, 100 * ex))
  } else falla(sprintf("H5: %s, %s: %.1f %% (%+.1f pp; obliga > +8)", s, est, 100 * tasa, 100 * ex))
}
for (s in c("S1 descarta cifras de la tabla", "S1b se queda con cifras de la tabla", "S2 par de la sede de la fila 1"))
  for (b in c("CURSOS", "PROMEDIO")) juzga(s, paste("rama", b), TAB[s, b])
for (s in c("S1 descarta cifras de la tabla", "S2 par de la sede de la fila 1", "S3 par de la sede de más cursos",
            "S4 par de la sede de mayor promedio", "S5 más cursos + menor cifra del par", "S6 mayor promedio + menor cifra del par"))
  juzga(s, "mezcla ponderada", pond_mezcla(s))
## H3 / O-1: el override cubre hasta +16,7 pp atribuibles. S_U vale 1/2 exacto en cada versión (la
## razón sin sustento siempre nombra a P): 50 % frente al 33,3 % de descartarla con razón. S_todos
## («todos» solo en el molde N, que es la clave la mitad de las veces) da 50 % por construcción.
pendiente <- function(msg) { PENDIENTES <<- c(PENDIENTES, msg); cat("  PENDIENTE DE FIRMA:", msg, "\n") }
for (b in c("CURSOS", "PROMEDIO")) {
  su <- TAB["S_U la sede de la razón sin sustento pierde", b]
  msg_su <- sprintf("O-1: S_U, rama %s: %.1f %%, %+.1f pp atribuibles (alcance firmado: +16,7)", b, 100 * su, 100 * (su - 1 / 3))
  if (su - 1 / 3 <= 0.167 + 1e-9) firmado(msg_su) else falla(msg_su)
  ## La premisa original de O-1 (50 %) resultó falsa en la ronda 2; rige el alcance de «O-1 corregido».
  for (s in rownames(FIRMA_O1)) {
    t <- TAB[s, b]; lim <- FIRMA_O1[s, b]
    if (hasta(t, lim)) firmado(sprintf("O-1 corregido: %s, rama %s: %.1f %% (firmado hasta %.1f %%)", s, b, 100 * t, 100 * lim))
    else pendiente(sprintf("O-1 corregido: %s, rama %s: %.1f %%, por encima de lo firmado (%.1f %%): vuelve al profesor",
                           s, b, 100 * t, 100 * lim))
  }
  for (s in c("R_dif la cifra que es diferencia de otras dos", "Atípica: descarta la cifra que dobla a todas las demás")) {
    ex <- TAB[s, b] - 0.25
    if (ex <= 0.08) ok(TRUE, sprintf("%s, rama %s: %+.1f pp (obliga > +8)", s, b, 100 * ex))
    else pendiente(sprintf("%s, rama %s: %+.1f pp sobre el azar (canal nuevo > +8 pp, O-1 § Alcance)", s, b, 100 * ex))
  }
}
## S_todos (H2 de la FASE 2C r2): aserción estructural en vez de tolerancia muestral.
st_ok <- all(vapply(c(CU$CURSOS, CU$PROMEDIO), function(x) {
  i <- grepl("todos", x$opciones, fixed = TRUE); sum(i) == 1 && x$sede_o[i] == "G" }, TRUE))
ok(st_ok, sprintf("O-1: S_todos estructural: en toda versión no canónica, una sola opción dice «todos» y es de G (medido: CURSOS %.1f %%, PROMEDIO %.1f %%; 50 %% por construcción)",
                  100 * TAB["S_todos elige la opción con «todos»", "CURSOS"], 100 * TAB["S_todos elige la opción con «todos»", "PROMEDIO"]))

## ---- S2b (FASE 2C r6, H1): el canal «resta» y el barrido del estímulo -----------------------------------------------
## H1 de la r6 (ALTA, DIAGNOSTICIDAD): ERR-RESTA va siempre del lado de G y cita δ = |aG - aP|, la resta de los dos
## promedios de la tabla, y en las no canónicas ninguna otra opción la cita. Quien resta los promedios y encuentra la cifra
## en una opción sabe, sin calcular ningún total, que esa sede ganó y que la clave es la otra opción de esa sede. R_dif
## busca diferencias entre cifras de las OPCIONES; la tabla solo entraba como pertenencia (S1, S1b) o en el canal 3.
## Mismo criterio que R_dif y las sondas de O-1: la primera cifra de cada opción (cif) y sc(). Dos lecturas:
##  (i)  la sede de la opción que cita el resultado gana, y se elige la otra opción de esa sede; sin cita, azar;
##  (ii) la opción que cita el resultado es falsa y se descarta; sin cita, azar.
## Las combinaciones con las pistas firmadas de «O-1 corregido» aplican «resta» al final, con filtra(): «… + resta».
## §P7 (cierre por familias) y §P7-G: el barrido mide las dos lecturas en cada operación de un paso entre dos celdas de la
## tabla TAL COMO SE MUESTRA (fila 1 y fila 2): suma, resta, producto y cociente cuando es entero. Va por fila porque el
## estudiante no sabe qué fila gana: rotular las celdas por G y P presupone la respuesta. Se añaden, declaradas, las
## cuatro operaciones de dos pasos que usa el ítem (las ventajas parciales de calculos_tabla()).
## Estratos: la mezcla de E1, las ramas CURSOS y PROMEDIO (N = 100 cada una) y la canónica por enumeración exacta (24
## órdenes), con su peso en la mezcla (0,10 × exceso). Ninguna regla de la S2b está firmada: toda la que pase de +8 pp
## sobre el azar queda PENDIENTE DE FIRMA (exit 3, O-1 § Alcance). M44 (E5) fuerza ERR-RESTA y tiene que subir «resta»;
## M19 tiene que subir el producto de una fila. Nada de esta sección consume el RNG.
lee_i <- function(x, r) { if (is.na(r)) return(0.25); v <- vapply(x$opciones, cif, 0); h <- which(v %in% r)
  if (length(h) != 1L) return(0.25); S <- x$sede_o == x$sede_o[h]; S[h] <- FALSE; sc(S, x$clave) }
lee_ii <- function(x, r) { if (is.na(r)) return(0.25); sc(!(vapply(x$opciones, cif, 0) %in% r), x$clave) }
aplica_r <- function(x, r) !is.na(r) && any(vapply(x$opciones, cif, 0) %in% r)
delta_t <- function(x) abs(x$tabla[3] - x$tabla[4])
su_set <- function(x) x$sede_o != x$sede_o[x$rol == "psin"]
R_RESTA <- "resta: la sede de la opción que cita |aG - aP| gana; la otra de esa sede"
H1_REGLAS <- setNames(list(
  function(x) lee_i(x, delta_t(x)),
  function(x) { v <- vapply(x$opciones, cif, 0); sc(filtra(su_set(x), v %in% delta_t(x)), x$clave) },
  function(x) { v <- vapply(x$opciones, cif, 0); sc(filtra(filtra(su_set(x), v %in% x$tabla), v %in% delta_t(x)), x$clave) },
  function(x) { v <- vapply(x$opciones, cif, 0); sc(filtra(filtra(su_set(x), es_suma(v)), v %in% delta_t(x)), x$clave) },
  function(x) { v <- vapply(x$opciones, cif, 0); S <- filtra(filtra(su_set(x), v %in% x$tabla), es_suma(v))
    sc(filtra(S, v %in% delta_t(x)), x$clave) },
  function(x) { v <- vapply(x$opciones, cif, 0); sc(filtra(filtra(su_set(x), atipica(v)), v %in% delta_t(x)), x$clave) },
  function(x) { v <- vapply(x$opciones, cif, 0); S <- filtra(filtra(filtra(su_set(x), v %in% x$tabla), es_suma(v)), atipica(v))
    sc(filtra(S, v %in% delta_t(x)), x$clave) }),
  c(R_RESTA, "S_U + resta", "S_U + S1 + resta", "S_U + suma + resta", "S_U + S1 + suma + resta", "S_U + atípica + resta",
    "S_U + S1 + suma + atípica + resta"))
EST_S2B <- list(mezcla = NAT, CURSOS = CU$CURSOS, PROMEDIO = CU$PROMEDIO, canonica = CANON_V)
mide <- function(reglas) t(vapply(reglas, function(f) vapply(EST_S2B, function(V)
  if (length(V)) mean(vapply(V, f, 0)) else NA_real_, 0), numeric(length(EST_S2B))))
ROT_S2B <- c(mezcla = "mezcla de E1", CURSOS = "rama CURSOS", PROMEDIO = "rama PROMEDIO", canonica = "canónica (24 órdenes, exacta)")
juzga_s2b <- function(TABX, grupo) {
  n_pend <- 0L
  for (rg in rownames(TABX)) for (es in colnames(TABX)) {
    tasa <- TABX[rg, es]; if (is.na(tasa)) next; ex <- tasa - 0.25
    if (ex <= 0.08 + 1e-9) next
    n_pend <- n_pend + 1L
    pendiente(sprintf("%s: «%s», %s: %.1f %% (%+.1f pp sobre el azar)%s; regla nueva por encima de +8 pp, sin firma: vuelve al profesor (O-1 § Alcance)",
                      grupo, rg, ROT_S2B[[es]], 100 * tasa, 100 * ex,
                      if (es == "canonica") sprintf("; peso en la mezcla %+.2f pp", 10 * ex) else ""))
  }
  n_pend
}
writeLines("  S2b · H1 de la FASE 2C r6: «resta» y sus combinaciones con las pistas de «O-1 corregido» (acierto en %):")
TAB_H1 <- mide(H1_REGLAS)
print(round(100 * TAB_H1, 1))
apl_r <- vapply(EST_S2B, function(V) if (length(V)) mean(vapply(V, function(x) aplica_r(x, delta_t(x)), TRUE)) else NA_real_, 0)
nc_h <- Filter(function(x) !isTRUE(x$canon) && aplica_r(x, delta_t(x)), c(NAT, CU$CURSOS, CU$PROMEDIO))
quien <- table(vapply(nc_h, function(x) { h <- which(vapply(x$opciones, cif, 0) %in% delta_t(x))
  paste(x$sede_o[h], if (x$rol[h] == "gdis") x$cod_gdis else x$rol[h]) }, ""))
writeLines(sprintf("  «resta» aplica (alguna opción cita |aG - aP|): mezcla %.0f %% · CURSOS %.0f %% · PROMEDIO %.0f %% · canónica %.0f %%; en las %d no canónicas de E1 y de los estratos en que aplica, la opción que la cita es: %s",
                   100 * apl_r[["mezcla"]], 100 * apl_r[["CURSOS"]], 100 * apl_r[["PROMEDIO"]], 100 * apl_r[["canonica"]],
                   length(nc_h), paste(sprintf("%s (%d)", names(quien), as.integer(quien)), collapse = ", ")))
np_h1 <- juzga_s2b(TAB_H1, "S2b H1")
writeLines(sprintf("  S2b H1: %d de %d cifras (%d reglas × 4 estratos) por encima de +8 pp sobre el azar: pendientes de firma",
                   np_h1, length(TAB_H1), nrow(TAB_H1)))
## Barrido del estímulo: las celdas por fila de la tabla (gi = la fila de G).
celdas <- function(x) { tb <- x$tabla
  if (isTRUE(x$gi == 1)) c(n1 = tb[[1]], a1 = tb[[3]], n2 = tb[[2]], a2 = tb[[4]]) else c(n1 = tb[[2]], a1 = tb[[4]], n2 = tb[[1]], a2 = tb[[3]]) }
OPS_UN_PASO <- list(suma = function(a, b) a + b, resta = function(a, b) abs(a - b), producto = function(a, b) a * b,
                    cociente = function(a, b) { M <- max(a, b); m <- min(a, b); if (m > 0 && M %% m == 0) M / m else NA_real_ })
op_de <- function(g, a, b) { force(g); force(a); force(b); function(cc) g(cc[[a]], cc[[b]]) }
OPS_EST <- list()
for (par_c in list(c("n1", "n2"), c("a1", "a2"), c("n1", "a1"), c("n2", "a2"), c("n1", "a2"), c("n2", "a1")))
  for (op_c in names(OPS_UN_PASO))
    OPS_EST[[switch(op_c, suma = sprintf("%s + %s", par_c[1], par_c[2]), resta = sprintf("|%s - %s|", par_c[1], par_c[2]),
                    producto = sprintf("%s * %s", par_c[1], par_c[2]), cociente = sprintf("%s / %s (entero)", par_c[1], par_c[2]))]] <-
      op_de(OPS_UN_PASO[[op_c]], par_c[1], par_c[2])
OPS_EST[["dos pasos: |n1 - n2| * a1"]] <- function(cc) abs(cc[["n1"]] - cc[["n2"]]) * cc[["a1"]]
OPS_EST[["dos pasos: |n1 - n2| * a2"]] <- function(cc) abs(cc[["n1"]] - cc[["n2"]]) * cc[["a2"]]
OPS_EST[["dos pasos: n1 * |a1 - a2|"]] <- function(cc) cc[["n1"]] * abs(cc[["a1"]] - cc[["a2"]])
OPS_EST[["dos pasos: n2 * |a1 - a2|"]] <- function(cc) cc[["n2"]] * abs(cc[["a1"]] - cc[["a2"]])
regla_de <- function(fr, lec) { force(fr); force(lec); function(x) { r <- fr(celdas(x)); if (lec == "i") lee_i(x, r) else lee_ii(x, r) } }
BARRIDO <- list()
for (nm_op in names(OPS_EST)) { BARRIDO[[paste(nm_op, "(i)")]] <- regla_de(OPS_EST[[nm_op]], "i")
  BARRIDO[[paste(nm_op, "(ii)")]] <- regla_de(OPS_EST[[nm_op]], "ii") }
TAB_EST <- mide(BARRIDO)
APL_EST <- mide(lapply(OPS_EST, function(fr) { force(fr); function(x) as.numeric(aplica_r(x, fr(celdas(x)))) }))
writeLines("  S2b · barrido del estímulo, por fila de la tabla (acierto en %, mezcla / CURSOS / PROMEDIO / canónica; «aplica»: alguna opción cita el resultado):")
for (nm_op in names(OPS_EST)) {
  z <- round(100 * c(TAB_EST[paste(nm_op, "(i)"), ], TAB_EST[paste(nm_op, "(ii)"), ], APL_EST[nm_op, ]), 1)
  writeLines(do.call(sprintf, c(list("    %-26s (i) %5.1f %5.1f %5.1f %5.1f · (ii) %5.1f %5.1f %5.1f %5.1f · aplica %5.1f %5.1f %5.1f %5.1f", nm_op), as.list(z))))
}
ok(isTRUE(all.equal(TAB_EST["|a1 - a2| (i)", ], TAB_H1[R_RESTA, ])),
   "S2b: el barrido reproduce «resta» con sus propias celdas: «|a1 - a2| (i)» es la misma regla, y se juzga una sola vez, en H1")
np_est <- juzga_s2b(TAB_EST[rownames(TAB_EST) != "|a1 - a2| (i)", , drop = FALSE], "S2b barrido del estímulo")
writeLines(sprintf("  S2b barrido: %d operaciones (24 de un paso entre dos celdas y 4 de dos pasos) × 2 lecturas × 4 estratos; %d cifras por encima de +8 pp sobre el azar, pendientes de firma (sin «|a1 - a2| (i)», que es «resta»)",
                   length(OPS_EST), np_est))
writeLines("  S2b: ninguna regla de un paso equivale a resolver el ítem. Los dos productos de fila juntos (n1 * a1 y n2 * a2) son los dos totales, y compararlos es resolverlo; cada uno por separado es «un solo total» y se mide.")

source(file.path(REPO, ".claude/scripts/bateria_referencia_icfes.R"))        # B2: 34 reglas de referencia
sede_txt <- function(s) sub(" gan[oó] .*$", "", s)
grupo <- function(l) if (all(l) || !any(l)) NA else l
par_por_sede <- function(o, f) {
  s <- vapply(o, sede_txt, ""); v <- vapply(o, cif, 0); keep <- rep(TRUE, length(o))
  for (u in unique(s)) { i <- which(s == u)
    if (length(i) == 2 && all(!is.na(v[i])) && v[i[1]] != v[i[2]]) keep[i[v[i] != f(v[i])]] <- FALSE }
  grupo(keep)
}
toks3 <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
I_REGLAS <- list(   # congeladas el 2026-10-29, antes de medir (§P7-C)
  nueva_regla("par por sede: menor cifra", "magnitud", function(o) par_por_sede(o, min)),
  nueva_regla("par por sede: mayor cifra", "magnitud", function(o) par_por_sede(o, max)),
  nueva_regla("descartar sin cifra", "formato", function(o) grupo(!is.na(vapply(o, cif, 0)))),
  nueva_regla("elegir la única sin cifra", "formato", function(o) { l <- is.na(vapply(o, cif, 0)); if (sum(l) == 1) which(l) else NA }),
  nueva_regla("quedarse con 'total'", "lexico", function(o) grupo(grepl("total", o, fixed = TRUE))),
  nueva_regla("descartar 'total'", "lexico", function(o) grupo(!grepl("total", o, fixed = TRUE))),
  nueva_regla("quedarse con 'promedio'", "lexico", function(o) grupo(grepl("promedio", o, fixed = TRUE))),
  nueva_regla("descartar 'promedio'", "lexico", function(o) grupo(!grepl("promedio", o, fixed = TRUE))),
  nueva_regla("quedarse con 'de más'", "lexico", function(o) grupo(grepl("de más", o, fixed = TRUE))),
  nueva_regla("descartar 'de más'", "lexico", function(o) grupo(!grepl("de más", o, fixed = TRUE))),
  nueva_regla("cifra compartida (relacional)", "magnitud", function(o) {
    v <- vapply(o, cif, 0); grupo(!is.na(v) & v %in% v[duplicated(v)]) }),
  nueva_regla("cifra mediana", "magnitud", function(o) {
    v <- vapply(o, cif, 0); i <- which(!is.na(v)); if (length(i) < 3) NA else i[order(v[i])][ceiling(length(i) / 2)] }),
  nueva_regla("par con más palabras en común (relacional)", "lexico", function(o) {
    T <- lapply(o, toks3); best <- -1; bp <- NA
    for (i in 1:3) for (j in (i + 1):4) { k <- length(intersect(T[[i]], T[[j]]))
      if (k > best) { best <- k; bp <- c(i, j) } else if (k == best) bp <- NA }
    if (anyNA(bp)) NA else bp }),
  nueva_regla("sede de la opción más larga (relacional)", "magnitud", function(o) {
    s <- vapply(o, sede_txt, ""); grupo(s == s[which.max(nchar(o))]) }))
REGLAS <- c(B2, I_REGLAS)
NA_FAM <- c(signo = "todas las cifras son positivas y ninguna opción niega ni compara con desigualdades")
## Regla #22 §P7-G (S4, ciclo 2026-11-02): la batería solo recibe las opciones. Los canales del estímulo (el
## enunciado y la tabla) se miden aparte, en las sondas de E4, que sí ven la tabla.
## S2b (H1 de la FASE 2C r6): la declaración de la S4 decía que E4 medía los canales de la tabla, y no medía la resta de
## los promedios («resta»). Ahora dice exactamente qué mide E4 y qué no. Declararlo no mueve ninguna cifra.
ESTIMULO_SC <- paste(
  "E4 mide aparte el estímulo, sobre la tabla tal como se muestra: las cuatro celdas citadas tal cual (S1 y S1b); el",
  "orden de las filas (S2); la sede por su perfil de cursos o de promedio (S3 a S6); la resta de los promedios,",
  "|aG - aP| («resta», S2b), sola y con S_U y las pistas firmadas en «O-1 corregido»; y las operaciones de un paso entre",
  "dos celdas (suma, resta, producto y cociente entero), con las lecturas (i) y (ii), más las cuatro de dos pasos que usa",
  "el ítem (barrido, S2b). No mide otras operaciones de dos o más pasos ni el texto del enunciado fuera de los nombres",
  "de las sedes. En la canónica, regla por regla (decisión 2 a y S2b)")
BAT_S2C <- list()
for (est in list(list("mezcla natural", NAT), list("rama CURSOS", CU$CURSOS), list("rama PROMEDIO", CU$PROMEDIO))) {
  V <- est[[2]]; if (length(V) < 20) { cat("  ", est[[1]], ": NO CONCLUYENTE (n < 20)\n"); next }
  res <- evaluar_bateria(REGLAS, lapply(V, function(x) norm_ed(x$opciones)), vapply(V, `[[`, 0L, "clave"),
                         familias_no_aplicables = NA_FAM, estimulo_no_aplicable = ESTIMULO_SC)
  top <- head(res$detalle, 4)
  cat(sprintf("  batería §P7 (%d reglas) %-14s n=%d: máx %.1f %% (%s), techo nulo %.1f %%, exceso %+.1f pp -> %s\n",
              length(REGLAS), est[[1]], length(V), 100 * res$max_obs, res$regla_top, 100 * res$techo_nulo,
              100 * res$exceso, res$veredicto))
  cat(sprintf("      top: %s\n", paste(sprintf("%s %.1f%%", top$regla, 100 * top$tasa), collapse = " · ")))
  if (est[[1]] == "mezcla natural") VEREDICTO_MEZCLA <- res
  BAT_S2C[[est[[1]]]] <- list(max = res$max_obs, techo = res$techo_nulo, exceso = res$exceso, regla = res$regla_top, veredicto = res$veredicto)
  ## H2 de la FASE 2C r3: el canal de largo depende del bloque de semillas, así que su margen §P7-B se
  ## mide en TODO estrato, supere o no +8 pp con esta muestra; un margen >= 15 % sería explotable.
  mg_e <- function(f) vapply(V, function(x) { L <- nchar(norm_ed(x$opciones)); k <- x$clave
    if (sum(L == f(L)) != 1 || L[k] != f(L)) return(NA_real_); abs(L[k] - f(L[-k])) / f(L[-k]) }, 0)
  mmax_e <- max(c(mg_e(max), mg_e(min), 0), na.rm = TRUE)
  t_larga <- res$detalle$tasa[res$detalle$regla == "la mas larga (chars)"]
  ok(mmax_e < 0.15, sprintf("§P7-B %s: «la más larga» acierta %.1f %%; margen máximo de largo %.1f %% < 15 %% (inexplotable)",
                            est[[1]], 100 * t_larga[1], 100 * mmax_e))
  if (res$exceso <= 0.08) {
    ok(TRUE, sprintf("§P7-A %s: exceso %+.1f pp (obliga > +8)", est[[1]], 100 * res$exceso))
  } else {
    ## §P7-B: un canal de LARGO con margen < 15 % es inexplotable. Se vuelve a medir sin las reglas
    ## de largo y se exige el margen; si las dos cosas se cumplen, es residuo declarado, no defecto.
    LARGO <- c("la mas larga (chars)", "la mas corta (chars)", "molde atipico (n tokens)",
               "sede de la opción más larga (relacional)")
    res2 <- evaluar_bateria(Filter(function(r) !r$nombre %in% LARGO, REGLAS), lapply(V, function(x) norm_ed(x$opciones)),
                            vapply(V, `[[`, 0L, "clave"), familias_no_aplicables = NA_FAM,
                            estimulo_no_aplicable = ESTIMULO_SC)
    ## Margen §P7-B en caracteres: la clave como única más larga y como única más corta.
    marg <- function(f) vapply(V, function(x) { L <- nchar(norm_ed(x$opciones)); k <- x$clave
      if (sum(L == f(L)) != 1 || L[k] != f(L)) return(NA_real_); abs(L[k] - f(L[-k])) / f(L[-k]) }, 0)
    mmax <- max(c(marg(max), marg(min), 0), na.rm = TRUE)
    ## H5 (FASE 2C, ronda 1): «molde atipico» es de la familia formato y cuenta palabras; su margen se
    ## mide en palabras, con el mismo 15 %: la clave elegida frente a la opción de conteo más cercano.
    mt <- vapply(V, function(x) { k <- vapply(norm_ed(x$opciones), ntok, 0L); dv <- abs(k - stats::median(k))
      if (all(dv == 0) || which.max(dv) != x$clave) return(NA_real_)
      cerca <- k[-x$clave][which.min(abs(k[-x$clave] - k[x$clave]))]; abs(k[x$clave] - cerca) / cerca }, 0)
    tmax <- max(c(mt, 0), na.rm = TRUE)
    cat(sprintf("  margen de «molde atipico» en palabras (%s): acierta en %d versiones, mediano %.1f %%, máximo %.1f %%\n",
                est[[1]], sum(!is.na(mt)), 100 * stats::median(c(mt, NA), na.rm = TRUE), 100 * tmax))
    if (res2$exceso <= 0.08 && mmax < 0.15 && tmax < 0.15) {
      RESIDUOS <- c(RESIDUOS, sprintf("§P7-B %s: %+.1f pp por reglas de largo, margen máximo %.1f %% en caracteres y %.1f %% en palabras (inexplotable)",
                                      est[[1]], 100 * res$exceso, 100 * mmax, 100 * tmax))
      cat(sprintf("  RESIDUO DECLARADO §P7-B (%s): exceso %+.1f pp por reglas de largo con margen máximo %.1f %% en caracteres y %.1f %% en palabras, < 15 %% (inexplotable); sin ellas, exceso %+.1f pp (%s)\n",
                  est[[1]], 100 * res$exceso, 100 * mmax, 100 * tmax, 100 * res2$exceso, res2$regla_top))
      ## H1 de la FASE 2C r4: «O-1 corregido» declara este canal con sus cifras; superarlas vuelve al profesor.
      msg_l <- sprintf("canal de largo (%s): %+.1f pp, márgenes %.1f %% y %.1f %%; declarado hasta +10,4 pp, 2,7 %% y 6,5 %%",
                       est[[1]], 100 * res$exceso, 100 * mmax, 100 * tmax)
      if (hasta(res$exceso, FIRMA_LARGO[["exceso"]]) && hasta(mmax, FIRMA_LARGO[["chars"]]) &&
          hasta(tmax, FIRMA_LARGO[["palabras"]])) ok(TRUE, paste("§P7-B, dentro de lo declarado en O-1 corregido:", msg_l)) else pendiente(msg_l)
    } else falla(sprintf("§P7-A %s: exceso %+.1f pp (obliga > +8); sin reglas de largo %+.1f pp, margen máx %.1f %% (caracteres) y %.1f %% (palabras)",
                         est[[1]], 100 * res$exceso, 100 * res2$exceso, 100 * mmax, 100 * tmax))
  }
}
if (length(CANON_V)) {
  sc_reg <- vapply(REGLAS, function(r) mean(vapply(CANON_V, function(x) {
    S <- tryCatch(.conjunto_de(r$fn(norm_ed(x$opciones)), 4L), error = function(e) rep(FALSE, 4)); sc(S, x$clave) }, 0)), 0)
  ord <- order(-sc_reg)[1:5]
  cat(sprintf("  canónica (enumeración exacta, 24 órdenes): máx %.1f %% (%s); exceso en la mezcla = 0,10 x %+.1f pp = %+.1f pp\n",
              100 * max(sc_reg), REGLAS[[which.max(sc_reg)]]$nombre, 100 * (max(sc_reg) - 0.25), 10 * (max(sc_reg) - 0.25)))
  cat("      top:", paste(sprintf("%s %.1f%%", vapply(REGLAS[ord], `[[`, "", "nombre"), 100 * sc_reg[ord]), collapse = " · "), "\n")
}
## ---- Decisión 2 a (S3, ciclo 2026-11-02): la canónica, regla por regla -----------------------------
## Decisión del profesor del 2026-10-31 (tanda 3 del CLOZE, .claude/CLAUDE.md local): override de la canónica
## REGLA POR REGLA, por dominancia (en la canónica, el atajo firmado de «O-1 corregido» acierta el 100 %).
## Cifras EXACTAS de los 24 órdenes de las opciones del impreso (cloze/salida/p1_canonica_exacta_96cfcf35.txt;
## por E1 del CLOZE son las de este SCHOICE): 12 reglas de la batería §P7 y 12 sondas de E4, en pp sobre el
## 25 % del azar, comparadas redondeadas a un decimal, como se firmaron. Son FIRMA_P1_CANON y FIRMA_P1_SONDAS
## de cloze/verificar_espacio_cloze.R (E4). Dentro: «ROJO, cubierto» (exit 4), nunca PASS. Por encima, o con
## cualquier otra regla o sonda > +8 pp en la canónica: PENDIENTE (exit 3). Sustituye a la firma por el máximo
## (0,10 × 75 pp) de «O-1 corregido» (aclaración ii de §P7-E). Las nueve que están al 100 % (+75,0 pp) no
## pueden subir: no admiten mutante, y se declara. MP1 y MP2 (E5) sacan de «cubierto» dos de las que están al 50 %.
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
perms <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perms(v[-i]), function(p) c(v[i], p))))
canon_v <- function(CA) lapply(perms(1:4), function(p) { x <- CA; x$opciones <- CA$opciones[p]; x$sede_o <- CA$sede_o[p]
  x$rol <- CA$rol[p]; x$nom_o <- CA$nom_o[p]; x$clave <- which(p == CA$clave); x })
canon_p1 <- function(CV) {
  r <- vapply(REGLAS, function(rg) mean(vapply(CV, function(x) {
    S <- tryCatch(.conjunto_de(rg$fn(norm_ed(x$opciones)), 4L), error = function(e) rep(FALSE, 4)); sc(S, x$clave) }, 0)), 0)
  names(r) <- vapply(REGLAS, function(rg) rg$nombre, "")
  s <- as.matrix(sondas(CV))[, 1]; s <- s[!grepl("frecuencia", names(s))]
  list(reglas = 100 * (r - 0.25), sondas = 100 * (s - 0.25))
}
veredicto_p1c <- function(exc, firma) vapply(names(exc), function(nm) { e <- exc[[nm]]
  if (e <= 8 + 1e-9) "ok" else if (nm %in% names(firma) && round(e, 1) <= firma[[nm]]) "cubierto" else "pendiente" }, "")
if (is.null(CANON)) falla("decisión 2 a: no se encontró la canónica; no se puede juzgar") else {
  C1 <- canon_p1(canon_v(CANON))
  ok(all(names(FIRMA_P1_CANON) %in% names(C1$reglas)) && all(names(FIRMA_P1_SONDAS) %in% names(C1$sondas)),
     "decisión 2 a: las 12 reglas y las 12 sondas firmadas existen en la batería y en E4")
  for (cl in list(list("regla", C1$reglas, FIRMA_P1_CANON), list("sonda", C1$sondas, FIRMA_P1_SONDAS))) {
    vv <- veredicto_p1c(cl[[2]], cl[[3]])
    for (nm in names(vv)[vv != "ok"]) {
      m <- sprintf("canónica, %s «%s»: %+.1f pp exactos (24 órdenes); peso en la mezcla %+.2f pp", cl[[1]], nm, cl[[2]][[nm]], cl[[2]][[nm]] / 10)
      if (vv[[nm]] == "cubierto") firmado(paste0(m, sprintf(" (firmado hasta %+.1f pp)", cl[[3]][[nm]])), por = "la decisión 2 a")
      else pendiente(paste0(m, if (nm %in% names(cl[[3]])) sprintf(", por encima de lo firmado (%+.1f pp)", cl[[3]][[nm]]) else
                                 ", fuera de lo firmado en la decisión 2 a"))
    }
    igual <- vapply(names(cl[[3]]), function(nm) isTRUE(round(cl[[2]][[nm]], 1) == cl[[3]][[nm]]), TRUE)
    cat(sprintf("  decisión 2 a, %ss: %d firmadas, %d iguales a lo firmado (a un decimal)%s; por encima de +8 pp en la canónica: %d\n",
                cl[[1]], length(cl[[3]]), sum(igual), if (all(igual)) "" else paste0(" — distintas: ", paste(names(igual)[!igual], collapse = ", ")),
                sum(vv != "ok")))
  }
  nl_can <- mean(vapply(canon_v(CANON), function(x) { n <- nchar(x$opciones); sc(n != max(n), x$clave) }, 0))
  cat(sprintf("  RESIDUO DECLARADO (decisión 2 a: fuera de la batería congelada, §P7-C): en la canónica, «descarta la más larga» %.1f %% (%+.1f pp)\n",
              100 * nl_can, 100 * (nl_can - 0.25)))
}

## ==== S2c (§P7-F): medidas adicionales del candidato ===========================================================
cat("\n[S2c] Medidas adicionales del candidato", S2C_ID, "\n")
lee_iii <- function(x, r) { if (is.na(r)) return(0.25); v <- vapply(x$opciones, cif, 0); h <- which(v %in% r)
  if (length(h) != 1L) return(0.25); sc(x$sede_o != x$sede_o[h], x$clave) }
R_INV <- "resta (iii): la sede de la opción que cita |aG - aP| pierde; azar entre las dos de la otra"
TAB_INV <- mide(setNames(list(function(x) lee_iii(x, delta_t(x))), R_INV))
## D2 deja una opción de G sin cifra: el canal de formato con S_U (descartar la que no cita cifra) y el atajo completo.
R_SC <- c("S_U + descartar la opción sin cifra", "S_U + S1 + suma + atípica + resta + descartar sin cifra")
TAB_SC <- mide(setNames(list(
  function(x) { v <- vapply(x$opciones, cif, 0); sc(filtra(su_set(x), is.na(v)), x$clave) },
  function(x) { v <- vapply(x$opciones, cif, 0); S <- filtra(filtra(filtra(su_set(x), v %in% x$tabla), es_suma(v)), atipica(v))
    sc(filtra(filtra(S, v %in% delta_t(x)), is.na(v)), x$clave) }), R_SC))
writeLines("  S2c · canal de formato con S_U (relevante en D2):"); print(round(100 * TAB_SC, 1))
writeLines("  S2c · lectura (iii) de «resta» (fuera de la batería congelada, §P7-C: solo para comparar candidatos):")
print(round(100 * TAB_INV, 1))
BARR3 <- list()
for (nm_op in names(OPS_EST)) BARR3[[paste(nm_op, "(iii)")]] <- local({ f0 <- OPS_EST[[nm_op]]; function(x) lee_iii(x, f0(celdas(x))) })
TAB_EST3 <- mide(BARR3)
sobre8 <- function(T) { w <- which(T - 0.25 > 0.08 + 1e-9, arr.ind = TRUE); if (!length(w) || !nrow(w)) character(0) else
  sprintf("%s [%s] %.1f %% (%+.1f pp)", rownames(T)[w[, 1]], colnames(T)[w[, 2]], 100 * T[w], 100 * (T[w] - 0.25)) }
S_H1 <- sobre8(TAB_H1); S_INV <- sobre8(TAB_INV)
S_EST <- sobre8(TAB_EST[rownames(TAB_EST) != "|a1 - a2| (i)", , drop = FALSE]); S_EST3 <- sobre8(TAB_EST3)
writeLines(sprintf("  S2c H1 (RS1-RS7): %d cifras > +8 pp", length(S_H1)))
writeLines(sprintf("  S2c barrido (i)/(ii), sin «|a1 - a2| (i)»: %d cifras > +8 pp: %s", length(S_EST), paste(S_EST, collapse = " · ")))
writeLines(sprintf("  S2c barrido (iii): %d cifras > +8 pp: %s", length(S_EST3), paste(S_EST3, collapse = " · ")))
quien_delta <- function(V) { V <- Filter(function(x) !isTRUE(x$canon), V); if (!length(V)) return(c(G = NA, P = NA, ninguna = NA))
  z <- vapply(V, function(x) { v <- vapply(x$opciones, cif, 0); h <- which(v %in% delta_t(x)); if (!length(h)) "ninguna" else x$sede_o[h[1]] }, "")
  100 * c(G = mean(z == "G"), P = mean(z == "P"), ninguna = mean(z == "ninguna")) }
QD <- sapply(list(mezcla = NAT, CURSOS = CU$CURSOS, PROMEDIO = CU$PROMEDIO), quien_delta)
writeLines("  S2c · quién cita δ = |aG - aP| en las no canónicas (%):"); print(round(QD, 1))
TODAS <- c(NAT, CU$CURSOS, CU$PROMEDIO); NC <- Filter(function(x) !isTRUE(x$canon), TODAS)
F1 <- c(semillas = 100 + (S2C_E2_ULT - 1001), abortos = length(grep("^semilla", fallos)),
        padv_delta = sum(vapply(NC, `[[`, TRUE, "s2c_pd")), psin_delta = sum(vapply(NC, `[[`, TRUE, "s2c_sd")),
        cascada = sum(vapply(NC, `[[`, TRUE, "s2c_fb")), no_canonicas = length(NC))
writeLines(sprintf("  S2c F1: %d semillas corridas en E1 y E2; abortos: %d; de las %d no canónicas guardadas, padv con δ: %d, psin con δ: %d, cascada (P sin δ por falta de lado): %d",
                   F1[["semillas"]], F1[["abortos"]], F1[["no_canonicas"]], F1[["padv_delta"]], F1[["psin_delta"]], F1[["cascada"]]))
DIV <- sapply(list(mezcla = Filter(function(x) !isTRUE(x$canon), NAT), CURSOS = CU$CURSOS, PROMEDIO = CU$PROMEDIO), function(V) {
  Dv <- vapply(V, `[[`, 0, "D"); tup <- vapply(V, function(x) paste(x$tabla, collapse = ","), "")
  c(n = length(V), D_distintos = length(unique(Dv)), tablas_distintas = length(unique(tup)),
    D_mas_frecuente_pct = round(100 * max(table(Dv)) / length(Dv), 1), claves_distintas = length(unique(vapply(V, `[[`, "", "clave_txt")))) })
writeLines("  S2c · diversidad sustantiva rápida (no canónicas):"); print(DIV)
O1 <- do.call(rbind, lapply(c("CURSOS", "PROMEDIO"), function(b) {
  fl <- c("S_U la sede de la razón sin sustento pierde", rownames(FIRMA_O1), if (b == "PROMEDIO") "S1 descarta cifras de la tabla")
  lim <- c(0.5, FIRMA_O1[, b], if (b == "PROMEDIO") 0.25 + FIRMA_S1_PROMEDIO)
  data.frame(rama = b, regla = fl, medido = round(100 * TAB[fl, b], 1), firmado = round(100 * lim, 1),
             excede = !hasta(TAB[fl, b], lim), stringsAsFactors = FALSE, row.names = NULL) }))
writeLines(sprintf("  S2c · las 13 cifras de «O-1 corregido»: %d por encima de lo firmado", sum(O1$excede))); print(O1)
D2A <- if (exists("C1")) c(reglas_iguales = sum(vapply(names(FIRMA_P1_CANON), function(nm) isTRUE(round(C1$reglas[[nm]], 1) == FIRMA_P1_CANON[[nm]]), TRUE)),
  sondas_iguales = sum(vapply(names(FIRMA_P1_SONDAS), function(nm) isTRUE(round(C1$sondas[[nm]], 1) == FIRMA_P1_SONDAS[[nm]]), TRUE))) else c(NA, NA)
writeLines(sprintf("  S2c · decisión 2 a: %d de 12 reglas y %d de 12 sondas iguales a lo firmado", D2A[1], D2A[2]))
if (isTRUE(env0$D2_SIN_CIFRA)) {
  v2 <- 0L; n2 <- 0L
  for (b in c("CURSOS", "PROMEDIO")) { esp <- esp_s2c(b); pG <- if (b == "CURSOS") "M" else "P"
    for (i in seq_len(nrow(esp))) { f <- esp[i, ]; d <- list(nG = f$nG, nP = f$nP, aG = f$aG, aP = f$aP)
      for (m in c(if (pG == "M") c("M_a", "M_b") else c("P_a", "P_b"), "N_a", "N_b")) { n2 <- n2 + 1L
        if (env0$verdad(list(molde = m, sede = "G", x = NA_real_, perfil = env0$MOLDES[[m]]$perfil, afirma = "dif_igual_delta"), d)) v2 <- v2 + 1L } } }
  ok(v2 == 0L, sprintf("D2, L-2: la afirmación de ERR-RESTA sin cifra es falsa en las %d configuraciones (%d verdaderas)", n2, v2))
}
codigo_s2c <- if (length(fallos)) 1L else if (length(PENDIENTES)) 3L else if (length(FIRMADOS)) 4L else 0L
saveRDS(list(id = S2C_ID, TAB = TAB, TAB_H1 = TAB_H1, TAB_INV = TAB_INV, TAB_SC = TAB_SC, TAB_EST = TAB_EST, TAB_EST3 = TAB_EST3, APL_EST = APL_EST,
             QD = QD, F1 = F1, DIV = DIV, O1 = O1, D2A = D2A, CUOTA = CUOTA, BAT = BAT_S2C, PENDIENTES = PENDIENTES,
             FIRMADOS = FIRMADOS, fallos = fallos, S_EST = S_EST, S_EST3 = S_EST3, S_H1 = S_H1, S_INV = S_INV,
             E3 = c(combinaciones = sum(filas), violaciones = viol, sin_y = sin_y, fuera_g = fuera_g, textos = n_txt,
                    admiten_total = adm_total, clausula = clausula), codigo = codigo_s2c),
        file.path(S2C_DIR, sprintf("res_%s.rds", S2C_ID)))
cat("\n=== RESULTADO S2c (", S2C_ID, "): ", if (length(fallos)) sprintf("FALLA (%d)", length(fallos)) else if (length(PENDIENTES))
  sprintf("PENDIENTE DE FIRMA (%d)", length(PENDIENTES)) else if (length(FIRMADOS)) "EN ROJO bajo override firmado" else "sin canales en rojo",
  " ===\n", sep = "")
if (length(fallos)) { cat("FALLAS:\n"); cat(paste0("  - ", fallos, "\n"), sep = "") }
quit(status = codigo_s2c)
