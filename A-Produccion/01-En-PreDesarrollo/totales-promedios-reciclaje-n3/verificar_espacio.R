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
for (b in names(CU)) ok(length(CU[[b]]) >= 20, sprintf("estrato %s: n = %d (n < 20 sería NO CONCLUYENTE)", b, length(CU[[b]])))
ok(!is.null(CANON), "se encontró la instancia canónica")
## H5 de la FASE 2C r5 (tanda S2a): el ok() de S7 era tautológico (el lado de la canónica salía de sus cifras, y la
## guarda verbatim ya fija x = 40 y D = 20). Lo sustituye una guarda real del canal 2 en las no canónicas de E1 y de los
## estratos: la cifra falsa de G cae del lado de D que se sorteó para ella (lado_gdis). M40 quita el filtro
## «cand_g <- cand_g[lado_ok, ]» y la tiene que ver esta guarda.
lado_cumple <- function(x) isTRUE(x$canon) || isTRUE((x$X < x$D) == (x$lado == "debajo"))
VL <- Filter(function(x) !isTRUE(x$canon), c(NAT, CU$CURSOS, CU$PROMEDIO))
n_lado_mal <- sum(!vapply(c(NAT, CU$CURSOS, CU$PROMEDIO), lado_cumple, TRUE))
ok(n_lado_mal == 0L, sprintf("canal 2 (H5 r5): en las %d no canónicas de E1 y de los estratos, la cifra falsa de G cae del lado de D sorteado para ella (debajo en %d, encima en %d); fuera de su lado: %d",
                             length(VL), sum(vapply(VL, function(x) identical(x$lado, "debajo"), TRUE)),
                             sum(vapply(VL, function(x) identical(x$lado, "encima"), TRUE)), n_lado_mal))

## ---- E3. Enumeración exacta del espacio (L-2.4) --------------------------------
cat("\n[E3] Enumeración del espacio de parámetros\n")
env0 <- correr(1L)$env
viol <- 0L; filas <- c(CURSOS = 0L, PROMEDIO = 0L); trampa_promedio <- 0L; sin_y <- 0L; fuera_g <- 0L
for (b in c("CURSOS", "PROMEDIO")) {
  esp <- env0$espacio_rama(b); filas[[b]] <- nrow(esp)
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
  esp <- env0$espacio_rama(b); pG <- if (b == "CURSOS") "M" else "P"; pP <- if (b == "CURSOS") "P" else "M"
  mtot <- function(p) if (p == "M") c("M_a", "M_b") else c("P_a", "P_b")
  for (i in seq_len(nrow(esp))) {
    f <- esp[i, ]; d <- list(nG = f$nG, nP = f$nP, aG = f$aG, aP = f$aP)
    casos <- c(
      do.call(c, lapply(c(mtot(pG), "N_a", "N_b"), function(m)
        lapply(unique(c(f$D, f$aG, f$aP, f$delta, f$parcialG, f$TG)), function(x) list(m = m, x = x, sede = "G")))),
      lapply(mtot(pP), function(m) list(m = m, x = f$parcialP, sede = "P")),
      lapply(c("U_a", "U_b", "U_c", "U_d"), function(m) list(m = m, x = f$D + 1, sede = "P")))
    for (cc in casos) {
      S <- if (cc$sede == "G") "la sede norte" else "la sede sur"; O <- if (cc$sede == "G") "la sede sur" else "la sede norte"
      t <- env0$MOLDES[[cc$m]]$f(S, O, cc$x); n_txt <- n_txt + 1L
      if ("total" %in% env0$lecturas_de(t)) adm_total <- adm_total + 1L
      if (grepl(ORACULO_CLAUSULA, t)) clausula <- clausula + 1L
      if (cc$sede == "G" && cc$x != f$D) {
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
    "canal 2: cifra falsa de G menor que D" = m(function(x) as.numeric(x$X < x$D)),
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
      S <- x$sede_o == "G"; v <- vapply(x$opciones, cif, 0); i <- which(S)
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
ok(abs(mean(vapply(nc, function(x) as.numeric(x$X < x$D), 0)) - 0.5) <= 0.12,
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
## enunciado y la tabla) se miden aparte, en las sondas de E4, que sí ven la tabla: S1 y S1b leen sus cifras;
## S2 y S3, sus filas; S5, S7 y S_U, las sedes que muestra, y sus combinaciones firmadas en «O-1 corregido»;
## y en la canónica, regla por regla (decisión 2 a). Declararlo no mueve ninguna cifra.
ESTIMULO_SC <- paste("los canales del enunciado y de la tabla se miden aparte, en las sondas de E4 (S1 y S1b leen",
                     "las cifras de la tabla; S2 y S3, sus filas; S5, S7 y S_U, las sedes que muestra, y sus",
                     "combinaciones) y, en la canónica, regla por regla (decisión 2 a)")
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

## ---- E5. Mutantes (Incidente P): en memoria, patrón único, sonda propia -----------
cat("\n[E5] Mutantes\n")
mutar <- function(cambios) {
  txt <- CODIGO
  for (cb in cambios) {
    n <- lengths(regmatches(txt, gregexpr(cb[1], txt, fixed = TRUE)))
    if (n != 1L) return(list(error = sprintf("el patrón «%s» aparece %d veces (se exige 1)", substr(cb[1], 1, 50), n)))
    txt <- sub(cb[1], cb[2], txt, fixed = TRUE)
  }
  list(expr = parse(text = txt, encoding = "UTF-8"))
}
MUT <- list(
  list(id = "M1", sonda = "L-2: una sola afirmación verdadera", desc = "revierte la construcción de la unicidad (filtros de D)",
       cambios = list(c("ok <- ok & g$D != g$aG & g$D != g$aP ", "ok <- ok "),
                      c("cand_g <- cand_g[cand_g$x != d$D, ]", "cand_g <- cand_g"),
                      c("cand_g$x < d$D else cand_g$x > d$D", "cand_g$x <= d$D else cand_g$x >= d$D")),
       llego = function(e) any(e$espacio_rama("CURSOS")$D == e$espacio_rama("CURSOS")$aG)),
  list(id = "M2", sonda = "L-3: cursos y promedio en sentidos opuestos", desc = "permite la dominancia en la rama CURSOS",
       cambios = list(c("g[g$nG > g$nP & g$aG < g$aP, ]", "g[g$nG > g$nP & g$aG < g$aP + 12, ]")),
       llego = function(e) any(e$espacio_rama("CURSOS")$aG > e$espacio_rama("CURSOS")$aP)),
  list(id = "M3", sonda = "L-1: reparto 2 + 2", desc = "la razón sin sustento nombra a G",
       cambios = list(c('psin  = afirmacion(molde_psin, "P", y, "ERR-INVENTA"))', 'psin  = afirmacion(molde_psin, "G", y, "ERR-INVENTA"))')),
       llego = function(e) isTRUE(e$es_canonica) || identical(e$afs$psin$sede, "G")),
  list(id = "M4", sonda = "L-1: canónica verbatim (opciones)", desc = "normaliza la rareza «la sede de secundaria» (H-2)",
       cambios = list(c("que la sede de secundaria.", "que la sede secundaria.")),
       llego = function(e) grepl("que la sede secundaria.", e$CANON_OPCIONES[["A"]], fixed = TRUE)),
  list(id = "M5", sonda = "L-1: canónica verbatim (tabla)", desc = "cambia una celda de la canónica (45 -> 46)",
       cambios = list(c("d <- list(nG = 5, nP = 4, aG = 40, aP = 45)", "d <- list(nG = 5, nP = 4, aG = 40, aP = 46)")),
       llego = function(e) !isTRUE(e$es_canonica) || e$d$aP == 46),
  list(id = "M6", sonda = "L-5: P_CANON <= 0,10", desc = "sube la proporción de la canónica",
       cambios = list(c("P_CANON <- 0.10", "P_CANON <- 0.20")), llego = function(e) e$P_CANON == 0.20),
  list(id = "M7", sonda = "L-1: sin vocabulario de error", desc = "mete «estudiantes» en un molde",
       cambios = list(c("hay más alumnos para recoger", "hay más estudiantes para recoger")),
       llego = function(e) grepl("estudiantes", e$MOLDES$U_c$f("S", "O", 1))),
  list(id = "M8", sonda = "L-3: la canónica va en la rama CURSOS", desc = "pone la canónica en la rama PROMEDIO",
       cambios = list(c('rama <- if (es_canonica) "CURSOS" else', 'rama <- if (es_canonica) "PROMEDIO" else')),
       llego = function(e) !isTRUE(e$es_canonica) || e$rama == "PROMEDIO"),
  list(id = "M9", sonda = "pregunta del impreso", desc = "cambia VERDADERA por verdadera",
       cambios = list(c("afirmaciones es VERDADERA?", "afirmaciones es verdadera?")),
       llego = function(e) grepl("verdadera\\?$", e$PREGUNTA)),
  list(id = "M10", sonda = "criterio: gana la mayor cantidad total", desc = "una plantilla omite el criterio del concurso",
       cambios = list(c('"la mayor cantidad de ", m', '"la mayor parte de ", m')),
       llego = function(e) grepl("mayor parte", e$contextos[[6]]$plantilla("sede", "papel", "x"))),
  ## M11 rehecho en la ronda 2: con H1, la cifra sin sustento ya no puede repetir otra (calculos_tabla()
  ## la excluye antes), así que el canal 8 se prueba por el lado de G, quitando el filtro del canal 3.
  list(id = "M11", sonda = "canal 8: cifras distintas entre opciones", desc = "quita el filtro del canal 3: la cifra de G puede repetir la de P",
       cambios = list(c('if (f$p_tabla) cand_g <- cand_g[cand_g$codigo != "ERR-DATO", ]', "if (FALSE) cand_g <- cand_g")),
       llego = function(e) isTRUE(e$f$p_tabla) && identical(e$afs$gdis$codigo, "ERR-DATO")),
  list(id = "M13", sonda = "H1: la cifra de la razón sin sustento", desc = "revierte la exclusión de los cálculos con la tabla (H1)",
       cambios = list(c("d$aG, d$aP, calculos_tabla(d))", "d$aG, d$aP)")),
       llego = function(e) !isTRUE(e$es_canonica) && exists("usadas", envir = e, inherits = FALSE) &&
         !all(e$calculos_tabla(e$d) %in% e$usadas)),
  list(id = "M14", sonda = "L-1: un solo verbo en el enunciado y en el encabezado", desc = "las no canónicas vuelven a «recolectado» en el encabezado",
       cambios = list(c('verbo_tabla <- if (es_canonica) "recolectado" else "recogido"', 'verbo_tabla <- "recolectado"')),
       llego = function(e) !isTRUE(e$es_canonica) && identical(e$verbo_tabla, "recolectado")),
  ## Ronda 3: M16 devuelve el cierre de la ronda 2 (lectura alternativa en todas las opciones; la clave
  ## deja de ser verdadera con «total»); M17 deja ese cierre solo en ERR-TOTAL (la cifra falsa de G se
  ## vuelve verdadera con «total»); M18 pone una cifra fuera del complemento del comparativo.
  list(id = "M16", sonda = "regla #13 v1.1: ninguna lectura que admite el texto cambia el veredicto",
       desc = "vuelve al cierre «su total fue de x kilos más que el de O» (lectura alternativa)",
       cambios = list(c('cierre <- function(x, O) paste0("su total fue más alto que el de ", O, " en ", x, ED, "kilos.")',
                        'cierre <- function(x, O) paste0("su total fue de ", x, ED, "kilos más que el de ", O, ".")')),
       llego = function(e) !isTRUE(e$es_canonica) && grepl("fue de [0-9]+[ \u00a0]kilos más que", e$afs$clave$texto)),
  list(id = "M17", sonda = "regla #13 v1.1: ninguna lectura que admite el texto cambia el veredicto",
       desc = "ERR-TOTAL con el cierre «fue de T_G kilos más que» (ERR-TOTAL y la lectura alternativa)",
       cambios = list(c('afirmacion(molde_gdis, "G", fila_g$x, cod_gdis)',
                        paste0('(function(a) { if (a$codigo == "ERR-TOTAL") a$texto <- paste0(sub(" su total fue más alto.*$", "", a$texto), ',
                               '" su total fue de ", a$x, " kilos más que el de ", nomP, "."); a })(afirmacion(molde_gdis, "G", fila_g$x, cod_gdis))'))),
       llego = function(e) identical(e$afs$gdis$codigo, "ERR-TOTAL") && grepl("fue de [0-9]+ kilos", e$afs$gdis$texto)),
  list(id = "M18", sonda = "regla #13 v1.1: la cifra va solo en el complemento",
       desc = "una razón sin sustento cita otra cifra («durante 5 días más»)",
       cambios = list(c("reciclaron durante más días que los demás", "reciclaron durante 5 días más que los demás")),
       llego = function(e) identical(e$afs$psin$molde, "U_d")),
  ## Ciclo 2026-11-02 (S1): el espacio duro. M23 lo pone en la canónica fuera de su sitio (la guarda verbatim lo
  ## compara sin él y no lo ve); M24 lo quita de la canónica; M25, de «El Bosque»; M26, del cierre de las no
  ## canónicas, que vigila la guarda del complemento.
  list(id = "M23", sonda = "S1: en la canónica, el espacio duro solo entre cifra y unidad",
       desc = "la canónica lleva un espacio duro fuera de su sitio («que la sede»)",
       cambios = list(c('"kilos más que la sede de secundaria."', '"kilos más que la", ED, "sede de secundaria."')),
       llego = function(e) grepl("que la\u00a0sede de secundaria", e$CANON_OPCIONES[["A"]], fixed = TRUE)),
  list(id = "M24", sonda = "S1: espacio duro entre cifra y unidad",
       desc = "la canónica pierde el espacio duro de «40 kilos»",
       cambios = list(c('porque recogió 40", ED, "kilos más', 'porque recogió 40 kilos más')),
       llego = function(e) grepl("recogió 40 kilos", e$CANON_OPCIONES[["A"]], fixed = TRUE)),
  list(id = "M25", sonda = "S1: espacio duro entre cifra y unidad",
       desc = "«El Bosque» vuelve al espacio normal en los nombres",
       cambios = list(c('paste0("la sede El", ED, "Bosque")', '"la sede El Bosque"')),
       llego = function(e) identical(e$CONJUNTOS[[5]]$nombres[1], "la sede El Bosque")),
  list(id = "M26", sonda = "regla #13 v1.1: la cifra va solo en el complemento",
       desc = "el cierre de las no canónicas pierde el espacio duro",
       cambios = list(c('" en ", x, ED, "kilos.")', '" en ", x, " kilos.")')),
       llego = function(e) !isTRUE(e$es_canonica) && grepl(" en [0-9]+ kilos\\.$", e$afs$clave$texto)),
  ## Tanda S1b: M30 devuelve la unidad del feedback de la clave fuera de la fórmula, como en S1 («$», ED, «kilos»).
  list(id = "M30", sonda = "L-6: la unidad que cierra una fórmula va dentro de la fórmula",
       desc = "el feedback de la clave cierra la fórmula antes de «kilos» (revierte la variante V1)",
       cambios = list(c('d$D, "\\\\ \\\\text{kilos.}$"),', 'd$D, "$", ED, "kilos."),')),
       llego = function(e) grepl("\\$\u00a0kilos\\.$", e$fb[["clave"]])),
  ## Tanda S1b relanzada: M33 saca el punto de la fórmula en el feedback de la clave («\\text{kilos}$.», como en V1).
  list(id = "M33", sonda = "L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula",
       desc = "el feedback de la clave deja el punto fuera de la fórmula (revierte V1')",
       cambios = list(c('d$D, "\\\\ \\\\text{kilos.}$"),', 'd$D, "\\\\ \\\\text{kilos}$."),')),
       llego = function(e) grepl("\\\\text\\{kilos\\}\\$\\.$", e$fb[["clave"]])),
  ## Tanda S2a (H2 de la FASE 2C r5): M37 devuelve el punto de la explicación de ERR-RESTA fuera de su fórmula sin
  ## unidad («= 5$.»), como en 6c9d7705. La guarda del signo de la S1b solo miraba las fórmulas con unidad: no lo veía.
  list(id = "M37", sonda = "L-6: el signo que sigue a una fórmula, con unidad o sin ella, va dentro de la fórmula",
       desc = "la explicación de ERR-RESTA deja el punto fuera de su fórmula sin unidad (revierte H2)",
       cambios = list(c('p$delta, "\\\\text{.}$ Esa resta', 'p$delta, "$. Esa resta')),
       llego = function(e) grepl("= 5$. Esa resta", e$errores_conceptuales[["ERR-RESTA"]]$explica(
         list(aG = 40, aP = 45, delta = 5, tipo = "sede", nG = 5, nP = 4, TG = 200, TP = 180, D = 20), list(x = 5)),
         fixed = TRUE)))
for (mu in MUT) {
  mm <- mutar(mu$cambios)
  if (!is.null(mm$error)) { falla(sprintf("%s MAL CONSTRUIDO: %s", mu$id, mm$error)); next }
  muerte <- NA; msg <- NA; llego <- FALSE
  for (s in seq_len(TOPE)) {
    r <- correr(s, mm$expr)
    if (!llego) llego <- isTRUE(tryCatch(mu$llego(r$env), error = function(e) FALSE))
    if (!is.null(r$err)) { muerte <- s; msg <- r$err; break }
  }
  if (!llego) falla(sprintf("%s MUTANTE MAL CONSTRUIDO: la mutación no llegó al entorno", mu$id))
  else if (is.na(muerte)) falla(sprintf("%s (%s) SOBREVIVIÓ %d semillas", mu$id, mu$desc, TOPE))
  else if (!grepl(mu$sonda, msg, fixed = TRUE)) falla(sprintf("MUTANTE %s CAZADO POR LA SONDA EQUIVOCADA: «%s»", mu$id, msg))
  else cat(sprintf("  OK: %s (%s) cazado por su sonda «%s» en la semilla %d\n", mu$id, mu$desc, mu$sonda, muerte))
}
## Mutante de medición: sin la moneda del canal 2, la sonda del canal 2 debe dispararse.
mm <- mutar(list(c("b_debajo <- pick(0:3)", "b_debajo <- 0L")))
V <- list(); for (s in 2000L + seq_len(TOPE)) { r <- correr(s, mm$expr); if (is.null(r$err) && !r$env$es_canonica) V[[length(V) + 1]] <- rasgos(r$env); if (length(V) >= N) break }
ex <- mean(vapply(V, function(x) { S <- x$sede_o == "G"; v <- vapply(x$opciones, cif, 0); i <- which(S)
  S[i[v[i] != min(v[i])]] <- FALSE; sc(S, x$clave) }, 0)) - 0.5
ok(ex > 0.08, sprintf("M12 (fija D como la menor cifra): la sonda S7 lo detecta, %+.1f pp > +8", 100 * ex))
## Mutante de medición M15 (H5): sin el filtro del canal 3, dos distractores pueden citar datos de la
## tabla, y la cota estructural de S1 (1/3 por versión) debe romperse.
mm <- mutar(list(c('if (f$p_tabla) cand_g <- cand_g[cand_g$codigo != "ERR-DATO", ]', "if (FALSE) cand_g <- cand_g")))
V <- list(); for (s in 3000L + seq_len(TOPE)) { r <- correr(s, mm$expr); if (is.null(r$err) && !r$env$es_canonica) V[[length(V) + 1]] <- rasgos(r$env); if (length(V) >= N) break }
n_c3 <- sum(!vapply(V, c3_v, TRUE))
ok(n_c3 > 0, sprintf("M15 (quita el filtro del canal 3): la guarda del canal 3 lo detecta en %d de %d versiones", n_c3, length(V)))
## Mutante de medición M40 (H5 de la FASE 2C r5): sin el filtro del lado sorteado, la cifra falsa de G puede caer del
## otro lado de D. La guarda real del canal 2 (E2) lo tiene que ver, sin que ninguna guarda del .Rmd aborte el render.
mm <- mutar(list(c("cand_g <- cand_g[lado_ok, ]", "cand_g <- cand_g")))
if (!is.null(mm$error)) falla(paste("M40 MAL CONSTRUIDO:", mm$error)) else {
  V <- list(); err40 <- 0L; llego40 <- FALSE
  for (s in seq_len(N)) {
    r <- correr(s, mm$expr); if (!is.null(r$err)) { err40 <- err40 + 1L; next }
    if (!isTRUE(r$env$es_canonica) && nrow(r$env$cand_g) > sum(r$env$lado_ok)) llego40 <- TRUE
    V[[length(V) + 1]] <- rasgos(r$env)
  }
  n40 <- sum(!vapply(V, lado_cumple, TRUE))
  ver40 <- if (!llego40) "mal_construido" else if (err40 > 0L) "cazado_por_otra" else if (n40 > 0L) "cazado_por_su_sonda" else "no_detectado"
  ok(ver40 == "cazado_por_su_sonda", sprintf("M40 (quita el filtro cand_g[lado_ok, ]): la guarda real del canal 2 lo detecta en %d de %d versiones de E1 (%d con error) — %s",
                                             n40, length(V), err40, ver40))
}
## Mutante de medición M19 (H1 de la FASE 2C r4): con ERR-TOTAL como distractor de G siempre que se
## pueda, la cifra atípica sale más a menudo y algún atajo de O-1 debe salir de lo firmado.
mm <- mutar(list(c("cod_gdis <- pick(unique(cand_g$codigo))",
                   'cod_gdis <- if ("ERR-TOTAL" %in% cand_g$codigo) "ERR-TOTAL" else pick(unique(cand_g$codigo))')))
if (!is.null(mm$error)) falla(paste("M19 MAL CONSTRUIDO:", mm$error)) else {
  CM <- list(CURSOS = list(), PROMEDIO = list())
  for (s in 1000L + seq_len(TOPE)) {
    if (all(lengths(CM) >= N)) break
    r <- correr(s, mm$expr); if (!is.null(r$err) || isTRUE(r$env$es_canonica)) next
    x <- rasgos(r$env); if (length(CM[[x$rama]]) < N) CM[[x$rama]][[length(CM[[x$rama]]) + 1]] <- x
  }
  TM <- cbind(CURSOS = sondas(CM$CURSOS), PROMEDIO = sondas(CM$PROMEDIO))[rownames(FIRMA_O1), colnames(FIRMA_O1)]
  sup <- !hasta(TM, FIRMA_O1); peor <- which.max(TM - FIRMA_O1)
  ok(any(sup), sprintf("M19 (ERR-TOTAL siempre que se pueda): sale de lo firmado en %d de %d cifras de O-1 corregido (mayor exceso: %s, rama %s, %.1f %% frente a %.1f %%)",
                       sum(sup), length(sup), rownames(TM)[row(TM)[peor]], colnames(TM)[col(TM)[peor]], 100 * TM[peor], 100 * FIRMA_O1[peor]))
}
## Mutante de medición M20 (H3 de la FASE 2C r4): una celda de la canónica cambia DESPUÉS de tabla_df.
## La guarda del .Rmd no lo ve (mira tabla_df); la sonda H3 r4 sobre tabla_md sí.
mm <- mutar(list(c('x, "</span>")', 'sub("45", "46", x), "</span>")')))
if (!is.null(mm$error)) falla(paste("M20 MAL CONSTRUIDO:", mm$error)) else {
  envM <- NULL
  for (s in 1:300) { r <- correr(s, mm$expr); if (is.null(r$err) && isTRUE(r$env$es_canonica)) { envM <- r$env; break } }
  ok(!is.null(envM) && grepl("46 kilos", envM$tabla_md, fixed = TRUE) && !canon_tabla_ok(envM),
     "M20 (cambia una celda de la canónica en tabla_md, no en tabla_df): el .Rmd no lo caza y la sonda H3 r4 sí")
}
## Mutantes de la decisión 2 a (S3): COPIAS del registro canónico con una opción retocada (la canónica es verbatim
## y su guarda de literales aborta cualquier mutante en el código). Cada uno tiene que sacar SU regla de «cubierto»,
## con la misma puerta (canon_p1 + veredicto_p1c, 24 órdenes). Son MP1 y MP2 de cloze/verificar_espacio_cloze.R (E8);
## MP2 lleva el espacio duro de S1 en su patrón.
mut_p1 <- function(id, regla, de, a) {
  i <- grep(de, CANON$opciones, fixed = TRUE)
  if (length(i) != 1L) return(invisible(falla(sprintf("%s MAL CONSTRUIDO: «%s» aparece en %d opciones de la canónica", id, de, length(i)))))
  CM <- CANON; CM$opciones[i] <- sub(de, a, CM$opciones[i], fixed = TRUE)
  ex <- canon_p1(canon_v(CM))$reglas; vv <- veredicto_p1c(ex, FIRMA_P1_CANON)
  llego <- !identical(CM$opciones, CANON$opciones) && identical(CM$clave, CANON$clave)
  ver <- if (!llego) "mal_construido" else if (vv[[regla]] == "pendiente") "cazado_por_su_sonda" else "no_detectado"
  ok(ver == "cazado_por_su_sonda", sprintf("%s (decisión 2 a; copia de la canónica, «%s» -> «%s»): %s %+.1f pp (firmado %+.1f) -> %s — %s",
                                           id, de, a, regla, ex[[regla]], FIRMA_P1_CANON[[regla]], vv[[regla]], ver))
}
if (!is.null(CANON)) {
  mut_p1("MP1", "quedarse con 'promedio'", "tuvo mayor promedio", "tuvo mayor media")
  mut_p1("MP2", "mas digitos", "recogió 40\u00a0kilos más", "recogió 4\u00a0kilos más")
  cat("  Decisión 2 a: las nueve reglas y sondas al 100 % (+75,0 pp) no pueden subir; ningún mutante puede sacarlas de «cubierto».\n")
}
## S1 (ciclo 2026-11-02), sonda S1-Sol: en las opciones, la Solution y el feedback por opción que se emiten, ninguna cifra
## (ni fórmula que cierra en $) separada de su unidad por un espacio normal, ni «El Bosque» o «La Pradera». La guarda
## de data_generation no ve la Solution, que se arma en su propio chunk. M27 quita un espacio duro de la Solution.
chunk_de <- function(lin, nm) { i0 <- grep(paste0("^```\\{r ", nm, "[,}]"), lin); i1 <- which(lin == "```"); i1 <- i1[i1 > i0][1]
  parse(text = paste(lin[(i0 + 1):(i1 - 1)], collapse = "\n"), encoding = "UTF-8") }
PAT_S1 <- "[0-9$] (kilos?|cursos?)\\b|El Bosque|La Pradera"
## Tanda S2a (H1 de la FASE 2C r5): el texto emitido se calcula una sola vez por versión del .Rmd (memoria por
## contenido), y el data_generation sale del mismo texto que las otras tres partes, así que un mutante del
## data_generation (M34, M35, M36) llega también a la Solution. Con el .Rmd sin mutar es el mismo EXPR de siempre.
MEMO_SOL <- list()
textos_sol <- function(lin, semillas) {
  for (m in MEMO_SOL) if (identical(m$lin, lin) && identical(m$sem, semillas)) return(m$t)
  ex <- chunk_de(lin, "data_generation")
  ch <- lapply(c("opciones", "solucion", "feedback"), function(nm) chunk_de(lin, nm))
  t <- vapply(semillas, function(s) { r <- correr(s, ex); if (!is.null(r$err)) return(NA_character_)
    paste(unlist(lapply(ch, function(e) capture.output(eval(e, envir = r$env)))), collapse = "\n") }, "")
  MEMO_SOL[[length(MEMO_SOL) + 1L]] <<- list(lin = lin, sem = semillas, t = t)
  t
}
cuenta_v <- function(t, pat) vapply(t, function(x) if (is.na(x)) NA_real_ else
  as.numeric(lengths(regmatches(x, gregexpr(pat, x, perl = TRUE)))), 0, USE.NAMES = FALSE)
s1_sol <- function(lin, semillas, pat = PAT_S1) sum(cuenta_v(textos_sol(lin, semillas), pat))
s1_sol_v <- function(lin, semillas, pat) cuenta_v(textos_sol(lin, semillas), pat)
n_s1 <- s1_sol(l, 1:N)
ok(identical(n_s1, 0), sprintf("S1-Sol: en las %d versiones de E1, opciones, Solution y feedback sin cifra ni nombre partido por un espacio normal (%s)", N, n_s1))
l_m27 <- sub('d$D,\n    "\\\\ \\\\text{kilos}$ a favor de "', 'd$D,\n    "$ kilos a favor de "', paste(l, collapse = "\n"), fixed = TRUE)
l_m27 <- strsplit(l_m27, "\n", fixed = TRUE)[[1]]
n_m27 <- if (identical(l_m27, l)) NA else s1_sol(l_m27, 1:N)
ok(!is.na(n_m27) && n_m27 > 0, sprintf("M27 (la Solution pierde el espacio duro de «kilos a favor de»): la sonda S1-Sol lo detecta en %s casos", n_m27))
## Tanda S1b, sonda S1-Sol fórmula: ninguna fórmula cierra en «$» seguido de la unidad. M31 revierte la variante en
## «kilos a favor de» y tiene que verlo esta sonda, no S1-Sol (con el espacio duro, S1-Sol no lo ve).
## Tanda S2a (H1 de la FASE 2C r5):
##  - Separadores. Entre el cierre de una fórmula y la unidad, o entre el cierre y el signo (S1-Sol signo), las sondas
##    admiten hasta tres caracteres cualesquiera que no sean letra ni cifra: espacio normal, U+00A0, U+2009, U+202F,
##    U+2060… La clase anterior era una lista negra (espacio normal o U+00A0) y no veía el espacio fino (M39, M41).
##  - Controles positivos exactos, deterministas con las semillas 1..100. Con un umbral (>= N, > 0) no veían que su
##    cuenta bajara: la r5 sacó la unidad de la lista de totales con U+2009, y el control pasó de 1158 a 958 en OK.
##    Cada uno lleva un mutante que quita un solo sitio (M34, M35, M36).
SIG_S1 <- "[.,:;!?)\\]»”…]"
SEP_S1 <- "[^A-Za-z0-9$\\n]{0,3}"
PAT_S1F <- paste0("\\$", SEP_S1, "(kilos?|cursos?)\\b")
PAT_S1F_VIEJA <- paste0("\\$[ ", intToUtf8(0xa0), "]+(kilos?|cursos?)\\b")
PAT_DENTRO <- paste0("\\\\ \\\\text\\{(kilos?|cursos?)", SIG_S1, "?\\}\\$")
CTRL_DENTRO <- 1158; CTRL_SIGNO <- 730; CTRL_SIN_U <- 42
n_s1f <- s1_sol(l, 1:N, PAT_S1F); n_dentro <- s1_sol(l, 1:N, PAT_DENTRO)
ok(identical(n_s1f, 0) && isTRUE(n_dentro == CTRL_DENTRO),
   sprintf("S1-Sol fórmula: en las %d versiones de E1, ninguna fórmula cierra antes de su unidad, con ningún separador (%s); unidad dentro de la fórmula: %s (control exacto: %s)",
           N, n_s1f, n_dentro, CTRL_DENTRO))
l_m31 <- sub('d$D,\n    "\\\\ \\\\text{kilos}$ a favor de "', 'd$D,\n    "$", ED, "kilos a favor de "', paste(l, collapse = "\n"), fixed = TRUE)
l_m31 <- strsplit(l_m31, "\n", fixed = TRUE)[[1]]
n_m31 <- if (identical(l_m31, l)) NA else s1_sol(l_m31, 1:N, PAT_S1F)
n_m31_s1 <- if (identical(l_m31, l)) NA else s1_sol(l_m31, 1:N)
ok(!is.na(n_m31) && n_m31 > 0 && identical(n_m31_s1, 0), sprintf("M31 (la Solution vuelve a cerrar la fórmula antes de «kilos a favor de», con el espacio duro): la sonda S1-Sol fórmula lo detecta en %s casos y S1-Sol en %s (cazado por su sonda)", n_m31, n_m31_s1))
## Sonda S1-Sol signo (tanda S1b relanzada, V1'; generalizada en la tanda S2a por la decisión del profesor «Cerrarlo
## ahora»): ninguna fórmula, con unidad o sin ella, deja fuera el signo que la sigue. Con el signo fuera, Chromium parte
## el renglón entre la fórmula y el signo (6b de V1: el punto solo en 34 semillas a 360 px y en 5 a 320 px). Hasta la
## S2a la sonda pedía la unidad («\\text{kilos}$.») y no veía el «= 5$.» de ERR-RESTA (42 casos en 100 versiones).
## Controles exactos: la unidad con el signo dentro («\\text{kilos.}$», 730) y el signo dentro de una fórmula sin
## unidad («\\text{.}$»), uno por versión con ERR-RESTA como distractor de G (42 en E1). M32, M38 y M41 prueban la sonda.
PAT_S1S <- paste0("\\$", SEP_S1, SIG_S1)
PAT_S1S_VIEJA <- paste0("\\\\text\\{(kilos?|cursos?)\\}\\$", SIG_S1)
PAT_SIGNO_DENTRO <- paste0("\\\\text\\{(kilos?|cursos?)", SIG_S1, "\\}\\$")
PAT_SIN_U <- paste0("\\\\text\\{", SIG_S1, "\\}\\$")
n_s1s <- s1_sol(l, 1:N, PAT_S1S)
v_dentro_s <- s1_sol_v(l, 1:N, PAT_SIGNO_DENTRO)
ok(identical(n_s1s, 0) && !anyNA(v_dentro_s) && all(v_dentro_s > 0) && isTRUE(sum(v_dentro_s) == CTRL_SIGNO),
   sprintf("S1-Sol signo: en las %d versiones de E1, ninguna fórmula, con unidad o sin ella, deja fuera el signo que la sigue (%s); unidad con el signo dentro en %d de %d versiones (%s en total; control exacto: %s)",
           N, n_s1s, sum(v_dentro_s > 0, na.rm = TRUE), N, sum(v_dentro_s), CTRL_SIGNO))
es_resta <- vapply(NAT, function(x) identical(x$cod_gdis, "ERR-RESTA"), TRUE)
v_sin_u <- s1_sol_v(l, 1:N, PAT_SIN_U)
ok(!anyNA(v_sin_u) && isTRUE(sum(v_sin_u) == CTRL_SIN_U) && sum(es_resta) == CTRL_SIN_U && identical(v_sin_u > 0, es_resta),
   sprintf("S1-Sol signo, fórmula sin unidad: el signo dentro («\\text{.}$») sale %s veces, en las %d versiones de E1 con ERR-RESTA como distractor de G y solo en ellas (control exacto: %s)",
           sum(v_sin_u), sum(es_resta), CTRL_SIN_U))
PAT_M32 <- '"\\\\ \\\\text{kilos.}$\\n", sep = "")'
n_p32 <- sum(lengths(regmatches(l, gregexpr(PAT_M32, l, fixed = TRUE))))
l_m32 <- sub(PAT_M32, '"\\\\ \\\\text{kilos}$.\\n", sep = "")', l, fixed = TRUE)
n_m32 <- if (n_p32 != 1L || identical(l_m32, l)) NA else s1_sol(l_m32, 1:N, PAT_S1S)
n_m32_otras <- if (is.na(n_m32)) NA else s1_sol(l_m32, 1:N) + s1_sol(l_m32, 1:N, PAT_S1F)
ok(n_p32 == 1L && !is.na(n_m32) && n_m32 > 0 && identical(n_m32_otras, 0),
   sprintf("M32 (la lista de totales del procedimiento correcto saca el punto de la fórmula; el patrón aparece %d vez): la sonda S1-Sol signo lo detecta en %s casos, y S1-Sol y S1-Sol fórmula en %s (cazado por su sonda)",
           n_p32, n_m32, n_m32_otras))
## Tanda S2a: mutantes de las sondas y de los controles exactos de S1-Sol, sobre el texto del .Rmd en memoria
## (mut_lin() exige que el patrón aparezca una sola vez). Cada uno declara su sonda: la que tiene que cazarlo, con las
## otras dos sondas en 0. Los de control quitan un solo sitio, y solo los caza su control exacto. Los caracteres
## especiales se insertan con intToUtf8(), nunca literales.
mut_lin <- function(de, a) { tx <- paste(l, collapse = "\n"); n <- lengths(regmatches(tx, gregexpr(de, tx, fixed = TRUE)))
  list(n = n, lin = if (n == 1L) strsplit(sub(de, a, tx, fixed = TRUE), "\n", fixed = TRUE)[[1]] else NULL) }
SONDAS_S1 <- c("S1" = PAT_S1, "S1-Sol fórmula" = PAT_S1F, "S1-Sol signo" = PAT_S1S)
CONTROLES_S1 <- list("unidad dentro" = list(pat = PAT_DENTRO, n = CTRL_DENTRO),
                     "unidad con el signo dentro" = list(pat = PAT_SIGNO_DENTRO, n = CTRL_SIGNO),
                     "signo dentro sin unidad" = list(pat = PAT_SIN_U, n = CTRL_SIN_U))
juzgar_s1 <- function(id, desc, de, a, sonda, vieja = NULL) {
  m <- mut_lin(de, a)
  if (is.null(m$lin)) return(invisible(falla(sprintf("%s MAL CONSTRUIDO: el patrón «%s» aparece %d veces (se exige 1)", id, substr(de, 1, 40), m$n))))
  t <- textos_sol(m$lin, 1:N)
  if (anyNA(t)) return(invisible(falla(sprintf("MUTANTE %s CAZADO POR LA SONDA EQUIVOCADA: el data_generation aborta en %d versiones", id, sum(is.na(t))))))
  s <- vapply(SONDAS_S1, function(p) sum(cuenta_v(t, p)), 0)
  cc <- vapply(CONTROLES_S1, function(z) sum(cuenta_v(t, z$pat)), 0)
  cmal <- cc != vapply(CONTROLES_S1, function(z) z$n, 0)
  ver <- if (identical(m$lin, l)) "mal_construido" else if (sonda %in% names(SONDAS_S1)) {
    if (s[[sonda]] == 0) "no_detectado" else if (any(s[names(s) != sonda] > 0)) "cazado_por_otra" else "cazado_por_su_sonda"
  } else {
    if (any(s > 0) || any(cmal[names(cmal) != sonda])) "cazado_por_otra" else if (!cmal[[sonda]]) "no_detectado" else "cazado_por_su_sonda"
  }
  v_txt <- if (is.null(vieja)) "" else sprintf("; la clase anterior: %s", sum(cuenta_v(t, vieja)))
  ok(ver == "cazado_por_su_sonda",
     sprintf("%s (%s): S1 %s, S1-Sol fórmula %s, S1-Sol signo %s%s; controles: %s — sonda esperada «%s»: %s",
             id, desc, s[[1]], s[[2]], s[[3]], v_txt,
             paste(sprintf("%s %s%s", names(cc), cc, ifelse(cmal, " (falla)", "")), collapse = ", "), sonda, ver))
}
juzgar_s1("M34", "la explicación de ERR-PARCIAL, rama CURSOS, saca la unidad de su fórmula «… que compensan»; quita un solo sitio",
          'p$parcialP, "\\\\ \\\\text{kilos}$ que compensan', 'p$parcialP, "$ que compensan', "unidad dentro")
juzgar_s1("M35", "la explicación de ERR-PROM con el molde P_ven pierde la coma de su fórmula; quita un solo sitio",
          '"\\\\ \\\\text{kilos,}$ es menor que el de "', '"\\\\ \\\\text{kilos}$ es menor que el de "', "unidad con el signo dentro")
juzgar_s1("M36", "la explicación de ERR-RESTA pierde el punto de su fórmula sin unidad; quita el único sitio",
          'p$delta, "\\\\text{.}$ Esa resta', 'p$delta, "$ Esa resta', "signo dentro sin unidad")
juzgar_s1("M38", "la lista de totales pierde la unidad y deja el punto fuera de la fórmula, «= 200$.»",
          PAT_M32, '"$.\\n", sep = "")', "S1-Sol signo", vieja = PAT_S1S_VIEJA)
for (cp in list(c("a", "2009", "espacio fino"), c("b", "202f", "espacio duro estrecho"), c("c", "2060", "unión de palabras"))) {
  juzgar_s1(paste0("M39", cp[1]), sprintf("la lista de totales saca la unidad de la fórmula con U+%s, %s (mutante de la r5)", toupper(cp[2]), cp[3]),
            PAT_M32, sprintf('"$", intToUtf8(0x%s), "kilos.\\n", sep = "")', cp[2]), "S1-Sol fórmula", vieja = PAT_S1F_VIEJA)
  juzgar_s1(paste0("M41", cp[1]), sprintf("la lista de totales deja el punto fuera de la fórmula tras U+%s, %s", toupper(cp[2]), cp[3]),
            PAT_M32, sprintf('"\\\\ \\\\text{kilos}$", intToUtf8(0x%s), ".\\n", sep = "")', cp[2]), "S1-Sol signo", vieja = PAT_S1S_VIEJA)
}
## L-6 (firmada el 2026-11-02): ED es una variable con el escape de U+00A0 y el .Rmd no lleva el carácter literal
## (la herramienta de edición convierte el escape en literal sin avisar). M29 escribe el literal en la definición de ED.
lit_ok <- function(lin) !any(grepl("\u00a0", lin, fixed = TRUE)) && length(grep('^ED <- "\\\\u00a0"', lin)) == 1L
ok(lit_ok(l), "L-6: el .Rmd no lleva el carácter U+00A0 literal y define ED con su escape")
l_m29 <- sub('ED <- "\\u00a0"', paste0('ED <- "', "\u00a0", '"'), l, fixed = TRUE)
ok(!identical(l_m29, l) && !lit_ok(l_m29), "M29 (ED con el carácter literal): la guarda L-6 lo detecta")
## Tanda S2a (H1 y H4 de la FASE 2C r5): ni el .Rmd ni este verificador llevan U+00A0, U+2009, U+202F ni U+2060
## literales; los mutantes de S1-Sol los construyen con intToUtf8().
ESP_LIT <- paste0("[", intToUtf8(c(0xa0, 0x2009, 0x202f, 0x2060)), "]")
yo_ver <- if (length(arg_file)) normalizePath(sub("^--file=", "", arg_file[1])) else NA_character_
ok(!any(grepl(ESP_LIT, l, perl = TRUE)) && !is.na(yo_ver) && !any(grepl(ESP_LIT, readLines(yo_ver, encoding = "UTF-8"), perl = TRUE)),
   "L-6 (S2a): ni el .Rmd ni el verificador llevan U+00A0, U+2009, U+202F ni U+2060 literales")
## M28 (decisión (a)): el mismo verificador, en otro proceso, sin la normalización del espacio duro. Tiene que dar un
## solo pendiente, el canal de largo de la rama PROMEDIO, con el margen en palabras de 6,7 % (la cifra sin normalizar).
if (NORMALIZAR_ED) {
  yo <- normalizePath(sub("^--file=", "", arg_file[1]))
  sal <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--no-init-file", shQuote(yo), shQuote(normalizePath(rmd))),
                                  stdout = TRUE, stderr = TRUE, env = "VERIF_SIN_NORMALIZAR=1"))
  pend <- grep("PENDIENTE DE FIRMA:", sal, value = TRUE, fixed = TRUE)
  larg <- grep("canal de largo (rama PROMEDIO)", pend, value = TRUE, fixed = TRUE)
  m_pal <- if (length(larg) == 1L) as.numeric(sub("^.*márgenes [0-9.]+ % y ([0-9.]+) %.*$", "\\1", larg)) else NA_real_
  ver28 <- if (!length(larg)) "no_detectado" else if (!isTRUE(m_pal == 6.7)) "mal_construido" else if (length(pend) != 1L) "cazado_por_otra" else "cazado_por_su_sonda"
  ok(ver28 == "cazado_por_su_sonda", sprintf("M28 (sin la normalización del espacio duro al medir; exit %s): %d pendiente(s); %s — %s",
                                             format(attr(sal, "status")), length(pend), if (length(larg)) trimws(larg) else "-", ver28))
}
if (length(FIRMADOS)) cat("\nEn rojo, cubiertos por un override firmado («O-1 corregido», 2026-10-29; decisión 2 a, 2026-10-31; regla #22 §P7-E):\n",
                          paste0("  - [", POR, "] ", FIRMADOS, "\n"), sep = "")
if (length(RESIDUOS)) cat("\nResiduos declarados §P7-B (margen < 15 %: inexplotables, no son defecto):\n",
                          paste0("  - ", RESIDUOS, "\n"), sep = "")
if (length(PENDIENTES)) cat("\nPendientes de firma del profesor (> +8 pp o premisa firmada superada; exit 3):\n",
                            paste0("  - ", PENDIENTES, "\n"), sep = "")

cat("\n=== RESULTADO:", if (length(fallos)) sprintf("FALLA (%d)", length(fallos)) else
  if (length(PENDIENTES)) sprintf("PENDIENTE DE FIRMA (%d; 0 fallos)", length(PENDIENTES)) else
  if (length(FIRMADOS)) sprintf("EN ROJO BAJO OVERRIDE FIRMADO (%d canales: %d dentro del alcance de O-1 corregido y %d de la decisión 2 a; 0 fallos, 0 pendientes)",
                                 length(FIRMADOS), sum(POR == "O-1 corregido"), sum(POR == "la decisión 2 a")) else
  "APROBADO (0 fallos)", "===\n")
quit(status = if (length(fallos)) 1L else if (length(PENDIENTES)) 3L else if (length(FIRMADOS)) 4L else 0L)
