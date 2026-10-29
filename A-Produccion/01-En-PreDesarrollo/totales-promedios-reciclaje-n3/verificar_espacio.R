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
## Uso: Rscript --no-init-file verificar_espacio.R [ruta.Rmd]     (exit 1 si algo falla; 3 = pendiente de firma)
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
rasgos <- function(env) with(env, list(
  canon = es_canonica, rama = rama, opciones = opciones, clave = which(sol), rol = rol_opcion,
  D = d$D, X = afs$gdis$x, cod_gdis = afs$gdis$codigo, molde_gdis = afs$gdis$molde,
  molde_clave = afs$clave$molde, molde_padv = afs$padv$molde, molde_psin = afs$psin$molde,
  sede_o = vapply(afs[rol_opcion], `[[`, "", "sede"), nom_o = ifelse(vapply(afs[rol_opcion], `[[`, "", "sede") == "G",
  nomG, nomP), fila1 = conj$nombres[1], tabla = c(d$nG, d$nP, d$aG, d$aP), gi = gi, lado = lado_gdis))

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
                           "juntaron|con|de|es|son|hubo) [0-9]+ kilos")
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
      t <- env0$MOLDES[[cc$m]]$f(S, O, cc$x, "papel"); n_txt <- n_txt + 1L
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
  mg <- function(f) { z <- vapply(est[[2]], function(x) { L <- nchar(x$opciones); k <- x$clave
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
juzga <- function(s, est, tasa) {
  ex <- tasa - 0.25
  if (ex <= 0.08) return(invisible(ok(TRUE, sprintf("H5: %s, %s: %.1f %% (%+.1f pp; obliga > +8)", s, est, 100 * tasa, 100 * ex))))
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
  ok(su - 1 / 3 <= 0.167 + 1e-9, sprintf("O-1: S_U, rama %s: %.1f %%, %+.1f pp atribuibles (alcance firmado: +16,7)",
                                         b, 100 * su, 100 * (su - 1 / 3)))
  ## O-1 firmó «aun con el atajo, elegir entre las dos opciones de G exige calcular D» (50 %). Si una
  ## pista superficial lo supera, la premisa firmada es falsa y vuelve al profesor (FASE 2C r2, H1).
  for (s in c("S_U + S1 (par de G sin la cifra de la tabla)", "S_U + suma (par de G sin la cifra que es suma de otras dos)",
              "S_U + S1 + suma (las dos pistas)", "S_U + atípica", "S_U + S1 + suma + atípica")) {
    t <- TAB[s, b]
    if (t <= 0.5 + 1e-9) ok(TRUE, sprintf("O-1: %s, rama %s: %.1f %% (<= 50 %% firmado)", s, b, 100 * t))
    else pendiente(sprintf("O-1: %s, rama %s: %.1f %%, por encima del 50 %% que supone la premisa firmada", s, b, 100 * t))
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
for (est in list(list("mezcla natural", NAT), list("rama CURSOS", CU$CURSOS), list("rama PROMEDIO", CU$PROMEDIO))) {
  V <- est[[2]]; if (length(V) < 20) { cat("  ", est[[1]], ": NO CONCLUYENTE (n < 20)\n"); next }
  res <- evaluar_bateria(REGLAS, lapply(V, `[[`, "opciones"), vapply(V, `[[`, 0L, "clave"),
                         familias_no_aplicables = NA_FAM)
  top <- head(res$detalle, 4)
  cat(sprintf("  batería §P7 (%d reglas) %-14s n=%d: máx %.1f %% (%s), techo nulo %.1f %%, exceso %+.1f pp -> %s\n",
              length(REGLAS), est[[1]], length(V), 100 * res$max_obs, res$regla_top, 100 * res$techo_nulo,
              100 * res$exceso, res$veredicto))
  cat(sprintf("      top: %s\n", paste(sprintf("%s %.1f%%", top$regla, 100 * top$tasa), collapse = " · ")))
  if (est[[1]] == "mezcla natural") VEREDICTO_MEZCLA <- res
  ## H2 de la FASE 2C r3: el canal de largo depende del bloque de semillas, así que su margen §P7-B se
  ## mide en TODO estrato, supere o no +8 pp con esta muestra; un margen >= 15 % sería explotable.
  mg_e <- function(f) vapply(V, function(x) { L <- nchar(x$opciones); k <- x$clave
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
    res2 <- evaluar_bateria(Filter(function(r) !r$nombre %in% LARGO, REGLAS), lapply(V, `[[`, "opciones"),
                            vapply(V, `[[`, 0L, "clave"), familias_no_aplicables = NA_FAM)
    ## Margen §P7-B en caracteres: la clave como única más larga y como única más corta.
    marg <- function(f) vapply(V, function(x) { L <- nchar(x$opciones); k <- x$clave
      if (sum(L == f(L)) != 1 || L[k] != f(L)) return(NA_real_); abs(L[k] - f(L[-k])) / f(L[-k]) }, 0)
    mmax <- max(c(marg(max), marg(min), 0), na.rm = TRUE)
    ## H5 (FASE 2C, ronda 1): «molde atipico» es de la familia formato y cuenta palabras; su margen se
    ## mide en palabras, con el mismo 15 %: la clave elegida frente a la opción de conteo más cercano.
    mt <- vapply(V, function(x) { k <- vapply(x$opciones, ntok, 0L); dv <- abs(k - stats::median(k))
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
    } else falla(sprintf("§P7-A %s: exceso %+.1f pp (obliga > +8); sin reglas de largo %+.1f pp, margen máx %.1f %% (caracteres) y %.1f %% (palabras)",
                         est[[1]], 100 * res$exceso, 100 * res2$exceso, 100 * mmax, 100 * tmax))
  }
}
if (length(CANON_V)) {
  sc_reg <- vapply(REGLAS, function(r) mean(vapply(CANON_V, function(x) {
    S <- tryCatch(.conjunto_de(r$fn(x$opciones), 4L), error = function(e) rep(FALSE, 4)); sc(S, x$clave) }, 0)), 0)
  ord <- order(-sc_reg)[1:5]
  cat(sprintf("  canónica (enumeración exacta, 24 órdenes): máx %.1f %% (%s); exceso en la mezcla = 0,10 x %+.1f pp = %+.1f pp\n",
              100 * max(sc_reg), REGLAS[[which.max(sc_reg)]]$nombre, 100 * (max(sc_reg) - 0.25), 10 * (max(sc_reg) - 0.25)))
  cat("      top:", paste(sprintf("%s %.1f%%", vapply(REGLAS[ord], `[[`, "", "nombre"), 100 * sc_reg[ord]), collapse = " · "), "\n")
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
       llego = function(e) grepl("estudiantes", e$MOLDES$U_c$f("S", "O", 1, "papel"))),
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
       cambios = list(c('cierre <- function(x, O) paste0("su total fue más alto que el de ", O, " en ", x, " kilos.")',
                        'cierre <- function(x, O) paste0("su total fue de ", x, " kilos más que el de ", O, ".")')),
       llego = function(e) !isTRUE(e$es_canonica) && grepl("fue de [0-9]+ kilos más que", e$afs$clave$texto)),
  list(id = "M17", sonda = "regla #13 v1.1: ninguna lectura que admite el texto cambia el veredicto",
       desc = "ERR-TOTAL con el cierre «fue de T_G kilos más que» (ERR-TOTAL y la lectura alternativa)",
       cambios = list(c('afirmacion(molde_gdis, "G", fila_g$x, cod_gdis)',
                        paste0('(function(a) { if (a$codigo == "ERR-TOTAL") a$texto <- paste0(sub(" su total fue más alto.*$", "", a$texto), ',
                               '" su total fue de ", a$x, " kilos más que el de ", nomP, "."); a })(afirmacion(molde_gdis, "G", fila_g$x, cod_gdis))'))),
       llego = function(e) identical(e$afs$gdis$codigo, "ERR-TOTAL") && grepl("fue de [0-9]+ kilos", e$afs$gdis$texto)),
  list(id = "M18", sonda = "regla #13 v1.1: la cifra va solo en el complemento",
       desc = "una razón sin sustento cita otra cifra («durante 5 días más»)",
       cambios = list(c("reciclaron durante más días que los demás", "reciclaron durante 5 días más que los demás")),
       llego = function(e) identical(e$afs$psin$molde, "U_d")))
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
if (length(RESIDUOS)) cat("\nResiduos declarados §P7-B (margen < 15 %: inexplotables, no son defecto):\n",
                          paste0("  - ", RESIDUOS, "\n"), sep = "")
if (length(PENDIENTES)) cat("\nPendientes de firma del profesor (> +8 pp o premisa firmada superada; exit 3):\n",
                            paste0("  - ", PENDIENTES, "\n"), sep = "")

cat("\n=== RESULTADO:", if (length(fallos)) sprintf("FALLA (%d)", length(fallos)) else
  if (length(PENDIENTES)) sprintf("PENDIENTE DE FIRMA (%d; 0 fallos)", length(PENDIENTES)) else "APROBADO (0 fallos)", "===\n")
quit(status = if (length(fallos)) 1L else if (length(PENDIENTES)) 3L else 0L)   # 3 = pendiente de firma
