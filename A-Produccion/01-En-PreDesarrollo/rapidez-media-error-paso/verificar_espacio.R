#!/usr/bin/env Rscript
# =============================================================================
# verificar_espacio.R — rapidez-media-error-paso (SCHOICE)
#
# Uso: Rscript --no-init-file verificar_espacio.R [smoke|oraculo|enumerar|bateria|exacto|h1|mutantes|mutantes_o2|rapido|todo]
# «rapido» es todo menos «enumerar», «exacto» y «mutantes_o2» (los tres tardan varios minutos).
# Exit: 1 = falla (incluye un error DENTRO de una regla, un amarre del generador que no cuadra y cualquier diferencia con el modelo);
#       3 = pendiente de firma (O-2): FIRMADA es FALSE, una regla fuera de FIRMA_O2 o por encima de su cifra, una cifra que no reproduce,
#           o alcance sin evaluar, o un modo distinto de «todo», o un .Rmd cuyo md5 difiere de RMD_MD5 (H1 de la r4);
#       4 = ROJO cubierto por O-2: SOLO en modo «todo» y SOLO con FIRMADA TRUE (nunca PASS); 0 no ocurre con este ítem.
#       La decisión es la función pura decidir_exit().
#
# 1. ORÁCULO: parte del TEXTO que lee el estudiante (no de las variables del .Rmd): deduce los
#    datos del enunciado, rehace cada paso y decide qué opción es verdadera. Límite declarado:
#    comparte con el .Rmd la definición de «paso correcto» (final menos inicial, km sobre h), y no
#    ve lecturas alternativas del enunciado: la FASE 2C r1 encontró dos que se le escapaban.
#    Contrasta la prosa de la Solution (con la frase de cada razón y las cifras del enunciado, H7 de la r3) y los
#    veredictos del feedback con el texto, exige que ningún paso muestre un negativo, ni en el resultado ni en un operando
#    (L-8), y que ningún signo de puntuación quede fuera de una fórmula (L-6); L-6 también se sondea sobre el fuente.
# 1b. ENUMERACIÓN EXACTA del espacio finito (regla #13): toda fila, d1, error y paso repetido, con
#    toda razón candidata. Exige que ninguna razón repare el procedimiento (L-4).
# 2. BATERÍA §P7 (regla #22). CONGELADA desde r3 (51 reglas: ni se añaden, ni se quitan, ni se modifican). Muestra por
#    cuotas: N = 100 por rol de la clave (regla #23). Las cifras por rol condicionan a la respuesta misma: el estudiante
#    no ve esa rama sin resolver el ítem, así que el veredicto lo da el total. La canónica se mide aparte, por enumeración
#    exacta (una sola instancia).
#    Constructo, marca por marca (H8 de la r3). Fuera de la batería, a propósito: las reglas que leen el paso que está mal,
#    con las marcas «+», «h/km» y «− 0». «h/km» exige contrastar con «en km/h» y «+» exige saber que dos posiciones desde
#    un mismo punto no se suman: son CONSTRUCTO. «− 0» es constructo PARCIAL (O-2): basta ver que se resta algo que no hace nada,
#    sin leer el enunciado. M0 y RE se declaran con su cifra. Localizar el error NO es resolver el ítem sin razonar.
# 2c. EXACTO (regla #23, espacio finito): la batería, los canales de sustitución, Q y las vigiladas sobre los 9 564 estados
#    con sus pesos reales. Queda ATADO al generador real: las ramas de error (salen del switch de error_cometido, H4); el ancla de
#    d1 (cand contra el del generador); el pool de razones, sus precondiciones, repara(), el filtro de signo, la obligación L-9 y los
#    pesos de RANGO_META (el chunk corre tal cual y se leen e$candidatos y e$pesos). NO lo lee la enumeración: lo que sortean pick() y
#    sample() (la probabilidad de cada estado, pst, está escrita aquí a mano) ni lo que corre tras la línea que sortea elegidos (el
#    peso de cada conjunto sale de e$pesos, no de esa línea). Eso lo cubre RMD_MD5 (H1 de la r4): con otro md5 el exit es 3.
#    Un error dentro de una regla cuenta como fallo y la salida se compara con el modelo Python de la variante C
#    (opcion_c/c2.json) y con las 15 reglas de H3 (H3_ESP), a la centésima. Es lo único que clasifica O-2 (FIRMA_O2, regla por
#    regla, en los dos ámbitos): nunca una cifra de muestra.
# 3. MUTANTES (contrato del Incidente P): sonda declarada, patrón que aparece exactamente una
#    vez, aplicados en memoria sobre el chunk data_generation, semillas hasta la primera muerte.
# 3b. MUTANTES DE O-2: sobre una copia del resultado (modo exacto) y sobre el generador, re-enumerando (mutantes_o2).
# =============================================================================
suppressPackageStartupMessages(library(exams))
args <- commandArgs(FALSE)
aqui <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)][1])))
RMD  <- file.path(aqui, "rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd")
modo <- { a <- commandArgs(TRUE); if (length(a)) a[1] else "todo" }
# H1 de la FASE 2C r4: las cifras de FIRMA_O2 valen solo para este .Rmd. La enumeración no lee lo que sortean pick() y sample() ni lo que
# corre tras elegidos; ese hueco lo cubre el md5. Con otro md5 el exit es 3 (decidir_exit), aunque todo lo demás cuadre.
RMD_MD5 <- "904af1ca1271d97c54123218c9db03c0"
md5_coincide <- function(ruta) isTRUE(unname(tools::md5sum(ruta)) == RMD_MD5)
MD5_OK <- md5_coincide(RMD)

lineas <- readLines(RMD, encoding = "UTF-8", warn = FALSE)
chunk <- function(nom) {
  ini <- grep(paste0("^```\\{r ", nom, "[,}]"), lineas)
  stopifnot(length(ini) == 1L)
  fin <- ini + which(lineas[(ini + 1L):length(lineas)] == "```")[1L]
  paste(lineas[(ini + 1L):(fin - 1L)], collapse = "\n")
}
COD <- chunk("data_generation")
compilar <- function(cod) parse(text = cod, keep.source = FALSE)
correr <- function(expr, semilla) {
  env <- new.env(parent = globalenv())
  set.seed(semilla)
  eval(expr, envir = env)
  env
}
# Fuerza la rama sin tocar P_CANON, que tiene su propia guarda de tope.
fijar_canon <- function(cod, valor) {
  pat <- "es_canonica <- sample(c(TRUE, FALSE), 1L, prob = c(P_CANON, 1 - P_CANON))"
  stopifnot(lengths(regmatches(cod, gregexpr(pat, cod, fixed = TRUE))) == 1L)
  sub(pat, paste0("es_canonica <- ", valor), cod, fixed = TRUE)
}
sust1 <- function(cod, pat, nuevo) {
  if (lengths(regmatches(cod, gregexpr(pat, cod, fixed = TRUE))) != 1L) stop("patrón no único: ", pat)
  sub(pat, nuevo, cod, fixed = TRUE)
}
EX_NOCANON <- compilar(fijar_canon(COD, "FALSE"))
EX_CANON   <- compilar(fijar_canon(COD, "TRUE"))

# ---- H4: el generador manda ---------------------------------------------------------------------------------------
# Las ramas de error de la enumeración se obtienen evaluando los brazos del switch REAL de error_cometido con
# pick <- identity; ERRS_ESPERADO es el diseño firmado y errs_amarre() falla si el generador difiere.
ERRS_ESPERADO <- list(dist = c("EV-D-SUMA", "EV-D-FINAL"), tiem = "EV-T-FINAL", coc = "EV-C-UNIDAD")   # L-8: sin ramas de signo
errs_del_generador <- function(cod) {
  buscar <- function(x) {
    if (!is.call(x)) return(NULL)
    if (identical(x[[1L]], as.name("<-")) && length(x) == 3L && identical(x[[2L]], as.name("error_cometido")) &&
        is.call(x[[3L]]) && identical(x[[3L]][[1L]], as.name("switch"))) return(x[[3L]])
    for (i in seq_along(x)) {
      r <- tryCatch(buscar(x[[i]]), error = function(e) NULL)
      if (!is.null(r)) return(r)
    }
    NULL
  }
  sw <- NULL
  for (e in as.list(compilar(cod))) { sw <- buscar(e); if (!is.null(sw)) break }
  if (is.null(sw)) stop("no hay switch de error_cometido en data_generation")
  brazos <- as.list(sw)[-c(1L, 2L)]
  amb <- list2env(list(pick = identity), parent = baseenv())
  lapply(brazos, eval, envir = amb)
}
errs_amarre <- function(cod) isTRUE(identical(errs_del_generador(cod), ERRS_ESPERADO))
# Ancla de d1: cand del verificador contra el del generador (así los pesos 1/length(cand) quedan comprobados).
ANCLA_D1 <- "  stopifnot(identical(as.integer(cand), as.integer(.ST$cand))); d1 <- .ST$d1"

# ---- O-2: autorización del profesor del 2026-11-05 («Autorizo C y mantengo el − 0») --------------------------------
TECHO_O2 <- 0.288   # techo nulo del alcance (constante: no es el techo de una muestra)
# Exceso en pp sobre TECHO_O2 de cada combinación regla-ámbito con exceso >= 8, batería y vigiladas. Se llena SOLO con la
# salida del modo «exacto» (salida/firma_o2_medida.csv) y la firma el profesor en el paso 11; no se edita mientras corre.
FIRMA_O2 <- data.frame(
  regla = c("X1b", "X1", "R30-90", "R20-120", "R10-150", "R20-120+ent", "RE", "RE-con-fraccion", "U", "NM", "X0+", "E2", "E3",
    "M0", "M2", "M3", "MAG-segundo-afirmado", "X1b", "X1", "R30-90", "R20-120", "R10-150", "R20-120+ent", "RE",
    "RE-con-fraccion", "U", "NM", "X0+", "E1", "E2", "E3", "M0", "M2", "M3",
    "NM+frac", "NM+pasos", "NM+kmh", "NM+X0", "NM+X0+frac", "NM+kmh+frac", "NM+pos+frac", "EST-1", "EST+-1", "EST+-1-par",
    "E3-1", "E3-1-par", "M3-par", "entre-media", "E3-1-par-media", "NM+frac", "NM+pasos", "NM+kmh", "NM+X0", "NM+X0+frac",
    "NM+kmh+frac", "NM+pos+frac", "EST-1", "EST+-1", "EST+-1-par", "E3-1", "E3-1-par", "M3-par", "entre-media",
    "E3-1-par-media"),
  ambito = c("nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon",
    "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla",
    "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla",
    "mezcla", "mezcla",
    "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon", "nocanon",
    "nocanon", "nocanon", "nocanon", "nocanon", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla",
    "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla", "mezcla"),
  exceso = c(15.51, 29.43, 30.59, 19.58, 12.32, 36.67, 21.2, 14.95, 13.72, 10.62, 8.27, 17.51, 24.49, 46.2, 51.2, 61.88, 8.36,
    14.55, 29.02, 30.12, 19.66, 12.77, 35.89, 19.95, 14.01, 16.59, 10.31, 11.41, 9.88, 20.2, 26.83, 47.45, 52.2, 62.35,
    27.28, 25.9, 25.08, 21.5, 35.65, 35.46, 34.37, 15.75, 17.46, 21.07, 44.93, 49.25, 66.64, 19.17, 69.67, 26.14, 24.42,
    24.06, 21.48, 34.93, 33.91, 36.21, 14.77, 16.4, 19.82, 46.25, 50.34, 66.87, 18.02, 69.74),
  stringsAsFactors = FALSE)
FIRMADA <- TRUE    # solo la cambia el coordinador, tras la firma del profesor (paso 11); con FALSE el exit es 3 aunque todo cuadre
# Decisión de salida, pura. 4 solo en «todo» y solo firmada; nunca 0.
decidir_exit <- function(errores, evaluado, pendientes, firmada, modo, md5_ok) {
  if (errores > 0L) return(1L)
  if (!isTRUE(md5_ok) || !isTRUE(evaluado) || pendientes > 0L || !isTRUE(firmada) || modo != "todo") return(3L)
  4L
}

# Texto visible, propio del verificador (no usa visible() del .Rmd). El menos pasa a ASCII para parsear.
ver <- function(s) {
  s <- gsub("\\\\text\\{([^}]*)\\}", "\\1", s)
  s <- gsub("\\\\ ", " ", s)
  s <- gsub("$", "", s, fixed = TRUE)
  s <- gsub(" ", " ", s, fixed = TRUE)
  s <- gsub("−", "-", s, fixed = TRUE)
  gsub("**", "", s, fixed = TRUE)
}

# ---- Prosa de la Solution (H2 de la FASE 2C r2) ------------------------------------------
# El oráculo lee enunciado, pasos y opciones; esta sonda lee lo que dice la Solution y el feedback y lo contrasta
# con los datos que el ENUNCIADO muestra (no con las variables del .Rmd): procedimiento correcto, «llega a X en
# lugar de Y», caso específico, clase de cada veredicto, que cada razón falsa se analice, 3 razones, sin letras.
SOL_EX <- compilar(chunk("solucion"))
solucion_texto <- function(e) paste(capture.output(eval(SOL_EX, envir = e)), collapse = "\n")
vis <- function(s) {
  s <- gsub("\\\\text\\{([^}]*)\\}", "\\1", s)
  s <- gsub("\\\\times", "x", s)
  s <- gsub("\\\\ ", " ", s)
  s <- gsub("$", "", s, fixed = TRUE)
  s <- gsub("\u00a0", " ", s, fixed = TRUE)
  gsub("\\*\\*", "", s)
}
chequeo <- function(e, x) {
  p <- character(0); en <- vis(e$enunciado)
  h <- as.integer(sub(":.*", "", regmatches(en, gregexpr("[0-9]+:00", en))[[1]]))
  k <- as.numeric(sub(" km", "", regmatches(en, gregexpr("[0-9]+ km\\b", en))[[1]]))
  t1 <- h[1]; t2 <- h[2]; d1 <- k[1]; d2 <- k[2]; D <- d2 - d1; dur <- t2 - t1; v <- D / dur
  nec <- c(sprintf("Distancia recorrida entre las dos horas: %d km - %d km = %d km.", d2, d1, D),
           sprintf("Tiempo transcurrido: %d h - %d h = %d h.", t2, t1, dur),
           sprintf("Rapidez media, en km/h: %d km/%d h = %d km/h.", D, dur, v),
           sprintf("avanza %d km en %d h: son %d km/h.", D, dur, v), sprintf("%d x %d = %d.", v, dur, D))
  for (n in nec) if (!grepl(n, x, fixed = TRUE)) p <- c(p, paste("FALTA:", n))
  pp <- vis(c(e$paso_1, e$paso_2, e$paso_3))
  r1 <- as.numeric(sub("^.*= (-?[0-9]+) km$", "\\1", pp[1])); r2 <- as.numeric(sub("^.*= (-?[0-9]+) h$", "\\1", pp[2]))
  r3 <- as.numeric(sub("^.*= (-?[0-9]+) (km/h|h/km)$", "\\1", pp[3]))
  mal <- if (r1 != D || grepl("+", pp[1], fixed = TRUE)) 1L else if (r2 != dur) 2L else 3L
  if (mal < 3L) {
    n <- sprintf("llega a %d km/h en lugar de %d km/h.", r3, v)
    if (!grepl(n, x, fixed = TRUE)) p <- c(p, paste("FALTA:", n))
  } else if (grepl("en lugar de", x, fixed = TRUE)) p <- c(p, "SOBRA consecuencia")
  op <- vis(e$opciones); fb <- vis(e$feedback); ko <- which(e$sol == 1L)
  pasoop <- as.integer(sub("^En el paso ([0-9]),.*$", "\\1", op))
  for (i in 1:4) {
    f <- fb[i]
    if (i == ko) { if (!startsWith(f, sprintf("Correcta. El paso %d tiene el error", mal))) p <- c(p, "FB clave")
    } else if (pasoop[i] == mal) { if (!grepl(sprintf("^Incorrecta. El paso %d sí tiene un error", mal), f)) p <- c(p, "FB mismo paso")
    } else if (pasoop[i] == 3L) { if (!startsWith(f, "Incorrecta. El paso 3 hace bien")) p <- c(p, "FB paso3")
    } else if (!grepl(sprintf("^Incorrecta. El paso %d está bien hecho", pasoop[i]), f)) p <- c(p, "FB bien hecho")
  }
  if (!grepl(paste0("afirma: «", op[ko], "»."), x, fixed = TRUE)) p <- c(p, "CLAVE no citada")
  for (i in setdiff(1:4, ko)) if (!grepl(paste0("«", op[i], "»"), x, fixed = TRUE)) p <- c(p, "FALSA no analizada")
  if (lengths(regmatches(x, gregexpr("El argumento «", x, fixed = TRUE))) != 3L) p <- c(p, "no son 3 razones")
  if (grepl("\\bOpci[oó]n [A-D]\\b", x)) p <- c(p, "letra")
  # H7 de la FASE 2C r3: por cada código que aparece en la Solution, su frase con las cifras tomadas del ENUNCIADO, dentro de
  # su propio párrafo; y, en el análisis del error cometido, el tramo correcto de ese paso.
  con_frase <- function(txt, f) {
    q <- gregexpr(f, txt, fixed = TRUE)[[1L]]
    any(q > 0L & !grepl("[0-9]", substring(txt, q - 1L, q - 1L)))
  }
  tramo_d <- sprintf("%d km - %d km = %d km.", d2, d1, D); tramo_t <- sprintf("%d h - %d h = %d h.", t2, t1, dur)
  frases <- function(cod) switch(cod,
    "EV-D-INICIAL" = c(sprintf("%d km es lo que", d1), tramo_d),
    "EV-D-FINAL"   = c(sprintf("%d km es lo que separa", d2), tramo_d),
    "EV-D-SUMA"    = tramo_d, "EV-D-SIGNO" = tramo_d,
    "EV-D-PROM"    = c(sprintf("%d km es la posición a mitad de camino", (d1 + d2) / 2), tramo_d),
    "EV-T-FINAL"   = c(sprintf("%d h es una hora del reloj", t2), tramo_t),
    "EV-T-INICIAL" = c(sprintf("%d h es la hora de la primera lectura", t1), tramo_t),
    "EV-T-SUMA"    = tramo_t, "EV-T-SIGNO" = tramo_t,
    "EV-T-CUENTA"  = c(sprintf("hay %d marcas del reloj, pero entre ellas solo hay %d intervalos", dur + 1, dur), tramo_t),
    "EV-C-CIFRAS"  = sprintf("%d km figura como distancia y %d h como tramo", r1, r2),
    character(0))
  cods <- unique(sub(" — $", "", regmatches(x, gregexpr("EV-[A-Z]-[A-Z]+ — ", x))[[1L]]))
  for (cd in cods) {
    par_ <- sub("(?s)\n\n.*$", "", substring(x, regexpr(paste0(cd, " — "), x, fixed = TRUE)), perl = TRUE)
    fr <- frases(cd); if (cd == "EV-C-CIFRAS") fr <- sprintf("%d km figura como distancia y %d h como tiempo", r1, r2)
    for (f in fr) if (!con_frase(par_, f)) p <- c(p, paste0("FRASE ", cd, ": falta «", f, "»"))
  }
  i0 <- regexpr("Análisis del error", x, fixed = TRUE)
  if (i0 < 0L) { p <- c(p, "falta el análisis del error") } else {
    resto <- substring(x, i0); j0 <- regexpr("*Causa de fondo:*", resto, fixed = TRUE)
    ana <- sub("(?s)^.*```\\s*", "", substr(resto, 1L, j0 - 1L), perl = TRUE)
    fin <- if (mal == 1L) tramo_d else if (mal == 2L) tramo_t else sprintf("%d km/%d h = %d km/h.", D, dur, v)
    if (!con_frase(ana, fin)) p <- c(p, paste0("ANALISIS del error: falta «", fin, "»"))
  }
  p
}
# L-6, sonda sobre el FUENTE: ninguna comilla invertida en ningún chunk (la FASE 2D corta el chunk en la primera).
# El código de data_generation se recibe aparte para poder probar un mutante.
CHUNKS_L6 <- c("enunciado", "opciones", "solucion", "feedback")
l6_fuente <- function(cod_dg) any(grepl(intToUtf8(96L), c(cod_dg, vapply(CHUNKS_L6, chunk, "")), fixed = TRUE))

# ---- 1. ORÁCULO ---------------------------------------------------------------
hora24 <- function(h) { n <- as.integer(sub(":.*", "", h)); if (grepl("p\\.", h)) n + 12L else n }
oraculo <- function(e) {
  enun <- ver(e$enunciado)
  horas <- regmatches(enun, gregexpr("[0-9]+:00 [ap]\\. m\\.", enun))[[1]]
  kms   <- as.numeric(sub(" km", "", regmatches(enun, gregexpr("[0-9]+ km\\b", enun))[[1]]))
  if (length(horas) != 2L || length(kms) != 2L) return(list(problema = "ORACULO_DATOS: no hay 2 horas y 2 distancias"))
  t1 <- hora24(horas[1]); t2 <- hora24(horas[2]); d1 <- kms[1]; d2 <- kms[2]
  relojT <- function(t) if (t > 12L) t - 12L else t
  Dv <- d2 - d1; Tv <- t2 - t1
  if (Dv <= 0 || Tv <= 0) return(list(problema = "ORACULO_DATOS: intervalo no positivo"))
  p1 <- regmatches(ver(e$paso_1), regexec("^Paso 1\\. Se calcula cuánta distancia se recorrió: (-?[0-9]+) km ([-+]) (-?[0-9]+) km = (-?[0-9]+) km$", ver(e$paso_1)))[[1]]
  p2 <- regmatches(ver(e$paso_2), regexec("^Paso 2\\. Se calcula el tiempo que transcurrió: (-?[0-9]+) h ([-+]) (-?[0-9]+) h = (-?[0-9]+) h$", ver(e$paso_2)))[[1]]
  p3 <- regmatches(ver(e$paso_3), regexec("^Paso 3\\. Se halla el cociente entre los valores obtenidos: (-?[0-9]+) (km|h)/\\(?(-?[0-9]+) (km|h)\\)? = (-?[0-9]+) (km/h|h/km)$", ver(e$paso_3)))[[1]]
  if (length(p1) != 5L || length(p2) != 5L || length(p3) != 7L) return(list(problema = "ORACULO_PASOS: un paso no tiene la forma esperada"))
  a1 <- as.numeric(p1[2]); b1 <- as.numeric(p1[4]); c1 <- as.numeric(p1[5])
  a2 <- as.numeric(p2[2]); b2 <- as.numeric(p2[4]); c2 <- as.numeric(p2[5])
  x3 <- as.numeric(p3[2]); y3 <- as.numeric(p3[4]); z3 <- as.numeric(p3[6])
  ar1 <- (if (p1[3] == "+") a1 + b1 else a1 - b1) == c1
  ar2 <- (if (p2[3] == "+") a2 + b2 else a2 - b2) == c2
  ar3 <- x3 / y3 == z3
  if (!all(ar1, ar2, ar3)) return(list(problema = "ORACULO_ARITMETICA: una cuenta del procedimiento da mal (L-1)"))
  # L-8 (decisión del profesor, O-1; vigente): ningún paso muestra un negativo ni un cero.
  if (any(c(c1, c2, z3) <= 0) || any(c(a1, b1, a2, b2, x3, y3) < 0)) return(list(problema = "ORACULO_L8: un paso muestra un negativo"))
  ok1 <- p1[3] == "-" && a1 == d2 && b1 == d1
  ok2 <- p2[3] == "-" && a2 == relojT(t2) && b2 == relojT(t1)
  ok3 <- p3[3] == "km" && p3[5] == "h" && p3[7] == "km/h" && x3 == c1 && y3 == c2
  mal <- which(!c(ok1, ok2, ok3))
  if (length(mal) != 1L) return(list(problema = paste0("ORACULO_PASOS: ", length(mal), " pasos mal; debe ser 1")))
  mostrado <- list(c1, c2, c(x3, y3))
  ops <- ver(e$opciones)
  verdad <- vapply(ops, function(o) {
    n <- as.integer(sub("^En el paso ([0-9]),.*$", "\\1", o))
    nom <- c("distancia recorrida", "tiempo transcurrido", "cociente")[n]
    if (!grepl(nom, o, fixed = TRUE)) return(NA)
    cuerpo <- sub("^.* es ", "", o)
    af <- sub(" y no .*$", "", cuerpo)
    if (grepl(" = ", af, fixed = TRUE)) af <- sub("^.* = ", "", af)
    if (n != mal) return(FALSE)
    if (n == 1L) return(identical(af, paste0(Dv, " km")))
    if (n == 2L) return(identical(af, paste0(Tv, " h")))
    identical(af, paste0(Dv, " km/", Tv, " h"))
  }, TRUE, USE.NAMES = FALSE)
  if (anyNA(verdad)) return(list(problema = "ORACULO_OPCION: el sustantivo no corresponde al paso"))
  # L-4: ninguna razón falsa, puesta en su paso, da la rapidez correcta.
  for (i in which(!verdad)) {
    n <- as.integer(sub("^En el paso ([0-9]),.*$", "\\1", ops[i]))
    if (n == 3L || isTRUE(e$es_canonica)) next
    x <- as.numeric(regmatches(sub(" y no .*$", "", sub("^.* es ", "", ops[i])), regexpr("-?[0-9]+", sub(" y no .*$", "", sub("^.* es ", "", ops[i])))))
    r <- if (n == 1L) x / c2 else c1 / x
    if (isTRUE(all.equal(r, Dv / Tv))) return(list(problema = paste0("ORACULO_REPARA: la razón del paso ", n, " da la rapidez correcta")))
  }
  if (sum(verdad) != 1L) return(list(problema = paste0("ORACULO_CLAVE: ", sum(verdad), " opciones verdaderas; debe ser 1")))
  if (!identical(which(verdad), which(e$sol == 1L))) return(list(problema = "ORACULO_CLAVE: la marca no es la opción verdadera"))
  # L-9 (H6 de la FASE 2C r4): si la versión admitía una razón nueva, alguna de las cuatro opciones lo es. Mira lo que quedó en e$ops,
  # no la guarda del chunk: así ve lo que corre DESPUÉS de ella.
  if (!isTRUE(e$es_canonica) && length(e$con_nueva) >= 1L &&
      !any(vapply(e$ops, function(o) o$codigo, "") %in% c("EV-D-PROM", "EV-T-CUENTA")))
    return(list(problema = "ORACULO_L9: la versión admitía una razón nueva (EV-D-PROM o EV-T-CUENTA) y ninguna opción lo es"))
  # La parte «y no X» cita lo que el procedimiento muestra (forma Y).
  for (o in ops[grepl(" y no ", ops, fixed = TRUE)]) {
    n <- as.integer(sub("^En el paso ([0-9]),.*$", "\\1", o))
    rech <- sub("^.* y no ", "", o)
    txt <- c(ver(e$paso_1), ver(e$paso_2), ver(e$paso_3))[n]
    if (!grepl(paste0("(= |: )", gsub("([()/])", "\\\\\\1", rech), "( = .*)?$"), txt) && !isTRUE(e$es_canonica))
      return(list(problema = paste0("ORACULO_RECHAZO: «y no ", rech, "» no es lo que muestra el paso ", n)))
  }
  # L-6, sonda sobre el TEXTO: ningún signo de puntuación pegado al cierre de una fórmula (va dentro de ella).
  sol_x <- solucion_texto(e)
  textos <- c(e$enunciado, e$paso_1, e$paso_2, e$paso_3, e$opciones, e$feedback, sol_x)
  if (any(grepl("\\$[.,:;]", textos))) return(list(problema = "L-6: signo fuera de la fórmula"))
  # H2: la prosa de la Solution y los veredictos del feedback, contrastados con el texto del enunciado.
  pr <- chequeo(e, vis(sol_x))
  if (length(pr)) return(list(problema = paste0("ORACULO_SOLUTION: ", pr[1L])))
  list(problema = NA_character_, mal = mal, Dv = Dv, Tv = Tv)
}

# ---- Muestra por cuotas ---------------------------------------------------------
muestrear <- function(expr, cuota = 100L, tope = 50000L, inicio = 1L) {
  claves <- c("dist", "tiem", "coc")
  n <- setNames(integer(length(claves)), claves)
  sal <- list(); s <- inicio - 1L
  while (any(n < cuota) && s < inicio + tope) {
    s <- s + 1L
    e <- correr(expr, s)
    k <- e$rol_clave
    if (n[[k]] >= cuota) next
    n[[k]] <- n[[k]] + 1L
    sal[[length(sal) + 1L]] <- list(s = s, k = k, rol = e$rol_clave, err = e$error_cometido,
      rep = e$rol_repetido, ops = ver(e$opciones), key = which(e$sol == 1L),
      est = ver(paste(e$enunciado, e$paso_1, e$paso_2, e$paso_3)), cods = vapply(e$ops, function(o) o$codigo, ""),
      env = e)
  }
  stopifnot(all(n >= cuota))
  sal
}

if (modo == "smoke") {
  for (s in 1:6) { e <- correr(compilar(COD), s); cat("---", s, e$rol_clave, e$error_cometido, "rep", e$rol_repetido, e$plantilla, "\n")
    cat(ver(e$enunciado), "\n", ver(e$paso_1), "\n", ver(e$paso_2), "\n", ver(e$paso_3), "\n", sep = ""); cat(paste0(" ", ifelse(e$sol == 1L, "*", " "), " ", ver(e$opciones), "\n"), sep = "") }
  e <- correr(EX_CANON, 1L); cat("--- CANONICA\n", ver(e$enunciado), "\n", ver(e$paso_1), "\n", ver(e$paso_2), "\n", ver(e$paso_3), "\n", sep = ""); cat(paste0(" ", ifelse(e$sol == 1L, "*", " "), " ", ver(e$opciones), "\n"), sep = "")
  print(nrow(e$espacio)); quit(status = 0L)
}

errores_total <- 0L
if (modo %in% c("oraculo", "rapido", "todo")) {
  cat("=== 1. ORÁCULO LITERAL (N = 100 por estrato + canónica) ===\n")
  if (l6_fuente(COD)) { cat("L-6: comilla invertida en un chunk\n"); errores_total <- errores_total + 1L }
  m <- muestrear(EX_NOCANON)
  prob <- vapply(m, function(v) { r <- oraculo(v$env); if (is.na(r$problema)) "" else r$problema }, "")
  cat(sprintf("Versiones: %d | con problema: %d\n", length(m), sum(nzchar(prob))))
  if (any(nzchar(prob))) { print(table(prob[nzchar(prob)])); errores_total <- errores_total + sum(nzchar(prob)) }
  rc <- oraculo(correr(EX_CANON, 1L))
  cat("Canónica:", if (is.na(rc$problema)) "OK (clave = paso 3)" else rc$problema, "\n")
  if (!is.na(rc$problema)) errores_total <- errores_total + 1L
  tab <- table(vapply(m, `[[`, "", "err")); print(tab)
  cat("Repetido = paso de la clave:", round(mean(vapply(m, function(v) v$rep == v$rol, TRUE)), 3), "\n")
}

# ---- 1b. ENUMERACIÓN EXACTA ----------------------------------------------------------
if (modo %in% c("enumerar", "todo")) {
  cat("\n=== 1b. ENUMERACIÓN EXACTA DEL ESPACIO (regla #13) ===\n")
  cod_e <- fijar_canon(COD, "FALSE")
  cod_e <- sust1(cod_e, "fila <- espacio[sample.int(nrow(espacio), 1L), ]", "fila <- .ST$fila")
  cod_e <- sust1(cod_e, "  d1 <- pick(cand)", ANCLA_D1)
  cod_e <- sust1(cod_e, "rol_clave <- pick(ROLES)", "rol_clave <- .ST$rol")
  cod_e <- sust1(cod_e, "  rol_repetido <- sample(ROLES, 1L, prob = ifelse(ROLES == rol_clave, 1 / 2, 1 / 4))",
                 "  error_cometido <- .ST$err\n  rol_repetido <- .ST$rep")
  ex_e <- compilar(cod_e)
  esp <- correr(EX_NOCANON, 1L)$espacio
  errs <- errs_del_generador(COD)
  if (!errs_amarre(COD)) { cat("AMARRE: las ramas de error del generador difieren de ERRS_ESPERADO\n"); errores_total <- errores_total + 1L }
  mult <- c(10L, 15L, 20L, 10L)
  n_est <- 0L; n_raz <- 0L; prob <- character(0); opc_tot <- 0L
  for (i in seq_len(nrow(esp))) {
    f <- esp[i, , drop = FALSE]
    m0 <- mult[f$dur - 1L]; cand <- seq(m0, 2L * f$v, by = m0); cand <- cand[cand != f$v & cand != f$D]
    for (d1 in cand) for (rol in names(errs)) for (err in errs[[rol]]) for (rp in c("dist", "tiem", "coc")) {
      assign(".ST", list(fila = f, d1 = d1, cand = cand, rol = rol, err = err, rep = rp), envir = globalenv())
      n_est <- n_est + 1L
      e <- tryCatch(correr(ex_e, n_est), error = function(x) x)
      if (inherits(e, "error")) { prob <- c(prob, paste("ABORTA:", conditionMessage(e))); next }
      opc_tot <- opc_tot + length(e$candidatos)
      for (cd in unique(unlist(e$candidatos))) {
        n_raz <- n_raz + 1L
        x <- switch(cd, "EV-D-FINAL" = d1 + f$D, "EV-D-SUMA" = 2L * d1 + f$D, "EV-D-SIGNO" = -f$D, "EV-D-INICIAL" = d1,
                    "EV-T-FINAL" = f$t2, "EV-T-SUMA" = f$t1 + f$t2, "EV-T-SIGNO" = -f$dur, "EV-T-INICIAL" = f$t1,
                    "EV-D-PROM" = (2L * d1 + f$D) %/% 2L, "EV-T-CUENTA" = f$dur + 1L, NA)
        if (is.na(x)) { if (cd == "EV-C-CIFRAS" && e$Tm / e$Dm == f$v) prob <- c(prob, "REPARA: EV-C-CIFRAS"); next }
        r <- if (startsWith(cd, "EV-D")) x / e$Tm else e$Dm / x
        if (r == f$v) prob <- c(prob, paste("REPARA:", cd, "con", err))
      }
      o <- oraculo(e); if (!is.na(o$problema)) prob <- c(prob, o$problema)
    }
  }
  cat(sprintf("Estados: %d | razones candidatas revisadas: %d | conjuntos candidatos: %d | problemas: %d\n", n_est, n_raz, opc_tot, length(prob)))
  if (length(prob)) print(utils::head(sort(table(prob), decreasing = TRUE), 12))
  errores_total <- errores_total + length(prob)
}

# ---- 2. BATERÍA §P7, congelada 2026-11-05 ------------------------------------------
# Definiciones (las comparten la batería de muestra, el modo exacto y los mutantes). Las reglas NO se tocan.
if (modo %in% c("bateria", "exacto", "mutantes", "mutantes_o2", "rapido", "todo")) {
  source(file.path(aqui, "..", "..", "..", ".claude", "scripts", "bateria_eliminacion.R"))
  P_CANON <- 0.05
  stopifnot("P_CANON del verificador = el del .Rmd" = grepl("P_CANON <- 0.05\n", COD, fixed = TRUE))
  paso_de <- function(op) as.integer(sub("^En el paso ([0-9]),.*$", "\\1", op))
  cuerpo  <- function(op) sub("^.* es ", "", op)
  afirm   <- function(op) { b <- sub(" y no .*$", "", cuerpo(op)); ifelse(grepl(" = ", b, fixed = TRUE), sub("^.* = ", "", b), b) }
  num1    <- function(op) vapply(afirm(op), function(b) as.numeric(regmatches(b, regexpr("-?[0-9]+", b))), 0, USE.NAMES = FALSE)
  nums    <- function(x) as.numeric(regmatches(x, gregexpr("[0-9]+", x))[[1]])
  conj    <- function(x) if (any(x) && !all(x)) x else NA      # no elimina nada: la regla no aplica
  tokens  <- function(s) unique(strsplit(gsub("[^[:alpha:]]+", " ", tolower(s)), " +")[[1]])
  L <- function(nom, fam, fn, est = FALSE) nueva_regla(nom, fam, fn, usa_estimulo = est)
  margen <- function(op, f) { n <- sort(nchar(op), decreasing = f); (abs(n[1] - n[2])) / n[2] }
  reglas <- list(
    # magnitud
    L("MAG-larga", "magnitud", function(o) conj(nchar(o) == max(nchar(o)))),
    L("MAG-corta", "magnitud", function(o) conj(nchar(o) == min(nchar(o)))),
    L("MAG-larga-margen15", "magnitud", function(o) if (margen(o, TRUE) >= .15) conj(nchar(o) == max(nchar(o))) else NA),
    L("MAG-corta-margen15", "magnitud", function(o) if (margen(o, FALSE) >= .15) conj(nchar(o) == min(nchar(o))) else NA),
    L("MAG-no-larga", "magnitud", function(o) conj(nchar(o) != max(nchar(o)))),
    L("MAG-no-corta", "magnitud", function(o) conj(nchar(o) != min(nchar(o)))),
    L("MAG-mayor-afirmado", "magnitud", function(o) conj(num1(o) == max(num1(o)))),
    L("MAG-menor-afirmado", "magnitud", function(o) conj(num1(o) == min(num1(o)))),
    L("MAG-mayor-abs", "magnitud", function(o) conj(abs(num1(o)) == max(abs(num1(o))))),   # añadida tras r0; todo re-medido
    L("MAG-segundo-afirmado", "magnitud", function(o) conj(rank(-num1(o), ties.method = "min") == 2L)),
    L("MAG-mas-cifras", "magnitud", function(o) { n <- nchar(gsub("[^0-9]", "", o)); conj(n == max(n)) }),
    # divisibilidad
    L("DIV-mult10", "divisibilidad", function(o) conj(num1(o) %% 10 == 0)),
    L("DIV-no-mult10", "divisibilidad", function(o) conj(num1(o) %% 10 != 0)),
    L("DIV-mult5", "divisibilidad", function(o) conj(num1(o) %% 5 == 0)),
    L("DIV-par", "divisibilidad", function(o) conj(num1(o) %% 2 == 0)),
    L("DIV-impar", "divisibilidad", function(o) conj(num1(o) %% 2 != 0)),
    # signo
    L("SIG-sin-negativo", "signo", function(o) conj(!grepl("-[0-9]", o))),
    L("SIG-con-negativo", "signo", function(o) conj(grepl("-[0-9]", o))),
    L("SIG-afirma-positivo", "signo", function(o) conj(num1(o) > 0)),
    # posición (el orden de las opciones lo sortea exshuffle: la posición es el paso que nombran)
    L("POS-paso1", "posicion", function(o) conj(paso_de(o) == 1L)),
    L("POS-paso2", "posicion", function(o) conj(paso_de(o) == 2L)),
    L("POS-paso3", "posicion", function(o) conj(paso_de(o) == 3L)),
    L("POS-paso-extremo", "posicion", function(o) conj(paso_de(o) != 2L)),
    # formato
    L("FMT-fraccion", "formato", function(o) conj(grepl("/", o, fixed = TRUE))),
    L("FMT-sin-fraccion", "formato", function(o) conj(!grepl("/", o, fixed = TRUE))),
    L("FMT-parentesis", "formato", function(o) conj(grepl("(", o, fixed = TRUE))),
    L("FMT-sin-parentesis", "formato", function(o) conj(!grepl("(", o, fixed = TRUE))),
    L("FMT-empieza-km", "formato", function(o) conj(grepl("^-?[0-9]+ km", cuerpo(o)))),
    L("FMT-empieza-h", "formato", function(o) conj(grepl("^-?[0-9]+ h", cuerpo(o)))),
    # léxico
    L("LEX-token-exclusivo", "lexico", function(o) {
      tk <- lapply(o, tokens); ex <- vapply(seq_along(o), function(i) length(setdiff(tk[[i]], unlist(tk[-i]))) > 0L, TRUE); conj(ex) }),
    L("LEX-sin-token-exclusivo", "lexico", function(o) {
      tk <- lapply(o, tokens); ex <- vapply(seq_along(o), function(i) length(setdiff(tk[[i]], unlist(tk[-i]))) > 0L, TRUE); conj(!ex) }),
    L("LEX-distancia", "lexico", function(o) conj(grepl("distancia", o, fixed = TRUE))),
    L("LEX-tiempo", "lexico", function(o) conj(grepl("tiempo", o, fixed = TRUE))),
    L("LEX-cociente", "lexico", function(o) conj(grepl("cociente", o, fixed = TRUE))),
    # relacionales entre opciones (§P7-E)
    L("REL-paso-repetido", "formato", function(o) { p <- paso_de(o); conj(p %in% p[duplicated(p)]) }),
    L("REL-paso-unico", "formato", function(o) { p <- paso_de(o); conj(!(p %in% p[duplicated(p)])) }),
    L("REL-comparte-mas-cifras", "magnitud", function(o) {
      n <- lapply(o, nums); s <- vapply(seq_along(o), function(i) sum(n[[i]] %in% unlist(n[-i])), 0L); conj(s == max(s)) }),
    L("REL-comparte-menos-cifras", "magnitud", function(o) {
      n <- lapply(o, nums); s <- vapply(seq_along(o), function(i) sum(n[[i]] %in% unlist(n[-i])), 0L); conj(s == min(s)) }),
    L("REL-repetido-menor", "magnitud", function(o) {
      p <- paso_de(o); r <- p[duplicated(p)]; if (!length(r)) return(NA); x <- abs(num1(o)); i <- which(p == r); if (length(unique(x[i])) < 2L) return(NA); i[which.min(x[i])] }),
    L("REL-repetido-mayor", "magnitud", function(o) {
      p <- paso_de(o); r <- p[duplicated(p)]; if (!length(r)) return(NA); x <- abs(num1(o)); i <- which(p == r); if (length(unique(x[i])) < 2L) return(NA); i[which.max(x[i])] }),
    L("REL-afirmado-citado-en-otra", "magnitud", function(o) {
      a <- abs(num1(o)); n <- lapply(o, nums); conj(vapply(seq_along(o), function(i) a[i] %in% unlist(n[-i]), TRUE)) }),
    L("REL-afirmado-no-citado", "magnitud", function(o) {
      a <- abs(num1(o)); n <- lapply(o, nums); conj(vapply(seq_along(o), function(i) !(a[i] %in% unlist(n[-i])), TRUE)) }),
    # añadidas tras r1, antes de la primera FASE 2C; todo re-medido (§P7-C)
    L("REL-afirmado-menor-que-rechazado", "signo", function(o) {
      if (!all(grepl(" y no ", o, fixed = TRUE))) return(NA)
      r <- vapply(sub("^.* y no ", "", o), function(b) as.numeric(regmatches(b, regexpr("-?[0-9]+", b))), 0, USE.NAMES = FALSE)
      conj(num1(o) < r) }),
    L("REL-afirmado-mayor-que-rechazado", "signo", function(o) {
      if (!all(grepl(" y no ", o, fixed = TRUE))) return(NA)
      r <- vapply(sub("^.* y no ", "", o), function(b) as.numeric(regmatches(b, regexpr("-?[0-9]+", b))), 0, USE.NAMES = FALSE)
      conj(num1(o) > r) }),
    L("EST-afirmado-menor-que-el-paso", "signo", function(o, e) {
      m <- function(n) { x <- regmatches(e, regexpr(paste0("Paso ", n, "\\..*?= (-?[0-9]+) "), e, perl = TRUE)); as.numeric(sub("^.*= (-?[0-9]+) $", "\\1", x)) }
      v <- c(m(1), m(2), m(3)); if (length(v) != 3L) return(NA); conj(num1(o) < v[paso_de(o)]) }, est = TRUE),
    L("EST-afirmado-mayor-que-el-paso", "signo", function(o, e) {
      m <- function(n) { x <- regmatches(e, regexpr(paste0("Paso ", n, "\\..*?= (-?[0-9]+) "), e, perl = TRUE)); as.numeric(sub("^.*= (-?[0-9]+) $", "\\1", x)) }
      v <- c(m(1), m(2), m(3)); if (length(v) != 3L) return(NA); conj(num1(o) > v[paso_de(o)]) }, est = TRUE),
    # estímulo (§P7-G): lo que muestran el enunciado y los pasos
    L("EST-afirmado-nuevo", "lexico", function(o, e) conj(!(abs(num1(o)) %in% nums(e))), est = TRUE),
    L("EST-afirmado-viejo", "lexico", function(o, e) conj(abs(num1(o)) %in% nums(e)), est = TRUE),
    L("EST-afirmado-en-enunciado", "lexico", function(o, e) {
      en <- sub(" Paso 1\\..*$", "", e); conj(abs(num1(o)) %in% nums(gsub("[0-9]+:00", "", en))) }, est = TRUE),
    L("EST-afirmado-no-en-enunciado", "lexico", function(o, e) {
      en <- sub(" Paso 1\\..*$", "", e); conj(!(abs(num1(o)) %in% nums(gsub("[0-9]+:00", "", en)))) }, est = TRUE),
    L("EST-todas-cifras-en-pasos", "lexico", function(o, e) {
      conj(vapply(o, function(x) all(nums(x) %in% nums(e)), TRUE, USE.NAMES = FALSE)) }, est = TRUE)
  )
  medir <- function(sub_m, etiqueta) {
    r <- evaluar_bateria(reglas, lapply(sub_m, `[[`, "ops"), vapply(sub_m, `[[`, 0L, "key"),
                         estimulos = lapply(sub_m, `[[`, "est"))
    d <- r$detalle
    cat(sprintf("\n[%s] n = %d | veredicto %s | máx %.1f %% (%s) | techo nulo %.1f %% | exceso %+.1f pp\n", etiqueta, length(sub_m),
                r$veredicto, 100 * r$max_obs, r$regla_top, 100 * r$techo_nulo, 100 * (r$max_obs - r$techo_nulo)))
    print(utils::head(transform(d[, c("regla", "familia", "tasa", "aplicabilidad")], tasa = round(100 * tasa, 1), aplicabilidad = round(100 * aplicabilidad)), 6), row.names = FALSE)
    invisible(r)
  }
  sust_val <- function(o, e) {
    c1 <- as.numeric(sub("^.*Paso 1\\..*?= (-?[0-9]+) km.*$", "\\1", e, perl = TRUE))
    c2 <- as.numeric(sub("^.*Paso 2\\..*?= (-?[0-9]+) h.*$", "\\1", e, perl = TRUE))
    lapply(o, function(x) { n <- paso_de(x); a <- afirm(x)
      if (n == 1L) return(list(v = num1(x) / c2, u = "km/h"))
      if (n == 2L) return(list(v = c1 / num1(x), u = "km/h"))
      m <- regmatches(a, regexec("^(-?[0-9]+) (km|h)/\\(?(-?[0-9]+) (km|h)", a))[[1]]
      list(v = as.numeric(m[2]) / as.numeric(m[4]), u = if (m[3] == "km") "km/h" else "h/km") })
  }
}

# ---- 2c. EXACTO: definiciones -----------------------------------------------------------------------------------
# Enumeración exacta (regla #23, excepción de espacio finito) de la batería §P7 congelada, los canales de sustitución, Q y las
# vigiladas, con los pesos reales de cada estado. Es lo ÚNICO que clasifica O-2 (§P7-E): nunca una cifra de muestra.
if (modo %in% c("exacto", "mutantes_o2", "todo")) {
  suppressPackageStartupMessages(library(parallel))
  NUEVAS <- c("EV-D-PROM", "EV-T-CUENTA")
  nr <- vapply(reglas, `[[`, "", "nombre")
  XN <- c("X0", "X1a", "X1b", "X1", "R30-90", "R20-120", "R10-150", "R20-120+ent")
  H3_ESP <- data.frame(   # H3 de la r4 (tabla del reporte, modelo Python det4/compuestas.py): no canónicas, mezcla y canónica, en %
    regla = c("NM+frac", "NM+pasos", "NM+kmh", "NM+X0", "NM+X0+frac", "NM+kmh+frac", "NM+pos+frac", "EST-1", "EST+-1", "EST+-1-par",
              "E3-1", "E3-1-par", "M3-par", "entre-media", "E3-1-par-media"),
    nocanon = c(56.08, 54.70, 53.88, 50.30, 64.45, 64.26, 63.17, 44.55, 46.26, 49.87, 73.73, 78.05, 95.44, 47.97, 98.47),
    mezcla = c(54.94, 53.22, 52.86, 50.28, 63.73, 62.71, 65.01, 43.57, 45.20, 48.62, 75.05, 79.14, 95.67, 46.82, 98.54),
    canon = c(33.33, 25, 33.33, 50, 50, 33.33, 100, 25, 25, 25, 100, 100, 100, 25, 100), stringsAsFactors = FALSE)
  EXTRA <- c("EST-sin-rotulos", "RE", "RE-con-fraccion", "U", "NM", "X0+", "EST+", "E1", "E2", "E3", "M0", "M2", "M3", "entre", H3_ESP$regla)
  MEDIBLES <- c(nr, XN, "Q", EXTRA)                                   # lo que clasifica O-2
  MET <- c(nr, XN, "solo R30-90", "solo R20-120", "Q", EXTRA)
  stopifnot(!anyDuplicated(MET), all(FIRMA_O2$regla %in% MEDIBLES), all(FIRMA_O2$ambito %in% c("nocanon", "mezcla")),
            !anyDuplicated(paste(FIRMA_O2$regla, FIRMA_O2$ambito)), all(FIRMA_O2$exceso >= 8))
  sc_log <- function(s) { k <- sum(s); if (k == 0L || k == 4L) .25 else if (s[1L]) 1 / k else 0 }
  # Una regla que LANZA un error devuelve NA (no «no aplica»: ese NA lo resuelve la regla y da 25 %). El error se cuenta.
  sc_regla <- function(r, op, est) {
    S <- tryCatch(.conjunto_de(.aplicar_regla(r, op, est), 4L), error = function(e) NULL)
    if (is.null(S)) return(NA_real_)
    sc_log(S)
  }
  # Lo que muestra el paso n: resultado de los pasos 1 y 2; numerador del paso 3 (es el «rechazado» implícito).
  c_paso <- function(est, n) {
    if (n == 1L) {
      as.numeric(sub("^.*Paso 1\\..*?= (-?[0-9]+) km.*$", "\\1", est, perl = TRUE))
    } else if (n == 2L) {
      as.numeric(sub("^.*Paso 2\\..*?= (-?[0-9]+) h.*$", "\\1", est, perl = TRUE))
    } else nums(sub("^.*Paso 3\\.[^:]*:", "", est))[1L]
  }
  # op[1] es la clave. Devuelve las métricas (puntuación de cada una) con el atributo «err»: reglas que fallaron.
  metricas <- function(op, est, rg = reglas) {
    v <- numeric(length(MET)); names(v) <- MET; fallas <- character(0)
    ev <- function(nm, expr) tryCatch(expr, error = function(e) { fallas <<- c(fallas, nm); .25 })
    for (j in seq_along(rg)) {
      x <- sc_regla(rg[[j]], op, est)
      if (is.na(x)) { fallas <- c(fallas, rg[[j]]$nombre); x <- .25 }
      v[j] <- x
    }
    ctx <- tryCatch({
      sv <- sust_val(op, est); val <- vapply(sv, `[[`, 0, "v"); kmh_ <- vapply(sv, function(s) s$u == "km/h", TRUE)
      en <- sub(" Paso 1\\..*$", "", est); pso <- paso_de(op); n_ <- num1(op); a_ <- abs(n_); pos <- n_ > 0
      rn <- vapply(pso, function(p) c_paso(est, p), 0)      # rechazado implícito: lo que muestra el paso de cada opción
      p3 <- nums(sub("^.*Paso 3\\.[^:]*:", "", est))[1:2]
      af_ <- lapply(sub(" y no .*$", "", cuerpo(op)), nums)
      k_ <- as.numeric(sub(" km", "", regmatches(en, gregexpr("[0-9]+ km\\b", en))[[1]]))
      h_ <- as.numeric(sub(":.*", "", regmatches(en, gregexpr("[0-9]+:00", en))[[1]]))
      pas_ <- nums(gsub("Paso [0-9]\\.", "", substring(est, regexpr("Paso 1\\.", est))))   # cifras de los tres pasos, sin rótulos
      list(sv = sv, val = val, kmh_ = kmh_, en = en, pso = pso, n_ = n_, a_ = a_, pos = pos, rn = rn, p3 = p3, af_ = af_, k_ = k_, h_ = h_, pas_ = pas_)
    }, error = function(e) NULL)
    if (is.null(ctx)) { v[c(XN, "solo R30-90", "solo R20-120", "Q", EXTRA)] <- .25; attr(v, "err") <- c(fallas, "contexto"); return(v) }
    list2env(ctx, environment())
    x0 <- !(a_ %in% nums(en)); xa <- val > 0 & kmh_; xb <- val == round(val)
    r1 <- kmh_ & val >= 30 & val <= 90; r2 <- kmh_ & val >= 20 & val <= 120; r3 <- kmh_ & val >= 10 & val <= 150
    v["X0"] <- ev("X0", sc_log(x0)); v["X1a"] <- ev("X1a", sc_log(xa)); v["X1b"] <- ev("X1b", sc_log(xb)); v["X1"] <- ev("X1", sc_log(xa & xb))
    v["R30-90"] <- ev("R30-90", sc_log(r1)); v["R20-120"] <- ev("R20-120", sc_log(r2)); v["R10-150"] <- ev("R10-150", sc_log(r3))
    v["R20-120+ent"] <- ev("R20-120+ent", sc_log(r2 & xb))
    # Q: la opción cuyo valor afirmado es el opuesto del que rechaza (quitar el menos). Solo con la forma «y no» en las cuatro.
    v["Q"] <- ev("Q", if (all(grepl(" y no ", op, fixed = TRUE))) {
      rech <- vapply(sub("^.* y no ", "", op), function(b) as.numeric(regmatches(b, regexpr("-?[0-9]+", b))), 0, USE.NAMES = FALSE)
      sc_log(rech < 0 & n_ == -rech)
    } else .25)
    v["solo R30-90"] <- as.numeric(sum(r1) == 1L && r1[1L]); v["solo R20-120"] <- as.numeric(sum(r2) == 1L && r2[1L])
    # Vigiladas (las definiciones exactas de opcion_c/exp.py). El rechazado de RE, U, NM y RE-con-fraccion es el implícito.
    est_nr <- !(a_ %in% nums(gsub("Paso [0-9]\\.", "", est)))
    v["EST-sin-rotulos"] <- ev("EST-sin-rotulos", sc_log(est_nr))
    v["RE"] <- ev("RE", sc_log(pso < 3L & rn %in% nums(en)))                       # lectura literal: solo pasos 1 y 2
    v["RE-con-fraccion"] <- ev("RE-con-fraccion", sc_log(rn %in% nums(en)))        # incluye el primer número de la fracción
    v["U"] <- ev("U", sc_log(pso == 3L & vapply(af_, function(a) identical(a[1:2], p3), TRUE)))
    v["NM"] <- ev("NM", sc_log(n_ <= rn))
    v["X0+"] <- ev("X0+", sc_log(x0 & pos)); v["EST+"] <- ev("EST+", sc_log(est_nr & pos))
    cota <- ifelse(pso == 1L, max(k_), max(h_))
    e1 <- (pso == 3L) | (pos & !(a_ %in% nums(en)) & a_ <= cota); e2 <- e1 & (pso != 3L | kmh_); e3 <- e2 & (pso != 3L | val >= 1)
    v["E1"] <- ev("E1", sc_log(e1)); v["E2"] <- ev("E2", sc_log(e2)); v["E3"] <- ev("E3", sc_log(e3))
    p1_ <- sub("^.*(Paso 1\\..*) Paso 2\\..*$", "\\1", est); p2_ <- sub("^.*(Paso 2\\..*) Paso 3\\..*$", "\\1", est); p3_ <- sub("^.*(Paso 3\\..*)$", "\\1", est)
    mk <- if (grepl(" + ", p1_, fixed = TRUE) || grepl("- 0 km", p1_, fixed = TRUE) || grepl("= -", p1_, fixed = TRUE)) 1L else if (grepl("- 0 h", p2_, fixed = TRUE) || grepl("= -", p2_, fixed = TRUE)) 2L else if (grepl("h/km", p3_, fixed = TRUE)) 3L else NA_integer_
    v["M0"] <- ev("M0", sc_log(pso == mk)); v["M2"] <- ev("M2", sc_log(pso == mk & pos & !(a_ %in% nums(en)))); v["M3"] <- ev("M3", sc_log(pso == mk & e3))
    v["entre"] <- ev("entre", sc_log(ifelse(pso == 1L, n_ > k_[1L] & n_ < k_[2L], ifelse(pso == 2L, n_ > h_[1L] & n_ < h_[2L], FALSE))))
    # H3 de la r4: conjunciones sin cálculo (definiciones de det4/compuestas.py y rasgos.py). El rechazado de NM es el implícito (rn).
    nm_ <- n_ <= rn; en_e <- a_ %in% nums(en); en_p <- a_ %in% pas_
    fh_ <- pso == 3L & !kmh_
    fm_ <- pso == 3L & vapply(af_, function(a) length(a) >= 2L && a[1L] < a[2L], TRUE)
    d1r <- pso < 3L & n_ == rn + 1L
    d1m <- vapply(seq_along(n_), function(i) { j <- which(seq_along(n_) != i & pso == pso[i] & abs(n_ - n_[i]) == 1)
      pso[i] < 3L && length(j) > 0L && any(n_[i] > n_[j]) }, TRUE)
    ml_ <- pso == 1L & 2 * n_ == k_[1L] + k_[2L]
    ent_ <- ifelse(pso == 1L, n_ > k_[1L] & n_ < k_[2L], ifelse(pso == 2L, n_ > h_[1L] & n_ < h_[2L], FALSE))
    v["NM+frac"] <- ev("NM+frac", sc_log(nm_ & !fm_)); v["NM+pasos"] <- ev("NM+pasos", sc_log(nm_ & !en_p))
    v["NM+kmh"] <- ev("NM+kmh", sc_log(nm_ & !fh_)); v["NM+X0"] <- ev("NM+X0", sc_log(nm_ & !en_e))
    v["NM+X0+frac"] <- ev("NM+X0+frac", sc_log(nm_ & !en_e & !fm_)); v["NM+kmh+frac"] <- ev("NM+kmh+frac", sc_log(nm_ & !fh_ & !fm_))
    v["NM+pos+frac"] <- ev("NM+pos+frac", sc_log(pos & nm_ & !fm_))
    v["EST-1"] <- ev("EST-1", sc_log(est_nr & !d1r)); v["EST+-1"] <- ev("EST+-1", sc_log(est_nr & pos & !d1r))
    v["EST+-1-par"] <- ev("EST+-1-par", sc_log(est_nr & pos & !d1r & !d1m))
    v["E3-1"] <- ev("E3-1", sc_log(e3 & !d1r)); v["E3-1-par"] <- ev("E3-1-par", sc_log(e3 & !d1r & !d1m))
    v["M3-par"] <- ev("M3-par", sc_log(pso == mk & e3 & !d1m))
    v["entre-media"] <- ev("entre-media", sc_log(ent_ & !ml_)); v["E3-1-par-media"] <- ev("E3-1-par-media", sc_log(e3 & !d1r & !d1m & !ml_))
    attr(v, "err") <- fallas
    v
  }
  # Mismas anclas que el 1b: un estado se fija por .ST y el resto del generador corre tal cual. d1 se ancla contra cand.
  preparar <- function(cod) {
    c0 <- fijar_canon(cod, "FALSE")
    c0 <- sust1(c0, "fila <- espacio[sample.int(nrow(espacio), 1L), ]", "fila <- .ST$fila")
    c0 <- sust1(c0, "  d1 <- pick(cand)", ANCLA_D1)
    c0 <- sust1(c0, "rol_clave <- pick(ROLES)", "rol_clave <- .ST$rol")
    c0 <- sust1(c0, "  rol_repetido <- sample(ROLES, 1L, prob = ifelse(ROLES == rol_clave, 1 / 2, 1 / 4))",
                "  error_cometido <- .ST$err\n  rol_repetido <- .ST$rep")
    compilar(c0)
  }
  MULT <- c(10L, 15L, 20L, 10L)
  FALLOS <- c("opciones repetidas", "razón falsa afirma el valor correcto", "razón afirma lo que rechaza su paso",
              "más de una razón de signo", "razón FINAL contra el paso mal", "razón falsa que, sustituida, da la rapidez correcta",
              "la clave no da la rapidez correcta en km/h")
  afirma_correcto <- function(o, e) switch(o$rol, dist = o$valor[1L] == e$D, tiem = o$valor[1L] == e$dur,
    coc = identical(o$unidad, "km/h") && o$valor[1L] == e$D && o$valor[2L] == e$dur)
  # Devuelve M (suma ponderada de cada métrica por rama de error), P (peso de los estados que no abortan), los abortos, los conjuntos,
  # los fallos de las comprobaciones sobre TODOS los conjuntos de peso > 0, el peso de los conjuntos con razón nueva (NU) y los
  # errores dentro de reglas (ERN). Las ramas de error salen del generador (H4).
  enumerar <- function(cod, etq) {
    errs <- errs_del_generador(cod)
    expr <- preparar(cod); est <- list()
    # El espacio no depende del mutante; si la semilla 1 aborta en el generador mutado, se toma del original.
    esp <- tryCatch(correr(compilar(fijar_canon(cod, "FALSE")), 1L)$espacio, error = function(x) correr(EX_NOCANON, 1L)$espacio)
    for (i in seq_len(nrow(esp))) {
      f <- esp[i, , drop = FALSE]; m0 <- MULT[f$dur - 1L]; cand <- seq(m0, 2L * f$v, by = m0); cand <- cand[cand != f$v & cand != f$D]
      for (d1 in cand) for (rol in names(errs)) for (err in errs[[rol]]) for (rp in c("dist", "tiem", "coc")) {
        pst <- 1 / nrow(esp) / length(cand) / 3 / length(errs[[rol]]) * (if (rp == rol) 1 / 2 else 1 / 4)
        est[[length(est) + 1L]] <- list(f = f, d1 = d1, cand = cand, rol = rol, err = err, rp = rp, p = pst)
      }
    }
    stopifnot(abs(sum(vapply(est, `[[`, 0, "p")) - 1) < 1e-9)
    ramas <- sort(unique(unlist(errs)))
    bloques <- split(seq_along(est), cut(seq_along(est), 60L, labels = FALSE))
    res <- mclapply(bloques, function(idx) {
      M <- matrix(0, length(ramas), length(MET), dimnames = list(ramas, MET)); P <- setNames(numeric(length(ramas)), ramas)
      ab <- 0L; ns <- 0L; Fl <- setNames(integer(length(FALLOS)), FALLOS); NU <- 0; ERN <- character(0)
      for (n in idx) {
        s <- est[[n]]; assign(".ST", list(fila = s$f, d1 = s$d1, cand = s$cand, rol = s$rol, err = s$err, rep = s$rp), envir = globalenv())
        e <- tryCatch(correr(expr, n), error = function(x) x)
        if (inherits(e, "error")) { ab <- ab + 1L; next }
        w <- e$pesos / sum(e$pesos); cl <- ver(e$clave$texto); ee <- ver(paste(e$enunciado, e$paso_1, e$paso_2, e$paso_3))
        cods <- unique(unlist(e$candidatos)); fa <- setNames(lapply(cods, e$falsa), cods)
        tx <- vapply(fa, function(o) ver(o$texto), "")
        for (j in seq_along(e$candidatos)) if (w[j] > 0) {
          cj <- e$candidatos[[j]]; ops_j <- c(list(e$clave), fa[cj]); op <- unname(c(cl, tx[cj]))
          sv <- sust_val(op, ee)
          Fl[1L] <- Fl[1L] + (length(unique(vapply(ops_j, function(o) o$texto, ""))) != 4L)
          Fl[2L] <- Fl[2L] + any(vapply(fa[cj], afirma_correcto, TRUE, e = e))
          Fl[3L] <- Fl[3L] + any(vapply(ops_j, function(o) o$afirma == e$rechaza[[o$rol]], TRUE))
          Fl[4L] <- Fl[4L] + (sum(grepl("SIGNO$", cj)) > 1L)
          Fl[5L] <- Fl[5L] + any(vapply(cj, function(cd) cd %in% c("EV-D-FINAL", "EV-T-FINAL") && e$errores_conceptuales[[cd]]$rol == e$rol_clave, TRUE))
          Fl[6L] <- Fl[6L] + any(vapply(sv[-1L], function(z) isTRUE(abs(z$v - s$f$v) < 1e-9) && z$u == "km/h", TRUE))
          Fl[7L] <- Fl[7L] + !(isTRUE(abs(sv[[1L]]$v - s$f$v) < 1e-9) && sv[[1L]]$u == "km/h")
          mm <- metricas(op, ee); ERN <- c(ERN, attr(mm, "err"))
          M[s$err, ] <- M[s$err, ] + s$p * w[j] * as.numeric(mm); ns <- ns + 1L
          if (any(cj %in% NUEVAS)) NU <- NU + s$p * w[j]
        }
        P[s$err] <- P[s$err] + s$p
      }
      list(M = M, P = P, ab = ab, ns = ns, F = Fl, NU = NU, ERN = ERN)
    }, mc.cores = 10L, mc.preschedule = FALSE)
    malos <- vapply(res, function(x) !is.list(x), TRUE)
    if (any(malos)) stop("un bloque de la enumeración falló: ", paste(as.character(res[[which(malos)[1L]]]), collapse = " "))
    list(etq = etq, errs = errs, M = Reduce(`+`, lapply(res, `[[`, "M")), P = Reduce(`+`, lapply(res, `[[`, "P")),
         ab = sum(vapply(res, `[[`, 0L, "ab")), ns = sum(vapply(res, `[[`, 0L, "ns")), n = length(est),
         F = Reduce(`+`, lapply(res, `[[`, "F")), NU = sum(vapply(res, `[[`, 0, "NU")),
         ERN = as.character(unlist(lapply(res, `[[`, "ERN"))))
  }
  # La canónica: una sola instancia, con la clave primera.
  ec <- correr(EX_CANON, 1L); oc <- ver(ec$opciones); kc <- which(ec$sol == 1L)
  ee_c <- ver(paste(ec$enunciado, ec$paso_1, ec$paso_2, ec$paso_3))
  CANON <- metricas(oc[c(kc, setdiff(1:4, kc))], ee_c)

  # ---- O-2: alcance, regla por regla y en los dos ámbitos (§P7-E, regla #24 H-5) ----------------------------------
  # tot: métricas exactas de las no canónicas; canon: las de la canónica; «mezcla» = (1 - P_CANON) tot + P_CANON canon, para TODAS.
  # Veredicto de cada combinación regla-ámbito con exceso >= 8 (o listada en FIRMA_O2):
  #   misma cifra que FIRMA_O2 = listada; por encima, o fuera de la tabla = pendiente; por debajo = no_reproduce.
  clasificar_o2 <- function(tot, canon, firma = FIRMA_O2) {
    mezcla <- (1 - P_CANON) * tot[MEDIBLES] + P_CANON * canon[MEDIBLES]
    cand <- expand.grid(regla = MEDIBLES, ambito = c("nocanon", "mezcla"), stringsAsFactors = FALSE)
    frac <- mapply(function(r, a) if (a == "mezcla") mezcla[[r]] else tot[[r]], cand$regla, cand$ambito, USE.NAMES = FALSE)
    cand$medida <- 100 * frac; cand$exceso <- round(100 * (frac - TECHO_O2), 2)
    cand$firmado <- firma$exceso[match(paste(cand$regla, cand$ambito), paste(firma$regla, firma$ambito))]
    f <- cand[cand$exceso >= 8 | !is.na(cand$firmado), , drop = FALSE]
    f$nueva <- is.na(f$firmado)
    f$veredicto <- ifelse(f$nueva | f$exceso > f$firmado + 1e-6, "pendiente", ifelse(abs(f$exceso - f$firmado) <= 1e-6, "listada", "no_reproduce"))
    f$tipo <- ifelse(f$regla %in% nr, "bateria", "vigilada")
    rownames(f) <- NULL; f
  }
  clave_o2 <- function(cl) setNames(cl$veredicto, paste(cl$regla, cl$ambito))
  imprimir_o2 <- function(cl) {
    o <- cl; o$medida <- round(o$medida, 2); o$regla <- ifelse(o$nueva, paste0(o$regla, " (NUEVA)"), o$regla)
    print(o[, c("regla", "tipo", "ambito", "medida", "exceso", "firmado", "veredicto")], row.names = FALSE)
  }
  # Comparación con el modelo Python de la variante C (opcion_c/c2.json), a la centésima. Cualquier diferencia es un error.
  comparar_modelo <- function(EXR, tot) {
    dir_c <- file.path(aqui, "salida", "medicion_exacta", "opcion_c")
    js <- jsonlite::fromJSON(file.path(dir_c, "c2.json"))
    equiv <- c("EST sin rotulos" = "EST-sin-rotulos", "EST con rotulos" = "EST-afirmado-nuevo", "MAG-segundo" = "MAG-segundo-afirmado", "RE" = "RE")
    mio <- function(k) if (k %in% names(equiv)) equiv[[k]] else k
    filas <- list()
    add <- function(m, amb, a, b, dec = 2L) filas[[length(filas) + 1L]] <<- data.frame(metrica = m, ambito = amb, modelo = a, verificador = b,
      dif = b - a, ok = (abs(b - a) < 1e-6) || isTRUE(round(a, dec) == round(b, dec)), stringsAsFactors = FALSE)
    for (k in names(js$tot)) {
      add(k, "total", js$tot[[k]], 100 * tot[[mio(k)]])
      for (b in names(js$ram[[k]])) add(k, b, js$ram[[k]][[b]], 100 * EXR$M[b, mio(k)] / EXR$P[b])
    }
    add("n", "estados", js$n, EXR$n, 0L); add("ns", "conjuntos", js$ns, EXR$ns, 0L); add("ab", "abortos", js$ab, EXR$ab, 0L)
    add("nueva", "peso con razón nueva", js$nueva, EXR$NU / sum(EXR$P), 5L)
    add("dos", "dos claves o duplicadas", js$dos, EXR$F[[1L]] + EXR$F[[6L]], 0L); add("noreal", "clave no da v", js$noreal, EXR$F[[7L]], 0L)
    bq <- readLines(file.path(dir_c, "busq_c2.txt"), warn = FALSE)
    cifra <- function(lit) as.numeric(strsplit(trimws(grep(paste0("\\s", lit, "$"), bq, value = TRUE)[1L]), "\\s+")[[1L]][1L])
    add("RE-con-fraccion", "total (busq_c2.txt)", cifra("rech_en_enun"), 100 * tot[["RE-con-fraccion"]])
    add("entre", "total (busq_c2.txt)", cifra("entre"), 100 * tot[["entre"]])
    do.call(rbind, filas)
  }
  # H4 de la r4: cifra predicha por el modelo Python ANTES de correr el R (opcion_c/c1.json, base.json), a la centésima, en toda fila de
  # «tot», más n, ns y el peso con razón nueva. Lo usan los mutantes O2-A y O2-B.
  comparar_json <- function(EXR, tot, fichero) {
    js <- jsonlite::fromJSON(file.path(aqui, "salida", "medicion_exacta", "opcion_c", fichero))
    equiv <- c("EST sin rotulos" = "EST-sin-rotulos", "EST con rotulos" = "EST-afirmado-nuevo", "MAG-segundo" = "MAG-segundo-afirmado", "RE" = "RE")
    mio <- function(k) if (k %in% names(equiv)) equiv[[k]] else k
    filas <- list()
    add <- function(m, a, b, dec = 2L) filas[[length(filas) + 1L]] <<- data.frame(metrica = m, modelo = a, verificador = b, dif = b - a,
      ok = (abs(b - a) < 1e-6) || isTRUE(round(a, dec) == round(b, dec)), stringsAsFactors = FALSE)
    for (k in names(js$tot)) add(k, js$tot[[k]], 100 * tot[[mio(k)]])
    add("n", js$n, EXR$n, 0L); add("ns", js$ns, EXR$ns, 0L); add("nueva", js$nueva, EXR$NU / sum(EXR$P), 5L)
    do.call(rbind, filas)
  }
  # H3 de la r4: las 15 reglas, no canónicas, mezcla y canónica, contra H3_ESP (cifras escritas arriba), a la centésima.
  comparar_h3 <- function(tot, canon) {
    r <- H3_ESP$regla; mez <- (1 - P_CANON) * tot[r] + P_CANON * canon[r]
    mk <- function(amb, a, b) data.frame(regla = r, ambito = amb, esperada = a, verificador = round(100 * b, 4), dif = round(100 * b - a, 4),
      ok = abs(round(100 * b, 2) - a) < 1e-9, stringsAsFactors = FALSE)
    rbind(mk("nocanon", H3_ESP$nocanon, tot[r]), mk("mezcla", H3_ESP$mezcla, mez), mk("canonica", H3_ESP$canon, canon[r]))
  }
}

if (modo %in% c("bateria", "rapido", "todo")) {
  cat("\n=== 2. BATERÍA §P7 (", length(reglas), " reglas, N = 100 por estrato) ===\n", sep = "")
  m <- muestrear(EX_NOCANON)
  r_all <- medir(m, "no canónicas, N = 100 por rol")
  rg <- vapply(m, function(v) { x <- num1(v$ops); as.integer(1L + sum(x > x[v$key])) }, 0L)
  cat("\nRango de la clave entre las cifras afirmadas (1 = la mayor), total:\n"); print(round(table(rg) / length(rg), 3))
  print(round(prop.table(table(rol = vapply(m, `[[`, "", "rol"), rango = rg), 1), 3))
  for (x in c("dist", "tiem", "coc")) medir(Filter(function(v) v$rol == x, m), paste0("rol clave = ", x))
  for (x in sort(unique(vapply(m, `[[`, "", "err")))) { sm <- Filter(function(v) v$err == x, m)
    cat(sprintf("[error %s] n = %d: %s\n", x, length(sm), if (length(sm) < 20L) "NO CONCLUYENTE (n < 20)" else "se mide en la mezcla")) }
  # Canónica: una sola instancia, enumeración exacta. Puntuación 1/|S| si la clave sobrevive.
  ec <- correr(EX_CANON, 1L); oc <- ver(ec$opciones); kc <- which(ec$sol == 1L)
  ec_est <- ver(paste(ec$enunciado, ec$paso_1, ec$paso_2, ec$paso_3))
  cat("\n=== Canónica (enumeración exacta) y mezcla con P_CANON =", P_CANON, "===\n")
  tas <- r_all$detalle; rownames(tas) <- tas$regla
  filas <- lapply(reglas, function(R) {
    s <- tryCatch(if (R$usa_estimulo) R$fn(oc, ec_est) else R$fn(oc), error = function(e) NA)
    S <- if (is.logical(s) && length(s) == 4L) which(s) else if (is.numeric(s) && length(s) == 1L && !is.na(s)) as.integer(s) else integer(0)
    sc <- if (!length(S)) .25 else if (kc %in% S) 1 / length(S) else 0
    data.frame(regla = R$nombre, canon = sc, nocanon = tas[R$nombre, "tasa"], mezcla = P_CANON * sc + (1 - P_CANON) * tas[R$nombre, "tasa"])
  })
  fc <- do.call(rbind, filas); fc <- fc[order(-fc$mezcla), ]
  fc$exceso_mezcla_pp <- round(100 * (fc$mezcla - r_all$techo_nulo), 1)
  print(utils::head(transform(fc, canon = round(100 * canon, 1), nocanon = round(100 * nocanon, 1), mezcla = round(100 * mezcla, 1)), 10), row.names = FALSE)
  cat("Reglas que aíslan la clave en la canónica (puntuación 100 %):", paste(fc$regla[fc$canon == 1], collapse = ", "), "\n")
  cat("Reglas de la mezcla por encima de +8 pp:", paste(fc$regla[fc$exceso_mezcla_pp >= 8], collapse = ", "), "\n")

  # Canales que encontró la FASE 2C r1 y que la batería congelada no mide. §P7-C: se declaran con su cifra y no
  # cambian la vara de este ciclo. X0 es la regla EST con las horas conservadas; la congelada las borraba.
  cat("\n=== Canales declarados, FUERA de la batería congelada (H4 y H5 de la FASE 2C r1) ===\n")
  X <- list(
    "X0 no repite un dato del enunciado (con horas)" = function(o, e) { en <- sub(" Paso 1\\..*$", "", e); !(abs(num1(o)) %in% nums(en)) },
    "X1 sustituida da positiva, km/h y entera" = function(o, e) vapply(sust_val(o, e), function(s) s$v > 0 && s$u == "km/h" && s$v == round(s$v), TRUE),
    "X1a sustituida da positiva y km/h (constructo)" = function(o, e) vapply(sust_val(o, e), function(s) s$v > 0 && s$u == "km/h", TRUE),
    "X1b sustituida da entera" = function(o, e) vapply(sust_val(o, e), function(s) s$v == round(s$v), TRUE))
  punt <- function(s, k) { if (!any(s) || all(s)) return(.25); if (s[k]) 1 / sum(s) else 0 }
  m2 <- muestrear(EX_NOCANON, inicio = 200001L)
  for (nom in names(X)) {
    t1_ <- mean(vapply(m, function(v) punt(X[[nom]](v$ops, v$est), v$key), 0))
    t2_ <- mean(vapply(m2, function(v) punt(X[[nom]](v$ops, v$est), v$key), 0))
    ap <- mean(vapply(m, function(v) { s <- X[[nom]](v$ops, v$est); any(s) && !all(s) }, TRUE))
    cn <- punt(X[[nom]](oc, ec_est), kc)
    cat(sprintf("%-48s muestra 1: %5.1f %% (%+.1f pp) | muestra 2: %5.1f %% (%+.1f pp) | aplica %3.0f %% | canónica %3.0f %%\n", nom,
                100 * t1_, 100 * (t1_ - r_all$techo_nulo), 100 * t2_, 100 * (t2_ - r_all$techo_nulo), 100 * ap, 100 * cn))
  }
}

# ---- 2b. H1 por rama sobre el texto VISIBLE (Incidente F) y versiones únicas --------------
if (modo %in% c("h1", "rapido", "todo")) {
  cat("\n=== 2b. H1 POR RAMA, TEXTO VISIBLE (N = 100 por rol; margen = sobre la rival más próxima) ===\n")
  m <- muestrear(EX_NOCANON)
  fila <- function(sm, nom) {
    largo <- function(v) { n <- nchar(v$ops); k <- n[v$key]; r <- n[-v$key]
      c(larga = k > max(r), corta = k < min(r), m_l = (k - max(r)) / max(r), m_c = (min(r) - k) / min(r)) }
    x <- t(vapply(sm, largo, numeric(4)))
    data.frame(rama = nom, n = nrow(x), mas_larga = round(100 * mean(x[, 1])), margen_med_l = round(100 * median(x[x[, 1] == 1, 3]), 1),
               l_expl = round(100 * mean(x[, 1] == 1 & x[, 3] >= .15)), mas_corta = round(100 * mean(x[, 2])),
               margen_med_c = round(100 * median(x[x[, 2] == 1, 4]), 1), c_expl = round(100 * mean(x[, 2] == 1 & x[, 4] >= .15)))
  }
  tabla <- rbind(fila(m, "total"), do.call(rbind, lapply(c("dist", "tiem", "coc"), function(r) fila(Filter(function(v) v$rol == r, m), r))))
  print(tabla, row.names = FALSE)
  cat("l_expl y c_expl: % de versiones con la clave en el extremo Y margen >= 15 % (§P7-B).\n")
  ex <- compilar(COD)
  huellas <- vapply(1:300, function(s) { e <- correr(ex, s); digest::digest(paste(e$enunciado, e$paso_1, e$paso_2, e$paso_3, paste(sort(e$opciones), collapse = "|"))) }, "")
  cat("Versiones únicas (enunciado, pasos y opciones sin orden) en 300 semillas:", length(unique(huellas)), "\n")
  clv <- vapply(1:300, function(s) { e <- correr(ex, s); ver(e$opciones[e$sol == 1L]) }, "")
  cat("Claves distintas:", length(unique(clv)), "| canónicas en 300:", sum(vapply(1:300, function(s) correr(ex, s)$es_canonica, TRUE)), "\n")
}

# ---- 2c. EXACTO: corrida, clasificación de O-2 y comparación con el modelo ---------------------------------------------
O2_EVAL <- FALSE; O2_LIST <- 0L; O2_PEND <- 0L; O2_NOREP <- 0L; MAX_EXACTA <- NULL
if (modo %in% c("exacto", "todo")) {
  cat("\n=== 2c. ENUMERACIÓN EXACTA: batería, canales, vigiladas y O-2 (regla #23, espacio finito; techo constante", 100 * TECHO_O2, "%) ===\n")
  t0 <- Sys.time()
  if (!errs_amarre(COD)) { cat("AMARRE: las ramas de error del generador difieren de ERRS_ESPERADO\n"); errores_total <- errores_total + 1L }
  EXR <- enumerar(COD, "actual")
  tot <- colSums(EXR$M) / sum(EXR$P)
  cat(sprintf("Estados: %d | conjuntos de razones: %d | estados que abortan: %d | %.1f min\n", EXR$n, EXR$ns, EXR$ab,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  cat("Ramas de error (del switch del generador):", paste(names(EXR$errs), vapply(EXR$errs, paste, "", collapse = "+"), sep = "=", collapse = " | "), "\n")
  if (EXR$ab > 0L) { cat("  ABORTAN estados del espacio actual\n"); errores_total <- errores_total + EXR$ab }
  cat("Comprobaciones sobre TODOS los conjuntos de peso > 0 (fallos):\n"); print(EXR$F)
  errores_total <- errores_total + sum(EXR$F)
  ern <- c(EXR$ERN, attr(CANON, "err"))
  if (length(ern)) {
    cat("ERROR DENTRO DE UNA REGLA (no puntúa 25 %: es un fallo), por regla:\n"); print(table(ern))
    errores_total <- errores_total + length(ern)
  } else cat("Errores dentro de reglas o métricas: 0\n")
  mezcla_ <- (1 - P_CANON) * tot[nr] + P_CANON * CANON[nr]
  b <- sort(tot[nr], decreasing = TRUE)[1:10]
  cat("\nLas 10 primeras de las 51 reglas, no canónicas (exacta; exceso sobre", 100 * TECHO_O2, "%; canónica; mezcla con P_CANON =", P_CANON, "):\n")
  print(data.frame(regla = names(b), exacta = round(100 * b, 2), exceso = round(100 * (b - TECHO_O2), 2),
                   canonica = round(100 * CANON[names(b)]), mezcla05 = round(100 * mezcla_[names(b)], 2)), row.names = FALSE)
  bm <- sort(mezcla_, decreasing = TRUE)[1:5]
  cat("\nLas 5 primeras de la mezcla:\n")
  print(data.frame(regla = names(bm), mezcla05 = round(100 * bm, 2), exceso = round(100 * (bm - TECHO_O2), 2)), row.names = FALSE)
  x <- tot[XN]
  cat("\nCanales de sustitución, no canónicas:\n")
  print(data.frame(canal = XN, exacta = round(100 * x, 2), exceso = round(100 * (x - TECHO_O2), 2), canonica = round(100 * CANON[XN])), row.names = FALSE)
  cat("\nVigiladas (no canónicas; exceso; canónica):\n")
  print(data.frame(regla = c("Q", EXTRA), exacta = round(100 * tot[c("Q", EXTRA)], 2), exceso = round(100 * (tot[c("Q", EXTRA)] - TECHO_O2), 2),
                   canonica = round(100 * CANON[c("Q", EXTRA)], 2)), row.names = FALSE)
  cat(sprintf("\nSolo la clave sobrevive: R30-90 %.1f %% | R20-120 %.1f %% | Q (quita el menos) %.2f %%\n",
              100 * tot["solo R30-90"], 100 * tot["solo R20-120"], 100 * tot["Q"]))
  rt <- EXR$M / EXR$P; sel <- c(names(b)[1L], "X0", "X1a", "X1b", "X1", "R30-90", "R20-120", "solo R20-120", "Q", "RE", "M0")
  cat("Por rama de error (% por rama; P de la rama entre paréntesis):\n")
  o <- round(100 * rt[, sel, drop = FALSE], 1); rownames(o) <- sprintf("%s (%.1f)", rownames(o), 100 * EXR$P / sum(EXR$P)); print(o)
  if (exists("r_all")) cat(sprintf("\n(Solo informativo: techo nulo de la muestra de la batería, %.1f %%; la clasificación usa %.1f %%.)\n",
                                   100 * r_all$techo_nulo, 100 * TECHO_O2))

  cat("\n--- Comparación con el modelo (opcion_c/c2.json y busq_c2.txt), a la centésima ---\n")
  cmp <- comparar_modelo(EXR, tot)
  print(cmp[cmp$ambito %in% c("total", "estados", "conjuntos", "abortos", "peso con razón nueva", "dos claves o duplicadas", "clave no da v", "total (busq_c2.txt)"), ], row.names = FALSE)
  n_dif <- sum(!cmp$ok)
  cat(sprintf("Filas comparadas: %d | diferencias: %d | máxima |dif| por rama: %.6f\n", nrow(cmp), n_dif, max(abs(cmp$dif[cmp$ambito %in% names(EXR$errs[[1L]]) | grepl("^EV-", cmp$ambito)]))))
  if (n_dif > 0L) { cat("DIFERENCIAS CON EL MODELO:\n"); print(cmp[!cmp$ok, ], row.names = FALSE); errores_total <- errores_total + n_dif }
  c3 <- comparar_h3(tot, CANON)
  cat(sprintf("\n--- Reglas de H3 de la r4 contra H3_ESP (tabla del reporte), a la centésima: %d cifras | diferencias: %d ---\n", nrow(c3), sum(!c3$ok)))
  print(c3, row.names = FALSE)
  if (any(!c3$ok)) { cat("DIFERENCIAS CON LA TABLA DE H3\n"); errores_total <- errores_total + sum(!c3$ok) }

  cat("\n--- O-2: cifra medida contra FIRMA_O2 (exceso en pp; FIRMADA =", FIRMADA, ") ---\n")
  cl <- clasificar_o2(tot, CANON); imprimir_o2(cl)
  O2_EVAL <- TRUE
  O2_LIST <- sum(cl$veredicto == "listada"); O2_PEND <- sum(cl$veredicto == "pendiente"); O2_NOREP <- sum(cl$veredicto == "no_reproduce")
  cat(sprintf("Filas de FIRMA_O2: %d | combinaciones con exceso >= 8: %d\n", nrow(FIRMA_O2), sum(cl$exceso >= 8)))
  for (i in which(cl$veredicto == "listada"))
    cat(sprintf("  ROJO, listada en O-2: %s, %s: exceso %+.2f pp\n", cl$regla[i], cl$ambito[i], cl$exceso[i]))
  for (i in which(cl$veredicto == "pendiente"))
    cat(sprintf("  PENDIENTE DE FIRMA: %s%s, %s: exceso %+.2f pp%s\n", cl$regla[i], if (cl$nueva[i]) " (fuera de FIRMA_O2)" else "", cl$ambito[i],
                cl$exceso[i], if (cl$nueva[i]) "" else sprintf(" por encima de lo listado (%+.2f)", cl$firmado[i])))
  for (i in which(cl$veredicto == "no_reproduce"))
    cat(sprintf("  NO REPRODUCE: %s, %s: exceso %+.2f pp contra %+.2f listado: se para y se lleva al profesor\n", cl$regla[i], cl$ambito[i], cl$exceso[i], cl$firmado[i]))
  m8 <- cl$exceso >= 8
  csv <- data.frame(regla = cl$regla[m8], tipo = cl$tipo[m8], ambito = cl$ambito[m8], exacta = round(cl$medida[m8], 4), exceso = cl$exceso[m8],
                    canonica = round(100 * as.numeric(CANON[cl$regla[m8]]), 2), stringsAsFactors = FALSE)
  write.csv(csv, file.path(aqui, "salida", "firma_o2_medida.csv"), row.names = FALSE)
  cat("CSV:", file.path(aqui, "salida", "firma_o2_medida.csv"), "(", nrow(csv), "filas )\n")
  MAX_EXACTA <- list(nocanon = b[1L], mezcla = bm[1L])
  cat(sprintf("Máximo de la batería exacta: no canónicas %.2f %% (%s, %+.2f pp) | mezcla %.2f %% (%s, %+.2f pp)\n",
              100 * b[[1L]], names(b)[1L], 100 * (b[[1L]] - TECHO_O2), 100 * bm[[1L]], names(bm)[1L], 100 * (bm[[1L]] - TECHO_O2)))

  # Mutantes de O-2 sobre una copia del resultado (no tocan `tot` ni `CANON`). Subir una fila no debe mover la otra del mismo
  # ámbito de la regla: se compensa con la canónica para que el otro ámbito quede igual.
  cat("\n--- Mutantes de O-2 sobre una copia (fila +0,02 pp = pendiente; regla nueva a +8,01 pp = pendiente; fila -0,02 pp = no_reproduce; solo esa fila cambia) ---\n")
  base_k <- clave_o2(cl)
  probar <- function(t2, c2, objetivo, esperado) {
    k2 <- clave_o2(clasificar_o2(t2, c2)); todas <- union(names(base_k), names(k2))
    b0 <- setNames(base_k[todas], todas); b1 <- setNames(k2[todas], todas)
    otras <- setdiff(todas, objetivo)
    cambio <- otras[!vapply(otras, function(k) identical(unname(b0[k]), unname(b1[k])), TRUE)]
    list(ok = !is.na(b1[objetivo]) && b1[[objetivo]] == esperado && !length(cambio), real = b1[[objetivo]], cambio = cambio)
  }
  mover <- function(r, a, d) {
    t2 <- tot; c2 <- CANON
    if (a == "nocanon") { t2[r] <- t2[r] + d; c2[r] <- c2[r] - d * (1 - P_CANON) / P_CANON } else c2[r] <- c2[r] + d / P_CANON
    list(t = t2, c = c2)
  }
  fijar <- function(r, a, exc_pp) {
    t2 <- tot; c2 <- CANON; obj <- TECHO_O2 + exc_pp / 100; mz <- (1 - P_CANON) * tot[[r]] + P_CANON * CANON[[r]]
    if (a == "nocanon") { t2[r] <- obj; c2[r] <- (mz - (1 - P_CANON) * obj) / P_CANON } else c2[r] <- (obj - (1 - P_CANON) * tot[[r]]) / P_CANON
    list(t = t2, c = c2)
  }
  n_mo <- 0L
  linea <- function(id, vm, r, a, esp) {
    ver_ <- if (vm$ok) "cazado_por_su_sonda" else "no_detectado"
    cat(sprintf("%-6s %-20s %-22s %-8s | esperada: %-9s | real: %s%s\n", id, ver_, r, a, esp, vm$real,
                if (length(vm$cambio)) paste0(" (cambiaron también: ", paste(vm$cambio, collapse = ", "), ")") else ""))
    if (!vm$ok) errores_total <<- errores_total + 1L
    n_mo <<- n_mo + 1L
  }
  for (i in seq_len(nrow(FIRMA_O2))) {
    r <- FIRMA_O2$regla[i]; a <- FIRMA_O2$ambito[i]
    m_ <- mover(r, a, 0.0002); linea(paste0("S", i), probar(m_$t, m_$c, paste(r, a), "pendiente"), r, a, "pendiente")
    m_ <- mover(r, a, -0.0002); linea(paste0("B", i), probar(m_$t, m_$c, paste(r, a), "no_reproduce"), r, a, "no_reproduce")
  }
  for (r in c("FMT-empieza-km", "Q")) for (a in c("nocanon", "mezcla")) {
    stopifnot(!(paste(r, a) %in% names(base_k)))
    m_ <- fijar(r, a, 8.01); linea("N", probar(m_$t, m_$c, paste(r, a), "pendiente"), r, a, "pendiente")
  }
  cat("Mutantes de O-2 en copia:", n_mo, "\n")

  # Mutantes del verificador: amarre de ramas, error dentro de una regla, ancla de cand y exit.
  cat("\n--- Mutantes del verificador (amarre del generador, error de regla, ancla de cand, exit) ---\n")
  vm_ok <- function(id, desc, ok) {
    cat(sprintf("%-4s %-22s | %s\n", id, if (ok) "cazado_por_su_sonda" else "no_detectado", desc))
    if (!ok) errores_total <<- errores_total + 1L
  }
  vm_ok("V1", "copia con «dist = \"EV-D-SUMA\",» rompe el amarre de ERRS (y el original lo cumple)",
        errs_amarre(COD) && !errs_amarre(sust1(COD, 'dist = pick(c("EV-D-SUMA", "EV-D-FINAL")),', 'dist = "EV-D-SUMA",')))
  rg_m <- c(list(L("BOOM", "magnitud", function(o) stop("boom"))), reglas[-1L])
  mm_ <- metricas(oc[c(kc, setdiff(1:4, kc))], ee_c, rg = rg_m)
  vm_ok("V2", "una regla que lanza stop() cuenta como error (no puntúa 25 % en silencio)", "BOOM" %in% attr(mm_, "err"))
  esp_ <- correr(EX_NOCANON, 1L)$espacio; f_ <- esp_[1, , drop = FALSE]; m0_ <- MULT[f_$dur - 1L]
  cand_ <- seq(m0_, 2L * f_$v, by = m0_); cand_ <- cand_[cand_ != f_$v & cand_ != f_$D]
  assign(".ST", list(fila = f_, d1 = cand_[1L], cand = cand_, rol = "dist", err = "EV-D-FINAL", rep = "dist"), envir = globalenv())
  r_ <- tryCatch(correr(preparar(sust1(COD, 'mult <- c(10L, 15L, 20L, 10L)[fila$dur - 1L]', 'mult <- 5L')), 1L), error = function(e) e)
  vm_ok("V3", "un cand del generador distinto del del verificador aborta el estado (ancla de d1)", inherits(r_, "error") && grepl("cand", conditionMessage(r_)))
  vm_ok("V4", "decidir_exit: «exacto» limpio y firmado no da 4; «todo» firmado da 4; sin firma da 3; con errores da 1",
        decidir_exit(0L, TRUE, 0L, TRUE, "exacto", TRUE) == 3L && decidir_exit(0L, TRUE, 0L, TRUE, "todo", TRUE) == 4L &&
        decidir_exit(0L, TRUE, 0L, FALSE, "todo", TRUE) == 3L && decidir_exit(1L, TRUE, 0L, TRUE, "todo", TRUE) == 1L)
  # V5 y V6 (H1 de la r4): una copia del .Rmd con un byte cambiado no coincide con RMD_MD5 y el exit es 3 aunque todo cuadre, con FIRMADA en
  # FALSE (V5) y en TRUE (V6). El original sí coincide.
  tmp_rmd <- tempfile(fileext = ".Rmd"); b_ <- readBin(RMD, "raw", file.size(RMD))
  b_[length(b_)] <- as.raw(bitwXor(as.integer(b_[length(b_)]), 1L)); writeBin(b_, tmp_rmd)
  md5_copia <- md5_coincide(tmp_rmd); unlink(tmp_rmd)
  vm_ok("V5", "copia del .Rmd con un byte cambiado, FIRMADA FALSE: md5 no coincide y el exit es 3 (el original coincide)",
        md5_coincide(RMD) && !md5_copia && decidir_exit(0L, TRUE, 0L, FALSE, "todo", md5_copia) == 3L)
  vm_ok("V6", "copia del .Rmd con un byte cambiado, FIRMADA TRUE: md5 no coincide y el exit es 3, no 4",
        md5_coincide(RMD) && !md5_copia && decidir_exit(0L, TRUE, 0L, TRUE, "todo", md5_copia) == 3L)
}

# ---- 3. MUTANTES (Incidente P) --------------------------------------------------------
if (modo %in% c("mutantes", "rapido", "todo")) {
  cat("\n=== 3. MUTANTES ===\n")
  mutar <- function(pat, nuevo) {
    k <- lengths(regmatches(COD, gregexpr(pat, COD, fixed = TRUE)))
    if (k != 1L) stop("MUTANTE MAL CONSTRUIDO: el patrón aparece ", k, " veces: ", pat)
    sub(pat, nuevo, COD, fixed = TRUE)
  }
  # sonda: texto del stopifnot del .Rmd o código del oráculo. Va en orden: primero la sonda L-6 sobre el fuente, luego
  # las guardas internas, porque un mutante que no aborta el chunk se le pasa al oráculo.
  correr_mut <- function(m, tope = 600L) {
    cod <- m$cod
    if (l6_fuente(cod)) return(list(sem = 0L, msg = "L-6: comilla invertida en un chunk"))
    if (isTRUE(m$forzar_no_canon)) cod <- fijar_canon(cod, "FALSE")
    if (isTRUE(m$forzar_canon))    cod <- fijar_canon(cod, "TRUE")
    ex <- compilar(cod)
    for (s in seq_len(if (is.null(m$tope)) tope else m$tope)) {
      r <- tryCatch({ e <- correr(ex, s); o <- if (isTRUE(m$solo_guarda)) list(problema = NA_character_) else oraculo(e)
                      if (is.na(o$problema)) NA_character_ else o$problema },
                    error = function(err) conditionMessage(err))
      if (!is.na(r)) return(list(sem = s, msg = r))
    }
    list(sem = NA, msg = NA_character_)
  }
  # Guarda del .Rmd que un mutante debe apagar para que la sonda probada sea la que muera (como M7).
  sin_guarda_afirma <- function(cod) sust1(cod, '"ninguna razón afirma lo que rechaza" = all(vapply(ops, function(o) o$afirma != rechaza[[o$rol]], TRUE)),',
                                           '"ninguna razón afirma lo que rechaza" = TRUE,')
  # L-8 completa (resultado y operandos) tal como la escribe el .Rmd; sin ella, solo el oráculo de texto puede ver el negativo.
  GUARDA_L8 <- '!endsWith(error_cometido, "SIGNO") && min(Dm, Tm, Vm) > 0 &&\n            min(paso_dist$a, paso_dist$b, paso_tiem$a, paso_tiem$b) >= 0L)'
  sin_L8 <- function(cod) sust1(cod, GUARDA_L8, 'TRUE)')
  PASO_T_FINAL <- '"EV-T-FINAL" = list(a = t2, b = 0L, op = "-", c = t2),'
  M17D <- mutar('"EV-D-FINAL" = list(a = d2, b = 0L, op = "-", c = d2),', '"EV-D-FINAL" = list(a = d2 - 20L, b = -20L, op = "-", c = d2),')
  muts <- list(
    list(id = "M1", sonda = "ORACULO_CLAVE", cod = mutar('dist = opcion("dist", "CLAVE", D, km(D)),', 'dist = opcion("dist", "CLAVE", D, km(D + 10L)),'), forzar_no_canon = TRUE,
         desc = "La clave de distancia afirma un valor falso; `valor` sigue correcto, así que solo el oráculo de texto puede verla"),
    list(id = "M2", sonda = "L-1: ningún paso tiene un error de aritmética", cod = mutar('"EV-T-FINAL" = list(a = t2, b = 0L, op = "-", c = t2),', '"EV-T-FINAL" = list(a = t2, b = 0L, op = "-", c = t2 - 1L),'), forzar_no_canon = TRUE,
         desc = "La cuenta del paso de tiempo da mal (la entrada SIGNO ya no existe: se usa la de EV-T-FINAL)"),
    list(id = "M3b", sonda = "L-4: ninguna razón FINAL contra el paso que ya está mal", cod = mutar('function(p) p$rol_clave != "dist", function(p) d2,', 'function(p) p$rol_clave != "dist" || p$error_cometido == "EV-D-SUMA", function(p) d2,'), forzar_no_canon = TRUE,
         desc = "EV-D-FINAL solo contra EV-D-SUMA, sin apagar ninguna otra guarda: L-4 es la única defensa en ese estado (H9 de la r3)"),
    list(id = "M4", sonda = "L-3: opciones verbatim", cod = mutar('M(resta(km, d1, d2, -d1))', 'M(resta(km, d1, d2, -D))'), forzar_canon = TRUE,
         desc = "La canónica «corrige» el −20 impreso de la última opción (regla #24 H-2)"),
    list(id = "M5", sonda = "L-3: Paso 3 verbatim", cod = mutar('expr_coc  <- if (error_cometido == "EV-C-UNIDAD") {', 'expr_coc  <- if (FALSE) {'), forzar_canon = TRUE,
         desc = "La canónica normaliza la trampa del Paso 3"),
    list(id = "M6", sonda = "a lo sumo una razón afirma un valor negativo", cod = mutar('candidatos <- Filter(function(s) sum(grepl("SIGNO$", s)) <= 1L, candidatos)', 'candidatos <- Filter(function(s) sum(grepl("SIGNO$", s)) <= 2L, candidatos)'), forzar_no_canon = TRUE,
         desc = "Se quita el tope de razones de signo"),
    list(id = "M7", sonda = "regla #13: exactamente una razón es verdadera",
         cod = sin_guarda_afirma(sub('aplicables <- Filter(function(e) !repara(e), aplicables)', 'aplicables <- aplicables', mutar('function(p) TRUE, function(p) d1),', 'function(p) TRUE, function(p) D),'), fixed = TRUE)), forzar_no_canon = TRUE, solo_guarda = TRUE,
         desc = "La razón EV-D-INICIAL afirma el valor correcto: segunda clave. Quita también el filtro repara(), que la bloquearía antes de esta guarda, y «ninguna razón afirma lo que rechaza», que con un paso 1 correcto la mata antes (como M3), y mide solo las guardas del chunk: sin eso, ORACULO_REPARA lo caza primero"),
    list(id = "M8", sonda = "molde común", cod = mutar('paste0(" y no $", rechaza[[rol]], "$"))', 'if (codigo != "CLAVE") paste0(" y no $", rechaza[[rol]], "$"))'), forzar_no_canon = TRUE,
         desc = "La clave lleva un molde distinto del de las demás"),
    list(id = "M9", sonda = "L-1: exactamente un paso está mal", cod = mutar('list(a = t2, b = t1, op = "-", c = dur))', 'list(a = t2 + 1L, b = t1, op = "-", c = dur + 1L))'), forzar_no_canon = TRUE,
         desc = "Dos pasos mal a la vez: el de tiempo queda mal además del sorteado"),
    list(id = "M11", sonda = "MAG-mayor-afirmado", cod = mutar('meta <- RANGO_META[[rol_clave]]', 'meta <- if (rol_clave == "tiem") RANGO_META$tiem else c(1, 0, 0, 0)'), forzar_no_canon = TRUE, bateria = TRUE,
         desc = "Se fuerza la clave al rango 1: la batería debe verlo en su regla de magnitud"),
    list(id = "M12", sonda = "REL-paso-unico", cod = mutar('prob = ifelse(ROLES == rol_clave, 1 / 2, 1 / 4)', 'prob = ifelse(ROLES == rol_clave, 0, 1 / 2)'), forzar_no_canon = TRUE, bateria = TRUE,
         desc = "El paso de la clave nunca se repite, como en el impreso: la batería debe verlo en su regla relacional"),
    list(id = "M13", sonda = "L-2: rapidez entera y datos con sentido", cod = mutar('mult <- c(10L, 15L, 20L, 10L)[fila$dur - 1L]', 'mult <- 5L'), forzar_no_canon = TRUE,
         desc = "d1 deja de ser múltiplo de dur (H7 de la FASE 2C r1)"),
    list(id = "M14", sonda = "L-7: la primera hora va antes de la primera distancia", cod = mutar('" donde inició, y a las ", t2s, " se encontraba a "', '" donde inició a las ", t1s, ", y a las ", t2s, " se encontraba a "'), forzar_no_canon = TRUE,
         desc = "La primera hora vuelve a pegarse a «inició» (H1)"),
    list(id = "M15", sonda = "ORACULO_REPARA", cod = mutar('aplicables <- Filter(function(e) !repara(e), aplicables)', 'aplicables <- aplicables'), forzar_no_canon = TRUE,
         desc = "Se ofrece una razón que repara el procedimiento (H2 y H6). Sin signos que se compensen solo queda d2 = v t1 o v t2 (d1 = 2 v, la salida 2 h antes): 11 de 2 000 semillas, 0,55 %. Tope por defecto (600) basta"),
    list(id = "M16", sonda = "regla #7: contracciones al y del", cod = mutar('" es lo que separa ", veh$al, " ", lugar$del, " a las "', '" es lo que separa a ", veh$el, " de ", lugar$el, " a las "'), forzar_no_canon = TRUE,
         desc = "Vuelve «a el auto de el pueblo» (H3)"),
    list(id = "M10", sonda = "ORACULO_RECHAZO", cod = mutar('rechaza <- c(dist = km(Dm), tiem = hs(Tm),', 'rechaza <- c(dist = km(D + 5L), tiem = hs(Tm),'), forzar_no_canon = TRUE,
         desc = "La forma Y rechaza una cifra que el procedimiento no muestra"),
    # ---- r9: variante D (L-8), H1-H4 de la FASE 2C ----
    list(id = "M17a1", sonda = "L-8: el procedimiento nunca muestra un negativo", cod = mutar('tiem = "EV-T-FINAL",', 'tiem = pick(c("EV-T-FINAL", "EV-T-SIGNO")),'), forzar_no_canon = TRUE,
         desc = "L-8, primera componente, solo el sorteo: EV-T-SIGNO vuelve a sortearse como error cometido, sin entrada en el switch"),
    list(id = "M17a2", sonda = "L-8: el procedimiento nunca muestra un negativo", cod = sust1(mutar('tiem = "EV-T-FINAL",', 'tiem = pick(c("EV-T-FINAL", "EV-T-SIGNO")),'), PASO_T_FINAL, paste0(PASO_T_FINAL, '\n  "EV-T-SIGNO" = list(a = t1, b = t2, op = "-", c = -dur),')), forzar_no_canon = TRUE,
         desc = "L-8, primera componente completa: el sorteo y la entrada EV-T-SIGNO en paso_tiem (H9 de la r3)"),
    list(id = "M17b", sonda = "L-8: el procedimiento nunca muestra un negativo", cod = mutar('"EV-T-FINAL" = list(a = t2, b = 0L, op = "-", c = t2),', '"EV-T-FINAL" = list(a = t1, b = t2, op = "-", c = -dur),'), forzar_no_canon = TRUE,
         desc = "L-8, otra componente (min(Dm, Tm, Vm) > 0): el paso de tiempo muestra un negativo sin que el error cometido se llame SIGNO"),
    list(id = "M17c", sonda = "ORACULO_L8", cod = sin_L8(mutar(PASO_T_FINAL, '"EV-T-FINAL" = list(a = t1, b = t2, op = "-", c = -dur),')), forzar_no_canon = TRUE,
         desc = "M17b sin la guarda L-8: solo el oráculo de texto puede ver el negativo"),
    list(id = "M17d", sonda = "L-8: el procedimiento nunca muestra un negativo", cod = M17D, forzar_no_canon = TRUE,
         desc = "L-8, operando negativo con resultado positivo: «d2 - 20 km - (-20 km) = d2 km» (H6 de la r3)"),
    list(id = "M17e", sonda = "ORACULO_L8", cod = sin_L8(M17D), forzar_no_canon = TRUE,
         desc = "M17d sin la guarda: el oráculo debe ver el operando negativo (H6 de la r3)"),
    list(id = "M18", sonda = "regla #7: sin doble punto", cod = mutar('t1s, " El tiempo transcurrido', 't1s, ". El tiempo transcurrido'), forzar_no_canon = TRUE,
         desc = "Vuelve «a. m.. El tiempo» (H1 de la FASE 2C r2)"),
    list(id = "M19", sonda = "ORACULO_SOLUTION", cod = mutar('" en lugar de ", M(kmh(v), "."))', '" en lugar de ", M(kmh(v + 5L), "."))'), forzar_no_canon = TRUE,
         desc = "G (H2): la consecuencia de la Solution cita una rapidez falsa"),
    list(id = "M20", sonda = "ORACULO_SOLUTION", cod = mutar('paste0("Correcta. El paso ", N_PASO[[o$rol]], " tiene el error y esta razón lo corrige.")', 'paste0("Correcta. El paso ", N_PASO[[1L + (N_PASO[[o$rol]] %% 3L)]], " tiene el error y esta razón lo corrige.")'), forzar_no_canon = TRUE,
         desc = "H (H2): el veredicto de la opción correcta nombra otro paso"),
    list(id = "M21", sonda = "ninguna razón afirma lo que rechaza", cod = mutar('function(p) TRUE, function(p) t1),', 'function(p) TRUE, function(p) p$Tm),'), forzar_no_canon = TRUE,
         desc = "A' (H3): EV-T-INICIAL afirma lo que rechaza el paso (equivalente del mutante A, cuya precondición ya no existe)"),
    list(id = "M21b", sonda = "ninguna razón afirma lo que rechaza", cod = mutar('function(p) p$error_cometido != "EV-D-SUMA", function(p) d1 + d2,', 'function(p) TRUE, function(p) d1 + d2,'), forzar_no_canon = TRUE,
         desc = "EV-D-SUMA sin su precondición: se ofrece con EV-D-SUMA cometido y afirma lo que rechaza el paso 1 (H9 de la r3)"),
    list(id = "M22", sonda = "ninguna razón afirma lo que rechaza", cod = mutar('function(p) p$rol_clave != "coc", function(p) c(p$Dm, p$Tm),', 'function(p) TRUE, function(p) c(p$Dm, p$Tm),'), forzar_no_canon = TRUE,
         desc = "B (H3): EV-C-UNIDAD se ofrece contra un paso 3 que ya la comete: la misma fracción a los dos lados de «y no»"),
    list(id = "M23", sonda = "L-6: comilla invertida", cod = mutar('library(exams)', paste0('library(exams)  # ', intToUtf8(96L), 'x', intToUtf8(96L))), forzar_no_canon = TRUE,
         desc = "H4: una comilla invertida en un comentario del chunk (la FASE 2D lo corta ahí)"),
    list(id = "M24", sonda = "L-6: signo fuera de la fórmula", cod = mutar('tramo_d <- M(resta(km, d2, d1, D), ".")', 'tramo_d <- paste0(M(resta(km, d2, d1, D)), ".")'), forzar_no_canon = TRUE,
         desc = "H4: el punto final queda fuera de la fórmula de tramo_d"),
    # ---- r11: variante C (O-2) y H7 de la FASE 2C r3 ----
    list(id = "M25", sonda = "L-9", cod = mutar('if (length(con_nueva) >= 1L) candidatos <- con_nueva', ''), forzar_no_canon = TRUE,
         desc = "Se quita la obligación de L-9: la guarda de data_generation debe verlo"),
    list(id = "M25b", sonda = "ORACULO_L9", cod = mutar('  clave <- switch(rol_clave,', paste0('  elegidos[elegidos == "EV-T-CUENTA" & !("EV-T-SUMA" %in% elegidos) & sample.int(2L, 1L) == 1L] <- "EV-T-SUMA"\n',
                                                                                            '  clave <- switch(rol_clave,')), forzar_no_canon = TRUE,
         desc = "H6 de la r4 (mutante C): tras la guarda de L-9, con P = 1/2 EV-T-CUENTA pasa a EV-T-SUMA en elegidos; solo el oráculo, que mira e$ops, lo ve"),
    list(id = "M26", sonda = "ORACULO_SOLUTION", cod = mutar('prom <- (d1 + d2) %/% 2L', 'prom <- (d1 + d2) %/% 2L + 5L'), forzar_no_canon = TRUE,
         desc = "H7: la cifra del promedio de la Solution (y de la opción) se corre 5 km"),
    list(id = "M27", sonda = "ORACULO_SOLUTION", cod = mutar('M(paste0(dur + 1L))', 'M(paste0(dur + 2L))'), forzar_no_canon = TRUE,
         desc = "H7: la Solution cuenta una marca de más en EV-T-CUENTA"),
    list(id = "M28", sonda = "ORACULO_SOLUTION", cod = mutar('paste0(M(km(d1)), " es lo que ", veh$el,', 'paste0(M(km(d2)), " es lo que ", veh$el,'), forzar_no_canon = TRUE,
         desc = "H7: la cifra de EV-D-INICIAL es d2 en vez de d1"),
    list(id = "M29", sonda = "ORACULO_SOLUTION", cod = mutar('" El tiempo transcurrido entre las dos lecturas es ",\n      tramo_t)),', '" El tiempo transcurrido entre las dos lecturas es ",\n      tramo_d)),'), forzar_no_canon = TRUE,
         desc = "H7: el análisis de EV-T-FINAL cita tramo_d (llama tiempo a una distancia)")
  )
  res <- list()
  for (m in muts) {
    if (isTRUE(m$bateria)) {
      # Mutante de canal: no aborta nada; muere si su regla pasa de +8 pp sobre el techo nulo.
      if (!exists("evaluar_bateria")) { cat(m$id, "requiere correr también la batería (modo todo)\n"); next }
      mm <- muestrear(compilar(fijar_canon(m$cod, "FALSE")), cuota = 100L)
      rb <- evaluar_bateria(reglas, lapply(mm, `[[`, "ops"), vapply(mm, `[[`, 0L, "key"), estimulos = lapply(mm, `[[`, "est"))
      tasa <- rb$detalle$tasa[rb$detalle$regla == m$sonda]
      ver_ <- if (tasa - rb$techo_nulo >= .08) "cazado_por_su_sonda" else "no_detectado"
      cat(sprintf("%-4s %-22s | esperada: %s (+8 pp) | real: tasa %.1f %%, exceso %+.1f pp\n     %s\n", m$id, ver_, m$sonda, 100 * tasa, 100 * (tasa - rb$techo_nulo), m$desc))
      res[[m$id]] <- ver_; next
    }
    r <- correr_mut(m)
    ver_ <- if (is.na(r$msg)) "no_detectado" else if (grepl(m$sonda, r$msg, fixed = TRUE)) "cazado_por_su_sonda" else "cazado_por_otra"
    cat(sprintf("%-4s %-22s semilla %-4s | esperada: %s\n     real: %s\n     %s\n", m$id, ver_, r$sem, m$sonda, substr(r$msg, 1, 110), m$desc))
    res[[m$id]] <- ver_
  }
  fallan <- sum(vapply(res, function(x) x != "cazado_por_su_sonda", TRUE))
  cat("Mutantes:", length(res), "| cazados por su sonda:", length(res) - fallan, "| fallan:", fallan, "\n")
  errores_total <- errores_total + fallan
}

# ---- 3b. MUTANTES DE O-2 sobre el generador (re-enumeran de verdad) -------------------------------------------------
# H4 de la FASE 2C r4: cada mutante tiene una regla causal y una cifra predicha por el modelo Python ANTES de correr el R (opcion_c/c1.json y
# base.json, que no salen de este script). Cambia el .Rmd en memoria, vuelve a enumerar el espacio entero y exige DOS cosas: que reproduzca su
# JSON a la centésima en toda fila de «tot», más n, ns y el peso con razón nueva, y que su regla causal deje de estar «listada» (pendiente o
# no_reproduce). Si no reproduce el JSON, es no_detectado: se para y se busca la causa, no se ajusta nada.
if (modo %in% c("mutantes_o2", "todo")) {
  cat("\n=== 3b. MUTANTES DE O-2 (generador mutado, enumeración exacta; cifra predicha por el modelo Python) ===\n")
  mut_o2 <- list(
    list(id = "O2-A", json = "c1.json", causal = "EST-sin-rotulos",
         cod = sust1(sust1(COD, 'if (length(con_nueva) >= 1L) candidatos <- con_nueva', ''),
                     'length(con_nueva) == 0L || any(elegidos %in% c("EV-D-PROM", "EV-T-CUENTA")))', 'TRUE)'),
         desc = "sin la obligación de L-9 (se quita la línea de con_nueva y se apaga su guarda): debe dar opcion_c/c1.json, EST sin rótulos +16,39"),
    list(id = "O2-B", json = "base.json", causal = "E3",
         cod = sust1(sust1(COD, 'function(p) (d1 + d2) %% 2L == 0L && !(prom %in% c(p$D, p$Dm, d1, d2)), function(p) prom),', 'function(p) FALSE, function(p) prom),'),
                     'function(p) !((p$dur + 1L) %in% c(t1, t2, p$Tm, p$dur)), function(p) p$dur + 1L),', 'function(p) FALSE, function(p) p$dur + 1L),'),
         desc = "sin las dos razones nuevas (precondiciones de EV-D-PROM y EV-T-CUENTA en FALSE): debe dar opcion_c/base.json, E3 al 100 %")
  )
  n_o2 <- 0L; fallan_o2 <- 0L
  for (m in mut_o2) {
    t0 <- Sys.time(); r <- enumerar(m$cod, m$id)
    totm <- colSums(r$M) / sum(r$P); clm <- clasificar_o2(totm, CANON)
    cmpj <- comparar_json(r, totm, m$json)
    sale <- unique(clm$regla[clm$veredicto != "listada"])
    repro <- all(cmpj$ok); causal <- m$causal %in% sale
    ver_ <- if (repro && causal) "cazado_por_su_sonda" else "no_detectado"
    cat(sprintf("%-5s %-22s | estados %d, abortos %d (peso que sobrevive %.4f), conjuntos %d, errores de regla %d, %.1f min\n",
                m$id, ver_, r$n, r$ab, sum(r$P), r$ns, length(r$ERN), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    cat(sprintf("      reproduce %s: %s | filas %d, diferencias %d, máx |dif| %.6f\n", m$json, if (repro) "SÍ" else "NO", nrow(cmpj), sum(!cmpj$ok), max(abs(cmpj$dif))))
    if (!repro) print(cmpj[!cmpj$ok, ], row.names = FALSE)
    cc <- clm[clm$regla == m$causal, c("regla", "ambito", "medida", "exceso", "firmado", "veredicto")]; cc$medida <- round(cc$medida, 2)
    cat(sprintf("      regla causal %s fuera de «listada»: %s\n", m$causal, if (causal) "SÍ" else "NO")); print(cc, row.names = FALSE)
    cat(sprintf("      dejan de estar listadas (%d): %s\n      %s\n", length(sale), paste(sale, collapse = ", "), m$desc))
    if (ver_ == "no_detectado") fallan_o2 <- fallan_o2 + 1L
    n_o2 <- n_o2 + 1L
  }
  cat("Mutantes de O-2 sobre el generador:", n_o2, "| fallan:", fallan_o2, "\n")
  errores_total <- errores_total + fallan_o2
}

# ---- RESULTADO -----------------------------------------------------------------------------------------------------
# Regla #22 §P7-E: la batería queda en rojo y el override no la vuelve PASS. Con FIRMADA FALSE (la firma del profesor llega en el
# paso 11) el exit es 3 aunque todo cuadre: «PENDIENTE DE FIRMA (O-2)». El 4 (rojo listado) solo en «todo» y solo firmada; nunca 0.
max_txt <- if (is.null(MAX_EXACTA)) "" else sprintf(" | máximo de la batería exacta: no canónicas %.2f %% (%s), mezcla %.2f %% (%s)",
  100 * MAX_EXACTA$nocanon, names(MAX_EXACTA$nocanon), 100 * MAX_EXACTA$mezcla, names(MAX_EXACTA$mezcla))
estado <- if (errores_total > 0L) { "CON ERRORES"
} else if (!O2_EVAL) { "ALCANCE O-2 SIN EVALUAR (corre «exacto» o «todo»)"
} else if (!MD5_OK) { "PENDIENTE: EL .Rmd NO ES EL FIRMADO (md5 distinto de RMD_MD5)"
} else if (O2_PEND > 0L || O2_NOREP > 0L || !isTRUE(FIRMADA)) { "PENDIENTE DE FIRMA (O-2)"
} else if (modo != "todo") { paste0("ROJO listado por O-2, pero el modo «", modo, "» no corre todas las comprobaciones")
} else "ROJO, listado por O-2"
cat("\nRESULTADO:", estado, "(", errores_total, ")", "| md5 del .Rmd:", if (MD5_OK) "coincide con RMD_MD5" else "DIFIERE de RMD_MD5", if (O2_EVAL) sprintf("| O-2: %d listadas, %d pendientes, %d no reproducen | FIRMADA = %s", O2_LIST, O2_PEND, O2_NOREP, FIRMADA), max_txt,
    if (exists("r_all")) paste0("| batería de muestra: ", r_all$veredicto, " (solo informativa)"), "\n")
quit(status = decidir_exit(errores_total, O2_EVAL, O2_PEND + O2_NOREP, FIRMADA, modo, MD5_OK))
