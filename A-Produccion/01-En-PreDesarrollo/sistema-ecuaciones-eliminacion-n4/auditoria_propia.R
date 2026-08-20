#!/usr/bin/env Rscript
# =============================================================================
# auditoria_propia.R — verificador propio de sistema-ecuaciones-eliminacion-n4
#
# Cubre lo que NINGUN validador del arsenal cubre para este item:
#   (A) bateria de eliminacion §P7 con CIERRE POR LAS 6 FAMILIAS
#   (B) prueba de aceptacion LEXICA (Error 32), agregada Y POR RAMA
#   (C) verificacion semantica ejecutable de las 4 opciones en cada version
#   (D) estratos >= 20 (regla #23) y H1 en AMBAS direcciones
# Muestra estandar N = 100 (regla #23).
# =============================================================================
suppressPackageStartupMessages({ library(stats) })

RAIZ <- "/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
HELP <- file.path(RAIZ, ".claude/scripts/bateria_eliminacion.R")
if (!file.exists(HELP))
  stop("FALTA el helper canonico ", HELP,
       " — los cortes deben tener UNA sola fuente; abortando (regla #22 §P7).")
source(HELP)

RMD <- "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_schoice_v1.Rmd"
N   <- 100L   # muestra ESTANDAR de la regla #23; NO elegible por sesion
errores <- character(0)
add <- function(...) errores <<- c(errores, paste0(...))

# ---- 0. Recolectar N versiones -------------------------------------------
lin  <- readLines(RMD, warn = FALSE)
ini  <- grep("^```\\{r data_generation", lin)[1] + 1
fin  <- ini - 1 + which(grepl("^```\\s*$", lin[ini:length(lin)]))[1] - 1
CODE <- lin[ini:fin]

BASE <- as.integer(Sys.getenv("AUD_BASE", "1000")); STEP <- as.integer(Sys.getenv("AUD_STEP", "7"))
recolectar <- function(n = N, base = BASE) {
  out <- vector("list", n)
  for (i in seq_len(n)) {
    set.seed(base + STEP * i)
    env <- new.env()
    ok <- tryCatch({ eval(parse(text = CODE), envir = env); TRUE }, error = function(e) {
      add("RENDER: semilla ", i, " fallo -> ", conditionMessage(e)); FALSE })
    if (!ok) next
    out[[i]] <- list(ops = env$opciones, clave = which(env$sol == 1L),
                     paso = env$paso_real, cod = env$err_real$codigo,
                     canon = env$is_canonical, R = env$par$R,
                     vprop = env$valores_prop, Rr = env$par$R)
  }
  Filter(Negate(is.null), out)
}
V <- recolectar()
cat("=== versiones evaluadas:", length(V), "/", N, "===\n\n")
if (length(V) < N) add("RENDER: solo ", length(V), " de ", N, " versiones evaluables")

opciones <- lapply(V, function(v) v$ops)
claves   <- vapply(V, function(v) v$clave, integer(1))
pasos    <- vapply(V, function(v) v$paso, integer(1))

# ---- (C) verificacion semantica ejecutable --------------------------------
mal <- 0L
for (v in V) {
  vp <- v$vprop
  if (!isTRUE(all.equal(as.numeric(vp[1]), as.numeric(v$Rr)))) mal <- mal + 1L
  d <- vp[-1]
  if (any(!is.na(d) & d == v$Rr)) mal <- mal + 1L
}
cat("(C) verificacion semantica ejecutable: ", if (mal == 0L)
      "OK — la clave conduce al valor correcto y ningun senuelo lo hace\n" else
      paste0("FALLO en ", mal, " version(es)\n"))
if (mal > 0L) add("SEMANTICA: ", mal, " versiones con segunda clave o clave que no conduce a R")

# ---- (D) estratos y H1 bidireccional --------------------------------------
tab <- table(pasos)
cat("\n(D) estratos por paso:", paste(names(tab), unname(tab), sep = "=", collapse = "  "), "\n")
if (any(tab < 20L)) add("ESTRATO CORTO (regla #23): ", paste(names(tab)[tab < 20L], collapse = ","),
                        " con n < 20 -> ese estrato es NO CONCLUYENTE")
## OBJ 6: con tipo de clave invariante, table() tiene UNA celda y `any(tab < 20)` es FALSE:
## el caso degenerado se escapaba sin aviso. H3b no puede verlo (su firma conserva los
## simbolos de contexto: 37 firmas, max 7 %), asi que la sonda tiene que estar AQUI.
if (length(tab) < 3L)
  add("TIPO DE CLAVE INVARIANTE (§P4-bis): solo ", length(tab),
      " de 3 pasos aparecen como error real; H3b NO puede detectarlo en este item")
h1 <- t(vapply(V, function(v) {
  L <- nchar(v$ops); k <- v$clave
  c(max = as.numeric(L[k] == max(L) && sum(L == max(L)) == 1L),
    min = as.numeric(L[k] == min(L) && sum(L == min(L)) == 1L))
}, numeric(2)))
cat("    H1 clave = unica mas larga:", sprintf("%.1f %%", 100 * mean(h1[, "max"])),
    "| unica mas corta:", sprintf("%.1f %%", 100 * mean(h1[, "min"])),
    "  (azar 25 %; se vigilan AMBAS direcciones)\n")
if (mean(h1[, "max"]) > 0.60) add("H1: la clave es la mas larga en > 60 % de las versiones")
if (mean(h1[, "min"]) > 0.60) add("H1 INVERSA: la clave es la mas corta en > 60 %")
## OBJ 7 / Error 30: el agregado DILUYE. Se mide dentro de cada rama.
for (pp in sort(unique(pasos))) {
  ii <- which(pasos == pp)
  if (length(ii) < 20L) next
  mx <- mean(h1[ii, "max"]); mn <- mean(h1[ii, "min"])
  cat(sprintf("      rama %d: mas larga %.1f %% | mas corta %.1f %%\n", pp, 100 * mx, 100 * mn))
  if (mx > 0.45) add("H1 rama ", pp, ": clave = mas larga en ", sprintf("%.1f %%", 100 * mx))
  if (mn > 0.45) add("H1 rama ", pp, ": clave = mas corta en ", sprintf("%.1f %%", 100 * mn))
}

# ---- (A) BATERIA §P7: seis familias --------------------------------------
nums <- function(s) {
  m <- regmatches(s, gregexpr("-?[0-9][0-9.]*", s))[[1]]
  m <- gsub("\\.", "", m); suppressWarnings(as.numeric(m))
}
## control positivo PEGADO a la definicion: sin el "-?" esto devolvia 3500 y las cinco
## reglas de magnitud quedaban ciegas al signo (detractor 2026-08-19, objecion 5a).
stopifnot(identical(tail(nums("de donde R = -3.500"), 1), -3500))
ult  <- function(s) { z <- nums(s); if (!length(z)) NA_real_ else z[length(z)] }
maxn <- function(s) { z <- nums(s); if (!length(z)) NA_real_ else max(z) }
solo1 <- function(idx) if (length(idx) == 1L) idx else NA_integer_
tiene <- function(ops, pat) grepl(pat, ops, ignore.case = TRUE)
unico_si <- function(v) solo1(which(v))

B <- list(
  # --- magnitud -----------------------------------------------------------
  nueva_regla("la mas larga",              "magnitud", function(o) unico_si(nchar(o) == max(nchar(o)))),
  nueva_regla("la mas corta",              "magnitud", function(o) unico_si(nchar(o) == min(nchar(o)))),
  nueva_regla("mayor cifra final",         "magnitud", function(o) { z <- vapply(o, ult, numeric(1)); unico_si(!is.na(z) & z == max(z, na.rm = TRUE)) }),
  nueva_regla("menor cifra final",         "magnitud", function(o) { z <- vapply(o, ult, numeric(1)); unico_si(!is.na(z) & z == min(z, na.rm = TRUE)) }),
  nueva_regla("mayor numero cualquiera",   "magnitud", function(o) { z <- vapply(o, maxn, numeric(1)); unico_si(!is.na(z) & z == max(z, na.rm = TRUE)) }),
  nueva_regla("mas palabras",              "magnitud", function(o) { w <- lengths(strsplit(o, "\\s+")); unico_si(w == max(w)) }),
  # --- divisibilidad ------------------------------------------------------
  nueva_regla("cifra final multiplo de 100000", "divisibilidad", function(o) { z <- vapply(o, ult, numeric(1)); unico_si(!is.na(z) & z %% 100000 == 0) }),
  nueva_regla("cifra final NO multiplo de 1000","divisibilidad", function(o) { z <- vapply(o, ult, numeric(1)); unico_si(!is.na(z) & z %% 1000 != 0) }),
  nueva_regla("cifra final par",                "divisibilidad", function(o) { z <- vapply(o, ult, numeric(1)); unico_si(!is.na(z) & z %% 2 == 0) }),
  nueva_regla("cifra final impar",              "divisibilidad", function(o) { z <- vapply(o, ult, numeric(1)); unico_si(!is.na(z) & z %% 2 == 1) }),
  nueva_regla("mas de 3 numeros en el texto",   "divisibilidad", function(o) { z <- vapply(o, function(s) length(nums(s)), numeric(1)); unico_si(z == max(z)) }),
  # --- signo --------------------------------------------------------------
  ## OBJ 2 del detractor: "descartar el precio imposible". Se mide con la bateria ya
  ## NO ciega al signo (nums() con "-?"), para saber cuanto del +26,6 pp sobrevive.
  nueva_regla("descartar precio imposible", "magnitud", function(o) {
    v <- vapply(o, function(s) { z <- nums(s); if (!length(z)) NA_real_ else z[length(z)] }, numeric(1))
    which(!is.na(v) & v > 0) }),
  nueva_regla("descartar valor no entero-plausible", "divisibilidad", function(o) {
    v <- vapply(o, function(s) { z <- nums(s); if (!length(z)) NA_real_ else z[length(z)] }, numeric(1))
    which(!is.na(v) & v > 0 & v %% 50 == 0) }),
  nueva_regla("unica con signo negativo",  "signo", function(o) unico_si(tiene(o, "[ =]-[0-9]|: ?-[0-9]"))),
  nueva_regla("unica SIN signo negativo",  "signo", function(o) unico_si(!tiene(o, "[ =]-[0-9]|: ?-[0-9]"))),
  nueva_regla("unica con resta explicita", "signo", function(o) unico_si(tiene(o, " - "))),
  # formulacion exacta que el detractor midio al 46,5 % en la version previa:
  # no exige unicidad, sobrevive TODO el conjunto sin signo negativo (score 1/|S|)
  ## DETECTOR FIEL: el regex "[ =:]-[0-9]" NO casaba con "B = 600.000 - 75" (molde del
  ## senuelo ERR-ALG-06 var 2). La familia signo figuraba CON SONDA estando medio ciega
  ## en 55/100 versiones: §P7 ocurriendo dentro de la propia sonda (detractor 2026-08-19).
  nueva_regla("descartar toda opcion con guion", "signo",
              function(o) which(!grepl("-", o, fixed = TRUE))),
  nueva_regla("elegir toda opcion con guion",    "signo",
              function(o) which(grepl("-", o, fixed = TRUE))),
  # --- posicion -----------------------------------------------------------
  ## UNA sola sonda posicional: las cuatro eran de la misma familia y nulas por
  ## construccion (el item baraja dos veces: sample.int + exshuffle). Cuatro copias
  ## no dan cobertura, solo BAJAN el techo nulo -> relleno que §P7 nombra.
  nueva_regla("la primera", "posicion", function(o) 1L),
  # --- formato ------------------------------------------------------------
  nueva_regla("unica con ecuacion de 1 incognita", "formato", function(o) unico_si(!tiene(o, "[0-9][A-Z] \\+"))),
  nueva_regla("unica con ecuacion de 2 incognitas","formato", function(o) unico_si(tiene(o, "[0-9][A-Z] \\+"))),
  nueva_regla("unica que senala su paso",          "formato", function(o) { p <- sub("^En el paso ([0-9]).*$", "\\1", o); tb <- table(p); unico_si(p %in% names(tb)[tb == 1]) }),
  nueva_regla("senala el paso mayoritario",        "formato", function(o) { p <- sub("^En el paso ([0-9]).*$", "\\1", o); tb <- table(p); which(p %in% names(tb)[tb == max(tb)]) }),
  nueva_regla("unica sin 'asi:'",                  "formato", function(o) unico_si(!tiene(o, "as[ií]:"))),
  # --- lexico -------------------------------------------------------------
  nueva_regla("unica con 'toda la ecuacion'", "lexico", function(o) unico_si(tiene(o, "toda la ecuaci"))),
  nueva_regla("unica con 'debio'",            "lexico", function(o) unico_si(tiene(o, "debi[oó]"))),
  nueva_regla("unica con 'se debe/deben'",    "lexico", function(o) unico_si(tiene(o, "se deb[ee]n?|se divide|se cambia|se suman"))),
  nueva_regla("unica con 'deberia'",          "lexico", function(o) unico_si(tiene(o, "deber[ií]a"))),
  nueva_regla("unica que menciona 'incognita'","lexico", function(o) unico_si(tiene(o, "inc[oó]gnita")))
)

cat("\n(A) BATERIA DE ELIMINACION §P7 — cierre por las 6 familias\n")
res <- evaluar_bateria(B, opciones, claves, familias_no_aplicables = character(0))
imprimir_bateria(res)
if (!identical(res$veredicto, "PASS"))
  add("BATERIA §P7: veredicto ", res$veredicto)

# ---- (B) prueba de aceptacion LEXICA (Error 32) ---------------------------
# NOTA SOBRE ESTA SONDA (limite declarado, no ajuste para aprobar):
#  - Soporte minimo 20 versiones: es el MISMO criterio que la regla #23 impone a los
#    estratos ("n < 20 -> NO CONCLUYENTE"). Con n = 10 una tasa del 100 % tiene un
#    intervalo tan ancho que no distingue senal de azar de redaccion.
#  - Se separan tokens de CONTENIDO de las palabras FUNCIONALES (preposiciones,
#    articulos, conectores). Con 4 opciones de ~20 palabras es casi inevitable que
#    alguna palabra funcional quede exclusiva de una opcion, y eso NO es explotable:
#    el estudiante no puede saber a priori que "la que dice 'para'" es la clave.
#    El VEREDICTO lo deciden los tokens de contenido; los funcionales se IMPRIMEN
#    igualmente para que la ceguera quede declarada, nunca oculta.
FUNCIONALES <- c("para","entre","toda","todo","toda","que","los","las","del","con","por",
                 "una","uno","este","esta","esto","sus","son","como","solo","otro","otra",
                 "mas","más","pero","así","asi","aqui","aquí","ella","ello","sino","cada",
                 "debe","debia","debía","hay","ser","sea","era","tiene","puede","donde")
tokens <- function(s) {
  s <- tolower(s); s <- gsub("[^a-záéíóúñü ]", " ", s)
  t <- unlist(strsplit(s, "\\s+")); unique(t[nchar(t) > 2L])
}
fuga_lexica <- function(idx, etiqueta) {
  if (length(idx) < 20L) {
    cat("   ", etiqueta, ": n =", length(idx), "< 20 -> NO CONCLUYENTE (regla #23)\n")
    add("LEXICO ", etiqueta, ": estrato con n = ", length(idx), " < 20, sin veredicto")
    return(invisible(NULL))
  }
  voc <- unique(unlist(lapply(V[idx], function(v) tokens(paste(v$ops, collapse = " ")))))
  peor_c <- NULL; peor_f <- NULL
  for (tk in voc) {
    enc <- 0L; excl <- 0L
    for (v in V[idx]) {
      tt  <- lapply(v$ops, tokens)
      hay <- vapply(tt, function(z) tk %in% z, logical(1))
      if (!any(hay)) next
      enc <- enc + 1L
      if (hay[v$clave] && sum(hay) == 1L) excl <- excl + 1L
    }
    if (enc >= 20L) {                       # soporte minimo, criterio regla #23
      r <- excl / enc
      if (tk %in% FUNCIONALES) { if (is.null(peor_f) || r > peor_f$tasa) peor_f <- list(tk = tk, tasa = r, n = enc) }
      else                     { if (is.null(peor_c) || r > peor_c$tasa) peor_c <- list(tk = tk, tasa = r, n = enc) }
    }
  }
  ## regla #23: un token con soporte < 20 NO se declara verde ni rojo. Se NOMBRA como
  ## NO CONCLUYENTE junto al N que lo dictaminaria, en vez de subir el N por defecto.
  sop <- vapply(voc, function(tk) {
    sum(vapply(V[idx], function(v) any(vapply(lapply(v$ops, tokens),
        function(z) tk %in% z, logical(1))), logical(1))) }, numeric(1))
  cortos <- voc[sop > 0 & sop < 20L]
  if (length(cortos)) {
    nmin <- min(sop[sop > 0 & sop < 20L])
    cat(sprintf("   %-16s NO CONCLUYENTE: %d token(s) con soporte < 20 (menor n = %.0f); N necesario = %.0f\n",
                paste0(etiqueta, ":"), length(cortos), nmin, ceiling(length(idx) * 20 / nmin)))
    add("LEXICO ", etiqueta, ": ", length(cortos), " token(s) con soporte < 20 -> NO CONCLUYENTE")
  }
  f <- function(x) if (is.null(x)) "-- (sin soporte >= 20)" else
        sprintf("'%s' %.1f %% de %d", x$tk, 100 * x$tasa, x$n)
  cat(sprintf("   %-16s CONTENIDO: %-26s | FUNCIONAL: %-24s %s\n",
              paste0(etiqueta, ":"), f(peor_c), f(peor_f),
              if (!is.null(peor_c) && peor_c$tasa >= 0.70) "<< FUGA" else "OK"))
  if (!is.null(peor_c) && peor_c$tasa >= 0.70)
    add("LEXICO ", etiqueta, ": token de contenido '", peor_c$tk,
        "' exclusivo de la clave en ", sprintf("%.1f %%", 100 * peor_c$tasa), " (umbral 70 %)")
}
cat("\n(B) PRUEBA DE ACEPTACION LEXICA (Error 32) — umbral 70 % sobre tokens de CONTENIDO\n")
fuga_lexica(seq_along(V), "agregado")
for (p in sort(unique(pasos))) fuga_lexica(which(pasos == p), paste0("rama paso ", p))

# ---- veredicto ------------------------------------------------------------
cat("\n============================================\n")
if (length(errores) == 0L) {
  cat("  RESULTADO: APROBADO (0 hallazgos)\n")
} else {
  cat("  RESULTADO: ", length(errores), " HALLAZGO(S)\n")
  for (e in errores) cat("   - ", e, "\n", sep = "")
}
cat("============================================\n")
quit(status = if (length(errores) == 0L) 0L else 1L)
