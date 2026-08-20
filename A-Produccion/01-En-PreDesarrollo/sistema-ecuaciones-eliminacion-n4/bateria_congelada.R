#!/usr/bin/env Rscript
## =============================================================================
## BATERIA CONGELADA — sistema-ecuaciones-eliminacion-n4
## =============================================================================
## PRE-REGISTRO (§P7-C). Congelada el 2026-08-20, ANTES de aplicar el fix de
## divisores. NO se le anaden reglas durante el ciclo: si una auditoria descubre
## una familia sin sonda, se anade y SE RE-MIDE EL HISTORICO COMPLETO, o se
## declara que las cifras anteriores no son comparables.
##
## POR QUE EXISTE. La bateria anterior (M5 · N3 · M3 · CC · L1) no contenia
## NINGUNA regla que comparase dos numeros DENTRO de la misma opcion. Resultado
## medido: dio V4 por bueno en todo cuando en realidad empeoro tres reglas y una
## cambio de signo, y dejo sin ver un canal de +20,2 pp que era PREEXISTENTE.
## Bajo §P7 exigencia 1, una familia sin sonda obliga a SIN_COBERTURA, no a PASS.
##
## CONVENCION (§P7 exigencia 4): score = 1/|S| si la clave sobrevive · 0 si no ·
## 1/n si |S| = 0. Con clave uniforme E[score] = 1/n EXACTO para toda regla, sea
## cual sea su selectividad. Techo nulo para n = 4: 25,0 %.
##
## ARBITRAJE YA RESUELTO (2026-08-20): una opcion que NO es candidata para la
## regla queda FUERA de S, no dentro. Meterla dentro infla |S| y hunde el score
## artificialmente: es lo que produjo la discrepancia +40,5 pp vs +6,0 pp sobre
## los mismos datos. Todas las reglas de aqui siguen el criterio estricto.
##
## Uso:  Rscript bateria_congelada.R [ruta.Rmd] [semilla_base] [multiplicador]
## =============================================================================

args <- commandArgs(TRUE)
RMD  <- if (length(args) >= 1) args[1] else
  "sistema_ecuaciones_eliminacion_numerico_variacional_argumentacion_n4_schoice_v1.Rmd"
BASE <- if (length(args) >= 2) as.integer(args[2]) else 424242L
MULT <- if (length(args) >= 3) as.integer(args[3]) else 31L
N    <- 100L   # regla #23: muestra estandar, NO elegible por sesion

## --- cortes: FUENTE UNICA en el helper compartido ---------------------------
raiz <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE)[1],
                 error = function(e) NA_character_)
helper <- file.path(raiz, ".claude", "scripts", "bateria_eliminacion.R")
if (is.na(raiz) || !file.exists(helper))
  stop("No se encuentra .claude/scripts/bateria_eliminacion.R: los cortes de §P7 ",
       "tienen UNA sola fuente y este script no puede inventarlos.")
source(helper)
CORTE_CANAL_L <- get("CORTE_CANAL"); CORTE_RUIDO_L <- get("CORTE_RUIDO")

## --- extraccion del chunk ----------------------------------------------------
lin <- readLines(RMD, warn = FALSE)
i0  <- grep("^```\\{r data_generation", lin)[1] + 1L
i1  <- i0 - 1L + which(grepl("^```\\s*$", lin[i0:length(lin)]))[1] - 1L
CODE <- lin[i0:i1]

num  <- function(s) suppressWarnings(as.numeric(gsub("\\.", "", gsub(",", ".", trimws(s)))))
nums <- function(s) { z <- regmatches(s, gregexpr("-?[0-9][0-9.,]*", s))[[1]]
                      if (!length(z)) numeric(0) else num(z) }
ultn <- function(s) { z <- nums(s); if (!length(z)) NA_real_ else z[length(z)] }
## divisor impreso por una opcion de paso 3, o NA
divisor <- function(o) { m <- regmatches(o, regexec("dividir entre (-?[0-9.]+)", o))[[1]]
                         if (length(m) == 2) num(m[2]) else NA_real_ }
## ecuacion que la opcion exhibe (tras "ser: " o "reducida ")
ecuacion <- function(o) sub("^.*(ser: |reducida )", "", o)
## coeficientes que acompanan a una incognita en esa ecuacion
coefs_ecu <- function(o) { e <- ecuacion(o)
  z <- regmatches(e, gregexpr("-?[0-9][0-9.]*(?=[A-Za-z])", e, perl = TRUE))[[1]]
  if (!length(z)) numeric(0) else num(z) }
n_incog  <- function(o) length(unique(regmatches(ecuacion(o),
                        gregexpr("[0-9][A-Za-z]", ecuacion(o)))[[1]]))
n_nums   <- function(o) length(nums(o))

## --- LAS REGLAS (congeladas) -------------------------------------------------
## Cada fn devuelve el conjunto S de indices que la regla deja en pie, o
## integer(0) si la regla no deja a nadie. `par` y `estimulo` se inyectan.
REGLAS <- list(
  ## --- familia FORMATO: coherencia INTRA-OPCION (la que faltaba) ------------
  list(n="V1 divisor == coef de su propia ecuacion", f="formato", fn=function(o, p)
        which(vapply(o, function(x){d <- divisor(x); if (is.na(d)) return(FALSE)
                                    any(abs(coefs_ecu(x)) == abs(d))}, logical(1)))),
  list(n="E2 divisor NO es coef del enunciado",      f="formato", fn=function(o, p)
        which(vapply(o, function(x){d <- divisor(x); if (is.na(d)) return(FALSE)
                                    !(abs(d) %in% abs(c(p$a,p$b,p$c,p$d)))}, logical(1)))),
  list(n="NTm la opcion con MENOS numeros",          f="formato", fn=function(o, p){
        k <- vapply(o, n_nums, integer(1)); which(k == min(k))}),
  list(n="U1 su ecuacion tiene UNA incognita",       f="formato", fn=function(o, p)
        which(vapply(o, n_incog, integer(1)) == 1L)),
  ## --- familia MAGNITUD -----------------------------------------------------
  list(n="V5 divide entre el numero MAS GRANDE",     f="magnitud", fn=function(o, p){
        d <- vapply(o, divisor, numeric(1)); if (all(is.na(d))) return(integer(0))
        which(!is.na(d) & abs(d) == max(abs(d), na.rm=TRUE))}),
  list(n="M5 la de MENOR |valor| final",             f="magnitud", fn=function(o, p){
        v <- vapply(o, ultn, numeric(1)); which(abs(v) == min(abs(v), na.rm=TRUE))}),
  list(n="M6c DESCARTAR la de mayor |valor|",        f="magnitud", fn=function(o, p){
        v <- vapply(o, ultn, numeric(1)); which(abs(v) != max(abs(v), na.rm=TRUE))}),
  list(n="L1 la mas corta",                          f="magnitud", fn=function(o, p){
        k <- nchar(o); which(k == min(k))}),
  list(n="L2 la mas larga",                          f="magnitud", fn=function(o, p){
        k <- nchar(o); which(k == max(k))}),
  ## --- familia DIVISIBILIDAD ------------------------------------------------
  list(n="D50 valor final multiplo de 50",           f="divisibilidad", fn=function(o, p){
        v <- vapply(o, ultn, numeric(1)); which(!is.na(v) & v %% 50 == 0)}),
  list(n="DEN divisor multiplo de 10",               f="divisibilidad", fn=function(o, p){
        d <- vapply(o, divisor, numeric(1)); which(!is.na(d) & abs(d) %% 10 == 0)}),
  ## --- familia SIGNO --------------------------------------------------------
  list(n="S1 sin ningun signo negativo",             f="signo", fn=function(o, p)
        which(!grepl("-", o, fixed = TRUE))),
  ## --- familia POSICION -----------------------------------------------------
  list(n="P1 cita el paso 1",                        f="posicion", fn=function(o, p)
        which(grepl("^En el paso 1", o))),
  list(n="P3 cita el paso 3",                        f="posicion", fn=function(o, p)
        which(grepl("^En el paso 3", o))),
  ## --- familia LEXICO -------------------------------------------------------
  list(n="X1 dice 'toda la ecuacion'",               f="lexico", fn=function(o, p)
        which(grepl("toda la ecuaci", o)))
)

## --- recogida ----------------------------------------------------------------
OPS <- list(); KEY <- integer(0); PASO <- integer(0); PAR <- list(); ab <- 0L
for (s in seq_len(N)) {
  set.seed(BASE + MULT * s); e <- new.env()
  if (!isTRUE(tryCatch({ eval(parse(text = CODE), envir = e); TRUE },
                       error = function(x) FALSE))) { ab <- ab + 1L; next }
  OPS[[length(OPS)+1L]] <- e$opciones; KEY <- c(KEY, which(e$sol == 1L))
  PASO <- c(PASO, e$paso_real);        PAR[[length(PAR)+1L]] <- e$par
}
nv <- length(OPS)

evaluar <- function(idx) {
  if (!length(idx)) return(NULL)
  out <- lapply(REGLAS, function(R) {
    sc <- vapply(idx, function(j) {
      S <- tryCatch(R$fn(OPS[[j]], PAR[[j]]), error = function(e) integer(0))
      n <- length(OPS[[j]])
      if (!length(S)) 1/n else if (KEY[j] %in% S) 1/length(S) else 0
    }, numeric(1))
    data.frame(regla = R$n, familia = R$f, tasa = mean(sc),
               exceso = 100*(mean(sc) - 0.25), stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, out); d[order(-d$exceso), ]
}

pr <- function(tit, d) {
  if (is.null(d)) { cat("\n", tit, ": sin versiones\n", sep=""); return(invisible()) }
  cat(sprintf("\n--- %s (n = %d) ---\n", tit, attr(d, "nv")))
  for (r in seq_len(nrow(d)))
    cat(sprintf("   %-42s %-14s %5.1f %%   %+6.1f pp%s\n", d$regla[r], d$familia[r],
        100*d$tasa[r], d$exceso[r],
        if (d$exceso[r] >= 100*CORTE_CANAL_L) "  <== CANAL" else ""))
}

cat("=====================================================================\n")
cat("BATERIA CONGELADA §P7-C —", basename(RMD), "\n")
cat(sprintf("semillas %d + %d i | N = %d | renderizan %d | abortos %d\n",
            BASE, MULT, N, nv, ab))
cat(sprintf("cortes (fuente unica, helper): ruido %+.1f pp | canal %+.1f pp | control oficial +5,3 pp\n",
            100*CORTE_RUIDO_L, 100*CORTE_CANAL_L))
cat("cobertura: las SEIS familias tienen sonda (magnitud, divisibilidad, signo,\n")
cat("           posicion, formato, lexico) -> no procede SIN_COBERTURA\n")
cat("=====================================================================\n")

g <- evaluar(seq_len(nv)); attr(g, "nv") <- nv; pr("AGREGADO", g)
for (p in sort(unique(PASO))) {
  idx <- which(PASO == p); d <- evaluar(idx); attr(d, "nv") <- length(idx)
  ## regla #23: un estrato con n < 20 no se declara ni verde ni rojo
  pr(sprintf("ESTRATO paso %d%s", p, if (length(idx) < 20L) "  [n < 20: NO CONCLUYENTE]" else ""), d)
}

mx <- max(g$exceso)
cat(sprintf("\n>>> MAXIMO AGREGADO: %+.1f pp  (%s)\n", mx, g$regla[which.max(g$exceso)]))
cat(sprintf(">>> VEREDICTO: %s\n",
    if (mx >= 100*CORTE_CANAL_L) "BLOQUEA — por encima del corte de canal"
    else if (mx > 5.3) "ZONA GRIS — por encima del control oficial, bajo el corte"
    else "DENTRO DE LA VARA"))
quit(status = if (mx >= 100*CORTE_CANAL_L) 1L else 0L)
