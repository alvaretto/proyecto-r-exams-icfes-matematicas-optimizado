## =============================================================================
## candidatos_s2c.R — tanda S2c (§P7-F), ciclo 2026-11-02 del SCHOICE.
## Mutaciones EN MEMORIA del chunk data_generation (no tocan el .Rmd). Cada par c(viejo, nuevo) debe aparecer
## exactamente una vez en el código (lo comprueba verificar_s2c.R). Con los parámetros nulos (Q = 0, ERR-RESTA
## permitido en G, D2 apagado) el código mutado no consume el RNG de más: D0 debe reproducir B0 exactamente.
## Parámetros (variables de entorno): S2C_Q_C, S2C_Q_P (probabilidad de que P cite δ en CURSOS / PROMEDIO),
## S2C_RESTA_C, S2C_RESTA_P (1 = ERR-RESTA permitido en el distractor de G), S2C_D2 (1 = ERR-RESTA sin cifra).
## Vehículo de δ del lado de P: CURSOS, la ventaja de P (padv, ERR-PROM, como la C del impreso);
## PROMEDIO, la razón sin sustento (psin): allí P tiene el MENOR promedio y ningún error de la ventaja de P da δ.
## =============================================================================
num <- function(v, def) { x <- Sys.getenv(v, ""); if (nzchar(x)) as.numeric(x) else def }
CONST_S2C <- sprintf(paste0(
  "## S2c: parámetros del candidato (en memoria)\n",
  "Q_DELTA_P  <- c(CURSOS = %s, PROMEDIO = %s)\n",
  "VEHICULO   <- c(CURSOS = \"padv\", PROMEDIO = \"psin\")\n",
  "RESTA_G_OK <- c(CURSOS = %s, PROMEDIO = %s)\n",
  "D2_SIN_CIFRA <- %s\n"),
  num("S2C_Q_C", 0), num("S2C_Q_P", 0), if (num("S2C_RESTA_C", 1) == 1) "TRUE" else "FALSE",
  if (num("S2C_RESTA_P", 1) == 1) "TRUE" else "FALSE", if (num("S2C_D2", 0) == 1) "TRUE" else "FALSE")
CAMBIOS <- list(
  ## --- constantes
  c(r"---(## --- Espacio de parámetros (familia F1: se enumera y se sortea; nada se remuestrea))---",
    paste0(CONST_S2C, r"---(## --- Espacio de parámetros (familia F1: se enumera y se sortea; nada se remuestrea))---")),
  ## --- espacio_rama: la cifra de padv (xP) puede ser δ (D1/D3, rama CURSOS); ERR-RESTA puede salir de G (D3b/D4)
  c(r"---(espacio_rama <- function(rama) {)---", r"---(espacio_rama <- function(rama, xP_delta = FALSE) {)---"),
  c(r"---(  ok <- ok & g$D != g$delta & g$D != g$parcialP        # L-2 y canal 8: la resta y la ventaja de P tampoco)---",
    r"---(  g$xP <- if (xP_delta) g$delta else g$parcialP        # S2c: cifra de padv
  ok <- ok & g$D != g$delta & g$D != g$xP              # L-2 y canal 8: la resta y la cifra de P tampoco)---"),
  c(r"---(  g$p_tabla <- g$parcialP == g$aG | g$parcialP == g$aP | g$parcialP == g$nG | g$parcialP == g$nP)---",
    r"---(  g$p_tabla <- g$xP == g$aG | g$xP == g$aP | g$xP == g$nG | g$xP == g$nP)---"),
  c(r"---(  g$hay_debajo <- (g$delta < g$D & g$delta != g$parcialP) |
                  (!g$p_tabla & ((g$aG < g$D & g$aG != g$parcialP) | (g$aP < g$D & g$aP != g$parcialP))))---",
    r"---(  g$hay_debajo <- (RESTA_G_OK[[rama]] & !xP_delta & g$delta < g$D & g$delta != g$xP) |
                  (!g$p_tabla & ((g$aG < g$D & g$aG != g$xP) | (g$aP < g$D & g$aP != g$xP))))---"),
  ## --- sorteo: padv con δ (CURSOS) se decide ANTES de filtrar el espacio, para que el lugar de D cuente con su cifra
  c(r"---(  b_debajo <- pick(0:3)
  esp <- espacio_rama(rama))---",
    r"---(  b_debajo <- pick(0:3)
  s2c_padv_delta <- rama == "CURSOS" && VEHICULO[["CURSOS"]] == "padv" && Q_DELTA_P[["CURSOS"]] > 0 &&
    runif(1) < Q_DELTA_P[["CURSOS"]]
  esp <- espacio_rama(rama, xP_delta = s2c_padv_delta))---"),
  c(r"---(  pa_deb <- esp$parcialP < esp$D)---", r"---(  pa_deb <- esp$xP < esp$D)---"),
  c(r"---(  lado_padv <- if (d$parcialP < d$D) "debajo" else "encima")---",
    r"---(  lado_padv <- if (f$xP < d$D) "debajo" else "encima")---"),
  ## --- psin con δ (rama PROMEDIO): se restringen los repartos de lados a los que ponen psin del lado de δ y dejan a G
  ## una cifra de su lado sin ERR-RESTA; si no queda ninguno, cascada (F1): sin δ en P en esa versión (s2c_fallback)
  c(r"---(  stopifnot("F1: reparto de lados posible" = length(repartos) > 0)
  lados <- pick1(repartos); lado_gdis <- lados[["gdis"]])---",
    r"---(  stopifnot("F1: reparto de lados posible" = length(repartos) > 0)
  s2c_quiere_psin <- rama == "PROMEDIO" && VEHICULO[["PROMEDIO"]] == "psin" && Q_DELTA_P[["PROMEDIO"]] > 0 &&
    runif(1) < Q_DELTA_P[["PROMEDIO"]]
  s2c_psin_delta <- FALSE; s2c_fallback <- FALSE
  if (s2c_quiere_psin) {
    lado_delta <- if (d$delta < d$D) "debajo" else "encima"
    xs_g <- setdiff(c(if (!f$p_tabla) c(d$aG, d$aP), d$parcialG, d$TG), d$D)
    rep_d <- Filter(function(z) z[["psin"]] == lado_delta &&
                      (if (z[["gdis"]] == "debajo") any(xs_g < d$D) else any(xs_g > d$D)), repartos)
    if (length(rep_d)) { repartos <- rep_d; s2c_psin_delta <- TRUE } else s2c_fallback <- TRUE
  }
  lados <- pick1(repartos); lado_gdis <- lados[["gdis"]])---"),
  c(r"---(  cand_g <- cand_g[cand_g$x != d$D, ]                               # L-2: la cifra falsa nunca es D)---",
    r"---(  cand_g <- cand_g[cand_g$x != d$D, ]                               # L-2: la cifra falsa nunca es D
  if (s2c_padv_delta || s2c_psin_delta || !RESTA_G_OK[[rama]]) cand_g <- cand_g[cand_g$codigo != "ERR-RESTA", ])---"),
  c(r"---(  molde_padv <- pick(moldes_de("perfil", perfil_P)); fuente_padv <- "parcialP")---",
    r"---(  molde_padv <- pick(moldes_de("perfil", perfil_P)); fuente_padv <- if (s2c_padv_delta) "delta" else "parcialP")---"),
  c(r"---(  usadas <- c(d$D, fila_g$x, d$parcialP, d$nG, d$nP, d$aG, d$aP, calculos_tabla(d)))---",
    r"---(  usadas <- c(d$D, fila_g$x, d$parcialP, f$xP, d$nG, d$nP, d$aG, d$aP, calculos_tabla(d)))---"),
  c(r"---(  y <- pick(rango_y)
  stopifnot("H1: la cifra de la razón sin sustento no sale de un cálculo con la tabla" =
              !(y %in% calculos_tabla(d))))---",
    r"---(  y <- if (s2c_psin_delta) d$delta else pick(rango_y)
  stopifnot("H1: la cifra de la razón sin sustento no sale de un cálculo con la tabla" =
              s2c_psin_delta || !(y %in% calculos_tabla(d))))---"),
  c(r"---(    gdis  = afirmacion(molde_gdis, "G", fila_g$x, cod_gdis),
    padv  = afirmacion(molde_padv, "P", d$parcialP, cod_padv),)---",
    r"---(    gdis  = if (D2_SIN_CIFRA && cod_gdis == "ERR-RESTA")
              modifyList(afirmacion(molde_gdis, "G", NA_real_, cod_gdis), list(afirma = "dif_igual_delta")) else
              afirmacion(molde_gdis, "G", fila_g$x, cod_gdis),
    padv  = afirmacion(molde_padv, "P", f$xP, cod_padv),)---"),
  ## --- explicaciones y comprobaciones que dependen de la cifra
  c(r"---(         if (af$molde == "P_ven") {)---",
    r"---(         if (af$molde == "P_ven" || identical(p$fuente_padv, "delta")) {)---"),
  c(r"---(         if (!is.na(p$y) && (p$y == p$D || p$y %in% calculos_tabla(p))))---",
    r"---(         if (!is.na(p$y) && (p$y == p$D || (p$y %in% calculos_tabla(p) && !(isTRUE(p$s2c_psin_delta) && p$y == p$delta)))))---"),
  c(r"---(                    fuente_padv = fuente_padv, y = if (es_canonica) NA_real_ else y))---",
    r"---(                    fuente_padv = fuente_padv, y = if (es_canonica) NA_real_ else y,
                    s2c_psin_delta = if (es_canonica) FALSE else s2c_psin_delta))---"),
  ## --- D2: ERR-RESTA sin cifra («en lo mismo que se diferencian los dos promedios»), con su función de verdad
  c(r"---(cierre <- function(x, O) paste0("su total fue más alto que el de ", O, " en ", x, ED, "kilos."))---",
    r"---(cierre <- function(x, O) if (is.na(x)) paste0("su total fue más alto que el de ", O,
  " en lo mismo que se diferencian los dos promedios.") else paste0("su total fue más alto que el de ", O, " en ", x, ED, "kilos."))---"),
  c(r"---(    dif_prom     = a[[S]] - a[[O]] == af$x,)---",
    r"---(    dif_prom     = a[[S]] - a[[O]] == af$x,
    dif_igual_delta = tt[[S]] - tt[[O]] == abs(a[["G"]] - a[["P"]]),)---"),
  c(r"---(  all(vapply(afs, function(a) grepl()---", r"---(  all(vapply(afs, function(a) is.na(a$x) || grepl()---"),
  c(r"---(                      as.numeric(c(afs$gdis$x, afs$padv$x, afs$psin$x)))))---",
    r"---(                      as.numeric(c(if (is.na(afs$gdis$x)) params$delta else afs$gdis$x, afs$padv$x, afs$psin$x)))))---"),
  c(r"---(       explica = function(p, af) paste0("La cifra ", af$x, " sale de restar los promedios: $",)---",
    r"---(       explica = function(p, af) paste0(if (is.na(af$x)) "Esa afirmación toma la resta de los promedios como la diferencia entre los totales: $" else
         paste0("La cifra ", af$x, " sale de restar los promedios: $"),)---"),
  c(r"---(" sí ganó, pero por ", d$D, ED, "kilos, no por ", afs$gdis$x, "."),)---",
    r"---(" sí ganó, pero por ", d$D, ED, "kilos, no por ", if (is.na(afs$gdis$x)) paste0("lo que se diferencian los promedios, ",
                 d$delta, ED, "kilos") else afs$gdis$x, "."),)---")
)
