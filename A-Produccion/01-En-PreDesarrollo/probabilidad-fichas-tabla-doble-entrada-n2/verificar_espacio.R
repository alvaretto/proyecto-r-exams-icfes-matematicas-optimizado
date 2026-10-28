#!/usr/bin/env Rscript
## =============================================================================
## verificar_espacio.R — verificador propio del subproyecto (paso 9 del orquestador)
## probabilidad-fichas-tabla-doble-entrada-n2 · MAT-2026-07-032 · 2026-10-26
##
##   E1  Enumeración EXACTA del espacio no canónico de construir_cubo(): todas las
##       combinaciones alcanzables de (err_PQ, s, a, cc, (e, h), (f, g)). Comprueba I-5
##       (una sola pareja con frecuencias iguales, la clave), I-7 (celdas >= 1 y R2:
##       n1 + n2 < N en las cuatro parejas) y la precondición de cada error elegido.
##   E2  Muestreo por cuotas (regla #23): N = 100 versiones por estrato (los 6 pares
##       ordenados err_PQ) y la canónica aparte. Cada cubo muestreado debe pertenecer
##       al espacio de E1; si no, el enumerador no modela al generador.
##   E3  Instancia canónica contra literales copiados de ITEM_OFICIAL.md (no del .Rmd):
##       I-2 verbatim, I-5 y R2 con las tres lecturas de «se eligen dos fichas».
##   E4  Batería §P7 CONGELADA en la cabecera (antes de medir; §P7-C), con cierre por
##       familias, sobre la muestra no canónica, y la canónica medida aparte.
##   E5  Mutantes con sonda declarada (Incidente P), aplicados en memoria al chunk
##       data_generation; el patrón de cada mutante aparece exactamente una vez.
##
## Límite declarado: E1 re-implementa los conjuntos de elección de construir_cubo().
## E2 comprueba que el generador real no sale de ese conjunto, pero un enumerador y un
## generador que se equivocaran igual no se detectarían entre sí.
## Uso: Rscript --no-init-file verificar_espacio.R   (desde cualquier directorio)
## =============================================================================
suppressMessages(library(exams))

args_all <- commandArgs(trailingOnly = FALSE)
f_arg <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
DIR  <- if (length(f_arg)) dirname(normalizePath(f_arg)) else getwd()
ROOT <- normalizePath(file.path(DIR, "..", "..", ".."))
RMD  <- file.path(DIR, "probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd")
stopifnot(file.exists(RMD), file.exists(file.path(ROOT, ".claude/scripts/bateria_eliminacion.R")))
lin <- readLines(RMD, encoding = "UTF-8")
ini <- grep("^```\\{r data_generation", lin)[1]
fin <- which(lin == "```" & seq_along(lin) > ini)[1]
CODIGO <- paste(lin[(ini + 1):(fin - 1)], collapse = "\n")
EXPR <- parse(text = CODIGO)
N_ESTANDAR <- 100L        # regla #23
TOPE <- 4000L             # tope de semillas (cuotas y mutantes)
DIMS_ORD <- c("color", "categ", "figura")

errores <- character(0)
fallo <- function(...) { m <- paste0(...); errores <<- c(errores, m); cat("  FALLO:", m, "\n") }
bien  <- function(...) cat("  OK:", paste0(...), "\n")
titulo <- function(t) cat("\n", strrep("=", 78), "\n", t, "\n", strrep("=", 78), "\n", sep = "")

## ---- E4 · Batería §P7 CONGELADA el 2026-10-26, antes de la primera medición -----
## No se modifica en este ciclo (§P7-C). Reglas que ven solo las opciones (y, en las de
## molde, las dimensiones que nombra cada opción, que el estudiante también lee).
source(file.path(ROOT, ".claude/scripts/bateria_eliminacion.R"))
largo <- function(op) nchar(op)
molde <- function(op) sub("\\|.*$", "", names(op))
cruz  <- function(op) suppressWarnings(as.integer(sub("^.*\\|", "", names(op))))
sub_si <- function(s) if (any(s) && !all(s)) s else NA
REGLAS <- list(
  nueva_regla("mas_larga_unica", "magnitud", function(op) { l <- largo(op); if (sum(l == max(l)) == 1L) which.max(l) else NA }),
  nueva_regla("mas_corta_unica", "magnitud", function(op) { l <- largo(op); if (sum(l == min(l)) == 1L) which.min(l) else NA }),
  nueva_regla("descartar_mas_largas", "magnitud", function(op) sub_si(largo(op) < max(largo(op)))),
  nueva_regla("descartar_mas_cortas", "magnitud", function(op) sub_si(largo(op) > min(largo(op)))),
  nueva_regla("dos_mas_cortas", "magnitud", function(op) { l <- largo(op); sub_si(l <= sort(l)[2]) }),
  nueva_regla("dos_mas_largas", "magnitud", function(op) { l <- largo(op); sub_si(l >= sort(l, decreasing = TRUE)[2]) }),
  nueva_regla("molde_color_figura", "formato", function(op) sub_si(molde(op) == "color+figura")),
  nueva_regla("molde_color_categoria", "formato", function(op) sub_si(molde(op) == "color+categ")),
  nueva_regla("molde_categoria_figura", "formato", function(op) sub_si(molde(op) == "categ+figura")),
  nueva_regla("cruzada_en_su_molde (relacional)", "formato", function(op) {
    x <- cruz(op); if (all(is.na(x))) NA else sub_si(!is.na(x) & x == 1L) }),
  nueva_regla("con_categoria", "lexico", function(op) sub_si(grepl("categoría", op, fixed = TRUE))),
  nueva_regla("sin_categoria", "lexico", function(op) sub_si(!grepl("categoría", op, fixed = TRUE))),
  nueva_regla("prefijo_unico_3_palabras", "lexico", function(op) {
    p <- vapply(strsplit(op, " "), function(w) paste(head(w, 3), collapse = " "), "")
    u <- which(!(p %in% p[duplicated(p)])); if (length(u) == 1L) u else NA }),
  nueva_regla("token_exclusivo", "lexico", function(op) {
    tk <- lapply(op, function(x) unique(tolower(gsub("[[:punct:]«»]", "", strsplit(x, "\\s+")[[1]]))))
    n_ex <- vapply(seq_along(tk), function(i) sum(!tk[[i]] %in% unlist(tk[-i])), 0)
    if (max(n_ex) == 0) NA else sub_si(n_ex == max(n_ex)) }),
  nueva_regla("primera_posicion", "posicion", function(op) 1L),
  nueva_regla("ultima_posicion", "posicion", function(op) length(op)))
FAMILIAS_NA <- c(
  divisibilidad = "las opciones no contienen cifras: nombran tipos de ficha",
  signo = "las opciones no tienen polaridad (ni sí/no, ni aumenta/disminuye, ni signos)")
p_acierto <- function(regla, op, k) {
  s <- tryCatch(regla$fn(op), error = function(e) NA)
  if (length(s) == 0 || all(is.na(s))) return(1 / length(op))
  if (is.numeric(s)) s <- seq_along(op) %in% s
  if (!any(s)) return(1 / length(op))
  if (s[k]) 1 / sum(s) else 0
}

## ---- Anclas: el enumerador se ata al texto del generador ------------------------
ANCLAS <- c("r_resto <- 3:24",
  "a <- pick(r_resto); b <- pick(r_resto[a + r_resto >= 8 & a + r_resto <= 46]); s <- a + b",
  "c_ok <- r_resto[(s - r_resto) %in% r_resto & !(r_resto %in% c(a, b)) & abs(b - r_resto) <= 15]",
  "cc <- pick(c_ok); d <- s - cc", "m_malo <- if (abs(b - cc) == 1L) 2L * (b - cc) else NA",
  "e <- d; h <- pick(setdiff(r_resto, c(a, d, d - m_malo)))",
  "h <- a; e <- pick(setdiff(r_resto, c(d, a, a + m_malo)))",
  "deltas <- deltas[(d - a) + deltas != 0]", "delta <- pick(deltas); k <- d - a + delta",
  "h <- pick(setdiff(r_resto[(r_resto + k) %in% r_resto], c(a, a - delta, cc, b - k)))", "e <- h + k",
  "f <- cc; g <- pick(setdiff(r_resto, c(b, cc + m, cc, e)))",
  "g <- b; f <- pick(setdiff(r_resto, c(cc, b - m, b, h)))",
  "deltas <- deltas[(b - cc) + deltas != m & (b - cc) + deltas != 0]", "delta <- pick(deltas); k <- b - cc + delta",
  "f <- pick(setdiff(r_resto[(r_resto + k) %in% r_resto], c(cc, cc - delta, h, e - k)))", "g <- f + k",
  "aplicables <- setdiff(names(errores_conceptuales), c(\"PROB-SIM\", \"PROB-CRU\"))", "err_PQ <- sample(aplicables, 2)",
  "prob = c(P_CANON, 1 - P_CANON)")              # el sorteo usa P_CANON (detractor r3, H3)
for (a in ANCLAS) if (!grepl(a, CODIGO, fixed = TRUE)) stop("El generador cambió: falta «", a, "». Actualizar el enumerador.")

R_RESTO <- 3:24
C_OK <- function(a, b) {                         # el mismo conjunto que c_ok del generador
  s <- a + b; R_RESTO[(s - R_RESTO) %in% R_RESTO & !(R_RESTO %in% c(a, b)) & abs(b - R_RESTO) <= 15]
}
ERRS <- c("PROB-UNA", "PROB-CER")                # PROB-CRU solo en la canónica (2026-10-27)
PARES_ERR <- list(); for (x in ERRS) for (y in ERRS) if (x != y) PARES_ERR[[length(PARES_ERR) + 1]] <- c(x, y)
EH <- function(err1, a, b, cc, d) {
  m_malo <- if (abs(b - cc) == 1L) 2L * (b - cc) else NA
  if (err1 == "PROB-UNA") {
    rbind(cbind(e = d, h = setdiff(R_RESTO, c(a, d, d - m_malo))),
          cbind(e = setdiff(R_RESTO, c(d, a, a + m_malo)), h = a))
  } else {
    dl <- c(-1L, 1L); dl <- dl[(d - a) + dl != 0]
    do.call(rbind, lapply(dl, function(x) { k <- d - a + x
      h <- setdiff(R_RESTO[(R_RESTO + k) %in% R_RESTO], c(a, a - x, cc, b - k)); cbind(e = h + k, h = h) }))
  }
}
FG <- function(err2, b, cc, e, h, m) {
  if (err2 == "PROB-UNA") {
    rbind(cbind(f = cc, g = setdiff(R_RESTO, c(b, cc + m, cc, e))),
          cbind(f = setdiff(R_RESTO, c(cc, b - m, b, h)), g = b))
  } else {
    dl <- c(-1L, 1L); dl <- dl[(b - cc) + dl != m & (b - cc) + dl != 0]
    if (!length(dl)) return(NULL)                  # el generador abortaría: pick() sin deltas
    do.call(rbind, lapply(dl, function(x) { k <- b - cc + x
      f <- setdiff(R_RESTO[(R_RESTO + k) %in% R_RESTO], c(cc, cc - x, h, e - k)); cbind(f = f, g = f + k) }))
  }
}

## ---- E1 · Enumeración exacta ---------------------------------------------------------
titulo("E1 · Enumeración exacta del espacio no canónico")
cnt <- c(combinaciones = 0, aborto_c_ok = 0, aborto_m0 = 0, aborto_vacio = 0, repite_fuera_de_UNA = 0, celda_menor_1 = 0, clave_desigual = 0,
         segunda_igualdad = 0, R2_complementario = 0, lectura_exactamente_una_empata = 0, precondicion = 0)
por_estrato <- setNames(numeric(length(PARES_ERR)), vapply(PARES_ERR, paste, "", collapse = ">"))
t0 <- Sys.time()
for (pe in PARES_ERR) {
  e1 <- pe[1]; e2 <- pe[2]; k_est <- paste(pe, collapse = ">")
  for (a in R_RESTO) {
    for (b in R_RESTO[a + R_RESTO >= 8 & a + R_RESTO <= 46]) {
      s <- a + b; c_ok <- C_OK(a, b)
      if (!length(c_ok)) { cnt["aborto_c_ok"] <- cnt["aborto_c_ok"] + 1; next }
      for (cc in c_ok) {
        d <- s - cc; eh <- EH(e1, a, b, cc, d)
        if (!nrow(eh)) { cnt["aborto_vacio"] <- cnt["aborto_vacio"] + 1; next }   # pick() de un vacío (H4 r5)
        for (i in seq_len(nrow(eh))) {
          e <- eh[i, "e"]; h <- eh[i, "h"]; m <- e - h
          fg <- FG(e2, b, cc, e, h, m)
          if (is.null(fg)) { cnt["aborto_m0"] <- cnt["aborto_m0"] + 1; next }
          if (!nrow(fg)) { cnt["aborto_vacio"] <- cnt["aborto_vacio"] + 1; next }
          f <- fg[, "f"]; g <- fg[, "g"]; n <- length(f)
          cnt["combinaciones"] <- cnt["combinaciones"] + n; por_estrato[k_est] <- por_estrato[k_est] + n
          N <- a + b + cc + d + e + f + g + h
          celdas_ok <- min(a, b, cc, d, e, h) >= 1 & f >= 1 & g >= 1
          n_par <- list(clave = cbind(a + b, cc + d), esp = cbind(e + f, g + h),
                        P = cbind(e + a, d + h), Q = cbind(cc + g, f + b))
          igual <- sapply(n_par, function(x) rep_len(x[, 1] == x[, 2], n))
          if (!is.matrix(igual)) igual <- matrix(igual, nrow = n)
          sum_par <- sapply(n_par, function(x) rep_len(x[, 1] + x[, 2], n))
          if (!is.matrix(sum_par)) sum_par <- matrix(sum_par, nrow = n)
          ## lectura «exactamente una de las dos es del tipo»: empata si n1 = n2 o n1 + n2 = N
          empata_1 <- igual | (sum_par == N)
          pre_P <- switch(e1, "PROB-UNA" = (e == d | a == h) & (e + a != d + h),
                              "PROB-CRU" = (e == h | a == d) & (e + a != d + h),
                              "PROB-CER" = abs(e + a - d - h) == 1)
          pre_Q <- switch(e2, "PROB-UNA" = (cc == f | g == b) & (cc + g != f + b),
                              "PROB-CRU" = (cc == b | g == f) & (cc + g != f + b),
                              "PROB-CER" = abs(cc + g - f - b) == 1)
          pre_S <- (e + f) != (g + h)
          ## Canal de estímulo: solo la trampa PROB-UNA repite un valor de celda entre sus dos tipos
          rep_clave <- (a == cc) | (a == d) | (b == cc) | (b == d)
          rep_esp <- (e == g) | (e == h) | (f == g) | (f == h)
          rep_P <- (e == d) | (e == h) | (a == d) | (a == h)
          rep_Q <- (cc == f) | (cc == b) | (g == f) | (g == b)
          ok_est <- !rep_clave & !rep_esp & (if (e1 == "PROB-UNA") rep_P & !rep_Q else rep_Q & !rep_P)
          cnt["repite_fuera_de_UNA"] <- cnt["repite_fuera_de_UNA"] + sum(!rep_len(ok_est, n))
          cnt["celda_menor_1"] <- cnt["celda_menor_1"] + sum(!celdas_ok)
          cnt["clave_desigual"] <- cnt["clave_desigual"] + sum(!igual[, 1])
          cnt["segunda_igualdad"] <- cnt["segunda_igualdad"] + sum(rowSums(igual[, -1, drop = FALSE]) > 0)
          cnt["R2_complementario"] <- cnt["R2_complementario"] + sum(rowSums(sum_par >= N) > 0)
          cnt["lectura_exactamente_una_empata"] <- cnt["lectura_exactamente_una_empata"] + sum(rowSums(empata_1[, -1, drop = FALSE]) > 0)
          cnt["precondicion"] <- cnt["precondicion"] + sum(!(rep_len(pre_P, n) & rep_len(pre_Q, n) & pre_S))
        }
      }
    }
  }
}
cat(sprintf("  Tiempo: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(sprintf("  Combinaciones alcanzables: %s\n", format(cnt["combinaciones"], big.mark = ".")))
print(por_estrato)
for (k in setdiff(names(cnt), "combinaciones")) {
  if (cnt[k] == 0) bien(k, " = 0") else fallo("E1 ", k, " = ", cnt[k])
}
if (cnt["repite_fuera_de_UNA"] == 0) cat("  Canal de estímulo exacto: en las", format(cnt["combinaciones"], big.mark = "."),
  "combinaciones solo PROB-UNA repite una celda; la regla «descarta la que repite» acierta 1/3 (+8,3 pp)\n")

## ---- E2 · Muestreo por cuotas y pertenencia al espacio ---------------------------
titulo("E2 · Muestreo por cuotas (N = 100 por estrato) y pertenencia al espacio de E1")
meta_par <- function(par) {
  t1 <- par[[1]]; dims <- DIMS_ORD[sort(match(t1$d, DIMS_ORD))]
  paste0(paste(dims, collapse = "+"), "|", if (length(t1$d) == 2) as.integer(t1$l[1] != t1$l[2]) else NA)
}
reglas_tabla <- function(en) {                   # reglas que ven la tabla (detractor r5; congeladas 2026-10-27)
  ps <- en$pares; k <- which(names(ps) == "clave")
  cel <- lapply(ps, function(p) c(en$celdas_tipo(en$w, p[[1]]), en$celdas_tipo(en$w, p[[2]])))
  rp <- vapply(ps, function(p) any(en$celdas_tipo(en$w, p[[1]]) %in% en$celdas_tipo(en$w, p[[2]])), NA)
  molde <- vapply(ps, function(p) paste(sort(p[[1]]$d), collapse = "+"), "")
  gem <- vapply(seq_along(ps), function(j) any(rp[molde == molde[j] & seq_along(ps) != j]), NA)
  el <- function(drop) { q <- which(!drop); if (!length(q)) q <- seq_along(drop); if (k %in% q) 1 / length(q) else 0 }
  mx <- max(en$w); mn <- min(en$w); rg <- vapply(cel, function(x) diff(range(x)), 0)
  c(compuesta_repite_y_molde = el(rp | gem),
    fuera_5_20 = el(vapply(cel, function(x) any(x < 5 | x > 20), NA)),
    numero_mayor = el(vapply(cel, function(x) mx %in% x, NA)),
    mayor_o_menor = el(vapply(cel, function(x) any(c(mx, mn) %in% x), NA)),
    celdas_mas_juntas = { m <- which(rg == min(rg)); if (k %in% m) 1 / length(m) else 0 },
    ## Cadenas firmadas por alcance (detractor r6, H1): regla de repetición y luego la pareja más compacta
    repite_y_juntas = { q <- which(!rp); if (!length(q)) q <- seq_along(rp); m <- q[rg[q] == min(rg[q])]; if (k %in% m) 1 / length(m) else 0 },
    compuesta_y_juntas = { q <- which(!(rp | gem)); if (!length(q)) q <- seq_along(rp); m <- q[rg[q] == min(rg[q])]; if (k %in% m) 1 / length(m) else 0 })
}
acc_estimulo <- function(en) {                   # regla de estímulo (detractor r4, H1): ve la tabla
  rp <- vapply(en$pares, function(par) any(en$celdas_tipo(en$w, par[[1]]) %in% en$celdas_tipo(en$w, par[[2]])), NA)
  quedan <- names(rp)[!rp]; if (!length(quedan)) quedan <- names(rp)
  if ("clave" %in% quedan) 1 / length(quedan) else 0
}
pertenece <- function(env) {
  v <- env$v; err <- env$err_PQ
  a <- v[1, 1, 2]; b <- v[2, 1, 2]; cc <- v[1, 2, 1]; d <- v[2, 2, 1]
  e <- v[1, 1, 1]; f <- v[2, 1, 1]; g <- v[1, 2, 2]; h <- v[2, 2, 2]; s <- a + b
  if (!(a %in% R_RESTO) || !(b %in% R_RESTO) || s < 8 || s > 46 || cc + d != s) return(FALSE)
  if (!(cc %in% C_OK(a, b))) return(FALSE)
  eh <- EH(err[1], a, b, cc, d)
  if (!any(eh[, "e"] == e & eh[, "h"] == h)) return(FALSE)
  fg <- FG(err[2], b, cc, e, h, e - h)
  !is.null(fg) && any(fg[, "f"] == f & fg[, "g"] == g)
}
cuota <- setNames(integer(length(PARES_ERR)), names(por_estrato))
muestra <- list(); canon <- integer(0); s <- 0L; abortos <- 0L; fuera <- 0L
while (any(cuota < N_ESTANDAR) && s < TOPE) {
  s <- s + 1L; set.seed(s); env <- new.env()
  r <- tryCatch({ eval(EXPR, envir = env); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(r)) { abortos <- abortos + 1L; fallo("semilla ", s, " aborta: ", r); next }
  if (isTRUE(env$es_canonica)) { canon <- c(canon, s); next }
  k <- paste(env$err_PQ, collapse = ">")
  if (cuota[[k]] >= N_ESTANDAR) next
  cuota[[k]] <- cuota[[k]] + 1L
  if (!pertenece(env)) { fuera <- fuera + 1L; fallo("semilla ", s, ": el cubo generado no está en el espacio de E1") }
  op <- env$opciones; names(op) <- vapply(env$codigo_opcion, function(cod) meta_par(env$pares[[cod]]), "")
  muestra[[length(muestra) + 1]] <- list(semilla = s, estrato = k, opciones = op, clave = which(env$sol),
                                         molde_clave = sub("\\|.*$", "", meta_par(env$pares$clave)), nsol = sum(env$sol),
                                         acc_est = acc_estimulo(env), tab = reglas_tabla(env))
}
cat("  Semillas recorridas:", s, "| canónicas encontradas:", length(canon),
    sprintf("(%.1f %%)", 100 * length(canon) / s), "| abortos:", abortos, "\n")
print(cuota)
if (all(cuota >= N_ESTANDAR)) bien("cuotas completas: ", N_ESTANDAR, " por estrato") else fallo("cuotas incompletas")
## La mezcla de E4 usa P_CANON del generador: la proporción observada de canónicas debe ser compatible
## con ella, o el sorteo no la está usando (detractor r3, H3).
p_gen <- env$P_CANON
z_can <- (length(canon) - s * p_gen) / sqrt(s * p_gen * (1 - p_gen))
msg_can <- sprintf("proporción de canónicas %.1f %% frente a P_CANON = %.2f (z = %+.1f)", 100 * length(canon) / s, p_gen, z_can)
if (abs(z_can) <= 3) bien(msg_can) else fallo(msg_can, ": el sorteo no usa P_CANON")
if (fuera == 0) bien("los ", length(muestra), " cubos muestreados están en el espacio enumerado") else fallo(fuera, " cubos fuera del espacio")
if (all(vapply(muestra, `[[`, 0L, "nsol") == 1L)) bien("exactamente una clave en las ", length(muestra), " versiones") else fallo("versiones con nsol != 1")
cat("  §P4 · molde (dimensiones) de la clave en la muestra no canónica:\n")
print(table(vapply(muestra, `[[`, "", "molde_clave")))

## ---- E3 · Canónica contra literales de ITEM_OFICIAL.md ---------------------------
titulo("E3 · Instancia canónica contra ITEM_OFICIAL.md")
IMP_ENUNCIADO <- paste("Eduardo puso en una bolsa las fichas de un antiguo juego chino, en el que se usan fichas",
  "blancas y fichas negras de igual tamaño. Cada ficha tiene dibujada una figura geométrica y está clasificada en",
  "una categoría, par o impar. En la tabla se indica la cantidad de fichas de cada tipo que puso en la bolsa.")
IMP_PREGUNTA <- paste("Si se eligen al azar dos fichas de la bolsa, ¿cuáles de las siguientes fichas tienen la",
  "misma probabilidad de ser elegidas?")
IMP_OPC <- c(A = "Una ficha blanca con un círculo y una ficha negra con un cuadrado.",
             B = "Una ficha blanca con un cuadrado y una ficha negra con un círculo.",
             C = "Una ficha blanca de categoría par con un cuadrado y una ficha negra de categoría impar con un círculo.",
             D = "Una ficha blanca de categoría impar con un círculo y una ficha negra de categoría par con un cuadrado.")
IMP_TABLA <- rbind(c(12, 11), c(8, 15), c(16, 20), c(10, 8))   # filas: blanca par, blanca impar, negra par, negra impar
stopifnot(length(canon) > 0)
set.seed(canon[1]); envc <- new.env(); eval(EXPR, envir = envc)
chk <- function(cond, que) if (isTRUE(cond)) bien(que) else fallo("E3 ", que)
chk(identical(envc$enunciado, IMP_ENUNCIADO), "enunciado verbatim")
chk(identical(envc$pregunta, IMP_PREGUNTA), "pregunta verbatim")
chk(setequal(envc$opciones, IMP_OPC), "las cuatro opciones verbatim")
chk(identical(envc$respuesta_correcta, unname(IMP_OPC["B"])), "la clave es el contenido de B")
chk(isTRUE(all.equal(unname(envc$valores), IMP_TABLA)), "tabla celda a celda")
chk(identical(envc$lab, list(color = c("blanca", "negra"), categ = c("par", "impar"), figura = c("círculo", "cuadrado"))), "rótulos")
Nc <- envc$N; cu <- envc$cuentas
lect <- function(n) c(al_menos_una = 1 - choose(Nc - n, 2) / choose(Nc, 2), exactamente_una = n * (Nc - n) / choose(Nc, 2),
                      ambas = choose(n, 2) / choose(Nc, 2))
emp <- t(sapply(rownames(cu), function(r) abs(lect(cu[r, 1]) - lect(cu[r, 2])) < 1e-12))
print(cbind(cu, emp))
chk(all(emp["clave", ]) && !any(emp[rownames(emp) != "clave", ]), "solo la clave empata, con las tres lecturas")
chk(all(rowSums(cu) < Nc), "R2: ningún par complementario en la canónica")
## E3b · Solution: toda cifra n/N con «en cada extracción» o «una ficha extraída» (I-7; detractor r4, H3)
i_sol <- grep("^```\\{r solucion", lin)[1]; f_sol <- which(lin == "```" & seq_along(lin) > i_sol)[1]
EXPR_SOL <- parse(text = paste(lin[(i_sol + 1):(f_sol - 1)], collapse = "\n"))
sol_ok <- vapply(c(canon[1], muestra[[1]]$semilla, muestra[[length(muestra)]]$semilla), function(sd) {
  set.seed(sd); en <- new.env(); eval(EXPR, envir = en)
  txt <- paste(capture.output(eval(EXPR_SOL, envir = en)), collapse = " ")
  pos <- gregexpr("\\frac", txt, fixed = TRUE)[[1]]
  pos[1] > 0 && all(vapply(pos, function(p) grepl("extracci|extraída", substr(txt, max(1, p - 140), p)), NA))
}, NA)
chk(all(sol_ok), "Solution: cada n/N dice «en cada extracción» o «una ficha extraída» (canónica y 2 no canónicas)")

## ---- E4 · Batería §P7 ------------------------------------------------------------------
titulo("E4 · Batería §P7 congelada — muestra no canónica (estratos por err_PQ)")
OPS <- lapply(muestra, `[[`, "opciones"); CLV <- vapply(muestra, `[[`, 0L, "clave")
res <- evaluar_bateria(REGLAS, OPS, CLV, familias_no_aplicables = FAMILIAS_NA)
imprimir_bateria(res)
cat("\n  Por estrato (tasa de acierto de la regla; azar = 25 %):\n")
est <- vapply(muestra, `[[`, "", "estrato")
tab <- sapply(REGLAS, function(rg) tapply(seq_along(OPS), est, function(ii) mean(vapply(ii, function(i) p_acierto(rg, OPS[[i]], CLV[i]), 0))))
colnames(tab) <- vapply(REGLAS, `[[`, "", "nombre")
print(round(100 * t(tab), 1))
cat(sprintf("\n  Máximo por estrato: %.1f %% (%s, %s)\n", 100 * max(tab), colnames(tab)[which(tab == max(tab), arr.ind = TRUE)[1, 2]],
            rownames(tab)[which(tab == max(tab), arr.ind = TRUE)[1, 1]]))
titulo(paste0("E4 · Canónica, sobre sus ", length(canon), " semillas de E2 (residuo declarado: override firmado de §P7-E, I-8)"))
## El texto de la canónica es fijo, pero su orden interno cambia con la semilla: las reglas de
## posición se promedian sobre todas las canónicas, no sobre canon[1] (detractor FASE 2C
## 2026-10-27, H3: con una sola semilla, ultima_posicion daba +8,6 pp espurios en la mezcla).
ens_c <- lapply(canon, function(sd) { set.seed(sd); en <- new.env(); eval(EXPR, envir = en); en })
pc_sem <- vapply(ens_c, function(en) {
  op <- en$opciones; names(op) <- vapply(en$codigo_opcion, function(cod) meta_par(en$pares[[cod]]), "")
  vapply(REGLAS, function(rg) p_acierto(rg, op, which(en$sol)), 0)
}, numeric(length(REGLAS)))
pos_c <- vapply(ens_c, function(en) which(en$sol), 0L)
cat("  Posición interna de la clave en las canónicas (1..4):",
    paste(tabulate(pos_c, 4), collapse = " / "), "\n")
nom <- vapply(REGLAS, `[[`, "", "nombre")
pc  <- rowMeans(pc_sem)
pnc <- vapply(REGLAS, function(rg) mean(vapply(seq_along(OPS), function(i) p_acierto(rg, OPS[[i]], CLV[i]), 0)), 0)
p_can <- envc$P_CANON                            # probabilidad de la canónica, leída del generador
mezcla <- (1 - p_can) * pnc + p_can * pc
print(data.frame(no_canonica = sprintf("%.1f %%", 100 * pnc), canonica = sprintf("%.1f %%", 100 * pc),
                 exceso_canonica_pp = sprintf("%+.1f", 100 * (pc - 0.25)), mezcla_10pct = sprintf("%.1f %%", 100 * mezcla),
                 exceso_mezcla_pp = sprintf("%+.1f", 100 * (mezcla - 0.25)), row.names = nom))
cat("  Largos de las opciones canónicas:", paste(names(IMP_OPC), nchar(IMP_OPC), collapse = " · "), "\n")
cat("  Las reglas de posición miden el orden INTERNO: R/exams (exshuffle: TRUE) y Moodle vuelven a mezclar.\n")

## ---- E4b · Canal de estímulo (fuera de la batería: la regla ve la tabla) ----------------
titulo("E4b · Canal de estímulo: «descarta la pareja cuyos dos tipos repiten un valor de celda»")
acc_nc <- mean(vapply(muestra, `[[`, 0, "acc_est")); acc_c <- mean(vapply(ens_c, acc_estimulo, 0))
mez_est <- (1 - p_can) * acc_nc + p_can * acc_c
cat(sprintf("  no canónica: %.1f %% (%+.1f pp) | canónica: %.1f %% (%+.1f pp) | mezcla: %.1f %% (%+.1f pp)\n",
            100 * acc_nc, 100 * (acc_nc - .25), 100 * acc_c, 100 * (acc_c - .25), 100 * mez_est, 100 * (mez_est - .25)))
cat("  Residuo estructural firmado (2026-10-27): 1/3, el nivel del ítem oficial con esta regla.\n")
if (acc_nc <= 1/3 + 1e-9 && acc_c <= 1/3 + 1e-9) bien("canal de estímulo dentro de la firma (<= 1/3)") else fallo("canal de estímulo por encima de 1/3")

## ---- E4c · Reglas que ven la tabla (detractor r5; congeladas el 2026-10-27) -----------------------
titulo("E4c · Reglas que ven la tabla (N = 100 por estrato; el ítem oficial, aparte)")
TB <- t(vapply(muestra, `[[`, numeric(7), "tab")); TC <- t(vapply(ens_c, reglas_tabla, numeric(7)))
## Cifras firmadas o declaradas (CLAUDE.md local, 2026-10-27; exactas por depuración M = 50.000 del detractor r6).
## Una regla con cifra firmada falla si la muestra la supera en más de 3 errores típicos (detractor r6, H2).
FIRMADAS <- c(compuesta_repite_y_molde = 1/2, celdas_mas_juntas = 0.326, repite_y_juntas = 0.476, compuesta_y_juntas = 0.648)
for (rg in colnames(TB)) {
  x <- TB[, rg]; ex_nc <- mean(x) - .25; se <- sd(x) / sqrt(length(x)); ex_c <- mean(TC[, rg]) - .25
  mez <- (1 - p_can) * mean(x) + p_can * mean(TC[, rg]) - .25
  ver <- if (rg %in% names(FIRMADAS)) {
           if (mean(x) <= FIRMADAS[[rg]] + 3 * se + 1e-9) sprintf("firmada (<= %.1f %%)", 100 * FIRMADAS[[rg]]) else "POR ENCIMA DE LA FIRMA"
         } else if (ex_nc - 2 * se > .08) "CANAL" else if (ex_nc + 2 * se >= .08) "zona del corte (no concluyente)" else "ok"
  cat(sprintf("  %-26s no canónica %5.1f %% (%+5.1f pp ± %.1f) | oficial %+6.1f pp | mezcla %+5.1f pp | %s\n",
              rg, 100 * mean(x), 100 * ex_nc, 200 * se, 100 * ex_c, 100 * mez, ver))
  if (ver %in% c("CANAL", "POR ENCIMA DE LA FIRMA")) fallo("E4c ", rg, ": ", ver)
}
cat("  Firmas por alcance del 2026-10-27: compuesta (1/2) y cadenas de repetición + celdas más juntas; «celdas más\n",
    "  juntas» sola, declarada (+7,6 pp exactos). En el ítem oficial las tres dan 0 %.\n", sep = "")

## ---- E5 · Mutantes con sonda declarada --------------------------------------------------
titulo("E5 · Mutantes (sonda declarada, en memoria sobre el chunk; patrón exactamente una vez)")
MUT <- list(
  list(id = "M1", inv = "I-5 igualdad de la clave construida", patron = "cc <- pick(c_ok); d <- s - cc",
       nuevo = "cc <- pick(c_ok); d <- s - cc + 1L", sonda = c("sum(iguales) == 1", "iguales[[\"clave\"]]"),
       entorno = function(v) !is.null(v$v) && v$v[1, 1, 2] + v$v[2, 1, 2] != v$v[1, 2, 1] + v$v[2, 2, 1]),
  list(id = "M2", inv = "I-5 exclusión que deja desigual el espejo",
       patron = "f <- cc; g <- pick(setdiff(r_resto, c(b, cc + m, cc, e)))", nuevo = "f <- cc; g <- pick(setdiff(r_resto, c(b, cc, e)))",
       sonda = "sum(iguales) == 1", entorno = function(v) !is.null(v$v) && v$v[1, 1, 1] + v$v[2, 1, 1] == v$v[1, 2, 2] + v$v[2, 2, 2]),
  list(id = "M3", inv = "I-7 R2 (ningún par complementario)", patron = "P      = list(tp(c(1, 2), c(1, 1)), tp(c(1, 2), c(2, 2))),",
       nuevo = "P      = list(tp(1, 1), tp(1, 2)),", sonda = "rowSums(cuentas) < N",
       entorno = function(v) !is.null(v$cuentas) && any(rowSums(v$cuentas) == v$N)),
  list(id = "M4", inv = "I-7 celdas >= 1", patron = "r_resto <- 3:24", nuevo = "r_resto <- 0:24", sonda = "all(w >= 1)",
       entorno = function(v) !is.null(v$w) && any(v$w < 1)),
  list(id = "M5", inv = "I-1 sin vocabulario de error en las opciones", patron = "\"y\", txt_tipo(par[[2]], lab))), \".\")",
       nuevo = "\"y\", txt_tipo(par[[2]], lab))), \", por error.\")", sonda = "estudiante|error|confundi",
       entorno = function(v) !is.null(v$opciones_base) && any(grepl("por error", v$opciones_base, fixed = TRUE))),
  list(id = "M6", inv = "I-2 tabla canónica = impreso", patron = "CANON_W <- array(c(12, 16, 8, 10,",
       nuevo = "CANON_W <- array(c(13, 16, 7, 10,", sonda = "as.vector(w)",
       entorno = function(v) isTRUE(v$es_canonica) && v$w[1, 1, 1] == 13),
  list(id = "M7", inv = "I-2 enunciado canónico = impreso", patron = "fichas negras de igual tamaño.",
       nuevo = "fichas negras del mismo tamaño.", sonda = "utf8ToInt(enunciado)",
       entorno = function(v) isTRUE(v$es_canonica) && grepl("del mismo tamaño", v$enunciado, fixed = TRUE)),
  list(id = "M8", inv = "I-6 pool de errores >= 4",
       patron = "names(errores_conceptuales) <- vapply(errores_conceptuales, function(e) e$codigo, character(1))",
       nuevo = "names(errores_conceptuales) <- vapply(errores_conceptuales, function(e) e$codigo, character(1))\nerrores_conceptuales <- errores_conceptuales[1:3]",
       sonda = "length(errores_conceptuales) >= 4", entorno = function(v) length(v$errores_conceptuales) == 3),
  list(id = "M9", inv = "I-8 toda opción agregada fuera de la canónica",
       patron = "espejo = list(tp(c(2, 3), c(1, 1)), tp(c(2, 3), c(2, 2))),",
       nuevo = "espejo = list(tp(c(1, 2, 3), c(1, 1, 2)), tp(c(1, 2, 3), c(2, 2, 1))),", sonda = "vapply(pares",
       entorno = function(v) !is.null(v$pares) && any(vapply(v$pares, function(p) length(p[[1]]$d) == 3L, NA))),
  ## M11-M17: detractores FASE 2C del 2026-10-27 y decisiones del profesor (WAIT_USER #3).
  list(id = "M11", inv = "I-2 enunciado canónico sin permutar palabras",
       patron = "se usan fichas blancas y\",\n  \"fichas negras de igual",
       nuevo = "se usan fichas negras y\",\n  \"fichas blancas de igual", sonda = "seq_along(u_enun)",
       entorno = function(v) isTRUE(v$es_canonica) && grepl("fichas negras y fichas blancas", v$enunciado, fixed = TRUE)),
  list(id = "M12", inv = "I-6 precondición del error elegido",
       patron = "abs(sum(x$c1) - sum(x$c2)) == 1", nuevo = "abs(sum(x$c1) - sum(x$c2)) == 2",
       sonda = "precondicion", entorno = function(v) "PROB-CER" %in% v$codigos_sel),
  list(id = "M13", inv = "I-6 C canónica con PROB-CER: textos genéricos válidos para una celda",
       patron = "vale_celda_suelta = TRUE,", nuevo = "vale_celda_suelta = FALSE,",
       sonda = "textos genéricos válidos o los cuatro campos",
       entorno = function(v) isTRUE(v$es_canonica) && !isTRUE(v$errores_conceptuales[["PROB-CER"]]$vale_celda_suelta)),
  list(id = "M14", inv = "Solution canónica de D sin sumas (explicación)",
       patron = "no hace iguales sus valores.", nuevo = "no hace iguales sus valores, aunque no se sumaron las categorías.",
       sonda = "análisis de celdas sueltas sin sumas", entorno = function(v) isTRUE(v$es_canonica)),
  list(id = "M15", inv = "I-7 pregunta del impreso sin permutar palabras",
       patron = "\"Si se eligen al azar dos fichas", nuevo = "\"Si al azar se eligen dos fichas",
       sonda = "seq_along(u_preg)", entorno = function(v) grepl("Si al azar se eligen", v$pregunta, fixed = TRUE)),
  list(id = "M16", inv = "I-8 override del residuo canónico: P_CANON <= 0,10",
       patron = "P_CANON <- 0.10", nuevo = "P_CANON <- 0.15",
       sonda = "P_CANON <= 0.1", entorno = function(v) isTRUE(v$P_CANON == 0.15)),   # R escribe 0.10 como 0.1
  list(id = "M17", inv = "I-6 C canónica no vuelve a PROB-UNA",
       patron = "\"PROB-CER\" = CANON_PARES$C", nuevo = "\"PROB-UNA\" = CANON_PARES$C",
       sonda = "precondicion", entorno = function(v) isTRUE(v$es_canonica) && "PROB-UNA" %in% names(v$pares)),
  ## M18-M19: los mutantes X1 y X2 del detractor r3 (H2), que sobrevivían a la guarda léxica.
  list(id = "M18", inv = "C canónica (PROB-CER) sin vocabulario de agregación",
       patron = "\"son exactamente iguales; una ficha de diferencia ya las hace distintas.\"",
       nuevo = "\"son exactamente iguales; una ficha de diferencia ya las hace distintas; cada tipo reúne dos celdas.\"",
       sonda = "análisis de celdas sueltas sin sumas",
       entorno = function(v) isTRUE(v$es_canonica) &&
         grepl("cada tipo reúne dos celdas", v$errores_conceptuales[["PROB-CER"]]$descripcion_larga, fixed = TRUE)),
  list(id = "M19", inv = "C canónica (PROB-CER) sin la causa raíz de dos celdas",
       patron = "causa_raiz = \"Confunde «casi iguales» con «iguales».\"",
       nuevo = "causa_raiz = \"No reconoce que un tipo de ficha puede reunir varias celdas de la tabla.\"",
       sonda = "análisis de celdas sueltas sin sumas",
       entorno = function(v) isTRUE(v$es_canonica) &&
         grepl("reunir varias celdas", v$errores_conceptuales[["PROB-CER"]]$causa_raiz, fixed = TRUE)))
## M10a-d: quitar de TXT_CANON cualquiera de los cuatro campos visibles de D (PROB-CRU) devolvería
## un texto genérico de dos celdas, falso en la canónica (H1 de r2: antes solo se probaba uno).
M10 <- lapply(list(c("a", "nombre", "nombre = \"Toma las celdas"),
                   c("b", "descripcion_corta", "descripcion_corta = paste(\"Ve que"),
                   c("c", "descripcion_larga", "descripcion_larga = paste(\"Elegir una pareja"),
                   c("d", "causa_raiz", "causa_raiz = \"Elige por la posición")), function(z) {
  campo_m <- z[2]
  list(id = paste0("M10", z[1]), inv = paste("Solution canónica de D: override de", campo_m),
       patron = z[3], nuevo = sub(campo_m, paste0(campo_m, "_x"), z[3], fixed = TRUE),
       sonda = "textos genéricos válidos o los cuatro campos",
       entorno = function(v) isTRUE(v$es_canonica) && is.null(v$TXT_CANON[["PROB-CRU"]][[campo_m]]))
})
MUT <- c(MUT, M10)
## M20-M22: reapertura 7 (detractor r4, H1 y H3; decisión del profesor del 2026-10-27).
MUT <- c(MUT, list(
  list(id = "M20", inv = "I-7 «en cada extracción» en el feedback",
       patron = "fichas: en cada extracción, los dos tienen la misma ", nuevo = "fichas: los dos tienen la misma ",
       sonda = "n/N con «en cada extracción»",
       entorno = function(v) any(grepl("frac", v$feedback, fixed = TRUE)) && !any(grepl("extracci", v$feedback))),
  list(id = "M21", inv = "Canal de estímulo: el espejo no repite celdas",
       patron = "c(cc, cc - delta, h, e - k)", nuevo = "c(cc, cc - delta, e - k)",
       sonda = "solo PROB-UNA repite un valor de celda",
       entorno = function(v) !isTRUE(v$es_canonica) && !is.null(v$v) &&
         any(c(v$v[1, 1, 1], v$v[2, 1, 1]) %in% c(v$v[1, 2, 2], v$v[2, 2, 2]))),
  list(id = "M22", inv = "I-6 PROB-CRU solo en la canónica",
       patron = "c(\"PROB-SIM\", \"PROB-CRU\"))", nuevo = "\"PROB-SIM\")",
       sonda = c("precondicion", "solo PROB-UNA repite un valor de celda"),
       entorno = function(v) "PROB-CRU" %in% v$err_PQ)))
tab_mut <- NULL
for (mu in MUT) {
  n_pat <- lengths(regmatches(CODIGO, gregexpr(mu$patron, CODIGO, fixed = TRUE)))
  if (n_pat != 1L) { fallo(mu$id, ": el patrón aparece ", n_pat, " veces (debe ser 1)"); next }
  ex <- parse(text = sub(mu$patron, mu$nuevo, CODIGO, fixed = TRUE))
  muerte <- NA; msg <- ""; env_m <- NULL
  for (sd in seq_len(TOPE)) {
    set.seed(sd); env_m <- new.env()
    r <- tryCatch({ eval(ex, envir = env_m); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(r)) { muerte <- sd; msg <- r; break }
  }
  if (is.na(muerte)) {
    ver <- "no_detectado"; fallo(mu$id, " (", mu$inv, ") sobrevive ", TOPE, " semillas")
  } else if (!isTRUE(mu$entorno(env_m))) {
    ver <- "mal_construido"; fallo(mu$id, ": MUTANTE MAL CONSTRUIDO, la mutación no llegó al entorno")
  } else if (any(vapply(mu$sonda, function(z) grepl(z, msg, fixed = TRUE), NA))) {
    ver <- "cazado_por_su_sonda"
  } else {
    ver <- "cazado_por_otra"; fallo(mu$id, ": MUTANTE CAZADO POR LA SONDA EQUIVOCADA: ", msg)
  }
  tab_mut <- rbind(tab_mut, data.frame(id = mu$id, invariante = mu$inv, sonda = mu$sonda[1], semilla = muerte,
                                       veredicto = ver, mensaje = substr(gsub("\\s+", " ", msg), 1, 70)))
}
print(tab_mut, row.names = FALSE, right = FALSE)
cat("\n  Sin prueba de detección propia: I-3 (Nivel/DOK: metadatos, se cotejan en el paso 10);",
    "I-4 (misma bolsa: estructural, todo n_tipo() sale de la misma tabla w); opciones únicas (estructural:",
    "las cuatro parejas son distintas y los rótulos de cada dimensión también).\n")

titulo("RESULTADO")
if (length(errores)) {
  cat("RESULTADO: FALLIDO (", length(errores), " errores)\n", sep = ""); for (e in errores) cat(" -", e, "\n"); quit(status = 1)
} else cat("RESULTADO: APROBADO (0 errores)\n")
