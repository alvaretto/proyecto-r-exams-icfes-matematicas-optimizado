## =============================================================================
## bateria_partes_2_a_6.R — L-C4: batería CONGELADA por gap de las Partes 2 a 6 del CLOZE
## Tanda 4 (2026-10-31), decisión del profesor «1 a · 2 a · i ii iii» (../.claude/CLAUDE.md § «Decisión del profesor
## tras la tanda 3»). Regla #22 §P7 (seis familias, techo nulo, exceso), §P7-C (se congela ANTES de medir el reparto),
## §P7-E (al menos una regla relacional). Se congela por su md5, anotado en cloze/HANDOFF.md y comprobado por
## verificar_espacio_cloze.R (E8). Cambiar una regla = batería nueva: hay que volver a medir todo el histórico.
##
## Solo define reglas y funciones de medida. No lee el .Rmd, no escribe nada y no usa el azar salvo en el techo nulo
## por permutación (techo_p2(), techo_p6()), que guarda y restaura .Random.seed.
##
## REGISTRO. Cada versión entra como x = list(p2 = ..., p6 = ...) (registro_bateria(), desde el entorno del .Rmd):
##   p2: op (4 opciones, en el orden mostrado), k (índice de la clave), tipo, preg (texto de la Parte 2), otros (textos
##       de las Partes 3 a 5), enc (encabezados de la tabla del otro colegio).
##   p6: op (4 afirmaciones, en el orden mostrado), v (veredictos), rol (hp, hg, ventaja, ganador), n y a (cursos y
##       promedio por fila de la tabla MOSTRADA), nombres (grado de cada fila).
##
## PUNTUACIÓN Y NULO (declarados, como pide la tanda 4).
##   - Parte 2 (schoice, 4 opciones). Cada regla devuelve el conjunto superviviente S. Puntuación §P7 (exigencia 4):
##     1/|S| si la clave sobrevive, 0 si no, 1/4 si S es vacío o son las cuatro. Con la clave uniforme sobre las
##     posiciones, E[puntuación] = 1/4 EXACTAMENTE para toda regla: el exceso de una regla es acierto − 25 %. El techo
##     nulo de la batería (máximo esperado sobre todas las reglas sin canal) se calcula moviendo la clave a una posición
##     uniforme y dejando las reglas intactas (§P7 exigencia 2), con B = 200 réplicas.
##   - Parte 6 (mchoice, 4 afirmaciones), POR AFIRMACIÓN. Cada regla devuelve un veredicto predicho por afirmación
##     (TRUE, FALSE o NA = se abstiene). Puntuación por afirmación: 1 si acierta, 0 si falla, 1/2 si se abstiene. Se mide
##     por papel (hp, hg, ventaja, ganador). NULO EXACTO por permutación: si el veredicto de ese papel se permutara entre
##     las versiones (misma frecuencia de verdaderas q, sin relación con la tabla), una regla que predice «verdadera» con
##     frecuencia p acierta p·q + (1 − p)·(1 − q) (+ 1/2 por cada abstención). Exceso = acierto − ese nulo; así una regla
##     constante («HG siempre verdadera») no cuenta como canal aunque la frecuencia se aparte del 50 % (eso lo vigila E3,
##     50 % ± 12). El techo de la batería (máximo sobre reglas) se calcula permutando de verdad el veredicto, B = 200.
##   - Una regla y su contraria van las dos: el estudiante aprende de las versiones en qué sentido funciona.
##   - Umbral (§P7-A): exceso > +8 pp obliga; de +5,3 a +8 pp, zona gris declarada; ≤ +5,3 pp, aceptable.
##
## RELACIONALES DE LA PARTE 6, con su métrica propia (no son reglas por afirmación):
##   - RL1 «HP y HG con el mismo veredicto»: estrategia «adivina una y copia la otra». Métrica conjunta del par:
##     acierta las dos con probabilidad 1/2 · 1[HP = HG]; nulo: las dos independientes con sus frecuencias. Es el costo
##     que el profesor aceptó con k′ = 1 y pidió medir.
##   - RL2 «las hipótesis llevan el veredicto contrario al de la afirmación del ganador» (por afirmación, hp y hg).
##     CONDICIONAL: presupone que el estudiante ya sabe qué grado ganó (lo calculan las Partes 3 a 5) y se salta la
##     comparación del promedio con la diferencia. Nace de L-C1 («si las hipótesis coinciden, las otras dos llevan el
##     contrario»). Se mide y se declara aparte: no es «sin razonar», sino un atajo que evita el razonamiento propio de
##     las hipótesis.
##   - RL3 «la de ventaja y la del ganador con el mismo veredicto»: equivalencia lógica dada la tabla (L-C1, decisión
##     1 a); reconocerla es razonar. Se imprime su cifra, no entra en la decisión.
##
## FAMILIAS (§P7 exigencia 1).
##   Parte 2: léxico, formato, posición, relacional y estímulo con sonda. Magnitud, divisibilidad y signo NO APLICAN:
##   las opciones de la Parte 2 no llevan cifras (V11 prohíbe cifras en los textos de las Partes 2 a 6); el largo va
##   en formato.
##   Parte 6: magnitud (k′, δ′, cursos y promedios de la tabla), divisibilidad (paridad de los promedios), posición
##   (fila de la tabla y orden de las afirmaciones), formato (largo), léxico, relacional y estímulo con sonda. Signo NO
##   APLICA: todas las magnitudes de la tabla son positivas y las afirmaciones no llevan cifras.
##
## SONDAS QUE TIENEN QUE ESTAR (tanda 4): H1 de la FASE 2C r1 en la Parte 2 (P2-L1 «contiene “total”», P2-L2 «descarta
## “Lo que recogió …”», P2-L3 condicional en -ría); H2 en la Parte 6 (P6-K1 R_k, P6-HK1 R_hg, P6-D1 R_δ). R_δ (δ′ ≤ 4)
## deja de aplicar si el reparto exige δ′ ≥ 5: se declara su aplicabilidad (Error 37), y la familia sigue cubierta por
## P6-D2 (δ′ ≥ 10).
##
## PARTES 3 A 5 (num). Ninguna regla de eliminación aplica: no hay opciones y la tolerancia es 0 (adivinar un total
## exacto de dos o tres cifras no tiene probabilidad apreciable). Las vías de fuga de un gap numérico son que la
## respuesta esté impresa en otra parte o a una operación de lo que otra parte muestra: las mide V11 por valor y «a un
## paso» en E3 de verificar_espacio_cloze.R. Que la respuesta salga a una operación de la tabla del otro colegio
## (n′ · a′, T′_G − T′_P) es el cálculo que se pide, no un canal.
##
## CONTROL POSITIVO: P6-CTL calcula los totales (la vía legítima) y tiene que acertar el 100 % de las afirmaciones;
## si no, el lector de afirmaciones o la tabla leída están rotos y la batería no mide nada. No entra en la decisión.
## =============================================================================
BATERIA_VERSION <- "L-C4 v1 (2026-10-31)"
UMBRAL_OBLIGA <- 0.08
UMBRAL_GRIS <- 0.053

.tk <- function(s) unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]]))
.comun <- function(op, ref) { r <- .tk(paste(ref, collapse = " ")); k <- vapply(op, function(s) length(intersect(.tk(s), r)), 0L); k == max(k) }
.sc_op <- function(S, k) { S <- as.logical(S); n <- length(S); if (!any(S) || all(S)) 1 / n else if (S[k]) 1 / sum(S) else 0 }

## ---- Parte 2 ----------------------------------------------------------------------------------------------------
.pares <- function(op) { T <- lapply(op, .tk); n <- length(op); M <- matrix(0L, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) if (i != j) M[i, j] <- length(intersect(T[[i]], T[[j]])); M }
REGLAS_P2 <- list(
  list(id = "P2-L1", familia = "lexico", nombre = "contiene «total» (H1)", fn = function(x) grepl("(*UCP)\\btotal\\b", x$op, perl = TRUE, ignore.case = TRUE)),
  list(id = "P2-L2", familia = "lexico", nombre = "descarta las que empiezan por «Lo que recogió» (H1)", fn = function(x) !startsWith(x$op, "Lo que recogió")),
  list(id = "P2-L3", familia = "lexico", nombre = "elige el condicional en -ría (H1)", fn = function(x) grepl("(*UCP)\\b\\w+(aría|ería|iría)\\b", x$op, perl = TRUE)),
  list(id = "P2-L4", familia = "lexico", nombre = "descarta las que empiezan por «Únicamente»", fn = function(x) !startsWith(x$op, "Únicamente")),
  list(id = "P2-L5", familia = "lexico", nombre = "contiene «promedio»", fn = function(x) grepl("promedio", x$op, fixed = TRUE)),
  list(id = "P2-L6", familia = "lexico", nombre = "descarta las absolutas («únicamente», «sin tener en cuenta», «exactamente»)",
       fn = function(x) !grepl("[Úú]nicamente|sin tener en cuenta|exactamente", x$op)),
  list(id = "P2-F1", familia = "formato", nombre = "la más larga", fn = function(x) { n <- nchar(x$op); n == max(n) }),
  list(id = "P2-F2", familia = "formato", nombre = "la más corta", fn = function(x) { n <- nchar(x$op); n == min(n) }),
  list(id = "P2-F3", familia = "formato", nombre = "descarta la más larga", fn = function(x) { n <- nchar(x$op); n != max(n) }),
  list(id = "P2-F4", familia = "formato", nombre = "la que lleva coma", fn = function(x) grepl(",", x$op, fixed = TRUE)),
  list(id = "P2-F5", familia = "formato", nombre = "descarta las que llevan coma", fn = function(x) !grepl(",", x$op, fixed = TRUE)),
  list(id = "P2-P1", familia = "posicion", nombre = "la primera", fn = function(x) seq_along(x$op) == 1L),
  list(id = "P2-P2", familia = "posicion", nombre = "la última", fn = function(x) seq_along(x$op) == length(x$op)),
  list(id = "P2-R1", familia = "relacional", nombre = "par con más palabras en común (elige una del par)",
       fn = function(x) { M <- .pares(x$op); i <- which(apply(M == max(M), 1, any)); seq_along(x$op) %in% i }),
  list(id = "P2-R2", familia = "relacional", nombre = "la que más palabras comparte con las otras tres",
       fn = function(x) { s <- rowSums(.pares(x$op)); s == max(s) }),
  list(id = "P2-R3", familia = "relacional", nombre = "descarta la que menos palabras comparte con las otras tres",
       fn = function(x) { s <- rowSums(.pares(x$op)); s != min(s) }),
  list(id = "P2-E1", familia = "estimulo", nombre = "más palabras en común con su pregunta", fn = function(x) .comun(x$op, x$preg)),
  list(id = "P2-E2", familia = "estimulo", nombre = "más palabras en común con las Partes 3 a 5", fn = function(x) .comun(x$op, x$otros)),
  list(id = "P2-E3", familia = "estimulo", nombre = "nombra los dos encabezados de la tabla (cursos y promedio)",
       fn = function(x) grepl("curso", x$op, fixed = TRUE) & grepl("promedio", x$op, fixed = TRUE)),
  list(id = "P2-E4", familia = "estimulo", nombre = "la única con una palabra de las Partes 3 a 5 que no tiene otra opción",
       fn = function(x) { T <- lapply(x$op, .tk); o <- .tk(paste(x$otros, collapse = " "))
         e <- vapply(seq_along(T), function(i) length(setdiff(intersect(T[[i]], o), unlist(T[-i]))), 0L)
         if (max(e) == 0L) rep(TRUE, length(T)) else e == max(e) }))
FAMILIAS_NA_P2 <- c(magnitud = "las opciones de la Parte 2 no llevan cifras (V11); el largo va en formato",
                    divisibilidad = "las opciones de la Parte 2 no llevan cifras (V11)",
                    signo = "las opciones de la Parte 2 no llevan cifras (V11)")

## ---- Parte 6 ----------------------------------------------------------------------------------------------------
## Lo que el estudiante lee en cada afirmación (no el papel declarado por el código).
tipo6 <- function(s) {
  if (startsWith(s, "Ganó el concurso el grado ")) return("ganador")
  if (grepl("con un curso más,", s, fixed = TRUE)) return("hp")
  if (grepl("le hubiera faltado un curso,", s, fixed = TRUE)) return("hg")
  perdio <- grepl("^Si (el|al) grado que perdió", s)
  if (grepl("con tantos cursos como el que", s, fixed = TRUE)) return(if (perdio) "bc_perdio" else "bc_gano")
  if (grepl("el mismo promedio por curso que el que", s, fixed = TRUE)) return(if (perdio) "bp_perdio" else "bp_gano")
  "otro"
}
## Rasgos de la tabla mostrada: hi = fila del grado de más cursos (que, sin dominancia, tiene el menor promedio).
rasgos6 <- function(p) { hi <- which.max(p$n); lo <- 3L - hi
  list(hi = hi, lo = lo, k = p$n[hi] - p$n[lo], delta = p$a[lo] - p$a[hi], n_lo = p$n[lo], n_hi = p$n[hi], a_lo = p$a[hi], a_hi = p$a[lo]) }
## Predicción desde una conjetura sobre quién ganó: pc = TRUE si «ganó el grado de más cursos» (NA = se abstiene).
## «Ganó el grado X» y las de ventaja quedan decididas por pc; las hipótesis, no (se abstienen).
pred_rama <- function(p, pc) {
  r <- rasgos6(p); t <- vapply(p$op, tipo6, "")
  vapply(seq_along(t), function(i) {
    if (is.na(pc)) return(NA)
    switch(t[i], ganador = (sub("^Ganó el concurso el grado (.*)\\.$", "\\1", p$op[i]) == p$nombres[r$hi]) == pc,
           bc_perdio = pc, bc_gano = pc, bp_perdio = !pc, bp_gano = !pc, NA)
  }, NA)
}
## Predicción para las hipótesis de un curso (hp, hg); las otras se abstienen.
pred_hip <- function(p, val, cuales = c("hp", "hg")) { t <- vapply(p$op, tipo6, ""); ifelse(t %in% cuales, val, NA) }
## Una regla y su contraria.
.par6 <- function(id, familia, nombre, nombre_c, fn) list(
  list(id = id, familia = familia, nombre = nombre, fn = fn),
  list(id = paste0(id, "c"), familia = familia, nombre = nombre_c, fn = function(p) !fn(p)))
.estim <- function(f) function(p) { r <- rasgos6(p); f(r, p) }
REGLAS_P6 <- c(
  ## magnitud (estímulo): quién ganó
  .par6("P6-K1", "magnitud", "R_k (H2): k′ ≥ 2 ⇒ ganó el de más cursos; k′ = 1 ⇒ el de mayor promedio", "contraria de R_k",
        .estim(function(r, p) pred_rama(p, r$k >= 2))),
  .par6("P6-D1", "magnitud", "R_δ (H2): δ′ ≤ 4 ⇒ ganó el de más cursos (si no, se abstiene)", "contraria de R_δ: δ′ ≤ 4 ⇒ el de mayor promedio",
        .estim(function(r, p) pred_rama(p, if (r$delta <= 4) TRUE else NA))),
  .par6("P6-D2", "magnitud", "δ′ ≥ 10 ⇒ ganó el de mayor promedio; δ′ < 10 ⇒ el de más cursos", "contraria de δ′ ≥ 10",
        .estim(function(r, p) pred_rama(p, r$delta < 10))),
  .par6("P6-N1", "magnitud", "el grado de menos cursos tiene 5 o más ⇒ ganó el de mayor promedio", "contraria de «5 o más cursos»",
        .estim(function(r, p) pred_rama(p, r$n_lo < 5))),
  .par6("P6-A1", "magnitud", "el menor promedio es de 40 kilos o más ⇒ ganó el de más cursos", "contraria de «promedio de 40 o más»",
        .estim(function(r, p) pred_rama(p, r$a_lo >= 40))),
  ## magnitud (estímulo): las hipótesis de un curso
  .par6("P6-HK1", "magnitud", "R_hg (H2): HG verdadera ⟺ k′ = 1", "contraria de R_hg",
        .estim(function(r, p) pred_hip(p, r$k == 1, "hg"))),
  .par6("P6-HK2", "magnitud", "R_hp: HP verdadera ⟺ k′ = 1", "contraria de R_hp",
        .estim(function(r, p) pred_hip(p, r$k == 1, "hp"))),
  .par6("P6-HD", "magnitud", "las hipótesis de un curso son verdaderas ⟺ δ′ ≥ 10", "contraria (⟺ δ′ < 10)",
        .estim(function(r, p) pred_hip(p, r$delta >= 10))),
  .par6("P6-HN", "magnitud", "las hipótesis de un curso son verdaderas ⟺ el grado de menos cursos tiene 5 o más", "contraria (⟺ menos de 5)",
        .estim(function(r, p) pred_hip(p, r$n_lo >= 5))),
  .par6("P6-HA", "magnitud", "las hipótesis de un curso son verdaderas ⟺ el menor promedio es de 40 o más", "contraria (⟺ menos de 40)",
        .estim(function(r, p) pred_hip(p, r$a_lo >= 40))),
  ## divisibilidad
  .par6("P6-V1", "divisibilidad", "ganó el grado de promedio par (si solo uno lo es)", "contraria: ganó el de promedio impar",
        .estim(function(r, p) pred_rama(p, if (xor(r$a_lo %% 2 == 0, r$a_hi %% 2 == 0)) r$a_lo %% 2 == 0 else NA))),
  ## posición: fila de la tabla y orden de las afirmaciones
  .par6("P6-P1", "posicion", "ganó el grado de la primera fila", "contraria: ganó el de la segunda fila",
        .estim(function(r, p) pred_rama(p, r$hi == 1L))),
  .par6("P6-P2", "posicion", "las dos primeras afirmaciones son verdaderas y las dos últimas falsas", "contraria del orden",
        function(p) seq_along(p$op) <= 2L),
  ## formato
  .par6("P6-F1", "formato", "la afirmación más corta es verdadera", "la afirmación más corta es falsa",
        function(p) { n <- nchar(p$op); ifelse(n == min(n), TRUE, NA) }),
  .par6("P6-F2", "formato", "la afirmación más larga es verdadera", "la afirmación más larga es falsa",
        function(p) { n <- nchar(p$op); ifelse(n == max(n), TRUE, NA) }),
  ## léxico
  .par6("P6-L1", "lexico", "las que terminan en «habría ganado» son verdaderas; las de «habría perdido», falsas", "contraria",
        function(p) ifelse(grepl("habría ganado el concurso\\.$", p$op), TRUE, ifelse(grepl("habría perdido el concurso\\.$", p$op), FALSE, NA))),
  .par6("P6-L2", "lexico", "las que hablan del grado que perdió son verdaderas; las del que ganó, falsas", "contraria",
        function(p) ifelse(grepl("^Si (el|al) grado que perdió", p$op), TRUE, ifelse(grepl("^Si (el|al) grado que ganó", p$op), FALSE, NA))))
FAMILIAS_NA_P6 <- c(signo = "todas las magnitudes de la tabla son positivas y las afirmaciones no llevan cifras")
## Control positivo (no entra en la decisión): calcula los totales y evalúa cada afirmación.
CONTROL_P6 <- function(p) {
  r <- rasgos6(p); tt <- p$n * p$a; G <- which.max(tt); P <- 3L - G; t <- vapply(p$op, tipo6, ""); n <- p$n; a <- p$a
  vapply(seq_along(t), function(i) switch(t[i],
    ganador = sub("^Ganó el concurso el grado (.*)\\.$", "\\1", p$op[i]) == p$nombres[G],
    hp = (n[P] + 1) * a[P] > tt[G], hg = (n[G] - 1) * a[G] < tt[P],
    bc_perdio = n[G] * a[P] > tt[G], bc_gano = n[P] * a[G] < tt[P],
    bp_perdio = n[P] * a[G] > tt[G], bp_gano = n[G] * a[P] < tt[P], NA), NA)
}

## ---- Medida -----------------------------------------------------------------------------------------------------
## L: lista de registros; w: pesos (por defecto iguales; la enumeración exacta los trae).
.w <- function(L, w) { if (is.null(w)) w <- rep(1, length(L)); w / sum(w) }
medir_p2 <- function(L, w = NULL) {
  w <- .w(L, w)
  do.call(rbind, lapply(REGLAS_P2, function(r) { s <- vapply(L, function(x) .sc_op(r$fn(x$p2), x$p2$k), 0)
    data.frame(id = r$id, familia = r$familia, nombre = r$nombre, acierto = sum(w * s), nulo = 0.25,
               exceso = sum(w * s) - 0.25, stringsAsFactors = FALSE) }))
}
techo_p2 <- function(L, B = 200L) {
  if (exists(".Random.seed", globalenv())) { vieja <- get(".Random.seed", globalenv()); on.exit(assign(".Random.seed", vieja, globalenv())) }
  set.seed(20261031L)
  S <- lapply(REGLAS_P2, function(r) lapply(L, function(x) r$fn(x$p2)))
  mean(replicate(B, { kk <- sample.int(4L, length(L), replace = TRUE)
    max(vapply(S, function(Sr) mean(vapply(seq_along(L), function(i) .sc_op(Sr[[i]], kk[i]), 0)), 0)) }))
}
## Parte 6, por papel. Devuelve una fila por regla y papel.
.pred6 <- function(L, fn) lapply(L, function(x) fn(x$p6))
medir_p6 <- function(L, w = NULL, papeles = c("hp", "hg", "ventaja", "ganador")) {
  w <- .w(L, w)
  idx <- lapply(papeles, function(pp) vapply(L, function(x) which(x$p6$rol == pp), 0L)); names(idx) <- papeles
  ver <- lapply(papeles, function(pp) vapply(seq_along(L), function(i) L[[i]]$p6$v[idx[[pp]][i]], NA)); names(ver) <- papeles
  do.call(rbind, lapply(REGLAS_P6, function(r) { P <- .pred6(L, r$fn)
    do.call(rbind, lapply(papeles, function(pp) {
      pr <- vapply(seq_along(L), function(i) P[[i]][idx[[pp]][i]], NA); v <- ver[[pp]]; q <- sum(w * v)
      s <- ifelse(is.na(pr), 0.5, as.numeric(pr == v)); nul <- ifelse(is.na(pr), 0.5, ifelse(pr, q, 1 - q))
      data.frame(id = r$id, familia = r$familia, nombre = r$nombre, papel = pp, aplica = sum(w * !is.na(pr)),
                 acierto = sum(w * s), nulo = sum(w * nul), exceso = sum(w * s) - sum(w * nul), stringsAsFactors = FALSE)
    })) }))
}
techo_p6 <- function(L, B = 200L, papeles = c("hp", "hg", "ventaja", "ganador")) {
  if (exists(".Random.seed", globalenv())) { vieja <- get(".Random.seed", globalenv()); on.exit(assign(".Random.seed", vieja, globalenv())) }
  set.seed(20261031L)
  idx <- lapply(papeles, function(pp) vapply(L, function(x) which(x$p6$rol == pp), 0L)); names(idx) <- papeles
  P <- lapply(REGLAS_P6, function(r) .pred6(L, r$fn))
  stats::setNames(vapply(papeles, function(pp) {
    v <- vapply(seq_along(L), function(i) L[[i]]$p6$v[idx[[pp]][i]], NA)
    PR <- lapply(P, function(Pr) vapply(seq_along(L), function(i) Pr[[i]][idx[[pp]][i]], NA))
    mean(replicate(B, { vp <- sample(v); max(vapply(PR, function(pr) mean(ifelse(is.na(pr), 0.5, as.numeric(pr == vp))), 0)) }))
  }, 0), papeles)
}
## Relacionales de la Parte 6 (métricas declaradas en la cabecera).
relacionales_p6 <- function(L, w = NULL) {
  w <- .w(L, w)
  vv <- function(pp) vapply(L, function(x) x$p6$v[x$p6$rol == pp], NA)
  hp <- vv("hp"); hg <- vv("hg"); ve <- vv("ventaja"); ga <- vv("ganador")
  par_conj <- function(a, b) { obs <- sum(w * 0.5 * (a == b)); qa <- sum(w * a); qb <- sum(w * b)
    c(acierto = obs, nulo = 0.5 * (qa * qb + (1 - qa) * (1 - qb)), exceso = obs - 0.5 * (qa * qb + (1 - qa) * (1 - qb)), p_igual = sum(w * (a == b))) }
  cond <- function(v) { obs <- sum(w * (v == !ga)); q <- sum(w * v); qg <- sum(w * !ga)
    c(acierto = obs, nulo = q * qg + (1 - q) * (1 - qg), exceso = obs - (q * qg + (1 - q) * (1 - qg))) }
  list(RL1_par_hp_hg = par_conj(hp, hg), RL2_hp_contraria_ganador = cond(hp), RL2_hg_contraria_ganador = cond(hg),
       RL3_par_ventaja_ganador = par_conj(ve, ga))
}
control_p6 <- function(L) mean(vapply(L, function(x) identical(unname(CONTROL_P6(x$p6)), unname(x$p6$v)), NA))

## Registro desde el entorno de una versión del .Rmd.
registro_bateria <- function(en) with(en, list(
  p2 = list(op = opciones_p2, k = which(sol_p2), tipo = tipo_p2, preg = texto_p2, otros = c(texto_p3, texto_p4, texto_p5),
            enc = names(tabla_par_df)[2:3]),
  p6 = list(op = opciones_p6, v = sol_p6, rol = rol_p6, n = n_fila_par, a = a_fila_par, nombres = nom_grado),
  rama_par = rama_par, k = dp$k, delta = dp$delta, canon = isTRUE(es_canonica)))

## Reglas por encima del umbral en una tabla de medida (exceso > +8 pp), ordenadas.
sobre_umbral <- function(M) { M <- M[M$exceso > UMBRAL_OBLIGA + 1e-12, , drop = FALSE]; M[order(-M$exceso), , drop = FALSE] }
