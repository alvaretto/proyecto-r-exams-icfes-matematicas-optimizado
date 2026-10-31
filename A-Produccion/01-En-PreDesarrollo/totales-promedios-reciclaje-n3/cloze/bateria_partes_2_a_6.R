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
##
## ---- L-C4 v2 (tanda 7, 2026-10-31; decisión «a · 1 sí · 2 sí» del profesor tras la tanda 6) ----------------------------
## v1 = md5 de20e0a986f86d44f0d3412517d2f07b (commit 31b8fe9e). v2 ENDURECE (regla #24 H-5: endurecer es autónomo) y
## conserva intactas las reglas de v1: sus cifras se reproducen dentro de v2. Lo que añade (regla #22, texto íntegro, §P7-E
## «familia léxica» y §P7-G; H1, H2 y H3 de la FASE 2C r2):
##   1. VOCABULARIO, calculado sobre los textos y no elegido a mano. Token = palabra (letras, cualquier largo) o expresión
##      de 2 o 3 palabras consecutivas. Por cada token de las opciones, a lo largo de las versiones, dos reglas: «elige las
##      que dicen t» (P2-V «t») y su contraria «descarta las que dicen t» (P2-V «t»c). Cubren TODOS los tokens, no solo
##      los exclusivos: un token casi exclusivo es igual de explotable, y limitarse a los exclusivos sería cobertura
##      nominal (Error 37). Además, P2-VX «elige la que lleva vocabulario exclusivo de la clave» (tokens que aparecen en
##      alguna clave y en ningún distractor) con su contraria, y P2-VXE, el mismo vocabulario buscado en el estímulo
##      visible (asociación entre enunciado y opción). La unión del vocabulario exclusivo de los DISTRACTORES no entra como
##      regla: cada distractor lleva por construcción las palabras de su propio concepto, y esa unión equivale a saberse el
##      banco; el token suelto sí entra («descarta las que dicen t»), que es el atajo de H2 («por curso»).
##      Parte 6: P6-VX / P6-VXF, vocabulario exclusivo de las afirmaciones verdaderas o de las falsas, calculado A LO
##      LARGO DE TODAS LAS VERSIONES (fijar_universo_p6(); no dentro de cada estrato, donde la rama fija el veredicto de
##      la de ventaja y fabricaría una exclusividad que el estudiante no ve), con su contraria. Con los veredictos
##      sorteados primero (F-4, L-C1), ningún texto es siempre verdadero: P6-VX aplica 0 % y PUEDE disparar si un texto
##      quedara con veredicto fijo (Error 37: se declara que la sonda puede disparar). La regla por token de la Parte 6
##      (vocab_p6_tokens()) NO entra en la batería: condicionada por la tabla reproduce las reglas de magnitud ya firmadas
##      en «H2 a» (en los estratos de k′, hasta +16,7 pp, por debajo de K1, +32,8 y +33,5) y condicionada por la rama,
##      el contenido lógico de la afirmación de ventaja (RL3: reconocerlo es razonar); en la mezcla, 0,0 pp. Se mide y se
##      declara aparte (salida/bateria_v2_historico.txt), y el profesor decide si entra en una v3.
##   2. CONTRARIAS de todas las reglas de la Parte 2 de v1 que no la tenían (léxico, formato, relacional, estímulo,
##      posición), y molde: la única con su primera palabra, con sus tres primeras y con sus tres últimas (con contraria).
##   3. ESTÍMULO COMPLETO (§P7-G; H3 de la r2): el párrafo del otro colegio (P2-E5), su tabla, encabezados y celdas
##      (P2-E6), y todo el texto visible del ítem (P2-E7 a P2-E9: Parte 1 —enunciado, pregunta, tabla y opciones—, el
##      párrafo, la tabla, la pregunta de la Parte 2 y las Partes 3 a 6), con sus contrarias.
## MEDIDA EXACTA de la Parte 2 (medir_p2_exacta): las reglas de contenido no dependen del orden de las opciones, así que el
## espacio de textos de un tipo son sus 3 claves × C(5, 3) tríos, equiprobables; las reglas del estímulo se integran además
## sobre los portadores del estímulo (las versiones de la muestra estándar): exacta en la Parte 2, condicionada a esos
## portadores. Las de posición siguen en la muestra (dependen del sorteo o2).
## =============================================================================
BATERIA_VERSION <- "L-C4 v2 (2026-10-31)"
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

## ---- L-C4 v2 · Parte 2: contrarias, molde, estímulo completo y vocabulario --------------------------------------------
## .tk con memoria: misma salida que la de v1 (las reglas de v1 no cambian), solo más rápida con textos largos.
.TK <- new.env(hash = TRUE)
.tk <- function(s) { if (!nzchar(s)) return(character(0)); v <- .TK[[s]]; if (!is.null(v)) return(v)
  v <- unique(tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]{3,}", s))[[1]])); assign(s, v, envir = .TK); v }
## Tokens del vocabulario: palabras (letras, cualquier largo) y expresiones de 2 y 3 palabras consecutivas.
.pal <- function(s) tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]+", s))[[1]])
.NG <- new.env(hash = TRUE)
.ngr <- function(s) { if (!nzchar(s)) return(character(0)); v <- .NG[[s]]; if (!is.null(v)) return(v)
  w <- .pal(s); o <- character(0)
  for (n in 1:3) if (length(w) >= n) o <- c(o, vapply(seq_len(length(w) - n + 1L), function(i) paste(w[i:(i + n - 1L)], collapse = " "), ""))
  o <- unique(o); assign(s, o, envir = .NG); o }
## Celdas de una tabla pipe mostrada (sin span, negrita ni espacio duro).
.celdas <- function(md) { f <- grep("^\\|", strsplit(md, "\n", fixed = TRUE)[[1]], value = TRUE); f <- f[!grepl("^\\|[-:| ]+\\|$", f)]
  x <- unlist(lapply(f, function(r) trimws(strsplit(sub("^\\|", "", sub("\\|$", "", r)), "|", fixed = TRUE)[[1]])))
  gsub(" ", " ", gsub("<span[^>]*>|</span>|\\*\\*", "", x)) }
## Estímulo visible de una versión: Parte 1 (enunciado, pregunta, tabla, opciones), párrafo y tabla del otro colegio,
## pregunta de la Parte 2 y Partes 3 a 6. «ng» guarda sus tokens (para P2-VXE).
.EST_TODO <- c("p1", "intro", "tabla", "preg", "otros", "p6")
.est_txt <- function(x, partes) unlist(x[partes], use.names = FALSE)
.con_ng <- function(e) { e$ng <- unique(unlist(lapply(.est_txt(e, .EST_TODO), .ngr))); e }
estimulo_p2 <- function(en, preg = NULL) {
  e <- list(preg = if (is.null(preg)) en$texto_p2 else preg, otros = c(en$texto_p3, en$texto_p4, en$texto_p5),
            enc = names(en$tabla_par_df)[2:3], intro = en$intro_par, tabla = .celdas(en$tabla_par_md),
            p6 = c(en$texto_p6, en$opciones_p6), p1 = c(en$enunciado, en$pregunta, .celdas(en$tabla_md), en$opciones_p1))
  .con_ng(e) }
## Lo mismo desde un registro del verificador (rec() de verificar_espacio_cloze.R).
estimulo_p2_rec <- function(x, preg = x$texto_p2) {
  e <- list(preg = preg, otros = c(x$texto_p3, x$texto_p4, x$texto_p5), enc = NULL, intro = x$intro,
            tabla = .celdas(x$tabla_par_md), p6 = c(x$texto_p6, x$opciones_p6), p1 = c(x$enunciado, x$pregunta, .celdas(x$tabla_md), x$opciones))
  .con_ng(e) }
.comun_n <- function(op, ref) { r <- .tk(paste(ref, collapse = " ")); vapply(op, function(s) length(intersect(.tk(s), r)), 0L) }
.unica_estim <- function(op, ref) { T <- lapply(op, .tk); o <- .tk(paste(ref, collapse = " "))
  e <- vapply(seq_along(T), function(i) length(setdiff(intersect(T[[i]], o), unlist(T[-i]))), 0L)
  if (max(e) == 0L) rep(TRUE, length(T)) else e == max(e) }
.molde <- function(op, f) { m <- vapply(op, f, ""); m %in% names(which(table(m) == 1L)) }
.r1 <- function(id) Filter(function(r) r$id == id, REGLAS_P2)[[1]]
.contraria <- function(r) list(id = paste0(r$id, "c"), familia = r$familia, nombre = paste0("contraria de ", r$id, " (", r$nombre, ")"),
                               usa_estimulo = isTRUE(r$usa_estimulo), fn = function(x) !r$fn(x))
REGLAS_P2_V2_NUEVAS <- list(
  list(id = "P2-M1", familia = "lexico", nombre = "la única con su primera palabra (molde)", fn = function(x) .molde(x$op, function(o) .pal(o)[1])),
  list(id = "P2-M2", familia = "lexico", nombre = "la única con sus tres primeras palabras (molde)", fn = function(x) .molde(x$op, function(o) paste(utils::head(.pal(o), 3), collapse = " "))),
  list(id = "P2-M3", familia = "lexico", nombre = "la única con sus tres últimas palabras (molde)", fn = function(x) .molde(x$op, function(o) paste(utils::tail(.pal(o), 3), collapse = " "))),
  list(id = "P2-E5", familia = "estimulo", usa_estimulo = TRUE, nombre = "más palabras en común con el párrafo del otro colegio (H3 de la r2)", fn = function(x) .comun(x$op, x$intro)),
  list(id = "P2-E6", familia = "estimulo", usa_estimulo = TRUE, nombre = "más palabras en común con la tabla del otro colegio (encabezados y celdas)", fn = function(x) .comun(x$op, x$tabla)),
  list(id = "P2-E7", familia = "estimulo", usa_estimulo = TRUE, nombre = "más palabras en común con todo el texto visible del ítem", fn = function(x) .comun(x$op, .est_txt(x, .EST_TODO))),
  list(id = "P2-E8", familia = "estimulo", usa_estimulo = TRUE, nombre = "la única con una palabra del texto visible que no tiene otra opción", fn = function(x) .unica_estim(x$op, .est_txt(x, .EST_TODO))),
  list(id = "P2-E9", familia = "estimulo", usa_estimulo = TRUE, nombre = "la que menos palabras comparte con todo el texto visible", fn = function(x) { k <- .comun_n(x$op, .est_txt(x, .EST_TODO)); k == min(k) }))
## Las reglas del estímulo de v1 (P2-E1 a P2-E4) se marcan como tales para integrarlas sobre los portadores.
REGLAS_P2 <- lapply(REGLAS_P2, function(r) { if (startsWith(r$id, "P2-E")) r$usa_estimulo <- TRUE; r })
REGLAS_P2 <- c(REGLAS_P2,
  lapply(c("P2-L1", "P2-L2", "P2-L3", "P2-L4", "P2-L5", "P2-L6", "P2-F2", "P2-R1", "P2-R2", "P2-R3",
           "P2-E1", "P2-E2", "P2-E3", "P2-E4", "P2-P1", "P2-P2"), function(i) .contraria(.r1(i))),
  REGLAS_P2_V2_NUEVAS, lapply(REGLAS_P2_V2_NUEVAS, .contraria))
## Vocabulario de un banco (claves y distractores distintos): por token, en cuántas claves (k de nk) y en cuántos
## distractores (m de nd) aparece. Exclusivo de la clave: k > 0 y m = 0.
vocabulario <- function(claves, distractores) {
  claves <- unique(claves); distractores <- unique(distractores)
  T <- unique(unlist(lapply(c(claves, distractores), .ngr)))
  k <- vapply(T, function(t) sum(vapply(claves, function(s) t %in% .ngr(s), NA)), 0L)
  m <- vapply(T, function(t) sum(vapply(distractores, function(s) t %in% .ngr(s), NA)), 0L)
  data.frame(token = T, k = unname(k), nk = length(claves), m = unname(m), nd = length(distractores), stringsAsFactors = FALSE)
}
reglas_vocab_p2 <- function(V) {
  vx <- V$token[V$k > 0 & V$m == 0]
  tiene_vx <- function(x) vapply(x$op, function(o) any(.ngr(o) %in% vx), NA)
  vxe <- function(x) { e <- intersect(vx, if (!is.null(x$ng)) x$ng else unique(unlist(lapply(.est_txt(x, .EST_TODO), .ngr))))
    vapply(x$op, function(o) any(.ngr(o) %in% e), NA) }
  base <- list(
    list(id = "P2-VX", familia = "lexico", nombre = sprintf("elige la que lleva vocabulario exclusivo de la clave (%d tokens: %s)", length(vx),
         paste(utils::head(vx, 12), collapse = " · ")), fn = tiene_vx),
    list(id = "P2-VXc", familia = "lexico", nombre = "contraria de P2-VX: descarta las que llevan vocabulario exclusivo de la clave", fn = function(x) !tiene_vx(x)),
    list(id = "P2-VXE", familia = "estimulo", usa_estimulo = TRUE, nombre = "elige la que comparte con el texto visible un token exclusivo de la clave", fn = vxe),
    list(id = "P2-VXEc", familia = "estimulo", usa_estimulo = TRUE, nombre = "contraria de P2-VXE", fn = function(x) !vxe(x)))
  tok <- lapply(seq_len(nrow(V)), function(i) { t <- V$token[i]
    nm <- sprintf("«%s» (claves %d/%d, distractores %d/%d)", t, V$k[i], V$nk[i], V$m[i], V$nd[i])
    f <- function(x) vapply(x$op, function(o) t %in% .ngr(o), NA)
    list(list(id = sprintf("P2-V «%s»", t), familia = "lexico", nombre = paste("elige las que dicen", nm), fn = f),
         list(id = sprintf("P2-V «%s»c", t), familia = "lexico", nombre = paste("descarta las que dicen", nm), fn = function(x) !f(x))) })
  c(base, unlist(tok, recursive = FALSE))
}

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
.fila_p2 <- function(r, s) data.frame(id = r$id, familia = r$familia, nombre = r$nombre, acierto = s, nulo = 0.25,
                                       exceso = s - 0.25, stringsAsFactors = FALSE)
## Muestra (posición, mutantes): reglas de v1 y v2 y, si vocab, el vocabulario calculado sobre los textos de L.
medir_p2 <- function(L, w = NULL, vocab = TRUE) {
  w <- .w(L, w); R <- REGLAS_P2
  if (vocab) R <- c(R, reglas_vocab_p2(vocabulario(unlist(lapply(L, function(x) x$p2$op[x$p2$k])),
                                                   unlist(lapply(L, function(x) x$p2$op[-x$p2$k])))))
  do.call(rbind, lapply(R, function(r) .fila_p2(r, sum(w * vapply(L, function(x) .sc_op(r$fn(x$p2), x$p2$k), 0)))))
}
## EXACTA (v2): banco = list(clave, distractores) de un tipo; estimulos = portadores del estímulo (estimulo_p2()).
## Reglas de contenido sobre las 3 × C(5, 3) combinaciones equiprobables (la clave en la posición 1: no dependen del
## orden); las del estímulo, además, integradas sobre los portadores. Sin las de posición (muestra).
medir_p2_exacta <- function(banco, tipo, estimulos) {
  cl <- unique(unname(banco$clave)); di <- unique(unname(banco$distractores)); V <- vocabulario(cl, di)
  combos <- list(); for (i in seq_along(cl)) for (s in utils::combn(length(di), 3L, simplify = FALSE)) combos[[length(combos) + 1L]] <- c(cl[i], di[s])
  R <- Filter(function(r) r$familia != "posicion", c(REGLAS_P2, reglas_vocab_p2(V)))
  M <- do.call(rbind, lapply(R, function(r) { E <- if (isTRUE(r$usa_estimulo)) estimulos else estimulos[1]
    .fila_p2(r, mean(vapply(E, function(e) mean(vapply(combos, function(op) .sc_op(r$fn(c(list(op = op, k = 1L, tipo = tipo), e)), 1L), 0)), 0))) }))
  attr(M, "vocabulario") <- V; attr(M, "n_combos") <- length(combos); attr(M, "n_portadores") <- length(estimulos); M
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
  est <- lapply(REGLAS_P6, function(r) { P <- .pred6(L, r$fn)
    do.call(rbind, lapply(papeles, function(pp) {
      pr <- vapply(seq_along(L), function(i) P[[i]][idx[[pp]][i]], NA); v <- ver[[pp]]; q <- sum(w * v)
      s <- ifelse(is.na(pr), 0.5, as.numeric(pr == v)); nul <- ifelse(is.na(pr), 0.5, ifelse(pr, q, 1 - q))
      data.frame(id = r$id, familia = r$familia, nombre = r$nombre, papel = pp, aplica = sum(w * !is.na(pr)),
                 acierto = sum(w * s), nulo = sum(w * nul), exceso = sum(w * s) - sum(w * nul), stringsAsFactors = FALSE)
    })) })
  do.call(rbind, c(est, list(vocab_p6(L, w, papeles))))
}
## v2: vocabulario de la Parte 6, por afirmación y papel. «Las que dicen t son verdaderas» predice verdadera donde
## aparece t y se abstiene donde no; su contraria predice falsa. Se agrega por texto distinto (exacto y rápido):
## exceso = masa(t)·(P(verdadera | t) − q). P6-VX / P6-VXF: vocabulario exclusivo de las verdaderas / de las falsas.
UNIVERSO_P6 <- NULL
## Fija el vocabulario exclusivo de las verdaderas y de las falsas sobre TODAS las versiones (registros de la Parte 6).
fijar_universo_p6 <- function(L) {
  tx <- unlist(lapply(L, function(x) x$p6$op)); vv <- unlist(lapply(L, function(x) x$p6$v))
  ngv <- unique(unlist(lapply(unique(tx[vv]), .ngr))); ngf <- unique(unlist(lapply(unique(tx[!vv]), .ngr)))
  UNIVERSO_P6 <<- list(vx = setdiff(ngv, ngf), vxf = setdiff(ngf, ngv), n = length(L)); invisible(UNIVERSO_P6) }
vocab_p6 <- function(L, w, papeles, tokens = FALSE) {
  ix <- lapply(papeles, function(pp) vapply(L, function(x) which(x$p6$rol == pp), 0L)); names(ix) <- papeles
  tx <- lapply(papeles, function(pp) vapply(seq_along(L), function(i) L[[i]]$p6$op[ix[[pp]][i]], "")); names(tx) <- papeles
  vv <- lapply(papeles, function(pp) vapply(seq_along(L), function(i) L[[i]]$p6$v[ix[[pp]][i]], NA)); names(vv) <- papeles
  textos <- unique(unlist(tx)); NG <- lapply(textos, .ngr); names(NG) <- textos
  ngv <- unique(unlist(NG[unique(unlist(lapply(papeles, function(pp) tx[[pp]][vv[[pp]]])))]))
  ngf <- unique(unlist(NG[unique(unlist(lapply(papeles, function(pp) tx[[pp]][!vv[[pp]]])))]))
  vx <- setdiff(ngv, ngf); vxf <- setdiff(ngf, ngv)
  if (!is.null(UNIVERSO_P6)) { vx <- UNIVERSO_P6$vx; vxf <- UNIVERSO_P6$vxf }
  toks <- if (tokens) unique(unlist(NG)) else character(0); filas <- list()
  for (pp in papeles) {
    q <- sum(w * vv[[pp]]); a <- tapply(w, tx[[pp]], sum); av <- tapply(w * vv[[pp]], tx[[pp]], sum); tt <- names(a)
    add <- function(id, nombre, p) { ap <- sum(a[p]); at <- sum(av[p]); ex <- at - q * ap
      filas[[length(filas) + 1L]] <<- data.frame(id = c(id, paste0(id, "c")), familia = "lexico",
        nombre = c(nombre, paste("contraria:", sub("son verdaderas", "son falsas", nombre))), papel = pp, aplica = ap,
        acierto = 0.5 * (1 - ap) + c(at, ap - at), nulo = 0.5 * (1 - ap) + c(q * ap, (1 - q) * ap), exceso = c(ex, -ex),
        stringsAsFactors = FALSE) }
    add("P6-VX", sprintf("las que llevan vocabulario exclusivo de las verdaderas (%d tokens) son verdaderas", length(vx)),
        vapply(tt, function(s) any(NG[[s]] %in% vx), NA))
    add("P6-VXF", sprintf("las que llevan vocabulario exclusivo de las falsas (%d tokens) son verdaderas", length(vxf)),
        vapply(tt, function(s) any(NG[[s]] %in% vxf), NA))
    for (tk in toks) add(sprintf("P6-V «%s»", tk), sprintf("las que dicen «%s» son verdaderas", tk), vapply(tt, function(s) tk %in% NG[[s]], NA))
  }
  do.call(rbind, filas)
}
## Exploratoria, fuera de la batería (ver cabecera): las reglas por token de la Parte 6, solas.
vocab_p6_tokens <- function(L, w = NULL, papeles = c("hp", "hg", "ventaja", "ganador")) { w <- .w(L, w)
  V <- vocab_p6(L, w, papeles, tokens = TRUE); V[grepl("^P6-V «", V$id), , drop = FALSE] }
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
  p2 = c(list(op = opciones_p2, k = which(sol_p2), tipo = tipo_p2), estimulo_p2(en)),
  p6 = list(op = opciones_p6, v = sol_p6, rol = rol_p6, n = n_fila_par, a = a_fila_par, nombres = nom_grado),
  rama_par = rama_par, k = dp$k, delta = dp$delta, canon = isTRUE(es_canonica)))

## Reglas por encima del umbral en una tabla de medida (exceso > +8 pp), ordenadas.
sobre_umbral <- function(M) { M <- M[M$exceso > UMBRAL_OBLIGA + 1e-12, , drop = FALSE]; M[order(-M$exceso), , drop = FALSE] }
