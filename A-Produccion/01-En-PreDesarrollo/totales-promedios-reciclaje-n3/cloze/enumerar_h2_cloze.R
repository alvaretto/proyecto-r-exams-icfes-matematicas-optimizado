## =============================================================================
## enumerar_h2_cloze.R — L-C5 (tanda 5, 2026-10-31): la Parte 6 por ENUMERACIÓN EXACTA conducida por el código del
## .Rmd, para CUALQUIER Parte 1. Puerta del override «H2 a» del profesor (2026-10-31, ../.claude/CLAUDE.md § «Decisión
## del profesor tras la tanda 4»): las cifras firmadas son las de V0 en salida/reparto_h2_96cfcf35.txt, y la
## aclaración iii de §P7-E exige compararlas con lo que ejecuta el propio código, con las exclusiones de V11 incluidas
## (dependen de la Parte 1). La carga verificar_espacio_cloze.R (E8). No lee ni escribe archivos.
##
## Cómo. Para una semilla, el bloque P1 se evalúa una vez: la Parte 1 queda fija, y sus cifras deciden las exclusiones
## de V11 del caso paralelo. Después, las Partes 2 a 6 se evalúan una vez por hoja de un árbol de sorteos, con sample() y
## sample.int() sustituidos en el entorno del código (los heredan pick() y pick1(), que viven en ese entorno):
##   - CONTROLADOS (se ramifican; peso 1 / n.º de resultados de la llamada que se ejecuta): la rama del otro colegio, el
##     orden de las combinaciones de veredictos de las hipótesis (la cascada F1 corre dentro del código: si el paso 1 no
##     tiene candidatos, el árbol recorre los pasos que el código ejecuta), el veredicto de «ventaja»/«Ganó…» cuando se
##     sortea y la redacción de «ventaja».
##   - LIBRES que se integran EXACTAMENTE dentro de la hoja: la fila del caso paralelo (uniforme sobre las filas de ep,
##     capturadas en la llamada) y la fila de la tabla del grado que gana (gi_par, uniforme sobre 2).
##   - LIBRES que no entran en ninguna regla de L-C5: los grados (solo nombres; guarda: «Ganó…» es la afirmación más
##     corta con cualquier nombre, así que P6-F1 y P6-F2 no dependen del nombre), m3, la Parte 2, el orden de las opciones
##     (o2 y o6: las reglas del orden de las afirmaciones, P6-P2, quedan fuera de L-C5 y se juzgan en la muestra de E8) y
##     la reflexión.
##   - Cualquier otro sorteo después del marcador hace fallar la hoja, y cada hoja cuenta las llamadas de cada sorteo
##     libre (un sorteo nuevo con el texto de uno libre no se integraría: H4 de la FASE 2C r1).
## Mezcla de Partes 1: la puerta de verificar_espacio_cloze.R promedia las 100 semillas de la muestra estándar (regla
## #23). Es exacta en las Partes 2 a 6 y condicionada a esas 100 Partes 1, y es determinista: la misma cifra en cada
## corrida.
## =============================================================================
H2_CONTROLADOS <- c(
  rama_par = 'sample(c("CURSOS", "PROMEDIO"), 1L)',
  orden_h  = 'sample.int(3L)',
  v_c      = 'sample(c(TRUE, FALSE), 1L)',
  c6       = 'pick1(cand_c)')
H2_LIBRES <- c(
  fila_par  = 'sample.int(nrow(ep), 1L)',
  grados    = 'sample.int(4L, 2L)',
  gi_m3     = 'sample.int(2L, 1L)',
  tipo_p2   = 'sample(names(P2), 1L)',
  clave_p2  = 'pick(q2$clave)',
  dis_p2    = 'sample.int(length(q2$distractores), 3L)',
  orden     = 'sample.int(4L)',
  reflexion = 'pick(reflexiones_cloze)')
## Llamadas de cada sorteo libre por hoja (gi_par y m3 comparten texto; o2 y o6 también).
H2_LLAMADAS <- c(fila_par = 1L, grados = 1L, gi_m3 = 2L, tipo_p2 = 1L, clave_p2 = 1L, dis_p2 = 1L, orden = 2L, reflexion = 1L)

.h2_perm <- function(v) if (length(v) <= 1L) list(v) else
  do.call(c, lapply(seq_along(v), function(i) lapply(.h2_perm(v[-i]), function(p) c(v[i], p))))
.h2_id <- function(llamada, padre) {
  if (is.call(padre) && is.name(padre[[1L]]) && as.character(padre[[1L]]) %in% c("pick", "pick1"))
    return(paste(deparse(padre, width.cutoff = 500L), collapse = " "))
  paste(deparse(llamada, width.cutoff = 500L), collapse = " ")
}
.h2_muestreador <- function(ST) {
  elegir <- function(id, pob, size, replace, prob) {
    if (isTRUE(replace) || !is.null(prob)) stop("L-C5: el sorteo controlado «", id, "» usa replace o prob")
    n <- length(pob); if (is.null(size)) size <- n
    outs <- if (size == 1L) as.list(pob) else if (size == n) lapply(.h2_perm(seq_len(n)), function(p) pob[p]) else
      stop("L-C5: el sorteo controlado «", id, "» pide ", size, " de ", n)
    ST$pos <- ST$pos + 1L
    i <- if (ST$pos <= length(ST$guion)) ST$guion[[ST$pos]] else 1L
    ST$traza[[ST$pos]] <- list(id = id, i = i, n = length(outs))
    outs[[i]]
  }
  libre <- function(id) { if (!id %in% H2_LIBRES) stop("L-C5: sorteo no reconocido después del bloque P1: ", id)
    ST$libres <- c(ST$libres, id) }
  list(
    sample = function(x, size, replace = FALSE, prob = NULL) {
      id <- .h2_id(sys.call(), sys.call(-1L))
      if (id %in% H2_CONTROLADOS) {
        pob <- if (length(x) == 1L && is.numeric(x) && x >= 1) seq_len(x) else x   # misma semántica que base::sample
        return(elegir(id, pob, if (missing(size)) NULL else size, replace, prob))
      }
      libre(id)
      if (missing(size)) base::sample(x, replace = replace, prob = prob) else base::sample(x, size, replace, prob)
    },
    sample.int = function(n, size = n, replace = FALSE, prob = NULL, useHash = FALSE) {
      id <- .h2_id(sys.call(), sys.call(-1L))
      if (id %in% H2_CONTROLADOS) return(elegir(id, seq_len(n), if (missing(size)) NULL else size, replace, prob))
      libre(id)
      if (id == H2_LIBRES[["fila_par"]]) ST$ep <- get("ep", envir = parent.frame())
      base::sample.int(n, size, replace, prob)
    })
}

## Valor de una hoja: una fila por (fila del caso paralelo, fila del ganador en la tabla), con su peso dentro de la hoja.
h2_hoja <- function(enB, ST) {
  vis <- ST$libres
  vistos <- vapply(names(H2_LIBRES), function(k) sum(vis == H2_LIBRES[[k]]), 0L)
  mal <- names(vistos)[vistos != H2_LLAMADAS[names(vistos)]]
  if (length(mal)) stop("L-C5: sorteo libre con un número de llamadas distinto del esperado (",
    paste(sprintf("«%s» %d en vez de %d", H2_LIBRES[mal], vistos[mal], H2_LLAMADAS[mal]), collapse = "; "), ")")
  ep <- ST$ep; if (is.null(ep) || !nrow(ep)) stop("L-C5: la hoja no capturó las filas del caso paralelo")
  g <- function(x) get(x, envir = enB)
  v_hp <- g("v_hp"); v_hg <- g("v_hg")
  if (any((ep$aG > ep$D) != v_hg | (ep$aP > ep$D) != v_hp)) stop("L-C5: una fila de ep no lleva los veredictos sorteados")
  c6 <- g("c6"); ventajas <- vapply(g("P6_VENTAJA"), function(z) z$texto, "")
  gan_txt <- paste0("Ganó el concurso el grado ", g("GRADOS"), ".")
  if (max(nchar(gan_txt)) >= min(nchar(c(g("P6_HP"), g("P6_HG"), ventajas))))
    stop("L-C5: «Ganó…» ya no es la afirmación más corta con cualquier nombre (P6-F1/F2 dependerían del nombre)")
  m <- nrow(ep)
  data.frame(rama = g("rama_par"), nG = rep(ep$nG, 2L), nP = rep(ep$nP, 2L), aG = rep(ep$aG, 2L), aP = rep(ep$aP, 2L),
             gi = rep(1:2, each = m), v_hp = v_hp, v_hg = v_hg, v_c = g("v_c"), v_d = g("v_d"),
             hp_txt = g("P6_HP"), hg_txt = g("P6_HG"), c6_txt = c6$texto, cascada = g("cascada_par"),
             w = 1 / (2 * m), stringsAsFactors = FALSE)
}

## Enumera las Partes 2 a 6 para la Parte 1 de una semilla. Devuelve las filas de todas las hojas sin error, con su peso
## (suma 1 si ninguna hoja falla), la masa con error y los errores.
enumerar_h2 <- function(cod, marca, semilla) {
  partes <- strsplit(cod, marca, fixed = TRUE)[[1]]
  if (length(partes) != 2L) stop("L-C5: el marcador del bloque P1 debe partir el código en dos")
  eA <- parse(text = partes[1], encoding = "UTF-8"); eB <- parse(text = partes[2], encoding = "UTF-8")
  set.seed(semilla); enA <- new.env(); eval(eA, envir = enA)
  ST <- new.env(); ms <- .h2_muestreador(ST); enA$sample <- ms$sample; enA$sample.int <- ms$sample.int
  filas <- list(); masa_err <- 0; errores <- character(0); pila <- list(integer(0)); n_hojas <- 0L
  while (length(pila)) {
    pref <- pila[[length(pila)]]; pila[[length(pila)]] <- NULL
    ST$guion <- pref; ST$pos <- 0L; ST$traza <- list(); ST$ep <- NULL; ST$libres <- character(0)
    set.seed(semilla + 104729L)
    enB <- new.env(parent = enA)
    res <- tryCatch({ eval(eB, envir = enB); list(val = h2_hoja(enB, ST), err = NULL) },
                    error = function(e) list(val = NULL, err = conditionMessage(e)))
    tr <- ST$traza; el <- vapply(tr, function(t) t$i, 0L); no <- vapply(tr, function(t) t$n, 0L); w <- prod(1 / no)
    n_hojas <- n_hojas + 1L
    if (is.null(res$err)) { v <- res$val; v$w <- v$w * w; filas[[length(filas) + 1L]] <- v } else {
      masa_err <- masa_err + w; errores <- unique(c(errores, res$err)) }
    if (length(el) > length(pref)) for (q in (length(pref) + 1L):length(el)) for (alt in seq_len(no[q])[-1L])
      pila[[length(pila) + 1L]] <- c(el[seq_len(q - 1L)], alt)
  }
  list(filas = if (length(filas)) do.call(rbind, filas) else NULL, masa_err = masa_err, errores = errores, n_hojas = n_hojas)
}

## Suma los pesos de filas idénticas (las ramas de orden_h que no cambian el paso de la cascada dan las mismas filas).
h2_agregar <- function(F) {
  clave <- c("rama", "nG", "nP", "aG", "aP", "gi", "v_hp", "v_hg", "v_c", "v_d", "hp_txt", "hg_txt", "c6_txt")
  A <- stats::aggregate(F["w"], by = F[clave], FUN = sum)
  A[order(A$rama, A$nG, A$nP, A$aG, A$aP, A$gi, A$c6_txt), , drop = FALSE]
}

## Registros de la batería L-C4 (bateria_partes_2_a_6.R) a partir de las filas agregadas. Los nombres de grado son fijos:
## solo sirven para identificar la fila del ganador (la guarda de h2_hoja garantiza que no mueven ninguna regla).
h2_registros <- function(A, NOM = c("sexto", "noveno")) lapply(seq_len(nrow(A)), function(j) {
  r <- A[j, ]; g1 <- r$gi == 1L
  list(p6 = list(op = c(r$hp_txt, r$hg_txt, r$c6_txt, paste0("Ganó el concurso el grado ", if (r$v_d) NOM[r$gi] else NOM[3L - r$gi], ".")),
                 v = c(r$v_hp, r$v_hg, r$v_c, r$v_d), rol = c("hp", "hg", "ventaja", "ganador"),
                 n = if (g1) c(r$nG, r$nP) else c(r$nP, r$nG), a = if (g1) c(r$aG, r$aP) else c(r$aP, r$aG), nombres = NOM),
       rama_par = r$rama, k = abs(r$nG - r$nP))
})

## Estratos de la firma «H2 a»: mezcla, rama del otro colegio y k′ (diferencia de cursos de la tabla mostrada).
H2_ESTRATOS <- list(mezcla = function(A) rep(TRUE, nrow(A)), CURSOS = function(A) A$rama == "CURSOS",
                    PROMEDIO = function(A) A$rama == "PROMEDIO", k1 = function(A) abs(A$nG - A$nP) == 1L,
                    k2 = function(A) abs(A$nG - A$nP) >= 2L)
## Reglas del ORDEN de las afirmaciones: dependen de o6, que L-C5 no ramifica; se juzgan en la muestra de E8.
H2_SIN_ORDEN <- c("P6-P2", "P6-P2c")
## Tabla exacta por estrato: cada regla por papel, quedándose con la regla o su contraria (exceso opuesto), más RL1
## (métrica del par) y RL2 (condicional al ganador). EB: entorno de la batería L-C4 congelada.
h2_tabla <- function(A, EB) {
  out <- list()
  for (e in names(H2_ESTRATOS)) {
    s <- H2_ESTRATOS[[e]](A); if (!any(s)) next
    L <- h2_registros(A[s, , drop = FALSE]); w <- A$w[s] / sum(A$w[s])
    if (EB$control_p6(L) != 1) stop("L-C5: el control positivo de la batería no acierta las cuatro afirmaciones en el estrato ", e)
    M <- EB$medir_p6(L, w); M <- M[!M$id %in% H2_SIN_ORDEN, , drop = FALSE]; M$base <- sub("c$", "", M$id)
    B <- do.call(rbind, lapply(split(M, paste(M$base, M$papel)), function(d) d[which.max(d$exceso), , drop = FALSE]))
    R <- EB$relacionales_p6(L, w); rl <- list(c("RL1", "par", "RL1_par_hp_hg"), c("RL2", "hp", "RL2_hp_contraria_ganador"),
                                             c("RL2", "hg", "RL2_hg_contraria_ganador"))
    B <- rbind(B, do.call(rbind, lapply(rl, function(z) data.frame(id = z[1], familia = "relacional", nombre = z[3], papel = z[2],
      aplica = 1, acierto = R[[z[3]]][["acierto"]], nulo = R[[z[3]]][["nulo"]], exceso = R[[z[3]]][["exceso"]], base = z[1],
      stringsAsFactors = FALSE))))
    B$estrato <- e; B$masa <- sum(A$w[s]) / sum(A$w); out[[e]] <- B
  }
  T <- do.call(rbind, out); rownames(T) <- NULL; T
}
## Veredicto de cada celda contra la firma (pp, un decimal, como se firmó): «ok» (≤ +8 pp), «cubierto» (dentro de lo
## firmado) o «pendiente» (por encima de lo firmado, o > +8 pp en una celda que la firma no cubre).
h2_juzgar <- function(T, FIRMA) {
  T$firma <- vapply(seq_len(nrow(T)), function(i) { f <- FIRMA[FIRMA$base == T$base[i] & FIRMA$papel == T$papel[i], , drop = FALSE]
    if (!nrow(f) || !T$estrato[i] %in% names(f)) NA_real_ else f[[T$estrato[i]]][1] }, 0)
  ex <- 100 * T$exceso
  T$veredicto <- ifelse(ex <= 8 + 1e-9, "ok", ifelse(!is.na(T$firma) & round(ex, 1) <= T$firma + 1e-9, "cubierto", "pendiente"))
  T
}
