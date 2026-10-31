## =============================================================================
## enumerar_canonica_cloze.R — enumeración EXACTA de la instancia canónica, conducida por el código del .Rmd
## Tanda 3 (2026-10-30). Puerta de la canónica de L-C3, ampliada a X6, X9, X10 y X11 (firma del profesor del
## 2026-10-30, «a · aclaración sí»). La carga verificar_espacio_cloze.R con source() (E3: puerta; E6: mutantes
## MC18 a MC21). No lee ni escribe archivos.
##
## Por qué conducida por el código. salida/x_canonica_exacta.R calcula la batería cruzada en la canónica con el
## modelo de sorteos TRANSCRITO a mano (rama del otro colegio 1/2, combinación de hipótesis 1/3, ...). Una
## transcripción no ve un cambio del .Rmd que altere un sorteo (fijarlo, quitarlo o cambiar su soporte): X9, que
## en la canónica solo depende de cómo se sortea la rama del otro colegio, no tendría mutante posible. Aquí el
## peso de cada rama sale de la llamada al azar que el código ejecuta de verdad.
##
## Cómo. El bloque P1 se evalúa una vez con una semilla canónica (en la canónica la Parte 1 es fija salvo el orden
## de sus opciones, y ninguna regla depende de ese orden: el verificador lo comprueba con dos semillas). Después,
## las Partes 2 a 6 se evalúan una vez por hoja de un árbol de sorteos, con sample() y sample.int() sustituidos en
## el entorno del código (los heredan pick() y pick1(), que viven en ese entorno):
##   - CONTROLADOS: se ramifican todos sus resultados; el peso de cada uno es 1 / (n.º de resultados) de la llamada
##     que se ejecuta. Con prob o replace, la hoja falla: la enumeración no sabe ramificarlos.
##   - LIBRES: van al RNG. Dos deciden reglas y se integran EXACTAMENTE dentro de cada hoja (lo hace la función
##     f_hoja del verificador con lo que este archivo captura): la fila del caso paralelo, uniforme sobre las filas
##     de ep (X1 a X4), y los tres distractores de la Parte 2, uniformes sobre los C(5, 3) conjuntos (X5). El resto
##     (grados, fila del ganador, orden de mención, orden de opciones, reflexión) solo mueve nombres de grado, que no
##     comparten palabras con las opciones de la Parte 1 (guarda en f_hoja), y órdenes, que ninguna regla mira.
##   - Cualquier otro sorteo después del marcador del bloque P1 hace fallar la hoja: la enumeración no certifica.
##   - H4 de la FASE 2C r1 (2026-10-31): un sorteo NUEVO que repita el texto de uno libre pasaría como libre. Por eso
##     cada hoja cuenta las llamadas de cada sorteo libre y falla si no son exactamente LLAMADAS_LIBRES (mutante MC22).
## =============================================================================
SORTEOS_CONTROLADOS <- c(
  rama_par = 'sample(c("CURSOS", "PROMEDIO"), 1L)',
  orden_h  = 'sample.int(3L)',
  tipo_p2  = 'sample(names(P2), 1L)',
  clave_p2 = 'pick(q2$clave)',
  v_c      = 'sample(c(TRUE, FALSE), 1L)',
  c6       = 'pick1(cand_c)')
SORTEOS_LIBRES <- c(
  fila_par  = 'sample.int(nrow(ep), 1L)',
  grados    = 'sample.int(4L, 2L)',
  gi_m3     = 'sample.int(2L, 1L)',
  dis_p2    = 'sample.int(length(q2$distractores), 3L)',
  orden     = 'sample.int(4L)',
  reflexion = 'pick(reflexiones_cloze)')
## Llamadas que hace cada hoja de cada sorteo libre (gi_par y m3 comparten texto; o2 y o6 también).
LLAMADAS_LIBRES <- c(fila_par = 1L, grados = 1L, gi_m3 = 2L, dis_p2 = 1L, orden = 2L, reflexion = 1L)
.comprobar_libres <- function(ST) {
  vistos <- vapply(names(SORTEOS_LIBRES), function(k) sum(ST$libres == SORTEOS_LIBRES[[k]]), 0L)
  mal <- names(vistos)[vistos != LLAMADAS_LIBRES[names(vistos)]]
  if (length(mal)) stop("enumeración: sorteo libre con un número de llamadas distinto del esperado (",
    paste(sprintf("«%s» %d en vez de %d", SORTEOS_LIBRES[mal], vistos[mal], LLAMADAS_LIBRES[mal]), collapse = "; "),
    "): un sorteo nuevo con el texto de uno libre no se integra")
}

.perm_enum <- function(v) if (length(v) <= 1L) list(v) else
  do.call(c, lapply(seq_along(v), function(i) lapply(.perm_enum(v[-i]), function(p) c(v[i], p))))

## Identidad de un sorteo: la llamada tal como está escrita en el .Rmd; dentro de pick() o pick1(), la llamada a
## pick() o pick1() (pick(q2$clave) y pick(reflexiones_cloze) son sorteos distintos con la misma llamada interna).
.id_sorteo <- function(llamada, padre) {
  if (is.call(padre) && is.name(padre[[1L]]) && as.character(padre[[1L]]) %in% c("pick", "pick1"))
    return(paste(deparse(padre, width.cutoff = 500L), collapse = " "))
  paste(deparse(llamada, width.cutoff = 500L), collapse = " ")
}

.muestreador <- function(ST) {
  elegir <- function(id, pob, size, replace, prob) {
    if (isTRUE(replace) || !is.null(prob)) stop("enumeración: el sorteo controlado «", id, "» usa replace o prob")
    n <- length(pob); if (is.null(size)) size <- n
    outs <- if (size == 1L) as.list(pob) else if (size == n) lapply(.perm_enum(seq_len(n)), function(p) pob[p]) else
      stop("enumeración: el sorteo controlado «", id, "» pide ", size, " de ", n)
    ST$pos <- ST$pos + 1L
    i <- if (ST$pos <= length(ST$guion)) ST$guion[[ST$pos]] else 1L
    ST$traza[[ST$pos]] <- list(id = id, i = i, n = length(outs))
    outs[[i]]
  }
  libre <- function(id) { if (!id %in% SORTEOS_LIBRES) stop("enumeración: sorteo no reconocido después del bloque P1: ", id)
    ST$libres <- c(ST$libres, id) }
  list(
    sample = function(x, size, replace = FALSE, prob = NULL) {
      id <- .id_sorteo(sys.call(), sys.call(-1L))
      if (id %in% SORTEOS_CONTROLADOS) {
        pob <- if (length(x) == 1L && is.numeric(x) && x >= 1) seq_len(x) else x   # misma semántica que base::sample
        return(elegir(id, pob, if (missing(size)) NULL else size, replace, prob))
      }
      libre(id)
      if (missing(size)) base::sample(x, replace = replace, prob = prob) else base::sample(x, size, replace, prob)
    },
    sample.int = function(n, size = n, replace = FALSE, prob = NULL, useHash = FALSE) {
      id <- .id_sorteo(sys.call(), sys.call(-1L))
      if (id %in% SORTEOS_CONTROLADOS) return(elegir(id, seq_len(n), if (missing(size)) NULL else size, replace, prob))
      libre(id)
      if (id == SORTEOS_LIBRES[["fila_par"]]) ST$ep <- get("ep", envir = parent.frame())
      if (id == SORTEOS_LIBRES[["dis_p2"]]) ST$dis <- c(n = n, size = size)
      base::sample.int(n, size, replace, prob)
    })
}

## cod: el chunk data_generation completo (texto). marca: el marcador del bloque P1. semilla: una semilla que da la
## canónica. f_hoja(enB, ST): vector con el valor de cada regla en la hoja (enB, entorno de las Partes 2 a 6, cuyo
## padre es el del bloque P1; ST$ep y ST$dis, lo capturado de los sorteos libres que se integran).
## Devuelve las hojas: peso, error (o NULL), valores, ruta de sorteos controlados y lo que usan las guardas de los
## mutantes para comprobar que la mutación llegó al entorno.
enumerar_canonica <- function(cod, marca, semilla, f_hoja) {
  partes <- strsplit(cod, marca, fixed = TRUE)[[1]]
  if (length(partes) != 2L) stop("enumeración: el marcador del bloque P1 debe partir el código en dos")
  eA <- parse(text = partes[1], encoding = "UTF-8"); eB <- parse(text = partes[2], encoding = "UTF-8")
  set.seed(semilla); enA <- new.env(); eval(eA, envir = enA)
  if (!isTRUE(enA$es_canonica)) stop("enumeración: la semilla ", semilla, " no da la instancia canónica")
  ST <- new.env(); ms <- .muestreador(ST); enA$sample <- ms$sample; enA$sample.int <- ms$sample.int
  hojas <- list(); pila <- list(integer(0))
  while (length(pila)) {
    pref <- pila[[length(pila)]]; pila[[length(pila)]] <- NULL
    ST$guion <- pref; ST$pos <- 0L; ST$traza <- list(); ST$ep <- NULL; ST$dis <- NULL; ST$libres <- character(0)
    set.seed(semilla + 104729L)
    enB <- new.env(parent = enA)
    res <- tryCatch({ eval(eB, envir = enB); .comprobar_libres(ST); list(val = f_hoja(enB, ST), err = NULL) },
                    error = function(e) list(val = NULL, err = conditionMessage(e)))
    tr <- ST$traza
    el <- vapply(tr, function(t) t$i, 0L); no <- vapply(tr, function(t) t$n, 0L)
    hojas[[length(hojas) + 1L]] <- list(
      w = prod(1 / no), err = res$err, val = res$val,
      ruta = stats::setNames(el, names(SORTEOS_CONTROLADOS)[match(vapply(tr, function(t) t$id, ""), SORTEOS_CONTROLADOS)]),
      rama_par = enB$rama_par, clave_p2 = enB$clave_p2, libres = ST$libres,
      ventaja = if (is.list(enB$P6_VENTAJA)) vapply(enB$P6_VENTAJA, function(z) z$texto, "") else NULL)
    if (length(el) > length(pref)) for (q in (length(pref) + 1L):length(el)) for (alt in seq_len(no[q])[-1L])
      pila[[length(pila) + 1L]] <- c(el[seq_len(q - 1L)], alt)
  }
  hojas
}

## Valor exacto de cada regla (media ponderada de las hojas sin error), masa total y masa con error.
resumir_enumeracion <- function(hojas) {
  w <- vapply(hojas, function(h) h$w, 0); err <- vapply(hojas, function(h) !is.null(h$err), NA)
  V <- if (any(!err)) do.call(rbind, lapply(hojas[!err], function(h) h$val)) else NULL
  list(n_hojas = length(hojas), masa = sum(w), masa_err = sum(w[err]),
       errores = unique(unlist(lapply(hojas[err], function(h) h$err))),
       valores = if (is.null(V)) NULL else colSums(V * w[!err]) / sum(w[!err]))
}
