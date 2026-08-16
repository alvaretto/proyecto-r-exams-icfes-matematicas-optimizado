#!/usr/bin/env Rscript
# =============================================================================
# bateria_eliminacion.R — Cierre por FAMILIAS DE DIMENSIÓN, veredicto por EXCESO
# =============================================================================
#
# QUÉ RESUELVE
# ------------
# Un ítem puede resolverse "sin leer" no sólo por la vía que miden H1/H2/H3/H3b de
# `validar_diagnosticidad.R` (longitud, prefijo, veredicto, contenido), sino por
# cualquier regla de eliminación que el estudiante descubra. La lección, aparecida
# DOS VECES en dos ejercicios distintos:
#
#   > Una batería de reglas de eliminación necesita CIERRE POR FAMILIAS DE DIMENSIÓN
#   > (magnitud, divisibilidad, signo, posición, formato, léxico), no sólo por mínimo
#   > y máximo.
#
# En el incidente que la originó, el verificador medía seis reglas intra-celda y
# NINGUNA tocaba la divisibilidad; el canal real (47,4 %) estaba justo en esa familia
# sin sonda. Por eso el principio operativo es:
#
#   > una batería incompleta no mide «sin señal», mide «SIN SONDA».
#
# POR QUÉ EL VEREDICTO SE DECIDE POR EL EXCESO Y NO POR LA TASA (2026-08-15)
# --------------------------------------------------------------------------
# La primera versión de este helper bloqueaba por TASA ABSOLUTA (umbral 70 %). Una
# medición sobre **468 ítems oficiales únicos** de 6 cuadernillos de Matemáticas
# ICFES demostró que ese umbral mide, en buena parte, **el tamaño de la batería**:
#
#   | Población                              |  k | tasa   | techo nulo | exceso |
#   |----------------------------------------|----|--------|------------|--------|
#   | Oficiales 468, vara universal          | 25 | 34,8 % |   27,8 %   |  +7,0  |
#   | Oficiales 468, sin familia posición    | 19 | 27,4 % |   27,0 %   |  +0,4  |
#   | Ejercicio en revisión, vara universal  | 25 | 30,4 % |   31,5 %   |  -1,1  |
#   | Ejercicio en revisión, vara valor      | 20 | 31,5 % |   30,6 %   |  +0,9  |
#   | Oficiales 42 numéricos, vara valor     | 20 | 31,0 % |   33,0 %   |  -2,1  |
#   | Ejercicio en revisión, batería completa| 91 | 47,0 % |   ~35 %    |  ~+12  |
#
# La MISMA población da 27,4 % con 19 reglas y 34,8 % con 25 — y su techo nulo se
# mueve con ella (27,0 → 27,8). **El exceso es invariante; la tasa absoluta no.**
# Además, ningún ítem de ninguna población, con 19 a 91 reglas, superó el 47,0 %:
# un umbral del 70 % exigiría aislar la clave en 3 de cada 4 instancias. Eso no es
# severo, es INALCANZABLE — y una puerta que no se puede cruzar nunca se aprende a
# ignorar igual que una que siempre está en rojo.
#
# CONVENCIÓN DE PUNTUACIÓN (por qué ésta y no 0/1)
# ------------------------------------------------
#   score = 1/|S| si la clave sobrevive · 0 si no · 1/n si |S| = 0 (nadie sobrevive)
#
# Con n opciones y clave uniforme, **E[score] = 1/n EXACTAMENTE para toda regla**,
# sea cual sea su selectividad: P(clave ∈ S) = |S|/n y el pago es 1/|S|. Verificable
# por enumeración. Esa es la propiedad que hace comparables las tasas entre reglas,
# entre baterías y entre poblaciones. La convención 0/1 con la abstención puntuando
# 0 NO la tiene: el nulo de cada regla depende de cuánto se abstenga, así que el
# máximo de la batería mezclaba reglas con nulos distintos. Es también la convención
# de `acierto()` en los verificadores por ejercicio: ahora las dos cifras SON
# comparables, que es lo que antes obligaba a declararlas incomparables.
#
# CONTRATO DE LAS REGLAS
# ----------------------
# Cada regla es `nueva_regla(nombre, familia, fn)`, donde `fn(opciones)` recibe el
# vector de opciones de UNA versión y devuelve el CONJUNTO SUPERVIVIENTE, en
# cualquiera de estas tres formas:
#   - vector lógico de la misma longitud que `opciones` (TRUE = sobrevive);
#   - un índice entero (la regla se queda con UNA opción);
#   - NA / vector vacío (la regla no aplica: no elimina nada).
#
# Ver regla #22 §P7 (`.claude/rules/diversidad-sustantiva.md`).
# Muestra estándar: N = 100 (regla #23).
# =============================================================================

# Las seis familias de dimensión por las que una batería debe cerrar. Añadir una
# familia aquí obliga a que TODA batería la declare (con sonda o como no aplicable).
FAMILIAS_DIMENSION <- c(
  magnitud      = "tamano relativo: la mas larga/corta, la mayor/menor, la de mas cifras",
  divisibilidad = "estructura aritmetica: par/impar, multiplo de 5/10, entero vs decimal",
  signo         = "signo y sentido: negativo/positivo, aumenta/disminuye, si/no",
  posicion      = "lugar: cuadrante, orden, celda de la grilla, primera/ultima",
  formato       = "molde: unidades, notacion, tipo de grafico, presencia de cifras",
  lexico        = "vocabulario: token exclusivo de la clave, registro, prefijo"
)

# --- CORTES CALIBRADOS -------------------------------------------------------
# Calibrados contra las 6 poblaciones de la tabla de cabecera y contra la
# distribución del exceso bajo H0 (sin canal), medida por simulación a N = 100:
#   media +0,1 pp · sd 2,2 pp · p90 +2,9 pp · p95 +4,0 pp · máx de 500 réplicas +9,6 pp
#
#   CORTE_RUIDO = +2 pp  ~ p88 del ruido. Ahí caen las CUATRO comparaciones limpias
#                          de la tabla (+0,4 · -1,1 · +0,9 · -2,1): son cero medido.
#   CORTE_CANAL = +8 pp  ~ p99,5 del ruido a N = 100, y justo por encima del +7,0 pp
#                          que miden los 468 ítems OFICIALES con la familia posición
#                          incluida (sesgo posicional real de los cuadernillos, no
#                          ruido). Un ítem que no filtra más que el corpus oficial no
#                          se declara defectuoso; uno que filtra más, sí.
# La franja intermedia NO es un empate técnico que se resuelva a favor: es
# NO_CONCLUYENTE, que sale con exit 1 igual que el bloqueo.
CORTE_RUIDO <- 0.02
CORTE_CANAL <- 0.08
# Márgenes en desviaciones del ruido MEDIDO en esta corrida (no en una constante):
# es lo que impide acreditar un PASS con una muestra que no resuelve la banda.
K_SIGMA <- 2

nueva_regla <- function(nombre, familia, fn) {
  if (!familia %in% names(FAMILIAS_DIMENSION)) {
    stop(sprintf("Familia desconocida: '%s'. Validas: %s",
                 familia, paste(names(FAMILIAS_DIMENSION), collapse = ", ")))
  }
  if (!is.function(fn)) stop("fn debe ser una funcion(opciones) -> conjunto, indice o NA")
  structure(list(nombre = nombre, familia = familia, fn = fn), class = "regla_elim")
}

# Normaliza la salida de `fn` al conjunto superviviente (lógico de longitud n).
.conjunto_de <- function(r, n) {
  if (is.null(r)) return(rep(FALSE, n))
  if (is.logical(r) && length(r) == n) { r[is.na(r)] <- FALSE; return(r) }
  idx <- suppressWarnings(as.integer(r))
  idx <- idx[!is.na(idx) & idx >= 1L & idx <= n]
  s <- rep(FALSE, n); if (length(idx)) s[idx] <- TRUE
  s
}

# Matriz n_versiones x max_opciones con el score que la regla pagaría si la clave
# estuviera en cada posición. Precalcularla hace que las permutaciones del nulo
# sean re-indexado puro: 400 réplicas cuestan lo que costaba 1.
.matriz_score <- function(regla, opciones, n_op, maxn) {
  M <- matrix(NA_real_, nrow = length(opciones), ncol = maxn)
  for (i in seq_along(opciones)) {
    n <- n_op[i]
    S <- tryCatch(.conjunto_de(regla$fn(opciones[[i]]), n),
                  error = function(e) rep(FALSE, n))
    k <- sum(S)
    # |S| = 0 (nadie sobrevive) y |S| = n (no elimina nada) pagan lo mismo: 1/n.
    M[i, seq_len(n)] <- if (k == 0L) rep(1 / n, n) else ifelse(S, 1 / k, 0)
  }
  M
}

.tasa <- function(M, claves) mean(M[cbind(seq_len(nrow(M)), claves)])

#' Evalúa una batería de reglas de eliminación con cierre por familias.
#'
#' El veredicto lo decide el EXCESO del máximo observado sobre el techo nulo, NO
#' la tasa absoluta: la tasa depende del número de reglas, el exceso no.
#'
#' @param reglas    lista de objetos `nueva_regla`
#' @param opciones  lista de vectores de opciones, uno por versión
#' @param claves    vector entero: índice de la opción correcta por versión
#' @param corte_canal  exceso (en tanto por uno) a partir del cual hay canal real
#' @param corte_ruido  exceso por debajo del cual el hallazgo es indistinguible del ruido
#' @param k_sigma   márgenes exigidos, en desviaciones del ruido medido
#' @param n_perm    permutaciones para estimar el techo nulo
#' @param familias_no_aplicables  familias que el ejercicio declara inaplicables,
#'        CON su justificación: `c(signo = "todas las magnitudes son positivas")`
#' @param umbral    tasa absoluta de referencia. **INFORMATIVA: no decide nada.**
#'        Se conserva para poder imprimir la cifra que los verificadores antiguos
#'        citaban, y para dejar constancia de que ya no gobierna.
evaluar_bateria <- function(reglas, opciones, claves,
                            corte_canal = CORTE_CANAL,
                            corte_ruido = CORTE_RUIDO,
                            k_sigma = K_SIGMA,
                            n_perm = 400L,
                            familias_no_aplicables = character(0),
                            umbral = 0.70,
                            semilla = 20260815L) {

  stopifnot(length(reglas) > 0, length(opciones) == length(claves),
            length(opciones) > 0, corte_ruido < corte_canal)
  n_ver <- length(opciones)
  n_op  <- vapply(opciones, length, integer(1))
  maxn  <- max(n_op)

  # ---- 1. Tasa observada por regla -----------------------------------------
  MATS  <- lapply(reglas, .matriz_score, opciones = opciones, n_op = n_op, maxn = maxn)
  tasas <- vapply(MATS, .tasa, numeric(1), claves = claves)
  # Aplicabilidad: fracción de versiones en que la regla DICE algo (elimina, pero
  # no lo elimina todo). Una regla que nunca aplica puntúa el azar exacto.
  aplic <- vapply(seq_along(reglas), function(j) {
    mean(vapply(seq_len(n_ver), function(i) {
      s <- .conjunto_de(tryCatch(reglas[[j]]$fn(opciones[[i]]),
                                 error = function(e) NULL), n_op[i])
      sum(s) > 0L && sum(s) < n_op[i]
    }, logical(1)))
  }, numeric(1))

  nombres  <- vapply(reglas, `[[`, character(1), "nombre")
  familias <- vapply(reglas, `[[`, character(1), "familia")
  detalle  <- data.frame(regla = nombres, familia = familias,
                         tasa = tasas, aplicabilidad = aplic,
                         stringsAsFactors = FALSE)
  detalle   <- detalle[order(-detalle$tasa), , drop = FALSE]
  max_obs   <- max(tasas)
  regla_top <- nombres[which.max(tasas)]

  # ---- 2. Techo nulo -------------------------------------------------------
  # El máximo sobre k reglas está inflado por SELECCIÓN: con la convención 1/|S|
  # cada regla vale exactamente 1/n en esperanza, pero el MÁXIMO de decenas de
  # reglas queda muy por encima. Se permuta CUÁL opción es la clave dejando las
  # reglas intactas: eso mide cuánto sube el estadístico sin que haya canal.
  #
  # El nulo es UNIFORME sobre las posiciones, no una permutación del vector de
  # claves observado, Y ESO ES DELIBERADO: permutar el vector observado preservaría
  # su marginal y volvería invisible el canal posicional puro ("la clave siempre
  # está en la primera"), que es justo uno de los que la familia `posicion` existe
  # para cazar. El precio es que un sesgo posicional real del corpus cuenta como
  # exceso — y por eso los 468 ítems oficiales miden +7,0 pp: está contemplado en
  # la calibración de `corte_canal`, no es un defecto de la sonda.
  # Se guarda y restaura `.Random.seed`: este helper se invoca DESDE verificadores
  # que ya están sorteando, y dejarles el RNG reseteado cambiaría sus cifras sin
  # que nada lo indique. Es la regla #10 de `codigo-rmd.md` aplicada al arsenal.
  .semilla_previa <- if (exists(".Random.seed", envir = globalenv()))
    get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (!is.null(.semilla_previa)) assign(".Random.seed", .semilla_previa, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(semilla)
  maximos_nulos <- vapply(seq_len(n_perm), function(b) {
    claves_falsas <- vapply(n_op, function(k) sample.int(k, 1L), integer(1))
    max(vapply(MATS, .tasa, numeric(1), claves = claves_falsas))
  }, numeric(1))
  techo_nulo <- mean(maximos_nulos)
  techo_p95  <- unname(stats::quantile(maximos_nulos, 0.95))
  ruido      <- stats::sd(maximos_nulos)
  exceso     <- max_obs - techo_nulo
  margen     <- k_sigma * ruido
  # Exceso de la regla top sobre su PROPIO nulo exacto (1/n), que no depende de k.
  # Se publica junto al otro porque el criterio por exceso tiene un vector de evasion
  # conocido: RELLENAR la bateria con reglas que nadie usaria sube el techo nulo sin
  # mover el maximo, y puede empujar un canal real a la zona gris. El helper NO puede
  # detectarlo —no sabe que regla es plausible para un estudiante— pero las dos cifras
  # juntas lo hacen visible: en una bateria rellenada, este numero se queda grande
  # mientras el exceso sobre el techo se desploma. Ver §P7 «residuo declarado».
  azar_exacto    <- mean(1 / n_op)
  exceso_atomico <- max_obs - azar_exacto

  # ---- 3. Cobertura por familias -------------------------------------------
  con_sonda   <- unique(familias)
  declaradas  <- names(familias_no_aplicables)
  sin_sonda   <- setdiff(names(FAMILIAS_DIMENSION), c(con_sonda, declaradas))

  # ---- 4. Veredicto --------------------------------------------------------
  # El orden importa: la falta de cobertura manda sobre cualquier cifra. Un máximo
  # bajo en una batería incompleta NO es "sin señal", es "sin sonda".
  #
  # `UMBRAL_DEGENERADO` cambia de causa, no de espíritu: antes disparaba cuando el
  # techo nulo alcanzaba un umbral ABSOLUTO (una batería de ruido lo cruzaba); ahora
  # dispara cuando el techo nulo SATURA y deja menos margen que el propio corte —
  # entonces ni una regla omnisciente (100 %) produciría un exceso suficiente, y el
  # corte es inalcanzable. Las dos formas dicen lo mismo: el criterio no discrimina.
  # Se llega ahí con demasiadas reglas para demasiado pocas versiones (regla #23).
  margen_disponible <- 1 - techo_nulo
  umbral_degenerado <- margen_disponible < corte_canal

  veredicto <- if (length(sin_sonda) > 0) {
    "SIN_COBERTURA"
  } else if (umbral_degenerado) {
    "UMBRAL_DEGENERADO"
  } else if (exceso >= corte_canal && exceso - margen > corte_ruido) {
    "BLOQUEA"
  } else if (exceso > corte_ruido) {
    "NO_CONCLUYENTE"                       # zona gris: ni cero medido ni canal
  } else if (exceso + margen >= corte_canal) {
    "NO_CONCLUYENTE"                       # el ruido no resuelve la banda
  } else {
    "PASS"
  }

  structure(list(
    veredicto = veredicto, detalle = detalle,
    max_obs = max_obs, regla_top = regla_top,
    techo_nulo = techo_nulo, techo_p95 = techo_p95, ruido = ruido,
    exceso = exceso, margen = margen, k_sigma = k_sigma,
    azar_exacto = azar_exacto, exceso_atomico = exceso_atomico,
    corte_canal = corte_canal, corte_ruido = corte_ruido,
    umbral = umbral, n_versiones = n_ver, n_reglas = length(reglas),
    n_perm = n_perm,
    familias_con_sonda = con_sonda, familias_sin_sonda = sin_sonda,
    familias_no_aplicables = familias_no_aplicables,
    umbral_degenerado = umbral_degenerado
  ), class = "bateria_elim")
}

pct <- function(x) sprintf("%.1f %%", 100 * x)

imprimir_bateria <- function(res) {
  cat("\n========================================\n")
  cat("BATERIA DE ELIMINACION — cierre por familias, veredicto por EXCESO\n")
  cat("========================================\n")
  cat(sprintf("Versiones: %d | Reglas: %d | Cortes: ruido %s / canal %s\n",
              res$n_versiones, res$n_reglas, pct(res$corte_ruido), pct(res$corte_canal)))

  cat("\n-- Reglas (ordenadas por tasa) --\n")
  for (i in seq_len(nrow(res$detalle))) {
    d <- res$detalle[i, ]
    cat(sprintf("  %-28s [%-13s] tasa %-8s aplicable %s\n",
                d$regla, d$familia, pct(d$tasa), pct(d$aplicabilidad)))
  }

  cat("\n-- Calibracion contra el techo nulo --\n")
  cat(sprintf("  Maximo observado : %s  (regla '%s')\n", pct(res$max_obs), res$regla_top))
  cat(sprintf("  Techo nulo       : %s  (media del maximo con la clave permutada, %d replicas)\n",
              pct(res$techo_nulo), res$n_perm))
  cat(sprintf("  Techo nulo p95   : %s\n", pct(res$techo_p95)))
  cat(sprintf("  Ruido (sd)       : %s   -> margen exigido %.1f sd = %+.1f pp\n",
              pct(res$ruido), res$k_sigma, 100 * res$margen))
  cat(sprintf("  EXCESO           : %+.1f pp   <-- ESTO decide el veredicto\n", 100 * res$exceso))
  cat(sprintf("  Exceso ATOMICO   : %+.1f pp  (la regla top sobre su propio nulo exacto, %s;\n",
              100 * res$exceso_atomico, pct(res$azar_exacto)))
  cat("                     no depende del numero de reglas). Si esta cifra es grande y\n")
  cat("                     el EXCESO no, sospechar bateria RELLENADA con reglas que\n")
  cat("                     ningun estudiante usaria: suben el techo sin mover el maximo.\n")
  cat(sprintf("  [informativo, NO decide] tasa absoluta %s frente al viejo umbral %s\n",
              pct(res$max_obs), pct(res$umbral)))
  cat("  La tasa absoluta crece con el NUMERO DE REGLAS: medido sobre 468 items\n")
  cat("  oficiales, la misma poblacion da 27,4 % con 19 reglas y 34,8 % con 25, y\n")
  cat("  su techo nulo se mueve con ella. El exceso es lo invariante.\n")

  cat("\n-- Cobertura por familias de dimension --\n")
  for (f in names(FAMILIAS_DIMENSION)) {
    estado <- if (f %in% res$familias_con_sonda) "CON SONDA"
              else if (f %in% names(res$familias_no_aplicables)) "no aplicable (declarada)"
              else "*** SIN SONDA ***"
    cat(sprintf("  %-14s %-26s %s\n", f, estado, FAMILIAS_DIMENSION[[f]]))
  }
  if (length(res$familias_no_aplicables)) {
    cat("\n  Justificaciones declaradas:\n")
    for (f in names(res$familias_no_aplicables)) {
      cat(sprintf("    %s: %s\n", f, res$familias_no_aplicables[[f]]))
    }
  }

  cat("\n-- VEREDICTO --\n")
  switch(res$veredicto,
    SIN_COBERTURA = {
      cat("  SIN_COBERTURA (exit 1)\n")
      cat(sprintf("  Familias sin sonda: %s\n", paste(res$familias_sin_sonda, collapse = ", ")))
      cat("  Esta bateria NO mide 'sin senal', mide SIN SONDA. El canal real puede\n")
      cat("  estar justo en la familia que nadie sondeo. Anadir sonda, o declarar la\n")
      cat("  familia como no aplicable CON justificacion. PROHIBIDO leerlo como PASS.\n")
    },
    UMBRAL_DEGENERADO = {
      cat("  UMBRAL_DEGENERADO (exit 1)\n")
      cat(sprintf("  El techo nulo (%s) deja solo %.1f pp de margen, menos que el corte de\n",
                  pct(res$techo_nulo), 100 * (1 - res$techo_nulo)))
      cat(sprintf("  canal (%+.1f pp): ni una regla omnisciente (100 %%) produciria un exceso\n",
                  100 * res$corte_canal))
      cat("  suficiente. El criterio NO discrimina nada. Causa habitual: demasiadas\n")
      cat("  reglas para demasiado pocas versiones — subir N (regla #23 fija 100) o\n")
      cat("  reducir la bateria.\n")
    },
    BLOQUEA = {
      cat("  BLOQUEA (exit 1)\n")
      cat(sprintf("  '%s' [%s] acierta el %s: %+.1f pp sobre el techo nulo,\n",
                  res$regla_top, res$detalle$familia[1], pct(res$max_obs), 100 * res$exceso))
      cat(sprintf("  con el corte de canal en %+.1f pp y %.1f sd de ruido descontadas.\n",
                  100 * res$corte_canal, res$k_sigma))
      cat("  Hay un canal real de eliminacion: el item se resuelve sin razonar, y por\n")
      cat("  encima de lo que filtran los propios cuadernillos oficiales (+7,0 pp).\n")
    },
    NO_CONCLUYENTE = {
      cat("  NO_CONCLUYENTE (exit 1)\n")
      if (res$exceso > res$corte_ruido && res$exceso < res$corte_canal) {
        cat(sprintf("  El exceso (%+.1f pp) cae en la ZONA GRIS (%+.1f a %+.1f pp): por encima\n",
                    100 * res$exceso, 100 * res$corte_ruido, 100 * res$corte_canal))
        cat("  del ruido pero por debajo de lo que acredita un canal. Ni verde ni rojo.\n")
      } else if (res$exceso >= res$corte_canal) {
        cat(sprintf("  El exceso (%+.1f pp) alcanza el corte, pero el ruido medido (%s, %.1f sd\n",
                    100 * res$exceso, pct(res$ruido), res$k_sigma))
        cat(sprintf("  = %+.1f pp) lo devuelve a la zona gris: no se puede AFIRMAR el canal\n",
                    100 * res$margen))
        cat("  con esta muestra. Sigue sin ser PASS.\n")
      } else {
        cat(sprintf("  El exceso (%+.1f pp) esta dentro del ruido, pero el ruido mismo (%s,\n",
                    100 * res$exceso, pct(res$ruido)))
        cat(sprintf("  %.1f sd = %+.1f pp) es tan ancho que sumandolo alcanzaria el corte de\n",
                    res$k_sigma, 100 * res$margen))
        cat("  canal: esta muestra NO puede acreditar la ausencia. Subir N (regla #23).\n")
      }
      cat("  PROHIBIDO redondearlo a PASS: sale con exit 1 igual que el bloqueo.\n")
    },
    PASS = {
      cat("  PASS (exit 0)\n")
      cat(sprintf("  Exceso %+.1f pp sobre el techo nulo (%s), por debajo del corte de ruido\n",
                  100 * res$exceso, pct(res$techo_nulo)))
      cat(sprintf("  (%+.1f pp) incluso sumando %.1f sd del ruido medido (%+.1f pp), y las seis\n",
                  100 * res$corte_ruido, res$k_sigma, 100 * res$margen))
      cat("  familias tienen sonda o declaracion. Este PASS si acredita.\n")
    })
  cat("\n")
  invisible(res)
}

exit_bateria <- function(res) if (identical(res$veredicto, "PASS")) 0L else 1L
