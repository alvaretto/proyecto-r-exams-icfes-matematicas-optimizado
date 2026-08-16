#!/usr/bin/env Rscript
# =============================================================================
# bateria_eliminacion.R — Cierre por FAMILIAS DE DIMENSIÓN, con techo nulo
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
# Este helper NO inventa las reglas —son propias de cada ejercicio, porque la
# divisibilidad sólo aplica a claves enteras, el signo sólo donde hay negativos y la
# posición sólo donde hay disposición espacial—. Lo que aporta es la parte GENÉRICA
# que cada verificador venía reinventando (y calibrando mal):
#
#   1. COBERTURA: qué familias tienen sonda y cuáles NO. Una familia sin sonda se
#      declara, nunca se reporta como "sin hallazgos".
#   2. TECHO NULO: un máximo sobre miles de combinaciones está inflado por selección.
#      Se calibra permutando CUÁL opción es la clave y dejando las reglas intactas.
#      Sin esa calibración el número no significa nada.
#   3. BANDA DE INCERTIDUMBRE: si el máximo cae a menos de 5 pp del umbral, con
#      N = 100 el estadístico es un máximo sobre ~19 reglas y NO es reproducible
#      tirada a tirada. Hay que decirlo, no ocultarlo.
#
# CONTRATO DE LAS REGLAS
# ----------------------
# Cada regla es `nueva_regla(nombre, familia, fn)`, donde `fn(opciones)` recibe el
# vector de opciones de UNA versión y devuelve:
#   - el índice entero de la opción que la regla elegiría, o
#   - NA_integer_ si la regla no aplica a esa versión (se abstiene).
# Abstenerse cuenta como fallo en la tasa global —un estudiante no puede usar una
# regla que no aplica— pero se reporta aparte como `aplicabilidad`.
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

nueva_regla <- function(nombre, familia, fn) {
  if (!familia %in% names(FAMILIAS_DIMENSION)) {
    stop(sprintf("Familia desconocida: '%s'. Validas: %s",
                 familia, paste(names(FAMILIAS_DIMENSION), collapse = ", ")))
  }
  if (!is.function(fn)) stop("fn debe ser una funcion(opciones) -> indice o NA")
  structure(list(nombre = nombre, familia = familia, fn = fn), class = "regla_elim")
}

# Aplica una regla a todas las versiones. Devuelve el índice elegido por versión.
.picks_de <- function(regla, opciones) {
  vapply(opciones, function(op) {
    r <- tryCatch(regla$fn(op), error = function(e) NA_integer_)
    if (length(r) != 1L || is.null(r)) return(NA_integer_)
    r <- suppressWarnings(as.integer(r))
    if (is.na(r) || r < 1L || r > length(op)) NA_integer_ else r
  }, integer(1))
}

# Tasa de acierto de un vector de elecciones contra un vector de claves.
# La abstención (NA) cuenta como fallo: el estudiante no puede usarla.
.tasa <- function(picks, claves) mean(!is.na(picks) & picks == claves)

#' Evalúa una batería de reglas de eliminación con cierre por familias.
#'
#' @param reglas    lista de objetos `nueva_regla`
#' @param opciones  lista de vectores de opciones, uno por versión
#' @param claves    vector entero: índice de la opción correcta por versión
#' @param umbral    tasa a partir de la cual una regla se considera canal real
#' @param n_perm    permutaciones para estimar el techo nulo
#' @param familias_no_aplicables  familias que el ejercicio declara inaplicables,
#'        CON su justificación: `c(signo = "todas las magnitudes son positivas")`
evaluar_bateria <- function(reglas, opciones, claves,
                            umbral = 0.70, n_perm = 200L,
                            familias_no_aplicables = character(0),
                            semilla = 20260815L) {

  stopifnot(length(reglas) > 0, length(opciones) == length(claves),
            length(opciones) > 0)
  n_ver <- length(opciones)

  # ---- 1. Tasa observada por regla -----------------------------------------
  picks <- lapply(reglas, .picks_de, opciones = opciones)
  tasas <- vapply(picks, .tasa, numeric(1), claves = claves)
  aplic <- vapply(picks, function(p) mean(!is.na(p)), numeric(1))
  nombres <- vapply(reglas, `[[`, character(1), "nombre")
  familias <- vapply(reglas, `[[`, character(1), "familia")

  detalle <- data.frame(regla = nombres, familia = familias,
                        tasa = tasas, aplicabilidad = aplic,
                        stringsAsFactors = FALSE)
  detalle <- detalle[order(-detalle$tasa), , drop = FALSE]
  max_obs <- max(tasas)
  regla_top <- nombres[which.max(tasas)]

  # ---- 2. Techo nulo -------------------------------------------------------
  # El máximo sobre k reglas está inflado por selección: con 4 opciones cada regla
  # acierta ~25 % por azar, pero el MÁXIMO de 19 reglas ronda el 35 %. Permutamos
  # cuál opción es la clave (reglas y opciones INTACTAS) y medimos ese máximo.
  set.seed(semilla)
  maximos_nulos <- vapply(seq_len(n_perm), function(b) {
    claves_falsas <- vapply(opciones, function(op) sample.int(length(op), 1L), integer(1))
    max(vapply(picks, .tasa, numeric(1), claves = claves_falsas))
  }, numeric(1))
  techo_nulo <- mean(maximos_nulos)
  techo_p95  <- unname(stats::quantile(maximos_nulos, 0.95))
  exceso     <- max_obs - techo_nulo

  # ---- 3. Cobertura por familias -------------------------------------------
  con_sonda   <- unique(familias)
  declaradas  <- names(familias_no_aplicables)
  sin_sonda   <- setdiff(names(FAMILIAS_DIMENSION), c(con_sonda, declaradas))

  # ---- 4. Veredicto --------------------------------------------------------
  # El orden importa: la falta de cobertura manda sobre cualquier cifra. Un máximo
  # bajo en una batería incompleta NO es "sin señal", es "sin sonda".
  banda <- 0.05
  umbral_degenerado <- techo_nulo >= umbral

  veredicto <- if (length(sin_sonda) > 0) {
    "SIN_COBERTURA"
  } else if (umbral_degenerado) {
    "UMBRAL_DEGENERADO"
  } else if (max_obs >= umbral && exceso > 0) {
    "BLOQUEA"
  } else if (abs(max_obs - umbral) < banda) {
    "NO_CONCLUYENTE"
  } else {
    "PASS"
  }

  structure(list(
    veredicto = veredicto, detalle = detalle,
    max_obs = max_obs, regla_top = regla_top,
    techo_nulo = techo_nulo, techo_p95 = techo_p95, exceso = exceso,
    umbral = umbral, banda = banda, n_versiones = n_ver, n_reglas = length(reglas),
    familias_con_sonda = con_sonda, familias_sin_sonda = sin_sonda,
    familias_no_aplicables = familias_no_aplicables,
    umbral_degenerado = umbral_degenerado
  ), class = "bateria_elim")
}

pct <- function(x) sprintf("%.1f %%", 100 * x)

imprimir_bateria <- function(res) {
  cat("\n========================================\n")
  cat("BATERIA DE ELIMINACION — cierre por familias\n")
  cat("========================================\n")
  cat(sprintf("Versiones: %d | Reglas: %d | Umbral: %s\n",
              res$n_versiones, res$n_reglas, pct(res$umbral)))

  cat("\n-- Reglas (ordenadas por tasa) --\n")
  for (i in seq_len(nrow(res$detalle))) {
    d <- res$detalle[i, ]
    cat(sprintf("  %-28s [%-13s] tasa %-8s aplicable %s\n",
                d$regla, d$familia, pct(d$tasa), pct(d$aplicabilidad)))
  }

  cat("\n-- Calibracion contra el techo nulo --\n")
  cat(sprintf("  Maximo observado : %s  (regla '%s')\n", pct(res$max_obs), res$regla_top))
  cat(sprintf("  Techo nulo       : %s  (media del maximo con la clave permutada)\n",
              pct(res$techo_nulo)))
  cat(sprintf("  Techo nulo p95   : %s\n", pct(res$techo_p95)))
  cat(sprintf("  EXCESO           : %+.1f pp\n", 100 * res$exceso))
  cat("  Sin esta calibracion el maximo NO significa nada: un maximo sobre muchas\n")
  cat("  reglas esta inflado por seleccion, no por senal del item.\n")

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
      cat(sprintf("  El techo nulo (%s) alcanza o supera el umbral (%s): el umbral no\n",
                  pct(res$techo_nulo), pct(res$umbral)))
      cat("  discrimina nada — hasta una bateria de ruido lo cruzaria. Subir el umbral\n")
      cat("  por encima del techo nulo o reducir el numero de reglas.\n")
    },
    BLOQUEA = {
      cat("  BLOQUEA (exit 1)\n")
      cat(sprintf("  '%s' [%s] acierta el %s, %+.1f pp sobre el techo nulo.\n",
                  res$regla_top,
                  res$detalle$familia[1], pct(res$max_obs), 100 * res$exceso))
      cat("  Hay un canal real de eliminacion: el item se resuelve sin razonar.\n")
    },
    NO_CONCLUYENTE = {
      cat("  NO_CONCLUYENTE (exit 1)\n")
      cat(sprintf("  El maximo (%s) queda a %.1f pp del umbral (%s), dentro de la banda\n",
                  pct(res$max_obs), 100 * abs(res$max_obs - res$umbral), pct(res$umbral)))
      cat(sprintf("  de +-%s. Con esta N el estadistico es un MAXIMO sobre %d reglas y NO\n",
                  pct(res$banda), res$n_reglas))
      cat("  es reproducible tirada a tirada. NO se declara ni verde ni rojo: hay que\n")
      cat("  subir N o reducir la bateria. Prohibido redondear a PASS.\n")
    },
    PASS = {
      cat("  PASS (exit 0)\n")
      cat(sprintf("  Maximo %s bajo umbral %s, exceso %+.1f pp sobre el techo nulo,\n",
                  pct(res$max_obs), pct(res$umbral), 100 * res$exceso))
      cat("  y las seis familias tienen sonda o declaracion. Este PASS si acredita.\n")
    })
  cat("\n")
  invisible(res)
}

exit_bateria <- function(res) if (identical(res$veredicto, "PASS")) 0L else 1L
