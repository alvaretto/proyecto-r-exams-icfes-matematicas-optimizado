# =============================================================================
# Tests de .claude/scripts/bateria_eliminacion.R — cierre por familias de dimensión
# y VEREDICTO POR EXCESO sobre el techo nulo
# =============================================================================
# Origen (§P7): la misma lección apareció en DOS ejercicios distintos. Un
# verificador medía seis reglas de eliminación y NINGUNA tocaba la divisibilidad;
# el canal real (47,4 %) estaba justo en esa familia sin sonda, y el informe lo
# leyó como "sin señal". Regla #22 §P7.
#
# Origen del criterio por EXCESO (2026-08-15): una medición sobre 468 ítems
# oficiales demostró que un umbral de TASA ABSOLUTA mide, en parte, el tamaño de
# la batería — la misma población da 27,4 % con 19 reglas y 34,8 % con 25.
#
# Los dos controles que dan valor a esta suite:
#   · "MISMOS DATOS, SONDA RETIRADA": una batería incompleta reporta una cifra baja
#     y tranquilizadora sobre un ítem que se resuelve al 100 % sin razonar.
#   · "MÁS REGLAS DE RUIDO NO CAMBIAN EL VEREDICTO": es la lección medida hecha
#     aserción. Con el criterio viejo, añadir ruido cruzaba el umbral.

library(testthat)

repo_root <- tryCatch(system("git rev-parse --show-toplevel", intern = TRUE),
                      error = function(e) getwd())
if (length(repo_root) == 1 && nzchar(repo_root)) setwd(repo_root)
source(".claude/scripts/bateria_eliminacion.R")

# ---- Batería de referencia: una regla por familia ---------------------------
regla_magnitud <- nueva_regla("la mayor", "magnitud",
                              function(o) which.max(as.numeric(o)))
regla_divis <- nueva_regla("la unica par", "divisibilidad", function(o) {
  x <- as.numeric(o); i <- which(x %% 2 == 0); if (length(i) == 1L) i else NA_integer_
})
regla_signo <- nueva_regla("la negativa", "signo", function(o) {
  i <- which(as.numeric(o) < 0); if (length(i) == 1L) i else NA_integer_
})
regla_posicion <- nueva_regla("la primera", "posicion", function(o) 1L)
regla_formato <- nueva_regla("la de 3 cifras", "formato", function(o) {
  i <- which(nchar(gsub("[^0-9]", "", o)) == 3L); if (length(i) == 1L) i else NA_integer_
})
regla_lexico <- nueva_regla("contiene 7", "lexico", function(o) {
  i <- grep("7", o); if (length(i) == 1L) i else NA_integer_
})
BATERIA_COMPLETA <- list(regla_magnitud, regla_divis, regla_signo,
                         regla_posicion, regla_formato, regla_lexico)

# Reglas de PURO RUIDO, INDEPENDIENTES entre sí: cada una lleva su propia tabla de
# consulta sorteada al construirla y elige según un hash del contenido de la versión.
# No guardan relación con la clave, así que sólo inflan el máximo por selección —
# exactamente el efecto que el criterio por exceso debe anular.
# OJO: la independencia es lo que hace útil al fixture. Una versión anterior derivaba
# el índice de `i` con aritmética lineal y todas las reglas colapsaban en cuatro
# comportamientos distintos, así que ni el máximo subía ni el techo nulo saturaba:
# el fixture parecía tener k reglas y tenía cuatro.
.hash_ops <- function(o) sum(vapply(o, function(s)
  sum(as.integer(charToRaw(s))), numeric(1)) * seq_along(o))
reglas_ruido <- function(k, semilla = 4242L) {
  set.seed(semilla)
  fams <- names(FAMILIAS_DIMENSION)
  lapply(seq_len(k), function(i) {
    tabla <- sample.int(4L, 512L, replace = TRUE)
    nueva_regla(paste0("ruido", i), fams[1L + (i - 1L) %% length(fams)],
                local({ tb <- tabla; function(o)
                  1L + (tb[1L + (.hash_ops(o) %% 512)] - 1L) %% length(o) }))
  })
}

# Ítem con canal REAL en divisibilidad: la clave es siempre el único número par.
fixture_canal_divisibilidad <- function(n = 100L, semilla = 7L) {
  set.seed(semilla)
  ops <- vector("list", n); claves <- integer(n)
  for (i in seq_len(n)) {
    v <- c(sample(seq(11, 99, by = 2), 3), sample(seq(12, 98, by = 2), 1))
    ord <- sample(4)
    ops[[i]] <- as.character(v[ord]); claves[i] <- which(ord == 4L)
  }
  list(opciones = ops, claves = claves)
}

# Canal PARCIAL: la clave es el único par en una fracción `p` de las versiones.
# Sirve para colocar el exceso en un punto arbitrario — en particular DENTRO de la
# zona gris, que es donde el veredicto no puede ser ni verde ni rojo.
# El conteo es EXACTO (no un sorteo con probabilidad p): con n = 100 y p sorteada, la
# tasa realizada se desvía varios puntos y arrastraría al test fuera de la banda que
# quiere probar. Es la misma irreproducibilidad que este test documenta.
fixture_canal_parcial <- function(p = 0.34, n = 100L, semilla = 23L) {
  set.seed(semilla)
  n_canal <- round(p * n)
  con_canal <- sample(c(rep(TRUE, n_canal), rep(FALSE, n - n_canal)))
  ops <- vector("list", n); claves <- integer(n)
  for (i in seq_len(n)) {
    v <- c(sample(seq(11, 99, by = 2), 3), sample(seq(12, 98, by = 2), 1))
    ord <- sample(4)
    ops[[i]] <- as.character(v[ord])
    pos_par <- which(ord == 4L)
    claves[i] <- if (con_canal[i]) pos_par else sample(setdiff(seq_len(4), pos_par), 1)
  }
  list(opciones = ops, claves = claves)
}

# Ítem sano: la clave no guarda relación con ninguna familia.
fixture_sin_canal <- function(n = 100L, semilla = 11L) {
  set.seed(semilla)
  ops <- lapply(seq_len(n), function(i) as.character(sample(10:99, 4)))
  list(opciones = ops, claves = vapply(seq_len(n), function(i) sample(4, 1), integer(1)))
}

# -----------------------------------------------------------------------------

test_that("LA CONVENCIÓN TIENE NULO EXACTO: E[score] = 1/n para toda regla", {
  # Es la propiedad que hace comparables las tasas entre reglas, baterías y
  # poblaciones — y la razón de no puntuar 0/1 con la abstención valiendo 0.
  # Se comprueba por ENUMERACIÓN de todos los tamaños de conjunto superviviente.
  for (n in c(3L, 4L, 5L)) {
    for (k in 0:n) {
      regla <- nueva_regla("S fijo", "magnitud",
                           local({ kk <- k; function(o) if (kk == 0L) NA_integer_ else seq_len(kk) }))
      # Una versión por cada posición posible de la clave: la media sobre ellas
      # ES la esperanza bajo clave uniforme.
      ops <- rep(list(as.character(seq_len(n))), n)
      M <- .matriz_score(regla, ops, rep(n, n), n)
      expect_equal(mean(M[cbind(seq_len(n), seq_len(n))]), 1 / n,
                   info = sprintf("n = %d, |S| = %d", n, k))
    }
  }
})

test_that("CONTROL POSITIVO: caza un canal real de eliminación", {
  fx <- fixture_canal_divisibilidad()
  res <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves)

  expect_equal(res$veredicto, "BLOQUEA")
  expect_equal(exit_bateria(res), 1L)
  expect_equal(res$max_obs, 1)
  expect_equal(res$detalle$familia[1], "divisibilidad",
               info = "no atribuyó el canal a la familia correcta")
  expect_gt(res$exceso, 0.5)
})

test_that("CONTROL NEGATIVO: un ítem sano da PASS y no un rojo falso", {
  fx <- fixture_sin_canal()
  res <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves)

  expect_equal(res$veredicto, "PASS")
  expect_equal(exit_bateria(res), 0L)
  # Sin canal, el máximo debe quedar pegado al techo nulo (exceso dentro del ruido).
  expect_lt(res$exceso, res$corte_ruido)
})

test_that("LA LECCIÓN MEDIDA: más reglas de RUIDO suben la tasa pero NO el exceso", {
  # Éste es el test del cambio de criterio. Con el umbral absoluto viejo, añadir
  # reglas que no saben nada del ítem acercaba (y podía cruzar) la puerta. El
  # exceso no se mueve porque el techo nulo sube con el máximo.
  fx <- fixture_sin_canal()
  res_6  <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves)
  res_46 <- evaluar_bateria(c(BATERIA_COMPLETA, reglas_ruido(40)),
                            fx$opciones, fx$claves)

  # 1. La tasa absoluta SÍ sube al añadir ruido: es el defecto que se corrige.
  expect_gt(res_46$max_obs, res_6$max_obs + 0.03)
  # 2. El techo nulo sube con ella...
  expect_gt(res_46$techo_nulo, res_6$techo_nulo + 0.03)
  # 3. ...así que el exceso apenas se mueve, y el veredicto NO cambia.
  expect_lt(abs(res_46$exceso - res_6$exceso), 0.05)
  expect_equal(res_6$veredicto, "PASS")
  expect_equal(res_46$veredicto, "PASS")

  # 4. Control de que el escenario es real: con un umbral ABSOLUTO intermedio, las
  #    dos baterías caerían a lados distintos de la puerta sobre los MISMOS datos.
  umbral_viejo <- (res_6$max_obs + res_46$max_obs) / 2
  expect_lt(res_6$max_obs,  umbral_viejo)
  expect_gt(res_46$max_obs, umbral_viejo)
})

test_that("LA LECCIÓN: batería incompleta NO es 'sin señal', es 'sin sonda'", {
  # Mismos datos que el control positivo (canal real del 100 %), pero se retira la
  # sonda de divisibilidad — exactamente lo que pasó en el incidente.
  fx <- fixture_canal_divisibilidad()
  sin_divis <- list(regla_magnitud, regla_signo, regla_posicion,
                    regla_formato, regla_lexico)
  res <- evaluar_bateria(sin_divis, fx$opciones, fx$claves)

  # La cifra que reporta es baja y tranquilizadora. Si esta aserción cae, el fixture
  # dejó de reproducir el escenario: el máximo debía desplomarse al retirar la sonda.
  expect_lt(res$max_obs, 0.32)
  # ...y aun así NO puede declararse PASS.
  expect_equal(res$veredicto, "SIN_COBERTURA")
  expect_equal(exit_bateria(res), 1L)
  expect_true("divisibilidad" %in% res$familias_sin_sonda)
})

test_that("Una familia declarada no aplicable cierra la cobertura, pero exige justificación", {
  fx <- fixture_sin_canal()
  sin_signo <- list(regla_magnitud, regla_divis, regla_posicion,
                    regla_formato, regla_lexico)

  # Sin declararla: bloquea por falta de cobertura.
  res_mudo <- evaluar_bateria(sin_signo, fx$opciones, fx$claves)
  expect_equal(res_mudo$veredicto, "SIN_COBERTURA")
  expect_true("signo" %in% res_mudo$familias_sin_sonda)

  # Declarada con su razón: la cobertura queda cerrada.
  res_decl <- evaluar_bateria(
    sin_signo, fx$opciones, fx$claves,
    familias_no_aplicables = c(signo = "todas las magnitudes del item son positivas"))
  expect_equal(res_decl$veredicto, "PASS")
  expect_length(res_decl$familias_sin_sonda, 0)
})

test_that("ZONA GRIS: un exceso entre los dos cortes no es ni verde ni rojo", {
  # Canal parcial calibrado para dejar el exceso por encima del ruido (+2 pp) y por
  # debajo del corte de canal (+8 pp). Ahí NO se decide: exit 1, nunca PASS.
  fx <- fixture_canal_parcial(p = 0.34)
  res <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves)

  expect_equal(res$veredicto, "NO_CONCLUYENTE")
  # Un NO_CONCLUYENTE jamás puede salir con 0: se leería como aprobado.
  expect_equal(exit_bateria(res), 1L)
  # El escenario debe ser el previsto: dentro de la banda, no pegado a un borde.
  expect_gt(res$exceso, res$corte_ruido)
  expect_lt(res$exceso, res$corte_canal)

  # Los MISMOS datos con los cortes ensanchados sí acreditan: es el corte, y no la
  # tasa absoluta, lo que decide.
  expect_equal(evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves,
                               corte_ruido = 0.06, corte_canal = 0.30)$veredicto, "PASS")

  # Y con los cortes POR DEFECTO, un canal más penetrante sí llega a BLOQUEA: las
  # tres regiones existen y están ordenadas.
  fuerte <- fixture_canal_parcial(p = 0.55)
  res_f <- evaluar_bateria(BATERIA_COMPLETA, fuerte$opciones, fuerte$claves)
  expect_equal(res_f$veredicto, "BLOQUEA")
  expect_gt(res_f$exceso, res$exceso)

  # NO se puede forzar un BLOQUEA sobre la zona gris bajando el corte: el margen de
  # 2 sd del ruido medido (~2,7 pp aquí) lo impide. Un exceso de ~5 pp con ~3 pp de
  # ruido no acredita un canal por mucho que se mueva la puerta.
  expect_equal(evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves,
                               corte_ruido = 0.005, corte_canal = 0.02)$veredicto,
               "NO_CONCLUYENTE")
})

test_that("MUESTRA CORTA: si el ruido no resuelve la banda, no hay PASS aunque el exceso sea cero", {
  # A N = 30 la dispersión del techo nulo ronda 4-5 pp, así que sumarle 2 sd al
  # exceso alcanza el corte de canal: esta muestra no puede acreditar la AUSENCIA.
  # Es la misma disciplina que la regla #23 impone a los estratos con n < 20.
  fx <- fixture_sin_canal(n = 30L, semilla = 3L)
  res <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves)

  expect_lt(res$exceso, res$corte_ruido)          # el exceso está dentro del ruido...
  expect_gt(res$ruido, 0.03)                      # ...pero el ruido es ancho
  expect_equal(res$veredicto, "NO_CONCLUYENTE")   # ...así que NO se acredita
  expect_equal(exit_bateria(res), 1L)

  # Los mismos datos, la misma batería, con la muestra estándar N = 100: sí acredita.
  expect_equal(evaluar_bateria(BATERIA_COMPLETA, fixture_sin_canal()$opciones,
                               fixture_sin_canal()$claves)$veredicto, "PASS")
})

test_that("UMBRAL DEGENERADO: si el techo nulo satura, el corte es inalcanzable", {
  # Muchas reglas para muy pocas versiones: alguna acierta las 3 por azar en casi
  # toda permutación, el techo nulo roza el 100 % y ni una regla omnisciente
  # produciría un exceso suficiente. El criterio no discrimina nada.
  set.seed(5L)
  ops <- lapply(1:3, function(i) as.character(sample(10:99, 4)))
  res <- evaluar_bateria(reglas_ruido(200), ops, c(1L, 2L, 3L))

  expect_equal(res$veredicto, "UMBRAL_DEGENERADO")
  expect_equal(exit_bateria(res), 1L)
  expect_lt(1 - res$techo_nulo, res$corte_canal)
})

test_that("El techo nulo está por encima del azar puro (inflación por selección)", {
  # Con 4 opciones, la convención 1/|S| da exactamente 25 % en esperanza a CADA
  # regla. El MÁXIMO sobre varias está inflado: es justo la razón de calibrar en
  # vez de comparar contra el 25 %.
  fx <- fixture_sin_canal()
  res <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves)
  expect_gt(res$techo_nulo, 0.25)
  expect_lt(res$techo_nulo, 0.60)
  expect_gte(res$techo_p95, res$techo_nulo)
  # Y crece con el número de reglas: por eso la tasa absoluta no es comparable.
  res_46 <- evaluar_bateria(c(BATERIA_COMPLETA, reglas_ruido(40)), fx$opciones, fx$claves)
  expect_gt(res_46$techo_nulo, res$techo_nulo)
})

test_that("El ruido reportado ES la anchura real del exceso bajo H0", {
  # `ruido` sale de la dispersión del techo nulo. Si no coincidiera con la
  # dispersión empírica del exceso, los márgenes de 2 sd no significarían nada y
  # el guardián de muestra corta sería decorativo.
  emp <- vapply(1:40, function(b) {
    fx <- fixture_sin_canal(n = 100L, semilla = 500L + b)
    evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves,
                    n_perm = 120L, semilla = 900L + b)$exceso
  }, numeric(1))
  reportado <- evaluar_bateria(BATERIA_COMPLETA, fixture_sin_canal()$opciones,
                               fixture_sin_canal()$claves)$ruido
  expect_lt(abs(stats::sd(emp) - reportado), 0.015)
  expect_lt(abs(mean(emp)), 0.02)   # el exceso bajo H0 está centrado en cero
})

test_that("El contrato de las reglas se valida: familia inválida y fn no-función", {
  expect_error(nueva_regla("x", "familia_inventada", function(o) 1L),
               regexp = "Familia desconocida")
  expect_error(nueva_regla("x", "magnitud", "no soy funcion"),
               regexp = "funcion")
  expect_error(evaluar_bateria(BATERIA_COMPLETA, fixture_sin_canal()$opciones,
                               fixture_sin_canal()$claves,
                               corte_ruido = 0.20, corte_canal = 0.05))
})

test_that("Una regla que revienta se trata como abstención y puntúa el azar exacto", {
  # Bajo la convención 1/|S|, no eliminar nada paga 1/n: es el azar, no un cero.
  # Puntuarlo 0 haría que el nulo de cada regla dependiera de su abstención, que
  # es justo lo que rompía la comparabilidad entre baterías.
  regla_rota <- nueva_regla("revienta", "magnitud", function(o) stop("boom"))
  fx <- fixture_sin_canal(n = 20L)
  res <- evaluar_bateria(list(regla_rota, regla_divis, regla_signo,
                              regla_posicion, regla_formato, regla_lexico),
                         fx$opciones, fx$claves)
  fila <- res$detalle[res$detalle$regla == "revienta", ]
  expect_equal(fila$tasa, 0.25)
  expect_equal(fila$aplicabilidad, 0)
})

test_that("La regla puede devolver un CONJUNTO y se puntúa 1/|S|", {
  # Es lo que permite pasar directamente las reglas de los verificadores por
  # ejercicio (que devuelven un lógico de supervivientes) sin sortear entre ellos:
  # ese sorteo añadía varianza y era la razón declarada de que las dos cifras
  # fueran incomparables.
  ops <- rep(list(c("11", "22", "33", "44")), 4L)
  dos <- nueva_regla("sobreviven dos", "magnitud", function(o) c(TRUE, TRUE, FALSE, FALSE))
  M <- .matriz_score(dos, ops, rep(4L, 4L), 4L)
  expect_equal(M[1, ], c(0.5, 0.5, 0, 0))
  res <- evaluar_bateria(list(dos, regla_divis, regla_signo, regla_posicion,
                              regla_formato, regla_lexico),
                         ops, c(1L, 1L, 3L, 3L), n_perm = 50L)
  expect_equal(res$detalle$tasa[res$detalle$regla == "sobreviven dos"], 0.25)
})

test_that("Las seis familias del catálogo están declaradas y descritas", {
  expect_setequal(names(FAMILIAS_DIMENSION),
                  c("magnitud", "divisibilidad", "signo", "posicion", "formato", "lexico"))
  expect_true(all(nzchar(FAMILIAS_DIMENSION)),
              info = "una familia sin descripción no orienta a quien escribe la sonda")
})

test_that("RESIDUO DECLARADO: rellenar la batería sube el techo y erosiona el exceso", {
  # El criterio por exceso tiene un vector de evasión conocido: añadir reglas que
  # ningún estudiante usaría sube el techo nulo SIN mover el máximo observado, así
  # que el exceso baja. El helper no puede detectarlo —no sabe qué regla es
  # plausible— pero publica `exceso_atomico`, que no depende de k y NO se mueve.
  # Este test fija el mecanismo para que nadie lo descubra por sorpresa.
  fx <- fixture_canal_parcial(p = 0.55)
  base <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves)
  pad  <- evaluar_bateria(c(BATERIA_COMPLETA, reglas_ruido(300)), fx$opciones, fx$claves)

  expect_equal(pad$max_obs, base$max_obs)             # el máximo no se mueve...
  expect_gt(pad$techo_nulo, base$techo_nulo + 0.03)   # ...pero el techo sube...
  expect_lt(pad$exceso, base$exceso - 0.03)           # ...y el exceso se erosiona.
  # La cifra que NO se deja rellenar: la regla top contra su propio nulo exacto.
  expect_equal(pad$exceso_atomico, base$exceso_atomico)
  expect_equal(base$azar_exacto, 0.25)
  # Aquí el canal es tan fuerte que aguanta 300 reglas de relleno; el punto del
  # test es el MECANISMO, no que este caso concreto llegue a enmascararse.
  expect_equal(pad$veredicto, "BLOQUEA")
})

test_that("Los cortes son los calibrados y el veredicto NO usa la tasa absoluta", {
  expect_equal(CORTE_RUIDO, 0.02)
  expect_equal(CORTE_CANAL, 0.08)
  expect_gt(CORTE_CANAL, CORTE_RUIDO)
  # `umbral` sobrevive como cifra informativa: cambiarlo NO puede mover el veredicto.
  fx <- fixture_canal_parcial(p = 0.34)
  v1 <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves, umbral = 0.10)$veredicto
  v2 <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves, umbral = 0.99)$veredicto
  expect_equal(v1, v2)
})
