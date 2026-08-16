# =============================================================================
# Tests de .claude/scripts/bateria_eliminacion.R — cierre por familias de dimensión
# =============================================================================
# Origen: la misma lección apareció en DOS ejercicios distintos. Un verificador
# medía seis reglas de eliminación y NINGUNA tocaba la divisibilidad; el canal real
# (47,4 %) estaba justo en esa familia sin sonda, y el informe lo leyó como
# "sin señal". Regla #22 §P7.
#
# El control que da valor a esta suite es el caso "MISMOS DATOS, SONDA RETIRADA":
# demuestra que una batería incompleta reporta una cifra baja y tranquilizadora
# sobre un ítem que se resuelve al 100 % sin razonar.

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
# Sirve para colocar el máximo observado en un punto arbitrario, muy por encima del
# techo nulo pero cerca del umbral — el escenario donde el estadístico deja de ser
# reproducible tirada a tirada.
# El conteo es EXACTO (no un sorteo con probabilidad p): con n = 100 y p sorteada, la
# tasa realizada se desvía varios puntos —salió 61 % pidiendo 66 %— y arrastraría al
# test fuera de la banda que quiere probar. Es la misma irreproducibilidad que este
# test documenta, así que aquí se elimina por construcción.
fixture_canal_parcial <- function(p = 0.67, n = 100L, semilla = 23L) {
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
  # Sin canal, el máximo debe quedar pegado al techo nulo (exceso ~ 0).
  expect_lt(abs(res$exceso), 0.15)
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
  expect_lt(res$max_obs, 0.30)
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

test_that("BANDA DE INCERTIDUMBRE: a menos de 5 pp del umbral no se declara ni verde ni rojo", {
  # OJO al construir este caso: NO vale acercar el umbral al máximo de un ítem sano,
  # porque entonces el umbral cae por debajo del techo nulo y el veredicto correcto
  # pasa a ser UMBRAL_DEGENERADO (lo comprobó este mismo test al escribirlo). Hace
  # falta un canal PARCIAL: máximo muy por encima del ruido, pero rozando el umbral.
  fx <- fixture_canal_parcial(p = 0.67)
  res <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves, umbral = 0.70)

  expect_equal(res$veredicto, "NO_CONCLUYENTE")
  # Un NO_CONCLUYENTE jamás puede salir con 0: se leería como aprobado.
  expect_equal(exit_bateria(res), 1L)
  # El escenario debe ser el previsto: lejos del ruido y cerca del umbral.
  expect_gt(res$exceso, 0.20)
  expect_lt(abs(res$max_obs - 0.70), 0.05)

  # El MISMO dato, con el umbral lejos, sí concluye.
  res_lejos <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves, umbral = 0.90)
  expect_equal(res_lejos$veredicto, "PASS")
})

test_that("UMBRAL DEGENERADO: si el techo nulo alcanza el umbral, el umbral no discrimina", {
  fx <- fixture_sin_canal()
  # Umbral por debajo del techo nulo: hasta una batería de ruido lo cruzaría.
  res <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves, umbral = 0.10)
  expect_equal(res$veredicto, "UMBRAL_DEGENERADO")
  expect_equal(exit_bateria(res), 1L)
  expect_gte(res$techo_nulo, 0.10)
})

test_that("El techo nulo está por encima del azar puro (inflación por selección)", {
  # Con 4 opciones, una regla sola acierta ~25 % por azar. El MÁXIMO sobre varias
  # reglas está inflado: es justo la razón de calibrar en vez de comparar contra 25 %.
  fx <- fixture_sin_canal()
  res <- evaluar_bateria(BATERIA_COMPLETA, fx$opciones, fx$claves)
  # Sin inflación medible por encima del 25 %, la calibración no aportaría nada.
  expect_gt(res$techo_nulo, 0.25)
  expect_lt(res$techo_nulo, 0.60)
  expect_gte(res$techo_p95, res$techo_nulo)
})

test_that("El contrato de las reglas se valida: familia inválida y fn no-función", {
  expect_error(nueva_regla("x", "familia_inventada", function(o) 1L),
               regexp = "Familia desconocida")
  expect_error(nueva_regla("x", "magnitud", "no soy funcion"),
               regexp = "funcion")
})

test_that("Una regla que revienta se trata como abstención, no tumba la batería", {
  regla_rota <- nueva_regla("revienta", "magnitud", function(o) stop("boom"))
  fx <- fixture_sin_canal(n = 20L)
  res <- evaluar_bateria(list(regla_rota, regla_divis, regla_signo,
                              regla_posicion, regla_formato, regla_lexico),
                         fx$opciones, fx$claves)
  fila <- res$detalle[res$detalle$regla == "revienta", ]
  expect_equal(fila$tasa, 0)
  expect_equal(fila$aplicabilidad, 0)
})

test_that("Las seis familias del catálogo están declaradas y descritas", {
  expect_setequal(names(FAMILIAS_DIMENSION),
                  c("magnitud", "divisibilidad", "signo", "posicion", "formato", "lexico"))
  expect_true(all(nzchar(FAMILIAS_DIMENSION)),
              info = "una familia sin descripción no orienta a quien escribe la sonda")
})
