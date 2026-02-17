# Tests unitarios para Validación Semántica (Nivel 4)
# Cobertura: REGLAS_SEMANTICAS_KEYWORDS, 3 capas (A/B/C), función orquestadora
# Versión: 1.0
# Fecha: 2026-02-13

library(testthat)

# Cargar funciones del script de validación
source("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/validar_coherencia_matematica.R")

# ============================================================
# TESTS DE INFRAESTRUCTURA
# ============================================================

test_that("REGLAS_SEMANTICAS_KEYWORDS existe y tiene estructura correcta", {
  expect_true(exists("REGLAS_SEMANTICAS_KEYWORDS"))
  expect_true(is.list(REGLAS_SEMANTICAS_KEYWORDS))
  expect_true(length(REGLAS_SEMANTICAS_KEYWORDS) >= 6)

  for (i in seq_along(REGLAS_SEMANTICAS_KEYWORDS)) {
    regla <- REGLAS_SEMANTICAS_KEYWORDS[[i]]
    expect_true(!is.null(regla$patron),
      info = paste("Regla", i, "sin campo 'patron'"))
    expect_true(!is.null(regla$nombre),
      info = paste("Regla", i, "sin campo 'nombre'"))
    expect_true(is.function(regla$condicion),
      info = paste("Regla", i, ": 'condicion' no es función"))
    expect_true(!is.null(regla$mensaje),
      info = paste("Regla", i, "sin campo 'mensaje'"))
  }
})

test_that("Funciones de las 3 capas existen", {
  expect_true(exists("validar_capa_a_precondicion"))
  expect_true(is.function(validar_capa_a_precondicion))
  expect_true(exists("validar_capa_b_keywords"))
  expect_true(is.function(validar_capa_b_keywords))
  expect_true(exists("validar_capa_c_crossval"))
  expect_true(is.function(validar_capa_c_crossval))
  expect_true(exists("validar_precondiciones_error_pool"))
  expect_true(is.function(validar_precondiciones_error_pool))
})

test_that("Funciones auxiliares existen", {
  expect_true(exists("construir_params_desde_env"))
  expect_true(is.function(construir_params_desde_env))
  expect_true(exists("detectar_error_seleccionado"))
  expect_true(is.function(detectar_error_seleccionado))
})

# ============================================================
# TESTS DE KEYWORD RULES INDIVIDUALES
# ============================================================

test_that("Regla n_par detecta 'número par' en descripción", {
  regla <- Filter(function(r) r$nombre == "n_par", REGLAS_SEMANTICAS_KEYWORDS)[[1]]

  # Debe matchear estas descripciones
  expect_true(grepl(regla$patron, "Para un número par de datos", ignore.case = TRUE, perl = TRUE))
  expect_true(grepl(regla$patron, "tomó solo uno de los dos valores centrales", ignore.case = TRUE, perl = TRUE))
  expect_true(grepl(regla$patron, "par de datos ordenados", ignore.case = TRUE, perl = TRUE))

  # Condición: n par
  expect_true(regla$condicion(list(n = 8)))
  expect_false(regla$condicion(list(n = 7)))
  expect_null(regla$condicion(list()))  # Sin n → NULL (no verificable)
})

test_that("Regla n_impar detecta 'número impar' en descripción", {
  regla <- Filter(function(r) r$nombre == "n_impar", REGLAS_SEMANTICAS_KEYWORDS)[[1]]

  expect_true(grepl(regla$patron, "un solo valor central", ignore.case = TRUE, perl = TRUE))
  expect_true(grepl(regla$patron, "número impar de datos", ignore.case = TRUE, perl = TRUE))

  expect_true(regla$condicion(list(n = 7)))
  expect_false(regla$condicion(list(n = 8)))
  expect_null(regla$condicion(list()))
})

test_that("Regla moda_unica detecta 'unimodal' en descripción", {
  regla <- Filter(function(r) r$nombre == "moda_unica", REGLAS_SEMANTICAS_KEYWORDS)[[1]]

  expect_true(grepl(regla$patron, "moda única del conjunto", ignore.case = TRUE, perl = TRUE))
  expect_true(grepl(regla$patron, "datos unimodal", ignore.case = TRUE, perl = TRUE))

  # Datos unimodales: 3 aparece 3 veces (más que cualquier otro)
  expect_true(regla$condicion(list(datos_ord = c(1, 2, 3, 3, 3, 5))))
  # Datos bimodales: 2 y 4 aparecen 2 veces cada uno
  expect_false(regla$condicion(list(datos_ord = c(1, 2, 2, 4, 4, 5))))
  expect_null(regla$condicion(list()))
})

test_that("Regla bimodal detecta 'bimodal' en descripción", {
  regla <- Filter(function(r) r$nombre == "bimodal", REGLAS_SEMANTICAS_KEYWORDS)[[1]]

  expect_true(grepl(regla$patron, "distribución bimodal", ignore.case = TRUE, perl = TRUE))
  expect_true(grepl(regla$patron, "tiene dos modas", ignore.case = TRUE, perl = TRUE))

  expect_true(regla$condicion(list(datos_ord = c(1, 2, 2, 4, 4, 5))))
  expect_false(regla$condicion(list(datos_ord = c(1, 2, 3, 3, 3, 5))))
  expect_null(regla$condicion(list()))
})

test_that("Regla multimodal detecta 'múltiples modas' en descripción", {
  regla <- Filter(function(r) r$nombre == "multimodal", REGLAS_SEMANTICAS_KEYWORDS)[[1]]

  expect_true(grepl(regla$patron, "datos multimodal", ignore.case = TRUE, perl = TRUE))
  expect_true(grepl(regla$patron, "varias modas distintas", ignore.case = TRUE, perl = TRUE))

  # Trimodal: 1, 3 y 5 aparecen 2 veces
  expect_true(regla$condicion(list(datos_ord = c(1, 1, 3, 3, 5, 5))))
  # Unimodal
  expect_false(regla$condicion(list(datos_ord = c(1, 2, 3, 3, 3, 5))))
  expect_null(regla$condicion(list()))
})

test_that("Regla datos_iguales detecta 'todos iguales' en descripción", {
  regla <- Filter(function(r) r$nombre == "datos_iguales", REGLAS_SEMANTICAS_KEYWORDS)[[1]]

  expect_true(grepl(regla$patron, "todos los datos son iguales", ignore.case = TRUE, perl = TRUE))
  expect_true(grepl(regla$patron, "sin variabilidad", ignore.case = TRUE, perl = TRUE))

  expect_true(regla$condicion(list(datos_ord = c(5, 5, 5, 5))))
  expect_false(regla$condicion(list(datos_ord = c(5, 5, 5, 6))))
  expect_null(regla$condicion(list()))
})

# ============================================================
# TESTS DE CAPA A: PRECONDICIÓN DECLARADA
# ============================================================

test_that("Capa A: detecta precondición que no se cumple (ERR_SEM_A)", {
  pool <- list(
    list(
      codigo = "TEST-01",
      precondicion = function(params) params$n %% 2 == 0  # Solo n par
    )
  )
  error_sel <- pool[[1]]
  params <- list(n = 7)  # n impar → precondición falla

  errores <- validar_capa_a_precondicion(pool, error_sel, params)
  expect_true(length(errores) > 0)
  expect_true(any(grepl("ERR_SEM_A", errores)))
})

test_that("Capa A: acepta precondición que se cumple", {
  pool <- list(
    list(
      codigo = "TEST-01",
      precondicion = function(params) params$n %% 2 == 0
    )
  )
  error_sel <- pool[[1]]
  params <- list(n = 8)  # n par → precondición cumple

  errores <- validar_capa_a_precondicion(pool, error_sel, params)
  expect_equal(length(errores), 0)
})

test_that("Capa A: acepta error sin precondición", {
  pool <- list(list(codigo = "TEST-01"))
  error_sel <- pool[[1]]
  params <- list(n = 7)

  errores <- validar_capa_a_precondicion(pool, error_sel, params)
  expect_equal(length(errores), 0)
})

test_that("Capa A: acepta NULL error_sel", {
  pool <- list(list(codigo = "TEST-01"))
  errores <- validar_capa_a_precondicion(pool, NULL, list())
  expect_equal(length(errores), 0)
})

test_that("Capa A: maneja excepción en precondición", {
  pool <- list(
    list(
      codigo = "TEST-01",
      precondicion = function(params) stop("error forzado")
    )
  )
  error_sel <- pool[[1]]
  params <- list(n = 7)

  errores <- validar_capa_a_precondicion(pool, error_sel, params)
  expect_true(length(errores) > 0)
  expect_true(any(grepl("ERR_SEM_A", errores)))
})

# ============================================================
# TESTS DE CAPA B: KEYWORD SCANNER
# ============================================================

test_that("Capa B: detecta error SELECCIONADO incoherente (ERR_SEM_B)", {
  # Error dice "número par" pero n=7 (impar) y NO tiene precondición
  pool <- list(
    list(
      codigo = "EST-MTC-04",
      descripcion_corta = "Para un número par de datos, tomó solo uno de los dos valores centrales",
      descripcion_larga = ""
    )
  )
  error_sel <- pool[[1]]
  params <- list(n = 7)

  errores <- validar_capa_b_keywords(pool, error_sel, params)
  expect_true(any(grepl("ERR_SEM_B", errores)),
    info = "Debe detectar error SELECCIONADO con descripción 'par' cuando n=7")
})

test_that("Capa B: detecta bug latente en pool (WARN_SEM_B)", {
  # Error en pool dice "número par" pero n=7 y NO tiene precondición
  # PERO no es el seleccionado → WARN, no ERR
  pool <- list(
    list(
      codigo = "EST-MTC-04",
      descripcion_corta = "Para un número par de datos",
      descripcion_larga = ""
    ),
    list(
      codigo = "EST-MTC-01",
      descripcion_corta = "Confundió promedio con valor",
      descripcion_larga = ""
    )
  )
  error_sel <- pool[[2]]  # El seleccionado es otro
  params <- list(n = 7)

  errores <- validar_capa_b_keywords(pool, error_sel, params)
  expect_true(any(grepl("WARN_SEM_B", errores)),
    info = "Debe emitir WARN para error no seleccionado con descripción incoherente")
  expect_false(any(grepl("ERR_SEM_B", errores)),
    info = "NO debe emitir ERR para error no seleccionado")
})

test_that("Capa B: NO reporta si precondición protege el error", {
  # Error dice "número par", n=7, pero tiene precondicion que lo excluye
  pool <- list(
    list(
      codigo = "EST-MTC-04",
      descripcion_corta = "Para un número par de datos",
      descripcion_larga = "",
      precondicion = function(params) params$n %% 2 == 0  # Excluye n impar
    )
  )
  error_sel <- pool[[1]]
  params <- list(n = 7)

  errores <- validar_capa_b_keywords(pool, error_sel, params)
  expect_equal(length(errores), 0,
    info = "No debe reportar si precondicion previene la selección")
})

test_that("Capa B: acepta cuando condición se cumple", {
  pool <- list(
    list(
      codigo = "EST-MTC-04",
      descripcion_corta = "Para un número par de datos",
      descripcion_larga = ""
    )
  )
  error_sel <- pool[[1]]
  params <- list(n = 8)  # n par → condición se cumple

  errores <- validar_capa_b_keywords(pool, error_sel, params)
  expect_equal(length(errores), 0)
})

test_that("Capa B: maneja pool sin descripciones", {
  pool <- list(list(codigo = "TEST-01"))
  error_sel <- pool[[1]]
  params <- list(n = 7)

  errores <- validar_capa_b_keywords(pool, error_sel, params)
  expect_equal(length(errores), 0)
})

test_that("Capa B: reproduce el bug original EST-MTC-04 con n=7", {
  # Caso real que motivó la creación del sistema semántico
  pool <- list(
    list(
      codigo = "EST-MTC-01",
      descripcion_corta = "Confundió el promedio con uno de los valores del conjunto",
      descripcion_larga = "El estudiante interpretó erróneamente un valor del conjunto como el promedio"
    ),
    list(
      codigo = "EST-MTC-02",
      descripcion_corta = "Calculó la mediana sin ordenar los datos previamente",
      descripcion_larga = "Seleccionó el valor central de los datos desordenados"
    ),
    list(
      codigo = "EST-MTC-03",
      descripcion_corta = "Confundió la mediana con la moda",
      descripcion_larga = "Seleccionó el valor que más se repite en lugar de la mediana"
    ),
    list(
      codigo = "EST-MTC-04",
      descripcion_corta = "Para un número par de datos, tomó solo uno de los dos valores centrales",
      descripcion_larga = "En lugar de promediar los dos datos centrales, seleccionó solo uno"
      # SIN precondicion → bug latente
    )
  )
  error_sel <- pool[[4]]  # EST-MTC-04 seleccionado con n=7
  params <- list(n = 7, datos_ord = c(50, 55, 60, 65, 70, 75, 80))

  errores <- validar_capa_b_keywords(pool, error_sel, params)
  expect_true(any(grepl("ERR_SEM_B", errores)),
    info = "Debe detectar el bug original: EST-MTC-04 seleccionado con n=7")
  expect_true(any(grepl("EST-MTC-04", errores)))
})

# ============================================================
# TESTS DE CAPA C: CROSS-VALIDACIÓN
# ============================================================

test_that("Capa C: detecta calcula() que produce el mismo valor que correcto (ERR_SEM_C)", {
  env <- new.env(parent = emptyenv())
  env$mediana_calc <- 65
  env$mediana_erronea <- 65  # Mismo valor → error inútil
  env$errores_conceptuales <- list(
    list(codigo = "TEST-01")
  )
  env$error_sel <- env$errores_conceptuales[[1]]

  errores <- validar_capa_c_crossval(
    env$errores_conceptuales, env$error_sel, env
  )
  expect_true(any(grepl("ERR_SEM_C", errores)))
})

test_that("Capa C: acepta calcula() que produce valor diferente", {
  env <- new.env(parent = emptyenv())
  env$mediana_calc <- 65
  env$mediana_erronea <- 60  # Diferente → OK
  env$errores_conceptuales <- list(
    list(codigo = "TEST-01")
  )
  env$error_sel <- env$errores_conceptuales[[1]]

  errores <- validar_capa_c_crossval(
    env$errores_conceptuales, env$error_sel, env
  )
  expect_equal(length(errores), 0)
})

test_that("Capa C: maneja ausencia de variables correcta/errónea", {
  env <- new.env(parent = emptyenv())
  # Sin mediana_calc ni mediana_erronea
  pool <- list(list(codigo = "TEST-01"))
  error_sel <- pool[[1]]

  errores <- validar_capa_c_crossval(pool, error_sel, env)
  expect_equal(length(errores), 0)  # No puede verificar → no reporta error
})

# ============================================================
# TESTS DE FUNCIÓN ORQUESTADORA
# ============================================================

test_that("Orquestadora: retorna vacío si no hay pool de errores", {
  env <- new.env(parent = emptyenv())
  env$x <- 5  # Variable cualquiera, sin errores_conceptuales

  errores <- validar_precondiciones_error_pool(env)
  expect_equal(length(errores), 0)
})

test_that("Orquestadora: ejecuta las 3 capas sobre pool válido", {
  env <- new.env(parent = emptyenv())
  env$n <- 8
  env$datos_ord <- c(50, 55, 60, 65, 70, 75, 80, 85)
  env$mediana_calc <- 67.5
  env$mediana_erronea <- 65

  env$errores_conceptuales <- list(
    list(
      codigo = "EST-MTC-04",
      descripcion_corta = "Para un número par de datos, tomó solo uno de los dos valores centrales",
      descripcion_larga = "",
      precondicion = function(params) params$n %% 2 == 0,
      calcula = function(datos_ord) datos_ord[length(datos_ord) / 2]
    )
  )
  env$error_sel <- env$errores_conceptuales[[1]]

  errores <- validar_precondiciones_error_pool(env)
  expect_equal(length(errores), 0,
    info = "Pool válido con n par + precondición correcta no debe generar errores")
})

test_that("Orquestadora: detecta ERR_SEM_A y ERR_SEM_B simultáneamente", {
  env <- new.env(parent = emptyenv())
  env$n <- 7  # impar
  env$datos_ord <- c(50, 55, 60, 65, 70, 75, 80)
  env$mediana_calc <- 65
  env$mediana_erronea <- 60

  env$errores_conceptuales <- list(
    list(
      codigo = "EST-MTC-04",
      descripcion_corta = "Para un número par de datos, tomó solo uno de los dos valores centrales",
      descripcion_larga = "",
      precondicion = function(params) params$n %% 2 == 0  # n impar → falla
    )
  )
  env$error_sel <- env$errores_conceptuales[[1]]

  errores <- validar_precondiciones_error_pool(env)
  expect_true(any(grepl("ERR_SEM_A", errores)),
    info = "Capa A debe detectar precondición que no se cumple")
  # Capa B no reporta porque precondicion sí protege (previene la selección)
})

test_that("Orquestadora: separa ERR de WARN correctamente", {
  env <- new.env(parent = emptyenv())
  env$n <- 7
  env$datos_ord <- c(50, 55, 60, 65, 70, 75, 80)
  env$mediana_calc <- 65
  env$mediana_erronea <- 60

  env$errores_conceptuales <- list(
    list(
      codigo = "EST-MTC-01",
      descripcion_corta = "Confundió promedio con valor",
      descripcion_larga = ""
    ),
    list(
      codigo = "EST-MTC-04",
      descripcion_corta = "Para un número par de datos, tomó solo uno de los dos valores centrales",
      descripcion_larga = ""
      # SIN precondicion → desprotegido
    )
  )
  env$error_sel <- env$errores_conceptuales[[1]]  # Seleccionado es EST-MTC-01

  errores <- validar_precondiciones_error_pool(env)
  err_bloqueantes <- errores[grepl("^ERR_SEM", errores)]
  warnings <- errores[grepl("^WARN_SEM", errores)]

  expect_equal(length(err_bloqueantes), 0,
    info = "EST-MTC-01 seleccionado no tiene problemas semánticos")
  expect_true(length(warnings) > 0,
    info = "EST-MTC-04 desprotegido debe generar WARN_SEM_B")
})

# ============================================================
# TESTS DE HELPER: construir_params_desde_env
# ============================================================

test_that("construir_params_desde_env extrae n y datos_ord", {
  env <- new.env(parent = emptyenv())
  env$n <- 7
  env$datos_ord <- c(1, 2, 3, 4, 5, 6, 7)

  params <- construir_params_desde_env(env)
  expect_equal(params$n, 7)
  expect_equal(params$datos_ord, c(1, 2, 3, 4, 5, 6, 7))
})

test_that("construir_params_desde_env reconoce alias num_estudiantes", {
  env <- new.env(parent = emptyenv())
  env$num_estudiantes <- 10

  params <- construir_params_desde_env(env)
  expect_equal(params$n, 10)
})

test_that("construir_params_desde_env ordena datos si solo hay datos desordenados", {
  env <- new.env(parent = emptyenv())
  env$datos <- c(5, 3, 1, 4, 2)

  params <- construir_params_desde_env(env)
  expect_equal(params$datos_ord, c(1, 2, 3, 4, 5))
})

test_that("construir_params_desde_env retorna lista vacía si no hay variables", {
  env <- new.env(parent = emptyenv())
  env$x <- "irrelevante"

  params <- construir_params_desde_env(env)
  expect_null(params$n)
  expect_null(params$datos_ord)
})

# ============================================================
# TESTS DE HELPER: detectar_error_seleccionado
# ============================================================

test_that("detectar_error_seleccionado encuentra error_sel", {
  env <- new.env(parent = emptyenv())
  env$error_sel <- list(codigo = "TEST-01")

  result <- detectar_error_seleccionado(env)
  expect_equal(result$codigo, "TEST-01")
})

test_that("detectar_error_seleccionado encuentra error_seleccionado (alias)", {
  env <- new.env(parent = emptyenv())
  env$error_seleccionado <- list(codigo = "TEST-02")

  result <- detectar_error_seleccionado(env)
  expect_equal(result$codigo, "TEST-02")
})

test_that("detectar_error_seleccionado retorna NULL si no hay error", {
  env <- new.env(parent = emptyenv())
  result <- detectar_error_seleccionado(env)
  expect_null(result)
})

# ============================================================
# TESTS DE REGRESIÓN: BUG ORIGINAL
# ============================================================

test_that("REGRESIÓN: El bug original (EST-MTC-04 con n=7) es detectado por Capa B", {
  # Este test documenta el bug que motivó la creación del nivel semántico.
  # EST-MTC-04 dice "Para un número par de datos..." pero se seleccionaba con n=7.
  # Sin precondicion declarada, solo la Capa B (keywords) puede detectarlo.
  env <- new.env(parent = emptyenv())
  env$n <- 7
  env$datos_ord <- c(50, 55, 60, 65, 70, 75, 80)
  env$mediana_calc <- 65
  env$mediana_erronea <- 60

  env$errores_conceptuales <- list(
    list(
      codigo = "EST-MTC-01",
      descripcion_corta = "Confundió el promedio con uno de los valores",
      descripcion_larga = ""
    ),
    list(
      codigo = "EST-MTC-02",
      descripcion_corta = "Calculó la mediana sin ordenar los datos previamente",
      descripcion_larga = ""
    ),
    list(
      codigo = "EST-MTC-03",
      descripcion_corta = "Confundió la mediana con la moda",
      descripcion_larga = ""
    ),
    list(
      codigo = "EST-MTC-04",
      descripcion_corta = "Para un número par de datos, tomó solo uno de los dos valores centrales",
      descripcion_larga = "En lugar de promediar los dos datos centrales, seleccionó solo uno"
      # SIN precondicion — reproduce el bug original
    )
  )
  env$error_sel <- env$errores_conceptuales[[4]]  # EST-MTC-04 seleccionado

  errores <- validar_precondiciones_error_pool(env)
  err_bloqueantes <- errores[grepl("^ERR_SEM", errores)]

  expect_true(length(err_bloqueantes) > 0,
    info = "El bug original DEBE ser detectado como error bloqueante")
  expect_true(any(grepl("EST-MTC-04", err_bloqueantes)),
    info = "El error debe mencionar EST-MTC-04")
  expect_true(any(grepl("ERR_SEM_B", err_bloqueantes)),
    info = "Debe ser ERR_SEM_B (keyword scanner)")
})

# ============================================================
# TESTS DE CAPA D: DETERMINISMO DE calcula()
# Regresión del bug EST-MTC-03 (sample() interno en calcula)
# ============================================================

test_that("Capa D existe como función", {
  expect_true(exists("validar_capa_d_determinismo"))
  expect_true(is.function(validar_capa_d_determinismo))
})

test_that("Capa D detecta sample() en calcula() como ERR_SEM_D (error seleccionado)", {
  env <- new.env(parent = globalenv())
  env$datos_ord <- c(2, 5, 12, 13, 14, 15, 15)
  env$datos_presentados <- c(2, 15, 13, 12, 14, 5, 15)

  pool_buggy <- list(
    list(
      codigo = "EST-MTC-03-BUGGY",
      calcula = function(datos_ord, datos_presentados = NULL) {
        datos_desordenados <- sample(datos_ord)  # BUG: no determinista
        n <- length(datos_desordenados)
        datos_desordenados[(n + 1) / 2]
      }
    )
  )
  error_sel_buggy <- pool_buggy[[1]]

  errores <- validar_capa_d_determinismo(pool_buggy, error_sel_buggy, env)
  err_bloqueantes <- errores[grepl("^ERR_SEM_D", errores)]

  expect_true(length(err_bloqueantes) > 0,
    info = "Debe detectar sample() como ERR_SEM_D bloqueante")
  expect_true(any(grepl("sample", err_bloqueantes)),
    info = "El error debe mencionar sample()")
})

test_that("Capa D detecta sample() en calcula() como WARN_SEM_D (error no seleccionado)", {
  env <- new.env(parent = globalenv())
  env$datos_ord <- c(2, 5, 12, 13, 14, 15, 15)

  pool_buggy <- list(
    list(
      codigo = "ERR-OK",
      calcula = function(datos_ord, datos_presentados = NULL) round(mean(datos_ord), 2)
    ),
    list(
      codigo = "ERR-BUGGY",
      calcula = function(datos_ord, datos_presentados = NULL) {
        sample(datos_ord, 1)  # BUG latente
      }
    )
  )
  error_sel <- pool_buggy[[1]]  # El seleccionado es OK

  errores <- validar_capa_d_determinismo(pool_buggy, error_sel, env)
  warns <- errores[grepl("^WARN_SEM_D", errores)]

  expect_true(length(warns) > 0,
    info = "Debe reportar WARN_SEM_D para el error no seleccionado con sample()")
  expect_true(any(grepl("ERR-BUGGY", warns)),
    info = "El warning debe mencionar el código del error buggy")
})

test_that("Capa D aprueba calcula() deterministas sin sample()", {
  env <- new.env(parent = globalenv())
  env$datos_ord <- c(2, 5, 12, 13, 14, 15, 15)
  env$datos_presentados <- c(2, 15, 13, 12, 14, 5, 15)

  pool_ok <- list(
    list(
      codigo = "EST-MTC-01",
      calcula = function(datos_ord, datos_presentados = NULL) round(mean(datos_ord), 2)
    ),
    list(
      codigo = "EST-MTC-03-FIXED",
      calcula = function(datos_ord, datos_presentados = NULL) {
        if (is.null(datos_presentados)) stop("requiere datos_presentados")
        n <- length(datos_presentados)
        datos_presentados[(n + 1) / 2]
      }
    )
  )
  error_sel <- pool_ok[[2]]

  errores <- validar_capa_d_determinismo(pool_ok, error_sel, env)
  err_sem_d <- errores[grepl("SEM_D", errores)]

  expect_equal(length(err_sem_d), 0,
    info = paste("calcula() deterministas no deben generar errores D. Errores:", paste(err_sem_d, collapse="; ")))
})

test_that("Capa D detecta no-determinismo empírico (runif)", {
  env <- new.env(parent = globalenv())
  env$datos_ord <- c(1, 2, 3, 4, 5)

  pool_runif <- list(
    list(
      codigo = "ERR-RUNIF",
      calcula = function(datos_ord, datos_presentados = NULL) {
        round(runif(1, min(datos_ord), max(datos_ord)), 2)
      }
    )
  )
  error_sel <- pool_runif[[1]]

  errores <- validar_capa_d_determinismo(pool_runif, error_sel, env)
  err_bloqueantes <- errores[grepl("^ERR_SEM_D", errores)]

  expect_true(length(err_bloqueantes) > 0,
    info = "Debe detectar runif() como ERR_SEM_D")
})

test_that("Capa D integrada en orquestador principal", {
  env <- new.env(parent = globalenv())
  env$datos_ord <- c(2, 5, 12, 13, 14, 15, 15)
  env$datos_presentados <- c(2, 15, 13, 12, 14, 5, 15)
  env$n <- 7

  env$errores_conceptuales <- list(
    list(
      codigo = "EST-BUGGY",
      descripcion_corta = "Error de prueba",
      descripcion_larga = "Error de prueba largo",
      precondicion = function(params) TRUE,
      calcula = function(datos_ord, datos_presentados = NULL) {
        sample(datos_ord, 1)  # BUG
      }
    )
  )
  env$error_sel <- env$errores_conceptuales[[1]]
  env$mediana_erronea <- 12
  env$mediana_calc <- 13

  errores <- validar_precondiciones_error_pool(env)
  err_d <- errores[grepl("SEM_D", errores)]

  expect_true(length(err_d) > 0,
    info = "El orquestador principal debe ejecutar Capa D y detectar sample()")
})
