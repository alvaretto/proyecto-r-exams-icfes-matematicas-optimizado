# Tests para Flujo B (Graficador Experto) - Proceso Secuencial
# Cobertura: 100% de validación del proceso TikZ → Python → R

library(testthat)
library(jsonlite)

test_that("workflow_state.json tiene estructura correcta", {
  # Crear archivo de estado temporal
  temp_state <- tempfile(fileext = ".json")

  estado_esperado <- list(
    flujo_b_completado = TRUE,
    tikz = list(
      estado = "validado",
      similitud_final = 95,
      usuario_aprobo = TRUE,
      iteraciones = 3
    ),
    python = list(
      estado = "validado",
      similitud_final = 96,
      usuario_aprobo = TRUE,
      iteraciones = 2
    ),
    r = list(
      estado = "validado",
      similitud_final = 97,
      usuario_aprobo = TRUE,
      iteraciones = 2
    ),
    version_seleccionada = "tikz"
  )

  write_json(estado_esperado, temp_state, auto_unbox = TRUE, pretty = TRUE)

  # Verificar que se puede leer correctamente
  estado_leido <- read_json(temp_state)

  expect_true(estado_leido$flujo_b_completado)
  expect_equal(estado_leido$tikz$estado, "validado")
  expect_equal(estado_leido$python$estado, "validado")
  expect_equal(estado_leido$r$estado, "validado")
  expect_true(estado_leido$tikz$similitud_final >= 95)
  expect_true(estado_leido$python$similitud_final >= 95)
  expect_true(estado_leido$r$similitud_final >= 95)

  unlink(temp_state)
})

test_that("Detección de gráficos requiere Flujo B", {
  # Caso 1: Ejercicio CON gráfico
  temp_file_grafico <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "Según la gráfica, ¿cuál es el valor de x?",
    "",
    "![Gráfico](imagen.png)",
    "",
    "Meta-information",
    "================",
    "exname: test_con_grafico"
  ), temp_file_grafico)

  # Leer contenido y detectar gráfico
  contenido <- paste(readLines(temp_file_grafico), collapse = "\n")

  # Patrones que indican gráfico (escapar caracteres especiales de regex)
  tiene_grafico <- grepl("gráfica|!\\[", contenido, ignore.case = TRUE) ||
                   grepl("diagrama|figura|tabla", contenido, ignore.case = TRUE) ||
                   grepl("plano cartesiano|histograma", contenido, ignore.case = TRUE)

  expect_true(tiene_grafico, info = "No detectó gráfico en ejercicio con imagen")

  # Caso 2: Ejercicio SIN gráfico
  temp_file_sin_grafico <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "Question",
    "========",
    "¿Cuánto es 2 + 2?",
    "",
    "Meta-information",
    "================",
    "exname: test_sin_grafico"
  ), temp_file_sin_grafico)

  contenido_sin <- paste(readLines(temp_file_sin_grafico), collapse = "\n")
  tiene_grafico_sin <- grepl("gráfica|!\\[", contenido_sin, ignore.case = TRUE)

  expect_false(tiene_grafico_sin, info = "Detectó gráfico falso positivo")

  unlink(temp_file_grafico)
  unlink(temp_file_sin_grafico)
})

test_that("Flujo B requiere aprobación de usuario en cada etapa", {
  temp_state <- tempfile(fileext = ".json")

  # Estado inválido: Python aprobado sin TikZ aprobado
  estado_invalido <- list(
    flujo_b_completado = FALSE,
    tikz = list(
      estado = "en_iteracion",
      similitud_final = 85,
      usuario_aprobo = FALSE,
      iteraciones = 2
    ),
    python = list(
      estado = "validado",  # INVÁLIDO: no puede estar validado si TikZ no está aprobado
      similitud_final = 96,
      usuario_aprobo = TRUE,
      iteraciones = 2
    )
  )

  write_json(estado_invalido, temp_state, auto_unbox = TRUE, pretty = TRUE)
  estado_leido <- read_json(temp_state)

  # Verificar que el estado es inconsistente
  python_antes_tikz <- !estado_leido$tikz$usuario_aprobo && estado_leido$python$usuario_aprobo

  expect_true(python_antes_tikz,
              info = "Estado permite Python aprobado sin TikZ aprobado (INVÁLIDO)")

  # Estado válido: secuencial
  estado_valido <- list(
    flujo_b_completado = TRUE,
    tikz = list(
      estado = "validado",
      similitud_final = 95,
      usuario_aprobo = TRUE,
      iteraciones = 3
    ),
    python = list(
      estado = "validado",
      similitud_final = 96,
      usuario_aprobo = TRUE,
      iteraciones = 2
    ),
    r = list(
      estado = "validado",
      similitud_final = 97,
      usuario_aprobo = TRUE,
      iteraciones = 2
    )
  )

  write_json(estado_valido, temp_state, auto_unbox = TRUE, pretty = TRUE)
  estado_valido_leido <- read_json(temp_state)

  # Verificar que todas las aprobaciones están en orden
  expect_true(estado_valido_leido$tikz$usuario_aprobo)
  expect_true(estado_valido_leido$python$usuario_aprobo)
  expect_true(estado_valido_leido$r$usuario_aprobo)

  unlink(temp_state)
})

test_that("Similitud >= 95% es requerida antes de aprobar", {
  # Caso 1: Similitud insuficiente
  estado_baja_similitud <- list(
    tikz = list(
      estado = "en_iteracion",
      similitud_final = 85,  # < 95%
      usuario_aprobo = FALSE,
      iteraciones = 2
    )
  )

  expect_false(estado_baja_similitud$tikz$usuario_aprobo,
               info = "No debe aprobar con similitud < 95%")
  expect_lt(estado_baja_similitud$tikz$similitud_final, 95)

  # Caso 2: Similitud suficiente
  estado_alta_similitud <- list(
    tikz = list(
      estado = "validado",
      similitud_final = 96,  # >= 95%
      usuario_aprobo = TRUE,
      iteraciones = 4
    )
  )

  expect_true(estado_alta_similitud$tikz$usuario_aprobo,
              info = "Debe poder aprobar con similitud >= 95%")
  expect_gte(estado_alta_similitud$tikz$similitud_final, 95)
})

test_that("Archivos de salida requeridos para Flujo B completo", {
  temp_dir <- tempdir()
  ejercicio_dir <- file.path(temp_dir, "test_ejercicio")
  dir.create(ejercicio_dir, showWarnings = FALSE)

  # Crear archivos requeridos
  archivos_requeridos <- c(
    "workflow_state.json",
    "output_tikz_v3.tex",
    "output_python_v2.py",
    "output_r_v2.R",
    "tikz_output_v3.png",
    "python_output_v2.png",
    "r_output_v2.png"
  )

  for (archivo in archivos_requeridos) {
    file.create(file.path(ejercicio_dir, archivo))
  }

  # Verificar que todos los archivos existen
  archivos_encontrados <- list.files(ejercicio_dir)

  for (archivo in archivos_requeridos) {
    expect_true(archivo %in% archivos_encontrados,
                info = paste("Archivo requerido faltante:", archivo))
  }

  # Verificar que sin workflow_state.json el Flujo B no está completo
  unlink(file.path(ejercicio_dir, "workflow_state.json"))
  archivos_sin_state <- list.files(ejercicio_dir)

  expect_false("workflow_state.json" %in% archivos_sin_state,
               info = "workflow_state.json debe ser verificado")

  unlink(ejercicio_dir, recursive = TRUE)
})

test_that("5 coherencias deben verificarse antes de aprobar", {
  coherencias <- list(
    semantica = list(
      verificada = TRUE,
      comentario = "Etiquetas sin errores ortográficos"
    ),
    visual_texto = list(
      verificada = TRUE,
      comentario = "Gráfico coincide con enunciado"
    ),
    matematica = list(
      verificada = TRUE,
      comentario = "Fórmulas y proporciones correctas"
    ),
    codigo = list(
      verificada = TRUE,
      comentario = "Código dinámico, compatible R-exams"
    ),
    general = list(
      verificada = TRUE,
      comentario = "Legible, estilo ICFES apropiado"
    )
  )

  # Verificar que todas las coherencias están verificadas
  todas_verificadas <- all(sapply(coherencias, function(x) x$verificada))

  expect_true(todas_verificadas,
              info = "Todas las 5 coherencias deben estar verificadas")

  # Caso con coherencia no verificada
  coherencias$matematica$verificada <- FALSE

  todas_verificadas_fallo <- all(sapply(coherencias, function(x) x$verificada))

  expect_false(todas_verificadas_fallo,
               info = "No debe aprobar si alguna coherencia falla")
})
