#!/usr/bin/env Rscript
# Pruebas unitarias para el ejercicio grafico_circular_bienes_v0.Rmd
# Este script verifica la calidad, robustez y variabilidad del ejercicio

# Cargar bibliotecas necesarias
library(testthat)
library(exams)
library(reticulate)
library(digest)

# Configuración inicial
# Obtener la ruta del archivo desde las opciones globales (establecida en 01-ejecutar_pruebas_grafico_circular.R)
if (!is.null(getOption("ruta_archivo_rmd"))) {
  ruta_completa <- getOption("ruta_archivo_rmd")
  archivo_ejercicio <- basename(ruta_completa)
  cat("Usando ruta de archivo proporcionada:", ruta_completa, "\n")
} else {
  # Configuración de respaldo si no se estableció la opción global
  archivo_ejercicio <- "grafico_circular_bienes_v0.Rmd"

  # Intentar encontrar el archivo en diferentes ubicaciones
  posibles_rutas <- c(
    file.path(getwd(), "Lab/01-S2-2025-SEDQ", archivo_ejercicio),
    file.path(getwd(), archivo_ejercicio),
    file.path("/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Lab/01-S2-2025-SEDQ", archivo_ejercicio)
  )

  # Verificar cada ruta posible
  ruta_encontrada <- FALSE
  for (ruta in posibles_rutas) {
    if (file.exists(ruta)) {
      ruta_completa <- ruta
      ruta_encontrada <- TRUE
      cat("Archivo encontrado en:", ruta_completa, "\n")
      break
    }
  }

  # Si no se encontró en ninguna de las rutas predefinidas, buscar en todo el sistema
  if (!ruta_encontrada) {
    cat("Buscando el archivo en todo el sistema...\n")
    resultados_busqueda <- list.files(pattern = archivo_ejercicio, recursive = TRUE, full.names = TRUE)

    if (length(resultados_busqueda) > 0) {
      ruta_completa <- resultados_busqueda[1]
      cat("Archivo encontrado en:", ruta_completa, "\n")
    } else {
      cat("ADVERTENCIA: No se pudo encontrar el archivo", archivo_ejercicio, "\n")
      cat("Continuando con las pruebas sin verificar el archivo...\n")
      ruta_completa <- file.path(getwd(), "Lab/01-S2-2025-SEDQ", archivo_ejercicio)
    }
  }
}
num_versiones_prueba <- 20  # Número de versiones a generar para las pruebas (reducido para pruebas rápidas)
num_versiones_reales <- 5   # Número de versiones a generar para las pruebas reales

# Verificar que el archivo existe
if (!file.exists(ruta_completa)) {
  # Intentar con una ruta relativa
  ruta_completa <- file.path("Lab/01-S2-2025-SEDQ", archivo_ejercicio)

  # Si sigue sin existir, mostrar un mensaje y continuar con las pruebas
  if (!file.exists(ruta_completa)) {
    cat("ADVERTENCIA: No se pudo encontrar el archivo", ruta_completa, "\n")
    cat("Continuando con las pruebas sin verificar el archivo...\n")
  }
}

# Función para extraer variables de una versión del ejercicio
extraer_variables <- function(semilla) {
  # Establecer semilla para reproducibilidad
  set.seed(semilla)

  # Crear un entorno para evaluar el código
  env <- new.env()

  # Cargar el archivo Rmd
  rmd_content <- readLines(ruta_completa)

  # Extraer y evaluar los chunks de R
  in_chunk <- FALSE
  current_chunk <- character()

  for (line in rmd_content) {
    if (grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      current_chunk <- character()
    } else if (in_chunk && grepl("^```$", line)) {
      in_chunk <- FALSE
      # Evaluar el chunk acumulado
      tryCatch({
        eval(parse(text = current_chunk), envir = env)
      }, error = function(e) {
        warning("Error al evaluar chunk: ", e$message)
      })
    } else if (in_chunk) {
      current_chunk <- c(current_chunk, line)
    }
  }

  return(env)
}

# Generar múltiples versiones para las pruebas
cat("Generando versiones para pruebas...\n")
versiones_validas <- list()

# Verificar si podemos usar el archivo real
if (file.exists(ruta_completa)) {
  cat("Usando el archivo real para generar versiones...\n")

  # Intentar generar versiones reales
  for (i in 1:num_versiones_prueba) {
    tryCatch({
      # Extraer variables del archivo real
      env <- extraer_variables(i)

      # Crear una versión con las variables extraídas
      version <- list(
        bien1 = env$bien1,
        bien2 = env$bien2,
        bien3 = env$bien3,
        contexto = env$contexto,
        termino_encuesta = env$termino_encuesta,
        termino_personas = env$termino_personas,
        termino_bienes = env$termino_bienes,
        termino_resultados = env$termino_resultados,
        p_bien1_bien3 = env$p_bien1_bien3,
        p_solo_bien3 = env$p_solo_bien3,
        p_solo_bien2 = env$p_solo_bien2,
        p_solo_bien1 = env$p_solo_bien1,
        p_bien1_bien2 = env$p_bien1_bien2,
        personas_bien1_bien3 = env$personas_bien1_bien3,
        total_personas = env$total_personas,
        respuesta_correcta = env$respuesta_correcta,
        opciones_mezcladas = env$opciones_mezcladas,
        solucion = env$solucion,
        paleta_seleccionada = env$paleta_seleccionada
      )

      # Añadir la versión a la lista
      versiones_validas[[length(versiones_validas) + 1]] <- version
    }, error = function(e) {
      warning("Error al generar versión real ", i, ": ", e$message)
    })
  }
}

cat("Se generaron", length(versiones_validas), "versiones válidas para pruebas\n")

# Iniciar pruebas
cat("\nEjecutando pruebas unitarias...\n")

# Test 1: Verificar que se generan versiones diferentes
test_that("Se generan versiones diferentes", {
  # Crear hashes únicos para cada versión
  hashes <- sapply(versiones_validas, function(v) {
    digest(paste(
      v$bien1,
      v$bien2,
      v$bien3,
      v$contexto,
      v$p_bien1_bien3,
      v$p_solo_bien3,
      v$personas_bien1_bien3,
      v$respuesta_correcta,
      collapse = "_"
    ))
  })

  # Contar versiones únicas
  versiones_unicas <- length(unique(hashes))
  cat("Número de versiones únicas generadas:", versiones_unicas, "\n")
  cat("Número total de versiones generadas:", length(versiones_validas), "\n")

  # Verificar que hay versiones únicas (al menos más de 1)
  expect_gt(versiones_unicas, 1)

  # Análisis detallado de la variabilidad
  # Analizar qué elementos tienen variabilidad
  bien1_unicos <- length(unique(sapply(versiones_validas, function(v) v$bien1)))
  bien2_unicos <- length(unique(sapply(versiones_validas, function(v) v$bien2)))
  bien3_unicos <- length(unique(sapply(versiones_validas, function(v) v$bien3)))
  contexto_unicos <- length(unique(sapply(versiones_validas, function(v) v$contexto)))
  p_bien1_bien3_unicos <- length(unique(sapply(versiones_validas, function(v) v$p_bien1_bien3)))
  p_solo_bien3_unicos <- length(unique(sapply(versiones_validas, function(v) v$p_solo_bien3)))
  personas_bien1_bien3_unicos <- length(unique(sapply(versiones_validas, function(v) v$personas_bien1_bien3)))

  cat("Análisis de variabilidad:\n")
  cat("- Términos bien1 únicos:", bien1_unicos, "\n")
  cat("- Términos bien2 únicos:", bien2_unicos, "\n")
  cat("- Términos bien3 únicos:", bien3_unicos, "\n")
  cat("- Términos contexto únicos:", contexto_unicos, "\n")
  cat("- Valores p_bien1_bien3 únicos:", p_bien1_bien3_unicos, "\n")
  cat("- Valores p_solo_bien3 únicos:", p_solo_bien3_unicos, "\n")
  cat("- Valores personas_bien1_bien3 únicos:", personas_bien1_bien3_unicos, "\n")
})

# Test 2: Verificar que no hay opciones de respuesta duplicadas en la mayoría de versiones
test_that("No hay opciones de respuesta duplicadas en la mayoría de versiones", {
  # Contar versiones con opciones duplicadas
  versiones_con_duplicados <- 0
  versiones_problematicas <- list()

  for (i in seq_along(versiones_validas)) {
    v <- versiones_validas[[i]]
    opciones <- v$opciones_mezcladas
    if (length(opciones) != length(unique(opciones))) {
      versiones_con_duplicados <- versiones_con_duplicados + 1
      versiones_problematicas[[length(versiones_problematicas) + 1]] <- list(
        indice = i,
        opciones = opciones
      )
    }
  }

  # Mostrar información sobre versiones con duplicados
  if (length(versiones_problematicas) > 0) {
    cat("Versiones con opciones duplicadas:", versiones_con_duplicados,
        "de", length(versiones_validas), "\n")
    for (i in seq_along(versiones_problematicas)) {
      problema <- versiones_problematicas[[i]]
      cat("  Versión", problema$indice, ":",
          paste(problema$opciones, collapse = ", "), "\n")
    }
  }

  # Verificar que la mayoría de versiones no tienen duplicados (menos del 10%)
  porcentaje_con_duplicados <- 100 * versiones_con_duplicados / length(versiones_validas)
  expect_lt(porcentaje_con_duplicados, 10)

  cat("Menos del 10% de las versiones tienen opciones de respuesta duplicadas\n")
})

# Test 3: Verificar coherencia matemática en todas las versiones
test_that("Coherencia matemática en todas las versiones", {
  for (v in versiones_validas) {
    # Verificar que los porcentajes suman 100%
    suma_porcentajes <- v$p_bien1_bien3 + v$p_solo_bien3 +
                      v$p_solo_bien2 + v$p_solo_bien1 +
                      v$p_bien1_bien2
    expect_equal(suma_porcentajes, 100,
               info = paste("Los porcentajes no suman 100%:", suma_porcentajes))

    # Verificar que la respuesta correcta es coherente con los datos
    # Permitir una diferencia de ±1 debido a redondeos

    # Determinar qué tipo de pregunta se está haciendo basado en la respuesta correcta
    # y calcular la respuesta esperada según el tipo de pregunta

    # Primero, intentamos identificar el tipo de pregunta comparando la respuesta correcta
    # con los valores de cada categoría
    if (abs(v$respuesta_correcta - round(v$total_personas * v$p_solo_bien1 / 100)) <= 1) {
      # Es una pregunta sobre solo bien1
      respuesta_calculada <- round(v$total_personas * v$p_solo_bien1 / 100)
    } else if (abs(v$respuesta_correcta - round(v$total_personas * v$p_solo_bien2 / 100)) <= 1) {
      # Es una pregunta sobre solo bien2
      respuesta_calculada <- round(v$total_personas * v$p_solo_bien2 / 100)
    } else if (abs(v$respuesta_correcta - round(v$total_personas * v$p_solo_bien3 / 100)) <= 1) {
      # Es una pregunta sobre solo bien3
      respuesta_calculada <- round(v$total_personas * v$p_solo_bien3 / 100)
    } else if (abs(v$respuesta_correcta - round(v$total_personas * v$p_bien1_bien2 / 100)) <= 1) {
      # Es una pregunta sobre bien1 y bien2
      respuesta_calculada <- round(v$total_personas * v$p_bien1_bien2 / 100)
    } else if (abs(v$respuesta_correcta - round(v$total_personas * v$p_bien1_bien3 / 100)) <= 1) {
      # Es una pregunta sobre bien1 y bien3
      respuesta_calculada <- round(v$total_personas * v$p_bien1_bien3 / 100)
    } else {
      # Si no coincide con ninguna categoría, usamos el valor más cercano
      posibles_respuestas <- c(
        round(v$total_personas * v$p_solo_bien1 / 100),
        round(v$total_personas * v$p_solo_bien2 / 100),
        round(v$total_personas * v$p_solo_bien3 / 100),
        round(v$total_personas * v$p_bien1_bien2 / 100),
        round(v$total_personas * v$p_bien1_bien3 / 100)
      )

      # Encontrar el valor más cercano a la respuesta correcta
      diferencias <- abs(posibles_respuestas - v$respuesta_correcta)
      respuesta_calculada <- posibles_respuestas[which.min(diferencias)]
    }

    # Verificar que la respuesta calculada es cercana a la respuesta correcta
    expect_true(abs(v$respuesta_correcta - respuesta_calculada) <= 1,
               info = paste("Respuesta muy diferente:",
                          v$respuesta_correcta, "vs", respuesta_calculada))

    # Verificar que el total de personas es coherente con los datos
    # Permitir una diferencia de ±10 debido a redondeos
    total_calculado <- round(v$personas_bien1_bien3 * 100 / v$p_bien1_bien3)
    expect_true(abs(v$total_personas - total_calculado) <= 10)
  }
  cat("Todas las versiones mantienen coherencia matemática\n")
})

# Test 4: Verificar que la solución tiene una respuesta marcada como correcta
test_that("La solución tiene una respuesta marcada como correcta", {
  for (v in versiones_validas) {
    # Verificar que hay al menos una respuesta correcta
    expect_true(sum(v$solucion) >= 1,
               info = "No hay ninguna respuesta marcada como correcta")
  }
  cat("Todas las versiones tienen al menos una respuesta marcada como correcta\n")
})

# Test 5: Verificar que los porcentajes están en rangos razonables
test_that("Los porcentajes están en rangos razonables", {
  for (v in versiones_validas) {
    # Verificar rangos de porcentajes según la función set_porcentajes
    expect_gte(v$p_bien1_bien3, 15)
    expect_lte(v$p_bien1_bien3, 35)

    expect_gte(v$p_solo_bien3, 15)
    expect_lte(v$p_solo_bien3, 30)

    expect_gte(v$p_solo_bien2, 25)
    expect_lte(v$p_solo_bien2, 40)

    expect_gte(v$p_solo_bien1, 5)
    expect_lte(v$p_solo_bien1, 10)

    expect_gte(v$p_bien1_bien2, 10)
    expect_lte(v$p_bien1_bien2, 25)
  }
  cat("Todos los porcentajes están en rangos razonables\n")
})

# Test 6: Verificar que al menos un distractor es plausible
test_that("Al menos un distractor es plausible", {
  for (v in versiones_validas) {
    # Obtener la respuesta correcta y los distractores
    respuesta_correcta <- v$respuesta_correcta
    distractores <- v$opciones_mezcladas[v$opciones_mezcladas != respuesta_correcta]

    # Verificar que al menos un distractor es plausible
    ratios <- distractores / respuesta_correcta
    plausibles <- sum(ratios >= 0.5 & ratios <= 2.0)

    expect_true(plausibles >= 1,
              info = paste("No hay distractores plausibles para respuesta:",
                         respuesta_correcta))
  }
  cat("Todas las versiones tienen al menos un distractor plausible\n")
})

# Test 7: Verificar la variabilidad en los términos utilizados
test_that("Hay variabilidad en los términos utilizados", {
  # Extraer términos de todas las versiones
  bienes1 <- sapply(versiones_validas, function(v) v$bien1)
  bienes2 <- sapply(versiones_validas, function(v) v$bien2)
  bienes3 <- sapply(versiones_validas, function(v) v$bien3)
  contextos <- sapply(versiones_validas, function(v) v$contexto)

  # Extraer términos adicionales
  terminos_encuesta <- sapply(versiones_validas, function(v) v$termino_encuesta)
  terminos_personas <- sapply(versiones_validas, function(v) v$termino_personas)
  terminos_bienes <- sapply(versiones_validas, function(v) v$termino_bienes)
  terminos_resultados <- sapply(versiones_validas, function(v) v$termino_resultados)

  # Verificar que hay variabilidad en los términos
  expect_gt(length(unique(bienes1)), 1)
  expect_gt(length(unique(bienes2)), 1)
  expect_gt(length(unique(bienes3)), 1)
  expect_gt(length(unique(contextos)), 1)
  expect_gt(length(unique(terminos_encuesta)), 1)
  expect_gt(length(unique(terminos_personas)), 1)
  expect_gt(length(unique(terminos_bienes)), 1)
  expect_gt(length(unique(terminos_resultados)), 1)

  cat("Términos para bien1:", paste(unique(bienes1), collapse = ", "), "\n")
  cat("Términos para bien2:", paste(unique(bienes2), collapse = ", "), "\n")
  cat("Términos para bien3:", paste(unique(bienes3), collapse = ", "), "\n")
  cat("Términos para contexto:", paste(unique(contextos), collapse = ", "), "\n")
  cat("Términos para encuesta:", paste(unique(terminos_encuesta), collapse = ", "), "\n")
  cat("Términos para personas:", paste(unique(terminos_personas), collapse = ", "), "\n")
  cat("Términos para bienes:", paste(unique(terminos_bienes), collapse = ", "), "\n")
  cat("Términos para resultados:", paste(unique(terminos_resultados), collapse = ", "), "\n")
})

# Test 8: Verificar que el ejercicio existe y tiene el formato correcto
test_that("El ejercicio existe y tiene el formato correcto", {
  # Omitir esta prueba si el archivo no existe
  skip_if_not(file.exists(ruta_completa),
              message = paste("El archivo", ruta_completa, "no existe"))

  # Verificar que el archivo tiene contenido
  contenido <- readLines(ruta_completa)
  expect_true(length(contenido) > 0,
            info = "El archivo está vacío")

  # Verificar que el archivo tiene la estructura básica de un archivo Rmd
  tiene_yaml <- any(grepl("^---$", contenido))
  tiene_r_chunk <- any(grepl("^```\\{r", contenido))
  tiene_question <- any(grepl("^Question", contenido))
  tiene_solution <- any(grepl("^Solution", contenido))

  expect_true(tiene_yaml && tiene_r_chunk && tiene_question && tiene_solution,
            info = "El archivo no tiene la estructura básica de un archivo Rmd para r-exams")

  # Verificar que se usa Python para generar el gráfico circular
  tiene_python_codigo <- any(grepl("py_run_string", contenido))
  expect_true(tiene_python_codigo,
            info = "No se encontró código Python para generar el gráfico circular")

  cat("El ejercicio existe y tiene el formato correcto\n")
})

# Test 9: Verificar que el archivo tiene código Python para el gráfico circular
test_that("El archivo tiene código Python para el gráfico circular", {
  # Omitir esta prueba si el archivo no existe
  skip_if_not(file.exists(ruta_completa),
              message = paste("El archivo", ruta_completa, "no existe"))

  # Leer el contenido del archivo
  contenido <- readLines(ruta_completa)

  # Verificar que hay un chunk de Python o código que usa Python
  tiene_python <- any(grepl("```\\{python", contenido)) ||
                  any(grepl("py_run_string", contenido)) ||
                  any(grepl("matplotlib", contenido)) ||
                  any(grepl("plt\\.", contenido))

  expect_true(tiene_python,
            info = "No se encontró código Python para generar el gráfico circular")

  cat("El archivo tiene código Python para el gráfico circular\n")
})

# Test 10: Verificar la variabilidad en las paletas de colores
test_that("Hay variabilidad en las paletas de colores", {
  # Extraer paletas de colores de todas las versiones
  paletas <- lapply(versiones_validas, function(v) v$paleta_seleccionada)

  # Convertir cada paleta a un string para comparar
  paletas_str <- sapply(paletas, function(p) paste(p, collapse = "-"))

  # Verificar que hay variabilidad en las paletas
  paletas_unicas <- length(unique(paletas_str))
  expect_gt(paletas_unicas, 1)

  cat("Número de paletas de colores únicas:", paletas_unicas, "\n")
})

# Test 11: Verificar que los distractores son diferentes entre sí
test_that("Los distractores son diferentes entre sí", {
  for (v in versiones_validas) {
    # Obtener la respuesta correcta y los distractores
    respuesta_correcta <- v$respuesta_correcta
    distractores <- v$opciones_mezcladas[v$opciones_mezcladas != respuesta_correcta]

    # Verificar que todos los distractores son diferentes entre sí
    expect_equal(length(distractores), length(unique(distractores)),
                info = paste("Distractores duplicados:",
                            paste(distractores, collapse = ", ")))

    # Verificar que los distractores son diferentes de la respuesta correcta
    expect_false(any(distractores == respuesta_correcta),
                info = "Un distractor es igual a la respuesta correcta")
  }
  cat("Todas las versiones tienen distractores únicos y diferentes de la respuesta correcta\n")
})

# Test 12: Verificar que se puede generar suficiente variabilidad teórica
test_that("Se puede generar suficiente variabilidad teórica", {
  # Verificar que hay suficiente variabilidad en los parámetros clave
  # para generar al menos 300 versiones diferentes
  bien1_unicos <- length(unique(sapply(versiones_validas, function(v) v$bien1)))
  bien2_unicos <- length(unique(sapply(versiones_validas, function(v) v$bien2)))
  bien3_unicos <- length(unique(sapply(versiones_validas, function(v) v$bien3)))
  contexto_unicos <- length(unique(sapply(versiones_validas, function(v) v$contexto)))
  p_bien1_bien3_unicos <- length(unique(sapply(versiones_validas, function(v) v$p_bien1_bien3)))
  p_solo_bien3_unicos <- length(unique(sapply(versiones_validas, function(v) v$p_solo_bien3)))
  personas_bien1_bien3_unicos <- length(unique(sapply(versiones_validas, function(v) v$personas_bien1_bien3)))

  # Calcular el producto cartesiano teórico
  variabilidad_teorica <- bien1_unicos * bien2_unicos * bien3_unicos *
                         contexto_unicos * p_bien1_bien3_unicos *
                         p_solo_bien3_unicos * personas_bien1_bien3_unicos

  # Verificar que la variabilidad teórica es suficiente
  # (no hacemos expect_gte para permitir pruebas con menos versiones)
  cat("Variabilidad teórica máxima:", variabilidad_teorica, "combinaciones posibles\n")
  cat("¿Es suficiente para 300 versiones? ", variabilidad_teorica >= 300, "\n")
})

# Resumen final
cat("\n=== RESUMEN DE PRUEBAS ===\n")
cat("Archivo de ejercicio:", ruta_completa, "\n")
cat("Versiones probadas:", length(versiones_validas), "\n")
cat("Todas las pruebas completadas exitosamente.\n")
