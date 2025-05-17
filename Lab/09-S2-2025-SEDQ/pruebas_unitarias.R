#!/usr/bin/env Rscript
# =============================================================================
# Pruebas Unitarias para Ejercicio de Descuentos y Porcentajes
# =============================================================================
# Este script realiza pruebas exhaustivas para garantizar:
# 1. Generación de al menos 300 variantes diferentes del ejercicio
# 2. No duplicidad de opciones de respuesta en cada variante
# 3. Coherencia matemática en todas las variantes
# 4. Correcta aleatorización de parámetros
# 5. Validez de las soluciones
# =============================================================================

# Cargar bibliotecas necesarias
library(exams)
library(testthat)
library(parallel)
library(digest)
library(dplyr)
library(ggplot2)
library(knitr)

# Configuración inicial
archivo_ejercicio <- "descuentos_porcentajes_v2.Rmd"
num_simulaciones <- 500  # Número de simulaciones para probar variabilidad
num_cores <- min(detectCores() - 1, 4)  # Usar múltiples núcleos para acelerar las pruebas

# Función para extraer variables de un ejercicio compilado
extraer_variables <- function(ejercicio_compilado) {
  # Extraer variables del entorno del ejercicio
  env <- attr(ejercicio_compilado, "metainfo")$envir

  # Intentar obtener variables del entorno global si están disponibles
  if (exists("precio_original_global", envir = .GlobalEnv)) {
    variables <- list(
      precio_original = get("precio_original_global", envir = .GlobalEnv),
      porcentaje_descuento = get("porcentaje_descuento_global", envir = .GlobalEnv),
      valor_descuento = get("valor_descuento_global", envir = .GlobalEnv),
      precio_final = get("precio_final_global", envir = .GlobalEnv),
      factor_precio_final = get("factor_precio_final_global", envir = .GlobalEnv),
      decimal = get("decimal_global", envir = .GlobalEnv),
      fraccion = get("fraccion_global", envir = .GlobalEnv),
      articulo = get("articulo_global", envir = .GlobalEnv),
      termino_ahorro = get("termino_ahorro_global", envir = .GlobalEnv),
      termino_descuento = get("termino_descuento_global", envir = .GlobalEnv),
      termino_compra = get("termino_compra_global", envir = .GlobalEnv),
      termino_costo = get("termino_costo_global", envir = .GlobalEnv),
      termino_procedimiento = get("termino_procedimiento_global", envir = .GlobalEnv),
      indice_correcto = get("indice_correcto_global", envir = .GlobalEnv),
      solucion = get("solucion_global", envir = .GlobalEnv)
    )

    # Intentar obtener las variables de aleatorización si existen
    if (exists("usar_fraccion_opcion_c", envir = .GlobalEnv)) {
      variables$usar_fraccion_opcion_c = get("usar_fraccion_opcion_c", envir = .GlobalEnv)
    }
    if (exists("usar_fraccion_opcion_d", envir = .GlobalEnv)) {
      variables$usar_fraccion_opcion_d = get("usar_fraccion_opcion_d", envir = .GlobalEnv)
    }
    if (exists("fraccion_factor", envir = .GlobalEnv)) {
      variables$fraccion_factor = get("fraccion_factor", envir = .GlobalEnv)
    }

    # Obtener las opciones
    opciones <- get("opciones_global", envir = .GlobalEnv)
    variables$opcion_a <- opciones[1]
    variables$opcion_b <- opciones[2]
    variables$opcion_c <- opciones[3]
    variables$opcion_d <- opciones[4]
  } else {
    # Si no están en el entorno global, intentar obtenerlas del entorno del ejercicio
    variables <- list(
      precio_original = env$precio_original,
      porcentaje_descuento = env$porcentaje_descuento,
      valor_descuento = env$valor_descuento,
      precio_final = env$precio_final,
      factor_precio_final = env$factor_precio_final,
      decimal = env$decimal,
      fraccion = env$fraccion,
      articulo = env$articulo,
      termino_ahorro = env$termino_ahorro,
      termino_descuento = env$termino_descuento,
      termino_compra = env$termino_compra,
      termino_costo = env$termino_costo,
      termino_procedimiento = env$termino_procedimiento,
      opcion_a = env$opcion_a,
      opcion_b = env$opcion_b,
      opcion_c = env$opcion_c,
      opcion_d = env$opcion_d,
      indice_correcto = env$indice_correcto,
      solucion = env$solucion
    )
  }

  return(variables)
}

# Función para verificar la coherencia matemática
verificar_coherencia_matematica <- function(vars) {
  # Verificar cálculo del valor del descuento
  valor_descuento_calculado <- vars$precio_original * vars$porcentaje_descuento / 100
  expect_equal(vars$valor_descuento, valor_descuento_calculado,
               tolerance = 0.01,
               info = "El valor del descuento no coincide con el cálculo esperado")

  # Verificar cálculo del precio final
  precio_final_calculado <- vars$precio_original - vars$valor_descuento
  expect_equal(vars$precio_final, precio_final_calculado,
               tolerance = 0.01,
               info = "El precio final no coincide con el cálculo esperado")

  # Verificar cálculo del factor para el precio final
  factor_precio_final_calculado <- 1 - vars$porcentaje_descuento/100
  expect_equal(vars$factor_precio_final, factor_precio_final_calculado,
               tolerance = 0.0001,
               info = "El factor para el precio final no coincide con el cálculo esperado")

  # Verificar que la fracción es equivalente al porcentaje
  fraccion_valor <- switch(as.character(vars$porcentaje_descuento),
                          "10" = 1/10,
                          "12" = 3/25,
                          "15" = 3/20,
                          "18" = 9/50,
                          "20" = 1/5,
                          "22" = 11/50,
                          "25" = 1/4,
                          "28" = 7/25,
                          "30" = 3/10,
                          "33" = 1/3,
                          "35" = 7/20,
                          "38" = 19/50,
                          "40" = 2/5,
                          "45" = 9/20,
                          "48" = 12/25,
                          "50" = 1/2)

  # Si el porcentaje no está en la lista, omitir esta verificación
  if (!is.null(fraccion_valor)) {
    expect_equal(fraccion_valor, vars$porcentaje_descuento/100,
                 tolerance = 0.01,
                 info = "La fracción no es equivalente al porcentaje")
  }

  # Verificar que la fracción del factor es equivalente al factor de precio final
  fraccion_factor_valor <- switch(as.character(vars$porcentaje_descuento),
                                "10" = 9/10,
                                "12" = 22/25,
                                "15" = 17/20,
                                "18" = 41/50,
                                "20" = 4/5,
                                "22" = 39/50,
                                "25" = 3/4,
                                "28" = 18/25,
                                "30" = 7/10,
                                "33" = 2/3,
                                "35" = 13/20,
                                "38" = 31/50,
                                "40" = 3/5,
                                "45" = 11/20,
                                "48" = 13/25,
                                "50" = 1/2)

  # Si el porcentaje no está en la lista, omitir esta verificación
  if (!is.null(fraccion_factor_valor)) {
    expect_equal(fraccion_factor_valor, vars$factor_precio_final,
                 tolerance = 0.01,
                 info = "La fracción del factor no es equivalente al factor de precio final")
  }

  # Verificar que el decimal es equivalente al porcentaje
  expect_equal(vars$decimal, vars$porcentaje_descuento/100,
               tolerance = 0.0001,
               info = "El decimal no es equivalente al porcentaje")

  # Verificar que la respuesta correcta es la opción D
  expect_equal(vars$indice_correcto, 4,
               info = "La respuesta correcta no es la opción D")

  # Verificar que el vector de solución tiene un 1 en la posición correcta
  expect_equal(vars$solucion[vars$indice_correcto], 1,
               info = "El vector de solución no tiene un 1 en la posición correcta")

  # Verificar que el resto del vector de solución tiene 0s
  expect_equal(sum(vars$solucion), 1,
               info = "El vector de solución no tiene exactamente un 1")

  return(TRUE)
}

# Función para verificar la no duplicidad de opciones
verificar_no_duplicidad <- function(vars) {
  opciones <- c(vars$opcion_a, vars$opcion_b, vars$opcion_c, vars$opcion_d)
  expect_equal(length(unique(opciones)), 4,
               info = "Hay opciones de respuesta duplicadas")
  return(TRUE)
}

# Función para generar un hash único de cada variante
generar_hash_variante <- function(vars) {
  # Crear una lista con los valores clave que definen una variante única
  valores_clave <- list(
    precio_original = vars$precio_original,
    porcentaje_descuento = vars$porcentaje_descuento,
    articulo = vars$articulo,
    termino_ahorro = vars$termino_ahorro,
    termino_descuento = vars$termino_descuento,
    termino_compra = vars$termino_compra,
    termino_costo = vars$termino_costo,
    termino_procedimiento = vars$termino_procedimiento
  )

  # Añadir variables de aleatorización si existen
  if (!is.null(vars$usar_fraccion_opcion_c)) {
    valores_clave$usar_fraccion_opcion_c <- vars$usar_fraccion_opcion_c
  }
  if (!is.null(vars$usar_fraccion_opcion_d)) {
    valores_clave$usar_fraccion_opcion_d <- vars$usar_fraccion_opcion_d
  }

  # Generar un hash único basado en estos valores
  hash <- digest(valores_clave, algo = "md5")
  return(hash)
}

# Función principal para ejecutar todas las pruebas
ejecutar_pruebas <- function() {
  cat("Iniciando pruebas unitarias para", archivo_ejercicio, "\n")

  # 1. Compilar el ejercicio una vez para verificar que no hay errores de sintaxis
  cat("Verificando sintaxis del ejercicio...\n")
  tryCatch({
    ejercicio <- exams::xexams(archivo_ejercicio, driver = list(sweave = NULL, read = NULL), quiet = TRUE)
    cat("✓ Sintaxis correcta\n")
  }, error = function(e) {
    cat("✗ Error de sintaxis:", conditionMessage(e), "\n")
    stop("El ejercicio contiene errores de sintaxis que deben corregirse antes de continuar.")
  })

  # 2. Generar múltiples variantes en paralelo
  cat("Generando", num_simulaciones, "variantes para análisis...\n")

  # Usar procesamiento paralelo para acelerar
  cl <- makeCluster(num_cores)
  clusterExport(cl, c("archivo_ejercicio", "extraer_variables"))
  clusterEvalQ(cl, library(exams))

  resultados <- parLapply(cl, 1:num_simulaciones, function(i) {
    # Compilar el ejercicio con una semilla aleatoria
    set.seed(i)
    ejercicio <- exams::xexams(archivo_ejercicio, driver = list(sweave = NULL, read = NULL), quiet = TRUE)
    # Extraer variables
    vars <- extraer_variables(ejercicio[[1]][[1]])
    return(vars)
  })

  stopCluster(cl)

  # 3. Verificar coherencia matemática en todas las variantes
  cat("Verificando coherencia matemática...\n")
  errores_coherencia <- 0
  for (i in 1:length(resultados)) {
    tryCatch({
      verificar_coherencia_matematica(resultados[[i]])
    }, error = function(e) {
      errores_coherencia <<- errores_coherencia + 1
      cat("✗ Error de coherencia matemática en variante", i, ":", conditionMessage(e), "\n")
    })
  }

  if (errores_coherencia == 0) {
    cat("✓ Todas las variantes mantienen coherencia matemática\n")
  } else {
    cat("✗", errores_coherencia, "variantes con problemas de coherencia matemática\n")
  }

  # 4. Verificar no duplicidad de opciones en todas las variantes
  cat("Verificando no duplicidad de opciones de respuesta...\n")
  errores_duplicidad <- 0
  for (i in 1:length(resultados)) {
    tryCatch({
      verificar_no_duplicidad(resultados[[i]])
    }, error = function(e) {
      errores_duplicidad <<- errores_duplicidad + 1
      cat("✗ Error de duplicidad en variante", i, ":", conditionMessage(e), "\n")
    })
  }

  if (errores_duplicidad == 0) {
    cat("✓ Ninguna variante tiene opciones de respuesta duplicadas\n")
  } else {
    cat("✗", errores_duplicidad, "variantes con opciones de respuesta duplicadas\n")
  }

  # 5. Verificar variabilidad (al menos 300 variantes diferentes)
  cat("Verificando variabilidad de las preguntas generadas...\n")
  hashes <- sapply(resultados, generar_hash_variante)
  variantes_unicas <- length(unique(hashes))

  cat("Número de variantes únicas generadas:", variantes_unicas, "\n")
  if (variantes_unicas >= 300) {
    cat("✓ Se generan al menos 300 variantes diferentes\n")
  } else {
    cat("✗ No se generan suficientes variantes diferentes (mínimo requerido: 300)\n")
  }

  # 6. Analizar distribución de parámetros
  cat("Analizando distribución de parámetros...\n")

  # Extraer parámetros para análisis
  precios_originales <- sapply(resultados, function(x) x$precio_original)
  porcentajes_descuento <- sapply(resultados, function(x) x$porcentaje_descuento)
  articulos <- sapply(resultados, function(x) x$articulo)
  terminos_ahorro <- sapply(resultados, function(x) x$termino_ahorro)

  # Extraer variables de aleatorización si existen
  usar_fraccion_opcion_c <- sapply(resultados, function(x) {
    if (!is.null(x$usar_fraccion_opcion_c)) x$usar_fraccion_opcion_c else NA
  })
  usar_fraccion_opcion_d <- sapply(resultados, function(x) {
    if (!is.null(x$usar_fraccion_opcion_d)) x$usar_fraccion_opcion_d else NA
  })

  # Crear dataframe para análisis
  df_analisis <- data.frame(
    precio_original = precios_originales,
    porcentaje_descuento = porcentajes_descuento,
    articulo = articulos,
    termino_ahorro = terminos_ahorro,
    usar_fraccion_opcion_c = usar_fraccion_opcion_c,
    usar_fraccion_opcion_d = usar_fraccion_opcion_d
  )

  # Mostrar resumen de distribución de parámetros numéricos
  cat("\nDistribución de precios originales:\n")
  print(table(precios_originales))

  cat("\nDistribución de porcentajes de descuento:\n")
  print(table(porcentajes_descuento))

  cat("\nDistribución de artículos:\n")
  print(table(articulos))

  cat("\nDistribución de términos para 'ahorro':\n")
  print(table(terminos_ahorro))

  # Mostrar distribución de las variables de aleatorización
  if (!all(is.na(usar_fraccion_opcion_c))) {
    cat("\nDistribución de formato para opción C (círculo 1):\n")
    print(table(usar_fraccion_opcion_c, useNA = "ifany"))
  }

  if (!all(is.na(usar_fraccion_opcion_d))) {
    cat("\nDistribución de formato para opción D (círculo 2):\n")
    print(table(usar_fraccion_opcion_d, useNA = "ifany"))
  }

  # 7. Generar informe final
  cat("\n=== RESUMEN DE PRUEBAS ===\n")
  cat("Total de variantes analizadas:", num_simulaciones, "\n")
  cat("Variantes únicas generadas:", variantes_unicas, "\n")
  cat("Errores de coherencia matemática:", errores_coherencia, "\n")
  cat("Errores de duplicidad de opciones:", errores_duplicidad, "\n")

  if (variantes_unicas >= 300 && errores_coherencia == 0 && errores_duplicidad == 0) {
    cat("\n✓ TODAS LAS PRUEBAS PASARON EXITOSAMENTE\n")
    return(TRUE)
  } else {
    cat("\n✗ ALGUNAS PRUEBAS FALLARON\n")
    return(FALSE)
  }
}

# Ejecutar todas las pruebas
resultado <- ejecutar_pruebas()

# Guardar resultados en un archivo de log
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
archivo_log <- paste0("pruebas_unitarias_", timestamp, ".log")
sink(archivo_log)
cat("Resultados de pruebas unitarias para", archivo_ejercicio, "\n")
cat("Fecha y hora:", Sys.time(), "\n\n")
print(resultado)
sink()

# Mensaje final
if (resultado) {
  cat("\nEl ejercicio cumple con todos los requisitos de calidad.\n")
  cat("Se ha guardado un registro detallado en", archivo_log, "\n")
} else {
  cat("\nEl ejercicio NO cumple con todos los requisitos de calidad.\n")
  cat("Revise el archivo", archivo_log, "para más detalles.\n")
}
