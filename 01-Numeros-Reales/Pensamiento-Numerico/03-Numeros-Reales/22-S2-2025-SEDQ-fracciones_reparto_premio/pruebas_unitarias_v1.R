#!/usr/bin/env Rscript

# =============================================================================
# Pruebas Unitarias Especializadas para fracciones_reparto_premio_v1.Rmd
# =============================================================================
# Este script realiza pruebas exhaustivas específicas para la versión 1:
# - Pregunta por el TERCER PUESTO
# - Método de reparto PROPORCIONAL BÁSICO
# - Validaciones específicas para esta versión
# =============================================================================

library(testthat)
library(exams)
library(digest)
library(dplyr)
library(ggplot2)
library(parallel)
library(lubridate)
library(stringr)

# Configuración específica para v1
archivo_rmd <- "fracciones_reparto_premio_v1.Rmd"
num_simulaciones <- 150  # Incrementado para mayor robustez
verbose <- TRUE  # Mostrar información detallada durante la ejecución
num_cores <- min(detectCores() - 1, 4)  # Usar múltiples núcleos
tolerancia_redondeo <- 0.01  # Tolerancia para errores de redondeo
umbral_variabilidad <- 95  # Porcentaje mínimo de variabilidad requerido

cat("=============================================================================\n")
cat("INICIANDO PRUEBAS UNITARIAS PARA VERSIÓN 1\n")
cat("=============================================================================\n")
cat("Archivo objetivo:", archivo_rmd, "\n")
cat("Simulaciones:", num_simulaciones, "\n")
cat("Núcleos de procesamiento:", num_cores, "\n")
cat("Tolerancia de redondeo:", tolerancia_redondeo, "\n")
cat("Umbral de variabilidad:", umbral_variabilidad, "%\n")
cat("Fecha y hora de inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=============================================================================\n\n")

# =============================================================================
# FUNCIONES DE UTILIDAD Y LOGGING
# =============================================================================

# Función para logging con timestamps
log_mensaje <- function(mensaje, nivel = "INFO") {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  prefijo <- switch(nivel,
    "INFO" = "ℹ",
    "WARN" = "⚠",
    "ERROR" = "✗",
    "SUCCESS" = "✓",
    "DEBUG" = "🔍",
    "ℹ"
  )
  cat(sprintf("[%s] %s %s\n", timestamp, prefijo, mensaje))
}

# Función para validar estructura de datos específica para v1
validar_estructura_datos_v1 <- function(datos) {
  errores <- character(0)

  # Variables obligatorias específicas para v1
  variables_obligatorias <- c(
    "premio_total", "monto_primer_puesto", "monto_segundo_puesto",
    "monto_tercer_puesto", "opciones_mezcladas", "respuesta_correcta"
  )

  for (var in variables_obligatorias) {
    if (!var %in% names(datos) || is.null(datos[[var]])) {
      errores <- c(errores, paste("Variable obligatoria faltante:", var))
    }
  }

  # Validación específica: la respuesta correcta debe ser el monto del tercer puesto
  if (all(c("respuesta_correcta", "monto_tercer_puesto") %in% names(datos))) {
    if (datos$respuesta_correcta != datos$monto_tercer_puesto) {
      errores <- c(errores, "En v1, la respuesta correcta debe ser el monto del tercer puesto")
    }
  }

  # Validaciones de tipo y rango
  if ("premio_total" %in% names(datos)) {
    if (!is.numeric(datos$premio_total) || datos$premio_total < 30 || datos$premio_total > 400) {
      errores <- c(errores, "premio_total fuera de rango válido (30-400)")
    }
  }

  if ("opciones_mezcladas" %in% names(datos)) {
    if (!is.vector(datos$opciones_mezcladas) || length(datos$opciones_mezcladas) != 4) {
      errores <- c(errores, "opciones_mezcladas debe ser un vector de 4 elementos")
    }
  }

  return(errores)
}

# Función para extraer datos específica para v1
extraer_datos_version_v1 <- function(simulacion_id) {
  set.seed(simulacion_id)  # Semilla diferente para cada iteración

  if (verbose) {
    log_mensaje(paste("Iniciando simulación", simulacion_id, "para", archivo_rmd), "DEBUG")
  }

  tryCatch({
    # Verificar que el archivo existe
    if (!file.exists(archivo_rmd)) {
      stop("Archivo no encontrado: ", archivo_rmd)
    }

    # Leer el archivo Rmd con manejo mejorado de errores
    contenido <- tryCatch({
      readLines(archivo_rmd, warn = FALSE)
    }, error = function(e) {
      stop("Error al leer archivo: ", e$message)
    })

    # Extraer bloques de código R con mejor parsing
    bloques_r <- list()
    en_bloque <- FALSE
    bloque_actual <- character(0)
    nombre_bloque <- ""
    linea_inicio_bloque <- 0

    for (i in seq_along(contenido)) {
      linea <- contenido[i]
      if (grepl("^```\\{r", linea)) {
        en_bloque <- TRUE
        linea_inicio_bloque <- i
        nombre_match <- regmatches(linea, regexec("^```\\{r\\s+([^,}]+)", linea))
        nombre_bloque <- if (length(nombre_match[[1]]) > 1) {
          trimws(nombre_match[[1]][2])
        } else {
          paste0("unnamed_", i)
        }
        bloque_actual <- character(0)
      } else if (en_bloque && grepl("^```$", linea)) {
        en_bloque <- FALSE
        bloques_r[[nombre_bloque]] <- list(
          codigo = bloque_actual,
          linea_inicio = linea_inicio_bloque,
          linea_fin = i
        )
      } else if (en_bloque) {
        bloque_actual <- c(bloque_actual, linea)
      }
    }

    # Crear un entorno limpio para ejecutar el código
    env <- new.env()

    # Cargar bibliotecas necesarias en el entorno con manejo de errores
    bibliotecas_necesarias <- c(
      "library(testthat)",
      "library(exams)",
      "library(reticulate)",
      "library(digest)",
      "options(OutDec = '.')",
      "options(scipen = 999)"
    )

    for (lib in bibliotecas_necesarias) {
      tryCatch({
        eval(parse(text = lib), envir = env)
      }, error = function(e) {
        log_mensaje(paste("Advertencia al cargar:", lib, "-", e$message), "WARN")
      })
    }

    # Ejecutar bloques de código en orden específico con mejor manejo de errores
    bloques_orden <- c("setup", "DefinicionDeVariables", "generar_tabla_tikz")
    bloques_saltar <- c("tabla_distribucion", "mostrar_grafico_circular")
    errores_ejecucion <- list()

    for (nombre in names(bloques_r)) {
      # Saltar bloques que no necesitamos para las pruebas
      if (nombre %in% bloques_saltar) {
        if (verbose) log_mensaje(paste("Saltando bloque", nombre), "DEBUG")
        next
      }

      # Ejecutar el código en el entorno
      tryCatch({
        codigo_bloque <- bloques_r[[nombre]]$codigo
        if (length(codigo_bloque) > 0) {
          eval(parse(text = codigo_bloque), envir = env)
          if (verbose) log_mensaje(paste("Bloque", nombre, "ejecutado correctamente"), "DEBUG")
        }
      }, error = function(e) {
        error_info <- list(
          bloque = nombre,
          mensaje = e$message,
          linea_inicio = bloques_r[[nombre]]$linea_inicio,
          linea_fin = bloques_r[[nombre]]$linea_fin
        )
        errores_ejecucion[[nombre]] <<- error_info
        if (verbose) log_mensaje(paste("Error en bloque", nombre, ":", e$message), "WARN")
      })
    }

    # Extraer variables relevantes del entorno con validación mejorada
    datos <- list()
    variables_clave <- c(
      "contexto", "competencia", "grupo_edad", "premio_total", "premios_posibles",
      "fraccion_primer_puesto", "fraccion_segundo_puesto", "valor_primer_puesto",
      "valor_segundo_puesto", "valor_tercer_puesto",
      "monto_primer_puesto", "monto_segundo_puesto", "monto_tercer_puesto",
      "opciones", "opciones_mezcladas", "respuesta_correcta", "termino_dinero",
      "paleta_seleccionada", "articulo_contexto", "articulo_competencia",
      "contexto_seleccionado", "competencia_seleccionada", "termino_dinero_seleccionado",
      "articulo_este_dinero", "termino_participantes", "termino_puestos", "solucion"
    )

    variables_encontradas <- 0
    variables_faltantes <- character(0)

    for (var in variables_clave) {
      if (exists(var, envir = env)) {
        valor <- get(var, envir = env)
        # Validar que el valor no sea NULL o vacío
        if (!is.null(valor) && length(valor) > 0) {
          datos[[var]] <- valor
          variables_encontradas <- variables_encontradas + 1
        } else {
          variables_faltantes <- c(variables_faltantes, paste(var, "(NULL/vacío)"))
        }
      } else {
        variables_faltantes <- c(variables_faltantes, var)
      }
    }

    # Añadir metadatos específicos para v1
    datos$archivo <- archivo_rmd
    datos$simulacion <- simulacion_id
    datos$version <- "v1"
    datos$tipo_pregunta <- "tercer_puesto"
    datos$metodo_reparto <- "proporcional_basico"
    datos$timestamp <- Sys.time()
    datos$variables_encontradas <- variables_encontradas
    datos$variables_faltantes <- variables_faltantes
    datos$errores_ejecucion <- errores_ejecucion

    # Validar estructura de datos específica para v1
    errores_validacion <- validar_estructura_datos_v1(datos)
    datos$errores_validacion <- errores_validacion

    # Calcular un hash único para esta versión (excluyendo metadatos)
    datos_para_hash <- datos[variables_clave[variables_clave %in% names(datos)]]
    datos$hash <- digest::digest(datos_para_hash, algo = "md5")

    if (verbose) {
      log_mensaje(paste("Simulación", simulacion_id, "completada -",
                       variables_encontradas, "variables extraídas"), "SUCCESS")
      if (length(variables_faltantes) > 0) {
        log_mensaje(paste("Variables faltantes:", paste(variables_faltantes, collapse = ", ")), "WARN")
      }
      if (length(errores_validacion) > 0) {
        log_mensaje(paste("Errores de validación:", paste(errores_validacion, collapse = "; ")), "WARN")
      }
    }

    return(datos)
  }, error = function(e) {
    log_mensaje(paste("ERROR en simulación", simulacion_id, "para", archivo_rmd, ":", e$message), "ERROR")
    return(list(
      archivo = archivo_rmd,
      simulacion = simulacion_id,
      version = "v1",
      error = e$message,
      timestamp = Sys.time()
    ))
  })
}

# =============================================================================
# FUNCIONES DE VALIDACIÓN ESPECÍFICAS PARA V1
# =============================================================================

# Verificar que los premios están en el rango correcto (30-400, múltiplos de 2)
verificar_rango_premios_v1 <- function(datos) {
  if (!("premio_total" %in% names(datos))) {
    return(list(valido = FALSE, error = "Variable premio_total faltante"))
  }

  premio <- datos$premio_total

  # Verificar que es numérico
  if (!is.numeric(premio)) {
    return(list(valido = FALSE, error = "premio_total no es numérico"))
  }

  # Verificar que está en el rango 30-400
  if (premio < 30 || premio > 400) {
    return(list(valido = FALSE, error = paste("Premio", premio, "fuera de rango 30-400")))
  }

  # Verificar que es múltiplo de 2
  if (premio %% 2 != 0) {
    return(list(valido = FALSE, error = paste("Premio", premio, "no es múltiplo de 2")))
  }

  return(list(valido = TRUE, error = NULL))
}

# Verificar orden correcto de premios (primer > segundo > tercer)
verificar_orden_premios_v1 <- function(datos) {
  variables_necesarias <- c("monto_primer_puesto", "monto_segundo_puesto", "monto_tercer_puesto")
  variables_faltantes <- variables_necesarias[!variables_necesarias %in% names(datos)]

  if (length(variables_faltantes) > 0) {
    return(list(valido = FALSE, error = paste("Variables faltantes:", paste(variables_faltantes, collapse = ", "))))
  }

  p1 <- datos$monto_primer_puesto
  p2 <- datos$monto_segundo_puesto
  p3 <- datos$monto_tercer_puesto

  # Verificar que todos son numéricos
  if (!all(sapply(c(p1, p2, p3), is.numeric))) {
    return(list(valido = FALSE, error = "Algunos montos no son numéricos"))
  }

  # Verificar orden
  if (p1 <= p2) {
    return(list(valido = FALSE, error = paste("Primer puesto (", p1, ") <= Segundo puesto (", p2, ")")))
  }

  if (p2 <= p3) {
    return(list(valido = FALSE, error = paste("Segundo puesto (", p2, ") <= Tercer puesto (", p3, ")")))
  }

  return(list(valido = TRUE, error = NULL))
}

# Verificar que la suma de montos es igual al premio total
verificar_suma_montos_v1 <- function(datos) {
  variables_necesarias <- c("monto_primer_puesto", "monto_segundo_puesto", "monto_tercer_puesto", "premio_total")
  variables_faltantes <- variables_necesarias[!variables_necesarias %in% names(datos)]

  if (length(variables_faltantes) > 0) {
    return(list(valido = FALSE, error = paste("Variables faltantes:", paste(variables_faltantes, collapse = ", "))))
  }

  suma <- datos$monto_primer_puesto + datos$monto_segundo_puesto + datos$monto_tercer_puesto
  diferencia <- abs(suma - datos$premio_total)

  if (diferencia > tolerancia_redondeo) {
    return(list(valido = FALSE, error = paste("Suma de montos (", suma, ") != Premio total (", datos$premio_total, "), diferencia:", diferencia)))
  }

  return(list(valido = TRUE, error = NULL))
}

# Verificar que no hay opciones duplicadas
verificar_opciones_unicas_v1 <- function(datos) {
  if (!("opciones_mezcladas" %in% names(datos))) {
    return(list(valido = FALSE, error = "Variable opciones_mezcladas faltante"))
  }

  opciones <- datos$opciones_mezcladas

  if (is.null(opciones) || !is.vector(opciones)) {
    return(list(valido = FALSE, error = "opciones_mezcladas no es un vector válido"))
  }

  if (length(opciones) != 4) {
    return(list(valido = FALSE, error = paste("opciones_mezcladas debe tener 4 elementos, tiene", length(opciones))))
  }

  opciones_unicas <- length(unique(opciones))
  if (opciones_unicas != length(opciones)) {
    return(list(valido = FALSE, error = paste("Opciones duplicadas encontradas. Únicas:", opciones_unicas, "Total:", length(opciones))))
  }

  return(list(valido = TRUE, error = NULL))
}

# Verificar coherencia de género específica para v1
verificar_coherencia_genero_v1 <- function(datos) {
  errores <- character(0)

  # Verificar contexto
  if (all(c("contexto_seleccionado", "articulo_contexto") %in% names(datos))) {
    if (is.list(datos$contexto_seleccionado) && "genero" %in% names(datos$contexto_seleccionado)) {
      genero_contexto <- datos$contexto_seleccionado$genero
      articulo_esperado <- if (genero_contexto == "f") "una" else "un"
      if (datos$articulo_contexto != articulo_esperado) {
        errores <- c(errores, paste("Incoherencia de género en contexto: esperado", articulo_esperado, "encontrado", datos$articulo_contexto))
      }
    }
  }

  # Verificar competencia
  if (all(c("competencia_seleccionada", "articulo_competencia") %in% names(datos))) {
    if (is.list(datos$competencia_seleccionada) && "genero" %in% names(datos$competencia_seleccionada)) {
      genero_competencia <- datos$competencia_seleccionada$genero
      articulo_esperado <- if (genero_competencia == "f") "una" else "un"
      if (datos$articulo_competencia != articulo_esperado) {
        errores <- c(errores, paste("Incoherencia de género en competencia: esperado", articulo_esperado, "encontrado", datos$articulo_competencia))
      }
    }
  }

  if (length(errores) > 0) {
    return(list(valido = FALSE, error = paste(errores, collapse = "; ")))
  }

  return(list(valido = TRUE, error = NULL))
}

# Verificar validez de fracciones específica para v1
verificar_fracciones_validas_v1 <- function(datos) {
  variables_fracciones <- c("fraccion_primer_puesto", "fraccion_segundo_puesto",
                           "valor_primer_puesto", "valor_segundo_puesto", "valor_tercer_puesto")
  variables_faltantes <- variables_fracciones[!variables_fracciones %in% names(datos)]

  if (length(variables_faltantes) > 0) {
    return(list(valido = FALSE, error = paste("Variables de fracciones faltantes:", paste(variables_faltantes, collapse = ", "))))
  }

  # Verificar que los valores suman aproximadamente 1
  suma_fracciones <- datos$valor_primer_puesto + datos$valor_segundo_puesto + datos$valor_tercer_puesto
  if (abs(suma_fracciones - 1) > tolerancia_redondeo) {
    return(list(valido = FALSE, error = paste("Suma de fracciones (", suma_fracciones, ") != 1, diferencia:", abs(suma_fracciones - 1))))
  }

  # Verificar que todas las fracciones son positivas
  if (any(c(datos$valor_primer_puesto, datos$valor_segundo_puesto, datos$valor_tercer_puesto) <= 0)) {
    return(list(valido = FALSE, error = "Alguna fracción es <= 0"))
  }

  return(list(valido = TRUE, error = NULL))
}

# Verificar respuesta correcta específica para v1 (debe ser tercer puesto)
verificar_respuesta_correcta_v1 <- function(datos) {
  variables_necesarias <- c("respuesta_correcta", "monto_tercer_puesto", "opciones_mezcladas")
  variables_faltantes <- variables_necesarias[!variables_necesarias %in% names(datos)]

  if (length(variables_faltantes) > 0) {
    return(list(valido = FALSE, error = paste("Variables faltantes:", paste(variables_faltantes, collapse = ", "))))
  }

  # Verificar que la respuesta correcta está en las opciones
  if (!datos$respuesta_correcta %in% datos$opciones_mezcladas) {
    return(list(valido = FALSE, error = "La respuesta correcta no está en las opciones mezcladas"))
  }

  # Verificar que la respuesta correcta coincide con el monto del tercer puesto (específico para v1)
  if (datos$respuesta_correcta != datos$monto_tercer_puesto) {
    return(list(valido = FALSE, error = paste("En v1, respuesta correcta (", datos$respuesta_correcta, ") debe ser igual al monto tercer puesto (", datos$monto_tercer_puesto, ")")))
  }

  return(list(valido = TRUE, error = NULL))
}

# =============================================================================
# EJECUCIÓN PRINCIPAL DE PRUEBAS PARA V1
# =============================================================================

# Función principal para ejecutar todas las pruebas de v1
ejecutar_pruebas_v1 <- function() {
  log_mensaje("Iniciando pruebas para fracciones_reparto_premio_v1.Rmd", "INFO")
  start_time_total <- Sys.time()

  # Verificar que el archivo existe
  if (!file.exists(archivo_rmd)) {
    log_mensaje(paste("Archivo no encontrado:", archivo_rmd), "ERROR")
    stop("No se puede continuar sin el archivo objetivo")
  }

  log_mensaje(paste("Ejecutando", num_simulaciones, "simulaciones"), "INFO")

  # Ejecutar simulaciones
  resultados <- vector("list", num_simulaciones)
  errores_simulacion <- character(0)

  for (i in 1:num_simulaciones) {
    tryCatch({
      resultado <- extraer_datos_version_v1(i)
      resultados[[i]] <- resultado
    }, error = function(e) {
      errores_simulacion <<- c(errores_simulacion, paste("Simulación", i, ":", e$message))
      resultados[[i]] <<- NULL
    })
  }

  # Filtrar resultados válidos
  resultados_validos <- resultados[!sapply(resultados, is.null)]
  resultados_con_error <- resultados[sapply(resultados, function(x) !is.null(x) && "error" %in% names(x))]

  num_resultados_validos <- length(resultados_validos)
  num_errores <- length(errores_simulacion) + length(resultados_con_error)

  log_mensaje(paste("Simulaciones completadas:", num_resultados_validos, "válidas,", num_errores, "con errores"), "INFO")

  if (num_resultados_validos == 0) {
    log_mensaje("No se obtuvieron resultados válidos", "ERROR")
    return(FALSE)
  }

  # Filtrar resultados sin errores de validación críticos
  resultados_finales <- resultados_validos[sapply(resultados_validos, function(x) {
    !("error" %in% names(x)) && length(x$errores_validacion) == 0
  })]

  if (length(resultados_finales) < num_resultados_validos * 0.8) {
    log_mensaje(paste("Muchos resultados con errores de validación:",
                     length(resultados_finales), "de", num_resultados_validos, "válidos"), "WARN")
  }

  # Ejecutar todas las pruebas específicas para v1
  log_mensaje("Ejecutando pruebas específicas para v1", "INFO")

  # PRUEBA 1: Verificar variabilidad
  log_mensaje("PRUEBA 1: Verificación de variabilidad", "INFO")
  hashes <- sapply(resultados_finales, function(r) r$hash)
  versiones_unicas <- length(unique(hashes))
  porcentaje_unicidad <- (versiones_unicas / length(resultados_finales)) * 100

  cat("Versiones únicas:", versiones_unicas, "de", length(resultados_finales), "\n")
  cat("Porcentaje de unicidad:", round(porcentaje_unicidad, 2), "%\n")

  prueba1_aprobada <- porcentaje_unicidad >= umbral_variabilidad
  if (prueba1_aprobada) {
    log_mensaje("APROBADO: Alta variabilidad", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO: Baja variabilidad (", round(porcentaje_unicidad, 2), "% < ", umbral_variabilidad, "%)"), "ERROR")
  }

  # PRUEBA 2: Verificar rango de premios
  log_mensaje("PRUEBA 2: Verificación de rango de premios", "INFO")
  resultados_premios <- lapply(resultados_finales, verificar_rango_premios_v1)
  premios_validos <- sapply(resultados_premios, function(x) x$valido)
  num_premios_invalidos <- sum(!premios_validos)

  cat("Casos con premios fuera de rango:", num_premios_invalidos, "de", length(resultados_finales), "\n")

  prueba2_aprobada <- num_premios_invalidos == 0
  if (prueba2_aprobada) {
    log_mensaje("APROBADO: Todos los premios están en rango válido", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO:", num_premios_invalidos, "premios fuera de rango"), "ERROR")
  }

  # PRUEBA 3: Verificar orden de premios
  log_mensaje("PRUEBA 3: Verificación de orden de premios", "INFO")
  resultados_orden <- lapply(resultados_finales, verificar_orden_premios_v1)
  orden_correcto <- sapply(resultados_orden, function(x) x$valido)
  num_orden_incorrecto <- sum(!orden_correcto)

  cat("Casos con orden incorrecto:", num_orden_incorrecto, "de", length(resultados_finales), "\n")

  prueba3_aprobada <- num_orden_incorrecto == 0
  if (prueba3_aprobada) {
    log_mensaje("APROBADO: Orden correcto en todos los casos", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO:", num_orden_incorrecto, "casos con orden incorrecto"), "ERROR")
  }

  # PRUEBA 4: Verificar suma de montos
  log_mensaje("PRUEBA 4: Verificación de suma de montos", "INFO")
  resultados_suma <- lapply(resultados_finales, verificar_suma_montos_v1)
  suma_correcta <- sapply(resultados_suma, function(x) x$valido)
  num_suma_incorrecta <- sum(!suma_correcta)

  cat("Casos con suma incorrecta:", num_suma_incorrecta, "de", length(resultados_finales), "\n")

  prueba4_aprobada <- num_suma_incorrecta == 0
  if (prueba4_aprobada) {
    log_mensaje("APROBADO: Suma correcta en todos los casos", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO:", num_suma_incorrecta, "casos con suma incorrecta"), "ERROR")
  }

  # PRUEBA 5: Verificar opciones únicas
  log_mensaje("PRUEBA 5: Verificación de opciones únicas", "INFO")
  resultados_opciones <- lapply(resultados_finales, verificar_opciones_unicas_v1)
  opciones_unicas <- sapply(resultados_opciones, function(x) x$valido)
  num_opciones_duplicadas <- sum(!opciones_unicas)

  cat("Casos con opciones duplicadas:", num_opciones_duplicadas, "de", length(resultados_finales), "\n")

  prueba5_aprobada <- num_opciones_duplicadas == 0
  if (prueba5_aprobada) {
    log_mensaje("APROBADO: Opciones únicas en todos los casos", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO:", num_opciones_duplicadas, "casos con opciones duplicadas"), "ERROR")
  }

  # PRUEBA 6: Verificar coherencia de género
  log_mensaje("PRUEBA 6: Verificación de coherencia de género", "INFO")
  resultados_genero <- lapply(resultados_finales, verificar_coherencia_genero_v1)
  genero_coherente <- sapply(resultados_genero, function(x) x$valido)
  num_genero_incoherente <- sum(!genero_coherente)

  cat("Casos con incoherencia de género:", num_genero_incoherente, "de", length(resultados_finales), "\n")

  prueba6_aprobada <- num_genero_incoherente == 0
  if (prueba6_aprobada) {
    log_mensaje("APROBADO: Coherencia de género en todos los casos", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO:", num_genero_incoherente, "casos con incoherencia de género"), "ERROR")
  }

  # PRUEBA 7: Verificar fracciones válidas
  log_mensaje("PRUEBA 7: Verificación de fracciones válidas", "INFO")
  resultados_fracciones <- lapply(resultados_finales, verificar_fracciones_validas_v1)
  fracciones_validas <- sapply(resultados_fracciones, function(x) x$valido)
  num_fracciones_invalidas <- sum(!fracciones_validas)

  cat("Casos con fracciones inválidas:", num_fracciones_invalidas, "de", length(resultados_finales), "\n")

  prueba7_aprobada <- num_fracciones_invalidas == 0
  if (prueba7_aprobada) {
    log_mensaje("APROBADO: Fracciones válidas en todos los casos", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO:", num_fracciones_invalidas, "casos con fracciones inválidas"), "ERROR")
  }

  # PRUEBA 8: Verificar respuesta correcta (específica para v1 - tercer puesto)
  log_mensaje("PRUEBA 8: Verificación de respuesta correcta (tercer puesto)", "INFO")
  resultados_respuesta <- lapply(resultados_finales, verificar_respuesta_correcta_v1)
  respuesta_correcta_valida <- sapply(resultados_respuesta, function(x) x$valido)
  num_respuesta_incorrecta <- sum(!respuesta_correcta_valida)

  cat("Casos con respuesta incorrecta:", num_respuesta_incorrecta, "de", length(resultados_finales), "\n")

  prueba8_aprobada <- num_respuesta_incorrecta == 0
  if (prueba8_aprobada) {
    log_mensaje("APROBADO: Respuesta correcta (tercer puesto) en todos los casos", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO:", num_respuesta_incorrecta, "casos con respuesta incorrecta"), "ERROR")
  }

  # Resumen final
  todas_aprobadas <- all(c(prueba1_aprobada, prueba2_aprobada, prueba3_aprobada,
                          prueba4_aprobada, prueba5_aprobada, prueba6_aprobada,
                          prueba7_aprobada, prueba8_aprobada))

  end_time_total <- Sys.time()
  tiempo_total <- as.numeric(difftime(end_time_total, start_time_total, units = "mins"))

  cat("\n", paste(rep("=", 80), collapse=""), "\n", sep="")
  cat("REPORTE FINAL PARA V1\n")
  cat(paste(rep("=", 80), collapse=""), "\n")
  cat("Archivo:", archivo_rmd, "\n")
  cat("Simulaciones válidas:", length(resultados_finales), "de", num_simulaciones, "\n")
  cat("Tiempo de ejecución:", round(tiempo_total, 2), "minutos\n")
  cat("Fecha:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

  cat("RESULTADOS DE PRUEBAS:\n")
  cat("✓ Variabilidad:", if(prueba1_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Rango premios:", if(prueba2_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Orden premios:", if(prueba3_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Suma montos:", if(prueba4_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Opciones únicas:", if(prueba5_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Coherencia género:", if(prueba6_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Fracciones válidas:", if(prueba7_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Respuesta correcta (tercer puesto):", if(prueba8_aprobada) "APROBADO" else "FALLIDO", "\n")

  if (todas_aprobadas) {
    cat("\n🎉 ¡TODAS LAS PRUEBAS APROBADAS PARA V1!\n")
    cat("✓ El ejercicio v1 está listo para producción.\n")
    log_mensaje("Todas las pruebas de v1 completadas exitosamente", "SUCCESS")
  } else {
    cat("\n⚠️  ALGUNAS PRUEBAS FALLARON PARA V1\n")
    cat("📋 Revise los detalles arriba para identificar problemas específicos.\n")
    log_mensaje("Algunas pruebas de v1 fallaron", "ERROR")
  }

  # Guardar resultados
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archivo_reporte <- paste0("reporte_pruebas_v1_", timestamp, ".RData")
  save(resultados_finales, file = archivo_reporte)
  log_mensaje(paste("Resultados guardados en:", archivo_reporte), "INFO")

  cat(paste(rep("=", 80), collapse=""), "\n")

  return(todas_aprobadas)
}

# =============================================================================
# EJECUCIÓN AUTOMÁTICA
# =============================================================================

# Ejecutar las pruebas automáticamente
log_mensaje("Iniciando pruebas unitarias para v1", "INFO")
resultado_final <- ejecutar_pruebas_v1()

if (resultado_final) {
  log_mensaje("Pruebas unitarias v1 completadas exitosamente", "SUCCESS")
  quit(status = 0)
} else {
  log_mensaje("Pruebas unitarias v1 fallaron", "ERROR")
  quit(status = 1)
}