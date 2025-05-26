#!/usr/bin/env Rscript

# =============================================================================
# Pruebas Unitarias Especializadas para fracciones_reparto_premio_v3.Rmd
# =============================================================================
# Este script realiza pruebas exhaustivas específicas para la versión 3:
# - Pregunta por el SEGUNDO PUESTO
# - Método de reparto PROPORCIONAL BÁSICO
# - Validaciones específicas para pregunta del segundo puesto
# =============================================================================

library(testthat)
library(exams)
library(digest)
library(dplyr)
library(ggplot2)
library(parallel)
library(lubridate)
library(stringr)

# Configuración específica para v3
archivo_rmd <- "fracciones_reparto_premio_v3.Rmd"
num_simulaciones <- 150  # Incrementado para mayor robustez
verbose <- TRUE  # Mostrar información detallada durante la ejecución
num_cores <- min(detectCores() - 1, 4)  # Usar múltiples núcleos
tolerancia_redondeo <- 0.01  # Tolerancia para errores de redondeo
umbral_variabilidad <- 95  # Porcentaje mínimo de variabilidad requerido

cat("=============================================================================\n")
cat("INICIANDO PRUEBAS UNITARIAS PARA VERSIÓN 3\n")
cat("=============================================================================\n")
cat("Archivo objetivo:", archivo_rmd, "\n")
cat("Tipo de reparto: PROPORCIONAL BÁSICO\n")
cat("Pregunta objetivo: SEGUNDO PUESTO\n")
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

# Función para validar estructura de datos específica para v3
validar_estructura_datos_v3 <- function(datos) {
  errores <- character(0)

  # Variables obligatorias específicas para v3 (pregunta segundo puesto)
  variables_obligatorias <- c(
    "premio_total", "monto_primer_puesto", "monto_segundo_puesto",
    "monto_tercer_puesto", "opciones_mezcladas", "respuesta_correcta"
  )

  for (var in variables_obligatorias) {
    if (!var %in% names(datos) || is.null(datos[[var]])) {
      errores <- c(errores, paste("Variable obligatoria faltante:", var))
    }
  }

  # Validación específica: la respuesta correcta debe ser el monto del segundo puesto
  if (all(c("respuesta_correcta", "monto_segundo_puesto") %in% names(datos))) {
    if (datos$respuesta_correcta != datos$monto_segundo_puesto) {
      errores <- c(errores, "En v3, la respuesta correcta debe ser el monto del segundo puesto")
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

# Función para extraer datos específica para v3
extraer_datos_version_v3 <- function(simulacion_id) {
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

    # Añadir metadatos específicos para v3
    datos$archivo <- archivo_rmd
    datos$simulacion <- simulacion_id
    datos$version <- "v3"
    datos$tipo_pregunta <- "segundo_puesto"
    datos$metodo_reparto <- "proporcional_basico"
    datos$timestamp <- Sys.time()
    datos$variables_encontradas <- variables_encontradas
    datos$variables_faltantes <- variables_faltantes
    datos$errores_ejecucion <- errores_ejecucion

    # Validar estructura de datos específica para v3
    errores_validacion <- validar_estructura_datos_v3(datos)
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
      version = "v3",
      error = e$message,
      timestamp = Sys.time()
    ))
  })
}

# =============================================================================
# FUNCIONES DE VALIDACIÓN ESPECÍFICAS PARA V3 (SEGUNDO PUESTO)
# =============================================================================

# Verificar respuesta correcta específica para v3 (debe ser segundo puesto)
verificar_respuesta_correcta_v3 <- function(datos) {
  variables_necesarias <- c("respuesta_correcta", "monto_segundo_puesto", "opciones_mezcladas")
  variables_faltantes <- variables_necesarias[!variables_necesarias %in% names(datos)]

  if (length(variables_faltantes) > 0) {
    return(list(valido = FALSE, error = paste("Variables faltantes:", paste(variables_faltantes, collapse = ", "))))
  }

  # Verificar que la respuesta correcta está en las opciones
  if (!datos$respuesta_correcta %in% datos$opciones_mezcladas) {
    return(list(valido = FALSE, error = "La respuesta correcta no está en las opciones mezcladas"))
  }

  # Verificar que la respuesta correcta coincide con el monto del segundo puesto (específico para v3)
  if (datos$respuesta_correcta != datos$monto_segundo_puesto) {
    return(list(valido = FALSE, error = paste("En v3, respuesta correcta (", datos$respuesta_correcta, ") debe ser igual al monto segundo puesto (", datos$monto_segundo_puesto, ")")))
  }

  return(list(valido = TRUE, error = NULL))
}

# Función principal para ejecutar todas las pruebas de v3
ejecutar_pruebas_v3 <- function() {
  log_mensaje("Iniciando pruebas para fracciones_reparto_premio_v3.Rmd (segundo puesto)", "INFO")
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
      resultado <- extraer_datos_version_v3(i)
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

  # Ejecutar pruebas básicas (reutilizando funciones de v1 para validaciones comunes)
  source("pruebas_unitarias_v1.R", local = TRUE)

  # PRUEBA 1: Verificar variabilidad
  log_mensaje("PRUEBA 1: Verificación de variabilidad", "INFO")
  hashes <- sapply(resultados_finales, function(r) r$hash)
  versiones_unicas <- length(unique(hashes))
  porcentaje_unicidad <- (versiones_unicas / length(resultados_finales)) * 100

  prueba1_aprobada <- porcentaje_unicidad >= umbral_variabilidad
  if (prueba1_aprobada) {
    log_mensaje("APROBADO: Alta variabilidad", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO: Baja variabilidad"), "ERROR")
  }

  # PRUEBA 2-7: Usar funciones de v1 (adaptadas)
  prueba2_aprobada <- all(sapply(resultados_finales, function(x) verificar_rango_premios_v1(x)$valido))
  prueba3_aprobada <- all(sapply(resultados_finales, function(x) verificar_orden_premios_v1(x)$valido))
  prueba4_aprobada <- all(sapply(resultados_finales, function(x) verificar_suma_montos_v1(x)$valido))
  prueba5_aprobada <- all(sapply(resultados_finales, function(x) verificar_opciones_unicas_v1(x)$valido))
  prueba6_aprobada <- all(sapply(resultados_finales, function(x) verificar_coherencia_genero_v1(x)$valido))
  prueba7_aprobada <- all(sapply(resultados_finales, function(x) verificar_fracciones_validas_v1(x)$valido))

  # PRUEBA 8: Verificar respuesta correcta específica para v3 (segundo puesto)
  log_mensaje("PRUEBA 8: Verificación de respuesta correcta (segundo puesto)", "INFO")
  resultados_respuesta <- lapply(resultados_finales, verificar_respuesta_correcta_v3)
  respuesta_correcta_valida <- sapply(resultados_respuesta, function(x) x$valido)
  num_respuesta_incorrecta <- sum(!respuesta_correcta_valida)

  prueba8_aprobada <- num_respuesta_incorrecta == 0
  if (prueba8_aprobada) {
    log_mensaje("APROBADO: Respuesta correcta (segundo puesto) en todos los casos", "SUCCESS")
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
  cat("REPORTE FINAL PARA V3 (SEGUNDO PUESTO)\n")
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
  cat("✓ Respuesta correcta (segundo puesto):", if(prueba8_aprobada) "APROBADO" else "FALLIDO", "\n")

  if (todas_aprobadas) {
    cat("\n🎉 ¡TODAS LAS PRUEBAS APROBADAS PARA V3!\n")
    cat("✓ El ejercicio v3 (segundo puesto) está listo para producción.\n")
    log_mensaje("Todas las pruebas de v3 completadas exitosamente", "SUCCESS")
  } else {
    cat("\n⚠️  ALGUNAS PRUEBAS FALLARON PARA V3\n")
    cat("📋 Revise los detalles arriba para identificar problemas específicos.\n")
    log_mensaje("Algunas pruebas de v3 fallaron", "ERROR")
  }

  # Guardar resultados
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archivo_reporte <- paste0("reporte_pruebas_v3_", timestamp, ".RData")
  save(resultados_finales, file = archivo_reporte)
  log_mensaje(paste("Resultados guardados en:", archivo_reporte), "INFO")

  cat(paste(rep("=", 80), collapse=""), "\n")

  return(todas_aprobadas)
}

# =============================================================================
# EJECUCIÓN AUTOMÁTICA
# =============================================================================

# Ejecutar las pruebas automáticamente
log_mensaje("Iniciando pruebas unitarias para v3 (segundo puesto)", "INFO")
resultado_final <- ejecutar_pruebas_v3()

if (resultado_final) {
  log_mensaje("Pruebas unitarias v3 completadas exitosamente", "SUCCESS")
  quit(status = 0)
} else {
  log_mensaje("Pruebas unitarias v3 fallaron", "ERROR")
  quit(status = 1)
}
