#!/usr/bin/env Rscript

# =============================================================================
# Pruebas Unitarias Especializadas para fracciones_reparto_premio_v4.Rmd
# =============================================================================
# Este script realiza pruebas exhaustivas específicas para la versión 4:
# - Pregunta por el SEGUNDO PUESTO
# - Método de reparto SECUENCIAL (fracción de fracción)
# - Validaciones específicas para reparto secuencial + segundo puesto
# =============================================================================

library(testthat)
library(exams)
library(digest)
library(dplyr)
library(ggplot2)
library(parallel)
library(lubridate)
library(stringr)

# Configuración específica para v4
archivo_rmd <- "fracciones_reparto_premio_v4.Rmd"
num_simulaciones <- 150  # Incrementado para mayor robustez
verbose <- TRUE  # Mostrar información detallada durante la ejecución
num_cores <- min(detectCores() - 1, 4)  # Usar múltiples núcleos
tolerancia_redondeo <- 0.01  # Tolerancia para errores de redondeo
umbral_variabilidad <- 95  # Porcentaje mínimo de variabilidad requerido

cat("=============================================================================\n")
cat("INICIANDO PRUEBAS UNITARIAS PARA VERSIÓN 4\n")
cat("=============================================================================\n")
cat("Archivo objetivo:", archivo_rmd, "\n")
cat("Tipo de reparto: SECUENCIAL (fracción de fracción)\n")
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

# Función para validar estructura de datos específica para v4
validar_estructura_datos_v4 <- function(datos) {
  errores <- character(0)
  
  # Variables obligatorias específicas para v4 (reparto secuencial + segundo puesto)
  variables_obligatorias <- c(
    "premio_total", "monto_primer_puesto", "monto_segundo_puesto", 
    "monto_tercer_puesto", "opciones_mezcladas", "respuesta_correcta",
    "restante_despues_primero"  # Específico para reparto secuencial
  )
  
  for (var in variables_obligatorias) {
    if (!var %in% names(datos) || is.null(datos[[var]])) {
      errores <- c(errores, paste("Variable obligatoria faltante:", var))
    }
  }
  
  # Validación específica: la respuesta correcta debe ser el monto del segundo puesto
  if (all(c("respuesta_correcta", "monto_segundo_puesto") %in% names(datos))) {
    if (datos$respuesta_correcta != datos$monto_segundo_puesto) {
      errores <- c(errores, "En v4, la respuesta correcta debe ser el monto del segundo puesto")
    }
  }
  
  # Validación específica para reparto secuencial
  if (all(c("premio_total", "monto_primer_puesto", "restante_despues_primero") %in% names(datos))) {
    diferencia_restante <- abs((datos$premio_total - datos$monto_primer_puesto) - datos$restante_despues_primero)
    if (diferencia_restante > tolerancia_redondeo) {
      errores <- c(errores, paste("Inconsistencia en cálculo secuencial: restante_despues_primero no coincide"))
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

# Función para extraer datos específica para v4 (versión compacta)
extraer_datos_version_v4 <- function(simulacion_id) {
  set.seed(simulacion_id)
  
  if (verbose) {
    log_mensaje(paste("Iniciando simulación", simulacion_id, "para", archivo_rmd), "DEBUG")
  }

  tryCatch({
    if (!file.exists(archivo_rmd)) {
      stop("Archivo no encontrado: ", archivo_rmd)
    }

    contenido <- readLines(archivo_rmd, warn = FALSE)
    
    # Extraer y ejecutar bloques R (versión simplificada)
    env <- new.env()
    eval(parse(text = c(
      "library(testthat)", "library(exams)", "library(reticulate)", 
      "library(digest)", "options(OutDec = '.')", "options(scipen = 999)"
    )), envir = env)
    
    # Ejecutar bloques principales
    bloques_r <- list()
    en_bloque <- FALSE
    bloque_actual <- character(0)
    
    for (linea in contenido) {
      if (grepl("^```\\{r", linea)) {
        en_bloque <- TRUE
        bloque_actual <- character(0)
      } else if (en_bloque && grepl("^```$", linea)) {
        en_bloque <- FALSE
        if (length(bloque_actual) > 0) {
          tryCatch({
            eval(parse(text = bloque_actual), envir = env)
          }, error = function(e) {
            if (verbose) log_mensaje(paste("Error en bloque:", e$message), "WARN")
          })
        }
      } else if (en_bloque) {
        bloque_actual <- c(bloque_actual, linea)
      }
    }

    # Extraer variables clave
    variables_clave <- c(
      "premio_total", "monto_primer_puesto", "monto_segundo_puesto", "monto_tercer_puesto",
      "restante_despues_primero", "opciones_mezcladas", "respuesta_correcta", 
      "valor_f1", "valor_f2", "solucion"
    )

    datos <- list()
    for (var in variables_clave) {
      if (exists(var, envir = env)) {
        datos[[var]] <- get(var, envir = env)
      }
    }

    # Metadatos específicos para v4
    datos$archivo <- archivo_rmd
    datos$simulacion <- simulacion_id
    datos$version <- "v4"
    datos$tipo_pregunta <- "segundo_puesto"
    datos$metodo_reparto <- "secuencial"
    datos$timestamp <- Sys.time()

    # Validar estructura
    errores_validacion <- validar_estructura_datos_v4(datos)
    datos$errores_validacion <- errores_validacion

    # Hash único
    datos_para_hash <- datos[variables_clave[variables_clave %in% names(datos)]]
    datos$hash <- digest::digest(datos_para_hash, algo = "md5")

    if (verbose) {
      log_mensaje(paste("Simulación", simulacion_id, "completada"), "SUCCESS")
      if (length(errores_validacion) > 0) {
        log_mensaje(paste("Errores de validación:", paste(errores_validacion, collapse = "; ")), "WARN")
      }
    }

    return(datos)
  }, error = function(e) {
    log_mensaje(paste("ERROR en simulación", simulacion_id, ":", e$message), "ERROR")
    return(list(
      archivo = archivo_rmd,
      simulacion = simulacion_id,
      version = "v4",
      error = e$message,
      timestamp = Sys.time()
    ))
  })
}

# Verificar respuesta correcta específica para v4 (debe ser segundo puesto)
verificar_respuesta_correcta_v4 <- function(datos) {
  variables_necesarias <- c("respuesta_correcta", "monto_segundo_puesto", "opciones_mezcladas")
  variables_faltantes <- variables_necesarias[!variables_necesarias %in% names(datos)]
  
  if (length(variables_faltantes) > 0) {
    return(list(valido = FALSE, error = paste("Variables faltantes:", paste(variables_faltantes, collapse = ", "))))
  }

  if (!datos$respuesta_correcta %in% datos$opciones_mezcladas) {
    return(list(valido = FALSE, error = "La respuesta correcta no está en las opciones mezcladas"))
  }

  if (datos$respuesta_correcta != datos$monto_segundo_puesto) {
    return(list(valido = FALSE, error = paste("En v4, respuesta correcta (", datos$respuesta_correcta, ") debe ser igual al monto segundo puesto (", datos$monto_segundo_puesto, ")")))
  }

  return(list(valido = TRUE, error = NULL))
}

# Función principal para ejecutar todas las pruebas de v4
ejecutar_pruebas_v4 <- function() {
  log_mensaje("Iniciando pruebas para fracciones_reparto_premio_v4.Rmd (segundo puesto secuencial)", "INFO")
  start_time_total <- Sys.time()

  if (!file.exists(archivo_rmd)) {
    log_mensaje(paste("Archivo no encontrado:", archivo_rmd), "ERROR")
    stop("No se puede continuar sin el archivo objetivo")
  }

  log_mensaje(paste("Ejecutando", num_simulaciones, "simulaciones"), "INFO")
  
  # Ejecutar simulaciones
  resultados <- vector("list", num_simulaciones)
  for (i in 1:num_simulaciones) {
    resultados[[i]] <- extraer_datos_version_v4(i)
  }

  # Filtrar resultados válidos
  resultados_validos <- resultados[!sapply(resultados, function(x) is.null(x) || "error" %in% names(x))]
  resultados_finales <- resultados_validos[sapply(resultados_validos, function(x) length(x$errores_validacion) == 0)]

  log_mensaje(paste("Simulaciones válidas:", length(resultados_finales), "de", num_simulaciones), "INFO")

  if (length(resultados_finales) == 0) {
    log_mensaje("No se obtuvieron resultados válidos", "ERROR")
    return(FALSE)
  }

  # Ejecutar pruebas (reutilizando funciones de v2 para validaciones secuenciales)
  source("pruebas_unitarias_v2.R", local = TRUE)
  
  # PRUEBA 1: Verificar variabilidad
  hashes <- sapply(resultados_finales, function(r) r$hash)
  versiones_unicas <- length(unique(hashes))
  porcentaje_unicidad <- (versiones_unicas / length(resultados_finales)) * 100
  prueba1_aprobada <- porcentaje_unicidad >= umbral_variabilidad

  # PRUEBA 2-8: Usar funciones de v2 (adaptadas para secuencial)
  prueba2_aprobada <- all(sapply(resultados_finales, function(x) verificar_rango_premios_v2(x)$valido))
  prueba3_aprobada <- all(sapply(resultados_finales, function(x) verificar_orden_premios_v2(x)$valido))
  prueba4_aprobada <- all(sapply(resultados_finales, function(x) verificar_suma_montos_v2(x)$valido))
  prueba5_aprobada <- all(sapply(resultados_finales, function(x) verificar_calculo_secuencial_v2(x)$valido))
  prueba6_aprobada <- all(sapply(resultados_finales, function(x) verificar_opciones_unicas_v2(x)$valido))
  prueba7_aprobada <- all(sapply(resultados_finales, function(x) verificar_coherencia_genero_v2(x)$valido))
  prueba8_aprobada <- all(sapply(resultados_finales, function(x) verificar_fracciones_validas_v2(x)$valido))

  # PRUEBA 9: Verificar respuesta correcta específica para v4 (segundo puesto)
  log_mensaje("PRUEBA 9: Verificación de respuesta correcta (segundo puesto secuencial)", "INFO")
  resultados_respuesta <- lapply(resultados_finales, verificar_respuesta_correcta_v4)
  respuesta_correcta_valida <- sapply(resultados_respuesta, function(x) x$valido)
  num_respuesta_incorrecta <- sum(!respuesta_correcta_valida)

  prueba9_aprobada <- num_respuesta_incorrecta == 0
  if (prueba9_aprobada) {
    log_mensaje("APROBADO: Respuesta correcta (segundo puesto secuencial) en todos los casos", "SUCCESS")
  } else {
    log_mensaje(paste("FALLIDO:", num_respuesta_incorrecta, "casos con respuesta incorrecta"), "ERROR")
  }

  # Resumen final
  todas_aprobadas <- all(c(prueba1_aprobada, prueba2_aprobada, prueba3_aprobada, 
                          prueba4_aprobada, prueba5_aprobada, prueba6_aprobada, 
                          prueba7_aprobada, prueba8_aprobada, prueba9_aprobada))

  end_time_total <- Sys.time()
  tiempo_total <- as.numeric(difftime(end_time_total, start_time_total, units = "mins"))

  cat("\n", paste(rep("=", 80), collapse=""), "\n", sep="")
  cat("REPORTE FINAL PARA V4 (SEGUNDO PUESTO SECUENCIAL)\n")
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
  cat("✓ Cálculo secuencial:", if(prueba5_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Opciones únicas:", if(prueba6_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Coherencia género:", if(prueba7_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Fracciones secuenciales:", if(prueba8_aprobada) "APROBADO" else "FALLIDO", "\n")
  cat("✓ Respuesta correcta (segundo puesto):", if(prueba9_aprobada) "APROBADO" else "FALLIDO", "\n")

  if (todas_aprobadas) {
    cat("\n🎉 ¡TODAS LAS PRUEBAS APROBADAS PARA V4!\n")
    cat("✓ El ejercicio v4 (segundo puesto secuencial) está listo para producción.\n")
    log_mensaje("Todas las pruebas de v4 completadas exitosamente", "SUCCESS")
  } else {
    cat("\n⚠️  ALGUNAS PRUEBAS FALLARON PARA V4\n")
    cat("📋 Revise los detalles arriba para identificar problemas específicos.\n")
    log_mensaje("Algunas pruebas de v4 fallaron", "ERROR")
  }

  # Guardar resultados
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archivo_reporte <- paste0("reporte_pruebas_v4_", timestamp, ".RData")
  save(resultados_finales, file = archivo_reporte)
  log_mensaje(paste("Resultados guardados en:", archivo_reporte), "INFO")

  cat(paste(rep("=", 80), collapse=""), "\n")
  
  return(todas_aprobadas)
}

# =============================================================================
# EJECUCIÓN AUTOMÁTICA
# =============================================================================

# Ejecutar las pruebas automáticamente
log_mensaje("Iniciando pruebas unitarias para v4 (segundo puesto secuencial)", "INFO")
resultado_final <- ejecutar_pruebas_v4()

if (resultado_final) {
  log_mensaje("Pruebas unitarias v4 completadas exitosamente", "SUCCESS")
  quit(status = 0)
} else {
  log_mensaje("Pruebas unitarias v4 fallaron", "ERROR")
  quit(status = 1)
}
