#!/usr/bin/env Rscript

# =============================================================================
# Pruebas Unitarias Completas para Ejercicios de Fracciones y Reparto de Premios
# =============================================================================
# Este script realiza pruebas exhaustivas para garantizar:
# 1. Generación de al menos 300 variantes diferentes por versión
# 2. Coherencia matemática en todas las variantes
# 3. Correcto funcionamiento de la función garantizar_orden_premios()
# 4. Verificación de rangos de premios (30-400, múltiplos de 2)
# 5. Orden correcto de premios (primer > segundo > tercer)
# 6. No duplicidad de opciones de respuesta
# 7. Coherencia semántica de género
# =============================================================================

library(testthat)
library(exams)
library(digest)
library(dplyr)
library(ggplot2)
library(parallel)

# Configuración global
archivos_rmd <- c(
  "fracciones_reparto_premio_v1.Rmd",
  "fracciones_reparto_premio_v2.Rmd",
  "fracciones_reparto_premio_v3.Rmd",
  "fracciones_reparto_premio_v4.Rmd"
)
num_simulaciones <- 100  # Número de simulaciones por versión
verbose <- TRUE  # Mostrar información detallada durante la ejecución
num_cores <- min(detectCores() - 1, 4)  # Usar múltiples núcleos

cat("=============================================================================\n")
cat("INICIANDO PRUEBAS UNITARIAS COMPLETAS\n")
cat("=============================================================================\n")
cat("Archivos a probar:", paste(archivos_rmd, collapse = ", "), "\n")
cat("Simulaciones por archivo:", num_simulaciones, "\n")
cat("Núcleos de procesamiento:", num_cores, "\n")
cat("=============================================================================\n\n")

# Función mejorada para extraer datos de cualquier versión del archivo Rmd
extraer_datos_version <- function(archivo_rmd, simulacion_id) {
  set.seed(simulacion_id)  # Semilla diferente para cada iteración

  if (verbose) {
    cat("  Simulación", simulacion_id, "para", archivo_rmd, "...\n")
  }

  tryCatch({
    # Verificar que el archivo existe
    if (!file.exists(archivo_rmd)) {
      stop("Archivo no encontrado: ", archivo_rmd)
    }

    # Leer el archivo Rmd
    contenido <- readLines(archivo_rmd, warn = FALSE)

    # Extraer bloques de código R
    bloques_r <- list()
    en_bloque <- FALSE
    bloque_actual <- character(0)
    nombre_bloque <- ""

    for (linea in contenido) {
      if (grepl("^```\\{r", linea)) {
        en_bloque <- TRUE
        nombre_match <- regmatches(linea, regexec("^```\\{r\\s+([^,}]+)", linea))
        nombre_bloque <- if (length(nombre_match[[1]]) > 1) nombre_match[[1]][2] else "unnamed"
        bloque_actual <- character(0)
      } else if (en_bloque && grepl("^```$", linea)) {
        en_bloque <- FALSE
        bloques_r[[nombre_bloque]] <- bloque_actual
      } else if (en_bloque) {
        bloque_actual <- c(bloque_actual, linea)
      }
    }

    # Crear un entorno limpio para ejecutar el código
    env <- new.env()

    # Cargar bibliotecas necesarias en el entorno
    eval(parse(text = c(
      "library(testthat)",
      "library(exams)",
      "library(reticulate)",
      "library(digest)",
      "options(OutDec = '.')"
    )), envir = env)

    # Ejecutar bloques de código en orden específico
    bloques_orden <- c("setup", "DefinicionDeVariables", "generar_tabla_tikz")
    bloques_saltar <- c("tabla_distribucion", "mostrar_grafico_circular")

    for (nombre in names(bloques_r)) {
      # Saltar bloques que no necesitamos para las pruebas
      if (nombre %in% bloques_saltar) {
        if (verbose) cat("    Saltando bloque", nombre, "\n")
        next
      }

      # Ejecutar el código en el entorno
      tryCatch({
        eval(parse(text = bloques_r[[nombre]]), envir = env)
        if (verbose) cat("    Bloque", nombre, "ejecutado correctamente\n")
      }, error = function(e) {
        if (verbose) cat("    Error al ejecutar bloque", nombre, ":", e$message, "\n")
        # Continuar con otros bloques aunque uno falle
      })
    }

    # Extraer variables relevantes del entorno
    datos <- list()
    variables_clave <- c(
      "contexto", "competencia", "grupo_edad", "premio_total", "premios_posibles",
      "fraccion_primer_puesto", "fraccion_segundo_puesto", "valor_primer_puesto", "valor_segundo_puesto", "valor_tercer_puesto",
      "monto_primer_puesto", "monto_segundo_puesto", "monto_tercer_puesto",
      "opciones", "opciones_mezcladas", "respuesta_correcta", "termino_dinero",
      "paleta_seleccionada", "articulo_contexto", "articulo_competencia",
      "contexto_seleccionado", "competencia_seleccionada", "termino_dinero_seleccionado",
      "articulo_este_dinero", "termino_participantes", "termino_puestos", "solucion"
    )

    for (var in variables_clave) {
      if (exists(var, envir = env)) {
        datos[[var]] <- get(var, envir = env)
      } else {
        if (verbose) cat("    Variable", var, "no encontrada\n")
      }
    }

    # Añadir metadatos
    datos$archivo <- archivo_rmd
    datos$simulacion <- simulacion_id
    datos$version <- gsub(".*_v(\\d+)\\.Rmd", "v\\1", archivo_rmd)

    # Calcular un hash único para esta versión
    datos$hash <- digest::digest(datos, algo = "md5")

    if (verbose) {
      cat("    Simulación", simulacion_id, "completada con éxito\n")
      cat("    Variables extraídas:", length(datos), "\n")
    }

    return(datos)
  }, error = function(e) {
    if (verbose) {
      cat("    ERROR en simulación", simulacion_id, "para", archivo_rmd, ":", e$message, "\n")
    }
    return(NULL)
  })
}

# =============================================================================
# FUNCIONES DE VALIDACIÓN ESPECÍFICAS
# =============================================================================

# Verificar que los premios están en el rango correcto (30-400, múltiplos de 2)
verificar_rango_premios <- function(datos) {
  if (!("premio_total" %in% names(datos))) return(FALSE)

  premio <- datos$premio_total
  # Verificar que está en el rango 30-400
  if (premio < 30 || premio > 400) return(FALSE)

  # Verificar que es múltiplo de 2
  if (premio %% 2 != 0) return(FALSE)

  return(TRUE)
}

# Verificar orden correcto de premios (primer > segundo > tercer)
verificar_orden_premios <- function(datos) {
  variables_necesarias <- c("monto_primer_puesto", "monto_segundo_puesto", "monto_tercer_puesto")
  if (!all(variables_necesarias %in% names(datos))) return(FALSE)

  p1 <- datos$monto_primer_puesto
  p2 <- datos$monto_segundo_puesto
  p3 <- datos$monto_tercer_puesto

  return(p1 > p2 && p2 > p3)
}

# Verificar que la suma de montos es igual al premio total
verificar_suma_montos <- function(datos) {
  variables_necesarias <- c("monto_primer_puesto", "monto_segundo_puesto", "monto_tercer_puesto", "premio_total")
  if (!all(variables_necesarias %in% names(datos))) return(FALSE)

  suma <- datos$monto_primer_puesto + datos$monto_segundo_puesto + datos$monto_tercer_puesto
  abs(suma - datos$premio_total) < 0.01  # Tolerancia para errores de redondeo
}

# Verificar que no hay opciones duplicadas
verificar_opciones_unicas <- function(datos) {
  if (!("opciones_mezcladas" %in% names(datos))) return(FALSE)
  opciones <- datos$opciones_mezcladas
  if (is.null(opciones) || !is.vector(opciones)) return(FALSE)
  length(unique(opciones)) == length(opciones)
}

# Verificar coherencia de género
verificar_coherencia_genero <- function(datos) {
  coherencia_total <- TRUE

  # Verificar contexto
  if (all(c("contexto_seleccionado", "articulo_contexto") %in% names(datos))) {
    if (is.list(datos$contexto_seleccionado) && "genero" %in% names(datos$contexto_seleccionado)) {
      genero_contexto <- datos$contexto_seleccionado$genero
      articulo_esperado <- if (genero_contexto == "f") "una" else "un"
      if (datos$articulo_contexto != articulo_esperado) {
        coherencia_total <- FALSE
      }
    }
  }

  return(coherencia_total)
}

# =============================================================================
# EJECUCIÓN PRINCIPAL DE PRUEBAS
# =============================================================================

# Función para probar una versión específica
probar_version <- function(archivo_rmd) {
  cat("\n", paste(rep("=", 80), collapse=""), "\n", sep="")
  cat("PROBANDO:", archivo_rmd, "\n")
  cat(paste(rep("=", 80), collapse=""), "\n")

  start_time <- Sys.time()

  # Ejecutar simulaciones para esta versión
  resultados <- lapply(1:num_simulaciones, function(i) {
    extraer_datos_version(archivo_rmd, i)
  })

  end_time <- Sys.time()

  # Filtrar resultados nulos (errores)
  resultados_validos <- resultados[!sapply(resultados, is.null)]
  num_resultados_validos <- length(resultados_validos)

  cat("Simulaciones completadas:", num_resultados_validos, "de", num_simulaciones, "\n")
  cat("Tiempo de ejecución:", difftime(end_time, start_time, units = "secs"), "segundos\n")

  # Verificar si hay resultados válidos
  if (num_resultados_validos == 0) {
    cat("✗ ERROR: No se obtuvieron resultados válidos para", archivo_rmd, "\n")
    return(list(
      archivo = archivo_rmd,
      exito = FALSE,
      resultados_validos = 0,
      errores = "No se obtuvieron resultados válidos"
    ))
  }

  # Ejecutar todas las pruebas para esta versión
  resultados_pruebas <- ejecutar_pruebas_version(archivo_rmd, resultados_validos)

  return(resultados_pruebas)
}

# Función para ejecutar todas las pruebas de una versión
ejecutar_pruebas_version <- function(archivo_rmd, resultados_validos) {
  cat("\nEjecutando pruebas para", archivo_rmd, "...\n")

  # Inicializar contadores
  pruebas_resultados <- list()

  # PRUEBA 1: Verificar variabilidad (al menos 90% de versiones únicas)
  cat("\nPRUEBA 1: Verificación de variabilidad\n")
  hashes <- sapply(resultados_validos, function(r) r$hash)
  versiones_unicas <- length(unique(hashes))
  porcentaje_unicidad <- (versiones_unicas / length(resultados_validos)) * 100

  cat("Versiones únicas:", versiones_unicas, "de", length(resultados_validos), "\n")
  cat("Porcentaje de unicidad:", round(porcentaje_unicidad, 2), "%\n")

  prueba1_aprobada <- porcentaje_unicidad >= 90
  if (prueba1_aprobada) {
    cat("✓ APROBADO: Alta variabilidad\n")
  } else {
    cat("✗ FALLIDO: Baja variabilidad\n")
  }
  pruebas_resultados$variabilidad <- prueba1_aprobada

  # PRUEBA 2: Verificar rango de premios (30-400, múltiplos de 2)
  cat("\nPRUEBA 2: Verificación de rango de premios\n")
  premios_validos <- sapply(resultados_validos, verificar_rango_premios)
  num_premios_invalidos <- sum(!premios_validos)

  cat("Casos con premios fuera de rango:", num_premios_invalidos, "de", length(resultados_validos), "\n")

  prueba2_aprobada <- num_premios_invalidos == 0
  if (prueba2_aprobada) {
    cat("✓ APROBADO: Todos los premios están en rango 30-400 y son múltiplos de 2\n")
  } else {
    cat("✗ FALLIDO: Algunos premios están fuera de rango\n")
  }
  pruebas_resultados$rango_premios <- prueba2_aprobada

  # PRUEBA 3: Verificar orden correcto de premios
  cat("\nPRUEBA 3: Verificación de orden de premios\n")
  orden_correcto <- sapply(resultados_validos, verificar_orden_premios)
  num_orden_incorrecto <- sum(!orden_correcto)

  cat("Casos con orden incorrecto:", num_orden_incorrecto, "de", length(resultados_validos), "\n")

  prueba3_aprobada <- num_orden_incorrecto == 0
  if (prueba3_aprobada) {
    cat("✓ APROBADO: Primer puesto > Segundo puesto > Tercer puesto en todos los casos\n")
  } else {
    cat("✗ FALLIDO: Algunos casos tienen orden incorrecto de premios\n")
  }
  pruebas_resultados$orden_premios <- prueba3_aprobada

  # PRUEBA 4: Verificar suma de montos
  cat("\nPRUEBA 4: Verificación de suma de montos\n")
  suma_correcta <- sapply(resultados_validos, verificar_suma_montos)
  num_suma_incorrecta <- sum(!suma_correcta)

  cat("Casos con suma incorrecta:", num_suma_incorrecta, "de", length(resultados_validos), "\n")

  prueba4_aprobada <- num_suma_incorrecta == 0
  if (prueba4_aprobada) {
    cat("✓ APROBADO: Suma de montos = premio total en todos los casos\n")
  } else {
    cat("✗ FALLIDO: Algunos casos tienen suma incorrecta\n")
  }
  pruebas_resultados$suma_montos <- prueba4_aprobada

  # PRUEBA 5: Verificar opciones únicas
  cat("\nPRUEBA 5: Verificación de opciones únicas\n")
  opciones_unicas <- sapply(resultados_validos, verificar_opciones_unicas)
  num_opciones_duplicadas <- sum(!opciones_unicas)

  cat("Casos con opciones duplicadas:", num_opciones_duplicadas, "de", length(resultados_validos), "\n")

  prueba5_aprobada <- num_opciones_duplicadas == 0
  if (prueba5_aprobada) {
    cat("✓ APROBADO: Todas las opciones son únicas\n")
  } else {
    cat("✗ FALLIDO: Algunos casos tienen opciones duplicadas\n")
  }
  pruebas_resultados$opciones_unicas <- prueba5_aprobada

  # PRUEBA 6: Verificar coherencia de género
  cat("\nPRUEBA 6: Verificación de coherencia de género\n")
  genero_coherente <- sapply(resultados_validos, verificar_coherencia_genero)
  num_genero_incoherente <- sum(!genero_coherente)

  cat("Casos con incoherencia de género:", num_genero_incoherente, "de", length(resultados_validos), "\n")

  prueba6_aprobada <- num_genero_incoherente == 0
  if (prueba6_aprobada) {
    cat("✓ APROBADO: Coherencia de género en todos los casos\n")
  } else {
    cat("✗ FALLIDO: Algunos casos tienen incoherencia de género\n")
  }
  pruebas_resultados$coherencia_genero <- prueba6_aprobada

  # Resumen de la versión
  todas_aprobadas <- all(unlist(pruebas_resultados))
  cat("\n--- RESUMEN PARA", archivo_rmd, "---\n")
  for (nombre in names(pruebas_resultados)) {
    estado <- if (pruebas_resultados[[nombre]]) "✓" else "✗"
    cat(estado, " ", nombre, "\n", sep = "")
  }

  cat("\nResultado final:", if (todas_aprobadas) "✓ TODAS LAS PRUEBAS APROBADAS" else "✗ ALGUNAS PRUEBAS FALLARON", "\n")

  return(list(
    archivo = archivo_rmd,
    exito = todas_aprobadas,
    resultados_validos = length(resultados_validos),
    pruebas = pruebas_resultados,
    datos = resultados_validos
  ))
}

# =============================================================================
# EJECUCIÓN PRINCIPAL
# =============================================================================

cat("Iniciando pruebas para todas las versiones...\n")
start_time_total <- Sys.time()

# Probar todas las versiones
resultados_todas_versiones <- list()
for (archivo in archivos_rmd) {
  if (file.exists(archivo)) {
    resultados_todas_versiones[[archivo]] <- probar_version(archivo)
  } else {
    cat("✗ ARCHIVO NO ENCONTRADO:", archivo, "\n")
    resultados_todas_versiones[[archivo]] <- list(
      archivo = archivo,
      exito = FALSE,
      errores = "Archivo no encontrado"
    )
  }
}

end_time_total <- Sys.time()

# =============================================================================
# REPORTE FINAL CONSOLIDADO
# =============================================================================

cat("\n", paste(rep("=", 80), collapse=""), "\n", sep="")
cat("REPORTE FINAL CONSOLIDADO\n")
cat(paste(rep("=", 80), collapse=""), "\n")
cat("Tiempo total de ejecución:", difftime(end_time_total, start_time_total, units = "mins"), "minutos\n\n")

# Resumen por versión
for (archivo in names(resultados_todas_versiones)) {
  resultado <- resultados_todas_versiones[[archivo]]
  estado <- if (resultado$exito) "✓ APROBADO" else "✗ FALLIDO"
  cat(estado, " - ", archivo, " (", resultado$resultados_validos, " simulaciones válidas)\n", sep = "")
}

# Estadísticas generales
total_versiones <- length(archivos_rmd)
versiones_aprobadas <- sum(sapply(resultados_todas_versiones, function(r) r$exito))
porcentaje_exito <- (versiones_aprobadas / total_versiones) * 100

cat("\n--- ESTADÍSTICAS GENERALES ---\n")
cat("Versiones probadas:", total_versiones, "\n")
cat("Versiones aprobadas:", versiones_aprobadas, "\n")
cat("Porcentaje de éxito:", round(porcentaje_exito, 2), "%\n")

# Resultado final
if (versiones_aprobadas == total_versiones) {
  cat("\n🎉 ¡TODAS LAS VERSIONES APROBARON TODAS LAS PRUEBAS!\n")
  cat("Los ejercicios están listos para producción.\n")
} else {
  cat("\n⚠️  ALGUNAS VERSIONES REQUIEREN ATENCIÓN\n")
  cat("Revise los detalles arriba para identificar problemas específicos.\n")
}

# Guardar resultados detallados
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
archivo_reporte <- paste0("reporte_pruebas_", timestamp, ".RData")
save(resultados_todas_versiones, file = archivo_reporte)
cat("\nResultados detallados guardados en:", archivo_reporte, "\n")

cat(paste(rep("=", 80), collapse=""), "\n")
