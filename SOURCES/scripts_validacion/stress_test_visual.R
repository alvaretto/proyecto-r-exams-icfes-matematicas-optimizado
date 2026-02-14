#!/usr/bin/env Rscript
# =============================================================================
# stress_test_visual.R
# Stress Test Visual Multi-Semilla: renderiza N veces con exams2pdf(),
# convierte a PNG, extrae datos del entorno, analiza anomalías estadísticas
# y genera reporte JSON + PNGs para inspección visual.
#
# Uso:
#   Rscript stress_test_visual.R archivo.Rmd [--n 10] [--output-dir ./stress_test_output]
#
# Códigos de salida:
#   0 = Sin anomalías críticas
#   1 = Anomalías detectadas (ver reporte.json)
#   2 = Error de ejecución del script
# =============================================================================

suppressPackageStartupMessages({
  library(exams)
})

# Operador %||% si no existe
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# --- Detectar directorio del script actual ---
obtener_dir_script <- function() {
  # Método 1: commandArgs (funciona con Rscript)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg[1]))))
  }
  # Método 2: sys.frame (funciona con source())
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) {
      return(normalizePath(dirname(ofile)))
    }
  }
  # Método 3: getwd() como fallback
  getwd()
}

# --- Detectar raíz del proyecto (busca .git/ hacia arriba) ---
obtener_raiz_proyecto <- function(desde = getwd()) {
  dir <- normalizePath(desde, mustWork = FALSE)
  while (dir != dirname(dir)) {
    if (file.exists(file.path(dir, ".git"))) return(dir)
    dir <- dirname(dir)
  }
  NULL
}

# --- Cargar motor de validación existente ---
cargar_motor_validacion <- function() {
  script_dir <- obtener_dir_script()
  raiz <- obtener_raiz_proyecto(script_dir)

  posibles_rutas <- c(
    # Mismo directorio que este script (SOURCES/scripts_validacion/)
    file.path(script_dir, "validar_coherencia_matematica.R"),
    # Desde raíz del proyecto
    if (!is.null(raiz)) file.path(raiz, "SOURCES", "scripts_validacion", "validar_coherencia_matematica.R"),
    if (!is.null(raiz)) file.path(raiz, ".claude", "scripts", "validar_coherencia_matematica.R"),
    # Rutas relativas al CWD (fallback)
    "SOURCES/scripts_validacion/validar_coherencia_matematica.R",
    ".claude/scripts/validar_coherencia_matematica.R"
  )

  for (ruta in posibles_rutas) {
    if (!is.null(ruta) && file.exists(ruta)) {
      source(ruta)
      return(TRUE)
    }
  }
  FALSE
}

cargar_motor_validacion()

# =============================================================================
# FASE A: Renderizado masivo + extracción de datos
# =============================================================================

#' Extraer datos presentables del entorno de un ejercicio ejecutado
#' @param env Entorno R después de ejecutar chunks
#' @return Lista con opciones, sol, valor_correcto, enunciado, posicion_correcta
extraer_datos_semilla <- function(env) {
  # Extraer opciones (múltiples convenciones de nombres)
  opciones <- NULL
  opciones_var <- NULL
  for (var in c("opciones_mezcladas", "opciones", "opciones_valores",
                "opciones_num", "opciones_texto", "opciones_graficos")) {
    if (exists(var, envir = env)) {
      opciones <- get(var, envir = env)
      opciones_var <- var
      break
    }
  }

  # Extraer vector de solución
  sol <- NULL
  for (var in c("sol", "solucion", "solucion_vector")) {
    if (exists(var, envir = env)) {
      sol <- get(var, envir = env)
      break
    }
  }

  # Extraer valor correcto
  valor_correcto <- NULL
  for (var in c("valor_correcto", "mediana_calc", "respuesta_correcta",
                "media_correcta", "mediana_correcta", "moda_correcta")) {
    if (exists(var, envir = env)) {
      valor_correcto <- get(var, envir = env)
      break
    }
  }

  # Extraer enunciado / contexto narrativo

  enunciado <- NULL
  for (var in c("enunciado_contexto", "texto_pregunta", "enunciado",
                "contexto_narrativo", "pregunta_texto")) {
    if (exists(var, envir = env)) {
      enunciado <- get(var, envir = env)
      break
    }
  }

  # Extraer datos (para verificar rangos)
  datos <- NULL
  for (var in c("datos_ord", "datos", "datos_presentados")) {
    if (exists(var, envir = env)) {
      datos <- get(var, envir = env)
      break
    }
  }

  # Calcular posición correcta
  posicion_correcta <- if (!is.null(sol)) which(sol == 1) else integer(0)

  list(
    opciones = opciones,
    opciones_var = opciones_var,
    sol = sol,
    valor_correcto = valor_correcto,
    enunciado = enunciado,
    datos = datos,
    posicion_correcta = posicion_correcta
  )
}

#' Serializar opciones a formato comparable (texto plano)
#' @param opciones Lista u objeto de opciones
#' @return Vector de strings representando cada opción
serializar_opciones <- function(opciones) {
  if (is.null(opciones)) return(NULL)

  if (is.list(opciones)) {
    sapply(seq_along(opciones), function(i) {
      opt <- opciones[[i]]
      if (is.numeric(opt) && length(opt) == 1) {
        as.character(round(opt, 6))
      } else if (is.character(opt) && length(opt) == 1) {
        opt
      } else {
        digest::digest(opt)
      }
    })
  } else if (is.numeric(opciones)) {
    as.character(round(opciones, 6))
  } else if (is.character(opciones)) {
    opciones
  } else {
    sapply(seq_along(opciones), function(i) digest::digest(opciones[[i]]))
  }
}

#' Renderizar un .Rmd N veces con diferentes semillas y capturar datos
#' @param archivo_rmd Ruta al archivo .Rmd
#' @param n Número de semillas
#' @param output_dir Directorio de salida
#' @return Lista de resultados por semilla
renderizar_multisemilla <- function(archivo_rmd, n = 10, output_dir = "stress_test_output") {
  # Crear estructura de directorios
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  pngs_dir <- file.path(output_dir, "pngs")
  dir.create(pngs_dir, showWarnings = FALSE, recursive = TRUE)

  # Parsear el .Rmd una sola vez
  parsed <- parsear_rmd(archivo_rmd)

  # Semillas dispersas (mismo patrón que validar_multisemilla.R)
  semillas <- (1:n) * 7919

  # Directorio del .Rmd (exams2pdf necesita path relativo desde ahí)
  rmd_dir <- dirname(normalizePath(archivo_rmd))
  rmd_basename <- basename(archivo_rmd)

  resultados <- list()

  for (i in seq_along(semillas)) {
    semilla <- semillas[i]
    semilla_str <- sprintf("%05d", semilla)

    cat(sprintf("  Semilla %d/%d (seed=%d)...", i, n, semilla))

    resultado <- list(
      semilla = semilla,
      indice = i,
      estado = "pendiente",
      datos = NULL,
      png_paths = character(0),
      errores = character(0)
    )

    # --- Paso 1: Ejecutar chunks y extraer datos ---
    datos_ok <- tryCatch({
      set.seed(semilla)
      exec_result <- ejecutar_chunks(parsed$chunks_r)
      if (length(exec_result$errores) > 0) {
        resultado$errores <- exec_result$errores
        resultado$estado <- "fallida"
        FALSE
      } else {
        resultado$datos <- extraer_datos_semilla(exec_result$env)
        TRUE
      }
    }, error = function(e) {
      resultado$errores <<- c(resultado$errores, paste0("Error chunks: ", conditionMessage(e)))
      resultado$estado <<- "fallida"
      FALSE
    })

    if (!datos_ok) {
      cat(" FALLO (chunks)\n")
      resultados[[i]] <- resultado
      next
    }

    # --- Paso 2: Renderizar PDF ---
    pdf_ok <- tryCatch({
      temp_pdf_dir <- tempfile(pattern = "stress_pdf_")
      dir.create(temp_pdf_dir, showWarnings = FALSE)

      set.seed(semilla)
      suppressMessages(
        exams2pdf(
          rmd_basename,
          n = 1,
          dir = temp_pdf_dir,
          edir = rmd_dir,
          verbose = FALSE
        )
      )

      # Buscar PDF generado
      pdfs <- list.files(temp_pdf_dir, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
      if (length(pdfs) == 0) {
        resultado$errores <- c(resultado$errores, "No se generó PDF")
        resultado$estado <- "fallida"
        FALSE
      } else {
        resultado$pdf_path <- pdfs[1]
        TRUE
      }
    }, error = function(e) {
      resultado$errores <<- c(resultado$errores, paste0("Error PDF: ", conditionMessage(e)))
      resultado$estado <<- "fallida"
      FALSE
    })

    if (!pdf_ok) {
      cat(" FALLO (PDF)\n")
      resultados[[i]] <- resultado
      next
    }

    # --- Paso 3: Convertir PDF → PNG ---
    png_ok <- tryCatch({
      png_base <- file.path(pngs_dir, paste0("seed_", semilla_str))
      system2("magick",
        args = c("-density", "150", resultado$pdf_path, "-quality", "90",
                 paste0(png_base, ".png")),
        stdout = FALSE, stderr = FALSE
      )

      # magick genera seed_XXXXX-0.png, -1.png, etc. para múltiples páginas
      # o seed_XXXXX.png si es una sola página
      pngs_generados <- list.files(pngs_dir,
        pattern = paste0("seed_", semilla_str, ".*\\.png$"),
        full.names = TRUE)

      if (length(pngs_generados) == 0) {
        resultado$errores <- c(resultado$errores, "magick no generó PNGs")
        resultado$estado <- "fallida"
        FALSE
      } else {
        resultado$png_paths <- pngs_generados
        TRUE
      }
    }, error = function(e) {
      resultado$errores <<- c(resultado$errores, paste0("Error PNG: ", conditionMessage(e)))
      resultado$estado <<- "fallida"
      FALSE
    })

    if (!png_ok) {
      cat(" FALLO (PNG)\n")
      resultados[[i]] <- resultado
      next
    }

    # Limpiar temp
    unlink(dirname(resultado$pdf_path), recursive = TRUE)
    resultado$pdf_path <- NULL

    resultado$estado <- "exitosa"
    cat(" OK\n")
    resultados[[i]] <- resultado
  }

  resultados
}

# =============================================================================
# FASE B: Análisis estadístico de anomalías
# =============================================================================

#' Analizar datos extraídos de múltiples semillas buscando anomalías
#' @param resultados Lista de resultados por semilla
#' @param archivo_rmd Nombre del archivo (para contexto)
#' @return Lista de anomalías detectadas
analizar_datos <- function(resultados, archivo_rmd = "") {
  anomalias <- list()
  es_negativo <- grepl("_neg_", basename(archivo_rmd))

  # Filtrar solo semillas exitosas
  exitosas <- Filter(function(r) r$estado == "exitosa", resultados)
  n_exitosas <- length(exitosas)

  if (n_exitosas < 2) {
    anomalias[[length(anomalias) + 1]] <- list(
      tipo = "ANOM_POCAS_EXITOSAS",
      severidad = "critica",
      detalle = sprintf("Solo %d semilla(s) exitosa(s) de %d", n_exitosas, length(resultados))
    )
    return(anomalias)
  }

  # --- 1. Distribución de posición de respuesta correcta ---
  posiciones <- sapply(exitosas, function(r) {
    if (!is.null(r$datos$posicion_correcta) && length(r$datos$posicion_correcta) == 1) {
      r$datos$posicion_correcta
    } else {
      NA
    }
  })
  posiciones <- posiciones[!is.na(posiciones)]

  if (length(posiciones) >= 3) {
    tabla_pos <- table(factor(posiciones, levels = 1:4))
    max_freq <- max(tabla_pos)
    # Si más del 80% de las veces la correcta está en la misma posición → anomalía
    if (max_freq / length(posiciones) > 0.8) {
      letras <- c("A", "B", "C", "D")
      pos_dominante <- letras[which.max(tabla_pos)]
      dist_named <- setNames(as.integer(tabla_pos), letras)
      anomalias[[length(anomalias) + 1]] <- list(
        tipo = "ANOM_POS_FIJA",
        severidad = "alta",
        detalle = sprintf(
          "Respuesta correcta en posición %s en %d/%d semillas (%.0f%%)",
          pos_dominante, max_freq, length(posiciones),
          100 * max_freq / length(posiciones)
        ),
        distribucion = as.list(dist_named)
      )
    }
  }

  # --- 2. Opciones duplicadas por semilla ---
  for (r in exitosas) {
    if (is.null(r$datos$opciones)) next
    opts_serial <- serializar_opciones(r$datos$opciones)
    if (is.null(opts_serial)) next

    hashes <- sapply(opts_serial, digest::digest)

    if (es_negativo) {
      # Patrón _neg_: (N-1) iguales + 1 diferente
      freq <- table(hashes)
      n_opt <- length(hashes)
      if (length(freq) != 2 || !((n_opt - 1) %in% as.integer(freq))) {
        anomalias[[length(anomalias) + 1]] <- list(
          tipo = "ANOM_NEG_PATRON",
          severidad = "critica",
          semilla = r$semilla,
          detalle = sprintf(
            "Patrón _neg_ inválido: se esperaban %d iguales + 1 diferente, hay %d grupos",
            n_opt - 1, length(freq)
          )
        )
      }
    } else {
      # Patrón normal: todas diferentes
      if (length(unique(hashes)) != length(hashes)) {
        dups <- names(which(table(hashes) > 1))
        dup_indices <- which(hashes %in% dups)
        anomalias[[length(anomalias) + 1]] <- list(
          tipo = "ANOM_DUP_OPT",
          severidad = "critica",
          semilla = r$semilla,
          detalle = sprintf(
            "Opciones duplicadas en posiciones %s (semilla %d)",
            paste(dup_indices, collapse = ","), r$semilla
          ),
          evidencia_png = if (length(r$png_paths) > 0) basename(r$png_paths[1]) else NULL
        )
      }
    }
  }

  # --- 3. Distractor idéntico a respuesta correcta ---
  for (r in exitosas) {
    if (is.null(r$datos$opciones) || is.null(r$datos$sol)) next
    opts_serial <- serializar_opciones(r$datos$opciones)
    if (is.null(opts_serial)) next

    idx_correcto <- which(r$datos$sol == 1)
    if (length(idx_correcto) != 1) next

    hash_correcto <- digest::digest(opts_serial[idx_correcto])
    for (j in seq_along(opts_serial)) {
      if (j == idx_correcto) next
      if (digest::digest(opts_serial[j]) == hash_correcto) {
        anomalias[[length(anomalias) + 1]] <- list(
          tipo = "ANOM_DIST_EQ_CORR",
          severidad = "critica",
          semilla = r$semilla,
          detalle = sprintf(
            "Distractor en posición %d es idéntico a respuesta correcta (pos %d), semilla %d",
            j, idx_correcto, r$semilla
          ),
          evidencia_png = if (length(r$png_paths) > 0) basename(r$png_paths[1]) else NULL
        )
      }
    }
  }

  # --- 4. Variabilidad de valores numéricos ---
  valores_correctos <- sapply(exitosas, function(r) {
    vc <- r$datos$valor_correcto
    if (is.numeric(vc) && length(vc) == 1) vc else NA
  })
  valores_correctos <- valores_correctos[!is.na(valores_correctos)]

  if (length(valores_correctos) >= 3) {
    desv <- sd(valores_correctos)
    rango <- diff(range(valores_correctos))
    media <- mean(valores_correctos)

    # Si el coeficiente de variación es muy bajo → poca variabilidad
    if (media != 0 && abs(desv / media) < 0.01) {
      anomalias[[length(anomalias) + 1]] <- list(
        tipo = "ANOM_BAJA_VAR",
        severidad = "media",
        detalle = sprintf(
          "Valor correcto casi invariante: media=%.4f, sd=%.6f, rango=%.4f",
          media, desv, rango
        )
      )
    }
  }

  # --- 5. Rangos absurdos ---
  for (r in exitosas) {
    if (is.null(r$datos$opciones)) next
    opts_serial <- serializar_opciones(r$datos$opciones)
    if (is.null(opts_serial)) next

    # Intentar convertir a numérico para verificar rangos
    opts_num <- suppressWarnings(as.numeric(opts_serial))
    opts_num <- opts_num[!is.na(opts_num)]

    if (length(opts_num) > 0) {
      # NA/NaN/Inf
      if (any(is.na(opts_num)) || any(is.nan(opts_num)) || any(is.infinite(opts_num))) {
        anomalias[[length(anomalias) + 1]] <- list(
          tipo = "ANOM_NA_INF",
          severidad = "critica",
          semilla = r$semilla,
          detalle = sprintf("Opciones contienen NA/NaN/Inf en semilla %d", r$semilla),
          evidencia_png = if (length(r$png_paths) > 0) basename(r$png_paths[1]) else NULL
        )
      }

      # Si hay datos del contexto, verificar que opciones están en rango razonable
      if (!is.null(r$datos$datos) && is.numeric(r$datos$datos)) {
        datos_rango <- range(r$datos$datos, na.rm = TRUE)
        margen <- diff(datos_rango) * 2  # 2x el rango de datos como margen
        for (val in opts_num) {
          if (val < (datos_rango[1] - margen) || val > (datos_rango[2] + margen)) {
            anomalias[[length(anomalias) + 1]] <- list(
              tipo = "ANOM_RANGO",
              severidad = "alta",
              semilla = r$semilla,
              detalle = sprintf(
                "Opción valor=%.2f muy fuera de rango datos [%.2f, %.2f] (semilla %d)",
                val, datos_rango[1], datos_rango[2], r$semilla
              ),
              evidencia_png = if (length(r$png_paths) > 0) basename(r$png_paths[1]) else NULL
            )
            break  # Solo reportar una vez por semilla
          }
        }
      }
    }
  }

  # --- 6. Enunciado invariante ---
  enunciados <- sapply(exitosas, function(r) {
    if (!is.null(r$datos$enunciado) && is.character(r$datos$enunciado)) {
      r$datos$enunciado[1]
    } else {
      NA_character_
    }
  })
  enunciados <- enunciados[!is.na(enunciados)]

  if (length(enunciados) >= 3) {
    n_unicos <- length(unique(enunciados))
    if (n_unicos == 1 && length(enunciados) > 1) {
      anomalias[[length(anomalias) + 1]] <- list(
        tipo = "ANOM_CTX_REPET",
        severidad = "media",
        detalle = sprintf(
          "Enunciado idéntico en las %d semillas (0 variabilidad narrativa)",
          length(enunciados)
        )
      )
    } else if (n_unicos < length(enunciados) * 0.5) {
      anomalias[[length(anomalias) + 1]] <- list(
        tipo = "ANOM_CTX_REPET",
        severidad = "baja",
        detalle = sprintf(
          "Solo %d enunciados únicos de %d semillas (%.0f%% variabilidad)",
          n_unicos, length(enunciados), 100 * n_unicos / length(enunciados)
        )
      )
    }
  }

  # --- 7. Semillas fallidas ---
  fallidas <- Filter(function(r) r$estado == "fallida", resultados)
  if (length(fallidas) > 0) {
    for (f in fallidas) {
      anomalias[[length(anomalias) + 1]] <- list(
        tipo = "ANOM_COMPILE",
        severidad = "critica",
        semilla = f$semilla,
        detalle = sprintf("Fallo de compilación en semilla %d: %s",
          f$semilla, paste(f$errores, collapse = "; "))
      )
    }
  }

  anomalias
}

# =============================================================================
# FASE C: Clasificación y reporte
# =============================================================================

#' Clasificar semillas según anomalías
#' @param resultados Lista de resultados
#' @param anomalias Lista de anomalías
#' @return Lista con normales, sospechosas, fallidas
clasificar_semillas <- function(resultados, anomalias) {
  # Semillas con anomalías asociadas
  semillas_con_anomalia <- unique(unlist(lapply(anomalias, function(a) a$semilla)))

  normales <- c()
  sospechosas <- c()
  fallidas <- c()

  for (r in resultados) {
    if (r$estado == "fallida") {
      fallidas <- c(fallidas, r$semilla)
    } else if (r$semilla %in% semillas_con_anomalia) {
      sospechosas <- c(sospechosas, r$semilla)
    } else {
      normales <- c(normales, r$semilla)
    }
  }

  list(normales = normales, sospechosas = sospechosas, fallidas = fallidas)
}

#' Generar reporte completo (JSON + resumen texto)
#' @param archivo_rmd Ruta original
#' @param resultados Lista de resultados
#' @param anomalias Lista de anomalías
#' @param clasificacion Clasificación de semillas
#' @param output_dir Directorio de salida
generar_reporte <- function(archivo_rmd, resultados, anomalias, clasificacion, output_dir) {
  # --- Estadísticas generales ---
  exitosas <- Filter(function(r) r$estado == "exitosa", resultados)

  posiciones <- sapply(exitosas, function(r) {
    if (length(r$datos$posicion_correcta) == 1) r$datos$posicion_correcta else NA
  })
  posiciones <- posiciones[!is.na(posiciones)]
  dist_pos <- if (length(posiciones) > 0) {
    tabla <- table(factor(posiciones, levels = 1:4))
    setNames(as.integer(tabla), c("A", "B", "C", "D"))
  } else {
    c(A = 0L, B = 0L, C = 0L, D = 0L)
  }

  valores_correctos <- sapply(exitosas, function(r) {
    vc <- r$datos$valor_correcto
    if (is.numeric(vc) && length(vc) == 1) vc else NA
  })
  valores_correctos <- valores_correctos[!is.na(valores_correctos)]

  enunciados <- sapply(exitosas, function(r) {
    if (!is.null(r$datos$enunciado) && is.character(r$datos$enunciado)) {
      r$datos$enunciado[1]
    } else {
      NA_character_
    }
  })
  enunciados <- enunciados[!is.na(enunciados)]

  # Determinar si hay anomalías críticas
  tiene_criticas <- any(sapply(anomalias, function(a) a$severidad == "critica"))

  # --- Reporte JSON ---
  reporte <- list(
    archivo = basename(archivo_rmd),
    ruta_completa = normalizePath(archivo_rmd, mustWork = FALSE),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    semillas_total = length(resultados),
    semillas_exitosas = length(clasificacion$normales) + length(clasificacion$sospechosas),
    semillas_fallidas = length(clasificacion$fallidas),
    semillas_sospechosas = length(clasificacion$sospechosas),
    veredicto = if (tiene_criticas) "FALLA" else if (length(anomalias) > 0) "ADVERTENCIA" else "PASA",
    anomalias = anomalias,
    estadisticas = list(
      posicion_correcta = as.list(dist_pos),
      rango_valores = if (length(valores_correctos) > 0) {
        list(min = min(valores_correctos), max = max(valores_correctos))
      } else {
        list(min = NA, max = NA)
      },
      variabilidad_valor_correcto = if (length(valores_correctos) > 1) sd(valores_correctos) else NA,
      contextos_unicos = length(unique(enunciados))
    ),
    clasificacion = list(
      normales = clasificacion$normales,
      sospechosas = clasificacion$sospechosas,
      fallidas = clasificacion$fallidas
    ),
    pngs_sospechosas = unlist(lapply(
      Filter(function(r) r$semilla %in% clasificacion$sospechosas, resultados),
      function(r) basename(r$png_paths)
    )),
    pngs_muestra_normales = {
      normales_r <- Filter(function(r) r$semilla %in% clasificacion$normales, resultados)
      muestra <- head(normales_r, 3)
      unlist(lapply(muestra, function(r) basename(r$png_paths)))
    }
  )

  # Escribir JSON
  jsonlite_available <- requireNamespace("jsonlite", quietly = TRUE)
  reporte_path <- file.path(output_dir, "reporte.json")
  if (jsonlite_available) {
    writeLines(jsonlite::toJSON(reporte, auto_unbox = TRUE, pretty = TRUE), reporte_path)
  } else {
    # Fallback sin jsonlite
    writeLines(paste0("# Reporte generado sin jsonlite\n# Veredicto: ", reporte$veredicto),
               reporte_path)
  }

  # --- Resumen texto ---
  resumen_path <- file.path(output_dir, "resumen.txt")
  lineas <- c(
    "═══════════════════════════════════════════════════════════════",
    "  STRESS TEST VISUAL — REPORTE",
    "═══════════════════════════════════════════════════════════════",
    sprintf("  Archivo:              %s", basename(archivo_rmd)),
    sprintf("  Fecha:                %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("  Semillas probadas:    %d", length(resultados)),
    sprintf("  Exitosas:             %d", reporte$semillas_exitosas),
    sprintf("  Fallidas:             %d", reporte$semillas_fallidas),
    sprintf("  Sospechosas:          %d", reporte$semillas_sospechosas),
    "───────────────────────────────────────────────────────────────",
    sprintf("  VEREDICTO:            %s", reporte$veredicto),
    "───────────────────────────────────────────────────────────────"
  )

  if (length(anomalias) > 0) {
    lineas <- c(lineas, "", "  ANOMALÍAS DETECTADAS:", "")
    for (i in seq_along(anomalias)) {
      a <- anomalias[[i]]
      lineas <- c(lineas, sprintf(
        "  [%s] %s — %s",
        toupper(a$severidad), a$tipo, a$detalle
      ))
      if (!is.null(a$evidencia_png)) {
        lineas <- c(lineas, sprintf("         PNG: pngs/%s", a$evidencia_png))
      }
    }
  } else {
    lineas <- c(lineas, "", "  Sin anomalías detectadas.")
  }

  lineas <- c(lineas, "",
    "───────────────────────────────────────────────────────────────",
    "  ESTADÍSTICAS:",
    sprintf("  Posición correcta: A=%d B=%d C=%d D=%d",
            dist_pos["A"], dist_pos["B"], dist_pos["C"], dist_pos["D"]),
    if (length(valores_correctos) > 0) {
      sprintf("  Rango valor correcto: [%.4f, %.4f]",
              min(valores_correctos), max(valores_correctos))
    } else {
      "  Rango valor correcto: N/A"
    },
    sprintf("  Contextos únicos: %d", length(unique(enunciados))),
    "═══════════════════════════════════════════════════════════════"
  )

  writeLines(lineas, resumen_path)

  # --- Semillas sospechosas (para el hook) ---
  sosp_path <- file.path(output_dir, "semillas_sospechosas.txt")
  if (length(clasificacion$sospechosas) > 0) {
    writeLines(as.character(clasificacion$sospechosas), sosp_path)
  } else {
    writeLines("# Sin semillas sospechosas", sosp_path)
  }

  # --- CSV de datos ---
  csv_path <- file.path(output_dir, "datos_por_semilla.csv")
  filas <- lapply(resultados, function(r) {
    opts_str <- if (!is.null(r$datos$opciones)) {
      paste(serializar_opciones(r$datos$opciones), collapse = " | ")
    } else {
      "N/A"
    }
    data.frame(
      semilla = r$semilla,
      estado = r$estado,
      posicion_correcta = if (length(r$datos$posicion_correcta) == 1) r$datos$posicion_correcta else NA,
      valor_correcto = if (is.numeric(r$datos$valor_correcto) && length(r$datos$valor_correcto) == 1) {
        r$datos$valor_correcto
      } else {
        NA
      },
      opciones = opts_str,
      enunciado = if (!is.null(r$datos$enunciado)) substr(r$datos$enunciado[1], 1, 100) else "N/A",
      n_errores = length(r$errores),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, filas)
  write.csv(df, csv_path, row.names = FALSE)

  reporte
}

# =============================================================================
# Función principal
# =============================================================================

#' Ejecutar stress test visual completo
#' @param archivo_rmd Ruta al archivo .Rmd
#' @param n Número de semillas (default 10)
#' @param output_dir Directorio de salida (default: stress_test_output en directorio del .Rmd)
#' @return Lista con reporte completo
stress_test_visual <- function(archivo_rmd, n = 10, output_dir = NULL) {
  if (!file.exists(archivo_rmd)) {
    stop("Archivo no encontrado: ", archivo_rmd)
  }

  if (is.null(output_dir)) {
    output_dir <- file.path(dirname(normalizePath(archivo_rmd)), "stress_test_output")
  }

  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║  STRESS TEST VISUAL MULTI-SEMILLA                            ║\n")
  cat("╠═══════════════════════════════════════════════════════════════╣\n")
  cat(sprintf("║  Archivo: %-49s║\n", basename(archivo_rmd)))
  cat(sprintf("║  Semillas: %-48d║\n", n))
  cat(sprintf("║  Output: %-50s║\n", basename(output_dir)))
  cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

  # FASE A: Renderizado masivo
  cat("─── FASE A: Renderizado masivo ───\n")
  resultados <- renderizar_multisemilla(archivo_rmd, n, output_dir)
  n_exitosas <- sum(sapply(resultados, function(r) r$estado == "exitosa"))
  cat(sprintf("\n  Exitosas: %d/%d\n\n", n_exitosas, n))

  # FASE B: Análisis de anomalías
  cat("─── FASE B: Análisis de anomalías ───\n")
  anomalias <- analizar_datos(resultados, archivo_rmd)
  cat(sprintf("  Anomalías encontradas: %d\n\n", length(anomalias)))

  # FASE C: Clasificación y reporte
  cat("─── FASE C: Clasificación y reporte ───\n")
  clasificacion <- clasificar_semillas(resultados, anomalias)
  reporte <- generar_reporte(archivo_rmd, resultados, anomalias, clasificacion, output_dir)

  # Mostrar resumen
  cat(readLines(file.path(output_dir, "resumen.txt")), sep = "\n")
  cat("\n")

  # Instrucciones para Claude
  if (length(anomalias) > 0) {
    cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
    cat("║  INSTRUCCIÓN OBLIGATORIA PARA CLAUDE:                        ║\n")
    cat("║  Claude DEBE ejecutar Read() sobre los PNGs sospechosos     ║\n")
    cat("║  listados en semillas_sospechosas.txt y verificar            ║\n")
    cat("║  visualmente las anomalías reportadas.                       ║\n")
    cat("╚═══════════════════════════════════════════════════════════════╝\n")

    pngs_dir <- file.path(output_dir, "pngs")
    pngs_sosp <- list.files(pngs_dir, pattern = "\\.png$", full.names = TRUE)
    if (length(pngs_sosp) > 0) {
      cat("\nPNGs para inspección visual:\n")
      for (p in pngs_sosp) {
        cat(sprintf("  → %s\n", p))
      }
    }
  }

  reporte
}

# =============================================================================
# CLI
# =============================================================================

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 1) {
    cat("Uso: Rscript stress_test_visual.R archivo.Rmd [--n 10] [--output-dir ./stress_test_output]\n")
    quit(status = 2)
  }

  archivo_rmd <- args[1]
  n_semillas <- 10
  output_dir <- NULL

  # Parsear argumentos opcionales
  if ("--n" %in% args) {
    idx <- which(args == "--n")
    if (idx < length(args)) n_semillas <- as.integer(args[idx + 1])
  }
  if ("--output-dir" %in% args) {
    idx <- which(args == "--output-dir")
    if (idx < length(args)) output_dir <- args[idx + 1]
  }

  reporte <- tryCatch(
    stress_test_visual(archivo_rmd, n_semillas, output_dir),
    error = function(e) {
      cat(sprintf("\n❌ Error fatal: %s\n", conditionMessage(e)))
      quit(status = 2)
    }
  )

  # Código de salida según veredicto
  if (reporte$veredicto == "FALLA") {
    quit(status = 1)
  } else {
    quit(status = 0)
  }
}
