#!/usr/bin/env Rscript
# =============================================================================
# validar_coherencia_matematica.R
# Script ejecutable de validación matemática post-renderizado
# Se ejecuta AUTOMÁTICAMENTE después de cada exams2* exitoso
# Soporta: SCHOICE y CLOZE
#
# Uso:
#   Rscript validar_coherencia_matematica.R archivo.Rmd [--fix] [--strict]
#
# Códigos de salida:
#   0 = Sin errores
#   1 = Errores detectados
#   2 = Error de ejecución del script
# =============================================================================

suppressPackageStartupMessages({
  library(exams)
})

# --- Argumentos ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat("Uso: Rscript validar_coherencia_matematica.R archivo.Rmd [--strict]\n")
  quit(status = 2)
}

archivo_rmd <- args[1]
modo_estricto <- "--strict" %in% args

if (!file.exists(archivo_rmd)) {
  cat("ERROR: Archivo no encontrado:", archivo_rmd, "\n")
  quit(status = 2)
}

# --- Funciones auxiliares ---

# Parsear contenido del .Rmd en secciones
parsear_rmd <- function(archivo) {
  contenido <- readLines(archivo, warn = FALSE, encoding = "UTF-8")

  # Localizar secciones clave
  linea_question <- grep("^Question$", contenido)
  linea_solution <- grep("^Solution$", contenido)
  linea_meta <- grep("^Meta-information$", contenido)
  lineas_answerlist <- grep("^Answerlist$", contenido)

  # Extraer metadatos exams (después de Meta-information)
  meta_inicio <- if (length(linea_meta) > 0) linea_meta[1] + 2 else NA
  meta_lineas <- if (!is.na(meta_inicio)) contenido[meta_inicio:length(contenido)] else character(0)

  # Extraer chunks R (formato ```{r ...} ... ```)
  en_chunk <- FALSE
  chunks_r <- list()
  chunk_actual <- character(0)
  for (linea in contenido) {
    if (grepl("^```\\{r", linea)) {
      en_chunk <- TRUE
      chunk_actual <- character(0)
      next
    }
    if (en_chunk && grepl("^```$", linea)) {
      en_chunk <- FALSE
      chunks_r[[length(chunks_r) + 1]] <- chunk_actual
      next
    }
    if (en_chunk) {
      chunk_actual <- c(chunk_actual, linea)
    }
  }

  # Extraer Question Answerlist (primera Answerlist después de Question)
  q_answerlist <- character(0)
  if (length(linea_question) > 0 && length(lineas_answerlist) > 0) {
    q_al_idx <- lineas_answerlist[lineas_answerlist > linea_question[1]]
    if (length(linea_solution) > 0) {
      q_al_idx <- q_al_idx[q_al_idx < linea_solution[1]]
    }
    if (length(q_al_idx) > 0) {
      al_start <- q_al_idx[1] + 2  # Saltar "Answerlist" y "----------"
      al_end <- al_start
      while (al_end <= length(contenido) && grepl("^\\*", contenido[al_end])) {
        al_end <- al_end + 1
      }
      q_answerlist <- contenido[al_start:(al_end - 1)]
    }
  }

  return(list(
    contenido = contenido,
    meta = meta_lineas,
    chunks_r = chunks_r,
    q_answerlist = q_answerlist,
    num_q_opciones = length(q_answerlist)
  ))
}

# Extraer valor de un campo de metadatos
extraer_meta <- function(meta_lineas, campo) {
  patron <- paste0("^", campo, ":\\s*")
  idx <- grep(patron, meta_lineas)
  if (length(idx) == 0) return(NA_character_)
  sub(patron, "", meta_lineas[idx[1]])
}

# Ejecutar chunks R en entorno aislado y capturar variables
ejecutar_chunks <- function(chunks_r) {
  env <- new.env(parent = globalenv())
  errores <- character(0)
  warnings_cap <- character(0)

  for (i in seq_along(chunks_r)) {
    codigo <- paste(chunks_r[[i]], collapse = "\n")
    tryCatch(
      withCallingHandlers(
        eval(parse(text = codigo), envir = env),
        warning = function(w) {
          # Capturar warnings pero no detener
          warnings_cap <<- c(warnings_cap,
            paste0("WARNING en chunk ", i, ": ", conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        errores <<- c(errores,
          paste0("ERR_EXEC chunk ", i, ": ", conditionMessage(e)))
      }
    )
  }

  return(list(env = env, errores = errores, warnings = warnings_cap))
}

# --- Validaciones ---

validar_metadatos <- function(meta_lineas) {
  errores <- character(0)

  extype <- tolower(trimws(extraer_meta(meta_lineas, "extype")))
  exsolution_raw <- trimws(extraer_meta(meta_lineas, "exsolution"))
  exshuffle <- trimws(extraer_meta(meta_lineas, "exshuffle"))

  # Campos obligatorios
  campos_req <- c("exname", "extype", "exsolution")
  for (campo in campos_req) {
    val <- extraer_meta(meta_lineas, campo)
    if (is.na(val) || val == "") {
      errores <- c(errores, paste0("ERR_C4: Metadato obligatorio '", campo, "' faltante"))
    }
  }

  # exshuffle debe ser TRUE
  if (!is.na(exshuffle) && toupper(exshuffle) != "TRUE") {
    errores <- c(errores, "ERR_C4: exshuffle debe ser TRUE (ICFES requiere mezcla)")
  }

  # ICFES 6 dimensiones
  icfes <- c("Type", "Competencia", "Componente", "Afirmacion", "Evidencia", "Nivel")
  for (campo in icfes) {
    patron <- paste0("^exextra\\[", campo, "\\]:")
    if (!any(grepl(patron, meta_lineas))) {
      errores <- c(errores, paste0("ERR_C4: Metadato ICFES 'exextra[", campo, "]' faltante"))
    }
  }

  return(errores)
}

validar_coherencia_schoice <- function(meta_lineas, env, num_opciones) {
  errores <- character(0)

  exsolution <- trimws(extraer_meta(meta_lineas, "exsolution"))
  if (is.na(exsolution)) return(c("ERR_C1: exsolution no encontrada"))

  # Para SCHOICE, exsolution puede ser inline R o string binario
  # Si contiene `r ...` fue ya evaluado por exams, así que en meta es literal
  # Verificar formato binario
  if (!grepl("^[01]+$", exsolution)) {
    # Podría ser evaluado dinámicamente - no podemos verificar estáticamente
    return(errores)
  }

  # Verificar longitud vs opciones
  n_sol <- nchar(exsolution)
  if (num_opciones > 0 && n_sol != num_opciones) {
    errores <- c(errores, paste0(
      "ERR_C4: exsolution tiene ", n_sol, " caracteres pero Answerlist tiene ",
      num_opciones, " opciones"))
  }

  # Verificar exactamente un "1"
  n_correctas <- sum(strsplit(exsolution, "")[[1]] == "1")
  if (n_correctas != 1) {
    errores <- c(errores, paste0(
      "ERR_C1: SCHOICE requiere exactamente 1 respuesta correcta, encontradas: ",
      n_correctas))
  }

  return(errores)
}

validar_coherencia_cloze <- function(meta_lineas, env) {
  errores <- character(0)

  exclozetype_raw <- trimws(extraer_meta(meta_lineas, "exclozetype"))
  exsolution_raw <- trimws(extraer_meta(meta_lineas, "exsolution"))
  extol_raw <- trimws(extraer_meta(meta_lineas, "extol"))

  # Si contiene `r ...` son dinámicos - verificar en env
  if (!is.na(exclozetype_raw) && !grepl("`r", exclozetype_raw)) {
    tipos <- strsplit(exclozetype_raw, "\\|")[[1]]
    n_tipos <- length(tipos)

    # Verificar que tolerancias coincidan en cantidad
    if (!is.na(extol_raw) && !grepl("`r", extol_raw)) {
      tolerancias <- strsplit(extol_raw, "\\|")[[1]]
      if (length(tolerancias) != n_tipos) {
        errores <- c(errores, paste0(
          "ERR_C4: extol tiene ", length(tolerancias),
          " valores pero exclozetype tiene ", n_tipos, " tipos"))
      }
    }

    # Verificar que exsolution coincida en cantidad
    if (!is.na(exsolution_raw) && !grepl("`r", exsolution_raw)) {
      soluciones <- strsplit(exsolution_raw, "\\|")[[1]]
      if (length(soluciones) != n_tipos) {
        errores <- c(errores, paste0(
          "ERR_C4: exsolution tiene ", length(soluciones),
          " valores pero exclozetype tiene ", n_tipos, " tipos"))
      }
    }

    # Verificar tipos válidos
    tipos_validos <- c("num", "string", "schoice", "mchoice")
    for (i in seq_along(tipos)) {
      if (!trimws(tipos[i]) %in% tipos_validos) {
        errores <- c(errores, paste0(
          "ERR_C4: Tipo CLOZE '", tipos[i], "' no es válido"))
      }
    }
  }

  # Verificar variables de solución en el entorno R
  if (!is.null(env)) {
    # Buscar vectores de solución típicos
    vars_solucion <- c("solucion_cloze", "solucion_schoice",
                        "respuesta_1", "respuesta_2", "respuesta_3",
                        "respuesta_4", "respuesta_5", "respuesta_6")

    for (var in vars_solucion) {
      if (exists(var, envir = env)) {
        val <- get(var, envir = env)
        if (is.numeric(val) && any(is.na(val))) {
          errores <- c(errores, paste0(
            "ERR_C1: Variable '", var, "' contiene NA"))
        }
        if (is.numeric(val) && any(is.infinite(val))) {
          errores <- c(errores, paste0(
            "ERR_C1: Variable '", var, "' contiene Inf"))
        }
      }
    }

    # Verificar que solucion_schoice tenga exactamente un TRUE
    if (exists("solucion_schoice", envir = env)) {
      sol_sc <- get("solucion_schoice", envir = env)
      if (is.logical(sol_sc)) {
        n_true <- sum(sol_sc)
        if (n_true != 1) {
          errores <- c(errores, paste0(
            "ERR_C1: solucion_schoice tiene ", n_true,
            " respuestas correctas, debe ser exactamente 1"))
        }
      }
    }

    # Verificar que opciones_mezcladas no tenga duplicados
    if (exists("opciones_mezcladas", envir = env)) {
      opts <- get("opciones_mezcladas", envir = env)
      if (length(unique(opts)) != length(opts)) {
        errores <- c(errores,
          "ERR_C1: opciones_mezcladas contiene valores duplicados")
      }
    }
  }

  return(errores)
}

validar_coherencia_matematica_general <- function(env) {
  errores <- character(0)
  if (is.null(env)) return(errores)

  # Verificar que variables numéricas no sean NA, NaN, Inf
  objetos <- ls(envir = env)
  for (nombre in objetos) {
    val <- tryCatch(get(nombre, envir = env), error = function(e) NULL)
    if (is.null(val)) next

    if (is.numeric(val) && length(val) == 1) {
      if (is.na(val)) {
        errores <- c(errores, paste0("ERR_C1: Variable '", nombre, "' = NA"))
      }
      if (is.infinite(val)) {
        errores <- c(errores, paste0("ERR_C1: Variable '", nombre, "' = Inf"))
      }
      if (is.nan(val)) {
        errores <- c(errores, paste0("ERR_C1: Variable '", nombre, "' = NaN"))
      }
    }
  }

  # Verificar coherencia entre distancia, rapidez, tiempo si existen
  if (all(c("distancia", "rapidez", "duracion") %in% objetos)) {
    d <- get("distancia", envir = env)
    r <- get("rapidez", envir = env)
    t <- get("duracion", envir = env)
    if (is.numeric(d) && is.numeric(r) && is.numeric(t)) {
      if (abs(d - r * t) > 0.01) {
        errores <- c(errores, paste0(
          "ERR_C1: distancia (", d, ") != rapidez (", r, ") * duracion (", t,
          ") = ", r * t))
      }
    }
  }

  # Verificar coherencia pos_final = pos_inicial + distancia
  if (all(c("pos_final", "pos_inicial", "distancia") %in% objetos)) {
    pf <- get("pos_final", envir = env)
    pi_val <- get("pos_inicial", envir = env)
    d <- get("distancia", envir = env)
    if (is.numeric(pf) && is.numeric(pi_val) && is.numeric(d)) {
      if (abs(pf - (pi_val + d)) > 0.01) {
        errores <- c(errores, paste0(
          "ERR_C1: pos_final (", pf, ") != pos_inicial (", pi_val,
          ") + distancia (", d, ") = ", pi_val + d))
      }
    }
  }

  # Verificar coherencia hora_final = hora_inicial + duracion
  if (all(c("hora_final", "hora_inicial", "duracion") %in% objetos)) {
    hf <- get("hora_final", envir = env)
    hi <- get("hora_inicial", envir = env)
    dur <- get("duracion", envir = env)
    if (is.numeric(hf) && is.numeric(hi) && is.numeric(dur)) {
      if (abs(hf - (hi + dur)) > 0.01) {
        errores <- c(errores, paste0(
          "ERR_C1: hora_final (", hf, ") != hora_inicial (", hi,
          ") + duracion (", dur, ") = ", hi + dur))
      }
    }
  }

  return(errores)
}

validar_codigo <- function(contenido) {
  errores <- character(0)

  # Detectar funciones matemáticas sobre variables formateadas
  patrones_peligrosos <- list(
    c("abs\\([^)]*formateado", "abs() sobre variable formateada"),
    c("abs\\([^)]*_str", "abs() sobre variable string"),
    c("sqrt\\([^)]*formateado", "sqrt() sobre variable formateada"),
    c("round\\([^)]*formateado", "round() sobre variable formateada")
  )

  for (patron_info in patrones_peligrosos) {
    lineas <- grep(patron_info[1], contenido, value = TRUE)
    if (length(lineas) > 0) {
      errores <- c(errores, paste0("ERR_C3: ", patron_info[2],
        " en: ", trimws(lineas[1])))
    }
  }

  return(errores)
}

# --- Ejecución principal ---

cat("=== VALIDACIÓN DE COHERENCIA MATEMÁTICA ===\n")
cat("Archivo:", archivo_rmd, "\n")
cat("Modo:", if (modo_estricto) "ESTRICTO" else "NORMAL", "\n\n")

# 1. Parsear
parsed <- parsear_rmd(archivo_rmd)
extype <- tolower(trimws(extraer_meta(parsed$meta, "extype")))
cat("Tipo detectado:", toupper(extype), "\n\n")

todos_errores <- character(0)
todos_warnings <- character(0)

# 2. Ejecutar chunks R
cat("--- Ejecutando chunks R ---\n")
resultado <- ejecutar_chunks(parsed$chunks_r)
if (length(resultado$errores) > 0) {
  cat("  ERRORES de ejecución:\n")
  for (e in resultado$errores) cat("    ", e, "\n")
  todos_errores <- c(todos_errores, resultado$errores)
} else {
  cat("  Chunks ejecutados: OK\n")
}
if (length(resultado$warnings) > 0) {
  todos_warnings <- resultado$warnings
}

# 3. Validar metadatos
cat("\n--- Validando metadatos ---\n")
err_meta <- validar_metadatos(parsed$meta)
if (length(err_meta) > 0) {
  for (e in err_meta) cat("  ", e, "\n")
  todos_errores <- c(todos_errores, err_meta)
} else {
  cat("  Metadatos: OK\n")
}

# 4. Validar coherencia según tipo
cat("\n--- Validando coherencia", toupper(extype), "---\n")
if (extype == "schoice") {
  err_tipo <- validar_coherencia_schoice(parsed$meta, resultado$env,
                                          parsed$num_q_opciones)
} else if (extype == "cloze") {
  err_tipo <- validar_coherencia_cloze(parsed$meta, resultado$env)
} else {
  err_tipo <- character(0)
  cat("  Tipo '", extype, "' - validación básica\n")
}

if (length(err_tipo) > 0) {
  for (e in err_tipo) cat("  ", e, "\n")
  todos_errores <- c(todos_errores, err_tipo)
} else {
  cat("  Coherencia", toupper(extype), ": OK\n")
}

# 5. Validar matemática general
cat("\n--- Validando coherencia matemática general ---\n")
err_math <- validar_coherencia_matematica_general(resultado$env)
if (length(err_math) > 0) {
  for (e in err_math) cat("  ", e, "\n")
  todos_errores <- c(todos_errores, err_math)
} else {
  cat("  Matemática general: OK\n")
}

# 6. Validar código
cat("\n--- Validando coherencia de código ---\n")
err_code <- validar_codigo(parsed$contenido)
if (length(err_code) > 0) {
  for (e in err_code) cat("  ", e, "\n")
  todos_errores <- c(todos_errores, err_code)
} else {
  cat("  Código: OK\n")
}

# 7. Resumen
cat("\n")
cat("============================================\n")
if (length(todos_errores) == 0) {
  cat("  RESULTADO: APROBADO (0 errores)\n")
  cat("============================================\n")
  if (length(todos_warnings) > 0) {
    cat("\n  Warnings (no bloqueantes):\n")
    for (w in todos_warnings) cat("    ", w, "\n")
  }
  quit(status = 0)
} else {
  cat("  RESULTADO: ERRORES DETECTADOS (", length(todos_errores), ")\n")
  cat("============================================\n")
  cat("\n  Errores encontrados:\n")
  for (e in todos_errores) cat("    ", e, "\n")
  if (length(todos_warnings) > 0) {
    cat("\n  Warnings:\n")
    for (w in todos_warnings) cat("    ", w, "\n")
  }
  quit(status = 1)
}
