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

  # exshuffle debe ser TRUE (regla general)
  # Excepción: SCHOICE con opciones gráficas PNG + Solution que referencia letra_correcta
  # En ese caso, exshuffle: FALSE es correcto porque sample() interno ya aleatoriza
  # y TRUE rompería la referencia en Solution. Ver .claude/rules/graficos-como-opciones.md
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

# ============================================================
# VALIDACIÓN DE COHERENCIA SEMÁNTICA (Nivel 4)
#
# Nivel 1: Sintáctico  — código corre sin errores
# Nivel 2: Numérico    — valores válidos, no NA/NaN/Inf
# Nivel 3: Estructural — metadatos completos, formatos correctos
# Nivel 4: Semántico   — descripción del error corresponde a datos
#
# Este nivel opera en 3 capas:
#   Capa A: Precondición declarada — ¿el campo precondicion() se cumple?
#   Capa B: Escaneo de keywords   — ¿la descripción implica condiciones
#           no declaradas en precondicion?
#   Capa C: Cross-validación       — ¿calcula() produce valor ≠ correcto?
# ============================================================

# Reglas semánticas extensibles: patrón en descripción → condición testeable
# Para agregar reglas: añadir elemento a esta lista con patron/condicion/mensaje
REGLAS_SEMANTICAS_KEYWORDS <- list(
  list(
    patron = "numero par|n.mero par|par de datos|dos valores centrales|dos datos centrales",
    nombre = "n_par",
    condicion = function(params) {
      if (is.null(params$n)) return(NULL)
      params$n %% 2 == 0
    },
    mensaje = "n debe ser par"
  ),
  list(
    patron = "numero impar|n.mero impar|impar de datos|un solo valor central|.nico valor central|valor central .nico",
    nombre = "n_impar",
    condicion = function(params) {
      if (is.null(params$n)) return(NULL)
      params$n %% 2 == 1
    },
    mensaje = "n debe ser impar"
  ),
  list(
    patron = "moda .nica|\\bunimodal\\b",
    nombre = "moda_unica",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      tb <- table(params$datos_ord)
      sum(tb == max(tb)) == 1
    },
    mensaje = "los datos deben tener moda unica"
  ),
  list(
    patron = "\\bbimodal\\b|dos modas",
    nombre = "bimodal",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      tb <- table(params$datos_ord)
      sum(tb == max(tb)) == 2
    },
    mensaje = "los datos deben ser bimodales"
  ),
  list(
    patron = "\\bmultimodal\\b|varias modas|m.ltiples modas",
    nombre = "multimodal",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      tb <- table(params$datos_ord)
      sum(tb == max(tb)) >= 2
    },
    mensaje = "los datos deben ser multimodales"
  ),
  list(
    patron = "todos iguales|todos los datos son iguales|sin variabilidad",
    nombre = "datos_iguales",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      length(unique(params$datos_ord)) == 1
    },
    mensaje = "todos los datos deben ser iguales"
  ),
  # --- Reglas para datos ordenados/desordenados ---
  list(
    patron = "sin ordenar|datos desordenados|no orden.|orden original",
    nombre = "datos_desordenados",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      TRUE  # Siempre aplicable — el error ES no ordenar
    },
    mensaje = "el error implica datos desordenados"
  ),
  # --- Reglas para cuartiles ---
  list(
    patron = "\\bcuartil\\b|\\bQ1\\b|\\bQ3\\b|primer cuartil|tercer cuartil",
    nombre = "cuartiles",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      length(params$datos_ord) >= 4  # Mínimo 4 datos para cuartiles
    },
    mensaje = "se necesitan al menos 4 datos para cuartiles"
  ),
  # --- Reglas para rango/recorrido ---
  list(
    patron = "\\brango\\b|\\brecorrido\\b|diferencia entre m.ximo y m.nimo|valor m.ximo menos m.nimo",
    nombre = "rango",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      length(unique(params$datos_ord)) > 1  # Rango requiere datos no constantes
    },
    mensaje = "datos deben tener variabilidad para calcular rango"
  ),
  # --- Reglas para desviación estándar/varianza ---
  list(
    patron = "desviaci.n est.ndar|varianza|desviaci.n t.pica",
    nombre = "desviacion_estandar",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      length(params$datos_ord) >= 2  # Mínimo 2 datos
    },
    mensaje = "se necesitan al menos 2 datos para desviación estándar"
  ),
  # --- Reglas para datos negativos ---
  list(
    patron = "valores negativos|datos negativos|n.meros negativos",
    nombre = "datos_negativos",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      any(params$datos_ord < 0)
    },
    mensaje = "los datos deben contener valores negativos"
  ),
  # --- Reglas para datos con ceros ---
  list(
    patron = "contiene ceros|incluye cero|valor cero|dato cero",
    nombre = "datos_con_ceros",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      0 %in% params$datos_ord
    },
    mensaje = "los datos deben contener al menos un cero"
  ),
  # --- Reglas para datos enteros ---
  list(
    patron = "n.meros enteros|valores enteros|datos enteros|sin decimales",
    nombre = "datos_enteros",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      all(params$datos_ord == floor(params$datos_ord))
    },
    mensaje = "todos los datos deben ser enteros"
  ),
  # --- Reglas para datos con decimales ---
  list(
    patron = "valores decimales|datos decimales|con decimales|n.meros decimales",
    nombre = "datos_decimales",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      any(params$datos_ord != floor(params$datos_ord))
    },
    mensaje = "los datos deben contener al menos un decimal"
  ),
  # --- Reglas para frecuencia ---
  list(
    patron = "frecuencia relativa|proporci.n|porcentaje",
    nombre = "frecuencia_relativa",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      TRUE  # Siempre calculable si hay datos
    },
    mensaje = "se requieren datos para calcular frecuencia relativa"
  ),
  # --- Reglas para datos simétricos ---
  list(
    patron = "\\bsim.tric[oa]\\b|distribuci.n sim.trica",
    nombre = "datos_simetricos",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      if (length(params$datos_ord) < 3) return(NULL)
      m <- mean(params$datos_ord)
      md <- median(params$datos_ord)
      abs(m - md) / sd(params$datos_ord) < 0.1  # Asimetría baja
    },
    mensaje = "los datos deben ser aproximadamente simétricos"
  ),
  # --- Reglas para datos asimétricos ---
  list(
    patron = "\\basim.tric[oa]\\b|sesgo|sesgad[oa]",
    nombre = "datos_asimetricos",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      if (length(params$datos_ord) < 3) return(NULL)
      m <- mean(params$datos_ord)
      md <- median(params$datos_ord)
      abs(m - md) / sd(params$datos_ord) >= 0.1
    },
    mensaje = "los datos deben ser asimétricos"
  ),
  # --- Reglas para outliers/atípicos ---
  list(
    patron = "\\bat.pic[oa]s?\\b|\\boutlier\\b|valor extremo|datos extremos",
    nombre = "tiene_outliers",
    condicion = function(params) {
      if (is.null(params$datos_ord)) return(NULL)
      if (length(params$datos_ord) < 4) return(NULL)
      q1 <- quantile(params$datos_ord, 0.25)
      q3 <- quantile(params$datos_ord, 0.75)
      iqr <- q3 - q1
      any(params$datos_ord < q1 - 1.5 * iqr | params$datos_ord > q3 + 1.5 * iqr)
    },
    mensaje = "los datos deben contener valores atípicos"
  ),
  # --- Regla para muestra grande ---
  list(
    patron = "muestra grande|muchos datos|gran cantidad de datos",
    nombre = "muestra_grande",
    condicion = function(params) {
      if (is.null(params$n)) return(NULL)
      params$n >= 30
    },
    mensaje = "n debe ser >= 30 (muestra grande)"
  ),
  # --- Regla para muestra pequeña ---
  list(
    patron = "muestra peque.a|pocos datos|peque.a cantidad",
    nombre = "muestra_pequena",
    condicion = function(params) {
      if (is.null(params$n)) return(NULL)
      params$n < 30
    },
    mensaje = "n debe ser < 30 (muestra pequeña)"
  )
)

# --- Construir params desde el entorno del ejercicio ---
construir_params_desde_env <- function(env) {
  params <- list()
  # n: tamaño de muestra (varias convenciones de nombres)
  if (exists("n", envir = env)) params$n <- get("n", envir = env)
  if (exists("num_estudiantes", envir = env)) params$n <- get("num_estudiantes", envir = env)
  if (exists("n_datos", envir = env)) params$n <- get("n_datos", envir = env)
  # datos ordenados
  if (exists("datos_ord", envir = env)) params$datos_ord <- get("datos_ord", envir = env)
  if (exists("datos", envir = env) && is.null(params$datos_ord)) {
    d <- get("datos", envir = env)
    if (is.numeric(d)) params$datos_ord <- sort(d)
  }
  return(params)
}

# --- Detectar error seleccionado ---
detectar_error_seleccionado <- function(env) {
  if (exists("error_sel", envir = env)) return(get("error_sel", envir = env))
  if (exists("error_seleccionado", envir = env)) return(get("error_seleccionado", envir = env))
  return(NULL)
}

# --- Capa A: Verificar precondición declarada del error seleccionado ---
validar_capa_a_precondicion <- function(pool, error_sel, params) {
  errores <- character(0)
  if (is.null(error_sel)) return(errores)

  if (!is.null(error_sel$precondicion)) {
    codigo <- if (!is.null(error_sel$codigo)) error_sel$codigo else "desconocido"
    resultado <- tryCatch({
      cumple <- error_sel$precondicion(params)
      if (!isTRUE(cumple)) {
        paste0(
          "ERR_SEM_A: Error '", codigo,
          "' seleccionado pero su precondicion declarada no se cumple con los datos actuales"
        )
      } else {
        NULL
      }
    }, error = function(e) {
      paste0(
        "ERR_SEM_A: Error '", codigo,
        "' — precondicion() lanzó excepción: ", conditionMessage(e)
      )
    })
    if (!is.null(resultado)) errores <- c(errores, resultado)
  }

  return(errores)
}

# --- Capa B: Escaneo de keywords en descripciones ---
# Detecta condiciones IMPLICITAS en el texto que NO están
# declaradas en precondicion. Esto es lo que atrapa errores
# que el autor olvidó proteger con precondicion.
validar_capa_b_keywords <- function(pool, error_sel, params) {
  errores <- character(0)

  codigo_sel <- if (!is.null(error_sel) && !is.null(error_sel$codigo)) {
    error_sel$codigo
  } else {
    NULL
  }

  for (i in seq_along(pool)) {
    err <- pool[[i]]
    codigo <- if (!is.null(err$codigo)) err$codigo else paste0("error_", i)

    # Concatenar descripciones
    desc <- tolower(paste(
      if (!is.null(err$descripcion_corta)) err$descripcion_corta else "",
      if (!is.null(err$descripcion_larga)) err$descripcion_larga else ""
    ))

    for (regla in REGLAS_SEMANTICAS_KEYWORDS) {
      if (grepl(regla$patron, desc, perl = TRUE, ignore.case = TRUE)) {
        # Descripción implica esta condición — ¿se cumple?
        resultado <- tryCatch(regla$condicion(params), error = function(e) NULL)
        if (is.null(resultado)) next  # No se puede verificar

        if (!resultado) {
          # La condición NO se cumple. ¿La precondicion del error lo previene?
          precond_previene <- FALSE
          if (!is.null(err$precondicion)) {
            tryCatch({
              precond_previene <- !isTRUE(err$precondicion(params))
            }, error = function(e) {})
          }

          if (!precond_previene) {
            # FALLA: descripción implica condición, condición no se cumple,
            # precondicion no previene la selección
            es_seleccionado <- !is.null(codigo_sel) && codigo == codigo_sel
            if (es_seleccionado) {
              # ERROR BLOQUEANTE: este error FUE seleccionado y es incoherente
              errores <- c(errores, paste0(
                "ERR_SEM_B: Error '", codigo,
                "' SELECCIONADO — descripcion implica '", regla$mensaje,
                "' pero condicion no se cumple (n=",
                if (!is.null(params$n)) params$n else "?",
                ") y precondicion no lo previene"
              ))
            } else {
              # WARNING: error en pool tiene descripción desprotegida
              # No bloquea pero indica bug latente
              errores <- c(errores, paste0(
                "WARN_SEM_B: Error '", codigo,
                "' en pool — descripcion implica '", regla$mensaje,
                "' pero no tiene precondicion que lo restrinja cuando condicion no se cumple"
              ))
            }
          }
        }
      }
    }
  }

  return(errores)
}

# --- Capa C: Cross-validación calcula() vs valor correcto ---
# Verifica que el error seleccionado produce un valor diferente del correcto
validar_capa_c_crossval <- function(pool, error_sel, env) {
  errores <- character(0)
  if (is.null(error_sel)) return(errores)

  codigo <- if (!is.null(error_sel$codigo)) error_sel$codigo else "desconocido"

  # Detectar valor correcto y valor erróneo
  val_correcto <- NULL
  val_erroneo <- NULL
  for (vname in c("mediana_calc", "valor_correcto", "respuesta_correcta")) {
    if (exists(vname, envir = env)) { val_correcto <- get(vname, envir = env); break }
  }
  for (vname in c("mediana_erronea", "respuesta_erronea", "valor_erroneo")) {
    if (exists(vname, envir = env)) { val_erroneo <- get(vname, envir = env); break }
  }

  if (!is.null(val_correcto) && !is.null(val_erroneo)) {
    if (isTRUE(val_correcto == val_erroneo)) {
      errores <- c(errores, paste0(
        "ERR_SEM_C: Error '", codigo,
        "' seleccionado pero calcula() produce el mismo valor que la respuesta correcta (",
        val_correcto, ")"
      ))
    }
  }

  return(errores)
}

# --- Función principal: orquesta las 3 capas ---
validar_precondiciones_error_pool <- function(env) {
  errores <- character(0)

  tiene_pool <- exists("errores_conceptuales", envir = env)
  if (!tiene_pool) return(errores)

  pool <- get("errores_conceptuales", envir = env)
  error_sel <- detectar_error_seleccionado(env)
  params <- construir_params_desde_env(env)

  # Capa A: Precondición declarada
  errores <- c(errores, validar_capa_a_precondicion(pool, error_sel, params))

  # Capa B: Escaneo de keywords (detección automática)
  errores <- c(errores, validar_capa_b_keywords(pool, error_sel, params))

  # Capa C: Cross-validación calcula vs correcto
  errores <- c(errores, validar_capa_c_crossval(pool, error_sel, env))

  # Separar errores bloqueantes de warnings
  return(errores)
}

# ============================================================
# VALIDACIÓN DE CORRECTITUD DE RESPUESTA (Nivel 5)
#
# Nivel 1: Sintáctico  — código corre sin errores
# Nivel 2: Numérico    — valores válidos, no NA/NaN/Inf
# Nivel 3: Estructural — metadatos completos, formatos correctos
# Nivel 4: Semántico   — descripción del error corresponde a datos
# Nivel 5: Correctitud — respuesta marcada ES la correcta
#
# Sub-niveles:
#   5A: Evaluar exsolution dinámico (inline R)
#   5B: Cross-check respuesta marcada vs valor correcto
#   5C: Unicidad de opciones en runtime
#   5D: Validación de rangos matemáticos
#   5E: Distractor ≠ respuesta correcta
# ============================================================

# --- 5A: Evaluar exsolution dinámico ---
# Cuando exsolution contiene `r ...`, el Nivel 3 lo salta.
# Este nivel lo evalúa en el entorno del ejercicio.
validar_5a_exsolution_dinamico <- function(meta_lineas, env, extype) {
  errores <- character(0)
  if (is.null(env)) return(errores)

  exsolution_raw <- trimws(extraer_meta(meta_lineas, "exsolution"))
  if (is.na(exsolution_raw)) return(errores)

  # Solo actuar si es dinámico (contiene `r)
  if (!grepl("`r\\s", exsolution_raw)) return(errores)

  # Extraer la(s) expresión(es) R
  # Puede ser un solo `r expr` o múltiples separadas por |
  partes <- strsplit(exsolution_raw, "\\|")[[1]]

  for (i in seq_along(partes)) {
    parte <- trimws(partes[[i]])
    if (!grepl("`r\\s", parte)) next

    # Extraer la expresión R del inline
    expr_match <- regmatches(parte, regexpr("`r\\s+[^`]+`", parte))
    if (length(expr_match) == 0) next

    expr_r <- sub("^`r\\s+", "", sub("`$", "", expr_match))

    valor_eval <- tryCatch(
      eval(parse(text = expr_r), envir = env),
      error = function(e) {
        errores <<- c(errores, paste0(
          "ERR_ANS_A: exsolution dinámico parte ", i,
          " no pudo evaluarse: ", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(valor_eval)) next

    valor_str <- as.character(valor_eval)

    # Validar según tipo de ejercicio
    if (extype == "schoice") {
      # Debe ser string binario con exactamente 1 "1"
      if (!grepl("^[01]+$", valor_str)) {
        errores <- c(errores, paste0(
          "ERR_ANS_A: exsolution dinámico evalúa a '", valor_str,
          "' — no es formato binario válido para SCHOICE"))
      } else {
        n_correctas <- sum(strsplit(valor_str, "")[[1]] == "1")
        if (n_correctas != 1) {
          errores <- c(errores, paste0(
            "ERR_ANS_A: exsolution dinámico '", valor_str,
            "' tiene ", n_correctas, " correctas — SCHOICE requiere exactamente 1"))
        }
      }
    }
  }

  return(errores)
}

# --- 5B: Cross-check respuesta marcada vs valor correcto ---
# Verifica que la opción en la posición marcada como correcta
# en sol/exsolution REALMENTE corresponde al valor correcto calculado.
validar_5b_crosscheck <- function(env, extype) {
  errores <- character(0)
  if (is.null(env)) return(errores)

  objetos <- ls(envir = env)

  # Detectar vector de solución (sol)
  sol <- NULL
  for (vname in c("sol", "solucion", "solucion_vector")) {
    if (vname %in% objetos) {
      candidato <- get(vname, envir = env)
      if (is.numeric(candidato) && all(candidato %in% c(0, 1))) {
        sol <- candidato
        break
      }
    }
  }

  # Detectar valor correcto
  valor_correcto <- NULL
  for (vname in c("valor_correcto", "mediana_calc", "respuesta_correcta",
                   "media_correcta", "mediana_correcta", "moda_correcta")) {
    if (vname %in% objetos) {
      valor_correcto <- get(vname, envir = env)
      break
    }
  }

  # Detectar opciones (lista o vector con las opciones de respuesta)
  opciones <- NULL
  for (vname in c("opciones_mezcladas", "opciones_valores", "opciones",
                   "opciones_texto", "opciones_num")) {
    if (vname %in% objetos) {
      candidato <- get(vname, envir = env)
      if (is.list(candidato) || is.vector(candidato)) {
        opciones <- candidato
        break
      }
    }
  }

  # Solo validar si tenemos los 3 componentes
  if (is.null(sol) || is.null(valor_correcto) || is.null(opciones)) return(errores)

  # Verificar que sol marca exactamente 1 correcta (SCHOICE)
  if (extype == "schoice") {
    idx_correcto <- which(sol == 1)
    if (length(idx_correcto) != 1) return(errores)  # Ya validado por Nivel 3

    # Obtener la opción marcada como correcta
    if (idx_correcto <= length(opciones)) {
      opcion_marcada <- opciones[[idx_correcto]]

      # Comparar con valor correcto
      if (is.numeric(opcion_marcada) && is.numeric(valor_correcto)) {
        if (abs(opcion_marcada - valor_correcto) > 0.01) {
          errores <- c(errores, paste0(
            "ERR_ANS_B: Opción marcada como correcta (posición ", idx_correcto,
            ", valor=", opcion_marcada,
            ") NO coincide con valor_correcto (", valor_correcto, ")"))
        }
      }
    }
  }

  return(errores)
}

# --- 5C: Unicidad de opciones en runtime ---
# Verifica que las opciones de SCHOICE sean todas diferentes entre sí.
# Para ejercicios _neg_: verifica patrón (N-1) iguales + 1 diferente.
validar_5c_unicidad <- function(env, extype, archivo_rmd = NULL) {
  errores <- character(0)
  if (is.null(env) || extype != "schoice") return(errores)

  objetos <- ls(envir = env)

  # Detectar opciones (varias convenciones de nombres)
  opciones <- NULL
  opciones_nombre <- NULL
  for (vname in c("opciones_mezcladas", "opciones_graficos",
                   "opciones_valores", "opciones", "opciones_num")) {
    if (vname %in% objetos) {
      candidato <- get(vname, envir = env)
      if ((is.list(candidato) && length(candidato) >= 2) ||
          (is.vector(candidato) && length(candidato) >= 2)) {
        opciones <- candidato
        opciones_nombre <- vname
        break
      }
    }
  }

  if (is.null(opciones)) return(errores)

  # Calcular hashes de cada opción
  hashes <- sapply(seq_along(opciones), function(i) {
    digest::digest(opciones[[i]])
  })

  # ¿Es ejercicio _neg_?
  es_negativo <- FALSE
  if (!is.null(archivo_rmd)) {
    es_negativo <- grepl("_neg_", basename(archivo_rmd))
  }

  if (es_negativo) {
    # Patrón _neg_: exactamente (N-1) iguales + 1 diferente
    freq <- table(hashes)
    n_opciones <- length(hashes)
    if (length(freq) != 2 || !((n_opciones - 1) %in% as.integer(freq))) {
      errores <- c(errores, paste0(
        "ERR_ANS_C: Ejercicio _neg_ — se esperan ", n_opciones - 1,
        " opciones idénticas + 1 diferente, pero hay ",
        length(freq), " hashes distintos (frecuencias: ",
        paste(as.integer(freq), collapse = ","), ")"))
    }
  } else {
    # Patrón normal: TODAS las opciones deben ser diferentes
    n_unicas <- length(unique(hashes))
    if (n_unicas != length(hashes)) {
      # Identificar cuáles están duplicadas
      duplicados <- which(duplicated(hashes))
      errores <- c(errores, paste0(
        "ERR_ANS_C: Opciones duplicadas en SCHOICE — ",
        length(hashes), " opciones pero solo ", n_unicas, " únicas",
        " (duplicadas en posiciones: ", paste(duplicados, collapse = ","), ")"))
    }
  }

  return(errores)
}

# --- 5D: Validación de rangos matemáticos ---
# Verifica que los valores calculados estén en rangos válidos.
validar_5d_rangos <- function(env) {
  errores <- character(0)
  if (is.null(env)) return(errores)

  objetos <- ls(envir = env)

  # Obtener datos si existen
  datos <- NULL
  for (vname in c("datos_ord", "datos")) {
    if (vname %in% objetos) {
      candidato <- get(vname, envir = env)
      if (is.numeric(candidato) && length(candidato) >= 2) {
        datos <- candidato
        break
      }
    }
  }

  if (!is.null(datos)) {
    rango_min <- min(datos)
    rango_max <- max(datos)

    # Mediana debe estar en [min, max]
    for (vname in c("mediana_calc", "mediana_correcta", "mediana")) {
      if (vname %in% objetos) {
        val <- get(vname, envir = env)
        if (is.numeric(val) && length(val) == 1) {
          if (val < rango_min - 0.01 || val > rango_max + 0.01) {
            errores <- c(errores, paste0(
              "ERR_ANS_D: ", vname, " (", val,
              ") fuera del rango de datos [", rango_min, ", ", rango_max, "]"))
          }
        }
        break
      }
    }

    # Media debe estar en rango razonable (min - spread, max + spread)
    for (vname in c("media_calc", "media_correcta", "media")) {
      if (vname %in% objetos) {
        val <- get(vname, envir = env)
        if (is.numeric(val) && length(val) == 1) {
          if (val < rango_min - 0.01 || val > rango_max + 0.01) {
            errores <- c(errores, paste0(
              "ERR_ANS_D: ", vname, " (", val,
              ") fuera del rango de datos [", rango_min, ", ", rango_max, "]"))
          }
        }
        break
      }
    }

    # Cuartiles deben cumplir min <= Q1 <= Q2 <= Q3 <= max
    if ("cuartiles_correctos" %in% objetos) {
      q <- get("cuartiles_correctos", envir = env)
      if (is.list(q)) {
        qvals <- c(
          if (!is.null(q$q1)) q$q1 else NA,
          if (!is.null(q$mediana)) q$mediana else NA,
          if (!is.null(q$q3)) q$q3 else NA
        )
        qvals <- qvals[!is.na(qvals)]
        if (length(qvals) >= 2) {
          # Verificar orden
          if (is.unsorted(qvals)) {
            errores <- c(errores, paste0(
              "ERR_ANS_D: Cuartiles desordenados — valores: ",
              paste(qvals, collapse = ", ")))
          }
          # Verificar rango
          if (any(qvals < rango_min - 0.01) || any(qvals > rango_max + 0.01)) {
            errores <- c(errores, paste0(
              "ERR_ANS_D: Cuartil(es) fuera del rango de datos [",
              rango_min, ", ", rango_max, "] — valores: ",
              paste(qvals, collapse = ", ")))
          }
        }
      }
    }
  }

  # Probabilidades deben estar en [0, 1]
  for (vname in objetos) {
    if (grepl("prob", vname, ignore.case = TRUE)) {
      val <- tryCatch(get(vname, envir = env), error = function(e) NULL)
      if (is.numeric(val) && length(val) == 1) {
        if (val < -0.001 || val > 1.001) {
          errores <- c(errores, paste0(
            "ERR_ANS_D: Variable '", vname, "' (", val,
            ") fuera del rango de probabilidad [0, 1]"))
        }
      }
    }
  }

  # Porcentajes deben estar en [0, 100]
  for (vname in objetos) {
    if (grepl("porcentaje|pct|percent", vname, ignore.case = TRUE)) {
      val <- tryCatch(get(vname, envir = env), error = function(e) NULL)
      if (is.numeric(val) && length(val) == 1) {
        if (val < -0.01 || val > 100.01) {
          errores <- c(errores, paste0(
            "ERR_ANS_D: Variable '", vname, "' (", val,
            ") fuera del rango de porcentaje [0, 100]"))
        }
      }
    }
  }

  return(errores)
}

# --- 5E: Distractor ≠ respuesta correcta ---
# Verifica que ningún distractor sea idéntico a la respuesta correcta.
validar_5e_distractor_vs_correcto <- function(env, extype) {
  errores <- character(0)
  if (is.null(env) || extype != "schoice") return(errores)

  objetos <- ls(envir = env)

  # Detectar sol
  sol <- NULL
  for (vname in c("sol", "solucion", "solucion_vector")) {
    if (vname %in% objetos) {
      candidato <- get(vname, envir = env)
      if (is.numeric(candidato) && all(candidato %in% c(0, 1))) {
        sol <- candidato
        break
      }
    }
  }

  # Detectar opciones
  opciones <- NULL
  for (vname in c("opciones_mezcladas", "opciones_graficos",
                   "opciones_valores", "opciones", "opciones_num")) {
    if (vname %in% objetos) {
      candidato <- get(vname, envir = env)
      if ((is.list(candidato) || is.vector(candidato)) && length(candidato) >= 2) {
        opciones <- candidato
        break
      }
    }
  }

  if (is.null(sol) || is.null(opciones)) return(errores)
  if (length(sol) != length(opciones)) return(errores)

  idx_correcto <- which(sol == 1)
  if (length(idx_correcto) != 1) return(errores)

  hash_correcto <- digest::digest(opciones[[idx_correcto]])

  # Verificar que ningún distractor sea idéntico al correcto
  for (i in seq_along(opciones)) {
    if (i == idx_correcto) next
    hash_distractor <- digest::digest(opciones[[i]])
    if (hash_distractor == hash_correcto) {
      errores <- c(errores, paste0(
        "ERR_ANS_E: Distractor en posición ", i,
        " es IDÉNTICO a la respuesta correcta (posición ", idx_correcto, ")"))
    }
  }

  return(errores)
}

# --- Función orquestadora Nivel 5 ---
validar_nivel5_correctitud <- function(parsed, env, extype, archivo_rmd = NULL) {
  errores <- character(0)

  # 5A: Evaluar exsolution dinámico
  errores <- c(errores, validar_5a_exsolution_dinamico(parsed$meta, env, extype))

  # 5B: Cross-check respuesta marcada vs valor correcto
  errores <- c(errores, validar_5b_crosscheck(env, extype))

  # 5C: Unicidad de opciones en runtime
  errores <- c(errores, validar_5c_unicidad(env, extype, archivo_rmd))

  # 5D: Validación de rangos matemáticos
  errores <- c(errores, validar_5d_rangos(env))

  # 5E: Distractor ≠ respuesta correcta
  errores <- c(errores, validar_5e_distractor_vs_correcto(env, extype))

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

# --- Función callable (para source() desde tests y otros scripts) ---

#' Valida coherencia matemática de un archivo .Rmd
#' @param archivo_rmd Ruta al archivo .Rmd
#' @param strict Modo estricto (default FALSE)
#' @return Lista con: aprobado (logical), errores (character), warnings (character), extype (character)
validar_coherencia_matematica <- function(archivo_rmd, strict = FALSE) {
  if (!file.exists(archivo_rmd)) {
    return(list(
      aprobado = FALSE,
      errores = paste("ERROR: Archivo no encontrado:", archivo_rmd),
      warnings = character(0),
      extype = NA_character_
    ))
  }

  parsed <- parsear_rmd(archivo_rmd)
  extype <- tolower(trimws(extraer_meta(parsed$meta, "extype")))

  todos_errores <- character(0)
  todos_warnings <- character(0)

  resultado <- ejecutar_chunks(parsed$chunks_r)
  todos_errores <- c(todos_errores, resultado$errores)
  todos_warnings <- c(todos_warnings, resultado$warnings)

  todos_errores <- c(todos_errores, validar_metadatos(parsed$meta))

  # Excepción exshuffle: SCHOICE con opciones gráficas PNG (diagrama_*.png)
  # permite exshuffle:FALSE porque sample() interno ya aleatoriza
  # y TRUE rompería la referencia a letra_correcta en Solution
  contenido_completo <- readLines(archivo_rmd, warn = FALSE, encoding = "UTF-8")
  tiene_opciones_graficas_png <- any(grepl("!\\[\\]\\(diagrama_", contenido_completo))
  if (tiene_opciones_graficas_png && extype == "schoice") {
    todos_errores <- todos_errores[!grepl("exshuffle", todos_errores)]
  }

  if (extype == "schoice") {
    todos_errores <- c(todos_errores,
      validar_coherencia_schoice(parsed$meta, resultado$env, parsed$num_q_opciones))
  } else if (extype == "cloze") {
    todos_errores <- c(todos_errores,
      validar_coherencia_cloze(parsed$meta, resultado$env))
  }

  todos_errores <- c(todos_errores, validar_coherencia_matematica_general(resultado$env))

  # Validación semántica (3 capas): separar errores de warnings
  sem_resultados <- validar_precondiciones_error_pool(resultado$env)
  sem_errores <- sem_resultados[grepl("^ERR_SEM", sem_resultados)]
  sem_warnings <- sem_resultados[grepl("^WARN_SEM", sem_resultados)]
  todos_errores <- c(todos_errores, sem_errores)
  todos_warnings <- c(todos_warnings, sem_warnings)

  todos_errores <- c(todos_errores, validar_codigo(parsed$contenido))

  # Validación de correctitud de respuesta (Nivel 5)
  errores_n5 <- validar_nivel5_correctitud(parsed, resultado$env, extype, archivo_rmd)
  todos_errores <- c(todos_errores, errores_n5)

  return(list(
    aprobado = length(todos_errores) == 0,
    errores = todos_errores,
    warnings = todos_warnings,
    extype = extype
  ))
}

# --- Ejecución CLI (solo cuando se ejecuta con Rscript, no con source()) ---
if (sys.nframe() == 0) {

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

# 5b. Validar coherencia semántica del pool de errores (3 capas)
cat("\n--- Validando coherencia semántica (Nivel 4) ---\n")
err_sem <- validar_precondiciones_error_pool(resultado$env)
err_sem_bloqueantes <- err_sem[grepl("^ERR_SEM", err_sem)]
warn_sem <- err_sem[grepl("^WARN_SEM", err_sem)]
if (length(err_sem_bloqueantes) > 0) {
  cat("  ERRORES semánticos:\n")
  for (e in err_sem_bloqueantes) cat("    ", e, "\n")
  todos_errores <- c(todos_errores, err_sem_bloqueantes)
}
if (length(warn_sem) > 0) {
  cat("  Warnings semánticos (bugs latentes):\n")
  for (w in warn_sem) cat("    ", w, "\n")
  todos_warnings <- c(todos_warnings, warn_sem)
}
if (length(err_sem) == 0) {
  cat("  Capa A (precondicion declarada): OK\n")
  cat("  Capa B (keywords en descripciones): OK\n")
  cat("  Capa C (cross-validacion calcula vs correcto): OK\n")
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

# 6b. Validar correctitud de respuesta (Nivel 5)
cat("\n--- Validando correctitud de respuesta (Nivel 5) ---\n")
err_n5 <- validar_nivel5_correctitud(parsed, resultado$env, extype, archivo_rmd)
if (length(err_n5) > 0) {
  cat("  ERRORES de correctitud:\n")
  for (e in err_n5) cat("    ", e, "\n")
  todos_errores <- c(todos_errores, err_n5)
} else {
  cat("  5A (exsolution dinámico): OK\n")
  cat("  5B (cross-check respuesta marcada): OK\n")
  cat("  5C (unicidad de opciones): OK\n")
  cat("  5D (rangos matemáticos): OK\n")
  cat("  5E (distractor ≠ correcto): OK\n")
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

} # end if (sys.nframe() == 0)
