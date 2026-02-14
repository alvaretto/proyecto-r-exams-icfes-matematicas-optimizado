#!/usr/bin/env Rscript
# =============================================================================
# validar_multisemilla.R
# Script de stress-test: ejecuta un .Rmd N veces con diferentes semillas
# y valida correctitud (Nivel 5) en cada ejecución.
#
# Uso:
#   Rscript validar_multisemilla.R archivo.Rmd [--n 20] [--modo rapido|exhaustivo]
#
# Modos:
#   rapido     - 20 semillas (default, para hook automático)
#   exhaustivo - 100 semillas (pre-promoción)
#
# Códigos de salida:
#   0 = Sin fallos
#   1 = Al menos un fallo
#   2 = Error de ejecución del script
# =============================================================================

# Cargar la validación principal
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  # Fallback: buscar el script por ruta conocida
  posibles_rutas <- c(
    ".claude/scripts/validar_coherencia_matematica.R",
    "SOURCES/scripts_validacion/validar_coherencia_matematica.R"
  )
  for (ruta in posibles_rutas) {
    if (file.exists(ruta)) {
      source(ruta)
      break
    }
  }
} else {
  source(file.path(script_dir, "validar_coherencia_matematica.R"))
}

#' Ejecuta validación multi-semilla sobre un archivo .Rmd
#'
#' @param archivo_rmd Ruta al archivo .Rmd
#' @param n_semillas Número de semillas a probar (default 20)
#' @return Lista con: total_semillas, fallos (lista), tasa_exito, aprobado
validar_multisemilla <- function(archivo_rmd, n_semillas = 20) {
  if (!file.exists(archivo_rmd)) {
    return(list(
      total_semillas = n_semillas,
      fallos = list(list(semilla = 0, tipo = "archivo", errores = "Archivo no encontrado")),
      tasa_exito = 0,
      aprobado = FALSE
    ))
  }

  parsed <- parsear_rmd(archivo_rmd)
  extype <- tolower(trimws(extraer_meta(parsed$meta, "extype")))
  fallos <- list()

  # Semillas dispersas usando primos
  semillas <- (1:n_semillas) * 7919

  for (i in seq_along(semillas)) {
    semilla_actual <- semillas[i]

    # Ejecutar chunks con semilla fija
    resultado <- tryCatch({
      set.seed(semilla_actual)
      ejecutar_chunks(parsed$chunks_r)
    }, error = function(e) {
      list(env = NULL, errores = paste0("Error global: ", conditionMessage(e)), warnings = character(0))
    })

    # Error de ejecución
    if (length(resultado$errores) > 0) {
      fallos[[length(fallos) + 1]] <- list(
        semilla = semilla_actual,
        indice = i,
        tipo = "ejecucion",
        errores = resultado$errores
      )
      next
    }

    # Validar Nivel 5 completo
    errores_n5 <- validar_nivel5_correctitud(parsed, resultado$env, extype, archivo_rmd)

    if (length(errores_n5) > 0) {
      fallos[[length(fallos) + 1]] <- list(
        semilla = semilla_actual,
        indice = i,
        tipo = "correctitud",
        errores = errores_n5
      )
    }

    # También validar coherencia general y semántica
    errores_math <- validar_coherencia_matematica_general(resultado$env)
    errores_sem <- validar_precondiciones_error_pool(resultado$env)
    errores_sem_bloq <- errores_sem[grepl("^ERR_SEM", errores_sem)]

    errores_extra <- c(errores_math, errores_sem_bloq)
    if (length(errores_extra) > 0) {
      # Si ya se registró un fallo para esta semilla, agregar errores
      ya_registrado <- any(sapply(fallos, function(f) f$semilla == semilla_actual))
      if (ya_registrado) {
        idx <- which(sapply(fallos, function(f) f$semilla == semilla_actual))
        fallos[[idx]]$errores <- c(fallos[[idx]]$errores, errores_extra)
      } else {
        fallos[[length(fallos) + 1]] <- list(
          semilla = semilla_actual,
          indice = i,
          tipo = "matematica_semantica",
          errores = errores_extra
        )
      }
    }
  }

  tasa_exito <- 1 - length(fallos) / n_semillas

  return(list(
    total_semillas = n_semillas,
    fallos = fallos,
    tasa_exito = tasa_exito,
    aprobado = length(fallos) == 0
  ))
}

# --- Ejecución CLI ---
if (sys.nframe() == 0) {

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    cat("Uso: Rscript validar_multisemilla.R archivo.Rmd [--n 20] [--modo rapido|exhaustivo]\n")
    quit(status = 2)
  }

  archivo_rmd <- args[1]
  n_semillas <- 20
  modo <- "rapido"

  # Parsear argumentos opcionales
  if ("--n" %in% args) {
    idx <- which(args == "--n")
    if (idx < length(args)) n_semillas <- as.integer(args[idx + 1])
  }
  if ("--modo" %in% args) {
    idx <- which(args == "--modo")
    if (idx < length(args)) modo <- args[idx + 1]
  }
  if (modo == "exhaustivo") n_semillas <- max(n_semillas, 100)

  cat("=== VALIDACIÓN MULTI-SEMILLA ===\n")
  cat("Archivo:", archivo_rmd, "\n")
  cat("Semillas:", n_semillas, "\n")
  cat("Modo:", modo, "\n\n")

  resultado <- validar_multisemilla(archivo_rmd, n_semillas)

  cat("============================================\n")
  cat(sprintf("  Semillas probadas: %d\n", resultado$total_semillas))
  cat(sprintf("  Fallos: %d\n", length(resultado$fallos)))
  cat(sprintf("  Tasa de éxito: %.1f%%\n", resultado$tasa_exito * 100))
  cat("============================================\n")

  if (resultado$aprobado) {
    cat("\n  RESULTADO: APROBADO (0 fallos en ", n_semillas, " semillas)\n")
    quit(status = 0)
  } else {
    cat("\n  RESULTADO: FALLOS DETECTADOS\n\n")
    for (fallo in resultado$fallos) {
      cat(sprintf("  Semilla %d (índice %d) — tipo: %s\n",
        fallo$semilla, fallo$indice, fallo$tipo))
      for (e in fallo$errores) cat("    ", e, "\n")
      cat("\n")
    }
    quit(status = 1)
  }

} # end if (sys.nframe() == 0)
