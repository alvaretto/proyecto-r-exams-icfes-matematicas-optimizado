#!/usr/bin/env Rscript

# ============================================================================
# Puente R-Python para Análisis Visual
# ============================================================================
# 
# Integra el analizador visual de Python con el sistema R existente
#
# ============================================================================

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(reticulate)
  library(jsonlite)
  library(stringr)
})

# ============================================================================
# CONFIGURACIÓN
# ============================================================================

# Configurar Python
setup_python_environment <- function() {
  # Verificar si Python está disponible
  if (!py_available()) {
    stop("Python no está disponible. Instalar Python 3.8+ y las dependencias necesarias.")
  }
  
  # Verificar dependencias Python
  required_packages <- c("opencv-python", "pillow", "pytesseract", "numpy", "matplotlib")
  
  for (pkg in required_packages) {
    if (!py_module_available(str_replace(pkg, "-", "_"))) {
      message(paste("Instalando paquete Python:", pkg))
      py_install(pkg)
    }
  }
  
  message("Entorno Python configurado correctamente")
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE ANÁLISIS VISUAL
# ============================================================================

#' Analizar archivo Rmd con análisis visual
#' @param rmd_path Ruta al archivo .Rmd
#' @param config Lista de configuración
#' @return Lista con resultados del análisis visual
analyze_rmd_visual <- function(rmd_path, config = list()) {
  
  # Configurar entorno Python
  tryCatch({
    setup_python_environment()
  }, error = function(e) {
    return(list(
      error = TRUE,
      message = paste("Error configurando Python:", e$message),
      visual_analysis = NULL
    ))
  })
  
  # Obtener ruta del analizador Python
  script_dir <- dirname(normalizePath(commandArgs(trailingOnly = FALSE)[4]))
  if (is.na(script_dir) || script_dir == ".") {
    script_dir <- getwd()
  }
  
  python_analyzer <- file.path(dirname(script_dir), "core", "visual_analyzer.py")
  
  if (!file.exists(python_analyzer)) {
    return(list(
      error = TRUE,
      message = paste("Analizador Python no encontrado:", python_analyzer),
      visual_analysis = NULL
    ))
  }
  
  # Ejecutar análisis visual Python
  tryCatch({
    # Importar módulo Python
    source_python(python_analyzer)
    
    # Crear instancia del analizador
    analyzer <- VisualAnalyzer(config)
    
    # Ejecutar análisis
    results <- analyzer$analyze_rmd_file(rmd_path)
    
    # Procesar resultados
    processed_results <- process_visual_results(results)
    
    return(list(
      error = FALSE,
      message = "Análisis visual completado exitosamente",
      visual_analysis = processed_results
    ))
    
  }, error = function(e) {
    return(list(
      error = TRUE,
      message = paste("Error en análisis visual:", e$message),
      visual_analysis = NULL
    ))
  })
}

# ============================================================================
# PROCESAMIENTO DE RESULTADOS
# ============================================================================

#' Procesar resultados del análisis visual Python
#' @param python_results Resultados del analizador Python
#' @return Lista procesada para R
process_visual_results <- function(python_results) {
  
  # Convertir a formato R estándar
  processed <- list(
    file_path = python_results$file_path,
    summary = list(
      graphics_count = python_results$summary$total_graphics,
      visual_errors_count = python_results$summary$total_errors,
      math_errors_count = python_results$summary$total_math_errors,
      status = python_results$summary$status
    ),
    graphics_generated = python_results$graphics_generated,
    findings = list()
  )
  
  # Procesar errores visuales
  if (length(python_results$visual_errors) > 0) {
    for (error in python_results$visual_errors) {
      processed$findings <- append(processed$findings, list(list(
        type = "visual_error",
        severity = error$severity %||% "error",
        message = error$message,
        category = error$type,
        source = "visual_analysis"
      )))
    }
  }
  
  # Procesar errores matemáticos
  if (length(python_results$mathematical_inconsistencies) > 0) {
    for (error in python_results$mathematical_inconsistencies) {
      processed$findings <- append(processed$findings, list(list(
        type = "mathematical_error",
        severity = error$severity %||% "error", 
        message = error$message,
        category = error$type,
        source = "visual_analysis",
        values = error$values %||% NULL
      )))
    }
  }
  
  # Agregar datos extraídos
  processed$extracted_data <- python_results$data_extraction
  
  return(processed)
}

# ============================================================================
# INTEGRACIÓN CON SISTEMA EXISTENTE
# ============================================================================

#' Crear resultado de validación compatible con sistema existente
#' @param visual_results Resultados del análisis visual
#' @return Objeto validation_result
create_visual_validation_result <- function(visual_results) {
  
  if (visual_results$error) {
    return(list(
      errors = list(list(
        message = visual_results$message,
        type = "visual_analysis_error",
        severity = "error"
      )),
      warnings = list(),
      info = list(),
      suggestions = list(),
      summary = list(
        error_count = 1,
        warning_count = 0,
        info_count = 0,
        suggestion_count = 0
      )
    ))
  }
  
  analysis <- visual_results$visual_analysis
  
  # Separar por tipo de severidad
  errors <- list()
  warnings <- list()
  info <- list()
  suggestions <- list()
  
  for (finding in analysis$findings) {
    item <- list(
      message = finding$message,
      type = finding$category,
      source = finding$source
    )
    
    if (!is.null(finding$values)) {
      item$values <- finding$values
    }
    
    if (finding$severity == "error") {
      errors <- append(errors, list(item))
    } else if (finding$severity == "warning") {
      warnings <- append(warnings, list(item))
    } else {
      info <- append(info, list(item))
    }
  }
  
  # Agregar información sobre gráficas generadas
  if (analysis$summary$graphics_count > 0) {
    info <- append(info, list(list(
      message = paste("Gráficas analizadas visualmente:", analysis$summary$graphics_count),
      type = "visual_analysis_completed",
      source = "visual_analysis"
    )))
  }
  
  return(list(
    errors = errors,
    warnings = warnings,
    info = info,
    suggestions = suggestions,
    summary = list(
      error_count = length(errors),
      warning_count = length(warnings),
      info_count = length(info),
      suggestion_count = length(suggestions)
    ),
    visual_data = analysis
  ))
}

# ============================================================================
# FUNCIÓN DE TESTING
# ============================================================================

#' Función de testing para desarrollo
test_visual_analysis <- function(rmd_file = NULL) {
  
  if (is.null(rmd_file)) {
    # Usar archivo de ejemplo si no se especifica
    rmd_file <- "/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas/I_1796473-Opc-A2v2_optimizado.Rmd"
  }
  
  if (!file.exists(rmd_file)) {
    stop(paste("Archivo no encontrado:", rmd_file))
  }
  
  cat("=== TESTING ANÁLISIS VISUAL ===\n")
  cat("Archivo:", rmd_file, "\n\n")
  
  # Ejecutar análisis
  start_time <- Sys.time()
  results <- analyze_rmd_visual(rmd_file)
  end_time <- Sys.time()
  
  cat("Tiempo de ejecución:", round(as.numeric(end_time - start_time), 2), "segundos\n\n")
  
  # Mostrar resultados
  if (results$error) {
    cat("❌ ERROR:", results$message, "\n")
  } else {
    analysis <- results$visual_analysis
    cat("✅ ANÁLISIS COMPLETADO\n")
    cat("Gráficas analizadas:", analysis$summary$graphics_count, "\n")
    cat("Errores visuales:", analysis$summary$visual_errors_count, "\n")
    cat("Errores matemáticos:", analysis$summary$math_errors_count, "\n")
    cat("Estado:", analysis$summary$status, "\n\n")
    
    # Mostrar hallazgos
    if (length(analysis$findings) > 0) {
      cat("=== HALLAZGOS ===\n")
      for (i in seq_along(analysis$findings)) {
        finding <- analysis$findings[[i]]
        cat(sprintf("%d. [%s] %s\n", i, toupper(finding$severity), finding$message))
      }
    } else {
      cat("✅ No se encontraron problemas visuales\n")
    }
  }
  
  return(results)
}

# ============================================================================
# EJECUCIÓN PRINCIPAL
# ============================================================================

# Si se ejecuta directamente
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    cat("Uso: Rscript visual_bridge.R <archivo.Rmd>\n")
    cat("O para testing: Rscript visual_bridge.R --test\n")
    quit(status = 1)
  }
  
  if (args[1] == "--test") {
    test_visual_analysis()
  } else {
    results <- analyze_rmd_visual(args[1])
    cat(jsonlite::toJSON(results, pretty = TRUE, auto_unbox = TRUE))
  }
}
