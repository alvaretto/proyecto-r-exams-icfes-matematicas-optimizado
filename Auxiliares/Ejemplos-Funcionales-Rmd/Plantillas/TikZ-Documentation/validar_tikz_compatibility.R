#!/usr/bin/env Rscript
# =============================================================================
# SCRIPT DE VALIDACIÓN DE COMPATIBILIDAD TikZ CON R-EXAMS
# =============================================================================
# Propósito: Automatizar la validación de código TikZ para múltiples formatos
# Uso: source("Auxiliares/TikZ-Documentation/validar_tikz_compatibility.R")
# =============================================================================

library(exams)
library(stringr)
library(knitr)

# =============================================================================
# FUNCIÓN PRINCIPAL DE VALIDACIÓN
# =============================================================================

validar_tikz_compatibility <- function(archivo_rmd, 
                                     formatos = c("pdf", "html", "moodle"),
                                     n_pruebas = 3,
                                     directorio_salida = "tikz_validation_output") {
  
  cat("🔍 INICIANDO VALIDACIÓN DE COMPATIBILIDAD TikZ\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("📁 Archivo:", archivo_rmd, "\n")
  cat("📅 Fecha:", Sys.time(), "\n")
  cat("🎯 Formatos:", paste(formatos, collapse = ", "), "\n\n")
  
  # Crear directorio de salida
  if (!dir.exists(directorio_salida)) {
    dir.create(directorio_salida, recursive = TRUE)
  }
  
  # Verificar existencia del archivo
  if (!file.exists(archivo_rmd)) {
    stop("❌ ERROR: Archivo no encontrado: ", archivo_rmd)
  }
  
  # Inicializar resultados
  resultados <- list(
    archivo = archivo_rmd,
    fecha = Sys.time(),
    formatos_probados = formatos,
    resultados_por_formato = list(),
    analisis_codigo = list(),
    recomendaciones = list()
  )
  
  # ==========================================================================
  # ANÁLISIS PREVIO DEL CÓDIGO TikZ
  # ==========================================================================
  
  cat("📋 FASE 1: Análisis del Código TikZ\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  contenido <- readLines(archivo_rmd, warn = FALSE)
  codigo_completo <- paste(contenido, collapse = "\n")
  
  # Detectar bloques TikZ
  bloques_tikz <- str_extract_all(codigo_completo, 
                                  "\\\\begin\\{tikzpicture\\}.*?\\\\end\\{tikzpicture\\}")[[1]]
  
  if (length(bloques_tikz) == 0) {
    cat("⚠️ No se encontraron bloques TikZ en el archivo\n")
    return(resultados)
  }
  
  cat("✅ Encontrados", length(bloques_tikz), "bloques TikZ\n")
  
  # Analizar cada bloque TikZ
  for (i in seq_along(bloques_tikz)) {
    cat("🔍 Analizando bloque TikZ", i, "...\n")
    analisis <- analizar_codigo_tikz(bloques_tikz[i])
    resultados$analisis_codigo[[paste0("bloque_", i)]] <- analisis
    
    if (length(analisis$problemas) > 0) {
      cat("⚠️ Problemas detectados en bloque", i, ":\n")
      for (problema in analisis$problemas) {
        cat("  -", problema, "\n")
      }
    } else {
      cat("✅ Bloque", i, "parece compatible\n")
    }
  }
  
  # ==========================================================================
  # PRUEBAS DE GENERACIÓN POR FORMATO
  # ==========================================================================
  
  cat("\n🧪 FASE 2: Pruebas de Generación por Formato\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  for (formato in formatos) {
    cat("🎯 Probando formato:", formato, "\n")
    
    resultado_formato <- tryCatch({
      tiempo_inicio <- Sys.time()
      
      if (formato == "pdf") {
        exams2pdf(archivo_rmd, 
                  n = n_pruebas, 
                  name = paste0("tikz_test_pdf_", basename(archivo_rmd)),
                  dir = directorio_salida,
                  quiet = TRUE)
        
      } else if (formato == "html") {
        exams2html(archivo_rmd, 
                   n = n_pruebas, 
                   name = paste0("tikz_test_html_", basename(archivo_rmd)),
                   dir = directorio_salida,
                   quiet = TRUE)
        
      } else if (formato == "moodle") {
        exams2moodle(archivo_rmd, 
                     n = n_pruebas, 
                     name = paste0("tikz_test_moodle_", basename(archivo_rmd)),
                     dir = directorio_salida,
                     quiet = TRUE)
        
      } else if (formato == "pandoc") {
        exams2pandoc(archivo_rmd, 
                     n = n_pruebas, 
                     name = paste0("tikz_test_pandoc_", basename(archivo_rmd)),
                     dir = directorio_salida,
                     quiet = TRUE)
      }
      
      tiempo_fin <- Sys.time()
      tiempo_transcurrido <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))
      
      list(
        estado = "✅ ÉXITO",
        tiempo = tiempo_transcurrido,
        archivos_generados = n_pruebas,
        mensaje = paste("Generación exitosa en", round(tiempo_transcurrido, 2), "segundos")
      )
      
    }, error = function(e) {
      list(
        estado = "❌ ERROR",
        tiempo = NA,
        archivos_generados = 0,
        mensaje = as.character(e$message),
        error_completo = e
      )
    })
    
    resultados$resultados_por_formato[[formato]] <- resultado_formato
    cat("  ", resultado_formato$estado, "-", resultado_formato$mensaje, "\n")
  }
  
  # ==========================================================================
  # GENERACIÓN DE RECOMENDACIONES
  # ==========================================================================
  
  cat("\n💡 FASE 3: Generación de Recomendaciones\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  recomendaciones <- generar_recomendaciones(resultados)
  resultados$recomendaciones <- recomendaciones
  
  for (rec in recomendaciones) {
    cat("💡", rec, "\n")
  }
  
  # ==========================================================================
  # REPORTE FINAL
  # ==========================================================================
  
  cat("\n📊 RESUMEN FINAL\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  formatos_exitosos <- sum(sapply(resultados$resultados_por_formato, 
                                  function(x) x$estado == "✅ ÉXITO"))
  formatos_fallidos <- length(formatos) - formatos_exitosos
  
  cat("✅ Formatos exitosos:", formatos_exitosos, "/", length(formatos), "\n")
  cat("❌ Formatos fallidos:", formatos_fallidos, "/", length(formatos), "\n")
  
  if (formatos_exitosos == length(formatos)) {
    cat("🎉 COMPATIBILIDAD COMPLETA: El archivo es compatible con todos los formatos\n")
  } else if (formatos_exitosos > 0) {
    cat("⚠️ COMPATIBILIDAD PARCIAL: Requiere adaptaciones para algunos formatos\n")
  } else {
    cat("❌ INCOMPATIBLE: Requiere revisión completa del código TikZ\n")
  }
  
  # Guardar resultados
  archivo_resultados <- file.path(directorio_salida, 
                                  paste0("validation_report_", 
                                         gsub("\\.Rmd$", "", basename(archivo_rmd)), 
                                         "_", format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                         ".rds"))
  saveRDS(resultados, archivo_resultados)
  cat("📁 Resultados guardados en:", archivo_resultados, "\n")
  
  return(resultados)
}

# =============================================================================
# FUNCIÓN DE ANÁLISIS DE CÓDIGO TikZ
# =============================================================================

analizar_codigo_tikz <- function(codigo_tikz) {
  problemas <- character(0)
  advertencias <- character(0)
  
  # Detectar colores personalizados
  if (str_detect(codigo_tikz, "\\\\definecolor")) {
    problemas <- c(problemas, "Colores personalizados con \\definecolor (usar colores estándar)")
  }
  
  # Detectar bibliotecas problemáticas
  bibliotecas_problematicas <- c("shadows", "fadings", "blur", "decorations\\.text")
  for (lib in bibliotecas_problematicas) {
    if (str_detect(codigo_tikz, paste0("\\\\usetikzlibrary\\{.*", lib))) {
      problemas <- c(problemas, paste("Biblioteca problemática:", lib))
    }
  }
  
  # Detectar \pgfmathsetmacro
  if (str_detect(codigo_tikz, "\\\\pgfmathsetmacro")) {
    advertencias <- c(advertencias, "Usar variables R en lugar de \\pgfmathsetmacro")
  }
  
  # Detectar fuentes personalizadas
  if (str_detect(codigo_tikz, "\\\\fontsize|\\\\fontfamily")) {
    problemas <- c(problemas, "Fuentes personalizadas (usar fuentes estándar)")
  }
  
  # Detectar efectos avanzados
  if (str_detect(codigo_tikz, "opacity|transparency|blend")) {
    advertencias <- c(advertencias, "Efectos de transparencia (verificar compatibilidad)")
  }
  
  return(list(
    problemas = problemas,
    advertencias = advertencias,
    score_compatibilidad = max(0, 100 - length(problemas) * 20 - length(advertencias) * 10)
  ))
}

# =============================================================================
# FUNCIÓN DE GENERACIÓN DE RECOMENDACIONES
# =============================================================================

generar_recomendaciones <- function(resultados) {
  recomendaciones <- character(0)
  
  # Analizar resultados por formato
  formatos_fallidos <- names(resultados$resultados_por_formato)[
    sapply(resultados$resultados_por_formato, function(x) x$estado == "❌ ERROR")
  ]
  
  if (length(formatos_fallidos) > 0) {
    recomendaciones <- c(recomendaciones, 
                        paste("Revisar compatibilidad para formatos:", 
                              paste(formatos_fallidos, collapse = ", ")))
  }
  
  # Analizar problemas de código
  todos_problemas <- unlist(lapply(resultados$analisis_codigo, function(x) x$problemas))
  if (length(todos_problemas) > 0) {
    problemas_unicos <- unique(todos_problemas)
    for (problema in problemas_unicos) {
      recomendaciones <- c(recomendaciones, paste("Corregir:", problema))
    }
  }
  
  # Recomendaciones generales
  if (length(recomendaciones) == 0) {
    recomendaciones <- c("✅ El código TikZ es compatible con R-exams")
  } else {
    recomendaciones <- c(recomendaciones, 
                        "Consultar Auxiliares/TikZ-Documentation/referencias/compatibilidad.md")
  }
  
  return(recomendaciones)
}

# =============================================================================
# FUNCIÓN DE VALIDACIÓN MASIVA
# =============================================================================

validar_directorio_tikz <- function(directorio, patron = "\\.Rmd$") {
  archivos <- list.files(directorio, pattern = patron, full.names = TRUE, recursive = TRUE)
  
  cat("🔍 VALIDACIÓN MASIVA DE DIRECTORIO\n")
  cat("📁 Directorio:", directorio, "\n")
  cat("📄 Archivos encontrados:", length(archivos), "\n\n")
  
  resultados_masivos <- list()
  
  for (archivo in archivos) {
    cat("🎯 Procesando:", basename(archivo), "\n")
    resultado <- validar_tikz_compatibility(archivo, 
                                           formatos = c("pdf", "html"),
                                           n_pruebas = 1)
    resultados_masivos[[basename(archivo)]] <- resultado
  }
  
  return(resultados_masivos)
}

# =============================================================================
# EJEMPLO DE USO
# =============================================================================

if (FALSE) {
  # Validar un archivo específico
  resultado <- validar_tikz_compatibility(
    "Lab/36/volumen_cilindro_hueco_py_v1.Rmd",
    formatos = c("pdf", "html", "moodle"),
    n_pruebas = 2
  )
  
  # Validar todo el directorio Lab
  resultados_lab <- validar_directorio_tikz("Lab/")
  
  # Ver resultados
  print(resultado$recomendaciones)
}

cat("✅ Script de validación TikZ cargado exitosamente\n")
cat("💡 Uso: validar_tikz_compatibility('archivo.Rmd')\n")
cat("📚 Documentación: Auxiliares/TikZ-Documentation/\n")