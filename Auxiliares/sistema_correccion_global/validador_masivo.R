# ===============================================================================
# VALIDADOR MASIVO DE ARCHIVOS .RMD
# ===============================================================================
# Archivo: validador_masivo.R
# Propósito: Validar y reportar errores en múltiples archivos .Rmd
# Uso: source("Auxiliares/sistema_correccion_global/validador_masivo.R")
# ===============================================================================

# Cargar el sistema de corrección
source("Auxiliares/sistema_correccion_global/funciones_correccion.R")

#' Validar un archivo .Rmd individual
#' @param ruta_archivo Ruta al archivo .Rmd
#' @return Lista con errores encontrados
validar_archivo_individual <- function(ruta_archivo) {
  
  if (!file.exists(ruta_archivo)) {
    return(list(error = "Archivo no encontrado"))
  }
  
  contenido <- readLines(ruta_archivo, warn = FALSE, encoding = "UTF-8")
  contenido_texto <- paste(contenido, collapse = " ")
  
  errores <- list()
  
  # 1. Errores de concordancia de género
  errores_concordancia <- c()
  
  # Buscar patrones específicos de error
  patrones_error <- c(
    "familias matriculados", "familias registrados", "familias certificados",
    "familias beneficiarios", "familias asegurados", "familias acreditados",
    "familias becados", "familias autorizados", "familias habilitados",
    "empresas matriculados", "empresas registrados", "empresas certificados", 
    "empresas beneficiarios", "empresas asegurados", "empresas acreditados",
    "empresas becados", "empresas autorizados", "empresas habilitados"
  )
  
  for (patron in patrones_error) {
    if (str_detect(contenido_texto, fixed(patron))) {
      errores_concordancia <- c(errores_concordancia, patron)
    }
  }
  
  if (length(errores_concordancia) > 0) {
    errores$concordancia <- errores_concordancia
  }
  
  # 2. Verificar si tiene el sistema de corrección cargado
  tiene_sistema <- any(str_detect(contenido, "sistema_correccion_global"))
  if (!tiene_sistema) {
    errores$sin_sistema <- "No tiene el sistema de corrección cargado"
  }
  
  # 3. Verificar estructura de metadatos ICFES
  tiene_metadatos <- any(str_detect(contenido, "icfes:"))
  if (!tiene_metadatos) {
    errores$sin_metadatos <- "No tiene metadatos ICFES"
  }
  
  # 4. Verificar pruebas de diversidad
  tiene_pruebas_diversidad <- any(str_detect(contenido, "version_diversity_test|Prueba de diversidad"))
  if (!tiene_pruebas_diversidad) {
    errores$sin_pruebas_diversidad <- "No tiene pruebas de diversidad de versiones"
  }
  
  # 5. Verificar librerías esenciales
  librerias_esenciales <- c("exams", "stringr", "testthat")
  librerias_faltantes <- c()
  
  for (lib in librerias_esenciales) {
    patron_lib <- paste0("library\\(", lib, "\\)|require\\(", lib, "\\)")
    if (!any(str_detect(contenido, patron_lib))) {
      librerias_faltantes <- c(librerias_faltantes, lib)
    }
  }
  
  if (length(librerias_faltantes) > 0) {
    errores$librerias_faltantes <- librerias_faltantes
  }
  
  return(errores)
}

#' Generar reporte de validación masiva
#' @param directorio Directorio a validar
#' @param recursivo Si validar subdirectorios
#' @param generar_archivo Si generar archivo de reporte
#' @return Lista con resultados de validación
generar_reporte_validacion <- function(directorio = ".", 
                                      recursivo = TRUE, 
                                      generar_archivo = TRUE) {
  
  cat("🔍 Iniciando validación masiva en:", directorio, "\n")
  
  # Encontrar archivos .Rmd
  archivos <- list.files(directorio, 
                        pattern = "\\.Rmd$", 
                        recursive = recursivo, 
                        full.names = TRUE)
  
  if (length(archivos) == 0) {
    cat("❌ No se encontraron archivos .Rmd\n")
    return(list())
  }
  
  cat("📁 Validando", length(archivos), "archivos .Rmd\n\n")
  
  # Validar cada archivo
  resultados <- list()
  archivos_con_errores <- 0
  total_errores <- 0
  
  for (archivo in archivos) {
    cat("🔍 Validando:", basename(archivo), "...")
    
    errores <- validar_archivo_individual(archivo)
    
    if (length(errores) > 0) {
      resultados[[archivo]] <- errores
      archivos_con_errores <- archivos_con_errores + 1
      total_errores <- total_errores + length(unlist(errores))
      cat(" ❌", length(unlist(errores)), "errores\n")
    } else {
      cat(" ✅ OK\n")
    }
  }
  
  # Generar resumen
  cat("\n" %+% rep("=", 60) %+% "\n")
  cat("📊 RESUMEN DE VALIDACIÓN:\n")
  cat("   Archivos validados:", length(archivos), "\n")
  cat("   Archivos con errores:", archivos_con_errores, "\n")
  cat("   Archivos sin errores:", length(archivos) - archivos_con_errores, "\n")
  cat("   Total de errores:", total_errores, "\n")
  
  # Mostrar errores detallados
  if (length(resultados) > 0) {
    cat("\n📋 ERRORES DETALLADOS:\n")
    
    for (archivo in names(resultados)) {
      cat("\n❌", archivo, ":\n")
      errores <- resultados[[archivo]]
      
      if ("concordancia" %in% names(errores)) {
        cat("   🔤 Errores de concordancia:", paste(errores$concordancia, collapse = ", "), "\n")
      }
      if ("sin_sistema" %in% names(errores)) {
        cat("   📦 Sin sistema de corrección\n")
      }
      if ("sin_metadatos" %in% names(errores)) {
        cat("   📋 Sin metadatos ICFES\n")
      }
      if ("sin_pruebas_diversidad" %in% names(errores)) {
        cat("   🧪 Sin pruebas de diversidad\n")
      }
      if ("librerias_faltantes" %in% names(errores)) {
        cat("   📚 Librerías faltantes:", paste(errores$librerias_faltantes, collapse = ", "), "\n")
      }
    }
  }
  
  # Generar archivo de reporte si se solicita
  if (generar_archivo && length(resultados) > 0) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    archivo_reporte <- paste0("reporte_validacion_", timestamp, ".txt")
    
    # Escribir reporte
    sink(archivo_reporte)
    cat("REPORTE DE VALIDACIÓN MASIVA\n")
    cat("Fecha:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Directorio:", directorio, "\n")
    cat("Archivos validados:", length(archivos), "\n")
    cat("Archivos con errores:", archivos_con_errores, "\n")
    cat("Total de errores:", total_errores, "\n\n")
    
    for (archivo in names(resultados)) {
      cat("ARCHIVO:", archivo, "\n")
      errores <- resultados[[archivo]]
      
      for (tipo_error in names(errores)) {
        cat("  ", tipo_error, ":", paste(errores[[tipo_error]], collapse = ", "), "\n")
      }
      cat("\n")
    }
    sink()
    
    cat("📄 Reporte guardado en:", archivo_reporte, "\n")
  }
  
  cat("✅ Validación completada\n")
  
  return(resultados)
}

#' Generar estadísticas de calidad del proyecto
#' @param directorio Directorio a analizar
#' @return Lista con estadísticas
generar_estadisticas_calidad <- function(directorio = ".") {
  
  archivos <- list.files(directorio, 
                        pattern = "\\.Rmd$", 
                        recursive = TRUE, 
                        full.names = TRUE)
  
  if (length(archivos) == 0) {
    return(list(total_archivos = 0))
  }
  
  # Contadores
  con_sistema_correccion <- 0
  con_metadatos_icfes <- 0
  con_pruebas_diversidad <- 0
  con_errores_concordancia <- 0
  
  for (archivo in archivos) {
    contenido <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
    contenido_texto <- paste(contenido, collapse = " ")
    
    if (any(str_detect(contenido, "sistema_correccion_global"))) {
      con_sistema_correccion <- con_sistema_correccion + 1
    }
    
    if (any(str_detect(contenido, "icfes:"))) {
      con_metadatos_icfes <- con_metadatos_icfes + 1
    }
    
    if (any(str_detect(contenido, "version_diversity_test|Prueba de diversidad"))) {
      con_pruebas_diversidad <- con_pruebas_diversidad + 1
    }
    
    if (str_detect(contenido_texto, "familias matriculados|empresas registrados|empresas certificados")) {
      con_errores_concordancia <- con_errores_concordancia + 1
    }
  }
  
  estadisticas <- list(
    total_archivos = length(archivos),
    con_sistema_correccion = con_sistema_correccion,
    con_metadatos_icfes = con_metadatos_icfes,
    con_pruebas_diversidad = con_pruebas_diversidad,
    con_errores_concordancia = con_errores_concordancia,
    porcentaje_con_sistema = round((con_sistema_correccion / length(archivos)) * 100, 1),
    porcentaje_con_metadatos = round((con_metadatos_icfes / length(archivos)) * 100, 1),
    porcentaje_con_pruebas = round((con_pruebas_diversidad / length(archivos)) * 100, 1),
    porcentaje_con_errores = round((con_errores_concordancia / length(archivos)) * 100, 1)
  )
  
  # Mostrar estadísticas
  cat("📊 ESTADÍSTICAS DE CALIDAD DEL PROYECTO\n")
  cat("" %+% rep("=", 40) %+% "\n")
  cat("Total de archivos .Rmd:", estadisticas$total_archivos, "\n")
  cat("Con sistema de corrección:", estadisticas$con_sistema_correccion, 
      "(", estadisticas$porcentaje_con_sistema, "%)\n")
  cat("Con metadatos ICFES:", estadisticas$con_metadatos_icfes, 
      "(", estadisticas$porcentaje_con_metadatos, "%)\n")
  cat("Con pruebas de diversidad:", estadisticas$con_pruebas_diversidad, 
      "(", estadisticas$porcentaje_con_pruebas, "%)\n")
  cat("Con errores de concordancia:", estadisticas$con_errores_concordancia, 
      "(", estadisticas$porcentaje_con_errores, "%)\n")
  
  return(estadisticas)
}

# Operador de concatenación
`%+%` <- function(a, b) paste0(a, b)

cat("🔍 Validador Masivo cargado\n")
cat("📋 Funciones disponibles:\n")
cat("   - validar_archivo_individual(ruta_archivo)\n")
cat("   - generar_reporte_validacion(directorio)\n")
cat("   - generar_estadisticas_calidad(directorio)\n")
cat("💡 Ejemplo de uso:\n")
cat("   generar_reporte_validacion('Lab/')\n")
cat("   generar_estadisticas_calidad('.')\n\n")
