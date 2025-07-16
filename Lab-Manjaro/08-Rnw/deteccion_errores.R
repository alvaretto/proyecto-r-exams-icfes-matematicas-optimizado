# Script para detección automática de errores recurrentes
# Basado en la metodología de corrección de errores ICFES R-exams

detectar_errores_comunes <- function(archivo_rnw) {
  cat("=== DETECCIÓN AUTOMÁTICA DE ERRORES ===\n")
  cat("Archivo:", archivo_rnw, "\n\n")
  
  # Leer contenido del archivo
  contenido <- readLines(archivo_rnw, encoding = "UTF-8")
  contenido_completo <- paste(contenido, collapse = "\n")
  
  errores_encontrados <- list()
  
  # A) ERRORES GRAMATICALES/CONCORDANCIA
  cat("A) VERIFICANDO ERRORES GRAMATICALES/CONCORDANCIA...\n")
  
  # Buscar "La conteo" en lugar de "El conteo"
  if(grepl("La conteo", contenido_completo)) {
    errores_encontrados$gramaticales <- c(errores_encontrados$gramaticales, 
                                         "Error: 'La conteo' debe ser 'El conteo'")
  }
  
  # Buscar concordancia de género en variables dinámicas
  if(grepl("la cantidad.*él", contenido_completo, ignore.case = TRUE)) {
    errores_encontrados$gramaticales <- c(errores_encontrados$gramaticales,
                                         "Posible error de concordancia de género")
  }
  
  if(length(errores_encontrados$gramaticales) == 0) {
    cat("  ✅ No se encontraron errores gramaticales\n")
  } else {
    cat("  ❌ Errores encontrados:\n")
    for(error in errores_encontrados$gramaticales) {
      cat("    -", error, "\n")
    }
  }
  
  # B) ERRORES DE POSICIONAMIENTO TIKZ
  cat("\nB) VERIFICANDO POSICIONAMIENTO TIKZ...\n")
  
  # Verificar orden: texto → tabla → pregunta
  lineas_tikz <- grep("include_tikz", contenido)
  lineas_question <- grep("\\\\begin\\{question\\}", contenido)
  
  if(length(lineas_tikz) > 0 && length(lineas_question) > 0) {
    if(any(lineas_tikz < lineas_question[1])) {
      errores_encontrados$tikz_posicion <- "Posible error: TikZ antes de \\begin{question}"
    }
  }
  
  if(length(errores_encontrados$tikz_posicion) == 0) {
    cat("  ✅ Posicionamiento TikZ correcto\n")
  } else {
    cat("  ❌ Error encontrado:", errores_encontrados$tikz_posicion, "\n")
  }
  
  # C) ERRORES DE GENERACIÓN DE DATOS
  cat("\nC) VERIFICANDO GENERACIÓN DE DATOS...\n")
  
  # Verificar que existe función generar_datos
  if(!grepl("generar_datos.*function", contenido_completo)) {
    errores_encontrados$generacion_datos <- "Error: función generar_datos() no encontrada"
  }
  
  # Verificar que existe verificación de unicidad
  if(!grepl("unique.*versiones", contenido_completo)) {
    errores_encontrados$generacion_datos <- c(errores_encontrados$generacion_datos,
                                             "Advertencia: no se encontró verificación de unicidad")
  }
  
  if(length(errores_encontrados$generacion_datos) == 0) {
    cat("  ✅ Generación de datos correcta\n")
  } else {
    cat("  ❌ Errores encontrados:\n")
    for(error in errores_encontrados$generacion_datos) {
      cat("    -", error, "\n")
    }
  }
  
  # D) ERRORES DE COMPILACIÓN LATEX/TIKZ
  cat("\nD) VERIFICANDO CONFIGURACIÓN LATEX/TIKZ...\n")
  
  # Verificar paquetes esenciales
  paquetes_requeridos <- c("tikz", "colortbl", "xcolor")
  paquetes_faltantes <- c()
  
  for(paquete in paquetes_requeridos) {
    if(!grepl(paste0("usepackage.*", paquete), contenido_completo)) {
      paquetes_faltantes <- c(paquetes_faltantes, paquete)
    }
  }
  
  if(length(paquetes_faltantes) > 0) {
    errores_encontrados$latex_config <- paste("Paquetes faltantes:", paste(paquetes_faltantes, collapse = ", "))
  }
  
  # Verificar configuración include_tikz
  if(grepl("include_tikz", contenido_completo)) {
    if(!grepl("packages.*=.*c\\(", contenido_completo)) {
      errores_encontrados$latex_config <- c(errores_encontrados$latex_config,
                                           "Advertencia: include_tikz sin especificar packages")
    }
  }
  
  if(length(errores_encontrados$latex_config) == 0) {
    cat("  ✅ Configuración LaTeX/TikZ correcta\n")
  } else {
    cat("  ❌ Errores encontrados:\n")
    for(error in errores_encontrados$latex_config) {
      cat("    -", error, "\n")
    }
  }
  
  # E) ERRORES DE ESTRUCTURA R-EXAMS
  cat("\nE) VERIFICANDO ESTRUCTURA R-EXAMS...\n")
  
  # Verificar meta-información
  metadatos_requeridos <- c("\\\\exname", "\\\\extype", "\\\\exsolution")
  metadatos_faltantes <- c()
  
  for(metadato in metadatos_requeridos) {
    if(!grepl(metadato, contenido_completo)) {
      metadatos_faltantes <- c(metadatos_faltantes, metadato)
    }
  }
  
  if(length(metadatos_faltantes) > 0) {
    errores_encontrados$estructura <- paste("Metadatos faltantes:", paste(metadatos_faltantes, collapse = ", "))
  }
  
  # Verificar estructura básica
  if(!grepl("\\\\begin\\{question\\}", contenido_completo)) {
    errores_encontrados$estructura <- c(errores_encontrados$estructura, "Falta \\begin{question}")
  }
  
  if(!grepl("\\\\begin\\{solution\\}", contenido_completo)) {
    errores_encontrados$estructura <- c(errores_encontrados$estructura, "Falta \\begin{solution}")
  }
  
  if(length(errores_encontrados$estructura) == 0) {
    cat("  ✅ Estructura R-exams correcta\n")
  } else {
    cat("  ❌ Errores encontrados:\n")
    for(error in errores_encontrados$estructura) {
      cat("    -", error, "\n")
    }
  }
  
  # RESUMEN FINAL
  cat("\n=== RESUMEN DE DETECCIÓN ===\n")

  # Contar errores de forma segura
  total_errores <- 0
  if(length(errores_encontrados) > 0) {
    total_errores <- sum(sapply(errores_encontrados, function(x) if(is.null(x)) 0 else length(x)))
  }

  if(total_errores == 0) {
    cat("✅ NO SE ENCONTRARON ERRORES CRÍTICOS\n")
    cat("El archivo está listo para compilación.\n")
  } else {
    cat("❌ SE ENCONTRARON", total_errores, "ERRORES/ADVERTENCIAS\n")
    cat("Revisar y corregir antes de la compilación final.\n")
  }
  
  return(errores_encontrados)
}

# Ejecutar detección
archivo <- "Ahorro_opciones_porcentaje_interpretacion_representacion_n2_v1.Rnw"
errores <- detectar_errores_comunes(archivo)
