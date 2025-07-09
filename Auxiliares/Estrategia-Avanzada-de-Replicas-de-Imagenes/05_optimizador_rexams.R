# ============================================================================
# OPTIMIZADOR R-EXAMS MEJORADO
# Sistema Avanzado de Compatibilidad y Rendimiento para R-exams
# ============================================================================

#' FUNCIÓN PRINCIPAL: Optimizar código TikZ para R-exams
#' 
#' Esta función aplica optimizaciones avanzadas para garantizar compatibilidad
#' total con R-exams, reducir errores de compilación y mejorar rendimiento.
#' 
#' @param codigo_tikz Código TikZ generado por los templates inteligentes
#' @param caracteristicas Características originales de la imagen
#' @param tipo_grafico Tipo de gráfico detectado
#' @param configuracion_rexams Configuración específica de R-exams
#' @return Código TikZ optimizado para R-exams
optimizar_tikz_rexams_avanzado <- function(codigo_tikz, caracteristicas, tipo_grafico, 
                                          configuracion_rexams = NULL) {
  
  cat("🔧 Iniciando optimización avanzada para R-exams...\n")
  
  # Configuración por defecto si no se proporciona
  if (is.null(configuracion_rexams)) {
    configuracion_rexams <- obtener_configuracion_rexams_default()
  }
  
  # FASE 1: Optimizaciones de compatibilidad
  codigo_compatible <- aplicar_optimizaciones_compatibilidad(codigo_tikz, configuracion_rexams)
  
  # FASE 2: Optimizaciones de rendimiento
  codigo_optimizado <- aplicar_optimizaciones_rendimiento(codigo_compatible, tipo_grafico)
  
  # FASE 3: Optimizaciones específicas de R-exams
  codigo_rexams <- aplicar_optimizaciones_rexams_especificas(codigo_optimizado, caracteristicas)
  
  # FASE 4: Validación y corrección automática
  codigo_validado <- validar_y_corregir_automaticamente(codigo_rexams, configuracion_rexams)
  
  # FASE 5: Optimización final de interpolación R
  codigo_final <- optimizar_interpolacion_r_avanzada(codigo_validado, caracteristicas)
  
  cat("✅ Optimización R-exams completada exitosamente\n")
  
  # Generar reporte de optimizaciones aplicadas
  reporte_optimizaciones <- generar_reporte_optimizaciones(codigo_tikz, codigo_final)
  
  list(
    codigo_optimizado = codigo_final,
    reporte_optimizaciones = reporte_optimizaciones,
    configuracion_usada = configuracion_rexams,
    validacion_final = validar_compatibilidad_final(codigo_final)
  )
}

# ============================================================================
# CONFIGURACIÓN R-EXAMS
# ============================================================================

#' Obtener configuración R-exams por defecto
obtener_configuracion_rexams_default <- function() {
  
  list(
    # Configuración LaTeX
    latex_engine = "pdflatex",
    latex_packages = c(
      "\\usepackage{tikz}",
      "\\usepackage{pgfplots}",
      "\\usepackage{xcolor}",
      "\\usepackage{amsmath}",
      "\\usepackage{amssymb}",
      "\\usepackage{array}",
      "\\usepackage{colortbl}"
    ),
    
    # Configuración PGFPlots
    pgfplots_compat = "1.18",
    pgfplots_backend = "auto",
    
    # Configuración de rendimiento
    max_compilation_time = 30,  # segundos
    enable_externalization = TRUE,
    optimize_for_size = TRUE,
    
    # Configuración de formatos de salida
    output_formats = c("html", "pdf", "docx"),
    image_format = "png",
    image_dpi = 150,
    
    # Configuración de compatibilidad
    strict_mode = TRUE,
    fallback_enabled = TRUE,
    error_recovery = TRUE
  )
}

# ============================================================================
# OPTIMIZACIONES DE COMPATIBILIDAD
# ============================================================================

#' Aplicar optimizaciones de compatibilidad
aplicar_optimizaciones_compatibilidad <- function(codigo_tikz, configuracion) {
  
  cat("   🔄 Aplicando optimizaciones de compatibilidad...\n")
  
  # 1. Normalizar bibliotecas TikZ
  codigo_tikz <- normalizar_bibliotecas_tikz(codigo_tikz, configuracion)
  
  # 2. Convertir colores a formato estándar
  codigo_tikz <- convertir_colores_estandar_avanzado(codigo_tikz)
  
  # 3. Optimizar comandos TikZ para compatibilidad
  codigo_tikz <- optimizar_comandos_tikz_compatibilidad(codigo_tikz)
  
  # 4. Asegurar compatibilidad PGFPlots
  codigo_tikz <- asegurar_compatibilidad_pgfplots(codigo_tikz, configuracion)
  
  # 5. Corregir problemas comunes de sintaxis
  codigo_tikz <- corregir_sintaxis_comunes(codigo_tikz)
  
  codigo_tikz
}

#' Normalizar bibliotecas TikZ
normalizar_bibliotecas_tikz <- function(codigo_tikz, configuracion) {
  
  # Remover declaraciones de bibliotecas del código (se manejan en el preámbulo)
  codigo_tikz <- gsub("\\\\usetikzlibrary\\{[^}]*\\}", "", codigo_tikz)
  codigo_tikz <- gsub("\\\\usepackage\\{[^}]*\\}", "", codigo_tikz)
  
  # Asegurar que el código esté dentro de tikzpicture
  if (!grepl("\\\\begin\\{tikzpicture\\}", codigo_tikz)) {
    codigo_tikz <- paste0("\\begin{tikzpicture}\n", codigo_tikz, "\n\\end{tikzpicture}")
  }
  
  codigo_tikz
}

#' Convertir colores a formato estándar avanzado
convertir_colores_estandar_avanzado <- function(codigo_tikz) {
  
  # Mapeo de colores problemáticos a estándar
  mapeo_colores <- list(
    "blue!60" = "blue!60!white",
    "red!60" = "red!60!white",
    "green!60" = "green!60!white",
    "orange!60" = "orange!60!white",
    "purple!60" = "purple!60!white",
    "yellow!60" = "yellow!60!white"
  )
  
  for (color_orig in names(mapeo_colores)) {
    color_nuevo <- mapeo_colores[[color_orig]]
    codigo_tikz <- gsub(color_orig, color_nuevo, codigo_tikz, fixed = TRUE)
  }
  
  # Convertir definiciones RGB a formato estándar
  codigo_tikz <- gsub("\\\\definecolor\\{([^}]+)\\}\\{RGB\\}\\{([^}]+)\\}", 
                     "\\\\definecolor{\\1}{rgb}{\\2}", codigo_tikz)
  
  codigo_tikz
}

#' Optimizar comandos TikZ para compatibilidad
optimizar_comandos_tikz_compatibilidad <- function(codigo_tikz) {
  
  # Reemplazar comandos problemáticos
  reemplazos <- list(
    "\\\\filldraw" = "\\\\fill",  # Simplificar comandos complejos
    "\\\\shadedraw" = "\\\\fill",
    "\\\\pattern" = "\\\\fill"
  )
  
  for (comando_orig in names(reemplazos)) {
    comando_nuevo <- reemplazos[[comando_orig]]
    codigo_tikz <- gsub(comando_orig, comando_nuevo, codigo_tikz)
  }
  
  # Simplificar opciones complejas
  codigo_tikz <- simplificar_opciones_tikz(codigo_tikz)
  
  codigo_tikz
}

#' Asegurar compatibilidad PGFPlots
asegurar_compatibilidad_pgfplots <- function(codigo_tikz, configuracion) {
  
  # Agregar declaración de compatibilidad si hay código PGFPlots
  if (grepl("\\\\begin\\{axis\\}", codigo_tikz)) {
    
    # Verificar si ya tiene declaración de compatibilidad
    if (!grepl("\\\\pgfplotsset", codigo_tikz)) {
      compat_declaration <- sprintf("\\pgfplotsset{compat=%s}\n", configuracion$pgfplots_compat)
      codigo_tikz <- paste0(compat_declaration, codigo_tikz)
    }
    
    # Optimizar configuración de ejes
    codigo_tikz <- optimizar_configuracion_ejes_pgfplots(codigo_tikz)
  }
  
  codigo_tikz
}

# ============================================================================
# OPTIMIZACIONES DE RENDIMIENTO
# ============================================================================

#' Aplicar optimizaciones de rendimiento
aplicar_optimizaciones_rendimiento <- function(codigo_tikz, tipo_grafico) {
  
  cat("   ⚡ Aplicando optimizaciones de rendimiento...\n")
  
  # 1. Optimizar según tipo de gráfico
  codigo_tikz <- optimizar_por_tipo_grafico(codigo_tikz, tipo_grafico)
  
  # 2. Reducir complejidad innecesaria
  codigo_tikz <- reducir_complejidad_codigo(codigo_tikz)
  
  # 3. Optimizar cálculos matemáticos
  codigo_tikz <- optimizar_calculos_matematicos(codigo_tikz)
  
  # 4. Aplicar técnicas de cache
  codigo_tikz <- aplicar_tecnicas_cache(codigo_tikz)
  
  codigo_tikz
}

#' Optimizar por tipo de gráfico
optimizar_por_tipo_grafico <- function(codigo_tikz, tipo_grafico) {
  
  switch(tipo_grafico,
    "circular" = optimizar_grafico_circular(codigo_tikz),
    "barras" = optimizar_grafico_barras(codigo_tikz),
    "lineas" = optimizar_grafico_lineas(codigo_tikz),
    "funcion_matematica" = optimizar_funcion_matematica(codigo_tikz),
    codigo_tikz  # default: sin optimización específica
  )
}

#' Optimizar gráfico circular
optimizar_grafico_circular <- function(codigo_tikz) {
  
  # Usar arcos en lugar de muchos puntos para círculos
  codigo_tikz <- gsub("\\\\draw\\[([^\\]]+)\\] \\(([^)]+)\\) circle \\(([^)]+)\\);",
                     "\\\\draw[\\1] (\\2) circle (\\3);", codigo_tikz)
  
  # Optimizar sectores circulares
  codigo_tikz <- optimizar_sectores_circulares(codigo_tikz)
  
  codigo_tikz
}

#' Optimizar gráfico de barras
optimizar_grafico_barras <- function(codigo_tikz) {
  
  # Usar rectangles en lugar de paths complejos
  codigo_tikz <- gsub("\\\\path\\[([^\\]]+)\\] \\(([^)]+)\\) rectangle \\(([^)]+)\\);",
                     "\\\\fill[\\1] (\\2) rectangle (\\3);", codigo_tikz)
  
  codigo_tikz
}

cat("✅ Optimizador R-exams mejorado cargado exitosamente\n")
cat("🔧 Optimizaciones disponibles:\n")
cat("   - Compatibilidad avanzada con R-exams\n")
cat("   - Optimizaciones de rendimiento por tipo de gráfico\n")
cat("   - Validación y corrección automática\n")
cat("   - Preparación para interpolación R\n")
