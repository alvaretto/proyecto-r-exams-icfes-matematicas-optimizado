# ============================================================================
# AGENTE GRAFICADOR EXACTO - VERSIÓN MEJORADA
# Estrategia Robusta para Réplica Exacta - Proyecto ICFES R-exams
# ============================================================================

# Cargar módulos de apoyo
source("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/modulo_analisis_automatico_exacto.R")
source("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/modulo_validacion_cuantitativa.R")

#' Función principal del Agente Graficador Exacto
#' @param imagen_original Ruta a la imagen PNG original
#' @param umbral_exactitud Umbral mínimo para considerar exacto (default: 0.99)
#' @param max_iteraciones Máximo número de iteraciones de refinamiento (default: 10)
#' @return Lista con TikZ final y métricas de fidelidad
agente_graficador_exacto <- function(imagen_original, umbral_exactitud = 0.99, max_iteraciones = 10) {
  
  cat("🤖 INICIANDO AGENTE GRAFICADOR EXACTO\n")
  cat("=====================================\n")
  cat(sprintf("📁 Imagen: %s\n", basename(imagen_original)))
  cat(sprintf("🎯 Umbral exactitud: %.1f%%\n", umbral_exactitud * 100))
  cat(sprintf("🔄 Máx. iteraciones: %d\n\n", max_iteraciones))
  
  # FASE 1: ANÁLISIS AUTOMÁTICO EXHAUSTIVO
  cat("🔍 FASE 1: Análisis automático exhaustivo\n")
  caracteristicas <- analizar_imagen_matematica_exacta(imagen_original)
  
  # Determinar estrategia de replicación basada en análisis
  estrategia <- determinar_estrategia_replicacion(caracteristicas)
  cat(sprintf("📋 Estrategia seleccionada: %s\n\n", estrategia$nombre))
  
  # FASE 2: GENERACIÓN INICIAL INTELIGENTE
  cat("🎨 FASE 2: Generación inicial de TikZ\n")
  tikz_inicial <- generar_tikz_desde_caracteristicas_exactas(caracteristicas, estrategia)
  
  # FASE 3: REFINAMIENTO ITERATIVO AUTOMÁTICO
  cat("🔄 FASE 3: Refinamiento iterativo automático\n")
  resultado_refinamiento <- refinar_tikz_iterativamente(
    imagen_original, 
    tikz_inicial, 
    caracteristicas,
    umbral_exactitud, 
    max_iteraciones
  )
  
  # FASE 4: VALIDACIÓN FINAL Y REPORTE
  cat("✅ FASE 4: Validación final\n")
  validacion_final <- validar_fidelidad_exacta(
    imagen_original, 
    resultado_refinamiento$tikz_final, 
    umbral_exactitud
  )
  
  # Compilar resultado completo
  resultado_completo <- list(
    # Resultados principales
    tikz_final = resultado_refinamiento$tikz_final,
    fidelidad_alcanzada = validacion_final$fidelidad_total,
    exactitud_garantizada = validacion_final$aprobado,
    
    # Proceso de refinamiento
    iteraciones_usadas = resultado_refinamiento$iteraciones_usadas,
    historial_mejoras = resultado_refinamiento$historial_fidelidad,
    
    # Análisis detallado
    caracteristicas_extraidas = caracteristicas,
    estrategia_usada = estrategia,
    validacion_detallada = validacion_final,
    
    # Metadatos
    timestamp = Sys.time(),
    imagen_original = imagen_original,
    parametros = list(
      umbral_exactitud = umbral_exactitud,
      max_iteraciones = max_iteraciones
    )
  )
  
  # Generar reporte final
  generar_reporte_agente_graficador(resultado_completo)
  
  cat("\n🎯 AGENTE GRAFICADOR EXACTO COMPLETADO\n")
  cat("=====================================\n")
  cat(sprintf("✅ Fidelidad final: %.2f%%\n", validacion_final$fidelidad_total * 100))
  cat(sprintf("%s Estado: %s\n", 
              if(validacion_final$aprobado) "✅" else "⚠️",
              if(validacion_final$aprobado) "RÉPLICA EXACTA LOGRADA" else "REQUIERE AJUSTES MANUALES"))
  
  return(resultado_completo)
}

#' Determinar estrategia de replicación basada en características
determinar_estrategia_replicacion <- function(caracteristicas) {
  
  tipo_contenido <- caracteristicas$estructura$tipo_principal
  complejidad <- caracteristicas$estructura$complejidad
  
  if (tipo_contenido == "tabla") {
    estrategia <- list(
      nombre = "Replicación de Tabla Exacta",
      template_base = "tabla_datos_exacta",
      enfoque_colores = "extraccion_automatica",
      enfoque_coordenadas = "deteccion_bordes",
      prioridad_validacion = c("estructura", "contenido", "formato")
    )
  } else if (tipo_contenido == "grafica_barras") {
    estrategia <- list(
      nombre = "Replicación de Gráfica de Barras Exacta", 
      template_base = "grafica_barras_exacta",
      enfoque_colores = "paleta_dominante",
      enfoque_coordenadas = "deteccion_alturas",
      prioridad_validacion = c("proporciones", "colores", "etiquetas")
    )
  } else if (tipo_contenido == "grafica_circular") {
    estrategia <- list(
      nombre = "Replicación de Gráfica Circular Exacta",
      template_base = "grafica_circular_exacta", 
      enfoque_colores = "sectores_individuales",
      enfoque_coordenadas = "deteccion_angulos",
      prioridad_validacion = c("angulos", "colores", "leyenda")
    )
  } else {
    estrategia <- list(
      nombre = "Replicación Genérica Exacta",
      template_base = "template_generico_exacto",
      enfoque_colores = "analisis_completo",
      enfoque_coordenadas = "deteccion_general",
      prioridad_validacion = c("similitud_global", "elementos_clave")
    )
  }
  
  return(estrategia)
}

#' Generar TikZ inicial desde características extraídas
generar_tikz_desde_caracteristicas_exactas <- function(caracteristicas, estrategia) {
  
  cat("🎨 Generando TikZ desde características extraídas...\n")
  
  # Seleccionar template base según estrategia
  template_base <- cargar_template_exacto(estrategia$template_base)
  
  # Extraer parámetros específicos de las características
  parametros_tikz <- extraer_parametros_tikz_exactos(caracteristicas, estrategia)
  
  # Generar código TikZ parametrizado
  tikz_generado <- aplicar_parametros_a_template(template_base, parametros_tikz)
  
  # Optimizar código para compatibilidad R-exams
  tikz_optimizado <- optimizar_tikz_para_rexams(tikz_generado)
  
  cat("✅ TikZ inicial generado exitosamente\n")
  return(tikz_optimizado)
}

#' Refinar TikZ iterativamente hasta alcanzar exactitud
refinar_tikz_iterativamente <- function(imagen_original, tikz_inicial, caracteristicas, 
                                       umbral_exactitud, max_iteraciones) {
  
  cat("🔄 Iniciando refinamiento iterativo...\n")
  
  tikz_actual <- tikz_inicial
  historial_fidelidad <- numeric()
  iteracion <- 1
  
  while (iteracion <= max_iteraciones) {
    cat(sprintf("   Iteración %d/%d: ", iteracion, max_iteraciones))
    
    # Validar fidelidad actual
    validacion <- validar_fidelidad_exacta(imagen_original, tikz_actual, umbral_exactitud)
    fidelidad_actual <- validacion$fidelidad_total
    historial_fidelidad <- c(historial_fidelidad, fidelidad_actual)
    
    cat(sprintf("Fidelidad = %.2f%%\n", fidelidad_actual * 100))
    
    # Verificar si se alcanzó la exactitud requerida
    if (fidelidad_actual >= umbral_exactitud) {
      cat(sprintf("✅ Exactitud alcanzada en iteración %d\n", iteracion))
      break
    }
    
    # Identificar discrepancias específicas
    discrepancias <- identificar_discrepancias_especificas(validacion, caracteristicas)
    
    # Aplicar ajustes automáticos basados en discrepancias
    tikz_actual <- aplicar_ajustes_automaticos(tikz_actual, discrepancias, caracteristicas)
    
    iteracion <- iteracion + 1
  }
  
  if (iteracion > max_iteraciones) {
    cat("⚠️ Se alcanzó el máximo de iteraciones sin lograr exactitud completa\n")
  }
  
  return(list(
    tikz_final = tikz_actual,
    iteraciones_usadas = iteracion - 1,
    historial_fidelidad = historial_fidelidad,
    convergencia_exitosa = fidelidad_actual >= umbral_exactitud
  ))
}

#' Identificar discrepancias específicas para ajustes dirigidos
identificar_discrepancias_especificas <- function(validacion, caracteristicas) {
  
  discrepancias <- list()
  
  # Analizar métricas de validación para identificar problemas específicos
  if (validacion$metricas_detalladas$fidelidad_colores$fidelidad_promedio < 0.95) {
    discrepancias$colores <- list(
      tipo = "fidelidad_cromatica_baja",
      severidad = 1 - validacion$metricas_detalladas$fidelidad_colores$fidelidad_promedio,
      regiones_afectadas = which(validacion$metricas_detalladas$fidelidad_colores$diferencias_por_region > 0.1)
    )
  }
  
  if (validacion$metricas_detalladas$precision_geometrica$precision_proporciones < 0.95) {
    discrepancias$geometria <- list(
      tipo = "precision_geometrica_baja",
      severidad = 1 - validacion$metricas_detalladas$precision_geometrica$precision_proporciones,
      elementos_afectados = "proporciones_generales"
    )
  }
  
  if (validacion$metricas_detalladas$ssim$valor < 0.95) {
    discrepancias$estructura <- list(
      tipo = "similitud_estructural_baja", 
      severidad = 1 - validacion$metricas_detalladas$ssim$valor,
      aspecto = "estructura_general"
    )
  }
  
  return(discrepancias)
}

#' Aplicar ajustes automáticos basados en discrepancias identificadas
aplicar_ajustes_automaticos <- function(tikz_actual, discrepancias, caracteristicas) {
  
  tikz_ajustado <- tikz_actual
  
  # Ajustar colores si hay discrepancias cromáticas
  if (!is.null(discrepancias$colores)) {
    cat("   🎨 Ajustando colores...\n")
    tikz_ajustado <- ajustar_colores_tikz(tikz_ajustado, caracteristicas$colores_exactos)
  }
  
  # Ajustar geometría si hay discrepancias de proporciones
  if (!is.null(discrepancias$geometria)) {
    cat("   📐 Ajustando geometría...\n")
    tikz_ajustado <- ajustar_geometria_tikz(tikz_ajustado, caracteristicas$coordenadas)
  }
  
  # Ajustar estructura si hay discrepancias estructurales
  if (!is.null(discrepancias$estructura)) {
    cat("   🏗️ Ajustando estructura...\n")
    tikz_ajustado <- ajustar_estructura_tikz(tikz_ajustado, caracteristicas$geometria)
  }
  
  return(tikz_ajustado)
}

#' Generar reporte completo del Agente Graficador
generar_reporte_agente_graficador <- function(resultado) {
  
  reporte_path <- sprintf("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/REPORTE_AgentGraficador_%s.md", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  reporte_contenido <- sprintf("
# 🤖 REPORTE AGENTE GRAFICADOR EXACTO

## 📊 RESUMEN EJECUTIVO
- **Imagen procesada**: %s
- **Fidelidad alcanzada**: %.2f%%
- **Estado**: %s
- **Iteraciones usadas**: %d
- **Tiempo de procesamiento**: %s

## 🎯 RESULTADOS PRINCIPALES
- **Exactitud garantizada**: %s
- **Estrategia utilizada**: %s
- **Template base**: %s

## 📈 MÉTRICAS DETALLADAS
%s

## 🔄 HISTORIAL DE REFINAMIENTO
%s

## 🎨 CÓDIGO TIKZ FINAL
```latex
%s
```

## 📋 CARACTERÍSTICAS EXTRAÍDAS
- **Colores dominantes**: %d colores detectados
- **Tipo de contenido**: %s
- **Complejidad visual**: %s
- **Elementos geométricos**: %d elementos

---
*Reporte generado automáticamente por Agente Graficador Exacto*
*Timestamp: %s*
",
    basename(resultado$imagen_original),
    resultado$fidelidad_alcanzada * 100,
    if(resultado$exactitud_garantizada) "✅ EXACTITUD LOGRADA" else "⚠️ REQUIERE AJUSTES",
    resultado$iteraciones_usadas,
    format(resultado$timestamp, "%Y-%m-%d %H:%M:%S"),
    if(resultado$exactitud_garantizada) "SÍ" else "NO",
    resultado$estrategia_usada$nombre,
    resultado$estrategia_usada$template_base,
    resultado$validacion_detallada$reporte,
    paste(sprintf("Iteración %d: %.2f%%", seq_along(resultado$historial_mejoras), 
                  resultado$historial_mejoras * 100), collapse = "\n"),
    resultado$tikz_final,
    length(resultado$caracteristicas_extraidas$colores_exactos$colores_hex),
    resultado$caracteristicas_extraidas$estructura$tipo_principal,
    resultado$caracteristicas_extraidas$estructura$complejidad,
    length(resultado$caracteristicas_extraidas$geometria$lineas_horizontales),
    format(resultado$timestamp, "%Y-%m-%d %H:%M:%S")
  )
  
  writeLines(reporte_contenido, reporte_path)
  cat(sprintf("📄 Reporte guardado en: %s\n", reporte_path))
  
  return(reporte_path)
}

# ============================================================================
# FUNCIONES AUXILIARES ESPECIALIZADAS
# ============================================================================

# Templates exactos especializados compatibles con Qtikz/Ktikz
cargar_template_exacto <- function(nombre_template) {

  # Cargar templates desde archivos de referencia si existen
  template_path <- sprintf("Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/TikZ-Documentation/templates-rexams/robustos/%s.tikz",
                          gsub("_exacta", "-robusto", nombre_template))

  if (file.exists(template_path)) {
    return(paste(readLines(template_path), collapse = "\n"))
  }

  # Templates básicos compatibles con Qtikz/Ktikz
  templates <- list(
    tabla_datos_exacta = "
% Template compatible Qtikz/Ktikz - Tabla de Datos
\\begin{tikzpicture}
\\node[inner sep=0pt] {
  \\begin{tabular}{|c|c|}
    \\hline
    \\rowcolor{blue!20}
    \\textbf{%s} & \\textbf{%s} \\\\
    \\hline
    %s & %s \\\\
    \\hline
    %s & %s \\\\
    \\hline
    %s & %s \\\\
    \\hline
  \\end{tabular}
};
\\end{tikzpicture}",

    grafica_circular_exacta = "
% Template compatible Qtikz/Ktikz - Gráfico Circular
\\begin{tikzpicture}[scale=0.8]
\\coordinate (centro) at (0,0);

% Sectores del gráfico
\\fill[blue!60] (centro) -- (0:%s) arc (0:%s:%s) -- cycle;
\\fill[red!60] (centro) -- (%s:%s) arc (%s:%s:%s) -- cycle;
\\fill[green!60] (centro) -- (%s:%s) arc (%s:%s:%s) -- cycle;
\\fill[orange!60] (centro) -- (%s:%s) arc (%s:360:%s) -- cycle;

% Contornos
\\draw[black, thick] (centro) circle (%s);
\\draw[black] (centro) -- (0:%s);
\\draw[black] (centro) -- (%s:%s);
\\draw[black] (centro) -- (%s:%s);
\\draw[black] (centro) -- (%s:%s);

% Etiquetas de porcentajes
\\node[font=\\bfseries] at (%s:%s) {%s\\%%};
\\node[font=\\bfseries] at (%s:%s) {%s\\%%};
\\node[font=\\bfseries] at (%s:%s) {%s\\%%};
\\node[font=\\bfseries] at (%s:%s) {%s\\%%};
\\end{tikzpicture}",

    grafica_barras_exacta = "
% Template compatible Qtikz/Ktikz - Gráfico de Barras
\\begin{tikzpicture}[scale=0.8]
% Ejes
\\draw[->] (0,0) -- (0,%s) node[left] {%s};
\\draw[->] (0,0) -- (%s,0) node[below] {%s};

% Barras
\\fill[blue!60] (0.5,0) rectangle (1.5,%s);
\\fill[red!60] (2.0,0) rectangle (3.0,%s);
\\fill[green!60] (3.5,0) rectangle (4.5,%s);
\\fill[orange!60] (5.0,0) rectangle (6.0,%s);

% Etiquetas
\\node[below] at (1.0,-0.2) {%s};
\\node[below] at (2.5,-0.2) {%s};
\\node[below] at (4.0,-0.2) {%s};
\\node[below] at (5.5,-0.2) {%s};

% Valores
\\node[above] at (1.0,%s) {%s};
\\node[above] at (2.5,%s) {%s};
\\node[above] at (4.0,%s) {%s};
\\node[above] at (5.5,%s) {%s};
\\end{tikzpicture}",

    template_generico_exacto = "
% Template genérico compatible Qtikz/Ktikz
\\begin{tikzpicture}[scale=1]
%s
\\end{tikzpicture}"
  )

  return(templates[[nombre_template]] %||% templates$template_generico_exacto)
}

# Funciones auxiliares simplificadas (expandir en producción)
extraer_parametros_tikz_exactos <- function(caracteristicas, estrategia) {
  return(list(colores = caracteristicas$colores_exactos$paleta_tikz))
}

aplicar_parametros_a_template <- function(template, parametros) {
  return(sprintf(template, "contenido_placeholder"))
}

optimizar_tikz_para_rexams <- function(tikz) {
  return(tikz)
}

ajustar_colores_tikz <- function(tikz, colores) {
  return(tikz)
}

ajustar_geometria_tikz <- function(tikz, coordenadas) {
  return(tikz)
}

ajustar_estructura_tikz <- function(tikz, geometria) {
  return(tikz)
}

cat("✅ Agente Graficador Exacto cargado exitosamente\n")
