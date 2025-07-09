# ============================================================================
# SISTEMA INTEGRADO MEJORADO - TEMPLATES INTELIGENTES
# Integración Completa del Sistema de Templates Inteligentes con Mínima Intervención
# ============================================================================

# Cargar todos los módulos del sistema mejorado
source_if_exists <- function(file_path) {
  if (file.exists(file_path)) {
    tryCatch(source(file_path), error = function(e) {
      cat(sprintf("⚠️ Advertencia: No se pudo cargar %s\n", basename(file_path)))
    })
  }
}

# Cargar módulos del sistema mejorado
source_if_exists("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/02_detector_inteligente.R")
source_if_exists("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/06_funciones_auxiliares.R")
source_if_exists("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/03_templates_especializados.R")
source_if_exists("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/04_generador_coordenadas.R")
source_if_exists("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/05_optimizador_rexams.R")

#' FUNCIÓN PRINCIPAL MEJORADA: Sistema de Réplica con Templates Inteligentes
#' 
#' Esta función integra todo el sistema mejorado para generar réplicas exactas
#' con mínima intervención humana usando detección inteligente y templates optimizados.
#' 
#' @param imagen_path Ruta a la imagen PNG a replicar
#' @param umbral_exactitud Umbral mínimo de exactitud (default: 0.99)
#' @param usar_templates_inteligentes Si usar el nuevo sistema de templates (default: TRUE)
#' @param modo_debug Si mostrar información detallada del proceso (default: FALSE)
#' @return Lista con TikZ optimizado y métricas completas
sistema_replica_inteligente <- function(imagen_path, 
                                       umbral_exactitud = 0.99,
                                       usar_templates_inteligentes = TRUE,
                                       modo_debug = FALSE) {
  
  cat("🚀 INICIANDO SISTEMA DE RÉPLICA INTELIGENTE\n")
  cat("==========================================\n")
  
  inicio_tiempo <- Sys.time()
  
  # FASE 1: ANÁLISIS AUTOMÁTICO EXHAUSTIVO (usando sistema existente)
  cat("\n🔍 FASE 1: Análisis automático exhaustivo\n")
  caracteristicas <- analizar_imagen_matematica_exacta(imagen_path)
  
  if (usar_templates_inteligentes) {
    
    # FASE 2: DETECCIÓN INTELIGENTE DE TIPO DE GRÁFICO
    cat("\n🧠 FASE 2: Detección inteligente de tipo de gráfico\n")
    deteccion_resultado <- detectar_tipo_grafico_inteligente(caracteristicas)
    
    if (modo_debug) {
      cat(sprintf("   Tipo detectado: %s (confianza: %.2f%%)\n", 
                  deteccion_resultado$tipo_detectado, 
                  deteccion_resultado$confianza * 100))
      cat(sprintf("   Motor recomendado: %s\n", deteccion_resultado$motor_recomendado))
    }
    
    # FASE 3: GENERACIÓN AUTOMÁTICA DE COORDENADAS
    cat("\n📊 FASE 3: Generación automática de coordenadas\n")
    coordenadas_resultado <- generar_coordenadas_pgfplots_automatico(
      caracteristicas, 
      deteccion_resultado$tipo_detectado, 
      deteccion_resultado$parametros_especificos
    )
    
    # FASE 4: GENERACIÓN CON TEMPLATES INTELIGENTES
    cat("\n🎨 FASE 4: Generación con templates inteligentes\n")
    tikz_inicial <- generar_tikz_template_inteligente(
      deteccion_resultado$tipo_detectado,
      deteccion_resultado$motor_recomendado,
      deteccion_resultado$parametros_especificos,
      caracteristicas
    )
    
    # FASE 5: OPTIMIZACIÓN AVANZADA PARA R-EXAMS
    cat("\n🔧 FASE 5: Optimización avanzada para R-exams\n")
    optimizacion_resultado <- optimizar_tikz_rexams_avanzado(
      tikz_inicial,
      caracteristicas,
      deteccion_resultado$tipo_detectado
    )
    
    tikz_optimizado <- optimizacion_resultado$codigo_optimizado
    
  } else {
    # Usar sistema original como fallback
    cat("\n🔄 Usando sistema original como fallback\n")
    tikz_optimizado <- generar_tikz_desde_caracteristicas_exactas(caracteristicas, 
                                                                 list(template_base = "generico"))
  }
  
  # FASE 6: VALIDACIÓN FINAL
  cat("\n✅ FASE 6: Validación final\n")
  validacion_final <- validar_fidelidad_exacta(imagen_path, tikz_optimizado)
  
  fin_tiempo <- Sys.time()
  tiempo_total <- as.numeric(difftime(fin_tiempo, inicio_tiempo, units = "secs"))
  
  # Construir resultado completo
  resultado <- list(
    # Código TikZ final
    tikz_exacto = tikz_optimizado,
    
    # Métricas de calidad
    fidelidad_alcanzada = validacion_final$fidelidad_total,
    exactitud_garantizada = validacion_final$fidelidad_total >= umbral_exactitud,
    
    # Información del proceso
    proceso_replicacion = list(
      proceso_usado = if (usar_templates_inteligentes) "SISTEMA_INTELIGENTE" else "SISTEMA_ORIGINAL",
      tiempo_total = tiempo_total,
      tipo_detectado = if (usar_templates_inteligentes) deteccion_resultado$tipo_detectado else "no_detectado",
      motor_usado = if (usar_templates_inteligentes) deteccion_resultado$motor_recomendado else "tikz_generico",
      confianza_deteccion = if (usar_templates_inteligentes) deteccion_resultado$confianza else 0
    ),
    
    # Datos técnicos
    caracteristicas_extraidas = caracteristicas,
    validacion_detallada = validacion_final,
    
    # Información adicional del sistema inteligente
    deteccion_inteligente = if (usar_templates_inteligentes) deteccion_resultado else NULL,
    coordenadas_generadas = if (usar_templates_inteligentes) coordenadas_resultado else NULL,
    optimizaciones_aplicadas = if (usar_templates_inteligentes) optimizacion_resultado$reporte_optimizaciones else NULL,
    
    # Metadatos
    timestamp = Sys.time(),
    imagen_original = imagen_path,
    version_sistema = "SISTEMA_INTELIGENTE_v1.0"
  )
  
  # Generar reporte completo
  reporte_path <- generar_reporte_sistema_inteligente(resultado)
  resultado$reporte_generado = reporte_path
  
  cat("\n🎉 SISTEMA DE RÉPLICA INTELIGENTE COMPLETADO\n")
  cat("============================================\n")
  cat(sprintf("✅ Fidelidad alcanzada: %.2f%%\n", resultado$fidelidad_alcanzada * 100))
  cat(sprintf("⏱️ Tiempo total: %.1f segundos\n", tiempo_total))
  cat(sprintf("🧠 Tipo detectado: %s\n", resultado$proceso_replicacion$tipo_detectado))
  cat(sprintf("📄 Reporte: %s\n", reporte_path))
  
  return(resultado)
}

#' Función de conveniencia: Réplica rápida con sistema inteligente
replica_inteligente_rapida <- function(imagen_path) {
  
  cat("⚡ MODO RÁPIDO - Sistema Inteligente\n")
  
  sistema_replica_inteligente(
    imagen_path = imagen_path,
    umbral_exactitud = 0.95,  # Umbral más permisivo para velocidad
    usar_templates_inteligentes = TRUE,
    modo_debug = FALSE
  )
}

#' Función de conveniencia: Réplica de alta precisión
replica_inteligente_precision <- function(imagen_path) {
  
  cat("🎯 MODO PRECISIÓN - Sistema Inteligente\n")
  
  sistema_replica_inteligente(
    imagen_path = imagen_path,
    umbral_exactitud = 0.995,  # Umbral muy alto para máxima precisión
    usar_templates_inteligentes = TRUE,
    modo_debug = TRUE
  )
}

#' Función de comparación: Sistema inteligente vs original
comparar_sistemas <- function(imagen_path) {
  
  cat("📊 COMPARACIÓN DE SISTEMAS\n")
  cat("=========================\n")
  
  # Ejecutar sistema inteligente
  cat("\n🧠 Ejecutando sistema inteligente...\n")
  resultado_inteligente <- sistema_replica_inteligente(imagen_path, usar_templates_inteligentes = TRUE)
  
  # Ejecutar sistema original
  cat("\n🔄 Ejecutando sistema original...\n")
  resultado_original <- sistema_replica_inteligente(imagen_path, usar_templates_inteligentes = FALSE)
  
  # Generar comparación
  comparacion <- list(
    sistema_inteligente = list(
      fidelidad = resultado_inteligente$fidelidad_alcanzada,
      tiempo = resultado_inteligente$proceso_replicacion$tiempo_total,
      tipo_detectado = resultado_inteligente$proceso_replicacion$tipo_detectado
    ),
    sistema_original = list(
      fidelidad = resultado_original$fidelidad_alcanzada,
      tiempo = resultado_original$proceso_replicacion$tiempo_total,
      tipo_detectado = "generico"
    ),
    mejoras = list(
      mejora_fidelidad = resultado_inteligente$fidelidad_alcanzada - resultado_original$fidelidad_alcanzada,
      mejora_tiempo = resultado_original$proceso_replicacion$tiempo_total - resultado_inteligente$proceso_replicacion$tiempo_total,
      deteccion_automatica = !is.null(resultado_inteligente$deteccion_inteligente)
    )
  )
  
  # Mostrar resultados
  cat("\n📈 RESULTADOS DE COMPARACIÓN\n")
  cat("============================\n")
  cat(sprintf("Sistema Inteligente - Fidelidad: %.2f%%, Tiempo: %.1fs\n", 
              comparacion$sistema_inteligente$fidelidad * 100,
              comparacion$sistema_inteligente$tiempo))
  cat(sprintf("Sistema Original    - Fidelidad: %.2f%%, Tiempo: %.1fs\n", 
              comparacion$sistema_original$fidelidad * 100,
              comparacion$sistema_original$tiempo))
  cat(sprintf("Mejora en Fidelidad: %+.2f%%\n", comparacion$mejoras$mejora_fidelidad * 100))
  cat(sprintf("Mejora en Tiempo: %+.1fs\n", comparacion$mejoras$mejora_tiempo))
  
  return(comparacion)
}

#' Generar reporte del sistema inteligente
generar_reporte_sistema_inteligente <- function(resultado) {
  
  reporte_path <- sprintf("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/REPORTE_SistemaInteligente_%s.md", 
                         format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  contenido_reporte <- sprintf("
# 🧠 REPORTE SISTEMA INTELIGENTE DE RÉPLICAS

## 📊 RESUMEN EJECUTIVO
- **Imagen:** %s
- **Fidelidad:** %.2f%%
- **Estado:** %s
- **Tiempo:** %.1f segundos
- **Tipo Detectado:** %s
- **Motor Usado:** %s
- **Confianza Detección:** %.2f%%

## 🎯 DETECCIÓN INTELIGENTE
%s

## 📈 OPTIMIZACIONES APLICADAS
%s

## 🔧 COORDENADAS GENERADAS
%s

## 📋 CÓDIGO TIKZ RESULTANTE
```latex
%s
```

## ✅ VALIDACIÓN FINAL
%s

---
*Generado por Sistema Inteligente v1.0 - %s*
",
    basename(resultado$imagen_original),
    resultado$fidelidad_alcanzada * 100,
    if(resultado$exactitud_garantizada) "✅ EXACTITUD LOGRADA" else "⚠️ REQUIERE AJUSTES",
    resultado$proceso_replicacion$tiempo_total,
    resultado$proceso_replicacion$tipo_detectado,
    resultado$proceso_replicacion$motor_usado,
    resultado$proceso_replicacion$confianza_deteccion * 100,
    if(!is.null(resultado$deteccion_inteligente)) resultado$deteccion_inteligente$justificacion else "No disponible",
    if(!is.null(resultado$optimizaciones_aplicadas)) paste(resultado$optimizaciones_aplicadas$optimizaciones_aplicadas, collapse = "\n- ") else "No disponible",
    if(!is.null(resultado$coordenadas_generadas)) sprintf("Puntos generados: %d", resultado$coordenadas_generadas$metadatos$num_puntos) else "No disponible",
    resultado$tikz_exacto,
    if(!is.null(resultado$validacion_detallada)) sprintf("SSIM: %.3f, Fidelidad Total: %.3f", resultado$validacion_detallada$ssim, resultado$validacion_detallada$fidelidad_total) else "No disponible",
    Sys.time()
  )
  
  writeLines(contenido_reporte, reporte_path)
  cat(sprintf("📄 Reporte generado: %s\n", reporte_path))
  
  return(reporte_path)
}

cat("✅ Sistema Integrado Mejorado cargado exitosamente\n")
cat("🚀 Funciones principales disponibles:\n")
cat("   - sistema_replica_inteligente() - Sistema completo mejorado\n")
cat("   - replica_inteligente_rapida() - Modo rápido\n")
cat("   - replica_inteligente_precision() - Modo alta precisión\n")
cat("   - comparar_sistemas() - Comparación inteligente vs original\n")
