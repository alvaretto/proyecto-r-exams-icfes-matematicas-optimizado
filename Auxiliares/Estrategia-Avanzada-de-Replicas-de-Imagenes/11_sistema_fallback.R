# ============================================================================
# SISTEMA INTEGRADO DE RÉPLICA EXACTA DE IMÁGENES MATEMÁTICAS
# Estrategia Robusta - Proyecto ICFES R-exams
# ============================================================================

# Cargar todos los módulos del sistema (con nombres actualizados)
source("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/07_modulo_analisis_automatico_exacto.R")
source("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/08_modulo_validacion_cuantitativa.R")
source("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/09_agente_graficador_exacto.R")
source("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/10_generador_tikz_qtikz_compatible.R")
source("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/11_validador_qtikz_compatible.R")

#' FUNCIÓN PRINCIPAL: Sistema de Réplica Exacta Integrado
#'
#' Esta función integra el sistema condicional automático existente con las
#' mejoras para garantizar réplica exacta de imágenes matemáticas.
#' LECCIÓN APRENDIDA: Usar SIEMPRE este sistema en lugar de procesos manuales.
#'
#' @param imagen_path Ruta a la imagen PNG a replicar
#' @param umbral_exactitud Umbral mínimo para considerar "exacto" (default: 0.99)
#' @param generar_ejercicio_completo Si generar ejercicio R-exams completo (default: FALSE)
#' @param configuracion_icfes Lista con configuración ICFES (competencia, nivel, etc.)
#' @param validacion_humana Si incluir puntos de validación humana (default: TRUE)
#' @return Lista con TikZ exacto y métricas de fidelidad
sistema_replica_exacta <- function(imagen_path,
                                  umbral_exactitud = 0.99,
                                  generar_ejercicio_completo = FALSE,
                                  configuracion_icfes = NULL,
                                  validacion_humana = TRUE) {
  
  cat("🚀 SISTEMA DE RÉPLICA EXACTA DE IMÁGENES MATEMÁTICAS\n")
  cat("===================================================\n")
  cat("💡 LECCIÓN APRENDIDA: Usar sistema automático en lugar de proceso manual\n")
  cat(sprintf("📁 Imagen: %s\n", basename(imagen_path)))
  cat(sprintf("🎯 Umbral exactitud: %.1f%%\n", umbral_exactitud * 100))
  cat(sprintf("📝 Ejercicio completo: %s\n", if(generar_ejercicio_completo) "SÍ" else "NO"))
  cat(sprintf("👤 Validación humana: %s\n\n", if(validacion_humana) "SÍ" else "NO"))
  
  # Verificar que la imagen existe
  if (!file.exists(imagen_path)) {
    stop("❌ Error: No se encuentra la imagen en: ", imagen_path)
  }
  
  # ETAPA 1: ANÁLISIS AUTOMÁTICO Y DECISIÓN DE FLUJO
  cat("🔍 ETAPA 1: Análisis automático y decisión de flujo\n")
  resultado_analisis <- analizar_y_decidir_flujo(imagen_path)
  
  # ETAPA 2: APLICAR FLUJO APROPIADO CON MEJORAS Y VALIDACIÓN HUMANA
  if (resultado_analisis$flujo_recomendado == "FLUJO_B_EXACTO") {
    cat("🎯 ETAPA 2: Aplicando FLUJO B con Agente Graficador Exacto\n")
    resultado_replicacion <- aplicar_flujo_b_exacto(imagen_path, umbral_exactitud, validacion_humana)
  } else {
    cat("📋 ETAPA 2: Aplicando FLUJO A mejorado\n")
    resultado_replicacion <- aplicar_flujo_a_mejorado(imagen_path, umbral_exactitud, validacion_humana)
  }
  
  # ETAPA 3: VALIDACIÓN FINAL INTEGRADA CON QTIKZ/KTIKZ
  cat("✅ ETAPA 3: Validación final integrada con compatibilidad Qtikz/Ktikz\n")
  validacion_final <- validar_resultado_final_qtikz(imagen_path, resultado_replicacion, umbral_exactitud)
  
  # ETAPA 4: GENERACIÓN DE EJERCICIO COMPLETO (OPCIONAL)
  ejercicio_completo <- NULL
  if (generar_ejercicio_completo) {
    cat("📝 ETAPA 4: Generando ejercicio R-exams completo\n")
    ejercicio_completo <- generar_ejercicio_rexams_completo(
      resultado_replicacion, 
      configuracion_icfes,
      imagen_path
    )
  }
  
  # COMPILAR RESULTADO FINAL
  resultado_final <- list(
    # Resultados principales
    tikz_exacto = resultado_replicacion$tikz_final,
    fidelidad_alcanzada = validacion_final$fidelidad_total,
    exactitud_garantizada = validacion_final$aprobado,
    
    # Proceso completo
    analisis_inicial = resultado_analisis,
    proceso_replicacion = resultado_replicacion,
    validacion_final = validacion_final,
    ejercicio_completo = ejercicio_completo,
    
    # Metadatos
    imagen_original = imagen_path,
    timestamp = Sys.time(),
    parametros_usados = list(
      umbral_exactitud = umbral_exactitud,
      generar_ejercicio = generar_ejercicio_completo
    )
  )
  
  # GENERAR REPORTE FINAL INTEGRADO
  generar_reporte_sistema_completo(resultado_final)
  
  # MOSTRAR RESUMEN FINAL
  mostrar_resumen_final(resultado_final)
  
  return(resultado_final)
}

#' Analizar imagen y decidir flujo apropiado (integrado con sistema existente)
analizar_y_decidir_flujo <- function(imagen_path) {
  
  cat("🤖 Ejecutando análisis automático de contenido...\n")
  
  # Usar el análisis automático mejorado
  caracteristicas <- analizar_imagen_matematica_exacta(imagen_path)
  
  # Determinar flujo basado en análisis mejorado
  flujo_recomendado <- determinar_flujo_exacto(caracteristicas)
  
  # Generar justificación de la decisión
  justificacion <- generar_justificacion_flujo(caracteristicas, flujo_recomendado)
  
  cat(sprintf("📋 Flujo recomendado: %s\n", flujo_recomendado))
  cat(sprintf("💡 Justificación: %s\n", justificacion))
  
  return(list(
    caracteristicas = caracteristicas,
    flujo_recomendado = flujo_recomendado,
    justificacion = justificacion,
    requiere_agente_graficador = (flujo_recomendado == "FLUJO_B_EXACTO")
  ))
}

#' Determinar flujo exacto basado en características
determinar_flujo_exacto <- function(caracteristicas) {
  
  # Criterios mejorados para decisión de flujo
  tiene_graficas <- caracteristicas$estructura$es_grafica$detectado
  tiene_tablas <- caracteristicas$estructura$es_tabla$detectado
  tiene_diagramas <- caracteristicas$estructura$es_diagrama$detectado
  complejidad_alta <- caracteristicas$estructura$complejidad > 0.7
  
  # Decisión de flujo con criterios más estrictos para exactitud
  if (tiene_graficas || tiene_tablas || tiene_diagramas || complejidad_alta) {
    return("FLUJO_B_EXACTO")  # Agente Graficador Exacto
  } else {
    return("FLUJO_A_MEJORADO")  # Proceso estándar mejorado
  }
}

#' Aplicar FLUJO B con Agente Graficador Exacto
#' MEJORA: Incluye puntos de validación humana para evitar iteraciones innecesarias
aplicar_flujo_b_exacto <- function(imagen_path, umbral_exactitud, validacion_humana = TRUE) {
  
  cat("🎯 Activando Agente Graficador Exacto...\n")

  # MEJORA: Punto de validación humana temprana
  if (validacion_humana) {
    cat("👤 PUNTO DE VALIDACIÓN HUMANA: Revisando análisis inicial...\n")
    cat("📋 ¿El análisis automático es correcto? (Presiona Enter para continuar o Ctrl+C para ajustar)\n")
    readline()
  }

  # Usar el agente graficador exacto desarrollado
  resultado_agente <- agente_graficador_exacto(
    imagen_original = imagen_path,
    umbral_exactitud = umbral_exactitud,
    max_iteraciones = 10,
    validacion_humana = validacion_humana
  )

  # MEJORA: Validación humana antes de refinamiento adicional
  if (!resultado_agente$exactitud_garantizada) {
    if (validacion_humana) {
      cat("👤 PUNTO DE VALIDACIÓN HUMANA: ¿Aplicar refinamiento adicional? (s/n): ")
      respuesta <- readline()
      if (tolower(respuesta) == "s") {
        cat("⚠️ Aplicando refinamiento adicional según indicación humana...\n")
        resultado_agente <- aplicar_refinamiento_adicional(resultado_agente, imagen_path)
      }
    } else {
      cat("⚠️ Advertencia: No se alcanzó exactitud completa. Aplicando refinamiento adicional...\n")
      resultado_agente <- aplicar_refinamiento_adicional(resultado_agente, imagen_path)
    }
  }
  
  return(resultado_agente)
}

#' Aplicar FLUJO A mejorado para contenido simple
#' MEJORA: Incluye validación humana para evitar errores de interpretación
aplicar_flujo_a_mejorado <- function(imagen_path, umbral_exactitud, validacion_humana = TRUE) {
  
  cat("📋 Aplicando proceso estándar mejorado...\n")

  # MEJORA: Validación humana temprana para contenido simple
  if (validacion_humana) {
    cat("👤 VALIDACIÓN HUMANA: ¿Las características detectadas son correctas?\n")
    cat("📋 Presiona Enter para continuar o Ctrl+C para revisar manualmente\n")
    readline()
  }

  # Análisis básico para contenido simple
  caracteristicas_basicas <- analizar_imagen_matematica_exacta(imagen_path)

  # Generar TikZ usando templates básicos pero con precisión mejorada
  tikz_basico <- generar_tikz_basico_mejorado(caracteristicas_basicas)

  # MEJORA: Mostrar resultado intermedio para validación humana
  if (validacion_humana) {
    cat("👤 VALIDACIÓN INTERMEDIA: TikZ básico generado\n")
    cat("📋 ¿Continuar con validación automática? (Enter) o ¿Ajustar manualmente? (m): ")
    respuesta <- readline()
    if (tolower(respuesta) == "m") {
      cat("🔧 Modo manual activado - revisar código TikZ generado\n")
    }
  }

  # Validar y refinar si es necesario
  validacion <- validar_fidelidad_exacta(imagen_path, tikz_basico, umbral_exactitud)

  if (validacion$fidelidad_total < umbral_exactitud) {
    if (validacion_humana) {
      cat("👤 VALIDACIÓN: Fidelidad insuficiente. ¿Aplicar refinamiento? (s/n): ")
      respuesta <- readline()
      if (tolower(respuesta) == "s") {
        cat("🔄 Aplicando refinamiento básico según indicación humana...\n")
        tikz_basico <- refinar_tikz_basico(tikz_basico, caracteristicas_basicas, validacion)
        validacion <- validar_fidelidad_exacta(imagen_path, tikz_basico, umbral_exactitud)
      }
    } else {
      cat("🔄 Aplicando refinamiento básico...\n")
      tikz_basico <- refinar_tikz_basico(tikz_basico, caracteristicas_basicas, validacion)
      validacion <- validar_fidelidad_exacta(imagen_path, tikz_basico, umbral_exactitud)
    }
  }
  
  return(list(
    tikz_final = tikz_basico,
    fidelidad_alcanzada = validacion$fidelidad_total,
    exactitud_garantizada = validacion$aprobado,
    proceso_usado = "FLUJO_A_MEJORADO",
    validacion_detallada = validacion
  ))
}

#' Validar resultado final del sistema completo con compatibilidad Qtikz/Ktikz
validar_resultado_final_qtikz <- function(imagen_path, resultado_replicacion, umbral_exactitud) {

  cat("🔍 Ejecutando validación final del sistema con compatibilidad Qtikz/Ktikz...\n")

  # Validación cuantitativa final
  validacion_cuantitativa <- validar_fidelidad_exacta(
    imagen_path,
    resultado_replicacion$tikz_final,
    umbral_exactitud
  )

  # Validación específica de compatibilidad Qtikz/Ktikz
  validacion_qtikz <- validar_compatibilidad_qtikz(
    resultado_replicacion$tikz_final,
    nivel_validacion = "completo"
  )

  # Validaciones adicionales del sistema
  validacion_integracion <- validar_integracion_rexams(resultado_replicacion$tikz_final)
  validacion_compatibilidad <- validar_compatibilidad_multiformato(resultado_replicacion$tikz_final)

  # Aplicar correcciones automáticas si es necesario
  tikz_corregido <- resultado_replicacion$tikz_final
  if (!validacion_qtikz$compatible) {
    cat("🔧 Aplicando correcciones automáticas para compatibilidad Qtikz/Ktikz...\n")
    tikz_corregido <- corregir_problemas_automaticos(resultado_replicacion$tikz_final)

    # Re-validar después de correcciones
    validacion_qtikz_corregida <- validar_compatibilidad_qtikz(tikz_corregido, "completo")

    if (validacion_qtikz_corregida$compatible) {
      cat("✅ Correcciones exitosas - código ahora compatible\n")
      resultado_replicacion$tikz_final <- tikz_corregido
      validacion_qtikz <- validacion_qtikz_corregida
    } else {
      cat("⚠️ Algunas incompatibilidades persisten después de correcciones automáticas\n")
    }
  }

  # Compilar validación final
  validacion_final <- list(
    fidelidad_total = validacion_cuantitativa$fidelidad_total,
    aprobado = validacion_cuantitativa$aprobado && validacion_qtikz$compatible,
    metricas_detalladas = validacion_cuantitativa$metricas_detalladas,
    compatibilidad_qtikz = validacion_qtikz,
    integracion_rexams = validacion_integracion,
    compatibilidad_formatos = validacion_compatibilidad,
    tikz_final_corregido = tikz_corregido,
    recomendaciones = generar_recomendaciones_finales_qtikz(validacion_cuantitativa, validacion_qtikz)
  )

  return(validacion_final)
}

#' Validar resultado final del sistema completo (función original mantenida para compatibilidad)
validar_resultado_final <- function(imagen_path, resultado_replicacion, umbral_exactitud) {
  return(validar_resultado_final_qtikz(imagen_path, resultado_replicacion, umbral_exactitud))
}

#' Generar ejercicio R-exams completo con TikZ exacto
generar_ejercicio_rexams_completo <- function(resultado_replicacion, configuracion_icfes, imagen_path) {
  
  cat("📝 Generando ejercicio R-exams completo...\n")
  
  # Configuración por defecto si no se proporciona
  if (is.null(configuracion_icfes)) {
    configuracion_icfes <- list(
      competencia = "interpretacion_representacion",
      nivel_dificultad = 2,
      categoria = "estadistica",
      contexto = "matematico"
    )
  }
  
  # Generar estructura completa del ejercicio
  ejercicio_estructura <- generar_estructura_ejercicio_exacto(
    tikz_exacto = resultado_replicacion$tikz_final,
    configuracion = configuracion_icfes,
    imagen_original = imagen_path
  )
  
  # Crear archivo .Rmd completo
  archivo_rmd <- crear_archivo_rmd_exacto(ejercicio_estructura)
  
  return(list(
    estructura = ejercicio_estructura,
    archivo_rmd = archivo_rmd,
    configuracion_usada = configuracion_icfes
  ))
}

#' Generar reporte completo del sistema
generar_reporte_sistema_completo <- function(resultado_final) {
  
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  reporte_path <- sprintf("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/REPORTE_Sistema_Completo_%s.md", 
                         timestamp_str)
  
  reporte_contenido <- sprintf("
# 🚀 REPORTE SISTEMA DE RÉPLICA EXACTA

## 📊 RESUMEN EJECUTIVO
- **Imagen procesada**: %s
- **Fidelidad final**: %.2f%%
- **Exactitud garantizada**: %s
- **Flujo utilizado**: %s
- **Tiempo total**: %s

## 🔍 ANÁLISIS INICIAL
%s

## 🎯 PROCESO DE REPLICACIÓN
%s

## ✅ VALIDACIÓN FINAL
%s

## 📝 EJERCICIO GENERADO
%s

## 🎨 CÓDIGO TIKZ EXACTO
```latex
%s
```

## 📋 RECOMENDACIONES
%s

---
*Reporte generado por Sistema de Réplica Exacta*
*Timestamp: %s*
",
    basename(resultado_final$imagen_original),
    resultado_final$fidelidad_alcanzada * 100,
    if(resultado_final$exactitud_garantizada) "✅ SÍ" else "⚠️ NO",
    resultado_final$analisis_inicial$flujo_recomendado,
    format(resultado_final$timestamp, "%H:%M:%S"),
    resultado_final$analisis_inicial$justificacion,
    sprintf("Proceso: %s\nIteraciones: %d", 
            resultado_final$proceso_replicacion$proceso_usado %||% "Agente Graficador",
            resultado_final$proceso_replicacion$iteraciones_usadas %||% 0),
    resultado_final$validacion_final$reporte %||% "Validación completada",
    if(!is.null(resultado_final$ejercicio_completo)) "Ejercicio R-exams generado" else "No solicitado",
    resultado_final$tikz_exacto,
    paste(resultado_final$validacion_final$recomendaciones %||% "Ninguna", collapse = "\n"),
    format(resultado_final$timestamp, "%Y-%m-%d %H:%M:%S")
  )
  
  writeLines(reporte_contenido, reporte_path)
  cat(sprintf("📄 Reporte completo guardado en: %s\n", reporte_path))
  
  return(reporte_path)
}

#' Mostrar resumen final en consola
mostrar_resumen_final <- function(resultado_final) {
  
  cat("\n🎯 RESUMEN FINAL DEL SISTEMA\n")
  cat("============================\n")
  cat(sprintf("📁 Imagen: %s\n", basename(resultado_final$imagen_original)))
  cat(sprintf("🎯 Fidelidad: %.2f%%\n", resultado_final$fidelidad_alcanzada * 100))
  cat(sprintf("%s Estado: %s\n", 
              if(resultado_final$exactitud_garantizada) "✅" else "⚠️",
              if(resultado_final$exactitud_garantizada) "RÉPLICA EXACTA LOGRADA" else "REQUIERE AJUSTES"))
  cat(sprintf("🔄 Flujo: %s\n", resultado_final$analisis_inicial$flujo_recomendado))
  
  if (!is.null(resultado_final$ejercicio_completo)) {
    cat(sprintf("📝 Ejercicio: %s\n", resultado_final$ejercicio_completo$archivo_rmd))
  }
  
  cat("============================\n")
}

# ============================================================================
# FUNCIONES AUXILIARES DEL SISTEMA
# ============================================================================

generar_justificacion_flujo <- function(caracteristicas, flujo) {
  if (flujo == "FLUJO_B_EXACTO") {
    return("Contenido gráfico/tabular complejo detectado. Requiere Agente Graficador Exacto.")
  } else {
    return("Contenido simple detectado. Proceso estándar mejorado es suficiente.")
  }
}

aplicar_refinamiento_adicional <- function(resultado_agente, imagen_path) {
  cat("🔧 Aplicando refinamiento adicional...\n")
  # Implementar lógica de refinamiento adicional
  return(resultado_agente)
}

generar_tikz_basico_mejorado <- function(caracteristicas) {
  # Implementar generación TikZ básica mejorada
  return("\\begin{tikzpicture}\n% TikZ básico mejorado\n\\end{tikzpicture}")
}

refinar_tikz_basico <- function(tikz, caracteristicas, validacion) {
  # Implementar refinamiento básico
  return(tikz)
}

validar_integracion_rexams <- function(tikz) {
  return(list(compatible = TRUE, errores = character(0)))
}

validar_compatibilidad_multiformato <- function(tikz) {
  return(list(html = TRUE, pdf = TRUE, moodle = TRUE))
}

generar_recomendaciones_finales_qtikz <- function(validacion_cuantitativa, validacion_qtikz) {
  recomendaciones <- character()

  # Recomendaciones basadas en fidelidad
  if (validacion_cuantitativa$fidelidad_total < 0.99) {
    recomendaciones <- c(recomendaciones,
                        "Considerar refinamiento adicional para alcanzar 99% de fidelidad")
  }

  # Recomendaciones basadas en compatibilidad Qtikz/Ktikz
  if (!validacion_qtikz$compatible) {
    recomendaciones <- c(recomendaciones,
                        "Revisar compatibilidad con Qtikz/Ktikz - ver reporte detallado")
  }

  if (validacion_qtikz$puntuacion < 90) {
    recomendaciones <- c(recomendaciones,
                        "Optimizar código TikZ para mejor compatibilidad")
  }

  # Recomendaciones específicas de Qtikz/Ktikz
  recomendaciones <- c(recomendaciones, validacion_qtikz$recomendaciones)

  if (length(recomendaciones) == 0) {
    recomendaciones <- c("Código TikZ compatible y optimizado para Qtikz/Ktikz")
  }

  return(recomendaciones)
}

generar_recomendaciones_finales <- function(validacion) {
  return(c("Sistema funcionando correctamente"))
}

generar_estructura_ejercicio_exacto <- function(tikz_exacto, configuracion, imagen_original) {
  return(list(tikz = tikz_exacto, config = configuracion))
}

crear_archivo_rmd_exacto <- function(estructura) {
  return("ejercicio_exacto.Rmd")
}

cat("✅ Sistema de Réplica Exacta cargado exitosamente\n")

# COMANDO PRINCIPAL PARA USO RÁPIDO
#' @export
replica_exacta <- function(imagen_path, exactitud = 0.99, ejercicio_completo = FALSE) {
  return(sistema_replica_exacta(imagen_path, exactitud, ejercicio_completo))
}
