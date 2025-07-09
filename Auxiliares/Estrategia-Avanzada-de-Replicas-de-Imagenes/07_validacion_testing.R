# ============================================================================
# SISTEMA DE VALIDACIÓN Y TESTING
# Suite Completa de Pruebas para el Sistema Inteligente de Templates
# ============================================================================

#' FUNCIÓN PRINCIPAL: Ejecutar suite completa de pruebas
#' 
#' Esta función ejecuta una batería completa de pruebas para validar que
#' todas las mejoras del sistema inteligente funcionan correctamente.
#' 
#' @param directorio_imagenes Directorio con imágenes de prueba (opcional)
#' @param ejecutar_pruebas_completas Si ejecutar todas las pruebas (default: TRUE)
#' @param generar_reporte Si generar reporte detallado (default: TRUE)
#' @return Lista con resultados de todas las pruebas
ejecutar_suite_pruebas_completa <- function(directorio_imagenes = NULL, 
                                           ejecutar_pruebas_completas = TRUE,
                                           generar_reporte = TRUE) {
  
  cat("🧪 INICIANDO SUITE COMPLETA DE PRUEBAS\n")
  cat("=====================================\n")
  
  inicio_tiempo <- Sys.time()
  resultados_pruebas <- list()
  
  # PRUEBA 1: Validación de módulos
  cat("\n📦 PRUEBA 1: Validación de módulos\n")
  resultados_pruebas$modulos <- validar_carga_modulos()
  
  # PRUEBA 2: Pruebas de detección inteligente
  cat("\n🧠 PRUEBA 2: Detección inteligente\n")
  resultados_pruebas$deteccion <- probar_deteccion_inteligente()
  
  # PRUEBA 3: Pruebas de templates especializados
  cat("\n🎨 PRUEBA 3: Templates especializados\n")
  resultados_pruebas$templates <- probar_templates_especializados()
  
  # PRUEBA 4: Pruebas de generación de coordenadas
  cat("\n📊 PRUEBA 4: Generación de coordenadas\n")
  resultados_pruebas$coordenadas <- probar_generacion_coordenadas()
  
  # PRUEBA 5: Pruebas de optimización R-exams
  cat("\n🔧 PRUEBA 5: Optimización R-exams\n")
  resultados_pruebas$optimizacion <- probar_optimizacion_rexams()
  
  if (ejecutar_pruebas_completas) {
    
    # PRUEBA 6: Pruebas de integración
    cat("\n🚀 PRUEBA 6: Integración completa\n")
    resultados_pruebas$integracion <- probar_integracion_completa()
    
    # PRUEBA 7: Pruebas de rendimiento
    cat("\n⚡ PRUEBA 7: Rendimiento\n")
    resultados_pruebas$rendimiento <- probar_rendimiento_sistema()
    
    # PRUEBA 8: Pruebas con imágenes reales (si se proporciona directorio)
    if (!is.null(directorio_imagenes) && dir.exists(directorio_imagenes)) {
      cat("\n🖼️ PRUEBA 8: Imágenes reales\n")
      resultados_pruebas$imagenes_reales <- probar_imagenes_reales(directorio_imagenes)
    }
  }
  
  fin_tiempo <- Sys.time()
  tiempo_total <- as.numeric(difftime(fin_tiempo, inicio_tiempo, units = "secs"))
  
  # Calcular resumen de resultados
  resumen <- calcular_resumen_pruebas(resultados_pruebas, tiempo_total)
  
  # Generar reporte si se solicita
  if (generar_reporte) {
    reporte_path <- generar_reporte_pruebas(resultados_pruebas, resumen)
    resumen$reporte_generado <- reporte_path
  }
  
  # Mostrar resumen
  mostrar_resumen_pruebas(resumen)
  
  list(
    resultados = resultados_pruebas,
    resumen = resumen,
    tiempo_total = tiempo_total
  )
}

# ============================================================================
# PRUEBAS ESPECÍFICAS
# ============================================================================

#' Validar carga de módulos
validar_carga_modulos <- function() {
  
  modulos_requeridos <- c(
    "02_detector_inteligente.R",
    "06_funciones_auxiliares.R", 
    "03_templates_especializados.R",
    "04_generador_coordenadas.R",
    "05_optimizador_rexams.R",
    "01_sistema_principal.R"
  )
  
  resultados <- list()
  
  for (modulo in modulos_requeridos) {
    ruta_modulo <- file.path("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes", modulo)
    
    if (file.exists(ruta_modulo)) {
      tryCatch({
        source(ruta_modulo)
        resultados[[modulo]] <- list(estado = "OK", error = NULL)
        cat(sprintf("   ✅ %s\n", modulo))
      }, error = function(e) {
        resultados[[modulo]] <- list(estado = "ERROR", error = e$message)
        cat(sprintf("   ❌ %s: %s\n", modulo, e$message))
      })
    } else {
      resultados[[modulo]] <- list(estado = "NO_ENCONTRADO", error = "Archivo no existe")
      cat(sprintf("   ⚠️ %s: No encontrado\n", modulo))
    }
  }
  
  # Calcular estadísticas
  estados <- sapply(resultados, function(r) r$estado)
  exitos <- sum(estados == "OK")
  total <- length(estados)
  
  list(
    resultados_individuales = resultados,
    exitos = exitos,
    total = total,
    porcentaje_exito = (exitos / total) * 100,
    estado_general = if (exitos == total) "EXITOSO" else "CON_ERRORES"
  )
}

#' Probar detección inteligente
probar_deteccion_inteligente <- function() {
  
  # Crear características de prueba para diferentes tipos de gráficos
  casos_prueba <- list(
    circular = crear_caracteristicas_circular_prueba(),
    barras = crear_caracteristicas_barras_prueba(),
    lineas = crear_caracteristicas_lineas_prueba(),
    tabla = crear_caracteristicas_tabla_prueba()
  )
  
  resultados <- list()
  
  for (tipo in names(casos_prueba)) {
    cat(sprintf("   Probando detección: %s\n", tipo))
    
    tryCatch({
      caracteristicas <- casos_prueba[[tipo]]
      deteccion <- detectar_tipo_grafico_inteligente(caracteristicas, umbral_confianza = 0.7)
      
      # Verificar que la detección sea correcta
      deteccion_correcta <- deteccion$tipo_detectado == tipo || 
                           deteccion$tipo_detectado == "hibrido"
      
      resultados[[tipo]] <- list(
        estado = if (deteccion_correcta) "OK" else "FALLO",
        tipo_detectado = deteccion$tipo_detectado,
        confianza = deteccion$confianza,
        motor_recomendado = deteccion$motor_recomendado
      )
      
      cat(sprintf("      ✅ Detectado: %s (confianza: %.2f%%)\n", 
                  deteccion$tipo_detectado, deteccion$confianza * 100))
      
    }, error = function(e) {
      resultados[[tipo]] <- list(
        estado = "ERROR",
        error = e$message
      )
      cat(sprintf("      ❌ Error: %s\n", e$message))
    })
  }
  
  # Calcular estadísticas
  estados <- sapply(resultados, function(r) r$estado)
  exitos <- sum(estados == "OK")
  
  list(
    resultados_individuales = resultados,
    exitos = exitos,
    total = length(casos_prueba),
    porcentaje_exito = (exitos / length(casos_prueba)) * 100,
    estado_general = if (exitos == length(casos_prueba)) "EXITOSO" else "CON_ERRORES"
  )
}

#' Probar templates especializados
probar_templates_especializados <- function() {
  
  tipos_template <- c("circular", "barras", "lineas", "tabla")
  motores <- list(
    circular = "tikz_circular_simple",
    barras = "tikz_barras_simple", 
    lineas = "tikz_lineas_simple",
    tabla = "tikz_tabular"
  )
  
  resultados <- list()
  
  for (tipo in tipos_template) {
    cat(sprintf("   Probando template: %s\n", tipo))
    
    tryCatch({
      # Crear parámetros de prueba
      parametros <- crear_parametros_prueba(tipo)
      caracteristicas <- crear_caracteristicas_genericas_prueba()
      
      # Generar código TikZ
      codigo_tikz <- generar_tikz_template_inteligente(
        tipo, motores[[tipo]], parametros, caracteristicas
      )
      
      # Validar que el código generado sea válido
      es_valido <- validar_codigo_tikz_basico(codigo_tikz)
      
      resultados[[tipo]] <- list(
        estado = if (es_valido) "OK" else "FALLO",
        longitud_codigo = nchar(codigo_tikz),
        contiene_tikzpicture = grepl("\\\\begin\\{tikzpicture\\}", codigo_tikz),
        contiene_end = grepl("\\\\end\\{tikzpicture\\}", codigo_tikz)
      )
      
      cat(sprintf("      ✅ Código generado: %d caracteres\n", nchar(codigo_tikz)))
      
    }, error = function(e) {
      resultados[[tipo]] <- list(
        estado = "ERROR",
        error = e$message
      )
      cat(sprintf("      ❌ Error: %s\n", e$message))
    })
  }
  
  # Calcular estadísticas
  estados <- sapply(resultados, function(r) r$estado)
  exitos <- sum(estados == "OK")
  
  list(
    resultados_individuales = resultados,
    exitos = exitos,
    total = length(tipos_template),
    porcentaje_exito = (exitos / length(tipos_template)) * 100,
    estado_general = if (exitos == length(tipos_template)) "EXITOSO" else "CON_ERRORES"
  )
}

# ============================================================================
# FUNCIONES AUXILIARES DE PRUEBA
# ============================================================================

#' Crear características de prueba para gráfico circular
crear_caracteristicas_circular_prueba <- function() {
  list(
    colores_exactos = list(
      num_colores = 4,
      patron_distribucion = "balanceado",
      colores_hex = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00")
    ),
    geometria = list(
      circulos = list(list(x = 0, y = 0, radio = 2)),
      complejidad = 0.3,
      simetria = 0.8
    ),
    coordenadas = list(
      list(centro_x = 0.5, centro_y = 0.5, tipo = "sector")
    )
  )
}

#' Crear características de prueba para gráfico de barras
crear_caracteristicas_barras_prueba <- function() {
  list(
    colores_exactos = list(
      num_colores = 3,
      patron_distribucion = "uniforme"
    ),
    geometria = list(
      rectangulos = list(
        list(x = 1, y = 0, ancho = 0.8, alto = 3),
        list(x = 2, y = 0, ancho = 0.8, alto = 2),
        list(x = 3, y = 0, ancho = 0.8, alto = 4)
      ),
      lineas_h = list(list(y = 0)),
      lineas_v = list(list(x = 0))
    )
  )
}

cat("✅ Sistema de validación y testing cargado exitosamente\n")
cat("🧪 Funciones de prueba disponibles:\n")
cat("   - ejecutar_suite_pruebas_completa() - Suite completa\n")
cat("   - validar_carga_modulos() - Validar módulos\n")
cat("   - probar_deteccion_inteligente() - Probar detección\n")
cat("   - probar_templates_especializados() - Probar templates\n")
