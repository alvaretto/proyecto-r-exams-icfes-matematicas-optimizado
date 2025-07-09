# ============================================================================
# DETECTOR INTELIGENTE DE TIPOS DE GRÁFICOS
# Sistema Mejorado para Selección Automática de Templates Óptimos
# ============================================================================

#' FUNCIÓN PRINCIPAL: Detector Inteligente de Tipos de Gráficos
#' 
#' Esta función analiza las características extraídas de una imagen y determina
#' automáticamente el tipo de gráfico más eficiente para generar (TikZ básico,
#' PGFPlots, o template especializado).
#' 
#' @param caracteristicas Lista con características extraídas de la imagen
#' @param umbral_confianza Umbral mínimo de confianza para la detección (default: 0.8)
#' @return Lista con tipo detectado, motor recomendado y parámetros específicos
detectar_tipo_grafico_inteligente <- function(caracteristicas, umbral_confianza = 0.8) {
  
  cat("🧠 Iniciando detección inteligente de tipo de gráfico...\n")
  
  # Análisis multi-dimensional de características
  analisis_dimensional <- analizar_caracteristicas_multidimensional(caracteristicas)
  
  # Detectar patrones específicos
  patrones_detectados <- detectar_patrones_graficos(caracteristicas, analisis_dimensional)
  
  # Calcular confianza para cada tipo posible
  confianzas <- calcular_confianzas_tipos(patrones_detectados, caracteristicas)
  
  # Seleccionar el tipo con mayor confianza
  tipo_optimo <- seleccionar_tipo_optimo(confianzas, umbral_confianza)
  
  # Determinar motor de renderizado más eficiente
  motor_recomendado <- determinar_motor_optimo(tipo_optimo, caracteristicas)
  
  # Extraer parámetros específicos para el tipo detectado
  parametros_especificos <- extraer_parametros_tipo_especifico(tipo_optimo, caracteristicas)
  
  resultado <- list(
    tipo_detectado = tipo_optimo$tipo,
    confianza = tipo_optimo$confianza,
    motor_recomendado = motor_recomendado,
    parametros_especificos = parametros_especificos,
    justificacion = tipo_optimo$justificacion,
    alternativas = confianzas[confianzas$confianza > 0.5, ]
  )
  
  cat(sprintf("✅ Tipo detectado: %s (confianza: %.2f%%)\n", 
              resultado$tipo_detectado, resultado$confianza * 100))
  cat(sprintf("🔧 Motor recomendado: %s\n", resultado$motor_recomendado))
  
  return(resultado)
}

#' Análisis multi-dimensional de características
analizar_caracteristicas_multidimensional <- function(caracteristicas) {
  
  # Análisis de distribución de colores
  distribucion_colores <- analizar_distribucion_colores(caracteristicas$colores_exactos)
  
  # Análisis de geometría y formas
  analisis_geometrico <- analizar_patrones_geometricos(caracteristicas$geometria)
  
  # Análisis de estructura espacial
  estructura_espacial <- analizar_estructura_espacial(caracteristicas$coordenadas)
  
  # Análisis de contenido textual/numérico
  contenido_textual <- analizar_contenido_textual(caracteristicas$texto_numeros)
  
  return(list(
    colores = distribucion_colores,
    geometria = analisis_geometrico,
    espacial = estructura_espacial,
    textual = contenido_textual
  ))
}

#' Detectar patrones específicos de tipos de gráficos
detectar_patrones_graficos <- function(caracteristicas, analisis_dimensional) {
  
  patrones <- list()
  
  # PATRÓN 1: Gráfico Circular/Pie Chart
  patrones$circular <- detectar_patron_circular(analisis_dimensional)
  
  # PATRÓN 2: Gráfico de Barras
  patrones$barras <- detectar_patron_barras(analisis_dimensional)
  
  # PATRÓN 3: Gráfico de Líneas/Funciones
  patrones$lineas <- detectar_patron_lineas(analisis_dimensional)
  
  # PATRÓN 4: Tabla de Datos
  patrones$tabla <- detectar_patron_tabla(analisis_dimensional)
  
  # PATRÓN 5: Diagrama Geométrico
  patrones$geometrico <- detectar_patron_geometrico(analisis_dimensional)
  
  # PATRÓN 6: Diagrama de Venn
  patrones$venn <- detectar_patron_venn(analisis_dimensional)
  
  # PATRÓN 7: Gráfico 3D/Isométrico
  patrones$tridimensional <- detectar_patron_3d(analisis_dimensional)
  
  # PATRÓN 8: Función Matemática Compleja
  patrones$funcion_matematica <- detectar_patron_funcion_matematica(analisis_dimensional)
  
  return(patrones)
}

#' Calcular confianzas para cada tipo detectado
calcular_confianzas_tipos <- function(patrones_detectados, caracteristicas) {
  
  confianzas <- data.frame(
    tipo = character(),
    confianza = numeric(),
    justificacion = character(),
    stringsAsFactors = FALSE
  )
  
  for (tipo in names(patrones_detectados)) {
    patron <- patrones_detectados[[tipo]]
    
    if (!is.null(patron) && patron$detectado) {
      confianza_calculada <- calcular_confianza_especifica(tipo, patron, caracteristicas)
      
      confianzas <- rbind(confianzas, data.frame(
        tipo = tipo,
        confianza = confianza_calculada$valor,
        justificacion = confianza_calculada$justificacion,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Ordenar por confianza descendente
  confianzas <- confianzas[order(confianzas$confianza, decreasing = TRUE), ]
  
  return(confianzas)
}

#' Seleccionar tipo óptimo basado en confianzas
seleccionar_tipo_optimo <- function(confianzas, umbral_confianza) {
  
  if (nrow(confianzas) == 0) {
    return(list(
      tipo = "generico",
      confianza = 0.5,
      justificacion = "No se detectó patrón específico, usando template genérico"
    ))
  }
  
  tipo_principal <- confianzas[1, ]
  
  if (tipo_principal$confianza >= umbral_confianza) {
    return(list(
      tipo = tipo_principal$tipo,
      confianza = tipo_principal$confianza,
      justificacion = tipo_principal$justificacion
    ))
  } else {
    # Si la confianza es baja, usar enfoque híbrido
    return(list(
      tipo = "hibrido",
      confianza = tipo_principal$confianza,
      justificacion = sprintf("Confianza baja (%.2f%%), usando enfoque híbrido con elementos de %s", 
                             tipo_principal$confianza * 100, tipo_principal$tipo)
    ))
  }
}

#' Determinar motor de renderizado óptimo
determinar_motor_optimo <- function(tipo_optimo, caracteristicas) {
  
  # Reglas de decisión para motor óptimo
  motor <- switch(tipo_optimo$tipo,
    "circular" = determinar_motor_circular(caracteristicas),
    "barras" = determinar_motor_barras(caracteristicas),
    "lineas" = determinar_motor_lineas(caracteristicas),
    "funcion_matematica" = "pgfplots_avanzado",
    "tabla" = "tikz_tabular",
    "geometrico" = determinar_motor_geometrico(caracteristicas),
    "venn" = "tikz_especializado",
    "tridimensional" = "tikz_3d",
    "hibrido" = "tikz_flexible",
    "tikz_generico"  # default
  )
  
  return(motor)
}

#' Extraer parámetros específicos para el tipo detectado
extraer_parametros_tipo_especifico <- function(tipo_optimo, caracteristicas) {
  
  parametros <- switch(tipo_optimo$tipo,
    "circular" = extraer_parametros_circular(caracteristicas),
    "barras" = extraer_parametros_barras(caracteristicas),
    "lineas" = extraer_parametros_lineas(caracteristicas),
    "funcion_matematica" = extraer_parametros_funcion(caracteristicas),
    "tabla" = extraer_parametros_tabla(caracteristicas),
    "geometrico" = extraer_parametros_geometrico(caracteristicas),
    "venn" = extraer_parametros_venn(caracteristicas),
    "tridimensional" = extraer_parametros_3d(caracteristicas),
    extraer_parametros_genericos(caracteristicas)  # default
  )
  
  return(parametros)
}

# ============================================================================
# FUNCIONES DE DETECCIÓN DE PATRONES ESPECÍFICOS
# ============================================================================

#' Detectar patrón de gráfico circular
detectar_patron_circular <- function(analisis_dimensional) {
  
  # Buscar formas circulares en geometría
  circulos_detectados <- analisis_dimensional$geometria$circulos
  
  # Analizar distribución radial de colores
  distribucion_radial <- analizar_distribucion_radial(analisis_dimensional$colores)
  
  # Buscar sectores/wedges
  sectores_detectados <- detectar_sectores_circulares(analisis_dimensional$espacial)
  
  confianza <- 0
  justificacion <- ""
  
  if (length(circulos_detectados) > 0) {
    confianza <- confianza + 0.4
    justificacion <- paste(justificacion, "Círculos detectados;")
  }
  
  if (distribucion_radial$es_radial) {
    confianza <- confianza + 0.3
    justificacion <- paste(justificacion, "Distribución radial de colores;")
  }
  
  if (sectores_detectados$detectados) {
    confianza <- confianza + 0.3
    justificacion <- paste(justificacion, "Sectores circulares detectados;")
  }
  
  return(list(
    detectado = confianza > 0.5,
    confianza_base = confianza,
    justificacion = justificacion,
    detalles = list(
      circulos = circulos_detectados,
      distribucion_radial = distribucion_radial,
      sectores = sectores_detectados
    )
  ))
}

#' Detectar patrón de gráfico de barras
detectar_patron_barras <- function(analisis_dimensional) {
  
  # Buscar rectángulos alineados
  rectangulos <- analisis_dimensional$geometria$rectangulos
  
  # Analizar alineación horizontal/vertical
  alineacion <- analizar_alineacion_elementos(analisis_dimensional$espacial)
  
  # Buscar ejes cartesianos
  ejes_detectados <- detectar_ejes_cartesianos(analisis_dimensional$geometria)
  
  confianza <- 0
  justificacion <- ""
  
  if (length(rectangulos) >= 2) {
    confianza <- confianza + 0.4
    justificacion <- paste(justificacion, "Múltiples rectángulos detectados;")
  }
  
  if (alineacion$alineados) {
    confianza <- confianza + 0.3
    justificacion <- paste(justificacion, "Elementos alineados;")
  }
  
  if (ejes_detectados$detectados) {
    confianza <- confianza + 0.3
    justificacion <- paste(justificacion, "Ejes cartesianos detectados;")
  }
  
  return(list(
    detectado = confianza > 0.5,
    confianza_base = confianza,
    justificacion = justificacion,
    detalles = list(
      rectangulos = rectangulos,
      alineacion = alineacion,
      ejes = ejes_detectados
    )
  ))
}

# ============================================================================
# FUNCIONES AUXILIARES DE ANÁLISIS ESPECÍFICO
# ============================================================================

#' Analizar distribución de colores
analizar_distribucion_colores <- function(colores_exactos) {

  if (is.null(colores_exactos) || length(colores_exactos$colores_hex) == 0) {
    return(list(
      num_colores = 0,
      diversidad = 0,
      dominancia = 0,
      patron_distribucion = "uniforme"
    ))
  }

  num_colores <- length(colores_exactos$colores_hex)
  frecuencias <- colores_exactos$frecuencias

  # Calcular diversidad (entropía de Shannon)
  prob_colores <- frecuencias / sum(frecuencias)
  diversidad <- -sum(prob_colores * log2(prob_colores + 1e-10))

  # Calcular dominancia del color principal
  dominancia <- max(prob_colores)

  # Determinar patrón de distribución
  patron <- determinar_patron_distribucion_colores(prob_colores)

  return(list(
    num_colores = num_colores,
    diversidad = diversidad,
    dominancia = dominancia,
    patron_distribucion = patron,
    colores_principales = colores_exactos$colores_hex[1:min(5, num_colores)]
  ))
}

#' Analizar patrones geométricos
analizar_patrones_geometricos <- function(geometria) {

  if (is.null(geometria)) {
    return(list(
      formas_detectadas = character(),
      complejidad = 0,
      simetria = 0,
      regularidad = 0
    ))
  }

  # Contar tipos de formas
  formas <- c()
  if (!is.null(geometria$circulos) && length(geometria$circulos) > 0) formas <- c(formas, "circular")
  if (!is.null(geometria$rectangulos) && length(geometria$rectangulos) > 0) formas <- c(formas, "rectangular")
  if (!is.null(geometria$lineas_h) && length(geometria$lineas_h) > 0) formas <- c(formas, "lineal_h")
  if (!is.null(geometria$lineas_v) && length(geometria$lineas_v) > 0) formas <- c(formas, "lineal_v")

  # Calcular complejidad geométrica
  complejidad <- calcular_complejidad_geometrica(geometria)

  # Analizar simetría
  simetria <- analizar_simetria_formas(geometria)

  # Calcular regularidad
  regularidad <- calcular_regularidad_formas(geometria)

  return(list(
    formas_detectadas = formas,
    complejidad = complejidad,
    simetria = simetria,
    regularidad = regularidad,
    detalles_formas = geometria
  ))
}

#' Analizar estructura espacial
analizar_estructura_espacial <- function(coordenadas) {

  if (is.null(coordenadas) || length(coordenadas) == 0) {
    return(list(
      distribucion = "dispersa",
      alineacion = 0,
      agrupacion = 0,
      patron_espacial = "aleatorio"
    ))
  }

  # Extraer centros de regiones
  centros_x <- sapply(coordenadas, function(r) r$centro_x)
  centros_y <- sapply(coordenadas, function(r) r$centro_y)

  # Analizar distribución espacial
  distribucion <- analizar_distribucion_espacial(centros_x, centros_y)

  # Calcular alineación
  alineacion <- calcular_alineacion_puntos(centros_x, centros_y)

  # Detectar agrupaciones (clustering)
  agrupacion <- detectar_agrupaciones_espaciales(centros_x, centros_y)

  # Determinar patrón espacial general
  patron <- determinar_patron_espacial(distribucion, alineacion, agrupacion)

  return(list(
    distribucion = distribucion,
    alineacion = alineacion,
    agrupacion = agrupacion,
    patron_espacial = patron,
    centros = list(x = centros_x, y = centros_y)
  ))
}

#' Analizar contenido textual/numérico
analizar_contenido_textual <- function(texto_numeros) {

  if (is.null(texto_numeros)) {
    return(list(
      tiene_texto = FALSE,
      tiene_numeros = FALSE,
      tipo_contenido = "sin_texto",
      densidad_textual = 0
    ))
  }

  # Detectar presencia de texto y números
  tiene_texto <- !is.null(texto_numeros$texto_detectado) &&
                 length(texto_numeros$texto_detectado) > 0

  tiene_numeros <- !is.null(texto_numeros$numeros_detectados) &&
                   length(texto_numeros$numeros_detectados) > 0

  # Determinar tipo de contenido
  tipo_contenido <- determinar_tipo_contenido_textual(texto_numeros)

  # Calcular densidad textual
  densidad <- calcular_densidad_textual(texto_numeros)

  return(list(
    tiene_texto = tiene_texto,
    tiene_numeros = tiene_numeros,
    tipo_contenido = tipo_contenido,
    densidad_textual = densidad,
    detalles = texto_numeros
  ))
}

# ============================================================================
# FUNCIONES DE DETECCIÓN DE MOTORES ÓPTIMOS
# ============================================================================

#' Determinar motor óptimo para gráficos circulares
determinar_motor_circular <- function(caracteristicas) {

  num_sectores <- length(caracteristicas$colores_exactos$colores_hex)

  if (num_sectores <= 4) {
    return("tikz_circular_simple")
  } else if (num_sectores <= 8) {
    return("tikz_circular_medio")
  } else {
    return("pgfplots_circular")
  }
}

#' Determinar motor óptimo para gráficos de barras
determinar_motor_barras <- function(caracteristicas) {

  num_barras <- length(caracteristicas$geometria$rectangulos)

  if (num_barras <= 5) {
    return("tikz_barras_simple")
  } else if (num_barras <= 10) {
    return("pgfplots_barras")
  } else {
    return("pgfplots_barras_avanzado")
  }
}

#' Determinar motor óptimo para gráficos de líneas
determinar_motor_lineas <- function(caracteristicas) {

  # Detectar si es función matemática compleja
  if (es_funcion_matematica_compleja(caracteristicas)) {
    return("pgfplots_funcion")
  } else {
    return("tikz_lineas_simple")
  }
}

#' Determinar motor óptimo para figuras geométricas
determinar_motor_geometrico <- function(caracteristicas) {

  complejidad <- caracteristicas$geometria$complejidad

  if (complejidad < 0.3) {
    return("tikz_geometrico_simple")
  } else if (complejidad < 0.7) {
    return("tikz_geometrico_medio")
  } else {
    return("tikz_geometrico_avanzado")
  }
}

cat("✅ Detector inteligente de gráficos cargado exitosamente\n")
cat("🧠 Funciones principales disponibles:\n")
cat("   - detectar_tipo_grafico_inteligente()\n")
cat("   - analizar_caracteristicas_multidimensional()\n")
cat("   - detectar_patrones_graficos()\n")
