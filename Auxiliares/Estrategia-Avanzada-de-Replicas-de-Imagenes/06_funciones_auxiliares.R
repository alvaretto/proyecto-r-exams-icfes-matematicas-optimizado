# ============================================================================
# FUNCIONES AUXILIARES PARA DETECTOR INTELIGENTE DE GRÁFICOS
# Implementaciones específicas para análisis de patrones
# ============================================================================

# ============================================================================
# FUNCIONES DE ANÁLISIS DE DISTRIBUCIÓN DE COLORES
# ============================================================================

#' Determinar patrón de distribución de colores
determinar_patron_distribucion_colores <- function(prob_colores) {
  
  if (length(prob_colores) <= 1) return("monocromatico")
  
  # Calcular coeficiente de variación
  cv <- sd(prob_colores) / mean(prob_colores)
  
  # Determinar patrón basado en distribución
  if (cv < 0.3) {
    "uniforme"
  } else if (max(prob_colores) > 0.7) {
    "dominante"
  } else if (cv > 1.0) {
    "disperso"
  } else {
    "balanceado"
  }
}

#' Analizar distribución radial de colores
analizar_distribucion_radial <- function(colores_info) {
  
  # Simulación de análisis radial (en implementación real usaríamos coordenadas polares)
  num_colores <- colores_info$num_colores
  
  # Heurística: si hay 3-8 colores con distribución balanceada, probablemente es radial
  es_radial <- num_colores >= 3 && num_colores <= 8 && 
               colores_info$patron_distribucion %in% c("uniforme", "balanceado")
  
  list(
    es_radial = es_radial,
    confianza = if (es_radial) 0.7 else 0.2,
    num_sectores_estimados = if (es_radial) num_colores else 0
  )
}

# ============================================================================
# FUNCIONES DE ANÁLISIS GEOMÉTRICO
# ============================================================================

#' Calcular complejidad geométrica
calcular_complejidad_geometrica <- function(geometria) {
  
  if (is.null(geometria)) return(0)
  
  # Contar elementos geométricos
  num_circulos <- length(geometria$circulos %||% c())
  num_rectangulos <- length(geometria$rectangulos %||% c())
  num_lineas_h <- length(geometria$lineas_h %||% c())
  num_lineas_v <- length(geometria$lineas_v %||% c())
  
  total_elementos <- num_circulos + num_rectangulos + num_lineas_h + num_lineas_v
  
  # Normalizar complejidad (0-1)
  complejidad <- min(1.0, total_elementos / 20)
  
  complejidad
}

#' Analizar simetría de formas
analizar_simetria_formas <- function(geometria) {
  
  if (is.null(geometria)) return(0)
  
  # Heurística simple: si hay elementos circulares, mayor simetría
  num_circulos <- length(geometria$circulos %||% c())
  num_rectangulos <- length(geometria$rectangulos %||% c())
  
  if (num_circulos > 0) {
    0.8  # Círculos tienen alta simetría
  } else if (num_rectangulos > 0) {
    0.6  # Rectángulos tienen simetría media
  } else {
    0.3  # Otras formas, simetría baja
  }
}

#' Calcular regularidad de formas
calcular_regularidad_formas <- function(geometria) {
  
  if (is.null(geometria)) return(0)
  
  # Heurística: regularidad basada en repetición de formas similares
  num_circulos <- length(geometria$circulos %||% c())
  num_rectangulos <- length(geometria$rectangulos %||% c())
  
  total_formas <- num_circulos + num_rectangulos
  
  if (total_formas == 0) return(0)
  
  # Si hay muchas formas del mismo tipo, mayor regularidad
  max_tipo <- max(num_circulos, num_rectangulos)
  regularidad <- max_tipo / total_formas
  
  regularidad
}

#' Detectar sectores circulares
detectar_sectores_circulares <- function(estructura_espacial) {
  
  # Heurística: si los elementos están distribuidos radialmente
  if (estructura_espacial$patron_espacial == "radial") {
    list(
      detectados = TRUE,
      num_sectores = length(estructura_espacial$centros$x),
      confianza = 0.8
    )
  } else {
    list(
      detectados = FALSE,
      num_sectores = 0,
      confianza = 0.1
    )
  }
}

#' Detectar ejes cartesianos
detectar_ejes_cartesianos <- function(geometria) {
  
  if (is.null(geometria)) {
    return(list(detectados = FALSE, confianza = 0))
  }
  
  # Buscar líneas horizontales y verticales que podrían ser ejes
  lineas_h <- geometria$lineas_h %||% c()
  lineas_v <- geometria$lineas_v %||% c()
  
  # Heurística: si hay al menos una línea horizontal y una vertical
  tiene_ejes <- length(lineas_h) > 0 && length(lineas_v) > 0
  
  list(
    detectados = tiene_ejes,
    confianza = if (tiene_ejes) 0.7 else 0.1,
    lineas_h = length(lineas_h),
    lineas_v = length(lineas_v)
  )
}

# ============================================================================
# FUNCIONES DE ANÁLISIS ESPACIAL
# ============================================================================

#' Analizar distribución espacial
analizar_distribucion_espacial <- function(centros_x, centros_y) {
  
  if (length(centros_x) < 2) return("punto_unico")
  
  # Calcular dispersión
  var_x <- var(centros_x)
  var_y <- var(centros_y)
  
  # Determinar tipo de distribución
  if (var_x < 0.01 && var_y < 0.01) {
    "concentrada"
  } else if (var_x > 0.1 || var_y > 0.1) {
    "dispersa"
  } else {
    "intermedia"
  }
}

#' Calcular alineación de puntos
calcular_alineacion_puntos <- function(centros_x, centros_y) {
  
  if (length(centros_x) < 3) return(0)
  
  # Calcular alineación horizontal
  var_y <- var(centros_y)
  alineacion_h <- if (var_y < 0.05) 0.8 else 0
  
  # Calcular alineación vertical
  var_x <- var(centros_x)
  alineacion_v <- if (var_x < 0.05) 0.8 else 0
  
  # Retornar la mayor alineación
  max(alineacion_h, alineacion_v)
}

#' Detectar agrupaciones espaciales
detectar_agrupaciones_espaciales <- function(centros_x, centros_y) {
  
  if (length(centros_x) < 4) return(0)
  
  # Heurística simple: calcular distancias entre puntos
  distancias <- dist(cbind(centros_x, centros_y))
  
  # Si hay distancias muy pequeñas, hay agrupación
  min_dist <- min(distancias)
  max_dist <- max(distancias)
  
  if (min_dist / max_dist < 0.3) {
    0.7  # Alta agrupación
  } else {
    0.2  # Baja agrupación
  }
}

#' Determinar patrón espacial
determinar_patron_espacial <- function(distribucion, alineacion, agrupacion) {
  
  if (alineacion > 0.6) {
    "lineal"
  } else if (agrupacion > 0.6) {
    "agrupado"
  } else if (distribucion == "concentrada") {
    "concentrado"
  } else if (distribucion == "dispersa") {
    "disperso"
  } else {
    "aleatorio"
  }
}

# ============================================================================
# FUNCIONES DE ANÁLISIS TEXTUAL
# ============================================================================

#' Determinar tipo de contenido textual
determinar_tipo_contenido_textual <- function(texto_numeros) {
  
  if (is.null(texto_numeros)) return("sin_texto")
  
  tiene_texto <- !is.null(texto_numeros$texto_detectado) && 
                 length(texto_numeros$texto_detectado) > 0
  
  tiene_numeros <- !is.null(texto_numeros$numeros_detectados) && 
                   length(texto_numeros$numeros_detectados) > 0
  
  if (tiene_texto && tiene_numeros) {
    "mixto"
  } else if (tiene_numeros) {
    "numerico"
  } else if (tiene_texto) {
    "textual"
  } else {
    "sin_texto"
  }
}

#' Calcular densidad textual
calcular_densidad_textual <- function(texto_numeros) {
  
  if (is.null(texto_numeros)) return(0)
  
  num_texto <- length(texto_numeros$texto_detectado %||% c())
  num_numeros <- length(texto_numeros$numeros_detectados %||% c())
  
  total_elementos <- num_texto + num_numeros
  
  # Normalizar densidad (0-1)
  min(1.0, total_elementos / 10)
}

# ============================================================================
# FUNCIONES DE DETECCIÓN ESPECÍFICA
# ============================================================================

#' Detectar si es función matemática compleja
es_funcion_matematica_compleja <- function(caracteristicas) {
  
  # Heurística: función compleja si tiene curvas suaves y pocos colores
  num_colores <- caracteristicas$colores_exactos$num_colores
  complejidad <- caracteristicas$geometria$complejidad
  
  # Función compleja típicamente tiene pocos colores pero alta complejidad geométrica
  num_colores <= 3 && complejidad > 0.5
}

#' Analizar alineación de elementos
analizar_alineacion_elementos <- function(estructura_espacial) {
  
  alineacion_valor <- estructura_espacial$alineacion
  
  list(
    alineados = alineacion_valor > 0.6,
    valor_alineacion = alineacion_valor,
    tipo_alineacion = if (alineacion_valor > 0.6) "fuerte" else "debil"
  )
}

cat("✅ Funciones auxiliares del detector cargadas exitosamente\n")
cat("🔧 Funciones de análisis disponibles:\n")
cat("   - Análisis de colores y distribución\n")
cat("   - Análisis geométrico y espacial\n")
cat("   - Detección de patrones específicos\n")
