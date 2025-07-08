# ============================================================================
# MÓDULO DE ANÁLISIS AUTOMÁTICO EXACTO DE IMÁGENES MATEMÁTICAS
# Estrategia Robusta para Réplica Exacta - Proyecto ICFES R-exams
# ============================================================================

# Cargar librerías necesarias
library(magick)
library(imager)
library(tesseract)
library(grDevices)
library(stats)

#' Función principal para análisis automático de imagen matemática
#' @param ruta_imagen Ruta al archivo PNG de la imagen
#' @return Lista con características extraídas automáticamente
analizar_imagen_matematica_exacta <- function(ruta_imagen) {
  
  cat("🔍 Iniciando análisis automático exacto de imagen...\n")
  
  # Verificar que el archivo existe
  if (!file.exists(ruta_imagen)) {
    stop("❌ Error: No se encuentra el archivo de imagen en: ", ruta_imagen)
  }
  
  # Cargar imagen con magick
  img_magick <- magick::image_read(ruta_imagen)
  
  # Cargar imagen con imager para análisis detallado
  img_imager <- imager::load.image(ruta_imagen)
  
  # Extraer características principales
  caracteristicas <- list(
    # Información básica
    dimensiones = extraer_dimensiones_exactas(img_magick),
    
    # Análisis de colores RGB exactos
    colores_exactos = extraer_colores_rgb_exactos(img_magick),
    
    # Detección de elementos geométricos
    geometria = detectar_elementos_geometricos(img_imager),
    
    # Coordenadas precisas de elementos
    coordenadas = calcular_coordenadas_precisas(img_imager),
    
    # Extracción de texto y números (OCR)
    texto_numeros = extraer_texto_ocr_exacto(img_magick),
    
    # Análisis de estructura (tablas, gráficas, etc.)
    estructura = analizar_estructura_contenido(img_imager),
    
    # Metadatos para validación
    metadatos = list(
      archivo_original = ruta_imagen,
      timestamp = Sys.time(),
      resolucion = magick::image_info(img_magick)
    )
  )
  
  cat("✅ Análisis automático completado exitosamente\n")
  return(caracteristicas)
}

#' Extraer dimensiones exactas de la imagen
extraer_dimensiones_exactas <- function(img_magick) {
  info <- magick::image_info(img_magick)
  return(list(
    ancho = info$width,
    alto = info$height,
    ratio_aspecto = info$width / info$height,
    densidad = info$density
  ))
}

#' Extraer colores RGB exactos de la imagen
extraer_colores_rgb_exactos <- function(img_magick, max_colores = 20) {
  
  cat("🎨 Extrayendo colores RGB exactos...\n")
  
  # Cuantizar imagen para obtener colores dominantes
  img_cuantizada <- magick::image_quantize(img_magick, max = max_colores, colorspace = "RGB")
  
  # Obtener histograma de colores
  histograma <- magick::image_histogram(img_cuantizada)
  
  # Extraer códigos RGB exactos
  colores_rgb <- character()
  frecuencias <- numeric()
  
  for (i in 1:nrow(histograma)) {
    color_hex <- histograma$color[i]
    frecuencia <- histograma$count[i]
    
    # Convertir a RGB numérico
    rgb_vals <- col2rgb(color_hex)
    
    colores_rgb <- c(colores_rgb, color_hex)
    frecuencias <- c(frecuencias, frecuencia)
  }
  
  # Ordenar por frecuencia (más dominantes primero)
  orden <- order(frecuencias, decreasing = TRUE)
  
  return(list(
    colores_hex = colores_rgb[orden],
    frecuencias = frecuencias[orden],
    colores_rgb_numericos = t(col2rgb(colores_rgb[orden])),
    color_dominante = colores_rgb[orden[1]],
    paleta_tikz = convertir_a_tikz_colors(colores_rgb[orden])
  ))
}

#' Detectar elementos geométricos en la imagen
detectar_elementos_geometricos <- function(img_imager) {
  
  cat("📐 Detectando elementos geométricos...\n")
  
  # Convertir a escala de grises para análisis
  img_gris <- imager::grayscale(img_imager)
  
  # Detectar bordes
  bordes <- imager::imgradient(img_gris, "xy")
  
  # Detectar líneas horizontales y verticales
  lineas_h <- detectar_lineas_horizontales(img_gris)
  lineas_v <- detectar_lineas_verticales(img_gris)
  
  # Detectar formas circulares/elípticas
  circulos <- detectar_formas_circulares(img_gris)
  
  # Detectar rectángulos/cuadrados
  rectangulos <- detectar_rectangulos(img_gris)
  
  return(list(
    bordes_detectados = bordes,
    lineas_horizontales = lineas_h,
    lineas_verticales = lineas_v,
    formas_circulares = circulos,
    rectangulos = rectangulos,
    tipo_contenido = clasificar_tipo_contenido(lineas_h, lineas_v, circulos, rectangulos)
  ))
}

#' Calcular coordenadas precisas de elementos
calcular_coordenadas_precisas <- function(img_imager) {
  
  cat("📍 Calculando coordenadas precisas...\n")
  
  # Obtener dimensiones
  dims <- dim(img_imager)
  ancho <- dims[1]
  alto <- dims[2]
  
  # Detectar regiones de interés
  regiones <- detectar_regiones_interes(img_imager)
  
  # Calcular coordenadas normalizadas (0-1) para TikZ
  coordenadas_normalizadas <- list()
  
  for (i in seq_along(regiones)) {
    region <- regiones[[i]]
    coordenadas_normalizadas[[i]] <- list(
      x_min = region$x_min / ancho,
      x_max = region$x_max / ancho,
      y_min = region$y_min / alto,
      y_max = region$y_max / alto,
      centro_x = (region$x_min + region$x_max) / (2 * ancho),
      centro_y = (region$y_min + region$y_max) / (2 * alto),
      tipo = region$tipo
    )
  }
  
  return(list(
    coordenadas_pixel = regiones,
    coordenadas_normalizadas = coordenadas_normalizadas,
    coordenadas_tikz = convertir_a_coordenadas_tikz(coordenadas_normalizadas)
  ))
}

#' Extraer texto y números usando OCR
extraer_texto_ocr_exacto <- function(img_magick) {
  
  cat("🔤 Extrayendo texto y números con OCR...\n")
  
  tryCatch({
    # Mejorar imagen para OCR
    img_ocr <- img_magick %>%
      magick::image_resize("200%") %>%  # Aumentar resolución
      magick::image_enhance() %>%       # Mejorar contraste
      magick::image_sharpen()           # Aumentar nitidez
    
    # Extraer texto
    texto_extraido <- tesseract::ocr(img_ocr, engine = tesseract::tesseract("eng"))
    
    # Procesar y limpiar texto
    lineas_texto <- strsplit(texto_extraido, "\n")[[1]]
    lineas_texto <- lineas_texto[nzchar(trimws(lineas_texto))]
    
    # Detectar números específicamente
    numeros_detectados <- extraer_numeros_del_texto(lineas_texto)
    
    return(list(
      texto_completo = texto_extraido,
      lineas_texto = lineas_texto,
      numeros_detectados = numeros_detectados,
      etiquetas_ejes = detectar_etiquetas_ejes(lineas_texto),
      titulos = detectar_titulos(lineas_texto)
    ))
    
  }, error = function(e) {
    cat("⚠️ Advertencia: OCR no disponible o falló. Continuando sin extracción de texto.\n")
    return(list(
      texto_completo = "",
      lineas_texto = character(0),
      numeros_detectados = numeric(0),
      etiquetas_ejes = character(0),
      titulos = character(0)
    ))
  })
}

#' Analizar estructura del contenido (tabla, gráfica, etc.)
analizar_estructura_contenido <- function(img_imager) {
  
  cat("📊 Analizando estructura del contenido...\n")
  
  # Detectar si es tabla
  es_tabla <- detectar_estructura_tabla(img_imager)
  
  # Detectar si es gráfica
  es_grafica <- detectar_estructura_grafica(img_imager)
  
  # Detectar si es diagrama
  es_diagrama <- detectar_estructura_diagrama(img_imager)
  
  # Clasificar tipo principal
  tipo_principal <- determinar_tipo_principal(es_tabla, es_grafica, es_diagrama)
  
  return(list(
    es_tabla = es_tabla,
    es_grafica = es_grafica,
    es_diagrama = es_diagrama,
    tipo_principal = tipo_principal,
    complejidad = calcular_complejidad_visual(img_imager),
    requiere_agente_graficador = (es_tabla$detectado || es_grafica$detectado || es_diagrama$detectado)
  ))
}

# ============================================================================
# FUNCIONES AUXILIARES ESPECIALIZADAS
# ============================================================================

#' Convertir colores a formato TikZ
convertir_a_tikz_colors <- function(colores_hex) {
  tikz_colors <- character()
  
  for (i in seq_along(colores_hex)) {
    rgb_vals <- col2rgb(colores_hex[i])
    tikz_color <- sprintf("\\definecolor{color%d}{RGB}{%d,%d,%d}", 
                         i, rgb_vals[1], rgb_vals[2], rgb_vals[3])
    tikz_colors <- c(tikz_colors, tikz_color)
  }
  
  return(tikz_colors)
}

#' Detectar líneas horizontales
detectar_lineas_horizontales <- function(img_gris) {
  # Implementación simplificada - en producción usar algoritmos más sofisticados
  kernel_h <- matrix(c(-1, -1, -1, 2, 2, 2, -1, -1, -1), nrow = 3)
  lineas_h <- imager::correlate(img_gris, kernel_h)
  return(list(detectadas = sum(lineas_h > 0.5), intensidad = max(lineas_h)))
}

#' Detectar líneas verticales  
detectar_lineas_verticales <- function(img_gris) {
  # Implementación simplificada
  kernel_v <- matrix(c(-1, 2, -1, -1, 2, -1, -1, 2, -1), nrow = 3)
  lineas_v <- imager::correlate(img_gris, kernel_v)
  return(list(detectadas = sum(lineas_v > 0.5), intensidad = max(lineas_v)))
}

#' Detectar formas circulares
detectar_formas_circulares <- function(img_gris) {
  # Implementación simplificada - usar transformada de Hough en producción
  return(list(detectadas = 0, centros = list(), radios = numeric()))
}

#' Detectar rectángulos
detectar_rectangulos <- function(img_gris) {
  # Implementación simplificada
  return(list(detectadas = 0, coordenadas = list()))
}

#' Clasificar tipo de contenido basado en elementos detectados
clasificar_tipo_contenido <- function(lineas_h, lineas_v, circulos, rectangulos) {
  if (lineas_h$detectadas > 10 && lineas_v$detectadas > 10) {
    return("tabla_o_grafica_barras")
  } else if (circulos$detectadas > 0) {
    return("grafica_circular")
  } else if (lineas_h$detectadas > 5 || lineas_v$detectadas > 5) {
    return("grafica_lineas")
  } else {
    return("contenido_simple")
  }
}

cat("✅ Módulo de análisis automático exacto cargado exitosamente\n")
