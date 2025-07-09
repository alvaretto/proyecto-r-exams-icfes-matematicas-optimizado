# ============================================================================
# TEMPLATES INTELIGENTES ESPECIALIZADOS
# Sistema Optimizado de Templates con Mínima Intervención Humana
# ============================================================================

#' FUNCIÓN PRINCIPAL: Generar código TikZ usando templates inteligentes
#' 
#' Esta función selecciona y aplica automáticamente el template más eficiente
#' basado en la detección inteligente del tipo de gráfico.
#' 
#' @param tipo_detectado Tipo de gráfico detectado por el sistema inteligente
#' @param motor_recomendado Motor de renderizado recomendado
#' @param parametros_especificos Parámetros extraídos específicamente para el tipo
#' @param caracteristicas Características completas de la imagen
#' @return Código TikZ optimizado y listo para R-exams
generar_tikz_template_inteligente <- function(tipo_detectado, motor_recomendado, 
                                             parametros_especificos, caracteristicas) {
  
  cat(sprintf("🎨 Generando TikZ con template inteligente: %s\n", tipo_detectado))
  cat(sprintf("🔧 Motor: %s\n", motor_recomendado))
  
  # Seleccionar generador específico basado en tipo y motor
  codigo_tikz <- switch(tipo_detectado,
    "circular" = generar_circular_inteligente(motor_recomendado, parametros_especificos),
    "barras" = generar_barras_inteligente(motor_recomendado, parametros_especificos),
    "lineas" = generar_lineas_inteligente(motor_recomendado, parametros_especificos),
    "funcion_matematica" = generar_funcion_inteligente(motor_recomendado, parametros_especificos),
    "tabla" = generar_tabla_inteligente(motor_recomendado, parametros_especificos),
    "geometrico" = generar_geometrico_inteligente(motor_recomendado, parametros_especificos),
    "venn" = generar_venn_inteligente(motor_recomendado, parametros_especificos),
    "tridimensional" = generar_3d_inteligente(motor_recomendado, parametros_especificos),
    "hibrido" = generar_hibrido_inteligente(motor_recomendado, parametros_especificos),
    generar_generico_inteligente(motor_recomendado, parametros_especificos)
  )
  
  # Aplicar optimizaciones finales
  codigo_optimizado <- aplicar_optimizaciones_inteligentes(codigo_tikz, caracteristicas)
  
  # Validar compatibilidad R-exams
  codigo_validado <- validar_compatibilidad_rexams(codigo_optimizado)
  
  cat("✅ Template inteligente aplicado exitosamente\n")
  
  codigo_validado
}

# ============================================================================
# GENERADORES ESPECÍFICOS POR TIPO DE GRÁFICO
# ============================================================================

#' Generar gráfico circular con template inteligente
generar_circular_inteligente <- function(motor, parametros) {
  
  switch(motor,
    "tikz_circular_simple" = generar_circular_tikz_simple(parametros),
    "tikz_circular_medio" = generar_circular_tikz_medio(parametros),
    "pgfplots_circular" = generar_circular_pgfplots(parametros),
    generar_circular_tikz_simple(parametros)  # default
  )
}

#' Template TikZ simple para gráfico circular
generar_circular_tikz_simple <- function(parametros) {
  
  # Extraer parámetros
  sectores <- parametros$sectores %||% list(
    list(angulo = 90, color = "blue!60", etiqueta = "A"),
    list(angulo = 90, color = "red!60", etiqueta = "B"),
    list(angulo = 90, color = "green!60", etiqueta = "C"),
    list(angulo = 90, color = "orange!60", etiqueta = "D")
  )
  
  radio <- parametros$radio %||% 2
  
  # Generar código TikZ
  codigo_sectores <- ""
  angulo_acumulado <- 0
  
  for (i in seq_along(sectores)) {
    sector <- sectores[[i]]
    angulo_inicio <- angulo_acumulado
    angulo_fin <- angulo_acumulado + sector$angulo
    
    codigo_sectores <- paste0(codigo_sectores, sprintf(
      "\\fill[%s] (0,0) -- (%d:%s) arc (%d:%d:%s) -- cycle;\n",
      sector$color, angulo_inicio, radio, angulo_inicio, angulo_fin, radio
    ))
    
    # Agregar etiqueta
    angulo_medio <- angulo_inicio + sector$angulo / 2
    codigo_sectores <- paste0(codigo_sectores, sprintf(
      "\\node at (%d:%s) {%s};\n",
      angulo_medio, radio * 0.7, sector$etiqueta
    ))
    
    angulo_acumulado <- angulo_fin
  }
  
  template <- sprintf("
\\begin{tikzpicture}[scale=0.8]
%s
\\draw[black, thick] (0,0) circle (%s);
\\end{tikzpicture}", codigo_sectores, radio)
  
  template
}

#' Generar gráfico de barras con template inteligente
generar_barras_inteligente <- function(motor, parametros) {
  
  switch(motor,
    "tikz_barras_simple" = generar_barras_tikz_simple(parametros),
    "pgfplots_barras" = generar_barras_pgfplots(parametros),
    "pgfplots_barras_avanzado" = generar_barras_pgfplots_avanzado(parametros),
    generar_barras_tikz_simple(parametros)  # default
  )
}

#' Template TikZ simple para gráfico de barras
generar_barras_tikz_simple <- function(parametros) {
  
  # Extraer parámetros
  barras <- parametros$barras %||% list(
    list(altura = 3, color = "blue!60", etiqueta = "A"),
    list(altura = 2, color = "red!60", etiqueta = "B"),
    list(altura = 4, color = "green!60", etiqueta = "C")
  )
  
  ancho_barra <- parametros$ancho_barra %||% 0.8
  separacion <- parametros$separacion %||% 1.2
  
  # Generar código de barras
  codigo_barras <- ""
  max_altura <- max(sapply(barras, function(b) b$altura))
  
  for (i in seq_along(barras)) {
    barra <- barras[[i]]
    x_pos <- (i - 1) * separacion
    
    codigo_barras <- paste0(codigo_barras, sprintf(
      "\\fill[%s] (%s,0) rectangle (%s,%s);\n",
      barra$color, x_pos, x_pos + ancho_barra, barra$altura
    ))
    
    # Etiqueta en el eje X
    codigo_barras <- paste0(codigo_barras, sprintf(
      "\\node[below] at (%s,-0.3) {%s};\n",
      x_pos + ancho_barra/2, barra$etiqueta
    ))
  }
  
  # Ejes
  codigo_ejes <- sprintf(
    "\\draw[->] (0,0) -- (0,%s) node[left] {Valores};\n\\draw[->] (0,0) -- (%s,0) node[below] {Categorías};\n",
    max_altura + 1, length(barras) * separacion
  )
  
  template <- sprintf("
\\begin{tikzpicture}[scale=0.8]
%s
%s
\\end{tikzpicture}", codigo_ejes, codigo_barras)
  
  template
}

#' Template PGFPlots para gráfico de barras
generar_barras_pgfplots <- function(parametros) {
  
  # Extraer datos
  barras <- parametros$barras %||% list(
    list(altura = 3, etiqueta = "A"),
    list(altura = 2, etiqueta = "B"),
    list(altura = 4, etiqueta = "C")
  )
  
  # Generar datos para PGFPlots
  datos_x <- seq_along(barras)
  datos_y <- sapply(barras, function(b) b$altura)
  etiquetas <- sapply(barras, function(b) b$etiqueta)
  
  # Crear tabla de datos
  tabla_datos <- paste(
    paste(datos_x, datos_y, sep = " "),
    collapse = "\n"
  )
  
  template <- sprintf("
\\begin{tikzpicture}
\\begin{axis}[
  ybar,
  xlabel={Categorías},
  ylabel={Valores},
  symbolic x coords={%s},
  xtick=data,
  nodes near coords,
  nodes near coords align={vertical},
]
\\addplot coordinates {
%s
};
\\end{axis}
\\end{tikzpicture}", 
    paste(etiquetas, collapse = ","),
    paste(sprintf("(%s,%s)", etiquetas, datos_y), collapse = " ")
  )
  
  template
}

#' Generar gráfico de líneas con template inteligente
generar_lineas_inteligente <- function(motor, parametros) {
  
  switch(motor,
    "tikz_lineas_simple" = generar_lineas_tikz_simple(parametros),
    "pgfplots_funcion" = generar_funcion_pgfplots(parametros),
    generar_lineas_tikz_simple(parametros)  # default
  )
}

#' Template TikZ simple para gráfico de líneas
generar_lineas_tikz_simple <- function(parametros) {
  
  # Extraer puntos
  puntos <- parametros$puntos %||% list(
    c(0, 1), c(1, 2), c(2, 1.5), c(3, 3)
  )
  
  color_linea <- parametros$color %||% "blue"
  grosor <- parametros$grosor %||% "thick"
  
  # Generar coordenadas
  coordenadas <- sapply(puntos, function(p) sprintf("(%s,%s)", p[1], p[2]))
  
  # Código de líneas
  codigo_lineas <- sprintf(
    "\\draw[%s, %s] %s;\n",
    color_linea, grosor, paste(coordenadas, collapse = " -- ")
  )
  
  # Puntos
  codigo_puntos <- paste(
    sapply(coordenadas, function(c) sprintf("\\fill[%s] %s circle (2pt);", color_linea, c)),
    collapse = "\n"
  )
  
  # Ejes
  max_x <- max(sapply(puntos, function(p) p[1]))
  max_y <- max(sapply(puntos, function(p) p[2]))
  
  codigo_ejes <- sprintf(
    "\\draw[->] (0,0) -- (%s,0) node[below] {X};\n\\draw[->] (0,0) -- (0,%s) node[left] {Y};\n",
    max_x + 1, max_y + 1
  )
  
  template <- sprintf("
\\begin{tikzpicture}[scale=0.8]
%s
%s
%s
\\end{tikzpicture}", codigo_ejes, codigo_lineas, codigo_puntos)
  
  template
}

cat("✅ Templates inteligentes especializados cargados exitosamente\n")
cat("🎨 Generadores disponibles:\n")
cat("   - Gráficos circulares (TikZ simple/medio, PGFPlots)\n")
cat("   - Gráficos de barras (TikZ simple, PGFPlots básico/avanzado)\n")
cat("   - Gráficos de líneas (TikZ simple, PGFPlots funciones)\n")
