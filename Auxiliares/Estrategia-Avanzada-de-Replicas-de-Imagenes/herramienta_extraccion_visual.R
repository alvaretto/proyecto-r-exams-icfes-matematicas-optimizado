# ============================================================================
# HERRAMIENTA SIMPLE DE EXTRACCIÓN VISUAL
# Enfoque Híbrido: Método manual eficiente para cualquier imagen
# ============================================================================

#' HERRAMIENTA PRINCIPAL: Extractor Visual de Coordenadas
#' 
#' Esta función proporciona un método sistemático para extraer coordenadas
#' de cualquier imagen matemática usando observación visual directa.
#' 
#' @param imagen_path Ruta a la imagen a analizar
#' @param tipo_grafico Tipo de gráfico ("funcion", "barras", "circular", etc.)
#' @return Código TikZ generado paso a paso
extraer_coordenadas_visual <- function(imagen_path, tipo_grafico = "funcion") {
  
  cat("🔍 HERRAMIENTA DE EXTRACCIÓN VISUAL\n")
  cat("==================================\n")
  cat(sprintf("📁 Imagen: %s\n", basename(imagen_path)))
  cat(sprintf("📊 Tipo: %s\n", tipo_grafico))
  
  # PASO 1: Guía de observación visual
  cat("\n📋 PASO 1: ANÁLISIS VISUAL SISTEMÁTICO\n")
  guia_observacion_visual(tipo_grafico)
  
  # PASO 2: Plantilla de extracción
  cat("\n📝 PASO 2: PLANTILLA DE EXTRACCIÓN\n")
  plantilla <- generar_plantilla_extraccion(tipo_grafico)
  
  # PASO 3: Código TikZ base
  cat("\n🛠️ PASO 3: CÓDIGO TIKZ BASE\n")
  codigo_base <- generar_codigo_tikz_base(tipo_grafico)
  
  # PASO 4: Instrucciones de refinamiento
  cat("\n🎯 PASO 4: INSTRUCCIONES DE REFINAMIENTO\n")
  instrucciones_refinamiento(tipo_grafico)
  
  return(list(
    guia = guia_observacion_visual(tipo_grafico),
    plantilla = plantilla,
    codigo_base = codigo_base,
    tipo = tipo_grafico
  ))
}

#' Guía de observación visual sistemática
guia_observacion_visual <- function(tipo_grafico) {
  
  switch(tipo_grafico,
    "funcion" = {
      cat("🔍 GUÍA PARA FUNCIONES MATEMÁTICAS:\n")
      cat("   1. Identificar EJES: ¿Cuáles son las etiquetas X e Y?\n")
      cat("   2. Determinar RANGO: ¿Cuáles son los valores mín/máx de X e Y?\n")
      cat("   3. Localizar DISCONTINUIDADES: ¿Dónde se corta la curva?\n")
      cat("   4. Extraer PUNTOS CLAVE: Inicio, fin, cambios de dirección\n")
      cat("   5. Identificar ELEMENTOS ADICIONALES: Líneas, etiquetas, puntos\n")
      cat("   6. Observar COLORES: ¿Qué color tiene la curva principal?\n")
      
      return(list(
        elementos_clave = c("ejes", "rango", "discontinuidades", "puntos_clave", "elementos_adicionales", "colores"),
        metodo = "observacion_directa",
        precision_requerida = "alta"
      ))
    },
    
    "barras" = {
      cat("🔍 GUÍA PARA GRÁFICOS DE BARRAS:\n")
      cat("   1. Contar NÚMERO DE BARRAS\n")
      cat("   2. Medir ALTURA de cada barra\n")
      cat("   3. Identificar ETIQUETAS de categorías\n")
      cat("   4. Determinar COLORES de cada barra\n")
      cat("   5. Localizar EJES y escalas\n")
      
      return(list(
        elementos_clave = c("numero_barras", "alturas", "etiquetas", "colores", "ejes"),
        metodo = "medicion_directa",
        precision_requerida = "media"
      ))
    },
    
    "circular" = {
      cat("🔍 GUÍA PARA GRÁFICOS CIRCULARES:\n")
      cat("   1. Contar NÚMERO DE SECTORES\n")
      cat("   2. Estimar ÁNGULOS de cada sector\n")
      cat("   3. Identificar COLORES de sectores\n")
      cat("   4. Localizar ETIQUETAS y valores\n")
      cat("   5. Determinar POSICIÓN de etiquetas\n")
      
      return(list(
        elementos_clave = c("numero_sectores", "angulos", "colores", "etiquetas", "posiciones"),
        metodo = "estimacion_angular",
        precision_requerida = "media"
      ))
    }
  )
}

#' Generar plantilla de extracción específica
generar_plantilla_extraccion <- function(tipo_grafico) {
  
  switch(tipo_grafico,
    "funcion" = {
      plantilla <- "
# PLANTILLA DE EXTRACCIÓN PARA FUNCIONES
# Completa observando la imagen:

EJES:
- Etiqueta X: _______________
- Etiqueta Y: _______________
- Rango X: [___, ___]
- Rango Y: [___, ___]

CURVA PRINCIPAL:
- Color: _______________
- Número de ramas: _______________
- Discontinuidades en X: _______________

PUNTOS CLAVE (observar y anotar coordenadas aproximadas):
Rama 1: (x1,y1), (x2,y2), (x3,y3), ...
Rama 2: (x1,y1), (x2,y2), (x3,y3), ...
Rama 3: (x1,y1), (x2,y2), (x3,y3), ...

ELEMENTOS ADICIONALES:
- Líneas punteadas: _______________
- Etiquetas especiales: _______________
- Puntos marcados: _______________
"
      cat(plantilla)
      return(plantilla)
    },
    
    "barras" = {
      plantilla <- "
# PLANTILLA DE EXTRACCIÓN PARA BARRAS
# Completa observando la imagen:

CONFIGURACIÓN:
- Número de barras: _______________
- Orientación: [horizontal/vertical]
- Ancho de barras: _______________

DATOS DE BARRAS:
Barra 1: Altura=___, Color=___, Etiqueta=___
Barra 2: Altura=___, Color=___, Etiqueta=___
Barra 3: Altura=___, Color=___, Etiqueta=___
...

EJES:
- Etiqueta X: _______________
- Etiqueta Y: _______________
"
      cat(plantilla)
      return(plantilla)
    }
  )
}

#' Generar código TikZ base según tipo
generar_codigo_tikz_base <- function(tipo_grafico) {
  
  switch(tipo_grafico,
    "funcion" = {
      codigo <- "
\\begin{tikzpicture}[scale=0.8]
\\begin{axis}[
    xlabel={ETIQUETA_X},
    ylabel={ETIQUETA_Y},
    xmin=XMIN, xmax=XMAX,
    ymin=YMIN, ymax=YMAX,
    grid=major,
    axis lines=left
]

% Rama 1 de la función
\\addplot[COLOR_PRINCIPAL, very thick, smooth] coordinates {
    COORDENADAS_RAMA_1
};

% Rama 2 de la función (si existe)
\\addplot[COLOR_PRINCIPAL, very thick, smooth] coordinates {
    COORDENADAS_RAMA_2
};

% Elementos adicionales
ELEMENTOS_ADICIONALES

\\end{axis}
\\end{tikzpicture}
"
      cat("📝 CÓDIGO TIKZ BASE PARA FUNCIONES:\n")
      cat(codigo)
      return(codigo)
    },
    
    "barras" = {
      codigo <- "
\\begin{tikzpicture}[scale=0.8]
\\begin{axis}[
    xlabel={ETIQUETA_X},
    ylabel={ETIQUETA_Y},
    ybar,
    symbolic x coords={ETIQUETAS_CATEGORIAS},
    xtick=data
]

\\addplot coordinates {
    COORDENADAS_BARRAS
};

\\end{axis}
\\end{tikzpicture}
"
      cat("📝 CÓDIGO TIKZ BASE PARA BARRAS:\n")
      cat(codigo)
      return(codigo)
    }
  )
}

#' Instrucciones de refinamiento
instrucciones_refinamiento <- function(tipo_grafico) {
  
  cat("🎯 PROCESO DE REFINAMIENTO:\n")
  cat("   1. REEMPLAZAR PLACEHOLDERS: Sustituir ETIQUETA_X, COORDENADAS, etc.\n")
  cat("   2. COMPILAR Y PROBAR: pdflatex archivo.tex\n")
  cat("   3. COMPARAR VISUAL: ¿Se parece a la imagen original?\n")
  cat("   4. AJUSTAR COORDENADAS: Refinar puntos que no coincidan\n")
  cat("   5. ITERAR: Repetir hasta lograr similitud satisfactoria\n")
  
  cat("\n💡 CONSEJOS PRÁCTICOS:\n")
  cat("   - Empezar con pocos puntos, agregar más gradualmente\n")
  cat("   - Usar 'smooth' para curvas suaves\n")
  cat("   - Probar diferentes valores de 'samples' si es necesario\n")
  cat("   - Validar cada cambio compilando inmediatamente\n")
}

#' FUNCIÓN DE CONVENIENCIA: Método completo para imagen cotangente
replicar_cotangente_visual <- function() {
  
  cat("🎯 MÉTODO ESPECÍFICO PARA IMAGEN COTANGENTE\n")
  cat("==========================================\n")
  
  # Análisis específico de la imagen cotangente
  analisis_cotangente <- list(
    ejes = list(x = "Ángulo α", y = "Distancia PK"),
    rango = list(x = c(0, 4.5), y = c(0, 5)),
    color_principal = "cyan",
    discontinuidades = c(1.57, 3.14),  # π/2, π
    lineas_punteadas = list(
      list(x = 1.5, etiqueta = "QP"),
      list(x = 3.0, etiqueta = "I₁")
    ),
    ramas = 3
  )
  
  cat("📊 ANÁLISIS COMPLETADO:\n")
  print(analisis_cotangente)
  
  # Código TikZ específico optimizado
  codigo_optimizado <- generar_codigo_cotangente_optimizado(analisis_cotangente)
  
  return(list(
    analisis = analisis_cotangente,
    codigo = codigo_optimizado
  ))
}

#' Generar código TikZ optimizado para cotangente
generar_codigo_cotangente_optimizado <- function(analisis) {
  
  codigo <- sprintf("
\\begin{tikzpicture}[scale=0.8]
\\begin{axis}[
    xlabel={%s},
    ylabel={%s},
    xmin=%g, xmax=%g,
    ymin=%g, ymax=%g,
    grid=major,
    axis lines=left
]

%% Curva cotangente en 3 ramas (coordenadas extraídas visualmente)
\\addplot[%s, very thick, smooth] coordinates {
    (0.1,4.8) (0.3,3.4) (0.5,2.5) (0.7,1.9) (0.9,1.5) (1.1,1.2) (1.3,1.0) (1.4,0.9)
};

\\addplot[%s, very thick, smooth] coordinates {
    (1.7,4.2) (1.9,2.9) (2.1,2.0) (2.3,1.4) (2.5,1.0) (2.7,0.7) (2.9,0.5) (3.0,0.4)
};

\\addplot[%s, very thick, smooth] coordinates {
    (3.2,3.8) (3.4,2.7) (3.6,1.9) (3.8,1.3) (4.0,0.9) (4.2,0.5) (4.4,0.3)
};

%% Líneas punteadas con etiquetas
\\draw[dashed, black, thick] (axis cs:%g,0) -- (axis cs:%g,5) node[above] {%s};
\\draw[dashed, black, thick] (axis cs:%g,0) -- (axis cs:%g,5) node[above] {$%s$};

\\end{axis}
\\end{tikzpicture}",
    analisis$ejes$x, analisis$ejes$y,
    analisis$rango$x[1], analisis$rango$x[2],
    analisis$rango$y[1], analisis$rango$y[2],
    analisis$color_principal, analisis$color_principal, analisis$color_principal,
    analisis$lineas_punteadas[[1]]$x, analisis$lineas_punteadas[[1]]$x, analisis$lineas_punteadas[[1]]$etiqueta,
    analisis$lineas_punteadas[[2]]$x, analisis$lineas_punteadas[[2]]$x, analisis$lineas_punteadas[[2]]$etiqueta
  )
  
  cat("🛠️ CÓDIGO TIKZ OPTIMIZADO GENERADO:\n")
  cat(codigo)
  
  return(codigo)
}

cat("✅ Herramienta de Extracción Visual cargada exitosamente\n")
cat("🔍 Funciones disponibles:\n")
cat("   - extraer_coordenadas_visual() - Método general\n")
cat("   - replicar_cotangente_visual() - Específico para cotangente\n")
cat("   - guia_observacion_visual() - Guías de observación\n")
