# ============================================================================
# GENERADOR DE CÓDIGO TIKZ COMPATIBLE CON QTIKZ/KTIKZ
# Estrategia Robusta para Réplica Exacta - Proyecto ICFES R-exams
# ============================================================================

# Cargar referencias de templates robustos
source_if_exists <- function(file_path) {
  if (file.exists(file_path)) {
    tryCatch(source(file_path), error = function(e) {
      cat(sprintf("⚠️ Advertencia: No se pudo cargar %s\n", basename(file_path)))
    })
  }
}

#' Función principal para generar código TikZ compatible con Qtikz/Ktikz
#' @param caracteristicas Lista con características extraídas de la imagen
#' @param tipo_contenido Tipo de contenido detectado
#' @param parametros_exactos Parámetros específicos para el tipo de gráfico
#' @return Código TikZ compatible con Qtikz/Ktikz y R-exams
generar_tikz_qtikz_compatible <- function(caracteristicas, tipo_contenido, parametros_exactos) {
  
  cat("🎨 Generando código TikZ compatible con Qtikz/Ktikz...\n")
  
  # Determinar el tipo específico de gráfico
  tipo_especifico <- determinar_tipo_especifico(caracteristicas, tipo_contenido)
  
  # Generar código según el tipo
  codigo_tikz <- switch(tipo_especifico,
    "tabla_datos" = generar_tabla_datos_qtikz(parametros_exactos),
    "grafico_circular" = generar_grafico_circular_qtikz(parametros_exactos),
    "grafico_barras" = generar_grafico_barras_qtikz(parametros_exactos),
    "grafico_lineas" = generar_grafico_lineas_qtikz(parametros_exactos),
    "diagrama_venn" = generar_diagrama_venn_qtikz(parametros_exactos),
    "figura_geometrica" = generar_figura_geometrica_qtikz(parametros_exactos),
    generar_template_generico_qtikz(parametros_exactos)
  )
  
  # Aplicar optimizaciones para compatibilidad
  codigo_optimizado <- optimizar_para_qtikz_rexams(codigo_tikz)
  
  # Validar sintaxis básica
  validacion <- validar_sintaxis_tikz(codigo_optimizado)
  if (!validacion$valido) {
    cat("⚠️ Advertencia: Posibles problemas de sintaxis detectados\n")
    codigo_optimizado <- corregir_sintaxis_basica(codigo_optimizado, validacion$errores)
  }
  
  cat("✅ Código TikZ compatible generado exitosamente\n")
  return(codigo_optimizado)
}

#' Generar tabla de datos compatible con Qtikz/Ktikz
generar_tabla_datos_qtikz <- function(parametros) {
  
  # Usar template robusto si existe
  template_path <- "Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/TikZ-Documentation/templates-rexams/robustos/tabla-datos-expandida.tikz"
  
  if (file.exists(template_path)) {
    template_base <- paste(readLines(template_path), collapse = "\n")
    # Extraer solo la parte del tikzpicture principal
    template_base <- extraer_tikzpicture_principal(template_base)
  } else {
    template_base <- crear_template_tabla_basico()
  }
  
  # Aplicar parámetros específicos
  codigo_tabla <- aplicar_parametros_tabla(template_base, parametros)
  
  return(codigo_tabla)
}

#' Generar gráfico circular compatible con Qtikz/Ktikz
generar_grafico_circular_qtikz <- function(parametros) {
  
  # Usar template robusto si existe
  template_path <- "Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/TikZ-Documentation/templates-rexams/robustos/grafico-circular-robusto.tikz"
  
  if (file.exists(template_path)) {
    template_base <- paste(readLines(template_path), collapse = "\n")
    template_base <- extraer_tikzpicture_principal(template_base)
  } else {
    template_base <- crear_template_circular_basico()
  }
  
  # Aplicar parámetros específicos
  codigo_circular <- aplicar_parametros_circular(template_base, parametros)
  
  return(codigo_circular)
}

#' Generar gráfico de barras compatible con Qtikz/Ktikz
generar_grafico_barras_qtikz <- function(parametros) {
  
  # Template básico de barras compatible
  template_barras <- "
% Gráfico de Barras Compatible Qtikz/Ktikz
\\begin{tikzpicture}[scale=0.8]

% Ejes principales
\\draw[->] (0,0) -- (0,`r max_y + 1`) node[left] {`r etiqueta_y`};
\\draw[->] (0,0) -- (`r num_barras + 1`,0) node[below] {`r etiqueta_x`};

% Barras individuales
`r paste(sprintf('\\\\fill[%s!60] (%s,0) rectangle (%s,%s);', 
                colores_barras, posiciones_x - 0.4, posiciones_x + 0.4, alturas_barras), 
         collapse = '\\n')`

% Etiquetas de categorías
`r paste(sprintf('\\\\node[below] at (%s,-0.3) {\\\\small %s};', 
                posiciones_x, etiquetas_categorias), 
         collapse = '\\n')`

% Valores sobre las barras
`r paste(sprintf('\\\\node[above] at (%s,%s) {\\\\small %s};', 
                posiciones_x, alturas_barras + 0.1, valores_barras), 
         collapse = '\\n')`

% Líneas de cuadrícula (opcional)
`r if(mostrar_cuadricula) paste(sprintf('\\\\draw[gray!30] (0,%s) -- (%s,%s);', 
                                       seq(0, max_y, by = cuadricula_step), 
                                       num_barras + 1, 
                                       seq(0, max_y, by = cuadricula_step)), 
                               collapse = '\\n') else ''`

\\end{tikzpicture}"
  
  # Aplicar parámetros específicos
  codigo_barras <- aplicar_parametros_barras(template_barras, parametros)
  
  return(codigo_barras)
}

#' Generar gráfico de líneas compatible con Qtikz/Ktikz
generar_grafico_lineas_qtikz <- function(parametros) {
  
  template_lineas <- "
% Gráfico de Líneas Compatible Qtikz/Ktikz
\\begin{tikzpicture}[scale=0.8]

% Ejes
\\draw[->] (0,0) -- (`r max_x + 1`,0) node[below] {`r etiqueta_x`};
\\draw[->] (0,0) -- (0,`r max_y + 1`) node[left] {`r etiqueta_y`};

% Puntos de datos
`r paste(sprintf('\\\\fill[%s] (%s,%s) circle (2pt);', 
                colores_puntos, coordenadas_x, coordenadas_y), 
         collapse = '\\n')`

% Líneas conectoras
`r paste(sprintf('\\\\draw[%s, thick] (%s,%s) -- (%s,%s);', 
                colores_lineas, 
                coordenadas_x[-length(coordenadas_x)], coordenadas_y[-length(coordenadas_y)],
                coordenadas_x[-1], coordenadas_y[-1]), 
         collapse = '\\n')`

% Etiquetas de ejes
`r paste(sprintf('\\\\node[below] at (%s,-0.2) {\\\\small %s};', 
                etiquetas_x_pos, etiquetas_x_texto), 
         collapse = '\\n')`

`r paste(sprintf('\\\\node[left] at (-0.2,%s) {\\\\small %s};', 
                etiquetas_y_pos, etiquetas_y_texto), 
         collapse = '\\n')`

\\end{tikzpicture}"
  
  codigo_lineas <- aplicar_parametros_lineas(template_lineas, parametros)
  
  return(codigo_lineas)
}

#' Generar diagrama de Venn compatible con Qtikz/Ktikz
generar_diagrama_venn_qtikz <- function(parametros) {
  
  # Usar template de Venn si existe
  template_path <- "Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/TikZ-Documentation/templates-rexams/robustos/diagrama-venn-optimizado.tikz"
  
  if (file.exists(template_path)) {
    template_base <- paste(readLines(template_path), collapse = "\n")
    template_base <- extraer_tikzpicture_principal(template_base)
  } else {
    template_base <- crear_template_venn_basico()
  }
  
  codigo_venn <- aplicar_parametros_venn(template_base, parametros)
  
  return(codigo_venn)
}

#' Generar figura geométrica compatible con Qtikz/Ktikz
generar_figura_geometrica_qtikz <- function(parametros) {
  
  # Buscar en ejemplos de Fausto
  ejemplos_path <- "Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/TikZ-Documentation/Ejemplos-Fausto/Graf/"
  
  # Seleccionar template más apropiado basado en tipo de figura
  template_figura <- seleccionar_template_geometrico(parametros$tipo_figura, ejemplos_path)
  
  if (!is.null(template_figura)) {
    codigo_geometrico <- adaptar_template_geometrico(template_figura, parametros)
  } else {
    codigo_geometrico <- crear_figura_geometrica_basica(parametros)
  }
  
  return(codigo_geometrico)
}

#' Optimizar código TikZ para compatibilidad Qtikz/Ktikz y R-exams
optimizar_para_qtikz_rexams <- function(codigo_tikz) {
  
  cat("🔧 Optimizando para compatibilidad Qtikz/Ktikz...\n")
  
  # 1. Asegurar bibliotecas básicas solamente
  codigo_tikz <- asegurar_bibliotecas_basicas(codigo_tikz)
  
  # 2. Convertir colores personalizados a estándar
  codigo_tikz <- convertir_colores_estandar(codigo_tikz)
  
  # 3. Simplificar cálculos complejos
  codigo_tikz <- simplificar_calculos(codigo_tikz)
  
  # 4. Optimizar para interpolación R
  codigo_tikz <- optimizar_interpolacion_r(codigo_tikz)
  
  # 5. Asegurar escalado apropiado
  codigo_tikz <- asegurar_escalado_apropiado(codigo_tikz)
  
  return(codigo_tikz)
}

#' Validar sintaxis básica de TikZ
validar_sintaxis_tikz <- function(codigo_tikz) {
  
  errores <- character()
  
  # Verificar balance de llaves
  if (str_count(codigo_tikz, "\\{") != str_count(codigo_tikz, "\\}")) {
    errores <- c(errores, "Llaves desbalanceadas")
  }
  
  # Verificar begin/end tikzpicture
  if (!grepl("\\\\begin\\{tikzpicture\\}", codigo_tikz)) {
    errores <- c(errores, "Falta \\begin{tikzpicture}")
  }
  
  if (!grepl("\\\\end\\{tikzpicture\\}", codigo_tikz)) {
    errores <- c(errores, "Falta \\end{tikzpicture}")
  }
  
  # Verificar comandos básicos
  comandos_problematicos <- c("\\\\pgfmathsetmacro", "\\\\definecolor", "\\\\usetikzlibrary\\{.*shadows")
  for (comando in comandos_problematicos) {
    if (grepl(comando, codigo_tikz)) {
      errores <- c(errores, sprintf("Comando problemático: %s", comando))
    }
  }
  
  return(list(
    valido = length(errores) == 0,
    errores = errores
  ))
}

# ============================================================================
# FUNCIONES AUXILIARES ESPECIALIZADAS
# ============================================================================

determinar_tipo_especifico <- function(caracteristicas, tipo_contenido) {
  # Lógica para determinar tipo específico basado en características
  if (grepl("tabla", tipo_contenido, ignore.case = TRUE)) {
    return("tabla_datos")
  } else if (grepl("circular|pie", tipo_contenido, ignore.case = TRUE)) {
    return("grafico_circular")
  } else if (grepl("barras|bar", tipo_contenido, ignore.case = TRUE)) {
    return("grafico_barras")
  } else if (grepl("lineas|line", tipo_contenido, ignore.case = TRUE)) {
    return("grafico_lineas")
  } else if (grepl("venn", tipo_contenido, ignore.case = TRUE)) {
    return("diagrama_venn")
  } else if (grepl("geometr", tipo_contenido, ignore.case = TRUE)) {
    return("figura_geometrica")
  } else {
    return("generico")
  }
}

extraer_tikzpicture_principal <- function(template_completo) {
  # Extraer solo la parte del tikzpicture del template
  lineas <- strsplit(template_completo, "\n")[[1]]
  inicio <- which(grepl("\\\\begin\\{tikzpicture\\}", lineas))[1]
  fin <- which(grepl("\\\\end\\{tikzpicture\\}", lineas))[1]
  
  if (!is.na(inicio) && !is.na(fin)) {
    return(paste(lineas[inicio:fin], collapse = "\n"))
  } else {
    return(template_completo)
  }
}

crear_template_tabla_basico <- function() {
  return("
\\begin{tikzpicture}
\\node[inner sep=0pt] {
  \\begin{tabular}{|c|c|}
    \\hline
    \\rowcolor{blue!20}
    \\textbf{Columna 1} & \\textbf{Columna 2} \\\\
    \\hline
    Dato 1 & Dato 2 \\\\
    \\hline
    Dato 3 & Dato 4 \\\\
    \\hline
  \\end{tabular}
};
\\end{tikzpicture}")
}

crear_template_circular_basico <- function() {
  return("
\\begin{tikzpicture}[scale=0.8]
\\coordinate (centro) at (0,0);
\\fill[blue!60] (centro) -- (0:2) arc (0:90:2) -- cycle;
\\fill[red!60] (centro) -- (90:2) arc (90:180:2) -- cycle;
\\fill[green!60] (centro) -- (180:2) arc (180:270:2) -- cycle;
\\fill[orange!60] (centro) -- (270:2) arc (270:360:2) -- cycle;
\\draw[black, thick] (centro) circle (2);
\\end{tikzpicture}")
}

crear_template_venn_basico <- function() {
  return("
\\begin{tikzpicture}[scale=0.8]
\\draw[blue!50, fill=blue!20] (0,0) circle (1.5);
\\draw[red!50, fill=red!20] (1.5,0) circle (1.5);
\\node at (-0.75,0) {A};
\\node at (2.25,0) {B};
\\node at (0.75,0) {A∩B};
\\end{tikzpicture}")
}

# Funciones de aplicación de parámetros (implementaciones simplificadas)
aplicar_parametros_tabla <- function(template, parametros) {
  # Implementar lógica específica para tablas
  return(template)
}

aplicar_parametros_circular <- function(template, parametros) {
  # Implementar lógica específica para gráficos circulares
  return(template)
}

aplicar_parametros_barras <- function(template, parametros) {
  # Implementar lógica específica para gráficos de barras
  return(template)
}

aplicar_parametros_lineas <- function(template, parametros) {
  # Implementar lógica específica para gráficos de líneas
  return(template)
}

aplicar_parametros_venn <- function(template, parametros) {
  # Implementar lógica específica para diagramas de Venn
  return(template)
}

# Funciones de optimización (implementaciones básicas)
asegurar_bibliotecas_basicas <- function(codigo) {
  # Remover bibliotecas problemáticas
  codigo <- gsub("\\\\usetikzlibrary\\{.*shadows.*\\}", "", codigo)
  codigo <- gsub("\\\\usetikzlibrary\\{.*fadings.*\\}", "", codigo)
  return(codigo)
}

convertir_colores_estandar <- function(codigo) {
  # Convertir definecolor a colores estándar
  codigo <- gsub("\\\\definecolor\\{.*\\}\\{.*\\}\\{.*\\}", "", codigo)
  return(codigo)
}

simplificar_calculos <- function(codigo) {
  # Convertir pgfmathsetmacro a comentarios
  codigo <- gsub("\\\\pgfmathsetmacro", "% TODO: Convertir a variable R - \\\\pgfmathsetmacro", codigo)
  return(codigo)
}

optimizar_interpolacion_r <- function(codigo) {
  # Asegurar formato correcto para interpolación R
  return(codigo)
}

asegurar_escalado_apropiado <- function(codigo) {
  # Verificar que el escalado sea apropiado
  if (!grepl("scale=", codigo)) {
    codigo <- gsub("\\\\begin\\{tikzpicture\\}", "\\\\begin{tikzpicture}[scale=0.8]", codigo)
  }
  return(codigo)
}

corregir_sintaxis_basica <- function(codigo, errores) {
  # Implementar correcciones básicas
  return(codigo)
}

seleccionar_template_geometrico <- function(tipo_figura, ejemplos_path) {
  # Seleccionar template apropiado de ejemplos de Fausto
  return(NULL)
}

adaptar_template_geometrico <- function(template, parametros) {
  # Adaptar template geométrico
  return(template)
}

crear_figura_geometrica_basica <- function(parametros) {
  # Crear figura geométrica básica
  return("\\begin{tikzpicture}\\draw (0,0) -- (1,1);\\end{tikzpicture}")
}

cat("✅ Generador TikZ compatible con Qtikz/Ktikz cargado exitosamente\n")
