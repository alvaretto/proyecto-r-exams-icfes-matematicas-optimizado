# Template R para usar include_tikz() - Basado en Archivos Exitosos
# Fuente: logic_TikZ.Rmd
# Propósito: Máxima compatibilidad multi-formato con TikZ

# =============================================================================
# FUNCIÓN TEMPLATE PARA DIAGRAMAS TikZ COMPATIBLES
# =============================================================================

# Función para generar código TikZ como vector de strings
generar_tikz_compatible <- function(tipo_diagrama, parametros) {
  
  if (tipo_diagrama == "tabla_simple") {
    return(c(
      "\\begin{tikzpicture}",
      "\\node[inner sep=0pt] {",
      "  \\begin{tabular}{|c|c|}",
      "    \\hline",
      paste0("    \\textbf{", parametros$col1, "} & \\textbf{", parametros$col2, "} \\\\"),
      "    \\hline",
      paste0("    ", parametros$datos[1], " & ", parametros$datos[2], " \\\\"),
      "    \\hline",
      "  \\end{tabular}",
      "};",
      "\\end{tikzpicture}"
    ))
  }
  
  if (tipo_diagrama == "venn_dos_conjuntos") {
    return(c(
      "\\begin{tikzpicture}[scale=0.8]",
      "  \\begin{scope}[opacity=0.5]",
      "    \\fill[red] (-1,0) circle (1.5);",
      "    \\fill[blue] (1,0) circle (1.5);",
      "  \\end{scope}",
      paste0("  \\node at (-2.2,0) {", parametros$conjunto_A, "};"),
      paste0("  \\node at (2.2,0) {", parametros$conjunto_B, "};"),
      paste0("  \\node at (-1,0) {", parametros$solo_A, "};"),
      paste0("  \\node at (1,0) {", parametros$solo_B, "};"),
      paste0("  \\node at (0,0) {", parametros$interseccion, "};"),
      "\\end{tikzpicture}"
    ))
  }
  
  if (tipo_diagrama == "compuerta_logica") {
    return(c(
      "\\begin{tikzpicture}[thick]",
      paste0("  \\node[left,draw, logic gate inputs=nn, ", parametros$operador, " gate US,fill=none,scale=2.5] (G1) at (0,0) {};"),
      "  \\draw (G1.output) --++ (0.5,0) node[right] (y) {$y$};",
      "  \\draw (G1.input 1) --++ (-0.5,0) node[left] {$a$};",
      "  \\draw (G1.input 2) --++ (-0.5,0) node[left] {$b$};",
      "\\end{tikzpicture}"
    ))
  }
  
  # Agregar más tipos según necesidad
  stop("Tipo de diagrama no soportado: ", tipo_diagrama)
}

# =============================================================================
# FUNCIÓN PARA INTEGRAR CON R-EXAMS
# =============================================================================

crear_tikz_rexams <- function(tipo_diagrama, parametros, nombre = "diagrama", 
                              ancho = "4cm", bibliotecas = NULL) {
  
  # Obtener tipo de dispositivo actual
  typ <- match_exams_device()
  
  # Generar código TikZ
  codigo_tikz <- generar_tikz_compatible(tipo_diagrama, parametros)
  
  # Bibliotecas por defecto según tipo de diagrama
  if (is.null(bibliotecas)) {
    bibliotecas <- switch(tipo_diagrama,
      "tabla_simple" = c("array", "booktabs"),
      "venn_dos_conjuntos" = NULL,
      "compuerta_logica" = c("arrows", "shapes.gates.logic.US", "calc"),
      NULL
    )
  }
  
  # Usar include_tikz para máxima compatibilidad
  return(include_tikz(
    codigo_tikz,
    name = nombre,
    markup = "markdown",
    format = typ,
    library = bibliotecas,
    width = ancho
  ))
}

# =============================================================================
# EJEMPLOS DE USO
# =============================================================================

if (FALSE) {
  
  # Ejemplo 1: Tabla simple
  parametros_tabla <- list(
    col1 = "Variable",
    col2 = "Valor",
    datos = c("Altura", "175 cm")
  )
  
  imagen_tabla <- crear_tikz_rexams(
    tipo_diagrama = "tabla_simple",
    parametros = parametros_tabla,
    nombre = "tabla_datos",
    ancho = "6cm"
  )
  
  # Ejemplo 2: Diagrama de Venn
  parametros_venn <- list(
    conjunto_A = "Matemáticas",
    conjunto_B = "Física", 
    solo_A = 15,
    solo_B = 12,
    interseccion = 8
  )
  
  imagen_venn <- crear_tikz_rexams(
    tipo_diagrama = "venn_dos_conjuntos",
    parametros = parametros_venn,
    nombre = "venn_materias",
    ancho = "5cm"
  )
  
  # Ejemplo 3: Compuerta lógica
  parametros_compuerta <- list(
    operador = "and"  # or, and, xor, nand, nor
  )
  
  imagen_compuerta <- crear_tikz_rexams(
    tipo_diagrama = "compuerta_logica", 
    parametros = parametros_compuerta,
    nombre = "compuerta_and",
    ancho = "3cm"
  )
  
  # Uso en documento R Markdown:
  # ![Descripción](`r imagen_tabla`){width=6cm}
  # ![Diagrama de Venn](`r imagen_venn`){width=5cm}
  # ![Compuerta AND](`r imagen_compuerta`){width=3cm}
}

# =============================================================================
# VALIDACIÓN DE COMPATIBILIDAD
# =============================================================================

validar_tikz_template <- function(tipo_diagrama, parametros) {
  
  # Verificar que los parámetros requeridos estén presentes
  parametros_requeridos <- switch(tipo_diagrama,
    "tabla_simple" = c("col1", "col2", "datos"),
    "venn_dos_conjuntos" = c("conjunto_A", "conjunto_B", "solo_A", "solo_B", "interseccion"),
    "compuerta_logica" = c("operador"),
    character(0)
  )
  
  parametros_faltantes <- setdiff(parametros_requeridos, names(parametros))
  
  if (length(parametros_faltantes) > 0) {
    stop("Parámetros faltantes para ", tipo_diagrama, ": ", 
         paste(parametros_faltantes, collapse = ", "))
  }
  
  # Validaciones específicas
  if (tipo_diagrama == "compuerta_logica") {
    operadores_validos <- c("and", "or", "xor", "nand", "nor")
    if (!parametros$operador %in% operadores_validos) {
      stop("Operador no válido. Use: ", paste(operadores_validos, collapse = ", "))
    }
  }
  
  return(TRUE)
}

# =============================================================================
# CONFIGURACIÓN RECOMENDADA PARA CHUNK SETUP
# =============================================================================

# Agregar al chunk setup de archivos .Rmd:
configurar_tikz_rexams <- function() {
  
  # Configuración LaTeX para TikZ
  options(tikzLatex = "pdflatex")
  options(tikzXelatex = FALSE)
  options(tikzLatexPackages = c(
    "\\usepackage{tikz}",
    "\\usepackage{colortbl}",
    "\\usepackage{graphicx}",
    "\\usepackage{float}",
    "\\usepackage{booktabs}",
    "\\usepackage{array}",
    "\\usetikzlibrary{arrows,shapes.gates.logic.US,calc}"
  ))
  
  # Configuración de chunks
  knitr::opts_chunk$set(
    warning = FALSE,
    message = FALSE,
    fig.showtext = FALSE,
    fig.cap = "",
    fig.keep = 'all',
    dev = c("png", "pdf"),
    dpi = 150,
    fig.pos = "H"
  )
  
  cat("✅ Configuración TikZ R-exams aplicada exitosamente\n")
}

# Mensaje de carga
cat("📊 Template TikZ R-exams cargado exitosamente\n")
cat("💡 Uso: crear_tikz_rexams('tipo_diagrama', parametros)\n")
cat("🔧 Configurar: configurar_tikz_rexams()\n")
cat("📚 Tipos disponibles: tabla_simple, venn_dos_conjuntos, compuerta_logica\n")