# =============================================================================
# RÉPLICA TIKZ PIXEL-PERFECT: Lab/19/all.png
# =============================================================================
# Descripción: Función para generar réplica TikZ exacta de all.png (gráfico circular)
# Objetivo: Lograr 95%+ de fidelidad visual con la imagen original
# Fecha: 2025-01-27
# =============================================================================

library(exams)

# Función para generar código TikZ personalizado para gráfico circular
generar_tikz_circular_lab19 <- function(sabores, porcentajes, 
                                        colores = c("31,119,180", "255,127,14", "44,160,44", "214,39,40", "148,103,189"),
                                        radio = 2.5,
                                        mostrar_sombra = TRUE) {
  
  # Verificar que los porcentajes sumen 100
  if (abs(sum(porcentajes) - 100) > 0.1) {
    stop("Los porcentajes deben sumar 100%")
  }
  
  # Calcular ángulos acumulativos (comenzando desde 90 grados como matplotlib)
  angulo_inicio <- 90
  angulos_acum <- cumsum(porcentajes * 360 / 100)
  angulos_acum <- c(angulo_inicio, angulo_inicio - angulos_acum)
  
  # Generar código TikZ
  codigo_tikz <- c(
    "\\begin{tikzpicture}[scale=1.2]",
    "  % Configuración de colores (paleta matplotlib Paired)"
  )
  
  # Definir colores
  for (i in 1:length(colores)) {
    codigo_tikz <- c(codigo_tikz,
      paste0("  \\definecolor{color", i, "}{RGB}{", colores[i], "}")
    )
  }
  
  codigo_tikz <- c(codigo_tikz,
    "  \\definecolor{colorborde}{RGB}{255,255,255}",
    "  \\definecolor{colortext}{RGB}{0,0,0}",
    "  \\definecolor{colorsombra}{RGB}{128,128,128}",
    "",
    "  % Centro del círculo",
    "  \\coordinate (centro) at (0,0);",
    paste0("  \\def\\radio{", radio, "}")
  )
  
  # Agregar sombra si está habilitada
  if (mostrar_sombra) {
    codigo_tikz <- c(codigo_tikz,
      "",
      "  % Sombra del gráfico (efecto shadow de matplotlib)",
      "  \\begin{scope}[xshift=0.1cm, yshift=-0.1cm]",
      "    \\fill[colorsombra, opacity=0.3] (centro) circle (\\radio);",
      "  \\end{scope}"
    )
  }
  
  # Generar segmentos del gráfico circular
  codigo_tikz <- c(codigo_tikz, "")
  for (i in 1:length(sabores)) {
    angulo_ini <- angulos_acum[i]
    angulo_fin <- angulos_acum[i + 1]
    
    codigo_tikz <- c(codigo_tikz,
      paste0("  % Segmento ", i, " (", porcentajes[i], "%)"),
      paste0("  \\fill[color", i, "] (centro) -- (", angulo_ini, ":\\radio)"),
      paste0("    arc (", angulo_ini, ":", angulo_fin, ":\\radio) -- cycle;"),
      paste0("  \\draw[colorborde, thick] (centro) -- (", angulo_ini, ":\\radio)"),
      paste0("    arc (", angulo_ini, ":", angulo_fin, ":\\radio) -- cycle;"),
      ""
    )
  }
  
  # Calcular posiciones para porcentajes (dentro de segmentos)
  codigo_tikz <- c(codigo_tikz,
    "  % Etiquetas de porcentajes (dentro de cada segmento)"
  )
  
  for (i in 1:length(sabores)) {
    angulo_ini <- angulos_acum[i]
    angulo_fin <- angulos_acum[i + 1]
    angulo_medio <- (angulo_ini + angulo_fin) / 2
    
    codigo_tikz <- c(codigo_tikz,
      paste0("  \\coordinate (pos", i, ") at (", angulo_medio, ":{\\radio*0.7});")
    )
  }
  
  # Agregar porcentajes en blanco y negrita
  codigo_tikz <- c(codigo_tikz, "")
  for (i in 1:length(sabores)) {
    codigo_tikz <- c(codigo_tikz,
      paste0("  \\node[white, font=\\bfseries\\large] at (pos", i, ") {", porcentajes[i], "\\%};")
    )
  }
  
  # Calcular posiciones para etiquetas de sabores (fuera del círculo)
  codigo_tikz <- c(codigo_tikz,
    "",
    "  % Etiquetas de sabores (fuera del círculo)"
  )
  
  for (i in 1:length(sabores)) {
    angulo_ini <- angulos_acum[i]
    angulo_fin <- angulos_acum[i + 1]
    angulo_medio <- (angulo_ini + angulo_fin) / 2
    
    codigo_tikz <- c(codigo_tikz,
      paste0("  \\coordinate (label", i, ") at (", angulo_medio, ":{\\radio*1.3});")
    )
  }
  
  # Agregar etiquetas de sabores
  codigo_tikz <- c(codigo_tikz, "")
  for (i in 1:length(sabores)) {
    codigo_tikz <- c(codigo_tikz,
      paste0("  \\node[colortext, font=\\bfseries] at (label", i, ") {", sabores[i], "};")
    )
  }
  
  # Agregar líneas conectoras
  codigo_tikz <- c(codigo_tikz,
    "",
    "  % Líneas conectoras desde el borde del círculo a las etiquetas"
  )
  
  for (i in 1:length(sabores)) {
    angulo_ini <- angulos_acum[i]
    angulo_fin <- angulos_acum[i + 1]
    angulo_medio <- (angulo_ini + angulo_fin) / 2
    
    codigo_tikz <- c(codigo_tikz,
      paste0("  \\draw[colortext, thin] (", angulo_medio, ":\\radio) -- (label", i, ");")
    )
  }
  
  codigo_tikz <- c(codigo_tikz,
    "",
    "\\end{tikzpicture}"
  )
  
  return(codigo_tikz)
}

# Función para crear imagen TikZ compatible con R-exams
crear_replica_tikz_lab19 <- function(sabores = c("Naranja", "Limón", "Fresa", "Uva", "Manzana"),
                                     porcentajes = c(30, 25, 20, 15, 10),
                                     nombre = "replica_lab19",
                                     ancho = "10cm") {
  
  # Obtener tipo de dispositivo actual
  typ <- match_exams_device()
  
  # Generar código TikZ
  codigo_tikz <- generar_tikz_circular_lab19(sabores, porcentajes)
  
  # Usar include_tikz para máxima compatibilidad
  return(include_tikz(
    codigo_tikz,
    name = nombre,
    markup = "markdown",
    format = typ,
    library = NULL,  # Sin bibliotecas externas
    width = ancho
  ))
}

# Función de prueba para validar la réplica
validar_replica_lab19 <- function() {
  cat("🔍 Validando réplica TikZ Lab/19...\n")
  
  # Datos de prueba basados en el patrón observado
  sabores <- c("Naranja", "Limón", "Fresa", "Uva", "Manzana")
  porcentajes <- c(30, 25, 20, 15, 10)
  
  tryCatch({
    imagen <- crear_replica_tikz_lab19(sabores, porcentajes)
    cat("✅ Réplica TikZ generada exitosamente\n")
    cat("🍊 Sabores:", paste(sabores, collapse=", "), "\n")
    cat("📊 Porcentajes:", paste(porcentajes, collapse="%, "), "%\n")
    cat("🎯 Total:", sum(porcentajes), "%\n")
    return(imagen)
  }, error = function(e) {
    cat("❌ Error en la generación:", e$message, "\n")
    return(NULL)
  })
}

# Mensaje de carga
cat("🎯 Réplica TikZ Lab/19 cargada exitosamente\n")
cat("💡 Uso: crear_replica_tikz_lab19(sabores, porcentajes)\n")
cat("🔧 Validar: validar_replica_lab19()\n")
