# =============================================================================
# RÉPLICA TIKZ PIXEL-PERFECT: Lab/02-Geometria/all.png
# =============================================================================
# Descripción: Función para generar réplica TikZ exacta de all.png (cilindro hueco)
# Objetivo: Lograr 95%+ de fidelidad visual con la imagen original
# Fecha: 2025-01-27
# =============================================================================

library(exams)

# Función para generar código TikZ personalizado para cilindro hueco
generar_tikz_cilindro_lab02 <- function(altura_cilindro = 6, 
                                        radio_interno = 1, 
                                        grosor_pared = 1,
                                        escala_grafico = 1.0,
                                        mostrar_etiquetas = TRUE) {
  
  # Calcular radio externo
  radio_externo <- radio_interno + grosor_pared
  
  # Generar código TikZ
  codigo_tikz <- c(
    "\\begin{tikzpicture}[",
    paste0("  scale=", escala_grafico, ","),
    "  thick,",
    "  >=stealth",
    "]",
    "",
    "% Coordenadas principales",
    "\\coordinate (centro_base) at (0,0);",
    paste0("\\coordinate (centro_tapa) at (0,", altura_cilindro, ");"),
    "",
    "% Puntos clave base inferior",
    paste0("\\coordinate (ext_der_base) at (", radio_externo, ", 0);"),
    paste0("\\coordinate (ext_izq_base) at (", -radio_externo, ", 0);"),
    paste0("\\coordinate (int_der_base) at (", radio_interno, ", 0);"),
    paste0("\\coordinate (int_izq_base) at (", -radio_interno, ", 0);"),
    "",
    "% Puntos clave tapa superior",
    paste0("\\coordinate (ext_der_tapa) at (", radio_externo, ", ", altura_cilindro, ");"),
    paste0("\\coordinate (ext_izq_tapa) at (", -radio_externo, ", ", altura_cilindro, ");"),
    paste0("\\coordinate (int_der_tapa) at (", radio_interno, ", ", altura_cilindro, ");"),
    paste0("\\coordinate (int_izq_tapa) at (", -radio_interno, ", ", altura_cilindro, ");"),
    "",
    "% Perspectiva elíptica (valores fijos para compatibilidad)",
    "\\def\\perspectiva{0.4}",
    "\\def\\perspectivaTapa{0.5}",
    "",
    "% === PARTES OCULTAS (líneas punteadas) ===",
    "% Arcos traseros base",
    paste0("\\draw[dashed, gray] (ext_der_base) arc (0:180:{", radio_externo, "} and {\\perspectiva});"),
    paste0("\\draw[dashed, gray] (int_der_base) arc (0:180:{", radio_interno, "} and {\\perspectiva*0.7});"),
    "",
    "% Líneas verticales internas ocultas",
    "\\draw[dashed, gray] (int_izq_base) -- (int_izq_tapa);",
    "\\draw[dashed, gray] (int_der_base) -- (int_der_tapa);",
    "",
    "% === PARTES VISIBLES ===",
    "% Líneas verticales externas",
    "\\draw[black, very thick] (ext_izq_base) -- (ext_izq_tapa);",
    "\\draw[black, very thick] (ext_der_base) -- (ext_der_tapa);",
    "",
    "% Arcos frontales base (visibles)",
    paste0("\\draw[black, very thick] (ext_der_base) arc (0:-180:{", radio_externo, "} and {\\perspectiva});"),
    paste0("\\draw[black, very thick] (int_der_base) arc (0:-180:{", radio_interno, "} and {\\perspectiva*0.7});"),
    "",
    "% Elipses superiores (tapa)",
    paste0("\\draw[black, very thick] (centro_tapa) ellipse ({", radio_externo, "} and {\\perspectivaTapa});"),
    paste0("\\draw[black, very thick] (centro_tapa) ellipse ({", radio_interno, "} and {\\perspectivaTapa*0.7});"),
    "",
    "% === PUNTOS CENTRALES ===",
    "\\fill[black] (centro_base) circle (1pt);",
    "\\fill[black] (centro_tapa) circle (1pt);"
  )
  
  # Agregar etiquetas si están habilitadas
  if (mostrar_etiquetas) {
    codigo_tikz <- c(codigo_tikz,
      "",
      "% === ETIQUETAS Y MEDIDAS ===",
      "% Altura",
      paste0("\\draw[|<->|, black] (", radio_externo + 0.4, ", 0) -- "),
      paste0("                    (", radio_externo + 0.4, ", ", altura_cilindro, ");"),
      paste0("\\node[right, black] at (", radio_externo + 0.5, ", ", altura_cilindro/2, ")"),
      paste0("     {\\small Altura};"),
      "",
      "% Radio interno",
      "\\draw[<->, black, thick] (centro_tapa) -- (int_der_tapa);",
      paste0("\\node[above right, black] at (", radio_interno/2, ", ", altura_cilindro + 0.3, ")"),
      paste0("     {\\small Radio interno = ", radio_interno, " m};"),
      "",
      "% Grosor",
      "\\draw[|<->|, black] (int_izq_tapa) -- (ext_izq_tapa);",
      paste0("\\node[above left, black] at (", -(radio_interno + grosor_pared/2), ", ", altura_cilindro + 0.4, ")"),
      paste0("     {\\small Grosor = ", grosor_pared, " m};"),
      "",
      "% Diámetro externo",
      paste0("\\node[above, black] at (0, ", altura_cilindro + 0.8, ")"),
      "     {\\small Diámetro externo};",
      "",
      "% Radio externo",
      paste0("\\node[below right, black] at (", radio_externo/2, ", -0.3)"),
      paste0("     {\\small Radio externo};")
    )
  }
  
  codigo_tikz <- c(codigo_tikz,
    "",
    "\\end{tikzpicture}"
  )
  
  return(codigo_tikz)
}

# Función para crear imagen TikZ compatible con R-exams
crear_replica_tikz_lab02 <- function(altura_cilindro = 6,
                                     radio_interno = 1,
                                     grosor_pared = 1,
                                     nombre = "replica_lab02",
                                     ancho = "10cm") {
  
  # Obtener tipo de dispositivo actual
  typ <- match_exams_device()
  
  # Generar código TikZ
  codigo_tikz <- generar_tikz_cilindro_lab02(altura_cilindro, radio_interno, grosor_pared)
  
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

# Función para generar datos aleatorios como en el original
generar_datos_cilindro_lab02 <- function() {
  # Basado en el patrón del código original
  altura_cilindro <- sample(c(4, 5, 6, 7, 8), 1)
  radio_interno <- round(runif(1, 0.8, 1.5), 1)
  grosor_pared <- round(runif(1, 0.3, 0.8), 1)
  
  return(list(
    altura = altura_cilindro,
    radio_interno = radio_interno,
    grosor = grosor_pared,
    radio_externo = radio_interno + grosor_pared
  ))
}

# Función de prueba para validar la réplica
validar_replica_lab02 <- function() {
  cat("🔍 Validando réplica TikZ Lab/02-Geometria...\n")
  
  # Datos de prueba basados en el patrón observado
  datos <- generar_datos_cilindro_lab02()
  
  tryCatch({
    imagen <- crear_replica_tikz_lab02(
      altura_cilindro = datos$altura,
      radio_interno = datos$radio_interno,
      grosor_pared = datos$grosor
    )
    cat("✅ Réplica TikZ generada exitosamente\n")
    cat("📏 Altura:", datos$altura, "m\n")
    cat("🔵 Radio interno:", datos$radio_interno, "m\n")
    cat("📐 Grosor:", datos$grosor, "m\n")
    cat("🔴 Radio externo:", round(datos$radio_externo, 1), "m\n")
    return(imagen)
  }, error = function(e) {
    cat("❌ Error en la generación:", e$message, "\n")
    return(NULL)
  })
}

# Mensaje de carga
cat("🎯 Réplica TikZ Lab/02-Geometria cargada exitosamente\n")
cat("💡 Uso: crear_replica_tikz_lab02(altura, radio_interno, grosor)\n")
cat("🔧 Validar: validar_replica_lab02()\n")
