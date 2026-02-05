# =============================================================================
# RÉPLICA TIKZ PIXEL-PERFECT: Lab/17/all.png
# =============================================================================
# Descripción: Función para generar réplica TikZ exacta de all.png
# Objetivo: Lograr 95%+ de fidelidad visual con la imagen original
# Fecha: 2025-01-27
# =============================================================================

library(exams)

# Función para generar código TikZ personalizado basado en datos reales
generar_tikz_lab17 <- function(datos_tiempo, datos_combustible, datos_distancia,
                               color_combustible = "31,119,180", 
                               color_distancia = "255,127,14",
                               titulo_x = "Tiempo (horas)",
                               titulo_y = "Valores") {
  
  # Normalizar datos para el gráfico (0-1)
  norm_combustible <- (datos_combustible - min(datos_combustible)) / 
                      (max(datos_combustible) - min(datos_combustible))
  norm_distancia <- (datos_distancia - min(datos_distancia)) / 
                    (max(datos_distancia) - min(datos_distancia))
  norm_tiempo <- (datos_tiempo - min(datos_tiempo)) / 
                 (max(datos_tiempo) - min(datos_tiempo))
  
  # Calcular coordenadas para TikZ
  ancho <- 8
  alto <- 6
  margenx <- 1.2
  margeny <- 1
  
  # Coordenadas de puntos combustible
  coords_combustible <- paste0(
    "(", margenx + norm_tiempo * ancho, ",", 
    margeny + norm_combustible * alto, ")"
  )
  
  # Coordenadas de puntos distancia  
  coords_distancia <- paste0(
    "(", margenx + norm_tiempo * ancho, ",",
    margeny + norm_distancia * alto, ")"
  )
  
  # Generar código TikZ
  codigo_tikz <- c(
    "\\begin{tikzpicture}[scale=0.8]",
    paste0("  \\definecolor{colorlinea1}{RGB}{", color_combustible, "}"),
    paste0("  \\definecolor{colorlinea2}{RGB}{", color_distancia, "}"),
    "  \\definecolor{colorgrid}{RGB}{176,176,176}",
    "  \\definecolor{colortext}{RGB}{0,0,0}",
    "",
    "  % Dimensiones del gráfico",
    paste0("  \\def\\ancho{", ancho, "}"),
    paste0("  \\def\\alto{", alto, "}"),
    paste0("  \\def\\margenx{", margenx, "}"),
    paste0("  \\def\\margeny{", margeny, "}"),
    "",
    "  % Coordenadas del área de ploteo",
    "  \\coordinate (origen) at (\\margenx,\\margeny);",
    "  \\coordinate (esquinasup) at (\\margenx+\\ancho,\\margeny+\\alto);",
    "",
    "  % Marco del gráfico",
    "  \\draw[thick] (origen) rectangle (esquinasup);",
    "",
    "  % Grid (líneas de cuadrícula)",
    "  \\foreach \\x in {0,1,2,3,4} {",
    "    \\draw[colorgrid, thin, dashed]",
    "      (\\margenx+\\x*\\ancho/4, \\margeny) --",
    "      (\\margenx+\\x*\\ancho/4, \\margeny+\\alto);",
    "  }",
    "  \\foreach \\y in {0,1,2,3,4,5} {",
    "    \\draw[colorgrid, thin, dashed]",
    "      (\\margenx, \\margeny+\\y*\\alto/5) --",
    "      (\\margenx+\\ancho, \\margeny+\\y*\\alto/5);",
    "  }"
  )
  
  # Agregar coordenadas de puntos
  for (i in 1:length(coords_combustible)) {
    codigo_tikz <- c(codigo_tikz,
      paste0("  \\coordinate (pc", i, ") at ", coords_combustible[i], ";"),
      paste0("  \\coordinate (pd", i, ") at ", coords_distancia[i], ";")
    )
  }
  
  # Agregar líneas y puntos
  linea_combustible <- paste0("  \\draw[colorlinea1, thick] ", 
                              paste0("(pc", 1:length(coords_combustible), ")", 
                                     collapse = " -- "), ";")
  linea_distancia <- paste0("  \\draw[colorlinea2, thick] ", 
                            paste0("(pd", 1:length(coords_distancia), ")", 
                                   collapse = " -- "), ";")
  
  codigo_tikz <- c(codigo_tikz, "", linea_combustible, linea_distancia, "")
  
  # Agregar puntos
  for (i in 1:length(coords_combustible)) {
    codigo_tikz <- c(codigo_tikz,
      paste0("  \\fill[colorlinea1] (pc", i, ") circle (3pt);"),
      paste0("  \\fill[colorlinea2] (pd", i, ") circle (3pt);")
    )
  }
  
  # Agregar etiquetas y leyenda
  codigo_tikz <- c(codigo_tikz,
    "",
    "  % Etiquetas de los ejes",
    paste0("  \\node[below] at (\\margenx+\\ancho/2, \\margeny-0.3) {\\textbf{", titulo_x, "}};"),
    paste0("  \\node[rotate=90, above] at (\\margenx-0.5, \\margeny+\\alto/2) {\\textbf{", titulo_y, "}};"),
    "",
    "  % Marcas del eje X",
    paste0("  \\foreach \\x/\\label in {",
           paste0(0:(length(datos_tiempo)-1), "/", datos_tiempo, collapse = ", "), "} {"),
    paste0("    \\draw (\\margenx+\\x*\\ancho/", (length(datos_tiempo)-1), ", \\margeny) -- (\\margenx+\\x*\\ancho/", (length(datos_tiempo)-1), ", \\margeny-0.1);"),
    paste0("    \\node[below] at (\\margenx+\\x*\\ancho/", (length(datos_tiempo)-1), ", \\margeny-0.15) {\\label};"),
    "  }",
    "",
    "  % Leyenda",
    "  \\coordinate (leyenda) at (\\margenx+\\ancho-2.5, \\margeny+\\alto-0.8);",
    "  \\draw[fill=white, draw=black, thin] (leyenda) rectangle ++(2.3, 0.6);",
    "  \\draw[colorlinea1, thick] ([xshift=0.1cm, yshift=0.4cm]leyenda) -- ++(0.3, 0);",
    "  \\fill[colorlinea1] ([xshift=0.25cm, yshift=0.4cm]leyenda) circle (2pt);",
    "  \\node[right, font=\\tiny] at ([xshift=0.4cm, yshift=0.4cm]leyenda) {Combustible disponible (litros)};",
    "  \\draw[colorlinea2, thick] ([xshift=0.1cm, yshift=0.15cm]leyenda) -- ++(0.3, 0);",
    "  \\fill[colorlinea2] ([xshift=0.25cm, yshift=0.15cm]leyenda) circle (2pt);",
    "  \\node[right, font=\\tiny] at ([xshift=0.4cm, yshift=0.15cm]leyenda) {Distancia recorrida (kilómetros)};",
    "",
    "\\end{tikzpicture}"
  )
  
  return(codigo_tikz)
}

# Función para crear imagen TikZ compatible con R-exams
crear_replica_tikz_lab17 <- function(datos_tiempo = c(0, 2, 4, 6, 8),
                                     datos_combustible = c(80, 65, 50, 35, 20),
                                     datos_distancia = c(0, 25, 50, 75, 100),
                                     nombre = "replica_lab17",
                                     ancho = "10cm") {
  
  # Obtener tipo de dispositivo actual
  typ <- match_exams_device()
  
  # Generar código TikZ
  codigo_tikz <- generar_tikz_lab17(datos_tiempo, datos_combustible, datos_distancia)
  
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
validar_replica_lab17 <- function() {
  cat("🔍 Validando réplica TikZ Lab/17...\n")
  
  # Datos de prueba basados en el patrón observado
  tiempo <- c(0, 2, 4, 6, 8)
  combustible <- c(80, 65, 50, 35, 20)  # Decreciente
  distancia <- c(0, 25, 50, 75, 100)    # Creciente
  
  tryCatch({
    imagen <- crear_replica_tikz_lab17(tiempo, combustible, distancia)
    cat("✅ Réplica TikZ generada exitosamente\n")
    cat("📊 Datos: Tiempo", paste(tiempo, collapse=", "), "\n")
    cat("⛽ Combustible:", paste(combustible, collapse=", "), "\n") 
    cat("🛣️ Distancia:", paste(distancia, collapse=", "), "\n")
    return(imagen)
  }, error = function(e) {
    cat("❌ Error en la generación:", e$message, "\n")
    return(NULL)
  })
}

# Mensaje de carga
cat("🎯 Réplica TikZ Lab/17 cargada exitosamente\n")
cat("💡 Uso: crear_replica_tikz_lab17(tiempo, combustible, distancia)\n")
cat("🔧 Validar: validar_replica_lab17()\n")
