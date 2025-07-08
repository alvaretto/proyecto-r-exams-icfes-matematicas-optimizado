# ============================================================================
# EJEMPLO PRÁCTICO DE COMPATIBILIDAD QTIKZ/KTIKZ
# Estrategia Robusta para Réplica Exacta - Proyecto ICFES R-exams
# ============================================================================

# Cargar sistema completo
source("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/sistema_replica_exacta.R")

#' Función de demostración para compatibilidad Qtikz/Ktikz
#' @param tipo_demo Tipo de demostración: "tabla", "circular", "barras", "validacion"
demo_compatibilidad_qtikz <- function(tipo_demo = "validacion") {
  
  cat("🎯 DEMOSTRACIÓN DE COMPATIBILIDAD QTIKZ/KTIKZ\n")
  cat("============================================\n\n")
  
  if (tipo_demo == "tabla") {
    demo_tabla_qtikz()
  } else if (tipo_demo == "circular") {
    demo_grafico_circular_qtikz()
  } else if (tipo_demo == "barras") {
    demo_grafico_barras_qtikz()
  } else if (tipo_demo == "validacion") {
    demo_validacion_completa()
  } else {
    cat("Tipos disponibles: 'tabla', 'circular', 'barras', 'validacion'\n")
  }
}

#' Demostración de tabla compatible con Qtikz/Ktikz
demo_tabla_qtikz <- function() {
  
  cat("📋 DEMO: Tabla de Datos Compatible\n")
  cat("==================================\n")
  
  # Código TikZ compatible basado en templates robustos
  tikz_tabla <- "
% Tabla de Datos Compatible Qtikz/Ktikz
\\begin{tikzpicture}[scale=0.8]
\\node[inner sep=0pt] {
  \\begin{tabular}{|c|c|c|}
    \\hline
    \\rowcolor{blue!20}
    \\textbf{Categoría} & \\textbf{Frecuencia} & \\textbf{Porcentaje} \\\\
    \\hline
    A & 15 & 30\\% \\\\
    \\hline
    B & 20 & 40\\% \\\\
    \\hline
    C & 10 & 20\\% \\\\
    \\hline
    D & 5 & 10\\% \\\\
    \\hline
    \\rowcolor{gray!20}
    \\textbf{Total} & \\textbf{50} & \\textbf{100\\%} \\\\
    \\hline
  \\end{tabular}
};
\\end{tikzpicture}"
  
  # Validar compatibilidad
  validacion <- validar_compatibilidad_qtikz(tikz_tabla, "completo")
  
  # Mostrar resultados
  cat("📊 CÓDIGO TIKZ GENERADO:\n")
  cat(tikz_tabla)
  cat("\n\n")
  
  cat("🔍 VALIDACIÓN DE COMPATIBILIDAD:\n")
  cat(validacion$reporte)
  
  # Guardar ejemplo
  writeLines(tikz_tabla, "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/ejemplo_tabla_qtikz.tikz")
  cat("\n📁 Ejemplo guardado en: ejemplo_tabla_qtikz.tikz\n")
  cat("💡 Puedes abrir este archivo directamente en Qtikz/Ktikz\n")
  
  return(list(codigo = tikz_tabla, validacion = validacion))
}

#' Demostración de gráfico circular compatible con Qtikz/Ktikz
demo_grafico_circular_qtikz <- function() {
  
  cat("🥧 DEMO: Gráfico Circular Compatible\n")
  cat("===================================\n")
  
  # Código TikZ compatible basado en ejemplos de Fausto
  tikz_circular <- "
% Gráfico Circular Compatible Qtikz/Ktikz
\\begin{tikzpicture}[scale=0.8]
\\coordinate (centro) at (0,0);

% Sectores del gráfico (ángulos calculados)
\\fill[blue!60] (centro) -- (0:2.5) arc (0:108:2.5) -- cycle;
\\fill[red!60] (centro) -- (108:2.5) arc (108:216:2.5) -- cycle;
\\fill[green!60] (centro) -- (216:2.5) arc (216:288:2.5) -- cycle;
\\fill[orange!60] (centro) -- (288:2.5) arc (288:360:2.5) -- cycle;

% Contorno principal
\\draw[black, thick] (centro) circle (2.5);

% Líneas divisorias
\\draw[black] (centro) -- (0:2.5);
\\draw[black] (centro) -- (108:2.5);
\\draw[black] (centro) -- (216:2.5);
\\draw[black] (centro) -- (288:2.5);

% Etiquetas de porcentajes
\\node[font=\\bfseries] at (54:1.8) {30\\%};
\\node[font=\\bfseries] at (162:1.8) {30\\%};
\\node[font=\\bfseries] at (252:1.8) {20\\%};
\\node[font=\\bfseries] at (324:1.8) {20\\%};

% Leyenda
\\node[right] at (3.5,1.5) {\\textcolor{blue}{Categoría A (30\\%)}};
\\node[right] at (3.5,1.0) {\\textcolor{red}{Categoría B (30\\%)}};
\\node[right] at (3.5,0.5) {\\textcolor{green}{Categoría C (20\\%)}};
\\node[right] at (3.5,0.0) {\\textcolor{orange}{Categoría D (20\\%)}};

\\end{tikzpicture}"
  
  # Validar compatibilidad
  validacion <- validar_compatibilidad_qtikz(tikz_circular, "completo")
  
  # Mostrar resultados
  cat("📊 CÓDIGO TIKZ GENERADO:\n")
  cat(tikz_circular)
  cat("\n\n")
  
  cat("🔍 VALIDACIÓN DE COMPATIBILIDAD:\n")
  cat(validacion$reporte)
  
  # Guardar ejemplo
  writeLines(tikz_circular, "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/ejemplo_circular_qtikz.tikz")
  cat("\n📁 Ejemplo guardado en: ejemplo_circular_qtikz.tikz\n")
  cat("💡 Puedes abrir este archivo directamente en Qtikz/Ktikz\n")
  
  return(list(codigo = tikz_circular, validacion = validacion))
}

#' Demostración de gráfico de barras compatible con Qtikz/Ktikz
demo_grafico_barras_qtikz <- function() {
  
  cat("📊 DEMO: Gráfico de Barras Compatible\n")
  cat("====================================\n")
  
  # Código TikZ compatible
  tikz_barras <- "
% Gráfico de Barras Compatible Qtikz/Ktikz
\\begin{tikzpicture}[scale=0.8]

% Ejes principales
\\draw[->] (0,0) -- (0,6) node[left] {Frecuencia};
\\draw[->] (0,0) -- (8,0) node[below] {Categorías};

% Barras individuales
\\fill[blue!60] (0.5,0) rectangle (1.5,4.5);
\\fill[red!60] (2.0,0) rectangle (3.0,3.0);
\\fill[green!60] (3.5,0) rectangle (4.5,5.0);
\\fill[orange!60] (5.0,0) rectangle (6.0,2.5);

% Contornos de barras
\\draw[black] (0.5,0) rectangle (1.5,4.5);
\\draw[black] (2.0,0) rectangle (3.0,3.0);
\\draw[black] (3.5,0) rectangle (4.5,5.0);
\\draw[black] (5.0,0) rectangle (6.0,2.5);

% Etiquetas de categorías
\\node[below] at (1.0,-0.3) {A};
\\node[below] at (2.5,-0.3) {B};
\\node[below] at (4.0,-0.3) {C};
\\node[below] at (5.5,-0.3) {D};

% Valores sobre las barras
\\node[above] at (1.0,4.6) {45};
\\node[above] at (2.5,3.1) {30};
\\node[above] at (4.0,5.1) {50};
\\node[above] at (5.5,2.6) {25};

% Líneas de cuadrícula
\\draw[gray!30] (0,1) -- (7,1);
\\draw[gray!30] (0,2) -- (7,2);
\\draw[gray!30] (0,3) -- (7,3);
\\draw[gray!30] (0,4) -- (7,4);
\\draw[gray!30] (0,5) -- (7,5);

% Etiquetas del eje Y
\\node[left] at (-0.2,1) {10};
\\node[left] at (-0.2,2) {20};
\\node[left] at (-0.2,3) {30};
\\node[left] at (-0.2,4) {40};
\\node[left] at (-0.2,5) {50};

\\end{tikzpicture}"
  
  # Validar compatibilidad
  validacion <- validar_compatibilidad_qtikz(tikz_barras, "completo")
  
  # Mostrar resultados
  cat("📊 CÓDIGO TIKZ GENERADO:\n")
  cat(tikz_barras)
  cat("\n\n")
  
  cat("🔍 VALIDACIÓN DE COMPATIBILIDAD:\n")
  cat(validacion$reporte)
  
  # Guardar ejemplo
  writeLines(tikz_barras, "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/ejemplo_barras_qtikz.tikz")
  cat("\n📁 Ejemplo guardado en: ejemplo_barras_qtikz.tikz\n")
  cat("💡 Puedes abrir este archivo directamente en Qtikz/Ktikz\n")
  
  return(list(codigo = tikz_barras, validacion = validacion))
}

#' Demostración de validación completa del sistema
demo_validacion_completa <- function() {
  
  cat("🔍 DEMO: Validación Completa del Sistema\n")
  cat("=======================================\n")
  
  # Ejemplo de código problemático
  tikz_problematico <- "
\\usetikzlibrary{shadows,fadings}
\\definecolor{micolor}{RGB}{123,45,67}
\\begin{tikzpicture}[scale=3.5]
\\pgfmathsetmacro{\\mivar}{2*3.14159}
\\fill[micolor,drop shadow] (0,0) circle (\\mivar);
\\end{tikzpicture}"
  
  cat("❌ CÓDIGO PROBLEMÁTICO (ANTES):\n")
  cat(tikz_problematico)
  cat("\n\n")
  
  # Validar código problemático
  validacion_antes <- validar_compatibilidad_qtikz(tikz_problematico, "estricto")
  cat("🔍 VALIDACIÓN ANTES DE CORRECCIONES:\n")
  cat(validacion_antes$reporte)
  cat("\n")
  
  # Aplicar correcciones automáticas
  cat("🔧 APLICANDO CORRECCIONES AUTOMÁTICAS...\n")
  tikz_corregido <- corregir_problemas_automaticos(tikz_problematico)
  
  cat("✅ CÓDIGO CORREGIDO (DESPUÉS):\n")
  cat(tikz_corregido)
  cat("\n\n")
  
  # Validar código corregido
  validacion_despues <- validar_compatibilidad_qtikz(tikz_corregido, "estricto")
  cat("🔍 VALIDACIÓN DESPUÉS DE CORRECCIONES:\n")
  cat(validacion_despues$reporte)
  
  # Comparar resultados
  cat("\n📊 COMPARACIÓN DE RESULTADOS:\n")
  cat(sprintf("Antes:   Puntuación %.0f/100 - %s\n", 
              validacion_antes$puntuacion,
              if(validacion_antes$compatible) "Compatible" else "Incompatible"))
  cat(sprintf("Después: Puntuación %.0f/100 - %s\n", 
              validacion_despues$puntuacion,
              if(validacion_despues$compatible) "Compatible" else "Incompatible"))
  
  return(list(
    antes = list(codigo = tikz_problematico, validacion = validacion_antes),
    despues = list(codigo = tikz_corregido, validacion = validacion_despues)
  ))
}

#' Función para probar el sistema completo con una imagen
probar_sistema_completo_qtikz <- function(imagen_path = NULL) {
  
  cat("🧪 PRUEBA COMPLETA DEL SISTEMA CON COMPATIBILIDAD QTIKZ/KTIKZ\n")
  cat("============================================================\n")
  
  if (is.null(imagen_path)) {
    cat("⚠️ No se proporcionó imagen. Creando imagen de prueba...\n")
    
    # Crear imagen de prueba simple
    img_prueba <- magick::image_blank(300, 200, "white")
    img_prueba <- magick::image_annotate(img_prueba, "Gráfico de Prueba", 
                                        size = 20, location = "+50+100", color = "black")
    
    # Agregar algunos elementos gráficos
    img_prueba <- magick::image_draw(img_prueba)
    rect(50, 50, 100, 80, col = "blue")
    rect(120, 60, 170, 90, col = "red")
    rect(190, 40, 240, 100, col = "green")
    dev.off()
    
    imagen_path <- "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/imagen_prueba_qtikz.png"
    magick::image_write(img_prueba, imagen_path)
    cat(sprintf("✅ Imagen de prueba creada: %s\n", imagen_path))
  }
  
  # Ejecutar sistema completo
  cat("\n🚀 Ejecutando sistema de réplica exacta...\n")
  resultado <- sistema_replica_exacta(
    imagen_path = imagen_path,
    umbral_exactitud = 0.99,
    generar_ejercicio_completo = FALSE
  )
  
  # Mostrar resultados específicos de compatibilidad
  cat("\n🎯 RESULTADOS DE COMPATIBILIDAD QTIKZ/KTIKZ:\n")
  if (!is.null(resultado$validacion_final$compatibilidad_qtikz)) {
    cat(resultado$validacion_final$compatibilidad_qtikz$reporte)
  }
  
  # Guardar código TikZ final
  tikz_path <- "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/resultado_final_qtikz.tikz"
  writeLines(resultado$tikz_exacto, tikz_path)
  cat(sprintf("\n📁 Código TikZ final guardado en: %s\n", tikz_path))
  cat("💡 Puedes abrir este archivo directamente en Qtikz/Ktikz para verificar\n")
  
  return(resultado)
}

# Función de ayuda rápida
ayuda_qtikz <- function() {
  cat("\n🆘 AYUDA RÁPIDA - COMPATIBILIDAD QTIKZ/KTIKZ\n")
  cat("===========================================\n")
  cat("📖 Comandos disponibles:\n")
  cat("   demo_compatibilidad_qtikz('tabla')      # Demo tabla\n")
  cat("   demo_compatibilidad_qtikz('circular')   # Demo gráfico circular\n")
  cat("   demo_compatibilidad_qtikz('barras')     # Demo gráfico barras\n")
  cat("   demo_compatibilidad_qtikz('validacion') # Demo validación\n")
  cat("   probar_sistema_completo_qtikz()         # Prueba completa\n")
  cat("\n🔍 Validación manual:\n")
  cat("   validar_compatibilidad_qtikz(codigo_tikz)\n")
  cat("   corregir_problemas_automaticos(codigo_tikz)\n")
  cat("\n📚 Referencias:\n")
  cat("   - Templates robustos: Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/TikZ-Documentation/templates-rexams/robustos/\n")
  cat("   - Ejemplos Fausto: Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/TikZ-Documentation/Ejemplos-Fausto/Graf/\n")
}

cat("✅ Ejemplos de compatibilidad Qtikz/Ktikz cargados exitosamente\n")
cat("💡 Ejecuta ayuda_qtikz() para ver comandos disponibles\n")
