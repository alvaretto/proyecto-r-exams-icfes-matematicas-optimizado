# ============================================================================
# VALIDADOR DE COMPATIBILIDAD QTIKZ/KTIKZ
# Estrategia Robusta para Réplica Exacta - Proyecto ICFES R-exams
# ============================================================================

#' Función principal para validar compatibilidad con Qtikz/Ktikz
#' @param codigo_tikz Código TikZ a validar
#' @param nivel_validacion Nivel de validación: "basico", "completo", "estricto"
#' @return Lista con resultado de validación y recomendaciones
validar_compatibilidad_qtikz <- function(codigo_tikz, nivel_validacion = "completo") {
  
  cat("🔍 Validando compatibilidad con Qtikz/Ktikz...\n")
  
  # Inicializar resultado
  resultado <- list(
    compatible = TRUE,
    errores = character(),
    advertencias = character(),
    recomendaciones = character(),
    puntuacion = 0,
    nivel_usado = nivel_validacion
  )
  
  # Validaciones según nivel
  if (nivel_validacion %in% c("basico", "completo", "estricto")) {
    resultado <- validar_sintaxis_basica(codigo_tikz, resultado)
    resultado <- validar_bibliotecas_permitidas(codigo_tikz, resultado)
    resultado <- validar_colores_compatibles(codigo_tikz, resultado)
  }
  
  if (nivel_validacion %in% c("completo", "estricto")) {
    resultado <- validar_comandos_problematicos(codigo_tikz, resultado)
    resultado <- validar_escalado_apropiado(codigo_tikz, resultado)
    resultado <- validar_interpolacion_r(codigo_tikz, resultado)
  }
  
  if (nivel_validacion == "estricto") {
    resultado <- validar_referencias_ejemplos(codigo_tikz, resultado)
    resultado <- validar_optimizacion_multiformato(codigo_tikz, resultado)
  }
  
  # Calcular puntuación final
  resultado <- calcular_puntuacion_compatibilidad(resultado)
  
  # Generar reporte
  resultado$reporte <- generar_reporte_compatibilidad(resultado)
  
  cat(sprintf("✅ Validación completada. Compatibilidad: %s (%.0f/100)\n", 
              if(resultado$compatible) "✅ COMPATIBLE" else "❌ INCOMPATIBLE",
              resultado$puntuacion))
  
  return(resultado)
}

#' Validar sintaxis básica de TikZ
validar_sintaxis_basica <- function(codigo_tikz, resultado) {
  
  # Verificar estructura básica
  if (!grepl("\\\\begin\\{tikzpicture\\}", codigo_tikz)) {
    resultado$errores <- c(resultado$errores, "Falta \\begin{tikzpicture}")
    resultado$compatible <- FALSE
  }
  
  if (!grepl("\\\\end\\{tikzpicture\\}", codigo_tikz)) {
    resultado$errores <- c(resultado$errores, "Falta \\end{tikzpicture}")
    resultado$compatible <- FALSE
  }
  
  # Verificar balance de llaves
  num_abrir <- str_count(codigo_tikz, "\\{")
  num_cerrar <- str_count(codigo_tikz, "\\}")
  if (num_abrir != num_cerrar) {
    resultado$errores <- c(resultado$errores, 
                          sprintf("Llaves desbalanceadas: %d abrir, %d cerrar", num_abrir, num_cerrar))
    resultado$compatible <- FALSE
  }
  
  # Verificar balance de corchetes
  num_abrir_cor <- str_count(codigo_tikz, "\\[")
  num_cerrar_cor <- str_count(codigo_tikz, "\\]")
  if (num_abrir_cor != num_cerrar_cor) {
    resultado$advertencias <- c(resultado$advertencias, 
                               sprintf("Corchetes posiblemente desbalanceados: %d abrir, %d cerrar", 
                                      num_abrir_cor, num_cerrar_cor))
  }
  
  # Verificar comandos básicos válidos
  comandos_basicos <- c("\\\\draw", "\\\\fill", "\\\\node", "\\\\coordinate")
  tiene_comandos <- any(sapply(comandos_basicos, function(cmd) grepl(cmd, codigo_tikz)))
  
  if (!tiene_comandos) {
    resultado$advertencias <- c(resultado$advertencias, 
                               "No se detectaron comandos TikZ básicos")
  }
  
  return(resultado)
}

#' Validar bibliotecas permitidas
validar_bibliotecas_permitidas <- function(codigo_tikz, resultado) {
  
  # Bibliotecas compatibles con Qtikz/Ktikz
  bibliotecas_compatibles <- c(
    "calc", "positioning", "arrows", "math", "intersections", 
    "through", "decorations.markings", "patterns", "3d", "babel"
  )
  
  # Bibliotecas problemáticas
  bibliotecas_problematicas <- c(
    "shadows", "fadings", "blur", "external", "spy", "mindmap",
    "calendar", "er", "petri", "shadows.blur"
  )
  
  # Buscar bibliotecas en el código
  patron_biblioteca <- "\\\\usetikzlibrary\\{([^}]+)\\}"
  bibliotecas_encontradas <- regmatches(codigo_tikz, gregexpr(patron_biblioteca, codigo_tikz))
  
  if (length(bibliotecas_encontradas[[1]]) > 0) {
    for (bib_completa in bibliotecas_encontradas[[1]]) {
      # Extraer nombres de bibliotecas
      nombres_bib <- gsub("\\\\usetikzlibrary\\{([^}]+)\\}", "\\1", bib_completa)
      nombres_bib <- trimws(strsplit(nombres_bib, ",")[[1]])
      
      # Verificar cada biblioteca
      for (nombre in nombres_bib) {
        if (nombre %in% bibliotecas_problematicas) {
          resultado$errores <- c(resultado$errores, 
                                sprintf("Biblioteca problemática: %s", nombre))
          resultado$compatible <- FALSE
        } else if (!nombre %in% bibliotecas_compatibles) {
          resultado$advertencias <- c(resultado$advertencias, 
                                     sprintf("Biblioteca no validada: %s", nombre))
        }
      }
    }
  }
  
  return(resultado)
}

#' Validar colores compatibles
validar_colores_compatibles <- function(codigo_tikz, resultado) {
  
  # Colores estándar compatibles
  colores_estandar <- c(
    "red", "blue", "green", "yellow", "orange", "purple", "brown", 
    "black", "white", "gray", "cyan", "magenta", "lime", "pink"
  )
  
  # Buscar definiciones de colores personalizados (problemático)
  if (grepl("\\\\definecolor", codigo_tikz)) {
    resultado$errores <- c(resultado$errores, 
                          "Uso de \\definecolor detectado - usar colores estándar")
    resultado$compatible <- FALSE
  }
  
  # Buscar colores RGB directos (problemático en algunos contextos)
  if (grepl("\\{RGB\\}", codigo_tikz)) {
    resultado$advertencias <- c(resultado$advertencias, 
                               "Uso de RGB directo - preferir colores estándar con intensidad")
  }
  
  # Verificar uso de intensidades apropiadas
  patron_intensidad <- "(red|blue|green|yellow|orange|purple|brown|gray)!(\\d+)"
  intensidades <- regmatches(codigo_tikz, gregexpr(patron_intensidad, codigo_tikz))
  
  if (length(intensidades[[1]]) > 0) {
    for (intensidad_match in intensidades[[1]]) {
      valor_intensidad <- as.numeric(gsub(".*!(\\d+)", "\\1", intensidad_match))
      if (valor_intensidad > 80) {
        resultado$advertencias <- c(resultado$advertencias, 
                                   sprintf("Intensidad muy alta: %s", intensidad_match))
      } else if (valor_intensidad < 10) {
        resultado$advertencias <- c(resultado$advertencias, 
                                   sprintf("Intensidad muy baja: %s", intensidad_match))
      }
    }
  }
  
  return(resultado)
}

#' Validar comandos problemáticos
validar_comandos_problematicos <- function(codigo_tikz, resultado) {
  
  # Comandos problemáticos para Qtikz/Ktikz
  comandos_problematicos <- list(
    "\\\\pgfmathsetmacro" = "Usar variables R en lugar de \\pgfmathsetmacro",
    "\\\\pgfmathparse" = "Usar cálculos R en lugar de \\pgfmathparse",
    "\\\\tikzfading" = "Evitar efectos de desvanecimiento",
    "\\\\shade" = "Usar \\fill con colores simples",
    "\\\\tikzset" = "Definir estilos directamente en tikzpicture",
    "\\\\foreach" = "Usar bucles R para generar elementos repetitivos"
  )
  
  for (comando in names(comandos_problematicos)) {
    if (grepl(comando, codigo_tikz)) {
      resultado$advertencias <- c(resultado$advertencias, 
                                 sprintf("%s: %s", comando, comandos_problematicos[[comando]]))
    }
  }
  
  # Verificar uso de fuentes específicas
  if (grepl("\\\\fontfamily|\\\\fontsize", codigo_tikz)) {
    resultado$advertencias <- c(resultado$advertencias, 
                               "Uso de fuentes específicas - verificar disponibilidad")
  }
  
  return(resultado)
}

#' Validar escalado apropiado
validar_escalado_apropiado <- function(codigo_tikz, resultado) {
  
  # Buscar parámetro scale
  patron_scale <- "scale\\s*=\\s*([0-9.]+)"
  escalas <- regmatches(codigo_tikz, gregexpr(patron_scale, codigo_tikz))
  
  if (length(escalas[[1]]) > 0) {
    for (escala_match in escalas[[1]]) {
      valor_escala <- as.numeric(gsub("scale\\s*=\\s*([0-9.]+)", "\\1", escala_match))
      
      if (valor_escala > 2.0) {
        resultado$advertencias <- c(resultado$advertencias, 
                                   sprintf("Escala muy grande: %s", escala_match))
      } else if (valor_escala < 0.3) {
        resultado$advertencias <- c(resultado$advertencias, 
                                   sprintf("Escala muy pequeña: %s", escala_match))
      }
    }
  } else {
    resultado$recomendaciones <- c(resultado$recomendaciones, 
                                  "Considerar agregar parámetro scale para control de tamaño")
  }
  
  return(resultado)
}

#' Validar interpolación R
validar_interpolacion_r <- function(codigo_tikz, resultado) {
  
  # Buscar patrones de interpolación R
  patron_r <- "`r\\s+[^`]+`"
  interpolaciones <- regmatches(codigo_tikz, gregexpr(patron_r, codigo_tikz))
  
  if (length(interpolaciones[[1]]) > 0) {
    resultado$recomendaciones <- c(resultado$recomendaciones, 
                                  sprintf("Interpolaciones R detectadas: %d", length(interpolaciones[[1]])))
    
    # Verificar sintaxis básica de interpolaciones
    for (interp in interpolaciones[[1]]) {
      if (grepl("\\$|\\{|\\}", interp)) {
        resultado$advertencias <- c(resultado$advertencias, 
                                   sprintf("Interpolación compleja: %s", interp))
      }
    }
  }
  
  return(resultado)
}

#' Validar referencias a ejemplos
validar_referencias_ejemplos <- function(codigo_tikz, resultado) {
  
  # Verificar si sigue patrones de ejemplos funcionales
  ejemplos_path <- "Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/TikZ-Documentation/"
  
  if (dir.exists(ejemplos_path)) {
    # Buscar similitudes con templates robustos
    templates_robustos <- list.files(
      file.path(ejemplos_path, "templates-rexams/robustos"), 
      pattern = "\\.tikz$", 
      full.names = TRUE
    )
    
    if (length(templates_robustos) > 0) {
      resultado$recomendaciones <- c(resultado$recomendaciones, 
                                    sprintf("Templates robustos disponibles: %d", length(templates_robustos)))
    }
  }
  
  return(resultado)
}

#' Validar optimización multiformato
validar_optimizacion_multiformato <- function(codigo_tikz, resultado) {
  
  # Verificar características que pueden causar problemas en diferentes formatos
  
  # Texto muy pequeño
  if (grepl("\\\\tiny|\\\\scriptsize", codigo_tikz)) {
    resultado$advertencias <- c(resultado$advertencias, 
                               "Texto muy pequeño - puede ser ilegible en algunos formatos")
  }
  
  # Coordenadas muy grandes
  patron_coord <- "\\(([0-9.]+),([0-9.]+)\\)"
  coordenadas <- regmatches(codigo_tikz, gregexpr(patron_coord, codigo_tikz))
  
  if (length(coordenadas[[1]]) > 0) {
    coords_numericas <- sapply(coordenadas[[1]], function(coord) {
      nums <- as.numeric(gsub("\\(([0-9.]+),([0-9.]+)\\)", "\\1,\\2", coord))
      max(nums, na.rm = TRUE)
    })
    
    if (any(coords_numericas > 20, na.rm = TRUE)) {
      resultado$advertencias <- c(resultado$advertencias, 
                                 "Coordenadas muy grandes - considerar ajustar escala")
    }
  }
  
  return(resultado)
}

#' Calcular puntuación de compatibilidad
calcular_puntuacion_compatibilidad <- function(resultado) {
  
  puntuacion_base <- 100
  
  # Penalizar errores
  puntuacion_base <- puntuacion_base - (length(resultado$errores) * 20)
  
  # Penalizar advertencias
  puntuacion_base <- puntuacion_base - (length(resultado$advertencias) * 5)
  
  # Bonificar si no hay problemas
  if (length(resultado$errores) == 0 && length(resultado$advertencias) == 0) {
    puntuacion_base <- puntuacion_base + 10
  }
  
  # Asegurar rango 0-100
  resultado$puntuacion <- max(0, min(100, puntuacion_base))
  
  # Actualizar compatibilidad basada en puntuación
  if (resultado$puntuacion < 70) {
    resultado$compatible <- FALSE
  }
  
  return(resultado)
}

#' Generar reporte de compatibilidad
generar_reporte_compatibilidad <- function(resultado) {
  
  reporte <- sprintf("
🔍 REPORTE DE COMPATIBILIDAD QTIKZ/KTIKZ
========================================

📊 PUNTUACIÓN: %.0f/100
%s ESTADO: %s

", 
    resultado$puntuacion,
    if(resultado$compatible) "✅" else "❌",
    if(resultado$compatible) "COMPATIBLE" else "INCOMPATIBLE"
  )
  
  if (length(resultado$errores) > 0) {
    reporte <- paste0(reporte, "\n❌ ERRORES CRÍTICOS:\n")
    for (error in resultado$errores) {
      reporte <- paste0(reporte, sprintf("   • %s\n", error))
    }
  }
  
  if (length(resultado$advertencias) > 0) {
    reporte <- paste0(reporte, "\n⚠️ ADVERTENCIAS:\n")
    for (advertencia in resultado$advertencias) {
      reporte <- paste0(reporte, sprintf("   • %s\n", advertencia))
    }
  }
  
  if (length(resultado$recomendaciones) > 0) {
    reporte <- paste0(reporte, "\n💡 RECOMENDACIONES:\n")
    for (recomendacion in resultado$recomendaciones) {
      reporte <- paste0(reporte, sprintf("   • %s\n", recomendacion))
    }
  }
  
  return(reporte)
}

#' Función de ayuda para corregir problemas comunes
corregir_problemas_automaticos <- function(codigo_tikz) {
  
  cat("🔧 Aplicando correcciones automáticas...\n")
  
  # Remover definecolor
  codigo_tikz <- gsub("\\\\definecolor\\{[^}]+\\}\\{[^}]+\\}\\{[^}]+\\}", "", codigo_tikz)
  
  # Convertir pgfmathsetmacro a comentarios
  codigo_tikz <- gsub("\\\\pgfmathsetmacro", "% TODO: Convertir a variable R - \\\\pgfmathsetmacro", codigo_tikz)
  
  # Remover bibliotecas problemáticas
  codigo_tikz <- gsub("\\\\usetikzlibrary\\{[^}]*shadows[^}]*\\}", "", codigo_tikz)
  codigo_tikz <- gsub("\\\\usetikzlibrary\\{[^}]*fadings[^}]*\\}", "", codigo_tikz)
  
  # Asegurar escala si no existe
  if (!grepl("scale\\s*=", codigo_tikz)) {
    codigo_tikz <- gsub("\\\\begin\\{tikzpicture\\}", "\\\\begin{tikzpicture}[scale=0.8]", codigo_tikz)
  }
  
  cat("✅ Correcciones automáticas aplicadas\n")
  return(codigo_tikz)
}

cat("✅ Validador de compatibilidad Qtikz/Ktikz cargado exitosamente\n")
