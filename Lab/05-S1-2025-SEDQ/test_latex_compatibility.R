# SCRIPT DE PRUEBA PARA COMPATIBILIDAD CON LATEX
# Verifica que el archivo se compile correctamente con pdflatex

library(exams)
library(rmarkdown)

# Función para probar compilación con diferentes motores LaTeX
probar_compilacion_latex <- function(archivo = "proporciones_encuesta_deportiva_v1.Rmd") {
  cat("=== PRUEBA DE COMPATIBILIDAD CON LATEX ===\n")
  cat("Archivo:", archivo, "\n\n")
  
  resultados <- list()
  
  # Prueba 1: Compilación con exams2pdf (usa pdflatex por defecto)
  cat("1. Probando compilación con exams2pdf (pdflatex)...\n")
  tryCatch({
    resultado_pdf <- exams2pdf(archivo, 
                              n = 1, 
                              name = "test_latex_pdflatex",
                              dir = "test_latex_output",
                              quiet = TRUE)
    cat("   ✅ ÉXITO: Compilación con pdflatex exitosa\n")
    resultados$pdflatex <- "ÉXITO"
  }, error = function(e) {
    if (grepl("Unicode", e$message) || grepl("LaTeX Error", e$message)) {
      cat("   ❌ ERROR UNICODE/LATEX:", e$message, "\n")
      resultados$pdflatex <- paste("ERROR UNICODE:", e$message)
    } else {
      cat("   ⚠️  OTRO ERROR:", e$message, "\n")
      resultados$pdflatex <- paste("OTRO ERROR:", e$message)
    }
  })
  
  # Prueba 2: Compilación con rmarkdown::render usando xelatex
  cat("\n2. Probando compilación con xelatex...\n")
  tryCatch({
    # Crear archivo temporal con configuración xelatex
    archivo_temp <- "temp_xelatex_test.Rmd"
    contenido <- readLines(archivo)
    
    # Modificar header para usar xelatex
    header_modificado <- c(
      "---",
      "output:",
      "  pdf_document:",
      "    latex_engine: xelatex",
      "    keep_tex: true",
      "---"
    )
    
    # Encontrar donde termina el header YAML original
    fin_yaml <- which(contenido == "---")[2]
    contenido_modificado <- c(header_modificado, contenido[(fin_yaml+1):length(contenido)])
    
    writeLines(contenido_modificado, archivo_temp)
    
    # Intentar compilar
    rmarkdown::render(archivo_temp, 
                     output_file = "test_xelatex.pdf",
                     output_dir = "test_latex_output",
                     quiet = TRUE)
    
    cat("   ✅ ÉXITO: Compilación con xelatex exitosa\n")
    resultados$xelatex <- "ÉXITO"
    
    # Limpiar archivo temporal
    if (file.exists(archivo_temp)) file.remove(archivo_temp)
    
  }, error = function(e) {
    cat("   ❌ ERROR XELATEX:", e$message, "\n")
    resultados$xelatex <- paste("ERROR:", e$message)
    
    # Limpiar archivo temporal
    archivo_temp <- "temp_xelatex_test.Rmd"
    if (file.exists(archivo_temp)) file.remove(archivo_temp)
  })
  
  # Prueba 3: Verificar caracteres problemáticos específicos
  cat("\n3. Verificando caracteres potencialmente problemáticos...\n")
  contenido_archivo <- paste(readLines(archivo), collapse = "\n")
  
  caracteres_problematicos <- c(
    "✓" = "checkmark",
    "✅" = "check mark button", 
    "❌" = "cross mark",
    "⚠️" = "warning sign",
    "🎯" = "direct hit",
    "🔧" = "wrench",
    "📊" = "bar chart",
    "💡" = "light bulb"
  )
  
  problemas_encontrados <- c()
  for (char in names(caracteres_problematicos)) {
    if (grepl(char, contenido_archivo, fixed = TRUE)) {
      problemas_encontrados <- c(problemas_encontrados, 
                               paste0(char, " (", caracteres_problematicos[char], ")"))
    }
  }
  
  if (length(problemas_encontrados) > 0) {
    cat("   ⚠️  Caracteres Unicode encontrados:\n")
    for (problema in problemas_encontrados) {
      cat("      -", problema, "\n")
    }
    resultados$unicode_chars <- problemas_encontrados
  } else {
    cat("   ✅ No se encontraron caracteres Unicode problemáticos\n")
    resultados$unicode_chars <- "NINGUNO"
  }
  
  # Resumen final
  cat("\n=== RESUMEN DE PRUEBAS ===\n")
  cat("PDFLaTeX:", resultados$pdflatex, "\n")
  cat("XeLaTeX:", resultados$xelatex, "\n")
  cat("Caracteres Unicode:", 
      if(is.character(resultados$unicode_chars) && resultados$unicode_chars == "NINGUNO") {
        "Ninguno problemático"
      } else {
        paste(length(resultados$unicode_chars), "encontrados")
      }, "\n")
  
  return(resultados)
}

# Función para crear versión compatible con LaTeX
crear_version_latex_compatible <- function(archivo_original, archivo_salida = NULL) {
  cat("\n=== CREANDO VERSIÓN COMPATIBLE CON LATEX ===\n")
  
  if (is.null(archivo_salida)) {
    archivo_salida <- gsub("\\.Rmd$", "_latex_compatible.Rmd", archivo_original)
  }
  
  contenido <- readLines(archivo_original)
  
  # Reemplazos de caracteres Unicode problemáticos
  reemplazos <- list(
    "✓" = "(correcto)",
    "✅" = "(correcto)",
    "❌" = "(error)",
    "⚠️" = "(advertencia)",
    "🎯" = "[objetivo]",
    "🔧" = "[herramienta]",
    "📊" = "[gráfico]",
    "💡" = "[idea]"
  )
  
  contenido_modificado <- contenido
  cambios_realizados <- 0
  
  for (unicode_char in names(reemplazos)) {
    reemplazo <- reemplazos[[unicode_char]]
    lineas_modificadas <- grep(unicode_char, contenido_modificado, fixed = TRUE)
    
    if (length(lineas_modificadas) > 0) {
      cat("Reemplazando", unicode_char, "por", reemplazo, "en", length(lineas_modificadas), "líneas\n")
      contenido_modificado <- gsub(unicode_char, reemplazo, contenido_modificado, fixed = TRUE)
      cambios_realizados <- cambios_realizados + length(lineas_modificadas)
    }
  }
  
  if (cambios_realizados > 0) {
    writeLines(contenido_modificado, archivo_salida)
    cat("✅ Versión compatible creada:", archivo_salida, "\n")
    cat("Total de cambios realizados:", cambios_realizados, "\n")
    return(archivo_salida)
  } else {
    cat("✅ No se necesitaron cambios - archivo ya compatible\n")
    return(archivo_original)
  }
}

# Función para verificar configuración del sistema LaTeX
verificar_sistema_latex <- function() {
  cat("\n=== VERIFICACIÓN DEL SISTEMA LATEX ===\n")
  
  # Verificar si pdflatex está disponible
  tryCatch({
    system("pdflatex --version", intern = TRUE, ignore.stdout = TRUE)
    cat("✅ pdflatex disponible\n")
  }, error = function(e) {
    cat("❌ pdflatex no disponible\n")
  })
  
  # Verificar si xelatex está disponible
  tryCatch({
    system("xelatex --version", intern = TRUE, ignore.stdout = TRUE)
    cat("✅ xelatex disponible\n")
  }, error = function(e) {
    cat("❌ xelatex no disponible\n")
  })
  
  # Verificar configuración de rmarkdown
  cat("Configuración de rmarkdown:\n")
  cat("  - Pandoc version:", rmarkdown::pandoc_version(), "\n")
  cat("  - Pandoc available:", rmarkdown::pandoc_available(), "\n")
}

# Ejecutar pruebas si el script se ejecuta directamente
if (interactive()) {
  cat("EJECUTANDO SUITE DE PRUEBAS DE COMPATIBILIDAD LATEX\n")
  cat("==================================================\n\n")
  
  # Crear directorio de salida si no existe
  if (!dir.exists("test_latex_output")) {
    dir.create("test_latex_output")
  }
  
  # Ejecutar verificaciones
  verificar_sistema_latex()
  resultados <- probar_compilacion_latex()
  
  # Si hay problemas con pdflatex, crear versión compatible
  if (grepl("ERROR", resultados$pdflatex)) {
    cat("\n=== CREANDO VERSIÓN CORREGIDA ===\n")
    archivo_corregido <- crear_version_latex_compatible("proporciones_encuesta_deportiva_v1.Rmd")
    
    if (archivo_corregido != "proporciones_encuesta_deportiva_v1.Rmd") {
      cat("\nProbando versión corregida...\n")
      resultados_corregidos <- probar_compilacion_latex(archivo_corregido)
    }
  }
  
  cat("\n=== PRUEBAS COMPLETADAS ===\n")
}
