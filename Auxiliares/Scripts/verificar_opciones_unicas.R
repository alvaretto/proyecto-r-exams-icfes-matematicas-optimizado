#!/usr/bin/env Rscript

# Script de verificación rápida para detectar opciones duplicadas
# Uso: Rscript verificar_opciones_unicas.R archivo.Rmd [num_versiones]

# Función para verificar opciones únicas en un archivo Rmd
verificar_opciones_unicas <- function(archivo_rmd, num_versiones = 5) {
  
  if (!file.exists(archivo_rmd)) {
    cat("❌ ERROR: Archivo no encontrado:", archivo_rmd, "\n")
    return(FALSE)
  }
  
  cat("🔍 VERIFICANDO OPCIONES ÚNICAS:", basename(archivo_rmd), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  duplicados_encontrados <- 0
  
  for (i in 1:num_versiones) {
    cat("Versión", i, "... ")
    
    # Renderizar temporalmente
    temp_html <- tempfile(fileext = ".html")
    
    tryCatch({
      # Renderizar con semilla diferente
      set.seed(i * 1000)
      rmarkdown::render(archivo_rmd, 
                       output_format = "html_document",
                       output_file = temp_html, 
                       quiet = TRUE)
      
      # Leer y buscar opciones
      contenido <- readLines(temp_html, warn = FALSE)
      lineas_answerlist <- grep("<li><span class=\"math inline\">", contenido, value = TRUE)
      
      if (length(lineas_answerlist) > 0) {
        # Extraer texto de opciones
        opciones <- gsub(".*<span class=\"math inline\">\\\\\\((.*)\\\\\\)</span>.*", "\\1", lineas_answerlist)
        opciones <- gsub("<.*?>", "", opciones)  # Limpiar HTML residual
        
        # Verificar duplicados
        if (length(unique(opciones)) != length(opciones)) {
          cat("❌ DUPLICADOS!\n")
          cat("   Opciones:", paste(opciones, collapse = " | "), "\n")
          duplicados_encontrados <- duplicados_encontrados + 1
        } else {
          cat("✅ OK (", length(opciones), "únicas)\n")
        }
      } else {
        cat("⚠️  No se encontraron opciones\n")
      }
      
    }, error = function(e) {
      cat("❌ Error:", e$message, "\n")
    }, finally = {
      if (file.exists(temp_html)) file.remove(temp_html)
    })
  }
  
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  if (duplicados_encontrados == 0) {
    cat("🎉 ¡ÉXITO! No se encontraron opciones duplicadas.\n")
    return(TRUE)
  } else {
    cat("⚠️  ADVERTENCIA:", duplicados_encontrados, "versiones con duplicados.\n")
    cat("🔧 ACCIÓN REQUERIDA: Revisar función formato_numero() en el archivo.\n")
    return(FALSE)
  }
}

# Función principal para uso desde línea de comandos
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    cat("Uso: Rscript verificar_opciones_unicas.R archivo.Rmd [num_versiones]\n")
    cat("Ejemplo: Rscript verificar_opciones_unicas.R ejercicio.Rmd 10\n")
    quit(status = 1)
  }
  
  archivo <- args[1]
  num_versiones <- if (length(args) >= 2) as.numeric(args[2]) else 5
  
  # Cargar librerías necesarias
  suppressPackageStartupMessages({
    library(rmarkdown)
    library(knitr)
  })
  
  # Ejecutar verificación
  resultado <- verificar_opciones_unicas(archivo, num_versiones)
  
  # Salir con código apropiado
  if (resultado) {
    quit(status = 0)  # Éxito
  } else {
    quit(status = 1)  # Error encontrado
  }
}

# Ejecutar si se llama directamente
if (!interactive()) {
  main()
}
