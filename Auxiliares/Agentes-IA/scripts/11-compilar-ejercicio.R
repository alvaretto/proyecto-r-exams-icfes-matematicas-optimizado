#!/usr/bin/env Rscript

# ============================================================================
# Script de Compilación de Ejercicios R-exams ICFES
# ============================================================================
# Uso: Rscript 11-compilar-ejercicio.R archivo.Rmd [formato]
# Formatos: html, pdf, moodle, nops, todos
# ============================================================================

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(exams)
  library(tools)
})

# Colores para output en terminal
color_rojo <- "\033[0;31m"
color_verde <- "\033[0;32m"
color_amarillo <- "\033[1;33m"
color_azul <- "\033[0;34m"
color_reset <- "\033[0m"

# Función para imprimir con color
cat_color <- function(texto, color = color_reset) {
  cat(paste0(color, texto, color_reset, "\n"))
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE COMPILACIÓN
# ============================================================================

compilar_ejercicio <- function(archivo, formato = "html", directorio_salida = NULL) {
  
  # Banner
  cat_color("╔════════════════════════════════════════════════════════════╗", color_azul)
  cat_color("║  COMPILADOR DE EJERCICIOS R-EXAMS ICFES                   ║", color_azul)
  cat_color("╚════════════════════════════════════════════════════════════╝", color_azul)
  cat("\n")
  
  # Verificar que el archivo existe
  if (!file.exists(archivo)) {
    cat_color(paste("❌ ERROR: Archivo no encontrado:", archivo), color_rojo)
    return(invisible(FALSE))
  }
  
  cat_color(paste("📄 Archivo:", archivo), color_amarillo)
  cat_color(paste("📅 Fecha:", Sys.time()), color_amarillo)
  cat_color(paste("🎯 Formato:", formato), color_amarillo)
  cat("\n")
  
  # Determinar directorio de salida
  if (is.null(directorio_salida)) {
    directorio_salida <- file.path(dirname(archivo), "salida")
  }
  
  # Crear directorio de salida si no existe
  if (!dir.exists(directorio_salida)) {
    dir.create(directorio_salida, recursive = TRUE)
    cat_color(paste("📁 Directorio de salida creado:", directorio_salida), color_verde)
  }
  
  cat_color("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━", color_azul)
  cat("\n")
  
  # Compilar según formato
  exito <- TRUE
  
  tryCatch({
    
    if (formato == "html" || formato == "todos") {
      cat_color("[1/4] Compilando a HTML...", color_azul)
      exams2html(archivo, 
                 n = 3,
                 dir = directorio_salida,
                 encoding = "UTF-8",
                 converter = "pandoc-mathjax")
      cat_color("✅ HTML generado exitosamente", color_verde)
      cat("\n")
    }
    
    if (formato == "pdf" || formato == "todos") {
      cat_color("[2/4] Compilando a PDF...", color_azul)
      exams2pdf(archivo,
                n = 3,
                dir = directorio_salida,
                encoding = "UTF-8")
      cat_color("✅ PDF generado exitosamente", color_verde)
      cat("\n")
    }
    
    if (formato == "moodle" || formato == "todos") {
      cat_color("[3/4] Compilando a Moodle XML...", color_azul)
      exams2moodle(archivo,
                   n = 3,
                   dir = directorio_salida,
                   encoding = "UTF-8")
      cat_color("✅ Moodle XML generado exitosamente", color_verde)
      cat("\n")
    }
    
    if (formato == "nops" || formato == "todos") {
      cat_color("[4/4] Compilando a NOPS (escaneable)...", color_azul)
      exams2nops(archivo,
                 n = 3,
                 dir = directorio_salida,
                 encoding = "UTF-8",
                 language = "es")
      cat_color("✅ NOPS generado exitosamente", color_verde)
      cat("\n")
    }
    
  }, error = function(e) {
    cat_color(paste("❌ ERROR durante compilación:", e$message), color_rojo)
    exito <<- FALSE
  })
  
  # Resumen final
  cat_color("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━", color_azul)
  cat("\n")
  
  if (exito) {
    cat_color("╔════════════════════════════════════════════════════════════╗", color_azul)
    cat_color("║  ✅ COMPILACIÓN EXITOSA                                    ║", color_verde)
    cat_color("╚════════════════════════════════════════════════════════════╝", color_azul)
    cat("\n")
    cat_color(paste("📂 Archivos generados en:", directorio_salida), color_verde)
    
    # Listar archivos generados
    archivos_generados <- list.files(directorio_salida, full.names = FALSE)
    if (length(archivos_generados) > 0) {
      cat("\n")
      cat_color("📋 Archivos generados:", color_amarillo)
      for (arch in archivos_generados) {
        cat(paste("   -", arch), "\n")
      }
    }
  } else {
    cat_color("╔════════════════════════════════════════════════════════════╗", color_azul)
    cat_color("║  ❌ COMPILACIÓN FALLIDA                                    ║", color_rojo)
    cat_color("╚════════════════════════════════════════════════════════════╝", color_azul)
    cat("\n")
    cat_color("💡 Recomendaciones:", color_amarillo)
    cat("1. Verificar sintaxis del archivo .Rmd\n")
    cat("2. Ejecutar script de validación primero\n")
    cat("3. Consultar ejemplos funcionales\n")
  }
  
  cat("\n")
  return(invisible(exito))
}

# ============================================================================
# EJECUCIÓN DESDE LÍNEA DE COMANDOS
# ============================================================================

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    cat_color("❌ ERROR: Debe proporcionar al menos el archivo a compilar", color_rojo)
    cat("\n")
    cat("Uso: Rscript 11-compilar-ejercicio.R archivo.Rmd [formato]\n")
    cat("Formatos disponibles: html, pdf, moodle, nops, todos\n")
    cat("Ejemplo: Rscript 11-compilar-ejercicio.R ejercicio.Rmd html\n")
    quit(status = 1)
  }
  
  archivo <- args[1]
  formato <- if (length(args) >= 2) args[2] else "html"
  
  resultado <- compilar_ejercicio(archivo, formato)
  
  if (resultado) {
    quit(status = 0)
  } else {
    quit(status = 1)
  }
}

