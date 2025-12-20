# Configuración de Compilación para Ejercicio de Trigonometría
# Archivo: SemilleroUnico_v2.R
# Ejercicio: trigonometria_punto_k_v1.Rnw

# Cargar librerías necesarias
library(exams)
library(knitr)
library(tools)

# Configurar directorio de trabajo (ya estamos en el directorio correcto)
# setwd("Lab-Manjaro/PuntoK-Rnw/")

# Configuración global
options(OutDec = ".")
Sys.setlocale("LC_ALL", "C")

# Archivo del ejercicio
archivo_ejercicio <- "trigonometria_punto_k_v3.Rnw"

# Verificar que el archivo existe
if(!file.exists(archivo_ejercicio)) {
  stop("Error: No se encuentra el archivo ", archivo_ejercicio)
}

cat("🎯 CONFIGURACIÓN DE COMPILACIÓN\n")
cat("==============================\n")
cat("Archivo:", archivo_ejercicio, "\n")
cat("Directorio:", getwd(), "\n\n")

# Función para compilar HTML
compilar_html <- function(n = 5, nombre = "trigonometria_punto_k") {
  cat("📄 Compilando HTML...\n")
  
  tryCatch({
    # Crear directorio de salida
    dir_salida <- "salida_html"
    if(!dir.exists(dir_salida)) dir.create(dir_salida)
    
    # Compilar
    resultado <- exams2html(archivo_ejercicio, 
                           n = n,
                           name = nombre,
                           dir = dir_salida,
                           encoding = "UTF-8",
                           quiet = FALSE)
    
    cat("✅ HTML compilado exitosamente en:", dir_salida, "\n")
    return(resultado)
    
  }, error = function(e) {
    cat("❌ Error en compilación HTML:", e$message, "\n")
    return(NULL)
  })
}

# Función para compilar PDF
compilar_pdf <- function(n = 3, nombre = "trigonometria_punto_k") {
  cat("📄 Compilando PDF...\n")
  
  tryCatch({
    # Crear directorio de salida
    dir_salida <- "salida_pdf"
    if(!dir.exists(dir_salida)) dir.create(dir_salida)
    
    # Compilar
    resultado <- exams2pdf(archivo_ejercicio, 
                          n = n,
                          name = nombre,
                          dir = dir_salida,
                          encoding = "UTF-8",
                          quiet = FALSE)
    
    cat("✅ PDF compilado exitosamente en:", dir_salida, "\n")
    return(resultado)
    
  }, error = function(e) {
    cat("❌ Error en compilación PDF:", e$message, "\n")
    return(NULL)
  })
}

# Función para compilar Moodle
compilar_moodle <- function(n = 10, nombre = "trigonometria_punto_k") {
  cat("🎓 Compilando Moodle...\n")
  
  tryCatch({
    # Crear directorio de salida
    dir_salida <- "salida_moodle"
    if(!dir.exists(dir_salida)) dir.create(dir_salida)
    
    # Compilar
    resultado <- exams2moodle(archivo_ejercicio, 
                             n = n,
                             name = nombre,
                             dir = dir_salida,
                             encoding = "UTF-8",
                             quiet = FALSE)
    
    cat("✅ Moodle compilado exitosamente en:", dir_salida, "\n")
    return(resultado)
    
  }, error = function(e) {
    cat("❌ Error en compilación Moodle:", e$message, "\n")
    return(NULL)
  })
}

# Función para compilar NOPS (exámenes impresos)
compilar_nops <- function(n = 5, nombre = "trigonometria_punto_k") {
  cat("📋 Compilando NOPS...\n")
  
  tryCatch({
    # Crear directorio de salida
    dir_salida <- "salida_nops"
    if(!dir.exists(dir_salida)) dir.create(dir_salida)
    
    # Compilar
    resultado <- exams2nops(archivo_ejercicio, 
                           n = n,
                           name = nombre,
                           dir = dir_salida,
                           encoding = "UTF-8",
                           quiet = FALSE)
    
    cat("✅ NOPS compilado exitosamente en:", dir_salida, "\n")
    return(resultado)
    
  }, error = function(e) {
    cat("❌ Error en compilación NOPS:", e$message, "\n")
    return(NULL)
  })
}

# Función para compilar todos los formatos
compilar_todos <- function(n_html = 5, n_pdf = 3, n_moodle = 10, n_nops = 5) {
  cat("\n🚀 COMPILANDO TODOS LOS FORMATOS\n")
  cat("=================================\n\n")
  
  # Compilar cada formato
  html_result <- compilar_html(n_html)
  pdf_result <- compilar_pdf(n_pdf)
  moodle_result <- compilar_moodle(n_moodle)
  nops_result <- compilar_nops(n_nops)
  
  # Resumen
  cat("\n📊 RESUMEN DE COMPILACIÓN\n")
  cat("=========================\n")
  cat("HTML:", ifelse(is.null(html_result), "❌ Error", "✅ Exitoso"), "\n")
  cat("PDF:", ifelse(is.null(pdf_result), "❌ Error", "✅ Exitoso"), "\n")
  cat("Moodle:", ifelse(is.null(moodle_result), "❌ Error", "✅ Exitoso"), "\n")
  cat("NOPS:", ifelse(is.null(nops_result), "❌ Error", "✅ Exitoso"), "\n")
  
  # Verificar archivos generados
  cat("\n📁 ARCHIVOS GENERADOS\n")
  cat("=====================\n")
  
  dirs <- c("salida_html", "salida_pdf", "salida_moodle", "salida_nops")
  for(dir in dirs) {
    if(dir.exists(dir)) {
      archivos <- list.files(dir, recursive = TRUE)
      cat(dir, ":", length(archivos), "archivos\n")
      if(length(archivos) > 0) {
        cat("  -", paste(head(archivos, 3), collapse = ", "))
        if(length(archivos) > 3) cat(", ...")
        cat("\n")
      }
    }
  }
  
  return(list(html = html_result, pdf = pdf_result, moodle = moodle_result, nops = nops_result))
}

# Función de testing rápido
test_rapido <- function() {
  cat("\n🧪 TEST RÁPIDO\n")
  cat("==============\n")
  
  # Compilar una versión de cada formato
  resultado <- compilar_todos(n_html = 1, n_pdf = 1, n_moodle = 2, n_nops = 1)
  
  # Verificar que al menos uno funcionó
  exitos <- sum(!sapply(resultado, is.null))
  
  if(exitos >= 2) {
    cat("\n✅ TEST RÁPIDO EXITOSO\n")
    cat("El ejercicio funciona correctamente en", exitos, "formatos.\n")
  } else {
    cat("\n❌ TEST RÁPIDO FALLÓ\n")
    cat("Solo", exitos, "formatos funcionaron correctamente.\n")
  }
  
  return(resultado)
}

# Función principal de demostración
demo_completo <- function() {
  cat("\n🎯 DEMOSTRACIÓN COMPLETA\n")
  cat("========================\n")
  
  # Ejecutar compilación completa
  resultado <- compilar_todos()
  
  cat("\n🎉 DEMOSTRACIÓN COMPLETADA\n")
  cat("Revisa las carpetas de salida para ver los resultados.\n")
  
  return(resultado)
}

# Mensaje de bienvenida
cat("\n🎯 SEMILLERO ÚNICO v2 - TRIGONOMETRÍA PUNTO K\n")
cat("==============================================\n")
cat("Funciones disponibles:\n")
cat("- compilar_html(n)     : Compilar versiones HTML\n")
cat("- compilar_pdf(n)      : Compilar versiones PDF\n")
cat("- compilar_moodle(n)   : Compilar versiones Moodle\n")
cat("- compilar_nops(n)     : Compilar versiones NOPS\n")
cat("- compilar_todos()     : Compilar todos los formatos\n")
cat("- test_rapido()        : Test rápido de funcionamiento\n")
cat("- demo_completo()      : Demostración completa\n\n")

cat("💡 Ejemplo de uso:\n")
cat("   source('SemilleroUnico_v2.R')\n")
cat("   test_rapido()              # Para probar rápidamente\n")
cat("   compilar_html(10)          # Para generar 10 versiones HTML\n")
cat("   demo_completo()            # Para demostración completa\n\n")

cat("📁 Archivo listo para compilación:", archivo_ejercicio, "\n")
cat("🚀 ¡Ejecuta las funciones para generar los ejercicios!\n")
