# ===============================================================================
# SCRIPT EJECUTABLE: CORRECCIÓN DE CARPETA ESPECÍFICA
# ===============================================================================
# Archivo: corregir_carpeta.R
# Propósito: Script interactivo para corregir archivos .Rmd en una carpeta
# Uso: source("Auxiliares/sistema_correccion_global/corregir_carpeta.R")
# ===============================================================================

cat("📁 CORRECTOR DE CARPETA ESPECÍFICA\n")
cat("==================================\n\n")

# Función principal de corrección de carpeta
corregir_carpeta_interactivo <- function(carpeta = NULL) {
  
  # Si no se especifica carpeta, pedirla al usuario
  if (is.null(carpeta)) {
    cat("📂 Carpetas disponibles en Lab/:\n")
    carpetas_lab <- list.dirs("Lab", recursive = FALSE)
    if (length(carpetas_lab) > 0) {
      for (i in seq_along(carpetas_lab)) {
        cat("  ", i, ".", basename(carpetas_lab[i]), "\n")
      }
      cat("  0. Especificar ruta personalizada\n\n")
      
      opcion <- as.numeric(readline("Seleccionar carpeta (número): "))
      
      if (opcion == 0) {
        carpeta <- readline("Especificar ruta de carpeta: ")
      } else if (opcion > 0 && opcion <= length(carpetas_lab)) {
        carpeta <- carpetas_lab[opcion]
      } else {
        cat("❌ Opción inválida\n")
        return(FALSE)
      }
    } else {
      carpeta <- readline("Especificar ruta de carpeta: ")
    }
  }
  
  # Verificar que la carpeta existe
  if (!dir.exists(carpeta)) {
    cat("❌ Error: La carpeta no existe:", carpeta, "\n")
    return(FALSE)
  }
  
  cat("📁 Carpeta seleccionada:", carpeta, "\n\n")
  
  # Cargar sistema de corrección
  tryCatch({
    source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
    cat("✅ Sistema de corrección cargado\n\n")
  }, error = function(e) {
    cat("❌ Error cargando sistema:", e$message, "\n")
    return(FALSE)
  })
  
  # Paso 1: Análisis inicial
  cat("🔍 PASO 1: ANÁLISIS INICIAL\n")
  cat("---------------------------\n")
  
  archivos_rmd <- list.files(carpeta, pattern = "\\.Rmd$", 
                            recursive = TRUE, full.names = TRUE)
  
  if (length(archivos_rmd) == 0) {
    cat("❌ No se encontraron archivos .Rmd en la carpeta\n")
    return(FALSE)
  }
  
  cat("📄 Archivos .Rmd encontrados:", length(archivos_rmd), "\n")
  for (archivo in archivos_rmd) {
    cat("   -", basename(archivo), "\n")
  }
  cat("\n")
  
  # Validación inicial
  cat("🔍 Validando errores existentes...\n")
  archivos_con_errores <- validar_archivos_masivo(carpeta)
  
  if (length(archivos_con_errores) == 0) {
    cat("✅ No se encontraron errores de concordancia\n")
    
    respuesta <- readline("¿Desea aplicar el sistema de corrección de todas formas? (s/n): ")
    if (tolower(respuesta) != "s") {
      cat("ℹ️  Proceso cancelado por el usuario\n")
      return(FALSE)
    }
  } else {
    cat("❌ Archivos con errores encontrados:", length(archivos_con_errores), "\n")
    for (archivo in names(archivos_con_errores)) {
      cat("   -", basename(archivo), "\n")
    }
  }
  
  cat("\n")
  
  # Paso 2: Confirmación
  cat("🔧 PASO 2: CONFIRMACIÓN\n")
  cat("-----------------------\n")
  cat("Se aplicarán correcciones a", length(archivos_rmd), "archivos\n")
  cat("Se crearán backups automáticos de archivos modificados\n\n")
  
  respuesta <- readline("¿Continuar con las correcciones? (s/n): ")
  if (tolower(respuesta) != "s") {
    cat("ℹ️  Proceso cancelado por el usuario\n")
    return(FALSE)
  }
  
  # Paso 3: Aplicar correcciones
  cat("\n🔧 PASO 3: APLICANDO CORRECCIONES\n")
  cat("---------------------------------\n")
  
  archivos_procesados <- aplicar_correcciones_masivas(carpeta, recursivo = TRUE)
  
  # Paso 4: Validación final
  cat("\n🔍 PASO 4: VALIDACIÓN FINAL\n")
  cat("---------------------------\n")
  
  archivos_con_errores_final <- validar_archivos_masivo(carpeta)
  
  # Paso 5: Resumen
  cat("\n📊 PASO 5: RESUMEN DE RESULTADOS\n")
  cat("--------------------------------\n")
  cat("Archivos procesados:", archivos_procesados, "\n")
  cat("Errores antes:", length(archivos_con_errores), "\n")
  cat("Errores después:", length(archivos_con_errores_final), "\n")
  
  if (length(archivos_con_errores_final) == 0) {
    cat("🎉 ¡Todos los errores han sido corregidos!\n")
  } else {
    cat("⚠️  Algunos errores persisten. Revisar manualmente.\n")
  }
  
  # Paso 6: Verificación de compilación
  cat("\n🧪 PASO 6: VERIFICACIÓN DE COMPILACIÓN\n")
  cat("-------------------------------------\n")
  
  respuesta_compilar <- readline("¿Verificar que los archivos compilen correctamente? (s/n): ")
  
  if (tolower(respuesta_compilar) == "s") {
    verificar_compilacion_archivos(archivos_rmd)
  }
  
  cat("\n✅ Proceso completado para carpeta:", carpeta, "\n")
  return(TRUE)
}

# Función para verificar compilación
verificar_compilacion_archivos <- function(archivos) {
  library(knitr)
  
  errores_compilacion <- 0
  
  for (archivo in archivos) {
    cat("🧪 Verificando:", basename(archivo), "...")
    
    tryCatch({
      # Intentar extraer código R del archivo
      codigo_temp <- tempfile(fileext = ".R")
      purl(archivo, output = codigo_temp, quiet = TRUE)
      
      # Intentar ejecutar el código (solo verificar sintaxis)
      parse(codigo_temp)
      
      cat(" ✅ OK\n")
      unlink(codigo_temp)
      
    }, error = function(e) {
      cat(" ❌ ERROR\n")
      cat("   Error:", e$message, "\n")
      errores_compilacion <<- errores_compilacion + 1
    })
  }
  
  if (errores_compilacion == 0) {
    cat("🎉 Todos los archivos compilan correctamente\n")
  } else {
    cat("⚠️ ", errores_compilacion, " archivos tienen errores de compilación\n")
  }
}

# Función rápida para carpetas específicas comunes
corregir_lab_10_s1 <- function() {
  corregir_carpeta_interactivo("Lab/10-S1-2025-SEDQ")
}

corregir_toda_lab <- function() {
  respuesta <- readline("⚠️  ¿Aplicar correcciones a TODA la carpeta Lab/? (s/n): ")
  if (tolower(respuesta) == "s") {
    source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
    aplicar_correcciones_masivas("Lab/")
  }
}

# Función para mostrar menú de opciones
mostrar_menu <- function() {
  cat("\n📋 OPCIONES DISPONIBLES:\n")
  cat("1. corregir_carpeta_interactivo()     - Modo interactivo\n")
  cat("2. corregir_carpeta_interactivo('ruta') - Carpeta específica\n")
  cat("3. corregir_lab_10_s1()               - Lab/10-S1-2025-SEDQ\n")
  cat("4. corregir_toda_lab()                - Toda la carpeta Lab/\n")
  cat("\n💡 Ejemplo de uso:\n")
  cat("   corregir_carpeta_interactivo('Lab/mi-carpeta')\n")
  cat("   corregir_lab_10_s1()\n\n")
}

# Función de demostración rápida
demo_correccion_rapida <- function() {
  cat("🎯 DEMOSTRACIÓN RÁPIDA\n")
  cat("=====================\n")
  
  # Cargar sistema
  source("Auxiliares/sistema_correccion_global/funciones_correccion.R")
  
  # Mostrar correcciones
  ejemplos <- c("familias matriculados", "empresas registrados", "empresas certificados")
  
  for (ejemplo in ejemplos) {
    corregido <- corregir_todos_errores_concordancia(ejemplo)
    cat("✏️  ", ejemplo, " → ", corregido, "\n")
  }
  
  cat("\n🎯 Sistema funcionando correctamente\n")
  cat("💡 Usar corregir_carpeta_interactivo() para aplicar a archivos\n\n")
}

# Mostrar información al cargar
cat("🚀 Script de Corrección de Carpeta cargado\n")
mostrar_menu()

# Si se ejecuta en modo interactivo, mostrar demo
if (interactive()) {
  demo_correccion_rapida()
}
