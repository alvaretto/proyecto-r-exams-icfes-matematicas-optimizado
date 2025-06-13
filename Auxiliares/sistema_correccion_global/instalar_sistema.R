# ===============================================================================
# INSTALADOR AUTOMÁTICO DEL SISTEMA DE CORRECCIÓN GLOBAL
# ===============================================================================
# Archivo: instalar_sistema.R
# Propósito: Instalar y configurar el sistema en todo el proyecto
# Uso: source("Auxiliares/sistema_correccion_global/instalar_sistema.R")
# ===============================================================================

cat("🚀 INSTALADOR DEL SISTEMA DE CORRECCIÓN GLOBAL\n")
cat("" %+% rep("=", 50) %+% "\n")

# Operador de concatenación
`%+%` <- function(a, b) paste0(a, b)

#' Verificar y crear estructura de directorios
verificar_estructura <- function() {
  cat("📁 Verificando estructura de directorios...\n")
  
  directorio_sistema <- "Auxiliares/sistema_correccion_global"
  
  if (!dir.exists(directorio_sistema)) {
    dir.create(directorio_sistema, recursive = TRUE)
    cat("✅ Directorio creado:", directorio_sistema, "\n")
  } else {
    cat("✅ Directorio existe:", directorio_sistema, "\n")
  }
  
  # Verificar archivos del sistema
  archivos_requeridos <- c(
    "funciones_correccion.R",
    "aplicador_automatico.R", 
    "template_inclusion.R",
    "validador_masivo.R",
    "README.md"
  )
  
  archivos_faltantes <- c()
  for (archivo in archivos_requeridos) {
    ruta_completa <- file.path(directorio_sistema, archivo)
    if (!file.exists(ruta_completa)) {
      archivos_faltantes <- c(archivos_faltantes, archivo)
    }
  }
  
  if (length(archivos_faltantes) > 0) {
    cat("❌ Archivos faltantes:", paste(archivos_faltantes, collapse = ", "), "\n")
    return(FALSE)
  } else {
    cat("✅ Todos los archivos del sistema están presentes\n")
    return(TRUE)
  }
}

#' Probar el sistema de corrección
probar_sistema <- function() {
  cat("🧪 Probando el sistema de corrección...\n")
  
  tryCatch({
    # Cargar el sistema
    source("Auxiliares/sistema_correccion_global/funciones_correccion.R")
    
    # Probar correcciones básicas
    test1 <- corregir_todos_errores_concordancia("familias matriculados")
    test2 <- corregir_todos_errores_concordancia("empresas registrados")
    
    if (test1 == "familias matriculadas" && test2 == "empresas registradas") {
      cat("✅ Pruebas de corrección exitosas\n")
      return(TRUE)
    } else {
      cat("❌ Pruebas de corrección fallaron\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("❌ Error al probar el sistema:", e$message, "\n")
    return(FALSE)
  })
}

#' Generar reporte inicial del proyecto
generar_reporte_inicial <- function() {
  cat("📊 Generando reporte inicial del proyecto...\n")
  
  tryCatch({
    source("Auxiliares/sistema_correccion_global/validador_masivo.R")
    
    # Generar estadísticas
    stats <- generar_estadisticas_calidad(".")
    
    cat("📋 ESTADO INICIAL DEL PROYECTO:\n")
    cat("   Total archivos .Rmd:", stats$total_archivos, "\n")
    cat("   Con errores de concordancia:", stats$con_errores_concordancia, "\n")
    cat("   Porcentaje con errores:", stats$porcentaje_con_errores, "%\n")
    
    return(stats)
    
  }, error = function(e) {
    cat("❌ Error al generar reporte:", e$message, "\n")
    return(NULL)
  })
}

#' Aplicar correcciones automáticas a todo el proyecto
aplicar_correcciones_proyecto <- function() {
  cat("🔧 Aplicando correcciones automáticas...\n")
  
  respuesta <- readline("¿Desea aplicar correcciones a todos los archivos .Rmd? (s/n): ")
  
  if (tolower(respuesta) %in% c("s", "si", "y", "yes")) {
    tryCatch({
      source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
      
      # Aplicar correcciones masivas
      archivos_procesados <- aplicar_correcciones_masivas(".", recursivo = TRUE)
      
      cat("✅ Correcciones aplicadas a", archivos_procesados, "archivos\n")
      return(TRUE)
      
    }, error = function(e) {
      cat("❌ Error al aplicar correcciones:", e$message, "\n")
      return(FALSE)
    })
  } else {
    cat("ℹ️  Correcciones no aplicadas (por elección del usuario)\n")
    return(FALSE)
  }
}

#' Generar reporte final
generar_reporte_final <- function() {
  cat("📊 Generando reporte final...\n")
  
  tryCatch({
    source("Auxiliares/sistema_correccion_global/validador_masivo.R")
    
    # Generar reporte completo
    resultados <- generar_reporte_validacion(".", generar_archivo = TRUE)
    
    # Estadísticas finales
    stats <- generar_estadisticas_calidad(".")
    
    cat("📋 ESTADO FINAL DEL PROYECTO:\n")
    cat("   Total archivos .Rmd:", stats$total_archivos, "\n")
    cat("   Con sistema de corrección:", stats$con_sistema_correccion, "\n")
    cat("   Con errores de concordancia:", stats$con_errores_concordancia, "\n")
    cat("   Porcentaje con errores:", stats$porcentaje_con_errores, "%\n")
    
    return(stats)
    
  }, error = function(e) {
    cat("❌ Error al generar reporte final:", e$message, "\n")
    return(NULL)
  })
}

#' Mostrar instrucciones de uso
mostrar_instrucciones <- function() {
  cat("\n📖 INSTRUCCIONES DE USO:\n")
  cat("" %+% rep("-", 30) %+% "\n")
  cat("1. Para nuevos ejercicios:\n")
  cat("   - Copiar código de template_inclusion.R\n")
  cat("   - Modificar función generar_datos()\n")
  cat("   - Usar variables elemento_texto y condicion_texto\n\n")
  
  cat("2. Para validar archivos:\n")
  cat("   source('Auxiliares/sistema_correccion_global/validador_masivo.R')\n")
  cat("   generar_reporte_validacion('Lab/')\n\n")
  
  cat("3. Para aplicar correcciones:\n")
  cat("   source('Auxiliares/sistema_correccion_global/aplicador_automatico.R')\n")
  cat("   aplicar_correcciones_masivas('Lab/')\n\n")
  
  cat("4. Para estadísticas:\n")
  cat("   generar_estadisticas_calidad('.')\n\n")
}

#' Función principal de instalación
instalar_sistema_completo <- function() {
  cat("🎯 Iniciando instalación del Sistema de Corrección Global\n\n")
  
  # Paso 1: Verificar estructura
  if (!verificar_estructura()) {
    cat("❌ Instalación abortada: estructura incompleta\n")
    return(FALSE)
  }
  
  # Paso 2: Probar sistema
  if (!probar_sistema()) {
    cat("❌ Instalación abortada: sistema no funciona\n")
    return(FALSE)
  }
  
  # Paso 3: Reporte inicial
  stats_inicial <- generar_reporte_inicial()
  
  # Paso 4: Aplicar correcciones (opcional)
  correcciones_aplicadas <- aplicar_correcciones_proyecto()
  
  # Paso 5: Reporte final
  if (correcciones_aplicadas) {
    stats_final <- generar_reporte_final()
  }
  
  # Paso 6: Instrucciones
  mostrar_instrucciones()
  
  cat("\n🎉 INSTALACIÓN COMPLETADA EXITOSAMENTE\n")
  cat("✅ Sistema de Corrección Global listo para usar\n")
  cat("📖 Consultar README.md para documentación completa\n")
  
  return(TRUE)
}

# Ejecutar instalación automáticamente
if (interactive()) {
  instalar_sistema_completo()
} else {
  cat("💡 Para instalar, ejecutar: instalar_sistema_completo()\n")
}
