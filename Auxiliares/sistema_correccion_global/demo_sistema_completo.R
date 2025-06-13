# ===============================================================================
# DEMOSTRACIÓN COMPLETA DEL SISTEMA DE CORRECCIÓN GLOBAL
# ===============================================================================
# Archivo: demo_sistema_completo.R
# Propósito: Demostrar todas las capacidades del sistema
# Uso: source("Auxiliares/sistema_correccion_global/demo_sistema_completo.R")
# ===============================================================================

cat("🎯 DEMOSTRACIÓN DEL SISTEMA DE CORRECCIÓN GLOBAL\n")
cat("" %+% rep("=", 60) %+% "\n\n")

# Operador de concatenación
`%+%` <- function(a, b) paste0(a, b)

# Cargar todo el sistema
source("Auxiliares/sistema_correccion_global/funciones_correccion.R")
source("Auxiliares/sistema_correccion_global/aplicador_automatico.R")
source("Auxiliares/sistema_correccion_global/validador_masivo.R")

cat("📦 SISTEMA CARGADO COMPLETAMENTE\n\n")

# ===============================================================================
# DEMOSTRACIÓN 1: CORRECCIONES BÁSICAS
# ===============================================================================

cat("🔧 DEMOSTRACIÓN 1: CORRECCIONES BÁSICAS\n")
cat("" %+% rep("-", 40) %+% "\n")

errores_ejemplo <- c(
  "familias matriculados",
  "empresas registrados", 
  "empresas certificados",
  "familias beneficiarios",
  "empresas asegurados"
)

for (error in errores_ejemplo) {
  corregido <- corregir_todos_errores_concordancia(error)
  cat("✏️  ", error, " → ", corregido, "\n")
}

cat("\n")

# ===============================================================================
# DEMOSTRACIÓN 2: TEXTO COMPLEJO
# ===============================================================================

cat("🔧 DEMOSTRACIÓN 2: CORRECCIÓN DE TEXTO COMPLEJO\n")
cat("" %+% rep("-", 40) %+% "\n")

texto_complejo <- "Según el ICBF, el doble de familias matriculados y el promedio de empresas registrados se calcularía mediante la operación de familias beneficiarios."

cat("📝 Texto original:\n")
cat("   ", texto_complejo, "\n\n")

texto_corregido <- aplicar_correcciones_completas(texto_complejo)

cat("✅ Texto corregido:\n")
cat("   ", texto_corregido, "\n\n")

# ===============================================================================
# DEMOSTRACIÓN 3: VALIDACIÓN DE COHERENCIA
# ===============================================================================

cat("🔍 DEMOSTRACIÓN 3: VALIDACIÓN DE COHERENCIA\n")
cat("" %+% rep("-", 40) %+% "\n")

casos_coherencia <- list(
  list(entidad = "ICBF", elemento = "familias", condicion = "beneficiarias", total = 1000000),
  list(entidad = "ICBF", elemento = "vehículos", condicion = "asegurados", total = 1000000),
  list(entidad = "DANE", elemento = "hogares", condicion = "registrados", total = 2000000),
  list(entidad = "Ministerio de Transporte", elemento = "familias", condicion = "matriculadas", total = 500000)
)

for (caso in casos_coherencia) {
  errores <- validar_coherencia(caso$entidad, caso$elemento, caso$condicion, caso$total)
  
  if (length(errores) == 0) {
    cat("✅ ", caso$entidad, " - ", caso$elemento, ": COHERENTE\n")
  } else {
    cat("❌ ", caso$entidad, " - ", caso$elemento, ": ", paste(errores, collapse = ", "), "\n")
  }
}

cat("\n")

# ===============================================================================
# DEMOSTRACIÓN 4: ESTADÍSTICAS DEL PROYECTO
# ===============================================================================

cat("📊 DEMOSTRACIÓN 4: ESTADÍSTICAS DEL PROYECTO\n")
cat("" %+% rep("-", 40) %+% "\n")

stats <- generar_estadisticas_calidad(".")

cat("📈 Resumen de calidad:\n")
cat("   - Archivos con errores de concordancia: ", stats$con_errores_concordancia, 
    " (", stats$porcentaje_con_errores, "%)\n")
cat("   - Archivos con sistema de corrección: ", stats$con_sistema_correccion, 
    " (", stats$porcentaje_con_sistema, "%)\n")
cat("   - Archivos con metadatos ICFES: ", stats$con_metadatos_icfes, 
    " (", stats$porcentaje_con_metadatos, "%)\n")

cat("\n")

# ===============================================================================
# DEMOSTRACIÓN 5: PROCESAMIENTO DE DATOS
# ===============================================================================

cat("🔄 DEMOSTRACIÓN 5: PROCESAMIENTO DE DATOS\n")
cat("" %+% rep("-", 40) %+% "\n")

# Simular datos como los que genera un ejercicio
datos_ejemplo <- list(
  entidad = "ICBF",
  elemento = "familias",
  condicion = "beneficiario",  # Error intencional
  total = 1500000,
  numerador = 3,
  denominador = 5
)

cat("📝 Datos originales:\n")
cat("   elemento: ", datos_ejemplo$elemento, "\n")
cat("   condicion: ", datos_ejemplo$condicion, "\n")

# Procesar con correcciones
datos_corregidos <- procesar_datos_con_correcciones(datos_ejemplo)

cat("\n✅ Datos corregidos:\n")
cat("   elemento_texto: ", datos_corregidos$elemento_texto, "\n")
cat("   condicion_texto: ", datos_corregidos$condicion_texto, "\n")
cat("   condicion_corregida: ", datos_corregidos$condicion_corregida, "\n")

cat("\n")

# ===============================================================================
# DEMOSTRACIÓN 6: VALIDACIÓN DE ARCHIVOS
# ===============================================================================

cat("🔍 DEMOSTRACIÓN 6: VALIDACIÓN DE ARCHIVOS\n")
cat("" %+% rep("-", 40) %+% "\n")

# Validar el directorio Lab/
cat("📁 Validando archivos en Lab/:\n")
archivos_con_errores <- validar_archivos_masivo("Lab/")

if (length(archivos_con_errores) > 0) {
  cat("\n📋 Archivos que necesitan corrección:\n")
  for (archivo in names(archivos_con_errores)) {
    cat("   📄 ", basename(archivo), "\n")
  }
} else {
  cat("✅ No se encontraron errores en Lab/\n")
}

cat("\n")

# ===============================================================================
# DEMOSTRACIÓN 7: EJEMPLO DE USO EN CÓDIGO .RMD
# ===============================================================================

cat("📝 DEMOSTRACIÓN 7: CÓDIGO PARA USAR EN .RMD\n")
cat("" %+% rep("-", 40) %+% "\n")

codigo_ejemplo <- '
# En el chunk de inicio:
source("Auxiliares/sistema_correccion_global/funciones_correccion.R")

# En la función generar_datos():
generar_datos <- function() {
  # ... tu lógica aquí ...
  datos_corregidos <- procesar_datos_con_correcciones(datos)
  return(datos_corregidos)
}

# Usar variables corregidas:
elemento <- datos$elemento_texto    # Sin errores
condicion <- datos$condicion_texto  # Sin errores
'

cat("💡 Código de ejemplo:\n")
cat(codigo_ejemplo)

cat("\n")

# ===============================================================================
# RESUMEN FINAL
# ===============================================================================

cat("🎯 RESUMEN DE CAPACIDADES DEL SISTEMA\n")
cat("" %+% rep("=", 60) %+% "\n")

capacidades <- c(
  "✅ Corrección automática de errores de concordancia",
  "✅ Validación de coherencia entidad-elemento", 
  "✅ Aplicación masiva a múltiples archivos",
  "✅ Validación y reportes detallados",
  "✅ Estadísticas de calidad del proyecto",
  "✅ Fácil integración en nuevos ejercicios",
  "✅ Procesamiento automático de datos",
  "✅ Escalabilidad para nuevos tipos de errores"
)

for (capacidad in capacidades) {
  cat(capacidad, "\n")
}

cat("\n🚀 SISTEMA LISTO PARA PRODUCCIÓN\n")
cat("📖 Consultar README.md para documentación completa\n")
cat("💡 Usar aplicador_automatico.R para aplicar a archivos existentes\n")
cat("🎯 Usar template_inclusion.R para nuevos ejercicios\n\n")

cat("🎉 DEMOSTRACIÓN COMPLETADA EXITOSAMENTE\n")
