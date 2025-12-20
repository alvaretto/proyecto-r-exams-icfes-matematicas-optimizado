# ===============================================================================
# DEMOSTRACIÓN DE OPCIONES DEL SCRIPT SEMILLEROCLOZE.R
# ===============================================================================
# Este script demuestra todas las opciones disponibles del generador híbrido

library(exams)

# Cargar el script principal
source("SemilleroCloze.R")

cat("🎯 DEMOSTRACIÓN DE TODAS LAS OPCIONES DISPONIBLES\n")
cat(strrep("=", 60), "\n\n")

# ===============================================================================
# OPCIÓN 1: SOLO HTML
# ===============================================================================
cat("📋 OPCIÓN 1: SOLO HTML (recomendado para pruebas)\n")
cat("   - Genera archivos HTML interactivos\n")
cat("   - Ideal para visualización y pruebas rápidas\n")
cat("   - No requiere plataforma LMS específica\n\n")

# Ejecutar opción 1
cat("🚀 Ejecutando Opción 1...\n")
resultados_html <- generar_todos_formatos(c("html"))
cat("✅ Opción 1 completada\n\n")

# ===============================================================================
# OPCIÓN 2: SOLO MOODLE
# ===============================================================================
cat("📋 OPCIÓN 2: SOLO MOODLE (recomendado para producción directa)\n")
cat("   - Genera únicamente archivo XML para Moodle\n")
cat("   - Importación directa a Moodle\n")
cat("   - Eficiente cuando solo necesitas Moodle\n\n")

# Ejecutar opción 2
cat("🚀 Ejecutando Opción 2...\n")
resultados_moodle <- generar_todos_formatos(c("moodle"))
cat("✅ Opción 2 completada\n\n")

# ===============================================================================
# OPCIÓN 3: HTML + MOODLE
# ===============================================================================
cat("📋 OPCIÓN 3: HTML + MOODLE (recomendado para producción completa)\n")
cat("   - Genera archivos HTML para revisión\n")
cat("   - Genera archivo XML para Moodle\n")
cat("   - Mejor de ambos mundos\n\n")

# Ejecutar opción 3
cat("🚀 Ejecutando Opción 3...\n")
resultados_completo <- generar_todos_formatos(c("html", "moodle"))
cat("✅ Opción 3 completada\n\n")

# ===============================================================================
# OPCIÓN 4: TODOS LOS FORMATOS
# ===============================================================================
cat("📋 OPCIÓN 4: TODOS LOS FORMATOS (máxima compatibilidad)\n")
cat("   - HTML: Para visualización\n")
cat("   - Moodle: Para Moodle LMS\n")
cat("   - Canvas: Para Canvas LMS\n")
cat("   - PDF: Para exámenes escritos\n\n")

# Ejecutar opción 4
cat("🚀 Ejecutando Opción 4...\n")
resultados_todos <- generar_todos_formatos(c("html", "moodle", "canvas", "pdf"))
cat("✅ Opción 4 completada\n\n")

# ===============================================================================
# RESUMEN FINAL
# ===============================================================================
cat(strrep("=", 60), "\n")
cat("📊 RESUMEN DE TODAS LAS OPCIONES EJECUTADAS\n")
cat(strrep("=", 60), "\n\n")

opciones_ejecutadas <- list(
  "Opción 1 (Solo HTML)" = resultados_html,
  "Opción 2 (Solo Moodle)" = resultados_moodle,
  "Opción 3 (HTML + Moodle)" = resultados_completo,
  "Opción 4 (Todos los formatos)" = resultados_todos
)

for (nombre_opcion in names(opciones_ejecutadas)) {
  resultado <- opciones_ejecutadas[[nombre_opcion]]
  exitos <- sum(unlist(resultado))
  total <- length(resultado)
  
  cat(sprintf("%-25s: %d/%d formatos exitosos\n", 
              nombre_opcion, exitos, total))
}

cat("\n📁 Todos los archivos generados están en: salida_hibrida/\n")
cat("💾 Información detallada en: salida_hibrida/info_sesion.rds\n")

# ===============================================================================
# INSTRUCCIONES DE USO
# ===============================================================================
cat("\n", strrep("=", 60), "\n")
cat("📖 INSTRUCCIONES DE USO DEL SCRIPT PRINCIPAL\n")
cat(strrep("=", 60), "\n\n")

cat("🔧 EJECUCIÓN INTERACTIVA:\n")
cat("   Rscript SemilleroCloze.R\n")
cat("   (El script mostrará un menú de opciones)\n\n")

cat("🔧 EJECUCIÓN PROGRAMÁTICA:\n")
cat("   # Solo HTML\n")
cat("   Rscript -e \"source('SemilleroCloze.R'); generar_todos_formatos(c('html'))\"\n\n")
cat("   # Solo Moodle\n")
cat("   Rscript -e \"source('SemilleroCloze.R'); generar_todos_formatos(c('moodle'))\"\n\n")
cat("   # HTML + Moodle\n")
cat("   Rscript -e \"source('SemilleroCloze.R'); generar_todos_formatos(c('html', 'moodle'))\"\n\n")

cat("🎯 RECOMENDACIONES DE USO:\n")
cat("   • Opción 1 (Solo HTML): Para pruebas y revisión\n")
cat("   • Opción 2 (Solo Moodle): Para producción directa en Moodle\n")
cat("   • Opción 3 (HTML + Moodle): Para producción completa\n")
cat("   • Opción 4 (Todos): Para máxima compatibilidad\n\n")

cat("✨ VENTAJAS DEL FORMATO HÍBRIDO:\n")
cat("   • Previene resolución mecánica\n")
cat("   • Promueve pensamiento analítico\n")
cat("   • Evaluación integral (proceso + resultado)\n")
cat("   • Compatible con múltiples plataformas\n\n")

cat(strrep("=", 60), "\n")
cat("🎉 DEMOSTRACIÓN COMPLETADA EXITOSAMENTE\n")
cat("📁 Revise todos los archivos generados en: salida_hibrida/\n")
cat(strrep("=", 60), "\n")
