# ============================================================================
# SCRIPT: Generación de 5 Demos HTML Individuales con exams2webquiz()
# FECHA: Diciembre 2025
# TECNOLOGÍA: exams2forms (paquete más reciente)
# ============================================================================

# Verificar e instalar paquetes necesarios
if (!require("exams2forms")) {
  cat("📦 Instalando exams2forms...\n")
  install.packages("exams2forms")
}

library(exams2forms)
library(exams)

# Configuración
cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║  GENERACIÓN DE DEMOS HTML INTERACTIVOS                ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

# Directorio de trabajo
setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/En-Desarrollo/consumo_telefonico_adicional")

# Archivo del ejercicio
ejercicio <- "consumo_telefonico_adicional_n2_v1.Rmd"

# Verificar que el archivo existe
if (!file.exists(ejercicio)) {
  stop(sprintf("❌ ERROR: No se encuentra el archivo %s", ejercicio))
}

cat(sprintf("✓ Archivo encontrado: %s\n", ejercicio))

# Directorio de salida
dir_salida <- "Estrategia-LinkedIn/demos-html"
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
  cat(sprintf("✓ Directorio creado: %s\n", dir_salida))
}

# Generar 5 demos individuales
cat("\n🚀 Iniciando generación de demos...\n\n")

for (i in 1:5) {
  cat(sprintf("═══ Generando Demo %d/5 ═══\n", i))
  
  # Nombre del archivo de salida
  nombre_salida <- sprintf("demo_consumo_telefonico_v%d", i)
  
  tryCatch({
    # Generar demo con exams2webquiz()
    exams2webquiz(
      file = ejercicio,
      n = 1,                    # 1 versión por demo
      name = nombre_salida,
      dir = dir_salida,
      edir = ".",              # Directorio del ejercicio
      solution = TRUE,         # Incluir solución completa
      mathjax = TRUE,          # Habilitar MathJax para fórmulas
      title = sprintf("Demo Interactivo - Consumo Telefónico (Versión %d)", i),
      encoding = "UTF-8"
    )
    
    cat(sprintf("✅ Demo %d generado: %s.html\n\n", i, nombre_salida))
    
  }, error = function(e) {
    cat(sprintf("❌ ERROR en Demo %d: %s\n\n", i, e$message))
  })
}

# Verificar archivos generados
cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║  VERIFICACIÓN DE ARCHIVOS GENERADOS                   ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

archivos_html <- list.files(dir_salida, pattern = "\\.html$", full.names = TRUE)

if (length(archivos_html) == 5) {
  cat("✅ ÉXITO: 5 demos HTML generados correctamente\n\n")
  
  for (archivo in archivos_html) {
    tamano <- file.info(archivo)$size / 1024
    cat(sprintf("  📄 %s (%.1f KB)\n", basename(archivo), tamano))
  }
  
  cat("\n📁 Ubicación: ", normalizePath(dir_salida), "\n")
  
  cat("\n🎯 PRÓXIMOS PASOS:\n")
  cat("1. Abrir los archivos HTML en un navegador para verificar\n")
  cat("2. Ejecutar script de copia a docs/ para GitHub Pages\n")
  cat("3. Crear imágenes para carrusel de LinkedIn\n")
  cat("4. Preparar texto de publicación\n\n")
  
} else {
  cat(sprintf("⚠️ ADVERTENCIA: Se generaron %d archivos (esperados: 5)\n", length(archivos_html)))
  cat("Revisar errores anteriores\n\n")
}

# Generar recursos adicionales (PDF y Moodle)
cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║  GENERACIÓN DE RECURSOS DESCARGABLES                  ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

dir_recursos <- "Estrategia-LinkedIn/recursos-descargables"
if (!dir.exists(dir_recursos)) {
  dir.create(dir_recursos, recursive = TRUE)
}

# PDF con 10 versiones
cat("📄 Generando PDF con 10 versiones...\n")
tryCatch({
  exams2pdf(
    file = ejercicio,
    n = 10,
    name = "muestra_10_versiones_consumo_telefonico",
    dir = dir_recursos,
    edir = ".",
    template = "plain",
    header = list(
      Date = format(Sys.Date(), "%d/%m/%Y"),
      ID = "DEMO-LINKEDIN-2025"
    )
  )
  cat("✅ PDF generado exitosamente\n\n")
}, error = function(e) {
  cat(sprintf("❌ ERROR generando PDF: %s\n\n", e$message))
})

# Moodle XML
cat("🎓 Generando archivo Moodle XML...\n")
tryCatch({
  exams2moodle(
    file = ejercicio,
    n = 5,
    name = "consumo_telefonico_moodle",
    dir = dir_recursos,
    edir = "."
  )
  cat("✅ Archivo Moodle XML generado exitosamente\n\n")
}, error = function(e) {
  cat(sprintf("❌ ERROR generando Moodle XML: %s\n\n", e$message))
})

# Resumen final
cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║  PROCESO COMPLETADO                                    ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

cat("📊 RESUMEN:\n")
cat(sprintf("  ✅ Demos HTML: %d archivos\n", length(archivos_html)))
cat(sprintf("  ✅ PDF: %s\n", file.exists(file.path(dir_recursos, "muestra_10_versiones_consumo_telefonico1.pdf"))))
cat(sprintf("  ✅ Moodle XML: %s\n", file.exists(file.path(dir_recursos, "consumo_telefonico_moodle.xml"))))

cat("\n📁 UBICACIONES:\n")
cat(sprintf("  Demos HTML: %s\n", normalizePath(dir_salida)))
cat(sprintf("  Recursos: %s\n", normalizePath(dir_recursos)))

cat("\n✨ ¡Listo para publicar en LinkedIn!\n\n")

