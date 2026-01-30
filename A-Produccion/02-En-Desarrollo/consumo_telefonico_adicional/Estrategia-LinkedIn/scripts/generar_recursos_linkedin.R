# ============================================================================
# SCRIPT: Generación de Recursos para LinkedIn (PDF + HTML + Moodle)
# FECHA: Diciembre 2025
# SISTEMA: Usa SemilleroUnico_v2.R que ya funciona en el proyecto
# ============================================================================

# Configuración inicial
cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║  GENERACIÓN DE RECURSOS PARA LINKEDIN                 ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

# Directorio de trabajo
setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/En-Desarrollo/consumo_telefonico_adicional")

# Cargar librerías necesarias
library(exams)
library(reticulate)
library(knitr)

# Verificar que el archivo existe
ejercicio <- "consumo_telefonico_adicional_n2_v1.Rmd"
if (!file.exists(ejercicio)) {
  stop(sprintf("❌ ERROR: No se encuentra el archivo %s", ejercicio))
}

cat(sprintf("✓ Archivo encontrado: %s\n\n", ejercicio))

# Crear directorios de salida
dir_recursos <- "Estrategia-LinkedIn/recursos-descargables"
dir_demos <- "Estrategia-LinkedIn/demos-html"

if (!dir.exists(dir_recursos)) {
  dir.create(dir_recursos, recursive = TRUE)
  cat(sprintf("✓ Directorio creado: %s\n", dir_recursos))
}

if (!dir.exists(dir_demos)) {
  dir.create(dir_demos, recursive = TRUE)
  cat(sprintf("✓ Directorio creado: %s\n", dir_demos))
}

# ============================================================================
# PASO 1: Generar PDF con 10 versiones usando SemilleroUnico_v2.R
# ============================================================================

cat("\n📄 PASO 1: Generando PDF con 10 versiones...\n")

# Cargar el script que ya funciona
source("SemilleroUnico_v2.R")

tryCatch({
  # Generar 10 versiones en PDF
  set.seed(NULL)  # Semilla aleatoria
  
  exams2pdf(
    file = ejercicio,
    n = 10,
    name = "muestra_10_versiones_consumo_telefonico",
    dir = dir_recursos,
    edir = ".",
    template = c("pcielo.tex", "pcielo_nosol.tex"),
    encoding = "UTF-8",
    header = list(
      Date = format(Sys.Date(), "%d/%m/%Y"),
      ID = "DEMO-LINKEDIN-2025"
    )
  )
  
  cat("✅ PDF generado exitosamente\n")
  
  # Verificar archivo
  pdf_file <- file.path(dir_recursos, "muestra_10_versiones_consumo_telefonico1.pdf")
  if (file.exists(pdf_file)) {
    tamano <- file.info(pdf_file)$size / 1024
    cat(sprintf("   📄 Tamaño: %.1f KB\n", tamano))
  }
  
}, error = function(e) {
  cat(sprintf("❌ ERROR generando PDF: %s\n", e$message))
  cat("💡 Alternativa: Copiar PDF existente de carpeta 'salida/'\n")
  
  # Copiar PDF existente si está disponible
  pdf_existente <- "salida/consumo_telefonico_adicional_n2_v1_1.pdf"
  if (file.exists(pdf_existente)) {
    file.copy(pdf_existente, 
              file.path(dir_recursos, "muestra_10_versiones_consumo_telefonico1.pdf"),
              overwrite = TRUE)
    cat("✅ PDF copiado desde salida existente\n")
  }
})

# ============================================================================
# PASO 2: Generar archivo Moodle XML
# ============================================================================

cat("\n🎓 PASO 2: Generando archivo Moodle XML...\n")

# Cargar script de Moodle
source("SemilleroMoodle_v2.R")

tryCatch({
  set.seed(NULL)
  
  exams2moodle(
    file = ejercicio,
    n = 5,
    name = "consumo_telefonico_moodle",
    dir = dir_recursos,
    edir = ".",
    encoding = "UTF-8"
  )
  
  cat("✅ Archivo Moodle XML generado exitosamente\n")
  
  # Verificar archivo
  xml_file <- file.path(dir_recursos, "consumo_telefonico_moodle.xml")
  if (file.exists(xml_file)) {
    tamano <- file.info(xml_file)$size / 1024
    cat(sprintf("   📄 Tamaño: %.1f KB\n", tamano))
  }
  
}, error = function(e) {
  cat(sprintf("❌ ERROR generando Moodle XML: %s\n", e$message))
})

# ============================================================================
# PASO 3: Generar 5 demos HTML individuales
# ============================================================================

cat("\n🌐 PASO 3: Generando 5 demos HTML...\n")

for (i in 1:5) {
  cat(sprintf("\n  Demo %d/5...", i))
  
  tryCatch({
    set.seed(NULL)
    
    exams2html(
      file = ejercicio,
      n = 1,
      name = sprintf("demo_consumo_telefonico_v%d", i),
      dir = dir_demos,
      edir = ".",
      encoding = "UTF-8",
      mathjax = TRUE,
      solution = TRUE
    )
    
    cat(" ✅")
    
  }, error = function(e) {
    cat(sprintf(" ❌ Error: %s", e$message))
  })
}

cat("\n")

# ============================================================================
# VERIFICACIÓN FINAL
# ============================================================================

cat("\n╔════════════════════════════════════════════════════════╗\n")
cat("║  VERIFICACIÓN DE ARCHIVOS GENERADOS                   ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")

# Verificar PDFs
pdfs <- list.files(dir_recursos, pattern = "\\.pdf$", full.names = TRUE)
cat(sprintf("📄 PDFs generados: %d\n", length(pdfs)))
for (pdf in pdfs) {
  cat(sprintf("   ✓ %s\n", basename(pdf)))
}

# Verificar XMLs
xmls <- list.files(dir_recursos, pattern = "\\.xml$", full.names = TRUE)
cat(sprintf("\n🎓 Archivos Moodle XML: %d\n", length(xmls)))
for (xml in xmls) {
  cat(sprintf("   ✓ %s\n", basename(xml)))
}

# Verificar HTMLs
htmls <- list.files(dir_demos, pattern = "\\.html$", full.names = TRUE)
cat(sprintf("\n🌐 Demos HTML: %d\n", length(htmls)))
for (html in htmls) {
  cat(sprintf("   ✓ %s\n", basename(html)))
}

cat("\n✨ PROCESO COMPLETADO\n\n")
cat("📁 UBICACIONES:\n")
cat(sprintf("   Recursos: %s\n", normalizePath(dir_recursos)))
cat(sprintf("   Demos HTML: %s\n", normalizePath(dir_demos)))

cat("\n🎯 PRÓXIMOS PASOS:\n")
cat("1. Copiar index.html a docs/\n")
cat("2. Ejecutar copiar_a_docs.sh\n")
cat("3. Crear carrusel para LinkedIn\n")
cat("4. Publicar en LinkedIn\n\n")

