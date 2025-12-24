################################################################################
# SCRIPT: Generar PDF con 10 Versiones para GitHub Pages
# Propósito: Crear un PDF con 10 versiones únicas del ejercicio
# Autor: Proyecto Matemáticas ICFES R-Exams
# Fecha: Diciembre 2024
################################################################################

# Cargar librerías necesarias
library(exams)

# Configuración
setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/En-Desarrollo/consumo_telefonico_adicional")

archivo_ejercicio <- "consumo_telefonico_adicional_n2_v1.Rmd"
dir_salida <- "../../../docs/recursos"

cat("================================================================================\n")
cat("GENERANDO PDF CON 10 VERSIONES PARA GITHUB PAGES\n")
cat("================================================================================\n\n")

# Crear directorio si no existe
if (!dir.exists(dir_salida)) {
  dir.create(dir_salida, recursive = TRUE)
  cat("✓ Directorio creado:", dir_salida, "\n\n")
}

cat("Generando PDF con 10 versiones...\n")

# Generar PDF con 10 versiones
exams2pdf(
  archivo_ejercicio,
  n = 10,
  name = "muestra_10_versiones_consumo_telefonico",
  dir = dir_salida,
  edir = ".",
  encoding = "UTF-8",
  template = "plain.tex",
  solution = TRUE
)

cat("\n✅ PDFs individuales generados!\n")
cat("📁 Ubicación:", dir_salida, "\n\n")

# Combinar todos los PDFs en uno solo
cat("Combinando PDFs en un solo archivo...\n")

archivos_pdf <- list.files(dir_salida,
                           pattern = "muestra_10_versiones_consumo_telefonico\\d+\\.pdf$",
                           full.names = TRUE)
archivos_pdf <- sort(archivos_pdf)

if (length(archivos_pdf) == 10) {
  # Usar pdfunite para combinar
  archivo_final <- file.path(dir_salida, "muestra_10_versiones_consumo_telefonico1.pdf")

  comando <- paste("pdfunite",
                   paste(archivos_pdf, collapse = " "),
                   archivo_final)

  system(comando)

  # Eliminar archivos individuales
  file.remove(archivos_pdf)

  cat("✅ PDF combinado exitosamente!\n")
  cat("📁 Ubicación:", archivo_final, "\n")

  # Verificar número de páginas
  info_cmd <- paste("pdfinfo", archivo_final, "| grep Pages")
  cat("\n")
  system(info_cmd)
  cat("\n")
} else {
  cat("⚠️ Advertencia: Se esperaban 10 PDFs pero se encontraron", length(archivos_pdf), "\n")
}

cat("================================================================================\n")
cat("PRÓXIMOS PASOS\n")
cat("================================================================================\n\n")

cat("1. Verificar el PDF generado:\n")
cat("   - Abrir el archivo en un visor PDF\n")
cat("   - Confirmar que contiene 10 versiones diferentes\n\n")

cat("2. Actualizar en GitHub:\n")
cat("   cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams\n")
cat("   git add docs/recursos/muestra_10_versiones_consumo_telefonico1.pdf\n")
cat("   git commit -m \"Actualizar PDF con 10 versiones completas\"\n")
cat("   git push origin gh-pages\n\n")

cat("3. Verificar en GitHub Pages (esperar 1-2 minutos):\n")
cat("   https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/recursos/muestra_10_versiones_consumo_telefonico1.pdf\n\n")

cat("================================================================================\n")

