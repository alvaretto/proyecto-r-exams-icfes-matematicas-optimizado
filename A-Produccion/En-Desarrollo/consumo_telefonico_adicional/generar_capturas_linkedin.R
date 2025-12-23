################################################################################
# SCRIPT: Generar Capturas de Pantalla para LinkedIn
# Propósito: Crear imágenes de alta calidad del ejercicio para publicación
# Autor: Proyecto Matemáticas ICFES R-Exams
# Fecha: Diciembre 2025
################################################################################

# Cargar librerías necesarias
library(exams)
library(knitr)
library(rmarkdown)

# Configuración
setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/En-Desarrollo/consumo_telefonico_adicional")

archivo_ejercicio <- "consumo_telefonico_adicional_n2_v1.Rmd"
dir_capturas <- "capturas_linkedin"

# Crear directorio para capturas
if (!dir.exists(dir_capturas)) {
  dir.create(dir_capturas, recursive = TRUE)
}

cat("================================================================================\n")
cat("GENERANDO CAPTURAS PARA LINKEDIN\n")
cat("================================================================================\n\n")

################################################################################
# 1. GENERAR VERSIONES HTML PARA CAPTURAS
################################################################################

cat("1. Generando versiones HTML individuales...\n")

# Generar 3 versiones HTML standalone para tomar capturas
exams2html(archivo_ejercicio,
           n = 3,
           name = "captura_version",
           dir = dir_capturas,
           edir = ".",
           encoding = "UTF-8",
           mathjax = TRUE,
           solution = TRUE,
           template = "plain8.html")  # Template limpio para capturas

cat("   ✓ 3 versiones HTML generadas en:", dir_capturas, "\n\n")

################################################################################
# 2. GENERAR VERSIONES PDF PARA CAPTURAS
################################################################################

cat("2. Generando versiones PDF individuales...\n")

exams2pdf(archivo_ejercicio,
          n = 3,
          name = "captura_pdf_version",
          dir = dir_capturas,
          edir = ".",
          encoding = "UTF-8",
          template = "plain.tex")

cat("   ✓ 3 versiones PDF generadas en:", dir_capturas, "\n\n")

################################################################################
# 3. INSTRUCCIONES PARA TOMAR CAPTURAS
################################################################################

cat("================================================================================\n")
cat("INSTRUCCIONES PARA TOMAR CAPTURAS DE PANTALLA\n")
cat("================================================================================\n\n")

cat("📸 CAPTURA 1: Ejercicio Completo\n")
cat("   Archivo: capturas_linkedin/captura_version1.html\n")
cat("   Acción:\n")
cat("   1. Abrir en navegador (Chrome/Firefox)\n")
cat("   2. Zoom: 100%\n")
cat("   3. Capturar: Gráfico + Tabla + Pregunta + Opciones\n")
cat("   4. Guardar como: ejercicio_completo.png\n")
cat("   5. Resolución mínima: 1920x1080\n\n")

cat("📸 CAPTURA 2: Comparación de Versiones\n")
cat("   Archivos: captura_version1.html y captura_version2.html\n")
cat("   Acción:\n")
cat("   1. Abrir ambas versiones en pestañas separadas\n")
cat("   2. Usar herramienta de captura side-by-side\n")
cat("   3. Resaltar diferencias con flechas/círculos\n")
cat("   4. Guardar como: comparacion_versiones.png\n\n")

cat("📸 CAPTURA 3: Gráfico Individual\n")
cat("   Archivo: grafico_consumo.png (generado automáticamente)\n")
cat("   Acción:\n")
cat("   1. Copiar desde directorio temporal\n")
cat("   2. Optimizar resolución si es necesario\n")
cat("   3. Guardar como: grafico_ejemplo.png\n\n")

cat("📸 CAPTURA 4: Código con Syntax Highlighting\n")
cat("   Herramienta recomendada: https://carbon.now.sh\n")
cat("   Código a capturar: Ver fragmentos en GUIA_PUBLICACION_LINKEDIN.md\n")
cat("   Configuración:\n")
cat("   - Tema: Dracula / Monokai / One Dark\n")
cat("   - Lenguaje: Python / R\n")
cat("   - Padding: Medium\n")
cat("   - Exportar como PNG (2x)\n\n")

################################################################################
# 4. GENERAR GIF ANIMADO (OPCIONAL)
################################################################################

cat("🎬 GIF ANIMADO (Opcional pero recomendado)\n")
cat("   Herramientas:\n")
cat("   - Linux: Peek (sudo pacman -S peek)\n")
cat("   - Windows: ScreenToGif\n")
cat("   - Mac: Kap\n\n")
cat("   Pasos:\n")
cat("   1. Abrir captura_version1.html en navegador\n")
cat("   2. Iniciar grabación de pantalla\n")
cat("   3. Acciones a grabar:\n")
cat("      - Scroll por el ejercicio (2 seg)\n")
cat("      - Seleccionar una respuesta (1 seg)\n")
cat("      - Click en 'Verificar' (1 seg)\n")
cat("      - Mostrar retroalimentación (2 seg)\n")
cat("   4. Detener grabación\n")
cat("   5. Exportar como GIF (max 5 MB)\n")
cat("   6. Guardar como: demo_interactivo.gif\n\n")

################################################################################
# 5. RESUMEN DE ARCHIVOS GENERADOS
################################################################################

cat("================================================================================\n")
cat("RESUMEN DE ARCHIVOS GENERADOS\n")
cat("================================================================================\n\n")

archivos_generados <- list.files(dir_capturas, full.names = FALSE)
cat("Directorio:", dir_capturas, "\n")
cat("Archivos:\n")
for (archivo in archivos_generados) {
  cat("  -", archivo, "\n")
}

cat("\n✅ Proceso completado exitosamente!\n\n")

cat("📋 PRÓXIMOS PASOS:\n")
cat("   1. Tomar capturas de pantalla según instrucciones\n")
cat("   2. Editar/anotar capturas si es necesario\n")
cat("   3. Optimizar tamaño de imágenes (< 1 MB cada una)\n")
cat("   4. Guardar en carpeta 'capturas_linkedin/finales/'\n")
cat("   5. Usar en carrusel PDF de LinkedIn\n\n")

cat("🔗 Ver guía completa en: GUIA_PUBLICACION_LINKEDIN.md\n\n")

