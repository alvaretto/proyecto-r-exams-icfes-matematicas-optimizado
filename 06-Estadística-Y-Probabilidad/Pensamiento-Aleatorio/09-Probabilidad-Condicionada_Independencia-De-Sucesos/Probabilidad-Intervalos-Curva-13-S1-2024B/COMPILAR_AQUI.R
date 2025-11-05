# ============================================================================
# SCRIPT DE COMPILACIÓN DIRECTA
# ============================================================================
# 
# PROPÓSITO:
# Compilar el archivo .Rmd corregido sin usar el botón "Knit" de RStudio
# Esto evita problemas de caché del editor
#
# INSTRUCCIONES:
# 1. Abrir este archivo en RStudio
# 2. Ejecutar todo el script (Ctrl+Shift+Enter o Cmd+Shift+Enter)
# 3. Los archivos generados estarán en la carpeta "compilacion_directa"
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("COMPILACIÓN DIRECTA - Evitando caché de RStudio\n")
cat("============================================================================\n\n")

# Cargar librería
library(exams)

# Archivo a compilar
archivo <- "probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd"

cat("📋 Archivo a compilar:\n")
cat("   ", archivo, "\n\n")

# ============================================================================
# COMPILACIÓN 1: HTML
# ============================================================================

cat("🔍 COMPILACIÓN 1: Formato HTML\n")
cat("   Compilando...\n\n")

tryCatch({
  exams2html(
    file = archivo,
    n = 1,
    name = "ejercicio_html",
    dir = "compilacion_directa",
    encoding = "UTF-8",
    quiet = FALSE
  )
  
  cat("\n   ✅ ÉXITO: Archivo HTML generado\n")
  cat("      Ubicación: compilacion_directa/ejercicio_html1.html\n\n")
  
}, error = function(e) {
  cat("\n   ❌ ERROR durante la compilación HTML:\n")
  cat("      ", conditionMessage(e), "\n\n")
})

# ============================================================================
# COMPILACIÓN 2: PDF
# ============================================================================

cat("🔍 COMPILACIÓN 2: Formato PDF\n")
cat("   Compilando...\n\n")

tryCatch({
  exams2pdf(
    file = archivo,
    n = 1,
    name = "ejercicio_pdf",
    dir = "compilacion_directa",
    encoding = "UTF-8",
    quiet = FALSE
  )
  
  cat("\n   ✅ ÉXITO: Archivo PDF generado\n")
  cat("      Ubicación: compilacion_directa/ejercicio_pdf1.pdf\n\n")
  
}, error = function(e) {
  cat("\n   ⚠️  ADVERTENCIA: Error durante compilación PDF:\n")
  cat("      ", conditionMessage(e), "\n")
  cat("      (Esto puede ser normal si no tienes LaTeX instalado)\n\n")
})

# ============================================================================
# COMPILACIÓN 3: DOCX (Pandoc)
# ============================================================================

cat("🔍 COMPILACIÓN 3: Formato DOCX\n")
cat("   Compilando...\n\n")

tryCatch({
  exams2pandoc(
    file = archivo,
    n = 1,
    name = "ejercicio_docx",
    dir = "compilacion_directa",
    encoding = "UTF-8",
    quiet = FALSE
  )
  
  cat("\n   ✅ ÉXITO: Archivo DOCX generado\n")
  cat("      Ubicación: compilacion_directa/ejercicio_docx1.docx\n\n")
  
}, error = function(e) {
  cat("\n   ⚠️  ADVERTENCIA: Error durante compilación DOCX:\n")
  cat("      ", conditionMessage(e), "\n\n")
})

# ============================================================================
# VERIFICACIÓN DE IMÁGENES
# ============================================================================

cat("🔍 VERIFICACIÓN: Imágenes PNG generadas\n\n")

imagenes_esperadas <- c(
  "tabla_opcion_a.png",
  "tabla_opcion_b.png",
  "tabla_opcion_c.png",
  "tabla_opcion_d.png"
)

imagenes_encontradas <- 0

for (img in imagenes_esperadas) {
  if (file.exists(img)) {
    cat("   ✅", img, "\n")
    imagenes_encontradas <- imagenes_encontradas + 1
  } else {
    cat("   ❌", img, "NO ENCONTRADA\n")
  }
}

cat("\n")

# ============================================================================
# RESUMEN FINAL
# ============================================================================

cat("============================================================================\n")
cat("RESUMEN DE COMPILACIÓN\n")
cat("============================================================================\n\n")

cat("Archivo compilado:", archivo, "\n")
cat("Imágenes generadas:", imagenes_encontradas, "/", length(imagenes_esperadas), "\n\n")

if (imagenes_encontradas == length(imagenes_esperadas)) {
  cat("✅ COMPILACIÓN EXITOSA\n")
  cat("   El archivo está funcionando correctamente.\n")
  cat("   Los archivos generados están en la carpeta 'compilacion_directa/'\n\n")
} else {
  cat("⚠️  ADVERTENCIA\n")
  cat("   Algunas imágenes no fueron generadas.\n")
  cat("   Revisar los mensajes de error anteriores.\n\n")
}

cat("============================================================================\n\n")

# Abrir carpeta de salida
cat("📂 Para ver los archivos generados:\n")
cat("   Navega a la carpeta 'compilacion_directa/' en el explorador de archivos\n\n")

# Opcional: Abrir el HTML generado en el navegador
if (file.exists("compilacion_directa/ejercicio_html1.html")) {
  cat("🌐 ¿Deseas abrir el HTML en el navegador? (s/n): ")
  respuesta <- readline()
  
  if (tolower(respuesta) == "s") {
    browseURL("compilacion_directa/ejercicio_html1.html")
    cat("✅ HTML abierto en el navegador\n\n")
  }
}

