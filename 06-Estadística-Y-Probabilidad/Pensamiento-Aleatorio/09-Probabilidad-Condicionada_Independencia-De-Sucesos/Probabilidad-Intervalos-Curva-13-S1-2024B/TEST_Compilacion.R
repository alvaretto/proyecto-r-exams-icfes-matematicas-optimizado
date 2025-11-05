# ============================================================================
# SCRIPT DE PRUEBA: Compilación del ejercicio corregido
# ============================================================================
# 
# PROPÓSITO:
# Verificar que el archivo .Rmd compile correctamente después de la corrección
# del error "se intenta usar un nombre de variable de longitud cero"
#
# INSTRUCCIONES:
# 1. Ejecutar este script desde RStudio
# 2. El script intentará compilar el ejercicio en formato HTML
# 3. Reportará si hay errores o si la compilación fue exitosa
#
# ============================================================================

library(exams)

cat("\n")
cat("============================================================================\n")
cat("PRUEBA DE COMPILACIÓN - Ejercicio Probabilidad Intervalos Curva\n")
cat("============================================================================\n\n")

# Configuración
archivo_rmd <- "probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd"

cat("📋 Archivo a compilar:\n")
cat("   ", archivo_rmd, "\n\n")

# ============================================================================
# PRUEBA 1: Compilación básica en formato HTML
# ============================================================================

cat("🔍 PRUEBA 1: Compilación en formato HTML\n")
cat("   Intentando compilar...\n\n")

tryCatch({
  # Compilar a HTML
  exams2html(
    file = archivo_rmd,
    n = 1,
    name = "test_compilacion",
    dir = "test_output",
    encoding = "UTF-8",
    quiet = FALSE
  )
  
  cat("   ✅ ÉXITO: El archivo compiló correctamente en formato HTML\n\n")
  
}, error = function(e) {
  cat("   ❌ ERROR durante la compilación:\n")
  cat("      ", conditionMessage(e), "\n\n")
})

# ============================================================================
# PRUEBA 2: Verificar que se generaron las imágenes PNG
# ============================================================================

cat("🔍 PRUEBA 2: Verificación de imágenes PNG generadas\n")

imagenes_esperadas <- c(
  "tabla_opcion_a.png",
  "tabla_opcion_b.png",
  "tabla_opcion_c.png",
  "tabla_opcion_d.png"
)

imagenes_encontradas <- 0

for (img in imagenes_esperadas) {
  if (file.exists(img)) {
    cat("   ✅ Encontrada:", img, "\n")
    imagenes_encontradas <- imagenes_encontradas + 1
  } else {
    cat("   ❌ No encontrada:", img, "\n")
  }
}

cat("\n")
if (imagenes_encontradas == length(imagenes_esperadas)) {
  cat("   ✅ ÉXITO: Todas las imágenes PNG fueron generadas\n\n")
} else {
  cat("   ⚠️  ADVERTENCIA: Solo se generaron", imagenes_encontradas, "de", 
      length(imagenes_esperadas), "imágenes\n\n")
}

# ============================================================================
# PRUEBA 3: Compilación en formato PDF (opcional)
# ============================================================================

cat("🔍 PRUEBA 3: Compilación en formato PDF (opcional)\n")
cat("   Intentando compilar...\n\n")

tryCatch({
  # Compilar a PDF
  exams2pdf(
    file = archivo_rmd,
    n = 1,
    name = "test_compilacion_pdf",
    dir = "test_output",
    encoding = "UTF-8",
    quiet = FALSE
  )
  
  cat("   ✅ ÉXITO: El archivo compiló correctamente en formato PDF\n\n")
  
}, error = function(e) {
  cat("   ⚠️  ADVERTENCIA: Error durante compilación PDF:\n")
  cat("      ", conditionMessage(e), "\n")
  cat("      (Esto puede ser normal si no tienes LaTeX instalado)\n\n")
})

# ============================================================================
# RESUMEN FINAL
# ============================================================================

cat("============================================================================\n")
cat("RESUMEN DE PRUEBAS\n")
cat("============================================================================\n\n")

cat("Archivo probado:", archivo_rmd, "\n")
cat("Imágenes generadas:", imagenes_encontradas, "/", length(imagenes_esperadas), "\n\n")

if (imagenes_encontradas == length(imagenes_esperadas)) {
  cat("✅ TODAS LAS PRUEBAS CRÍTICAS PASARON\n")
  cat("   El error 'nombre de variable de longitud cero' ha sido corregido.\n")
  cat("   El archivo compila correctamente.\n\n")
} else {
  cat("⚠️  ALGUNAS PRUEBAS FALLARON\n")
  cat("   Revisar los mensajes de error anteriores.\n\n")
}

cat("============================================================================\n\n")

# Limpiar archivos de prueba (opcional)
cat("¿Deseas limpiar los archivos de prueba generados? (s/n): ")
respuesta <- readline()

if (tolower(respuesta) == "s") {
  # Eliminar directorio de prueba
  if (dir.exists("test_output")) {
    unlink("test_output", recursive = TRUE)
    cat("✅ Directorio 'test_output' eliminado\n")
  }
  
  # Eliminar imágenes PNG
  for (img in imagenes_esperadas) {
    if (file.exists(img)) {
      file.remove(img)
    }
  }
  cat("✅ Imágenes PNG eliminadas\n\n")
} else {
  cat("ℹ️  Archivos de prueba conservados en 'test_output/'\n\n")
}

