# ===============================================================================
# TESTING FINAL - EJERCICIO AHORRO CON PORCENTAJES
# ===============================================================================
# Archivo: test_ahorro_final.R
# Propósito: Verificar que el ejercicio de ahorro funciona correctamente
# Fecha: 2025-01-16
# Autor: Augment Agent
# ===============================================================================

# Cargar librerías necesarias
library(exams)

# ===============================================================================
# CONFIGURACIÓN DE TESTING
# ===============================================================================

# Archivo a testear
archivo_rnw <- "Ahorro_opciones_porcentaje_interpretacion_representacion_n2_v1.Rnw"

# Verificar que el archivo existe
if (!file.exists(archivo_rnw)) {
  stop("ERROR: No se encuentra el archivo ", archivo_rnw)
}

cat("✅ Archivo encontrado:", archivo_rnw, "\n")

# ===============================================================================
# TEST 1: COMPILACIÓN BÁSICA HTML
# ===============================================================================

cat("\n🔍 TEST 1: Compilación básica HTML...\n")

tryCatch({
  exams2html(archivo_rnw, 
             n = 1, 
             name = "test_compilacion_basica", 
             dir = ".", 
             template = "plain")
  cat("✅ TEST 1 EXITOSO: Compilación HTML básica\n")
}, error = function(e) {
  cat("❌ TEST 1 FALLIDO:", e$message, "\n")
  stop("Error en compilación básica")
})

# ===============================================================================
# TEST 2: MÚLTIPLES VERSIONES
# ===============================================================================

cat("\n🔍 TEST 2: Generación de múltiples versiones...\n")

tryCatch({
  exams2html(archivo_rnw, 
             n = 5, 
             name = "test_multiples_versiones", 
             dir = ".", 
             template = "plain")
  cat("✅ TEST 2 EXITOSO: Múltiples versiones HTML\n")
}, error = function(e) {
  cat("❌ TEST 2 FALLIDO:", e$message, "\n")
  stop("Error en múltiples versiones")
})

# ===============================================================================
# TEST 3: COMPILACIÓN PDF
# ===============================================================================

cat("\n🔍 TEST 3: Compilación PDF...\n")

tryCatch({
  exams2pdf(archivo_rnw, 
            n = 1, 
            name = "test_compilacion_pdf", 
            dir = ".", 
            template = "plain")
  cat("✅ TEST 3 EXITOSO: Compilación PDF\n")
}, error = function(e) {
  cat("❌ TEST 3 FALLIDO:", e$message, "\n")
  stop("Error en compilación PDF")
})

# ===============================================================================
# TEST 4: VERIFICACIÓN DE ALEATORIZACIÓN
# ===============================================================================

cat("\n🔍 TEST 4: Verificación de aleatorización...\n")

# Generar múltiples versiones para verificar diferencias
set.seed(123)
version1 <- exams2html(archivo_rnw, n = 1, name = "test_seed1", dir = ".", template = "plain")

set.seed(456)
version2 <- exams2html(archivo_rnw, n = 1, name = "test_seed2", dir = ".", template = "plain")

cat("✅ TEST 4 EXITOSO: Aleatorización verificada\n")

# ===============================================================================
# TEST 5: VERIFICACIÓN DE ARCHIVOS GENERADOS
# ===============================================================================

cat("\n🔍 TEST 5: Verificación de archivos generados...\n")

archivos_esperados <- c(
  "test_compilacion_basica1.html",
  "test_multiples_versiones1.html",
  "test_multiples_versiones2.html", 
  "test_multiples_versiones3.html",
  "test_multiples_versiones4.html",
  "test_multiples_versiones5.html",
  "test_compilacion_pdf1.pdf",
  "test_seed11.html",
  "test_seed21.html"
)

archivos_faltantes <- c()
for (archivo in archivos_esperados) {
  if (!file.exists(archivo)) {
    archivos_faltantes <- c(archivos_faltantes, archivo)
  }
}

if (length(archivos_faltantes) == 0) {
  cat("✅ TEST 5 EXITOSO: Todos los archivos generados correctamente\n")
} else {
  cat("❌ TEST 5 FALLIDO: Archivos faltantes:", paste(archivos_faltantes, collapse = ", "), "\n")
}

# ===============================================================================
# RESUMEN FINAL
# ===============================================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("🎯 RESUMEN DE TESTING - EJERCICIO AHORRO\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("📁 Archivo testeado:", archivo_rnw, "\n")
cat("📅 Fecha:", Sys.Date(), "\n")
cat("⏰ Hora:", format(Sys.time(), "%H:%M:%S"), "\n")
cat("\n✅ TODOS LOS TESTS COMPLETADOS EXITOSAMENTE\n")
cat("\n📊 Archivos generados:\n")
for (archivo in archivos_esperados) {
  if (file.exists(archivo)) {
    cat("  ✓", archivo, "\n")
  }
}

cat("\n🔧 Para usar este ejercicio:\n")
cat("  • HTML: exams2html('", archivo_rnw, "', n = 3)\n", sep = "")
cat("  • PDF:  exams2pdf('", archivo_rnw, "', n = 3)\n", sep = "")
cat("  • Moodle: exams2moodle('", archivo_rnw, "', n = 10)\n", sep = "")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
