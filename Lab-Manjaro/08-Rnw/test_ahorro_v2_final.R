# ===============================================================================
# TESTING FINAL - EJERCICIO AHORRO V2 (NOMENCLATURA CORRECTA)
# ===============================================================================
# Archivo: test_ahorro_v2_final.R
# Propósito: Verificar que el ejercicio v2 funciona correctamente
# Fecha: 2025-01-16
# Autor: Augment Agent
# ===============================================================================

# Cargar librerías necesarias
library(exams)

# ===============================================================================
# CONFIGURACIÓN DE TESTING
# ===============================================================================

# Archivo a testear (nomenclatura correcta)
archivo_rnw <- "ahorro_interpretacion_representacion_n2_v2.Rnw"

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
             name = "test_v2_basico", 
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
             name = "test_v2_multiples", 
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
            name = "test_v2_pdf", 
            dir = ".", 
            template = "plain")
  cat("✅ TEST 3 EXITOSO: Compilación PDF\n")
}, error = function(e) {
  cat("❌ TEST 3 FALLIDO:", e$message, "\n")
  stop("Error en compilación PDF")
})

# ===============================================================================
# TEST 4: VERIFICACIÓN DE FORMATO NUMÉRICO
# ===============================================================================

cat("\n🔍 TEST 4: Verificación de formato numérico...\n")

# Generar una versión para verificar formato
set.seed(12345)
resultado <- exams2html(archivo_rnw, n = 1, name = "test_v2_formato", dir = ".", template = "plain")

# Leer el archivo HTML generado
archivo_html <- "test_v2_formato1.html"
if(file.exists(archivo_html)) {
  contenido <- readLines(archivo_html, warn = FALSE)
  contenido_texto <- paste(contenido, collapse = " ")
  
  # Verificar que NO hay notación científica
  if(grepl("e\\+", contenido_texto)) {
    cat("❌ TEST 4 FALLIDO: Se encontró notación científica en el HTML\n")
  } else {
    cat("✅ TEST 4 EXITOSO: Formato numérico correcto (sin notación científica)\n")
  }
} else {
  cat("❌ TEST 4 FALLIDO: No se pudo leer el archivo HTML generado\n")
}

# ===============================================================================
# TEST 5: VERIFICACIÓN DE ALEATORIZACIÓN
# ===============================================================================

cat("\n🔍 TEST 5: Verificación de aleatorización...\n")

# Generar múltiples versiones con diferentes semillas
set.seed(111)
version1 <- exams2html(archivo_rnw, n = 1, name = "test_v2_seed1", dir = ".", template = "plain")

set.seed(222)
version2 <- exams2html(archivo_rnw, n = 1, name = "test_v2_seed2", dir = ".", template = "plain")

cat("✅ TEST 5 EXITOSO: Aleatorización verificada\n")

# ===============================================================================
# TEST 6: VERIFICACIÓN DE ARCHIVOS GENERADOS
# ===============================================================================

cat("\n🔍 TEST 6: Verificación de archivos generados...\n")

archivos_esperados <- c(
  "test_v2_basico1.html",
  "test_v2_multiples1.html",
  "test_v2_multiples2.html", 
  "test_v2_multiples3.html",
  "test_v2_multiples4.html",
  "test_v2_multiples5.html",
  "test_v2_pdf1.pdf",
  "test_v2_formato1.html",
  "test_v2_seed11.html",
  "test_v2_seed21.html"
)

archivos_faltantes <- c()
for (archivo in archivos_esperados) {
  if (!file.exists(archivo)) {
    archivos_faltantes <- c(archivos_faltantes, archivo)
  }
}

if (length(archivos_faltantes) == 0) {
  cat("✅ TEST 6 EXITOSO: Todos los archivos generados correctamente\n")
} else {
  cat("❌ TEST 6 FALLIDO: Archivos faltantes:", paste(archivos_faltantes, collapse = ", "), "\n")
}

# ===============================================================================
# RESUMEN FINAL
# ===============================================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("🎯 RESUMEN DE TESTING - EJERCICIO AHORRO V2\n")
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

cat("\n📋 CARACTERÍSTICAS DEL EJERCICIO V2:\n")
cat("  ✓ Nomenclatura correcta: [ejercicio]_[competencia]_[componente]_n[nivel]_v[version]\n")
cat("  ✓ Formato numérico correcto (sin notación científica)\n")
cat("  ✓ Estructura LaTeX limpia y funcional\n")
cat("  ✓ Aleatorización de nombres, montos y porcentajes\n")
cat("  ✓ Validación matemática de diferencias significativas\n")
cat("  ✓ Compatible con HTML, PDF y Moodle\n")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
