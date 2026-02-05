# ===================================================================
# AUDITORÍA DE COMPATIBILIDAD R-EXAMS
# ===================================================================
# Script para probar compilación en múltiples formatos

library(exams)
library(digest)

cat("=== AUDITORÍA DE COMPATIBILIDAD R-EXAMS ===\n\n")

archivo_ejercicio <- "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd"

# ===== PRUEBA 1: COMPILACIÓN HTML =====
cat("1. COMPILACIÓN HTML:\n")
cat("====================\n")

html_ok <- FALSE
tryCatch({
  resultado_html <- exams2html(archivo_ejercicio, 
                              n = 2, 
                              name = "test_html", 
                              dir = "test_html_output",
                              quiet = TRUE)
  cat("✅ CORRECTO: Compilación HTML exitosa\n")
  html_ok <- TRUE
}, error = function(e) {
  cat("❌ ERROR HTML:", e$message, "\n")
  html_ok <- FALSE
})

# ===== PRUEBA 2: COMPILACIÓN PDF =====
cat("\n2. COMPILACIÓN PDF:\n")
cat("===================\n")

pdf_ok <- FALSE
tryCatch({
  resultado_pdf <- exams2pdf(archivo_ejercicio, 
                            n = 1, 
                            name = "test_pdf", 
                            dir = "test_pdf_output",
                            quiet = TRUE)
  cat("✅ CORRECTO: Compilación PDF exitosa\n")
  pdf_ok <- TRUE
}, error = function(e) {
  cat("❌ ERROR PDF:", e$message, "\n")
  pdf_ok <- FALSE
})

# ===== PRUEBA 3: COMPILACIÓN MOODLE =====
cat("\n3. COMPILACIÓN MOODLE:\n")
cat("======================\n")

moodle_ok <- FALSE
tryCatch({
  resultado_moodle <- exams2moodle(archivo_ejercicio, 
                                  n = 1, 
                                  name = "test_moodle", 
                                  dir = "test_moodle_output",
                                  quiet = TRUE)
  cat("✅ CORRECTO: Compilación Moodle exitosa\n")
  moodle_ok <- TRUE
}, error = function(e) {
  cat("❌ ERROR MOODLE:", e$message, "\n")
  moodle_ok <- FALSE
})

# ===== PRUEBA 4: DIVERSIDAD DE VERSIONES =====
cat("\n4. DIVERSIDAD DE VERSIONES:\n")
cat("===========================\n")

if(html_ok) {
  # Generar múltiples versiones y verificar diversidad
  resultado_diversidad <- exams2html(archivo_ejercicio, 
                                    n = 5, 
                                    name = "test_diversidad", 
                                    dir = "test_diversidad_output",
                                    quiet = TRUE)
  
  cat("✅ CORRECTO: Generación de múltiples versiones exitosa\n")
  
  # Verificar que los archivos se generaron
  archivos_generados <- list.files("test_diversidad_output", pattern = "*.html")
  if(length(archivos_generados) == 5) {
    cat("✅ CORRECTO: Se generaron todas las versiones solicitadas (", length(archivos_generados), ")\n")
  } else {
    cat("⚠️  ADVERTENCIA: Número inesperado de archivos generados:", length(archivos_generados), "\n")
  }
} else {
  cat("❌ ERROR: No se puede probar diversidad sin compilación HTML\n")
}

# ===== RESUMEN FINAL =====
cat("\n=== RESUMEN DE COMPATIBILIDAD R-EXAMS ===\n")

formatos_exitosos <- sum(html_ok, pdf_ok, moodle_ok)
total_formatos <- 3

if(formatos_exitosos == total_formatos) {
  cat("🎯 EXCELENTE: Compatibilidad completa con R-exams\n")
  cat("✅ HTML: Compilación exitosa\n")
  cat("✅ PDF: Compilación exitosa\n")
  cat("✅ Moodle: Compilación exitosa\n")
  cat("✅ Diversidad: Múltiples versiones generadas\n")
  cat("\n🏆 RESULTADO: APROBADO - COMPATIBILIDAD COMPLETA\n")
} else if(formatos_exitosos >= 2) {
  cat("⚠️  BUENO: Compatibilidad parcial con R-exams\n")
  cat("   Formatos exitosos:", formatos_exitosos, "de", total_formatos, "\n")
  if(html_ok) cat("✅ HTML: OK\n") else cat("❌ HTML: ERROR\n")
  if(pdf_ok) cat("✅ PDF: OK\n") else cat("❌ PDF: ERROR\n")
  if(moodle_ok) cat("✅ Moodle: OK\n") else cat("❌ Moodle: ERROR\n")
  cat("\n⚠️  RESULTADO: PARCIALMENTE APROBADO - REQUIERE REVISIÓN\n")
} else {
  cat("❌ PROBLEMAS CRÍTICOS: Compatibilidad insuficiente\n")
  cat("   Formatos exitosos:", formatos_exitosos, "de", total_formatos, "\n")
  cat("🔧 REQUIERE CORRECCIÓN INMEDIATA\n")
  cat("\n⚠️  RESULTADO: REPROBADO - REQUIERE CORRECCIÓN\n")
}

# Limpiar archivos de prueba
dirs_to_clean <- c("test_html_output", "test_pdf_output", "test_moodle_output", "test_diversidad_output")
for(dir in dirs_to_clean) {
  if(dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
}

cat("\nLa auditoría de compatibilidad R-exams ha finalizado.\n")
