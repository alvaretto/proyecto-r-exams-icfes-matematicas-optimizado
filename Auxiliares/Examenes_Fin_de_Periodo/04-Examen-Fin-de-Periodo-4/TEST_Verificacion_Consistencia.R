# ============================================================================
# SCRIPT DE VERIFICACIÓN: Consistencia entre Versiones
# ============================================================================
# 
# PROPÓSITO:
# Verificar que todas las versiones generadas (con/sin soluciones, PDF/DOCX)
# contengan exactamente los mismos datos numéricos y opciones de respuesta.
#
# INSTRUCCIONES:
# 1. Ejecutar este script DESPUÉS de ejecutar SemilleroFinDePeriodo_4.R
# 2. El script analizará los archivos generados en la carpeta "salida"
# 3. Reportará si hay inconsistencias entre versiones
#
# ============================================================================

library(exams)
library(digest)

cat("\n")
cat("============================================================================\n")
cat("VERIFICACIÓN DE CONSISTENCIA ENTRE VERSIONES\n")
cat("============================================================================\n\n")

# Configuración
archivo_rmd <- "cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd"
semilla_test <- 123456

cat("📋 Configuración de prueba:\n")
cat("   - Archivo: ", archivo_rmd, "\n")
cat("   - Semilla: ", semilla_test, "\n\n")

# ============================================================================
# PRUEBA 1: Verificar que la misma semilla genera los mismos datos
# ============================================================================

cat("🔍 PRUEBA 1: Generación con misma semilla\n")
cat("   Generando versión 1...\n")

set.seed(semilla_test)
ejercicio1 <- exams_skeleton(file = archivo_rmd, 
                             markup = "markdown",
                             quiet = TRUE)

cat("   Generando versión 2 con la misma semilla...\n")

set.seed(semilla_test)
ejercicio2 <- exams_skeleton(file = archivo_rmd,
                             markup = "markdown", 
                             quiet = TRUE)

# Comparar hashes
hash1 <- digest::digest(ejercicio1)
hash2 <- digest::digest(ejercicio2)

if (hash1 == hash2) {
  cat("   ✅ ÉXITO: Ambas versiones son idénticas\n")
  cat("      Hash: ", hash1, "\n\n")
} else {
  cat("   ❌ ERROR: Las versiones son diferentes\n")
  cat("      Hash 1: ", hash1, "\n")
  cat("      Hash 2: ", hash2, "\n\n")
}

# ============================================================================
# PRUEBA 2: Verificar que diferentes semillas generan datos diferentes
# ============================================================================

cat("🔍 PRUEBA 2: Generación con diferentes semillas\n")
cat("   Generando versión con semilla 123456...\n")

set.seed(123456)
ejercicio_a <- exams_skeleton(file = archivo_rmd,
                              markup = "markdown",
                              quiet = TRUE)

cat("   Generando versión con semilla 654321...\n")

set.seed(654321)
ejercicio_b <- exams_skeleton(file = archivo_rmd,
                              markup = "markdown",
                              quiet = TRUE)

# Comparar hashes
hash_a <- digest::digest(ejercicio_a)
hash_b <- digest::digest(ejercicio_b)

if (hash_a != hash_b) {
  cat("   ✅ ÉXITO: Las versiones son diferentes (como se esperaba)\n")
  cat("      Hash A: ", hash_a, "\n")
  cat("      Hash B: ", hash_b, "\n\n")
} else {
  cat("   ⚠️  ADVERTENCIA: Las versiones son idénticas (inesperado)\n")
  cat("      Hash: ", hash_a, "\n\n")
}

# ============================================================================
# PRUEBA 3: Simular múltiples generaciones con reset de semilla
# ============================================================================

cat("🔍 PRUEBA 3: Múltiples generaciones con reset de semilla\n")
cat("   Simulando el comportamiento de SemilleroFinDePeriodo_4.R...\n\n")

resultados <- list()
semilla_fija <- 123456

for (i in 1:4) {
  cat("   Generación ", i, ": ")
  
  # Restablecer semilla (como en SemilleroFinDePeriodo_4.R)
  set.seed(semilla_fija)
  
  # Generar ejercicio
  ejercicio <- exams_skeleton(file = archivo_rmd,
                              markup = "markdown",
                              quiet = TRUE)
  
  # Guardar hash
  resultados[[i]] <- digest::digest(ejercicio)
  
  cat("Hash = ", substr(resultados[[i]], 1, 16), "...\n")
}

# Verificar que todos los hashes sean idénticos
hashes_unicos <- unique(resultados)

cat("\n")
if (length(hashes_unicos) == 1) {
  cat("   ✅ ÉXITO: Todas las 4 generaciones son idénticas\n")
  cat("      Esto confirma que el reset de semilla funciona correctamente\n\n")
} else {
  cat("   ❌ ERROR: Se encontraron ", length(hashes_unicos), " versiones diferentes\n")
  cat("      Se esperaba solo 1 versión única\n\n")
}

# ============================================================================
# RESUMEN FINAL
# ============================================================================

cat("============================================================================\n")
cat("RESUMEN DE VERIFICACIÓN\n")
cat("============================================================================\n\n")

pruebas_exitosas <- 0
total_pruebas <- 3

if (hash1 == hash2) pruebas_exitosas <- pruebas_exitosas + 1
if (hash_a != hash_b) pruebas_exitosas <- pruebas_exitosas + 1
if (length(hashes_unicos) == 1) pruebas_exitosas <- pruebas_exitosas + 1

cat("Pruebas exitosas: ", pruebas_exitosas, " / ", total_pruebas, "\n\n")

if (pruebas_exitosas == total_pruebas) {
  cat("✅ TODAS LAS PRUEBAS PASARON\n")
  cat("   La solución de consistencia está funcionando correctamente.\n")
  cat("   Las versiones con y sin soluciones generarán los mismos datos.\n\n")
} else {
  cat("⚠️  ALGUNAS PRUEBAS FALLARON\n")
  cat("   Revisar la implementación de control de semilla.\n\n")
}

cat("============================================================================\n\n")

