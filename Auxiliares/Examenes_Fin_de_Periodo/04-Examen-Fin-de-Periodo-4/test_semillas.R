# Script de prueba para verificar que diferentes semillas generen diferentes resultados
# Ejecutar este script ANTES de generar los PDFs para confirmar que el sistema funciona

cat("=== PRUEBA DE FUNCIONAMIENTO DE SEMILLAS ===\n\n")

# Definir las preguntas
preg01 <- "cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd"
preg03 <- "ExportacionesGraficosEstadisticaInterpretacion_n3_v1.Rmd"
preg04 <- "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd"

# Función para generar orden de preguntas con una semilla específica
generar_orden <- function(semilla_test) {
  set.seed(semilla_test)
  orden <- sample(c(
    rep(preg01, 5),
    rep(preg03, 7),
    rep(preg04, 8)
  ))
  return(orden)
}

# Probar con 3 semillas diferentes
cat("Probando con semilla 1001:\n")
orden_1001 <- generar_orden(1001)
cat("Primeras 5 preguntas:", paste(basename(orden_1001[1:5]), collapse = ", "), "\n\n")

cat("Probando con semilla 1002:\n")
orden_1002 <- generar_orden(1002)
cat("Primeras 5 preguntas:", paste(basename(orden_1002[1:5]), collapse = ", "), "\n\n")

cat("Probando con semilla 1003:\n")
orden_1003 <- generar_orden(1003)
cat("Primeras 5 preguntas:", paste(basename(orden_1003[1:5]), collapse = ", "), "\n\n")

# Verificar que son diferentes
if (identical(orden_1001, orden_1002)) {
  cat("❌ ERROR: Semillas 1001 y 1002 generan el MISMO orden\n")
} else {
  cat("✓ OK: Semillas 1001 y 1002 generan órdenes DIFERENTES\n")
}

if (identical(orden_1002, orden_1003)) {
  cat("❌ ERROR: Semillas 1002 y 1003 generan el MISMO orden\n")
} else {
  cat("✓ OK: Semillas 1002 y 1003 generan órdenes DIFERENTES\n")
}

if (identical(orden_1001, orden_1003)) {
  cat("❌ ERROR: Semillas 1001 y 1003 generan el MISMO orden\n")
} else {
  cat("✓ OK: Semillas 1001 y 1003 generan órdenes DIFERENTES\n")
}

cat("\n=== PRUEBA DE NÚMEROS ALEATORIOS ===\n\n")

# Probar que diferentes semillas generan diferentes números aleatorios
generar_numeros <- function(semilla_test) {
  set.seed(semilla_test)
  numeros <- sample(1:1000000, 5)
  return(numeros)
}

nums_1001 <- generar_numeros(1001)
nums_1002 <- generar_numeros(1002)
nums_1003 <- generar_numeros(1003)

cat("Números con semilla 1001:", paste(nums_1001, collapse = ", "), "\n")
cat("Números con semilla 1002:", paste(nums_1002, collapse = ", "), "\n")
cat("Números con semilla 1003:", paste(nums_1003, collapse = ", "), "\n\n")

if (identical(nums_1001, nums_1002)) {
  cat("❌ ERROR: Semillas 1001 y 1002 generan los MISMOS números\n")
} else {
  cat("✓ OK: Semillas 1001 y 1002 generan números DIFERENTES\n")
}

cat("\n=== CONCLUSIÓN ===\n")
cat("Si todos los tests muestran ✓ OK, el sistema de semillas funciona correctamente.\n")
cat("Si hay errores ❌, hay un problema con el generador de números aleatorios de R.\n")

