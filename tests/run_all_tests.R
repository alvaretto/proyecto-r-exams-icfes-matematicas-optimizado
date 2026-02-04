#!/usr/bin/env Rscript
# Script principal para ejecutar toda la suite de tests
# Uso: Rscript tests/run_all_tests.R

library(testthat)
library(exams)

cat("\n")
cat("========================================\n")
cat("  SUITE DE TESTING COMPLETA\n")
cat("  Repositorio Matemáticas ICFES R-Exams\n")
cat("========================================\n\n")

# Configurar entorno de testing
Sys.setenv(TESTING = "TRUE")

# Función para ejecutar tests y reportar resultados
ejecutar_suite <- function(nombre, archivo) {
  cat(sprintf("Ejecutando: %s\n", nombre))
  cat(strrep("-", 50), "\n")

  inicio <- Sys.time()

  resultado <- tryCatch({
    test_file(archivo, reporter = "summary")
    list(exito = TRUE, tiempo = difftime(Sys.time(), inicio, units = "secs"))
  }, error = function(e) {
    list(exito = FALSE, error = e$message, tiempo = difftime(Sys.time(), inicio, units = "secs"))
  })

  if (resultado$exito) {
    cat(sprintf("✓ %s completado en %.2f segundos\n\n", nombre, resultado$tiempo))
  } else {
    cat(sprintf("✗ %s FALLÓ: %s\n\n", nombre, resultado$error))
  }

  return(resultado)
}

# Definir suites de tests
suites <- list(
  list(
    nombre = "Validación Matemática",
    archivo = "tests/testthat/test_validacion_matematica.R",
    critico = TRUE
  ),
  list(
    nombre = "Ortografía Española",
    archivo = "tests/testthat/test_ortografia_espanol.R",
    critico = TRUE
  ),
  list(
    nombre = "Renderizado 4 Formatos",
    archivo = "tests/testthat/test_renderizado_4_formatos.R",
    critico = TRUE
  ),
  list(
    nombre = "Aleatorización y Diversidad",
    archivo = "tests/testthat/test_aleatorization_diversity.R",
    critico = TRUE
  ),
  list(
    nombre = "Flujo B (Graficador)",
    archivo = "tests/testthat/test_flujo_b_graficador.R",
    critico = TRUE
  ),
  list(
    nombre = "Tests de Regresión",
    archivo = "tests/testthat/test_regression_suite.R",
    critico = TRUE
  )
)

# Ejecutar todas las suites
resultados <- list()
exitos <- 0
fallos <- 0
tiempo_total <- 0

for (suite in suites) {
  if (file.exists(suite$archivo)) {
    resultado <- ejecutar_suite(suite$nombre, suite$archivo)
    resultados[[suite$nombre]] <- resultado

    if (resultado$exito) {
      exitos <- exitos + 1
    } else {
      fallos <- fallos + 1
    }

    tiempo_total <- tiempo_total + as.numeric(resultado$tiempo)
  } else {
    cat(sprintf("⚠ Archivo no encontrado: %s\n\n", suite$archivo))
    fallos <- fallos + 1
  }
}

# Reporte final
cat("\n")
cat("========================================\n")
cat("  REPORTE FINAL\n")
cat("========================================\n\n")

cat(sprintf("Suites ejecutadas: %d\n", exitos + fallos))
cat(sprintf("✓ Exitosas: %d\n", exitos))
cat(sprintf("✗ Fallidas: %d\n", fallos))
cat(sprintf("Tiempo total: %.2f segundos\n\n", tiempo_total))

# Calcular cobertura estimada
cobertura_estimada <- (exitos / (exitos + fallos)) * 100

cat(sprintf("Cobertura de testing: %.1f%%\n", cobertura_estimada))

if (cobertura_estimada >= 100) {
  cat("🎉 ¡OBJETIVO DE 100%% ALCANZADO!\n")
} else if (cobertura_estimada >= 90) {
  cat("⚠ Cobertura alta pero por debajo del objetivo (100%%)\n")
} else if (cobertura_estimada >= 70) {
  cat("⚠ Cobertura moderada - Se requiere mejora\n")
} else {
  cat("❌ Cobertura baja - Acción requerida urgente\n")
}

cat("\n")

# Salir con código apropiado
if (fallos > 0) {
  cat("❌ ALGUNOS TESTS FALLARON\n\n")
  quit(status = 1)
} else {
  cat("✅ TODOS LOS TESTS PASARON\n\n")
  quit(status = 0)
}
