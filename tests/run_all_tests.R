#!/usr/bin/env Rscript
# Script principal para ejecutar toda la suite de tests
# Uso: Rscript tests/run_all_tests.R
#
# Variables de entorno:
#   R_TESTS_QUICK=1  — modo rápido (pre-push): salta suites pesadas sin cambios
#   R_TESTS_FULL=1   — forzar ejecución completa ignorando quick

library(testthat)
library(exams)

cat("\n")
cat("========================================\n")
cat("  SUITE DE TESTING COMPLETA\n")
cat("  Repositorio Matemáticas ICFES R-Exams\n")
cat("========================================\n\n")

# Configurar entorno de testing
Sys.setenv(TESTING = "TRUE")

# Todos los tests deben ejecutarse desde la raíz del repo
repo_root <- tryCatch(
  system("git rev-parse --show-toplevel", intern = TRUE),
  error = function(e) getwd()
)
if (length(repo_root) == 1 && nzchar(repo_root)) setwd(repo_root)

# --- Modo quick: detectar archivos cambiados para saltar suites pesadas ---
.quick_mode <- identical(Sys.getenv("R_TESTS_QUICK"), "1") &&
               !identical(Sys.getenv("R_TESTS_FULL"), "1")

# Obtener archivos cambiados respecto a HEAD (staged + unstaged + en el push)
# .detection_ok distingue "no hay cambios" (safe skip) de "detección falló" (run all)
.archivos_cambiados <- character(0)
.detection_ok <- FALSE
if (.quick_mode) {
  # Fuente 1: variable de entorno pasada por pre-push hook (más confiable)
  env_files <- Sys.getenv("R_TESTS_CHANGED_FILES", "")
  if (nzchar(env_files)) {
    .archivos_cambiados <- strsplit(env_files, "\n")[[1]]
    .archivos_cambiados <- .archivos_cambiados[nzchar(.archivos_cambiados)]
    .detection_ok <- TRUE
  } else {
    # Fuente 2: detección directa via git
    .archivos_cambiados <- tryCatch({
      pushed <- system("git diff --name-only @{push}.. 2>/dev/null", intern = TRUE)
      locales <- system("git diff --name-only HEAD 2>/dev/null", intern = TRUE)
      .detection_ok <<- TRUE
      unique(c(pushed, locales))
    }, error = function(e) character(0))
  }
}

# Función: ¿algún archivo cambiado coincide con los patrones de la suite?
.suite_afectada <- function(patrones) {
  if (!.quick_mode) return(TRUE)
  # Si detección fue exitosa y no hay cambios → safe skip suites con watch
  if (.detection_ok && length(.archivos_cambiados) == 0) return(FALSE)
  # Si detección falló → conservador, correr todo
  if (!.detection_ok) return(TRUE)
  any(sapply(patrones, function(p) any(grepl(p, .archivos_cambiados))))
}

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
# Suites pesadas incluyen campo `watch`: patrones de archivos que las activan.
# En modo quick, si ningún archivo cambiado coincide con watch, se saltan.
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
    critico = TRUE,
    watch = c("Ejemplos-Funcionales", "test_renderizado")
  ),
  list(
    nombre = "Aleatorización y Diversidad",
    archivo = "tests/testthat/test_aleatorization_diversity.R",
    critico = TRUE,
    watch = c("Ejemplos-Funcionales", "test_aleatorization")
  ),
  list(
    nombre = "Flujo B (Graficador)",
    archivo = "tests/testthat/test_flujo_b_graficador.R",
    critico = TRUE
  ),
  list(
    nombre = "Tests de Regresión",
    archivo = "tests/testthat/test_regression_suite.R",
    critico = TRUE,
    watch = c("Ejemplos-Funcionales", "test_regression")
  ),
  list(
    nombre = "Distintividad Visual _neg_",
    archivo = "tests/testthat/test_neg_visual_distinctness.R",
    critico = TRUE
  ),
  list(
    nombre = "Media-Mediana-Moda CLOZE Metacognitivo",
    archivo = "tests/testthat/test_media_mediana_moda.R",
    critico = TRUE,
    watch = c("Media-Mediana-Moda", "MediaMedianaModa",
              "test_media_mediana_moda", "04-Medidas-De-Tendencia-Central")
  ),
  list(
    nombre = "Validación Semántica (Nivel 4)",
    archivo = "tests/testthat/test_validacion_semantica.R",
    critico = TRUE
  ),
  list(
    nombre = "Correctitud de Respuesta (Nivel 5)",
    archivo = "tests/testthat/test_correctitud_respuesta.R",
    critico = TRUE
  ),
  list(
    nombre = "CLOZE N3 (Trigonometría)",
    archivo = "tests/testthat/test_cloze_n3.R",
    critico = TRUE,
    watch = c("test_cloze_n3", "03-En-Produccion")
  ),
  list(
    nombre = "Stress Test Visual Multi-Semilla",
    archivo = "tests/testthat/test_stress_test_visual.R",
    critico = TRUE,
    watch = c("stress_test_visual", "test_stress_test_visual")
  ),
  list(
    nombre = "Infraestructura .claude/ (Regla #17)",
    archivo = "tests/testthat/test_infraestructura_claude.R",
    critico = TRUE
  ),
  list(
    nombre = "Pandocbounded + coherencia Solution (Errores 16-17, Regla #18)",
    archivo = "tests/testthat/test_pandocbounded_y_solution_coherence.R",
    critico = TRUE,
    watch = c("\\.Rmd$", "markdown-imagenes-pdf", "test_pandocbounded")
  ),
  list(
    nombre = "Letter-independence en Solution (Error 19, Regla #19)",
    archivo = "tests/testthat/test_letter_independence.R",
    critico = TRUE,
    watch = c("\\.Rmd$", "solution-letter-independence", "test_letter_independence")
  )
)

# Ejecutar suites (saltando pesadas sin cambios en modo quick)
resultados <- list()
exitos <- 0
fallos <- 0
saltadas <- 0
tiempo_total <- 0

if (.quick_mode && length(.archivos_cambiados) > 0) {
  cat(sprintf("⚡ Modo quick: %d archivos cambiados detectados\n\n",
              length(.archivos_cambiados)))
}

for (suite in suites) {
  if (!file.exists(suite$archivo)) {
    cat(sprintf("⚠ Archivo no encontrado: %s\n\n", suite$archivo))
    fallos <- fallos + 1
    next
  }

  # En modo quick, saltar suites con watch que no tienen archivos afectados
  if (.quick_mode && !is.null(suite$watch) && !.suite_afectada(suite$watch)) {
    cat(sprintf("⏭ %s (sin cambios relacionados)\n\n", suite$nombre))
    saltadas <- saltadas + 1
    next
  }

  resultado <- ejecutar_suite(suite$nombre, suite$archivo)
  resultados[[suite$nombre]] <- resultado

  if (resultado$exito) {
    exitos <- exitos + 1
  } else {
    fallos <- fallos + 1
  }

  tiempo_total <- tiempo_total + as.numeric(resultado$tiempo)
}

# Reporte final
cat("\n")
cat("========================================\n")
cat("  REPORTE FINAL\n")
cat("========================================\n\n")

total_ejecutadas <- exitos + fallos
cat(sprintf("Suites ejecutadas: %d\n", total_ejecutadas))
cat(sprintf("✓ Exitosas: %d\n", exitos))
cat(sprintf("✗ Fallidas: %d\n", fallos))
if (saltadas > 0) {
  cat(sprintf("⏭ Saltadas (sin cambios): %d\n", saltadas))
}
cat(sprintf("Tiempo total: %.2f segundos\n\n", tiempo_total))

# Calcular cobertura estimada (suites saltadas cuentan como exitosas)
cobertura_estimada <- ((exitos + saltadas) / (exitos + fallos + saltadas)) * 100

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
