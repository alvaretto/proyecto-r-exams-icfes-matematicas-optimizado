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

  # Aislar cada suite en su propio proceso R (tests/run_one_suite.R): evita la
  # contaminación cruzada de estado global entre suites (Python/reticulate — un
  # único intérprete por proceso —, RNG, options, variables globales). El
  # subproceso reporta su veredicto vía exit status: 0 = todo verde; !=0 = hay
  # expect-failures/errores o el script reventó. El helper inspecciona
  # test_file(..., stop_on_failure = FALSE) para no enmascarar fallas de expectativa.
  rscript <- file.path(R.home("bin"), "Rscript")
  status <- tryCatch(
    system2(rscript, args = c("tests/run_one_suite.R", archivo),
            stdout = "", stderr = ""),
    error = function(e) 1L
  )
  tiempo <- difftime(Sys.time(), inicio, units = "secs")
  exito <- identical(as.integer(status), 0L)
  resultado <- if (exito) {
    list(exito = TRUE, tiempo = tiempo)
  } else {
    list(exito = FALSE,
         error = sprintf("subproceso terminó con exit status %s (expect-failures/errores o script roto)",
                         status),
         tiempo = tiempo)
  }

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
  ),
  list(
    nombre = "Tablas Markdown guard 'none' (Error 21, Regla #20)",
    archivo = "tests/testthat/test_markdown_tablas_none_guard.R",
    critico = TRUE,
    watch = c("\\.Rmd$", "markdown-tablas-pandoc", "test_markdown_tablas_none_guard")
  ),
  list(
    nombre = "data_generation sin cuelgue (Error 22, bucle repeat sin cota)",
    archivo = "tests/testthat/test_data_generation_no_hang.R",
    critico = TRUE,
    watch = c("Rango-Colesterol-Pacientes-Cloze", "test_data_generation_no_hang")
  ),
  list(
    nombre = "CLOZE gráficas-opción fuera del gap (V5, Incidente G)",
    archivo = "tests/testthat/test_cloze_graficas_no_en_gap.R",
    critico = TRUE,
    watch = c("\\.Rmd$", "graficos-como-opciones", "test_cloze_graficas_no_en_gap")
  ),
  list(
    nombre = "_neg_ Variante B + params propio (validador 5C / Capa A)",
    archivo = "tests/testthat/test_neg_variante_b.R",
    critico = TRUE,
    watch = c("validacion-neg-opciones-repetidas", "test_neg_variante_b",
              "validar_coherencia_matematica")
  ),
  list(
    nombre = "Contrato de entrega e independencia del detractor (Regla #9 v1.2)",
    archivo = "tests/testthat/test_contrato_detractor.R",
    critico = TRUE,
    watch = c("agente-detractor", "detractor-obligatorio", "test_contrato_detractor")
  ),
  list(
    nombre = "Contrato de entrega de los orquestadores (paridad con el detractor)",
    archivo = "tests/testthat/test_contrato_entrega_orquestadores.R",
    critico = TRUE,
    watch = c("orquestador-schoice", "orquestador-cloze", "agente-detractor",
              "test_contrato_entrega_orquestadores")
  ),
  list(
    nombre = "Nomenclatura oficial de .Rmd (fuente única + gate + citas)",
    archivo = "tests/testthat/test_nomenclatura_rmd.R",
    critico = TRUE,
    watch = c("NOMENCLATURA_ARCHIVOS_RMD", "pre-write-rmd-gate", "nomenclatura-legacy",
              "test_nomenclatura_rmd", "\\.Rmd$", "generar-schoice", "generar-cloze")
  ),
  list(
    nombre = "Muestra estándar de validación N=100 (regla #23)",
    archivo = "tests/testthat/test_muestra_estandar.R",
    critico = TRUE,
    watch = c("muestra-estandar-validacion", "validar_diagnosticidad",
              "validar_diversidad_sustantiva", "validar_multisemilla",
              "post-exams2-validation", "settings\\.json", "test_muestra_estandar",
              "orquestador-schoice", "orquestador-cloze")
  ),
  list(
    nombre = "Invocabilidad de validar_multisemilla (Error 31)",
    archivo = "tests/testthat/test_validar_multisemilla_invocable.R",
    critico = TRUE,
    watch = c("validar_multisemilla", "stress_test_visual",
              "test_validar_multisemilla_invocable", "post-exams2-validation")
  ),
  list(
    nombre = "Diversidad Sustantiva (Regla #22)",
    archivo = "tests/testthat/test_diversidad_sustantiva.R",
    critico = TRUE,
    watch = c("diversidad-sustantiva", "test_diversidad_sustantiva",
              "validar_diversidad_sustantiva")
  ),
  list(
    nombre = "Diagnosticidad de distractores (V9, margen relativo de H1)",
    archivo = "tests/testthat/test_diagnosticidad.R",
    critico = TRUE,
    watch = c("validar_diagnosticidad", "test_diagnosticidad",
              "orquestador-cloze", "orquestador-schoice")
  ),
  list(
    nombre = "Invariante I-2 barco (bbox del casco == clave)",
    archivo = "tests/testthat/test_barco_bbox_invariante.R",
    critico = TRUE,
    watch = c("plano-cartesiano-barco", "coordenadas_vertices_plano_cartesiano",
              "barco-parametrico", "test_barco_bbox_invariante")
  ),
  list(
    nombre = "Invariantes I-1..I-7 permutaciones (clave == n!)",
    archivo = "tests/testthat/test_permutaciones_invariantes.R",
    critico = TRUE,
    watch = c("permutaciones-pescadores-venia", "permutaciones_pescadores",
              "permutaciones-parametricas", "test_permutaciones_invariantes")
  ),
  list(
    nombre = "Diversidad grafica circular (tabla, meses y par del error)",
    archivo = "tests/testthat/test_diversidad_grafica_circular.R",
    critico = TRUE,
    watch = c("grafica-circular-consumo-agua", "grafica_circular_consumo_agua",
              "test_diversidad_grafica_circular")
  ),
  list(
    nombre = "FASE 2I: detector \\pandocbounded distingue definicion de uso",
    archivo = "tests/testthat/test_fase2i_pandocbounded_detector.R",
    critico = TRUE,
    watch = c("post-exams2-validation", "markdown-imagenes-pdf",
              "test_fase2i_pandocbounded_detector")
  ),
  list(
    nombre = "is_latex_output() es FALSE en los pipelines de R/exams",
    archivo = "tests/testthat/test_is_latex_output_rexams.R",
    critico = TRUE,
    watch = c("familias-soluciones-rmd", "markdown-imagenes-pdf", "codigo-rmd",
              "snippets_familias_rmd", "test_is_latex_output_rexams")
  ),
  list(
    nombre = "Bateria de eliminacion: cierre por familias + techo nulo (regla #22 P7)",
    archivo = "tests/testthat/test_bateria_eliminacion.R",
    critico = TRUE,
    watch = c("bateria_eliminacion", "diversidad-sustantiva",
              "test_bateria_eliminacion")
  ),
  list(
    nombre = "FASE 2O: glifos Unicode que rompen pdflatex (ERR_GLIFO_LATEX)",
    archivo = "tests/testthat/test_glifos_latex.R",
    critico = TRUE,
    watch = c("validar_glifos_latex", "post-exams2-validation",
              "glifos-latex-legacy", "glifos-latex-prohibidos",
              "test_glifos_latex")
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
