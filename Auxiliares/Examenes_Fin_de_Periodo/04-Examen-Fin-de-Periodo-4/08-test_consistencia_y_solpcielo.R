#!/usr/bin/env Rscript
# ==============================================================================
# SCRIPT DE DIAGNÓSTICO: Consistencia de preguntas y template solpcielo
# ==============================================================================
# Propósito: 
# 1. Verificar que exams2pandoc y exams2pdf generen las mismas preguntas
# 2. Diagnosticar el error con el template "solpcielo"
# ==============================================================================

library(exams)

# Configuración
dir_ejercicios <- getwd()
dir_salida <- file.path(dir_ejercicios, "salida_test")
dir.create(dir_salida, showWarnings = FALSE, recursive = TRUE)

# Usar una semilla fija para reproducibilidad
semilla_fija <- 12345

# Seleccionar 3 ejercicios de prueba (que sabemos que funcionan)
ejercicios_prueba <- c(
  "124-descuentos_porcentajes_v2.Rmd",
  "125-descuentos_porcentajes_v1.Rmd",
  "094-parabrisas-2.Rmd"
)

cat("================================================================================\n")
cat("  TEST DE CONSISTENCIA Y DIAGNÓSTICO SOLPCIELO\n")
cat("================================================================================\n\n")

cat("Ejercicios de prueba:\n")
for(i in seq_along(ejercicios_prueba)) {
  cat(sprintf("  %d. %s\n", i, ejercicios_prueba[i]))
}
cat("\n")

# ==============================================================================
# TEST 1: exams2pandoc (DOCX con soluciones)
# ==============================================================================

cat("--------------------------------------------------------------------------------\n")
cat("TEST 1: exams2pandoc (DOCX con soluciones)\n")
cat("--------------------------------------------------------------------------------\n")

tryCatch({
  set.seed(semilla_fija)
  exams2pandoc(ejercicios_prueba,
               n = 1,
               name = "test_pandoc_sol",
               encoding = "UTF-8",
               template = "pcielo.tex",
               dir = dir_salida,
               edir = dir_ejercicios,
               verbose = FALSE,
               type = "docx")
  cat("✓ exams2pandoc (con soluciones) - EXITOSO\n\n")
}, error = function(e) {
  cat(sprintf("✗ exams2pandoc (con soluciones) - ERROR: %s\n\n", e$message))
})

# ==============================================================================
# TEST 2: exams2pandoc (DOCX sin soluciones)
# ==============================================================================

cat("--------------------------------------------------------------------------------\n")
cat("TEST 2: exams2pandoc (DOCX sin soluciones)\n")
cat("--------------------------------------------------------------------------------\n")

tryCatch({
  set.seed(semilla_fija)
  exams2pandoc(ejercicios_prueba,
               n = 1,
               name = "test_pandoc_nosol",
               encoding = "UTF-8",
               template = "pcielo_nosol.tex",
               solution = FALSE,
               dir = dir_salida,
               edir = dir_ejercicios,
               verbose = FALSE,
               type = "docx")
  cat("✓ exams2pandoc (sin soluciones) - EXITOSO\n\n")
}, error = function(e) {
  cat(sprintf("✗ exams2pandoc (sin soluciones) - ERROR: %s\n\n", e$message))
})

# ==============================================================================
# TEST 3: exams2pdf con template "exam" (sin soluciones)
# ==============================================================================

cat("--------------------------------------------------------------------------------\n")
cat("TEST 3: exams2pdf con template 'exam' (sin soluciones)\n")
cat("--------------------------------------------------------------------------------\n")

tryCatch({
  set.seed(semilla_fija)
  exams2pdf(ejercicios_prueba,
            n = 1,
            name = "test_pdf_exam",
            encoding = "UTF-8",
            template = "exam",
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = FALSE)
  cat("✓ exams2pdf (template 'exam') - EXITOSO\n\n")
}, error = function(e) {
  cat(sprintf("✗ exams2pdf (template 'exam') - ERROR: %s\n\n", e$message))
})

# ==============================================================================
# TEST 4: exams2pdf con template "solpcielo" (con soluciones)
# ==============================================================================

cat("--------------------------------------------------------------------------------\n")
cat("TEST 4: exams2pdf con template 'solpcielo' (con soluciones)\n")
cat("--------------------------------------------------------------------------------\n")

# Primero verificar si el template existe
template_path <- system.file("tex", "solpcielo.tex", package = "exams")
if(template_path == "") {
  # Buscar en el directorio local
  local_templates <- list.files(dir_ejercicios, pattern = "solpcielo\\.tex$", 
                                recursive = TRUE, full.names = TRUE)
  if(length(local_templates) > 0) {
    cat(sprintf("Template 'solpcielo' encontrado en: %s\n", local_templates[1]))
    template_to_use <- local_templates[1]
  } else {
    cat("⚠ Template 'solpcielo' NO encontrado en el paquete ni localmente\n")
    cat("Buscando en todo el repositorio...\n")
    template_to_use <- "solpcielo"
  }
} else {
  cat(sprintf("Template 'solpcielo' encontrado en paquete: %s\n", template_path))
  template_to_use <- "solpcielo"
}

tryCatch({
  set.seed(semilla_fija)
  exams2pdf(ejercicios_prueba,
            n = 1,
            name = "test_pdf_solpcielo",
            encoding = "UTF-8",
            template = template_to_use,
            dir = dir_salida,
            edir = dir_ejercicios,
            verbose = TRUE)  # verbose = TRUE para ver detalles del error
  cat("✓ exams2pdf (template 'solpcielo') - EXITOSO\n\n")
}, error = function(e) {
  cat(sprintf("✗ exams2pdf (template 'solpcielo') - ERROR: %s\n\n", e$message))
  cat("Detalles del error:\n")
  print(e)
  cat("\n")
})

# ==============================================================================
# TEST 5: Verificar consistencia de preguntas
# ==============================================================================

cat("================================================================================\n")
cat("  VERIFICACIÓN DE CONSISTENCIA DE PREGUNTAS\n")
cat("================================================================================\n\n")

cat("Generando exámenes con la misma semilla para comparar...\n\n")

# Generar con exams2pandoc
set.seed(semilla_fija)
exam_pandoc <- exams2pandoc(ejercicios_prueba,
                            n = 1,
                            name = "consistencia_pandoc",
                            encoding = "UTF-8",
                            template = "pcielo.tex",
                            dir = dir_salida,
                            edir = dir_ejercicios,
                            verbose = FALSE,
                            type = "docx")

# Generar con exams2pdf
set.seed(semilla_fija)
exam_pdf <- exams2pdf(ejercicios_prueba,
                      n = 1,
                      name = "consistencia_pdf",
                      encoding = "UTF-8",
                      template = "exam",
                      dir = dir_salida,
                      edir = dir_ejercicios,
                      verbose = FALSE)

cat("Comparando las preguntas generadas...\n\n")

# Extraer información de las preguntas
for(i in seq_along(ejercicios_prueba)) {
  cat(sprintf("Ejercicio %d: %s\n", i, ejercicios_prueba[i]))
  
  # Comparar las respuestas correctas
  if(!is.null(exam_pandoc[[1]][[i]]$metainfo$solution) && 
     !is.null(exam_pdf[[1]][[i]]$metainfo$solution)) {
    
    sol_pandoc <- exam_pandoc[[1]][[i]]$metainfo$solution
    sol_pdf <- exam_pdf[[1]][[i]]$metainfo$solution
    
    if(identical(sol_pandoc, sol_pdf)) {
      cat("  ✓ Soluciones IDÉNTICAS\n")
    } else {
      cat("  ✗ Soluciones DIFERENTES\n")
      cat(sprintf("    - exams2pandoc: %s\n", paste(sol_pandoc, collapse = "")))
      cat(sprintf("    - exams2pdf:    %s\n", paste(sol_pdf, collapse = "")))
    }
  }
  cat("\n")
}

# ==============================================================================
# RESUMEN
# ==============================================================================

cat("================================================================================\n")
cat("  RESUMEN DE RESULTADOS\n")
cat("================================================================================\n\n")

cat("Archivos generados en:", dir_salida, "\n\n")

archivos_generados <- list.files(dir_salida, pattern = "\\.(pdf|docx)$")
if(length(archivos_generados) > 0) {
  cat("Archivos creados:\n")
  for(archivo in archivos_generados) {
    cat(sprintf("  - %s\n", archivo))
  }
} else {
  cat("⚠ No se generaron archivos\n")
}

cat("\n")
cat("Revisa los resultados arriba para identificar:\n")
cat("  1. Si el template 'solpcielo' funciona correctamente\n")
cat("  2. Si las preguntas son consistentes entre formatos\n")
cat("\n")
cat("================================================================================\n")

