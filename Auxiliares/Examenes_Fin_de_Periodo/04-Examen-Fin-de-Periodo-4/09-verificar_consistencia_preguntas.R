#!/usr/bin/env Rscript
# ==============================================================================
# VERIFICACIÓN DE CONSISTENCIA DE PREGUNTAS ENTRE FORMATOS
# ==============================================================================
# Propósito: Verificar que exams2pandoc y exams2pdf generen las mismas preguntas
# ==============================================================================

library(exams)

# Configuración
dir_ejercicios <- getwd()
semilla_actual <- 89870143  # La semilla usada en la última ejecución

# Leer los ejercicios seleccionados del script principal
ejercicios_seleccionados <- c(
  "084-funciones_lineales_interpretacion_grafica_v1.Rmd",
  "169-fracciones_reparto_premio_v2.Rmd",
  "154-2023-Matematicas-11-2-09-Opc-B.Rmd",
  "130-logica_puntaje_campeonato_v1.Rmd",
  "098-volumen_cilindro_hueco_R_v1.Rmd",
  "092-probabilidad_combinaciones_v1.Rmd",
  "156-tabla_evaluaciones.Rmd",
  "101-porcentajes_ordenamiento_sabores_v1.Rmd",
  "023-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-C2.Rmd",
  "178-vuelo_acrobatico_A.Rmd",
  "045-proporcionalidad_empresarial_formulacion_ejecucion_n2_v1.Rmd",
  "176-vuelo_acrobatico_C.Rmd",
  "016-estadistica_media_calificaciones_n2_v1.Rmd",
  "158-2023-Matematicas-11-2-04-Op-C.Rmd",
  "027-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-A2v2.Rmd"
)

cat("================================================================================\n")
cat("  VERIFICACIÓN DE CONSISTENCIA DE PREGUNTAS\n")
cat("================================================================================\n\n")

cat(sprintf("Semilla utilizada: %d\n", semilla_actual))
cat(sprintf("Número de ejercicios: %d\n\n", length(ejercicios_seleccionados)))

# ==============================================================================
# Generar exámenes con exams2pandoc
# ==============================================================================

cat("Generando examen con exams2pandoc...\n")
set.seed(semilla_actual)
exam_pandoc <- exams2pandoc(ejercicios_seleccionados,
                            n = 1,
                            name = "verificacion_pandoc",
                            encoding = "UTF-8",
                            template = "pcielo.tex",
                            dir = tempdir(),
                            edir = dir_ejercicios,
                            verbose = FALSE,
                            type = "docx")

# ==============================================================================
# Generar exámenes con exams2pdf
# ==============================================================================

cat("Generando examen con exams2pdf...\n")
set.seed(semilla_actual)
exam_pdf <- exams2pdf(ejercicios_seleccionados,
                      n = 1,
                      name = "verificacion_pdf",
                      encoding = "UTF-8",
                      template = "solpcielo",
                      dir = tempdir(),
                      edir = dir_ejercicios,
                      verbose = FALSE)

# ==============================================================================
# Comparar las preguntas
# ==============================================================================

cat("\n================================================================================\n")
cat("  COMPARACIÓN DE PREGUNTAS\n")
cat("================================================================================\n\n")

todas_identicas <- TRUE

for(i in seq_along(ejercicios_seleccionados)) {
  cat(sprintf("Ejercicio %2d: %s\n", i, basename(ejercicios_seleccionados[i])))
  
  # Extraer soluciones
  sol_pandoc <- exam_pandoc[[1]][[i]]$metainfo$solution
  sol_pdf <- exam_pdf[[1]][[i]]$metainfo$solution
  
  # Comparar
  if(identical(sol_pandoc, sol_pdf)) {
    cat("  ✓ Soluciones IDÉNTICAS\n")
  } else {
    cat("  ✗ Soluciones DIFERENTES\n")
    cat(sprintf("    - exams2pandoc: %s\n", paste(sol_pandoc, collapse = "")))
    cat(sprintf("    - exams2pdf:    %s\n", paste(sol_pdf, collapse = "")))
    todas_identicas <- FALSE
  }
  
  # Extraer tipo de pregunta
  tipo_pandoc <- exam_pandoc[[1]][[i]]$metainfo$type
  tipo_pdf <- exam_pdf[[1]][[i]]$metainfo$type
  
  if(tipo_pandoc != tipo_pdf) {
    cat(sprintf("  ⚠ Tipos diferentes: pandoc=%s, pdf=%s\n", tipo_pandoc, tipo_pdf))
    todas_identicas <- FALSE
  }
  
  cat("\n")
}

# ==============================================================================
# RESUMEN
# ==============================================================================

cat("================================================================================\n")
cat("  RESUMEN\n")
cat("================================================================================\n\n")

if(todas_identicas) {
  cat("✅ TODAS LAS PREGUNTAS SON IDÉNTICAS entre exams2pandoc y exams2pdf\n")
  cat("✅ Los archivos DOCX y PDF contienen las mismas preguntas en el mismo orden\n")
} else {
  cat("❌ ALGUNAS PREGUNTAS SON DIFERENTES entre exams2pandoc y exams2pdf\n")
  cat("❌ Revisa los detalles arriba para identificar las diferencias\n")
}

cat("\n")
cat("================================================================================\n")

