#!/usr/bin/env Rscript

# Script para diagnosticar ejercicios problemáticos individualmente
# Prueba cada ejercicio de la última ejecución para identificar cuál causa el error

library(exams)

# Ejercicios de la última ejecución (según el log)
ejercicios <- c(
  "026-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-A.Rmd",
  "099-volumen_cilindro_hueco_py_v1.Rmd",
  "002-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_2.Rmd",
  "115-volumen_cilindro_interpretacion_v1.Rmd",
  "155-2023-Matematicas-11-2-09-Opc-A.Rmd",
  "143-grafico_circular_bienes_v0.Rmd",
  "153-2023-Matematicas-11-2-09-Op-C.Rmd",
  "110-porcentajes_grupos_poblacion_v1.Rmd",
  "016-estadistica_media_calificaciones_n2_v1.Rmd",
  "024-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-B2v2.Rmd",
  "175-vuelo_acrobatico_D.Rmd",
  "178-vuelo_acrobatico_A.Rmd",
  "098-volumen_cilindro_hueco_R_v1.Rmd",
  "172-vuelo_acrobatico_mejorado_C.Rmd",
  "091-probabilidad_extraccion_bolas_v1.Rmd"
)

cat("================================================================================\n")
cat("  DIAGNÓSTICO DE EJERCICIOS INDIVIDUALES\n")
cat("================================================================================\n\n")

problematicos <- character(0)

for (i in seq_along(ejercicios)) {
  cat(sprintf("[%2d/%2d] Probando: %s\n", i, length(ejercicios), ejercicios[i]))
  
  tryCatch({
    # Intentar generar el ejercicio en formato DOCX
    exams2pandoc(ejercicios[i],
                 n = 1,
                 name = paste0("test_", i),
                 encoding = "UTF-8",
                 template = "pcielo.tex",
                 dir = "test_diagnostico",
                 verbose = FALSE,
                 type = "docx")
    
    cat("        ✓ OK\n")
    
  }, error = function(e) {
    cat(sprintf("        ✗ ERROR: %s\n", e$message))
    problematicos <<- c(problematicos, ejercicios[i])
  })
}

cat("\n")
cat("================================================================================\n")
cat("  RESUMEN\n")
cat("================================================================================\n\n")

if (length(problematicos) == 0) {
  cat("✓ Todos los ejercicios se compilaron correctamente de forma individual.\n")
  cat("  El problema puede estar en la combinación de ejercicios o en la configuración.\n\n")
} else {
  cat(sprintf("✗ Se encontraron %d ejercicio(s) problemático(s):\n\n", length(problematicos)))
  for (i in seq_along(problematicos)) {
    cat(sprintf("  %d. %s\n", i, problematicos[i]))
  }
  cat("\n")
}

# Limpiar directorio de prueba
if (dir.exists("test_diagnostico")) {
  unlink("test_diagnostico", recursive = TRUE)
}

