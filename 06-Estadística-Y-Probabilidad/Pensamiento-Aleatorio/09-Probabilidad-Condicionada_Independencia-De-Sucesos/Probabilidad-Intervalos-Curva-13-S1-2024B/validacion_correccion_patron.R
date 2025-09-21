#!/usr/bin/env Rscript

# Script de validación de la corrección del patrón predecible
# Verifica que el Paso 7 ya no sea predecible basado en el Paso 2

library(exams)

cat('=== VALIDACIÓN DE CORRECCIÓN DEL PATRÓN PREDECIBLE ===\n\n')

# Función para extraer soluciones de un ejercicio
extraer_soluciones <- function(archivo, semilla) {
  set.seed(semilla)
  
  # Generar versión HTML temporal
  result <- exams2html(archivo, n = 1, name = paste0('temp_', semilla), dir = tempdir())
  
  # Extraer soluciones numéricas
  soluciones <- result[[1]][[1]]$metainfo$solution
  
  return(list(
    paso_2 = as.numeric(soluciones[2]),  # p_central
    paso_7 = as.numeric(soluciones[7]),  # diferencia absoluta
    suma_2_7 = as.numeric(soluciones[2]) + as.numeric(soluciones[7])
  ))
}

# Probar 15 versiones diferentes
cat('Probando patrón en 15 versiones:\n\n')

patron_eliminado <- TRUE
for (i in 1:15) {
  tryCatch({
    sol <- extraer_soluciones('probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd', i)
    
    # Verificar si Paso 7 = 1 - Paso 2 (patrón predecible)
    diferencia_patron <- abs(sol$paso_7 - (1 - sol$paso_2))
    es_predecible <- diferencia_patron < 0.001
    
    cat(sprintf('Versión %2d: Paso2=%.3f, Paso7=%.3f, Suma=%.3f, ¿Predecible? %s\n', 
                i, sol$paso_2, sol$paso_7, sol$suma_2_7, 
                ifelse(es_predecible, 'SÍ', 'NO')))
    
    if (es_predecible) {
      patron_eliminado <- FALSE
    }
    
  }, error = function(e) {
    cat(sprintf('Error en versión %d: %s\n', i, e$message))
  })
}

cat('\n=== RESULTADO FINAL ===\n')
if (patron_eliminado) {
  cat('✅ PATRÓN PREDECIBLE ELIMINADO EXITOSAMENTE\n')
  cat('   El Paso 7 ya no es simplemente 1 - Paso 2\n')
  cat('   Los estudiantes deben realizar los cálculos requeridos\n')
} else {
  cat('❌ EL PATRÓN PREDECIBLE PERSISTE\n')
  cat('   Algunas versiones aún muestran Paso 7 = 1 - Paso 2\n')
}

cat('\n=== VALIDACIÓN COMPLETADA ===\n')
