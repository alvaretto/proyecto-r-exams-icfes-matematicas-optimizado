# Script para verificar que las opciones de respuesta son únicas
library(testthat)

# Función para comparar si dos conjuntos de estadísticas son idénticos
son_identicos <- function(stats1, stats2, tolerancia = 0.01) {
  abs(stats1$minimo - stats2$minimo) < tolerancia &&
    abs(stats1$q1 - stats2$q1) < tolerancia &&
    abs(stats1$mediana - stats2$mediana) < tolerancia &&
    abs(stats1$q3 - stats2$q3) < tolerancia &&
    abs(stats1$maximo - stats2$maximo) < tolerancia
}

# Simular múltiples ejecuciones del generador de distractores
cat("Probando unicidad de opciones en múltiples ejecuciones...\n\n")

for (ejecucion in 1:10) {
  cat("Ejecución", ejecucion, ":\n")
  
  # Simular datos de prueba
  set.seed(ejecucion * 123)
  datos_test <- round(rnorm(15, 75, 10), 1)
  
  # Calcular estadísticas correctas (simulando el método tradicional)
  stats_correctas <- list(
    minimo = min(datos_test),
    q1 = quantile(datos_test, 0.25, type = 1),
    mediana = median(datos_test),
    q3 = quantile(datos_test, 0.75, type = 1),
    maximo = max(datos_test)
  )
  
  # Simular generación de distractores (versión simplificada)
  distractores <- list()
  
  # Distractor 1: Mediana desplazada
  d1 <- stats_correctas
  d1$mediana <- stats_correctas$mediana + 2
  distractores[[1]] <- d1
  
  # Distractor 2: Dispersión modificada
  d2 <- stats_correctas
  d2$q1 <- stats_correctas$q1 - 1
  d2$q3 <- stats_correctas$q3 + 1
  distractores[[2]] <- d2
  
  # Distractor 3: Extremos modificados
  d3 <- stats_correctas
  d3$minimo <- stats_correctas$minimo - 2
  d3$maximo <- stats_correctas$maximo + 2
  distractores[[3]] <- d3
  
  # Crear todas las opciones
  todas_opciones <- c(list(stats_correctas), distractores)
  
  # Verificar unicidad
  opciones_unicas <- TRUE
  for (i in 1:length(todas_opciones)) {
    for (j in 1:length(todas_opciones)) {
      if (i != j && son_identicos(todas_opciones[[i]], todas_opciones[[j]])) {
        cat("  ✗ Opciones", i, "y", j, "son idénticas!\n")
        opciones_unicas <- FALSE
      }
    }
  }
  
  if (opciones_unicas) {
    cat("  ✓ Todas las opciones son únicas\n")
  }
  
  # Mostrar estadísticas de cada opción
  cat("  Estadísticas generadas:\n")
  for (i in 1:length(todas_opciones)) {
    opt <- todas_opciones[[i]]
    nombre <- if (i == 1) "Correcta" else paste("Distractor", i-1)
    cat("    ", nombre, ": min=", opt$minimo, ", Q1=", opt$q1, 
        ", med=", opt$mediana, ", Q3=", opt$q3, ", max=", opt$maximo, "\n")
  }
  cat("\n")
}

cat("Prueba completada. Verificar que todas las ejecuciones muestren '✓ Todas las opciones son únicas'\n")
