# Script para debuggear la aleatorización
library(exams)

# Simular múltiples ejecuciones
resultados <- c()

for(i in 1:10) {
  # Establecer semilla aleatoria para diversidad
  semilla_base <- as.numeric(Sys.time())
  semilla_micro <- as.numeric(format(Sys.time(), "%OS6")) * 1000000
  semilla_proceso <- Sys.getpid()
  semilla_final <- (semilla_base + semilla_micro + semilla_proceso + i*1000) %% .Machine$integer.max
  set.seed(semilla_final)
  
  # Definir los tipos de gráficas disponibles
  tipos_graficas <- c(
    "circular_categoria",    # Gráfica circular por categoría (CORRECTA)
    "barras_apiladas",      # Gráfica de barras apiladas por semana
    "circular_semana",      # Gráfica circular por semana
    "barras_agrupadas"      # Gráfica de barras agrupadas por categoría
  )
  
  # Aleatorizar el orden en que se muestran las gráficas en el enunciado
  orden_presentacion <- sample(1:4)
  tipos_en_orden_mostrado <- tipos_graficas[orden_presentacion]
  
  # Crear mapeo: cada letra A, B, C, D corresponde a una gráfica en el orden aleatorizado
  mapeo_graficas <- data.frame(
    letra = LETTERS[1:4],  # A, B, C, D (fijo para Answerlist)
    tipo_grafica = tipos_en_orden_mostrado,  # Tipos en orden aleatorio
    posicion_mostrada = 1:4,  # Posición en el enunciado (1=primera, 2=segunda, etc.)
    stringsAsFactors = FALSE
  )
  
  # Encontrar qué letra corresponde a la gráfica circular por categoría
  indice_respuesta_correcta <- which(mapeo_graficas$tipo_grafica == "circular_categoria")
  
  # La respuesta correcta es la letra correspondiente
  respuesta_correcta_schoice <- LETTERS[indice_respuesta_correcta]
  
  resultados[i] <- respuesta_correcta_schoice
  
  cat("Iteración", i, "- Orden:", paste(orden_presentacion, collapse=","), 
      "- Respuesta correcta:", respuesta_correcta_schoice, "\n")
}

cat("\nResumen de resultados:\n")
table(resultados)
