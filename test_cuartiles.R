# Script de prueba para verificar que los cuartiles se calculan correctamente
library(testthat)

# Función para calcular estadísticas usando método tradicional
calcular_estadisticas <- function(datos) {
  datos_ordenados <- sort(datos)
  n <- length(datos_ordenados)
  
  # Método tradicional para cuartiles
  pos_q1 <- (n + 1) / 4
  pos_q2 <- (n + 1) / 2
  pos_q3 <- 3 * (n + 1) / 4
  
  # Función para interpolar cuando la posición no es entera
  interpolar <- function(datos, posicion) {
    if (posicion == floor(posicion)) {
      return(datos[posicion])
    } else {
      pos_inferior <- floor(posicion)
      pos_superior <- ceiling(posicion)
      peso <- posicion - pos_inferior
      
      if (pos_superior > length(datos)) {
        return(datos[pos_inferior])
      }
      
      return(datos[pos_inferior] * (1 - peso) + datos[pos_superior] * peso)
    }
  }
  
  q1 <- interpolar(datos_ordenados, pos_q1)
  mediana <- interpolar(datos_ordenados, pos_q2)
  q3 <- interpolar(datos_ordenados, pos_q3)
  
  list(
    minimo = unname(as.numeric(min(datos_ordenados))),
    q1 = unname(as.numeric(q1)),
    mediana = unname(as.numeric(mediana)),
    q3 = unname(as.numeric(q3)),
    maximo = unname(as.numeric(max(datos_ordenados)))
  )
}

# Datos de prueba
datos_test <- c(65, 70, 72, 75, 78, 80, 82, 85, 88, 90, 92, 95)

# Calcular estadísticas
stats <- calcular_estadisticas(datos_test)

cat("Datos ordenados:", paste(sort(datos_test), collapse = ", "), "\n")
cat("n =", length(datos_test), "\n")
cat("Posición Q1:", (length(datos_test) + 1) / 4, "\n")
cat("Posición Q2:", (length(datos_test) + 1) / 2, "\n")
cat("Posición Q3:", 3 * (length(datos_test) + 1) / 4, "\n")
cat("\nEstadísticas calculadas:\n")
cat("Mínimo:", stats$minimo, "\n")
cat("Q1:", stats$q1, "\n")
cat("Mediana:", stats$mediana, "\n")
cat("Q3:", stats$q3, "\n")
cat("Máximo:", stats$maximo, "\n")

# Comparar con quantile() de R
cat("\nComparación con quantile() de R:\n")
q_r <- quantile(datos_test, c(0.25, 0.5, 0.75))
cat("Q1 (R):", q_r[1], "vs Q1 (tradicional):", stats$q1, "\n")
cat("Q2 (R):", q_r[2], "vs Q2 (tradicional):", stats$mediana, "\n")
cat("Q3 (R):", q_r[3], "vs Q3 (tradicional):", stats$q3, "\n")

# Verificar que las estadísticas son coherentes
validar_estadisticas <- function(stats) {
  stats$minimo <= stats$q1 &&
    stats$q1 <= stats$mediana &&
    stats$mediana <= stats$q3 &&
    stats$q3 <= stats$maximo
}

cat("\n¿Estadísticas coherentes?", validar_estadisticas(stats), "\n")
