#!/usr/bin/env Rscript

# Script de prueba para verificar la aleatorización de opciones
# Este script ejecuta la función generar_datos() múltiples veces para verificar
# que la aleatorización funciona correctamente

# Cargar librerías necesarias
library(exams)

# Configurar opciones
options(scipen = 999)
options(OutDec = ".")
Sys.setlocale(category = "LC_NUMERIC", locale = "C")

# Función de formato estándar
formato_estandar <- function(x, decimales = 2) {
  if (decimales == 0) {
    return(as.character(as.integer(x)))
  } else {
    resultado <- sprintf(paste0("%.", decimales, "f"), x)
    return(resultado)
  }
}

# Función auxiliar para símbolos matemáticos
format_le <- function() {
  "\\le"
}

# Función principal de generación de datos (copiada del archivo principal)
generar_datos <- function() {
  # 1. Aleatorización de Parámetros Matemáticos
  p_central <- sample(seq(0.40, 0.55, by = 0.01), 1)
  p_lateral <- (1 - p_central) / 2

  limite1 <- sample(3:6, 1)
  ancho_central <- sample(2:6, 1)
  limite2 <- limite1 + ancho_central
  limite_sup <- 14

  if (limite2 >= limite_sup) {
    limite2 <- limite_sup - 2
  }

  # 3. Creación de Intervalos (Formato adaptativo)
  intervalo1_txt <- paste0("0 ", format_le(), " x ", format_le(), " ", limite1)
  intervalo2_txt <- paste0(limite1, " < x ", format_le(), " ", limite2)
  intervalo3_txt <- paste0(limite2, " < x ", format_le(), " ", limite_sup)

  # 3. Generación de Opciones (DataFrames)
  tabla_correcta <- data.frame(
    Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
    Probabilidad = c(p_lateral, p_central, p_lateral),
    stringsAsFactors = FALSE
  )
  tabla_distractor_A <- data.frame(
    Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
    Probabilidad = c(p_lateral, p_lateral + p_central, 1),
    stringsAsFactors = FALSE
  )
  tabla_distractor_B <- data.frame(
    Intervalo = c(paste0("0 ", format_le(), " x ", format_le(), " ", limite1),
                  paste0("0 ", format_le(), " x ", format_le(), " ", limite2),
                  paste0("0 ", format_le(), " x ", format_le(), " ", limite_sup)),
    Probabilidad = c(p_lateral, p_central, p_lateral),
    stringsAsFactors = FALSE
  )
  tabla_distractor_D <- data.frame(
    Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
    Probabilidad = c(p_central, p_lateral, p_lateral),
    stringsAsFactors = FALSE
  )

  # 4. SISTEMA DE ALEATORIZACIÓN DE OPCIONES
  # Crear una tabla alternativa con encabezados intercambiados para mayor diversidad
  tabla_correcta_alt <- data.frame(
    Probabilidad = c(p_lateral, p_central, p_lateral),
    Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
    stringsAsFactors = FALSE
  )
  
  # Aleatorizar qué tabla correcta usar (normal o con encabezados intercambiados)
  usar_encabezados_alt <- sample(c(TRUE, FALSE), 1)
  tabla_correcta_final <- if (usar_encabezados_alt) tabla_correcta_alt else tabla_correcta
  
  # Crear lista de todas las opciones disponibles
  todas_las_opciones <- list(tabla_distractor_A, tabla_distractor_B, tabla_correcta_final, tabla_distractor_D)
  
  # Aleatorizar posiciones: seleccionar 3 posiciones de las 4 disponibles
  posiciones_seleccionadas <- sample(1:4, 3, replace = FALSE)
  
  # Asegurar que la tabla correcta esté incluida
  if (!3 %in% posiciones_seleccionadas) {
    # Si la tabla correcta no está seleccionada, reemplazar una posición aleatoria
    posicion_a_reemplazar <- sample(1:3, 1)
    posiciones_seleccionadas[posicion_a_reemplazar] <- 3
  }
  
  # Crear lista final de opciones y determinar posición correcta
  lista_tablas_aleatorias <- todas_las_opciones[posiciones_seleccionadas]
  posicion_correcta_aleatoria <- which(posiciones_seleccionadas == 3)

  return(list(
    p_central = p_central,
    p_lateral = p_lateral,
    limite1 = limite1,
    limite2 = limite2,
    limite_sup = limite_sup,
    opciones = lista_tablas_aleatorias,
    posicion_correcta = posicion_correcta_aleatoria,
    usar_encabezados_alt = usar_encabezados_alt,
    posiciones_seleccionadas = posiciones_seleccionadas
  ))
}

# Ejecutar pruebas
cat("=== PRUEBA DE ALEATORIZACIÓN DE OPCIONES ===\n\n")

# Contadores para estadísticas
posiciones_correctas <- c()
uso_encabezados_alt <- c()
combinaciones_posiciones <- list()

# Ejecutar 20 pruebas
for (i in 1:20) {
  datos <- generar_datos()
  
  cat(sprintf("Prueba %d:\n", i))
  cat(sprintf("  Posición correcta: %d (Tabla %s)\n", datos$posicion_correcta, LETTERS[datos$posicion_correcta]))
  cat(sprintf("  Posiciones seleccionadas: %s\n", paste(datos$posiciones_seleccionadas, collapse = ", ")))
  cat(sprintf("  Encabezados alternativos: %s\n", datos$usar_encabezados_alt))
  cat(sprintf("  Parámetros: p_central=%.2f, límites=[%d, %d]\n\n", 
              datos$p_central, datos$limite1, datos$limite2))
  
  # Recopilar estadísticas
  posiciones_correctas <- c(posiciones_correctas, datos$posicion_correcta)
  uso_encabezados_alt <- c(uso_encabezados_alt, datos$usar_encabezados_alt)
  combinaciones_posiciones[[i]] <- sort(datos$posiciones_seleccionadas)
}

# Mostrar estadísticas
cat("=== ESTADÍSTICAS DE ALEATORIZACIÓN ===\n")
cat(sprintf("Distribución de posiciones correctas:\n"))
for (pos in 1:3) {
  count <- sum(posiciones_correctas == pos)
  cat(sprintf("  Tabla %s: %d veces (%.1f%%)\n", LETTERS[pos], count, count/20*100))
}

cat(sprintf("\nUso de encabezados alternativos:\n"))
count_alt <- sum(uso_encabezados_alt)
cat(sprintf("  Sí: %d veces (%.1f%%)\n", count_alt, count_alt/20*100))
cat(sprintf("  No: %d veces (%.1f%%)\n", 20-count_alt, (20-count_alt)/20*100))

cat(sprintf("\nCombinaciones de posiciones únicas: %d\n", length(unique(combinaciones_posiciones))))

cat("\n=== PRUEBA COMPLETADA ===\n")
