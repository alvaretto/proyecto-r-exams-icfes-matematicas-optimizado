# Demo: Aleatorización de meses en el ejercicio
# Muestra la diversidad de combinaciones de meses generadas

cat("=== DEMO: MESES ALEATORIZADOS ===\n\n")

# Simular la lógica del ejercicio
meses_disponibles <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                      "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

cat("Meses disponibles:", length(meses_disponibles), "\n")
cat(paste(meses_disponibles, collapse=", "), "\n\n")

cat("Ejemplos de combinaciones generadas:\n")
cat("====================================\n")

# Generar 15 ejemplos aleatorios
set.seed(456)  # Para reproducibilidad en la demo
for(i in 1:15) {
  # Seleccionar 7 meses consecutivos aleatoriamente
  inicio_mes <- sample(1:6, 1)  # Asegurar que quepan 7 meses
  meses_seleccionados <- meses_disponibles[inicio_mes:(inicio_mes + 6)]
  
  # Asignar roles: mes de la pregunta (posición aleatoria) y mes de la factura (último mes)
  posicion_pregunta <- sample(1:6, 1)  # No puede ser el último mes
  mes_pregunta <- meses_seleccionados[posicion_pregunta]
  mes_factura <- meses_seleccionados[7]  # Último mes para la factura
  
  cat(sprintf("%2d. Período: %s a %s | Pregunta: %s | Factura: %s\n", 
              i, meses_seleccionados[1], meses_seleccionados[7], mes_pregunta, mes_factura))
}

cat("\nAnálisis de diversidad:\n")
cat("=======================\n")

# Analizar todas las combinaciones posibles
combinaciones_posibles <- 0
meses_pregunta_posibles <- c()
meses_factura_posibles <- c()

for(inicio in 1:6) {
  meses_periodo <- meses_disponibles[inicio:(inicio + 6)]
  mes_factura <- meses_periodo[7]
  
  for(pos_pregunta in 1:6) {
    mes_pregunta <- meses_periodo[pos_pregunta]
    combinaciones_posibles <- combinaciones_posibles + 1
    meses_pregunta_posibles <- c(meses_pregunta_posibles, mes_pregunta)
    meses_factura_posibles <- c(meses_factura_posibles, mes_factura)
  }
}

cat("Total de períodos de 7 meses consecutivos:", 6, "\n")
cat("Posiciones posibles para mes de pregunta:", 6, "\n")
cat("Total de combinaciones únicas:", combinaciones_posibles, "\n")

cat("\nMeses que pueden ser de pregunta:\n")
meses_pregunta_unicos <- unique(meses_pregunta_posibles)
cat(paste(meses_pregunta_unicos, collapse=", "), "\n")
cat("Total:", length(meses_pregunta_unicos), "meses diferentes\n")

cat("\nMeses que pueden ser de factura:\n")
meses_factura_unicos <- unique(meses_factura_posibles)
cat(paste(meses_factura_unicos, collapse=", "), "\n")
cat("Total:", length(meses_factura_unicos), "meses diferentes\n")

cat("\nEjemplos de períodos posibles:\n")
cat("==============================\n")
for(inicio in 1:6) {
  meses_periodo <- meses_disponibles[inicio:(inicio + 6)]
  cat(sprintf("Período %d: %s a %s\n", inicio, meses_periodo[1], meses_periodo[7]))
}

cat("\nBeneficios de esta aleatorización:\n")
cat("==================================\n")
cat("✓ Evita monotonía (no siempre octubre-junio)\n")
cat("✓ Mayor diversidad temporal (36 combinaciones)\n")
cat("✓ Períodos realistas de 7 meses consecutivos\n")
cat("✓ Coherencia: factura siempre del último mes\n")
cat("✓ Flexibilidad: pregunta puede ser cualquier mes excepto el último\n")
cat("✓ Cobertura: incluye todos los meses del año\n")

cat("\nImpacto en la diversidad total:\n")
cat("===============================\n")
cat("Combinaciones de meses:", combinaciones_posibles, "\n")
cat("Combinaciones de nombres:", 10 * 10 * 2, "\n")
cat("Otros parámetros aleatorios:", 4 * 7 * 4 * 5, "aprox.\n")
cat("DIVERSIDAD TOTAL ESTIMADA:", combinaciones_posibles * 200 * 560, "versiones únicas\n")

cat("\n=== FIN DE LA DEMO ===\n")
