# Demo: Verificación de símbolos de moneda en el ejercicio
# Muestra cómo se formatean los valores monetarios

cat("=== DEMO: SÍMBOLOS DE MONEDA ===\n\n")

# Simular la lógica del ejercicio para valores monetarios
set.seed(789)  # Para reproducibilidad

# Generar valores como en el ejercicio
cargo_fijo <- sample(c(2500, 2700, 2800, 3000, 3200), 1)
precio_por_m3 <- sample(seq(800, 1200, 50), 1)
consumo_factura <- sample(10:15, 1)
total_factura <- cargo_fijo + (consumo_factura * precio_por_m3)

cat("VALORES GENERADOS:\n")
cat("==================\n")
cat("Cargo fijo:", cargo_fijo, "\n")
cat("Precio por m³:", precio_por_m3, "\n")
cat("Consumo del mes de factura:", consumo_factura, "m³\n")
cat("Costo variable:", consumo_factura * precio_por_m3, "\n")
cat("Total factura:", total_factura, "\n\n")

cat("FORMATO EN EL TEXTO DEL PROBLEMA:\n")
cat("=================================\n")

# Simular el texto como aparece en el problema
texto_problema <- paste0(
  "En el mes consumieron ", consumo_factura, " metros cúbicos de gas natural ",
  "y la factura fue de $", format(total_factura, big.mark = ".", decimal.mark = ","), 
  ", incluido el cargo fijo de $", format(cargo_fijo, big.mark = ".", decimal.mark = ","), 
  " que es el mismo todos los meses."
)

cat("\"", texto_problema, "\"\n\n")

cat("FORMATO EN LA SOLUCIÓN:\n")
cat("=======================\n")

# Simular el texto de verificación como aparece en la solución
texto_solucion <- paste0(
  "La factura ($", format(total_factura, big.mark = ".", decimal.mark = ","), 
  ") corresponde exactamente al consumo mostrado en la gráfica (",
  consumo_factura, " m³ × $", format(precio_por_m3, big.mark = ".", decimal.mark = ","),
  " + $", format(cargo_fijo, big.mark = ".", decimal.mark = ","), " cargo fijo)."
)

cat("\"", texto_solucion, "\"\n\n")

cat("VERIFICACIÓN MATEMÁTICA:\n")
cat("========================\n")
cat("Cálculo manual:\n")
cat("- Consumo:", consumo_factura, "m³\n")
cat("- Precio por m³: $", format(precio_por_m3, big.mark = ".", decimal.mark = ","), "\n")
cat("- Costo variable:", consumo_factura, "×", "$", format(precio_por_m3, big.mark = ".", decimal.mark = ","), 
    "= $", format(consumo_factura * precio_por_m3, big.mark = ".", decimal.mark = ","), "\n")
cat("- Cargo fijo: $", format(cargo_fijo, big.mark = ".", decimal.mark = ","), "\n")
cat("- TOTAL: $", format(consumo_factura * precio_por_m3, big.mark = ".", decimal.mark = ","), 
    "+ $", format(cargo_fijo, big.mark = ".", decimal.mark = ","), 
    "= $", format(total_factura, big.mark = ".", decimal.mark = ","), "\n")

# Verificar coherencia
coherente <- (cargo_fijo + (consumo_factura * precio_por_m3)) == total_factura
cat("¿Es coherente?", ifelse(coherente, "✓ SÍ", "✗ NO"), "\n\n")

cat("EJEMPLOS DE DIFERENTES RANGOS:\n")
cat("==============================\n")

# Mostrar ejemplos de diferentes rangos de valores
ejemplos_cargo <- c(2500, 2700, 2800, 3000, 3200)
ejemplos_precio <- c(800, 950, 1100, 1200)

cat("Rangos de cargo fijo:\n")
for(cargo in ejemplos_cargo) {
  cat("- $", format(cargo, big.mark = ".", decimal.mark = ","), "\n")
}

cat("\nRangos de precio por m³:\n")
for(precio in ejemplos_precio) {
  cat("- $", format(precio, big.mark = ".", decimal.mark = ","), "\n")
}

cat("\nEjemplos de facturas totales (consumo 12 m³):\n")
for(i in 1:4) {
  cargo_ej <- sample(ejemplos_cargo, 1)
  precio_ej <- sample(ejemplos_precio, 1)
  total_ej <- cargo_ej + (12 * precio_ej)
  cat("- Cargo $", format(cargo_ej, big.mark = ".", decimal.mark = ","), 
      "+ Variable $", format(12 * precio_ej, big.mark = ".", decimal.mark = ","), 
      "= $", format(total_ej, big.mark = ".", decimal.mark = ","), "\n")
}

cat("\nCARACTERÍSTICAS DEL FORMATO:\n")
cat("============================\n")
cat("✓ Símbolo '$' antes de cada valor monetario\n")
cat("✓ Separador de miles: punto (.)\n")
cat("✓ Separador decimal: coma (,) - formato colombiano\n")
cat("✓ Sin decimales para valores enteros\n")
cat("✓ Formato consistente en todo el ejercicio\n")
cat("✓ Valores realistas para el contexto colombiano\n")

cat("\n=== FIN DE LA DEMO ===\n")
