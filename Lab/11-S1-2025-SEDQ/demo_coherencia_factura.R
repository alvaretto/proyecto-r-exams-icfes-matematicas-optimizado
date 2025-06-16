# Demo: Coherencia entre factura de octubre y consumo mostrado en la gráfica
# Muestra cómo se calcula la factura basándose en el consumo real

cat("=== DEMO: COHERENCIA FACTURA-CONSUMO ===\n\n")

# Simular la lógica del ejercicio
set.seed(123)  # Para reproducibilidad

# Generar datos como en el ejercicio
consumo_maximo <- sample(c(18, 20, 22, 25), 1)
porcentaje_junio <- sample(c(60, 65, 70, 75, 80, 85, 90), 1)
consumo_junio <- round((porcentaje_junio * consumo_maximo) / 100)

meses <- c("Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre")
consumos_base <- c(
  sample(10:13, 1),  # Abril
  sample(11:14, 1),  # Mayo  
  consumo_junio,     # Junio (valor clave)
  sample(10:13, 1),  # Julio
  sample(10:13, 1),  # Agosto
  sample(11:14, 1),  # Septiembre
  sample(12:15, 1)   # Octubre
)

# Asegurar que ningún consumo exceda el máximo menos 1
for(i in c(1,2,4,5,6,7)) {
  consumos_base[i] <- min(consumos_base[i], consumo_maximo - 1)
}

consumos <- consumos_base
names(consumos) <- meses

# Generar datos económicos coherentes
cargo_fijo <- sample(c(2500, 2700, 2800, 3000, 3200), 1)
consumo_octubre <- consumos[which(names(consumos) == "Octubre")]
precio_por_m3 <- sample(seq(800, 1200, 50), 1)
costo_variable <- consumo_octubre * precio_por_m3
total_factura <- cargo_fijo + costo_variable

cat("DATOS GENERADOS:\n")
cat("================\n")
cat("Consumo máximo posible:", consumo_maximo, "m³\n")
cat("Consumo de junio:", consumo_junio, "m³\n")
cat("Porcentaje de junio:", round((consumo_junio/consumo_maximo)*100), "%\n\n")

cat("CONSUMOS MENSUALES:\n")
cat("===================\n")
for(i in 1:length(meses)) {
  destacar <- if(meses[i] == "Junio") " ← PREGUNTA" else if(meses[i] == "Octubre") " ← FACTURA" else ""
  cat(sprintf("%-12s: %2d m³%s\n", meses[i], consumos[i], destacar))
}

cat("\nCÁLCULO DE LA FACTURA DE OCTUBRE:\n")
cat("==================================\n")
cat("Consumo de octubre:", consumo_octubre, "m³\n")
cat("Cargo fijo:", paste0("$", format(cargo_fijo, big.mark=".", decimal.mark=",")), "\n")
cat("Precio por m³:", paste0("$", format(precio_por_m3, big.mark=".", decimal.mark=",")), "\n")
cat("Costo variable:", paste0("$", format(costo_variable, big.mark=".", decimal.mark=",")), 
    " (", consumo_octubre, " × $", format(precio_por_m3, big.mark=".", decimal.mark=","), ")\n")
cat("TOTAL FACTURA:", paste0("$", format(total_factura, big.mark=".", decimal.mark=",")), "\n")

cat("\nVERIFICACIÓN:\n")
cat("=============\n")
verificacion <- cargo_fijo + (consumo_octubre * precio_por_m3)
cat("Cargo fijo + Costo variable =", paste0("$", format(verificacion, big.mark=".", decimal.mark=",")), "\n")
cat("¿Coincide con total factura?", ifelse(verificacion == total_factura, "✓ SÍ", "✗ NO"), "\n")

cat("\nTEXTO DEL PROBLEMA:\n")
cat("===================\n")
cat("\"En octubre consumieron", consumo_octubre, "metros cúbicos de gas natural\n")
cat("y la factura fue de $", format(total_factura, big.mark=".", decimal.mark=","), 
    ", incluido el cargo fijo de\n")
cat("$", format(cargo_fijo, big.mark=".", decimal.mark=","), 
    " que es el mismo todos los meses.\"\n")

cat("\nBENEFICIOS DE ESTA CORRECCIÓN:\n")
cat("==============================\n")
cat("✓ Coherencia total entre texto y gráfica\n")
cat("✓ Datos económicos realistas\n")
cat("✓ Factura calculada matemáticamente\n")
cat("✓ Información verificable por el estudiante\n")
cat("✓ Mayor credibilidad del problema\n")

cat("\n=== FIN DE LA DEMO ===\n")
