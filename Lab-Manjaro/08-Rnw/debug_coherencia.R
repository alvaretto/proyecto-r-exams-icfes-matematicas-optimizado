# Debug del test de coherencia matemática

# Cargar función generar_datos
source_lines <- readLines("Ahorro_opciones_porcentaje_interpretacion_representacion_n2_v1.Rnw")
start_line <- grep("generar_datos.*function", source_lines)
end_line <- grep("^}$", source_lines)
end_line <- end_line[end_line > start_line][1]

func_lines <- source_lines[start_line:end_line]
func_code <- paste(func_lines, collapse = "\n")
eval(parse(text = func_code))

cat("=== DEBUG COHERENCIA MATEMÁTICA ===\n")

# Generar datos de prueba
datos_test <- generar_datos()

cat("Datos generados:\n")
cat("- Contexto:", datos_test$contexto, "\n")
cat("- Nombre:", datos_test$nombre, "\n")
cat("- Cantidad base:", datos_test$cantidad_base, "\n")
cat("- Número de meses:", datos_test$num_meses, "\n")
cat("- Porcentaje constante:", datos_test$porcentaje_constante, "%\n")
cat("- Porcentajes variables:", paste(datos_test$porcentajes_variables, collapse = ", "), "%\n")

cat("\nAhorros acumulados:\n")
for(i in 1:length(datos_test$ahorros_acumulados)) {
  cat("  Mes", i, ":", datos_test$ahorros_acumulados[i], "\n")
}

cat("\nCálculos Opción 1 (", datos_test$familiar1, "):\n")
for(i in 1:min(3, datos_test$num_meses)) {
  regalo <- datos_test$ahorros_acumulados[i] * (datos_test$porcentaje_constante / 100)
  cat("  Mes", i, ":", datos_test$ahorros_acumulados[i], "×", datos_test$porcentaje_constante, "% =", regalo, "\n")
}
cat("  Total calculado manualmente:", sum(datos_test$ahorros_acumulados[1:min(3, datos_test$num_meses)] * (datos_test$porcentaje_constante / 100)), "\n")
cat("  Total en datos_test:", datos_test$total_regalos_opcion1, "\n")

cat("\nCálculos Opción 2 (", datos_test$familiar2, "):\n")
for(i in 1:length(datos_test$porcentajes_variables)) {
  regalo <- datos_test$ahorros_acumulados[i] * (datos_test$porcentajes_variables[i] / 100)
  cat("  Mes", i, ":", datos_test$ahorros_acumulados[i], "×", datos_test$porcentajes_variables[i], "% =", regalo, "\n")
}
cat("  Total calculado manualmente:", sum(datos_test$ahorros_acumulados[1:length(datos_test$porcentajes_variables)] * (datos_test$porcentajes_variables / 100)), "\n")
cat("  Total en datos_test:", datos_test$total_regalos_opcion2, "\n")

cat("\nOpción mejor según datos_test:", datos_test$opcion_mejor, "\n")
if(datos_test$opcion_mejor == 1) {
  cat("La", datos_test$familiar1, "da más ayuda\n")
} else {
  cat("El", datos_test$familiar2, "da más ayuda\n")
}
