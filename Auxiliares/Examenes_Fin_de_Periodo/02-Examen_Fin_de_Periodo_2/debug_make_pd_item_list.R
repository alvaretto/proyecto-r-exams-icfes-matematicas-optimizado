library(exams)

# Script para debuggear el problema específico en make_pd_item_list
cat("=== DEBUG DE make_pd_item_list ===\n")

# Primero, veamos si podemos acceder a la función
if(exists("make_pd_item_list", envir = asNamespace("exams"))) {
  cat("✓ make_pd_item_list encontrada en namespace de exams\n")
  
  # Obtener la función
  make_pd_item_list <- get("make_pd_item_list", envir = asNamespace("exams"))
  
  # Mostrar el código de la función
  cat("\n=== CÓDIGO DE make_pd_item_list ===\n")
  print(make_pd_item_list)
  
} else {
  cat("✗ make_pd_item_list NO encontrada\n")
}

# Intentar reproducir el error con datos mínimos
cat("\n=== INTENTANDO REPRODUCIR EL ERROR ===\n")

# Crear datos de prueba que podrían causar el problema
test_data <- list(
  list(question = "Pregunta 1", questionlist = c("A", "B", "C")),
  list(question = "Pregunta 2", questionlist = c("X", "Y", "Z"))
)

cat("Datos de prueba creados\n")

# Intentar llamar la función si existe
if(exists("make_pd_item_list", envir = asNamespace("exams"))) {
  try({
    result <- make_pd_item_list(test_data)
    cat("✓ make_pd_item_list funcionó con datos de prueba\n")
  }, silent = FALSE)
}

cat("\n=== INFORMACIÓN DE VERSIÓN ===\n")
cat("exams version:", as.character(packageVersion("exams")), "\n")
cat("R version:", R.version.string, "\n")

cat("\n=== FIN DEL DEBUG ===\n")
