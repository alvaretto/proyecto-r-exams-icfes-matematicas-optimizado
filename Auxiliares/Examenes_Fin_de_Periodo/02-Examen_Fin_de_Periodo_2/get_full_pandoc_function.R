library(exams)

cat("=== FUNCIÓN COMPLETA make_exams_write_pandoc ===\n")

# Obtener la función completa
exams_ns <- asNamespace("exams")
pandoc_func <- get("make_exams_write_pandoc", envir = exams_ns)

# Convertir a texto
func_text <- deparse(pandoc_func)

# Mostrar toda la función
cat("Función completa:\n")
cat("=================\n")
for(i in seq_along(func_text)) {
  cat(sprintf("%3d: %s\n", i, func_text[i]))
}

cat("\n=== FIN DE LA FUNCIÓN ===\n")
