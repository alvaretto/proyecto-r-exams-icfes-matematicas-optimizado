library(exams)

cat("=== EXAMINANDO make_exams_write_pandoc ===\n")

# Obtener la función
exams_ns <- asNamespace("exams")
pandoc_func <- get("make_exams_write_pandoc", envir = exams_ns)

# Mostrar el código de la función
cat("Código de make_exams_write_pandoc:\n")
cat("=====================================\n")

# Convertir la función a texto para examinarla
func_text <- deparse(pandoc_func)

# Buscar líneas que contengan "append" y "after"
append_lines <- grep("append.*after", func_text, value = TRUE)

if(length(append_lines) > 0) {
  cat("Líneas problemáticas encontradas:\n")
  for(i in seq_along(append_lines)) {
    cat(sprintf("%d: %s\n", i, trimws(append_lines[i])))
  }
} else {
  cat("No se encontraron líneas con 'append' y 'after'\n")
}

# Buscar también líneas que contengan "make_pd_item_list"
pd_lines <- grep("make_pd_item_list", func_text, value = TRUE)
if(length(pd_lines) > 0) {
  cat("\nLíneas con make_pd_item_list:\n")
  for(i in seq_along(pd_lines)) {
    cat(sprintf("%d: %s\n", i, trimws(pd_lines[i])))
  }
}

# Mostrar las primeras 50 líneas de la función para entender su estructura
cat("\nPrimeras 50 líneas de la función:\n")
cat("=================================\n")
for(i in 1:min(50, length(func_text))) {
  cat(sprintf("%3d: %s\n", i, func_text[i]))
}

cat("\n=== FIN DEL EXAMEN ===\n")
