library(exams)

cat("=== FUNCIONES INTERNAS DE EXAMS ===\n")

# Obtener todas las funciones del namespace de exams
exams_ns <- asNamespace("exams")
all_objects <- ls(exams_ns, all.names = TRUE)

# Filtrar funciones que contengan "pd" o "item" o "list"
relevant_functions <- all_objects[grepl("(pd|item|list|make)", all_objects, ignore.case = TRUE)]

cat("Funciones relevantes encontradas:\n")
for(func in relevant_functions) {
  cat("-", func, "\n")
}

# Buscar específicamente make_pd_item_list con diferentes variaciones
possible_names <- c("make_pd_item_list", ".make_pd_item_list", "make.pd.item.list", 
                   "makePdItemList", "make_pandoc_item_list")

cat("\nBuscando variaciones del nombre:\n")
for(name in possible_names) {
  if(exists(name, envir = exams_ns)) {
    cat("✓ Encontrada:", name, "\n")
  } else {
    cat("✗ No encontrada:", name, "\n")
  }
}

# Buscar en el stack trace - la función debe existir
cat("\nBuscando funciones que contengan 'append' o 'after':\n")
append_functions <- all_objects[sapply(all_objects, function(x) {
  if(exists(x, envir = exams_ns)) {
    obj <- get(x, envir = exams_ns)
    if(is.function(obj)) {
      func_body <- deparse(body(obj))
      any(grepl("append.*after", func_body, ignore.case = TRUE))
    } else {
      FALSE
    }
  } else {
    FALSE
  }
})]

if(length(append_functions) > 0) {
  cat("Funciones con 'append' y 'after':\n")
  for(func in append_functions) {
    cat("-", func, "\n")
  }
} else {
  cat("No se encontraron funciones con 'append' y 'after'\n")
}

cat("\n=== FIN DE BÚSQUEDA ===\n")
