#!/usr/bin/env Rscript

cat("🎯 Verificación de instalación R-exams ICFES\n")
cat("===========================================\n\n")

# Verificar R
cat("📊 R:\n")
cat("  Versión:", R.version.string, "\n")
cat("  Rutas de biblioteca:", paste(.libPaths(), collapse = ", "), "\n\n")

# Verificar paquetes principales
required_packages <- c("exams", "knitr", "rmarkdown", "ggplot2", "dplyr", "reticulate")
cat("📦 Paquetes principales:\n")
for (pkg in required_packages) {
  status <- if (require(pkg, character.only = TRUE, quietly = TRUE)) "✓ Instalado" else "✗ Faltante"
  cat("  ", pkg, ":", status, "\n")
}

# Verificar LaTeX
cat("\n📄 LaTeX:\n")
if (require("tinytex", quietly = TRUE)) {
  if (tinytex::is_tinytex()) {
    cat("  TinyTeX: ✓ Instalado\n")
  } else {
    cat("  TeX Live: ✓ Disponible (sistema)\n")
  }
} else {
  cat("  Estado: Verificar manualmente con 'pdflatex --version'\n")
}

# Verificar Python
cat("\n🐍 Python:\n")
if (require("reticulate", quietly = TRUE)) {
  tryCatch({
    config <- py_config()
    cat("  Versión:", config$version, "\n")
    cat("  Ejecutable:", config$python, "\n")
    
    # Verificar módulos Python
    modules <- c("numpy", "matplotlib", "pandas")
    for (mod in modules) {
      status <- if (py_module_available(mod)) "✓" else "✗"
      cat("  ", mod, ":", status, "\n")
    }
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
  })
} else {
  cat("  reticulate no disponible\n")
}

cat("\n🎯 Verificación completada\n")
