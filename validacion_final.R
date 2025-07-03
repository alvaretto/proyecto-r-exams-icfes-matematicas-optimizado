#!/usr/bin/env Rscript

cat("🎯 VALIDACIÓN FINAL DEL SISTEMA R-EXAMS ICFES\n")
cat("=" %R% 50, "\n")

# Verificar R
cat("📊 R Version:", R.version.string, "\n")

# Verificar RStudio
rstudio_path <- Sys.which("rstudio")
if (rstudio_path != "") {
  cat("✅ RStudio instalado en:", rstudio_path, "\n")
} else {
  cat("❌ RStudio no encontrado\n")
}

# Verificar paquetes críticos
paquetes_criticos <- c("exams", "knitr", "rmarkdown", "reticulate", "tinytex", "ggplot2", "tidyverse")
cat("\n🔧 Paquetes críticos:\n")
todos_ok <- TRUE
for (paquete in paquetes_criticos) {
  if (require(paquete, character.only = TRUE, quietly = TRUE)) {
    cat("✅", paquete, "\n")
  } else {
    cat("❌", paquete, "\n")
    todos_ok <- FALSE
  }
}

# Verificar TinyTeX
cat("\n📄 TinyTeX:\n")
if (require("tinytex", quietly = TRUE) && tinytex::is_tinytex()) {
  cat("✅ TinyTeX operativo\n")
  cat("📍 Ubicación:", tinytex::tinytex_root(), "\n")
} else {
  cat("❌ TinyTeX no configurado\n")
  todos_ok <- FALSE
}

# Verificar Python
cat("\n�� Python:\n")
if (require("reticulate", quietly = TRUE)) {
  tryCatch({
    reticulate::use_python("/usr/bin/python3", required = TRUE)
    config <- reticulate::py_config()
    cat("✅ Python:", config$python, "\n")
    cat("📍 Versión:", config$version, "\n")
    
    # Verificar paquetes Python críticos
    paquetes_python <- c("numpy", "matplotlib", "pandas")
    for (pkg in paquetes_python) {
      if (reticulate::py_module_available(pkg)) {
        cat("✅ Python package:", pkg, "\n")
      } else {
        cat("❌ Python package faltante:", pkg, "\n")
        todos_ok <- FALSE
      }
    }
  }, error = function(e) {
    cat("❌ Error Python:", e$message, "\n")
    todos_ok <- FALSE
  })
}

# Test de compilación final
cat("\n🧪 Test de compilación final:\n")
tryCatch({
  library(exams)
  
  # Test con ejercicio simple
  if (file.exists("test_simple.Rmd")) {
    result <- exams2html("test_simple.Rmd", n = 1, name = "validacion_final")
    cat("✅ Compilación R-exams exitosa\n")
  } else {
    cat("⚠️ Archivo test_simple.Rmd no encontrado\n")
  }
  
}, error = function(e) {
  cat("❌ Error compilación:", e$message, "\n")
  todos_ok <- FALSE
})

cat("\n", paste(rep("=", 50), collapse=""), "\n")
if (todos_ok) {
  cat("🎯 INSTALACIÓN COMPLETAMENTE EXITOSA\n")
  cat("✅ Sistema listo para desarrollo R-exams ICFES\n")
  cat("🚀 Puedes iniciar RStudio con: rstudio\n")
} else {
  cat("⚠️ INSTALACIÓN CON ALGUNOS PROBLEMAS\n")
  cat("📝 Revisar errores arriba\n")
}
cat(paste(rep("=", 50), collapse=""), "\n")
