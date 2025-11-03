################################################################################
# SCRIPT: configurar_python.R
# DESCRIPCIÓN: Configura reticulate para usar el Python del sistema
#              con matplotlib y numpy instalados
# FECHA: 2025-01-30
################################################################################

library(reticulate)

cat("================================================================================\n")
cat("  CONFIGURACIÓN DE PYTHON PARA RETICULATE\n")
cat("================================================================================\n\n")

# Configurar para usar Python del sistema
use_python("/usr/bin/python3", required = TRUE)

cat("Configuración de Python:\n")
cat("------------------------\n")
py_config()

cat("\n================================================================================\n")
cat("  VERIFICACIÓN DE MÓDULOS PYTHON\n")
cat("================================================================================\n\n")

# Verificar matplotlib
cat("Verificando matplotlib...\n")
tryCatch({
  py_run_string("import matplotlib")
  matplotlib_version <- py_run_string("import matplotlib; print(matplotlib.__version__)")
  cat("✓ matplotlib instalado\n\n")
}, error = function(e) {
  cat("✗ matplotlib NO disponible\n")
  cat("  Error:", e$message, "\n\n")
})

# Verificar numpy
cat("Verificando numpy...\n")
tryCatch({
  py_run_string("import numpy")
  numpy_version <- py_run_string("import numpy; print(numpy.__version__)")
  cat("✓ numpy instalado\n\n")
}, error = function(e) {
  cat("✗ numpy NO disponible\n")
  cat("  Error:", e$message, "\n\n")
})

cat("================================================================================\n")
cat("  CONFIGURACIÓN COMPLETADA\n")
cat("================================================================================\n\n")

cat("Si matplotlib y numpy están disponibles, puedes proceder a generar exámenes.\n")
cat("Si no están disponibles, ejecuta:\n")
cat("  sudo pacman -S python-matplotlib python-numpy\n\n")

