################################################################################
# SCRIPT: verificar_solucion_matplotlib.R
# DESCRIPCIÓN: Verifica que la solución de matplotlib/reticulate esté funcionando
# FECHA: 2025-01-30
################################################################################

cat("\n")
cat("================================================================================\n")
cat("  VERIFICACIÓN DE SOLUCIÓN MATPLOTLIB/RETICULATE\n")
cat("================================================================================\n\n")

# Colores para output
verde <- "\033[32m"
rojo <- "\033[31m"
amarillo <- "\033[33m"
reset <- "\033[0m"

exito <- function(msg) cat(paste0(verde, "✓ ", msg, reset, "\n"))
error <- function(msg) cat(paste0(rojo, "✗ ", msg, reset, "\n"))
advertencia <- function(msg) cat(paste0(amarillo, "⚠ ", msg, reset, "\n"))

# Contador de tests
tests_pasados <- 0
tests_totales <- 0

# TEST 1: Verificar variable RETICULATE_USE_MANAGED_VENV
cat("TEST 1: Verificar variable RETICULATE_USE_MANAGED_VENV\n")
cat("-------------------------------------------------------\n")
tests_totales <- tests_totales + 1

managed_venv <- Sys.getenv("RETICULATE_USE_MANAGED_VENV")
cat(sprintf("Valor: '%s'\n", managed_venv))

if (managed_venv == "no") {
  exito("Variable configurada correctamente")
  tests_pasados <- tests_pasados + 1
} else {
  error("Variable NO configurada o valor incorrecto")
  advertencia("Ejecuta: echo 'RETICULATE_USE_MANAGED_VENV=\"no\"' >> ~/.Renviron")
}
cat("\n")

# TEST 2: Verificar variable RETICULATE_PYTHON
cat("TEST 2: Verificar variable RETICULATE_PYTHON\n")
cat("---------------------------------------------\n")
tests_totales <- tests_totales + 1

reticulate_python <- Sys.getenv("RETICULATE_PYTHON")
cat(sprintf("Valor: '%s'\n", reticulate_python))

if (reticulate_python == "/usr/bin/python3") {
  exito("Variable configurada correctamente")
  tests_pasados <- tests_pasados + 1
} else {
  error("Variable NO configurada o valor incorrecto")
  advertencia("Ejecuta: echo 'RETICULATE_PYTHON=\"/usr/bin/python3\"' >> ~/.Renviron")
}
cat("\n")

# TEST 3: Cargar reticulate y verificar Python usado
cat("TEST 3: Verificar Python usado por reticulate\n")
cat("----------------------------------------------\n")
tests_totales <- tests_totales + 1

library(reticulate)
config <- py_config()

cat(sprintf("Python: %s\n", config$python))
cat(sprintf("Versión: %s\n", config$version))

if (grepl("/usr/bin/python3", config$python)) {
  exito("Usando Python del sistema")
  tests_pasados <- tests_pasados + 1
} else {
  error("NO está usando Python del sistema")
  if (grepl("uv", config$python)) {
    advertencia("Está usando entorno virtual de uv")
    advertencia("Elimina el caché: rm -rf ~/.cache/R/reticulate")
  }
}
cat("\n")

# TEST 4: Verificar que NO esté usando entorno virtual de uv
cat("TEST 4: Verificar que NO esté usando entorno virtual\n")
cat("-----------------------------------------------------\n")
tests_totales <- tests_totales + 1

if (!grepl("uv", config$python) && !grepl("virtualenv", config$python)) {
  exito("NO está usando entorno virtual")
  tests_pasados <- tests_pasados + 1
} else {
  error("Está usando entorno virtual")
  advertencia("Elimina el caché: rm -rf ~/.cache/R/reticulate")
}
cat("\n")

# TEST 5: Verificar disponibilidad de matplotlib
cat("TEST 5: Verificar disponibilidad de matplotlib\n")
cat("-----------------------------------------------\n")
tests_totales <- tests_totales + 1

matplotlib_disponible <- py_module_available("matplotlib")
cat(sprintf("matplotlib disponible: %s\n", matplotlib_disponible))

if (matplotlib_disponible) {
  exito("matplotlib está disponible")
  tests_pasados <- tests_pasados + 1
  
  # Obtener versión
  tryCatch({
    py_run_string("import matplotlib")
    version <- py_run_string("import matplotlib; print(matplotlib.__version__)")
    cat(sprintf("Versión de matplotlib: %s\n", version))
  }, error = function(e) {
    advertencia("No se pudo obtener la versión de matplotlib")
  })
} else {
  error("matplotlib NO está disponible")
  advertencia("Instala matplotlib: sudo pacman -S python-matplotlib")
}
cat("\n")

# TEST 6: Verificar disponibilidad de numpy
cat("TEST 6: Verificar disponibilidad de numpy\n")
cat("------------------------------------------\n")
tests_totales <- tests_totales + 1

numpy_disponible <- py_module_available("numpy")
cat(sprintf("numpy disponible: %s\n", numpy_disponible))

if (numpy_disponible) {
  exito("numpy está disponible")
  tests_pasados <- tests_pasados + 1
  
  # Obtener versión
  tryCatch({
    py_run_string("import numpy")
    version <- py_run_string("import numpy; print(numpy.__version__)")
    cat(sprintf("Versión de numpy: %s\n", version))
  }, error = function(e) {
    advertencia("No se pudo obtener la versión de numpy")
  })
} else {
  error("numpy NO está disponible")
  advertencia("Instala numpy: sudo pacman -S python-numpy")
}
cat("\n")

# TEST 7: Probar importación de matplotlib.pyplot
cat("TEST 7: Probar importación de matplotlib.pyplot\n")
cat("------------------------------------------------\n")
tests_totales <- tests_totales + 1

tryCatch({
  py_run_string("import matplotlib.pyplot as plt")
  exito("matplotlib.pyplot se importó correctamente")
  tests_pasados <- tests_pasados + 1
}, error = function(e) {
  error("Error al importar matplotlib.pyplot")
  cat(sprintf("Error: %s\n", e$message))
})
cat("\n")

# TEST 8: Verificar que el mensaje de py_config sea correcto
cat("TEST 8: Verificar mensaje de forzado de Python\n")
cat("-----------------------------------------------\n")
tests_totales <- tests_totales + 1

# Capturar output de py_config
output <- capture.output(py_config())
mensaje_correcto <- any(grepl("forced by RETICULATE_PYTHON", output))

if (mensaje_correcto) {
  exito("Python forzado por RETICULATE_PYTHON (correcto)")
  tests_pasados <- tests_pasados + 1
} else {
  mensaje_incorrecto <- any(grepl("forced by py_require", output))
  if (mensaje_incorrecto) {
    error("Python forzado por py_require() (incorrecto)")
    advertencia("Elimina el caché: rm -rf ~/.cache/R/reticulate")
  } else {
    advertencia("No se encontró mensaje de forzado")
  }
}
cat("\n")

# RESUMEN FINAL
cat("================================================================================\n")
cat("  RESUMEN DE VERIFICACIÓN\n")
cat("================================================================================\n\n")

porcentaje <- round((tests_pasados / tests_totales) * 100)
cat(sprintf("Tests pasados: %d/%d (%d%%)\n\n", tests_pasados, tests_totales, porcentaje))

if (tests_pasados == tests_totales) {
  exito("¡TODOS LOS TESTS PASARON!")
  cat("\n")
  cat(paste0(verde, "✓ La solución está funcionando correctamente", reset, "\n"))
  cat(paste0(verde, "✓ Puedes compilar archivos .Rmd con chunks Python", reset, "\n"))
  cat(paste0(verde, "✓ matplotlib y numpy están disponibles", reset, "\n"))
  cat("\n")
} else if (tests_pasados >= tests_totales * 0.75) {
  advertencia("La mayoría de los tests pasaron, pero hay algunos problemas")
  cat("\nRevisa los tests que fallaron arriba para más detalles.\n")
} else {
  error("Varios tests fallaron")
  cat("\nRevisa la documentación en SOLUCION_DEFINITIVA_MATPLOTLIB_RETICULATE.md\n")
  cat("para aplicar la solución completa.\n")
}

cat("\n")
cat("================================================================================\n\n")

# Retornar código de salida
if (tests_pasados == tests_totales) {
  quit(status = 0)
} else {
  quit(status = 1)
}

