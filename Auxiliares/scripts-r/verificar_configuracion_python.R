#!/usr/bin/env Rscript

# ==============================================================================
# SCRIPT DE VERIFICACIÓN DE CONFIGURACIÓN PYTHON/RETICULATE
# ==============================================================================
# Propósito: Verificar que la configuración de Python/Reticulate está correcta
# Uso: Rscript verificar_configuracion_python.R
# ==============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  VERIFICACIÓN DE CONFIGURACIÓN PYTHON/RETICULATE               ║\n")
cat("║  Proyecto: ICFES R-exams Matemáticas                          ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Contador de errores
errores <- 0
advertencias <- 0

# ==============================================================================
# 1. VERIFICAR R
# ==============================================================================
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("📊 VERIFICANDO R\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

cat("Versión de R:", R.version.string, "\n")

# Verificar versión mínima de R
r_version <- as.numeric(paste(R.version$major, R.version$minor, sep = "."))
if (r_version >= 4.5) {
  cat("✅ Versión de R adecuada (>= 4.5)\n")
} else {
  cat("⚠️  Versión de R antigua. Se recomienda R >= 4.5\n")
  advertencias <- advertencias + 1
}

cat("\nRutas de bibliotecas:\n")
for (i in seq_along(.libPaths())) {
  cat(sprintf("  [%d] %s\n", i, .libPaths()[i]))
}

# Verificar biblioteca personal
if (any(grepl("~/R/library|/home/.*/R/library", .libPaths()))) {
  cat("✅ Biblioteca personal configurada\n")
} else {
  cat("⚠️  Biblioteca personal no encontrada en .libPaths()\n")
  advertencias <- advertencias + 1
}

cat("\n")

# ==============================================================================
# 2. VERIFICAR PAQUETES R
# ==============================================================================
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("📦 VERIFICANDO PAQUETES R\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

paquetes_requeridos <- c("exams", "reticulate", "knitr", "rmarkdown", "testthat")
paquetes_opcionales <- c("ggplot2", "tidyverse", "digest")

cat("\nPaquetes requeridos:\n")
for (pkg in paquetes_requeridos) {
  if (require(pkg, quietly = TRUE, character.only = TRUE)) {
    version <- as.character(packageVersion(pkg))
    cat(sprintf("  ✅ %-15s %s\n", pkg, version))
  } else {
    cat(sprintf("  ❌ %-15s NO INSTALADO\n", pkg))
    errores <- errores + 1
  }
}

cat("\nPaquetes opcionales:\n")
for (pkg in paquetes_opcionales) {
  if (require(pkg, quietly = TRUE, character.only = TRUE)) {
    version <- as.character(packageVersion(pkg))
    cat(sprintf("  ✅ %-15s %s\n", pkg, version))
  } else {
    cat(sprintf("  ⚠️  %-15s NO INSTALADO (opcional)\n", pkg))
  }
}

cat("\n")

# ==============================================================================
# 3. VERIFICAR PYTHON
# ==============================================================================
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("🐍 VERIFICANDO PYTHON\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

library(reticulate)

if (py_available()) {
  cat("✅ Python disponible\n\n")
  
  config <- py_config()
  cat("Configuración de Python:\n")
  cat(sprintf("  Ejecutable:  %s\n", config$python))
  cat(sprintf("  Versión:     %s\n", config$version))
  cat(sprintf("  libpython:   %s\n", config$libpython))
  cat(sprintf("  pythonhome:  %s\n", config$pythonhome))
  
  # Verificar que es Python del sistema
  if (grepl("/usr/bin/python", config$python)) {
    cat("✅ Usando Python del sistema (/usr/bin/python3)\n")
  } else {
    cat("⚠️  No está usando /usr/bin/python3\n")
    advertencias <- advertencias + 1
  }
  
  # Verificar versión de Python
  python_version <- as.numeric(strsplit(config$version, " ")[[1]][1])
  if (python_version >= 3.10) {
    cat("✅ Versión de Python adecuada (>= 3.10)\n")
  } else {
    cat("⚠️  Versión de Python antigua. Se recomienda >= 3.10\n")
    advertencias <- advertencias + 1
  }
  
} else {
  cat("❌ Python NO disponible\n")
  cat("   Ejecutar: sudo pacman -S python\n")
  errores <- errores + 1
}

cat("\n")

# ==============================================================================
# 4. VERIFICAR MÓDULOS PYTHON
# ==============================================================================
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("📚 VERIFICANDO MÓDULOS PYTHON\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

modulos_requeridos <- c("matplotlib", "numpy", "matplotlib.pyplot")
modulos_opcionales <- c("pandas", "scipy", "seaborn", "pillow")

cat("\nMódulos requeridos:\n")
for (mod in modulos_requeridos) {
  if (py_module_available(mod)) {
    cat(sprintf("  ✅ %s\n", mod))
  } else {
    cat(sprintf("  ❌ %s NO DISPONIBLE\n", mod))
    errores <- errores + 1
  }
}

cat("\nMódulos opcionales:\n")
for (mod in modulos_opcionales) {
  if (py_module_available(mod)) {
    cat(sprintf("  ✅ %s\n", mod))
  } else {
    cat(sprintf("  ⚠️  %s NO DISPONIBLE (opcional)\n", mod))
  }
}

cat("\n")

# ==============================================================================
# 5. PROBAR TRANSFERENCIA R → PYTHON
# ==============================================================================
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("🔄 PROBANDO TRANSFERENCIA R → PYTHON\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

test_numero <- 42
test_texto <- "Hola desde R"
test_vector <- c(1, 2, 3, 4, 5)

tryCatch({
  py_run_string("resultado_numero = r.test_numero * 2")
  py_run_string("resultado_texto = r.test_texto + ' → Python'")
  py_run_string("import numpy as np; resultado_vector = np.array(r.test_vector) * 2")
  
  resultado_numero <- py$resultado_numero
  resultado_texto <- py$resultado_texto
  resultado_vector <- py$resultado_vector
  
  if (resultado_numero == 84) {
    cat("✅ Transferencia de números: OK\n")
    cat(sprintf("   R: %d → Python: %d\n", test_numero, resultado_numero))
  } else {
    cat("❌ Error en transferencia de números\n")
    errores <- errores + 1
  }
  
  if (grepl("Python", resultado_texto)) {
    cat("✅ Transferencia de texto: OK\n")
    cat(sprintf("   R: '%s' → Python: '%s'\n", test_texto, resultado_texto))
  } else {
    cat("❌ Error en transferencia de texto\n")
    errores <- errores + 1
  }
  
  if (all(resultado_vector == test_vector * 2)) {
    cat("✅ Transferencia de vectores: OK\n")
    cat(sprintf("   R: [%s] → Python: [%s]\n", 
                paste(test_vector, collapse=", "),
                paste(resultado_vector, collapse=", ")))
  } else {
    cat("❌ Error en transferencia de vectores\n")
    errores <- errores + 1
  }
  
}, error = function(e) {
  cat("❌ Error en transferencia R → Python:", e$message, "\n")
  errores <- errores + 1
})

cat("\n")

# ==============================================================================
# 6. PROBAR MATPLOTLIB
# ==============================================================================
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("📊 PROBANDO MATPLOTLIB\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

tryCatch({
  py_run_string("
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np

# Crear gráfico de prueba
x = np.linspace(0, 10, 100)
y = np.sin(x)

plt.figure(figsize=(6, 4))
plt.plot(x, y, color='blue', linewidth=2)
plt.title('Test Plot - Matplotlib')
plt.xlabel('x')
plt.ylabel('sin(x)')
plt.grid(True, alpha=0.3)

# Guardar en /tmp
plt.savefig('/tmp/test_matplotlib_verificacion.png', dpi=150, bbox_inches='tight')
plt.close()

print('OK')
  ")
  
  cat("✅ Matplotlib funciona correctamente\n")
  cat("   Gráfico de prueba guardado en: /tmp/test_matplotlib_verificacion.png\n")
  
  # Verificar que el archivo existe
  if (file.exists("/tmp/test_matplotlib_verificacion.png")) {
    file_info <- file.info("/tmp/test_matplotlib_verificacion.png")
    cat(sprintf("   Tamaño del archivo: %.1f KB\n", file_info$size / 1024))
  }
  
}, error = function(e) {
  cat("❌ Error en matplotlib:", e$message, "\n")
  errores <- errores + 1
})

cat("\n")

# ==============================================================================
# 7. VERIFICAR ARCHIVOS DE CONFIGURACIÓN
# ==============================================================================
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("📄 VERIFICANDO ARCHIVOS DE CONFIGURACIÓN\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Verificar .Rprofile
if (file.exists(".Rprofile")) {
  cat("✅ .Rprofile encontrado en directorio actual\n")
  
  # Verificar que contiene use_python
  rprofile_content <- readLines(".Rprofile")
  if (any(grepl("use_python", rprofile_content))) {
    cat("✅ .Rprofile contiene configuración de Python\n")
  } else {
    cat("⚠️  .Rprofile no contiene use_python()\n")
    advertencias <- advertencias + 1
  }
} else {
  cat("⚠️  .Rprofile no encontrado en directorio actual\n")
  advertencias <- advertencias + 1
}

# Verificar .Renviron
renviron_path <- path.expand("~/.Renviron")
if (file.exists(renviron_path)) {
  cat("✅ ~/.Renviron encontrado\n")
  
  # Verificar que contiene R_LIBS_USER
  renviron_content <- readLines(renviron_path)
  if (any(grepl("R_LIBS_USER", renviron_content))) {
    cat("✅ ~/.Renviron contiene R_LIBS_USER\n")
  } else {
    cat("⚠️  ~/.Renviron no contiene R_LIBS_USER\n")
    advertencias <- advertencias + 1
  }
} else {
  cat("⚠️  ~/.Renviron no encontrado\n")
  advertencias <- advertencias + 1
}

cat("\n")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  RESUMEN DE VERIFICACIÓN                                       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

if (errores == 0 && advertencias == 0) {
  cat("🎉 ¡EXCELENTE! Configuración completamente correcta\n")
  cat("   No se encontraron errores ni advertencias.\n")
  cat("   El sistema está listo para usar Python/Reticulate.\n")
} else if (errores == 0) {
  cat("✅ CONFIGURACIÓN FUNCIONAL\n")
  cat(sprintf("   Se encontraron %d advertencia(s) pero no hay errores críticos.\n", advertencias))
  cat("   El sistema debería funcionar correctamente.\n")
} else {
  cat("❌ SE ENCONTRARON PROBLEMAS\n")
  cat(sprintf("   Errores críticos: %d\n", errores))
  cat(sprintf("   Advertencias: %d\n", advertencias))
  cat("\n")
  cat("   Revisar los mensajes anteriores y corregir los errores.\n")
  cat("   Consultar: CONFIGURACION_PYTHON_RETICULATE_COMPLETA.md\n")
}

cat("\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("Fecha de verificación:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\n")

# Retornar código de salida apropiado
if (errores > 0) {
  quit(status = 1)
} else {
  quit(status = 0)
}

