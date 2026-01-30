# Script de verificación del entorno para evaluación Gemini-CLI vs Augment
# Autor: Evaluación Gemini-CLI MCP
# Fecha: Sys.Date()

cat("🔍 VERIFICACIÓN DEL ENTORNO DE PRUEBAS\n")
cat(rep("=", 60), "\n\n")

# Función para verificar paquetes R
verificar_paquete <- function(paquete) {
  if (requireNamespace(paquete, quietly = TRUE)) {
    version <- packageVersion(paquete)
    cat("✅", paquete, "versión", as.character(version), "\n")
    return(TRUE)
  } else {
    cat("❌", paquete, "NO INSTALADO\n")
    return(FALSE)
  }
}

# Función para verificar comandos del sistema
verificar_comando <- function(comando) {
  resultado <- system(paste("which", comando), intern = TRUE, ignore.stderr = TRUE)
  if (length(resultado) > 0 && resultado != "") {
    cat("✅", comando, "disponible en:", resultado, "\n")
    return(TRUE)
  } else {
    cat("❌", comando, "NO ENCONTRADO\n")
    return(FALSE)
  }
}

# 1. VERIFICAR PAQUETES R ESENCIALES
cat("📦 VERIFICANDO PAQUETES R\n")
cat(rep("-", 30), "\n")

paquetes_requeridos <- c(
  "exams", "reticulate", "knitr", "rmarkdown", 
  "testthat", "digest", "tikzDevice"
)

paquetes_ok <- sapply(paquetes_requeridos, verificar_paquete)
cat("\n")

# 2. VERIFICAR HERRAMIENTAS DEL SISTEMA
cat("🛠️ VERIFICANDO HERRAMIENTAS DEL SISTEMA\n")
cat(rep("-", 30), "\n")

comandos_requeridos <- c("python3", "pdflatex", "node", "npm", "git")
comandos_ok <- sapply(comandos_requeridos, verificar_comando)
cat("\n")

# 3. VERIFICAR GEMINI-CLI
cat("💎 VERIFICANDO GEMINI-CLI\n")
cat(rep("-", 30), "\n")

gemini_disponible <- verificar_comando("gemini")
if (gemini_disponible) {
  version_gemini <- system("gemini --version 2>/dev/null || echo 'versión no disponible'", 
                          intern = TRUE, ignore.stderr = TRUE)
  cat("📋 Versión Gemini-CLI:", version_gemini, "\n")
}
cat("\n")

# 4. VERIFICAR PYTHON Y LIBRERÍAS
cat("🐍 VERIFICANDO PYTHON Y LIBRERÍAS\n")
cat(rep("-", 30), "\n")

if (verificar_comando("python3")) {
  # Verificar librerías Python esenciales
  librerias_python <- c("matplotlib", "numpy", "pandas")
  
  for (lib in librerias_python) {
    resultado <- system(paste("python3 -c 'import", lib, "; print(\"✅\", \"", lib, "\")'"), 
                       intern = TRUE, ignore.stderr = TRUE)
    if (length(resultado) > 0) {
      cat(resultado, "\n")
    } else {
      cat("❌", lib, "NO DISPONIBLE\n")
    }
  }
}
cat("\n")

# 5. VERIFICAR CONFIGURACIÓN RETICULATE
cat("🔗 VERIFICANDO CONFIGURACIÓN RETICULATE\n")
cat(rep("-", 30), "\n")

if (paquetes_ok["reticulate"]) {
  library(reticulate)
  
  # Verificar Python disponible para reticulate
  python_config <- py_config()
  cat("✅ Python para reticulate:", python_config$python, "\n")
  cat("✅ Versión Python:", python_config$version, "\n")
  
  # Probar importación básica
  tryCatch({
    py_run_string("import matplotlib.pyplot as plt")
    py_run_string("import numpy as np")
    cat("✅ Matplotlib y NumPy disponibles en reticulate\n")
  }, error = function(e) {
    cat("❌ Error en librerías Python para reticulate:", e$message, "\n")
  })
}
cat("\n")

# 6. VERIFICAR ARCHIVOS DE PRUEBA
cat("📁 VERIFICANDO ARCHIVOS DE PRUEBA\n")
cat(rep("-", 30), "\n")

archivos_prueba <- c(
  "../Copia de 01-S1-2024B/gastos_carro_graficas_comparacion_interpretacion_representacion_n2_op*_v1/gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_v1.Rmd",
  "../Copia de 01-S1-2024B/gastos_carro_graficas_comparacion_interpretacion_representacion_n2_op*_v1/gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_cloze_v1.Rmd"
)

for (archivo in archivos_prueba) {
  if (file.exists(archivo)) {
    size <- file.size(archivo)
    cat("✅", basename(archivo), "- Tamaño:", size, "bytes\n")
  } else {
    cat("❌", basename(archivo), "NO ENCONTRADO\n")
  }
}
cat("\n")

# 7. RESUMEN FINAL
cat("📊 RESUMEN DE VERIFICACIÓN\n")
cat(rep("=", 30), "\n")

total_paquetes <- length(paquetes_requeridos)
paquetes_instalados <- sum(paquetes_ok)

total_comandos <- length(comandos_requeridos)
comandos_disponibles <- sum(comandos_ok)

cat("📦 Paquetes R:", paquetes_instalados, "/", total_paquetes, "\n")
cat("🛠️ Comandos sistema:", comandos_disponibles, "/", total_comandos, "\n")
cat("💎 Gemini-CLI:", ifelse(gemini_disponible, "✅ DISPONIBLE", "❌ NO DISPONIBLE"), "\n")

# Determinar estado general
if (paquetes_instalados == total_paquetes && 
    comandos_disponibles == total_comandos && 
    gemini_disponible) {
  cat("\n🎉 ENTORNO COMPLETAMENTE CONFIGURADO\n")
  cat("✅ Listo para ejecutar pruebas de evaluación\n")
} else {
  cat("\n⚠️ CONFIGURACIÓN INCOMPLETA\n")
  cat("❗ Revisar elementos faltantes antes de continuar\n")
}

cat("\n📋 PRÓXIMOS PASOS:\n")
cat("1. Configurar API key de Gemini si no está configurada\n")
cat("2. Ejecutar pruebas específicas de cada herramienta\n")
cat("3. Comparar resultados y generar métricas\n")
cat("\n")
