# =============================================================================
# CONFIGURACIÓN CORREGIDA DE PYTHON PARA RETICULATE
# =============================================================================
# Este script soluciona el error: "Specified version of python '' does not exist"

# Librerías esenciales
library(exams)
library(ggplot2)
library(knitr)
library(reticulate)
library(testthat)

# =============================================================================
# SOLUCIÓN 1: Configurar Python usando la ruta completa
# =============================================================================

# Verificar qué versiones de Python están disponibles
cat("=== DIAGNÓSTICO DE PYTHON ===\n")
cat("Verificando rutas de Python disponibles:\n")

# Verificar python3 (más común en sistemas Linux)
python3_path <- Sys.which("python3")
cat("python3:", python3_path, "\n")

# Verificar python (menos común, pero posible)
python_path <- Sys.which("python")
cat("python:", python_path, "\n")

# =============================================================================
# CONFIGURACIÓN AUTOMÁTICA DE PYTHON
# =============================================================================

configurar_python <- function() {
  # Intentar configurar python3 primero (más común en Linux)
  if (python3_path != "") {
    cat("\n=== CONFIGURANDO PYTHON3 ===\n")
    cat("Usando python3 en:", python3_path, "\n")
    
    # Configurar reticulate para usar python3
    use_python(python3_path, required = TRUE)
    
    # Verificar la configuración
    cat("Configuración exitosa!\n")
    cat("Versión de Python configurada:", py_config()$python, "\n")
    cat("Versión:", py_config()$version, "\n")
    
    return(TRUE)
    
  } else if (python_path != "") {
    cat("\n=== CONFIGURANDO PYTHON ===\n")
    cat("Usando python en:", python_path, "\n")
    
    # Configurar reticulate para usar python
    use_python(python_path, required = TRUE)
    
    # Verificar la configuración
    cat("Configuración exitosa!\n")
    cat("Versión de Python configurada:", py_config()$python, "\n")
    cat("Versión:", py_config()$version, "\n")
    
    return(TRUE)
    
  } else {
    cat("\n=== ERROR ===\n")
    cat("No se encontró ninguna instalación de Python en el sistema.\n")
    cat("Por favor, instala Python o verifica que esté en el PATH.\n")
    
    return(FALSE)
  }
}

# =============================================================================
# EJECUTAR CONFIGURACIÓN
# =============================================================================

# Intentar configurar Python automáticamente
if (configurar_python()) {
  cat("\n=== PRUEBA DE FUNCIONALIDAD ===\n")
  
  # Probar que Python funciona correctamente
  tryCatch({
    # Ejecutar un comando simple de Python
    py_run_string("print('¡Python configurado correctamente desde R!')")
    py_run_string("import sys; print(f'Versión de Python: {sys.version}')")
    
    cat("✅ Python está funcionando correctamente con reticulate!\n")
    
  }, error = function(e) {
    cat("❌ Error al ejecutar Python:", e$message, "\n")
  })
  
} else {
  cat("❌ No se pudo configurar Python. Revisa la instalación.\n")
}

# =============================================================================
# ALTERNATIVAS MANUALES (si la configuración automática falla)
# =============================================================================

cat("\n=== ALTERNATIVAS MANUALES ===\n")
cat("Si la configuración automática falla, puedes usar:\n\n")

cat("# Opción 1: Especificar ruta completa\n")
cat("use_python('/usr/bin/python3', required = TRUE)\n\n")

cat("# Opción 2: Usar conda (si está instalado)\n")
cat("use_condaenv('base')\n\n")

cat("# Opción 3: Usar virtualenv (si está configurado)\n")
cat("use_virtualenv('nombre_del_entorno')\n\n")

cat("# Opción 4: Configurar variable de entorno\n")
cat("Sys.setenv(RETICULATE_PYTHON = '/usr/bin/python3')\n")
cat("use_python(Sys.getenv('RETICULATE_PYTHON'), required = TRUE)\n\n")

# =============================================================================
# CÓDIGO CORREGIDO PARA TU PROYECTO
# =============================================================================

cat("=== CÓDIGO CORREGIDO PARA TU PROYECTO ===\n")
cat("Reemplaza tu código original con:\n\n")

codigo_corregido <- '
# Librerías esenciales
library(exams)
library(ggplot2)
library(knitr)
library(reticulate)
library(testthat)

# Configurar Python (VERSIÓN CORREGIDA)
python3_path <- Sys.which("python3")
if (python3_path != "") {
  use_python(python3_path, required = TRUE)
  cat("Python configurado correctamente:", python3_path, "\\n")
} else {
  # Fallback: intentar ruta manual común en Linux
  use_python("/usr/bin/python3", required = TRUE)
}
'

cat(codigo_corregido)