# =============================================================================
# SOLUCIÓN COMPLETA PARA ERRORES DE MATPLOTLIB Y RETICULATE
# =============================================================================
# Soluciona los errores:
# 1. "Specified version of python '' does not exist"
# 2. "TypeError: use() got an unexpected keyword argument 'warn'"

# Librerías esenciales
library(exams)
library(ggplot2)
library(knitr)
library(reticulate)
library(testthat)

# =============================================================================
# PASO 1: CONFIGURAR PYTHON CORRECTAMENTE
# =============================================================================

configurar_python_seguro <- function() {
  cat("=== CONFIGURANDO PYTHON PARA RETICULATE ===\n")
  
  # Buscar python3 primero
  python3_path <- Sys.which("python3")
  
  if (python3_path != "") {
    cat("✅ Python3 encontrado en:", python3_path, "\n")
    
    # Configurar reticulate
    use_python(python3_path, required = TRUE)
    
    cat("✅ Python configurado correctamente\n")
    return(TRUE)
    
  } else {
    cat("❌ No se encontró Python3. Intentando rutas manuales...\n")
    
    # Intentar rutas comunes
    rutas_comunes <- c("/usr/bin/python3", "/usr/local/bin/python3", "/opt/python3/bin/python3")
    
    for (ruta in rutas_comunes) {
      if (file.exists(ruta)) {
        cat("✅ Python encontrado en:", ruta, "\n")
        use_python(ruta, required = TRUE)
        return(TRUE)
      }
    }
    
    cat("❌ No se pudo configurar Python\n")
    return(FALSE)
  }
}

# =============================================================================
# PASO 2: CONFIGURAR MATPLOTLIB SIN ERRORES
# =============================================================================

configurar_matplotlib_seguro <- function() {
  cat("\n=== CONFIGURANDO MATPLOTLIB ===\n")
  
  tryCatch({
    # Configurar matplotlib antes de importarlo
    py_run_string("
import os
import sys

# Configurar matplotlib para usar backend no interactivo
import matplotlib
matplotlib.use('Agg')  # Sin parámetros adicionales para evitar errores

# Importar pyplot después de configurar el backend
import matplotlib.pyplot as plt
import numpy as np

print('✅ Matplotlib configurado correctamente con backend Agg')
print(f'Versión de matplotlib: {matplotlib.__version__}')
")
    
    cat("✅ Matplotlib configurado exitosamente\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error configurando matplotlib:", e$message, "\n")
    
    # Intentar configuración alternativa
    cat("Intentando configuración alternativa...\n")
    
    tryCatch({
      py_run_string("
import matplotlib
# Configuración más básica
matplotlib.rcParams['backend'] = 'Agg'
import matplotlib.pyplot as plt
import numpy as np
print('✅ Matplotlib configurado con método alternativo')
")
      
      cat("✅ Matplotlib configurado con método alternativo\n")
      return(TRUE)
      
    }, error = function(e2) {
      cat("❌ Error en configuración alternativa:", e2$message, "\n")
      return(FALSE)
    })
  })
}

# =============================================================================
# PASO 3: FUNCIÓN PARA GENERAR GRÁFICAS SEGURAS
# =============================================================================

generar_grafica_python_segura <- function(paises_destino, datos_ultimo_año, colores_graficas, 
                                         sector_industrial, año_mostrar) {
  
  cat("\n=== GENERANDO GRÁFICA CON PYTHON ===\n")
  
  # Crear código Python seguro
  codigo_python_seguro <- sprintf("
import matplotlib
matplotlib.use('Agg')  # Configurar backend antes de importar pyplot
import matplotlib.pyplot as plt
import numpy as np

# Datos para la gráfica
paises = %s
valores = %s
colores = %s
sector = '%s'
año = %s

# Crear la figura
fig, ax = plt.subplots(figsize=(12, 8))

# Crear gráfica de barras
barras = ax.bar(paises, valores, color=colores, alpha=0.8, edgecolor='black', linewidth=1)

# Configurar título y etiquetas
ax.set_title(f'Exportaciones de {sector} en {año}', fontsize=16, fontweight='bold', pad=20)
ax.set_xlabel('Países de Destino', fontsize=12, fontweight='bold')
ax.set_ylabel('Valor de Exportaciones (Millones USD)', fontsize=12, fontweight='bold')

# Rotar etiquetas del eje x para mejor legibilidad
plt.xticks(rotation=45, ha='right')

# Agregar valores en las barras
for i, (barra, valor) in enumerate(zip(barras, valores)):
    height = barra.get_height()
    ax.text(barra.get_x() + barra.get_width()/2., height + max(valores)*0.01,
            f'{valor:,.0f}', ha='center', va='bottom', fontweight='bold')

# Configurar grid
ax.grid(True, alpha=0.3, axis='y')
ax.set_axisbelow(True)

# Ajustar layout
plt.tight_layout()

# Guardar la gráfica
plt.savefig('grafica_exportaciones.png', dpi=300, bbox_inches='tight', 
            facecolor='white', edgecolor='none')

print('✅ Gráfica generada exitosamente: grafica_exportaciones.png')

# Limpiar la figura para evitar problemas de memoria
plt.close()
",
    paste0("['", paste(paises_destino, collapse="', '"), "']"),
    paste0("[", paste(datos_ultimo_año, collapse=", "), "]"),
    paste0("['", paste(colores_graficas, collapse="', '"), "']"),
    sector_industrial,
    año_mostrar
  )
  
  # Ejecutar código Python
  tryCatch({
    py_run_string(codigo_python_seguro)
    cat("✅ Gráfica generada exitosamente\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error generando gráfica:", e$message, "\n")
    return(FALSE)
  })
}

# =============================================================================
# PASO 4: EJECUTAR CONFIGURACIÓN COMPLETA
# =============================================================================

ejecutar_configuracion_completa <- function() {
  cat("=== INICIANDO CONFIGURACIÓN COMPLETA ===\n")
  
  # Paso 1: Configurar Python
  if (!configurar_python_seguro()) {
    cat("❌ Falló la configuración de Python. Abortando.\n")
    return(FALSE)
  }
  
  # Paso 2: Configurar Matplotlib
  if (!configurar_matplotlib_seguro()) {
    cat("❌ Falló la configuración de Matplotlib. Abortando.\n")
    return(FALSE)
  }
  
  # Paso 3: Probar funcionalidad
  cat("\n=== PROBANDO FUNCIONALIDAD ===\n")
  
  # Datos de prueba
  paises_prueba <- c("Estados Unidos", "China", "Alemania", "Reino Unido")
  datos_prueba <- c(1500, 1200, 800, 600)
  colores_prueba <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")
  
  if (generar_grafica_python_segura(paises_prueba, datos_prueba, colores_prueba, 
                                   "Tecnología", 2023)) {
    cat("✅ CONFIGURACIÓN COMPLETA EXITOSA\n")
    cat("✅ Puedes usar Python y Matplotlib sin problemas\n")
    return(TRUE)
  } else {
    cat("❌ Falló la prueba de funcionalidad\n")
    return(FALSE)
  }
}

# =============================================================================
# CÓDIGO CORREGIDO PARA TU PROYECTO
# =============================================================================

cat("=== CÓDIGO CORREGIDO PARA TU PROYECTO ===\n")
cat("Reemplaza tu configuración inicial con:\n\n")

codigo_proyecto_corregido <- '
# Librerías esenciales
library(exams)
library(ggplot2)
library(knitr)
library(reticulate)
library(testthat)

# CONFIGURACIÓN CORREGIDA DE PYTHON
python3_path <- Sys.which("python3")
if (python3_path != "") {
  use_python(python3_path, required = TRUE)
} else {
  use_python("/usr/bin/python3", required = TRUE)
}

# CONFIGURACIÓN SEGURA DE MATPLOTLIB
py_run_string("
import matplotlib
matplotlib.use(\\"Agg\\")  # Sin parámetros adicionales
import matplotlib.pyplot as plt
import numpy as np
")

# Ahora puedes usar tu código Python sin errores
'

cat(codigo_proyecto_corregido)

# =============================================================================
# EJECUTAR CONFIGURACIÓN
# =============================================================================

# Ejecutar la configuración completa
resultado <- ejecutar_configuracion_completa()

if (resultado) {
  cat("\n🎉 ¡CONFIGURACIÓN EXITOSA! 🎉\n")
  cat("Ya puedes usar reticulate y matplotlib sin errores.\n")
} else {
  cat("\n❌ Hubo problemas en la configuración.\n")
  cat("Revisa los mensajes de error anteriores.\n")
}