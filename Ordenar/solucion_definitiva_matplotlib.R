# =============================================================================
# SOLUCIÓN DEFINITIVA PARA EL ERROR DE MATPLOTLIB EN RETICULATE
# =============================================================================
# Este script soluciona el error persistente:
# "TypeError: use() got an unexpected keyword argument 'warn'"

# Librerías esenciales
library(exams)
library(ggplot2)
library(knitr)
library(reticulate)
library(testthat)

# =============================================================================
# PASO 1: CONFIGURAR PYTHON
# =============================================================================

cat("=== CONFIGURANDO PYTHON ===\n")
python3_path <- Sys.which("python3")
if (python3_path != "") {
  use_python(python3_path, required = TRUE)
  cat("✅ Python configurado:", python3_path, "\n")
} else {
  use_python("/usr/bin/python3", required = TRUE)
  cat("✅ Python configurado: /usr/bin/python3\n")
}

# =============================================================================
# PASO 2: DESHABILITAR EL HOOK AUTOMÁTICO DE MATPLOTLIB
# =============================================================================

cat("\n=== DESHABILITANDO HOOK AUTOMÁTICO DE MATPLOTLIB ===\n")

# Deshabilitar el hook automático que causa el error
options(reticulate.matplotlib.backend = NULL)

# Configurar variable de entorno para evitar el hook
Sys.setenv(MPLBACKEND = "Agg")

cat("✅ Hook automático deshabilitado\n")

# =============================================================================
# PASO 3: CONFIGURAR MATPLOTLIB MANUALMENTE Y DE FORMA SEGURA
# =============================================================================

cat("\n=== CONFIGURANDO MATPLOTLIB MANUALMENTE ===\n")

# Configurar matplotlib ANTES de cualquier importación automática
py_run_string("
import os
import sys

# Configurar backend antes de importar matplotlib
os.environ['MPLBACKEND'] = 'Agg'

# Importar matplotlib con configuración manual
import matplotlib
matplotlib.use('Agg', force=True)  # Forzar el backend sin parámetros problemáticos

# Ahora importar pyplot
import matplotlib.pyplot as plt
import numpy as np

print('✅ Matplotlib configurado manualmente sin errores')
print(f'Backend actual: {matplotlib.get_backend()}')
print(f'Versión de matplotlib: {matplotlib.__version__}')
")

cat("✅ Matplotlib configurado manualmente\n")

# =============================================================================
# PASO 4: FUNCIÓN PARA GENERAR GRÁFICAS SIN ERRORES
# =============================================================================

generar_grafica_segura <- function(paises_destino, datos_ultimo_año, 
                                  colores_graficas, sector_industrial, año_mostrar) {
  
  cat("\n=== GENERANDO GRÁFICA ===\n")
  
  # Código Python que NO activa el hook problemático
  codigo_python_seguro <- sprintf("
# matplotlib ya está configurado arriba, no se reimporta
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

# Rotar etiquetas del eje x
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
plt.savefig('grafica_exportaciones_segura.png', dpi=300, bbox_inches='tight', 
            facecolor='white', edgecolor='none')

print('✅ Gráfica generada exitosamente: grafica_exportaciones_segura.png')

# Limpiar la figura
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
    cat("✅ Gráfica generada sin errores\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Error:", e$message, "\n")
    return(FALSE)
  })
}

# =============================================================================
# PASO 5: PROBAR LA SOLUCIÓN
# =============================================================================

cat("\n=== PROBANDO LA SOLUCIÓN ===\n")

# Datos de prueba
paises_prueba <- c("Estados Unidos", "China", "Alemania", "Reino Unido", "Francia")
datos_prueba <- c(1500, 1200, 800, 600, 450)
colores_prueba <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57")

# Probar la función
resultado <- generar_grafica_segura(
  paises_destino = paises_prueba,
  datos_ultimo_año = datos_prueba,
  colores_graficas = colores_prueba,
  sector_industrial = "Tecnología",
  año_mostrar = 2023
)

if (resultado) {
  cat("\n🎉 ¡SOLUCIÓN EXITOSA! 🎉\n")
  cat("La configuración funciona correctamente.\n")
} else {
  cat("\n❌ La solución necesita ajustes adicionales.\n")
}

# =============================================================================
# CÓDIGO FINAL PARA TU PROYECTO
# =============================================================================

cat("\n=== CÓDIGO PARA TU PROYECTO ===\n")
cat("Usa este código al inicio de tu archivo .Rmd:\n\n")

cat('```{r setup-matplotlib-seguro, include=FALSE}\n')
cat('# Librerías esenciales\n')
cat('library(exams)\n')
cat('library(ggplot2)\n')
cat('library(knitr)\n')
cat('library(reticulate)\n')
cat('library(testthat)\n')
cat('\n')
cat('# Configurar Python\n')
cat('python3_path <- Sys.which("python3")\n')
cat('if (python3_path != "") {\n')
cat('  use_python(python3_path, required = TRUE)\n')
cat('} else {\n')
cat('  use_python("/usr/bin/python3", required = TRUE)\n')
cat('}\n')
cat('\n')
cat('# DESHABILITAR HOOK AUTOMÁTICO (CLAVE PARA EVITAR EL ERROR)\n')
cat('options(reticulate.matplotlib.backend = NULL)\n')
cat('Sys.setenv(MPLBACKEND = "Agg")\n')
cat('\n')
cat('# Configurar matplotlib manualmente\n')
cat('py_run_string("\n')
cat('import os\n')
cat('os.environ[\'MPLBACKEND\'] = \'Agg\'\n')
cat('import matplotlib\n')
cat('matplotlib.use(\'Agg\', force=True)\n')
cat('import matplotlib.pyplot as plt\n')
cat('import numpy as np\n')
cat('")\n')
cat('```\n\n')

cat('Después usa tu código Python normal sin problemas.\n')