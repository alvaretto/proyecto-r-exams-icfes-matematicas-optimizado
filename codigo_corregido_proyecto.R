# =============================================================================
# CÓDIGO CORREGIDO PARA TU PROYECTO R-EXAMS
# =============================================================================
# Este código reemplaza la configuración problemática original y soluciona:
# 1. Error: "Specified version of python '' does not exist"
# 2. Error: "TypeError: use() got an unexpected keyword argument 'warn'"

# =============================================================================
# LIBRERÍAS ESENCIALES (CONFIGURACIÓN CORREGIDA)
# =============================================================================

library(exams)
library(ggplot2)
library(knitr)
library(reticulate)
library(testthat)

# =============================================================================
# CONFIGURACIÓN CORREGIDA DE PYTHON
# =============================================================================

# ANTES (PROBLEMÁTICO):
# use_python(Sys.which("python"), required = TRUE)  # ❌ Error: python no existe

# DESPUÉS (CORREGIDO):
python3_path <- Sys.which("python3")
if (python3_path != "") {
  use_python(python3_path, required = TRUE)
  cat("✅ Python configurado correctamente:", python3_path, "\n")
} else {
  # Fallback para sistemas donde python3 no está en PATH
  use_python("/usr/bin/python3", required = TRUE)
  cat("✅ Python configurado usando ruta manual: /usr/bin/python3\n")
}

# =============================================================================
# CONFIGURACIÓN SEGURA DE MATPLOTLIB
# =============================================================================

# Configurar matplotlib ANTES de usarlo para evitar errores de backend
py_run_string("
import matplotlib
matplotlib.use('Agg')  # Backend no interactivo, sin parámetros problemáticos
import matplotlib.pyplot as plt
import numpy as np
print('✅ Matplotlib configurado correctamente')
")

# =============================================================================
# FUNCIÓN CORREGIDA PARA GENERAR GRÁFICAS
# =============================================================================

generar_grafica_exportaciones <- function(paises_destino, datos_ultimo_año, 
                                         colores_graficas, sector_industrial, año_mostrar) {
  
  # Código Python corregido (sin errores de matplotlib)
  codigo_python_corregido <- sprintf("
# Matplotlib ya está configurado correctamente arriba
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

print('✅ Gráfica generada exitosamente')
plt.close()  # Limpiar memoria
",
    paste0("['", paste(paises_destino, collapse="', '"), "']"),
    paste0("[", paste(datos_ultimo_año, collapse=", "), "]"),
    paste0("['", paste(colores_graficas, collapse="', '"), "']"),
    sector_industrial,
    año_mostrar
  )
  
  # Ejecutar código Python
  py_run_string(codigo_python_corregido)
}

# =============================================================================
# EJEMPLO DE USO CORREGIDO
# =============================================================================

cat("\n=== EJEMPLO DE USO ===\n")

# Datos de ejemplo
paises_ejemplo <- c("Estados Unidos", "China", "Alemania", "Reino Unido", "Francia")
datos_ejemplo <- c(1500, 1200, 800, 600, 450)
colores_ejemplo <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57")

# Generar gráfica de ejemplo
generar_grafica_exportaciones(
  paises_destino = paises_ejemplo,
  datos_ultimo_año = datos_ejemplo,
  colores_graficas = colores_ejemplo,
  sector_industrial = "Tecnología",
  año_mostrar = 2023
)

# =============================================================================
# CÓDIGO PARA INTEGRAR EN TU ARCHIVO .Rmd
# =============================================================================

cat("\n=== CÓDIGO PARA TU ARCHIVO .Rmd ===\n")
cat("Reemplaza el bloque problemático con este código:\n\n")

# Mostrar código para .Rmd sin ejecutarlo (para evitar errores de escape)
cat('```{r setup-python, include=FALSE}\n')
cat('# Librerías esenciales\n')
cat('library(exams)\n')
cat('library(ggplot2)\n')
cat('library(knitr)\n')
cat('library(reticulate)\n')
cat('library(testthat)\n')
cat('\n')
cat('# Configurar Python (VERSIÓN CORREGIDA)\n')
cat('python3_path <- Sys.which("python3")\n')
cat('if (python3_path != "") {\n')
cat('  use_python(python3_path, required = TRUE)\n')
cat('} else {\n')
cat('  use_python("/usr/bin/python3", required = TRUE)\n')
cat('}\n')
cat('\n')
cat('# Configurar matplotlib de forma segura\n')
cat('py_run_string("\n')
cat('import matplotlib\n')
cat('matplotlib.use(\'Agg\')\n')
cat('import matplotlib.pyplot as plt\n')
cat('import numpy as np\n')
cat('")\n')
cat('```\n\n')

cat('```{r generar-grafica, echo=FALSE, results="asis"}\n')
cat('# Tu código de generación de datos aquí...\n')
cat('# paises_destino <- c(...)\n')
cat('# datos_ultimo_año <- c(...)\n')
cat('# colores_graficas <- c(...)\n')
cat('\n')
cat('# Código Python corregido\n')
cat('codigo_python_opcion_a <- sprintf("\n')
cat('import matplotlib.pyplot as plt\n')
cat('import numpy as np\n')
cat('\n')
cat('paises = %s\n')
cat('valores = %s\n')
cat('colores = %s\n')
cat('sector = \'%s\'\n')
cat('año = %s\n')
cat('\n')
cat('fig, ax = plt.subplots(figsize=(12, 8))\n')
cat('barras = ax.bar(paises, valores, color=colores, alpha=0.8, edgecolor=\'black\', linewidth=1)\n')
cat('ax.set_title(f\'Exportaciones de {sector} en {año}\', fontsize=16, fontweight=\'bold\', pad=20)\n')
cat('ax.set_xlabel(\'Países de Destino\', fontsize=12, fontweight=\'bold\')\n')
cat('ax.set_ylabel(\'Valor de Exportaciones (Millones USD)\', fontsize=12, fontweight=\'bold\')\n')
cat('plt.xticks(rotation=45, ha=\'right\')\n')
cat('\n')
cat('for i, (barra, valor) in enumerate(zip(barras, valores)):\n')
cat('    height = barra.get_height()\n')
cat('    ax.text(barra.get_x() + barra.get_width()/2., height + max(valores)*0.01,\n')
cat('            f\'{valor:,.0f}\', ha=\'center\', va=\'bottom\', fontweight=\'bold\')\n')
cat('\n')
cat('ax.grid(True, alpha=0.3, axis=\'y\')\n')
cat('ax.set_axisbelow(True)\n')
cat('plt.tight_layout()\n')
cat('plt.savefig(\'grafica_exportaciones.png\', dpi=300, bbox_inches=\'tight\')\n')
cat('plt.close()\n')
cat('",\n')
cat('  paste0("[\'", paste(paises_destino, collapse="\', \'"), "\']"),\n')
cat('  paste0("[", paste(datos_ultimo_año, collapse=", "), "]"),\n')
cat('  paste0("[\'", paste(colores_graficas, collapse="\', \'"), "\']"),\n')
cat('  sector_industrial,\n')
cat('  año_mostrar\n')
cat(')\n')
cat('\n')
cat('# Ejecutar código Python (AHORA SIN ERRORES)\n')
cat('py_run_string(codigo_python_opcion_a)\n')
cat('```\n')

cat("\n✅ CONFIGURACIÓN COMPLETA\n")
cat("✅ Python configurado correctamente\n")
cat("✅ Matplotlib funcionando sin errores\n")
cat("✅ Gráfica de ejemplo generada exitosamente\n")
cat("\nAhora puedes usar este código en tu proyecto sin problemas.\n")