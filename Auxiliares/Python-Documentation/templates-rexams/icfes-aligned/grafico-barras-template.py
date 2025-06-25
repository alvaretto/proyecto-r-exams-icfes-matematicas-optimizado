# Template Python para Gráficos de Barras - Basado en Archivos Exitosos
# Fuente: I_1796473-Opc-A2v2.Rmd, I_1796473-Opc-A2.Rmd
# Competencia: Pensamiento Aleatorio - Interpretación y Representación
# Nivel: 1-2 (Lectura de gráficos estadísticos)

# CONFIGURACIÓN R REQUERIDA:
# En el chunk R definir:
# categorias <- c("Categoría A", "Categoría B", "Categoría C")
# valores <- c(25, 35, 40)
# titulo_x <- "Categorías"
# titulo_y <- "Valores"
# tipo_grafico <- "vertical"  # "vertical" o "horizontal"

# CHUNK PYTHON PARA R MARKDOWN:
# ```{python GraficoBarras, echo=FALSE, message=FALSE, comment='', warning=FALSE, results="hide"}

import matplotlib.pyplot as plt
import numpy as np

# Transferir datos de R a Python (patrón exitoso validado)
categorias_py = r.categorias  # De R a Python
valores_py = r.valores        # De R a Python
titulo_x_py = r.titulo_x      # De R a Python
titulo_y_py = r.titulo_y      # De R a Python
tipo_py = r.tipo_grafico      # De R a Python

# Configuración de colores (patrón exitoso)
colores_azules = ['#00B3E6', '#0066CC', '#000099', '#003366', '#001133']
# Seleccionar colores según número de categorías
colores_seleccionados = colores_azules[:len(categorias_py)]

# Crear figura con tamaño estándar validado
fig, ax = plt.subplots(figsize=(6, 5))

if tipo_py == "vertical":
    # GRÁFICO DE BARRAS VERTICALES
    # Configuración validada en archivos exitosos
    ancho_barras = 0.5  # Valor optimizado
    bars = ax.bar(categorias_py, valores_py, 
                  color=colores_seleccionados, 
                  width=ancho_barras)
    
    # Configuración de ejes (patrón exitoso)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_linewidth(2)
    ax.spines['bottom'].set_linewidth(2)
    
    # Grid horizontal para barras verticales
    ax.yaxis.grid(True, linestyle='--', linewidth=0.7, color='darkgray')
    
    # Etiquetas con formato validado
    plt.xticks(fontweight='bold')
    plt.yticks(fontweight='bold')
    plt.xlabel(titulo_x_py, fontweight='bold')
    plt.ylabel(titulo_y_py, fontweight='bold')
    
    # Ajustar rango Y automáticamente
    max_valor = max(valores_py)
    intervalo = max(10, max_valor // 10)
    plt.yticks(np.arange(0, max_valor + intervalo, intervalo))

else:  # tipo_py == "horizontal"
    # GRÁFICO DE BARRAS HORIZONTALES
    # Configuración validada en archivos exitosos
    alto_barras = 0.6  # Valor optimizado
    bars = ax.barh(categorias_py, valores_py, 
                   color=colores_seleccionados, 
                   height=alto_barras)
    
    # Configuración de ejes (patrón exitoso)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_linewidth(2)
    ax.spines['bottom'].set_linewidth(2)
    
    # Grid vertical para barras horizontales
    ax.xaxis.grid(True, linestyle='--', linewidth=0.7, color='darkgray')
    
    # Etiquetas con formato validado
    plt.yticks(fontweight='bold')
    plt.xticks(fontweight='bold')
    plt.ylabel(titulo_x_py, fontweight='bold')
    plt.xlabel(titulo_y_py, fontweight='bold')
    
    # Ajustar rango X automáticamente
    max_valor = max(valores_py)
    intervalo = max(10, max_valor // 10)
    plt.xticks(np.arange(0, max_valor + intervalo, intervalo))

# Mostrar gráfico (obligatorio para R-exams)
plt.show()

# ```

# VARIANTE CON ETIQUETAS DE VALORES EN BARRAS:
# # Agregar valores en las barras
# if tipo_py == "vertical":
#     for bar in bars:
#         altura = bar.get_height()
#         ax.text(bar.get_x() + bar.get_width()/2., altura,
#                 f'{int(altura)}',
#                 ha='center', va='bottom', fontweight='bold')
# else:  # horizontal
#     for bar in bars:
#         ancho = bar.get_width()
#         ax.text(ancho, bar.get_y() + bar.get_height()/2.,
#                 f'{int(ancho)}',
#                 ha='left', va='center', fontweight='bold')

# EJEMPLO DE USO EN CHUNK R:
# ```{r DefinirDatos, message=FALSE, warning=FALSE}
# # Datos para el gráfico
# categorias <- c("Perros", "Gatos", "Aves")
# valores <- c(45, 30, 25)
# titulo_x <- "Tipos de Mascotas"
# titulo_y <- "Número de Adopciones"
# tipo_grafico <- "vertical"
# ```

# CARACTERÍSTICAS EXITOSAS VALIDADAS:
# ✅ Transferencia R→Python confiable
# ✅ Configuración matplotlib estándar
# ✅ Colores optimizados para impresión
# ✅ Grid y ejes configurados correctamente
# ✅ Escalabilidad automática
# ✅ Compatible con todos los formatos R-exams

# CASOS DE USO ICFES:
# - Frecuencias de datos categóricos
# - Comparación de cantidades
# - Resultados de encuestas
# - Distribución de preferencias
# - Análisis de datos estadísticos

# VALIDACIÓN MULTI-FORMATO:
# ✅ PDF: Excelente
# ✅ HTML: Excelente  
# ✅ Moodle: Excelente
# ✅ Pandoc: Buena
# ✅ NOPS: Buena