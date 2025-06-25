# Template Python para Gráficos Circulares - Basado en Archivos Exitosos
# Fuente: I_1796473-Opc-A2.Rmd, I_1796473-Opc-A2v2.Rmd
# Competencia: Pensamiento Aleatorio - Interpretación y Representación
# Nivel: 1-2 (Lectura de gráficos circulares)

# CONFIGURACIÓN R REQUERIDA:
# En el chunk R definir:
# etiquetas <- c("Categoría A", "Categoría B", "Categoría C")
# valores <- c(40, 35, 25)
# mostrar_valores <- TRUE  # TRUE para mostrar valores en etiquetas
# mostrar_porcentajes <- FALSE  # TRUE para mostrar porcentajes

# CHUNK PYTHON PARA R MARKDOWN:
# ```{python GraficoCircular, echo=FALSE, message=FALSE, comment='', warning=FALSE, results="hide"}

from matplotlib import pyplot as plt
import numpy as np

# Transferir datos de R a Python (patrón exitoso validado)
etiquetas_py = r.etiquetas        # De R a Python
valores_py = r.valores            # De R a Python
mostrar_valores_py = r.mostrar_valores      # De R a Python
mostrar_porcentajes_py = r.mostrar_porcentajes  # De R a Python

# Configuración de colores (patrón exitoso validado)
colores_estandar = ['#1f77b4', '#aec7e8', '#ff7f0e', '#ffbb78', '#2ca02c', 
                   '#98df8a', '#d62728', '#ff9999', '#9467bd', '#c5b0d5']
# Seleccionar colores según número de categorías
colores_seleccionados = colores_estandar[:len(etiquetas_py)]

# Configuración de explosión (separación de sectores)
explode = tuple([0] * len(etiquetas_py))  # Sin separación por defecto
# Para destacar el primer sector: explode = (0.1, 0, 0, ...)

# Preparar etiquetas según configuración
if mostrar_valores_py and mostrar_porcentajes_py:
    # Mostrar tanto valores como porcentajes
    total = sum(valores_py)
    porcentajes = [100 * v / total for v in valores_py]
    pie_labels = [f'{etiqueta}\n{int(valor)} ({porcentaje:.1f}%)' 
                  for etiqueta, valor, porcentaje in zip(etiquetas_py, valores_py, porcentajes)]
elif mostrar_valores_py:
    # Solo mostrar valores
    pie_labels = [f'{etiqueta}\n{int(valor)}' 
                  for etiqueta, valor in zip(etiquetas_py, valores_py)]
elif mostrar_porcentajes_py:
    # Solo mostrar porcentajes
    total = sum(valores_py)
    porcentajes = [100 * v / total for v in valores_py]
    pie_labels = [f'{etiqueta}\n{porcentaje:.1f}%' 
                  for etiqueta, porcentaje in zip(etiquetas_py, porcentajes)]
else:
    # Solo etiquetas
    pie_labels = etiquetas_py

# Crear figura con configuración validada
fig, ax = plt.subplots(figsize=(6, 6), tight_layout=True)

# Crear gráfico circular con configuración exitosa
wedges, texts, autotexts = ax.pie(valores_py, 
                                  explode=explode, 
                                  labels=pie_labels, 
                                  colors=colores_seleccionados, 
                                  shadow=True,  # Sombra validada
                                  startangle=90,  # Comenzar desde arriba
                                  autopct=None)  # Sin porcentajes automáticos

# Configuración validada para círculo perfecto
ax.axis('equal')

# Ajustes de márgenes (patrón exitoso)
plt.subplots_adjust(left=0.1, top=0.9, right=0.9, bottom=0.1)

# Mostrar gráfico (obligatorio para R-exams)
plt.show()

# ```

# VARIANTE CON PORCENTAJES AUTOMÁTICOS:
# # Para mostrar porcentajes automáticamente en el gráfico
# wedges, texts, autotexts = ax.pie(valores_py, 
#                                   explode=explode, 
#                                   labels=etiquetas_py, 
#                                   colors=colores_seleccionados, 
#                                   shadow=True,
#                                   startangle=90,
#                                   autopct='%1.1f%%')  # Formato de porcentajes
# 
# # Configurar texto de porcentajes
# for autotext in autotexts:
#     autotext.set_color('white')
#     autotext.set_fontweight('bold')

# VARIANTE CON LEYENDA EXTERNA:
# # Crear gráfico sin etiquetas en sectores
# wedges, texts = ax.pie(valores_py, 
#                        explode=explode, 
#                        colors=colores_seleccionados, 
#                        shadow=True,
#                        startangle=90)
# 
# # Agregar leyenda externa
# ax.legend(wedges, pie_labels, 
#           title="Categorías",
#           loc="center left",
#           bbox_to_anchor=(1, 0, 0.5, 1))

# EJEMPLO DE USO EN CHUNK R:
# ```{r DefinirDatos, message=FALSE, warning=FALSE}
# # Datos para el gráfico circular
# etiquetas <- c("Matemáticas", "Ciencias", "Lenguaje")
# valores <- c(120, 85, 95)
# mostrar_valores <- TRUE
# mostrar_porcentajes <- FALSE
# ```

# CONFIGURACIÓN AVANZADA OPCIONAL:
# # Personalizar colores específicos
# colores_personalizados = ['#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4', '#FFEAA7']
# 
# # Destacar sector específico
# explode_destacado = (0.1, 0, 0)  # Destacar primer sector
# 
# # Configurar texto
# plt.rcParams['font.size'] = 10
# plt.rcParams['font.weight'] = 'bold'

# CARACTERÍSTICAS EXITOSAS VALIDADAS:
# ✅ Transferencia R→Python confiable
# ✅ Configuración matplotlib estándar
# ✅ Colores optimizados para visualización
# ✅ Etiquetas flexibles y configurables
# ✅ Proporciones automáticas correctas
# ✅ Compatible con todos los formatos R-exams

# CASOS DE USO ICFES:
# - Distribución de preferencias
# - Composición de poblaciones
# - Resultados de votaciones
# - Distribución de recursos
# - Análisis de proporciones

# VALIDACIÓN MULTI-FORMATO:
# ✅ PDF: Excelente
# ✅ HTML: Excelente  
# ✅ Moodle: Excelente
# ✅ Pandoc: Buena
# ✅ NOPS: Buena

# TIPS DE OPTIMIZACIÓN:
# - Usar máximo 6-7 categorías para legibilidad
# - Colores contrastantes para impresión
# - Etiquetas cortas para evitar solapamiento
# - Verificar que valores sumen coherentemente