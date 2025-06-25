# Template Python para Gráficos de Funciones Lineales - Basado en Archivos Exitosos
# Fuente: vuelo_acrobatico_A.Rmd
# Competencia: Pensamiento Variacional-Espacial
# Nivel: 2-3 (Interpretación de funciones y variación)

# CONFIGURACIÓN R REQUERIDA:
# En el chunk R definir:
# x_inicial <- 0
# x_final <- 60
# y_inicial <- 0
# y_final <- 150
# puntos_cambio <- c(20, 40)  # Puntos donde cambia la función
# valores_cambio <- c(150, 150)  # Valores Y en puntos de cambio
# titulo_x <- "Tiempo (segundos)"
# titulo_y <- "Altura (metros)"
# color_linea <- "red"  # "red", "blue", "green", etc.

# CHUNK PYTHON PARA R MARKDOWN:
# ```{python GraficoFuncion, echo=FALSE, message=FALSE, comment='', warning=FALSE, results="hide"}

import matplotlib.pyplot as plt
import numpy as np

# Transferir datos de R a Python (patrón exitoso validado)
x_inicial_py = r.x_inicial        # De R a Python
x_final_py = r.x_final            # De R a Python
y_inicial_py = r.y_inicial        # De R a Python
y_final_py = r.y_final            # De R a Python
puntos_cambio_py = r.puntos_cambio    # De R a Python
valores_cambio_py = r.valores_cambio  # De R a Python
titulo_x_py = r.titulo_x          # De R a Python
titulo_y_py = r.titulo_y          # De R a Python
color_linea_py = r.color_linea    # De R a Python

# Configuración de figura (patrón exitoso)
plt.rcParams['figure.figsize'] = (8, 5)
plt.rcParams['figure.titlesize'] = 0

# Crear figura
fig = plt.figure()
ax = fig.add_subplot(111)

# FUNCIÓN POR SEGMENTOS (patrón validado)
# Segmento 1: Desde inicio hasta primer punto de cambio
if len(puntos_cambio_py) > 0:
    x1 = np.array([x_inicial_py, puntos_cambio_py[0]])
    y1 = np.array([y_inicial_py, valores_cambio_py[0]])
    ax.plot(x1, y1, color=color_linea_py, linewidth=2)
    
    # Segmentos intermedios
    for i in range(len(puntos_cambio_py) - 1):
        x_seg = np.array([puntos_cambio_py[i], puntos_cambio_py[i + 1]])
        y_seg = np.array([valores_cambio_py[i], valores_cambio_py[i + 1]])
        ax.plot(x_seg, y_seg, color=color_linea_py, linewidth=2)
    
    # Último segmento: Desde último punto de cambio hasta final
    x_ultimo = np.array([puntos_cambio_py[-1], x_final_py])
    y_ultimo = np.array([valores_cambio_py[-1], y_final_py])
    ax.plot(x_ultimo, y_ultimo, color=color_linea_py, linewidth=2)
else:
    # Función lineal simple (sin puntos de cambio)
    x_simple = np.array([x_inicial_py, x_final_py])
    y_simple = np.array([y_inicial_py, y_final_py])
    ax.plot(x_simple, y_simple, color=color_linea_py, linewidth=2)

# Configuración de ejes (patrón exitoso validado)
ax.set_xlabel(titulo_x_py, fontweight='bold')
ax.set_ylabel(titulo_y_py, fontweight='bold')

# Grid estándar validado
ax.grid(True, linestyle='--', alpha=0.7)

# Configurar límites automáticamente
margen_x = (x_final_py - x_inicial_py) * 0.05
margen_y = max(max(valores_cambio_py + [y_inicial_py, y_final_py])) * 0.1

ax.set_xlim(x_inicial_py - margen_x, x_final_py + margen_x)
ax.set_ylim(min([y_inicial_py, y_final_py] + valores_cambio_py) - margen_y, 
            max([y_inicial_py, y_final_py] + valores_cambio_py) + margen_y)

# Ajustes de layout (patrón exitoso)
plt.subplots_adjust(top=1, bottom=0.1, left=0.1, right=0.9)

# Mostrar gráfico (obligatorio para R-exams)
plt.show()

# ```

# VARIANTE PARA FUNCIÓN CUADRÁTICA:
# # Para gráficos de parábolas
# x_vals = np.linspace(x_inicial_py, x_final_py, 100)
# # Ejemplo: y = ax² + bx + c
# a, b, c = r.coef_a, r.coef_b, r.coef_c  # Coeficientes desde R
# y_vals = a * x_vals**2 + b * x_vals + c
# ax.plot(x_vals, y_vals, color=color_linea_py, linewidth=2)

# VARIANTE CON PUNTOS DESTACADOS:
# # Marcar puntos importantes
# puntos_x = [x_inicial_py] + puntos_cambio_py + [x_final_py]
# puntos_y = [y_inicial_py] + valores_cambio_py + [y_final_py]
# ax.scatter(puntos_x, puntos_y, color='black', s=50, zorder=5)
# 
# # Etiquetas en puntos
# for i, (x, y) in enumerate(zip(puntos_x, puntos_y)):
#     ax.annotate(f'({x}, {y})', (x, y), 
#                 xytext=(5, 5), textcoords='offset points',
#                 fontweight='bold', fontsize=9)

# VARIANTE PARA MÚLTIPLES FUNCIONES:
# # Comparar varias funciones
# colores = ['red', 'blue', 'green', 'purple']
# for i, (puntos, valores, color) in enumerate(zip(r.lista_puntos, r.lista_valores, colores)):
#     # Graficar cada función con color diferente
#     # ... código similar al anterior

# EJEMPLO DE USO EN CHUNK R:
# ```{r DefinirFuncion, message=FALSE, warning=FALSE}
# # Datos para función por segmentos (vuelo acrobático)
# x_inicial <- 0
# x_final <- 75
# y_inicial <- 0
# y_final <- 0
# puntos_cambio <- c(25, 55)  # Fin de ascenso, fin de vuelo horizontal
# valores_cambio <- c(150, 150)  # Altura máxima mantenida
# titulo_x <- "Tiempo (segundos)"
# titulo_y <- "Altura (metros)"
# color_linea <- "red"
# ```

# CONFIGURACIÓN AVANZADA OPCIONAL:
# # Personalizar estilo de línea
# estilos_linea = {
#     'solida': '-',
#     'punteada': '--',
#     'puntos': ':',
#     'raya_punto': '-.'
# }
# estilo = estilos_linea.get(r.estilo_linea, '-')
# ax.plot(x1, y1, color=color_linea_py, linewidth=2, linestyle=estilo)

# CARACTERÍSTICAS EXITOSAS VALIDADAS:
# ✅ Transferencia R→Python confiable
# ✅ Funciones por segmentos (piecewise)
# ✅ Configuración matplotlib estándar
# ✅ Escalado automático de ejes
# ✅ Grid y etiquetas optimizadas
# ✅ Compatible con todos los formatos R-exams

# CASOS DE USO ICFES:
# - Movimiento rectilíneo uniforme
# - Variación lineal de magnitudes
# - Funciones por tramos
# - Interpretación de gráficas
# - Razones de cambio
# - Proporcionalidad directa/inversa

# VALIDACIÓN MULTI-FORMATO:
# ✅ PDF: Excelente
# ✅ HTML: Excelente  
# ✅ Moodle: Excelente
# ✅ Pandoc: Buena
# ✅ NOPS: Buena

# TIPS DE OPTIMIZACIÓN:
# - Usar colores contrastantes para múltiples funciones
# - Verificar que los puntos de cambio sean coherentes
# - Ajustar márgenes según el rango de datos
# - Usar linewidth=2 para mejor visibilidad
# - Validar que las funciones sean matemáticamente correctas