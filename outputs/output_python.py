#!/usr/bin/env python3
"""
Gráfico de Población por País (1960-2013)
Generado automáticamente para reproducir imagen ICFES
"""

import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

# Configuración de estilo
plt.rcParams['font.size'] = 10
plt.rcParams['axes.linewidth'] = 1.0

# Datos de los países
años_base = [1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013]

# País 1 - Cyan punteada, crecimiento lineal de 20M a 43M
pais1_años = [1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013]
pais1_pob = [20e6, 24e6, 27.5e6, 30e6, 32e6, 35e6, 37.5e6, 39e6, 40.5e6, 41.5e6, 42.5e6, 43e6]

# País 2 - Negra discontinua
pais2_años = [1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 1998, 2000, 2005, 2010, 2013]
pais2_pob = [22e6, 23.5e6, 25e6, 27e6, 29e6, 31.5e6, 34e6, 36e6, 37e6, 37.5e6, 38.5e6, 39.5e6, 40e6]

# País 3 - Naranja/marrón sólida, crecimiento continuo de 15M a 38M
pais3_años = [1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013]
pais3_pob = [15e6, 17e6, 19e6, 22e6, 25e6, 27e6, 29e6, 31e6, 33e6, 35e6, 37e6, 38e6]

# País 4 - Azul sólida con triángulos
pais4_años = [1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013]
pais4_pob = [30e6, 31.5e6, 33e6, 35e6, 37e6, 39e6, 41e6, 42.5e6, 44e6, 45e6, 46e6, 47e6]

# País 5 - Naranja sólida con círculos
pais5_años = [1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 1998, 2000, 2005, 2010, 2013]
pais5_pob = [30e6, 30.5e6, 31e6, 32e6, 33e6, 34e6, 35e6, 36e6, 37e6, 37.5e6, 38e6, 38e6, 38.5e6]

# Crear figura - proporciones más similares a TikZ (12cm x 8cm aprox)
fig, ax = plt.subplots(figsize=(9, 6))

# Colores según paleta identificada
color_pais1 = '#00BFFF'  # Cyan
color_pais2 = '#000000'  # Negro
color_pais3 = '#CC6600'  # Naranja/marrón
color_pais4 = '#0066CC'  # Azul
color_pais5 = '#FF9900'  # Naranja

# País 1 - Línea punteada cyan (densely dotted - más densa)
ax.plot(pais1_años, pais1_pob, color=color_pais1, linestyle=(0, (1, 1)), linewidth=2, label='País 1')

# País 2 - Línea discontinua negra (dashes más cortos)
ax.plot(pais2_años, pais2_pob, color=color_pais2, linestyle=(0, (5, 3)), linewidth=2, label='País 2')

# País 3 - Línea sólida naranja/marrón
ax.plot(pais3_años, pais3_pob, color=color_pais3, linestyle='-', linewidth=2, label='País 3')

# País 4 - Línea sólida azul con triángulos
ax.plot(pais4_años, pais4_pob, color=color_pais4, linestyle='-', linewidth=2,
        marker='^', markersize=6, markerfacecolor=color_pais4, label='País 4')

# País 5 - Línea sólida naranja con círculos
ax.plot(pais5_años, pais5_pob, color=color_pais5, linestyle='-', linewidth=2,
        marker='o', markersize=5, markerfacecolor=color_pais5, label='País 5')

# Configurar ejes
ax.set_xlabel('Año')
ax.set_ylabel('Población')
ax.set_xlim(1960, 2013)
ax.set_ylim(15e6, 47e6)

# Ticks del eje X - rotados 45 grados
ax.set_xticks([1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013])
ax.set_xticklabels([1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013],
                   rotation=45, ha='right')

# Ticks del eje Y - formato con separadores de miles
def format_poblacion(x, pos):
    return f'{int(x):,}'.replace(',', '.')

ax.yaxis.set_major_formatter(ticker.FuncFormatter(format_poblacion))
ax.set_yticks([15e6, 20e6, 25e6, 30e6, 35e6, 40e6, 45e6])

# Grilla
ax.grid(True, linestyle='-', linewidth=0.5, color='#CCCCCC', alpha=0.7)
ax.set_axisbelow(True)

# Leyenda fuera del gráfico a la derecha
ax.legend(loc='upper left', bbox_to_anchor=(1.02, 1), frameon=False)

# Ajustar layout para que no se corte la leyenda
plt.tight_layout()
plt.subplots_adjust(right=0.85)

# Guardar como PDF y PNG
plt.savefig('python_final.pdf', format='pdf', bbox_inches='tight', dpi=150)
plt.savefig('python_final.png', format='png', bbox_inches='tight', dpi=150)

print("Gráfico generado exitosamente:")
print("  - python_final.pdf")
print("  - python_final.png")
