#!/usr/bin/env python3
"""
Gráfica de poblaciones de 5 países (1960-2013)
Generado por Graficador-Experto
"""

import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

# Configuración de estilo
plt.rcParams['font.size'] = 10
plt.rcParams['axes.grid'] = True
plt.rcParams['grid.alpha'] = 0.3
plt.rcParams['grid.color'] = '#CCCCCC'

# Datos de los países (años y poblaciones)
anos = [1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013]

# País 1 - Crecimiento desde ~19M a ~41M
pais1 = [19e6, 21e6, 23.5e6, 26e6, 28.5e6, 31e6, 33.5e6, 36e6, 38e6, 39.5e6, 40.5e6, 41e6]

# País 2 - Crecimiento desde ~22M a ~38M
pais2 = [22e6, 24e6, 26.5e6, 29e6, 30e6, 31.5e6, 33e6, 35e6, 36.5e6, 37e6, 37.5e6, 38e6]

# País 3 - Crecimiento desde ~20M a ~38M
pais3 = [20e6, 22e6, 24e6, 26e6, 28e6, 30e6, 32e6, 34e6, 35.5e6, 36.5e6, 37.5e6, 38e6]

# País 4 - Mayor crecimiento desde ~31M a ~47M
pais4 = [31e6, 33e6, 35e6, 37e6, 38.5e6, 40e6, 41.5e6, 43e6, 44.5e6, 45.5e6, 46.5e6, 47e6]

# País 5 - Relativamente estable desde ~30M a ~38M
pais5 = [30e6, 30.5e6, 31e6, 31.5e6, 32e6, 33e6, 34.5e6, 36e6, 37e6, 37.5e6, 38e6, 38e6]

# Crear figura
fig, ax = plt.subplots(figsize=(10, 6))

# Colores según paleta identificada
color_cyan = '#00CED1'
color_negro = '#000000'
color_marron = '#8B4513'
color_naranja = '#FF8C00'

# Graficar cada país con su estilo específico
# País 1 - Línea punteada cyan (sin marcadores)
ax.plot(anos, pais1, color=color_cyan, linestyle=':', linewidth=2,
        label='País 1')

# País 2 - Línea discontinua negra con marcadores cuadrados
ax.plot(anos, pais2, color=color_negro, linestyle='--', linewidth=2,
        marker='s', markersize=6, markevery=2, label='País 2')

# País 3 - Línea sólida marrón (sin marcadores)
ax.plot(anos, pais3, color=color_marron, linestyle='-', linewidth=2,
        label='País 3')

# País 4 - Línea sólida cyan con marcadores triangulares
ax.plot(anos, pais4, color=color_cyan, linestyle='-', linewidth=2,
        marker='^', markersize=7, markevery=2, label='País 4')

# País 5 - Línea sólida naranja con marcadores circulares
ax.plot(anos, pais5, color=color_naranja, linestyle='-', linewidth=2,
        marker='o', markersize=6, markevery=2, label='País 5')

# Configurar ejes
ax.set_xlabel('Año', fontsize=11)
ax.set_ylabel('Población', fontsize=11)
ax.set_xlim(1960, 2013)
ax.set_ylim(15e6, 46e6)

# Ticks del eje X
ax.set_xticks(anos)
ax.set_xticklabels(anos, rotation=45, ha='right')

# Ticks del eje Y con formato de millones
def formato_millones(x, pos):
    return f'{x/1e6:.0f}.000.000'

ax.yaxis.set_major_formatter(ticker.FuncFormatter(formato_millones))
ax.set_yticks([15e6, 20e6, 25e6, 30e6, 35e6, 40e6, 45e6])

# Leyenda
ax.legend(loc='upper left', bbox_to_anchor=(1.02, 1), frameon=False)

# Ajustar layout
plt.tight_layout()

# Guardar figura
plt.savefig('output_python.png', dpi=150, bbox_inches='tight',
            facecolor='white', edgecolor='none')
plt.savefig('output_python.pdf', bbox_inches='tight',
            facecolor='white', edgecolor='none')

print("Gráfica guardada en output_python.png y output_python.pdf")
