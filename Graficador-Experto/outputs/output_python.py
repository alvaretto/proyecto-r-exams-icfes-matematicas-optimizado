#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Gráfico de Poblaciones de 5 Países (1960-2013)
Generado por Graficador-Experto - Compatible con R-Markdown via reticulate
"""

import matplotlib.pyplot as plt
import numpy as np

# Datos
años = np.array([1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013])

# Población de cada país (valores del gráfico original)
pais1 = np.array([30, 31.5, 33, 34.5, 36, 37.5, 38.5, 39.5, 40.5, 41.5, 42, 42.5]) * 1e6
pais2 = np.array([22, 23.5, 25, 27, 29, 30.5, 32, 34, 35.5, 36.5, 37.5, 38]) * 1e6
pais3 = np.array([30, 30.5, 31, 31.5, 32, 33, 34, 35, 36, 37, 37.5, 38]) * 1e6
pais4 = np.array([30.5, 32, 33.5, 35, 37, 38.5, 40, 42, 44, 45.5, 46.5, 47]) * 1e6
pais5 = np.array([30, 29.5, 29, 28.5, 29, 30, 31.5, 33, 35, 36.5, 37.5, 38]) * 1e6

# Crear figura
fig, ax = plt.subplots(figsize=(10, 7))

# Graficar cada país con su estilo exacto del original
ax.plot(años, pais1, color='#00CED1', linestyle=':', linewidth=2, label='País 1')  # Celeste punteada
ax.plot(años, pais2, color='black', linestyle='--', linewidth=2, label='País 2')  # Negra discontinua
ax.plot(años, pais3, color='#CD853F', linestyle='-', linewidth=2, label='País 3')  # Marrón/Siena sólida
ax.plot(años, pais4, color='#00CED1', linestyle='-', linewidth=2, 
        marker='^', markersize=8, markerfacecolor='#00CED1', label='País 4')  # Celeste con triángulos
ax.plot(años, pais5, color='#FF8C00', linestyle='-', linewidth=2, 
        marker='o', markersize=6, markerfacecolor='#FF8C00', label='País 5')  # Naranja con círculos

# Configurar ejes
ax.set_xlabel('Año', fontsize=12, fontweight='bold')
ax.set_ylabel('Población', fontsize=12, fontweight='bold')
ax.set_xlim(1960, 2013)
ax.set_ylim(15e6, 48e6)

# Formato del eje Y
def formato_poblacion(x, pos):
    return f'{int(x/1e6)}.000.000'

ax.yaxis.set_major_formatter(plt.FuncFormatter(formato_poblacion))
ax.set_yticks([15e6, 20e6, 25e6, 30e6, 35e6, 40e6, 45e6])
ax.set_xticks([1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013])

# Inclinar etiquetas del eje X
plt.xticks(rotation=45, ha='right')

# Cuadrícula
ax.grid(True, linestyle='-', alpha=0.4, color='gray')
ax.set_axisbelow(True)

# Leyenda
ax.legend(loc='upper left', bbox_to_anchor=(1.02, 1), fontsize=10, frameon=True)

# Ajustar layout
plt.tight_layout()

# Guardar
plt.savefig('renders/render_python.png', dpi=300, bbox_inches='tight', 
            facecolor='white', edgecolor='none')
plt.close()

print("Gráfico guardado en: renders/render_python.png")
