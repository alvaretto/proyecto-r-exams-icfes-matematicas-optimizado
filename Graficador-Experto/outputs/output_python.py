#!/usr/bin/env python3
"""
Gráfica de Poblaciones de 5 Países (1960-2013)
Ejercicio ICFES - Generado con matplotlib
"""

import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

# Datos de población por país
años = [1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2013]

# País 1 - Crecimiento muy pronunciado (línea punteada cian)
pais1 = [20e6, 23e6, 26e6, 29.5e6, 33e6, 36e6, 38.5e6, 40.5e6, 42e6, 43.5e6, 45e6, 47e6]

# País 2 - Crecimiento moderado (línea discontinua negra)
# Cruza con País 5 en ~1986
pais2 = [20e6, 22e6, 24e6, 26e6, 28.5e6, 30e6, 32.5e6, 34.5e6, 36e6, 37e6, 38e6, 38.5e6]

# País 3 - Crecimiento desde abajo (línea sólida marrón)
pais3 = [15e6, 17e6, 19.5e6, 22.5e6, 26e6, 29e6, 32e6, 34.5e6, 36.5e6, 37.5e6, 38e6, 38.5e6]

# País 4 - Empieza alto, crecimiento moderado (línea sólida cian con triángulos)
pais4 = [31e6, 31.5e6, 32.5e6, 34e6, 36e6, 37.5e6, 39e6, 40e6, 40.5e6, 41.5e6, 42e6, 42.5e6]

# País 5 - Horizontal hasta 1980, luego crece (línea sólida naranja con círculos)
# Cruza con País 2 en ~1986
pais5 = [30e6, 30e6, 30e6, 30e6, 30e6, 30.5e6, 33e6, 35e6, 36.5e6, 37.5e6, 38e6, 38.5e6]

# Crear figura
fig, ax = plt.subplots(figsize=(12, 8))

# Graficar cada país con su estilo específico
ax.plot(años, pais1, 'c:', linewidth=1.5, label='País 1')  # Punteada cian
ax.plot(años, pais2, 'k--', linewidth=1.5, label='País 2')  # Discontinua negra
ax.plot(años, pais3, '-', color='brown', linewidth=1.5, label='País 3')  # Sólida marrón
ax.plot(años, pais4, 'c-', marker='^', markersize=6, linewidth=1.5, label='País 4')  # Cian con triángulos
ax.plot(años, pais5, '-', color='orange', marker='o', markersize=5, linewidth=1.5, label='País 5')  # Naranja con círculos

# Configurar ejes
ax.set_xlabel('Año', fontsize=12)
ax.set_ylabel('Población', fontsize=12)
ax.set_xlim(1958, 2016)
ax.set_ylim(14e6, 48e6)

# Configurar ticks del eje X
ax.set_xticks(años)
ax.set_xticklabels(años, rotation=45, ha='right', fontsize=9)

# Configurar ticks del eje Y con formato de punto como separador de miles
yticks = [15e6, 20e6, 25e6, 30e6, 35e6, 40e6, 45e6]
ax.set_yticks(yticks)
ax.set_yticklabels([f'{int(y/1e6):,}'.replace(',', '.') + '.000.000' for y in yticks], fontsize=9)

# Grilla - visible pero suave como en la imagen original
ax.grid(True, which='major', linestyle='-', linewidth=0.6, color='silver', alpha=0.9)
ax.set_axisbelow(True)

# Leyenda
ax.legend(loc='upper left', bbox_to_anchor=(1.02, 1), frameon=False, fontsize=10)

# Ajustar layout
plt.tight_layout()

# Guardar figura
plt.savefig('output_python.png', dpi=300, bbox_inches='tight', facecolor='white')
plt.savefig('output_python.pdf', bbox_inches='tight', facecolor='white')

print("Gráfica guardada en: output_python.png y output_python.pdf")
plt.show()
