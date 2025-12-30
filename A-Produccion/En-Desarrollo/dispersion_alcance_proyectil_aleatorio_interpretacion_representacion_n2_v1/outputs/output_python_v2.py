#!/usr/bin/env python3
"""
Graficador Experto - Dispersión Alcance Proyectil - v2
Iteración para alcanzar ≥95% similitud con imagen ICFES original
"""

import numpy as np
import matplotlib.pyplot as plt

# Configurar semilla para reproducibilidad
np.random.seed(42)

# Parámetros físicos ajustados para coincidir con imagen original
g = 9.8  # gravedad (m/s^2)
v0 = 11.3  # velocidad inicial ajustada para alcance max ~13m
n_puntos = 99  # número exacto de lanzamientos como en original

# Generar ángulos uniformemente distribuidos
angulos = np.random.uniform(0.05, 1.55, n_puntos)

# Calcular alcance teórico: R = v0^2 * sin(2*theta) / g
alcance_teorico = (v0**2 * np.sin(2 * angulos)) / g

# Ruido proporcional al alcance (mayor dispersión en alcances mayores)
# Ajustado para coincidir con patrón visual de imagen original
ruido_base = 0.42
ruido = np.random.normal(0, ruido_base * np.sqrt(np.maximum(0.5, alcance_teorico)))
alcance_observado = alcance_teorico + ruido
alcance_observado = np.clip(alcance_observado, 0.3, 13.8)  # Limitar rango

# Crear figura con dimensiones similares a original
fig, ax = plt.subplots(figsize=(9, 6))

# Scatter plot con diamantes cyan - ajustado a imagen original
ax.scatter(angulos, alcance_observado,
           c='#00CED1',  # cyan turquesa exacto
           marker='D',   # diamante
           s=35,         # tamaño ajustado
           alpha=0.9,
           edgecolors='none')

# Configurar ejes idénticos a imagen original
ax.set_xlabel('Ángulo (en radianes)', fontsize=11)
ax.set_ylabel('Alcance horizontal (m)', fontsize=11)
ax.set_xlim(0, 1.7)
ax.set_ylim(0, 15)
ax.set_xticks([0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6])
ax.set_yticks([0, 2, 4, 6, 8, 10, 12, 14])

# Cuadrícula similar a original (líneas horizontales más visibles)
ax.grid(True, axis='y', linestyle='-', alpha=0.4, color='gray')
ax.grid(True, axis='x', linestyle='-', alpha=0.2, color='gray')
ax.set_axisbelow(True)

# Eliminar bordes superiores y derechos
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

# Fondo blanco
ax.set_facecolor('white')
fig.patch.set_facecolor('white')

# Guardar figura
plt.tight_layout()
plt.savefig('python_output_v2.png', dpi=150, bbox_inches='tight',
            facecolor='white', edgecolor='none')
plt.close()

print("Gráfico v2 generado: python_output_v2.png")
