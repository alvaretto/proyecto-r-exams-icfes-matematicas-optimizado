#!/usr/bin/env python3
"""
Graficador Experto - Dispersión Alcance Proyectil
Genera scatter plot replicando imagen ICFES original
"""

import numpy as np
import matplotlib.pyplot as plt

# Configurar semilla para reproducibilidad
np.random.seed(42)

# Parámetros físicos
g = 9.8  # gravedad (m/s^2)
v0 = 11.5  # velocidad inicial (m/s)
n_puntos = 99  # número de lanzamientos

# Generar ángulos uniformemente distribuidos
angulos = np.random.uniform(0.05, 1.55, n_puntos)

# Calcular alcance teórico: R = v0^2 * sin(2*theta) / g
alcance_teorico = (v0**2 * np.sin(2 * angulos)) / g

# Agregar ruido proporcional al alcance (mayor dispersión en alcances mayores)
ruido_base = 0.45
ruido = np.random.normal(0, ruido_base * np.sqrt(alcance_teorico))
alcance_observado = alcance_teorico + ruido
alcance_observado = np.maximum(0.2, alcance_observado)  # Evitar valores negativos

# Crear figura
fig, ax = plt.subplots(figsize=(10, 6.5))

# Scatter plot con diamantes cyan
ax.scatter(angulos, alcance_observado,
           c='#00CED1',  # cyan/turquesa
           marker='D',   # diamante
           s=40,         # tamaño
           alpha=0.85,
           edgecolors='#00CED1',
           linewidths=0.5)

# Configurar ejes
ax.set_xlabel('Ángulo (en radianes)', fontsize=11)
ax.set_ylabel('Alcance horizontal (m)', fontsize=11)
ax.set_xlim(0, 1.7)
ax.set_ylim(0, 15)
ax.set_xticks([0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6])
ax.set_yticks([0, 2, 4, 6, 8, 10, 12, 14])

# Cuadrícula
ax.grid(True, linestyle='-', alpha=0.3, color='gray')
ax.set_axisbelow(True)

# Estilo de ejes
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

# Fondo blanco
ax.set_facecolor('white')
fig.patch.set_facecolor('white')

# Guardar figura
plt.tight_layout()
plt.savefig('python_output.png', dpi=150, bbox_inches='tight',
            facecolor='white', edgecolor='none')
plt.close()

print("Gráfico generado: python_output.png")
