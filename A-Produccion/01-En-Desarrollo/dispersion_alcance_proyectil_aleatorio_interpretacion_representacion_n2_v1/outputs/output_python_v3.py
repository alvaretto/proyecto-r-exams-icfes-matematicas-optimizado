#!/usr/bin/env python3
"""Python v3 - Ajuste fino para ≥95% similitud"""
import numpy as np
import matplotlib.pyplot as plt

np.random.seed(42)

g = 9.8
v0 = 11.2
n_puntos = 99

# Distribución de ángulos similar a imagen original
angulos = np.concatenate([
    np.random.uniform(0.05, 0.25, 15),
    np.random.uniform(0.25, 0.55, 25),
    np.random.uniform(0.55, 1.0, 30),
    np.random.uniform(1.0, 1.35, 20),
    np.random.uniform(1.35, 1.55, 9)
])

alcance_teorico = (v0**2 * np.sin(2 * angulos)) / g
ruido_factor = 0.38 * np.sqrt(np.maximum(0.3, alcance_teorico / alcance_teorico.max())) * np.sqrt(alcance_teorico)
ruido = np.random.normal(0, ruido_factor)
alcance = np.clip(alcance_teorico + ruido, 0.3, 13.5)

fig, ax = plt.subplots(figsize=(9, 6))
ax.scatter(angulos, alcance, c='#00CED1', marker='D', s=35, alpha=0.9, edgecolors='none')
ax.set_xlabel('Ángulo (en radianes)', fontsize=11)
ax.set_ylabel('Alcance horizontal (m)', fontsize=11)
ax.set_xlim(0, 1.7)
ax.set_ylim(0, 15)
ax.set_xticks([0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6])
ax.set_yticks([0, 2, 4, 6, 8, 10, 12, 14])
ax.grid(True, axis='y', linestyle='-', alpha=0.4, color='gray')
ax.grid(True, axis='x', linestyle='-', alpha=0.2, color='gray')
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.set_facecolor('white')
fig.patch.set_facecolor('white')
plt.tight_layout()
plt.savefig('python_output_v3.png', dpi=150, bbox_inches='tight', facecolor='white')
plt.close()
print("Python v3 generado")
