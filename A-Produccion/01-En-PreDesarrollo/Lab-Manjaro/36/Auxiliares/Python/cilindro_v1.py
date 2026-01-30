import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Ellipse, Rectangle, Arrow, FancyArrowPatch

# Crear figura y ejes
fig, ax = plt.subplots(figsize=(10, 12))

# Parámetros del cilindro
radio_interno = 1  # m
grosor = 0.5  # m
radio_externo = radio_interno + grosor  # 1.5 m
altura = 3  # m

# Ajustar límites del gráfico
ax.set_xlim(-2, 2)
ax.set_ylim(-0.5, 4)
ax.axis('equal')
ax.axis('off')

# Factor de compresión para la perspectiva
factor_compresion = 0.4

# Dibujar el cilindro externo
# Base inferior
ax.add_patch(Ellipse((0, 0), width=2*radio_externo, height=2*radio_externo*factor_compresion,
                     fill=False, edgecolor='black', linestyle='-'))

# Base superior
ax.add_patch(Ellipse((0, altura), width=2*radio_externo, height=2*radio_externo*factor_compresion,
                     fill=False, edgecolor='black', linestyle='-'))

# Laterales externos
ax.plot([-radio_externo, -radio_externo], [0, altura], 'k-')
ax.plot([radio_externo, radio_externo], [0, altura], 'k-')

# Dibujar el cilindro interno
# Base inferior interna (línea punteada)
ax.add_patch(Ellipse((0, 0), width=2*radio_interno, height=2*radio_interno*factor_compresion,
                     fill=False, edgecolor='black', linestyle='--'))

# Base superior interna (línea punteada)
ax.add_patch(Ellipse((0, altura), width=2*radio_interno, height=2*radio_interno*factor_compresion,
                     fill=False, edgecolor='black', linestyle='--'))

# Laterales internos (líneas punteadas)
ax.plot([-radio_interno, -radio_interno], [0, altura], 'k--')
ax.plot([radio_interno, radio_interno], [0, altura], 'k--')

# Añadir etiquetas con flechas
# Diámetro externo en la parte superior
ax.annotate("Diámetro externo", xy=(0, altura + 0.6), xytext=(0, altura + 0.8),
            ha='center', va='center', fontsize=11)
ax.arrow(-radio_externo, altura + 0.7, 2*radio_externo, 0, head_width=0.1, head_length=0.1, fc='black', ec='black')
ax.arrow(radio_externo, altura + 0.7, -2*radio_externo, 0, head_width=0.1, head_length=0.1, fc='black', ec='black')

# Radio interno
ax.annotate("Radio interno = 1 m", xy=(radio_interno/2, altura/2 + 0.3), xytext=(radio_interno/2, altura/2 + 0.3),
            ha='center', va='center', fontsize=11)
ax.arrow(0, altura/2, radio_interno, 0, head_width=0.1, head_length=0.1, fc='black', ec='black')

# Grosor
ax.annotate("Grosor = 0,5 m", xy=(-radio_externo - 0.5, altura/1.5), xytext=(-radio_externo - 0.5, altura/1.5),
            ha='center', va='center', fontsize=11)
ax.arrow(-radio_externo, altura/1.5, grosor, 0, head_width=0.1, head_length=0.1, fc='black', ec='black')

# Altura
ax.annotate("Altura", xy=(radio_externo + 0.5, altura/2), xytext=(radio_externo + 0.5, altura/2),
            ha='center', va='center', fontsize=11)
ax.arrow(radio_externo + 0.3, 0, 0, altura, head_width=0.1, head_length=0.1, fc='black', ec='black')

# Radio externo
ax.annotate("Radio externo", xy=(radio_externo/2, -0.5), xytext=(radio_externo/2, -0.1),
            ha='center', va='center', fontsize=11)
ax.arrow(0, -0.0, radio_externo, 0, head_width=0.1, head_length=0.1, fc='black', ec='black')

# Guardar y mostrar
plt.title('Cilindro Hueco', fontsize=14, pad=20)
plt.tight_layout()
plt.savefig('cilindro_hueco.png', dpi=300, bbox_inches='tight')
plt.show()
plt.close()
