import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Rectangle, FancyArrowPatch
import math

# Parámetros del cilindro
radio_interno = 1.0  # m
grosor = 0.5  # m
radio_externo = radio_interno + grosor  # 1.5 m
altura = 3.0  # m

# Crear figura
plt.figure(figsize=(10, 12))
ax = plt.gca()

# Establecer límites
margin = 0.5
plt.xlim(-radio_externo - margin, radio_externo + margin)
plt.ylim(-altura/2 - margin, altura/2 + margin)

# Dibujar contornos externos del cilindro (lados)
ax.add_patch(Rectangle((-radio_externo, -altura/2), 2*radio_externo, altura, fill=False, color='black'))

# Dibujar contornos internos del cilindro (lados)
ax.add_patch(Rectangle((-radio_interno, -altura/2), 2*radio_interno, altura, fill=False, color='black', linestyle='--'))

# Dibujar elipses para dar sensación de perspectiva
# Parte superior
angulos = np.linspace(0, 2*np.pi, 100)
x_ext_top = radio_externo * np.cos(angulos)
y_ext_top = altura/2 + 0.1 * np.sin(angulos)
plt.plot(x_ext_top, y_ext_top, 'k-')

x_int_top = radio_interno * np.cos(angulos)
y_int_top = altura/2 + 0.1 * np.sin(angulos)
plt.plot(x_int_top, y_int_top, 'k--')

# Parte inferior
x_ext_bottom = radio_externo * np.cos(angulos)
y_ext_bottom = -altura/2 + 0.1 * np.sin(angulos)
plt.plot(x_ext_bottom, y_ext_bottom, 'k--')

x_int_bottom = radio_interno * np.cos(angulos)
y_int_bottom = -altura/2 + 0.1 * np.sin(angulos)
plt.plot(x_int_bottom, y_int_bottom, 'k--')

# Añadir líneas verticales para mejor visualización
plt.plot([radio_externo, radio_externo], [-altura/2, altura/2], 'k-')
plt.plot([-radio_externo, -radio_externo], [-altura/2, altura/2], 'k-')
plt.plot([radio_interno, radio_interno], [-altura/2, altura/2], 'k--')
plt.plot([-radio_interno, -radio_interno], [-altura/2, altura/2], 'k--')

# Añadir etiquetas con flechas

# Diámetro externo
plt.annotate('', xy=(radio_externo, altura/2+0.25), xytext=(-radio_externo, altura/2+0.25),
             arrowprops=dict(arrowstyle='<->', color='black'))
plt.text(0, altura/2+0.35, 'Diámetro externo', ha='center')

# Radio interno
plt.annotate('Radio interno = 1 m', xy=(radio_interno/2, 0.1), xytext=(0.3, 0.3),
             arrowprops=dict(arrowstyle='->', color='black'))

# Grosor
plt.annotate('Grosor = 0.5 m', xy=(radio_interno+grosor/2, 0.3), xytext=(radio_interno+grosor/2, 0.8),
             arrowprops=dict(arrowstyle='->', color='black'))

# Altura
plt.annotate('', xy=(radio_externo+0.2, altura/2), xytext=(radio_externo+0.2, -altura/2),
             arrowprops=dict(arrowstyle='<->', color='black'))
plt.text(radio_externo+0.35, 0, 'Altura', va='center', rotation=-90)

# Radio externo
plt.annotate('Radio externo', xy=(radio_externo/2, -altura/2-0.2), xytext=(radio_externo/2, -altura/2-0.4),
             arrowprops=dict(arrowstyle='->', color='black'))

# Configurar aspecto
plt.axis('equal')
plt.axis('off')
plt.title('Cilindro Hueco', fontsize=14)

# Guardar y mostrar
plt.tight_layout()
plt.savefig('cilindro_hueco.png', dpi=100, bbox_inches='tight')
plt.show()

_mfajlsdf98q21_image_title_list = ["Cilindro Hueco"]
