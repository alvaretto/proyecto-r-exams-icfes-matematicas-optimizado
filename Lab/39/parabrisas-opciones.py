import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=(8, 4))

# Rectángulo exterior
rect_x = [0, 8, 8, 0, 0]
rect_y = [0, 0, 3, 3, 0]
ax.plot(rect_x, rect_y, color='brown')

# Dibujar semicírculos
def dibujar_semicirculo(x_center, y_origin, radius):
    theta = np.linspace(0, np.pi/2, 100)
    x = x_center + radius * np.cos(theta)
    y = y_origin + radius * np.sin(theta)
    ax.plot(x, y, color='brown')
    # Líneas rectas (base del semicírculo)
    ax.plot([x_center, x_center + radius], [y_origin, y_origin], color='brown')
    ax.plot([x_center, x_center], [y_origin, y_origin + radius], color='brown')
    # Pequeño símbolo de ángulo recto
    ax.plot([x_center, x_center + 0.2], [y_origin, y_origin], color='brown')
    ax.plot([x_center, x_center], [y_origin, y_origin + 0.2], color='brown')

# Posición de los semicírculos
dibujar_semicirculo(0, 0, 3)
dibujar_semicirculo(5, 0, 3)

# Añadir anotaciones de dimensiones
# Ancho total
ax.annotate('', xy=(8, 3.2), xytext=(0, 3.2), 
            arrowprops=dict(arrowstyle='<->', color='red'))
ax.text(4, 3.4, '8 cm', color='red', ha='center')

# Altura
ax.annotate('', xy=(-0.2, 3), xytext=(-0.2, 0), 
            arrowprops=dict(arrowstyle='<->', color='red'))
ax.text(-0.5, 1.5, '3 cm', color='red', ha='center', va='center', rotation=90)

# Ancho de semicírculos
ax.annotate('', xy=(6, -0.2), xytext=(0, -0.2), 
            arrowprops=dict(arrowstyle='<->', color='red'))
ax.text(3, -0.4, '6 cm', color='red', ha='center')

ax.annotate('', xy=(12, -0.2), xytext=(6, -0.2), 
            arrowprops=dict(arrowstyle='<->', color='red'))
ax.text(9, -0.4, '6 cm', color='red', ha='center')

# Ajustes de gráficos
ax.set_aspect('equal')
ax.axis('off')
plt.xlim(-1, 13)
plt.ylim(-1, 4)

plt.tight_layout()
plt.show()
