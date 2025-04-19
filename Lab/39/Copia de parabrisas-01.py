import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=(8, 4))

# Rectángulo punteado
rect_x = [0.5, 6.7, 6.7, 0.5, 0.5]
rect_y = [0, 0, 3, 3, 0]
ax.plot(rect_x, rect_y, linestyle='dashed', color='gray', dashes=(4, 4))

def dibujar_abanico(x_origin, y_origin, radius, lines_count=5):
    # Arco punteado
    theta = np.linspace(0, np.pi/2, 100)
    ax.plot(x_origin + radius * np.cos(theta), y_origin + radius * np.sin(theta),
            color='gray', linestyle='dashed', dashes=(4,4))
    
    # Líneas radiales
    for i in range(lines_count):
        angle = (np.pi/2) * i / (lines_count - 1)
        x_end = x_origin + radius * np.cos(angle)
        y_end = y_origin + radius * np.sin(angle)
        
        if i == 0 or i == 3:  
            linewidth = 2.0 if i == 1 else 1.5
            ax.plot([x_origin, x_end], [y_origin, y_end], color='black', linewidth=linewidth)
        else:
            ax.plot([x_origin, x_end], [y_origin, y_end],
                    color='gray', linestyle='dashed', dashes=(4,4), linewidth=0.8)

# Posición de los abanicos
dibujar_abanico(1, 0, 2.5, lines_count=5)
dibujar_abanico(3.75, 0, 2.5, lines_count=5)

# Ajustar la posición de la flecha y texto "Plumilla"
ax.annotate('Plumilla', xy=(1.7, 1.7), xytext=(0.5, 1.5),
            arrowprops=dict(facecolor='black', arrowstyle='->', linewidth=1),
            fontsize=10, ha='right', va='center')

# Ajustes de gráficos
ax.set_aspect('equal')
ax.axis('off')
plt.xlim(0, 7.5)
plt.ylim(-0.6, 3.5)

plt.tight_layout()
plt.show()
