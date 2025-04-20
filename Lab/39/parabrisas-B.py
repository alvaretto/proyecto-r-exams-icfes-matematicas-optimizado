import matplotlib.pyplot as plt
import numpy as np

fig, ax = plt.subplots(figsize=(8, 4))

# Rectángulo punteado
rect_x = [0.5, 6.7, 6.7, 0.5, 0.5]
rect_y = [0, 0, 3, 3, 0]
ax.plot(rect_x, rect_y, linestyle='dashed', color='gray', dashes=(4, 4))

def dibujar_abanico(x_origin, y_origin, radius, lines_count=5):
    # Arco sólido (ya no punteado)
    theta = np.linspace(0, np.pi/2, 100)
    ax.plot(x_origin + radius * np.cos(theta), y_origin + radius * np.sin(theta),
            color='black', linestyle='solid')
    
    # Líneas radiales - ahora todas sólidas y negras
    for i in range(lines_count):
        angle = (np.pi/2) * i / (lines_count - 1)
        x_end = x_origin + radius * np.cos(angle)
        y_end = y_origin + radius * np.sin(angle)
        
        # Todas las líneas sólidas
        if i == 0 or i == lines_count-1:  
            linewidth = 1.5
            ax.plot([x_origin, x_end], [y_origin, y_end], color='black', linewidth=linewidth)
        else:
            pass
    
    # Añadir símbolo de 90 grados en el vértice
    square_size = 0.15
    x_square = [x_origin, x_origin + square_size, x_origin + square_size, x_origin, x_origin]
    y_square = [y_origin, y_origin, y_origin + square_size, y_origin + square_size, y_origin]
    ax.plot(x_square, y_square, color='black', linewidth=0.8)

# Posición de los abanicos
dibujar_abanico(1, 0, 2.5, lines_count=2)  
dibujar_abanico(3.75, 0, 2.5, lines_count=2)  

# Añadir las dimensiones
# Dimensión horizontal superior - 8 cm
ax.annotate('13 cm', xy=(3.6, 3.2), xytext=(3.6, 3.2), ha='center', color='red')
ax.plot([0.5, 6.7], [3.1, 3.1], color='red', linewidth=1)
ax.plot([0.5, 0.5], [3.05, 3.15], color='red', linewidth=1)
ax.plot([6.7, 6.7], [3.05, 3.15], color='red', linewidth=1)

# Dimensión vertical - 13 cm
ax.annotate('8 cm', xy=(0, 1.5), xytext=(0.15, 1.5), rotation=90, va='center', color='red')
ax.plot([0.4, 0.4], [0, 3], color='red', linewidth=1)
ax.plot([0.35, 0.45], [0, 0], color='red', linewidth=1)
ax.plot([0.35, 0.45], [3, 3], color='red', linewidth=1)

# Dimensiones horizontales inferiores - 6 cm para cada abanico
ax.annotate('6 cm', xy=(1.75, -0.3), xytext=(2.3, -0.45), ha='center', color='red')
ax.plot([1, 3.5], [-0.2, -0.2], color='red', linewidth=1)
ax.plot([1, 1], [-0.25, -0.15], color='red', linewidth=1)
ax.plot([3.5, 3.5], [-0.25, -0.15], color='red', linewidth=1)

ax.annotate('8 cm', xy=(4.5, -0.3), xytext=(5, -0.45), ha='center', color='red')
ax.plot([3.75, 6.25], [-0.2, -0.2], color='red', linewidth=1)
ax.plot([3.75, 3.75], [-0.25, -0.15], color='red', linewidth=1)
ax.plot([6.25, 6.25], [-0.25, -0.15], color='red', linewidth=1)

# Ajustes de gráficos
ax.set_aspect('equal')
ax.axis('off')
plt.xlim(0, 7.5)
plt.ylim(-0.6, 3.5)

plt.tight_layout()
plt.savefig("opcionB.png", dpi=150, bbox_inches="tight")
plt.close()
