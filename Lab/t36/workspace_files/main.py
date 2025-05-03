import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection, Line3DCollection

# Global variable to store image titles
_mfajlsdf98q21_image_title_list = []

# Parámetros del cilindro
r_interno = 1.0  # Radio interno = 1 m
grosor = 0.5     # Grosor = 0.5 m
r_externo = r_interno + grosor  # Radio externo = 1.5 m
altura = 3.0     # Altura = 3 m

# Configurar el gráfico 3D
fig = plt.figure(figsize=(10, 12))
ax = fig.add_subplot(111, projection='3d')

# Crear datos para cilindro externo
theta = np.linspace(0, 2*np.pi, 100)
z = np.linspace(0, altura, 2)
theta_grid, z_grid = np.meshgrid(theta, z)

# Coordenadas de los cilindros
x_ext = r_externo * np.cos(theta_grid)
y_ext = r_externo * np.sin(theta_grid)

x_int = r_interno * np.cos(theta_grid)
y_int = r_interno * np.sin(theta_grid)

# Dibujar cilindros
ax.plot_surface(x_ext, y_ext, z_grid, alpha=0.7, color='lightgray', linewidth=0.25, edgecolor='gray')
ax.plot_surface(x_int, y_int, z_grid, alpha=0.7, color='lightgray', linewidth=0.25, edgecolor='gray')

# Dibujar círculos superior e inferior del cilindro externo
theta_line = np.linspace(0, 2*np.pi, 100)
x_circle_ext = r_externo * np.cos(theta_line)
y_circle_ext = r_externo * np.sin(theta_line)

ax.plot(x_circle_ext, y_circle_ext, [0]*100, 'k-', linewidth=1.5)
ax.plot(x_circle_ext, y_circle_ext, [altura]*100, 'k-', linewidth=1.5)

# Dibujar círculos superior e inferior del cilindro interno
x_circle_int = r_interno * np.cos(theta_line)
y_circle_int = r_interno * np.sin(theta_line)

ax.plot(x_circle_int, y_circle_int, [0]*100, 'k-', linewidth=1.5)
ax.plot(x_circle_int, y_circle_int, [altura]*100, 'k-', linewidth=1.5)

# Líneas punteadas para partes ocultas (mitad posterior de los círculos)
theta_back = np.linspace(0, np.pi, 50)
x_back_ext = r_externo * np.cos(theta_back)
y_back_ext = r_externo * np.sin(theta_back)
x_back_int = r_interno * np.cos(theta_back)
y_back_int = r_interno * np.sin(theta_back)

ax.plot(x_back_ext, y_back_ext, [0]*50, 'k--', linewidth=1.2)
ax.plot(x_back_ext, y_back_ext, [altura]*50, 'k--', linewidth=1.2)
ax.plot(x_back_int, y_back_int, [0]*50, 'k--', linewidth=1.2)
ax.plot(x_back_int, y_back_int, [altura]*50, 'k--', linewidth=1.2)

# Añadir líneas verticales en los extremos
ax.plot([r_externo, r_externo], [0, 0], [0, altura], 'k-', linewidth=1.5)
ax.plot([-r_externo, -r_externo], [0, 0], [0, altura], 'k-', linewidth=1.5)
ax.plot([0, 0], [r_externo, r_externo], [0, altura], 'k-', linewidth=1.5)
ax.plot([0, 0], [-r_externo, -r_externo], [0, altura], 'k-', linewidth=1.5)

# Líneas punteadas verticales
ax.plot([r_interno, r_interno], [0, 0], [0, altura], 'k--', linewidth=1.2)
ax.plot([-r_interno, -r_interno], [0, 0], [0, altura], 'k--', linewidth=1.2)
ax.plot([0, 0], [r_interno, r_interno], [0, altura], 'k--', linewidth=1.2)
ax.plot([0, 0], [-r_interno, -r_interno], [0, altura], 'k--', linewidth=1.2)

# Añadir punto central y punto de referencia en radio interno
ax.scatter([0], [0], [altura/2], color='black', s=20)

# Configurar vista
ax.view_init(elev=15, azim=-40)
ax.dist = 10

# Añadir etiquetas
# Flecha para diámetro externo
ax.text(-r_externo, 0, altura+0.3, "Diámetro externo", ha='left', va='center', fontsize=10)
ax.plot([-r_externo, r_externo], [0, 0], [altura+0.1, altura+0.1], 'k-', linewidth=1)
ax.plot([-r_externo, -r_externo-0.2], [0, 0], [altura+0.1, altura+0.1], 'k-', linewidth=1)
ax.plot([r_externo, r_externo+0.2], [0, 0], [altura+0.1, altura+0.1], 'k-', linewidth=1)

# Flecha para grosor
ax.text(-0.5, 1.7, altura/2, "Grosor = 0,5 m", ha='left', va='center', fontsize=10)
ax.plot([-0.3, -r_interno], [1.5, 1.3], [altura/2, altura/2], 'k-', linewidth=1)
ax.plot([-(r_interno+grosor/2)], [0.9], [altura/2], 'o', markerfacecolor='orange', 
        markeredgecolor='orange', markersize=5)

# Flecha para radio interno
ax.text(0.8, -0.2, altura/2, "Radio interno = 1 m", ha='left', va='center', fontsize=10)
ax.plot([0.7, 0.3], [-0.2, -0.6], [altura/2, altura/2], 'k-', linewidth=1)
ax.plot([0.2], [-0.7], [altura/2], 'o', markerfacecolor='orange', 
        markeredgecolor='orange', markersize=5)

# Flecha para altura
ax.text(r_externo+0.5, 0, altura/2, "Altura", ha='left', va='center', fontsize=10)
ax.plot([r_externo+0.3, r_externo+0.3], [0, 0], [0, altura], 'k-', linewidth=1)
ax.plot([r_externo+0.3, r_externo+0.3-0.1], [0, 0], [0, 0-0.2], 'k-', linewidth=1)
ax.plot([r_externo+0.3, r_externo+0.3-0.1], [0, 0], [altura, altura+0.2], 'k-', linewidth=1)

# Flecha para radio externo
ax.text(-0.2, -1.7, 0, "Radio externo", ha='left', va='center', fontsize=10)
ax.plot([0, -0.5], [-1.5, -1.5], [0, 0], 'k-', linewidth=1)
ax.plot([-0.6], [-1.5], [0], 'o', markerfacecolor='orange', 
        markeredgecolor='orange', markersize=5)

# Eliminar ejes y poner fondo blanco
ax.set_axis_off()
ax.set_facecolor('white')
fig.set_facecolor('white')

# Ajustar límites para una mejor visualización
ax.set_xlim(-r_externo-1, r_externo+1)
ax.set_ylim(-r_externo-1, r_externo+1)
ax.set_zlim(0, altura+1)

# Ajustar aspecto
ax.set_box_aspect([1, 1, altura/(r_externo*2)])

# Guardar y mostrar el gráfico
plt.tight_layout()
plt.title("Cilindro Hueco")
_mfajlsdf98q21_image_title_list.append("Cilindro Hueco")
plt.savefig('cilindro_hueco.png', dpi=100, bbox_inches='tight')
plt.show()

#@mf:requirement:numpy
#@mf:requirement:matplotlib
