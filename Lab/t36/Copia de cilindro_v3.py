import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.patheffects as PathEffects

_mfajlsdf98q21_image_title_list = []

# Parámetros del cilindro 
factor_escala = 1.5
radio_interno = 1.0 * factor_escala  # m (originalmente 1.0 m)
grosor = 0.5 * factor_escala  # m (originalmente 0.5 m)
radio_externo = radio_interno + grosor  # 2.25 m (originalmente 1.5 m)
altura = 3.0 * factor_escala  # m (originalmente 3.0 m)
diametro_externo = 2 * radio_externo  # 4.5 m (originalmente 3.0 m)

# Crear figura y ejes 3D
fig = plt.figure(figsize=(10, 12))
ax = fig.add_subplot(111, projection='3d')

# Función para crear puntos de un círculo
def circle_points(r, h, n=100):
    theta = np.linspace(0, 2*np.pi, n)
    x = r * np.cos(theta)
    y = r * np.sin(theta)
    z = np.ones_like(theta) * h
    return x, y, z

# Crear cilindro externo
theta = np.linspace(0, 2*np.pi, 100)
z = np.linspace(0, altura, 20)
theta_grid, z_grid = np.meshgrid(theta, z)
x_ext = radio_externo * np.cos(theta_grid)
y_ext = radio_externo * np.sin(theta_grid)

# Crear cilindro interno
x_int = radio_interno * np.cos(theta_grid)
y_int = radio_interno * np.sin(theta_grid)

# Dibujar superficie externa del cilindro
ax.plot_surface(x_ext, y_ext, z_grid, alpha=0.5, color='gray', shade=False, zorder=1)

# Dibujar superficie interna del cilindro
ax.plot_surface(x_int, y_int, z_grid, alpha=0.5, color='lightgray', shade=False, zorder=1)

# Dibujar círculos superior e inferior
x_top_ext, y_top_ext, z_top_ext = circle_points(radio_externo, altura)
x_top_int, y_top_int, z_top_int = circle_points(radio_interno, altura)
x_bot_ext, y_bot_ext, z_bot_ext = circle_points(radio_externo, 0)
x_bot_int, y_bot_int, z_bot_int = circle_points(radio_interno, 0)

# Círculos externos
ax.plot(x_top_ext, y_top_ext, z_top_ext, 'k', lw=2)
ax.plot(x_bot_ext, y_bot_ext, z_bot_ext, 'k', lw=2)

# Círculos internos
ax.plot(x_top_int, y_top_int, z_top_int, 'k', lw=2)
ax.plot(x_bot_int, y_bot_int, z_bot_int, 'k', lw=2)

# Líneas punteadas para mostrar partes ocultas
ax.plot(x_top_ext[50:], y_top_ext[50:], z_top_ext[50:], 'k--', lw=1)
ax.plot(x_bot_ext[50:], y_bot_ext[50:], z_bot_ext[50:], 'k--', lw=1)
ax.plot(x_top_int[50:], y_top_int[50:], z_top_int[50:], 'k--', lw=1)
ax.plot(x_bot_int[50:], y_bot_int[50:], z_bot_int[50:], 'k--', lw=1)

# Líneas verticales en los lados
ax.plot([radio_externo, radio_externo], [0, 0], [0, altura], 'k', lw=2)
ax.plot([-radio_externo, -radio_externo], [0, 0], [0, altura], 'k', lw=2)
ax.plot([0, 0], [radio_externo, radio_externo], [0, altura], 'k', lw=2)
ax.plot([0, 0], [-radio_externo, -radio_externo], [0, altura], 'k', lw=2)

# Líneas verticales internas (punteadas las ocultas)
ax.plot([radio_interno, radio_interno], [0, 0], [0, altura], 'k--', lw=1)
ax.plot([-radio_interno, -radio_interno], [0, 0], [0, altura], 'k--', lw=1)
ax.plot([0, 0], [radio_interno, radio_interno], [0, altura], 'k--', lw=1)
ax.plot([0, 0], [-radio_interno, -radio_interno], [0, altura], 'k--', lw=1)

# Centro del cilindro en la parte superior e inferior
ax.plot([0], [0], [0], 'k.', markersize=5)
ax.plot([0], [0], [altura], 'k.', markersize=5)

# ----- ETIQUETAS MEJORADAS CON MAYOR SEPARACIÓN ----- #

# Diámetro externo (arriba) - Movido más arriba
ax.text(-radio_externo, 0, altura + 1.2, 'Diámetro externo', horizontalalignment='center', size=11, fontweight='bold')
ax.plot([-radio_externo, radio_externo], [0, 0], [altura + 0.7, altura + 0.7], 'k-', lw=1)
ax.plot([-radio_externo, -radio_externo], [0, 0], [altura, altura + 0.7], 'k-', lw=1)
ax.plot([radio_externo, radio_externo], [0, 0], [altura, altura + 0.7], 'k-', lw=1)

# Radio interno - Mucho más separado y con flecha clara
ax.text(-radio_externo - 1.3, 1.5, altura + 0.8, f"Radio interno = {radio_interno:.1f} m", size=11, 
        bbox=dict(facecolor='white', alpha=0.7, boxstyle='round,pad=0.3'))
# Línea hacia el radio interno con puntos intermedios para mejorar la visibilidad
ax.plot([-radio_externo - 1.0, -radio_interno/2, 0], [1.2, 0.6, 0], [altura + 0.6, altura + 0.3, altura], 'k-', lw=1)
# Línea que muestra el radio interno
ax.plot([0, radio_interno], [0, 0], [altura, altura], 'r-', lw=2, alpha=0.8)

# Grosor - Etiqueta movida mucho más abajo y a la derecha
ax.text(radio_externo + 1.0, -1.5, altura - 1.0, f"Grosor = {grosor:.2f} m", size=11, 
        bbox=dict(facecolor='white', alpha=0.7, boxstyle='round,pad=0.3'))
# Línea hacia el grosor con puntos intermedios
ax.plot([radio_externo + 0.8, radio_interno + grosor/2, radio_interno + grosor/2], [-1.2, -0.5, 0], [altura - 0.8, altura - 0.3, altura], 'k-', lw=1)
# Línea que muestra el grosor
ax.plot([radio_interno, radio_externo], [0, 0], [altura, altura], 'r-', lw=2, alpha=0.8)

# Altura - Etiqueta movida mucho más a la derecha
ax.text(radio_externo + 1.5, 1.5, altura/2, f"Altura = {altura:.1f} m", size=11, 
        bbox=dict(facecolor='white', alpha=0.7, boxstyle='round,pad=0.3'))
# Línea hacia la altura con puntos intermedios
ax.plot([radio_externo + 1.3, radio_externo + 0.7, radio_externo + 0.4], [1.2, 0.6, 0], [altura/2, altura/2, altura/2], 'k-', lw=1)
# Línea vertical que muestra la altura
ax.plot([radio_externo + 0.4, radio_externo + 0.4], [0, 0], [0, altura], 'k-', lw=1)
ax.plot([radio_externo, radio_externo + 0.4], [0, 0], [0, 0], 'k-', lw=1)
ax.plot([radio_externo, radio_externo + 0.4], [0, 0], [altura, altura], 'k-', lw=1)

# Radio externo (abajo) - Etiqueta movida más abajo y a la izquierda con mayor separación
ax.text(-radio_externo - 1.0, -1.5, -0.8, f"Radio externo = {radio_externo:.2f} m", size=11, 
        bbox=dict(facecolor='white', alpha=0.7, boxstyle='round,pad=0.3'))
# Línea hacia el radio externo con puntos intermedios
ax.plot([-radio_externo - 0.7, -radio_externo/2, 0], [-1.2, -0.6, 0], [-0.6, -0.3, 0], 'k-', lw=1)
# Línea que muestra el radio externo
ax.plot([0, radio_externo], [0, 0], [0, 0], 'r-', lw=2, alpha=0.8)

# Configuración visual del gráfico
ax.set_box_aspect([1, 1, 1.5])  # Ajustar proporciones
ax.view_init(elev=15, azim=-60)  # Vista isométrica
ax.set_xlim(-radio_externo*1.8, radio_externo*1.8)  # Ampliar límites para acomodar etiquetas
ax.set_ylim(-radio_externo*1.8, radio_externo*1.8)  # Ampliar límites para acomodar etiquetas
ax.set_zlim(-1.2, altura + 1.5)  # Ampliar límites para acomodar etiquetas
ax.set_axis_off()  # Ocultar ejes

# Título del cilindro
ax.text2D(0.5, 0.95, "CILINDRO HUECO (ESCALADO 1.5x)", transform=plt.gcf().transFigure, size=14, ha='center', fontweight='bold')

fig.tight_layout()
_mfajlsdf98q21_image_title_list.append("Cilindro Hueco con Etiquetas Mejoradas")
plt.show()
