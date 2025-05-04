import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# --- Parámetros del cilindro (basados en cil.png) ---
radio_interno = 1.0  # m
grosor = 0.5  # m
radio_externo = radio_interno + grosor  # 1.5 m
altura = 3.0  # m (Asumido, no especificado en cil.png)
diametro_externo = 2 * radio_externo  # 3.0 m

# --- Crear figura y ejes 3D ---
fig = plt.figure(figsize=(8, 10))
ax = fig.add_subplot(111, projection='3d')

# --- Función para crear puntos de un círculo ---
def circle_points(r, h_offset, n=100):
    theta = np.linspace(0, 2*np.pi, n)
    x = r * np.cos(theta)
    y = r * np.sin(theta)
    z = np.ones_like(theta) * h_offset
    return x, y, z

# --- Generar puntos para los círculos ---
n_points = 100
x_top_ext, y_top_ext, z_top_ext = circle_points(radio_externo, altura, n_points)
x_top_int, y_top_int, z_top_int = circle_points(radio_interno, altura, n_points)
x_bot_ext, y_bot_ext, z_bot_ext = circle_points(radio_externo, 0, n_points)
x_bot_int, y_bot_int, z_bot_int = circle_points(radio_interno, 0, n_points)

# --- Determinar qué parte del círculo está oculta ---
split_idx = n_points // 2 # Índice aproximado que separa parte frontal/trasera

# --- Dibujar bordes del cilindro (estilo cil.png) ---
line_color = 'k'
line_width = 1.0
dashed_style = '--'

# Círculo superior externo
ax.plot(x_top_ext[:split_idx+1], y_top_ext[:split_idx+1], z_top_ext[:split_idx+1], color=line_color, lw=line_width)
ax.plot(x_top_ext[split_idx:], y_top_ext[split_idx:], z_top_ext[split_idx:], color=line_color, lw=line_width, linestyle=dashed_style)

# Círculo superior interno
ax.plot(x_top_int[:split_idx+1], y_top_int[:split_idx+1], z_top_int[:split_idx+1], color=line_color, lw=line_width)
ax.plot(x_top_int[split_idx:], y_top_int[split_idx:], z_top_int[split_idx:], color=line_color, lw=line_width, linestyle=dashed_style)

# Círculo inferior externo
ax.plot(x_bot_ext[:split_idx+1], y_bot_ext[:split_idx+1], z_bot_ext[:split_idx+1], color=line_color, lw=line_width)
ax.plot(x_bot_ext[split_idx:], y_bot_ext[split_idx:], z_bot_ext[split_idx:], color=line_color, lw=line_width, linestyle=dashed_style)

# Círculo inferior interno (totalmente oculto)
ax.plot(x_bot_int, y_bot_int, z_bot_int, color=line_color, lw=line_width, linestyle=dashed_style)

# Líneas verticales externas (visibles en los bordes)
idx_front_right = np.argmax(x_top_ext[:split_idx+1]*np.cos(np.radians(-50)) + y_top_ext[:split_idx+1]*np.sin(np.radians(-50)))
idx_front_left = np.argmin(x_top_ext[:split_idx+1]*np.cos(np.radians(-50)) + y_top_ext[:split_idx+1]*np.sin(np.radians(-50)))
ax.plot([x_top_ext[idx_front_right], x_bot_ext[idx_front_right]], [y_top_ext[idx_front_right], y_bot_ext[idx_front_right]], [altura, 0], color=line_color, lw=line_width)
ax.plot([x_top_ext[idx_front_left], x_bot_ext[idx_front_left]], [y_top_ext[idx_front_left], y_bot_ext[idx_front_left]], [altura, 0], color=line_color, lw=line_width) # [matplotlib.org]

# Líneas verticales internas (ocultas)
ax.plot([radio_interno, radio_interno], [0, 0], [0, altura], color=line_color, lw=line_width, linestyle=dashed_style)
ax.plot([-radio_interno, -radio_interno], [0, 0], [0, altura], color=line_color, lw=line_width, linestyle=dashed_style)

# Centro del cilindro (puntos)
ax.plot(0, 0, altura, marker='.', color=line_color, markersize=4) # Arriba
ax.plot(0, 0, 0, marker='.', color=line_color, markersize=4) # Abajo


# --- ANOTACIONES (estilo cil.png con mayor separación) ---
txt_color = 'k'
txt_size = 10
arrow_color = 'k'
arrow_lw = 0.8
tick_size = 0.05

# --- NUEVOS OFFSETS para alejar las etiquetas ---
label_dist_factor = 1.5 # Factor general para aumentar la distancia

# Offsets ajustados para alejar etiquetas
z_offset_top_line = altura + 0.5 * label_dist_factor # Línea de cota diámetro
z_offset_top_text = z_offset_top_line + 0.4 # Texto diámetro

z_offset_bot_line = -0.3 * label_dist_factor  # Línea de cota radio externo (no se usa directamente, solo para texto)
z_offset_bot_text = z_offset_bot_line - 0.2 # Texto radio externo

x_offset_side_line = radio_externo + 0.4 * label_dist_factor # Línea de cota altura
x_offset_side_text = x_offset_side_line + 0.5 # Texto altura

# Diámetro externo
ax.text(0, 0, z_offset_top_text, "Diámetro externo", color=txt_color, size=txt_size, ha='center', va='center')
ax.plot([-radio_externo, radio_externo], [0, 0], [z_offset_top_line, z_offset_top_line], color=arrow_color, lw=arrow_lw)
# Ticks conectando la línea de cota al cilindro
ax.plot([-radio_externo, -radio_externo], [0, 0], [z_offset_top_line, altura], color=arrow_color, lw=arrow_lw)
ax.plot([radio_externo, radio_externo], [0, 0], [z_offset_top_line, altura], color=arrow_color, lw=arrow_lw)

# Altura
ax.text(x_offset_side_text, 0, altura / 2, "Altura", color=txt_color, size=txt_size, ha='center', va='center', rotation=90)
ax.plot([x_offset_side_line, x_offset_side_line], [0, 0], [0, altura], color=arrow_color, lw=arrow_lw)
# Ticks conectando la línea de cota al cilindro
ax.plot([x_offset_side_line, radio_externo], [0, 0], [0, 0], color=arrow_color, lw=arrow_lw)
ax.plot([x_offset_side_line, radio_externo], [0, 0], [altura, altura], color=arrow_color, lw=arrow_lw)

# Radio interno
# Posición del texto más alejada
text_ri_x = radio_interno * 0.7
text_ri_y = radio_interno * 1.5 * label_dist_factor
text_ri_z = altura + 0.3 * label_dist_factor
ax.text(text_ri_x, text_ri_y, text_ri_z, "Radio interno = 1 m", color=txt_color, size=txt_size, ha='left', va='bottom')
# Línea del radio real
ax.plot([0, radio_interno], [0, 0], [altura, altura], color=line_color, lw=line_width)
# Leader line desde cerca del texto hasta el final del radio
leader_ri_start_x = text_ri_x - 0.1
leader_ri_start_y = text_ri_y - 0.1
leader_ri_start_z = text_ri_z - 0.05
ax.plot([leader_ri_start_x, radio_interno], [leader_ri_start_y, 0], [leader_ri_start_z, altura], color=arrow_color, lw=arrow_lw)

# Grosor
# Posición del texto más alejada
text_gr_x = -radio_externo * 0.9 * label_dist_factor
text_gr_y = -radio_externo * 0.9 * label_dist_factor
text_gr_z = altura + 0.4 * label_dist_factor
ax.text(text_gr_x, text_gr_y, text_gr_z, "Grosor = 0,5 m", color=txt_color, size=txt_size, ha='center', va='center')
# Punto objetivo para el leader (en la mitad del grosor, en el borde superior )
target_gr_x = - (radio_interno + grosor / 2)
target_gr_y = 0
target_gr_z = altura
# Leader line desde cerca del texto hasta el punto objetivo
leader_gr_start_x = text_gr_x + 0.1
leader_gr_start_y = text_gr_y + 0.1
leader_gr_start_z = text_gr_z - 0.05
ax.plot([leader_gr_start_x, target_gr_x], [leader_gr_start_y, target_gr_y], [leader_gr_start_z, target_gr_z], color=arrow_color, lw=arrow_lw)

# Radio externo
# Posición del texto más alejada
text_re_x = radio_externo * 0.5
text_re_y = -radio_externo * 1.2 * label_dist_factor
text_re_z = z_offset_bot_text
ax.text(text_re_x, text_re_y, text_re_z, "Radio externo", color=txt_color, size=txt_size, ha='center', va='center')
# Línea del radio real
ax.plot([0, radio_externo], [0, 0], [0, 0], color=line_color, lw=line_width)
# Leader line desde cerca del texto hasta el final del radio
leader_re_start_x = text_re_x
leader_re_start_y = text_re_y + 0.1
leader_re_start_z = text_re_z + 0.05
ax.plot([leader_re_start_x, radio_externo], [leader_re_start_y, 0], [leader_re_start_z, 0], color=arrow_color, lw=arrow_lw)


# --- Configuración visual del gráfico ---
ax.set_box_aspect([diametro_externo, diametro_externo, altura]) # Mantener proporciones relativas
ax.view_init(elev=25, azim=-50) # Ángulo de vista

# Ajustar límites para dar más espacio a las etiquetas alejadas
max_dim_x = max(radio_externo, abs(x_offset_side_text)) * 1.2
max_dim_y = max(radio_externo, abs(text_re_y), abs(text_gr_y), abs(text_ri_y)) * 1.2
max_dim_z_pos = max(altura, abs(z_offset_top_text), abs(text_gr_z), abs(text_ri_z)) * 1.1
max_dim_z_neg = abs(text_re_z) * 1.1

ax.set_xlim(-max_dim_x, max_dim_x)
ax.set_ylim(-max_dim_y, max_dim_y)
ax.set_zlim(-max_dim_z_neg, max_dim_z_pos)

ax.set_axis_off() # Ocultar ejes

plt.show()
