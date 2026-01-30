import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.tri import Triangulation # Para la superficie anular superior

# --- Parámetros del cilindro (basados en cil.png) ---
radio_interno = 1.0  # m
grosor = 0.5  # m
radio_externo = radio_interno + grosor  # 1.5 m
altura = 3.0  # m (Asumido, no especificado en cil.png)
diametro_externo = 2 * radio_externo  # 3.0 m

# --- Crear figura y ejes 3D ---
fig = plt.figure(figsize=(9, 11)) # Ligeramente más grande para compensar
ax = fig.add_subplot(111, projection='3d') # [matplotlib.org]

# --- Función para crear puntos de un círculo ---
def circle_points(r, h_offset, n=100):
    theta = np.linspace(0, 2*np.pi, n)
    x = r * np.cos(theta)
    y = r * np.sin(theta)
    z = np.ones_like(theta) * h_offset
    return x, y, z

# --- Generar puntos para los círculos (bordes) ---
n_points = 100
x_top_ext, y_top_ext, z_top_ext = circle_points(radio_externo, altura, n_points)
x_top_int, y_top_int, z_top_int = circle_points(radio_interno, altura, n_points)
x_bot_ext, y_bot_ext, z_bot_ext = circle_points(radio_externo, 0, n_points)
x_bot_int, y_bot_int, z_bot_int = circle_points(radio_interno, 0, n_points)

# --- Generar malla para las superficies ---
theta_surf = np.linspace(0, 2*np.pi, 50)
z_surf = np.linspace(0, altura, 10)
theta_grid, z_grid = np.meshgrid(theta_surf, z_surf)

# Coordenadas superficie externa
x_ext_surf = radio_externo * np.cos(theta_grid)
y_ext_surf = radio_externo * np.sin(theta_grid)

# Coordenadas superficie interna
x_int_surf = radio_interno * np.cos(theta_grid)
y_int_surf = radio_interno * np.sin(theta_grid)

# --- Dibujar Superficies (EFECTO 3D) ---
surface_alpha = 0.6 # Transparencia para ver a través
outer_color = 'lightblue'
inner_color = 'lightgrey'
top_color = 'dodgerblue'

# Superficie externa
ax.plot_surface(x_ext_surf, y_ext_surf, z_grid, color=outer_color, alpha=surface_alpha, shade=True, rstride=1, cstride=1, zorder=1) # [matplotlib.org]

# Superficie interna (opcional, puede añadir claridad o desorden)
ax.plot_surface(x_int_surf, y_int_surf, z_grid, color=inner_color, alpha=surface_alpha, shade=True, rstride=1, cstride=1, zorder=1)

# Superficie anular superior (tapa)
n_radii = 10
n_angles = 50
radii = np.linspace(radio_interno, radio_externo, n_radii)
angles = np.linspace(0, 2 * np.pi, n_angles, endpoint=False)
angles = np.repeat(angles[..., np.newaxis], n_radii, axis=1)
radii = np.repeat(radii[np.newaxis, ...], n_angles, axis=0)

x_top_annulus = (radii * np.cos(angles)).flatten()
y_top_annulus = (radii * np.sin(angles)).flatten()
z_top_annulus = np.full(x_top_annulus.shape, altura)

# Crear triangulación para la superficie anular
tri = Triangulation(x_top_annulus, y_top_annulus)
ax.plot_trisurf(x_top_annulus, y_top_annulus, z_top_annulus, triangles=tri.triangles,
                cmap=plt.get_cmap('Blues'), shade=True, alpha=0.9, zorder=1.5, # Más opaco y encima de las paredes
                linewidth=0) # [matplotlib.org]


# --- Dibujar Bordes (para definición sobre las superficies) ---
line_color = 'k'
line_width = 1.0
dashed_style = '--'
line_zorder = 2 # Asegurar que las líneas estén sobre las superficies

# Determinar qué parte del círculo está oculta (aproximado)
split_idx = n_points // 2 # Índice aproximado que separa parte frontal/trasera

# Círculo superior externo
ax.plot(x_top_ext[:split_idx+1], y_top_ext[:split_idx+1], z_top_ext[:split_idx+1], color=line_color, lw=line_width, zorder=line_zorder)
ax.plot(x_top_ext[split_idx:], y_top_ext[split_idx:], z_top_ext[split_idx:], color=line_color, lw=line_width, linestyle=dashed_style, zorder=line_zorder)

# Círculo superior interno
ax.plot(x_top_int[:split_idx+1], y_top_int[:split_idx+1], z_top_int[:split_idx+1], color=line_color, lw=line_width, zorder=line_zorder)
ax.plot(x_top_int[split_idx:], y_top_int[split_idx:], z_top_int[split_idx:], color=line_color, lw=line_width, linestyle=dashed_style, zorder=line_zorder)

# Círculo inferior externo
ax.plot(x_bot_ext[:split_idx+1], y_bot_ext[:split_idx+1], z_bot_ext[:split_idx+1], color=line_color, lw=line_width, zorder=line_zorder)
ax.plot(x_bot_ext[split_idx:], y_bot_ext[split_idx:], z_bot_ext[split_idx:], color=line_color, lw=line_width, linestyle=dashed_style, zorder=line_zorder)

# Círculo inferior interno (totalmente oculto por defecto, dibujado como oculto)
# Con superficies, este a menudo no es necesario o visible
# ax.plot(x_bot_int, y_bot_int, z_bot_int, color=line_color, lw=line_width, linestyle=dashed_style, zorder=line_zorder)

# Líneas verticales externas (visibles en los bordes)
idx_front_right = np.argmax(x_top_ext[:split_idx+1]*np.cos(np.radians(-50)) + y_top_ext[:split_idx+1]*np.sin(np.radians(-50)))
idx_front_left = np.argmin(x_top_ext[:split_idx+1]*np.cos(np.radians(-50)) + y_top_ext[:split_idx+1]*np.sin(np.radians(-50)))
ax.plot([x_top_ext[idx_front_right], x_bot_ext[idx_front_right]], [y_top_ext[idx_front_right], y_bot_ext[idx_front_right]], [altura, 0], color=line_color, lw=line_width, zorder=line_zorder)
ax.plot([x_top_ext[idx_front_left], x_bot_ext[idx_front_left]], [y_top_ext[idx_front_left], y_bot_ext[idx_front_left]], [altura, 0], color=line_color, lw=line_width, zorder=line_zorder)

# Líneas verticales internas (ocultas)
# Con superficies, estas a menudo no son necesarias o visibles si las superficies no son muy transparentes
# ax.plot([radio_interno, radio_interno], [0, 0], [0, altura], color=line_color, lw=line_width, linestyle=dashed_style, zorder=line_zorder)
# ax.plot([-radio_interno, -radio_interno], [0, 0], [0, altura], color=line_color, lw=line_width, linestyle=dashed_style, zorder=line_zorder)

# Centro del cilindro (puntos)
ax.plot(0, 0, altura, marker='.', color=line_color, markersize=4, zorder=line_zorder+1) # Arriba
ax.plot(0, 0, 0, marker='.', color=line_color, markersize=4, zorder=line_zorder+1) # Abajo


# --- ANOTACIONES (estilo cil.png con mayor separación) ---
txt_color = 'k'
txt_size = 10
arrow_color = 'k'
arrow_lw = 0.8
tick_size = 0.05
anno_zorder = 5 # Asegurar que las anotaciones estén encima de todo

# --- Offsets para alejar las etiquetas ---
label_dist_factor = 1.5

# Offsets ajustados
z_offset_top_line = altura + 0.5 * label_dist_factor
z_offset_top_text = z_offset_top_line + 0.4

z_offset_bot_line = -0.3 * label_dist_factor
z_offset_bot_text = z_offset_bot_line - 0.2

x_offset_side_line = radio_externo + 0.4 * label_dist_factor
x_offset_side_text = x_offset_side_line + 0.5

# Diámetro externo
ax.text(0, 0, z_offset_top_text, "Diámetro externo", color=txt_color, size=txt_size, ha='center', va='center', zorder=anno_zorder)
ax.plot([-radio_externo, radio_externo], [0, 0], [z_offset_top_line, z_offset_top_line], color=arrow_color, lw=arrow_lw, zorder=anno_zorder)
ax.plot([-radio_externo, -radio_externo], [0, 0], [z_offset_top_line, altura], color=arrow_color, lw=arrow_lw, zorder=anno_zorder)
ax.plot([radio_externo, radio_externo], [0, 0], [z_offset_top_line, altura], color=arrow_color, lw=arrow_lw, zorder=anno_zorder)

# Altura
ax.text(x_offset_side_text, 0, altura / 2, "Altura", color=txt_color, size=txt_size, ha='center', va='center', rotation=90, zorder=anno_zorder)
ax.plot([x_offset_side_line, x_offset_side_line], [0, 0], [0, altura], color=arrow_color, lw=arrow_lw, zorder=anno_zorder)
ax.plot([x_offset_side_line, radio_externo], [0, 0], [0, 0], color=arrow_color, lw=arrow_lw, zorder=anno_zorder)
ax.plot([x_offset_side_line, radio_externo], [0, 0], [altura, altura], color=arrow_color, lw=arrow_lw, zorder=anno_zorder)

# Radio interno
text_ri_x = radio_interno * 0.7
text_ri_y = radio_interno * 1.5 * label_dist_factor
text_ri_z = altura + 0.3 * label_dist_factor
ax.text(text_ri_x, text_ri_y, text_ri_z, "Radio interno = 1 m", color=txt_color, size=txt_size, ha='left', va='bottom', zorder=anno_zorder)
ax.plot([0, radio_interno], [0, 0], [altura, altura], color=line_color, lw=line_width, zorder=line_zorder) # Línea del radio
leader_ri_start_x = text_ri_x - 0.1
leader_ri_start_y = text_ri_y - 0.1
leader_ri_start_z = text_ri_z - 0.05
ax.plot([leader_ri_start_x, radio_interno], [leader_ri_start_y, 0], [leader_ri_start_z, altura], color=arrow_color, lw=arrow_lw, zorder=anno_zorder) # Leader

# Grosor
text_gr_x = -radio_externo * 0.9 * label_dist_factor
text_gr_y = -radio_externo * 0.9 * label_dist_factor
text_gr_z = altura + 0.4 * label_dist_factor
ax.text(text_gr_x, text_gr_y, text_gr_z, "Grosor = 0,5 m", color=txt_color, size=txt_size, ha='center', va='center', zorder=anno_zorder)
target_gr_x = - (radio_interno + grosor / 2)
target_gr_y = 0
target_gr_z = altura
leader_gr_start_x = text_gr_x + 0.1
leader_gr_start_y = text_gr_y + 0.1
leader_gr_start_z = text_gr_z - 0.05
ax.plot([leader_gr_start_x, target_gr_x], [leader_gr_start_y, target_gr_y], [leader_gr_start_z, target_gr_z], color=arrow_color, lw=arrow_lw, zorder=anno_zorder) # Leader

# Radio externo
text_re_x = radio_externo * 0.5
text_re_y = -radio_externo * 1.2 * label_dist_factor
text_re_z = z_offset_bot_text
ax.text(text_re_x, text_re_y, text_re_z, "Radio externo", color=txt_color, size=txt_size, ha='center', va='center', zorder=anno_zorder)
ax.plot([0, radio_externo], [0, 0], [0, 0], color=line_color, lw=line_width, zorder=line_zorder) # Línea del radio
leader_re_start_x = text_re_x
leader_re_start_y = text_re_y + 0.1
leader_re_start_z = text_re_z + 0.05
ax.plot([leader_re_start_x, radio_externo], [leader_re_start_y, 0], [leader_re_start_z, 0], color=arrow_color, lw=arrow_lw, zorder=anno_zorder) # Leader


# --- Configuración visual del gráfico ---
# Usar proporciones relativas ayuda a que se vea menos distorsionado
# ax.set_box_aspect([diametro_externo, diametro_externo, altura])
# O forzar aspecto igual puede ser mejor para visualización 3D
ax.set_box_aspect([1, 1, 1]) # Proporciones iguales

ax.view_init(elev=25, azim=-50) # Ángulo de vista

# Ajustar límites para dar más espacio a las etiquetas alejadas
# (Calculados como antes, pero puede necesitar más ajuste con aspecto 1:1:1)
max_extent = max(diametro_externo, altura) * label_dist_factor * 0.8
ax.set_xlim(-max_extent, max_extent)
ax.set_ylim(-max_extent, max_extent)
ax.set_zlim(-max_extent*0.3, max_extent*1.2) # Ajustar Z según sea necesario

ax.set_axis_off() # Ocultar ejes

plt.show()
