import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Ellipse, Arc, FancyArrowPatch
from matplotlib.lines import Line2D

# Define variables from the TikZ code
altura = 3  # Height
radiointerno = 1  # Internal radius
grosor = 0.5  # Thickness
radioexterno = radiointerno + grosor  # External radius
vradioexternobot = 0.45  # Vertical external radius (bottom)
vradiointernobot = 0.3  # Vertical internal radius (bottom)
vradioexternotop = 0.6  # Vertical external radius (top)
vradiointernotop = 0.4  # Vertical internal radius (top)

# Create figure
fig, ax = plt.subplots(figsize=(10, 12))

# Set equal aspect ratio
ax.set_aspect('equal')

# Define coordinates
centro_inferior = (0, 0)
centro_superior = (0, altura)
R0b = (radioexterno, 0)
neg_R0b = (-radioexterno, 0)
r0b = (radiointerno, 0)
neg_r0b = (-radiointerno, 0)
R0t = (radioexterno, altura)
neg_R0t = (-radioexterno, altura)
r0t = (radiointerno, altura)
neg_r0t = (-radiointerno, altura)

# Draw dashed parts (hidden)
# Dashed arc - external bottom (back)
back_arc_ext = Arc((0, 0), 2*radioexterno, 2*vradioexternobot,
                  theta1=0, theta2=180, linestyle='dashed')
ax.add_patch(back_arc_ext)

# Dashed arc - internal bottom (back)
back_arc_int = Arc((0, 0), 2*radiointerno, 2*vradiointernobot,
                  theta1=0, theta2=180, linestyle='dashed')
ax.add_patch(back_arc_int)

# Dashed vertical lines (internal)
ax.add_line(Line2D([neg_r0b[0], neg_r0t[0]], [neg_r0b[1], neg_r0t[1]], linestyle='dashed'))
ax.add_line(Line2D([r0b[0], r0t[0]], [r0b[1], r0t[1]], linestyle='dashed'))

# Draw solid parts (visible)
# Solid vertical lines (external)
ax.add_line(Line2D([neg_R0b[0], neg_R0t[0]], [neg_R0b[1], neg_R0t[1]]))
ax.add_line(Line2D([R0b[0], R0t[0]], [R0b[1], R0t[1]]))

# Solid arc - external bottom (front)
front_arc_ext = Arc((0, 0), 2*radioexterno, 2*vradioexternobot,
                   theta1=180, theta2=360)
ax.add_patch(front_arc_ext)

# Solid arc - internal bottom (front)
front_arc_int = Arc((0, 0), 2*radiointerno, 2*vradiointernobot,
                   theta1=180, theta2=360)
ax.add_patch(front_arc_int)

# Ellipses for top
top_ellipse_ext = Ellipse(centro_superior, 2*radioexterno, 2*vradioexternotop,
                         fill=False)
ax.add_patch(top_ellipse_ext)

top_ellipse_int = Ellipse(centro_superior, 2*radiointerno, 2*vradiointernotop,
                         fill=False)
ax.add_patch(top_ellipse_int)

# Center points
ax.scatter([centro_inferior[0], centro_superior[0]],
          [centro_inferior[1], centro_superior[1]], color='black', s=20)

# Draw dimension lines and labels
# Height
ax.add_line(Line2D([radioexterno + 0.3, radioexterno + 0.3], [0, altura]))
ax.text(radioexterno + 0.4, altura/2, "Altura", fontsize=10, va='center')

# Internal radius
arrow_int_radius = FancyArrowPatch(centro_superior, r0t, arrowstyle='->',
                                  mutation_scale=15)
ax.add_patch(arrow_int_radius)
ax.text((centro_superior[0] + r0t[0])/2, altura + 0.1,
       "Radio interno = 1 m", fontsize=10, ha='center')

# Thickness (grosor)
ax.add_line(Line2D([r0t[0], R0t[0]], [r0t[1], R0t[1]]))
ax.text((r0t[0] + R0t[0])/2, altura + 0.1,
       "Grosor = 0.5 m", fontsize=10, ha='center')

# External diameter
ax.add_line(Line2D([-radioexterno - 0.5, radioexterno + 0.5],
                  [altura + 0.5, altura + 0.5]))
ax.text(0, altura + 0.7, "Diámetro externo", fontsize=10, ha='center')

# External radius
arrow_ext_radius = FancyArrowPatch(centro_inferior, R0b, arrowstyle='->',
                                  mutation_scale=15)
ax.add_patch(arrow_ext_radius)
ax.text(radioexterno/2, -0.2, "Radio externo", fontsize=10, ha='center')

# Setting the limits and removing axes
ax.set_xlim(-radioexterno - 1, radioexterno + 1)
ax.set_ylim(-0.5, altura + 1)
ax.axis('off')

# List for image titles (required by the execution environment)
_mfajlsdf98q21_image_title_list = ["Hollow Cylinder TikZ Diagram Visualization"]

plt.tight_layout()
plt.show()
