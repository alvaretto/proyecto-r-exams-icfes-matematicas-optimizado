import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Ellipse, Arc, FancyArrowPatch
from matplotlib.lines import Line2D

# Variables extracted from TikZ code
altura = 3  # From TikZ
radiointerno = 1  # From TikZ
grosor = 0.5  # From TikZ
radioexterno = \radiointerno + \grosor  # From TikZ
vradioexternobot = 0.45  # From TikZ
vradiointernobot = 0.3  # From TikZ
vradioexternotop = 0.6  # From TikZ
vradiointernotop = 0.4  # From TikZ

# Create figure
fig, ax = plt.subplots(figsize=(10, 12))

# Set equal aspect ratio
ax.set_aspect('equal')

# Define coordinates from TikZ
centroinferior = (0, 0)
centrosuperior = (0, altura)
R0b = (radioexterno, 0)
r0b = (radiointerno, 0)
R0t = (radioexterno, altura)
r0t = (radiointerno, altura)

# Ellipse: center at centrosuperior, dims {radioexterno} and {vradioexternotop}
ellipse_radioexterno_vradioexternotop = Ellipse(centro_superior, 2*radioexterno, 2*vradioexternotop, fill=False)
ax.add_patch(ellipse_radioexterno_vradioexternotop)

# Ellipse: center at centrosuperior, dims {radiointerno} and {vradiointernotop}
ellipse_radiointerno_vradiointernotop = Ellipse(centro_superior, 2*radiointerno, 2*vradiointernotop, fill=False)
ax.add_patch(ellipse_radiointerno_vradiointernotop)

# Line from -r0b to -r0t
ax.add_line(Line2D([(-r0b)[0], (-r0t)[0]], [(-r0b)[1], (-r0t)[1]], linestyle="dashed"))

# Line from r0b to r0t
ax.add_line(Line2D([r0b[0], r0t[0]], [r0b[1], r0t[1]], linestyle="dashed"))

# Line from -R0b to -R0t
ax.add_line(Line2D([(-R0b)[0], (-R0t)[0]], [(-R0b)[1], (-R0t)[1]]))

# Line from R0b to R0t
ax.add_line(Line2D([R0b[0], R0t[0]], [R0b[1], R0t[1]]))

# Setting the limits
ax.set_xlim(-radioexterno - 1, radioexterno + 1)
ax.set_ylim(-0.5, altura + 1)
ax.axis('off')

# List for image titles
_mfajlsdf98q21_image_title_list = ["TikZ Diagram Visualization"]

plt.tight_layout()
plt.show()
