import math
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon, Circle, Arc

SLATE_900 = "#0F172A"
SLATE_800 = "#1E293B"
SLATE_600 = "#475569"
TEAL = "#0F766E"
ORANGE = "#EA580C"

fig, ax = plt.subplots(figsize=(8, 6), dpi=100)
ax.set_xlim(0, 800)
ax.set_ylim(600, 0)
ax.axis("off")
fig.patch.set_facecolor("white")
ax.set_facecolor("white")


def arrowheads(cx, cy):
    ax.add_patch(Polygon([[cx, cy-116], [cx-4, cy-108], [cx+4, cy-108]], closed=True, color=SLATE_600))
    ax.add_patch(Polygon([[cx, cy+116], [cx-4, cy+108], [cx+4, cy+108]], closed=True, color=SLATE_600))
    ax.add_patch(Polygon([[cx-116, cy], [cx-108, cy-4], [cx-108, cy+4]], closed=True, color=SLATE_600))
    ax.add_patch(Polygon([[cx+116, cy], [cx+108, cy-4], [cx+108, cy+4]], closed=True, color=SLATE_600))


def draw_axes(cx, cy, airport_side="right"):
    ax.plot([cx-108, cx+108], [cy, cy], color=SLATE_600, lw=1.5)
    ax.plot([cx, cx], [cy-108, cy+108], color=SLATE_600, lw=1.5)
    arrowheads(cx, cy)
    ax.text(cx, cy-125, "N", ha="center", va="center", fontsize=15, fontweight="bold", color=SLATE_800)
    if airport_side == "right":
        ax.text(cx+12, cy-7, "Aeropuerto", ha="left", va="center", fontsize=13, fontweight="medium", color=SLATE_600)
    else:
        ax.text(cx-12, cy-7, "Aeropuerto", ha="right", va="center", fontsize=13, fontweight="medium", color=SLATE_600)
    ax.add_patch(Circle((cx, cy), 4, color=SLATE_600))


def draw_diagram(label, cx, cy, ex, ey, dist_text, airport_side="left", angle_mode="north", value_pos=None, plane_pos=None):
    ax.text(cx-155, cy-105, f"{label}.", ha="left", va="center", fontsize=24, fontweight="bold", color=SLATE_900)
    draw_axes(cx, cy, airport_side)
    ax.plot([cx, ex], [cy, ey], color=TEAL, lw=2.5, solid_capstyle="round")
    ax.add_patch(Circle((ex, ey), 5, color=ORANGE))
    if angle_mode == "north":
        ax.add_patch(Arc((cx, cy), 50, 50, angle=0, theta1=270, theta2=320, color=ORANGE, lw=1.5))
        ax.text(cx-6, cy-22, "50°", ha="center", va="center", fontsize=13, fontweight="semibold", color=ORANGE)
    else:
        ax.add_patch(Arc((cx, cy), 50, 50, angle=0, theta1=90, theta2=140, color=ORANGE, lw=1.5))
        ax.text(cx-4, cy+32, "50°", ha="center", va="center", fontsize=13, fontweight="semibold", color=ORANGE)
    if plane_pos is None:
        plane_pos = (ex, ey-14, "center")
    if value_pos is None:
        value_pos = (ex+12, ey+13, "left")
    ax.text(plane_pos[0], plane_pos[1], "Avión", ha=plane_pos[2], va="center", fontsize=13, fontweight="medium", color=SLATE_600)
    ax.text(value_pos[0], value_pos[1], dist_text, ha=value_pos[2], va="center", fontsize=13, fontweight="semibold", color=TEAL)


draw_diagram("A", 200, 160, 169.4, 185.7, "30 km", airport_side="right", angle_mode="south", value_pos=(148, 198, "right"), plane_pos=(148, 216, "right"))
draw_diagram("B", 600, 160, 630.6, 134.3, "30 km", airport_side="left", angle_mode="north", value_pos=(644, 142, "left"), plane_pos=(644, 126, "left"))
draw_diagram("C", 200, 440, 257.5, 391.8, "70 km", airport_side="left", angle_mode="north", value_pos=(270, 405, "left"), plane_pos=(257.5, 378, "center"))
draw_diagram("D", 600, 440, 691.9, 362.9, "130 km", airport_side="left", angle_mode="north", value_pos=(704, 376, "left"), plane_pos=(691.9, 349, "center"))

plt.savefig("output_python_v1.png", dpi=100, bbox_inches=None, pad_inches=0, facecolor="white")
plt.close(fig)
