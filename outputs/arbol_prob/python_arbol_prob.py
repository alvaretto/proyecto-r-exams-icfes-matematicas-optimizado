"""
Árbol de Probabilidad de 2 Niveles — matplotlib
Uso: python3 python_arbol_prob.py
Salida: arbol_prob_python.png (mismo directorio)

Variables paramétricas al inicio del script para reutilización desde R/reticulate.
"""

import matplotlib
matplotlib.use("Agg")  # Backend sin GUI (compatible con reticulate)
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch
import os

# =============================================================================
# PARÁMETROS (modificar aquí para parametrizar desde R/reticulate)
# =============================================================================

# Etiquetas
LABEL_RAIZ      = "Inicio"
LABEL_L1_IZQ    = "Llueve"
LABEL_L1_DER    = "No llueve"
LABEL_L2_IZQ_L  = "Tráfico"
LABEL_L2_DER_L  = "Sin tráfico"
LABEL_L2_IZQ_R  = "Tráfico"
LABEL_L2_DER_R  = "Sin tráfico"

# Probabilidades nivel 1
P_L1_IZQ = 0.6   # P(Llueve)
P_L1_DER = 0.4   # P(No llueve)

# Probabilidades nivel 2 dado Llueve
P_L2_IZQ_L = 0.7  # P(Tráfico | Llueve)
P_L2_DER_L = 0.3  # P(Sin tráfico | Llueve)

# Probabilidades nivel 2 dado No llueve
P_L2_IZQ_R = 0.3  # P(Tráfico | No llueve)
P_L2_DER_R = 0.7  # P(Sin tráfico | No llueve)

# Probabilidades finales (conjuntas)
P_FINAL_LL_TR  = P_L1_IZQ * P_L2_IZQ_L   # 0.42
P_FINAL_LL_ST  = P_L1_IZQ * P_L2_DER_L   # 0.18
P_FINAL_NL_TR  = P_L1_DER * P_L2_IZQ_R   # 0.12
P_FINAL_NL_ST  = P_L1_DER * P_L2_DER_R   # 0.28

# Archivo de salida
NOMBRE_SALIDA = "arbol_prob_python.png"

# =============================================================================
# PALETA DE COLORES
# =============================================================================

COLOR_RAIZ_BORDE = "#1971c2"
COLOR_RAIZ_FONDO = "#a5d8ff"

COLOR_LLUEVE_BORDE  = "#2f9e44"
COLOR_LLUEVE_FONDO  = "#b2f2bb"

COLOR_NOLUEVE_BORDE = "#e8590c"
COLOR_NOLUEVE_FONDO = "#ffd8a8"

COLOR_HOJA_L_BORDE  = "#2f9e44"
COLOR_HOJA_L_FONDO  = "#d4edda"

COLOR_HOJA_R_BORDE  = "#e8590c"
COLOR_HOJA_R_FONDO  = "#ffe8cc"

COLOR_PROB_FINAL_TEXTO = "#1971c2"
COLOR_PROB_FINAL_FONDO = "#e8f4fd"
COLOR_PROB_FINAL_BORDE = "#1971c2"

COLOR_FLECHA = "#555555"
COLOR_PROB_RAMA = "#333333"

# =============================================================================
# UTILIDADES
# =============================================================================

def fmt(valor):
    """Formato decimal español (coma como separador)."""
    texto = f"{valor:.1f}" if valor == round(valor, 1) else f"{valor:.2f}"
    return texto.replace(".", ",")


def fmt2(valor):
    """Formato con 2 decimales, coma española."""
    return f"{valor:.2f}".replace(".", ",")


def nodo_rect(ax, cx, cy, w, h, texto, color_fondo, color_borde,
              fontsize=10, bold=False):
    """Dibuja un rectángulo redondeado con texto centrado."""
    x = cx - w / 2
    y = cy - h / 2
    fancy = FancyBboxPatch(
        (x, y), w, h,
        boxstyle="round,pad=0.03",
        facecolor=color_fondo,
        edgecolor=color_borde,
        linewidth=2.0,
        zorder=3
    )
    ax.add_patch(fancy)
    weight = "bold" if bold else "normal"
    ax.text(cx, cy, texto,
            ha="center", va="center",
            fontsize=fontsize, fontweight=weight,
            color="#1a1a1a", zorder=4)


def nodo_elipse(ax, cx, cy, rx, ry, texto, color_fondo, color_borde,
                fontsize=11, bold=True):
    """Dibuja una elipse con texto centrado."""
    elipse = mpatches.Ellipse(
        (cx, cy), rx * 2, ry * 2,
        facecolor=color_fondo,
        edgecolor=color_borde,
        linewidth=2.5,
        zorder=3
    )
    ax.add_patch(elipse)
    weight = "bold" if bold else "normal"
    ax.text(cx, cy, texto,
            ha="center", va="center",
            fontsize=fontsize, fontweight=weight,
            color="#1a1a1a", zorder=4)


def flecha(ax, x1, y1, x2, y2):
    """Dibuja una flecha de (x1,y1) a (x2,y2)."""
    ax.annotate(
        "",
        xy=(x2, y2),
        xytext=(x1, y1),
        arrowprops=dict(
            arrowstyle="-|>",
            color=COLOR_FLECHA,
            lw=1.8,
            mutation_scale=16,
        ),
        zorder=2
    )


def etiqueta_rama(ax, x, y, texto, ha="center"):
    """Etiqueta de probabilidad sobre una rama."""
    ax.text(x, y, texto,
            ha=ha, va="center",
            fontsize=9.5, color=COLOR_PROB_RAMA,
            fontweight="semibold", zorder=5)


def caja_prob_final(ax, cx, cy, texto):
    """Caja azul claro con la probabilidad final."""
    w, h = 1.3, 0.32
    x = cx - w / 2
    y = cy - h / 2
    fancy = FancyBboxPatch(
        (x, y), w, h,
        boxstyle="round,pad=0.02",
        facecolor=COLOR_PROB_FINAL_FONDO,
        edgecolor=COLOR_PROB_FINAL_BORDE,
        linewidth=1.2,
        zorder=3
    )
    ax.add_patch(fancy)
    ax.text(cx, cy, texto,
            ha="center", va="center",
            fontsize=8.5, color=COLOR_PROB_FINAL_TEXTO,
            fontweight="bold", zorder=4)


# =============================================================================
# POSICIONES (sistema de coordenadas normalizado)
# =============================================================================
#
#   X: 1.0  2.5  4.5  6.5  8.0  9.5  11.0
#   Y: 7.5 (raíz) → 5.5 (nivel 1) → 3.0 y 3.0 (nivel 2) → 1.0 (prob final)
#
# Nivel 0 — Raíz
X_RAIZ, Y_RAIZ = 6.0, 7.2

# Nivel 1
X_L1_IZQ, Y_L1 = 3.0, 5.0
X_L1_DER        = 9.0

# Nivel 2
Y_L2 = 2.8
X_L2_LL_IZQ = 1.5
X_L2_LL_DER = 4.0
X_L2_NL_IZQ = 8.0
X_L2_NL_DER = 10.5

# Probabilidades finales
Y_FINAL = 1.2

# Dimensiones nodos
W_L1, H_L1    = 1.9, 0.55
W_L2, H_L2    = 1.7, 0.52
RX_RAIZ, RY_RAIZ = 1.0, 0.38

# =============================================================================
# CONSTRUCCIÓN DEL GRÁFICO
# =============================================================================

fig, ax = plt.subplots(figsize=(12, 8), dpi=150)
ax.set_xlim(0, 12)
ax.set_ylim(0.4, 8.2)
ax.axis("off")
fig.patch.set_facecolor("white")

# --- Título ---
ax.set_title("Árbol de Probabilidad", fontsize=15, fontweight="bold",
             color="#1a1a1a", pad=12)

# -----------------------------------------------------------------------
# FLECHAS (primero, para quedar bajo los nodos)
# -----------------------------------------------------------------------

# Raíz → Nivel 1
flecha(ax, X_RAIZ, Y_RAIZ - RY_RAIZ - 0.02,
           X_L1_IZQ, Y_L1 + H_L1 / 2 + 0.02)
flecha(ax, X_RAIZ, Y_RAIZ - RY_RAIZ - 0.02,
           X_L1_DER, Y_L1 + H_L1 / 2 + 0.02)

# Llueve → Nivel 2
flecha(ax, X_L1_IZQ, Y_L1 - H_L1 / 2 - 0.02,
           X_L2_LL_IZQ, Y_L2 + H_L2 / 2 + 0.02)
flecha(ax, X_L1_IZQ, Y_L1 - H_L1 / 2 - 0.02,
           X_L2_LL_DER, Y_L2 + H_L2 / 2 + 0.02)

# No llueve → Nivel 2
flecha(ax, X_L1_DER, Y_L1 - H_L1 / 2 - 0.02,
           X_L2_NL_IZQ, Y_L2 + H_L2 / 2 + 0.02)
flecha(ax, X_L1_DER, Y_L1 - H_L1 / 2 - 0.02,
           X_L2_NL_DER, Y_L2 + H_L2 / 2 + 0.02)

# -----------------------------------------------------------------------
# NODOS
# -----------------------------------------------------------------------

# Raíz (elipse)
nodo_elipse(ax, X_RAIZ, Y_RAIZ, RX_RAIZ, RY_RAIZ,
            LABEL_RAIZ, COLOR_RAIZ_FONDO, COLOR_RAIZ_BORDE,
            fontsize=12, bold=True)

# Nivel 1
nodo_rect(ax, X_L1_IZQ, Y_L1, W_L1, H_L1,
          LABEL_L1_IZQ, COLOR_LLUEVE_FONDO, COLOR_LLUEVE_BORDE,
          fontsize=10, bold=True)
nodo_rect(ax, X_L1_DER, Y_L1, W_L1, H_L1,
          LABEL_L1_DER, COLOR_NOLUEVE_FONDO, COLOR_NOLUEVE_BORDE,
          fontsize=10, bold=True)

# Nivel 2 — rama Llueve
nodo_rect(ax, X_L2_LL_IZQ, Y_L2, W_L2, H_L2,
          LABEL_L2_IZQ_L, COLOR_HOJA_L_FONDO, COLOR_HOJA_L_BORDE,
          fontsize=9.5)
nodo_rect(ax, X_L2_LL_DER, Y_L2, W_L2, H_L2,
          LABEL_L2_DER_L, COLOR_HOJA_L_FONDO, COLOR_HOJA_L_BORDE,
          fontsize=9.5)

# Nivel 2 — rama No llueve
nodo_rect(ax, X_L2_NL_IZQ, Y_L2, W_L2, H_L2,
          LABEL_L2_IZQ_R, COLOR_HOJA_R_FONDO, COLOR_HOJA_R_BORDE,
          fontsize=9.5)
nodo_rect(ax, X_L2_NL_DER, Y_L2, W_L2, H_L2,
          LABEL_L2_DER_R, COLOR_HOJA_R_FONDO, COLOR_HOJA_R_BORDE,
          fontsize=9.5)

# -----------------------------------------------------------------------
# ETIQUETAS DE PROBABILIDAD EN RAMAS
# -----------------------------------------------------------------------

# Raíz → Llueve (izquierda-arriba)
mid_x = (X_RAIZ + X_L1_IZQ) / 2 - 0.15
mid_y = (Y_RAIZ - RY_RAIZ + Y_L1 + H_L1 / 2) / 2 + 0.08
etiqueta_rama(ax, mid_x - 0.3, mid_y, f"P = {fmt(P_L1_IZQ)}", ha="right")

# Raíz → No llueve (derecha-arriba)
mid_x = (X_RAIZ + X_L1_DER) / 2 + 0.15
mid_y = (Y_RAIZ - RY_RAIZ + Y_L1 + H_L1 / 2) / 2 + 0.08
etiqueta_rama(ax, mid_x + 0.3, mid_y, f"P = {fmt(P_L1_DER)}", ha="left")

# Llueve → Tráfico
mid_x = (X_L1_IZQ + X_L2_LL_IZQ) / 2
mid_y = (Y_L1 - H_L1 / 2 + Y_L2 + H_L2 / 2) / 2
etiqueta_rama(ax, mid_x - 0.5, mid_y, f"P = {fmt(P_L2_IZQ_L)}", ha="right")

# Llueve → Sin tráfico
mid_x = (X_L1_IZQ + X_L2_LL_DER) / 2
mid_y = (Y_L1 - H_L1 / 2 + Y_L2 + H_L2 / 2) / 2
etiqueta_rama(ax, mid_x + 0.35, mid_y, f"P = {fmt(P_L2_DER_L)}", ha="left")

# No llueve → Tráfico
mid_x = (X_L1_DER + X_L2_NL_IZQ) / 2
mid_y = (Y_L1 - H_L1 / 2 + Y_L2 + H_L2 / 2) / 2
etiqueta_rama(ax, mid_x - 0.35, mid_y, f"P = {fmt(P_L2_IZQ_R)}", ha="right")

# No llueve → Sin tráfico
mid_x = (X_L1_DER + X_L2_NL_DER) / 2
mid_y = (Y_L1 - H_L1 / 2 + Y_L2 + H_L2 / 2) / 2
etiqueta_rama(ax, mid_x + 0.5, mid_y, f"P = {fmt(P_L2_DER_R)}", ha="left")

# -----------------------------------------------------------------------
# PROBABILIDADES FINALES (cajas azules debajo de hojas)
# -----------------------------------------------------------------------

def texto_final(p_rama, p_cond, p_final):
    return f"{fmt(p_rama)}×{fmt(p_cond)} = {fmt2(p_final)}"

caja_prob_final(ax, X_L2_LL_IZQ, Y_FINAL,
                texto_final(P_L1_IZQ, P_L2_IZQ_L, P_FINAL_LL_TR))
caja_prob_final(ax, X_L2_LL_DER, Y_FINAL,
                texto_final(P_L1_IZQ, P_L2_DER_L, P_FINAL_LL_ST))
caja_prob_final(ax, X_L2_NL_IZQ, Y_FINAL,
                texto_final(P_L1_DER, P_L2_IZQ_R, P_FINAL_NL_TR))
caja_prob_final(ax, X_L2_NL_DER, Y_FINAL,
                texto_final(P_L1_DER, P_L2_DER_R, P_FINAL_NL_ST))

# Líneas punteadas verticales desde hojas hasta cajas finales
for xh in [X_L2_LL_IZQ, X_L2_LL_DER, X_L2_NL_IZQ, X_L2_NL_DER]:
    ax.plot([xh, xh],
            [Y_L2 - H_L2 / 2 - 0.02, Y_FINAL + 0.16],
            linestyle=":", color="#aaaaaa", linewidth=1.0, zorder=1)

# -----------------------------------------------------------------------
# LEYENDA DISCRETA
# -----------------------------------------------------------------------
leyenda_items = [
    mpatches.Patch(facecolor=COLOR_RAIZ_FONDO, edgecolor=COLOR_RAIZ_BORDE,
                   linewidth=1.5, label="Nodo raíz"),
    mpatches.Patch(facecolor=COLOR_LLUEVE_FONDO, edgecolor=COLOR_LLUEVE_BORDE,
                   linewidth=1.5, label="Rama: Llueve"),
    mpatches.Patch(facecolor=COLOR_NOLUEVE_FONDO, edgecolor=COLOR_NOLUEVE_BORDE,
                   linewidth=1.5, label="Rama: No llueve"),
    mpatches.Patch(facecolor=COLOR_PROB_FINAL_FONDO, edgecolor=COLOR_PROB_FINAL_BORDE,
                   linewidth=1.0, label="Probabilidad conjunta"),
]
ax.legend(handles=leyenda_items, loc="lower center",
          bbox_to_anchor=(0.5, -0.02), ncol=4,
          frameon=True, fontsize=8.5,
          edgecolor="#cccccc", facecolor="white")

# -----------------------------------------------------------------------
# GUARDAR
# -----------------------------------------------------------------------
directorio = os.path.dirname(os.path.abspath(__file__))
ruta_salida = os.path.join(directorio, NOMBRE_SALIDA)

plt.tight_layout(pad=0.5)
plt.savefig(ruta_salida, dpi=150, bbox_inches="tight",
            facecolor="white", edgecolor="none")
plt.close()

print(f"PNG guardado en: {ruta_salida}")
