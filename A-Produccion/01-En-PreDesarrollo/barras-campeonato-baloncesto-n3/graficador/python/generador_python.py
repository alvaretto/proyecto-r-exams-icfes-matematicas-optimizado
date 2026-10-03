# -*- coding: utf-8 -*-
"""Graficador Flujo B (Python/matplotlib) - barras-campeonato-baloncesto-n3.
Todo entra por parametro; sin valores del original dentro. Uso desde R: reticulate::source_python()."""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Patch

W_IN, H_IN, DPI = 6.0, 4.2, 150   # tamano fijo: 900 x 630 px
GRIS = "#C8C8C8"
TXT = "#222222"


def _fmt(v):
    v = float(v)
    return str(int(v)) if v == int(v) else str(v).replace(".", ",")


def _ejes(fig, rect, ymax, etiqueta_y):
    ax = fig.add_axes(rect)
    paso = 1 if ymax <= 15 else 2
    ax.set_ylim(0, ymax)
    ax.set_yticks(np.arange(0, ymax + 1, paso))
    ax.yaxis.grid(True, color=GRIS, linewidth=0.8)
    ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    for s in ("left", "bottom"):
        ax.spines[s].set_linewidth(1.2)
        ax.spines[s].set_color(TXT)
    ax.tick_params(axis="y", labelsize=10, colors=TXT, length=3)
    ax.tick_params(axis="x", labelsize=11, colors=TXT, length=0, pad=6)
    ax.set_ylabel(etiqueta_y, fontsize=13, fontweight="bold", color=TXT, labelpad=6)
    return ax


def barras_simples(valores, categorias, colores, titulo, etiqueta_y, archivo_png):
    valores = [float(v) for v in np.asarray(valores).ravel()]
    ymax = int(np.ceil(max(valores)))
    fig = plt.figure(figsize=(W_IN, H_IN), dpi=DPI, facecolor="white")
    ax = _ejes(fig, [0.14, 0.12, 0.83, 0.62], ymax, etiqueta_y)
    ax.set_xlim(-0.5, 1.5)
    ax.bar([0, 1], valores, width=0.34, color=list(colores), linewidth=0)
    ax.set_xticks([0, 1])
    ax.set_xticklabels(list(categorias))
    fig.text(0.56, 0.96, titulo, ha="center", va="top", fontsize=14,
             fontweight="bold", color=TXT, linespacing=1.25)
    fig.savefig(archivo_png, dpi=DPI, facecolor="white")
    plt.close(fig)
    return archivo_png


def barras_opcion(matriz, grupos, categorias, colores, modo, titulo, etiqueta_y, archivo_png, ymax=None):
    m = np.asarray(matriz, dtype=float).reshape(2, 2)   # [grupo x categoria]
    if ymax is None:   # por defecto 0..max (total si apilada); el impreso de B usa 13
        ymax = int(np.ceil(m.sum(axis=0).max() if modo == "apilada" else m.max()))
    fig = plt.figure(figsize=(W_IN, H_IN), dpi=DPI, facecolor="white")
    ax = _ejes(fig, [0.12, 0.11, 0.62, 0.74], ymax, etiqueta_y)
    ax.set_xlim(-0.55, 1.55)
    if modo == "apilada":
        w = 0.37
        for c in range(2):
            ax.bar(c, m[1, c], width=w, color=colores[1], linewidth=0)             # grupo 2 abajo
            ax.bar(c, m[0, c], width=w, bottom=m[1, c], color=colores[0], linewidth=0)  # grupo 1 arriba
    else:
        w = 0.36
        for c in range(2):
            ax.bar(c - w / 2, m[0, c], width=w, color=colores[0], linewidth=0)
            ax.bar(c + w / 2, m[1, c], width=w, color=colores[1], linewidth=0)
    ax.set_xticks([0, 1])
    ax.set_xticklabels(list(categorias))
    fig.text(0.40, 0.96, titulo, ha="center", va="top", fontsize=13,
             fontweight="bold", color=TXT)
    fig.legend(handles=[Patch(facecolor=colores[0], label=grupos[0]),
                        Patch(facecolor=colores[1], label=grupos[1])],
               loc="center left", bbox_to_anchor=(0.76, 0.50), frameon=False,
               fontsize=10, handlelength=1.0, handleheight=1.0, labelspacing=0.7)
    fig.savefig(archivo_png, dpi=DPI, facecolor="white")
    plt.close(fig)
    return archivo_png
