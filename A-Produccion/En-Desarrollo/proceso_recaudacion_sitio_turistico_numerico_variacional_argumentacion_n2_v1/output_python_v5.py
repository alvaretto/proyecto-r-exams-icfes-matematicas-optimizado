# -*- coding: utf-8 -*-
"""
Diagrama de Flujo - Proceso de Recaudación Sitio Turístico
Versión Python/matplotlib v5 - Círculos pequeños ajuste fino
"""

import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Circle

def generar_diagrama_flujo(precio_reserva=22.5, precio_sin_reserva=17,
                           output_file="python_output_v5.png"):

    precio_reserva_fmt = str(precio_reserva).replace('.', ',')
    precio_sin_reserva_fmt = str(precio_sin_reserva).replace('.', ',')

    fig, ax = plt.subplots(1, 1, figsize=(11, 3.5))
    ax.set_xlim(0, 11)
    ax.set_ylim(0, 3.5)
    ax.set_aspect('equal')
    ax.axis('off')

    color_caja = '#EDF7FA'
    color_borde = '#4A90A4'
    color_circulo = '#E67E22'
    color_flecha = '#4A90A4'

    def crear_caja(x, y, width, height, texto):
        caja = FancyBboxPatch((x, y), width, height,
                               boxstyle="round,pad=0.01,rounding_size=0.05",
                               facecolor=color_caja, edgecolor=color_borde, linewidth=1.2)
        ax.add_patch(caja)
        ax.text(x + width/2, y + height/2, texto,
                ha='center', va='center', fontsize=7.5,
                multialignment='center', linespacing=1.2)

    def crear_circulo(x, y, numero, radius=0.18):
        circulo = Circle((x, y), radius, facecolor=color_circulo, edgecolor=color_circulo)
        ax.add_patch(circulo)
        ax.text(x, y, str(numero), ha='center', va='center',
                fontsize=9, fontweight='bold', color='white')

    def crear_circulo_peq(x, y, numero, radius=0.09):
        circulo = Circle((x, y), radius, facecolor=color_circulo, edgecolor=color_circulo)
        ax.add_patch(circulo)
        ax.text(x, y, str(numero), ha='center', va='center',
                fontsize=5, fontweight='bold', color='white')

    y_superior = 2.2
    y_inferior = 0.7
    y_medio = (y_superior + y_inferior) / 2 + 0.35

    ancho_caja1 = 2.0
    alto_caja = 0.85
    ancho_caja2 = 2.3
    ancho_caja3 = 1.2

    # === FILA SUPERIOR ===
    crear_circulo(0.4, y_superior + alto_caja/2, 1)
    x1_sup = 0.75
    crear_caja(x1_sup, y_superior, ancho_caja1, alto_caja,
               "Sumar la cantidad de\npersonas que entraron con\nreserva durante la semana.")

    crear_circulo(3.1, y_superior + alto_caja/2, 2)
    x2_sup = 3.45
    crear_caja(x2_sup, y_superior, ancho_caja2, alto_caja,
               f"Multiplicar la cantidad\nobtenida en el paso    por\n{precio_reserva_fmt}.")
    # Círculo pequeño "1" - ajuste fino (+0.48 en lugar de +0.42)
    crear_circulo_peq(x2_sup + ancho_caja2/2 + 0.48, y_superior + alto_caja/2, 1)

    # === FILA INFERIOR ===
    crear_circulo(0.4, y_inferior + alto_caja/2, 1)
    crear_caja(x1_sup, y_inferior, ancho_caja1, alto_caja,
               "Sumar la cantidad de\npersonas que entraron sin\nreserva durante la semana.")

    crear_circulo(3.1, y_inferior + alto_caja/2, 2)
    crear_caja(x2_sup, y_inferior, ancho_caja2, alto_caja,
               f"Multiplicar la cantidad\nobtenida en el paso    por\n{precio_sin_reserva_fmt}.")
    # Círculo pequeño "1" - ajuste fino
    crear_circulo_peq(x2_sup + ancho_caja2/2 + 0.48, y_inferior + alto_caja/2, 1)

    # === PASO 3 ===
    x3 = 7.8
    crear_caja(x3, y_medio - alto_caja/2, ancho_caja3, alto_caja, "Comparar los\nresultados")
    crear_circulo(7.45, y_medio - 0.2, 3)

    # === FLECHAS ===
    ax.annotate('', xy=(2.92, y_superior + alto_caja/2),
                xytext=(x1_sup + ancho_caja1, y_superior + alto_caja/2),
                arrowprops=dict(arrowstyle='->', color=color_flecha, lw=1.2))
    ax.annotate('', xy=(2.92, y_inferior + alto_caja/2),
                xytext=(x1_sup + ancho_caja1, y_inferior + alto_caja/2),
                arrowprops=dict(arrowstyle='->', color=color_flecha, lw=1.2))

    x_conv = 6.8
    ax.plot([x2_sup + ancho_caja2, x_conv], [y_superior + alto_caja/2]*2, color=color_flecha, lw=1.2)
    ax.plot([x_conv]*2, [y_superior + alto_caja/2, y_medio], color=color_flecha, lw=1.2)
    ax.plot([x2_sup + ancho_caja2, x_conv], [y_inferior + alto_caja/2]*2, color=color_flecha, lw=1.2)
    ax.plot([x_conv]*2, [y_inferior + alto_caja/2, y_medio], color=color_flecha, lw=1.2)
    ax.annotate('', xy=(x3, y_medio), xytext=(x_conv, y_medio),
                arrowprops=dict(arrowstyle='->', color=color_flecha, lw=1.2))

    plt.tight_layout()
    plt.savefig(output_file, dpi=150, bbox_inches='tight', facecolor='white', edgecolor='none')
    plt.close()
    return output_file

if __name__ == "__main__":
    output = generar_diagrama_flujo(22.5, 17, "python_output_v5.png")
    print(f"Diagrama generado: {output}")
