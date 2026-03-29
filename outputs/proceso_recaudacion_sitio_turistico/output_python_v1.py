# -*- coding: utf-8 -*-
"""
Diagrama de Flujo - Proceso de Recaudación Sitio Turístico
Versión Python/matplotlib compatible con R-exams (reticulate)
"""

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch, Circle, FancyArrowPatch
import numpy as np

def generar_diagrama_flujo(precio_reserva=22.5, precio_sin_reserva=17,
                           output_file="python_output_v1.png"):
    """
    Genera el diagrama de flujo del proceso de recaudación.

    Args:
        precio_reserva: Precio de entrada con reserva
        precio_sin_reserva: Precio de entrada sin reserva
        output_file: Nombre del archivo de salida
    """

    # Formatear precios con coma decimal (español)
    precio_reserva_fmt = str(precio_reserva).replace('.', ',')
    precio_sin_reserva_fmt = str(precio_sin_reserva).replace('.', ',')

    # Crear figura
    fig, ax = plt.subplots(1, 1, figsize=(12, 4))
    ax.set_xlim(0, 12)
    ax.set_ylim(0, 4)
    ax.set_aspect('equal')
    ax.axis('off')

    # Colores
    color_caja = '#E8F4F8'
    color_borde = '#5DADE2'
    color_circulo = '#E67E22'
    color_flecha = '#5DADE2'

    # Función para crear caja de proceso
    def crear_caja(x, y, width, height, texto):
        caja = FancyBboxPatch((x, y), width, height,
                               boxstyle="round,pad=0.02,rounding_size=0.1",
                               facecolor=color_caja,
                               edgecolor=color_borde,
                               linewidth=1.5)
        ax.add_patch(caja)
        ax.text(x + width/2, y + height/2, texto,
                ha='center', va='center', fontsize=8,
                wrap=True, multialignment='center')
        return caja

    # Función para crear círculo numerado
    def crear_circulo(x, y, numero, radius=0.2):
        circulo = Circle((x, y), radius, facecolor=color_circulo,
                         edgecolor=color_circulo, linewidth=1)
        ax.add_patch(circulo)
        ax.text(x, y, str(numero), ha='center', va='center',
                fontsize=10, fontweight='bold', color='white')

    # Posiciones Y
    y_superior = 2.5
    y_inferior = 0.8
    y_medio = (y_superior + y_inferior) / 2 + 0.3

    # Dimensiones cajas
    ancho_caja1 = 2.2
    alto_caja = 0.9
    ancho_caja2 = 2.5
    ancho_caja3 = 1.4

    # === FILA SUPERIOR (Con reserva) ===
    # Círculo 1
    crear_circulo(0.5, y_superior + alto_caja/2, 1)

    # Caja 1 superior
    x1_sup = 0.9
    crear_caja(x1_sup, y_superior, ancho_caja1, alto_caja,
               "Sumar la cantidad de\npersonas que entraron con\nreserva durante la semana.")

    # Círculo 2 superior
    crear_circulo(3.5, y_superior + alto_caja/2, 2)

    # Caja 2 superior
    x2_sup = 3.9
    crear_caja(x2_sup, y_superior, ancho_caja2, alto_caja,
               f"Multiplicar la cantidad\nobtenida en el paso ① por\n{precio_reserva_fmt}.")

    # === FILA INFERIOR (Sin reserva) ===
    # Círculo 1
    crear_circulo(0.5, y_inferior + alto_caja/2, 1)

    # Caja 1 inferior
    crear_caja(x1_sup, y_inferior, ancho_caja1, alto_caja,
               "Sumar la cantidad de\npersonas que entraron sin\nreserva durante la semana.")

    # Círculo 2 inferior
    crear_circulo(3.5, y_inferior + alto_caja/2, 2)

    # Caja 2 inferior
    crear_caja(x2_sup, y_inferior, ancho_caja2, alto_caja,
               f"Multiplicar la cantidad\nobtenida en el paso ① por\n{precio_sin_reserva_fmt}.")

    # === PASO 3 (Resultado) ===
    # Caja 3
    x3 = 8.5
    crear_caja(x3, y_medio - alto_caja/2, ancho_caja3, alto_caja,
               "Comparar los\nresultados")

    # Círculo 3 (posicionado a la izquierda, ligeramente abajo)
    crear_circulo(8.1, y_medio - 0.25, 3)

    # === FLECHAS ===
    # Flecha de caja1 a círculo2 (superior)
    ax.annotate('', xy=(3.3, y_superior + alto_caja/2),
                xytext=(x1_sup + ancho_caja1, y_superior + alto_caja/2),
                arrowprops=dict(arrowstyle='->', color=color_flecha, lw=1.5))

    # Flecha de caja1 a círculo2 (inferior)
    ax.annotate('', xy=(3.3, y_inferior + alto_caja/2),
                xytext=(x1_sup + ancho_caja1, y_inferior + alto_caja/2),
                arrowprops=dict(arrowstyle='->', color=color_flecha, lw=1.5))

    # Flechas convergentes a caja3
    # Desde caja2 superior
    ax.plot([x2_sup + ancho_caja2, 7.5, 7.5],
            [y_superior + alto_caja/2, y_superior + alto_caja/2, y_medio],
            color=color_flecha, lw=1.5)
    ax.annotate('', xy=(x3, y_medio),
                xytext=(7.5, y_medio),
                arrowprops=dict(arrowstyle='->', color=color_flecha, lw=1.5))

    # Desde caja2 inferior
    ax.plot([x2_sup + ancho_caja2, 7.5, 7.5],
            [y_inferior + alto_caja/2, y_inferior + alto_caja/2, y_medio],
            color=color_flecha, lw=1.5)

    plt.tight_layout()
    plt.savefig(output_file, dpi=150, bbox_inches='tight',
                facecolor='white', edgecolor='none')
    plt.close()

    return output_file

# Ejecutar si es script principal
if __name__ == "__main__":
    output = generar_diagrama_flujo(22.5, 17, "python_output_v1.png")
    print(f"Diagrama generado: {output}")
