# -*- coding: utf-8 -*-
"""
Diagrama de Flujo - Proceso de Recaudación Sitio Turístico
Versión Python/matplotlib v3 - Círculos pequeños corregidos
Compatible con R-exams (reticulate)
"""

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch, Circle
import numpy as np

def generar_diagrama_flujo(precio_reserva=22.5, precio_sin_reserva=17,
                           output_file="python_output_v3.png"):
    """
    Genera el diagrama de flujo del proceso de recaudación.
    """

    # Formatear precios con coma decimal (español)
    precio_reserva_fmt = str(precio_reserva).replace('.', ',')
    precio_sin_reserva_fmt = str(precio_sin_reserva).replace('.', ',')

    # Crear figura
    fig, ax = plt.subplots(1, 1, figsize=(11, 3.5))
    ax.set_xlim(0, 11)
    ax.set_ylim(0, 3.5)
    ax.set_aspect('equal')
    ax.axis('off')

    # Colores (más similares a TikZ)
    color_caja = '#EDF7FA'
    color_borde = '#4A90A4'
    color_circulo = '#E67E22'
    color_flecha = '#4A90A4'

    # Función para crear caja de proceso
    def crear_caja(x, y, width, height, texto):
        caja = FancyBboxPatch((x, y), width, height,
                               boxstyle="round,pad=0.01,rounding_size=0.05",
                               facecolor=color_caja,
                               edgecolor=color_borde,
                               linewidth=1.2)
        ax.add_patch(caja)
        ax.text(x + width/2, y + height/2, texto,
                ha='center', va='center', fontsize=7.5,
                wrap=True, multialignment='center',
                linespacing=1.2)

    # Función para crear círculo numerado grande (pasos principales)
    def crear_circulo(x, y, numero, radius=0.18):
        circulo = Circle((x, y), radius, facecolor=color_circulo,
                         edgecolor=color_circulo, linewidth=1)
        ax.add_patch(circulo)
        ax.text(x, y, str(numero), ha='center', va='center',
                fontsize=9, fontweight='bold', color='white')

    # Función para crear círculo pequeño (dentro del texto)
    def crear_circulo_peq(x, y, numero, radius=0.09):
        circulo = Circle((x, y), radius, facecolor=color_circulo,
                         edgecolor=color_circulo, linewidth=0.5)
        ax.add_patch(circulo)
        ax.text(x, y, str(numero), ha='center', va='center',
                fontsize=5, fontweight='bold', color='white')

    # Posiciones Y
    y_superior = 2.2
    y_inferior = 0.7
    y_medio = (y_superior + y_inferior) / 2 + 0.35

    # Dimensiones cajas
    ancho_caja1 = 2.0
    alto_caja = 0.85
    ancho_caja2 = 2.3
    ancho_caja3 = 1.2

    # === FILA SUPERIOR (Con reserva) ===
    crear_circulo(0.4, y_superior + alto_caja/2, 1)

    x1_sup = 0.75
    crear_caja(x1_sup, y_superior, ancho_caja1, alto_caja,
               "Sumar la cantidad de\npersonas que entraron con\nreserva durante la semana.")

    crear_circulo(3.1, y_superior + alto_caja/2, 2)

    x2_sup = 3.45
    # Caja 2 superior - texto con espacio para el círculo
    crear_caja(x2_sup, y_superior, ancho_caja2, alto_caja,
               f"Multiplicar la cantidad\nobtenida en el paso     por\n{precio_reserva_fmt}.")
    # Círculo pequeño "1" - ajustado a la posición correcta después de "paso"
    crear_circulo_peq(x2_sup + ancho_caja2/2 + 0.32, y_superior + alto_caja/2, 1)

    # === FILA INFERIOR (Sin reserva) ===
    crear_circulo(0.4, y_inferior + alto_caja/2, 1)

    crear_caja(x1_sup, y_inferior, ancho_caja1, alto_caja,
               "Sumar la cantidad de\npersonas que entraron sin\nreserva durante la semana.")

    crear_circulo(3.1, y_inferior + alto_caja/2, 2)

    # Caja 2 inferior - texto con espacio para el círculo
    crear_caja(x2_sup, y_inferior, ancho_caja2, alto_caja,
               f"Multiplicar la cantidad\nobtenida en el paso     por\n{precio_sin_reserva_fmt}.")
    # Círculo pequeño "1" - ajustado a la posición correcta después de "paso"
    crear_circulo_peq(x2_sup + ancho_caja2/2 + 0.32, y_inferior + alto_caja/2, 1)

    # === PASO 3 (Resultado) ===
    x3 = 7.8
    crear_caja(x3, y_medio - alto_caja/2, ancho_caja3, alto_caja,
               "Comparar los\nresultados")

    # Círculo 3 (posicionado como en TikZ v6)
    crear_circulo(7.45, y_medio - 0.2, 3)

    # === FLECHAS ===
    # Flecha de caja1 a círculo2 (superior)
    ax.annotate('', xy=(2.92, y_superior + alto_caja/2),
                xytext=(x1_sup + ancho_caja1, y_superior + alto_caja/2),
                arrowprops=dict(arrowstyle='->', color=color_flecha, lw=1.2))

    # Flecha de caja1 a círculo2 (inferior)
    ax.annotate('', xy=(2.92, y_inferior + alto_caja/2),
                xytext=(x1_sup + ancho_caja1, y_inferior + alto_caja/2),
                arrowprops=dict(arrowstyle='->', color=color_flecha, lw=1.2))

    # Flechas convergentes a caja3
    x_convergencia = 6.8

    # Desde caja2 superior
    ax.plot([x2_sup + ancho_caja2, x_convergencia],
            [y_superior + alto_caja/2, y_superior + alto_caja/2],
            color=color_flecha, lw=1.2)
    ax.plot([x_convergencia, x_convergencia],
            [y_superior + alto_caja/2, y_medio],
            color=color_flecha, lw=1.2)

    # Desde caja2 inferior
    ax.plot([x2_sup + ancho_caja2, x_convergencia],
            [y_inferior + alto_caja/2, y_inferior + alto_caja/2],
            color=color_flecha, lw=1.2)
    ax.plot([x_convergencia, x_convergencia],
            [y_inferior + alto_caja/2, y_medio],
            color=color_flecha, lw=1.2)

    # Flecha final a caja3
    ax.annotate('', xy=(x3, y_medio),
                xytext=(x_convergencia, y_medio),
                arrowprops=dict(arrowstyle='->', color=color_flecha, lw=1.2))

    plt.tight_layout()
    plt.savefig(output_file, dpi=150, bbox_inches='tight',
                facecolor='white', edgecolor='none')
    plt.close()

    return output_file

# Ejecutar si es script principal
if __name__ == "__main__":
    output = generar_diagrama_flujo(22.5, 17, "python_output_v3.png")
    print(f"Diagrama generado: {output}")
