import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Arc

def dibujar_parabrisa(ax, ancho, alto, plumilla_izq, plumilla_der):
    """
    Dibuja un parabrisa con sus plumillas.
    
    Parámetros:
    - ax: El subplot donde dibujar
    - ancho: Ancho del parabrisa en cm (dimensión horizontal)
    - alto: Alto del parabrisa en cm (dimensión vertical)
    - plumilla_izq: Radio de la plumilla izquierda en cm
    - plumilla_der: Radio de la plumilla derecha en cm
    """
    # Dibujar el parabrisa (rectángulo)
    ax.plot([0, ancho, ancho, 0, 0], [0, 0, alto, alto, 0], 'r-', linewidth=1.5)
    
    # Posición de las plumillas (esquinas inferiores)
    pos_izq = (0, 0)
    pos_der = (ancho, 0)
    
    # Dibujar la plumilla izquierda (arco de 90 grados)
    arco_izq = Arc(pos_izq, 2*plumilla_izq, 2*plumilla_izq, 
                  theta1=0, theta2=90, color='black', linewidth=1.5)
    ax.add_patch(arco_izq)
    
    # Líneas de referencia para la plumilla izquierda
    ax.plot([pos_izq[0], pos_izq[0] + plumilla_izq], [pos_izq[1], pos_izq[1]], 'k--', alpha=0.7)
    ax.plot([pos_izq[0], pos_izq[0]], [pos_izq[1], pos_izq[1] + plumilla_izq], 'k--', alpha=0.7)
    
    # Dibujar la plumilla derecha (arco de 90 grados invertido)
    arco_der = Arc(pos_der, 2*plumilla_der, 2*plumilla_der, 
                  theta1=90, theta2=180, color='black', linewidth=1.5)
    ax.add_patch(arco_der)
    
    # Líneas de referencia para la plumilla derecha
    ax.plot([pos_der[0], pos_der[0] - plumilla_der], [pos_der[1], pos_der[1]], 'k--', alpha=0.7)
    ax.plot([pos_der[0], pos_der[0]], [pos_der[1], pos_der[1] + plumilla_der], 'k--', alpha=0.7)
    
    # Configurar los ejes
    ax.set_xlim(-1, ancho + 1)
    ax.set_ylim(-1, alto + 1)
    ax.set_aspect('equal')
    ax.axis('off')  # Ocultar ejes
    
    # Agregar dimensiones del parabrisa
    ax.text(ancho/2, -0.5, f"{ancho} cm", ha='center', va='top', fontsize=9, color='red')
    ax.text(-0.5, alto/2, f"{alto} cm", ha='right', va='center', fontsize=9, color='red', rotation=90)
    
    # Agregar dimensiones de las plumillas
    ax.text(plumilla_izq/2, -0.3, f"{plumilla_izq} cm", ha='center', va='top', fontsize=9)
    ax.text(ancho - plumilla_der/2, -0.3, f"{plumilla_der} cm", ha='center', va='top', fontsize=9)

# Crear figura con 4 subplots para las 4 opciones
fig, axs = plt.subplots(2, 2, figsize=(10, 10))
plt.subplots_adjust(hspace=0.4, wspace=0.3)

# Configurar títulos de la figura principal
fig.suptitle('Dimensiones del Parabrisas y Configuración de Plumillas', fontsize=14, y=0.98)
plt.figtext(0.5, 0.92, 'El esquema que representa los datos de las dimensiones del parabrisas es:', 
           ha='center', fontsize=11)

# Esquema A (arriba izquierda)
axs[0, 0].set_title('① Esquema A', fontsize=12)
dibujar_parabrisa(axs[0, 0], 13, 8, 6, 6)

# Esquema B (arriba derecha)
axs[0, 1].set_title('② Esquema B', fontsize=12)
dibujar_parabrisa(axs[0, 1], 13, 8, 6, 8)

# Esquema C (abajo izquierda)
axs[1, 0].set_title('③ Esquema C', fontsize=12)
dibujar_parabrisa(axs[1, 0], 13, 8, 6, 6)

# Esquema D (abajo derecha)
axs[1, 1].set_title('④ Esquema D', fontsize=12)
dibujar_parabrisa(axs[1, 1], 13, 6, 8, 8)

plt.savefig('parabrisa_esquemas.png', dpi=300, bbox_inches='tight')
plt.show()

# Diagrama adicional de la plumilla con ángulo de apertura
fig2, ax2 = plt.subplots(figsize=(6, 4))
ax2.set_xlim(-1, 8)
ax2.set_ylim(-1, 8)
ax2.set_aspect('equal')
ax2.axis('off')

# Dibujar una plumilla de referencia
pos = (0, 0)
radio = 6
arco = Arc(pos, 2*radio, 2*radio, theta1=0, theta2=90, color='black', linewidth=2)
ax2.add_patch(arco)

# Líneas punteadas y etiqueta
ax2.plot([pos[0], pos[0] + radio], [pos[1], pos[1]], 'k--', alpha=0.7)
ax2.plot([pos[0], pos[0]], [pos[1], pos[1] + radio], 'k--', alpha=0.7)
ax2.text(2, 3, 'Plumilla', ha='center', fontsize=10)
ax2.text(3, 1.5, '90°', ha='center', fontsize=10)

plt.savefig('plumilla_detalle.png', dpi=300, bbox_inches='tight')
plt.show()
