#!/usr/bin/env python3
"""
GENERADOR DE COTANGENTE CON PYTHON
Genera código TikZ preciso para una porción de función cotangente
Basado en cálculos matemáticos exactos
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
import math

def generar_cotangente_tikz(x_min=0.1, x_max=4.5, y_min=0, y_max=5, 
                           discontinuidades=[np.pi/2, np.pi, 3*np.pi/2],
                           num_puntos=100):
    """
    Genera código TikZ para una porción de cotangente con discontinuidades
    
    Args:
        x_min, x_max: Rango del eje X
        y_min, y_max: Rango del eje Y  
        discontinuidades: Lista de puntos donde hay discontinuidades
        num_puntos: Número de puntos por segmento
    """
    
    print("🐍 GENERADOR PYTHON DE COTANGENTE")
    print("=" * 40)
    print(f"📊 Rango X: [{x_min:.2f}, {x_max:.2f}]")
    print(f"📊 Rango Y: [{y_min:.2f}, {y_max:.2f}]")
    print(f"⚡ Discontinuidades en: {[f'{d:.3f}' for d in discontinuidades]}")
    
    # Crear segmentos entre discontinuidades
    segmentos = []
    puntos_discontinuidad = [x_min] + [d for d in discontinuidades if x_min < d < x_max] + [x_max]
    
    for i in range(len(puntos_discontinuidad) - 1):
        inicio = puntos_discontinuidad[i]
        fin = puntos_discontinuidad[i + 1]
        
        # Ajustar para evitar singularidades
        margen = 0.05
        if i > 0:  # No es el primer segmento
            inicio += margen
        if i < len(puntos_discontinuidad) - 2:  # No es el último segmento
            fin -= margen
            
        if fin > inicio:
            segmentos.append((inicio, fin))
    
    print(f"📈 Segmentos calculados: {len(segmentos)}")
    
    # Generar coordenadas para cada segmento
    coordenadas_segmentos = []
    
    for i, (inicio, fin) in enumerate(segmentos):
        print(f"   Segmento {i+1}: [{inicio:.3f}, {fin:.3f}]")
        
        # Generar puntos X uniformemente distribuidos
        x_vals = np.linspace(inicio, fin, num_puntos)
        
        # Calcular cotangente: cot(x) = cos(x)/sin(x) = 1/tan(x)
        y_vals = []
        x_vals_validos = []
        
        for x in x_vals:
            try:
                # Calcular cotangente
                cot_val = 1.0 / np.tan(x)
                
                # Filtrar valores dentro del rango Y
                if y_min <= cot_val <= y_max:
                    y_vals.append(cot_val)
                    x_vals_validos.append(x)
                elif cot_val > y_max:
                    # Si es muy alto, usar el máximo
                    y_vals.append(y_max)
                    x_vals_validos.append(x)
                    
            except (ZeroDivisionError, ValueError):
                # Saltar puntos problemáticos cerca de singularidades
                continue
        
        if len(x_vals_validos) > 0:
            coordenadas_segmentos.append(list(zip(x_vals_validos, y_vals)))
            print(f"      → {len(x_vals_validos)} puntos válidos")
    
    return coordenadas_segmentos

def coordenadas_a_tikz(coordenadas_segmentos, color="cyan"):
    """
    Convierte coordenadas a código TikZ
    """
    codigo_tikz = []
    
    for i, coordenadas in enumerate(coordenadas_segmentos):
        codigo_tikz.append(f"% Segmento {i+1} de la cotangente")
        codigo_tikz.append(f"\\addplot[{color}, very thick, smooth] coordinates {{")
        
        # Formatear coordenadas
        for x, y in coordenadas[::5]:  # Tomar cada 5to punto para no sobrecargar
            codigo_tikz.append(f"    ({x:.3f},{y:.3f})")
        
        codigo_tikz.append("};")
        codigo_tikz.append("")
    
    return "\n".join(codigo_tikz)

def generar_tikz_completo():
    """
    Genera el documento TikZ completo
    """
    
    # Generar coordenadas de cotangente
    coordenadas = generar_cotangente_tikz()
    
    # Convertir a código TikZ
    codigo_curva = coordenadas_a_tikz(coordenadas)
    
    # Plantilla completa
    tikz_completo = f"""\\documentclass[border=2mm]{{standalone}}
\\usepackage{{tikz}}
\\usepackage{{pgfplots}}
\\pgfplotsset{{compat=1.18}}

\\begin{{document}}

% COTANGENTE GENERADA CON PYTHON - Cálculos matemáticos exactos
% Función: cot(x) = 1/tan(x) = cos(x)/sin(x)

\\begin{{tikzpicture}}[scale=1.0]
\\begin{{axis}}[
    xlabel={{Ángulo $\\alpha$}},
    ylabel={{Distancia PK}},
    xmin=0, xmax=4.5,
    ymin=0, ymax=5,
    axis lines=left,
    xtick=\\empty,
    ytick=\\empty,
    clip=false
]

{codigo_curva}

% Líneas punteadas verticales (posiciones observadas en imagen original)
\\draw[dashed, black, thick] (axis cs:1.2,0) -- (axis cs:1.2,5) 
    node[left, black, yshift=8pt] {{\\footnotesize $QP$}};
\\draw[dashed, black, thick] (axis cs:2.8,0) -- (axis cs:2.8,5) 
    node[left, black, yshift=8pt] {{\\footnotesize $I_1$}};

% Flecha vertical cyan (elemento distintivo de la imagen)
\\draw[cyan, very thick, -stealth] (axis cs:0.25,4.2) -- (axis cs:0.25,4.8);

% Punto destacado en la curva
\\addplot[cyan, mark=*, mark size=2pt, only marks] coordinates {{(2.8,0.3)}};

\\end{{axis}}
\\end{{tikzpicture}}

\\end{{document}}"""
    
    return tikz_completo

def visualizar_cotangente():
    """
    Crear visualización con matplotlib para validar
    """
    print("\n📊 GENERANDO VISUALIZACIÓN PARA VALIDACIÓN")
    
    # Generar coordenadas
    coordenadas = generar_cotangente_tikz()
    
    # Crear gráfico
    fig, ax = plt.subplots(1, 1, figsize=(10, 6))
    
    # Plotear cada segmento
    colores = ['cyan', 'blue', 'teal']
    for i, coords in enumerate(coordenadas):
        if coords:
            x_vals, y_vals = zip(*coords)
            ax.plot(x_vals, y_vals, color=colores[i % len(colores)], 
                   linewidth=3, label=f'Segmento {i+1}')
    
    # Configurar ejes
    ax.set_xlim(0, 4.5)
    ax.set_ylim(0, 5)
    ax.set_xlabel('Ángulo α', fontsize=12)
    ax.set_ylabel('Distancia PK', fontsize=12)
    ax.grid(True, alpha=0.3)
    
    # Líneas punteadas
    ax.axvline(x=1.2, color='black', linestyle='--', alpha=0.7)
    ax.axvline(x=2.8, color='black', linestyle='--', alpha=0.7)
    ax.text(1.2, 4.5, 'QP', fontsize=10, ha='center')
    ax.text(2.8, 4.5, 'I₁', fontsize=10, ha='center')
    
    # Flecha vertical
    ax.arrow(0.25, 4.2, 0, 0.6, head_width=0.05, head_length=0.1, 
             fc='cyan', ec='cyan', linewidth=2)
    
    ax.set_title('Cotangente - Validación Python', fontsize=14, fontweight='bold')
    ax.legend()
    
    plt.tight_layout()
    return fig

if __name__ == "__main__":
    print("🚀 EJECUTANDO GENERADOR DE COTANGENTE")
    
    # Generar código TikZ
    codigo_tikz = generar_tikz_completo()
    
    # Guardar archivo
    with open("cotangente_python_generado.tex", "w", encoding="utf-8") as f:
        f.write(codigo_tikz)
    
    print("✅ Archivo generado: cotangente_python_generado.tex")
    
    # Crear visualización
    fig = visualizar_cotangente()
    fig.savefig("cotangente_python_validacion.png", dpi=150, bbox_inches='tight')
    print("✅ Visualización guardada: cotangente_python_validacion.png")
    
    # Mostrar parte del código generado
    print("\n📝 CÓDIGO TIKZ GENERADO (muestra):")
    print("=" * 50)
    lineas = codigo_tikz.split('\n')
    for i, linea in enumerate(lineas[15:35]):  # Mostrar parte del código
        print(f"{i+16:2d}: {linea}")
    print("    ...")
    
    print(f"\n🎯 TOTAL: {len(lineas)} líneas de código TikZ generadas")
    print("🔧 Listo para compilar con: pdflatex cotangente_python_generado.tex")
