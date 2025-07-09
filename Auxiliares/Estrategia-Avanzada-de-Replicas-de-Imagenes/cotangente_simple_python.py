#!/usr/bin/env python3
"""
GENERADOR SIMPLE DE COTANGENTE
Versión simplificada para generar código TikZ de cotangente
"""

import math

def generar_cotangente_simple():
    """
    Genera coordenadas de cotangente de manera simple y directa
    """
    print("🐍 GENERADOR SIMPLE DE COTANGENTE")
    print("=" * 35)
    
    # Parámetros básicos
    x_inicio = 0.1
    x_fin = 4.5
    num_puntos = 50
    
    # Discontinuidades aproximadas (π/2, π, 3π/2)
    disc1 = math.pi / 2  # ≈ 1.57
    disc2 = math.pi      # ≈ 3.14
    
    print(f"📊 Rango: [{x_inicio}, {x_fin}]")
    print(f"⚡ Discontinuidades: {disc1:.3f}, {disc2:.3f}")
    
    # Segmento 1: Antes de π/2
    segmento1 = []
    x_vals1 = [x_inicio + i * (disc1 - 0.1 - x_inicio) / num_puntos for i in range(num_puntos)]
    
    for x in x_vals1:
        try:
            y = 1.0 / math.tan(x)  # cotangente
            if 0 <= y <= 5:  # Filtrar valores válidos
                segmento1.append((x, y))
        except:
            continue
    
    # Segmento 2: Entre π/2 y π
    segmento2 = []
    x_vals2 = [disc1 + 0.1 + i * (disc2 - 0.1 - (disc1 + 0.1)) / num_puntos for i in range(num_puntos)]
    
    for x in x_vals2:
        try:
            y = 1.0 / math.tan(x)  # cotangente
            if 0 <= y <= 5:  # Filtrar valores válidos
                segmento2.append((x, y))
        except:
            continue
    
    # Segmento 3: Después de π
    segmento3 = []
    x_vals3 = [disc2 + 0.1 + i * (x_fin - (disc2 + 0.1)) / num_puntos for i in range(num_puntos)]
    
    for x in x_vals3:
        try:
            y = 1.0 / math.tan(x)  # cotangente
            if 0 <= y <= 5:  # Filtrar valores válidos
                segmento3.append((x, y))
        except:
            continue
    
    print(f"✅ Segmento 1: {len(segmento1)} puntos")
    print(f"✅ Segmento 2: {len(segmento2)} puntos") 
    print(f"✅ Segmento 3: {len(segmento3)} puntos")
    
    return [segmento1, segmento2, segmento3]

def crear_codigo_tikz(segmentos):
    """
    Crear código TikZ completo
    """
    codigo = """\\documentclass[border=2mm]{standalone}
\\usepackage{tikz}
\\usepackage{pgfplots}
\\pgfplotsset{compat=1.18}

\\begin{document}

% COTANGENTE GENERADA CON PYTHON
% Cálculos matemáticos exactos: cot(x) = 1/tan(x)

\\begin{tikzpicture}[scale=1.0]
\\begin{axis}[
    xlabel={Ángulo $\\alpha$},
    ylabel={Distancia PK},
    xmin=0, xmax=4.5,
    ymin=0, ymax=5,
    axis lines=left,
    xtick=\\empty,
    ytick=\\empty,
    clip=false
]

"""
    
    # Agregar cada segmento
    for i, segmento in enumerate(segmentos):
        if segmento:
            codigo += f"% Segmento {i+1} de cotangente\n"
            codigo += "\\addplot[cyan, very thick, smooth] coordinates {\n"
            
            # Tomar cada 3er punto para no sobrecargar
            for j in range(0, len(segmento), 3):
                x, y = segmento[j]
                codigo += f"    ({x:.3f},{y:.3f})\n"
            
            codigo += "};\n\n"
    
    # Agregar elementos adicionales
    codigo += """% Líneas punteadas verticales (posiciones de la imagen original)
\\draw[dashed, black, thick] (axis cs:1.2,0) -- (axis cs:1.2,5) 
    node[left, black, yshift=8pt] {\\footnotesize $QP$};
\\draw[dashed, black, thick] (axis cs:2.8,0) -- (axis cs:2.8,5) 
    node[left, black, yshift=8pt] {\\footnotesize $I_1$};

% Flecha vertical cyan (elemento distintivo)
\\draw[cyan, very thick, -stealth] (axis cs:0.25,4.2) -- (axis cs:0.25,4.8);

% Punto destacado
\\addplot[cyan, mark=*, mark size=2pt, only marks] coordinates {(2.8,0.3)};

\\end{axis}
\\end{tikzpicture}

\\end{document}"""
    
    return codigo

def main():
    """
    Función principal
    """
    print("🚀 INICIANDO GENERADOR SIMPLE")
    
    # Generar segmentos de cotangente
    segmentos = generar_cotangente_simple()
    
    # Crear código TikZ
    codigo_tikz = crear_codigo_tikz(segmentos)
    
    # Guardar archivo
    nombre_archivo = "cotangente_python_simple.tex"
    with open(nombre_archivo, "w", encoding="utf-8") as f:
        f.write(codigo_tikz)
    
    print(f"✅ Archivo generado: {nombre_archivo}")
    print(f"📝 Líneas de código: {len(codigo_tikz.split())}")
    
    # Mostrar muestra del código
    print("\n📋 MUESTRA DEL CÓDIGO GENERADO:")
    print("-" * 40)
    lineas = codigo_tikz.split('\n')
    for i in range(20, 35):
        if i < len(lineas):
            print(f"{i:2d}: {lineas[i]}")
    
    print("\n🔧 Para compilar: pdflatex cotangente_python_simple.tex")
    return True

if __name__ == "__main__":
    main()
