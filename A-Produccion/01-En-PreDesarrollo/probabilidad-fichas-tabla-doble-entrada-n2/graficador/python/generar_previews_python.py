"""Genera los previews del graficador Python con la versión final del código (no forma parte del ejercicio).

Uso: python3 generar_previews_python.py [output_python_vN.py]
"""
import importlib.util
import os
import sys

AQUI = os.path.dirname(os.path.abspath(__file__))
modulo = sys.argv[1] if len(sys.argv) > 1 else os.path.join(AQUI, "output_python_v8.py")
spec = importlib.util.spec_from_file_location("graficador", modulo)
g = importlib.util.module_from_spec(spec)
spec.loader.exec_module(g)

# Datos del impreso (pregunta 32)
g.dibujar_tabla_fichas([[12, 11], [8, 15], [16, 20], [10, 8]], "Figura geométrica", "Color", "Categoría",
                       ["Círculo", "Cuadrado"], ["Blanca", "Negra"], ["Par", "Impar"],
                       os.path.join(AQUI, "preview_python_canonica.png"))
# Caso de estrés: 1-3 dígitos y los textos más largos permitidos (25 caracteres en el título, 13 en el resto)
g.dibujar_tabla_fichas([[1, 105], [28, 9], [100, 64], [7, 12]], "Figura geométrica impresa", "Clasificación",
                       "Numeración", ["Rectángulo", "Triángulo"], ["Amarilla", "Azul oscuro"], ["Primo", "Compuesto"],
                       os.path.join(AQUI, "preview_python_extremo.png"))
print("previews generados con", os.path.basename(modulo))
