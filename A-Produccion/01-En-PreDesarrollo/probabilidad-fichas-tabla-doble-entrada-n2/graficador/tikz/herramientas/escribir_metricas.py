#!/usr/bin/env python3
"""Escribe ../metricas_tikz.json: una entrada por iteración (v1..v8) con la puntuación de la rúbrica
(herramientas/puntuar.py), el RMSE auxiliar (herramientas/comparar.py) y lo que cambió."""
import json, sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import puntuar
CAMBIOS = {
 'v1': 'Primera construcción. Geometría medida sobre el escaneo (4 columnas de 127,25 px, 6 filas de 35,5 px; línea gruesa en el contorno exterior y bajo «Figura geométrica»). Helvetica Narrow Bold comprimida en horizontal (xscale) y engrosada con trazo de contorno (modo de texto 2 de PDF); una sola tinta (80,16,52) y un solo ancho por tipo de línea. Anchos de columna adaptativos medidos por LaTeX.',
 'v2': 'Trazo del encabezado más grueso (astas 4,2 px) y cuerpo de letra por clase; línea base 1 px más arriba; anchos de línea por orientación (en el escaneo las verticales llevan entre un 14 % y un 22 % menos de tinta que las horizontales); tinta de línea (88,13,58); argumento opcional `estilo` para afinar.',
 'v3': 'Clase propia para las cifras (12,4 pt, sx 0,80); tinta de la línea fina más clara (125,40,92) con anchos 1,25/1,05 pt, como en el escaneo (pico 72-132 frente a 21-24 de la gruesa); línea base por clase (título 2 px más arriba); encabezado 11,8 pt con trazo 0,90.',
 'v4': 'Fondo blanco opaco más grande que la caja del dibujo (el PNG de exams::tex2image salía con una columna semitransparente) y caja fijada con \\useasboundingbox; ligaduras ff/fi/fl rotas; sin líneas en blanco ni mediciones repetidas; línea gruesa (76,11,48); texto más ligero (trazos 0,30/0,34/0,85) para igualar la cobertura de tinta.',
 'v5': 'Manchado horizontal opcional (tres copias desplazadas ±δ) para engrosar las astas verticales sin engordar los trazos horizontales; segmento y=51,5 de las columnas 3-4 con su propio grosor; anchos 1,55/1,38/1,28/1,08 pt; tinta fina (116,58,90). Con el manchado desactivado, equivale a v4 más las líneas.',
 'v6': 'Parámetros del texto elegidos por barrido (unos 280 renders de laboratorio, fuera de la carpeta entregable): Par/Impar en Helvetica Narrow c (11,6 pt, manchado 0,7 px), cifras 13 pt con manchado 0,5, encabezado con manchado 0,7; el dígito 1 se estrecha al 70 % (los del impreso son proporcionales); color del texto de encabezado (112,5,44).',
 'v7': 'Rótulo combinado con su propia escala horizontal (ancho 158 px = impreso) y línea base del título corregida; rótulos Blanca/Negra con trazo 0,32 y línea base 0,5 px más arriba.',
 'v8': 'Final. Corregido un defecto de las versiones anteriores: las mediciones de ancho solo cubrían la primera etiqueta de cada grupo (tex_esc no vectorizada), lo que afectaba al ensanche adaptativo de las columnas pero no a la canónica. Ajustes finales: segmento y=51,5 de las columnas 3-4 a 1,30 pt, verticales finas a 1,00 pt, encabezado con trazo 0,40 y manchado 0,65 px. Código reescrito con comentarios y sin <<-.'
}
salida = []
for v in ['v1', 'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8']:
    res = puntuar.puntuar(v)
    med = json.load(open(f'{puntuar.D}/medidas_{v}.json'))
    salida.append(dict(
        iteracion=int(v[1:]),
        puntuacion_total=round(sum(x[0] for x in res.values()), 1),
        por_categoria={k: round(x[0], 1) for k, x in res.items()},
        rmse_aux=med['rmse_aux'],
        cambios=CAMBIOS[v],
        detalle={k: x[1] for k, x in res.items()}))
ruta = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'metricas_tikz.json')
json.dump(salida, open(ruta, 'w', encoding='utf-8'), ensure_ascii=False, indent=1)
print(os.path.normpath(ruta))
for e in salida: print(e['iteracion'], e['puntuacion_total'], e['por_categoria'], e['rmse_aux']['normalizado'])
