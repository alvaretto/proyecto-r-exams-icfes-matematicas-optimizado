import random
import numpy as np
from matplotlib.figure import Figure
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
import base64
import io

def calcular_cuartiles_tradicional(datos):
    n = len(datos)
    datos_ordenados = sorted(datos)
    if n % 2 == 0:
        pos_med1 = n // 2
        pos_med2 = pos_med1 + 1
        q2 = (datos_ordenados[pos_med1-1] + datos_ordenados[pos_med2-1]) / 2
        primera_mitad = datos_ordenados[:pos_med1]
        segunda_mitad = datos_ordenados[pos_med1:]
    else:
        pos_med = (n + 1) // 2
        q2 = datos_ordenados[pos_med-1]
        primera_mitad = datos_ordenados[:(pos_med-1)]
        segunda_mitad = datos_ordenados[pos_med:]
    q1 = np.median(primera_mitad)
    q3 = np.median(segunda_mitad)
    return {"q1": q1, "q2": q2, "q3": q3}

def redondear_cuartil(valor):
    valor_redondeado = round(valor, 1)
    if valor_redondeado % 1 == 0:
        return int(valor_redondeado)
    else:
        return valor_redondeado

def generar_ejercicio():
    n = np.random.randint(9, 26)
    base_min = np.random.randint(145, 166)
    base_max = np.random.randint(168, 189)
    estaturas = np.round(np.random.uniform(base_min, base_max, n)).astype(int)
    cuartiles = calcular_cuartiles_tradicional(estaturas)
    q1 = redondear_cuartil(cuartiles["q1"])
    q2 = redondear_cuartil(cuartiles["q2"])
    q3 = redondear_cuartil(cuartiles["q3"])
    min_diferencia = 1.0
    valores = sorted([np.min(estaturas), q1, q2, q3, np.max(estaturas)])
    hay_valores_cercanos = False
    for i in range(1, len(valores)):
        if valores[i] - valores[i-1] < min_diferencia:
            hay_valores_cercanos = True
            break
    if hay_valores_cercanos:
        return generar_ejercicio()
    stats = {
        'minimo': int(np.min(estaturas)),
        'q1': q1,
        'mediana': q2,
        'q3': q3,
        'maximo': int(np.max(estaturas)),
        'datos': estaturas.tolist()
    }
    diagramas = []
    diagramas.append(stats.copy())
    incorrecto1 = stats.copy()
    incorrecto1['q1'], incorrecto1['q3'] = incorrecto1['q3'], incorrecto1['q1']
    diagramas.append(incorrecto1)
    incorrecto2 = stats.copy()
    incorrecto2['minimo'] += 5
    incorrecto2['maximo'] -= 5
    diagramas.append(incorrecto2)
    incorrecto3 = stats.copy()
    incorrecto3['mediana'] += 10
    diagramas.append(incorrecto3)
    for diagrama in diagramas[1:]:
        valores = sorted([diagrama['minimo'], diagrama['q1'], diagrama['mediana'], diagrama['q3'], diagrama['maximo']])
        for i in range(1, len(valores)):
            if valores[i] - valores[i-1] < min_diferencia:
                return generar_ejercicio()
    indice_correcto = 0
    diagramas_ordenados = diagramas.copy()
    random.shuffle(diagramas_ordenados)
    for i, d in enumerate(diagramas_ordenados):
        if d == diagramas[0]:
            indice_correcto = i
            break
    sol = "".join(["1" if i == indice_correcto else "0" for i in range(len(diagramas))])
    return {
        'stats': stats,
        'diagramas_ordenados': diagramas_ordenados,
        'respuesta_correcta': indice_correcto,
        'sol': sol
    }

def dibujar_boxplot_simple(valores, indice=0):
    fig = Figure(figsize=(7.0, 4.0))
    canvas = FigureCanvas(fig)
    ax = fig.add_subplot(111)
    ax.set_title(f"Diagrama")
    ax.set_ylabel("Estatura (cm)")
    ax.set_xlim(0, 2.5)
    ax.set_xticks([])
    pos = 1
    width = 0.6
    q1_plot = min(valores['q1'], valores['q3'])
    q3_plot = max(valores['q1'], valores['q3'])
    box = plt.Rectangle((pos-width/2, q1_plot), width, q3_plot-q1_plot, fill=True, color='lightblue', edgecolor='blue')
    ax.add_patch(box)
    ax.hlines(y=valores['mediana'], xmin=pos-width/2, xmax=pos+width/2, color='red', linewidth=2)
    min_plot = min(valores['minimo'], q1_plot)
    max_plot = max(valores['maximo'], q3_plot)
    ax.vlines(x=pos, ymin=min_plot, ymax=q1_plot, color='blue', linewidth=1.5)
    ax.vlines(x=pos, ymin=q3_plot, ymax=max_plot, color='blue', linewidth=1.5)
    ax.hlines(y=min_plot, xmin=pos-width/4, xmax=pos+width/4, color='blue', linewidth=1.5)
    ax.hlines(y=max_plot, xmin=pos-width/4, xmax=pos+width/4, color='blue', linewidth=1.5)
    ax.text(1.5, valores['minimo'], f"Mínimo: {valores['minimo']}", va='center')
    ax.text(1.5, valores['q1'], f"Q1: {valores['q1']}", va='center')
    ax.text(1.5, valores['mediana'], f"Mediana: {valores['mediana']}", va='center')
    ax.text(1.5, valores['q3'], f"Q3: {valores['q3']}", va='center')
    ax.text(1.5, valores['maximo'], f"Máximo: {valores['maximo']}", va='center')
    todos_valores = [valores['minimo'], valores['q1'], valores['mediana'], valores['q3'], valores['maximo'], min_plot, max_plot]
    margen = 5
    ax.set_ylim(min(todos_valores) - margen, max(todos_valores) + margen)
    buf = io.BytesIO()
    fig.savefig(buf, format='png', bbox_inches='tight')
    buf.seek(0)
    img_str = base64.b64encode(buf.read()).decode('utf-8')
    return f'<img src="data:image/png;base64,{img_str}" alt="Boxplot {indice+1}">'

def obtener_datos_desordenados(stats):
    datos = stats['datos'].copy()
    random.seed(42)
    random.shuffle(datos)
    return datos

def obtener_datos_ordenados(stats):
    return sorted(stats['datos'])

def generar_tabla_html(datos, titulo="Datos de estaturas"):
    filas_por_columna = 10
    num_columnas = (len(datos) + filas_por_columna - 1) // filas_por_columna
    html = f"<p><strong>{titulo}</strong></p>"
    html += "<table border='1' style='border-collapse: collapse; margin: 10px;'>"
    for i in range(filas_por_columna):
        html += "<tr>"
        for j in range(num_columnas):
            idx = j * filas_por_columna + i
            if idx < len(datos):
                html += f"<td style='padding: 5px;'>{datos[idx]}</td>"
            else:
                html += "<td></td>"
        html += "</tr>"
    html += "</table>"
    return html
