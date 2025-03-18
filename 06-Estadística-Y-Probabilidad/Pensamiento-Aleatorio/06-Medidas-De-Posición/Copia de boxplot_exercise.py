import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import random
import time
import io
import base64

# Función para calcular cuartiles usando el método tradicional
def calcular_cuartiles_tradicional(datos):
    n = len(datos)
    datos_ordenados = sorted(datos)

    # Calcular Q2 (mediana)
    if n % 2 == 0:
        # Número par de elementos
        pos_med1 = n // 2
        pos_med2 = pos_med1 + 1
        q2 = (datos_ordenados[pos_med1-1] + datos_ordenados[pos_med2-1]) / 2

        # Dividir en dos mitades
        primera_mitad = datos_ordenados[:pos_med1]
        segunda_mitad = datos_ordenados[pos_med1:]
    else:
        # Número impar de elementos
        pos_med = (n + 1) // 2
        q2 = datos_ordenados[pos_med-1]

        # Dividir en dos mitades
        primera_mitad = datos_ordenados[:pos_med-1]
        segunda_mitad = datos_ordenados[pos_med:]

    # Calcular Q1 (mediana de la primera mitad)
    if len(primera_mitad) > 0:
        q1 = np.median(primera_mitad)
    else:
        q1 = datos_ordenados[0]

    # Calcular Q3 (mediana de la segunda mitad)
    if len(segunda_mitad) > 0:
        q3 = np.median(segunda_mitad)
    else:
        q3 = datos_ordenados[-1]

    return {'q1': q1, 'q2': q2, 'q3': q3}

# Función para redondear cuartiles según las reglas especificadas
def redondear_cuartil(valor):
    valor_redondeado = round(valor, 1)
    if valor_redondeado % 1 == 0:
        return int(valor_redondeado)
    else:
        return valor_redondeado

# Función para generar datos aleatorios de estatura
def generar_datos_estatura(base_min=None, base_max=None, n=None):
    if base_min is None:
        base_min = random.randint(145, 165)
    if base_max is None:
        base_max = random.randint(168, 188)
    if n is None:
        n = random.randint(9, 25)

    variacion = random.randint(5, 15)
    min_valor = base_min - random.randint(0, variacion)
    max_valor = base_max + random.randint(0, variacion)

    # Generar datos aleatorios y ordenarlos
    datos = sorted([round(random.uniform(min_valor, max_valor)) for _ in range(n)])

    # Calcular estadísticas usando el método tradicional
    cuartiles = calcular_cuartiles_tradicional(datos)

    stats = {
        'datos': datos,
        'datos_desordenados': random.sample(datos, len(datos)),
        'minimo': min(datos),
        'q1': cuartiles['q1'],
        'mediana': cuartiles['q2'],
        'q3': cuartiles['q3'],
        'maximo': max(datos)
    }

    # Redondear cuartiles para permitir un decimal si no es .0
    stats['q1'] = redondear_cuartil(stats['q1'])
    stats['mediana'] = redondear_cuartil(stats['mediana'])
    stats['q3'] = redondear_cuartil(stats['q3'])

    return stats

# Función para dibujar el boxplot
def dibujar_boxplot(valores, tipo="correcto"):
    fig, ax = plt.subplots(figsize=(6, 4))

    # Dibujar línea vertical central
    ax.vlines(x=1, ymin=valores['minimo'], ymax=valores['maximo'], linewidth=2)

    # MEJORA: Dibujar la caja (rectángulo) con color de fondo y borde visible
    rect = plt.Rectangle((0.8, valores['q1']), 0.4, valores['q3'] - valores['q1'],
                       fill=True, color='lightblue', edgecolor='black', linewidth=1.5, alpha=0.7)
    ax.add_patch(rect)

    # Dibujar la mediana con línea más destacada
    ax.hlines(y=valores['mediana'], xmin=0.8, xmax=1.2, linewidth=2.5, color='darkblue')

    # Dibujar los bigotes
    ax.hlines(y=valores['minimo'], xmin=0.8, xmax=1.2, linewidth=2)
    ax.hlines(y=valores['maximo'], xmin=0.8, xmax=1.2, linewidth=2)

    # Calcular espacios para evitar solapamiento de etiquetas
    rango = valores['maximo'] - valores['minimo']

    # Etiquetas
    etiquetas = [
        f"Mín = {valores['minimo']}",
        f"Q1 = {valores['q1']}",
        f"Q2 = {valores['mediana']}",
        f"Q3 = {valores['q3']}",
        f"Máx = {valores['maximo']}"
    ]

    # Posiciones para las etiquetas
    posiciones = [valores['minimo'], valores['q1'], valores['mediana'], valores['q3'], valores['maximo']]

    # Dibujar etiquetas
    for i in range(5):
        ax.text(1.3, posiciones[i], etiquetas[i], verticalalignment='center')

    ax.set_xlim(0.5, 1.5)
    ax.set_ylim(valores['minimo'] - (valores['maximo'] - valores['minimo']) * 0.15,
             valores['maximo'] + (valores['maximo'] - valores['minimo']) * 0.15)
    ax.set_xlabel("")
    ax.set_ylabel("Estatura (cm)")
    ax.set_title("Diagrama")
    ax.set_xticks([])

    # Convertir la figura a imagen base64 para incrustar en HTML
    buf = io.BytesIO()
    fig.savefig(buf, format='png')
    buf.seek(0)
    img_str = base64.b64encode(buf.read()).decode('utf-8')
    plt.close(fig)

    return f'<img src="data:image/png;base64,{img_str}" width="600" height="400">'

# Función para calcular valores para los diagramas
def calcular_valores_diagrama(stats, tipo="correcto"):
    # Valores base
    valores = {
        'minimo': stats['minimo'],
        'q1': stats['q1'],
        'mediana': stats['mediana'],
        'q3': stats['q3'],
        'maximo': stats['maximo']
    }

    # Modificar según el tipo de diagrama
    if tipo == "escala":
        # Multiplicar todos los valores por 10
        valores['minimo'] *= 10
        valores['q1'] *= 10
        valores['mediana'] *= 10
        valores['q3'] *= 10
        valores['maximo'] *= 10
    elif tipo == "invertido":
        # Invertir Q1 y Q3
        temp = valores['q1']
        valores['q1'] = valores['q3']
        valores['q3'] = temp
    elif tipo == "mediana_falsa":
        # Usar la media como mediana
        valores['mediana'] = round(sum(stats['datos']) / len(stats['datos']))

    return valores

# Función principal para generar el ejercicio
def generar_ejercicio():
    # Configurar semilla aleatoria basada en el tiempo
    random.seed(int(time.time()) % 10000 + random.randint(1, 1000))

    # Generar datos y calcular estadísticas
    stats = generar_datos_estatura()

    # Crear diagramas con diferentes variaciones
    diagramas = {
        'correcto': calcular_valores_diagrama(stats, "correcto"),
        'escala': calcular_valores_diagrama(stats, "escala"),
        'invertido': calcular_valores_diagrama(stats, "invertido"),
        'mediana_falsa': calcular_valores_diagrama(stats, "mediana_falsa")
    }

    # Aleatorizar la posición de la respuesta correcta
    posiciones = random.sample(range(4), 4)
    respuesta_correcta = posiciones.index(0)

    # Asignar letras a las posiciones
    letras = ["A", "B", "C", "D"]
    solucion = letras[respuesta_correcta]

    # Crear vector con los tipos de diagramas en orden aleatorio
    tipos_diagramas = ["correcto", "escala", "invertido", "mediana_falsa"]
    tipos_ordenados = [tipos_diagramas[posiciones[i]] for i in range(4)]

    # Crear vector de solución
    sol = ['0', '0', '0', '0']
    sol[respuesta_correcta] = '1'
    sol = ''.join(sol)

    # Crear lista de diagramas ordenados según las posiciones
    diagramas_ordenados = [
        diagramas[tipos_ordenados[0]],
        diagramas[tipos_ordenados[1]],
        diagramas[tipos_ordenados[2]],
        diagramas[tipos_ordenados[3]]
    ]

    return {
        'stats': stats,
        'diagramas_ordenados': diagramas_ordenados,
        'respuesta_correcta': respuesta_correcta,
        'sol': sol,
        'solucion': solucion,
        'tipos_ordenados': tipos_ordenados
    }

# Funciones auxiliares para acceder a los datos desde R
def obtener_datos_desordenados(stats):
    return list(stats['datos_desordenados'])

def obtener_datos_ordenados(stats):
    return list(sorted(stats['datos']))

# Función modificada para generar tabla HTML
def generar_tabla_html(datos, titulo=None):
    # Asegurar que datos sea una lista Python
    if isinstance(datos, dict):
        if 'datos_desordenados' in datos:
            datos_lista = list(datos['datos_desordenados'])
        else:
            datos_lista = list(datos['datos'])
    else:
        datos_lista = list(datos)

    # Crear DataFrame
    estudiantes = list(range(1, len(datos_lista) + 1))
    html = '<table border="1" cellpadding="5" cellspacing="0">'

    # Título si se proporciona
    if titulo:
        html += f'<caption>{titulo}</caption>'

    # Encabezados
    html += '<tr><th>Estudiante</th><th>Estatura (cm)</th></tr>'

    # Datos
    for i, valor in enumerate(datos_lista):
        html += f'<tr><td style="text-align: center">{estudiantes[i]}</td><td style="text-align: center">{valor}</td></tr>'

    html += '</table>'
    return html
