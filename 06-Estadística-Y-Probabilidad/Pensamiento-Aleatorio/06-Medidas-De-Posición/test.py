import unittest
import random
import numpy as np
from matplotlib.figure import Figure
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
import base64
import io

# Importar las funciones desde el script principal
from schoice_cuartil_estatura_01_py import (
    calcular_cuartiles_tradicional,
    redondear_cuartil,
    generar_ejercicio,
    dibujar_boxplot_simple,
    obtener_datos_desordenados,
    obtener_datos_ordenados,
    generar_tabla_html
)

class TestDiagrams(unittest.TestCase):
    def test_unique_diagrams(self):
        # Generar un ejercicio
        ejercicio = generar_ejercicio()
        diagramas_ordenados = ejercicio['diagramas_ordenados']

        # Convertir cada diagrama a una tupla para comparación
        diagramas_tuplas = [tuple(d.values()) for d in diagramas_ordenados]

        # Verificar que no haya duplicados
        self.assertEqual(len(set(diagramas_tuplas)), len(diagramas_tuplas), "Se encontraron diagramas idénticos")

    def test_generate_multiple_variants(self):
        # Número de variantes a generar
        num_variants = 500

        # Conjunto para almacenar variantes únicos
        unique_variants = set()

        for _ in range(num_variants):
            # Generar un ejercicio
            ejercicio = generar_ejercicio()
            diagramas_ordenados = ejercicio['diagramas_ordenados']

            # Convertir cada diagrama a una tupla para comparación
            diagramas_tuplas = tuple(tuple(d.values()) for d in diagramas_ordenados)

            # Agregar el conjunto de diagramas al conjunto de variantes únicos
            unique_variants.add(diagramas_tuplas)

        # Verificar que se hayan generado al menos 500 variantes únicos
        self.assertGreaterEqual(len(unique_variants), num_variants, f"Se generaron menos de {num_variants} variantes únicos")

if __name__ == '__main__':
    unittest.main()
