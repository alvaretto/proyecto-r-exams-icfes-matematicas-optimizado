# Ejercicio de Probabilidad: Extracción de Bolas

Este ejercicio genera problemas de probabilidad que involucran la extracción de bolas de colores de un recipiente, calculando la probabilidad de obtener un número mínimo de bolas de un color específico.

## Características

- **Aleatorización avanzada**: Genera más de 150 versiones diferentes del problema.
- **Visualización**: Incluye representaciones visuales con TikZ y Python/Matplotlib.
- **Cálculos de probabilidad**: Utiliza combinaciones para calcular casos favorables y posibles.
- **Opciones de respuesta**: Genera 4 opciones distintas sin duplicados.

## Pruebas

El archivo `ejecutar_pruebas.R` verifica:

1. **Coherencia matemática**: Los cálculos de probabilidad son correctos.
2. **Opciones de respuesta**: Se generan 4 opciones distintas en el rango [0,1].
3. **Diversidad**: Se generan al menos 150 versiones diferentes.
4. **Compatibilidad**: El ejercicio es compatible con r-exams.

## Ejecución

Para ejecutar el ejercicio:

```r
library(exams)
exams2html("probabilidad_extraccion_bolas_v1.Rmd", n=1)
```

Para ejecutar las pruebas:

```r
source("ejecutar_pruebas.R")
```

## Estructura del Ejercicio

- **Pregunta**: Presenta un problema de extracción de bolas con visualización.
- **Opciones**: Muestra 4 probabilidades posibles.
- **Solución**: Explica el cálculo de la probabilidad correcta.
- **Metainformación**: Incluye datos para r-exams.

## Elementos Aleatorios

- Número total de bolas (entre 8 y 15)
- Distribución de bolas por color
- Colores seleccionados (de una lista de 11 colores)
- Número de bolas a extraer (entre 3 y 5)
- Número mínimo de bolas para ganar
- Contexto del problema (bolsa, urna, caja, etc.)
- Frases y verbos utilizados
