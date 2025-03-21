# Plan de Optimización para schoice-cuartil-estatura-03-py.Rmd

Este documento describe el plan para optimizar el archivo `schoice-cuartil-estatura-03-py.Rmd`, siguiendo las instrucciones proporcionadas.

## Objetivos

*   Convertir el código a tipo ‘schoice’ con cinco opciones: cuatro incorrectas y solo una correcta.
*   Garantizar la sintaxis visual de ‘Diagrama 4’ para todos los diagramas.

## Plan

1.  **Generar un "universo" de diagramas incorrectos:**
    *   Modificar la función `generar_ejercicio()` para generar un "universo" de 12 diagramas incorrectos.

2.  **Implementar lógica de errores comunes:**
    *   Implementar lógica para generar diagramas incorrectos basados en errores comunes que los estudiantes suelen cometer al interpretar los datos:
        *   Invertir Q1 y Q3.
        *   Modificar la mediana incorrectamente (ej: sumarle o restarle un valor).
        *   Cambiar los valores mínimo y máximo incorrectamente (ej: acortar el rango).
        *   Crear diagramas con asimetría incorrecta (ej: alargar un bigote más que el otro).
        *   Errores en el cálculo de los bigotes (whiskers).

3.  **Seleccionar diagramas aleatorios:**
    *   Escoger aleatoriamente 4 diagramas incorrectos del "universo" para cada pregunta.

4.  **Asegurar la sintaxis visual:**
    *   Asegurar la sintaxis visual de "Diagrama 4" para todos los diagramas. Esto significa que todos los diagramas deben tener el título "Diagrama 4". Modificar la función `dibujar_boxplot_simple()` para que todos los diagramas tengan este título.

5.  **Modificar la sección Answerlist:**
    *   Modificar la sección `Answerlist` para mostrar los cinco diagramas (1 correcto + 4 incorrectos).

6.  **Ajustar la sección Solution:**
    *   Ajustar la sección `Solution` para que corresponda con las cinco opciones.

7.  **Actualizar exsolution:**
    *   Actualizar `exsolution` para reflejar la nueva cantidad de opciones.

## Diagrama de Flujo

```mermaid
graph LR
A[Inicio] --> B{Modificar generar_ejercicio() para universo de 12 diagramas incorrectos};
B --> C{Implementar lógica de errores comunes (incl. whiskers)};
C --> D{Escoger aleatoriamente 4 diagramas incorrectos};
D --> E{Asegurar título "Diagrama 4" en todos los diagramas};
E --> F{Modificar Answerlist para 5 diagramas};
F --> G{Ajustar Solution para 5 opciones};
G --> H{Actualizar exsolution};
H --> I[Fin];