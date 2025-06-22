---
output:
  word_document: default
  html_document: default
  pdf_document: default
---

# Pregunta 10

-------------------

![](/home/manjaro_lenovo/Documentos/Math/2023-Matematicas-11-1/10/all.png)

-------------------

### Análisis

**Respuesta correcta: A**

**Explicación detallada:**

1. **Entendiendo el problema:**  
   Se tienen tres CDs (CD1, CD2, CD3), cada uno con canciones de **salsa (S)** y **merengue (M)**. Se debe elegir al azar una canción de cada CD en secuencia (primero CD1, luego CD2, luego CD3). El diagrama correcto debe representar todas las combinaciones posibles de selección, considerando que **cada CD contiene ambos géneros**.

2. **Análisis de opciones:**  
   - **Opción A:**  
     - Contiene filas con combinaciones variadas de S y M en diferentes posiciones.  
     - Ejemplo: `S | M | S`, `S | S | S`, `M | S | S`, etc.  
     - Esto indica que cada CD tiene al menos una canción de cada género, ya que hay resultados con M en CD1, CD2 o CD3.  

   - **Opción B:**  
     - Las primeras 4 filas son `S | S | S` y las siguientes 3 son `M | M | M`.  
     - Esto sugiere que los CDs solo tienen canciones de un género (todos S o todos M), lo cual contradice el enunciado.  

   - **Opción C:**  
     - Combina bloques de S y M, pero las primeras 3 filas son `S | S | S` y luego 3 filas `M | M | M`.  
     - Similar a B, no refleja la mezcla de géneros en cada CD.  

   - **Opción D:**  
     - Todas las filas son `S | S | S`.  
     - Implica que todos los CDs solo tienen canciones S, lo cual es incorrecto.  

3. **Conclusión:**  
   - La **Opción A** es la única que muestra variabilidad en la selección de géneros para cada CD, cumpliendo con la condición de que **cada CD contiene salsa y merengue**.  
   - Las demás opciones asumen que algún CD tiene solo un género, lo cual no se ajusta al problema planteado.  

**Fórmula de combinaciones posibles (ejemplo):**  
Si cada CD tiene \( n_1 \), \( n_2 \), y \( n_3 \) canciones respectivamente, el total de combinaciones es \( n_1 \times n_2 \times n_3 \). Sin embargo, los diagramas no muestran todas las combinaciones numéricas, sino ejemplos de selecciones válidas. 

La **Opción A** refleja correctamente la diversidad de elecciones.

------------------------

### Análisis de la Pregunta 2023-Matematicas-11-1-10, según SABER 11 ICFES

- **Nivel de Desempeño**: 3
- **Competencia**: Interpretación y Representación
  - **Afirmación**: Comprende y transforma la información cuantitativa y esquemática presentada en distintos formatos
  - **Evidencia**: Da cuenta de las características básicas de la información presentada en diferentes formatos como series, gráficas, tablas y esquemas
- **Componente**: Aleatorio
- **Estándar Asociado**: Resuelvo y formulo problemas a partir de un conjunto de datos provenientes de observaciones, consultas o experimentos
- **¿Qué evalúa?**: La capacidad de interpretar diagramas de árbol para representar espacios muestrales en experimentos secuenciales

## Respuesta Correcta: A

### Justificación:
El diagrama A representa correctamente la situación porque:

1. Muestra todas las posibles combinaciones al seleccionar una canción de cada CD en secuencia
2. Refleja que cada CD contiene tanto salsa (S) como merengue (M)
3. La estructura del árbol sigue el orden temporal de selección (CD1 → CD2 → CD3)
4. El espacio muestral resultante es $2 \times 2 \times 2 = 8$ posibles resultados

### Distractores:

- **Opción B**: 
  - Error conceptual: Agrupa las selecciones por género, sugiriendo incorrectamente que cada CD solo puede reproducir un tipo de música
  - Razonamiento erróneo: El estudiante podría pensar que las canciones deben ser del mismo género
  - No representa la mezcla de géneros en cada CD

- **Opción C**:
  - Error estructural: Separa los CDs en ramas independientes
  - Razonamiento erróneo: El estudiante podría confundir la secuencialidad del experimento
  - No refleja la naturaleza secuencial de las selecciones

- **Opción D**:
  - Error de representación: Repite el mismo patrón para cada CD
  - Razonamiento erróneo: El estudiante podría pensar que el orden de selección no importa
  - No muestra todas las posibles combinaciones

### Contenidos Matemáticos Curriculares:
- **Estadística**
- **Período**: 3
  - Conjuntos, Combinatoria y Probabilidad
    - Principio de multiplicación y diagramas de árbol

### Características Adicionales:
- **Genérico**: Sí
- **Eje Axial Disciplinar**: Eje 4 - Conjuntos, Casillas y Combinaciones
- **Tarea**: Identificar el diagrama de árbol que representa correctamente un experimento aleatorio secuencial con dos opciones en cada etapa
- **Grado sugerido**: 11°