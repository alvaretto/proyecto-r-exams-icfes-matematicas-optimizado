---
output:
  word_document: default
  html_document: default
  pdf_document: default
---

# Pregunta 04

-------------------

![](/home/manjaro_lenovo/Documentos/Math/2023-Matematicas-11-1/04/all.png)

-------------------

### Análisis

El problema nos dice que un docente ha preseleccionado un grupo de estudiantes, pero necesita seleccionar al azar un grupo de **3 estudiantes**. Además, nos informan que hay **10 posibles selecciones** para formar este grupo de 3 estudiantes. 

### Identificación del concepto clave
El número de formas en que se puede elegir un subconjunto de elementos de un conjunto mayor sin importar el orden, se calcula utilizando las **combinaciones**. La fórmula general para las combinaciones es:

\[
C(n, r) = \frac{n!}{r!(n-r)!}
\]

Donde:

- \( n \): Número total de elementos en el conjunto.
- \( r \): Número de elementos seleccionados.
- \( n! \): Factorial de \( n \), que se calcula como \( n \times (n-1) \times (n-2) \times \ldots \times 1 \).

### Aplicación al problema
Aquí sabemos que:

- \( r = 3 \) (se seleccionan 3 estudiantes).
- \( C(n, 3) = 10 \) (hay 10 formas posibles de hacer esta selección).

Necesitamos encontrar el valor de \( n \), que es el número total de estudiantes en el grupo preseleccionado. Utilizamos la fórmula de combinaciones:

\[
C(n, 3) = \frac{n!}{3!(n-3)!} = 10
\]

Sabemos que \( 3! = 3 \times 2 \times 1 = 6 \), así que la ecuación se simplifica a:

\[
\frac{n(n-1)(n-2)}{6} = 10
\]

Multiplicamos ambos lados de la ecuación por 6 para eliminar el denominador:

\[
n(n-1)(n-2) = 60
\]

### Resolución de la ecuación
Expandiendo los términos:

1. Probamos valores para \( n \) hasta que la ecuación sea válida.
   - Para \( n = 5 \):
     \[
     5 \cdot 4 \cdot 3 = 60
     \]

Esto satisface la ecuación, por lo que el número total de estudiantes preseleccionados es **5**.

### Respuesta correcta
La respuesta es:

**D. 5**

------------------------

### Análisis de la Pregunta 2023-Matematicas-11-1-04, según SABER 11 ICFES

- Nivel de Desempeño: 4
- Competencia: Formulación y Ejecución
  - Aprendizaje: Frente a un problema que involucre información cuantitativa, plantea e implementa estrategias que lleven a soluciones adecuadas.
  - Evidencia: Ejecuta un plan de solución para un problema que involucra información cuantitativa o esquemática.
- Componente: Aleatorio
- Estándar Asociado: Resuelvo y planteo problemas usando conceptos básicos de conteo y probabilidad (combinaciones, permutaciones, espacio muestral, muestreo aleatorio, muestreo con reemplazo).
- ¿Qué evalúa?: La capacidad de determinar el tamaño de un conjunto dado el número de combinaciones posibles al seleccionar un subconjunto específico.
- Respuesta Correcta: D. 5, porque al aplicar la fórmula de combinaciones $C(n,3) = \frac{n!}{3!(n-3)!} = 10$, se obtiene que n = 5 satisface la ecuación.
- Distractores:

  - A. 13: El estudiante podría llegar a este resultado al:
    - Sumar el número de combinaciones posibles (10) con el número de estudiantes a seleccionar (3)
    - Reflejar una comprensión superficial del problema donde simplemente opera con los números dados
    - No comprender el concepto de combinaciones ni su relación con el tamaño del conjunto original

  - B. 10: El estudiante podría seleccionar esta opción al:
    - Confundir el número de combinaciones posibles con el tamaño del conjunto original
    - No distinguir entre el resultado de la operación combinatoria y los elementos del conjunto inicial
    - Mostrar una interpretación literal del dato "10 posibles selecciones" como si fuera el número total de estudiantes

  - C. 6: El estudiante podría obtener este resultado al:
    - Confundir el denominador de la fórmula de combinaciones (3! = 6) con el tamaño del conjunto
    - Realizar una manipulación incorrecta de la fórmula $C(n,3) = \frac{n!}{3!(n-3)!}$
    - No verificar si su respuesta es coherente con el contexto del problema
    - Mostrar una comprensión parcial de la fórmula de combinaciones donde solo identifica algunos elementos sin entender su significado completo

- Contenidos Matemáticos Curriculares: Estadística
  - Período: 3
    - Conjuntos, Combinatoria y Probabilidad
      - Combinaciones
- No genérico
- Eje Axial Disciplinar: Eje 4 (Conjuntos, Casillas y Combinaciones)
- Tarea: Determinar el tamaño de un conjunto dado el número de combinaciones posibles al seleccionar un subconjunto de tamaño específico.
- Grado: 11°