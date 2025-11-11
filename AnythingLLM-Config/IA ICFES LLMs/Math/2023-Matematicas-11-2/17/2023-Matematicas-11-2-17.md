---
output:
  html_document: default
  word_document: default
  pdf_document: default
---
# Pregunta 17

-------------------

![](/home/manjaro_lenovo/Documentos/Math/2023-Matematicas-11-2/17/01.png)

-------------------

## Análisis detallado del problema

La opción correcta es la **D. $n^2$**.

**Paso a paso:**

1. **Identificar la secuencia de puntos:** El enunciado describe una secuencia de puntos ganados con cada carta verde:
   - Primera carta: 1 punto
   - Segunda carta: 3 puntos
   - Tercera carta: 5 puntos
   - ... y así sucesivamente.

   Podemos observar que los puntos ganados forman una secuencia de números impares.

2. **Encontrar una fórmula para los puntos de la *n*-ésima carta:**  Analizando la secuencia, podemos deducir una fórmula para la cantidad de puntos ganados con la *n*-ésima carta verde. Observemos la relación:
   - Para la 1ª carta (n=1): 1 punto  (2*1 - 1)
   - Para la 2ª carta (n=2): 3 puntos  (2*2 - 1)
   - Para la 3ª carta (n=3): 5 puntos  (2*3 - 1)
   - Para la *n*-ésima carta: $2n - 1$ puntos

3. **Calcular el puntaje total:** El puntaje total se obtiene sumando los puntos de todas las cartas verdes obtenidas. Si el jugador obtuvo *n* cartas verdes, necesitamos sumar los puntos de la primera carta hasta la *n*-ésima carta. Esto sería la suma de la siguiente serie:

   $1 + 3 + 5 + ... + (2n - 1)$

   Esta es la suma de los primeros *n* números impares.

4. **Aplicar la fórmula para la suma de los primeros *n* números impares:** Existe una fórmula matemática que establece que la suma de los primeros *n* números impares es igual a $n^2$.

   *Demostración (opcional):* Podemos ver esto como una serie aritmética donde el primer término es $a_1 = 1$, la diferencia común es $d = 2$, y el número de términos es $n$. La suma de una serie aritmética se calcula con la fórmula:

   $S_n = \frac{n}{2} [2a_1 + (n-1)d]$

   Sustituyendo los valores:

   $S_n = \frac{n}{2} [2(1) + (n-1)2]$
   $S_n = \frac{n}{2} [2 + 2n - 2]$
   $S_n = \frac{n}{2} [2n]$
   $S_n = n^2$

5. **Verificar las otras opciones:**

   * **A. $2n - 1$:** Esta fórmula representa los puntos ganados con la *n*-ésima carta, no el puntaje total.
   * **B. $2n + 1$:**  Esta fórmula no coincide con la secuencia de puntos ganados por las cartas.
   * **C. $n + 2$:** Esta fórmula tampoco coincide con la secuencia de puntos ganados ni con el puntaje total.

**Conclusión:** La fórmula que permite calcular el puntaje total de un jugador que obtuvo *n* cartas verdes es **$n^2$**. Cada carta sucesiva aporta un número impar de puntos, y la suma de los primeros *n* números impares es igual a $n^2$.

------------------------

### Análisis de la Pregunta 2023-Matematicas-11-2-17, según SABER 11 ICFES

- Nivel de Desempeño: 4
- Competencia: Interpretación y Representación
  - Afirmación: Comprende y transforma la información cuantitativa y esquemática presentada en distintos formatos
  - Evidencia: Da cuenta de las características básicas de la información presentada en diferentes formatos
- Componente: Numérico-Variacional
- Estándar Asociado: Reconozco y generalizo propiedades de las relaciones entre números racionales y de las operaciones entre ellos en diferentes contextos
- ¿Qué evalúa?: La capacidad para identificar patrones numéricos y traducirlos a una expresión algebraica que generaliza la suma de una secuencia aritmética de números impares

- Respuesta Correcta: D. $n^2$

Justificación:

La secuencia de puntos forma una progresión aritmética de números impares:
1, 3, 5, 7, ..., $(2n-1)$

La suma de los primeros $n$ números impares viene dada por la fórmula $n^2$, lo 
que se puede verificar:

- Para n=1: $1 = 1^2$
- Para n=2: $1 + 3 = 2^2$
- Para n=3: $1 + 3 + 5 = 3^2$
- Para n=4: $1 + 3 + 5 + 7 = 4^2$

Motivos de las opciones incorrectas:\
  - A. $2n-1$: Representa el último término de la secuencia, no la suma\
  - B. $2n+1$: Fórmula incorrecta que no corresponde al patrón\
  - C. $n+2$: No representa el comportamiento cuadrático de la suma

- Contenidos Matemáticos Curriculares:
  - Álgebra y Cálculo
  - Período 2
    - Expresiones algebraicas
      - Operaciones con expresiones algebraicas

- No genérico
- Eje Axial Disciplinar: Eje 3 - Función Lineal, Generalizaciones y Grado 2
- Tarea: Identificar la expresión algebraica que generaliza la suma de una secuencia aritmética de números impares
- Grado: 11°