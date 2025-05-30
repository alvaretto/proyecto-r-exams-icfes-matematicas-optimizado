---
output:
  word_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float", "tikz", "colortbl"]
  html_document: default
---






Question
========

En la cuadro se muestran las distribución de participantes en un curso intensivo, dependiendo del género y la edad.

\begin{center}
\begin{tabular}{|c|c|c|}
\hline
\textbf{Grupo de edad} & \textbf{Hombres} & \textbf{Participantes Femeninas} \\
\hline
Menores de 17 años & 0.1 & 0.2 \\
\hline
Con Más de 17 años & 0.3 & 0.4 \\
\hline
\end{tabular}
\end{center}

Por ejemplo, el 40% de los participantes son participantes femeninas con más de 17 años. Según la cuadro, ¿cuál es la probabilidad de que al escoger una persona al azar tenga con más de 17 años, si ya se sabe que es participantes femeninas?

Answerlist
----------
- 0.6/0.4
- 0.4/1.0
- 0.4/0.4
- 0.4/0.6

Solution
========

Para resolver este problema de probabilidad condicional, necesitamos aplicar la fórmula de probabilidad condicional y trabajar con la información de la tabla de contingencia.

### Paso 1: Identificar el tipo de problema
Este es un problema de **probabilidad condicional**, donde buscamos:
$$P(\\text{con más de 17 años} | \\text{participantes femeninas})$$

### Paso 2: Recordar la fórmula de probabilidad condicional
La probabilidad condicional se calcula como:
$$P(A|B) = \\frac{P(A \\cap B)}{P(B)}$$

Donde:
- $A$ = evento de interés (con más de 17 años)
- $B$ = condición dada (participantes femeninas)
- $P(A \\cap B)$ = probabilidad de que ocurran ambos eventos
- $P(B)$ = probabilidad de la condición

### Paso 3: Extraer información de la tabla
De la tabla de contingencia podemos obtener:

**Probabilidades conjuntas:**
- P(menores de 17 años $\cap$ hombres) = 0.1
- P(menores de 17 años $\cap$ participantes femeninas) = 0.2
- P(con más de 17 años $\cap$ hombres) = 0.3
- P(con más de 17 años $\cap$ participantes femeninas) = 0.4

**Probabilidades marginales:**
- P(hombres) = 0.1 + 0.3 = 0.4
- P(participantes femeninas) = 0.2 + 0.4 = 0.6
- P(menores de 17 años) = 0.1 + 0.2 = 0.3
- P(con más de 17 años) = 0.3 + 0.4 = 0.7

### Paso 4: Aplicar la fórmula
Para nuestro problema específico:
$$P(\\text{con más de 17 años} | \\text{participantes femeninas}) = \\frac{P(\\text{con más de 17 años} \\cap \\text{participantes femeninas})}{P(\\text{participantes femeninas})}$$

Sustituyendo los valores:
$$P(\\text{con más de 17 años} | \\text{participantes femeninas}) = \\frac{0.4}{0.6}$$

### Paso 5: Verificación
Podemos verificar que este resultado tiene sentido:
- El numerador (0.4) representa la probabilidad conjunta del evento y la condición
- El denominador (0.6) representa la probabilidad marginal de la condición
- El resultado 0.4/0.6 = 0.667 está entre 0 y 1, como debe ser toda probabilidad

### Análisis de distractores comunes:
- **0.6/0.4**: Error de invertir numerador y denominador
- **0.4/1.0**: Error de usar solo la probabilidad conjunta sin dividir por la probabilidad de la condición
- **0.4/0.4**: Error de usar el complemento de la probabilidad de la condición

### Conclusión
Por lo tanto, la probabilidad de que una persona tenga con más de 17 años, dado que es participantes femeninas, es **0.4/0.6**.

Answerlist
----------
- Falso
- Falso
- Falso
- Verdadero

Meta-information
================
exname: probabilidad_condicional_tabla_contingencia_mejorado
extype: schoice
exsolution: 0001
exshuffle: TRUE
exsection: Probabilidad|Probabilidad condicional|Tablas de contingencia
exextra[Type]: Cálculo
exextra[Program]: R
exextra[Language]: es
exextra[Level]: 3
