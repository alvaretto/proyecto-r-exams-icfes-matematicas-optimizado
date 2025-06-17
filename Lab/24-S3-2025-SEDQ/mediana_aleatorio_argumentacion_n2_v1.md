---
output:
  html_document: default
  word_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float", "tikz", "xcolor"]

# Metadatos ICFES
icfes:
  competencia:
    - argumentacion
  nivel_dificultad: 2
  contenido:
    categoria: estadistica
    tipo: generico
  contexto: familiar
  eje_axial: eje4
  componente: aleatorio
---









Question
========

Un administrador de un biblioteca está analizando los datos de libros consultados registrados en 8 salas diferentes durante la última semana. Los datos recopilados se muestran en la siguiente tabla:

![](tabla_datos.png){width=8cm}

El administrador necesita calcular la mediana de estos datos para su informe semanal. Cuatro analistas han propuesto diferentes afirmaciones sobre cuál es la mediana y cómo se calcula.

**¿Cuál de las siguientes afirmaciones es CORRECTA y está BIEN JUSTIFICADA matemáticamente?**

Answerlist
----------
- La mediana es 39 porque es el promedio de los dos valores centrales (38 y 40)
- La mediana es 26 porque no hay valores repetidos, entonces se toma el menor
- La mediana es 54 porque es uno de los valores extremos del conjunto
- La mediana es 40.9 porque se calcula sumando todos los valores y dividiendo por el número de datos

Solution
========

Para resolver este problema de **argumentación matemática**, debemos evaluar cada afirmación y determinar cuál está correctamente justificada según las propiedades de la mediana.

**Datos ordenados:** 26, 29, 37, 38, 40, 50, 53, 54

**Análisis de la mediana:**

Como tenemos 8 datos (número **par**), la mediana es el promedio de los dos valores centrales.

Posiciones centrales: 4 y 5 

Valores centrales: 38 y 40 

Mediana = ( 38 + 40 ) ÷ 2 = 39 

**Evaluación de las afirmaciones:**

La afirmación correcta es: **"La mediana es 39 porque es el promedio de los dos valores centrales (38 y 40)"**

**¿Por qué es correcta?**
- Aplica correctamente la definición de mediana
- Utiliza el procedimiento matemático apropiado según el número de datos
- La justificación es coherente con las propiedades estadísticas

**¿Por qué las otras afirmaciones son incorrectas?**
- Confunden conceptos estadísticos (media, moda, mediana)
- Aplican procedimientos incorrectos
- No siguen las reglas matemáticas establecidas para el cálculo de la mediana

Answerlist
----------
- Verdadero
- Falso
- Falso
- Falso

Meta-information
================
exname: mediana_argumentacion_estadistica
extype: schoice
exsolution: 1000
exshuffle: TRUE
exsection: Argumentación en Estadística
