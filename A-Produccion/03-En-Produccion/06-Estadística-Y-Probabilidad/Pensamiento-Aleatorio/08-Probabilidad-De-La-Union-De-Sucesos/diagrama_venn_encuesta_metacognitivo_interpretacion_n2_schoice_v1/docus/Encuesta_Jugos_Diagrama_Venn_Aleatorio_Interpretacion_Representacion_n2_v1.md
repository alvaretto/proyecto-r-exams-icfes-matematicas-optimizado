---
output:
  pdf_document:
    latex_engine: xelatex
  html_document: default
---

# Pregunta: Diagrama de Venn — Encuesta Jugos en Restaurante

-------------------

![](all.png)

-------------------

### Análisis

## Descripción del Problema

En un restaurante se realizó una encuesta a 76 clientes sobre el jugo o los jugos que prefieren para acompañar el almuerzo. La dueña del restaurante graficó los resultados en un diagrama de Venn de tres conjuntos: **Jugo de Mango**, **Jugo de Fresa** y **Jugo de Mora**.

Los valores del diagrama de Venn son:

| Región | Valor |
|---|---|
| Solo Mango | 7 |
| Mango $\cap$ Fresa (sin Mora) | 21 |
| Solo Fresa | 5 |
| Mango $\cap$ Mora (sin Fresa) | 15 |
| Mango $\cap$ Fresa $\cap$ Mora | 7 |
| Fresa $\cap$ Mora (sin Mango) | 11 |
| Solo Mora | 8 |
| Ningún jugo | 2 |

**Verificación:** $7 + 21 + 5 + 15 + 7 + 11 + 8 + 2 = 76$ $\checkmark$

**Pregunta:** ¿En cuál de los siguientes diagramas de Venn aparece sombreada la región correspondiente a los clientes que prefieren jugo de Mora y de Fresa **pero no** de Mango?

## Solución

### Paso 1: Traducir el enunciado a operaciones de conjuntos

La condición "prefieren jugo de Mora **y** de Fresa **pero no** de Mango" se traduce como:

$$(Mora \cap Fresa) \setminus Mango$$

Es decir, la intersección de los conjuntos Mora y Fresa **excluyendo** la parte que también pertenece a Mango.

### Paso 2: Localizar la región en el diagrama

En el diagrama de Venn original, la intersección completa de Mora y Fresa tiene dos subregiones:
- Fresa $\cap$ Mora $\cap$ Mango (triple intersección) = **7**
- Fresa $\cap$ Mora sin Mango = **11**

Al aplicar la exclusión de Mango, solo queda la región con valor **11**: la zona donde los círculos de Fresa y Mora se cruzan, pero **fuera** del círculo de Mango.

### Paso 3: Comparar con las opciones

- **Opción A:** Sombrea la intersección completa de Fresa y Mora, incluyendo la triple intersección con Mango (valores 11 + 7 = 18). **No excluye Mango.** Incorrecta.
- **Opción B:** Sombrea exclusivamente la zona de intersección entre Fresa y Mora que queda fuera del círculo de Mango (valor 11). **Correcta.**
- **Opción C:** Sombrea la zona de Mora que no pertenece a Mango, incluyendo regiones fuera de Fresa (valores 8 + 11 = 19). Aplica la exclusión de Mango pero no la intersección con Fresa. **Incorrecta.**
- **Opción D:** Sombrea la intersección de Mango y Mora sin Fresa (valor 15). Confunde los conjuntos Fresa y Mango. **Incorrecta.**

## Conclusión

La respuesta correcta es la opción **B**, ya que es la única que sombrea exactamente la región $(Mora \cap Fresa) \setminus Mango$, correspondiente a los 11 clientes que prefieren jugo de Mora y de Fresa pero no de Mango.

------------------------

### Análisis según SABER 11 ICFES

- Nivel de Desempeño: 2
  - El estudiante debe identificar una región representativa en un diagrama de Venn a partir del significado que tiene en la situación. No requiere manipulaciones aritméticas, sino la traducción de una descripción verbal a una representación gráfica. Corresponde al descriptor: *"Identifica valores o puntos representativos en diferentes tipos de registro a partir del significado que tienen en la situación."*

- Competencia: Interpretación y Representación
  - Afirmación: Comprende y transforma la información cuantitativa y esquemática presentada en distintos formatos
  - Evidencia: Transforma la representación de una o más piezas de información

- Componente: Aleatorio

- Estándar Asociado: Resuelvo y formulo problemas a partir de un conjunto de datos provenientes de observaciones, consultas o experimentos

- ¿Qué evalúa?: La capacidad de transformar una descripción verbal (clientes que prefieren Mora y Fresa pero no Mango) en su representación gráfica correspondiente dentro de un diagrama de Venn de tres conjuntos, identificando la región $(Mora \cap Fresa) \setminus Mango$

- Respuesta Correcta: B
  - Justificación: La región de clientes que prefieren jugo de Mora y de Fresa pero no de Mango corresponde a la zona donde se solapan los círculos de Mora y Fresa sin estar dentro del círculo de Mango (valor 11). La opción B sombrea exclusivamente esa zona, siendo la única que aplica correctamente la operación de intersección con exclusión.

- Distractores:

  - A: El estudiante no aplica la condición de exclusión ("pero no de Mango") y sombrea la intersección completa de Mora y Fresa, incluyendo la zona de la triple intersección que también pertenece a Mango (valores 11 + 7). Confunde $(Mora \cap Fresa)$ con $(Mora \cap Fresa) \setminus Mango$. Este error revela que el estudiante comprende la intersección de dos conjuntos pero no domina la operación de diferencia.

  - C: El estudiante aplica correctamente la exclusión de Mango pero no restringe la intersección a Fresa: sombrea toda la zona de Mora que queda fuera de Mango (valores 8 + 11), es decir $Mora \setminus Mango$ en lugar de $(Mora \cap Fresa) \setminus Mango$. Interpreta parcialmente el enunciado, atendiendo a "Mora pero no Mango" y omitiendo la condición "y de Fresa".

  - D: El estudiante confunde los conjuntos Fresa y Mango, sombreando la intersección de Mango y Mora sin Fresa (valor 15) en lugar de Fresa y Mora sin Mango. Este error es típico de una lectura apresurada del enunciado o de una mala ubicación espacial de los conjuntos en el diagrama de Venn.

- Contenidos Matemáticos Curriculares:
  - Estadística
  - Período: 3
    - Conjuntos, Combinatoria y Probabilidad
      - Construcción e interpretación de diagramas de Venn (dos y tres conjuntos)

- Genérico: Sí — Intersección, unión y contenencia de conjuntos es contenido genérico de Estadística

- Eje Axial Disciplinar: Eje 1 — Diferentes a Series y Tablas: Interpretación y análisis de diagramas

- Tarea: Identificar en un diagrama de Venn de tres conjuntos la región sombreada que representa la intersección de dos conjuntos excluyendo un tercero

- Grado: 8° - 9°

------------------------

### Objeto JSON

```json
{
  "nivel_de_desempeno": 2,
  "competencia": {
    "nombre": "Interpretación y Representación",
    "afirmacion_aprendizaje": "Comprende y transforma la información cuantitativa y esquemática presentada en distintos formatos.",
    "evidencia": "Transforma la representación de una o más piezas de información."
  },
  "componente": "Aleatorio",
  "estandar_asociado": "Resuelvo y formulo problemas a partir de un conjunto de datos provenientes de observaciones, consultas o experimentos.",
  "que_evalua": "La capacidad de transformar una descripción verbal (clientes que prefieren Mora y Fresa pero no Mango) en su representación gráfica correspondiente dentro de un diagrama de Venn de tres conjuntos, identificando la región (Mora ∩ Fresa) \\ Mango.",
  "respuesta_correcta": {
    "opcion": "B",
    "justificacion": "La región de clientes que prefieren jugo de Mora y de Fresa pero no de Mango corresponde a la zona donde se solapan los círculos de Mora y Fresa sin estar dentro del círculo de Mango (valor 11). La opción B sombrea exclusivamente esa zona."
  },
  "motivos_distractores": {
    "A": "No aplica la condición de exclusión y sombrea la intersección completa de Mora y Fresa incluyendo la triple intersección con Mango (valores 11 + 7). Confunde (Mora ∩ Fresa) con (Mora ∩ Fresa) \\ Mango.",
    "C": "Aplica la exclusión de Mango pero no restringe a la intersección con Fresa: sombrea Mora \\ Mango (valores 8 + 11) en lugar de (Mora ∩ Fresa) \\ Mango. Atiende parcialmente al enunciado omitiendo la condición 'y de Fresa'.",
    "D": "Confunde los conjuntos Fresa y Mango, sombreando la intersección Mango ∩ Mora sin Fresa (valor 15) en lugar de Fresa ∩ Mora sin Mango. Error de lectura o mala ubicación espacial."
  },
  "contenidos_matematicos_curriculares": {
    "categoria": "Estadística",
    "periodo": 3,
    "tema": "Conjuntos, Combinatoria y Probabilidad",
    "subtema": "Construcción e interpretación de diagramas de Venn (dos y tres conjuntos)"
  },
  "generico_o_no_generico": "Genérico",
  "eje_axial_disciplinar": "Eje 1 — Diferentes a Series y Tablas: Interpretación y análisis de diagramas",
  "tarea": "Identificar en un diagrama de Venn de tres conjuntos la región sombreada que representa la intersección de dos conjuntos excluyendo un tercero.",
  "grado": "8° - 9°"
}
```
