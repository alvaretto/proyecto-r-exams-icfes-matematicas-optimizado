---
output:
  html_document: default
  word_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float", "amsmath"]
icfes:
  competencia: interpretacion_representacion
  componente: aleatorio
  afirmacion: Interpreta información presentada en tablas y gráficos
  evidencia: Compara e interpreta datos presentados en gráficos de barras y circulares
  nivel: 2
  tematica: Interpretación de gráficos estadísticos
  contexto: laboral
# CONFIGURACIÓN DE TOLERANCIAS PARA EVALUACIÓN AUTOMÁTICA:
# - Tipo: cloze (5 respuestas numéricas + 1 schoice)
# - Tolerancias: 1 para numéricas (valores monetarios), 0 para schoice
# - Formato: Sin separador de miles, punto decimal, sin notación científica
---



Test passed 🥳
Test passed 🎊
Test passed 🎊
Test passed 🥳




```
## Gráficas generadas exitosamente
```

Question
========

María lleva un registro detallado de los gastos relacionados con su motocicleta. La tabla muestra los gastos semanales durante un mes completo, organizados por categorías.

![](tabla_gastos.png){width=95%}

<br><br>

## Gráficas de análisis

A continuación se presentan cuatro tipos diferentes de gráficas que representan los mismos datos de la tabla:

<br>

![](grafica_circular_categoria.png){width=65%}

![](grafica_circular_semana.png){width=65%}

![](grafica_barras_apiladas.png){width=65%}

![](grafica_barras_agrupadas.png){width=65%}

<br><br>

## Análisis paso a paso

Para analizar qué categoría representó el mayor porcentaje del gasto total mensual, resuelva paso a paso:

**IMPORTANTE - Formato de números:**

- **Valores monetarios**: Sin separador de miles, use punto para decimales
  - Ejemplo: $16850.32 (no $16.850,32 ni $16850,32)
- **Respuestas numéricas**: Sin separador de miles, use punto para decimales
  - Ejemplo: 1234.5678 (no 1.234,5678 ni 1234,5678)

### Paso 1: Identificación del mayor gasto por categoría
Observe la tabla y identifique cuál fue el mayor gasto por categoría (total por categoría).

**Respuesta:** $##ANSWER1##

### Paso 2: Identificación del gasto total mensual
Según la tabla, ¿cuál es el gasto total del mes (suma de todas las semanas)?

**Respuesta:** $##ANSWER2##

### Paso 3: Configuración de la fórmula de porcentaje
Para calcular el porcentaje, complete la fórmula con los valores identificados:

Porcentaje = ( ##ANSWER3## ÷ ##ANSWER4## ) × 100%

### Paso 4: Verificación de valores
Confirme que los valores del numerador y denominador son correctos:

- Numerador (mayor gasto por categoría): $##ANSWER3##
- Denominador (gasto total mensual): $##ANSWER4##

### Paso 5: Cálculo del porcentaje final
Complete el cálculo del porcentaje:

Porcentaje = ( ##ANSWER3## ÷ ##ANSWER4## ) × 100% = ##ANSWER5##%

### Paso 6: Confirmación del tipo de gráfica (CON PUNTUACIÓN)
Basándose en su análisis anterior, **seleccione qué tipo de gráfica permite identificar más fácilmente qué categoría de gasto representa el mayor porcentaje del gasto total mensual**:

##ANSWER6##

**Conclusión:** La categoría con mayor gasto representó el ##ANSWER5##% del gasto total mensual.

Answerlist
----------
* A
* B
* C
* D

Solution
========

**NOTA IMPORTANTE - Configuración de evaluación automática:**

- **Tolerancias configuradas**: Tolerancia 1 para respuestas numéricas monetarias, tolerancia 0.1 para porcentajes, tolerancia 0 para respuestas schoice
- **Justificación**: Los valores monetarios son enteros grandes, tolerancia 1 evita rechazos incorrectos por diferencias mínimas de formato manteniendo precisión matemática
- **Formato numérico**: Sin separador de miles, punto como separador decimal

### Análisis paso a paso del problema de gastos de vehículo

Este problema de **interpretación de tablas** y **cálculo de porcentajes** requiere un análisis secuencial que demuestre el proceso de razonamiento matemático aplicado a contextos de gastos personales:

**NOTA IMPORTANTE - Formato de números estandarizado:**

- **Valores monetarios**: Sin separador de miles, use punto para decimales
  - Ejemplo: $16850.32 (no $16.850,32 ni $16850,32)
- **Respuestas numéricas**: Sin separador de miles, punto como separador decimal
  - Ejemplo: 1234.5678 (no 1.234,5678 ni 1234,5678)
- **Consistencia**: Mismo formato en enunciado, opciones y respuestas

### Paso 1: Identificación correcta del mayor gasto por categoría ✓

**Respuesta correcta:** $153102

**Análisis de gastos por categoría:**

- Gasolina: $153102
- Parqueadero: $99447
- Mantenimiento: $64330

La categoría Gasolina tuvo el mayor gasto con $153102.

### Paso 2: Identificación del gasto total mensual ✓

**Respuesta correcta:** $316879

El total mensual se calcula sumando todos los gastos semanales:

$$\text{Total mensual} = 82841 + 82111 + 81704 + 70223 = 316879$$

### Paso 3: Configuración correcta de la fórmula ✓

**Respuestas correctas:** Numerador = 153102, Denominador = 316879

La fórmula de porcentaje requiere:

- **Numerador:** El valor observado (153102 pesos)
- **Denominador:** El valor total (316879 pesos)

### Paso 4: Verificación de valores ✓

Los valores son coherentes con los datos de la tabla y representan correctamente:

- El mayor gasto semanal individual
- El gasto total mensual (suma de todas las semanas)

### Paso 5: Cálculo del porcentaje final ✓

**Respuesta correcta:** 48.3%

$$\text{Porcentaje} = \frac{153102}{316879} \times 100\% = 48.3\%$$

### Paso 6: Confirmación del tipo de gráfica ✓ (CON PUNTUACIÓN)

**Opciones presentadas:**

- **A**: A ← **RESPUESTA CORRECTA**
- **B**: B
- **C**: C
- **D**: D

**Análisis de la respuesta correcta:**

"A"

- Esta opción permite visualizar directamente qué categoría representa el mayor porcentaje del total mensual
- Muestra claramente las proporciones de cada categoría de gasto respecto al total
- Facilita la identificación inmediata de la categoría con mayor participación porcentual
- Es el tipo de gráfica más adecuado para comparar partes de un todo

**Análisis de distractores:**

- **Gráfica circular por semana**: Muestra proporciones de semanas, no de categorías
- **Gráfica de barras apiladas por semana**: Muestra composición por semana pero no facilita comparación de categorías
- **Gráfica de barras agrupadas por categoría**: Permite comparar categorías pero no muestra proporciones del total

### Verificación del proceso de razonamiento completo

**Datos del problema:**

- Mayor gasto por categoría: $153102 (Gasolina)
- Gasto total mensual: $316879
- Porcentaje representado: 48.3%

**El formato híbrido con puntuación dual (cloze + schoice) garantiza que los estudiantes:**

**Parte Analítica (Pasos 1-5):**

- **Lean cuidadosamente** la tabla para extraer datos precisos
- **Identifiquen correctamente** el mayor valor semanal y el total mensual
- **Configuren correctamente** la fórmula de porcentaje paso a paso
- **Realicen cálculos** matemáticos sin saltar etapas del proceso

**Parte de Confirmación (Paso 6):**

- **Demuestren coherencia** entre su análisis numérico y la comprensión conceptual
- **Identifiquen el tipo de gráfica** más apropiado para el análisis requerido
- **Consoliden su aprendizaje** mediante validación de resultados

### Conclusión

La categoría con mayor gasto representó el **48.3%** del gasto total mensual de María en su motocicleta.

Esta respuesta es coherente porque:

- Se basa en una lectura correcta de la tabla
- Aplica correctamente la fórmula de porcentaje
- El resultado está dentro del rango esperado (0% a 100%)
- La gráfica seleccionada es la más apropiada para este tipo de análisis

**Verificación adicional**: El total de gastos por categorías (Gasolina: $153102, Parqueadero: $99447, Mantenimiento: $64330) suma exactamente $316879, confirmando la coherencia de los datos.

Meta-information
================
exname: Gastos Carro Gráficas Comparación - Análisis Secuencial Cloze
extype: cloze
exsolution: 153102|316879|153102|316879|48.3|1000
exclozetype: num|num|num|num|num|schoice
extol: 1|1|1|1|0.1|0
exsection: Estadística|Interpretación de tablas|Porcentajes|Análisis de datos
exextra[Type]: Cálculo
exextra[Program]: R
exextra[Language]: es
exextra[Level]: 2
exextra[Competencia]: Interpretación y representación
exextra[Componente]: Aleatorio y sistemas de datos
exextra[Contexto]: Laboral
exextra[Dificultad]: Media
