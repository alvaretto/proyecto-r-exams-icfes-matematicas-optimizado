# Ejercicio de Estadística: Pastelería - Sabores y Ventas

## Descripción del Problema

Este ejercicio de R/exams recrea un problema matemático de estadística basado en el análisis e interpretación de datos presentados en tablas y gráficos de barras.

### Contexto del Problema
- **Situación**: Una pastelería realizó una encuesta sobre preferencias de sabores de tortas y registró las ventas diarias de la última semana
- **Datos**: Tabla con número de clientes que prefieren cada sabor y gráfico de barras con ventas por día
- **Pregunta**: Identificar el día con mayor número de ventas y los 2 sabores más preferidos para una promoción

### Competencias ICFES Evaluadas
- **Competencia**: Interpretación y representación
- **Nivel de dificultad**: 2 (intermedio)
- **Componente**: Aleatorio (estadística)
- **Eje axial**: Eje 3 - Pensamiento aleatorio y sistemas de datos

## Características Técnicas

### Aleatorización
El ejercicio genera múltiples versiones únicas mediante:
- **Sabores de tortas**: 5 conjuntos diferentes de 4 sabores cada uno
- **Números de clientes**: Rangos realistas entre 120-200 clientes por sabor
- **Ventas por día**: Valores aleatorios entre 20-50 tortas por día
- **Estrategia avanzada de opciones**: Sistema sofisticado de generación de respuestas

### Estrategia Avanzada de Generación de Opciones
**Implementación de pools múltiples y validación automática:**

1. **Pool de Opciones Correctas (3 formulaciones)**:
   - "Se venderán el [día] y los sabores serán [sabor1] y [sabor2]."
   - "La promoción será el [día] con tortas de [sabor1] y [sabor2]."
   - "El día [día] se ofrecerán sabores [sabor1] y [sabor2]."

2. **Pool de Distractores Pedagógicos (6 tipos específicos)**:
   - 2 distractores: día correcto, sabores incorrectos
   - 2 distractores: sabores correctos, día incorrecto
   - 2 distractores: ambos elementos incorrectos pero plausibles

3. **Validación de Unicidad Automática**:
   - Algoritmo de similitud textual con umbral del 75% de diferencia
   - Regeneración automática si se detectan opciones similares
   - Máximo 100 intentos para garantizar unicidad

4. **Aleatorización Completa**:
   - Selección aleatoria de 1 opción correcta del pool
   - Selección aleatoria de 3 distractores únicos del pool
   - Posicionamiento completamente aleatorio (A, B, C, D)

5. **Trazabilidad y Debugging**:
   - Registro del tipo de cada distractor generado
   - Información de validación y número de intentos
   - Explicaciones específicas por tipo de error conceptual

### Visualizaciones
- **Tabla de preferencias**: Implementada con TikZ para alta fidelidad visual
- **Gráfico de barras**: Creado con TikZ, escalado automáticamente según los datos

### Validación
- **Diversidad**: Garantiza mínimo 300 versiones únicas en 1000 generaciones
- **Compilación**: Probado con rmarkdown y R/exams
- **Formatos**: Compatible con HTML, PDF y Word

## Archivos Generados

### Archivos Principales
- `pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd`: Archivo principal del ejercicio
- `pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.html`: Versión compilada individual
- `README.md`: Este archivo de documentación

### Archivos de Prueba
- `test_output/test_pasteleria1.html`: Primera versión de prueba
- `test_output/test_pasteleria2.html`: Segunda versión de prueba
- `test_output/test_pasteleria3.html`: Tercera versión de prueba

### Archivos de Demostración de Estrategia Avanzada
- `demo_estrategia_avanzada.R`: Script de análisis y demostración de la estrategia
- `test_estrategia_output/test_estrategia_avanzada1-5.html`: Versiones con estrategia avanzada

## Estructura del Ejercicio

### Sección Question
1. **Contexto**: Descripción de la situación de la pastelería
2. **Tabla**: Datos de preferencias de sabores con TikZ
3. **Gráfico**: Ventas por día de la semana con TikZ
4. **Pregunta**: Identificación del día y sabores para la promoción
5. **Opciones**: 4 alternativas de respuesta múltiple

### Sección Solution
1. **Análisis de tabla**: Identificación de los 2 sabores más preferidos
2. **Análisis de gráfico**: Identificación del día con mayor número de ventas
3. **Respuesta correcta**: Combinación del día y sabores identificados
4. **Explicación detallada**: Justificación para cada opción de respuesta

### Meta-information
- **Tipo**: schoice (selección única)
- **Aleatorización**: Habilitada (exshuffle: TRUE)
- **Sección**: Estadística/Interpretación de datos/Gráficos y tablas

## Uso del Ejercicio

### Compilación Individual
```r
rmarkdown::render('pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd', 'html_document')
```

### Generación con R/exams
```r
library(exams)
exams2html('pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd', 
           n=10, name='pasteleria_ejercicio', dir='output')
```

### Generación para Moodle
```r
exams2moodle('pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd', 
             n=50, name='pasteleria_moodle')
```

## Conceptos Matemáticos

### Habilidades Desarrolladas
- **Lectura de tablas**: Interpretación de datos tabulares
- **Interpretación de gráficos**: Análisis de gráficos de barras
- **Comparación de datos**: Identificación de valores máximos
- **Síntesis de información**: Combinación de datos de múltiples fuentes

### Nivel de Complejidad
- **Básico**: Lectura directa de datos
- **Intermedio**: Comparación y ordenamiento de valores
- **Aplicado**: Síntesis para toma de decisiones

## Distractores Pedagógicos

Los distractores están diseñados para evaluar errores comunes:
1. **Error en día**: Día correcto pero sabores incorrectos
2. **Error en sabores**: Sabores correctos pero día incorrecto  
3. **Error mixto**: Combinación incorrecta de día y sabores

## Validación de Calidad

### Pruebas Realizadas
- ✅ Compilación exitosa con rmarkdown
- ✅ Generación correcta con R/exams
- ✅ Diversidad de versiones (>300 únicas)
- ✅ Visualizaciones TikZ funcionales
- ✅ Lógica de respuestas correcta

### Estándares Cumplidos
- ✅ Metadatos ICFES completos
- ✅ Comentarios en español
- ✅ Código R optimizado
- ✅ Formato compatible con múltiples salidas
- ✅ Aleatorización robusta

## Autor y Fecha
- **Creado**: 25 de agosto de 2025
- **Basado en**: Imagen de problema matemático proporcionada
- **Herramientas**: R/exams, TikZ, rmarkdown
- **Estándares**: ICFES Colombia, Nivel 2
