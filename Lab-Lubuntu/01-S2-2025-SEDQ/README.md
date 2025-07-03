# Ejercicio de Gráfico Circular de Bienes

## Descripción General

Este proyecto contiene un ejercicio interactivo de matemáticas para evaluar la interpretación de gráficos circulares (diagramas de pastel) y el cálculo de proporciones. El ejercicio está diseñado para ser utilizado en plataformas educativas como Moodle a través del paquete R-Exams.

El archivo principal `grafico_circular_bienes_v0.Rmd` genera un problema matemático donde los estudiantes deben interpretar un gráfico circular que muestra la distribución de diferentes tipos de bienes (como carros, casas y apartamentos) que poseen las personas de una organización, y calcular valores específicos utilizando reglas de tres y proporciones.

## Archivos del Proyecto

- **grafico_circular_bienes_v0.Rmd**: Archivo principal que genera el ejercicio con aleatorización de variables.
- **pruebas_unitarias_grafico_circular_bienes.R**: Conjunto de pruebas unitarias para verificar la calidad y robustez del ejercicio.
- **01-ejecutar_pruebas_grafico_circular.R**: Script ejecutable para correr las pruebas unitarias.
- **README.md**: Este archivo, que proporciona documentación sobre el proyecto.

## Características Principales

### Aleatorización Avanzada

El ejercicio implementa una aleatorización robusta que incluye:

- **Términos contextuales**: Variación en los términos utilizados para describir bienes (carro, auto, vehículo, etc.), contextos organizacionales (empresa, organización, compañía, etc.) y otros elementos narrativos.
- **Valores numéricos**: Aleatorización de porcentajes y cantidades manteniendo coherencia matemática.
- **Elementos visuales**: Variación en las paletas de colores del gráfico circular.
- **Opciones de respuesta**: Generación de distractores plausibles pero inequívocamente incorrectos.

### Visualización de Alta Calidad

- Gráfico circular generado con Python (matplotlib) que incluye:

  - Etiquetas externas con líneas conectoras
  - Porcentajes visibles en cada sector
  - Paletas de colores visualmente atractivas
  - Diseño optimizado para evitar solapamiento de etiquetas

### Compatibilidad con Múltiples Formatos

El ejercicio está diseñado para funcionar correctamente en diferentes formatos de salida:

- Moodle (HTML)
- PDF
- Word
- NOPS (exámenes impresos escaneables)

## Objetivos Pedagógicos

1. **Interpretación de gráficos**: Desarrollar la capacidad de extraer información de representaciones visuales de datos.
2. **Cálculo de proporciones**: Practicar el uso de reglas de tres y cálculos porcentuales.
3. **Resolución de problemas**: Aplicar razonamiento matemático para resolver situaciones contextualizadas.
4. **Pensamiento crítico**: Evaluar diferentes opciones de respuesta y descartar distractores.

## Requisitos Técnicos

- R (versión 3.6 o superior)
- Paquetes de R:
  - exams
  - reticulate
  - digest
  - testthat
  - knitr
- Python (versión 3.6 o superior)
- Bibliotecas de Python:
  - matplotlib
  - numpy

## Uso

### Generación del Ejercicio

Para generar el ejercicio en diferentes formatos:

```r
library(exams)

# Para Moodle
exams2moodle("Lab/01-S2-2025-SEDQ/grafico_circular_bienes_v0.Rmd", n = 10)

# Para PDF
exams2pdf("Lab/01-S2-2025-SEDQ/grafico_circular_bienes_v0.Rmd", n = 10)

# Para Word
exams2docx("Lab/01-S2-2025-SEDQ/grafico_circular_bienes_v0.Rmd", n = 10)

# Para NOPS (exámenes impresos escaneables)
exams2nops("Lab/01-S2-2025-SEDQ/grafico_circular_bienes_v0.Rmd", n = 10)
```

### Ejecución de Pruebas Unitarias

Para verificar la calidad y robustez del ejercicio:

```bash
Rscript Lab/01-S2-2025-SEDQ/01-ejecutar_pruebas_grafico_circular.R
```

## Garantías de Calidad

Las pruebas unitarias verifican:

1. **Variabilidad**: Capacidad de generar al menos 300 versiones diferentes del ejercicio.
2. **Coherencia matemática**: Validez de los cálculos en todas las versiones.
3. **Opciones de respuesta**: Unicidad y plausibilidad de las opciones.
4. **Estructura del ejercicio**: Conformidad con los estándares de R-Exams.
5. **Elementos visuales**: Correcta generación del gráfico circular con Python.

## Posibilidades de Mejora

1. **Ampliación de contextos**: Incorporar más escenarios y contextos para aumentar la variabilidad.
2. **Niveles de dificultad**: Implementar diferentes niveles de complejidad matemática.
3. **Accesibilidad**: Mejorar la accesibilidad para estudiantes con discapacidad visual mediante descripciones alternativas.
4. **Interactividad**: Añadir elementos interactivos para versiones HTML.
5. **Feedback adaptativo**: Proporcionar retroalimentación específica según el tipo de error cometido.
6. **Integración con análisis de datos**: Recopilar y analizar patrones de respuesta para identificar conceptos erróneos comunes.
7. **Expansión a otros tipos de gráficos**: Crear variantes con gráficos de barras, líneas o dispersión.
8. **Multilenguaje**: Implementar soporte para múltiples idiomas.

## Contribución

Para contribuir a este proyecto:

1. Realice un fork del repositorio
2. Cree una rama para su característica (`git checkout -b feature/nueva-caracteristica`)
3. Realice sus cambios y asegúrese de que las pruebas unitarias pasen
4. Envíe un pull request

## Licencia

Este proyecto está disponible bajo la licencia Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0).

## Contacto

Para preguntas o sugerencias, contacte a través del repositorio o mediante los canales oficiales del proyecto.

---

## Notas Técnicas

### Estructura del Código

El archivo `grafico_circular_bienes_v0.Rmd` está organizado en secciones:

1. **Configuración inicial**: Metadatos YAML y configuración del entorno R
2. **Definición de variables**: Aleatorización de términos, valores y elementos visuales
3. **Generación del gráfico**: Código Python para crear el gráfico circular
4. **Pregunta**: Enunciado del problema con variables aleatorizadas
5. **Opciones de respuesta**: Lista de posibles respuestas
6. **Solución**: Explicación detallada del proceso de resolución
7. **Metainformación**: Datos para R-Exams

### Algoritmo de Aleatorización

El ejercicio utiliza un enfoque de aleatorización por capas:

1. Aleatorización de términos contextuales (léxico)
2. Aleatorización de valores numéricos con restricciones de coherencia
3. Aleatorización de elementos visuales (colores, estilos)
4. Generación de distractores basados en errores comunes

### Consideraciones para Implementación en Producción

- Asegúrese de que Python esté correctamente configurado en el servidor
- Verifique la compatibilidad con la versión de Moodle u otra plataforma LMS
- Considere el tiempo de generación al crear grandes bancos de preguntas
- Ejecute las pruebas unitarias antes de implementar en producción para verificar la calidad del ejercicio
