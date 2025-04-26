---
output:
  html_document: default
  word_document: default
  pdf_document: default
---
# Repositorio Matemáticas ICFES - R-Exams

<div align="center">
  <img src="https://www.r-exams.org/images/rexams_logo.svg" alt="R-exams Logo" width="200"/>
</div>

## Descripción General

Este repositorio contiene una colección de ejercicios de matemáticas para preparación de pruebas ICFES, desarrollados con el paquete R-exams. El proyecto está diseñado para generar preguntas dinámicas con alto grado de aleatorización, permitiendo crear múltiples versiones de cada ejercicio con diferentes variables, textos, nombres y situaciones, garantizando un mínimo de 300 variantes por ejercicio.

## Características Principales

- **Alta aleatorización**: Cada ejercicio puede generar cientos de versiones diferentes con variables, textos y situaciones dinámicas
- **Pruebas unitarias**: Validación automática de la coherencia matemática y diversidad de versiones
- **Gráficos dinámicos**: Generación de visualizaciones personalizadas con TikZ y matplotlib (vía reticulate)
- **Tablas dinámicas**: Creación de tablas con datos aleatorios usando TikZ y kable
- **Múltiples formatos**: Soporte para PDF, HTML, NOPS y otros formatos de salida
- **Plantillas reutilizables**: Estructura modular para facilitar la creación de nuevos ejercicios
- **Compatibilidad con LaTeX**: Soporte completo para notación matemática avanzada
- **Integración R-Python**: Utiliza reticulate para generar gráficas con matplotlib desde R
- **Metadatos ICFES**: Sistema de etiquetado para alinear ejercicios con el marco de referencia ICFES

## Estado Actual

- El repositorio cuenta con una estructura organizada por áreas temáticas de matemáticas
- Se han implementado ejercicios en varias áreas, con énfasis en estadística y probabilidad
- Se han añadido pruebas unitarias para garantizar la coherencia matemática y diversidad de versiones
- Se utilizan tecnologías como TikZ para la generación de gráficos y tablas de alta calidad
- El proyecto soporta múltiples formatos de salida (PDF, HTML, NOPS)
- Se ha implementado un sistema de metadatos ICFES para clasificar y organizar los ejercicios

## Requisitos del Sistema

- **R**: Versión 4.0 o superior
- **RStudio**: Recomendado para una mejor experiencia de desarrollo
- **LaTeX**: Necesario para la generación de documentos PDF
- **TikZ**: Requerido para la generación de gráficos vectoriales
- **Python** (opcional): Para la generación de gráficos con matplotlib vía reticulate
- **Paquetes R**: exams, knitr, ggplot2, dplyr, testthat, reticulate, entre otros (ver script install_packages.R)

## Estructura del Repositorio

El repositorio está organizado siguiendo la estructura temática del currículo de matemáticas para ICFES:

```
├── 01-Numeros-Reales/
├── 02-Funciones/
├── 03-Razones-Trigonometricas/
├── 04-Funciones-Identidades-Trigonometricas/
├── 05-Geometria-Analitica/
├── 06-Estadística-Y-Probabilidad/
├── Auxiliares/     # Scripts y documentación auxiliar
│   ├── guia_implementacion_icfes.md
│   ├── matriz_alineacion_icfes.md
│   ├── plantilla_metadatos_icfes.md
│   ├── actualizar_metadatos_icfes.R
│   └── setup_project.R
├── Lab/            # Ejercicios experimentales y pruebas
└── General/
    └── Plantillas/ # Plantillas para nuevos ejercicios
```

Cada ejercicio sigue una estructura común:

```
├── ejercicios/     # Archivos .Rmd con el código fuente
├── docus/          # Documentación adicional
├── salida/         # Archivos generados
├── erres/          # Scripts R para generación y pruebas
└── _snaps/         # Capturas para pruebas (opcional)
```

## Áreas Temáticas

### 1. Números Reales
- **Pensamiento Numérico**
  - Números Racionales
  - Números Irracionales
  - Números Reales
  - Propiedades de Expresiones Decimales
  - Conjunto de Reales y Desigualdades
  - Valor Absoluto

### 2. Funciones
- **Pensamiento Variacional y Espacial**
  - Concepto de Función, Dominio y Recorrido
  - Operaciones con Funciones
  - Composición de Funciones
  - Funciones Inyectivas y Funciones Inversas
  - Propiedades de las Funciones
  - Funciones Pares e Impares
  - Funciones Periódicas
  - Función Exponencial
  - Función Logarítmica
  - Traslación y Dilatación
  - Variación Lineal y Exponencial, Razón de Cambio
    - **Contenido Actual:**
        - Función Lineal. Auto Viajero
        - Recursos disponibles:
          - Archivos RMarkdown (.Rmd)
          - Scripts R
          - Documentación
          - Ejercicios y recursos adicionales
  - Introducción al Límite de una Sucesión

### 3. Razones Trigonométricas
- **Pensamiento Espacial Métrico y Variacional**
  - Medidas de Ángulos
  - Triángulos
  - Razones Trigonométricas en un Triángulo Rectángulo
  - Razones Trigonométricas de Ángulos Notables
  - Resolución de Triángulos Rectángulos
  - Ángulo de Elevación y Ángulo de Depresión
  - Circunferencia Unitaria
  - Razones Trigonométricas en la Circunferencia Unitaria
  - Cálculo de Razones Trigonométricas Usando Ángulos de Referencia
  - Razones Trigonométricas para Ángulos Negativos, Complementarios y Coterminales
  - Definición de las Funciones Trigonométricas
  - Teorema del Seno
  - Teorema del Coseno

### 4. Funciones e Identidades Trigonométricas
- **Pensamiento Espacial y Variacional**
  - Función Seno
  - Función Coseno
  - Gráficas de las Funciones Sinusoidales
  - Función Tangente
  - Función Cotangente
  - Función Secante
  - Función Cosecante
  - Identidades Trigonométricas Fundamentales
  - Funciones Trigonométricas en Términos de las Otras
  - Simplificación de Expresiones Trigonométricas
  - Coordenadas Polares y Cartesianas

### 5. Geometría Analítica
- **Pensamiento Espacial**
  - Coordenadas Cartesianas
  - La Línea Recta
  - Posiciones Relativas de Dos Rectas en el Plano
  - Secciones Cónicas
  - La Circunferencia
  - Ecuación Canónica de la Circunferencia con Centro en (h,k)
  - Ecuación General de la Circunferencia
  - La Parábola
  - Ecuación Canónica de la Parábola con Vértice en (h,k)
  - Ecuación General de la Parábola
  - La Elipse
  - Ecuación Canónica de la Elipse con Centro en (h,k)
  - Ecuación General de la Elipse
  - La Hipérbola
  - Ecuación Canónica de la Hipérbola con Centro en (h,k)
  - Ecuación General de la Hipérbola

### 6. Estadística y Probabilidad
- **Pensamiento Aleatorio**
  - Variables Cualitativas y Distribución de Frecuencias
    - **Contenido Actual:**
      - Gráficos Estadísticos de Adopción de Mascotas
      - Accidentalidad Vial por Género (con pruebas unitarias)
      - Recursos disponibles:
        - Archivos RMarkdown (.Rmd)
        - Scripts R
        - Documentación
        - Recursos multimedia
        - Pruebas unitarias para validación matemática
        - Ejercicios y recursos adicionales
  - Variables Cuantitativas Discretas y Distribución de Frecuencias
  - Variables Cuantitativas Continuas y Distribución de Frecuencias
  - Medidas de Tendencia Central
    - **Estado Actual:**
      - Media (Promedios)
      - Caso Práctico: Notas Faltantes
      - Recursos disponibles:
        - Archivos RMarkdown (.Rmd)
        - Scripts R
        - Documentación
  - Medidas de Dispersión
    - **Estado Actual:**
      - Intervalo de Confianza
      - Caso Práctico: Gastos Turísticos
      - Recursos disponibles:
        - Archivos RMarkdown (.Rmd)
        - Scripts R
        - Documentación
        - Ejercicios y recursos adicionales
  - Medidas de Posición
  - Probabilidad: Principios Aditivo y Multiplicativo de Conteo
    - **Contenido Actual:**
      - Diagramas de Venn
      - Caso Práctico: Géneros Musicales
      - Recursos disponibles:
        - Archivos RMarkdown (.Rmd)
        - Scripts R
        - Documentación
        - Recursos multimedia
  - Probabilidad de la Unión de Sucesos
  - Probabilidad Condicionada e Independencia de Sucesos

## Guía de Uso

### Configuración Inicial

Para configurar el entorno de trabajo:

```r
# Instalar paquetes necesarios (primera vez)
source("Auxiliares/install_packages.R")

# Configurar el entorno de trabajo
source("Auxiliares/setup_project.R")
```

### Generación de Ejercicios

Para generar ejercicios en formato PDF:

```r
library(exams)
# Generar 5 versiones diferentes
exams2pdf("ruta/al/ejercicio.Rmd", n = 5)

# Generar versiones con formato NOPS para escaneo y corrección automática
exams2nops("ruta/al/ejercicio.Rmd", n = 30, language = "es")
```

Para generar ejercicios en formato HTML:

```r
library(exams)
# Generar 5 versiones diferentes
exams2html("ruta/al/ejercicio.Rmd", n = 5)

# Generar versiones para Moodle
exams2moodle("ruta/al/ejercicio.Rmd", n = 30)
```

### Creación de Nuevos Ejercicios

Para crear un nuevo ejercicio con metadatos ICFES:

```bash
# Copiar la plantilla
cp Auxiliares/plantilla_ejercicio_icfes.Rmd mi_nuevo_ejercicio.Rmd

# Editar el archivo y completar los metadatos ICFES
```

### Ejecución de Pruebas Unitarias

Para ejecutar las pruebas unitarias de un ejercicio:

```r
# Ejecutar pruebas para un ejercicio específico
source("ruta/al/ejercicio/erres/ejecutar_pruebas.R")

# Verificar la diversidad de versiones
source("ruta/al/ejercicio/erres/verificar_diversidad.R")
```

## Novedades

### Agosto 2024
- **Implementación del sistema de metadatos ICFES**
  - Se ha añadido un sistema de etiquetado para alinear ejercicios con el marco de referencia ICFES
  - Nuevos documentos de referencia:
    - `matriz_alineacion_icfes.md`: Mapeo entre la estructura del repositorio y el marco ICFES
    - `plantilla_metadatos_icfes.md`: Descripción del sistema de etiquetado ICFES
    - `plantilla_ejercicio_icfes.Rmd`: Plantilla para crear nuevos ejercicios con metadatos ICFES
  - Script `actualizar_metadatos_icfes.R` para añadir metadatos a ejercicios existentes
  - Clasificación de ejercicios por competencias, nivel de dificultad, contenido, contexto y eje axial

### Julio 2024
- **Implementación de pruebas unitarias en ejercicios de estadística**
  - Se han añadido pruebas unitarias al ejercicio de Accidentalidad Vial por Género
  - Las pruebas garantizan:
    - Coherencia matemática en los datos generados
    - Proporciones realistas en los datos de género
    - Tendencias temporales coherentes
    - Generación de más de 300 versiones diferentes del ejercicio
    - Validación de la diversidad en los datos
  - Se incluye un script independiente para ejecutar las pruebas

### Junio 2024
- **Mejoras en la generación de gráficos**
  - Integración con Python mediante reticulate para usar matplotlib
  - Eliminación de etiquetas automáticas como 'plot of chunk GraficoA'
  - Mayor personalización de colores y estilos en TikZ
  - Soporte mejorado para tablas dinámicas
  - Optimización de la visualización en diferentes formatos de salida

## Sistema de Metadatos ICFES

El repositorio implementa un sistema de metadatos para alinear los ejercicios con el marco de referencia ICFES:

```yaml
# Metadatos ICFES
icfes:
  competencia:
    - interpretacion_representacion  # Valores posibles: interpretacion_representacion, formulacion_ejecucion, argumentacion
  nivel_dificultad: 2                # Valores posibles: 1, 2, 3, 4
  contenido:
    categoria: estadistica           # Valores posibles: algebra_calculo, geometria, estadistica
    tipo: generico                   # Valores posibles: generico, no_generico
  contexto: familiar                 # Valores posibles: familiar, laboral, comunitario, matematico
  eje_axial: eje4                    # Valores posibles: eje1, eje2, eje3, eje4
  componente: aleatorio              # Valores posibles: geometrico_metrico, numerico_variacional, aleatorio
```

Para más detalles, consulta la [Guía de implementación ICFES](./Auxiliares/guia_implementacion_icfes.md).

## Contribuciones

Este repositorio está abierto a contribuciones. Si deseas colaborar:

1. Revisa la estructura de carpetas y las plantillas existentes
2. Sigue las convenciones de nomenclatura y organización
3. Asegúrate de incluir pruebas unitarias para validar la coherencia matemática
4. Documenta adecuadamente el código y los ejercicios
5. Incluye los metadatos ICFES en tus ejercicios

## Recursos Adicionales

- [Documentación oficial de R-exams](http://www.r-exams.org/)
- [Tutorial de TikZ](https://www.overleaf.com/learn/latex/TikZ_package)
- [Documentación de matplotlib](https://matplotlib.org/stable/index.html)
- [Guía de implementación ICFES](./Auxiliares/guia_implementacion_icfes.md)
- [Matriz de alineación ICFES](./Auxiliares/matriz_alineacion_icfes.md)
- [Estándares Básicos de Competencias en Matemáticas](https://www.mineducacion.gov.co/1621/articles-116042_archivo_pdf2.pdf)

## Contacto

Para consultas o sugerencias relacionadas con este repositorio, puedes contactar al mantenedor principal a través de GitHub.

[GitHub: proyecto-r-exams-icfes-matematicas-optimizado](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
