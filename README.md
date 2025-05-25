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

- **Repositorio en crecimiento activo**: Más de 50 ejercicios implementados y en desarrollo
- **Estructura organizada**: Organización por áreas temáticas de matemáticas y laboratorio de desarrollo
- **Ejercicios de alta calidad**: Implementados en múltiples áreas con énfasis en estadística, probabilidad, funciones y geometría
- **Pruebas unitarias robustas**: Sistema de validación automática para garantizar coherencia matemática y diversidad de versiones (mínimo 300 variantes por ejercicio)
- **Tecnologías avanzadas**:
  - TikZ para gráficos vectoriales y tablas de alta calidad
  - Python (matplotlib) vía reticulate para visualizaciones dinámicas
  - LaTeX para notación matemática avanzada
- **Múltiples formatos de salida**: PDF, HTML, Word, NOPS, Moodle XML
- **Sistema de metadatos ICFES**: Clasificación completa por competencias, nivel de dificultad, contenido, contexto y eje axial
- **Laboratorio activo**: Carpeta Lab/ con ejercicios experimentales y en desarrollo para el semestre 2025

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
│   └── Pensamiento-Numerico/
├── 02-Funciones/
│   └── Pensamiento-Variacional-Espacial/
├── 06-Estadística-Y-Probabilidad/
│   └── Pensamiento-Aleatorio/
├── Auxiliares/     # Scripts y documentación auxiliar
│   ├── guia_implementacion_icfes.md
│   ├── matriz_alineacion_icfes.md
│   ├── plantilla_metadatos_icfes.md
│   ├── actualizar_metadatos_icfes.R
│   ├── setup_project.R
│   ├── Instalaciones/
│   └── ExamenFinPeriodo-1/
├── Lab/            # Laboratorio de desarrollo activo (2025)
│   ├── 01-S2-2025-SEDQ/    # Gráficos circulares y proporciones
│   ├── 05-S2-2025-SEDQ/    # Lógica de torneos deportivos
│   ├── 09-S2-2025-SEDQ/    # Descuentos y porcentajes
│   ├── 12-S2-2025-SEDQ/    # Crecimiento exponencial
│   ├── 14-S2-2025-SEDQ/    # Porcentajes y grupos poblacionales
│   ├── 11/                 # Volumen de cilindros
│   ├── 17/                 # Interpretación gráfica de viajes
│   ├── 19/                 # Ordenamiento de sabores
│   ├── 36/                 # Cilindros huecos
│   ├── 37/                 # Semicírculos y radios
│   ├── 39/                 # Geometría de limpiaparabrisas
│   ├── 43/                 # Probabilidad con extracción de bolas
│   └── Real-*/             # Ejercicios basados en pruebas reales ICFES
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
  - **Porcentajes y Descuentos** (Lab/09-S2-2025-SEDQ/)
    - Cálculo de descuentos comerciales
    - Aplicación de porcentajes en contextos reales
    - Problemas de precios y ofertas

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
        - **Interpretación Gráfica de Viajes** (Lab/17/)
          - Análisis de gráficas de distancia vs tiempo
          - Interpretación de movimiento y velocidad
        - **Crecimiento Exponencial** (Lab/12-S2-2025-SEDQ/)
          - Modelado de poblaciones con funciones exponenciales
          - Cálculo de valores en tiempos específicos
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
  - **Geometría Aplicada** (Laboratorio)
    - **Volumen de Cilindros** (Lab/11/, Lab/36/)
      - Cálculo de volúmenes de cilindros simples y huecos
      - Aplicaciones en problemas de capacidad
    - **Semicírculos y Radios** (Lab/37/)
      - Problemas geométricos con semicírculos
      - Cálculos de áreas y perímetros
    - **Geometría de Limpiaparabrisas** (Lab/39/)
      - Aplicación de conceptos geométricos en contextos reales
      - Análisis de movimientos circulares

### 6. Estadística y Probabilidad
- **Pensamiento Aleatorio**
  - Variables Cualitativas y Distribución de Frecuencias
    - **Contenido Actual:**
      - Gráficos Estadísticos de Adopción de Mascotas
      - Accidentalidad Vial por Género (con pruebas unitarias)
      - **Gráficos Circulares y Proporciones** (Lab/01-S2-2025-SEDQ/)
        - Interpretación de diagramas de pastel
        - Cálculo de proporciones con regla de tres
        - Aleatorización avanzada con 25+ variantes de pregunta
        - Visualización con Python/matplotlib
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
    - **Contenido Actual:**
      - **Porcentajes y Grupos Poblacionales** (Lab/14-S2-2025-SEDQ/)
        - Análisis de distribuciones poblacionales
        - Cálculos de porcentajes y proporciones
  - Probabilidad: Principios Aditivo y Multiplicativo de Conteo
    - **Contenido Actual:**
      - Diagramas de Venn
      - Caso Práctico: Géneros Musicales
      - **Probabilidad con Extracción de Bolas** (Lab/43/)
        - Cálculos de probabilidad con combinaciones
        - Problemas de extracción sin reemplazo
      - **Lógica de Torneos Deportivos** (Lab/05-S2-2025-SEDQ/)
        - Sistemas de puntuación en competencias
        - Análisis combinatorio de resultados
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

# Ejemplo específico para ejercicios del laboratorio
source("Lab/01-S2-2025-SEDQ/01-ejecutar_pruebas_grafico_circular.R")
source("Lab/43/ejecutar_pruebas.R")
```

### Trabajando con el Laboratorio de Ejercicios

La carpeta `Lab/` contiene ejercicios experimentales y en desarrollo activo:

```r
# Generar ejercicios de la serie S2-2025-SEDQ
library(exams)

# Gráficos circulares
exams2moodle("Lab/01-S2-2025-SEDQ/01-S2-2025-SEDQ-grafico_circular_bienes_v0.Rmd", n = 10)

# Torneos deportivos
exams2pdf("Lab/05-S2-2025-SEDQ/05-S2-205-SEDQ-clasificacion_torneo_futbol_v2.Rmd", n = 5)

# Crecimiento exponencial
exams2html("Lab/12-S2-2025-SEDQ/crecimiento_exponencial_valor_inicial_v2.Rmd", n = 8)
```

## Novedades

### Enero 2025
- **Expansión masiva del laboratorio de ejercicios**
  - **Serie S2-2025-SEDQ**: Nueva colección de ejercicios para el segundo semestre 2025
    - Gráficos circulares con aleatorización avanzada (25+ variantes de pregunta)
    - Lógica de torneos deportivos con sistemas de puntuación complejos
    - Descuentos y porcentajes en contextos comerciales
    - Crecimiento exponencial con modelado poblacional
    - Análisis de grupos poblacionales y distribuciones
  - **Ejercicios de geometría aplicada**
    - Volumen de cilindros simples y huecos con visualización 3D
    - Problemas de semicírculos y radios
    - Geometría de limpiaparabrisas (aplicación real)
  - **Ejercicios basados en pruebas reales ICFES** (Real-*/): Adaptación de problemas oficiales
  - **Interpretación gráfica avanzada**: Análisis de viajes y movimiento
  - **Probabilidad aplicada**: Extracción de bolas y combinaciones

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

## Ejercicios Destacados del Laboratorio

### Serie S2-2025-SEDQ (Segundo Semestre 2025)

1. **Gráficos Circulares y Proporciones** (Lab/01-S2-2025-SEDQ/)
   - Interpretación de diagramas de pastel con aleatorización avanzada
   - 25+ variantes de pregunta con diferentes tipos de bienes y contextos
   - Visualización con Python/matplotlib y pruebas unitarias integradas

2. **Lógica de Torneos Deportivos** (Lab/05-S2-2025-SEDQ/)
   - Sistemas de puntuación complejos en competencias deportivas
   - Análisis combinatorio de resultados y clasificaciones
   - Tablas dinámicas con TikZ

3. **Descuentos y Porcentajes** (Lab/09-S2-2025-SEDQ/)
   - Problemas comerciales con descuentos múltiples
   - Cálculos de precios finales y ofertas
   - Contextos de compras y ventas

4. **Crecimiento Exponencial** (Lab/12-S2-2025-SEDQ/)
   - Modelado de poblaciones con funciones exponenciales
   - Cálculo de valores en tiempos específicos
   - Aplicaciones en biología y ecología

### Ejercicios de Geometría Aplicada

5. **Volumen de Cilindros** (Lab/11/, Lab/36/)
   - Cálculos de volúmenes simples y cilindros huecos
   - Visualización 3D con Python
   - Aplicaciones en problemas de capacidad

6. **Geometría de Limpiaparabrisas** (Lab/39/)
   - Aplicación real de conceptos geométricos
   - Análisis de movimientos circulares
   - Problemas de áreas y sectores

### Ejercicios Basados en Pruebas Reales

7. **Real-07-S01-01-2024** y **Real-31-S01-01-2024**
   - Adaptaciones de problemas oficiales del ICFES
   - Mantenimiento de la estructura y dificultad original
   - Aleatorización respetando el formato oficial

## Contribuciones

Este repositorio está abierto a contribuciones. Si deseas colaborar:

1. Revisa la estructura de carpetas y las plantillas existentes
2. Sigue las convenciones de nomenclatura y organización
3. Asegúrate de incluir pruebas unitarias para validar la coherencia matemática
4. Documenta adecuadamente el código y los ejercicios
5. Incluye los metadatos ICFES en tus ejercicios
6. Considera desarrollar en la carpeta `Lab/` antes de mover a las carpetas principales
7. Implementa aleatorización robusta (mínimo 300 variantes por ejercicio)

## Recursos Adicionales

- [Documentación oficial de R-exams](http://www.r-exams.org/)
- [Tutorial de TikZ](https://www.overleaf.com/learn/latex/TikZ_package)
- [Documentación de matplotlib](https://matplotlib.org/stable/index.html)
- [Guía de implementación ICFES](./Auxiliares/guia_implementacion_icfes.md)
- [Matriz de alineación ICFES](./Auxiliares/matriz_alineacion_icfes.md)
- [Estándares Básicos de Competencias en Matemáticas](https://www.mineducacion.gov.co/1621/articles-116042_archivo_pdf2.pdf)

## Estadísticas del Proyecto

- **Ejercicios implementados**: 50+ ejercicios activos
- **Variantes por ejercicio**: Mínimo 300 versiones diferentes
- **Formatos soportados**: PDF, HTML, Word, NOPS, Moodle XML
- **Áreas temáticas cubiertas**: 6 áreas principales de matemáticas ICFES
- **Tecnologías integradas**: R, Python, LaTeX, TikZ
- **Pruebas unitarias**: Sistema de validación automática implementado
- **Metadatos ICFES**: Sistema completo de clasificación pedagógica

## Roadmap 2025

### Próximos Desarrollos
- [ ] Expansión de ejercicios de trigonometría
- [ ] Implementación de ejercicios interactivos para HTML
- [ ] Sistema de análisis de respuestas y patrones de error
- [ ] Integración con plataformas LMS adicionales
- [ ] Desarrollo de ejercicios adaptativos por nivel
- [ ] Creación de bancos de preguntas por competencias específicas

### Mejoras Técnicas Planificadas
- [ ] Optimización de tiempos de generación
- [ ] Mejora en la accesibilidad de ejercicios
- [ ] Sistema de versionado automático
- [ ] Documentación interactiva
- [ ] API para integración externa

## Contacto

Para consultas o sugerencias relacionadas con este repositorio, puedes contactar al mantenedor principal a través de GitHub.

[GitHub: proyecto-r-exams-icfes-matematicas-optimizado](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)

---

**Última actualización**: Enero 2025
**Versión del README**: 2.0
**Estado del proyecto**: En desarrollo activo
