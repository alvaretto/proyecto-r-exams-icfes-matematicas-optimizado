# Repositorio Matemáticas ICFES - R-Exams

## Descripción General

Este repositorio contiene una colección de ejercicios de matemáticas para preparación de pruebas ICFES, desarrollados con el paquete R-exams. El proyecto está diseñado para generar preguntas dinámicas con alto grado de aleatorización, permitiendo crear múltiples versiones de cada ejercicio con diferentes variables, textos, nombres y situaciones.

## Estado Actual

- El repositorio cuenta con una estructura organizada por áreas temáticas de matemáticas
- Se han implementado ejercicios en varias áreas, con énfasis en estadística y probabilidad
- Se han añadido pruebas unitarias para garantizar la coherencia matemática y diversidad de versiones
- Se utilizan tecnologías como TikZ para la generación de gráficos y tablas de alta calidad
- El proyecto soporta múltiples formatos de salida (PDF, HTML, NOPS)

## Requisitos del Sistema

- **R**: Versión 4.0 o superior
- **RStudio**: Recomendado para una mejor experiencia de desarrollo
- **LaTeX**: Necesario para la generación de documentos PDF
- **TikZ**: Requerido para la generación de gráficos vectoriales
- **Paquetes R**: exams, knitr, ggplot2, entre otros (ver script install_packages.R)

## Estructura del Repositorio

El repositorio está organizado siguiendo la estructura temática del currículo de matemáticas para ICFES:

```
├── 01-Numeros-Reales/
├── 02-Funciones/
├── 03-Razones-Trigonometricas/
├── 04-Funciones_Identidades-Trigonometricas/
├── 05-Geometria-Analitica/
├── 06-Estadística-Y-Probabilidad/
└── General/
    └── Plantillas/
```

Cada ejercicio sigue una estructura común:

```
├── ejercicios/     # Archivos .Rmd con el código fuente
├── docus/          # Documentación adicional
├── salida/         # Archivos generados
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

## Características Principales

- **Alta aleatorización**: Cada ejercicio puede generar cientos de versiones diferentes
- **Pruebas unitarias**: Validación automática de la coherencia matemática
- **Gráficos dinámicos**: Generación de visualizaciones personalizadas con TikZ y ggplot2
- **Tablas dinámicas**: Creación de tablas con datos aleatorios usando TikZ y kable
- **Múltiples formatos**: Soporte para PDF, HTML, NOPS y otros formatos de salida
- **Plantillas reutilizables**: Estructura modular para facilitar la creación de nuevos ejercicios
- **Compatibilidad con LaTeX**: Soporte completo para notación matemática avanzada

## Guía de Uso

### Generación de Ejercicios

Para generar ejercicios en formato PDF:

```r
library(exams)
exams2pdf("ruta/al/ejercicio.Rmd", n = 5)  # Genera 5 versiones diferentes
```

Para generar ejercicios en formato HTML:

```r
library(exams)
exams2html("ruta/al/ejercicio.Rmd", n = 5)  # Genera 5 versiones diferentes
```

### Ejecución de Pruebas Unitarias

Para ejecutar las pruebas unitarias de un ejercicio:

```r
source("ruta/al/script_de_pruebas.R")
```

## Novedades

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
- **Mejoras en la generación de gráficos con TikZ**
  - Eliminación de etiquetas automáticas como 'plot of chunk GraficoA'
  - Mayor personalización de colores y estilos
  - Soporte mejorado para tablas dinámicas

## Contribuciones

Este repositorio está abierto a contribuciones. Si deseas colaborar:

1. Revisa la estructura de carpetas y las plantillas existentes
2. Sigue las convenciones de nomenclatura y organización
3. Asegúrate de incluir pruebas unitarias para validar la coherencia matemática
4. Documenta adecuadamente el código y los ejercicios

## Recursos Adicionales

- [Documentación oficial de R-exams](http://www.r-exams.org/)
- [Tutorial de TikZ](https://www.overleaf.com/learn/latex/TikZ_package)
- [Guía de implementación ICFES](./guia_implementacion_icfes.html)
- [Matriz de alineación ICFES](./matriz_alineacion_icfes.html)

## Contacto

Para consultas o sugerencias relacionadas con este repositorio, puedes contactar al mantenedor principal a través de GitHub.

[GitHub:](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
