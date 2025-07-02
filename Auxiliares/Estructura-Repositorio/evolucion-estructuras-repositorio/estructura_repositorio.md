---
output:
  pdf_document: default
  html_document: default
---
# Estructura del Repositorio RepositorioMatematicasICFES_R_Exams

## Estructura Principal

RepositorioMatematicasICFES_R_Exams/
├── 01-Numeros-Reales/
│   └── Pensamiento-Numerico/
├── 02-Funciones/
│   └── Pensamiento-Variacional-Espacial/
│       └── 11-Variacion-Lineal-Y-Exponencial_Razon-De-Cambio/
│           └── Variación-Lineal-Auto-Viajero-09/
├── 03-Razones-Trigonometricas/
├── 04-Funciones-Identidades-Trigonometricas/
├── 05-Geometria-Analitica/
├── 06-Estadística-Y-Probabilidad/
│   └── Pensamiento-Aleatorio/
│       ├── 01-Variables-Cualitativas_Distribucion-De-Frecuencias/
│       │   └── Graficos_Estadisticos_Adopcion_Mascotas/
│       ├── 04-Medidas-De-Tendencia-Central/
│       │   └── Media/
│       │       └── Promedios-Borrados/
│       └── 07-Probabilidad_Principios-Aditivo-Multiplicativo-Conteo/
│           └── Diagramas de Venn/
│               └── GénerosMusicales/
├── Auxiliares/
│   ├── Ejemplo/
│   │   └── plantilla_ejercicio_icfes.Rmd
│   ├── actualizar_metadatos_icfes.R
│   ├── generate_venn_exam.R
│   ├── guia_implementacion_icfes.md
│   ├── matriz_alineacion_icfes.md
│   ├── plantilla_metadatos_icfes.md
│   ├── quickstart.md
│   ├── run_example.R
│   ├── setup_project.R
│   └── workaround_no_pdftools.R
├── Lab/
│   └── 39/
│       ├── ICFES_Matematicas_Retroalimentacion.Rmd
│       └── all.png
├── docus/
│   ├── Copia de inventario.txt
│   ├── inventario.txt
│   └── rutas_carpetas_Rmd.md
├── README.md
└── RepositorioMatematicasICFES_R_Exams.Rproj

## Organización por Áreas Temáticas

### 1. Números Reales (01-Numeros-Reales)
- Pensamiento Numérico
  - Números Racionales
  - Números Irracionales
  - Propiedades de Expresiones Decimales
  - Conjunto de Reales y Desigualdades
  - Valor Absoluto

### 2. Funciones (02-Funciones)
- Pensamiento Variacional-Espacial
  - Variación Lineal y Exponencial
  - Razón de Cambio

### 3. Razones Trigonométricas (03-Razones-Trigonometricas)

### 4. Funciones e Identidades Trigonométricas (04-Funciones-Identidades-Trigonometricas)

### 5. Geometría Analítica (05-Geometria-Analitica)

### 6. Estadística y Probabilidad (06-Estadística-Y-Probabilidad)
- Pensamiento Aleatorio
  - Variables Cualitativas y Distribución de Frecuencias
  - Medidas de Tendencia Central
  - Probabilidad y Principios de Conteo
  - Diagramas de Venn

## Ubicación de Nuevos Ejercicios

Para ubicar un nuevo ejercicio:

1. Identificar el área temática principal (01 a 06)
2. Seleccionar el tipo de pensamiento correspondiente
3. Ubicar la subcategoría específica
4. Crear una nueva carpeta con nombre descriptivo si es necesario

Ejemplo: Un nuevo ejercicio sobre media aritmética debería ubicarse en:
`06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/04-Medidas-De-Tendencia-Central/Media/`
