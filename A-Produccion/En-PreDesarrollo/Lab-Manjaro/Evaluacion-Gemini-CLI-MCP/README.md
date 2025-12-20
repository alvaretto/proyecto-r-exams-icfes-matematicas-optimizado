# Evaluación Exhaustiva: Gemini-CLI con MCP vs Augment

## 🎯 Objetivos de la Evaluación

Realizar pruebas exhaustivas comparando Gemini-CLI integrado con Model Context Protocol (MCP) contra Augment para tareas específicas de análisis y generación de contenido matemático en archivos .Rmd.

## 📋 Archivos .Rmd Seleccionados para Pruebas

### 1. Ejercicio Schoice con Python/matplotlib
- **Archivo**: `gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_v1.Rmd`
- **Características**: 
  - Chunks de Python con matplotlib y numpy
  - Generación de gráficas estadísticas (circular, barras)
  - Integración reticulate
  - Formato schoice R-exams

### 2. Ejercicio Cloze con TikZ
- **Archivo**: `gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_cloze_v1.Rmd`
- **Características**:
  - Código TikZ para tablas
  - Formato cloze (5 numéricas + 1 schoice)
  - Tolerancias automáticas
  - Metadatos ICFES complejos

### 3. Ejercicio Mixto Avanzado
- **Archivo**: A seleccionar del directorio Lab-Manjaro con chunks complejos

## 🔧 Configuración Técnica

### Requisitos del Sistema
- Manjaro Plasma KDE
- R con exams, reticulate, knitr
- Python con matplotlib, numpy
- LaTeX con TikZ
- Gemini-CLI con MCP

### Estructura de Directorios
```
Lab-Manjaro/Evaluacion-Gemini-CLI-MCP/
├── README.md
├── configuracion/
│   ├── install-gemini-cli.sh
│   ├── config-mcp.json
│   └── test-setup.R
├── archivos-prueba/
│   ├── original/
│   ├── gemini-cli-output/
│   └── augment-output/
├── resultados/
│   ├── comparaciones/
│   ├── metricas/
│   └── documentacion/
└── scripts/
    ├── run-tests.sh
    ├── compare-outputs.R
    └── generate-report.Rmd
```

## 📊 Métricas de Evaluación

### 1. Análisis de Código R-exams
- Precisión en identificación de errores
- Calidad de sugerencias de mejora
- Comprensión de metadatos ICFES
- Tiempo de respuesta

### 2. Generación de Código TikZ
- Fidelidad visual (objetivo: 95%+)
- Sintaxis correcta
- Compatibilidad con R-exams
- Optimización del código

### 3. Optimización de Chunks Python
- Eficiencia del código generado
- Compatibilidad con reticulate
- Calidad de gráficas matplotlib
- Manejo de errores

### 4. Revisión LaTeX/R Markdown
- Detección de errores sintácticos
- Sugerencias de formato
- Compatibilidad multiplataforma
- Preservación de funcionalidad

## 🚀 Plan de Ejecución

1. **Configuración inicial** (30 min)
2. **Selección de archivos** (15 min)
3. **Instalación Gemini-CLI + MCP** (45 min)
4. **Pruebas sistemáticas** (2 horas)
5. **Análisis comparativo** (1 hora)
6. **Documentación** (30 min)

## 📈 Resultados Esperados

- Comparación detallada de capacidades
- Recomendaciones de uso por tipo de tarea
- Configuración optimizada para Manjaro Plasma
- Documentación para referencia futura
