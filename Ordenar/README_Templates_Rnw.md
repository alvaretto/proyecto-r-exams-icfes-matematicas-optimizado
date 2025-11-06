# 📚 TEMPLATES .RNW OFICIALES DE R-EXAMS

## 🎯 Descripción
Esta carpeta contiene **30 templates oficiales .Rnw** descargados desde https://www.r-exams.org/templates/ y organizados por tipo de ejercicio para facilitar su uso en el proyecto ICFES.

## 📂 Estructura de Directorios

### 🔘 SCHOICE (Single Choice) - 5 archivos
Ejercicios de selección única con una sola respuesta correcta:
- `deriv2.Rnw` - Derivadas con gráficos
- `dist3.Rnw` - Distribuciones de probabilidad
- `hessian.Rnw` - Matrices hessianas
- `swisscapital.Rnw` - Geografía (capitales)
- `tstat2.Rnw` - Estadística t con gráficos

### ☑️ MCHOICE (Multiple Choice) - 8 archivos
Ejercicios de selección múltiple con varias respuestas correctas:
- `anova.Rnw` - Análisis de varianza
- `boxplots.Rnw` - Diagramas de caja
- `gaussmarkov.Rnw` - Teorema de Gauss-Markov
- `relfreq.Rnw` - Frecuencias relativas
- `Rlogo.Rnw` - Reconocimiento de imágenes
- `scatterplot.Rnw` - Diagramas de dispersión
- `switzerland.Rnw` - Geografía múltiple
- `ttest.Rnw` - Pruebas t

### 🧩 CLOZE (Fill-in-the-blank) - 5 archivos
Ejercicios con múltiples partes y tipos de respuesta:
- `boxhist2.Rnw` - Histogramas y boxplots combinados
- `boxhist.Rnw` - Histogramas y boxplots básicos
- `dist2.Rnw` - Distribuciones con múltiples preguntas
- `fourfold2.Rnw` - Tablas de contingencia avanzadas
- `fourfold.Rnw` - Tablas de contingencia básicas

### 🔢 NUM (Numeric) - 9 archivos
Ejercicios con respuestas numéricas:
- `cholesky.Rnw` - Descomposición de Cholesky
- `confint2.Rnw` - Intervalos de confianza
- `currency8.Rnw` - Conversión de monedas
- `deriv.Rnw` - Derivadas básicas
- `dist.Rnw` - Distribuciones básicas
- `lagrange.Rnw` - Multiplicadores de Lagrange
- `lm.Rnw` - Regresión lineal
- `regression.Rnw` - Análisis de regresión
- `tstat.Rnw` - Estadística t básica

### 📝 STRING (Text) - 2 archivos
Ejercicios con respuestas de texto:
- `countrycodes.Rnw` - Códigos de países
- `function.Rnw` - Funciones matemáticas

### 📄 ESSAY (Essay) - 1 archivo
Ejercicios de respuesta libre:
- `essayreg.Rnw` - Ensayo sobre regresión

## 🔧 Uso Recomendado

### Para Ejercicios ICFES:
1. **Matemáticas básicas**: Usar templates `num/` como base
2. **Geometría/Gráficos**: Adaptar `scatterplot.Rnw`, `boxplots.Rnw`
3. **Estadística**: Usar `dist.Rnw`, `tstat.Rnw`, `confint2.Rnw`
4. **Selección múltiple**: Basar en templates `mchoice/`
5. **Problemas complejos**: Usar estructura `cloze/`

### Estructura Típica .Rnw:
```latex
\documentclass[11pt,a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage[spanish]{babel}
\usepackage{amsmath,amsfonts,amssymb}
\usepackage{tikz,pgfplots}

<<setup, include=FALSE>>=
library(exams)
@

<<data_generation, echo=FALSE, results=hide>>=
# Generación de datos aleatorios
@

\begin{question}
[Pregunta del ejercicio]
\end{question}

\begin{solution}
[Solución detallada]
\end{solution}

%% META-INFORMATION
%% \extype{schoice|mchoice|cloze|num|string|essay}
%% \exsolution{[patrón de respuesta]}
%% \exname{[nombre del ejercicio]}
```

## 📊 Estadísticas de Descarga
- ✅ **Descargados exitosamente**: 30 archivos
- ❌ **Fallidos**: 12 archivos (URLs no disponibles)
- 📁 **Organizados en**: 6 categorías por tipo de ejercicio
- 🌐 **Fuente**: https://www.r-exams.org/templates/

## 🎯 Integración con Proyecto ICFES
Estos templates están listos para ser adaptados siguiendo:
- Metodología TikZ avanzada para gráficos
- Sistema condicional automático
- Protocolo anti-errores de implementación
- Estándares de calidad ICFES

## 📝 Notas
- Todos los archivos están en formato LaTeX/Sweave (.Rnw)
- Compatibles con exams2html(), exams2pdf(), exams2moodle()
- Incluyen meta-información completa para R-exams
- Listos para personalización con datos ICFES
