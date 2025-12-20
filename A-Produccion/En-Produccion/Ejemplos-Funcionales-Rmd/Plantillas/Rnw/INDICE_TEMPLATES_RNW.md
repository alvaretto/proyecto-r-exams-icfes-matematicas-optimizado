# 📚 ÍNDICE COMPLETO DE TEMPLATES .RNW OFICIALES

## ✅ DESCARGA EXITOSA: 30 TEMPLATES ORGANIZADOS

### 🔘 SCHOICE (Single Choice) - 5 archivos
**Ejercicios de selección única con una sola respuesta correcta**

| Archivo | Descripción | Tema | Nivel |
|---------|-------------|------|-------|
| `deriv2.Rnw` | Derivadas con producto y exponencial | Cálculo | Intermedio |
| `dist3.Rnw` | Distribuciones de probabilidad | Estadística | Básico |
| `hessian.Rnw` | Matrices hessianas y optimización | Cálculo multivariable | Avanzado |
| `swisscapital.Rnw` | Capitales de Suiza | Geografía | Básico |
| `tstat2.Rnw` | Estadística t con gráficos | Estadística | Intermedio |

### ☑️ MCHOICE (Multiple Choice) - 8 archivos
**Ejercicios de selección múltiple con varias respuestas correctas**

| Archivo | Descripción | Tema | Nivel |
|---------|-------------|------|-------|
| `anova.Rnw` | Análisis de varianza | Estadística | Avanzado |
| `boxplots.Rnw` | Interpretación de diagramas de caja | Estadística descriptiva | Intermedio |
| `gaussmarkov.Rnw` | Teorema de Gauss-Markov | Econometría | Avanzado |
| `relfreq.Rnw` | Frecuencias relativas | Estadística básica | Básico |
| `Rlogo.Rnw` | Reconocimiento del logo de R | Programación | Básico |
| `scatterplot.Rnw` | Diagramas de dispersión | Estadística descriptiva | Intermedio |
| `switzerland.Rnw` | Geografía de Suiza múltiple | Geografía | Básico |
| `ttest.Rnw` | Pruebas t de hipótesis | Estadística inferencial | Intermedio |

### 🧩 CLOZE (Fill-in-the-blank) - 5 archivos
**Ejercicios con múltiples partes y tipos de respuesta mixtos**

| Archivo | Descripción | Tema | Nivel |
|---------|-------------|------|-------|
| `boxhist2.Rnw` | Histogramas y boxplots combinados | Estadística descriptiva | Intermedio |
| `boxhist.Rnw` | Histogramas y boxplots básicos | Estadística descriptiva | Básico |
| `dist2.Rnw` | Distribuciones con múltiples preguntas | Probabilidad | Intermedio |
| `fourfold2.Rnw` | Tablas de contingencia avanzadas | Estadística | Avanzado |
| `fourfold.Rnw` | Tablas de contingencia básicas | Estadística | Intermedio |

### 🔢 NUM (Numeric) - 9 archivos
**Ejercicios con respuestas numéricas exactas**

| Archivo | Descripción | Tema | Nivel |
|---------|-------------|------|-------|
| `cholesky.Rnw` | Descomposición de Cholesky | Álgebra lineal | Avanzado |
| `confint2.Rnw` | Intervalos de confianza | Estadística inferencial | Intermedio |
| `currency8.Rnw` | Conversión de monedas | Matemáticas aplicadas | Básico |
| `deriv.Rnw` | Derivadas básicas | Cálculo | Básico |
| `dist.Rnw` | Distribuciones de probabilidad | Probabilidad | Básico |
| `lagrange.Rnw` | Multiplicadores de Lagrange | Optimización | Avanzado |
| `lm.Rnw` | Regresión lineal | Estadística | Intermedio |
| `regression.Rnw` | Análisis de regresión | Estadística | Intermedio |
| `tstat.Rnw` | Estadística t básica | Estadística inferencial | Básico |

### 📝 STRING (Text) - 2 archivos
**Ejercicios con respuestas de texto**

| Archivo | Descripción | Tema | Nivel |
|---------|-------------|------|-------|
| `countrycodes.Rnw` | Códigos de países | Geografía | Básico |
| `function.Rnw` | Funciones matemáticas | Matemáticas | Básico |

### 📄 ESSAY (Essay) - 1 archivo
**Ejercicios de respuesta libre**

| Archivo | Descripción | Tema | Nivel |
|---------|-------------|------|-------|
| `essayreg.Rnw` | Ensayo sobre regresión | Estadística | Avanzado |

## 🎯 TEMPLATES RECOMENDADOS PARA ICFES MATEMÁTICAS

### Para Álgebra:
- `deriv.Rnw` - Estructura básica para derivadas
- `function.Rnw` - Funciones matemáticas

### Para Geometría:
- `scatterplot.Rnw` - Gráficos de coordenadas
- `boxplots.Rnw` - Interpretación gráfica

### Para Estadística y Probabilidad:
- `dist.Rnw` - Distribuciones básicas
- `relfreq.Rnw` - Frecuencias
- `confint2.Rnw` - Intervalos

### Para Cálculo:
- `deriv2.Rnw` - Derivadas avanzadas
- `lagrange.Rnw` - Optimización

## 🔧 ESTRUCTURA TÍPICA .RNW

Todos los templates siguen esta estructura estándar:

```latex
<<setup, echo=FALSE, results=hide>>=
# Configuración y librerías
@

<<data_generation, echo=FALSE, results=hide>>=
# Generación de datos aleatorios
@

\begin{question}
[Pregunta del ejercicio]
<<echo=FALSE, results=tex>>=
answerlist(questions)
@
\end{question}

\begin{solution}
[Solución detallada]
<<echo=FALSE, results=tex>>=
answerlist(solutions)
@
\end{solution}

%% META-INFORMATION
%% \extype{schoice|mchoice|cloze|num|string|essay}
%% \exsolution{[patrón]}
%% \exname{[nombre]}
```

## 📊 ESTADÍSTICAS FINALES
- ✅ **Total descargado**: 30 archivos .Rnw
- 📁 **Organizados en**: 6 categorías
- 🌐 **Fuente**: https://www.r-exams.org/templates/
- 📅 **Fecha descarga**: 2025-01-10
- 🎯 **Listos para**: Adaptación ICFES
