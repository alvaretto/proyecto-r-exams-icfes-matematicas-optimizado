# 🎯 Guía TikZ para Ejercicios ICFES Matemáticas

## 📋 Análisis de Recursos TikZ Existentes en el Proyecto

### 🔍 **Inventario Actual de Archivos TikZ**

#### **📐 Geometría 3D - Cuerpos Sólidos**
**Ubicación**: `Lab/36/Auxiliares/TikZ/`

1. **Cilindro Hueco** (`Cilindro-Hueco.tikz`)
   - **Competencia ICFES**: Pensamiento Espacial-Métrico
   - **Nivel**: 3-4 (Cálculo de volúmenes complejos)
   - **Características**:
     - Perspectiva 3D avanzada con elipses
     - Variables parametrizables: altura, radio interno, grosor
     - Etiquetas dimensionales automáticas
     - Colores personalizados y sombreado
     - Líneas ocultas con estilo dashed
   - **Bibliotecas TikZ**: `calc`, `decorations.markings`, `shadows.blur`, `fadings`
   - **Compatibilidad R-exams**: ⚠️ Requiere adaptación (ver `bob.md`)

2. **Cono Recto Base Circular** (`Cono-Recto_Base_Circular.tikz`)
   - **Competencia ICFES**: Pensamiento Espacial-Métrico
   - **Nivel**: 2-3 (Geometría básica 3D)
   - **Características**:
     - Perspectiva elíptica para base circular
     - Generatrices visibles/ocultas
     - Relleno de superficie visible
     - Etiquetas de puntos clave
   - **Bibliotecas TikZ**: Básicas (sin bibliotecas adicionales)
   - **Compatibilidad R-exams**: ✅ Alta compatibilidad

#### **📊 Diagramas Conceptuales**
**Ubicación**: `Lab/02-Geometria/`

3. **Diagrama de Venn** (`diagrama_venn_ktikz.tikz`)
   - **Competencia ICFES**: Pensamiento Aleatorio
   - **Nivel**: 1-2 (Conjuntos y relaciones)
   - **Características**:
     - Tres círculos con intersecciones
     - Colores semitransparentes
     - Etiquetas posicionadas automáticamente
   - **Bibliotecas TikZ**: Básicas
   - **Compatibilidad R-exams**: ✅ Excelente

#### **📈 Geometría 3D Avanzada**
**Ubicación**: `General/Plantillas/Ejemplos-Fausto/Graf/`

4. **Prisma Triangular** (`Figura_1.tikz`)
   - **Competencia ICFES**: Pensamiento Espacial-Métrico
   - **Nivel**: 3-4 (Geometría 3D con medidas)
   - **Características**:
     - Sistema de coordenadas 3D
     - Caras coloreadas diferenciadas
     - Líneas de medición con cotas
     - Líneas ocultas con dashed
   - **Bibliotecas TikZ**: `3d`, `calc`, `babel`
   - **Compatibilidad R-exams**: ⚠️ Requiere validación

## 🎯 **Patrones Exitosos Identificados en Archivos .Rmd Validados**

### ✅ **PATRONES ALTAMENTE COMPATIBLES** (Basados en archivos exitosos)

#### **1. Tablas con TikZ** (Patrón más exitoso)
**Fuente**: `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`
```latex
\begin{tikzpicture}
\node[inner sep=0pt] {
  \begin{tabular}{|c|c|}
    \hline
    \textbf{Columna 1} & \textbf{Columna 2} \\
    \hline
    `r valor1` & `r valor2` \\
    \hline
  \end{tabular}
};
\end{tikzpicture}
```
- **Ventajas**: Excelente compatibilidad multi-formato
- **Uso**: Presentación de datos, tablas de frecuencia
- **Variables R**: Integración directa con `r variable`

#### **2. Diagramas de Venn Simples**
**Fuente**: `DVenn_All_GenMus_01.Rmd`
```latex
\begin{tikzpicture}[scale=0.7]
  \begin{scope}[opacity=0.5]
    \fill[red]   ( 90:1.5) circle (2);
    \fill[green] (210:1.5) circle (2);
    \fill[blue]  (330:1.5) circle (2);
  \end{scope}
  \node at ( 90:3.3) {%s};
  \node at (210:3.3) {%s};
  \node at (330:3.3) {%s};
\end{tikzpicture}
```
- **Ventajas**: Colores estándar, coordenadas polares simples
- **Uso**: Probabilidad, conjuntos, lógica
- **Parametrización**: sprintf() para valores dinámicos

#### **3. Compuertas Lógicas con include_tikz()**
**Fuente**: `logic_TikZ.Rmd`
```r
tikz_gate <- function(op) {
  c("\\begin{tikzpicture}[thick]",
    paste0("  \\node[left,draw, logic gate inputs=nn, ", op," gate US,fill=none,,scale=2.5] (G1) at (0,0) {};"),
    "  \\draw (G1.output) --++ (0.5,0) node[right] (y) {$y$};",
    "\\end{tikzpicture}")
}

include_tikz(tikz_gate(ops1), name = "table", markup = "markdown",
  format = typ, packages = "booktabs", width = "2cm")
```
- **Ventajas**: Función include_tikz() para compatibilidad
- **Uso**: Diagramas técnicos, lógica, circuitos
- **Bibliotecas**: `arrows`, `shapes.gates.logic.US`, `calc`

### ✅ **CONFIGURACIONES TÉCNICAS VALIDADAS**

#### **Setup Chunk Estándar Exitoso**
```r
# Configuración para todos los formatos de salida
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")

# Configurar el motor LaTeX globalmente
options(tikzLatex = "pdflatex")
options(tikzXelatex = FALSE)
options(tikzLatexPackages = c(
  "\\usepackage{tikz}",
  "\\usepackage{colortbl}",
  "\\usepackage{graphicx}",
  "\\usepackage{float}",
  "\\usepackage{booktabs}",
  "\\usepackage{array}"
))

library(exams)
library(reticulate)
library(digest)
library(testthat)
library(knitr)

typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.showtext = FALSE,
  fig.cap = "",
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150,
  fig.pos = "H"
)
```

#### **Integración con Variables R**
```r
# Patrón exitoso para integrar variables R en TikZ
tabla_tikz <- sprintf('
\\begin{tikzpicture}
\\node{
\\begin{tabular}{|l|c|}
\\hline
\\textbf{%s} & \\textbf{%s} \\\\ \\hline
%s & %s \\\\ \\hline
\\end{tabular}
};
\\end{tikzpicture}
', encabezado1, encabezado2, valor1, valor2)
```

### ⚠️ **PATRONES QUE REQUIEREN ADAPTACIÓN**
1. **Colores personalizados** con `\definecolor` → Usar colores estándar
2. **Bibliotecas avanzadas** (`shadows`, `fadings`) → Evitar o simplificar
3. **Cálculos complejos** con `\pgfmathsetmacro` → Usar variables R
4. **Transparencias complejas** → Usar opacity simple
5. **Coordenadas 3D** complejas → Simplificar o usar 2D

### ❌ **PATRONES PROBLEMÁTICOS** (Evitar)
1. **Paquetes externos** no estándar
2. **Fuentes personalizadas**
3. **Efectos de sombreado** avanzados
4. **Animaciones** o elementos dinámicos
5. **Bibliotecas no básicas** sin validación previa

## 📋 **Clasificación por Competencias ICFES**

### 🔢 **Pensamiento Numérico-Variacional**
- **Necesidades identificadas**:
  - Gráficos de funciones lineales/cuadráticas
  - Representaciones de proporciones
  - Diagramas de variación
- **Recursos actuales**: Limitados
- **Prioridad de búsqueda**: 🔴 Alta

### 📐 **Pensamiento Espacial-Métrico**
- **Recursos actuales**: ✅ Buenos (cilindros, conos, prismas)
- **Fortalezas**: Geometría 3D, medidas, perspectiva
- **Gaps**: Transformaciones, construcciones geométricas
- **Prioridad de búsqueda**: 🟡 Media

### 📊 **Pensamiento Aleatorio**
- **Recursos actuales**: ⚠️ Básicos (solo Venn)
- **Necesidades**:
  - Gráficos estadísticos (barras, circulares, histogramas)
  - Diagramas de caja
  - Representaciones de probabilidad
- **Prioridad de búsqueda**: 🔴 Alta

## 🌐 **Plan de Búsqueda Recursiva Específico**

### 🎯 **Fase 1: Búsquedas Prioritarias**

#### **📊 Estadística y Probabilidad**
- [ ] **TeXample.net**: Buscar "statistical plots", "bar charts", "pie charts"
- [ ] **CTAN**: Paquetes `pgfplots`, `tikz-statistical`
- [ ] **GitHub**: Repositorios "tikz statistics", "tikz probability"
- [ ] **Overleaf**: Templates "statistics tikz", "data visualization"

#### **📈 Funciones y Álgebra**
- [ ] **TeXample.net**: "function plots", "coordinate systems"
- [ ] **PGFPlots Gallery**: Ejemplos de funciones matemáticas
- [ ] **GitHub**: "tikz functions", "mathematical plots"
- [ ] **Academic Papers**: Visualización de funciones educativas

### 🔧 **Fase 2: Adaptación para R-exams**

#### **Criterios de Adaptación**
1. **Simplificar colores**: Usar paleta estándar
2. **Eliminar bibliotecas problemáticas**: Mantener solo básicas
3. **Parametrizar valores**: Usar variables R en lugar de `\pgfmathsetmacro`
4. **Validar multi-formato**: Probar en HTML, PDF, Moodle

#### **Template de Adaptación**
```latex
% ANTES (problemático para R-exams)
\definecolor{customColor}{RGB}{60, 120, 180}
\pgfmathsetmacro{\radius}{2.5}

% DESPUÉS (compatible con R-exams)
% Usar colores estándar y variables R
\begin{tikzpicture}
  \draw[blue!70] (0,0) circle (`r radio_valor`);
\end{tikzpicture}
```

## 📁 **Organización de Recursos Encontrados**

### 📊 **Estadística** (`estadistica/`)
- `graficos-circulares/`: Pie charts, sectores
- `histogramas/`: Distribuciones de frecuencia
- `diagramas-caja/`: Box plots, quartiles
- `distribuciones/`: Curvas normales, binomiales

### 📐 **Geometría** (`geometria/`)
- `figuras-planas/`: Triángulos, polígonos, círculos
- `cuerpos-3d/`: Prismas, pirámides, cuerpos redondos
- `transformaciones/`: Rotaciones, traslaciones, reflexiones
- `construcciones/`: Construcciones con regla y compás

### 📈 **Álgebra-Cálculo** (`algebra-calculo/`)
- `funciones/`: Lineales, cuadráticas, exponenciales
- `ecuaciones/`: Sistemas, representaciones gráficas
- `graficas/`: Plano cartesiano, coordenadas
- `limites-derivadas/`: Representaciones de cálculo

## 🔄 **Proceso de Validación**

### ✅ **Checklist de Compatibilidad R-exams**
- [ ] Compila en `exams2pdf`
- [ ] Genera imagen en `exams2html`
- [ ] Funciona en `exams2moodle`
- [ ] Compatible con `exams2pandoc`
- [ ] Escalable para múltiples variantes

### 🧪 **Script de Prueba**
```r
# Probar compatibilidad TikZ
test_tikz_compatibility <- function(tikz_code) {
  # Crear archivo temporal .Rmd
  # Probar generación en múltiples formatos
  # Reportar errores y compatibilidad
}
```

## 📚 **Referencias y Fuentes**

### 🌐 **Recursos Web Principales**
- **TeXample.net**: http://www.texample.net/tikz/
- **PGF/TikZ Manual**: https://ctan.org/pkg/pgf
- **TikZ Gallery**: https://sites.google.com/site/kochiuyu/Tikz
- **Overleaf TikZ**: https://www.overleaf.com/learn/latex/TikZ_package

### 📖 **Documentación Técnica**
- **R-exams TikZ**: Compatibilidad y mejores prácticas
- **ICFES Matemáticas**: Competencias y estándares
- **LaTeX Graphics**: Integración con R Markdown

---

**Última actualización**: `r Sys.Date()`  
**Versión**: 1.0  
**Mantenedor**: Proyecto ICFES R-exams