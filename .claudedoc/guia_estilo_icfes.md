# 📚 GUÍA DE ESTILO ICFES R-EXAMS 2025
## Fuente de Verdad para Validación Automática

> **Referencia Principal**: `Auxiliares/rules_full/reglas-generales.md`  
> **Última Actualización**: 2025-01-XX  
> **Propósito**: Indexar reglas críticas para validación automática de archivos .Rmd

---

## 🎯 ESTRUCTURA OBLIGATORIA DEL ARCHIVO .RMD

### 1. ENCABEZADO YAML MÍNIMO REQUERIDO

```yaml
---
output:
  pdf_document:
    latex_engine: xelatex
    keep_tex: true
  html_document:
    df_print: paged
    mathjax: true
  word_document: default
header-includes:
- \usepackage[spanish]{babel}
- \usepackage{amsmath}
- \usepackage{fontspec}
- \usepackage{unicode-math}
- \usepackage{graphicx}
- \usepackage{adjustbox}
- \usepackage{tikz}
- \usepackage{pgfplots}
- \usetikzlibrary{3d,babel}
---
```

**Campos Críticos:**

- `latex_engine: xelatex` (OBLIGATORIO para TikZ)
- `header-includes` debe contener paquetes TikZ y babel
- `mathjax: true` para renderizado HTML correcto

### 2. METADATOS ICFES OBLIGATORIOS

```yaml
icfes:
  competencia: [interpretacion_representacion|formulacion_ejecucion|argumentacion]
  nivel_dificultad: [1|2|3|4]
  contenido:
    categoria: [algebra_calculo|geometria|estadistica]
    tipo: [generico|no_generico]
  contexto: [familiar|laboral|comunitario|matematico]
  eje_axial: [eje1|eje2|eje3|eje4]
  componente: [geometrico_metrico|numerico_variacional|aleatorio]
```

**Validaciones:**

- Todos los campos deben estar presentes
- Valores deben coincidir con las opciones permitidas (regex)
- `nivel_dificultad` debe ser entero entre 1 y 4

### 3. CHUNK DE CONFIGURACIÓN INICIAL (OBLIGATORIO)

**Nombre del chunk**: `{r inicio, include=FALSE}`

**Contenido Mínimo:**
```r
library(exams)
library(knitr)
# Si usa Python:
library(reticulate)
use_python("/usr/bin/python3", required = TRUE)

typ <- match_exams_device()
options(scipen = 999)
options(OutDec = ".")
options(digits = 10)
Sys.setlocale(category = "LC_NUMERIC", locale = "C")

knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150,
  echo = FALSE,
  results = "hide"
)

set.seed(sample(1:100000, 1))  # NUNCA set.seed() fijo
```

**Reglas Críticas:**

- `set.seed()` DEBE ser aleatorio (ej: `sample(1:100000, 1)`)
- `options(scipen = 999)` obligatorio (evitar notación científica)
- `options(OutDec = ".")` obligatorio (punto decimal)
- Si usa Python: `use_python()` debe estar configurado

### 4. SECCIÓN QUESTION (OBLIGATORIA)

```markdown
Question
========

[Enunciado del problema]

Answerlist
----------
- [Opción A]
- [Opción B]
- [Opción C]
- [Opción D]
```

**Validaciones:**

- Mínimo 4 opciones en Answerlist
- Todas las opciones deben ser diferentes (anti-duplicados)
- Formato markdown correcto (guiones para lista)

### 5. SECCIÓN SOLUTION (OBLIGATORIA)

```markdown
Solution
========

[Explicación detallada]

Answerlist
----------
- Verdadero/Falso con explicación
```

### 6. META-INFORMATION (OBLIGATORIA)

```markdown
Meta-information
================
exname: [Nombre descriptivo]
extype: schoice|cloze
exsolution: [Patrón binario, ej: 1000]
exshuffle: TRUE
exsection: [Sección temática]
```

**Para tipo CLOZE:**

- `exclozetype`: schoice|num|string (separados por |)
- `extol`: Tolerancias (schoice=0, numéricas≥1)
- Formato: `extol: 0|0|1|1|0|1|0`

---

## ⚠️ ERRORES COMUNES DE SINTAXIS LaTeX/TikZ

### Caracteres Especiales que Rompen Renderizado

**PROHIBIDOS sin escape:**

- `&` → `\&`
- `%` → `\%`
- `$` → `\$`
- `#` → `\#`
- `_` → `\_`
- `^` → `\^{}` o `\$^{}\$`
- `{` → `\{`
- `}` → `\}`

### Paquetes TikZ Faltantes

**Errores comunes:**

- Falta `\usepackage{tikz}` en header-includes
- Falta `\usetikzlibrary{3d,babel}` para diagramas 3D
- Falta `\usepackage{pgfplots}` para gráficos

### Sintaxis TikZ Incorrecta

**Validaciones:**

- Todo código TikZ debe estar dentro de `\begin{tikzpicture}...\end{tikzpicture}`
- Usar `include_tikz()` en chunks R (no código directo en markdown)
- Verificar que las coordenadas sean numéricas válidas

---

## 🔍 VALIDACIONES ESPECÍFICAS POR TIPO

### Ejercicios con Python (Reticulate)

**Chunk Python Requerido:**

```python
```{python nombre_chunk, echo=FALSE, results="hide"}
import matplotlib.pyplot as plt
import numpy as np

# Recibir datos desde R
datos_r = r.datos_variable

# [Código de visualización]
plt.savefig('grafico.png', dpi=150, bbox_inches='tight')
plt.close()
```

**Validaciones:**

- Chunk debe tener `engine='python'` o ser `{python ...}`
- Debe cerrar plt.close() para evitar memory leaks
- DPI mínimo: 150
- Debe usar `r.variable` para acceder a datos de R

### Ejercicios con TikZ

**Chunk TikZ Requerido:**
```r
```{r generar_tikz, echo=FALSE, results="asis"}
tikz_diagram <- '
\\begin{tikzpicture}[scale=1.2]
  % [Código TikZ]
\\end{tikzpicture}
'

include_tikz(tikz_diagram,
             name = "diagrama_tikz",
             markup = "markdown",
             format = typ,
             library = c("3d", "babel"),
             packages = c("tikz", "xcolor", "pgfplots"),
             width = "10cm")
```

**Validaciones:**

- Usar `include_tikz()` de exams
- Formato debe usar variable `typ` (match_exams_device())
- Librerías TikZ correctas según necesidades
- Escapar backslashes en strings R: `\\begin`

---

## 📊 CRITERIOS DE CALIDAD OBLIGATORIOS

### Diversidad de Versiones
- **Mínimo 300 versiones únicas** verificadas con test
- Función `generar_datos()` debe incluir validación
- Chunk de prueba de diversidad obligatorio

### Formato Numérico
- Sin notación científica: `options(scipen = 999)`
- Punto decimal: `options(OutDec = ".")`
- Sin separador de miles en respuestas numéricas
- Locale: `Sys.setlocale(category = "LC_NUMERIC", locale = "C")`

### Opciones de Respuesta
- Mínimo 4 opciones diferentes
- Sin valores duplicados (anti-duplicados)
- Distractores plausibles y educativos

### Metadatos ICFES
- Todos los campos presentes
- Valores válidos según enumeración
- Coherencia entre competencia, nivel y componente

---

## 🚨 PATRONES DE ERROR RECURRENTES

### Categoría A: Gramaticales/Concordancia
- "La conteo" → "El conteo"
- Verificar concordancia género/número en enunciados

### Categoría B: Posicionamiento TikZ
- Orden correcto: texto → tabla → pregunta
- Verificar que gráficos no oculten contenido crítico

### Categoría C: Generación de Datos
- Opciones únicas (problemas de moda con duplicados)
- Validar que datasets incluyan pares e impares para mediana

### Categoría D: Compilación LaTeX/TikZ
- Paquetes faltantes en header-includes
- Caracteres especiales sin escape
- Errores de sintaxis TikZ (coordenadas inválidas)

### Categoría E: Estructura R-exams
- YAML incompleto o incorrecto
- Chunks mal configurados (include_tikz incorrecto)
- Variables no definidas antes de uso

---

## 📁 REFERENCIAS OBLIGATORIAS

**Ejemplos Funcionales:**
- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (CONSULTA OBLIGATORIA)

**Archivos de Dependencia:**

- `SemilleroCloze.R`
- `SemilleroMoodle_v2.R`
- `SemilleroUnico_v2.R`
- `pcielo.tex`, `pcielo_nosol.tex`, `solpcielo.tex`

---

## ✅ CHECKLIST DE VALIDACIÓN PRE-RENDER

Antes de considerar un archivo .Rmd como válido, verificar:

- [ ] YAML header completo con paquetes LaTeX/TikZ
- [ ] Metadatos ICFES completos y válidos
- [ ] Chunk `{r inicio}` con configuración correcta
- [ ] `set.seed()` aleatorio (no fijo)
- [ ] `options(scipen = 999)` y `options(OutDec = ".")` presentes
- [ ] Sección Question con mínimo 4 opciones diferentes
- [ ] Sección Solution completa
- [ ] Meta-information completa
- [ ] Si usa Python: `use_python()` configurado
- [ ] Si usa TikZ: `include_tikz()` correctamente implementado
- [ ] Caracteres especiales escapados en LaTeX
- [ ] Chunk de prueba de diversidad de versiones (300+)
- [ ] Formato numérico consistente (sin notación científica)
- [ ] Sin valores duplicados en opciones de respuesta

---

*Este documento actúa como fuente de verdad única para validación automática de archivos .Rmd del proyecto ICFES R-Exams.*
