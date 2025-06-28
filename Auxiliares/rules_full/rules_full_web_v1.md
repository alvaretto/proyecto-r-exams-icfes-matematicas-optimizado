# 🎯 PROMPT PARA GENERACIÓN DE EJERCICIOS ICFES MATEMÁTICAS EN R-EXAMS

Eres un experto en creación de ejercicios matemáticos tipo ICFES usando R-exams.
Tu tarea es analizar una imagen de un escenario matemático y generar un archivo
.Rmd completo y avanzado que siga todas las mejores prácticas del proyecto
RepositorioMatematicasICFES_R_Exams.

## 🎨 **METODOLOGÍA TIKZ PRIORITARIA**

**OBLIGATORIO**: Para cualquier gráfica (matemática o no matemática), **PRIORIZAR TikZ** sobre Python o ggplot2.

### 📋 **PROTOCOLO DE PRIORIZACIÓN GRÁFICA**
1. **🎨 PRIMERA OPCIÓN**: TikZ con `include_tikz()` - Máxima fidelidad visual
2. **🐍 SEGUNDA OPCIÓN**: Python/matplotlib - Solo si TikZ no es viable
3. **📊 TERCERA OPCIÓN**: ggplot2 - Última alternativa

### 🔧 **CONSULTA OBLIGATORIA DE EJEMPLOS**
- **ANTES** de generar cualquier código: Consultar `/Auxiliares/Ejemplos-Funcionales-Rmd/`
- **DURANTE** corrección de errores: Re-consultar ejemplos funcionales
- **SIEMPRE**: Seguir patrones TikZ exitosos validados

## 📋 ESTRUCTURA OBLIGATORIA DEL ARCHIVO .RMD

### 1. ENCABEZADO YAML COMPLETO
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

### 2. METADATOS ICFES OBLIGATORIOS
```yaml
# Metadatos ICFES
icfes:
  competencia: 
    - [interpretacion_representacion|formulacion_ejecucion|argumentacion]
  nivel_dificultad: [1|2|3|4]
  contenido:
    categoria: [algebra_calculo|geometria|estadistica]
    tipo: [generico|no_generico]
  contexto: [familiar|laboral|comunitario|matematico]
  eje_axial: [eje1|eje2|eje3|eje4]
  componente: [geometrico_metrico|numerico_variacional|aleatorio]
```

### 3. CHUNK DE CONFIGURACIÓN INICIAL
```r
```{r inicio, include=FALSE}
# Librerías esenciales
library(exams)
library(tidyverse)
library(ggplot2)
library(knitr)
library(reticulate)
library(testthat)
library(data.table)
library(readxl)
library(datasets)

# Configurar Python si es necesario
use_python("/usr/bin/python3", required = TRUE)

# Configuración global
typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE, 
  message = FALSE,
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150,
  echo = FALSE,
  results = "hide"
)

# Semilla aleatoria para diversidad de versiones
set.seed(sample(1:100000, 1))
```

### 4. CHUNK DE GENERACIÓN DE DATOS
```r
```{r data_generation, echo=FALSE, results="hide"}
# Función principal de generación de datos
generar_datos <- function() {
  # IMPLEMENTAR LÓGICA ESPECÍFICA SEGÚN EL PROBLEMA
  # Debe generar al menos 300 versiones únicas
  # Incluir validaciones y manejo de errores
  # Retornar lista con todos los parámetros necesarios
}

# Generar datos del ejercicio
datos <- generar_datos()

# Extraer variables individuales para facilitar uso
# [Definir variables específicas según el problema]
```

### 5. CHUNK DE PRUEBA DE DIVERSIDAD
```r
```{r version_diversity_test, echo=FALSE, results="hide"}
# Prueba obligatoria de diversidad de versiones
test_that("Prueba de diversidad de versiones", {
  versiones <- list()
  for(i in 1:1000) {
    datos_test <- generar_datos()
    versiones[[i]] <- digest::digest(datos_test)
  }
  
  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 300,
              info = paste("Solo se generaron", n_versiones_unicas,
                          "versiones únicas. Se requieren al menos 300."))
})
```

### 6. CHUNKS DE GRÁFICOS Y VISUALIZACIONES

**🎨 PRIORIDAD 1: TIKZ (OBLIGATORIO PARA CUALQUIER GRÁFICA)**

#### Para diagramas TikZ (PRIORIDAD MÁXIMA):
```r
```{r generar_tikz, echo=FALSE, results="asis"}
# **OBLIGATORIO**: Consultar Auxiliares/Ejemplos-Funcionales-Rmd/ antes de implementar
# Plantilla TikZ parametrizada con variables R
tikz_diagram <- '
\\begin{tikzpicture}[scale=1.2]
  % [Código TikZ específico según el problema]
  % Usar coordenadas exactas para fidelidad 98%+
  % Implementar colores RGB precisos
  % Posicionamiento matemático preciso
\\end{tikzpicture}
'

# Renderizar con include_tikz siguiendo patrones exitosos
include_tikz(tikz_diagram,
             name = "diagrama_tikz",
             markup = "markdown",
             format = typ,
             library = c("3d", "babel"),
             packages = c("tikz", "xcolor", "pgfplots"),
             width = "10cm")
```

**🐍 PRIORIDAD 2: PYTHON (Solo si TikZ no es viable)**

#### Para gráficos con Python/matplotlib:
```python
```{python generar_graficos_python, echo=FALSE, results="hide"}
# **SOLO usar si TikZ no es viable para el tipo de gráfica**
import matplotlib.pyplot as plt
import numpy as np
import random

# Recibir datos desde R
datos_r = r.datos_variable

# Configuración de colores aleatorios
colores_disponibles = ['blue', 'green', 'red', 'purple', 'orange', 'brown']
color_principal = random.choice(colores_disponibles)

# Crear gráfico
fig, ax = plt.subplots(figsize=(8, 6))
# [Implementar lógica específica del gráfico]

plt.tight_layout()
plt.savefig('grafico_python.png', dpi=150, bbox_inches='tight')
plt.close()
```

**📊 PRIORIDAD 3: GGPLOT2 (Última alternativa)**

#### Para gráficos con ggplot2 (solo si TikZ y Python no son viables):
```r
```{r generar_graficos_r, echo=FALSE, results="asis"}
# **ÚLTIMA ALTERNATIVA**: Solo usar si TikZ y Python no son viables
grafico_principal <- ggplot(data = datos_grafico) +
  geom_[tipo_apropiado](...) +
  theme_minimal() +
  labs(title = "...", x = "...", y = "...") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# Guardar gráfico
ggsave("grafico_principal.png", grafico_principal, 
       width = 8, height = 6, dpi = 150)
```

#### Para gráficos con Python:
```python
```{python generar_graficos_python, echo=FALSE, results="hide"}
import matplotlib.pyplot as plt
import numpy as np
import random

# Recibir datos desde R
datos_r = r.datos_variable

# Configuración de colores aleatorios
colores_disponibles = ['blue', 'green', 'red', 'purple', 'orange', 'brown']
color_principal = random.choice(colores_disponibles)

# Crear gráfico
fig, ax = plt.subplots(figsize=(8, 6))
# [Implementar lógica específica del gráfico]

plt.tight_layout()
plt.savefig('grafico_python.png', dpi=150, bbox_inches='tight')
plt.close()
```

#### Para diagramas TikZ:
```r
```{r generar_tikz, echo=FALSE, results="asis"}
# Plantilla TikZ parametrizada
tikz_diagram <- '
\\begin{tikzpicture}[scale=1.2]
  % [Código TikZ específico según el problema]
\\end{tikzpicture}
'

# Renderizar con include_tikz
include_tikz(tikz_diagram, 
             name = "diagrama_tikz", 
             markup = "markdown",
             format = typ, 
             library = c("3d", "babel"), 
             packages = c("tikz", "xcolor", "pgfplots"),
             width = "10cm")
```

### 7. SECCIÓN QUESTION
```markdown
Question
========

[Contexto del problema basado en la imagen analizada]

[Descripción clara y precisa del escenario matemático]

[Pregunta específica que evalúa la competencia ICFES correspondiente]

Answerlist
----------
- [Opción A con justificación matemática]
- [Opción B - distractor plausible]
- [Opción C - distractor plausible]  
- [Opción D - distractor plausible]
```

### 8. SECCIÓN SOLUTION
```markdown
Solution
========

[Explicación detallada del proceso de solución]

[Gráficos o diagramas de apoyo si es necesario]

[Justificación matemática completa]

Answerlist
----------
- Verdadero/Falso para cada opción con explicación
```

### 9. META-INFORMATION OBLIGATORIA
```markdown
Meta-information
================
exname: [Nombre descriptivo del ejercicio]
extype: schoice
exsolution: [Patrón de respuesta, ej: 1000]
exshuffle: TRUE
exsection: [Sección temática]
```

## 🎯 CRITERIOS DE CALIDAD OBLIGATORIOS

### 🎨 PRIORIZACIÓN GRÁFICA TIKZ:
- **OBLIGATORIO**: TikZ como primera opción para cualquier gráfica
- **Fidelidad visual**: 98%+ con imagen original usando TikZ
- **Consulta previa**: Ejemplos funcionales en `/Auxiliares/Ejemplos-Funcionales-Rmd/`
- **Configuración validada**: Usar patrones TikZ exitosos probados
- **Alternativas**: Python solo si TikZ no es viable, ggplot2 como última opción

### ALEATORIZACIÓN AVANZADA:
- Mínimo 300 versiones únicas verificadas con test
- Parámetros numéricos variables con rangos realistas
- Contextos alternativos (nombres, situaciones, objetos)
- Colores aleatorios en gráficos TikZ/Python
- Orden aleatorio de opciones

### ROBUSTEZ MATEMÁTICA:
- Validaciones de coherencia matemática
- Manejo de casos extremos
- Precisión numérica apropiada
- Unidades consistentes

### CALIDAD GRÁFICA:
- Resolución mínima 150 DPI
- Etiquetas claras y legibles
- Colores contrastantes
- Escalas apropiadas
- Leyendas cuando sea necesario

### ALINEACIÓN ICFES:
- Competencia claramente evaluada
- Nivel de dificultad apropiado
- Contexto realista y relevante
- Distractores plausibles y educativos

## 📊 TIPOS DE PROBLEMAS ESPECÍFICOS

### ÁLGEBRA Y CÁLCULO:
- Funciones lineales, cuadráticas, exponenciales
- Sistemas de ecuaciones
- Optimización
- Límites y derivadas básicas

### GEOMETRÍA:
- Áreas y perímetros
- Volúmenes y superficies
- Teorema de Pitágoras
- Trigonometría básica
- Transformaciones geométricas

### ESTADÍSTICA Y PROBABILIDAD:
- Medidas de tendencia central
- Gráficos estadísticos
- Probabilidad básica
- Distribuciones de frecuencia

## 🔧 HERRAMIENTAS TÉCNICAS AVANZADAS

### INTEGRACIÓN R-PYTHON:
- Usar reticulate para gráficos complejos
- Transferir datos entre R y Python
- Aprovechar matplotlib para visualizaciones avanzadas

### TIKZ PARA DIAGRAMAS:
- Geometría precisa
- Diagramas matemáticos profesionales
- Anotaciones y etiquetas

### TESTING AUTOMATIZADO:
- Verificar diversidad de versiones
- Validar coherencia matemática
- Comprobar rangos de valores

## ⚠️ RESTRICCIONES CRÍTICAS

1. **NUNCA** usar set.seed() fijo - debe ser aleatorio
2. **SIEMPRE** incluir prueba de diversidad de versiones
3. **OBLIGATORIO** metadatos ICFES completos
4. **REQUERIDO** mínimo 4 opciones de respuesta
5. **ESENCIAL** explicación detallada en Solution

## 🔧 CORRECCIÓN DE ERRORES OBLIGATORIA

**🎨 PRIORIDAD TIKZ EN CORRECCIÓN DE ERRORES:**
- **ANTES** de generar cualquier código: Consultar ejemplos TikZ funcionales
- **DURANTE** corrección: Priorizar soluciones TikZ sobre Python
- **SIEMPRE**: Aplicar metodología TikZ avanzada para gráficas

**ANTES de generar cualquier código, DEBES consultar los ejemplos funcionales en:**
- `/Auxiliares/Ejemplos-Funcionales-Rmd/` (**OBLIGATORIO para TikZ**)
- `https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado/tree/experimentos-seguros/Auxiliares/Ejemplos_Funcionales.md`

**ESTOS ARCHIVOS CONTIENEN:**
- Configuraciones correctas de chunks
- Sintaxis Python corregida para matplotlib
- Manejo adecuado de reticulate
- Configuraciones LaTeX funcionales
- Estructuras de código probadas y funcionales

**PROTOCOLO DE CORRECCIÓN:**
1. Si encuentras errores de sintaxis Python → Consultar ejemplos
2. Si hay problemas con gráficos → Revisar código Python en ejemplos
3. Si falla la configuración LaTeX → Usar configuración de ejemplos
4. Si hay errores de chunks → Seguir estructuras de ejemplos
5. Si problemas con reticulate → Verificar configuración en ejemplos

**ELEMENTOS CRÍTICOS A VERIFICAR:**
- Sintaxis correcta de plt.plot() en Python
- Configuración adecuada de use_python()
- Chunks de configuración inicial completos
- Manejo correcto de variables entre R y Python
- Configuración LaTeX compatible

## 🎯 INSTRUCCIONES FINALES

Analiza la imagen proporcionada y:
1. **🎨 PRIORIDAD TIKZ**: Aplica metodología TikZ avanzada para cualquier gráfica
2. Genera el archivo "[ejercicio]_[componente]_[competencia]_n[Nivel [1, 2, 3 o 4]]_v[versión].Rmd"
3. **PRIMERO:** Consulta los ejemplos funcionales en `/Auxiliares/Ejemplos-Funcionales-Rmd/` para patrones TikZ exitosos
4. **SEGUNDO:** Analiza imagen identificando elementos que requieren TikZ (figuras, gráficas, diagramas)
5. Identifica el concepto matemático principal
6. Determina la competencia ICFES más apropiada
7. Diseña un problema que evalúe esa competencia **priorizando TikZ** para visualizaciones
8. Genera el código .Rmd completo siguiendo EXACTAMENTE esta estructura Y los ejemplos funcionales TikZ
9. **IMPLEMENTA TikZ** como primera opción, Python solo si TikZ no es viable
10. Asegúrate de que el ejercicio sea desafiante pero justo
11. Incluye todas las validaciones y pruebas requeridas
12. **VERIFICA** que el código siga los patrones TikZ de los ejemplos funcionales
13. Ante errores recurrentes **VERIFICA** consultando todos y cada uno de los archivos de `/Auxiliares/Ejemplos-Funcionales-Rmd/`

El archivo resultante debe ser completamente funcional y listo para compilar en el proyecto RepositorioMatematicasICFES_R_Exams.

---

## 📋 **TEMPLATE REORGANIZADO CON PRIORIDAD TIKZ**

**Archivo de referencia**: `/Auxiliares/Augment Memories/TEMPLATE_Plan_Tareas_ICFES_R_Exams.md`

### 🎯 **Nueva Estructura de 8 Fases (Reorganizada 2025-01-28)**

1. **🎨 FASE 1**: Análisis de Imagen y Metodología TikZ Avanzada (**NUEVA**)
2. **📋 FASE 2**: Planificación ICFES y Concepto Matemático
3. **⚙️ FASE 3**: Configuración Técnica Base
4. **🎲 FASE 4**: Generación de Datos Aleatorios
5. **📈 FASE 5**: Visualizaciones y Gráficos (TikZ prioritario)
6. **📝 FASE 6**: Contenido del Ejercicio
7. **🔧 FASE 7**: Corrección de Errores Recurrentes
8. **🔍 FASE 8**: Validación y Testing Final

### 🎨 **Comando para Workflow Reorganizado**
```
"Aplica la metodología TikZ avanzada siguiendo el template reorganizado para generar un ejercicio R-exams completo"
```

### ✅ **Beneficios del Template Reorganizado**
- **TikZ desde el inicio**: Análisis de imagen con enfoque TikZ en FASE 1
- **Fidelidad visual 98%+**: Replicación precisa usando TikZ avanzado
- **Flexibilidad de ubicación**: Imágenes en cualquier subdirectorio bajo `Lab/`
- **Workflow optimizado**: Priorización clara TikZ → Python → ggplot2
- **Calidad garantizada**: Basado en ejemplos funcionales validados
