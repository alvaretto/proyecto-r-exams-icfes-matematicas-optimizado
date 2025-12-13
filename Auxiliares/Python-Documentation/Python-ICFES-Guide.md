# 🐍 Guía Python (Reticulate) para Ejercicios ICFES Matemáticas

## 📋 Análisis de Recursos Python Existentes en el Proyecto

### 🔍 **Inventario Actual de Archivos .Rmd con Python**

#### **📊 Gráficos Estadísticos - Adopción de Mascotas**
**Ubicación**: `06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/01-Variables-Cualitativas_Distribucion-De-Frecuencias/Graficos_Estadisticos_Adopcion_Mascotas/`

1. **Gráficos Múltiples con Python** (`I_1796473-Opc-A2v2.Rmd`)
   - **Competencia ICFES**: Pensamiento Aleatorio - Interpretación y Representación
   - **Nivel**: 1-2 (Lectura de gráficos estadísticos)
   - **Características**:
     - Transferencia de datos R → Python con `r.variable`
     - Gráficos de barras verticales y horizontales
     - Gráficos circulares (pie charts)
     - Configuración matplotlib optimizada
     - Colores personalizados con códigos hex
   - **Bibliotecas Python**: `matplotlib.pyplot`, `numpy`
   - **Compatibilidad R-exams**: ✅ Excelente

2. **Plantillas Reutilizables** (`General/Plantillas/Python/AdopcionMascotas/`)
   - **Competencia ICFES**: Pensamiento Aleatorio
   - **Nivel**: 1-2 (Interpretación de datos)
   - **Características**:
     - Configuración Python estandarizada
     - Transferencia de variables optimizada
     - Múltiples tipos de gráficos
     - Formateo consistente
   - **Bibliotecas Python**: `matplotlib.pyplot`, `numpy`
   - **Compatibilidad R-exams**: ✅ Excelente

#### **📈 Funciones y Variación Lineal**
**Ubicación**: `02-Funciones/Pensamiento-Variacional-Espacial/11-Variacion-Lineal-Y-Exponencial_Razon-De-Cambio/Variacion-Lineal-Vuelo-Acrobatico/`

3. **Gráficos de Funciones Lineales** (`vuelo_acrobatico_A.Rmd`)
   - **Competencia ICFES**: Pensamiento Variacional-Espacial
   - **Nivel**: 2-3 (Interpretación de gráficos de funciones)
   - **Características**:
     - Gráficos de funciones por segmentos
     - Variables aleatorias integradas
     - Múltiples opciones de respuesta
     - Configuración matplotlib avanzada
     - Manejo de arrays numpy
   - **Bibliotecas Python**: `matplotlib.pyplot`, `numpy`
   - **Compatibilidad R-exams**: ✅ Excelente

#### **📊 Análisis Estadístico Avanzado**
**Ubicación**: `A-Produccion/Ejemplos-Funcionales-Rmd/`

4. **Gráficos de Líneas Múltiples** (`Ejemplo_01.Rmd`)
   - **Competencia ICFES**: Pensamiento Aleatorio
   - **Nivel**: 2-3 (Análisis de tendencias)
   - **Características**:
     - Generación de código Python dinámico
     - Sprintf para parametrización
     - Múltiples series de datos
     - Configuración avanzada matplotlib
     - Integración con TikZ para tablas
   - **Bibliotecas Python**: `matplotlib.pyplot`, `numpy`, `matplotlib`
   - **Compatibilidad R-exams**: ✅ Excelente

## 🎯 **Patrones Exitosos Identificados en Archivos .Rmd Validados**

### ✅ **PATRONES ALTAMENTE COMPATIBLES** (Basados en archivos exitosos)

#### **1. 🏆 Transferencia de Datos R → Python** (Patrón más exitoso)
**Fuente**: `I_1796473-Opc-A2v2.Rmd`, `vuelo_acrobatico_A.Rmd`
```python
# Patrón exitoso para transferir variables de R a Python
mascota1py = r.nombremascota1  # De R a Python
porcentaje1py = r.porxentaje1  # De R a Python
tiempo_aumento = r.tiempos_aumento  # De R a Python
altura_max = r.altura_maxima  # De R a Python
```
- **Ventajas**: Integración directa y confiable
- **Uso**: Transferir cualquier tipo de variable (números, strings, listas)
- **Sintaxis**: `variable_python = r.variable_r`

#### **2. 📊 Gráficos de Barras Parametrizables**
**Fuente**: `I_1796473-Opc-A2.Rmd`, `I_1796473-Opc-A2v2.Rmd`
```python
import matplotlib.pyplot as plt
import numpy as np

# Datos desde R
animales = [mascota1py, mascota2py, mascota3py]
cantidad = [porcentaje1py, porcentaje2py, porcentaje3py]

# Colores personalizados
colores_azules = ['#00B3E6', '#0066CC', '#000099']

# Crear gráfico
fig, ax = plt.subplots(figsize=(6, 5))
bars = ax.bar(animales, cantidad, color=colores_azules, width=0.5)

# Configuración estándar exitosa
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['left'].set_linewidth(2)
ax.spines['bottom'].set_linewidth(2)
ax.yaxis.grid(True, linestyle='--', linewidth=0.7, color='darkgray')
plt.xticks(fontweight='bold')
plt.yticks(fontweight='bold')
plt.xlabel("Etiqueta X", fontweight='bold')
plt.ylabel("Etiqueta Y", fontweight='bold')
plt.show()
```

#### **3. 🥧 Gráficos Circulares (Pie Charts)**
**Fuente**: `I_1796473-Opc-A2.Rmd`, `I_1796473-Opc-A2v2.Rmd`
```python
from matplotlib import pyplot as plt

# Datos desde R
sizes = [adoptantes1py, adoptantes2py, adoptantes3py]
labels = ['Etiqueta 1', 'Etiqueta 2', 'Etiqueta 3']
colors = ['#1f77b4', '#aec7e8', '#ff7f0e']
explode = (0, 0, 0)

# Crear gráfico circular
fig, ax = plt.subplots(figsize=(5, 4), tight_layout=True)
ax.pie(sizes, explode=explode, labels=labels, colors=colors, 
       shadow=True, startangle=0)
ax.axis('equal')

plt.subplots_adjust(left=0.1, top=0.9)
plt.show()
```

#### **4. 📈 Gráficos de Funciones Lineales**
**Fuente**: `vuelo_acrobatico_A.Rmd`
```python
import matplotlib.pyplot as plt
import numpy as np

# Variables desde R
tiempo_aumento = r.tiempos_aumento
tiempo_horizontal = r.tiempos_horizontal  
tiempo_aterrizaje = r.tiempos_aterrizaje
altura_max = r.altura_maxima

# Definir segmentos de la función
x1 = np.array([0, tiempo_aumento])
y1 = np.array([0, altura_max])

x2 = np.array([tiempo_aumento, tiempo_aumento + tiempo_horizontal])
y2 = np.array([altura_max, altura_max])

x3 = np.array([tiempo_aumento + tiempo_horizontal, 
               tiempo_aumento + tiempo_horizontal + tiempo_aterrizaje])
y3 = np.array([altura_max, 0])

# Crear figura
fig = plt.figure()
ax = fig.add_subplot(111)

# Graficar segmentos
ax.plot(x1, y1, 'r-', linewidth=2)
ax.plot(x2, y2, 'r-', linewidth=2)
ax.plot(x3, y3, 'r-', linewidth=2)

# Configuración
ax.set_xlabel('Tiempo (s)', fontweight='bold')
ax.set_ylabel('Altura (m)', fontweight='bold')
ax.grid(True, linestyle='--', alpha=0.7)

plt.subplots_adjust(top=1, bottom=0.1, left=0.1, right=0.9)
plt.show()
```

### ✅ **CONFIGURACIONES TÉCNICAS VALIDADAS**

#### **Setup Chunk Estándar Exitoso**
```r
library(exams)
library(reticulate)
library(digest)
library(testthat)

# Configurar Python (patrón exitoso)
use_python("/usr/bin/python3", required = TRUE)
# O alternativa más portable:
# use_python(Sys.which("python"), required = TRUE)

typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.showtext = FALSE,
  fig.cap = "",
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150
)

# Configuración para chunks de Python (patrón validado)
knitr::knit_engines$set(python = function(options) {
  knitr::engine_output(options, options$code, '')
})
```

#### **Configuración de Chunks Python**
```r
# Patrón exitoso para chunks Python
```{python NombreChunk, echo=FALSE, message=FALSE, comment='', warning=FALSE, results="hide"}
# Código Python aquí
```

#### **Generación Dinámica de Código Python**
**Fuente**: `Ejemplo_01.Rmd`
```r
# Patrón exitoso para código Python dinámico
codigo_base_python <- "
import matplotlib.pyplot as plt
import numpy as np
import matplotlib
matplotlib.rcParams['font.size'] = 9

años = %s
mortalidad_hombres = %s
mortalidad_mujeres = %s
color_hombres = '%s'
color_mujeres = '%s'
"

# Reemplazar valores
codigo_python_final <- sprintf(codigo_base_python, 
                              paste(años, collapse=", "), 
                              paste(mortalidad_hombres, collapse=", "), 
                              paste(mortalidad_mujeres, collapse=", "),
                              color_hombres,
                              color_mujeres)
```

### ✅ **BIBLIOTECAS PYTHON VALIDADAS**

#### **Bibliotecas Esenciales** (Compatibilidad garantizada)
```python
import matplotlib.pyplot as plt  # Gráficos principales
import numpy as np               # Arrays y cálculos
import matplotlib               # Configuración avanzada
```

#### **Configuraciones Matplotlib Exitosas**
```python
# Configuración de figura estándar
matplotlib.rcParams['font.size'] = 9
plt.rcParams['figure.figsize'] = (8, 5)
plt.rcParams['figure.titlesize'] = 0

# Configuración de ejes estándar
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['left'].set_linewidth(2)
ax.spines['bottom'].set_linewidth(2)

# Grid estándar
ax.grid(True, linestyle='--', alpha=0.7)
# O para ejes específicos:
ax.yaxis.grid(True, linestyle='--', linewidth=0.7, color='darkgray')

# Etiquetas con formato
plt.xticks(fontweight='bold')
plt.yticks(fontweight='bold')
plt.xlabel("Etiqueta", fontweight='bold')
plt.ylabel("Etiqueta", fontweight='bold')

# Ajustes de layout
plt.subplots_adjust(top=1, bottom=0.1, left=0.1, right=0.9)
plt.tight_layout()
```

### ⚠️ **PATRONES QUE REQUIEREN ADAPTACIÓN**
1. **Rutas absolutas de Python** → Usar `Sys.which("python")` o configuración portable
2. **Bibliotecas no estándar** → Validar disponibilidad antes de usar
3. **Configuraciones específicas del sistema** → Usar configuraciones genéricas
4. **Tamaños de figura muy grandes** → Optimizar para múltiples formatos
5. **Dependencias externas** → Mantener solo bibliotecas esenciales

### ❌ **PATRONES PROBLEMÁTICOS** (Evitar)
1. **Bibliotecas no instaladas** por defecto (seaborn, plotly, etc.)
2. **Configuraciones específicas del OS**
3. **Archivos temporales** sin gestión adecuada
4. **Imports innecesarios** que ralentizan la ejecución
5. **Configuraciones matplotlib** muy específicas del sistema

## 📋 **Clasificación por Competencias ICFES**

### 📊 **Pensamiento Aleatorio**
- **Recursos actuales**: ✅ Excelentes (gráficos de barras, circulares, líneas)
- **Fortalezas**: Visualización de datos, estadística descriptiva
- **Casos de uso**: Encuestas, frecuencias, probabilidades
- **Prioridad de búsqueda**: 🟡 Media (bien cubierto)

### 📈 **Pensamiento Variacional-Espacial**
- **Recursos actuales**: ✅ Buenos (funciones lineales, gráficos de coordenadas)
- **Fortalezas**: Gráficos de funciones, variación
- **Gaps**: Funciones cuadráticas, exponenciales, trigonométricas
- **Prioridad de búsqueda**: 🟡 Media

### 🔢 **Pensamiento Numérico-Variacional**
- **Recursos actuales**: ⚠️ Limitados
- **Necesidades**:
  - Representaciones numéricas
  - Gráficos de proporciones
  - Visualización de operaciones
- **Prioridad de búsqueda**: 🔴 Alta

### 📐 **Pensamiento Espacial-Métrico**
- **Recursos actuales**: ❌ Escasos
- **Necesidades**:
  - Gráficos geométricos 2D
  - Representaciones de medidas
  - Transformaciones geométricas
- **Prioridad de búsqueda**: 🔴 Alta

## 🌐 **Plan de Búsqueda Recursiva Específico**

### 🎯 **Fase 1: Búsquedas Prioritarias**

#### **📐 Geometría y Medidas**
- [ ] **Matplotlib Geometry**: Búscar "matplotlib geometry", "matplotlib shapes"
- [ ] **Educational Python**: "python geometry education", "matplotlib geometric figures"
- [ ] **GitHub**: Repositorios "python geometry matplotlib", "educational geometry python"
- [ ] **Stack Overflow**: Soluciones para gráficos geométricos educativos

#### **🔢 Representaciones Numéricas**
- [ ] **Number Line**: "matplotlib number line", "python number representation"
- [ ] **Fractions**: "matplotlib fractions", "python fraction visualization"
- [ ] **Proportions**: "matplotlib proportions", "python ratio visualization"

### 🔧 **Fase 2: Adaptación para R-exams**

#### **Criterios de Adaptación**
1. **Simplificar dependencias**: Usar solo matplotlib y numpy
2. **Optimizar transferencia R→Python**: Usar patrón `r.variable`
3. **Estandarizar configuración**: Aplicar patrones exitosos identificados
4. **Validar multi-formato**: Probar en HTML, PDF, Moodle

#### **Template de Adaptación**
```python
# ANTES (problemático)
import seaborn as sns
import pandas as pd
plt.style.use('seaborn')

# DESPUÉS (compatible con R-exams)
import matplotlib.pyplot as plt
import numpy as np
# Usar configuración estándar validada
```

## 📁 **Organización de Recursos Encontrados**

### 📊 **Estadística** (`estadistica/`)
- `graficos-barras/`: Verticales, horizontales, agrupadas
- `graficos-circulares/`: Pie charts, donut charts
- `histogramas/`: Distribuciones de frecuencia
- `diagramas-dispersion/`: Correlaciones, tendencias

### 📈 **Álgebra-Cálculo** (`algebra-calculo/`)
- `funciones-lineales/`: Rectas, sistemas, intersecciones
- `funciones-cuadraticas/`: Parábolas, vértices, raíces
- `graficos-coordenadas/`: Plano cartesiano, puntos
- `variacion/`: Razones de cambio, proporcionalidad

### 📐 **Geometría** (`geometria/`)
- `figuras-2d/`: Polígonos, círculos, construcciones
- `transformaciones/`: Rotaciones, traslaciones, reflexiones
- `medidas/`: Perímetros, áreas, volúmenes
- `coordenadas/`: Geometría analítica

## 🔄 **Proceso de Validación**

### ✅ **Checklist de Compatibilidad R-exams**
- [ ] Funciona con `reticulate`
- [ ] Transferencia R→Python exitosa
- [ ] Genera imagen en `exams2html`
- [ ] Compatible con `exams2pdf`
- [ ] Funciona en `exams2moodle`
- [ ] Escalable para múltiples variantes

### 🧪 **Script de Prueba**
```r
# Probar compatibilidad Python-Reticulate
test_python_compatibility <- function(rmd_file) {
  # Crear archivo temporal .Rmd
  # Probar generación en múltiples formatos
  # Reportar errores y compatibilidad
}
```

---

**Última actualización**: `r Sys.Date()`  
**Versión**: 1.0  
**Mantenedor**: Proyecto ICFES R-exams