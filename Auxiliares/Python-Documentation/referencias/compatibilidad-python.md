# 🐍 Guía de Compatibilidad Python-Reticulate con R-exams

## 🎯 **Análisis de Compatibilidad por Formato**

### ✅ **exams2pdf - Compatibilidad Excelente**
- **Procesamiento**: Python → matplotlib → PNG/PDF → LaTeX
- **Bibliotecas soportadas**: matplotlib, numpy (validadas)
- **Limitaciones**: Ninguna significativa
- **Recomendaciones**: 
  - Usar como formato de referencia para desarrollo
  - Configurar DPI=150 para calidad óptima

### ✅ **exams2html - Compatibilidad Excelente**
- **Procesamiento**: Python → matplotlib → PNG → HTML
- **Ventajas**: Renderizado rápido y confiable
- **Configuración exitosa**:
  - `results="hide"` en chunks Python
  - `plt.show()` obligatorio
  - Figsize optimizado: `(6, 5)` o `(8, 5)`

### ✅ **exams2moodle - Compatibilidad Excelente**
- **Procesamiento**: Similar a HTML
- **Consideraciones validadas**:
  - Imágenes se incrustan automáticamente
  - Colores optimizados para web
  - Tamaño de archivo controlado
- **Configuración exitosa**:
  - DPI=150 para balance calidad/tamaño
  - Colores con códigos hex específicos

### 🔄 **exams2pandoc - Compatibilidad Buena**
- **Dependencias**: Pandoc + Python + matplotlib
- **Factores**: Versión de reticulate y configuración Python
- **Recomendaciones**:
  - Validar configuración Python antes de usar
  - Usar configuración portable de Python
  - Probar con versión específica de Pandoc

### 📄 **exams2nops - Compatibilidad Buena**
- **Procesamiento**: Similar a PDF
- **Consideraciones**:
  - Optimizado para impresión en escala de grises
  - Evitar colores muy claros o similares
  - Usar contraste alto para escaneo

## 🛠️ **Configuraciones Validadas por Archivos Exitosos**

### ✅ **Setup Chunk Estándar** (Basado en archivos exitosos)
```r
library(exams)
library(reticulate)
library(digest)
library(testthat)

# Configuración Python validada
use_python("/usr/bin/python3", required = TRUE)
# Alternativa portable: use_python(Sys.which("python"), required = TRUE)

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

# Configuración chunks Python (patrón exitoso)
knitr::knit_engines$set(python = function(options) {
  knitr::engine_output(options, options$code, '')
})
```

### ✅ **Configuración de Chunks Python** (Patrones validados)
```r
# Configuración estándar exitosa
```{python NombreChunk, echo=FALSE, message=FALSE, comment='', warning=FALSE, results="hide"}
# Código Python aquí
plt.show()  # OBLIGATORIO para R-exams
```

### ✅ **Transferencia de Datos R→Python** (Patrón más exitoso)
```python
# Patrón validado en múltiples archivos exitosos
variable_python = r.variable_r  # Transferencia directa
lista_python = r.lista_r        # Listas/vectores
numero_python = r.numero_r      # Valores numéricos
texto_python = r.texto_r        # Strings/caracteres
```

## 📊 **Bibliotecas Python por Compatibilidad**

### ✅ **Alta Compatibilidad** (Validadas en archivos exitosos)
- **matplotlib.pyplot**: Gráficos principales ✅
- **numpy**: Arrays y cálculos matemáticos ✅
- **matplotlib**: Configuración avanzada ✅

### ⚠️ **Compatibilidad Media** (Requieren validación)
- **scipy**: Funciones científicas (verificar instalación)
- **pandas**: Manipulación de datos (no esencial)
- **math**: Funciones matemáticas básicas (usar numpy preferentemente)

### ❌ **Baja Compatibilidad** (Evitar)
- **seaborn**: No instalado por defecto
- **plotly**: Gráficos interactivos (no compatible con PDF)
- **bokeh**: Visualizaciones web (no compatible con R-exams)
- **altair**: Gramática de gráficos (dependencias complejas)

## 🎨 **Configuraciones Matplotlib Validadas**

### ✅ **Configuración Base Exitosa**
```python
import matplotlib.pyplot as plt
import numpy as np

# Configuración global validada
matplotlib.rcParams['font.size'] = 9
plt.rcParams['figure.figsize'] = (8, 5)
plt.rcParams['figure.titlesize'] = 0
```

### ✅ **Configuración de Ejes Estándar**
```python
# Patrón exitoso para ejes limpios
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['left'].set_linewidth(2)
ax.spines['bottom'].set_linewidth(2)

# Grid estándar
ax.grid(True, linestyle='--', alpha=0.7)
# O específico por eje:
ax.yaxis.grid(True, linestyle='--', linewidth=0.7, color='darkgray')

# Etiquetas con formato
plt.xticks(fontweight='bold')
plt.yticks(fontweight='bold')
plt.xlabel("Etiqueta X", fontweight='bold')
plt.ylabel("Etiqueta Y", fontweight='bold')
```

### ✅ **Colores Validados para Multi-formato**
```python
# Colores para gráficos de barras (validados)
colores_azules = ['#00B3E6', '#0066CC', '#000099', '#003366', '#001133']

# Colores para gráficos circulares (validados)
colores_estandar = ['#1f77b4', '#aec7e8', '#ff7f0e', '#ffbb78', '#2ca02c', 
                   '#98df8a', '#d62728', '#ff9999', '#9467bd', '#c5b0d5']

# Colores para líneas (validados)
colores_lineas = ['red', 'blue', 'green', 'purple', 'orange']
```

### ✅ **Layout y Ajustes Finales**
```python
# Ajustes de layout (patrón exitoso)
plt.subplots_adjust(top=1, bottom=0.1, left=0.1, right=0.9)
# O para gráficos circulares:
plt.subplots_adjust(left=0.1, top=0.9, right=0.9, bottom=0.1)

# Para figuras con tight_layout
fig, ax = plt.subplots(figsize=(6, 6), tight_layout=True)

# OBLIGATORIO: Mostrar gráfico
plt.show()
```

## 🔍 **Checklist de Validación**

### 📋 **Pre-implementación**
- [ ] ¿Python está correctamente configurado con `use_python()`?
- [ ] ¿Se usan solo bibliotecas validadas (matplotlib, numpy)?
- [ ] ¿Los chunks Python tienen configuración estándar?
- [ ] ¿Se incluye `plt.show()` al final?
- [ ] ¿La transferencia R→Python usa patrón `r.variable`?

### 🧪 **Testing Multi-formato**
- [ ] Genera imagen correctamente en `exams2html`
- [ ] Compila sin errores en `exams2pdf`
- [ ] Funciona en `exams2moodle` sin problemas
- [ ] Compatible con `exams2pandoc`
- [ ] Imprime correctamente en `exams2nops`

### 🔧 **Optimización**
- [ ] Tamaño de figura apropiado para formato
- [ ] Colores con contraste suficiente
- [ ] Texto legible en diferentes resoluciones
- [ ] Tiempo de generación < 5 segundos por gráfico

## 🛠️ **Herramientas de Diagnóstico**

### 📊 **Script de Prueba Python-Reticulate**
```r
# Función para probar compatibilidad Python
test_python_rexams <- function(rmd_file) {
  
  # Verificar configuración Python
  if (!py_available()) {
    return("❌ Python no disponible")
  }
  
  # Verificar bibliotecas esenciales
  bibliotecas <- c("matplotlib", "numpy")
  for (lib in bibliotecas) {
    if (!py_module_available(lib)) {
      return(paste("❌", lib, "no disponible"))
    }
  }
  
  # Probar transferencia R→Python
  test_var <- 42
  py_run_string("test_result = r.test_var * 2")
  resultado <- py$test_result
  
  if (resultado != 84) {
    return("❌ Transferencia R→Python fallida")
  }
  
  return("✅ Configuración Python exitosa")
}
```

### 🔍 **Análisis de Errores Comunes**
```r
# Detectar problemas comunes en código Python
analyze_python_code <- function(python_code) {
  issues <- list()
  
  # Detectar bibliotecas problemáticas
  if (grepl("import seaborn|import plotly|import bokeh", python_code)) {
    issues$libraries <- "⚠️ Bibliotecas no validadas detectadas"
  }
  
  # Detectar falta de plt.show()
  if (!grepl("plt\\.show\\(\\)", python_code)) {
    issues$show <- "❌ Falta plt.show() - Obligatorio para R-exams"
  }
  
  # Detectar configuraciones problemáticas
  if (grepl("plt\\.style\\.use", python_code)) {
    issues$style <- "⚠️ Evitar plt.style.use() - Usar configuración manual"
  }
  
  return(issues)
}
```

## 🚨 **Errores Comunes y Soluciones**

### ❌ **Error: "Python not found"**
**Solución**:
```r
# Verificar Python disponible
Sys.which("python")
Sys.which("python3")

# Configurar explícitamente
use_python("/usr/bin/python3", required = TRUE)
```

### ❌ **Error: "matplotlib not available"**
**Solución**:
```bash
# Instalar matplotlib
pip install matplotlib numpy

# O con conda
conda install matplotlib numpy
```

### ❌ **Error: "No plot generated"**
**Solución**:
```python
# Asegurar plt.show() al final
plt.show()

# Verificar configuración de chunks
# results="hide" en chunk header
```

### ❌ **Error: "Figure too large"**
**Solución**:
```python
# Usar tamaños validados
plt.rcParams['figure.figsize'] = (8, 5)  # Para funciones
plt.rcParams['figure.figsize'] = (6, 5)  # Para barras
plt.rcParams['figure.figsize'] = (6, 6)  # Para circulares
```

## 📈 **Métricas de Rendimiento**

### ⏱️ **Tiempos de Generación Típicos**
- Gráfico de barras simple: 1-2 segundos
- Gráfico circular: 1-2 segundos
- Función lineal: 1-3 segundos
- Múltiples gráficos: 3-8 segundos

### 💾 **Tamaños de Archivo Típicos**
- PNG (DPI=150): 50-200 KB
- PDF vectorial: 20-100 KB
- HTML embebido: 100-300 KB

### 🎯 **Objetivos de Optimización**
- Tiempo generación < 5 segundos
- Tamaño archivo < 500 KB
- Compatibilidad > 95% en formatos principales

---

**Última actualización**: `r Sys.Date()`  
**Versión**: 1.0  
**Mantenedor**: Proyecto ICFES R-exams