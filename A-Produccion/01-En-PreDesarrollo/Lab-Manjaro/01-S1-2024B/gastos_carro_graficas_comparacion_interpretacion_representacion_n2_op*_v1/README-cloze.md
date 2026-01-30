# README - Ejercicio de Completar (cloze)

## 📊 Gastos de Carro - Gráficas de Comparación e Interpretación

### 🎯 **Descripción del Ejercicio**

Ejercicio de completar (cloze) que evalúa la capacidad de interpretar y comparar representaciones gráficas de datos financieros. Los estudiantes deben completar los valores correspondientes a los gastos de un vehículo en diferentes semanas.

### 📋 **Información Técnica**

- **Archivo:** `gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_cloze_v1.Rmd`
- **Tipo:** Cloze (completar)
- **Nivel:** 2 (Intermedio)
- **Competencia ICFES:** Representación e Interpretación
- **Área:** Estadística y Probabilidad

### 🎨 **Características Visuales**

#### **Gráficas Profesionales:**
- **Paleta corporativa:** Tonos azules (#2E5984, #5B9BD5, #A5A5A5)
- **Efectos visuales:** Sombras, separación de sectores, bordes
- **Resolución:** 150 DPI para impresión de calidad
- **Formato de números:** Separadores de miles ($89,000)

### 🔧 **Compatibilidad de Formatos**

#### **✅ Formatos Soportados:**
- **HTML** - Gráficas PNG de alta calidad
- **PDF** - Gráficas PNG justificadas a la izquierda
- **DOCX** - Gráficas PNG compatibles con Microsoft Word
- **Moodle** - Formato estándar para LMS

#### **📝 Comandos de Generación:**
```r
# HTML
exams2html("archivo.Rmd", template = "plain.html")

# PDF
exams2pdf("archivo.Rmd", template = "plain")

# DOCX
exams2pandoc("archivo.Rmd", template = "plain.tex", format = "docx")
```

### 🎲 **Aleatorización de Datos**

#### **Rangos de Valores:**
- **Gasolina:** $20,000 - $50,000 por semana
- **Parqueadero:** $15,000 - $30,000 por semana
- **Peajes:** $10,000 - $25,000 por semana

#### **Variación Automática:**
- Datos diferentes en cada generación
- Totales calculados dinámicamente
- Porcentajes actualizados automáticamente

### 📊 **Archivos Generados**

#### **Gráficas PNG:**
- `grafica_a.png` - Circular por categoría (~60 KB)
- `grafica_b.png` - Barras apiladas por semana (~60 KB)
- `grafica_c.png` - Circular por semana (~77 KB)
- `grafica_d.png` - Barras agrupadas por categoría (~58 KB)

#### **Archivos Adicionales:**
- `tabla_gastos.png` - Tabla de datos de referencia
- `figure/` - Carpeta con archivos PDF adicionales

### 🧠 **Análisis Pedagógico**

#### **Competencias Evaluadas:**
1. **Interpretación de gráficas** estadísticas
2. **Comparación** de representaciones visuales
3. **Cálculo** de valores a partir de datos
4. **Análisis** de datos financieros

### 🔍 **Solución Detallada**

#### **Respuesta Correcta:**
Las respuestas correctas son los valores numéricos calculados para cada campo a completar.

#### **Justificación Matemática:**
- Semana 1: $89,000 total
- Semana 2: $71,000 total
- Semana 3: $85,000 total
- Semana 4: $76,000 total

### 🛠 **Implementación Técnica**

#### **Patrón de Ejemplos Funcionales:**
```python
# Generación de gráficas
plt.ioff()  # Modo no interactivo
# ... código de gráfica ...
plt.savefig('archivo.png', dpi=150, bbox_inches='tight', 
            facecolor='white', edgecolor='none')
plt.close()
```

#### **Inclusión en Documento:**
```markdown
- ![](grafica_a.png){width=70%}
- ![](grafica_b.png){width=80%}
- ![](grafica_c.png){width=70%}
- ![](grafica_d.png){width=90%}
```

### 📚 **Referencias y Ejemplos**

#### **Basado en:**
- Ejemplos funcionales de `/A-Produccion/Ejemplos-Funcionales-Rmd/`
- Patrón de `estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2_py.Rmd`
- Mejores prácticas de R-exams con Python

#### **Archivos Relacionados:**
- `SemilleroCloze.R` - Script de generación masiva
- `REPORTE_Gastos_Carro_Graficas.md` - Documentación adicional

### ⚙️ **Configuración Requerida**

#### **Dependencias R:**
```r
library(exams)
library(reticulate)
library(knitr)
```

#### **Dependencias Python:**
```python
import matplotlib.pyplot as plt
import numpy as np
```

#### **Configuración Python:**
```r
use_python('/usr/bin/python3')
```

### 🚀 **Uso en Producción**

#### **Generación Individual:**
```r
set.seed(123)  # Para reproducibilidad
exams2html("gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_cloze_v1.Rmd")
```

#### **Generación Masiva:**
```r
source("SemilleroCloze.R")
# Genera múltiples versiones automáticamente
```

### 📈 **Métricas de Calidad**

- ✅ **Compatibilidad:** 100% en HTML, PDF, DOCX
- ✅ **Calidad visual:** Resolución profesional 150 DPI
- ✅ **Funcionalidad:** Aleatorización completa
- ✅ **Pedagogía:** Competencias ICFES alineadas
- ✅ **Mantenibilidad:** Código documentado y modular

---

## 📞 **Soporte**

Para dudas o mejoras, consultar:

- Documentación de R-exams
- Ejemplos funcionales en `/Auxiliares/`
- Patrones de implementación Python + R

**Versión:** 1.0\
**Fecha:** Enero 2025\
**Estado:** ✅ Producción
